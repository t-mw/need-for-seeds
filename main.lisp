; -*- mode: scheme -*-
(import core/demand (demand))
(import core/method (debug))
(import data/struct (defstruct))
(import hump/camera)
(import hump/gamestate)
(import lua/basic (mod))
(import lua/math (cos sin pi floor))
(import love/graphics)
(import love/keyboard)
(import love/love (defevent))
(import love/timer)
(import love-repl)
(import lume)
(import math/vector ())
(import windfield)

(defun vector-x (v)
  (vector-item v 1))

(defun vector-y (v)
  (vector-item v 2))

(defun rotate (v r)
  (let ([c (cos r)]
        [s (sin r)])
    (vector (- (* c (vector-x v)) (* s (vector-y v)))
            (+ (* s (vector-x v)) (* c (vector-y v))))))

(defun to-vector (s)
  (vector (.> s :x) (.> s :y)))

(define field-tile-size 30)
(define field-width-tiles 16)
(define field-height-tiles 100)
(define field-width-world (* field-width-tiles field-tile-size))
(define field-height-world (* field-height-tiles field-tile-size))

(defun to-1d-idx (x y sx)
  (+ x (* sx y)))

(defun from-1d-idx (idx sx sy)
  (let ([x (mod idx sx)]
        [y (mod (floor (/ idx sx)) sy)])
    (values-list x y)))

(defun field-max-idx ()
  (* field-width-tiles field-height-tiles))

(defun make-field ()
  (let ([result (list)])
    (for _ 1 (field-max-idx) 1
         (push! result true))
    result))

(defun field-tile-from-world (x y)
  (values-list (floor (/ x field-tile-size) (/ y field-tile-size))))

(defun field-tile-to-world (x y)
  (values-list (* x field-tile-size) (* y field-tile-size)))

(defun field-tile-to-1d-idx (tile-x tile-y)
  (to-1d-idx tile-x tile-y field-width-tiles))

(defun field-tile-from-1d-idx (idx)
  (from-1d-idx idx field-width-tiles field-height-tiles))

(defun field-is-valid-tile (x y)
  (and (>= x 1) (<= x field-width-tiles) (>= y 1) (<= y field-height-tiles)))

(defstruct state
  (fields (mutable pos)
          (mutable camera)
          (mutable field)))
(define state (make-state))

(defstruct physics
  (fields (mutable world)
          (mutable harvester-main)
          (mutable harvester-head)))
(define physics (make-physics))

(define gamestate-start {})
(define gamestate-main {})
(define gamestate-repl {})

(define resources-test-image :mutable nil)
(defun load-resources ()
  (setq! resources-test-image
         (love/graphics/new-image "assets/test.png")))

(defun configure-packages ()
  (.<! love-repl/*love-repl* :screenshot true)
  (.<! love-repl/*love-repl* :toggle_key "ignore"))

(defevent :load ()
  (configure-packages)
  (love-repl/initialize)
  (load-resources)

  (hump/gamestate/register-events)
  (hump/gamestate/switch gamestate-main))

(defevent :update (dt))

(defevent :keypressed (key code)
  (when (and (not (love-repl/toggled)) (= key "f8"))
        (hump/gamestate/push gamestate-repl)))

(defevent (gamestate-start :draw) ())

(defevent (gamestate-start :keypressed) (key code)
  (case key
    ["space" (hump/gamestate/switch gamestate-main)]
    [?default]))

(defevent (gamestate-main :enter) ()
  (let ([start-x (/ field-width-world 2)]
        [start-y 20])
    (with (camera (hump/camera/new start-x start-y))
          (set-state-camera! state camera)
          (self camera :rotate pi)
          (.<! camera :smoother (hump/camera/smooth-damped 5)))
    (let* ([world (windfield/new-world 0 0 true)]
           [harvester-main (self world :newRectangleCollider (- start-x 40) (+ start-y 0) 80 80)]
           [harvester-head (self world :newRectangleCollider (- start-x 100) (+ start-y 100) 200 10)])
      (set-physics-world! physics world)
      (set-physics-harvester-main! physics harvester-main)
      (set-physics-harvester-head! physics harvester-head)
      (self harvester-main :setLinearDamping 0.5)
      (with (joint (self world :addJoint "RevoluteJoint" harvester-main harvester-head start-x (+ start-y 80) true))
            (self joint :setLimitsEnabled true)
            (self joint :setLowerLimit -0.4)
            (self joint :setUpperLimit 0.4)))
    (set-state-field! state (make-field))))

(defevent (gamestate-main :update) (dt)
  (self (physics-world physics) :update dt)
  (let ([pos (vector (self (physics-harvester-main physics) :getPosition))]
        [camera (state-camera state)])
    (self camera :lockPosition (vector-x pos) (vector-y pos)))
  (when (love/keyboard/is-down "up")
        (let* ([angle (self (physics-harvester-main physics) :getAngle)]
               [v (* 1000 (rotate (vector 0 1) angle))])
          (self (physics-harvester-main physics) :applyForce (vector-item v 1) (vector-item v 2))))
  (when (love/keyboard/is-down "left")
        (self (physics-harvester-main physics) :applyAngularImpulse -1000))
  (when (love/keyboard/is-down "right")
        (self (physics-harvester-main physics) :applyAngularImpulse 1000)))

(defevent (gamestate-main :draw) ()
  (setq! (nth (state-field state) 20) false)
  (let ([width (love/graphics/get-width)]
        [height (love/graphics/get-height)]
        [camera (state-camera state)])
    ;; (0 0) is at bottom-middle of screen.
    ;; x +ve is right, y +ve is up.
    (self camera :attach)
    (love/graphics/line 0 0 10 10)
    (love/graphics/set-color 50 100 50)
    (for i 1 (field-max-idx) 1
         (let* ([v (nth (state-field state) i)]
                [(x y) (field-tile-to-world (field-tile-from-1d-idx i))])
           (if v
               (love/graphics/rectangle "fill" x y field-tile-size field-tile-size)
               (love/graphics/rectangle "line" x y field-tile-size field-tile-size))))
    (self (physics-world physics) :draw)
    (self camera :detach)
    (love/graphics/origin)))

(defevent (gamestate-main :keypressed) (key code)
  (case key
    ["space" (hump/gamestate/switch gamestate-start)]
    [?default]))

(defevent (gamestate-repl :wheelmoved) (x y)
  (love-repl/wheelmoved x y))

(defevent (gamestate-repl :keypressed) (key code)
  (love-repl/keypressed key code)
  (when (= key "f8")
        (hump/gamestate/pop)))

(defevent (gamestate-repl :textinput) (t)
  (love-repl/textinput t))

(defevent (gamestate-repl :draw) ()
  (love-repl/draw))

(defevent (gamestate-repl :enter) ()
  (love-repl/expose "state" state false)
  (love-repl/toggle))

(defevent (gamestate-repl :leave) ()
  (love-repl/toggle))
