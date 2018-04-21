; -*- mode: scheme -*-
(import core/demand (demand))
(import core/method (debug))
(import data/struct (defstruct))
(import hump/camera)
(import hump/gamestate)
(import lua/basic (mod))
(import lua/math (cos sin pi floor random randomseed))
(import lua/os)
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
(define field-width-tiles 20)
(define field-height-tiles 200)
(define field-width-world (* field-width-tiles field-tile-size))
(define field-height-world (* field-height-tiles field-tile-size))

(define harvester-head-width 200)
(define harvester-head-piece-count 10)
(define harvester-head-piece-width (* 0.8 (/ harvester-head-width harvester-head-piece-count)))
(define harvester-head-piece-fraction (/ 1 harvester-head-piece-count))
(define harvester-head-positions
  (dolist ([i (range :from 0 :to (- harvester-head-piece-count 1))])
          (* i harvester-head-width harvester-head-piece-fraction)))

(define start-x (/ field-width-world 2))
(define start-y 20)

(defun to-1d-idx (x y sx)
  (+ (- x 1) (* sx (- y 1)) 1))

(defun from-1d-idx (idx sx sy)
  (let* ([idx (- idx 1)]
         [x (mod idx sx)]
         [y (mod (floor (/ idx sx)) sy)])
    (values-list (+ x 1) (+ y 1))))

(defun field-max-idx ()
  (* field-width-tiles field-height-tiles))

(defun make-field ()
  (let ([result (list)])
    (for _ 1 (field-max-idx) 1
         (push! result true))
    result))

(defun field-tile-from-world (x y)
  (values-list (floor (/ x field-tile-size)) (floor (/ y field-tile-size))))

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
          (mutable harvester-head-pieces)
          (mutable field)))
(define state (make-state))

(defstruct physics
  (fields (mutable world)
          (mutable harvester-main)
          (mutable harvester-head-pieces)
          (mutable obstacles)))
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
  (randomseed (lua/os/time))

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
  (with (camera (hump/camera/new start-x start-y))
        (set-state-camera! state camera)
        (self camera :rotate pi)
        (.<! camera :smoother (hump/camera/smooth-damped 5)))
  (let* ([world (with (world (windfield/new-world 0 0 true))
                      (self world :addCollisionClass "Head")
                      (self world :addCollisionClass "Obstacle")
                      (self world :addCollisionClass "Ghost" { :ignores { 1 "Head" 2 "Obstacle"} })
                      world)]
         [harvester-main (self world :newRectangleCollider (- start-x 40) (+ start-y 0) 80 80)]
         [origin-x (- start-x (/ harvester-head-width 2))]
         [origin-y (+ start-y 100)]
         [harvester-head-pieces
          (dolist ([pos harvester-head-positions])
                  (with (piece (self world :newRectangleCollider (+ origin-x pos) origin-y harvester-head-piece-width harvester-head-piece-width))
                        (self piece :setCollisionClass "Head")
                        piece))]
         [obstacles
          (dolist ([i (range :from 1 :to 40)])
                  (let* ([pos-x (* (random) field-width-world)]
                         [pos-y (+ (* 10 start-y) (* (random) (- field-height-world (* 10 start-y))))]
                         [obstacle (self world :newCircleCollider pos-x pos-y 20)])
                    (self obstacle :setType "static")
                    (self obstacle :setCollisionClass "Obstacle")
                    obstacle))])
    (set-physics-world! physics world)
    (set-physics-harvester-main! physics harvester-main)
    (set-physics-harvester-head-pieces! physics harvester-head-pieces)
    (set-physics-obstacles! physics obstacles)
    (self harvester-main :setLinearDamping 0.5)
    (for i 1 harvester-head-piece-count 1
         (let* ([piece (nth harvester-head-pieces i)]
                [joint (self world :addJoint "RevoluteJoint" piece harvester-main start-x (+ start-y 80) true)])
           (with (next-i (+ i 1))
                 (when (<= next-i harvester-head-piece-count)
                       (let ([next-piece (nth harvester-head-pieces next-i)]
                             [pos (nth harvester-head-positions next-i)])
                         (self world :addJoint "WeldJoint" piece next-piece (+ origin-x pos) origin-y)))))))
  (set-state-harvester-head-pieces! state (map (lambda () true) (physics-harvester-head-pieces physics)))
  (set-state-field! state (make-field)))

(defevent (gamestate-main :update) (dt)
  (self (physics-world physics) :update dt)
  (let* ([(x y) (self (physics-harvester-main physics) :getPosition)]
        [camera (state-camera state)])
    (self camera :lockPosition x y))
  (let ([pieces (physics-harvester-head-pieces physics)]
        [piece-states (state-harvester-head-pieces state)]
        [to-destroy (list)])
    (for i 1 (n pieces) 1
         (let ([piece (nth pieces i)]
               [piece-state (nth piece-states i)])
           (when piece-state
                 (if (self piece :enter "Obstacle")
                     (push! to-destroy i)
                     (let* ([(x y) (self piece :getPosition)]
                            [(tile-x tile-y) (field-tile-from-world x y)]
                            [idx (field-tile-to-1d-idx tile-x tile-y)])
                       (when (field-is-valid-tile tile-x tile-y)
                             (setq! (nth (state-field state) idx) false)))))))

    (let ([destroy-piece (lambda (i)
                           (with (piece (nth pieces i))
                                 (do ([joint (struct->list (self piece :getJointList))])
                                     (self joint :destroy))
                                 (self piece :setCollisionClass "Ghost")
                                 (setq! (nth piece-states i) false)))]
          [mid-idx (floor (/ (n pieces) 2))])
      (with (tmp-state true)
            (for i (+ mid-idx 1) (n pieces) 1
                 (when (elem? i to-destroy)
                       (setq! tmp-state false))
                 (when (and (not tmp-state) (nth piece-states i))
                       (destroy-piece i))))
      (with (tmp-state true)
            (for i mid-idx 1 -1
                 (when (elem? i to-destroy)
                       (setq! tmp-state false))
                 (when (and (not tmp-state) (nth piece-states i))
                       (destroy-piece i))))))

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
