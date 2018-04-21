; -*- mode: scheme -*-
(import core/demand (demand))
(import core/method (debug))
(import data/struct (defstruct))
(import hump/gamestate)
(import lua/math (cos sin))
(import love/graphics)
(import love/keyboard)
(import love/love (defevent))
(import love/timer)
(import love-repl)
(import lume)
(import math/vector ())
(import windfield)

(defun rotate (v r)
  (let ([c (cos r)]
        [s (sin r)])
    (vector (- (* c (vector-item v 1)) (* s (vector-item v 2)))
            (+ (* s (vector-item v 1)) (* c (vector-item v 2))))))

(define state { :foo "bar" })
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
  (let* ([world (windfield/new-world 0 0 true)]
         [harvester-main (self world :newRectangleCollider -40 0 80 80)]
         [harvester-head (self world :newRectangleCollider -100 100 200 10)])
        (set-physics-world! physics world)
        (set-physics-harvester-main! physics harvester-main)
        (set-physics-harvester-head! physics harvester-head)
        (self harvester-head :applyAngularImpulse 5000)
        (with (joint (self world :addJoint "RevoluteJoint" harvester-main harvester-head 0 80 true))
              (self joint :setLimitsEnabled true)
              (self joint :setLowerLimit -0.3)
              (self joint :setUpperLimit 0.3))))

(defevent (gamestate-main :update) (dt)
  (self (physics-world physics) :update dt)
  (when (love/keyboard/is-down "up")
        (let* ([angle (self (physics-harvester-main physics) :getAngle)]
               [v (* 1000 (rotate (vector 0 1) angle))])
          (self (physics-harvester-main physics) :applyForce (vector-item v 1) (vector-item v 2))))
  (when (love/keyboard/is-down "left")
        (self (physics-harvester-main physics) :applyAngularImpulse 1000))
  (when (love/keyboard/is-down "right")
        (self (physics-harvester-main physics) :applyAngularImpulse -1000)))

(defevent (gamestate-main :draw) ()
  (let ([width (love/graphics/get-width)]
        [height (love/graphics/get-height)])
    ;; (0 0) is at bottom-middle of screen.
    ;; x +ve is right, y +ve is up.
    (love/graphics/translate (/ width 2) height)
    (love/graphics/scale 1 -1)
    (self (physics-world physics) :draw)
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
