; -*- mode: scheme -*-
(import core/method (debug))
(import hump/gamestate)
(import lua/math (cos sin))
(import love/love (defevent))
(import love/graphics)
(import love/timer)
(import love-repl)
(import lume)
(import lurker)
(import math/vector ())

(define state { :foo "bar" })

(define gamestate-start {})
(define gamestate-test {})
(define gamestate-repl {})

(define resources-test-image :mutable nil)
(defun load-resources ()
  (setq! resources-test-image
         (love/graphics/new-image "assets/test.png")))

(defun configure-packages ()
  (.<! lurker/*lurker* :preswap
       (lambda ()
         (load-resources)))
  (.<! love-repl/*love-repl* :screenshot true)
  (.<! love-repl/*love-repl* :toggle_key "ignore"))

(defevent :update (dt)
  (lurker/update))

(defevent :load ()
  (load-resources)
  (configure-packages)
  (love-repl/initialize)
  (hump/gamestate/register-events)
  (hump/gamestate/switch gamestate-start))

(defevent :keypressed (key code)
  (when (and (not (love-repl/toggled)) (= key "f8"))
        (hump/gamestate/push gamestate-repl)))

(defevent (gamestate-start :draw) ()
  (let ([msg "Hello world! Hello world! Hello world! \
              Hello world! Hello world! Hello world! Hello world! \
              Hello world! Hello world! Hello world! Hello world! \
              Hello world! Hello world! Hello world! Hello world! \
              Hello world! Hello world! Hello world! Hello world! \
              Hello world! Hello world! Hello world! Hello world! \
              Hello world! Hello world! Hello world! Hello world! \
              Hello world! Hello world! Hello world! Hello world!"]
        [width (love/graphics/get-width)]
        [height (love/graphics/get-height)]
        [time (love/timer/get-time)])
    (love/graphics/print (lume/wordwrap msg 40) 400 300)
    (let ([x (+ (/ width 2) (* 100 (cos time)))]
          [y (+ (/ height 2) (* 100 (sin time)))])
      (love/graphics/draw resources-test-image x y))))

(defevent (gamestate-start :keypressed) (key code)
  (case key
    ["space" (hump/gamestate/switch gamestate-test)]
    [?default]))

(defevent (gamestate-test :draw) ()
  (love/graphics/print "foo" 400 300))

(defevent (gamestate-test :keypressed) (key code)
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
