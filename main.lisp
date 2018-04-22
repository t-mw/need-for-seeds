; -*- mode: scheme -*-
(import core/demand (demand))
(import core/method (debug))
(import data/struct (defstruct))
(import hump/camera)
(import hump/gamestate)
(import lua/basic (mod))
(import lua/math (cos sin deg rad pi floor max random randomseed))
(import lua/os)
(import lua/string)
(import love/audio)
(import love/filesystem)
(import love/graphics)
(import love/keyboard)
(import love/love (defevent))
(import love/timer)
(import love-repl)
(import lume)
(import math/vector ())
(import model-viewer)
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
(define field-height-tiles 30)
(define field-width-world (* field-width-tiles field-tile-size))
(define field-height-world (* field-height-tiles field-tile-size))

(define harvester-head-width 200)
(define harvester-head-piece-count 10)
(define harvester-head-piece-width (* 0.8 (/ harvester-head-width harvester-head-piece-count)))
(define harvester-head-piece-fraction (/ 1 harvester-head-piece-count))
(define harvester-head-positions
  (dolist ([i (range :from 0 :to (- harvester-head-piece-count 1))])
          (* i harvester-head-width harvester-head-piece-fraction)))

(define obstacle-count-max 5)

(define start-x (/ field-width-world 2))
(define start-y 100)
(define player-origin-tile-y 10)

(defun to-1d-idx (x y sx)
  (+ (- x 1) (* sx (- y 1)) 1))

(defun from-1d-idx (idx sx sy)
  (let* ([idx (- idx 1)]
         [x (mod idx sx)]
         [y (mod (floor (/ idx sx)) sy)])
    (values-list (+ x 1) (+ y 1))))

(defun field-max-idx ()
  (* field-width-tiles field-height-tiles))

(defstruct field
  (fields (mutable data)
          (mutable tile-offset-y)))

(defun create-field ()
  (let ([data (list)])
    (for _ 1 (field-max-idx) 1
         (push! data true))
    (make-field data 0)))

(defun field-shift-y (field y)
  (let ([data (field-data field)]
        [diff-y (- (field-tile-offset-y field) y)])
        (for i 1 (field-max-idx) 1
             (with (i2 (- i (* field-width-tiles diff-y)))
                   (when (and (>= i2 1) (<= i2 (field-max-idx)))
                         (setq! (nth data i) (nth data i2))))))
  (set-field-tile-offset-y! field y))

(defun field-tile-from-world (x y tile-offset-y)
  (values-list (+ (floor (/ x field-tile-size)) 1) (+ (- (floor (/ y field-tile-size)) tile-offset-y) 1)))

(defun field-tile-to-world (x y tile-offset-y)
  (values-list (* (- x 1) field-tile-size) (* (- (+ y tile-offset-y) 1) field-tile-size)))

(defun field-tile-to-1d-idx (tile-x tile-y)
  (to-1d-idx tile-x tile-y field-width-tiles))

(defun field-tile-from-1d-idx (idx)
  (from-1d-idx idx field-width-tiles field-height-tiles))

(defun field-is-valid-tile (x y)
  (and (>= x 1) (<= x field-width-tiles) (>= y 1) (<= y field-height-tiles)))

(defun seconds-to-clock (s)
  (let* ([m (floor (/ s 60))]
         [ms (* 100 (- s (floor s)))]
         [s (floor (- s (* m 60)))])
    (concat (list (lua/string/format "%02.f" m) ":" (lua/string/format "%02.f" s) ":" (lua/string/format "%02.f" ms)))))

(defstruct state
  (fields (mutable pos)
          (mutable camera)
          (mutable harvester-head-pieces)
          (mutable field)
          (mutable moving)
          (mutable start-time)
          (mutable end-time)
          (mutable score)))
(define state (make-state))

(define audio-queue (list))
(defun audio-queue-current ()
  (car audio-queue))
(defun audio-queue-push (source)
  (push! audio-queue source)
  (when (= (n audio-queue) 1)
        (love/audio/play source)))
(defun audio-queue-update ()
  (when (audio-queue-current)
        (when (not (self (audio-queue-current) :isPlaying))
              (remove-nth! audio-queue 1))
        (with (source (audio-queue-current))
              (when source (love/audio/play source)))))

(defstruct physics
  (fields (mutable world)
          (mutable harvester-main)
          (mutable harvester-head-pieces)
          (mutable obstacles)
          (mutable walls)))
(define physics (make-physics))

(defun position-from-body (body)
  (self body :getPosition))

(defun physics-player-position (physics)
  (position-from-body (physics-harvester-main physics)))

(defun camera-position (player-x player-y)
  (values-list player-x (+ player-y 200)))

(defun handle-harvester-collisions ()
  (let ([pieces (physics-harvester-head-pieces physics)]
        [piece-states (state-harvester-head-pieces state)]
        [field-data (field-data (state-field state))]
        [field-tile-offset-y (field-tile-offset-y (state-field state))]
        [to-destroy (list)])
    (for i 1 (n pieces) 1
         (let ([piece (nth pieces i)]
               [piece-state (nth piece-states i)])
           (when piece-state
                 (if (self piece :enter "Obstacle")
                     (push! to-destroy i)
                     (let* ([(x y) (self piece :getPosition)]
                            [(tile-x tile-y) (field-tile-from-world x y field-tile-offset-y)]
                            [idx (field-tile-to-1d-idx tile-x tile-y)])
                       (when (and (field-is-valid-tile tile-x tile-y) (nth field-data idx))
                             (setq! (nth field-data idx) false)
                             (set-state-score! state (+ (state-score state) 1))))))))

    (let ([destroy-piece (lambda (i)
                           (with (piece (nth pieces i))
                                 (do ([joint (struct->list (self piece :getJointList))])
                                     (self joint :destroy))
                                 (self piece :setCollisionClass "Ghost")
                                 (setq! (nth piece-states i) false)
                                 (love/audio/play resources-audio-hurt)))]
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
                       (destroy-piece i)))))))

(defun world-draw ()
  (let ([width (love/graphics/get-width)]
        [height (love/graphics/get-height)]
        [camera (state-camera state)])
    ;; (0 0) is at bottom-middle of screen.
    ;; x +ve is right, y +ve is up.
    (self camera :attach)
    (love/graphics/line 0 0 10 10)
    (love/graphics/set-color 50 100 50)
    (for i 1 (field-max-idx) 1
         (let* ([field (state-field state)]
                [field-data (field-data field)]
                [field-tile-offset-y (field-tile-offset-y field)]
                [(tile-x tile-y) (field-tile-from-1d-idx i)]
                [v (nth field-data i)]
                [(x y) (field-tile-to-world tile-x tile-y field-tile-offset-y)])
           (if v
               (love/graphics/rectangle "fill" x y field-tile-size field-tile-size)
               (love/graphics/rectangle "line" x y field-tile-size field-tile-size))))
    (self (physics-world physics) :draw)

    (love/graphics/set-color 255 255 255)
    (love/graphics/push)
    (love/graphics/scale -1 -1)
    (with (angle (self (physics-harvester-main physics) :getAngle))
          (.<! resources-model-harvester :rotation (deg angle)))
    (with ((x y) (physics-player-position physics))
          (self resources-model-harvester :drawModel (- 0 x) (- 0 y)))

    (let* ([pieces (physics-harvester-head-pieces physics)]
           [piece-states (state-harvester-head-pieces state)])
      (for i 1 (n pieces) 1
           (let ([piece (nth pieces i)]
                 [piece-state (nth piece-states i)]
                 [flip (= 0 (mod (+ (floor (/ (love/timer/get-time) 0.05)) (mod i 2)) 2))])
             (with (angle (self piece :getAngle))
                   (.<! resources-model-blades :rotation (deg angle)))
             (.<! resources-model-blades :zoom 0.25)
             (.<! resources-model-blades :flip (and piece-state flip))
             (with ((x y) (position-from-body piece))
                   (self resources-model-blades :drawModel (- 0 x) (- 0 y))))))
    (love/graphics/pop)

    ;; draw physics centers
    ;; (love/graphics/set-color 255 255 255)
    ;; (do ([o (physics-harvester-head-pieces physics)])
    ;;     (let* ([(x y) (position-from-body o)])
    ;;       (love/graphics/circle "fill" x y 5)))
    ;; (love/graphics/line 0 200 field-width-world 200)

    (self camera :detach)
    (love/graphics/origin)))

(define gamestate-start {})
(define gamestate-main {})
(define gamestate-end {})
(define gamestate-repl {})

(define resources-audio-hurt :mutable nil)
(define resources-audio-engine :mutable nil)

(define resources-music-intro :mutable nil)
(define resources-music-loop :mutable nil)

(define resources-model-blades (model-viewer/new (love/filesystem/new-file "assets/sprite-blades.png")))
(define resources-model-harvester (model-viewer/new (love/filesystem/new-file "assets/sprite-harvester.png") true))

(defun load-resources ()
  (setq! resources-audio-engine
         (love/audio/new-source "assets/audio-engine.wav" "static"))
  (self resources-audio-engine :setLooping true)
  (self resources-audio-engine :setVolume 0.2)
  (setq! resources-audio-hurt
         (love/audio/new-source "assets/audio-hurt.wav" "static"))

  (setq! resources-music-intro
         (love/audio/new-source "assets/music-intro.ogg" "stream"))
  (setq! resources-music-loop
         (love/audio/new-source "assets/music-loop.ogg" "stream"))
  (self resources-music-loop :setLooping true))

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

(defevent :draw ())

(defevent :keypressed (key code)
  (when (and (not (love-repl/toggled)) (= key "f8"))
        (hump/gamestate/push gamestate-repl)))

(defevent (gamestate-start :draw) ())

(defevent (gamestate-start :keypressed) (key code)
  (case key
    ["space" (hump/gamestate/switch gamestate-main)]
    [?default]))

(defevent (gamestate-main :enter) ()
  (love/audio/stop)
  (love/audio/play resources-audio-engine)

  (with (camera (hump/camera/new (camera-position start-x (+ start-y 80))))
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
         [walls (dolist
                 ([wall (list
                         (self world :newRectangleCollider field-width-world 0 100 999999999)
                         (self world :newRectangleCollider -100 0 100 999999999)
                         (self world :newRectangleCollider 0 -100 field-width-world 100))])
                 (self wall :setType "static")
                 wall)])
    (set-physics-world! physics world)
    (set-physics-harvester-main! physics harvester-main)
    (set-physics-harvester-head-pieces! physics harvester-head-pieces)
    (set-physics-obstacles! physics (list))
    (set-physics-walls! physics walls)
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
  (set-state-field! state (create-field))
  (set-state-moving! state false)
  (set-state-start-time! state 0)
  (set-state-end-time! state 0)
  (set-state-score! state 0))

(defevent (gamestate-main :update) (dt)
  (audio-queue-update)
  (when (and (not (audio-queue-current)) (state-moving state))
        (progn
         (audio-queue-push resources-music-intro)
         (audio-queue-push resources-music-loop)))

  (when (not (state-moving state))
        (set-state-start-time! state (love/timer/get-time)))

  ;; shift field for infinite scrolling
  (let* ([(x y) (physics-player-position physics)]
         [field (state-field state)]
         [field-tile-offset-y (field-tile-offset-y field)]
         [(_ player-tile-y) (field-tile-from-world x y player-origin-tile-y)])
    (when (> player-tile-y field-tile-offset-y)
          (field-shift-y (state-field state) player-tile-y)))

  (let* ([obstacles (physics-obstacles physics)]
         [obstacles-new (list)])
    ;; delete obstacles that are out of view
    (for i 1 (n obstacles) 1
         (let* ([obstacle (nth obstacles i)]
                [(x y) (position-from-body obstacle)]
                [(_ tile-y) (field-tile-from-world x y (field-tile-offset-y (state-field state)))])
           (if (>= tile-y 1)
               (push! obstacles-new obstacle)
               (self obstacle :destroy))))
    ;; generate new obstacles
    (for i 1 (- obstacle-count-max (n obstacles-new)) 1
         (let* ([radius 20]
                [pos-x (+ radius (* (random) (- field-width-world (* 2 radius))))]
                [(_ player-y) (physics-player-position physics)]
                ;; generate new obstacles out of view
                [pos-y (+ player-y field-height-world (* (random) field-height-world))]
                [obstacle (self (physics-world physics) :newCircleCollider pos-x pos-y radius)])
           (self obstacle :setType "static")
           (self obstacle :setCollisionClass "Obstacle")
           (push! obstacles-new obstacle)))
    (set-physics-obstacles! physics obstacles-new))

  (self (physics-world physics) :update dt)
  (let* ([camera (state-camera state)]
         ;; place player at bottom of screen
         [(new-x new-y) (camera-position (physics-player-position physics))])
    (let ([new-y (max new-y (.> camera :y))])
          (self camera :lockPosition new-x new-y)))

  (when (state-moving state)
        (handle-harvester-collisions))

  (let ([up (love/keyboard/is-down "up")]
        [left (love/keyboard/is-down "left")]
        [right (love/keyboard/is-down "right")])
    (when up
          (let* ([angle (self (physics-harvester-main physics) :getAngle)]
                 [v (* 1000 (rotate (vector 0 1) angle))]
                 [harvester-main (physics-harvester-main physics)])
            (self harvester-main :applyForce (vector-item v 1) (vector-item v 2))))
    (when left
          (self (physics-harvester-main physics) :applyAngularImpulse -1000))
    (when right
          (self (physics-harvester-main physics) :applyAngularImpulse 1000))
    (when (or up left right)
          (set-state-moving! state true)))
  (let* ([harvester-main (physics-harvester-main physics)]
         [pitch (+ 1 (/ (norm (vector (self harvester-main :getLinearVelocity))) 100))])
    (self resources-audio-engine :setPitch pitch))

  (unless (elem? true (state-harvester-head-pieces state))
          (set-state-end-time! state (love/timer/get-time))))

(defevent (gamestate-main :draw) ()
  (world-draw)

  (love/graphics/set-color 255 255 255)
  (love/graphics/print (seconds-to-clock (- (love/timer/get-time) (state-start-time state) 0 0)))
  (love/graphics/print (state-score state) 0 20)

  ;; switching state in 'update' causes gamestate drawing to be skipped for one frame
  (when (> (state-end-time state) 0)
        (hump/gamestate/switch gamestate-end)))

(defevent (gamestate-end :keypressed) (key code)
  (case key
    ["space" (hump/gamestate/switch gamestate-main)]
    [?default]))

(defevent (gamestate-end :draw) ()
  (world-draw)

  (love/graphics/set-color 255 255 255)
  (love/graphics/print (seconds-to-clock (- (state-end-time state) (state-start-time state) 0 0)))
  (love/graphics/print (state-score state) 0 20))

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
