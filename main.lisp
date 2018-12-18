(require :sdl2)

(defclass paddle ()
  ((paddle-x :accessor paddle-x :initarg :paddle-x)
   (paddle-y :accessor paddle-y :initarg :paddle-y)
   (paddle-height :accessor paddle-height :initarg :paddle-height)
   (paddle-width :accessor paddle-width :initarg :paddle-width)
   (paddle-speed :accessor paddle-speed :initarg :paddle-speed)))

(defclass ball ()
  ((ball-x :accessor ball-x :initarg :ball-x)
   (ball-y :accessor ball-y :initarg :ball-y)
   (ball-speed :accessor ball-speed :initarg :ball-speed)
   (ball-dir :accessor ball-dir :initarg :ball-dir)))

(defparameter *ball* (make-instance 'ball
                                    :ball-x 320
                                    :ball-y 240
                                    :ball-speed 240
                                    :ball-dir (vector 1 1)))

(defparameter *player-paddle* (make-instance 'paddle
                                             :paddle-x 0
                                             :paddle-y 0
                                             :paddle-width 20
                                             :paddle-height 60
                                             :paddle-speed 200))
(defparameter *enemy-paddle* (make-instance 'paddle
                                             :paddle-x 0
                                             :paddle-y 0
                                             :paddle-width 20
                                             :paddle-height 60
                                             :paddle-speed 200))

(defparameter *holding-down* nil)
(defparameter *holding-up* nil)

(defparameter *player-score* 0)
(defparameter *cpu-score* 0)

(defun handle-key-down (scancode)
  (cond
    ((sdl2:scancode= scancode :scancode-w) (setf *holding-up* t))
    ((sdl2:scancode= scancode :scancode-s) (setf *holding-down* t))))

(defun handle-key-up (scancode)
  (cond
    ((sdl2:scancode= scancode :scancode-w) (setf *holding-up* nil))
    ((sdl2:scancode= scancode :scancode-s) (setf *holding-down* nil))))

(defun update-ball (dt dst_surf)
  (when (> (ball-y *ball*) (- (sdl2:surface-height dst_surf) 30))
    (setf (aref (ball-dir *ball*) 1) (* -1 (+ 0.4 (random 1.0)))))
  (when (<= (ball-y *ball*) 0)
    (setf (aref (ball-dir *ball*) 1) (+ 0.4 (random 1.0))))
  
  ;(format t "~S~%" (ball-dir *ball*))

  (when (<= (ball-x *ball*) -30)
    (setf (aref (ball-dir *ball*) 0) 1)
    (setf (ball-x *ball*) (/ (sdl2:surface-width dst_surf) 2))
    (setf (ball-y *ball*) (/ (sdl2:surface-height dst_surf) 2))
    (setf (aref (ball-dir *ball*) 0) (random 1.0))
    (setf (aref (ball-dir *ball*) 1) (random 1.0))
    (setf *cpu-score* (+ 1 *cpu-score*))
    (format t "Player: ~a | CPU: ~a~%" *player-score* *cpu-score*)
    )

  (when (>= (ball-x *ball*) (sdl2:surface-width dst_surf))
    (setf (aref (ball-dir *ball*) 0) -1)
    (setf (ball-x *ball*) (/ (sdl2:surface-width dst_surf) 2))
    (setf (ball-y *ball*) (/ (sdl2:surface-height dst_surf) 2))
    (setf (aref (ball-dir *ball*) 0) (* -1 (random 1.0)))
    (setf (aref (ball-dir *ball*) 1) (random 1.0))
    (setf *player-score* (+ 1 *player-score*))
    (format t "Player: ~a | CPU: ~a~%" *player-score* *cpu-score*)
    )

  (setf (ball-x *ball*) (ceiling (+ (ball-x *ball*) (* (aref (ball-dir *ball*) 0) (* dt (ball-speed *ball*)) ))))
  (setf (ball-y *ball*) (ceiling (+ (ball-y *ball*) (* (aref (ball-dir *ball*) 1) (* dt (ball-speed *ball*)) ))))
  
  ;when ball hits player paddle
  (let ((paddle_rect (sdl2:make-rect (paddle-x *player-paddle*) (paddle-y *player-paddle*)
                                     (paddle-width *player-paddle*) (paddle-height *player-paddle*)))
        (ball_rect (sdl2:make-rect (ball-x *ball*) (ball-y *ball*)
                                   30 30))
        (enemy_rect (sdl2:make-rect (- (sdl2:surface-width dst_surf) (paddle-width *enemy-paddle*))
                                    (paddle-y *enemy-paddle*)
                                    (paddle-width *enemy-paddle*)
                                    (paddle-height *enemy-paddle*))))
    (when (sdl2:has-intersect paddle_rect ball_rect)
      (setf (aref (ball-dir *ball*) 0) (random 1.0)))
    (when (sdl2:has-intersect enemy_rect ball_rect)
      (setf (aref (ball-dir *ball*) 0) (* -1 (random 1.0)))))
  )

(defun update-enemy (dt)
  (when (> (- (ball-y *ball*) 15) (paddle-y *enemy-paddle*))
    (setf (paddle-y *enemy-paddle*) (floor (+ (paddle-y *enemy-paddle*) (* dt (paddle-speed *enemy-paddle*))))))
  (when (< (+ (ball-y *ball*) 15) (paddle-y *enemy-paddle*))
    (setf (paddle-y *enemy-paddle*) (floor (- (paddle-y *enemy-paddle*) (* dt (paddle-speed *enemy-paddle*))))))) 

(defun draw-enemy (dst_surf enemy)
  (let ((enemy_rect (sdl2:make-rect (- (sdl2:surface-width dst_surf) (paddle-width enemy)) (paddle-y enemy) (paddle-width enemy) (paddle-height enemy))))
    (sdl2:fill-rect dst_surf enemy_rect (sdl2:map-rgb (sdl2:surface-format dst_surf) #xff #xff #xff))))

(defun draw-ball (dst_surf ball)
  (let ((ball_rect (sdl2:make-rect (ball-x ball) (ball-y ball) 30 30)))
    (sdl2:fill-rect dst_surf ball_rect (sdl2:map-rgb (sdl2:surface-format dst_surf) #xff #xff #xff))))

(defun update (dt)
  (when *holding-down*
    (setf (paddle-y *player-paddle*) (floor (+ (* dt (paddle-speed *player-paddle*)) (paddle-y *player-paddle*))))) 
  (when *holding-up*
    (setf (paddle-y *player-paddle*) (floor (- (paddle-y *player-paddle*)(* dt (paddle-speed *player-paddle*))))))   
  )

(defun draw-paddle (dst_surf p)
  (let ((paddle_rect (sdl2:make-rect (paddle-x p) (paddle-y p) (paddle-width p) (paddle-height p))))
    (sdl2:fill-rect dst_surf paddle_rect (sdl2:map-rgb (sdl2:surface-format dst_surf) #xff #xff #xff))))

(defun draw-middle-line (dst_surf)
  (sdl2:fill-rect dst_surf 
                  (sdl2:make-rect (/ (sdl2:surface-width dst_surf) 2) 0
                                  10 (sdl2:surface-height dst_surf))
                  (sdl2:map-rgb (sdl2:surface-format dst_surf) #xff #xff #xff)))

(defun main ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "Pong" :flags '(:shown))
      (let* ((surf (sdl2:get-window-surface win))
             (time_seconds (/ (sdl2:get-ticks) 1000.0))
             (max_frame_ticks (/ 1000.0 60))
             (fps 0)
             (last_ticks (sdl2:get-ticks)))
        (sdl2:with-event-loop (:method :poll)
          (:idle ()
           (setf fps (+ 1 fps))
           (sdl2:fill-rect surf nil (sdl2:map-rgb (sdl2:surface-format surf) #x00 #x00 #x00))
           (let* ((new_time (/ (sdl2:get-ticks) 1000.0))
                  (delta (- new_time time_seconds))
                  (target_ticks (+ last_ticks (* fps max_frame_ticks)))
                  (current_ticks (sdl2:get-ticks)))
             (setf time_seconds new_time)
             (when (< current_ticks target_ticks)
               ;limmit frames per second for smoother performance
               ;frames should be locked at 60
               ;we need all the various ticks to calculate how much we are to delay
               ;if in doubt lookup on the web frame limmiting with sdl
               (progn
                 (sdl2:delay (round (- target_ticks current_ticks)))
                 (setf current_ticks (sdl2:get-ticks))))
            (when (< (- current_ticks last_ticks))
              (progn
                (setf fps 0)
                (setf last_ticks (sdl2:get-ticks))
                (update delta)
                (update-ball delta surf)
                (update-enemy delta)
                (draw-ball surf *ball*)
                (draw-paddle surf *player-paddle*)
                (draw-enemy surf *enemy-paddle*)
                (draw-middle-line surf)
                (sdl2:update-window win)))
             ))
          (:quit () t)
          (:keyup (:keysym keysym)
           (let ((scancode (sdl2:scancode-value keysym)))
             (when scancode
               (handle-key-up scancode))))
          (:keydown (:keysym keysym)
           (let* ((scancode (sdl2:scancode-value keysym)))
             (handle-key-down scancode)))
          )))))
