#lang racket
(require plot)
(require racket/gui/base)
(require racket/pretty)
(require racket/math)

;; En pixelboll i racket
;; Från Buzzern skrivet under påskhelgen

(define π (acos -1))
(define HEIGHT 600)
(define WIDTH 800)

;; from scheme.com since normal scheme lacks for/vector
(define make-matrix
  (lambda (rows columns)
    (do ((m (make-vector rows))
         (i 0 (+ i 1)))
      ((= i rows) m)
      (vector-set! m i (make-vector columns)))))

(define (print-dots amount)
  (let myloop ((count 0))
    (if (>= count amount)
        '()
        (begin
          (println count)
          (myloop (+ count 1))))))


(define (projection x y z width height)
  (let* ([camera-distance 300.0]  ; Push the object away from the camera
         [scale-factor 250.0]     ; Keep the object visible after pushing it back         
         ; Translate Z away from the viewer
         [z-translated (+ z camera-distance)]         
         ; Prevent divide-by-zero (just in case a point hits exactly 0)
         [z-safe (if (= z-translated 0.0) 0.001 z-translated)]
         
         ; Perspective projection with scaling
         [xp (* x (/ scale-factor z-safe))]
         [yp (* y (/ scale-factor z-safe))])
    ;over to screen coords
    (vector (+ xp (/ width 2.0)) (+ (/ height 2.0) yp))
    ))

(define (rotateX x y z angle)
  (let ([y-new (-(* y (cos angle)) (* z (sin angle)))]
        [z-new (+ (* y (sin angle)) (* z (cos angle)))])
    (vector x y-new z-new)))

(define (rotateY x y z angle)
  (let ([x-new (+(* x (cos angle)) (* z (sin angle)))]
        [z-new (+ (* (- x) (sin angle)) (* z (cos angle)))])
    (vector x-new y z-new)))

(define angleX 0.1)
(define angleXspeed 0)
(define angleY 0.1)
(define angleYspeed 0)

(define deltaTime (current-milliseconds))
(define my-pen (new pen% [color "blue"] [width 2]))

(define (paint canvas dc)
  (send dc clear)
  (send dc set-pen
        my-pen)
  (for ([x (in-range (- WIDTH) WIDTH)])
    (let* ([y (* 20.0(sin (degrees->radians x)))]
           [xy (projection x y 1.0 WIDTH HEIGHT)])
      (send dc draw-point (vector-ref xy 0) (vector-ref xy 1)))))

;; Sadly we need to subclass fram or canvas to be able to get custom handler for keyboard
;; I guess that is due it uses wxwindows under the hood
(define my-frame%
  (class frame%
    (super-new)
    
    ;; Override on-subwindow-char    
    (define/override (on-subwindow-char receiver event)
      (let ([key-char (send event get-key-code)])
        (cond
          [(eqv? key-char #\q) (send this show #f) #t] ; Return #t to swallow the event
          [(eqv? key-char 'escape) (send this show #f) #t]
          
          ;; If it's not a key we want, return #f to let the event pass through normally
          [else #f])))))

(define frame (new my-frame% [label "Animation"] [width WIDTH] [height HEIGHT]))

(define mycanvas (new canvas%
                    [parent frame]
                    [paint-callback paint]
                   ))
                    
(define timer (new timer% [notify-callback
                           (lambda ()
                             ;(set! x (add1 x))
                             (set! deltaTime (- (current-milliseconds) deltaTime))
                             (set! angleXspeed (* angleX deltaTime))
                             (set! angleYspeed (* angleY deltaTime))
                             ;(printf "~a\n" (current-milliseconds))
                             (send mycanvas refresh)
                             ;(send timer start 16)
                             )]
                   [interval #f]
                   [just-once? #f]
                   ))

(send frame show #t)
(send mycanvas focus)
(send timer start 16) ; ~60 FPS in best case i guess...
