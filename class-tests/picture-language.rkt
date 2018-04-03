#lang racket/gui

; the vector data structure '(x . y)
(define make-vect cons) 
(define xcor-vect car)
(define ycor-vect cdr)
(define (scale-vect s v)
  (make-vect (* (xcor-vect v) s)
             (* (ycor-vect v) s)))
(define (neg-vect v)
  (scale-vect -1 v))
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (add-vect v1 (neg-vect v2)))

; the frame data structure '(origin edge1 edge2)
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2)) 
(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

; returns a transform that fits the given vector into the frame
; first, scale the frame's e1 + e2 (diagonal) according to the given vector,
; so that it represents the vector given in the frame;
; then add the origin vector of the frame to move it into the frame
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

; the segment data structure '(start . end)
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

; bring in the graphics library
(require graphics/graphics)
; initialize the graphics routines
(open-graphics)
; create a new window
(define vp (open-viewport "SICP Picture Language" 800 500))

; draw into the window created
(define draw (draw-viewport vp))
; clear the window
(define (clear) ((clear-viewport vp)))
; draw a line in the window in red color
; (line start-posn end-posn)
(define (line start end) ((draw-line vp) start end "red"))

; convert list of painters to a painter
; posn: position in the viewport
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (let ((start-coord-map ((frame-coord-map frame) (start-segment segment)))
             (end-coord-map ((frame-coord-map frame) (end-segment segment))))
         (line
          (make-posn (xcor-vect start-coord-map) (ycor-vect start-coord-map))
          (make-posn (xcor-vect end-coord-map) (ycor-vect end-coord-map)))))
     segment-list)))

; convert list of points in a polyline into list of segments
(define (points->segments points)
  (cond ((null? points) null)
        ((null? (cdr points))
         (error "Not enough number of points -- POINTS->SEGMENTS" points))
        ((null? (cddr points))
         (list (make-segment (car points) (cadr points))))
        (else
         (cons (make-segment (car points) (cadr points))
               (points->segments (cdr points))))))

; the wave painter in SICP
(define wave
  (let ((v1 (list (make-vect 0 26)
                  (make-vect 6 17)
                  (make-vect 12 25)
                  (make-vect 14 21)
                  (make-vect 10 0)))
        (v2 (list (make-vect 16 0)
                  (make-vect 21 13)
                  (make-vect 25 0)))
        (v3 (list (make-vect 31 0)
                  (make-vect 25 19)
                  (make-vect 41 6)))
        (v4 (list (make-vect 41 15)
                  (make-vect 31 27)
                  (make-vect 25 27)
                  (make-vect 27 35)
                  (make-vect 25 41)))
        (v5 (list (make-vect 16 41)
                  (make-vect 14 35)
                  (make-vect 16 27)
                  (make-vect 12 27)
                  (make-vect 6 25)
                  (make-vect 0 35)))
        (scale (lambda (list-of-vec)
                 (map (lambda (v)
                        (scale-vect 0.024 v)) list-of-vec))))
    (segments->painter
     (foldr append
            null
            (map (compose1 points->segments
                           scale) (list v1 v2 v3 v4 v5))))))

; straighten the frame, as the client area in Racket is upside-down
(define (make-straightened-frame origin edge1 edge2)
  (make-frame (add-vect (make-vect 0 500)
                        (make-vect (xcor-vect origin)
                                   (- (ycor-vect origin))))
              (make-vect (xcor-vect edge1)
                         (- (ycor-vect edge1)))
              (make-vect (xcor-vect edge2)
                         (- (ycor-vect edge2)))))

; transform the painter to paint in the given region in the frame,
; denoted by origin, corner1 and corner2 as in the unit frame,
; with absolute coordinates
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

; create a new painter with painter1 on the left and painter2 on the right
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

; create a new painter with painter1 on the top and painter2 on the bottom
(define (below painter2 painter1)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-up
           (transform-painter painter1
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0)))
          (paint-down
           (transform-painter painter2
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point)))
      (lambda (frame)
        (paint-up frame)
        (paint-down frame)))))

; flip the painter vertically
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
; flip the painter horizontally
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define frame-1
  (make-straightened-frame (make-vect 20 180)
                           (make-vect 300 0)
                           (make-vect 0 300)))

(define frame-2
  (make-straightened-frame (make-vect 500 100)
                           (make-vect 300 -50)
                           (make-vect -100 300)))

((square-limit wave 4) frame-1)
((beside wave
         (flip-vert wave))
 frame-2)