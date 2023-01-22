(require data/heap)

(define-syntax compare-on-body 
    (syntax-rules ()
        [(compare-on-body _ _ res) res]
        [(compare-on-body x y (f lt eq) fs ...) 
            (let ((fx (f x)) (fy (f y))) 
                (or (lt fx fy) (and (eq fx fy) (compare-on-body x y fs ...))))]
    ))

(define-syntax compare-on 
    (syntax-rules () 
        ((compare-on fs ...) (lambda (x y) (compare-on-body x y fs ...)))))

(struct worker (idx l2r pick-old r2l put-new) #:transparent)
(struct task (time side worker) #:transparent)

(define (worker-delay worker) (+ (worker-l2r worker) (worker-r2l worker)))
 
(define worker-eff>=? (compare-on (worker-delay > =) (worker-idx > =) true))
(define task-time<=? (compare-on (task-time < =) true))

(define (heap-nonempty? h) (< 0 (heap-count h)))
(define (heap-take-min! h) (let ((x (heap-min h))) (heap-remove-min! h) x))

(define (find-crossing-time n _ time)
    (define workers (for/vector ((i (in-naturals)) (t time)) (apply worker (cons i t))))
    (define left (vector->heap worker-eff>=? workers))
    (define right (make-heap worker-eff>=?))
    (define tasks (make-heap task-time<=?))
    (define (unroll t) 
        (when (and (heap-nonempty? tasks) (>= t (task-time (heap-min tasks))))
            (match (heap-take-min! tasks)
                ((task _ 'left w) (heap-add! left w))
                ((task _ 'right w) (heap-add! right w)))
            (unroll t)
        )) 
    (let go ((new 0) (old n) (time 0) )
        (unroll time)
        (cond 
            ((heap-nonempty? right) 
                (define w (heap-take-min! right))
                (define t (+ time (worker-r2l w)))
                (heap-add! tasks (task (+ t (worker-put-new w)) 'left w))
                (go (add1 new) old t))
            ((and (> old 0) (heap-nonempty? left) )
                (define w (heap-take-min! left))
                (define t (+ time (worker-l2r w)))
                (heap-add! tasks (task (+ t (worker-pick-old w)) 'right w))
                (go new (sub1 old) t))
            ((= new n) time)
            (else (go new old (task-time (heap-min tasks)))))))