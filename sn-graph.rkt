(module sn-graph racket
  (provide sn-consistent
           sn-empty
           sn-add-user
           sn-users
           sn-add-frndshp
           )

  ;; required libraries. (imported above)
  ;;(require racket/dict)
  ;;(require racket/set)

  
  ; Hard
  (define (sn-consistent p) #t)

  ;; graph
  ;; -> [a]
  ;; Easy (+0.5)
  (define sn-empty '())

  ;; Easy
  ;; [(k,v)] -> [u]
  (define (sn-users graph)
    (map car graph))

  
  ;; Hard
  ;; [(k,v)] u -> [(k,v)] | (u,{})
  (define (sn-add-user graph user)
    (if (assoc user graph)
        graph
        (cons (cons user '()) graph)))

  ;; Hard
  ;; [(k,v)]|(u1,f1)|(u2,f2) ->
  ;;  [(k,v)] | (u1,f1+{f2}) | (u2,f2+{f1})
  (define (sn-add-frndshp graph u1 u2)
    (map (lambda (entry)
           (let ((key (car entry))
                 (value (cdr entry)))
             (cond ((eq? key u1) (cons key (cons u2 value)))
                   ((eq? key u2) (cons key (cons u1 value)))
                   (else entry))))
         graph))

  )
