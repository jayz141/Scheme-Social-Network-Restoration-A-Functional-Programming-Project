(module sn-social-network racket

  (provide 
   sn-ff-for
   sn-cmn-frnds-btwn
   sn-cmn-frnds
   sn-frnd-cnt
   sn-frndlst-user
   sn-unfrndlst-user)

  (require "sn-graph.rkt")
  (require "sn-utils.rkt")

  ;; Define max-key and min-key functions
  (define (max-key lst key-fn)
    (foldl (lambda (x max-so-far)
             (if (> (key-fn x) (key-fn max-so-far))
                 x
                 max-so-far))
           (first lst)
           (rest lst)))

  (define (min-key lst key-fn)
    (foldl (lambda (x min-so-far)
             (if (< (key-fn x) (key-fn min-so-far))
                 x
                 min-so-far))
           (first lst)
           (rest lst)))

  ;; social-network.
  ;; Easy
  ;; [(k,v)]| (u,vu) -> vu
  (define (sn-ff-for graph u1)
    (let ((result (assoc u1 graph)))
      (if result
          (cdr result)
          '())))

  ;; Medium
  ;; [(k,v)]|(u1,f1)|(u2,f2) ->
  ;; f2 & f3
  (define (sn-cmn-frnds-btwn graph u1 u2)
    (let ((u1-friends (sn-ff-for graph u1))
          (u2-friends (sn-ff-for graph u2)))
      (filter (lambda (x) (member x u2-friends)) u1-friends)))

  ;; Hard
  (define (sn-frnd-cnt graph)
    (map (lambda (entry)
           (cons (car entry) (length (cdr entry))))
         graph))

  ;; pre: length > 0 
  (define (sn-frndlst-user graph)
    (let ((frnd-cnt-list (sn-frnd-cnt graph)))
      (apply max-key frnd-cnt-list (list (lambda (x) (cdr x))))))

  ;; pre: length > 0
  (define (sn-unfrndlst-user graph)
    (let ((frnd-cnt-list (sn-frnd-cnt graph)))
      (apply min-key frnd-cnt-list (list (lambda (x) (cdr x))))))

  ;; this is for free. Do not mdify (ROM)
  (define (sn-cmn-frnds-ff graph u)
    (let*
        ([keys (sn-users graph)]
         [vals (map (lambda (key)                      (sn-cmn-frnds-btwn graph u key))                    keys)])
      (sn-dict-ks-vs keys vals)))

  ;; this is for free. Do not mdify (ROM)
  (define (sn-cmn-frnds graph)
    (let*
        ([keys (sn-users graph)]
         [vals (map (lambda (key)                      (sn-cmn-frnds-ff graph key))                    keys)])
      (sn-dict-ks-vs keys vals)))
  )
