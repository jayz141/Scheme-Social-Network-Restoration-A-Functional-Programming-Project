(module sn-utils racket

  (provide sn-dict-ks-vs
           sn-line->entry
           sn-list->dict)

  ;; utils
  ;; [k] [v] -> [(k,v)]
  ;; Easy
  (define (sn-dict-ks-vs ks vs)
    (map cons ks vs))

  ;; Medium
  ;; str -> (a,[a])
  (define (sn-line->entry ln)
    (let ((words (map string->symbol (string-split ln))))
      (cons (car words) (cdr words))))

  ;; [(a,b)] -> [(a,b)] 
  ;; Easy
  (define (sn-list->dict es)
    es))
