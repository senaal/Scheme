; fatma sena alci
; 2019400045
; compiling: yes
; complete: yes
#lang racket

(provide (all-defined-out))

; 10 points
(define := (lambda (var value) (cons var(cons value '()))))

; 10 points
(define -- (lambda args (list 'let args)))

; 10 points
(define @ (lambda (bindings expr) (append bindings expr)))

; 20 points
(define split_at_delim (lambda (delim args)(remove '()
        (foldr (lambda (item tail)
           (cond
             [(eq? item delim)
               (cons '() tail)]
             [(cons (cons item (car tail)) (cdr tail))]))
         (list '()) args)
     )
  )
)
(define reverse_split_at_delim (lambda (delim args)
    (foldl (lambda (item tail)
           (cond
             [(eq? item delim)
               (cons '() tail)]
               [(cons (cons item (car tail)) (cdr tail))]))
         (list '()) args)
   )
)

; 30 point                     
(define out_of_list(lambda (expr)
                (if (pair? expr)(expr)(parse_expr expr))))
(define mapping_math(lambda (delim expr) (cons delim (map parse_expr(split_at_delim delim expr)))))
(define make_dash(lambda (func expr) (apply -- (map parse_expr(split_at_delim func expr)))))
(define make_eq(lambda (func expr) (apply := (map parse_expr(split_at_delim func expr)))))
(define func_at(lambda (expr)( @ (first(map parse_expr(split_at_delim '@ expr))) (cdr (map parse_expr(split_at_delim '@ expr))))))

(define parse_expr (lambda (expr)                    
    (cond
         [(member '+ expr) (mapping_math '+ expr)]
         [(member '* expr) (mapping_math '* expr)]
         [(member '@ expr)  (func_at expr)]
         [(member '-- expr) (make_dash '-- expr)]
         [(member ':= expr) (make_eq ':= expr)]
         [(list? (car expr))(parse_expr(car expr))]
         [(eq? (car expr) 'quote)(cadr expr)]
         [(car expr)]
         )
     )
)

; 20 point
(define eval_expr (lambda (expr) (eval (parse_expr expr))))
