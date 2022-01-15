(define E (lambda (x) (lambda (y) (lambda (a) (lambda (b) (if (= x y) a b))))))
(define L (lambda (x) (lambda (y) (lambda (a) (lambda (b) (if (< x y) a b))))))
(define K (lambda (x) (lambda (y) x)))
(define : (lambda (x) (lambda (y) (lambda (a) (lambda (b) (b x y))))))
