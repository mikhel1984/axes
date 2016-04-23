;;;=========  Математические операции =========

;;;    a + b
(define (f+ a b)
   (cond 
      ((and (number? a) (number? b)) (+ a b))      
      ((and (number? a) (zero? a)) b)
      ((and (number? b) (zero? b)) a)
      (else (list '+ a b)))) 
      
;;;   a* b	       
(define (f* a b)
   (cond 
      ((and (number? a) (number? b)) (* a b))      	
      ((and (number? a) (= a 1)) b)
      ((and (number? a) (zero? a)) 0)
      ((and (number? b) (= b 1)) a)
      ((and (number? b) (zero? b)) 0)      
      (else (list '* a b))))
      
;;;   a - b	       
(define (f- a b)
   (cond 
      ((and (number? a) (number? b)) (- a b))      
      ((and (number? b) (zero? b)) a)      
      ((equal? a b) 0)
      (else (list '- a b))))
      
;;;   a / b	       
(define (f/ a b)
   (cond 
      ((eqv? b 0) 'ZERO_DIVIDE)
      ((and (number? a) (number? b)) (/ a b))      
      ((and (number? a) (zero? a)) 0)
      ((and (number? b) (= b 1)) a)      
      ((equal? a b) 1)
      (else (list '/ a b))))
      
;;;======== Сравнение математических выражений ========

;;; основной предикат
(define (mat-eqv? a b)
   (if (list? a)
      (if (list? b)
          (expr-eqv? a b)
	  #f)
      (eqv? a b))) 
      
;;;   выражение1  ??  выражение2	       
(define (expr-eqv? e1 e2)
   (if (eqv? (car e1) (car e2))     
      (if (direct-eqv? (cdr e1) (cdr e2))
         #t
	 (and (memq (car e1) '(+ *))
	          (reverse-eqv? (cdr e1) (cdr e2))))
      #f))
      
;;;   (a b) == (a b)       
(define (direct-eqv? a b)  
   (and (mat-eqv? (car a) (car b))
            (mat-eqv? (cadr a) (cadr b))))
	    
;;;   (a b) == (b a)     
(define (reverse-eqv? a b)   
   (and (mat-eqv? (car a) (cadr b))
            (mat-eqv? (car b) (cadr a))))
	    
;;;========= Представление переменных =================

;;; Для предствления выражения в виде многочлена и приведения
;;; подобных переменные преобразуются к группе
;;; (коэффициент переменная),
;;; например 3*k -> (3 k),
;;; а сумма записывается как список таких групп, т.п.
;;; 2*a + 3*b -> ((2 a) (3 b))
;;; Константа (число) записывается в виде (число 1)
;;;
;;;---------------------------------------------------------------------------------------------

;;; Исключение "групп" с нулевым коэффициентом		  
(define (grp-reduce lst)
   (let loop ((gr lst) (sum '()))
      (if (null? gr)
         sum
	 (if (zero? (caar gr))
	    (loop (cdr gr) sum)
	    (loop (cdr gr) (cons (car gr) sum))))))

;;; Добавление к списку новой "группы"
(define (grp-add x grp)
   (define (add-one rst i)
      (if (null? rst)
         (cons x grp)
	 (let ((val (car rst)))	    
	    (cond
	       ((mat-eqv? (cadr x) (cadr val))          
	          (set-car! (list-ref grp i) (+ (car x) (car val)))
		   (grp-reduce grp))
	       (else (add-one (cdr rst) (+ i 1)))))))
   (add-one grp 0))
   
;;; Сложение списков
(define (grp-add-lst grp1 grp2)
   (if (null? grp1)
      grp2
      (grp-add-lst (cdr grp1) (grp-add (car grp1) grp2))))

;;; Умножение "группы" на список
(define (grp-prod x grp)
   (map 
      (lambda (y)
         (list (* (car x) (car y)) (f* (cadr x) (cadr y))))
      grp))

;;; Перемножение списков
(define (grp-prod-lst grp1 grp2)
   (let loop ((g grp1) (res '()))
      (if (null? g) 
         res
	 (loop (cdr g)
            (if (null? res)
	       (grp-prod (car g) grp2)
	       (grp-add-lst (grp-prod (car g) grp2) res))))))

;;; Преобразование выражения в список
(define (to-grp expr)
   (cond
      ((number? expr) (cons (cons expr '(1)) '()))
      ((symbol? expr) (cons (list 1 expr) '()))
      (else
         (let ((a (cadr expr)) (b (caddr expr)))
            (case (car expr)
               ((+) (grp-add-lst (to-grp a) (to-grp b)))
	       ((-) (grp-add-lst (to-grp a) (grp-prod '(-1 1) (to-grp b))))
	       ((*) (grp-prod-lst (to-grp a) (to-grp b)))
	       ((/) (if (number? b) 
	                (grp-prod (cons (/ 1 b) '(1)) (to-grp a))
			(grp-prod (list 1 (f/ 1 b)) (to-grp a)))))))))

;;; Преобразование списка в выражение
(define (from-grp lst)   
   (let loop ((gr lst) (ans 0))
      (if (null? gr)
         ans	
	 ;(loop (cdr gr) (f+ ans (f* (caar gr) (cadar gr)))))))
	 (loop (cdr gr) (grp-sum-simp ans (car gr))))))

;;;  Корректировка знаков
(define (grp-sum-simp sum x)
   (if (negative? (car x))
      (f- sum (f* (- (car x)) (cadr x)))
      (f+ sum (f* (car x) (cadr x)))))

;;; Упрощение выражения
(define (simplify expr)
   (from-grp (to-grp expr)))
   
;;;========== Пересчёт выражения ============ 

;;; Вычисление выражения при новых условиях
(define (eval-for expr condition)
   (cond
      ((number? expr) expr)
      ((symbol? expr)
         (let ((val (assq expr condition)))
	    (if val (cadr val) expr)))     
      (else
         (let ((op (case (car expr) ((+) f+) ((-) f-) ((*) f*) ((/) f/))))	 
	    (op (eval-for (cadr expr) condition)
	          (eval-for (caddr expr) condition))))))
	    
   
	    


      

		 
	    
