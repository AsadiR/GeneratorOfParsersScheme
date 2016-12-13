

; генерирует словарь нетерминалов
(define (get-nonterms grammar)
  (define (rec grammar index)
    (cond ((equal? grammar '()) '())
          (else (cons (list (caar grammar) index)
                      (rec (cdr grammar) (+ index 1))))))
  (rec grammar 0))


;источник символов
(define (to-vector seq)
  (cond ((vector? seq) seq)
        ((list? seq) (list->vector seq))
        ((string? seq) (list->vector (string->list seq)))
        (else 'error)))

(define (make-source seq . xs)
  (if (null? xs)
      (cons 0 (cons #f (list (to-vector seq))))
      (cons 0 (cons (car xs) (list (to-vector seq))))))

(define (peek src)
  (if (< (car src) (vector-length (caddr src)))
      (vector-ref (caddr src) (car src))
      (cadr src)))

(define (next src)
  (define index (car src))
  (set-car! src (+ index 1))
  (if (< index (vector-length (caddr src)))
      (vector-ref (caddr src) index)
      (cadr src)))


;циклы через lambda, используются
;не удобно использовать при синтезе =(

(define (call f)
  ;(display "called\n")
  (apply f '()))

(define (call-all fs)
  (cond ((null? fs) '())
        (else (cons (call (car fs)) (call-all (cdr fs))))))



(define (do-while cond_f fs)
  ;(display "do-while\n")
  `( ,@(call-all fs) ,@(if (cond_f) (do-while cond_f fs) '())))

(define (while cond_f fs)
  (if (cond_f)
      `(,@(call-all fs) ,@(while cond_f fs))
      '()))

;тест для циклов
;(define gl -1)
;(define (minus) (set! gl (- gl 1)) gl)
;(define bl 2)
;(define (plus) (set! bl (+ bl 1)) bl)
;(call-all (list minus))
;(while (lambda () (> gl 0)) (list minus plus)) 


(define (parse% seq)
  (define src (make-source seq))
  (call-with-current-continuation
   (lambda (fail)
     ;грамматика для грамматик
     ;((grammar = rule+)
     ; (rule = <@NONTERM "=" multi_addendum> )
     ; (multi_addendum = addendum <"||" addendum>* )
     ; (addendum = factor+)
     ; (factor = "(" multi_addendum ")" op || ident op || )
     ; (op = "*" || "+" || "?" || )
     ; (ident  = @TERM || @NONTERM )))
     
     ; проверка first
     ; ТЕРМИНАЛЫ
     (define (term?)
       ;все что не является зарезервированными символами - терминалы
       (define cur (peek src))
       (and (not (equal? cur '$))
            (not (equal? cur '+))
            (not (equal? cur '*))
            (not (equal? cur '?))
            (not (equal? cur '=))
            (not (equal? cur '||))
            (not (equal? cur '<))
            (not (equal? cur '>))))
     
     (define (nonterm?)
       (define cur (peek src))
       ;(display cur) (newline)
       (and (list? cur)
            (equal? (car cur) '$nonterm)))
     
     (define (first? expected)
       (equal? (peek src) expected))
     
     ; НЕТЕРМИНАЛЫ
     (define (rule?)
       (first? '<))
     
     (define (factor?)
       (define cur (peek src))
       (or (first? '<)
           (nonterm?)
           (term?)))
     
     (define (ident?)
       (or (term?) (nonterm?)))
     
     
     ;проверка элемента в src
     (define (check condition)
       (if (condition)
           (list (next src))
           (fail `(error ,(next src)))))
     
     (define (check-op-br)
       (define sym (next src))
       (if (equal? sym '<)
           '()
           (fail `(error-op-br ,sym))))
     
     (define (check-cl-br)
       (define sym (next src))
       (if (equal? sym '>)
           '()
           (fail `(error-cl-br ,sym))))
     
     (define (check-alter)
       (define sym (next src))
       (if (equal? sym '||)
           '||
           (fail `(error-alter ,sym))))
     
     ; функции типа parse возвращают списки
     ; do-while и while возвращают списки списков
     ; (grammar = rule+)
     (define (parse-grammar)
       ;(display "grammar\n")
       (do-while rule? (list parse-rule)))
     
     ;(rule = @NONTERM "=" multi_addendum)
     (define (parse-rule)
       ;(display "rule\n")
       `(,@(check-op-br)
         ,@(check nonterm?)
         ,@(check (lambda () (first? '=)))
         ,(parse-multi-addendum)
         ,@(check-cl-br)))
     
     ;(multi_addendum = addendum ("||" addendum)*)
     (define (parse-multi-addendum)
       `(,(parse-addendum)
         ,@(while (lambda () (first? '||))
                  (list check-alter
                        parse-addendum))))
     
     ;(addendum = factor+)
     (define (parse-addendum)
       (do-while factor? (list parse-factor)))
     
     ; (factor = "(" multi_addendum ")" op || ident op || )
     (define (parse-factor)
       ; если операция пустая то множитель будет в двойных скобках
       (cond ((first? '<) `(,@(check-op-br) ,(parse-multi-addendum) ,@(check-cl-br) ,@(parse-op)))
             ((ident?)    `(,@(parse-ident) ,@(parse-op)))
             (else '(void))))
     
     ; (op = "*" || "+" || "?" || )
     (define (parse-op)
       (cond ((first? '*) (check (lambda () (first? '*))))
             ((first? '+) (check (lambda () (first? '+))))
             ((first? '?) (check (lambda () (first? '?))))
             (else '(void))))
     
     ; (ident  = @TERM || @NONTERM )))
     (define (parse-ident)
       (cond ((term?) (check term?))
             ((nonterm?) (check nonterm?))
             (else (fail 'error))))
     
     ; запуск и проверка последнего того, что проверена вся последовательностьы
     (let ((res (parse-grammar))
           (sym (next src)))
       (if sym `(error at-the-end: ,sym) res))
     )))


;lexical analyses
;() заменяются на <>
;нетерминалы упаковываются в ($nonterm ...)
(define (tokenize nonterms seql )
  (define (nonterm? sym)
    (assq sym nonterms))
  (define (term? sym)
    (not (assq sym nonterms)))
  
  (define (rec seql)
    (cond ((null? seql) '())
          ((list? (car seql))
           `(< ,@(rec (car seql)) > ,@(rec (cdr seql))))
          ((nonterm? (car seql))
           (cons `($nonterm ,(car seql)) (rec (cdr seql))))
          (else (cons (car seql) (rec (cdr seql))))))
  (rec seql))




;синтез
;генерируем определение функции parse
(define (generate ast)
  (define fset (gen-first-set ast '()))
  ;(display fset)
  
  (define (gen-grammar ast)
    (cond ((null? ast) '())
          (else (cons (gen-rule (car ast)) (gen-grammar (cdr ast))))))
  
  (define (symbol-append . ss)
    (define (rec ss)
      (cond ((null? ss) "")
            (else (string-append (symbol->string (car ss)) (rec (cdr ss))))))
    (string->symbol (rec ss)))
  
  (define (gen-rule rule)
    (define name (symbol-append 'parse- (cadar rule)))
    (define rp (caddr rule))
    `(define (,name) ,(gen-multi_addendum rp)))
  
  (define (gen-multi_addendum rp)
    (define empty-flag #f)
    (define (rec rp)
      (cond ((null? rp)
             `((else ,(if empty-flag 'void '(fail error-parse-ma)))))
            
            ((equal? (car rp) '((void))) (begin (set! empty-flag #t) (rec (cdr rp))))
            
            ((equal? (car rp) '||) (rec (cdr rp)))
            (else (cons `((member (peek src) (quote ,(get-first-set-a fset (car rp))))
                          ,(gen-addendum (car rp)))
                        (rec (cdr rp))))))
    (if (> (length rp) 1)
        `(cond ,@(rec rp))
        (gen-addendum (car rp))))
  
  (define (gen-addendum a)
    (define (rec a)
      ;(display a) (newline)
      (cond ((null? a) '())
            (else (cons (gen-factor (car a)) (rec (cdr a))))))
    `(list ,@(rec a)))
  
  
  ;условие для цикла
  (define (gen-lambda-f-c f)
    ;(display f) (newline)
    (define fset-f (get-first-set-f fset f))
    (lambda () (member (peek src) fset-f)))
  
  (define (gen-lambda-ma-c ma)
    ;(display ma) (newline)
    (define fset-ma (get-first-set-ma fset ma))
    (lambda () (member (peek src) fset-ma)))
  
  (define (gen-factor f)
    ;(display f) (newline)
    (cond
      ; Для РБНФ
      ;((and (factor+gen? f) (nonterm-gen? (car f)))
      ; `(do-while ,(gen-lambda-f-c (car f)) (list ,@(gen-factor (car f)))))
      ;((and (factor+gen? f) (list? (car f)))
      ; `(do-while ,(gen-lambda-ma-c (car f)) (list ,@(gen-lambda-list (gen-multi_addendum (car f))))))
      ;((factor+gen? f)
      ; `(do-while ,(gen-lambda-f-c (car f)) (list ,@(gen-factor (car f)))))
      
      ;((and (factor*gen? f) (nonterm-gen? (car f)))
      ; `(while ,(gen-lambda-f-c (car f)) (list ,@(gen-factor (car f)))))
      ;((and (factor*gen? f) (list? (car f)))
      ; `(while ,(gen-lambda-ma-c (car f)) (list ,@(gen-lambda-list (gen-multi_addendum (car f))))))
      ;((factor*gen? f)
      ; `(while ,(gen-lambda-f-c (car f)) (list ,@(gen-factor (car f)))))
      
      ;((and (factor?gen? f) (nonterm-gen? (car f)))
      ; `(if ,(gen-lambda-f-c (car f)) (list ,@(gen-factor (car f))) 'void))
      ;((and (factor?gen? f) (list? (car f)))
      ; `(if ,(gen-lambda-ma-c (car f)) (list ,@(gen-lambda-list (gen-multi_addendum (car f)))) 'void))
      ;((factor?gen? f)
      ; `(if ,(gen-lambda-f-c (car f)) (list ,@(gen-factor (car f))) 'void))
      
      ; Для БНФ
      ((factor-no-op-gen? f) (gen-factor (car f)))
      ((nonterm-gen? f) `( ,(symbol-append 'parse- (cadr f)) ))
      ((multi-addendum-gen? f) `( ,@(gen-multi_addendum (car f) )))
      ((void-gen? f) 'void)
      ((list? f) `(expected (quote ,(car f))))
      (else `(expected (quote ,f)))))
  
  `(define (parse seq)
     (define src (make-source seq))
     (define void 'void)
     (call-with-current-continuation
      (lambda (fail)
        (define (expected sym)
          (define cur (next src))
          (if (equal? sym cur)
              cur
              (fail `(error ,sym))))
        ,@(gen-grammar ast)
        (let ((res (,(symbol-append 'parse- (cadaar ast))))
              (sym (next src)))
          (if sym `(error at-the-end: ,sym) res))))))

;предикаты для генерации
(define (nonterm-gen? f)
  (and (list? f)
       ;(list? (car f))
       (equal? (car f) '$nonterm)))

(define (multi-addendum-gen? f)
  (and (list? f)
       (list? (car f))))

(define (factor+gen? f)
  (and (list? f)
       (= (length f) 2)
       (equal? (cadr f) '+)))

(define (factor*gen? f)
  (and (list? f)
       (= (length f) 2)
       (equal? (cadr f) '*)))

(define (factor?gen? f)
  (and (list? f)
       (= (length f) 2)
       (equal? (cadr f) '?)))

(define (factor-no-op-gen? f)
  (and (list? f)
       (= (length f) 2)
       (equal? (cadr f) 'void)))

(define (void-gen? f)
  (and (list? f)
       (equal? (car f) 'void)))  

;вычисляем first для нетерминальных символов грамматики
;оно используется в gen-multi_addendum и gen-factor
(define (gen-first-set ast fs)
  (define fset fs)
  (define smth-changed #f)
  
  (define (update-fset)
    (define (update-fset-rec rs)
      (cond ((null? rs) '())
            (else (cons (gen-first-rule (car rs))
                        (update-fset-rec (cdr rs))))))
    (set! smth-changed #f)
    (set! fset (update-fset-rec ast))
    ;(display fset) (newline)
    (if smth-changed
        (update-fset)))
  
  
  (define (gen-first-rule r)
    ;(display "gen-first-rule: ") (display r) (newline)
    (define name (cadar r))
    (define rp (caddr r))
    (define old-pair (assq (cadar r) fset))
    (list name (unique-append (if old-pair (cadr old-pair) '()) (gen-first-maddendum rp))))
  
  ;генерируем список уникальных символовы
  (define (unique-append old-set nes)
    ;(display "unique-appendm: ") (display old-set) (display nes) (newline)
    (cond ((null? nes) old-set)
          ((member (car nes) old-set) (unique-append old-set (cdr nes)))
          (else (begin
                  (set! smth-changed #t)
                  (unique-append (cons (car nes) old-set) (cdr nes))))))
  
  ;возвращают список символов, которые можно было бы добавить в fset 
  (define (gen-first-maddendum rp)
    ;(display "maddendum: ") (display rp) (newline)
    (cond ((null? rp) '())
          ((equal? (car rp) '||) (gen-first-maddendum (cdr rp)))
          (else (append (gen-first-addendum (car rp)) (gen-first-maddendum (cdr rp))))))
  
  
  (define (gen-first-addendum a)
    ;(display "addendum: ") (display a) (newline)
    (cond ((null? a) '()) 
          (else
           (let ((nes (gen-first-factor (car a))))
             (if (member 'void nes)
                 (append (gen-first-factor (car a)) (gen-first-addendum (cdr a)))
                 nes)))))
  
  (define (gen-first-factor f)
    ;(display "factor: ") (display f) (newline)
    (cond (;;; для РБНФ
           (and (factor+gen? f) (nonterm-gen? (car f)))
           (let ((old-set (assq (cadar f) fset)))
             (if old-set
                 (cadr old-set)
                 '())))
          ((and (factor+gen? f) (list? (car f))) (gen-first-maddendum (car f)))
          ((factor+gen? f) (list (car f)))
          
          
          ((and (factor*gen? f) (nonterm-gen? (car f))) (cons 'void (cadr (assq (cadar f)))))
          ((and (factor*gen? f) (list? (car f))) (cons 'void (gen-first-maddendum (car f))))
          ((factor*gen? f) (list (car f) 'void))
          
          ((and (factor?gen? f) (nonterm-gen? (car f))) (cons 'void (cadr (assq (cadar f)))))
          ((and (factor?gen? f) (list? (car f))) (cons 'void (gen-first-maddendum (car f))))
          ((factor?gen? f) (list (car f) 'void))
          
          ;для БНФ
          ((and (factor-no-op-gen? f) (nonterm-gen? (car f)))
           (let ((old-set (assq (cadar f) fset)))
             (if old-set
                 (cadr old-set)
                 '())))
          ((and (factor-no-op-gen? f) (list? (car f))) (gen-first-maddendum (car f)))
          ((factor-no-op-gen? f) (list (car f)))
          
          ((nonterm-gen? f)
           (let ((old-set (assq (cadr f) fset)))
             (if old-set
                 (cadr old-set)
                 '())))
          ((multi-addendum-gen? f) (gen-first-maddendum (car f)))
          ((void-gen? f) (list 'void))
          ((list? f) (car f))
          (else (list f))))
  
  (update-fset)
  fset)

(define (get-first-set-ma fset ma)
  (define temp-set (gen-first-set `((($nonterm @temp) = ,ma)) fset))
  (cadr (assq '@temp temp-set)))

(define (get-first-set-a fset a)
  (define temp-set (gen-first-set `((($nonterm @temp) = (,a))) fset))
  (cadr (assq '@temp temp-set)))

(define (get-first-set-f fset f)
  (define temp-set (gen-first-set `((($nonterm @temp) = ((,f)))) fset))
  (cadr (assq '@temp temp-set)))


; основная функция генератора парсеров
(define (gen-parser% seql)
  (define nonterms (get-nonterms seql))
  
  (let* ((nonterms (get-nonterms seql))
         (tokens (tokenize nonterms seql))
         (ast (parse% tokens)))
    (generate (parse% (tokenize nonterms seql)))))

; macros-wrapper
(define-syntax gen-parser
  (syntax-rules ()
    ((_ seq)
     (let ((def (gen-parser% (quote seq))))
       (eval def (interaction-environment))
       def))
    ))

;служебные символы || + * ? < > $nonterm =
;аксиома - нетерминал в левой части первого правила
;все остальные не составные типы являются терминаламиы

(gen-parser
 ((aRule = a bcRule)
  (bcRule = b bcRule || c bcRule || )))
(parse '(a b c b c))

(newline)

(gen-parser
 ((Expr = Num Addendum)
  (Num = 1 || 2 || 3 || 4 || 5 || 6 || 7 || 8 || 9 || 0)
  (Addendum = "+" Num Addendum || )))
(parse '(1 "+" 2 "+" 3))




;(define (my-while condition body)
;  (define cond-res (eval condition (interaction-environment)))  
;  (cond ((not cond-res) '()) 
;        (else (cons (eval body (interaction-environment))
;                    (my-while condition body)))))

;test
;(gen-parser
; ((grammar = rule+)
;  (rule = @NONTERM "=" multi_addendum)
;  (multi_addendum = addendum ("||" addendum)*)
;  (addendum = factor+)
;  (factor = "(" multi_addendum ")" op || ident op || )
;  (op = "*" || "+" || "?" || )
;  (ident  = @TERM || @NONTERM )))

;(gen-parser%
; '((grammar = rule +)
;   (rule = @NONTERM "=" multi_addendum)
;   (multi_addendum = addendum ("||" addendum)*)
;   (addendum = factor +)
;   (factor = "(" multi_addendum ")" op || ident op || )
;   (op = "*" || "+" || "?" || )
;   (ident  = @TERM || @NONTERM )))

;'((($nonterm grammar) = (((($nonterm rule) +))))
;  (($nonterm rule) = (((@nonterm void) ("=" void) (($nonterm multi_addendum) void))))
;  (($nonterm multi_addendum) = (((($nonterm addendum) void) (((("||" void) (($nonterm addendum) void))) *))))
;  (($nonterm addendum) = (((($nonterm factor) +))))
;  (($nonterm factor) = ((("(" void) (($nonterm multi_addendum) void) (")" void) (($nonterm op) void)) || ((($nonterm ident) void) (($nonterm op) void)) || ((void))))
;  (($nonterm op) = ((("*" void)) || (("+" void)) || (("?" void)) || ((void))))
;  (($nonterm ident) = (((@term void)) || ((@nonterm void)))))
