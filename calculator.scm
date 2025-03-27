(define (tokenize str)
  (let ((chars (string->list str)))
    (let loop ((chars chars) (result '()) (current '()) (type 'none))

      (define (add-current)
        (if (null? current)
            result
            (cons (list->string (reverse current)) result)))

      (cond
        ((null? chars)
         (reverse (add-current)))

        ((char-whitespace? (car chars))
         (loop (cdr chars) (add-current) '() 'none))

        (else
         (let ((c (car chars)))
           (cond
             ((or (char-alphabetic? c) (char=? c #\_))
              (case type
                ((none ident)
                 (loop (cdr chars) result (cons c current) 'ident))
                (else
                 (loop (cdr chars) (add-current) (list c) 'ident))))

             ((or (char-numeric? c) (char=? c #\.))
              (case type
                ((none number)
                 (loop (cdr chars) result (cons c current) 'number))
                (else
                 (loop (cdr chars) (add-current) (list c) 'number))))

             (else
              (loop (cdr chars)
                    (cons (string c) (add-current))
                    '()
                    'none)))))))))

(define (variable? token)
  (and (not (string->number token))
       (not (null? (string->list token)))
       (char-alphabetic? (string-ref token 0))))

(define (parse-infix tokens)
  (define (precedence op)
    (cond
      ((member op '("*" "/")) 2)
      ((member op '("+" "-")) 1)
      (else 0)))

  (define (operator? op)
    (member op '("+" "-" "*" "/")))

  (let loop ((tokens tokens) (output '()) (ops '()))
    (if (null? tokens)
        (append (reverse output) (reverse ops))
        (let ((token (car tokens)))
          (cond
            ((string->number token)
             (loop (cdr tokens) (cons (string->number token) output) ops))

            ((variable? token)
             (loop (cdr tokens) (cons (string->symbol token) output) ops))

            ((string=? token "(")
             (loop (cdr tokens) output (cons token ops)))

            ((string=? token ")")
             (let-values (((popped remaining-ops) (pop-until-paren ops)))
               (loop (cdr tokens) (append popped output) remaining-ops)))

            ((operator? token)
             (let ((op1 token))
               (let ((new-ops
                      (let drop ((current-ops ops))
                        (if (or (null? current-ops)
                                (string=? (car current-ops) "(")
                                (> (precedence (car current-ops)) (precedence op1)))
                            current-ops
                            (cons (car current-ops) (drop (cdr current-ops)))))))
                 (loop (cdr tokens) output (cons op1 new-ops)))))

            (else
             (error "Unknown token" token)))))))

(define (pop-until-paren ops)
  (let loop ((ops ops) (popped '()))
    (cond
      ((null? ops)
       (error "Mismatched parentheses"))

      ((string=? (car ops) "(")
       (values (reverse popped) (cdr ops)))

      (else
       (loop (cdr ops) (cons (car ops) popped))))))

(define (postfix-to-ast postfix)
  (let loop ((post postfix) (stack '()))
    (if (null? post)
        (if (= (length stack) 1)
            (car stack)
            (error "Invalid expression"))
        (let ((token (car post)))
          (cond
            ((number? token)
             (loop (cdr post) (cons token stack)))

            ((symbol? token)
             (loop (cdr post) (cons token stack)))

            ((string? token)
             (let ((op (string->symbol token)))
               (if (< (length stack) 2)
                   (error "Not enough operands for operator" op)
                   (let ((b (car stack))
                         (a (cadr stack)))
                     (loop (cdr post) (cons (list op a b) (cddr stack)))))))

            (else
             (error "Unexpected token in postfix" token)))))))

(define (parse-expr tokens)
  (postfix-to-ast (parse-infix tokens)))

(define (parse tokens)
  (cond
    ((and (>= (length tokens) 2)
          (variable? (car tokens))
          (string=? (cadr tokens) "="))
     (let ((var (string->symbol (car tokens)))
           (expr-tokens (cddr tokens)))
       (if (null? expr-tokens)
           (error "Missing expression after =")
           (list 'set! var (parse-expr expr-tokens)))))

    (else
     (parse-expr tokens))))

(define (eval-ast ast env)
  (cond
    ((number? ast)
     (values ast env))

    ((symbol? ast)
     (let ((pair (assq ast env)))
       (if pair
           (values (cdr pair) env)
           (error "Undefined variable" ast))))

    ((list? ast)
     (case (car ast)
       ((set!)
        (if (not (= (length ast) 3))
            (error "Invalid assignment syntax" ast)
            (let ((var (cadr ast)))
              (let*-values (((val new-env) (eval-ast (caddr ast) env)))
                (values val (cons (cons var val) new-env))))))
       (else
        (let*-values (((args env1) (eval-args (cdr ast) env)))
          (values (apply-op (car ast) args) env1)))))

    (else
     (error "Invalid AST node" ast))))

(define (eval-args ast env)

  (if (null? ast)
      (values '() env)
      (let*-values (((arg env1) (eval-ast (car ast) env))
                    ((args env2) (eval-args (cdr ast) env1)))
        (values (cons arg args) env2))))

(define (apply-op op args)

  (case op
    ((+)
     (if (not (= (length args) 2))
         (error "Incorrect number of arguments for +")
         (+ (car args) (cadr args))))

    ((-)
     (if (not (= (length args) 2))
         (error "Incorrect number of arguments for -")
         (- (car args) (cadr args))))

    ((*)
     (if (not (= (length args) 2))
         (error "Incorrect number of arguments for *")
         (* (car args) (cadr args))))

    ((/)
     (if (not (= (length args) 2))
         (error "Incorrect number of arguments for /")
         (if (zero? (cadr args))
             (error "Division by zero")
             (/ (car args) (cadr args)))))

    (else
     (error "Unknown operator" op))))

(define (main)
  (display "Simple Calculator in Scheme. Type 'quit' to exit.\n")
  (let loop ((env '()))
    (display "calc> ")
    (let ((input (read-line)))
      (cond
        ((or (eof-object? input) (string-ci=? input "quit"))
         (display "Goodbye!\n"))

        (else
         (guard (ex (else
                      (display "Error: ")
                      (display ex)
                      (newline)
                      (loop env)))
           (let ((tokens (tokenize input)))
             (if (null? tokens)
                 (loop env)
                 (let ((ast (parse tokens)))
                   (call-with-values
                       (lambda () (eval-ast ast env))
                     (lambda (result new-env)
                       (display result)
                       (newline)
                       (loop new-env))))))))))))

(main)
