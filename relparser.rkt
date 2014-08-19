#lang racket
(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

(define-tokens rle-number (number))
(define-empty-tokens rle-size (xeq yeq rule))
(define-empty-tokens rle-actions (on off endl endp eof))

(define rle-lex
  (lexer
   [(:: "#" (:* (:- any-char "\n")) "\n") (rle-lex input-port)]
   ["\n" (rle-lex input-port)]
   [(:+ numeric) (token-number (string->number lexeme))]
   ["o" (token-on)]
   ["b" (token-off)]
   ["$" (token-endl)]
   ["!" (token-endp)]
   ["x = " (token-xeq)]
   [", y = " (token-yeq)]
   [", rule = B3/S23\n" (token-rule)]
   [(eof) (token-eof)]))

(define-struct command (repetitions action) #:transparent)
(define-struct size (x y) #:transparent)
(define-struct program (size commands) #:transparent)

(define rle-parse
  (parser
   (grammar
    (program
     ((size-decl commands) (program $1 $2)))
    (size-decl
     ((xeq number yeq number rule) (size $2 $4)))
    (commands
     ((command commands) (cons $1 $2))
     ((command endp) (list $1 (command 1 (token-endl)) 'endp)))
    (command
     ((number action) (command $1 $2))
     ((action) (command 1 $1)))
    (action
     ((off) (token-off))
     ((on) (token-on))
     ((endl) (token-endl))))
   (error void)
   (tokens rle-number rle-actions rle-size)
   (end eof)
   (start program)))

(define (generate-binary commands line out)
  (match (first commands)
    [(command n (and action (or 'on 'off)))
     (define new-line
       (append
        (make-list n (match action ['on 1] ['off 0]))
        line))
     (generate-binary (rest commands) new-line out)]
    [(or (command n 'endl))
     (define line-finished 
       (cons
        (reverse (append
                  (make-list (- 128 (length line)) 0)
                  line))
        out))
     (define new-out
       (append
        (make-list (- n 1) (make-list 128 0))
        line-finished))
     (generate-binary (rest commands) '() new-out)]
    ['endp
     (reverse (append
               (make-list (- 64 (length out)) (make-list 128 0))
               out))]))

(define (bin->hex bin)
  (define chars #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F))
  (define powers-of-2 (reverse (map (curry expt 2) (range 4))))
  (list->string
   (map 
    (lambda (b) 
      (vector-ref chars (apply + (map * b powers-of-2))))
    (list (take bin 4) (drop bin 4)))))

(define (split-8 l out)
  (cond
    [(null? l) (reverse out)]
    [else (split-8 (drop l 8) (cons (take l 8) out))]))

(define (generate-hex binary)
  (define bytes (split-8 (flatten binary) '()))
  (string-join (map bin->hex bytes) "\n"))

(define (generate p)
  (match p
    [(program (size x y) commands)
     (if (or (> x 128) (> y 64))
         (error "configuration is too large.")
         (generate-hex (generate-binary commands '() '())))]))

(displayln (generate (rle-parse (lambda () (rle-lex (current-input-port))))))