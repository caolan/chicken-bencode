(module bencode

;; exports
(read-bencode
 write-bencode
 bencode->string
 string->bencode)

(cond-expand
 (chicken-4
  (import chicken scheme)
  (use data-structures extras miscmacros ports vector-lib))
 (chicken-5
  (import
   scheme
   (chicken base)
   (chicken condition)
   (chicken port)
   (chicken io)
   (chicken format)
   (chicken sort)
   (chicken string)
   miscmacros
   vector-lib)))

(define discard-char read-char)

(define (digit? ch)
  (and (char>=? ch #\0) (char<=? ch #\9)))

(define (make-bencode-condition location message)
  (make-composite-condition
    (make-property-condition
      'exn
      'location location
      'message message)
    (make-property-condition 'bencode)))

(define (unexpected-end position)
  (abort
    (make-bencode-condition 'read-bencode
      (sprintf "unexpected end of input at char ~A" position))))

(define (unexpected-char ch position)
  (abort
    (make-bencode-condition 'read-bencode
      (sprintf "unexpected character '~A' at char ~A" ch position))))

(define (make-decode-fold iterator initial finally)
  (lambda (start)
    (discard-char) ;; beginning marker
    (let loop ((ch (peek-char))
               (acc initial)
               (position (+ start 1)))
      (cond
       ((eof-object? ch)
        (unexpected-end (+ position 1)))
       ((char=? ch #\e)
        (discard-char) ;; discard end marker #\e
        (values (finally acc) (+ position 1)))
       (else
        (receive (data new-position) (decode position)
          (loop (peek-char)
                (iterator data (+ position 1) new-position acc)
                new-position)))))))

(define (decode-int* delimiter negative start)
  (let loop ((chars '())
             (position (+ start 1))
             (ch (read-char)))
    (cond
     ((eof-object? ch)
      (unexpected-end position))
     ((char=? ch delimiter)
      (let ((n (string->number (list->string (reverse chars)))))
        (if n
            (values n position)
            (unexpected-char ch position))))
     ((or (digit? ch) (and negative (char=? ch #\-)))
      (loop (cons ch chars)
            (+ position 1)
            (read-char)))
     (else
      (unexpected-char ch position)))))

(define (decode-string position)
  (receive (len position) (decode-int* #\: #f position)
    (let ((str (make-string len)))
      (if (< (read-string! len str) len)
          (unexpected-end (+ position len))
          (values str (+ position len))))))

(define (decode-integer position)
  (discard-char) ;; discard #\i
  (decode-int* #\e #t (+ position 1)))

(define decode-list
  (make-decode-fold
   (lambda (x start end acc)
     (cons x acc))
   '()
   (compose list->vector reverse)))

(define (decode-dictionary position)
  (let ((key #f) (last-key #f))
    ((make-decode-fold
      (lambda (x start end acc)
        (if key
            (begin0
             (cons (cons (string->symbol key) x) acc)
             (set! last-key key)
             (set! key #f))
            (begin
              (unless (string? x)
                (abort
                  (make-bencode-condition 'read-bencode
                    (sprintf "expected bencoded string as key at char ~A"
                             start))))
              (when (and last-key (string<? x last-key))
                (abort
                  (make-bencode-condition 'read-bencode
                    (sprintf
                      "key ~S starting at char ~A is not in lexographical order"
                      x start))))
              (set! key x)
              acc)))
      '()
      reverse)
     position)))

(define (decode #!optional (position 0))
  (let ((ch (peek-char)))
    (cond
     ((digit? ch) (decode-string position))
     ((char=? ch #\i) (decode-integer position))
     ((char=? ch #\l) (decode-list position))
     ((char=? ch #\d) (decode-dictionary position))
     (else
      (unexpected-char ch (+ position 1))))))

(define (read-bencode #!optional (port (current-input-port)))
  (with-input-from-port port
    (lambda ()
      (and (not (eof-object? (peek-char)))
	   (receive (data position) (decode)
	     data)))))

(define (invalid-type x)
  (abort
    (make-bencode-condition 'write-bencode
      "can only encode alist, vector, integer, or string")))

(define (encode-string x)
  (display (conc (string-length x) ":" x)))

(define (encode-integer x)
  (display (conc "i" x "e")))

(define (encode-list x)
  (display "l")
  (vector-for-each (lambda (i y) (encode y)) x)
  (display "e"))

(define (encode-dictionary x)
  (display "d")
  (for-each
   (lambda (y)
     (encode (car y))
     (encode (cdr y)))
   (sort
    (map (lambda (p)
           (unless (pair? p) (invalid-type p))
           (unless (symbol? (car p)) (invalid-type (car p)))
           (cons (symbol->string (car p)) (cdr p)))
         x)
    (lambda (a b)
      (string<? (car a) (car b)))))
  (display "e"))

(define (encode x)
  (cond ((string? x) (encode-string x))
        ((integer? x) (encode-integer x))
        ((vector? x) (encode-list x))
        ((or (pair? x) (null? x)) (encode-dictionary x))
        (else (invalid-type x))))

(define (write-bencode data #!optional (port (current-output-port)))
  (with-output-to-port port (cut encode data)))

(define (string->bencode str)
  (call-with-input-string str read-bencode))

(define (bencode->string data)
  (call-with-output-string (cut write-bencode data <>)))

)
