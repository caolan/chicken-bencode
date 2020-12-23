(cond-expand
 (chicken-4 (use test bencode))
 (chicken-5 (import test bencode (chicken condition) (chicken port))))

(define-syntax decode-test
  (syntax-rules ()
    ((_ description expected input)
     (test description
           expected
           (call-with-input-string input read-bencode)))))

(define-syntax decode-error-test
  (syntax-rules ()
    ((_ description expected input)
     (test description
           expected
           (handle-exceptions exn
             (get-condition-property exn 'exn 'message "no message supplied")
             (call-with-input-string input read-bencode))))))

(test-group "read-bencode: strings"
  (decode-test "basic string" "spam" "4:spam")
  (decode-test "empty string" "" "0:")
  (decode-test "string including ':' char" "foo:bar" "7:foo:bar")
  (decode-test "string including null char"
               (list->string '(#\f #\o #\o #\null))
               (list->string '(#\4 #\: #\f #\o #\o #\null)))
  (decode-error-test "string too short"
                     "unexpected end of input at char 6"
                     "4:foo")
  (decode-error-test "non digit in length prefix"
                     "unexpected character 'x' at char 2"
                     "1x2:hello world!")
  (decode-error-test "negative prefix"
                     "unexpected character '-' at char 1"
                     "-12:hello world!"))

(test-group "read-bencode: integers"
  (decode-test "integer" 42 "i42e")
  (decode-test "negative integer" -42 "i-42e")
  (decode-test "zero" 0 "i0e")
  (decode-error-test "empty integer"
                     "unexpected character 'e' at char 2"
                     "ie"))

(test-group "read-bencode: lists"
  (decode-test "list with strings and integers"
               #("hello" 42)
               "l5:helloi42ee")
  (decode-test "empty list"
               #()
               "le")
  (decode-test "nested lists"
               #("hello" 42 #(-1 0 1 2 3 "four") "five")
               "l5:helloi42eli-1ei0ei1ei2ei3e4:foure4:fivee")
  (decode-test "multiple empty lists"
               #(#() #() #(#(#(#()))))
               "llelelllleeeee")
  (decode-error-test "unclosed list segment"
                     "unexpected end of input at char 8"
                     "lllleee"))

(test-group "read-bencode: dictionaries"
  (decode-test "dictionary with strings and integers"
               '((age . 100) (name . "the dude"))
               "d3:agei100e4:name8:the dudee")
  (decode-test "nested dictionary"
               '((age . 100)
                 (info . ((email . "dude@dude.com") (number . 2488081446)))
                 (name . "the dude"))
               (string-append
                "d3:agei100e"
                "4:infod5:email13:dude@dude.com6:numberi2488081446ee"
                "4:name8:the dudee"))
  (decode-test "empty dictionary"
               '()
               "de")
  (decode-error-test "non-sorted dictionaries"
                     "key \"aaa\" starting at char 12 is not in lexographical order"
                     "d3:zzz3:bar3:aaai456ee")
  (decode-error-test "non-string keys should error"
                     "expected bencoded string as key at char 12"
                     "d3:aaa3:bari123ei456ee")
  (decode-error-test "unclosed dictionary segment"
                     "unexpected end of input at char 21"
                     "d3:aaad3:bbbd3:cccee"))

(test-group "read-bencode: mixed data from current-port"
  (test "dictionaries, lists, integers and strings"
        '((extra . #(#(((favorites . #()))) 500))
          (locations . #(((address . "484 street"))
                         ((address . "828 street")))))
        (with-input-from-string
            (string-append
             "d5:extralld9:favoritesleeei500ee"
             "9:locationsld7:address10:484 streeted"
             "7:address10:828 streeteee")
          read-bencode)))

(define-syntax encode-test
  (syntax-rules ()
    ((_ description expected input)
     (test description
           expected
           (call-with-output-string (cut write-bencode input <>))))))

(define-syntax encode-error-test
  (syntax-rules ()
    ((_ description expected input)
     (test description
           expected
           (handle-exceptions exn
             (get-condition-property exn 'exn 'message "no message supplied")
             (call-with-output-string (cut write-bencode input <>)))))))

(test-group "write-bencode: strings"
  (encode-test "basic string"
               "13:Hello, world!"
               "Hello, world!")
  (encode-test "empty string"
               "0:"
               ""))

(test-group "write-bencode: integers"
  (encode-test "integer" "i42e" 42)
  (encode-test "negative integer" "i-42e" -42)
  (encode-test "zero" "i0e" 0))

(test-group "write-bencode: lists"
  (encode-test "list with strings and integers"
               "l5:helloi42ee"
               #("hello" 42))
  (encode-test "empty list"
               "le"
               #())
  (encode-test "nested lists"
               "l5:helloi42eli-1ei0ei1ei2ei3e4:foure4:fivee"
               #("hello" 42 #(-1 0 1 2 3 "four") "five"))
  (encode-test "multiple empty lists"
               "llelelllleeeee"
               #(#() #() #(#(#(#()))))))

(test-group "write-bencode: dictionaries"
  (encode-test "dictionary with strings and integers"
               "d3:agei100e4:name8:the dudee"
               '((age . 100) (name . "the dude")))
  (encode-test "nested dictionary"
               (string-append
                "d3:agei100e"
                "4:infod5:email13:dude@dude.com6:numberi2488081446ee"
                "4:name8:the dudee")
               '((age . 100)
                 (info . ((email . "dude@dude.com") (number . 2488081446)))
                 (name . "the dude")))
  (encode-test "empty dictionary"
               "de"
               '())
  (encode-test "sort dictionary keys"
               "d7:integeri12345e6:string13:Hello, world!e"
               '((string . "Hello, world!")
                 (integer . 12345))))

(define-record testrecord x)
(define-record-printer (testrecord r port)
  (fprintf "<testrecord:~S>" (testrecord-x r)))

(test-group "write-bencode: invalid types"
  (encode-error-test "non-alist list"
                     "can only encode alist, vector, integer, or string"
                     '(1 2 3 4))
  (encode-error-test "non-integer number"
                     "can only encode alist, vector, integer, or string"
                     123.456)
  (encode-error-test "record"
                     "can only encode alist, vector, integer, or string"
                     (make-testrecord "foo"))
  (encode-error-test "symbol"
                     "can only encode alist, vector, integer, or string"
                     'asdf))

(test-group "working with strings"
  (test "string->bencode"
        #(1 2 "foo")
        (string->bencode "li1ei2e3:fooe"))
  (test "bencode->string"
        "li1ei2e3:fooe"
        (bencode->string #(1 2 "foo"))))

(test-exit)
