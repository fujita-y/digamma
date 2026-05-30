(import (core)
        (core test-lite))

(test-begin "core json")

(test-eval! (import (core) (core json)))

;; 1. Booleans and Null
(test-equal "boolean #t to json" "true"
  (generate-json-string #t))

(test-equal "boolean #f to json" "false"
  (generate-json-string #f))

(test-equal "null () to json" "null"
  (generate-json-string '()))

;; 2. Numbers
(test-equal "integer 20 to json" "20"
  (generate-json-string 20))

(test-equal "negative number to json" "-42"
  (generate-json-string -42))

(test-equal "float/real to json" "0.5"
  (generate-json-string 0.5))

;; 3. Strings & Escaping
(test-equal "simple string to json" "\"hello\""
  (generate-json-string "hello"))

(test-equal "escaped string to json" "\"hello \\\"world\\\"\""
  (generate-json-string "hello \"world\""))

(test-equal "control chars string to json" "\"line1\\nline2\\tindent\""
  (generate-json-string "line1\nline2\tindent"))

;; UTF-8 Strings
(test-equal "UTF-8 ASCII to json" "\"hello\""
  (generate-json-string "hello"))

(test-equal "UTF-8 latin extended to json" "\"café\""
  (generate-json-string "café"))

(test-equal "UTF-8 greek letters to json" "\"α β γ δ\""
  (generate-json-string "α β γ δ"))

(test-equal "UTF-8 emoji to json" "\"😀😁😂🤣\""
  (generate-json-string "😀😁😂🤣"))

(test-equal "UTF-8 mixed scripts to json" "\"Hello Привет こんにちは\""
  (generate-json-string "Hello Привет こんにちは"))

;; 4. Symbols
(test-equal "symbol to json string" "\"sym\""
  (generate-json-string 'sym))

;; 5. Vectors & Lists (JSON arrays)
(test-equal "empty vector to json" "[]"
  (generate-json-string '#()))

(test-equal "simple vector to json" "[1, 2, 3]"
  (generate-json-string '#(1 2 3)))

(test-equal "#[...] syntax vector to json" "[1, 2, 3]"
  (generate-json-string '#[1 2 3]))

(test-equal "nested vector to json" "[[1, 2], [3, 4]]"
  (generate-json-string '#(#(1 2) #(3 4))))

(test-equal "proper list raises error" raised-error
  (with-exception-handler
    (lambda (exc) 'raised-error)
    (lambda () (generate-json-string '(1 2 3)))))

;; 6. Association Lists (JSON objects)
(test-equal "alist with string keys" "{ \"a\": \"string\", \"b\": 20 }"
  (generate-json-string '(("a" . "string") ("b" . 20))))

(test-equal "alist containing vector" "{ \"v\": [1, 2, 3] }"
  (generate-json-string '(("v" . #[1 2 3]))))

(test-equal "nested alist" "{ \"user\": { \"name\": \"Bob\", \"age\": 30 } }"
  (generate-json-string '(("user" . (("name" . "Bob") ("age" . 30))))))

(test-equal "symbol key raises error" raised-error
  (with-exception-handler
    (lambda (exc) 'raised-error)
    (lambda () (generate-json-string '((a . 1))))))

;; 7. Parsing JSON Strings
(test-equal "parse null" ()
  (parse-json-string "null"))

(test-equal "parse true" #t
  (parse-json-string "true"))

(test-equal "parse false" #f
  (parse-json-string "false"))

(test-equal "parse integer" 20
  (parse-json-string "20"))

(test-equal "parse real" -42.5
  (parse-json-string "-42.5"))

(test-equal "parse string" "hello"
  (parse-json-string "\"hello\""))

(test-equal "parse escaped string" "hello \"world\""
  (parse-json-string "\"hello \\\"world\\\"\""))

(test-equal "parse unicode escape string" "hello \n world"
  (parse-json-string "\"hello \\u000a world\""))

;; UTF-8 Parsing Tests
(test-equal "parse UTF-8 latin extended" "café"
  (parse-json-string "\"café\""))

(test-equal "parse UTF-8 greek letters" "α β γ"
  (parse-json-string "\"α β γ\""))

(test-equal "parse UTF-8 emoji" "😀😁😂"
  (parse-json-string "\"😀😁😂\""))

(test-equal "parse UTF-8 mixed scripts" "Hello Привет こんにちは"
  (parse-json-string "\"Hello Привет こんにちは\""))

(test-equal "parse UTF-8 in object key" (("café" . 42))
  (parse-json-string "{ \"café\": 42 }"))

(test-equal "parse UTF-8 in object value" (("greeting" . "Здравствуй"))
  (parse-json-string "{ \"greeting\": \"Здравствуй\" }"))

(test-equal "parse UTF-8 in array" #("hello" "café" "😀")
  (parse-json-string "[\"hello\", \"café\", \"😀\"]"))

(test-equal "parse empty array" #()
  (parse-json-string "[]"))

(test-equal "parse simple array" #(1 2 3)
  (parse-json-string "[1, 2, 3]"))

(test-equal "parse nested array" #(#(1 2) #(3 4))
  (parse-json-string "[[1, 2], [3, 4]]"))

(test-equal "parse simple object" (("a" . "string") ("b" . 20))
  (parse-json-string "{ \"a\": \"string\", \"b\": 20 }"))

(test-equal "parse object containing array" (("v" . #(1 2 3)))
  (parse-json-string "{ \"v\": [1, 2, 3] }"))

(test-equal "parse nested object" (("user" . (("name" . "Bob") ("age" . 30))))
  (parse-json-string "{ \"user\": { \"name\": \"Bob\", \"age\": 30 } }"))

;; 8. Parsing Error Validation
(test-equal "incomplete object raises error" raised-error
  (with-exception-handler
    (lambda (exc) 'raised-error)
    (lambda () (parse-json-string "{ \"a\": 1"))))

(test-equal "incomplete array raises error" raised-error
  (with-exception-handler
    (lambda (exc) 'raised-error)
    (lambda () (parse-json-string "[1, 2"))))

(test-equal "invalid literal raises error" raised-error
  (with-exception-handler
    (lambda (exc) 'raised-error)
    (lambda () (parse-json-string "invalid"))))

(test-equal "trailing characters raise error" raised-error
  (with-exception-handler
    (lambda (exc) 'raised-error)
    (lambda () (parse-json-string "true trailing"))))

;; 9. Parsing JSON from Ports
(test-equal "parse true from port" #t
  (parse-json-string (open-string-input-port "true")))

(test-equal "parse simple array from port" #(1 2 3)
  (parse-json-string (open-string-input-port "[1, 2, 3]")))

(test-equal "parse nested object from port" (("user" . (("name" . "Bob") ("age" . 30))))
  (parse-json-string (open-string-input-port "{ \"user\": { \"name\": \"Bob\", \"age\": 30 } }")))

(test-equal "trailing characters from port raise error" raised-error
  (with-exception-handler
    (lambda (exc) 'raised-error)
    (lambda () (parse-json-string (open-string-input-port "true trailing")))))

(test-end)

(test-report)
(exit)
