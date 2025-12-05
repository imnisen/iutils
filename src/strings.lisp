(in-package :iutils)


(defun split-string-to-integer-list (a-string)
  "Spilt spaces separated strings to integer list"
  (loop :for (integer position)
        := (multiple-value-list
            (parse-integer a-string :start (or position 0) :junk-allowed t))
        :while integer
        :collect integer))

(defun split-string-to-char-list (a-string)
  "Split string to character list"
  (loop :for x :across a-string :collect x))

(defun split-string-to-string-list (a-string)
  "Split string to 'single character' string list(even the spaces)"
  (loop :for x :across a-string :collect (string x)))


(defun ensure-string (x)
  "Convert any value to its string representation.
  
  Arguments:
    x - Any Lisp value to convert to string
  
  Returns:
    A string representation of the input
  
  Examples:
    (ensure-string 42)              ; => \"42\"
    (ensure-string 'symbol)         ; => \"SYMBOL\"
    (ensure-string nil)             ; => \"NIL\"
    (ensure-string '(1 2 3))        ; => \"(1 2 3)\"
  
  Notes:
    - Uses FORMAT to convert values to strings"
  (format nil "~a" x))

(defun string->integer (a-string &key (radix 10))
  "Convert a string to an integer.
  
  Arguments:
    a-string - A string containing only numeric characters
  
  Keyword Arguments:
    radix - The base for parsing (2-36, default: 10)
  
  Returns:
    An integer parsed from the string
  
  Examples:
    (string->integer \"123\")              ; => 123
    (string->integer \"-456\")             ; => -456
    (string->integer \"0\")                ; => 0
    (string->integer \"FF\" :radix 16)     ; => 255
    (string->integer \"1010\" :radix 2)    ; => 10
  
  Errors:
    Signals an error if the string contains non-numeric characters
  
  Notes:
    - Does not allow trailing non-numeric characters
    - Radix can be any integer from 2 to 36"
  (nth-value 0 (parse-integer a-string :radix radix :junk-allowed nil)))

;; Depend on library parse-float
;; (defun string-to-float (a-string)
;;   (nth-value 0 (parse-float:parse-float a-string :radix 10 :junk-allowed nil)))


(defun string->number (a-string)
  "Convert a string to any numeric type (integer, float, ratio).
  
  Arguments:
    a-string - A string containing a numeric expression
  
  Returns:
    A number parsed from the string (integer, float, or ratio)
  
  Examples:
    (string->number \"123\")          ; => 123
    (string->number \"3.14\")         ; => 3.14
    (string->number \"1/2\")          ; => 1/2
    (string->number \"-4.5e2\")       ; => -450.0
  
  Notes:
    - Uses the Lisp reader to parse numbers
    - More flexible than string->integer
    - Can parse floats, ratios, and scientific notation"
  (with-input-from-string (in a-string)
    (read in)))

(defun conc-strings (&rest strings)
  "Concatenate multiple strings into one.
  
  Arguments:
    strings - Any number of strings to concatenate
  
  Returns:
    A single string containing all input strings joined together
  
  Examples:
    (conc-strings \"Hello\" \" \" \"World\")     ; => \"Hello World\"
    (conc-strings \"foo\" \"bar\" \"baz\")       ; => \"foobarbaz\"
    (conc-strings)                          ; => \"\"
  
  Notes:
    - Efficiently concatenates using FORMAT
    - Does not add separators between strings"
  (format nil "~{~a~}" strings))

(defun string+ (&rest strings)
  "Concatenate multiple strings (alias for conc-strings).
  
  Arguments:
    strings - Any number of strings to concatenate
  
  Returns:
    A single string containing all input strings joined together
  
  Examples:
    (string+ \"Hello\" \" \" \"World\")          ; => \"Hello World\"
    (string+ \"foo\" \"bar\")                  ; => \"foobar\"
    (string+ \"a\" \"b\" \"c\" \"d\")              ; => \"abcd\"
  
  Notes:
    - Alias for conc-strings
    - Provides a shorter name for string concatenation"
  (format nil "~{~a~}" strings))

(defun string-index (str i &key (charp nil) &aux (len (length str)))
  "Access a character in a string by index (supports negative indexing).
  
  Arguments:
    str - The string to index into
    i - The index (negative values count from end)
  
  Keyword Arguments:
    charp - If T, return a character; if NIL, return a string (default: NIL)
  
  Returns:
    The character at the given index, as a character or single-character string
  
  Examples:
    (string-index \"hello\" 0)                ; => \"h\"
    (string-index \"hello\" -1)               ; => \"o\"
    (string-index \"hello\" 1 :charp t)      ; => #\\e
    (string-index \"hello\" -2)               ; => \"l\"
  
  Errors:
    Signals an assertion error if index is out of bounds"
  (assert (or (and (>= i 0) (< i len))
              (and (< i 0) (< (+ len i) len))))
  (let ((s (aref str (if (>= i 0) i (+ len i)))))
    (if (not charp) (string s) s)))


(defun string-starts-with (prefix s)
  "Check if a string starts with a given prefix.
  
  Arguments:
    prefix - The prefix to check for
    s - The string to check
  
  Returns:
    T if the string starts with the prefix, NIL otherwise
  
  Examples:
    (string-starts-with \"Hello\" \"Hello World\")     ; => T
    (string-starts-with \"Hi\" \"Hello World\")        ; => NIL
    (string-starts-with \"\" \"anything\")             ; => T
    (string-starts-with \"Hello\" \"Hell\")            ; => NIL
  
  Notes:
    - Case-sensitive comparison
    - Empty prefix always returns T"
  (alexandria:starts-with-subseq prefix s))


(defun string-ends-with (suffix s)
  "Check if a string ends with a given suffix.
  
  Arguments:
    suffix - The suffix to check for
    s - The string to check
  
  Returns:
    T if the string ends with the suffix, NIL otherwise
  
  Examples:
    (string-ends-with \"World\" \"Hello World\")       ; => T
    (string-ends-with \"world\" \"Hello World\")       ; => NIL
    (string-ends-with \"\" \"anything\")               ; => T
    (string-ends-with \"Hello\" \"Hell\")              ; => NIL
  
  Notes:
    - Case-sensitive comparison
    - Empty suffix always returns T"
  (alexandria:ends-with-subseq suffix s))


(defun string-contains-p (substring string)
  "Check if a string contains a substring.
  
  Arguments:
    substring - The substring to search for
    string - The string to search in
  
  Returns:
    T if the substring is found, NIL otherwise
  
  Examples:
    (string-contains-p \"ell\" \"Hello World\")        ; => T
    (string-contains-p \"xyz\" \"Hello World\")        ; => NIL
    (string-contains-p \"\" \"anything\")              ; => T
    (string-contains-p \"Hello\" \"Hello\")            ; => T
  
  Notes:
    - Case-sensitive search
    - Empty substring always returns T"
  (not (null (search substring string))))


;;; Below need todo.

;;; Symbols and strings

;; Example
;; CL-USER> (mkstr pi " pieces of " 'pi)
;; "3.141592653589793d0 pieces of PI"
(defun mkstr (&rest args)
  "make a string out of args"
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))


(defun symb (&rest args)
  "Return a symbol making up of mkstr of args "
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
  (map 'list #'(lambda (c)
                 (intern (make-string 1 :initial-element c)))
       (symbol-name sym)))
