(in-package :cl-user)
(defpackage iutils
  (:use :cl :local-time)
  (:nicknames :iu)
  (:export
   ;; lists.lisp
   #:singlep
   #:last1
   #:append1
   #:nconc1
   #:ensure-list
   #:remove-if1
   #:group
   #:flatten
   #:flatten-1
   #:logner
   #:filter
   #:prunge
   #:shuffle
   #:list->1d-array
   #:list->hashtable
   #:alist->plist
   #:plist->alist
   #:safe-endp
   #:list->vector
   #:sequence->list
   #:lists-equalp

   ;; list search
   #:find2
   #:before
   #:after
   #:duplicate

   #:split-if
   #:most
   #:mostn
   #:best
   #:in
   #:not-in

   ;; list map
   #:map0-n
   #:map1-n
   #:mapa-b
   #:map->
   #:mappend
   #:mapcars
   #:rmapcar


   ;; print.lisp
   #:p-r
   #:print-and-return
   #:print-hash
   #:print-cons
   #:print-hash-and-return
   #:print-cons-and-return

   ;; bytes.lisp
   #:make-bytes-from-list
   #:make-initial-zeros-bytes
   #:*empty-byte*
   #:bytes->int
   #:int->bytes
   #:bytes->hexstr
   #:hexstr->bytes
   #:int->hexstr
   #:hexstr->int
   #:bytes->str
   #:str->bytes
   #:bytes->str2
   #:str->bytes2
   #:conc-bytes
   #:bytes-equal

   #:bytes->bitstr
   #:hexstr->bitstr

   #:list-of-bytes->bytes

   ;; bit-vector
   ;; #:make-initial-bits
   ;; #:bytes->bits
   ;; #:bits->bytes
   ;; #:int->bits
   ;; #:bits->int
   #:bit-vector-equalp

   ;; strings.lisp
   #:string+
   #:conc-strings
   #:string-index
   #:ensure-string
   #:string->integer
   #:string->number
   #:string-starts-with
   #:string-ends-with
   #:string-contains-p
   #:split-string-to-integer-list
   #:split-string-to-char-list
   #:split-string-to-string-list
   #:mkstr
   #:symb
   #:reread
   #:explode

   ;; sequences.lisp
   #:n-push

   ;; hashtable.lisp
   #:make-hashtable
   #:set-hashtable
   #:hashtable-key-to-list
   #:hashtable-value-to-list
   #:hashtable-to-list
   #:print-hashtable
   #:getmultihash
   #:must-gethash

   ;; arrays.lisp
   #:array-slice
   #:1d-array-to-list


   ;; time.lisp
   #:now-unix

   ;; numbers.lisp
   #:ensure-number
   #:number->string

   ;; io.lisp
   #:readlist
   #:prompt
   #:break-loop

   ;; others.lisp
   #:get-all-symbols

   ;; below is from local-time package symbols
   #:timestamp
   #:date
   #:time-of-day
   #:make-timestamp
   #:clone-timestamp
   #:day-of
   #:sec-of
   #:nsec-of
   #:timestamp<
   #:timestamp<=
   #:timestamp>
   #:timestamp>=
   #:timestamp=
   #:timestamp/=
   #:timestamp-maximum
   #:timestamp-minimum
   #:adjust-timestamp
   #:adjust-timestamp!
   #:timestamp-whole-year-difference
   #:days-in-month
   #:timestamp-
   #:timestamp+
   #:timestamp-difference
   #:timestamp-minimize-part
   #:timestamp-maximize-part
   #:with-decoded-timestamp
   #:decode-timestamp
   #:timestamp-century
   #:timestamp-day
   #:timestamp-day-of-week
   #:timestamp-decade
   #:timestamp-hour
   #:timestamp-microsecond
   #:timestamp-millennium
   #:timestamp-millisecond
   #:timestamp-minute
   #:timestamp-month
   #:timestamp-second
   #:timestamp-week
   #:timestamp-year
   #:parse-timestring
   #:format-timestring
   #:format-rfc1123-timestring
   #:to-rfc1123-timestring
   #:format-rfc3339-timestring
   #:to-rfc3339-timestring
   #:encode-timestamp
   #:parse-rfc3339-timestring
   #:universal-to-timestamp
   #:timestamp-to-universal
   #:unix-to-timestamp
   #:timestamp-to-unix
   #:timestamp-subtimezone
   #:define-timezone
   #:*default-timezone*
   #:*clock*
   #:leap-second-adjusted
   #:clock-now
   #:clock-today
   #:find-timezone-by-location-name
   #:reread-timezone-repository
   #:now
   #:today
   #:enable-read-macros
   #:+utc-zone+
   #:+gmt-zone+
   #:+month-names+
   #:+short-month-names+
   #:+day-names+
   #:+short-day-names+
   #:+seconds-per-day+
   #:+seconds-per-hour+
   #:+seconds-per-minute+
   #:+minutes-per-day+
   #:+minutes-per-hour+
   #:+hours-per-day+
   #:+days-per-week+
   #:+months-per-year+
   #:+iso-8601-format+
   #:+iso-8601-date-format+
   #:+iso-8601-time-format+
   #:+rfc3339-format+
   #:+rfc3339-format/date-only+
   #:+asctime-format+
   #:+rfc-1123-format+
   #:+iso-week-date-format+
   #:astronomical-julian-date
   #:modified-julian-date
   #:astronomical-modified-julian-date

   ;; crypto.lisp
   #:sha256
   #:sha256-from-str
   #:sha1
   #:ripemd160
   #:hash256
   #:hash160
   #:random-bytes

   ;; stream.lisp
   #:bytes->input-stream
   #:read-all-bytes
   #:read-exact

   ;; boolean.lisp
   #:ensure-bool

   ;; file.lisp
   #:read-file-to-bytes

   ;; snappy.lisp
   #:decode-snappy-bytes
   #:decode-snappy-file

   ;; yaml.lisp
   #:read-yaml-file
   ))
