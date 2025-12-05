(in-package :iutils)

(defmacro condition-transfer (excution-form condition transfer)
  `(if ,condition
       (,transfer ,excution-form)
       ,excution-form))
