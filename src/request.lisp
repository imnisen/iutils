(in-package :iutils)

;; TODO not finished yet. add post, get, and params

(defun request (url)
  (yason:parse
   (flexi-streams:octets-to-string
    (drakma:http-request url
                         :user-agent :FIREFOX
                         :accept "application/json"))))

(defun request-raw (url)
  (drakma:http-request url
                       :user-agent :FIREFOX
                       :accept "application/json"))
