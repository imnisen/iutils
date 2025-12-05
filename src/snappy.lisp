(in-package :iutils)

(defun decode-snappy-file (filename &key verbose)
  "Decode an SSZ Snappy compressed file
   Parameters:
   - filename: path to SSZ Snappy file
   - verbose: whether to output detailed information
   Returns:
   - decompressed byte array"
  (when verbose
    (format t "~%Decoding file: ~a~%" filename))
  (let ((compressed-data (read-file-to-bytes filename)))
    (decode-snappy-bytes compressed-data :verbose verbose)))

;; snappy can't decode some ssz_snappy files (e.g., vec_bool_31_max/serialized.ssz_snappy)
;; try using https://github.com/flambard/thnappy
;; thnappy is Common Lisp bindings to Google's Snappy compression library.
;; so need to install snappy first: such as brew install snappy
(defun decode-snappy-bytes (compressed-data &key verbose)
  "Decode a Snappy compressed byte array
   Parameters:
   - compressed-data: compressed byte array
   - verbose: whether to output detailed information
   Returns:
   - decompressed byte array"
  (when verbose
    (format t "Compressed data size: ~a bytes~%" (length compressed-data))
    (format t "Compressed data (first 20 bytes): ~{~2,'0X~^ ~}~:[~;...~]~%"
            (subseq (coerce compressed-data 'list) 0
                    (min 20 (length compressed-data)))
            (> (length compressed-data) 20)))

  (let ((decompressed (thnappy:uncompress compressed-data)))
    (when verbose
      (format t "Decompression successful!~%")
      (format t "Decompressed size: ~a bytes~%" (length decompressed))
      (when (<= (length decompressed) 100)
        (format t "Decompressed data: ~{~2,'0X~^ ~}~%"
                (coerce decompressed 'list))))
    decompressed))
