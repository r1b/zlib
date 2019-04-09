
(import (chicken file)
        (chicken file posix)
        (chicken io)
        (chicken port)
        (chicken random)
        srfi-1
        srfi-13
        simple-sha1
        scheme
        test
        zlib)

(define in-data-size #x20000) ; 128K of data

(define in-data (string-unfold null? car cdr (list-tabulate in-data-size (lambda (i) (integer->char (pseudo-random-integer 255))))))
(define sha1 (string->sha1sum in-data))

(define out-data "")

;; test for idempotence

(test-begin "in memory")

(test-assert "deflate"
             (begin
               (set! out-data
                     (let ((string-port (open-output-string)))
                       (with-output-to-port (open-zlib-compressed-output-port string-port)
                         (lambda ()
                           (write-string in-data)
                           (close-output-port (current-output-port))
                           (get-output-string string-port)))))
               (> (string-length out-data) #x10000)))

(test "inflate" sha1
      (string->sha1sum
       (let ((string-port (open-output-string)))
         (with-input-from-port (open-zlib-compressed-input-port (open-input-string out-data))
           (lambda ()
             (write-string (read-string) #f string-port)
             (close-input-port (current-input-port))
             (get-output-string string-port))))))

(test-end)

(test-begin "file i/o")

(define in-file "")
(define deflate-file "")

(dynamic-wind
 (lambda () 
   (set! in-file (create-temporary-file))
   (set! deflate-file (create-temporary-file)))
 (lambda ()
   (let ((fd (file-open in-file (+ open/wronly open/append open/creat))))
     (do ((i 0 (add1 i)))
         ((>= i #x2000))
       (file-write fd (string-unfold null? car cdr (list-tabulate #x100 (lambda (i) (integer->char (pseudo-random-integer 255)))))))
     (file-close fd))
   (test-assert "initialize" (= (file-size in-file) (* #x100 #x2000)))
   (set! sha1 (sha1sum in-file))
   (with-input-from-file in-file
     (lambda () (with-output-to-port (open-zlib-compressed-output-port (open-output-file deflate-file))
             (lambda ()
               (write-string (read-string) #f)
               (close-output-port (current-output-port))
               (close-input-port (current-input-port))))))
   (test-assert "deflate" (> (file-size deflate-file) #x100000))
   (with-output-to-file in-file
     (lambda () (with-input-from-port (open-zlib-compressed-input-port (open-input-file deflate-file))
             (lambda ()
               (write-string (read-string) #f)
               (close-output-port (current-output-port))
               (close-input-port (current-input-port))))))
   (test "inflate" sha1 (sha1sum in-file)))
 (lambda ()
   (when (file-exists? in-file)
     (delete-file in-file))
   (when (file-exists? deflate-file)
     (delete-file deflate-file))))

(test-end)

(test-exit)
