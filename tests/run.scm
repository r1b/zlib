
(import (chicken bitwise)
        (chicken file)
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

;; taken from python's zlib wrapper
(define expected "test")
(define expected-deflate "+I-.\x01\x00")
(define expected-zlib "x\xda+I-.\x01\x00\x04")
(define expected-gzip "\x1f\x8b\x08\x00\x00\x00\x00\x00\x02\x03+I-.\x01\x00\x0c~\x7f\xd8\x04\x00\x00\x00")

(define out-data "")

(define (compress str #!optional (wbits zlib/max-wbits))
  (call-with-output-string
   (lambda (out)
     (let ((out (open-zlib-compressed-output-port out window-bits: wbits)))
       (write-string str #f out)
       (close-output-port out)))))

(define (compress-deflate str) (compress str (- zlib/max-wbits)))
(define (compress-zlib str) (compress str zlib/max-wbits))
(define (compress-gzip str) (compress str (bitwise-ior zlib/max-wbits 16)))

(define (decompress str #!optional (wbits zlib/max-wbits))
  (call-with-output-string
   (lambda (out)
     (call-with-input-string str
       (lambda (in)
         (let ((in (open-zlib-compressed-input-port in window-bits: wbits)))
           (write-string (read-string #f in) #f out)))))))

(define (decompress-deflate str) (decompress str (- zlib/max-wbits)))
(define (decompress-zlib str) (decompress str zlib/max-wbits))
(define (decompress-gzip str) (decompress str (bitwise-ior zlib/max-wbits 16)))
(define (decompress-auto str) (decompress str (bitwise-ior zlib/max-wbits 32)))

;; test for idempotence

(test-begin "in memory")

(test-assert "deflate"
             (begin
               (set! out-data (compress-zlib in-data))
               (> (string-length out-data) #x10000)))
(test "inflate" sha1 (string->sha1sum (decompress out-data)))

(test "deflate (default)" expected (decompress (compress expected)))
(test "deflate (deflate)" expected (decompress-deflate (compress-deflate expected)))
(test "deflate (zlib)" expected (decompress-zlib (compress-zlib expected)))
(test "deflate (gzip)" expected (decompress-gzip (compress-gzip expected)))

(test "inflate (default)" expected (decompress expected-zlib))
(test "inflate (deflate)" expected (decompress-deflate expected-deflate))
(test "inflate (zlib)" expected (decompress-zlib expected-zlib))
(test "inflate (gzip)" expected (decompress-gzip expected-gzip))
(test "inflate (auto, zlib)" expected (decompress-auto expected-zlib))
(test "inflate (auto, gzip)" expected (decompress-auto expected-gzip))

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
