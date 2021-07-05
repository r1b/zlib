;; Copyright (C) 2011 by Joseph Gay
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
;;
;; Author: Joseph Gay, <gilleylen [at] gmail [dot] com>

;; Bindings for zlib, see http://www.zlib.net for zlib docs

;; zlib: Copyright (C) 1995-2010 Jean-loup Gailly and Mark Adler
;; see http://zlib.net/zlib_license.html

;; https://zlib.net/zlib_how.html

(module zlib

(open-zlib-compressed-input-port
 open-zlib-compressed-output-port)

(import (chicken base)
        (chicken condition)
        (chicken io)
        (chicken foreign)
        (chicken port)
        foreigners
        miscmacros
        scheme)

#>
#include <zlib.h>
<#

(define *buffer-size* (* 16 1024)) ; 16KiB

(define-foreign-variable Z_OK int)
(define-foreign-variable Z_NULL int)
(define-foreign-variable Z_STREAM_END int)
(define-foreign-variable Z_NO_FLUSH int)
(define-foreign-variable Z_STREAM_ERROR int)
(define-foreign-variable Z_NEED_DICT int)
(define-foreign-variable Z_DATA_ERROR int)
(define-foreign-variable Z_MEM_ERROR int)
(define-foreign-variable Z_FINISH int)
(define-foreign-variable Z_NO_COMPRESSION int)
(define-foreign-variable Z_BEST_SPEED int)
(define-foreign-variable Z_BEST_COMPRESSION int)
(define-foreign-variable Z_DEFAULT_COMPRESSION int)
(define-foreign-variable Z_DEFLATED int)
(define-foreign-variable Z_DEFAULT_STRATEGY int)
(define-foreign-variable MAX_WBITS int)
(define-foreign-variable MAX_MEM_LEVEL int)

(define-foreign-record-type (z-stream "z_stream")
  (constructor: make-z-stream)
  (destructor: free-z-stream)
  ((c-pointer char) next_in z-stream-next-in z-stream-next-in-set!)
  (unsigned-integer avail_in z-stream-avail-in z-stream-avail-in-set!)
  (unsigned-long total_in z-stream-total-in)
  ((c-pointer char) next_out z-stream-next-out z-stream-next-out-set!)
  (unsigned-integer avail_out z-stream-avail-out z-stream-avail-out-set!)
  (unsigned-long total_out z-stream-total-out)
  (c-string msg z-stream-msg)
  ((c-pointer (struct "internal_state")) state state)
  (c-pointer zalloc z-alloc z-stream-z-alloc-set!)
  (c-pointer zfree z-free z-stream-z-free-set!)
  (c-pointer opaque z-stream-opaque z-stream-opaque-set!)
  (integer data_type z-stream-data-type)
  (unsigned-long adler z-stream-adler)
  (unsigned-long reserved reserved))

(define inflate-init (foreign-lambda int "inflateInit2" z-stream int))
(define inflate (foreign-lambda int "inflate" z-stream int))
(define inflate-end (foreign-lambda void "inflateEnd" z-stream))

(define (z-abort type)
  (abort (make-property-condition 'z-error 'type type)))

(define (open-zlib-compressed-input-port input-port
                                         #!key (window-bits (- MAX_WBITS)))
  (let ((zlib-last-ret #f)
        (zlib-stream (make-z-stream))
        (zlib-input-buffer (make-string *buffer-size*))
        (zlib-output-buffer (make-string *buffer-size*))
        (port-bytes-available "")
        (port-position 0)
        (port-eof? #f)
	(total-decompressed-bytes 0))
    (z-stream-z-alloc-set! zlib-stream #f)
    (z-stream-z-free-set! zlib-stream #f)
    (z-stream-opaque-set! zlib-stream #f)
    (z-stream-avail-in-set! zlib-stream 0)
    (z-stream-next-in-set! zlib-stream #f)
    (set! zlib-last-ret (inflate-init zlib-stream window-bits))
    (if (not (= Z_OK zlib-last-ret)) (z-abort zlib-last-ret)
        (make-input-port
	 ; read-char
         (lambda ()
           (when (>= port-position (string-length port-bytes-available))
             (begin
               (set! port-position 0)
               (set! port-bytes-available "")
               (z-stream-avail-in-set! zlib-stream
				       (read-string! *buffer-size*
						     zlib-input-buffer
						     input-port))
               (if (or (= 0 (z-stream-avail-in zlib-stream))
                       (= Z_STREAM_END zlib-last-ret))
                   (begin (inflate-end zlib-stream) (set! port-eof? #t))
                   (begin
                     (z-stream-next-in-set! zlib-stream #$zlib-input-buffer)
                     (z-stream-avail-out-set! zlib-stream 0)
                     (while (= 0 (z-stream-avail-out zlib-stream))
                       (z-stream-avail-out-set! zlib-stream *buffer-size*)
                       (z-stream-next-out-set! zlib-stream #$zlib-output-buffer)
                       (set! zlib-last-ret (inflate zlib-stream Z_NO_FLUSH))
                       (assert (not (= Z_STREAM_ERROR zlib-last-ret)) "state clobbered")
                       (when (or (= Z_NEED_DICT zlib-last-ret)
                                 (= Z_DATA_ERROR zlib-last-ret)
                                 (= Z_MEM_ERROR zlib-last-ret))
                         (set! zlib-last-ret (if (= Z_NEED_DICT zlib-last-ret)
						 Z_DATA_ERROR
						 zlib-last-ret))
                         (inflate-end zlib-stream)
                         (z-abort zlib-last-ret))
		       (let* ((num-decompressed-bytes (- *buffer-size*
							  (z-stream-avail-out zlib-stream)))
			      (decompressed-bytes (substring zlib-output-buffer
						       0
						       num-decompressed-bytes)))
			   (set! port-bytes-available
			     (string-append port-bytes-available decompressed-bytes))))))))
           (if port-eof? #!eof
	       (let ((last-port-position port-position))
		 (begin 
		   (set! port-position (add1 last-port-position))
		   (string-ref port-bytes-available last-port-position)))))
	 ; char-ready?
         (lambda ()
           (not port-eof?))
	 ; close
         (lambda ()
           (unless (= zlib-last-ret Z_STREAM_END)
             (warning "~A\n" "not finished with inflate"))
           (unless port-eof? ; free up memory
             (inflate-end zlib-stream)))))))

(define deflate-init (foreign-lambda int "deflateInit2" z-stream int int int int int))
(define deflate (foreign-lambda int "deflate" z-stream int))
(define deflate-end (foreign-lambda void "deflateEnd" z-stream))

(define (open-zlib-compressed-output-port output-port
                                          #!key (level Z_DEFAULT_COMPRESSION)
                                                (method Z_DEFLATED)
                                                (window-bits (- MAX_WBITS))
                                                (mem-level MAX_MEM_LEVEL)
                                                (strategy Z_DEFAULT_STRATEGY))
  (let ((zlib-last-ret #f)
        (zlib-stream (make-z-stream))
        (zlib-flush Z_NO_FLUSH)
        (zlib-output-buffer (make-string *buffer-size*))
        (port-bytes-collected ""))
    (z-stream-z-alloc-set! zlib-stream #f)
    (z-stream-z-free-set! zlib-stream #f)
    (z-stream-opaque-set! zlib-stream #f)
    (assert (<= mem-level MAX_MEM_LEVEL) (error "invalid mem-level"))
    (set! zlib-last-ret (deflate-init zlib-stream level method window-bits mem-level strategy))
    (define (write-collected)
      (let ((avail-in (string-length port-bytes-collected)))
        (z-stream-avail-in-set! zlib-stream avail-in)
        (unless (= 0 avail-in)
          (z-stream-next-in-set! zlib-stream #$port-bytes-collected))
        (z-stream-avail-out-set! zlib-stream 0)
        (while (= 0 (z-stream-avail-out zlib-stream))
          (z-stream-avail-out-set! zlib-stream *buffer-size*)
          (z-stream-next-out-set! zlib-stream #$zlib-output-buffer)
          (set! zlib-last-ret (deflate zlib-stream zlib-flush))
          (assert (not (= Z_STREAM_ERROR zlib-last-ret)) "state clobbered")
	  (let* ((num-compressed-bytes (- *buffer-size* (z-stream-avail-out zlib-stream)))
		 (compressed-bytes (substring zlib-output-buffer
					     0
					     num-compressed-bytes)))
	    (write-string compressed-bytes #f output-port)))
        (assert (= 0 (z-stream-avail-in zlib-stream)) "could not process all input")
        (set! port-bytes-collected "")))
    (if (not (= Z_OK zlib-last-ret)) (z-abort zlib-last-ret)
        (make-output-port
	 ; write
         (lambda (bytes-written)
           (set! port-bytes-collected (string-append port-bytes-collected bytes-written))
           (when (>= (string-length port-bytes-collected) *buffer-size*)
             (write-collected)))
	 ; close
         (lambda ()
           (when (port-closed? output-port)
	     (error "could not finish deflate, destination port closed"))
           (set! zlib-flush Z_FINISH)
           (write-collected)
           (assert (= zlib-last-ret Z_STREAM_END) "could not write stream end")
           (deflate-end zlib-stream)
           (flush-output output-port))))))

)
