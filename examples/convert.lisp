;; ----------------------------------------------------------------------------
;; Convert example
;;
;; Copyright (c) 2019 Paul Griffioen
;; 
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(in-package :cl-sandbox)

;; ----------------------------------------------------------------------------
;;  convert-file-fun
;; ----------------------------------------------------------------------------

(defun convert-file-fun (in out)
  "Reads the contents of file IN, and if necessary converts it and
  writes the result in file OUT. Returns a non-nil value if the file
  was written, nil otherwise."
  (when (dirty-p in out)
    (with-open-file (s out
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
      (write-converted (read-contents in) s))
    t))

(defun write-converted (data stream)
  (format stream "data=~A" data))

(defun read-contents (filespec)
  (declare (ignore filespec))
  "dummy data")

(defun dirty-p (in out)
  (if (file-exists-p in)
      (or (not (file-exists-p out))
          (< (file-write-date out)
             (file-write-date in)))
      (error 'file-error :pathname in)))

;; ----------------------------------------------------------------------------
;; convert-file
;; ----------------------------------------------------------------------------

(defaction convert-file (in out)
  "Reads the contents of file in, converts it and writes the result in
  file out."

  (:pre-condition
   (and (file-exists-p in)
        (not (file-exists-p out))
        (not (directory-exists-p out))
        (directory-exists-p
         (uiop:pathname-directory-pathname out))))

  (:body
   (with-open-file (s out
                      :direction :output
                      :if-does-not-exist :create)
     (when (execute-p)
       (write-converted (read-contents in) s)))
   (values))

  (:post-condition
   (declare (ignore in))
   (lambda () (file-exists-p out)))

  (:difference
   (declare (ignore in))
    (lambda ()
      (lambda (removed added)
        (and (null removed)
             (equal added (list (truename out))))))))

;; ----------------------------------------------------------------------------
;; safe-convert-file
;; ----------------------------------------------------------------------------

(defaction safe-convert-file (in out)
  "Reads the contents of file in, converts it and writes the result in
  file out."

  (:pre-condition
   (and (file-exists-p in)
        (directory-exists-p
         (uiop:pathname-directory-pathname out))))

  (:body
   (handler-case
       (when (dirty-p in out)
         (with-open-file (s out
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede)
           (when (execute-p)
             (write-converted (read-contents in) s)))
         t)
     (file-error () nil)))

  (:post-condition
   (let ((needed-update (dirty-p in out)))
     (lambda (result)
       (and (implies result needed-update)
            (implies result (file-exists-p out))))))

  (:difference
   (declare (ignore in))
    (let ((existed (file-exists-p out)))
      (lambda (result)
        (lambda (removed added)
          (and (null removed)
               (if (and result (not existed))
                   (equal added (list (truename out)))
                   (null added))))))))
