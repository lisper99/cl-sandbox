;; ----------------------------------------------------------------------------
;; Exported functions and macros besides actions and observers
;;
;; See the prelude for the special variables.
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
;; ----------------------------------------------------------------------------

(in-package :cl-sandbox)

;; ----------------------------------------------------------------------------
;; Performing a plan
;; ----------------------------------------------------------------------------

(defun execute-plan (fun &key
                     (test-pre *test-pre*)
                     (test-post *test-post*)
                     (test-diff *test-diff*)
                     (confine *confine*))
  "Sets *simulate* to nil, sets the passed special variables and calls
function fun without any arguments."
  (let ((*simulate* nil)
        (*test-pre* test-pre)
        (*test-post* test-post)
        (*test-diff* test-diff)
        (*confine* confine))
    (funcall fun)))

(defun simulate-plan (fun &key
                      (test-pre *test-pre*)
                      (test-post *test-post*)
                      (test-diff *test-diff*)
                      (confine *confine*))
  "Sets *simulate* to t, sets the passed special variables and calls
function fun without any arguments."
  (let ((*simulate* t)
        (*test-pre* test-pre)
        (*test-post* test-post)
        (*test-diff* test-diff)
        (*confine* confine))
    (funcall fun)))

;; ----------------------------------------------------------------------------
;; Macros with-access and with-sandbox
;; ----------------------------------------------------------------------------

(defmacro with-access (directories &body body)
  "Allows access to directory while evaluating body. Argument
directories is evaluated and must return a list with absolute
pathnames. An excepion is that a string is also allowed. The macro
turns this into a list expression."
  (let ((var (gensym))
        (lst (gensym))
        (lp (gensym)))
    `(let ((,var ,directories))
       (let ((,lst (if (listp ,var) ,var (list ,var))))
         (let ((*whitelist* (append ,lst *whitelist*)))
;;           (when (simulate-p)
;;             (loop for ,lp in ,lst
;;                do (ensure-directories-exist ,lp)
;;                do (populate-file-system-from-disk *sandbox* ,lp)))
           ,@body)))))

(defmacro with-sandbox ((directory &key
                                   (kind :both)
                                   (test-pre '*test-pre*)
                                   (test-post '*test-post*)
                                   (test-diff '*test-diff*)
                                   (confine '*confine*)
                                   (verbose t))
                        &body body)
  "Simulates and/or executes body with in a sandbox for
directory. keyword kind must be one of :simulate :execute or :both."
  (let ((dir (gensym "DIR"))
        (dirs (gensym "DIRS")))
    `(let ((,dir ,directory))
       (let ((,dirs (if (listp ,dir) ,dir (list ,dir))))
         (let ((*test-pre* ,test-pre)
               (*test-post* ,test-post)
               (*test-diff* ,test-diff)
               (*confine* ,confine)
               (*whitelist* (append ,dirs *whitelist*)))
           (run-sandbox-body ,dirs ,kind (lambda () ,@body) :verbose ,verbose))))))

(defun run-sandbox-body (dirs kind body &key verbose)
  "Helper for with-sandbox."
  ;;(unless (uiop:directory-pathname-p dir)
    ;;(error "Arg ~A of with-sandbox is not a directory." dir))
  (ecase kind
    (:simulate
     (when verbose
       (format t "~2%* Running simulation...~%"))
     (let ((*sandbox* (make-populated-sandbox dirs)))
       (simulate-plan body))
     (when verbose
       (format t "~2%Simulation successful.~%")))
    (:execute
     (execute-plan body))
    (:both
     (when verbose
       (format t "~2%* Running simulation...~%"))
     (let ((*sandbox* (make-populated-sandbox dirs)))
       (simulate-plan body))
     (when verbose
       (format t "~2%* Simulation successful, executing for real...~%"))
     (execute-plan body))))

