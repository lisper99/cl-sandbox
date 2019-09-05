;; ----------------------------------------------------------------------------
;; Performing a statement
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
;; Performing an action
;; ----------------------------------------------------------------------------

(defun perform-statement (name body pre-condition post-condition difference)
  "Performs a n action."
  (let ((snapshot (when *test-diff* (take-snapshot))))
    (when *test-pre*
      (unless (funcall pre-condition)
        (cerror "Evaluate body anyway"
                (format nil "Pre condition for ~A fails" name))))
    (let ((results (multiple-value-list (funcall body))))
      (when *test-post*
        (unless (apply post-condition results)
          (cerror "Return as if successful"
                  (format nil
                          "Post condition for ~A fails for results (~{~A~^ ~})"
                          name
                          results))))
      (when *test-diff*
        (let ((new-snapshot (take-snapshot)))
          (unless (multiple-value-bind (same removed added)
                      (sorted-pathname-lists-equal snapshot new-snapshot)
                    (declare (ignore same))
                    (funcall (apply difference results)
                             removed added))
            (cerror "Return as if successful"
                    (format nil
                            "Difference condition for ~A fails for results (~{~A~^ ~}). Difference: ~A"
                            name
                            results
                            (with-output-to-string (*standard-output*)
                              (print-diff snapshot new-snapshot)))))))
      (values-list results))))

