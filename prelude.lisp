;; ----------------------------------------------------------------------------
;; Prelude
;;
;; This file is loaded before anything else
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
;; Public globals
;; ----------------------------------------------------------------------------

(defvar *sandbox* nil
  "The current sandbox or nil.")

(defvar *simulate* nil
  "Should actions be simulated instead of being executed for real?. If
  not nil then *sandbox* must contain a sandbox.")

(defvar *test-pre* nil
  "Are pre conditions tested before an action is performed? See
  function perform-statement.")

(defvar *test-post* nil
  "Are post conditions tested after an action is performed? See
  function perform-statement.")

(defvar *test-diff* nil
  "Are file system effects tested after an action is performed? See
  function perform-statement.")

(defvar *whitelist* ()
  "List of pathnames where the file system functions have access. See
  functions validate-access and legal-path.")

(defvar *confine* t
  "Are actions confined to whitelisted directories? See function
  validate-access.")

;; ----------------------------------------------------------------------------
;; Other globals
;; ----------------------------------------------------------------------------

(defvar *opened-test-streams* ()
  "All streams that have been opened during a random testing
  run. Closed streams are not removed from the list. Gets reset for
  each test run. See function test-file-system-actions.")

(defvar *chosen-restart* nil
  "Restart chosen by the user when an error is caught by the debugger
  during testing. See function test-file-system-actions.")

(defvar *action-log* nil
  "A list of actions. Used in testing to store the current scenario.")

;; ----------------------------------------------------------------------------
;; Checking *simulate*
;; ----------------------------------------------------------------------------

(defun simulate-p ()
  "Should actions be simulated instead of executed for real?"
  (not (null *simulate*)))

(defun execute-p ()
  "Should actions be executed for real instead of simulated?"
  (null *simulate*))

;; ----------------------------------------------------------------------------
;; Whitelist
;; ----------------------------------------------------------------------------

(defun validate-access (path)
  "Does nothing unless *CONFINE* is non-nil. In that case it checks
access and throws a continuable error if not allowed. Called from
actions to confine access to whitelisted directories. See legal-path."
  (when *confine*
    (unless (legal-path path)
      (cerror "Continue anyway"
              "Illegal access to path ~A" path))))

(defun legal-path (pathspec)
  "Is pathspec on the whitelist *whitelist*? Helper for function
validate-access."
  (loop
     for dir in *whitelist*
     do (assert (uiop:directory-pathname-p dir))
     do (assert (uiop:absolute-pathname-p dir))
     thereis (subpathp* pathspec dir)))

;; ----------------------------------------------------------------------------
;; Wrapping non file system UIOP functions
;; ----------------------------------------------------------------------------

;; Is this allowed?
#+:wrapuiop
(loop
   for name in '(pathname-directory-pathname
                 directory-pathname-p)
   do (setf (fdefinition name)
            (fdefinition (intern (symbol-name name) :uiop))))

;; ----------------------------------------------------------------------------
;; Logical utilities
;; ----------------------------------------------------------------------------

(defun equiv (x y)
  "Are logical values x and y equivalent?"
  (equal (null x) (null y)))

(defun xor (x y)
  "Are logical values x and y not equivalent?"
  (equal (null x) (not (null y))))

(defmacro implies (antecedent consequent)
  "Does antecedent logically imply consequent? The consequent is only
evaluated when the antecedent is true."
  `(or (not ,antecedent) ,consequent))

;; ----------------------------------------------------------------------------
;; Randomness utilities
;; ----------------------------------------------------------------------------

(defun pick-weighted (choices)
  "Randomly chooses a value according to given weights. List choices
  must contain (weight value) pairs with weight a natural
  number (including zero) and value any value."
  (loop
     with num = (random (loop for (weight nil) in choices sum weight))
     for (weight choice) in choices
     sum weight into cumulative
     if (< num cumulative)
     return choice))

(defmacro weighted-choice (&rest choices)
  "Randomly chooses a value according to given weights. Forms choices
  must contain (weight value) pairs with weight a natural
  number (including zero) and value any form that evaluates to a
  value. Only the chosen form gets evaluated. For example
  in (weighted-choice (1 x) (1 y)) x or y gets evaluated.
  In (pick-weighted (list (list 1 x) (list 1 y))) x and y get
  evaluated."
  `(funcall
    (pick-weighted
     (list ,@(loop for (weight . body) in choices
                collect `(list ,weight (lambda () ,@body)))))))

(defun pick (sequence)
  "A uniform random picked element from the given
sequence. It is an error if sequence is empty."
  (if (< 0 (length sequence))
    (elt sequence (random (length sequence)))
    (error "No elements to pick from.")))

;; ----------------------------------------------------------------------------
;; Pathname utilities
;; ----------------------------------------------------------------------------

;; See uiop:with-pathname-defaults
;; See uiop:call-with-current-directory
;; See uiop:with-current-directory

(defun absolute-pathname (pathname)
  "Wrapper around uiop:ensure-absolute-pathname that only uses
*default-pathname-defaults*. Sandboxable. Note that
using (get-pathname-defaults) instead of *default-pathname-defaults*
would still be sandboxable and make the absolute pathname depend on
the current working directory. It seems CCL uses the working directory
if no *default-pathname-defaults* is given, but sbcl doesn't. Seems
safest not to depend on cwd and just use *default-pathname-defaults*
to get an absolute directory."
  (uiop:ensure-absolute-pathname pathname *default-pathname-defaults*))

(defun subpathp* (maybe-subpath base-pathname)
  "Wrapper around UIOP:SUBPATHP that can handle relative
pathnames. Sandboxable."
  (or
   (uiop:subpathp
    (absolute-pathname (pathname maybe-subpath))
    (absolute-pathname (pathname base-pathname)))
   (uiop:pathname-equal
    (absolute-pathname (pathname maybe-subpath))
    (absolute-pathname (pathname base-pathname)))))

(defun vary-file-type (file type)
  (merge-pathnames (make-pathname :name (pathname-name file) :type type) file))

;; ----------------------------------------------------------------------------
;; Copied from cl:with-open-file
;; ----------------------------------------------------------------------------

(defmacro with-open-file ((var filename . args) 
                          &body body
                          &aux (stream (gensym))(done (gensym)))
  "Use open to create a file stream to file named by filename. Filename is
the name of the file to be opened. Options are used as keyword arguments
to open."
  `(let (,stream ,done)
     (unwind-protect
       (multiple-value-prog1
         (let ((,var (setq ,stream (open ,filename ,@args))))
           ,@body)
         (setq ,done t))
       (when ,stream (close ,stream :abort (null ,done))))))
