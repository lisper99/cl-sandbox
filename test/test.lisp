;; ----------------------------------------------------------------------------
;; Automated tests
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
;; Testing
;; ----------------------------------------------------------------------------

(defun test-all (directory)
  "Runs all tests in the cl-sandbox package."
  (let ((*default-pathname-defaults* (pathname directory)))
    (when t
      (when t
        (test-copy-file-system directory)
        (test-mirror-on-disk directory))
      (when nil
        (test-file-system-observers directory :mode :simulate))
      (when t
        (test-file-system-actions directory :mode :simulate :target 100)))
    (when t
      (test-file-system-actions directory :mode :execute :target 1000))))

;; ----------------------------------------------------------------------------
;; test-copy-file-system
;; ----------------------------------------------------------------------------

(defun test-copy-file-system (directory)
  "Copies a random file system and checks that the result is equal."
  (format t "~2%* Testing in-memory copy file system action with ~A" directory)
  (let ((*whitelist* (list directory)))
    (let* ((fs (random-sandbox directory))
           (copy (copy-sandbox fs)))
      (multiple-value-bind (okay)
          (file-system-equal fs copy)
        (if okay
            (format t "~2%Test result okay")
            (cerror "Continue with tests"
                    "Test copy file system fails"))))))

;; ----------------------------------------------------------------------------
;; test-mirror-on-disk
;; ----------------------------------------------------------------------------

(defun test-mirror-on-disk (directory)
  "Creates a mirror directory in directory, writes a random file
system in it, reloads it, and checks that the result is equal to the
original."
  (let ((dir (fresh-test-dir "mirror-test/mirror" :directory directory)))
    (let ((*whitelist* (list dir)))
      (format t "~2%* Mirroring into ~A" dir)
      (ensure-directories-exist dir)
      (let ((random-fs (random-sandbox dir)))
        (mirror-file-system-to-disk random-fs)
        (format t "~2%Mirror written on disk, reloading...")
        (let ((reloaded (make-populated-sandbox dir)))
          (format t "~2%Mirror reloaded from disk")
          (multiple-value-bind (okay)
              (file-system-equal random-fs reloaded)
            (if okay
                (format t "~2%Test result okay")
                (cerror "Continue with tests"
                        "Test copy file system fails"))))))))

;; ----------------------------------------------------------------------------
;; Test observers
;; ----------------------------------------------------------------------------

(defun test-file-system-observers (directory &key mode)
  "Runs random tests on Common Lisp and uiop file system functions."
  (when t
    (let* ((dir (fresh-test-dir "action-test/random" :directory directory))
           (*whitelist* (list dir))
           (*sandbox*
            (make-empty-sandbox (list (path-root directory))))
           (*simulate* (ecase mode
                         (:simulate t)
                         (:execute nil))))
    (fill-directory-randomly dir)
    (test-cl-wild-pathname-p dir)
    ;;(test-cl-directory dir)
    (test-uiop-directory-files dir)
    (test-uiop-subdirectories dir))))

(defun test-cl-wild-pathname-p (directory)
  (test-function
   :function 'wild-pathname-p
   :generator
   (lambda ()
     (list (random-pathname :prefix directory)
           (pick
            '(nil :host :device :directory :name :type :version))))
   :condition
   (lambda (pathname &optional field-key)
     (declare (ignore pathname))
     (member field-key
             '(nil :host :device :directory :name :type :version)))
   :oracle
   (lambda (pathname &optional field-key)
     (lambda (result)
       (equiv result
              (case field-key
                (:host (eql (pathname-host pathname) :wild))
                (:directory (eql (pathname-name pathname) :wild))
                (:name (eql (pathname-name pathname) :wild))
                (:type (eql (pathname-type pathname) :wild))
                ((nil) (or (member :wild (pathname-directory pathname))
                           (eql (pathname-name pathname) :wild)
                           (eql (pathname-type pathname) :wild)
                           (eql (pathname-version pathname) :wild)))))))))

(defun test-cl-directory (directory)
  "Random tests for Common Lisp function directory."
  (test-function
   :title "directory for random directories"
   :function
   (lambda (pathname)
     (directory
      (merge-pathnames uiop:*wild-directory*
                       (uiop:pathname-parent-directory-pathname pathname))))
   :generator
   (lambda ()
     (list (random-pathspec directory)))
   :condition
   (lambda (pathname)
     (and
      (uiop:directory-pathname-p pathname)
      (when (uiop:pathname-parent-directory-pathname pathname)
        (directory-exists-p
         (uiop:pathname-parent-directory-pathname pathname)))))
   :oracle
   (lambda (pathname)
     (lambda (result)
       (and
        (loop
           with parent = (uiop:pathname-parent-directory-pathname pathname)
           for path in result
           always (equal (uiop:pathname-parent-directory-pathname path)
                         parent))
        (equiv (member (pathname pathname) result :test #'equal)
               (directory-exists-p pathname))))))
  (test-function
   :title "DIRECTORY for random files"
   :function
   (lambda (pathname)
     (directory
      (merge-pathnames uiop:*wild-file*
                       (uiop:pathname-directory-pathname pathname))))
   :generator
   (lambda ()
     (list (random-pathspec directory)))
   :condition
   (lambda (pathname)
     (and
      (uiop:file-pathname-p pathname)
      (directory-exists-p
       (uiop:pathname-directory-pathname pathname))))
   :oracle
   (lambda (pathname)
     (lambda (result)
       (and
        (loop
           with parent = (uiop:pathname-directory-pathname pathname)
           for path in result
           always (equal (uiop:pathname-directory-pathname path)
                         parent))
        (equiv (member (pathname pathname) result :test #'equal)
               (file-exists-p pathname)))))))

(defun test-uiop-subdirectories (directory)
  "Random tests for uiop:subdirectories."
  (test-function
   :title "uiop:subdirectories"
   :function
   (lambda (pathname)
     (subdirectories (uiop:pathname-parent-directory-pathname pathname)))
   :generator
   (lambda ()
     (list (random-pathspec directory)))
   :condition
   (lambda (pathname)
     (and
      (uiop:directory-pathname-p pathname)
      (when (uiop:pathname-parent-directory-pathname pathname)
        (directory-exists-p
         (uiop:pathname-parent-directory-pathname pathname)))))
   :oracle
   (lambda (pathname)
     (lambda (result)
       (and
        (loop
           with parent = (uiop:pathname-parent-directory-pathname pathname)
           for path in result
           always (equal (uiop:pathname-parent-directory-pathname path)
                         parent))
        (equiv (member (pathname pathname) result :test #'equal)
               (directory-exists-p pathname)))))))

(defun test-uiop-directory-files (directory)
  "Random tests for uiop:directory-files."
  (test-function
   :title "uiop:directory-files"
   :function
   (lambda (pathname)
     (directory-files (uiop:pathname-directory-pathname pathname)))
   :generator
   (lambda ()
     (list (random-pathspec directory)))
   :condition
   (lambda (pathname)
     (and
      (uiop:file-pathname-p pathname)
      (directory-exists-p
       (uiop:pathname-directory-pathname pathname))))
   :oracle
   (lambda (pathname)
     (lambda (result)
       (and
        (loop
           with parent = (uiop:pathname-directory-pathname pathname)
           for path in result
           always (equal (uiop:pathname-directory-pathname path)
                         parent))
        (equiv (member (pathname pathname) result :test #'equal)
               (file-exists-p pathname)))))))

(defun test-make-pathname ()
  (test-function
   :function 'parse-namestring
   :generator
   (lambda ()
     (list (namestring (random-pathname))))
   :condition
   (lambda (thing &optional host (default-pathname *default-pathname-defaults*)
            &key (start 0) end junk-allowed)
     (declare (ignore thing host default-pathname start end junk-allowed))
     t)
   :oracle
   (lambda (thing &optional host (default-pathname *default-pathname-defaults*)
            &key (start 0) end junk-allowed)
     (declare (ignore host default-pathname start end junk-allowed))
     (lambda (pathname position)
       (declare (ignore position))
       (string-equal
        (namestring pathname)
        thing)))))

;; ----------------------------------------------------------------------------
;;  test-file-system-actions
;; ----------------------------------------------------------------------------

(defun test-file-system-actions (directory &key
                                 (mode :simulate)
                                 (verbosity 1)
                                 (max-attempts 100000)
                                 (target 100))
  "Sets up an environment for function test-actions and calls it."
  (let* ((*sandbox* (make-empty-sandbox (list (path-root directory))))
         (*simulate* (ecase mode
                       (:simulate t)
                       (:execute nil)))
         (dir (fresh-test-dir "action-test/random" :directory directory))
         (scenario-dir (merge-pathnames "scenario/" directory))
         (*whitelist* (list dir))
         (*default-pathname-defaults* (pathname dir)))
    (format t "~%Creating test directory ~A~%" dir)
    (ensure-directories-exist dir)
    (format t "~%Ensuring real scenario directory ~A~%" scenario-dir)
    (let ((*simulate* nil))
      (with-access scenario-dir
        (ensure-directories-exist scenario-dir)))
    (format t "~%Current directory is ~A~%" (getcwd))
    (fill-directory-randomly dir)
    (let ((*action-log* (snapshot-as-action-log (take-snapshot))))
      (test-actions dir scenario-dir
                    :target target
                    :verbosity verbosity
                    :max-attempts max-attempts
                    :generator 'random-action
                    :test-post t
                    :test-diff t)))
  (values))

(defun test-actions (directory scenario-directory &key
                     (verbosity 1)
                     (max-attempts 100000)
                     (target 100)
                     (generator 'random-action)
                     (test-post t)
                     (test-diff t))
  "Automated test using pre and post conditions with random actions on
a random file system."
  (flet ((log-lvl (level text &rest args)
           (when (<= verbosity level)
             (apply #'format t text args))))
    (format t "~%* Automated action tests~%")
    (format t "~%Settings:")
    (format t "~%  Test directory:       ~A" directory)
    (format t "~%  Scenario directory:   ~A" scenario-directory)
    (format t "~%  Mode:                 ~S" (if *simulate*
                                                 :simulate
                                                 :execute))
    (format t "~%  Verbosity:            ~S" verbosity)
    (format t "~%  Target:               ~S" target)
    (format t "~%  Max-attempts:         ~S" max-attempts)
    (format t "~%  Generator:            ~S" 'random-action)
    (format t "~%  Test post:            ~S" t)
    (format t "~%  Test diff:            ~S~%" t)
    (unwind-protect
         (loop
            with fs-copy = (take-snapshot)
            with success-table = (make-hash-table :test #'equal)
            with failure-table = (make-hash-table :test #'equal)
            with standard-output = *standard-output*
            with success-count = 0
            initially
              (setf *opened-test-streams* ())
              (setf *chosen-restart* nil)
            (when (<= 2 verbosity)
              (format t "~%Initial file system:~2%")
              (print-file-system *sandbox*)
              (terpri))
            (format t "~%* Running tests...~2%")
            repeat max-attempts
            for (name . args) = (funcall generator directory)
            for pre = (apply #'test-pre-condition name args)
            for unsuccessful = (if pre 0 1) then (if pre 0 (+ unsuccessful 1))
            for tests = 0 then (if pre (+ tests 1) tests)
            if pre
            do (restart-case
                   (let ((snapshot (take-snapshot)))
                     (handler-bind 
                       ((condition
                         (lambda (x)
                           (setf (gethash name failure-table)
                                 (+ (gethash name failure-table 0) 1))
                           (let ((*standard-output* standard-output))
                             (format t "~2%Automatic test interputed:~%")
                             (format t "~%~A" x)
                             (format t "~2%during test:~%")
                             (pprint (cons name args))
                             (terpri))
                           (dump-error-scenario
                            x
                            directory
                            scenario-directory
                            (cons name args)
                            snapshot)
                           (if *chosen-restart*
                               (invoke-restart
                                (find-restart *chosen-restart* x))
                               (error x)))))
                     (let ((results (multiple-value-list
                                     (let ((*test-pre* nil)
                                           (*test-post* test-post)
                                           (*test-diff* test-diff))
                                       (when (<= 2 verbosity)
                                         (terpri)
                                         (format t "~%* Running test ~A:" name)
                                         (terpri))
                                       (push (cons name args) *action-log*)
                                       ;;(setf *snapshot* (take-snapshot))
                                       (apply name args)))))
                       (incf success-count)
                       (setf (gethash name success-table)
                             (+ (gethash name success-table 0) 1))
                       (when (<= 2 verbosity)
                         (format t "~%* Test ~A finished" name )
                         (format t "~%result: ~{~A~^,~%~}~%" results))
                       (when (= 1 verbosity)
                         (when (zerop (mod success-count
                                           (ceiling target 80)))
                           ;;(princ ".")
                           (format t "~%~A/~A"
                                   success-count target))))))
                 (keep-testing ()
                   :report (lambda (stream)
                             (format stream "Keep testing")))
                 (keep-testing-silently ()
                   :report (lambda (stream)
                             (format stream "Keep testing and don't ask again"))
                   (setf *chosen-restart* 'keep-testing))
                 (dump-and-keep-testing ()
                   :report (lambda (stream)
                             (format stream "Print current state and keep testing"))
                   (format t "~%* Dumping file system...~%")
                   (print-file-system *sandbox*)
                   (format t "~%* File system dumped, resuming tests...~%"))
                 (dump-and-keep-testing-silently ()
                   :report (lambda (stream)
                             (format stream "Print current state, keep testing and don't ask again"))
                   (setf *chosen-restart* 'dump-and-keep-testing)
                   (format t "~%* Dumping file system~%")
                   (print-file-system *sandbox*)
                   (format t "~%* File system dumped, resuming tests...~%")))
            until (= tests target)
            finally
            (terpri)
            (when (<= 2 verbosity)
              (format t "~%* Tests done. File system changes:")
              (terpri)
              (print-diff fs-copy (take-snapshot))
              (terpri))
            (format t "~%* Automated action tests ready~%")
            (format t "~%Success count for tested functions:")
            (loop
               for name being the hash-key in success-table
               using (hash-value count)
               for i = 1 then (1+ i)
               do (format t "~%~2,' D. ~A: ~50T~5,' D" i name count))
            (format t "~&~%Failure count for tested functions:")
            (loop
               for name being the hash-key in failure-table
               using (hash-value count)
               for i = 1 then (1+ i)
               do (format t "~%~2,' D. ~A: ~50T~5,' D" i name count)))
      (log-lvl 1 "~2%* Closing opened test streams...")
      (reset-test-streams)
      (log-lvl 1 "done~%")))
  (values))

;; ----------------------------------------------------------------------------
;; Test utilities
;; ----------------------------------------------------------------------------

(defun test-function (&key function generator condition oracle
                      (verbose t)
                      (max-attempts 100000)
                      (target 1000)
                      title)
  ""
  (let ((successes 0))
    (loop
       initially (when verbose
                   (format t "~%* Testing function ~A~%" (or title function)))
       repeat max-attempts
       for arguments = (funcall generator)
       for pre = (apply condition arguments)
       for tests = (if pre 1 0) then (if pre (+ tests 1) tests)
       if pre
       do (let ((results (multiple-value-list (apply function arguments))))
            (if (apply (apply oracle arguments) results)
              (progn
                (incf successes)
                (when verbose
                  (when (zerop (mod successes (ceiling target 80)))
                    (princ "."))))
              (error
               "Function ~2%  ~A~%failed on arguments~2%  ~A and results ~2%~A"
               function arguments results)))
       until (= tests target)
       finally
         (when verbose
           (format t "~%Test ready. Success is ~A/~A" successes target)
           (terpri)))
    successes))

(defun fresh-test-dir (name &key directory)
  (assert directory)
  (loop
     initially (assert (< 0 (length name)))
     for tries = 0 then (+ tries 1)
     for dir = (format nil "~A~A~2,'0D/" directory name tries)
     while (directory-exists-p dir)
     if (< 10 tries)
     do (cerror "Try next"
                "More than 10 (currently trying ~A) ~A test directories" 
                tries name)
     finally
       (return dir)))

(defun fresh-filename (directory name)
  (loop
     initially (assert (< 0 (length name)))
     for tries = 0 then (+ tries 1)
     for filename = (format nil "~A-~4,'0D.lisp" name tries)
     for path = (merge-pathnames filename directory)
     while (file-exists-p path)
     finally (return path)))

(defun reset-test-streams ()
  (loop
     for stream in *opened-test-streams*
     do (ignore-errors (close stream))
     finally (setf *opened-test-streams* nil)))

(defun snapshot-as-action-log (snapshot)
  "A list of list expressions that recreates the directories and files
in snapshot."
  (loop
     for path in snapshot
     collect (list (if (uiop:directory-pathname-p path)
                       'ensure-directories-exist
                       'create-file)
                   path)))

(defun dump-error-scenario (condition test-dir dump-dir action snapshot)
  "Helper for function test-actions."
  (let ((simulate-p *simulate*)
        (cwd (getcwd))
        (*simulate* nil)
        (whitelist *whitelist*)
        (actions (reverse *action-log*)))
    (with-access dump-dir
      (let ((scenario-file (fresh-filename dump-dir "scenario")))
        (format t "~%Writing actions to scenario file ~A~%"
                scenario-file)
        (with-open-file (s scenario-file
                           :direction :output
                           :if-does-not-exist :create)
          (format s "(in-package :cl-user)")
          (format s "~%")
          (format s "~%; Condition '~A' thrown during action:~%"
                  (substitute #\- #\newline (princ-to-string condition)))
          (format s "~%;  (~{~S~^ ~})" action)
          (format s "~%")
          (format s "~%; Info:")
          (format s "~%;   Test directory = ~A" test-dir)
          (format s "~%;   Working directory = ~A" cwd)
          (format s "~%;   Var *whitelist* = ~A" whitelist)
          (format s "~%;   Switch *simulate* = ~A" simulate-p)
          (format s "~%;   Lisp type = ~A" (lisp-implementation-type))
          (format s "~%;   Lisp version = ~A" (lisp-implementation-version))
          (format s "~%")
          (format s "~%(setf cl-sandbox:*whitelist* '(~S))" test-dir)
          (format s "~%")
          (format s "~%(unless (cl-sandbox:directory-exists-p ~S)" test-dir)
          (format s "~%  (error \"Test directory ~A does not exist\"))"
                  test-dir)
          (format s "~%")
          (format s "~%(when (cl-sandbox:subdirectories ~S)" test-dir)
          (format s "~%  (error \"Test directory ~A is not empty\"))"
                  test-dir)
          (format s "~%")
          (format s "~%(when (cl-sandbox:directory-files ~S)" test-dir)
          (format s "~%  (error \"Test directory ~A is not empty\"))"
                  test-dir)
          (format s "~%")
          (format s "~%(setf *default-pathname-defaults* (pathname ~S))"
                  test-dir)
          (format s "~%; (cl-sandbox:chdir ~S)" test-dir)
          (format s "~%")
          (format s "~%(setf cl-sandbox::*opened-test-streams* nil)")
          (format s "~%(setf cl-sandbox::*test-pre* t)")
          (format s "~%(setf cl-sandbox::*test-post* t)")
          (format s "~%(setf cl-sandbox::*test-diff* t)")
          (format s "~%")
          (format s "~%(if t  ;; Setting to nil might be sufficient to reproduce the error and simplify analysis")
          (format s "~%")
          (format s "~%  ;; Scenario:~%" )
          (format s "~%  (progn")
          (loop
             for action in actions
             do (format s "~%    (~{~S~^ ~})" (quote-action action)))
          (format s ")")
          (format s "~%  (progn")
          (format s "~2%    ;; Snapshot:~%")
          (loop
             for x in snapshot
             do (format s "~%    (~S ~S)"
                        (if (uiop:directory-pathname-p x)
                            'ensure-directories-exist 'create-file)
                        x))
          (format s "~2%    ;; Action:~%")
          (format s "~%    (~{~S~^ ~})" (quote-action action))
          (format s "))"))))))

(defun quote-action (action)
  "Helper for function function dump-error-scenario. Quotes the
arguments in action when necessary. Makes it so that we can do (eval
action) instead of (apply (first action) (rest action))."
  (let ((fun (first action))
        (args (rest action)))
    (cons fun
          (loop
             with streams = *opened-test-streams*
             for arg in args
             collect (typecase arg
                       (list (when arg
                               (cons 'list arg)))
                       (symbol (if (keywordp arg)
                                   arg (list 'quote arg)))
                       (stream (list 'nth
                                     (position arg streams)
                                     '*opened-test-streams*))
                       (t arg))))))
