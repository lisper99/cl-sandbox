;; ----------------------------------------------------------------------------
;; All sandboxable actions
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
;; Common Lisp actions
;; ----------------------------------------------------------------------------

(defaction ensure-directories-exist (path &key verbose)
  "Sandboxable version of Common Lisp function ensure-directories-exist."

  (:pre-condition
   (declare (ignore verbose))
   (and
    (uiop:directory-pathname-p path)
    (ensureable path)))

  (:body
   (validate-access path)
   (if (execute-p)
       (cl:ensure-directories-exist path :verbose verbose)
       (sandbox-ensure-directories-exist *sandbox* path :verbose verbose)))

  (:post-condition
   (declare (ignore verbose))
   (lambda (pathspec created)
     (declare (ignore created))
     (and
      (equal pathspec path)
      (directory-exists-p path))))

  (:difference
   (declare (ignore verbose))
   (lambda (pathspec created)
     (declare (ignore pathspec))
     (lambda (removed added)
       (and
        (null removed)
        (equiv created added)
        (loop
           for new in added
           always (subpathp* path new)))))))

(defaction rename-file (filespec new-name)
  "Sandboxable implementation of Common Lisp function rename-file."

  (:pre-condition
   (let ((name (merge-pathnames new-name filespec)))
     (and
      (not (wild-pathname-p filespec))
      (not (wild-pathname-p new-name))
      (uiop:file-pathname-p name)
      (file-exists-p filespec)
      (not (open-file-p filespec))
      (directory-exists-p (uiop:pathname-directory-pathname name))
      (not (probe-file* name)))))

  (:body
   (validate-access filespec)
   (validate-access new-name)
   (if (execute-p)
       (cl:rename-file filespec new-name)
       (sandbox-rename-file *sandbox* filespec new-name)))

  (:post-condition
   (let ((filename (truename filespec)))
     (lambda (default old new)
       ;; HyperSpec says (merge-pathnames new-name filespec) but
       ;; sbcl and ccl seem to do (merge-pathnames new-name filename).
       ;; Is that logical?
       (let (;;(name (merge-pathnames new-name filespec))
             (name (merge-pathnames new-name filename)))
         (and
          (equal default name)
          (equal old filename)
          (equal new (truename name)))))))

  (:difference
   (declare (ignore filespec new-name))
   (lambda (default old new)
     (declare (ignore default))
     (lambda (removed added)
       (or (and
            (null removed)
            (null added)
            (equal old new))
           (and
            (equal added (list new))
            (equal removed (list old))))))))

(defaction delete-file (filespec)
  "Sandboxable implementation of Common Lisp function delete-file."

  (:pre-condition
   (and
    (not (wild-pathname-p filespec))
    (file-exists-p filespec)
    (not (open-file-p filespec))))

  (:body
   (validate-access filespec)
   (if (execute-p)
       (cl:delete-file filespec)
       (sandbox-delete-file *sandbox* filespec)))

  (:post-condition
    (lambda (result)
     (declare (ignore result))
     (not (file-exists-p filespec))))

  (:difference
   (let ((truename (truename filespec)))
     (lambda (result)
       (declare (ignore result))
       (lambda (removed added)
         (and
          (null added)
          (equal removed (list truename))))))))

;; ----------------------------------------------------------------------------
;; Common Lisp stream actions
;; ----------------------------------------------------------------------------

(defaction open (filename 
                 &key
                 (direction :input)
                 (element-type 'base-char)
                 (if-exists (if (eq (pathname-version filename) :newest)
                                :new-version
                                :error))
                 (if-does-not-exist (cond ((eq direction :probe)
                                           nil)
                                          ((or (eq direction :input)
                                               (eq if-exists :overwrite)
                                               (eq if-exists :append))
                                           :error)
                                          (t :create)))
                 (external-format :default))
  "Sandboxable implementation of Common Lisp function open."

  (:pre-condition
   (declare (ignore external-format element-type))
   (and
    (not (wild-pathname-p filename))
    (uiop:file-pathname-p filename)
    (not (directory-exists-p filename))
    ;;(not (open-file-p filename))
    ;; Why?
    #+sbcl (not (null if-does-not-exist))
    (if (file-exists-p filename)
        (or (equal direction :input)
            (equal direction :probe)
            (not (equal if-exists :error))
            (null if-exists))
        (and (not (equal if-does-not-exist :error))
             (directory-exists-p
              (uiop:pathname-directory-pathname filename))))))

  (:body
   (validate-access filename)
   (if (execute-p)
       (let ((stream
              (cl:open filename
                       :direction direction
                       :element-type element-type
                       :if-exists if-exists
                       :if-does-not-exist if-does-not-exist
                       :external-format external-format)))
         (when stream
           (setf *open-streams* (cons stream *open-streams*)))
         stream)
       (sandbox-open *sandbox*
                     filename
                     :direction direction
                     :if-exists if-exists
                     :if-does-not-exist if-does-not-exist)))

  (:post-condition
   (declare (ignore if-does-not-exist external-format element-type))
   (lambda (stream)
     (or (null stream) (typep stream 'stream))))

  (:difference
   (declare (ignore external-format element-type))
   (let ((existed (file-exists-p filename)))
     (lambda (stream)
       (lambda (removed added)
         (and
          (null removed)
          (equiv (null added)
                 (or existed
                     (and (not existed)
                          (equal if-does-not-exist nil))))
          (or
           (null added)
           (and
            ;; clisp doesn't like the inner equal
            ;; here. uiop:pathname-equal on pathnames would work
            (equal added (list (truename (pathname stream))))))))))))

(defaction close (stream &key abort)
  "Sandboxable implementation of Common Lisp function close."

  (:pre-condition
   (declare (ignore abort #-ccl stream))
   ;; Why does ccl not allow non-existing files? close has no
   ;; exceptional situations.
   #+ccl (file-exists-p (pathname stream))
   #-ccl t
   )

  (:body
   (prog1 (if (execute-p)
              (cl:close stream :abort abort)
              (sandbox-close *sandbox* stream :abort abort))
     (setf *open-streams* (remove stream *open-streams*))))

  (:post-condition
   (declare (ignore abort))
   (let ((openp (open-stream-p stream)))
     (lambda (result)
       (and
        (if (and #-(or sbcl clisp) nil
                 (execute-p))
            (equiv result t)
            (equiv result openp))
        (implies (file-exists-p (pathname stream))
                 (not (open-stream-p stream)))))))

  (:difference
   (declare (ignore abort))
   (let ((truename (when (file-exists-p (pathname stream))
                     (truename (pathname stream)))))
   (lambda (result)
     (declare (ignore result))
     (lambda (removed added)
       (and
        (null added)
        (or (null removed)
            (and ;;abort  hierop hikt ccl
             (equal removed (list truename))))))))))

(defaction write-string (string &optional output-stream
                                &key (start 0) (end nil))
  "Sandboxable version of Common Lisp function write-string."

  (:pre-condition
   (declare (ignore string start end))
   (implies output-stream (open-stream-p output-stream)))

  (:body
   (if (execute-p)
       (cl:write-string string output-stream :start start :end end)
       string))

  (:post-condition
   (declare (ignore output-stream start end))
   (lambda (output)
     (and
      (equal string output))))
  
  (:difference
   (declare (ignore output-stream string start end))
   (lambda (output)
     (declare (ignore output))
     (lambda (removed added)
       (and (null removed) (null added))))))

;; ----------------------------------------------------------------------------
;; Common Lisp sysem construction actions
;; ----------------------------------------------------------------------------

;; todo: compile-file

;; ----------------------------------------------------------------------------
;; UIOP OS actions
;; ----------------------------------------------------------------------------

(defaction chdir (pathspec)
  "Sandboxable version of uiop:chdir."
  
  (:pre-condition
   (and (not (wild-pathname-p pathspec))
        (directory-exists-p pathspec)))

  (:body
   (validate-access pathspec)
   (if (execute-p)
       (uiop:chdir pathspec)
       (sandbox-chdir *sandbox* pathspec)))

  (:post-condition
   (let ((truename (truename pathspec)))
     (lambda (result)
       (declare (ignore result))
       (equal (getcwd) truename))))

  (:difference
   (declare (ignore pathspec))
   (lambda (result)
     (declare (ignore result))
     (lambda (removed added)
       (and (null removed) (null added))))))

;; ----------------------------------------------------------------------------
;; UIOP pathname actions
;; ----------------------------------------------------------------------------

;; todo: ensure-pathname

;; ----------------------------------------------------------------------------
;; UIOP file system actions
;; ----------------------------------------------------------------------------

(defaction delete-directory-tree (directory-pathname
                                  &key validate (if-does-not-exist :error))
  "Sandboxable version of uiop:delete-directory-tree."

  (:pre-condition
   (and (member if-does-not-exist '(:ignore :error))
        (pathnamep directory-pathname)
        (uiop:directory-pathname-p directory-pathname)
        (not (wild-pathname-p directory-pathname))
        (implies (equal if-does-not-exist :error)
                 (directory-exists-p directory-pathname))
        (uiop:call-function validate directory-pathname)
        (not (any-open-file-p directory-pathname))
        (implies (directory-exists-p directory-pathname)
                 (let ((cwd (getcwd))
                       (truename (truename directory-pathname)))
                   (and
                    (not (uiop:pathname-equal cwd truename))
                    (not (subpathp* cwd truename)))))))
  
  (:body
   (validate-access directory-pathname)
   (if (execute-p)
       (uiop:delete-directory-tree directory-pathname
                                   :validate validate
                                   :if-does-not-exist if-does-not-exist)
       (sandbox-delete-directory-tree *sandbox* directory-pathname
                                      :validate validate
                                      :if-does-not-exist if-does-not-exist)))
  
  (:post-condition
   (declare (ignore validate if-does-not-exist))
   (lambda (result)
     (declare (ignore result))
     (not (directory-exists-p directory-pathname))))
  
  (:difference
   (declare (ignore validate if-does-not-exist))
   (let ((existed (directory-exists-p directory-pathname)))
     (lambda (result)
       (declare (ignore result))
       (lambda (removed added)
         (or (and existed
                  removed
                  (null added)
                  (loop
                     for x in removed
                     always (subpathp* x directory-pathname)))
             (and (not existed)
                  (null removed)
                  (null added))))))))
   
(defaction delete-empty-directory (directory-pathname)
  "Sandboxable version of uiop:delete-empty-directory."
  
  (:pre-condition
   (and (not (wild-pathname-p directory-pathname))
        (directory-exists-p directory-pathname)
        (null (subdirectories (truename directory-pathname)))  
        (null (directory-files (truename directory-pathname)))
        (not (uiop:pathname-equal (getcwd)
                                  (truename directory-pathname)))))

  (:body   
   (validate-access directory-pathname)
   (if (execute-p)
       (uiop:delete-empty-directory directory-pathname)
       (sandbox-delete-empty-directory *sandbox* directory-pathname)))
  
  (:post-condition
   (lambda (result)
     (declare (ignore result))
     (not (directory-exists-p directory-pathname))))
  
  (:difference
   (let ((true-name (truename  directory-pathname)))
     (lambda (result)
       (declare (ignore result))
       (lambda (removed added)
         (and (null added)
              (equal removed (list true-name))))))))

(defaction delete-file-if-exists (filename)
  "Sandboxable version of uiop:delete-file-if-exists."

  (:pre-condition
   (and (not (wild-pathname-p filename))
        ;;(not (directory-exists-p filename))
        ))

  (:body
   (validate-access filename)
   (if (execute-p)
       (uiop:delete-file-if-exists filename)
       (sandbox-delete-file-if-exists *sandbox* filename)))
  
  (:post-condition
   (let ((existed (file-exists-p filename))) 
     (lambda (result)
       (equiv result (and existed (not (file-exists-p filename)))))))
  
  (:difference
   (let ((truename (when (file-exists-p filename)
                     (truename filename))))
     (lambda (result)
       ;;(declare (ignore result))
       (lambda (removed added)
         (and (null added)
              (if (and truename result)
                  (equal removed (list truename))
                  (null removed))))))))

(defaction rename-file-overwriting-target (filespec new-name)
  "Sandboxable version of uiop:rename-file-overwriting-target."
  
  (:pre-condition
   (let ((name (merge-pathnames new-name filespec)))
     (and
      (not (wild-pathname-p filespec))
      (not (wild-pathname-p new-name))
      (not (equal name (pathname filespec)))
      (file-exists-p filespec)
      (not (open-file-p filespec))
      (not (directory-exists-p name))
      (implies (file-exists-p name) (not (open-file-p name)))
      (directory-exists-p (uiop:pathname-directory-pathname name)))))
  
  (:body
   (validate-access filespec)
   (validate-access new-name)
   (if (execute-p)
       (uiop:rename-file-overwriting-target filespec new-name)
       (sandbox-rename-file-overwriting-target *sandbox* filespec new-name)))
  
  (:post-condition
    (let ((filespec-truename (truename filespec)))
      (lambda (defaulted-new-name old-truename new-truename)
        (let ((name (merge-pathnames new-name filespec)))
          (and
           (equal defaulted-new-name name)
           (equal old-truename filespec-truename)
           (equal new-truename (truename name)))))))
  
  (:difference
   (let ((existed (file-exists-p (merge-pathnames new-name filespec))))
     (lambda (defaulted-new-name old-truename new-truename)
       (declare (ignore defaulted-new-name))
       (lambda (removed added)
         (or
          (and (null removed)
               (null added)
               (equal old-truename new-truename))
          (and (or
                (and existed (null added))
                (equal added (list new-truename)))
               (equal removed (list old-truename)))))))))

(defaction ensure-all-directories-exist (pathnames)
  "Sandboxable version of uiop:ensure-all-directories-exist."

  (:pre-condition
   (and
    (loop for path in pathnames
       always (uiop:directory-pathname-p path))
    (loop for path in pathnames
       always (ensureable path))))
  
  (:body
   (loop for path in pathnames
      do (validate-access path))
   (if (execute-p)
       (uiop:ensure-all-directories-exist pathnames)
       (sandbox-ensure-all-directories-exist *sandbox* pathnames)))
  
  (:post-condition
   (lambda (result)
     (and (null result)
          (loop for p in pathnames
             always (directory-exists-p p)))))
  
  (:difference
   (let ((all-existed (loop for p in pathnames
                         always (directory-exists-p p))))
     (lambda (result)
       (declare (ignore result))
       (lambda (removed added)
         (or (and all-existed
                  (and (null removed)
                       (null added)))
             (and
              added
              (null removed)
              (loop for new in added
                 always (loop for p in pathnames
                           thereis (subpathp* p new))))))))))

;; ----------------------------------------------------------------------------
;; UIOP stream actions
;; ----------------------------------------------------------------------------

;; todo: concatenate-files

(defaction copy-file (from to)
  "Sandboxable version of uiop:copy-file"
  
  (:pre-condition
   (and
    (file-exists-p from)
    (not (open-file-p from))
    (directory-exists-p (uiop:pathname-directory-pathname to))
    (uiop:file-pathname-p to)
    (not (probe-file to))))
  
  (:body
   (validate-access to)
   (validate-access from)
   (if (execute-p)
       (handler-case
           (progn (uiop:copy-file from to) t)
         (file-error () nil))
       (sandbox-copy-file *sandbox* from to)))
  
  (:post-condition
   (declare (ignore from))
   (lambda (result)
     (implies result (file-exists-p to))))
  
  (:difference
   (declare (ignore from))
   (lambda (result)
     (lambda (removed added)
       (and (null removed)
            (if result
                (let ((true-to (truename to)))
                  (and (member true-to added :test #'equal)
                       (loop
                          for new in added
                          always (or (subpathp* true-to new)
                                     (equal true-to new)))))
                (null added)))))))

;; ----------------------------------------------------------------------------
;; UIOP lisp-build actions
;; ----------------------------------------------------------------------------

;; todo: combine-fasls

;; todo: compile-file*

;; ----------------------------------------------------------------------------
;; Alternatives
;; ----------------------------------------------------------------------------

(defaction safe-copy-file (from to)
  "Safe version of copy-file"
  
  (:pre-condition
   (declare (ignore from to))
   (and
    ;;(file-exists-p from)
    ;; Or this for transaction behavior
    ;;(ensureable (uiop:pathname-parent-directory-pathname to))
    ;;(uiop:file-pathname-p to)
    ;;(not (probe-file to))
    t))
  
  (:body
   (validate-access to)
   (validate-access from)
   (handler-case
       (progn
         (if (execute-p)
              (uiop:copy-file from to)
             (sandbox-copy-file *sandbox* from to))
         t)
     (file-error () nil)
     (condition () nil)))
  
  (:post-condition
   (declare (ignore from))
   (lambda (result)
     (implies result (file-exists-p to))))
  
  (:difference
   (declare (ignore from))
   (lambda (result)
     (lambda (removed added)
       (and (null removed)
            (if result
                (let ((true-to (truename to)))
                  (and (member true-to added :test #'equal)
                       (loop
                          for new in added
                          always (or (subpathp* true-to new)
                                     (equal true-to new)))))
                ;;(null added)
                (loop
                   with abs-to =  (absolute-pathname to)
                   for new in added
                   always (subpathp* abs-to new))))))))

(defaction safe-delete-directory-tree (directory-pathname
                                       &key validate (if-does-not-exist :error))
  "Sandboxable version of uiop:delete-directory-tree."

  (:pre-condition
   (and (member if-does-not-exist '(:ignore :error))
        (pathnamep directory-pathname)
        (uiop:directory-pathname-p directory-pathname)
        (not (wild-pathname-p directory-pathname))
        (implies (equal if-does-not-exist :error)
                 (directory-exists-p directory-pathname))
        (uiop:call-function validate directory-pathname)
        ;;(not (any-open-file-p directory-pathname))
        (implies (directory-exists-p directory-pathname)
                 (let ((cwd (getcwd))
                       (truename (truename directory-pathname)))
                   (and
                    (not (uiop:pathname-equal cwd truename))
                    (not (subpathp* cwd truename)))))))
  
  (:body
   (validate-access directory-pathname)
   (handler-case
       (if (execute-p)
           (uiop:delete-directory-tree directory-pathname
                                       :validate validate
                                       :if-does-not-exist if-does-not-exist)
           (sandbox-delete-directory-tree *sandbox* directory-pathname
                                          :validate validate
                                          :if-does-not-exist if-does-not-exist))
     (file-error () nil)
     (condition () nil)))
  
  (:post-condition
   (declare (ignore validate if-does-not-exist))
   (lambda (result)
     ;;(declare (ignore result))
     (implies result (not (directory-exists-p directory-pathname)))))
  
  (:difference
   (declare (ignore validate if-does-not-exist))
   (let ((existed (directory-exists-p directory-pathname)))
     (lambda (result)
       (declare (ignore result))
       (lambda (removed added)
         (or (and existed
                  removed
                  (null added)
                  (loop
                     for x in removed
                     always (subpathp* x directory-pathname)))
             (and (not existed)
                  (null removed)
                  (null added))))))))

(defaction safe-delete-file (filespec)
  "Alternative implementation of Common Lisp function delete-file."
  
  (:pre-condition
    (and
     (not (wild-pathname-p filespec))
     ;;(not (open-file-p filespec))
     (file-exists-p filespec)))
  
   (:body 
    (validate-access filespec)
    (handler-case
        (progn
          (if (execute-p)
              (cl:delete-file filespec)
              (sandbox-delete-file *sandbox* filespec))
          #+ccl(not (file-exists-p filespec))
          #-ccl t)
      (file-error () nil)
      (condition () nil)))
   
   (:post-condition
    (lambda (result)
      (xor result (file-exists-p filespec))))
   
   (:difference
    (let ((truename (truename filespec)))
      (lambda (result)
        (lambda (removed added)
          (or
           (and (null result)
                (null removed)
                (null added))
           (and result
                (null added)
                (equal removed (list truename)))))))))

(defaction safe-rename-file (filespec new-name)
  "Safe variant of Common Lisp function rename-file."

  (:pre-condition
   (let ((name (merge-pathnames new-name filespec)))
     (and
      (not (wild-pathname-p filespec))
      (not (wild-pathname-p new-name))
      (uiop:file-pathname-p name)
      (file-exists-p filespec)
      ;;(not (open-file-p filespec))
      (directory-exists-p (uiop:pathname-directory-pathname name))
      (not (probe-file* name)))))

  (:body
   (validate-access filespec)
   (validate-access new-name)
      (handler-case
          (if (execute-p)
              (cl:rename-file filespec new-name)
              (sandbox-rename-file *sandbox* filespec new-name))
     (file-error () nil)
     (condition () nil)))

  (:post-condition
   (let ((filename (truename filespec)))
     (lambda (default &optional old new)
       (implies
        default
        (let ( ;;(name (merge-pathnames new-name filespec)) sbcl error
              (name (merge-pathnames new-name filename)))
          (and
           (equal default name)
           (equal old filename)
           (equal new (truename name))))))))

  (:difference
   (declare (ignore filespec new-name))
   (lambda (default &optional old new)
     (lambda (removed added)
       (implies default
                (or (and
                     (null removed)
                     (null added)
                     (equal old new))
                    (and
                     (equal added (list new))
                     (equal removed (list old)))))))))

(defaction safe-rename-file-overwriting-target (filespec new-name)
  "Sandboxable version of uiop:rename-file-overwriting-target."
  
  (:pre-condition
   (let ((name (merge-pathnames new-name filespec)))
     (and
      (not (wild-pathname-p filespec))
      (not (wild-pathname-p new-name))
      (not (equal name (pathname filespec)))
      (file-exists-p filespec)
;;      (not (open-file-p filespec))
      (not (directory-exists-p name))
      ;;(implies (file-exists-p name) (not (open-file-p name)))
      (directory-exists-p (uiop:pathname-directory-pathname name)))))
  
  (:body
   (validate-access filespec)
   (validate-access new-name)
   (handler-case
       (if (execute-p)
           (uiop:rename-file-overwriting-target filespec new-name)
           (sandbox-rename-file-overwriting-target *sandbox* filespec new-name))
     (file-error () nil)
     (condition () nil)))
  
  (:post-condition
    (let ((filespec-truename (truename filespec)))
      (lambda (defaulted-new-name &optional old-truename new-truename)
        (implies defaulted-new-name
        (let ((name (merge-pathnames new-name filespec)))
          (and
           (equal defaulted-new-name name)
           (equal old-truename filespec-truename)
           (equal new-truename (truename name))))))))
  
  (:difference
   (let ((existed (file-exists-p (merge-pathnames new-name filespec))))
     (lambda (defaulted-new-name &optional old-truename new-truename)
       ;;(declare (ignore defaulted-new-name))
       (lambda (removed added)
         (or
          (and (null removed)
               (null added)
               (or (null defaulted-new-name)
                   (equal old-truename new-truename)))
          (and (or
                (and existed (null added))
                (equal added (list new-truename)))
               (equal removed (list old-truename)))))))))

;; ----------------------------------------------------------------------------
;; Extra: copy-directory
;; ----------------------------------------------------------------------------

(defaction copy-directory (from to)
  "Copy a directory"
   
  (:pre-condition
   (and (directory-exists-p from)
        (not (probe-file to))))
  
  (:body
   (if
    #+windows (execute-p)
    #-windows nil
    (progn
      (cl:ensure-directories-exist to)
      (uiop:run-program (format nil "xcopy ~S ~S /E /q"
                                (string-trim "/" (namestring (truename from)))
                                (string-trim "/" (namestring (truename to))))
                        :output t
                        :error-output :output))
    ;; This also works on Windows but is more expensive
    (collect-sub*directories
     from
     (lambda (dir) (declare (ignore dir)) t)
     (lambda (dir) (declare (ignore dir)) t)
     (lambda (dir)
       (loop
          with dst = (merge-pathnames (enough-namestring dir from) to)
          initially (ensure-directories-exist dst)
          for file in (directory-files dir)
          for enough = (enough-namestring file from)
          do (copy-file file (merge-pathnames enough to)))))))
  
  (:post-condition
   (declare (ignore from to))
   (lambda (result)
     (declare (ignore result))
     t))
  
  (:difference
   (declare (ignore from to))
   (lambda (result)
     (declare (ignore result))
     t)))

;; ----------------------------------------------------------------------------
;; Extra: zip/unzip
;;
;; untested
;; ----------------------------------------------------------------------------

(defaction zip-directory (directory filespec)
  "Zips the directory to zip file filespec. Paths are stored relative
to the directory."
   
  (:pre-condition
   (and (directory-exists-p directory)
        (not (probe-file filespec))))
  
  (:body
   (validate-access directory)
   (validate-access filespec)
   (when (file-exists-p filespec)
     (error 'file-error :pathname (pathname filespec)))
   (if (execute-p)
       (zip:zip filespec directory)
       (create-file filespec))
   t)
  
  (:post-condition
   (lambda (result)
     (and result
          (file-exists-p filespec)
          (implies (execute-p)
                   (correct-zip-mirror filespec directory)))))
  
  (:difference
   (declare (ignore directory))
   (lambda (result)
     (lambda (removed added)
       (and result
            (null removed)
            (equal added (list (truename filespec))))))))

(defaction unzip-file (zipfile directory)
  "Unzips the zip file zipfile in directory."
   
  (:pre-condition
   (and (file-exists-p zipfile)
        (directory-exists-p directory)))
  
  (:body
   (validate-access zipfile)
   (validate-access directory)
   (when (execute-p)
     (zip:unzip zipfile directory))
   t)
  
  (:post-condition
   (lambda (result)
     (and result
          (implies (execute-p)
                   (correct-zip-mirror zipfile directory)))))
  
  (:difference
   (declare (ignore zipfile directory))
   (lambda (result)
     (lambda (removed added)
       (declare (ignore added))
       ;; check added against sorted-zip-pathnames
       (and result
            (null removed))))))

(defun correct-zip-mirror (zip-file directory)
  "Is the set of pathname in zip-file equal to the set of pathnames in
directory? Does not compare file contents."
  (sorted-pathname-lists-equal
   (sorted-zip-pathnames zip-file)
   (sorted-directory-pathnames directory)))

(defun sorted-zip-pathnames (zip-file)
  "Helper for correct-zip-mirror."
  (let ((dir (merge-pathnames
              (uiop:ensure-directory-pathname (pathname-name zip-file))
              (uiop:pathname-directory-pathname zip-file)))
        (paths ()))
    (push dir paths)
    (zip:do-zipfile-entries (name entry (zip:open-zipfile zip-file))
      (push (merge-pathnames (pathname name) dir) paths))
    (setf paths (sort paths #'string< :key #'namestring))
    paths))

(defun sorted-directory-pathnames (directory)
  "Helper for correct-zip-mirror."
  (let ((paths ()))
    (collect-sub*directories 
     directory 
     (lambda (x) (declare (ignore x)) t)
     (lambda (x) (declare (ignore x)) t)
     (lambda (x) (push (pathname x) paths)
             (loop for y in (directory-files x)
                do (push (pathname y) paths))))
     (setf paths (sort paths #'string< :key #'namestring))
     paths))

;; ----------------------------------------------------------------------------
;; Extra: download-file
;;
;; untested
;; ----------------------------------------------------------------------------

(defaction download-file (url filespec)
  "Downloads file url and saves it as file filespec."
   
  (:pre-condition
   (declare (ignore url))
   (not (directory-exists-p filespec)))
  
  (:body
   (validate-access filespec)
   (if (execute-p)
       (trivial-download:download url filespec)
       (create-file filespec)))
  
  (:post-condition
   (declare (ignore url))
   (lambda (result)
     (declare (ignore result))
     (file-exists-p filespec)))
  
  (:difference
   (declare (ignore url))
   (lambda (result)
     (declare (ignore result))
     (lambda (removed added)
       (and (null removed)
            (implies added
                     (equal added (list (truename filespec)))))))))
