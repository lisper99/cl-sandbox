;; ----------------------------------------------------------------------------
;; Package definition for the cl-sandbox package
;; 
;; Paul Griffioen 2019-2019
;; ----------------------------------------------------------------------------

(defpackage "CL-SANDBOX"
  (:documentation "The cl-sandbox package provides a library for safe
  file management. A virtual file system allows simulation of actions
  before they are really performed.")
  (:use "COMMON-LISP")
  (:shadow "ENSURE-DIRECTORIES-EXIST"
           "RENAME-FILE"
           "DELETE-FILE"
           "OPEN"
           "CLOSE"
           "WRITE-STRING"
           "WITH-OPEN-FILE"
           "FILE-LENGTH"
           "FILE-POSITION"
           "PATHNAME"
           "TRUENAME"
           "PROBE-FILE"
           "FILE-AUTHOR"
           "FILE-WRITE-DATE"
           "DIRECTORY")
  (:export
   "SANDBOX"
   "SANDBOX-STREAM"

   "*SANDBOX*"
   "*WHITELIST*"
   "*SIMULATE*"
   "*TEST-PRE*"
   "*TEST-POST*"
   "*TEST-DIFF*"
   "*CONFINE*"

   "WITH-SANDBOX"
   "WITH-ACCESS"
   "DEFACTION"

   "EXECUTE-P"
   "SIMULATE-P"

   "TEST-PRE-CONDITION"
   "POST-CONDITION-TEST"
   "DIFFERENCE-TEST"

   "ENSURE-DIRECTORIES-EXIST"
   "RENAME-FILE"
   "DELETE-FILE"
   "DELETE-FILE*"
   "OPEN"
   "CLOSE"
   "WITH-OPEN-FILE"
   "COPY-FILE"
   "DELETE-DIRECTORY-TREE"
   "DELETE-EMPTY-DIRECTORY"
   "DELETE-FILE-IF-EXISTS"
   "RENAME-FILE-OVERWRITING-TARGET"
   "ENSURE-ALL-DIRECTORIES-EXIST"
   "PATHNAME"
   "TRUENAME"
   "PROBE-FILE"
   "FILE-AUTHOR"
   "FILE-WRITE-DATE"
   "DIRECTORY"
   "DIRECTORY-EXISTS-P"
   "FILE-EXISTS-P"
   "SUBDIRECTORIES"
   "DIRECTORY-FILES"
   "COLLECT-SUB*DIRECTORIES"
   "DIRECTORY*"
   "SAFE-FILE-WRITE-DATE"

   "GETCWD"
   "CHDIR"
   
   "WRITE-STRING"

   #+:wrapuiop "PATHNAME-DIRECTORY-PATHNAME"
   #+:wrapuiop "DIRECTORY-PATHNAME-P"

   "VARY-FILE-TYPE"
   "COPY-DIRECTORY"
   "ZIP-DIRECTORY"
   "UNZIP-FILE"
   "DOWNLOAD-FILE"))
