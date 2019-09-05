;; ----------------------------------------------------------------------------
;; System definition for the cl-sandbox package
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

(in-package :cl-user)

(asdf:defsystem :cl-sandbox
  :version "0"
  :serial t
  :depends-on
  (:uiop
   :trivial-gray-streams)
  :components
  ((:file "package"                 :pathname "package")
   (:file "prelude"                 :pathname "prelude")
   (:file "action"                  :pathname "contract/action")
   (:file "perform-statement"       :pathname "contract/perform-statement")
   (:file "file"                    :pathname "sandbox/file-data")
   (:file "directory"               :pathname "sandbox/directory-data")
   (:file "node"                    :pathname "sandbox/fs-node")
   (:file "stream"                  :pathname "sandbox/stream")
   (:file "sandbox"                 :pathname "sandbox/sandbox")
   (:file "interface"               :pathname "sandbox/interface")
   (:file "actions"                 :pathname "interface/actions")
   (:file "observers"               :pathname "interface/observers")
   (:file "other"                   :pathname "interface/other")
   (:file "convert"                 :pathname "examples/convert")
   (:file "random"                  :pathname "test/random")
   (:file "test"                    :pathname "test/test")))
