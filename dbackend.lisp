(declaim (optimize (debug 3)))


(setf *trace-output* (open "log.txt" :direction :output))
;(defun chain (functionCalls)
;  (assert (listp functionCalls) (functionCalls))
;  (if (cdr functionCalls)
;    (apply (car functionCalls) chain (cdr functionCalls))
;)

; Executes fn for each element of range and executes separator between each call
; fn takes one argument and separator none
(defun interlacedEach (range fn separator)
  (assert (listp range))
  (funcall fn (car range))
  (if (cdr range)
    (progn
      (funcall separator)
      (interlacedEach (cdr range) fn separator)
    )
  )
)

; Obtained from https://stackoverflow.com/questions/11073250/tacit-programming-in-lisp?noredirect=1&lq=1
; Author: Miron Brezuleanu
(defmacro -> (obj &rest forms)
  "Similar to the -> macro from clojure, but with a tweak: if there is
  a $ symbol somewhere in the form, the object is not added as the
  first argument to the form, but instead replaces the $ symbol."
  (if forms
      (if (consp (car forms)) ; It's a list (non null)
          (let* ((first-form (first forms))
                 (other-forms (rest forms))
                 (pos (position '$ first-form)))
            (if pos
                `(-> ,(append (subseq first-form 0 pos)
                              (list obj)
                              (subseq first-form (1+ pos)))
                     ,@other-forms)
                `(-> ,(list* (first first-form) obj (rest first-form))
                     ,@other-forms)))
          `(-> ,(list (car forms) obj) ; Make into a list
               ,@(cdr forms)))
      obj)
)

; Executes functions with value as parameter
(defun split (value &rest functions)
  (progn (mapcar #'(lambda (fun) (funcall fun value)) functions))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Codegen ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sink (&rest text)
  (map nil #'(lambda (arg)
    (cond ((stringp arg)(format t arg)) ; Allows using things like ~%
          (t (format t "~A" arg))
    )
  ) text)
)

(defun processTree (&rest tree)
  (map nil #'(lambda (branch)
    ;(progn
    ;(format t "~A" "Processing: ")
    ;(format t "~A~%" branch)
    (cond ((stringp branch)(sink branch))                   ; Just output as format string
          ((numberp branch)(sink (write-to-string branch))) ; Make string
          ((eql branch nil)nil)                             ; Ignore
          ((listp branch)(apply (car branch)(cdr branch)))  ; Recurse
          (t (call (string branch)))                        ; Take symbol as function call
    )
    ;)
  ) tree)
)

(defun binOp (lhs op rhs)
  (processTree lhs " " op " " rhs)
)

(defun call (funName &optional args)
  ;(assert (listp args))
  (assert (stringp funName))
  (processTree funName "(")
  (if (not (null args))
    (progn
      (assert (eq (car args) 'n-arglist))
      (processTree args)
    )
 )
  (processTree ")")
)

(defun for (decl conditions post body)
  (processTree "for (" decl "; " conditions "; " post ") {~%" body "~%}~%")
)

(defun foreach (elementName range code)
  (processTree "foreach (" elementName "; " range ") {~%" code "~%}~%")
)

(defun n-arglist (&rest args)
  (interlacedEach
    args 
    #'(lambda (arg) (processTree arg))
    #'(lambda () (processTree ", ")))
)

(defun funDecl (returnType name args body)
  (assert (stringp name))
  (assert (listp args))
  (assert (eq (car args) 'n-arglist))
  (processTree returnType " " name " (" args "){~%" body "~%}~%")
)

(defun importDecl (name)
  (assert (stringp name))
  (processTree "import " name ";~%")
)

(setq lastId 0)

(defun newVar ()
  (progn
    (setq lastId (+ lastId 1))
    (concatenate 'string "var" (write-to-string lastId))
  )
)

(defun idx (var index)
  (processTree var "[" index "]")  
  )


(setq structs ())

(defun struct (name fields)
  (assert (stringp name))
  (processTree "struct " name " {~%" fields "~%}~%")
  (setq structs (push name structs))
)

(defun varDecl (&optional initialValue typename)
  (processTree (
    if (null typename) "auto" typename)
    " " (newVar)
    (if (null initialValue) ";~%" (concatenate 'string " = " initialValue ";~%"))
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Sugar ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun iota (&optional number)
  (if number '(call "iota" (number)) '(call "iota"))
)


(defun increasing (up-to body)
  (let ((varName (newVar)))
    (for
      (concatenate 'string "uint " varName " = 0")
      (concatenate 'string varName " < " (write-to-string up-to))
      (concatenate 'string varName "++")
      body
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Neural network ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun denseForward ()
  (processTree '(foreach "i" (call "iota" (n-arglist "4")) nue))
)


#|
(processTree "// Hello ~%")
(processTree nil)
(processTree '(importDecl "std.stdio"))
(processTree '(binOp "var1" "=" "var2") "~%")
(processTree '(foreach "el" "range" (call "writeln" (n-arglist "el" "`\n`"))))
(processTree '(funDecl "void" "main" (n-arglist "string[] args") (call "writeln" (n-arglist "args[0]"))))
(processTree '(for "int i = 0" "i<5" "i++" (call "testfun")))
(denseForward)
;(trace increasing)
(increasing 3 'writeln)
(idx "varIdx" 4)
|#
(struct "Neural" "int a; double b;")
(struct "Neural2" "int a; double b;")
(print structs)
(varDecl "5" "int")
(varDecl nil "double")
(varDecl)
(varDecl "6")
