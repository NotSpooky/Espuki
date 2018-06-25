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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Tree parsing ;;;;;;;;
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
    (cond ((stringp branch)(sink branch))
          ((eql branch nil)nil)
          (t(apply (car branch)(cdr branch)))
    )
    ;)
  ) tree)
)

(defun binOp (lhs op rhs)
  (processTree lhs " " op " " rhs)
)

(defun call (funName args)
  (assert (listp args))
  (assert (eq (car args) 'n-arglist))
  (processTree funName "(" args ")")
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

(defun funDecl (returnType name args code)
  (assert (stringp name))
  (assert (listp args))
  (assert (eq (car args) 'n-arglist))
  (processTree returnType " " name " (" args "){~%" code "~%}~%")
)

(defun importDecl (name)
  (assert (stringp name))
  (processTree "import " name ";")
)

(processTree "// Hello ~%")
(processTree nil)
(processTree '(binOp "var1" "=" "var2") "~%")
(processTree '(foreach "el" "range" (call "writeln" (n-arglist "el" "`\n`"))))
(processTree '(funDecl "void" "main" (n-arglist "string[] args") (call "writeln" (n-arglist "args[0]"))))
