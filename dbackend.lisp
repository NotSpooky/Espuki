(load "macros.lisp")

;(setf *trace-output* (open "log.txt" :direction :output))
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
;;;;;;;;; Codegen ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sink (&rest text)
  "Used for outputting generated text.
  If it's a string, it's used as a format string, allowing using for example ~%.
  If it isn't it's pretty printed to the output."
  (map nil #'(lambda (arg)
    (cond ((stringp arg)(format t arg))
          (t (format t "~A" arg))
    )
  ) text)
)

(defun processTree (&rest tree)
  "Takes an AST, processes it and outputs it.
  In case it's a string it's outputted as a format string.
  Numbers are outputted as-is.
  Lists with a function name as head are called.
  Nils are ignored."
  (map nil #'(lambda (branch)
    (cond ((stringp branch)(sink branch))                   ; Just output as format string
          ((numberp branch)(sink (write-to-string branch))) ; Make string
          ((eql branch nil)nil)                             ; Ignore
          ((listp branch)(apply (car branch)(cdr branch)))  ; Recurse
          (t (call (string branch)))                        ; Take symbol as function call
    )
  ) tree)
)

(defun binOp (lhs op rhs)
  "Binary operation."
  (processTree lhs " " op " " rhs)
)

(defun unOp (lhs op)
  "Unary operation."
  (processTree lhs " " op)
)

(defun assign (lhs rhs)
  "Variable assignment."
  (binOp lhs "=" rhs)
)

(defun seqn (&rest args)
  "Sequence of statements.
  Usually separated by ; or newlines"
  (mapcar #'(lambda (arg) (processTree arg ";~%")) args)
)

(defun call (funName &optional &rest args)
  "Function call"
  (assert (stringp funName))
  (processTree funName "(")
  (if (not (null args))
    (apply 'n-arglist args)
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
  "List of arguments, for example for function calls."
  (interlacedEach
    args 
    #'(lambda (arg) (processTree arg))
    #'(lambda () (processTree ", ")))
)

(defun funDecl (returnType name args body)
  (assert (stringp name))
  (assert (listp args))
  (processTree returnType " " name " (")
  (if (not (null args))
    (apply 'n-arglist args)
  )
  (processTree "){~%" body "~%}~%")
)

(defun importDecl (name)
  (assert (stringp name))
  (processTree "import " name)
)

(defvar lastId 0)

(defun newVar ()
  "Returns a new variable name"
  (progn
    (setq lastId (+ lastId 1))
    (concatenate 'string "var" (write-to-string lastId))
  )
)

(defvar structs ())

(defun n-struct (name fields)
  "Structure declaration"
  (assert (stringp name))
  (processTree "struct " name " {~%" fields "~%}~%")
  (setq structs (push name structs))
)

(defun varDecl (&optional initialValue typename name)
  "Declare a new variable.
  If initialValue isn't provided, the variable isn't initialized.
  If typename isn't provided, the variable is given a generic/inferred type.
  If name is not provided, a new one is auto generated."
  (let ((retVal (if name name (newVar)))) ; Use name or autogenerate one
    (processTree (
      if (null typename) "auto" typename)
      " " retVal
    )
    (if (not(null initialValue)) (processTree " = " initialValue))
    retVal
  )
)

(defun arrType (typeName &optional staticSize)
  "Generates a type name consisting of an array of element type typeName.
  staticSize is used when a static array is desired, it's the amount of elements."
  (format nil "~A[~A]" typeName (if staticSize  staticSize ""))
)

(defun incr (var)
  "Operation that increases in 1 the variable named var"
  (processTree var "++")
)

(defun slice (var &optional (initial 0) (ending "$"))
  (processTree var "[" initial ".." ending "]")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Sugar ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun iota (&optional number)
  (if number '(call "iota" (number)) '(call "iota"))
)


(defun increasing (up-to body &key varname)
  ; Use varName if it exists, create a new one otherwise.
  (let ((varName (if varname varname (newVar))))
    (for
      `(varDecl "0" "uint" ,varName)
      `(binOp ,varName "<" ,up-to)
      `(incr ,varName)
      body
    )
  )
)

(defun n-member (source memberName)
  (processTree source "." memberName)
)

(defun idx (source index)
  (processTree source "[" index "]")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Neural network ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun neuron-fw (inputs weights output)
  (seqn
    '(importDecl "std.numeric : dotProduct")
    `(assign ,output (call "dotProduct" ,weights ,inputs))
  )
)

(defun dense-fw (inputs weights outputs)
  ;(defun for (decl conditions post body)
  (let ((pos (newVar)))
    (increasing
      `(n-member ,weights "length")
      `(neuron-fw (idx ,inputs ,pos) (idx ,weights ,pos) (idx ,outputs ,pos))
      :varname pos
    )
  )
)

#|(defun dense-bw (deltaOutput average optimizer errorOfInput)
  ()
)|#

(defun bias-fw (inputs biases outputs)
  (seqn `(assign ,outputs (binOp ,inputs "+" ,biases)))
)

(defun activation-fw (data activation)
  (seqn `(assign ,data (call ,activation ,data)))
)

(defun bias-bw (deltaOutput)
  deltaOutput
)

;(dense-fw "inputs" "weights" "outputs")
(bias-fw "inputs" "biases" "outputs")

#|
(processTree "// Hello ~%")
(processTree nil)
(processTree '(importDecl "std.stdio"))
(processTree '(binOp "var1" "=" "var2") "~%")
(processTree '(foreach "el" "range" (call "writeln" ("el" "`\n`"))))
(processTree '(funDecl "void" "main" ("string[] args") (call "writeln" ("args[0]"))))
(processTree '(for "int i = 0" "i<5" "i++" (call "testfun")))
;(trace increasing)
(increasing 3 'writeln)
(idx "varIdx" 4)
(n-struct "Neural" "int a; double b;")
(n-struct "Neural2" "int a; double b;")
(print structs)
(varDecl "5" "int")
(varDecl nil "double")
(varDecl)
(varDecl "6")
(varDecl 'readln)
|#

#|
(defvar networkType "float")
(defvar connections 32)
(defvar inputType (arrType networkType connections))
(let ((weights "weights") (inputs "inputs") (outputs "outputs"))
  (seqn
    `(varDecl "[]" ,inputType ,weights)
    `(neuron-fw ,inputs ,weights ,outputs)
  )
)
|#
