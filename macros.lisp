(defmacro -> (obj &rest forms)
  "Obtained from https://stackoverflow.com/questions/11073250/tacit-programming-in-lisp
  Author: Miron Brezuleanu
  Similar to the -> macro from clojure, but with a tweak: if there is
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

(defmacro l0 (&rest funDecl)
  "Shortcut for creating a 0-parameter lambda.
  Example: (funcall (l0 print 4)) prints 4"
  `(lambda () ,funDecl)
)

(defmacro l1 (&rest fundecl)
  "Shortcut for creating a 1-parameter lambda.
  Internally uses the arrow -> to allow implicit usage of the parameter.
  Example: (funcall (l1 * 2) 2) produces 4"
  (let ((sym (gensym)))
    `(lambda (,sym) (-> ,sym ,fundecl))
  )
)

#|
(set-dispatch-macro-character #\# #\{
  #'(lambda (stream sub-char numarg)
    (declare (ignore sub-char numarg))
    `(print 4)
  )
)
|#
