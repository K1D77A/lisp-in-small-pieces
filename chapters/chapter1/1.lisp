(in-package #:c1)

(defun make-env ()
  '())

(alexandria:define-constant +false-value+ #\F)
(alexandria:define-constant +true-value+ #\T)

(defun problem (key &rest args)
  (error "Key: ~A. Args: ~{~A ~^~}" key args))

(defun lookup (symbol env)
  (unless (listp env)
    (problem :Unbound symbol))
  (let ((found? (assoc symbol env :test #'eq)))
    (if found?
        (cdr found?)
        (problem :Unbound symbol))))
        

(defun make-function (variables body environment)
  (lambda (values)
    (!prog body (extend environment variables values))))
           
    
;;further in the chapter you have the example where you can use symbol values
;;or the symbol property list to store values rather than an alist.
(defun modify (symbol value environment)
  "Modify by just pushing a new value to environment"
  (setf (cdr (assoc symbol environment :test #'eq)) value)
  value)

(defparameter *global-env* ())

(defmacro definitial (sym val)
  `(push (cons ,sym ,val) *global-env*))

(definitial nil +false-value+)
(definitial t +true-value+)

(defmacro defprimitive (name fun arity)
;;the problem with their impl is you are locked to an arity when there are lots of primitives
  ;;that accept a lot of arguments...
  `(push (cons ',name (lambda (values)
                       ,(if (eq arity :rest)
                            `(apply #',fun values)
                            `(if (= (length values) ,arity)
                                 (apply #',fun values)
                                 (problem :Bad-Arity values)))))
         *global-env*))

(defprimitive !cons cons 2)
(defprimitive !+ + :rest)
(defprimitive !- - :rest)
                          
(defun extend (env variables values)
  (unless (= (length variables)
             (length values))
    (problem :Incorrect-Length variables values))
  (pairlis (reverse variables) (reverse values) env))

(defmacro wrap (&body form)
  (alexandria:with-gensyms (body)
    `(let ((,body ,@form))
       (if (null ,body)
           +false-value+
           ,body))))

(defun !prog (forms env)
  (if (null forms)
      +false-value+
      ;;we are checking if it has arguments otherwise we just return our false value
      (if (listp forms)
          (wrap (first (last (mapeval forms env))))
          (!eval forms env))))
                           
(defun mapeval (forms env)
  (mapcar (lambda (form) (!eval form env)) forms))

(defun evlis (forms env)
  (if (listp forms)
      ;;this is already explicit left to right.
      ;;right to left would look like
      ;;(mapeval (reverse forms) env)
      (mapeval forms env)
      +false-value+))

(defun invoke (fun? args env)
  (unless (functionp fun?)
    (problem :Not-a-functionn fun? args))
  (funcall fun? (evlis args env)))

(defun call-lisp (fun args env)
  (apply fun (evlis args env)))

(defun !let (bindings body env)
  (let ((extended (extend env (mapcar #'car bindings)
                          (evlis (mapcar #'cadr bindings) env))))
    (!prog body extended)))

(defun !let* (bindings body env)
  ;;have to extend one by one evaling the next with the result of eval and extending the prev
  (let ((variables (mapcar #'car bindings))
        (values (mapcar #'cadr bindings))
        (nv env))
    (labels ((extend (var val env)
               (cons (cons var (!eval val env)) env)))
      (mapc (lambda (var val)
              (setf nv                    
                    (extend var val nv)))
            variables
            values)
      ;;could be recursive or whatever but meh
    (!prog body nv))))

(defun handle-special-form (form rest env)
  (case form
    (!quote (first rest))
    (!lambda (make-function (first rest) (rest rest) env))
    (!if (if (not (eq (!eval (first rest) env) +false-value+))
             ;;everything not +false-value+ is implicitly true             
             (!eval (second rest) env)
             (!eval (third rest) env)))
    (!prog (!prog rest env))
    (!change (modify (first rest) (!eval (second rest) env) env))
    (!lisp (call-lisp (first rest) (rest rest) env))
    (!let (!let (first rest) (rest rest) env))
    (!let* (!let* (first rest) (rest rest) env))
    (otherwise (invoke (!eval form env) rest env))))

(defun booleanp (form)
  (or (eq form +true-value+)
      (eq form +false-value+)))
  
(defun !eval (form env)
  (if (atom form)
      (cond ((and (symbolp form) (not (keywordp form)))
             (lookup form env))
            ((or (numberp form)
                 (booleanp form)
                 (stringp form)
                 (characterp form)
                 (arrayp form)
                 (keywordp form))
             form)            
            (t (problem :Cannot-Evaluate form)))
      (handle-special-form (first form) (rest form) env)))


(defun repl ()
  (loop (print (!eval (read) *global-env*))))
