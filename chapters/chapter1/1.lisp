(in-package #:c1)

(defun make-env ()
  '())

(alexandria:define-constant +false-value+ #\F)
(alexandria:define-constant +true-value+ #\T)

(defun problem (key &rest args)
  (error "Key: ~A. Args: ~{~A ~^~}" key args))

(defun lookup (symbol env)
  (car (assoc symbol env :test #'eq)))

(defun make-function (args body env)
  (eval `(lambda (,@args) ,@body)))

(defun modify (symbol value environment)
  (print symbol)
  (print value)
  (print environment)
  (push (cons symbol value) environment)
  (print environment)
  value)

(defparameter *env* '((a . "jew")))

(defmacro wrap (&body form)
  (alexandria:with-gensyms (body)
    `(let ((,body ,@form))
       (if (null ,body)
           +false-value+
           ,body))))

(defun handle-special-form (form rest env)
  (case form
    (!quote (first rest))
    (!lambda (make-function (first rest) (rest rest) env))
    (!if (if (not (eq (!eval (first rest) env) +false-value+))
             ;;everything not +false-value+ is implicitly true             
             (!eval (second rest) env)
             (!eval (third rest) env)))
    (!prog (wrap (first (last (mapcar (lambda (form)
                                        (!eval form env))
                                      rest)))))
    (!change (modify (first rest) (!eval (second rest) env) env))
    (otherwise (apply form (mapcar (lambda (form)
                                     (!eval form env))
                                   rest)))))

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

