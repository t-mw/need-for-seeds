(define-native *love-events*)

(defmacro defevent (ev args &body)
  (let [(sm (gensym))]
    (if (list? ev)
        `(let [(,sm (lambda ,(append '(_) args) ,@body))]
           (.<! ,(nth ev 1) ,(nth ev 2), sm))
        `(let* [(,sm (lambda ,args ,@body))]
           (.<! *love-events* ,ev ,sm)))))
