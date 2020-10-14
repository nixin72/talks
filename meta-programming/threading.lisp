(defmacro --> (exp &rest funcalls)
  (reduce (lambda (a x)
            (cons (car x) (cons a (cdr x))))
          funcalls
          :initial-value exp))

(defmacro -->> (exp &rest funcalls)
  (reduce (lambda (a x)
            (append x (list a)))
          funcalls
          :initial-value exp))

(defmacro -> (exp &rest funcalls)
  (reduce (lambda (a x)
            (let* ((found nil)
                   (insert (loop for el in x collect
                             (cond
                               ((eq el '_)
                                (setf found t)
                                a)
                               (t el)))))
              (if found
                  insert
                  (cons (car x) (cons a (cdr x))))))
          funcalls
          :initial-value exp))

                  
