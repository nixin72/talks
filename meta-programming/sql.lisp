(defmacro sql (&key select from where)
  (concatenate
   'string
   (create-select select)
   (create-from from)
   (create-where where)))

(defun keys->plist (alist)
  (cond ((null alist) nil)
        ((keywordp (car alist))
         (cons (list (car alist) (second alist))
               (keys->plist (cddr alist))))
        (t (cons (car alist) (keys->plist (cdr alist))))))

(defun create-select (columns)
  (concatenate
   'string
   "SELECT "
   (cond ((atom columns) (symbol-name columns))
         ((listp columns)
          (reduce (lambda (a x)
                    (if (listp x)
                        (format nil "~a~{~a~^.~}, " a x)
                        (format nil "~a~a, " a x)))
                  (keys->plist columns)
                  :initial-value "")))))

(defun create-from (tables) ...)
(defun create-where (conditions) ...)
