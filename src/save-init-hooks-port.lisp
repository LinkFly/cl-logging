(in-package :logging)

(defun save-init-hooks (&key save init)
  #+sbcl 
  (progn 
    (when save (pushnew save sb-ext:*save-hooks*))
    (when init (pushnew init sb-ext:*init-hooks*))))
