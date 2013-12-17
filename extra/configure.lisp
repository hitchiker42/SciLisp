(require 'cl-ppcre)
(use-package 'iterate)
(use-package 'cl-ppcre)
(defmacro strcat (&rest strings)
  `(concatenate 'string ,@strings))
(defun parse-headers (c-file)
  (let ((scanner (create-scanner "#include[ \t]*\"(\\w+\\.h)\""))
        (headers nil))
    (iter (for line in-file c-file using #'read-line)
          (let ((header-arr
                 (nth-value 1 (scan-to-strings scanner line))))
            (if header-arr
                (push (aref header-arr 0) headers))))
    headers))
(defun parse-headers-recursive (c-file)
  (let
      ((base-dir (directory-namestring c-file)))
    (labels
        ((acc (filenames headers)
           (if (null filenames)
               (delete-duplicates headers :test #'equal)
               (let
                   ((car-headers (parse-headers (strcat base-dir (car filenames)))))
                 (acc (append (cdr filenames)
                              (set-difference car-headers headers :test #'equal))
                      (append headers car-headers))))))
      (acc (list (file-namestring c-file)) nil))))

