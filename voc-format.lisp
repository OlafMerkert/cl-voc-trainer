(defpackage :voc-format
  (:use :cl :ol
        :cxml :cxml-dom
        :cl-who)
  (:export :voc-file :author-name :modules
           :voc-module :module-name :languages :voc-list
           :output))

(in-package :voc-format)

(defclass voc-file ()
  ((name :accessor author-name
                :initform "")
   (date)
   (modules :accessor modules
            :initform nil)))

(defclass voc-module ()
  ((name :accessor module-name
         :initform "")
   (languages :accessor languages
              :initform '(|Dansk| |Deutsch|))
   (voc-list :accessor voc-list)))

(defgeneric output (x file))

  
(defmethod output ((x voc-file) file)
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (output x stream)))

(defmethod output ((x voc-file) (stream stream))
  ;; xml declaration
  (format stream "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
  (with-html-output (stream)
    (htm (:voc-file
          (:author (esc (author-name x)))
          ;; TODO date
          (loop for m in (modules x)
             do (output m stream))))))

(defmethod output ((x voc-module) (stream stream))
  (with-html-output (stream)
    (htm (:voc-module
          (:title (esc (module-name x)))
          (:languages
           (loop for l in (languages x)
                do (htm (:lang (esc (symbol-name l))))))
          (:vocs
           (loop for vv across (voc-list x)
                do (htm (:vv
                         (loop for v in vv
                             do (htm (:v (esc v))))))))))))

;; (defmethod input (file)
;;   (with-open-file (stream file :direction :input)
;;     (input stream)))

;; (defmethod input ((stream stream))
;;   ;; TODO parse the voc file
;;   (let* ((document (parse-stream stream (make-dom-builder)))
;;          (root (aref (children document 0))))
;;     )
;;   )

;; (defun input-file (stream))

;; (defun input-module (stream))

;; (defun input-voc (stream module))
