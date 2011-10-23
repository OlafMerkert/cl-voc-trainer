(defpackage :voc-entry
  (:use :cl :ol))

(in-package :voc-entry)

;;; Oberfläche für die Eingabe

(defmacro ilet (bindings &body body)
  `(let ,(mapcar #`(,(car a1) (make-instance ,@(cdr a1))) bindings)
     ,@body))

(defun entry-ui ()
  (gtk:within-main-loop
    (ilet ((window 'gtk:gtk-window :title "Vokabeleingabe"
                   :default-height 800 :default-width 300)
           (mbox 'gtk:v-box)
           (lname 'gtk:label :label "<Lektionsname>")
           (bbox 'gtk:h-button-box)
           (edit-button 'gtk:button :label "Ändern")
           (del-button 'gtk:button :label "Löschen")
           )
      
      (gtk:box-pack-start bbox edit-button :expand  nil :padding 2)
      (gtk:box-pack-start bbox del-button  :padding 2)
      
      (gtk:container-add mbox lname)
      (gtk:container-add mbox bbox)
      
      (gtk:container-add window mbox)
      (gtk:widget-show window))))

(defun entry-ui ()
  (gtk:within-main-loop
    (let ((window       (make-instance 'gtk:gtk-window
                                       :title "Vokabeleingabe"
                                       :default-height 800
                                       :default-width 300 ))
          (lektionsname (make-instance 'gtk:label :label "<Lektionsname>"))
          (edit-button  (make-instance 'gtk:button :label "Bearbeiten"))
          (del-button   (make-instance 'gtk:button :label "Entfernen"))
          (save-button  (make-instance 'gtk:button :label "Speichern"))
          (modus :entry)
          (view  (make-instance 'gtk:tree-view))
          (model (make-instance 'gtk:list-store))
          (dansk-entry   (make-instance 'gtk:entry))
          (deutsch-entry (make-instance 'gtk:entry)))
      ;; Layout
      (let ((main-box (make-instance 'gtk:box          :orientation :vertical ))
            (button-box (make-instance 'gtk:button-box :orientation :horizontal :homogeneous nil))
            (entry-box  (make-instance 'gtk:box        :orientation :horizontal )))
        (dolist (b (list edit-button del-button))
          (gtk:box-pack-start button-box b :expand nil :fill nil :padding 10))
        (dolist (b (list dansk-entry deutsch-entry save-button))
          (gtk:box-pack-start entry-box b :expand nil :fill nil))
        (dolist (w (list lektionsname
                         button-box
                         view
                         entry-box))
          (gtk:box-pack-start main-box w :expand nil :fill nil))
        (gtk:container-add window main-box))
      (gtk:widget-show window))))
