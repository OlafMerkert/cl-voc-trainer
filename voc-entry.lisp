(defpackage :voc-entry
  (:shadowing-import-from :gtk :range)
  (:shadowing-import-from :cl-prevalence :name)
  (:use :cl :gtk :gdk :gobject :ol
        :cl-prevalence
        :prevalence-utils
        :gtk-utils)
  (:export
   :voc-main))

(in-package :voc-entry)

;;; Persistenz mit cl-prevalence

(define-prevalence-storage #P"~/Studium/HS11/Dänisch I/vokabeln3/")

(define-storage lektionen nil)

;;; Oberfläche für die Eingabe

(defclass vokabel (prevailing)
  ((dansk   :accessor dansk
            :initarg  :dansk
            :initform "")
   (deutsch :accessor deutsch
            :initarg  :deutsch
            :initform "")
   (richtig :accessor richtig
            :initform 0)
   (falsch  :accessor falsch
            :initform 0)))

(defclass lektion (prevailing)
  ((name     :accessor name
             :initform "Lektion ??")
   (vokabeln :accessor vokabeln
             :initform (make-array 10 :adjustable t :fill-pointer 0))))

(defmethod confidence (obj) 0)

(create-standard-print-object vokabel
                              dansk "-" deutsch (richtig falsch))
(create-standard-print-object lektion name)

(defun lektion-voc-count (lektion)
  (length (vokabeln lektion)))

(define-custom-store vokabel-entry
    ((dansk      :type string  :label "Dansk")
     (deutsch    :type string  :label "Deutsch")
     (confidence :type integer :label "Punkte")))

(define-custom-store lektionen
    ((name              :type string  :label "Lektionstitel")
     (lektion-voc-count :type integer :label "Anzahl Vokabeln")
     (confidence        :type integer :label "Minimale Punkte"))
  :initial-contents lektionen)

(defun voc-main ()
  (load-lektionen)
  (ensure-adjustable-array lektionen)
  (lektion-overview-ui))

(defun lektion-overview-ui ()
  (within-main-loop
    (let-ui (gtk-window
             :type :toplevel
             :title "Vokabeltrainer - Lektionsübersicht"
             :default-width 500
             :default-height 300
             :var window
             (v-box
              (h-button-box
               (button :label "Trainieren"  :var train-button)  :expand nil
               (button :label "Korrigieren" :var change-button) :expand nil
               (button :label "Neu anlegen" :var new-button)    :expand nil
               (button :label "Löschen"     :var del-button)    :expand nil
               (label)) :expand nil
              (scrolled-window
               :hscrollbar-policy :never
               :vscrollbar-policy :automatic
               (tree-view :var view))))
      (let ((lektionen-store (make-store 'lektionen)))
        ;; connect model to view
        (setf (tree-view-model view) lektionen-store)
        ;; Button presses
        (on-clicked change-button
          (aif (tree-view-selected-row view)
               (progn
                 #1=(setf (widget-sensitive window) nil)
                 (entry-ui window (store-item lektionen-store it)))))
        (on-clicked new-button
          (let ((l (make-instance 'lektion)))
            (store-add-item lektionen-store l)
            ;; propagate changes to persistance
            #5=(save-lektionen)
            #1#
            (entry-ui window l)))
        ;; TODO
        #|(on-clicked del-button
          (aif (tree-view-selected-row view)
               (progn
                 (store-remove-item lektionen
                                   (store-item lektionen-store it))
                 #5# ; propagate
                 )))|#
        ;; TODO
        (on-clicked train-button
          (aif (tree-view-selected-row view)
               (progn
                 #1#
                 (voc-train-ui window (store-item lektionen-store it))))))
      ;; configure tree view
      (setup-tree-view 'lektionen view)
      (default-destroy window)
      (widget-show window))))

(defun entry-ui (parent lektion)
  "Gebe Vokabeln in eine Liste ein."
  (within-main-loop
    ;; the general layout of the entry ui
    (let-ui (gtk-window
             :type :toplevel
             :title "Vokabeleingabe"
             :default-width 500
             :default-height 700
             :transient-for parent
             :var window
             (v-box
              (h-box
               (entry  :primary-icon-tooltip-text "Lektion" :text (name lektion) :var lektion-name)
               (label :label " ")     ; use a label as a simple spacer
               (button :label "Ändern" :var edit-button) :expand nil
               (button :label "Löschen" :var del-button) :expand nil) :expand nil
              (scrolled-window
               :hscrollbar-policy :never
               :vscrollbar-policy :automatic
               (tree-view :var view))
              (h-box
               (entry :primary-icon-tooltip-text "Dansk"   :var dansk-entry)
               (entry :primary-icon-tooltip-text "Deutsch" :var deutsch-entry)
               (button :label "Neu" :var save-button) :expand nil) :expand nil))
      ;; storage and state
      (ensure-adjustable-array (vokabeln lektion))
      (let ((vokabeln-store (make-store 'vokabel-entry (vokabeln lektion)))
            (modus :neu))
        ;; connect the model to the view
        (setf (tree-view-model view) vokabeln-store)
        ;; Neue Vokabel/Ändern
        (on-clicked save-button 
          ;; alter/insert data
          (if (eq modus :neu)
              ;; create new item
              (store-add-item vokabeln-store
                              (make-instance 'vokabel
                                             :dansk   #1=(entry-text dansk-entry)
                                             :deutsch #2=(entry-text deutsch-entry)))
              ;; store new values
              (setf (dansk modus)   #1#
                    (deutsch modus) #2#
                    ;; reset modus var and button label
                    (button-label save-button) "Neu"
                    modus :neu))
          ;; reset inputs
          (setf #1# ""
                #2# "")
          #3=(widget-grab-focus dansk-entry))
        ;; Änderung einleiten
        (on-clicked edit-button 
          ;; find out which rows are selected
          (aif (tree-view-selected-row view)
               (setf modus (store-item vokabeln-store it)
                     ;; Change button label
                     (button-label save-button) "Speichern"
                     ;; load current values to text entries
                     #1# (dansk modus)
                     #2# (deutsch modus)))
          #3#)
        ;; TODO Vokabel löschen
        (on-clicked del-button 
          (aif (tree-view-selected-row view)
               (store-remove-item vokabeln-store
                                  (store-item vokabeln-store it)))
          #3#)
        ;; on closing the window, move the edits back to the lektion.
        (on-destroy window
          (setf (name lektion) (entry-text lektion-name)
                (widget-sensitive parent) t)
          (save-lektionen)
          (leave-gtk-main)))
      ;; setup the tree view
      (setup-tree-view 'vokabel-entry view)
      ;; focus input
      #3#
      (widget-show window))))
