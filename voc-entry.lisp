(defpackage :voc-entry
  (:use :cl :gtk :gdk :gobject :ol
        :cl-prevalence)
  (:shadowing-import-from :ol :range)
  (:shadowing-import-from :cl-prevalence :name))

(in-package :voc-entry)

;;; Persistenz mit cl-prevalence

(defparameter *prevalence-dir* #P "~/Studium/Dänisch I/vokabeln2/")
(defparameter *default-store* nil)

(defun load-data ()
  (ensure-directories-exist *prevalence-dir*)
  (unless *default-store*
    ;; init prevalence
    (setf *default-store*
          (make-prevalence-system *prevalence-dir*))
    ;; initiliase the lektionen root object, if it is not available yet
    (unless #5=(get-root-object *default-store* :lektionen)
            (setf #5# (make-array 2 :adjustable t :fill-pointer 0)))))

;;; Oberfläche für die Eingabe

(defclass persistent-base ()
  ((id :accessor persistent-id)))

(defclass vokabel (persistent-base)
  ((dansk :accessor dansk
          :initarg  :dansk
          :initform "")
   (deutsch :accessor deutsch
            :initarg  :deutsch
            :initform "")
   (richtig :accessor richtig
            :initform 0)
   (falsch :accessor falsch
           :initform 0)))

(defclass lektion (persistent-base)
  ((name :accessor name
         :initform "Lektion ??")
   (vokabeln :accessor vokabeln
             :initform (make-array 10 :adjustable t :fill-pointer 0))))

(create-standard-print-object vokabel
                              dansk "-" deutsch (richtig falsch))
(create-standard-print-object lektion name)

(defun lektion-voc-count (lektion)
  (length (vokabeln lektion)))

(defun make-vokabel-entry-store ()
  (let ((store (make-instance 'array-list-store)))
    (store-add-column store "gchararray" #'dansk)
    (store-add-column store "gchararray" #'deutsch)
    store))

(defun make-lektion-store ()
  (let ((store (make-instance 'array-list-store)))
    (store-add-column store "gchararray" #'name)
    (store-add-column store "gint" #'lektion-voc-count)
    ;; populate from prevalence
    (setf (slot-value store 'gtk::items)
          (get-root-object *default-store* :lektionen))
    store))

(defun add-tv-column (view title col-index)
  (let ((column (make-instance 'tree-view-column :title title))
        (renderer (make-instance 'cell-renderer-text)))
        (tree-view-column-pack-start column renderer)
        (tree-view-column-add-attribute column renderer "text" col-index)
        (tree-view-append-column view column)))

(defun tv-selected-row (view)
  (let ((row-paths (tree-selection-selected-rows (tree-view-selection view))))
    (when row-paths
      (first (tree-path-indices (first row-paths))))))

(defmacro on-clicked (button &body body)
  `(connect-signal ,button "clicked"
                   (ilambda (b) ,@body)))

(defun voc-entry ()
  (load-data)
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
               (button :label "Trainieren" :var train-button) :expand nil
               (button :label "Korrigieren" :var change-button) :expand nil
               (button :label "Neu anlegen" :var new-button) :expand nil
               (button :label "Löschen" :var del-button) :expand nil
               (label) 
               (button :label "Sichern" :var save-button) :expand nil) :expand nil
              (scrolled-window
               :hscrollbar-policy :never
               :vscrollbar-policy :automatic
               (tree-view :var view))))
      (let ((lektionen (make-lektion-store)))
        ;; connect model to view
        (setf (tree-view-model view) lektionen)
        ;; Button presses
        (on-clicked change-button
          (aif (tv-selected-row view)
               (progn
                 #1=(setf (widget-sensitive window) nil)
                 (entry-ui window (store-item lektionen it)))))
        (on-clicked new-button
          (let ((l (make-instance 'lektion)))
            (store-add-item lektionen l)
            ;; propagate changes to persistance
            #5=(setf (get-root-object *default-store* :lektionen)
                     (gtk::store-items lektionen))
            #1#
            (entry-ui window l)))
        (on-clicked del-button
          (aif (tv-selected-row view)
               (progn
                 (store-remove-item lektionen
                                   (store-item lektionen it))
                 #5# ; propagate
                 )))
        ;; TODO
        #|(on-clicked train-button)|#
        (on-clicked save-button
          #5# ; propagate
          ;; write persistance data to disk
          (snapshot *default-store*)))
      ;; configure tree view
      (add-tv-column view "Lektionstitel" 0)
      (add-tv-column view "Anzahl Vokabeln" 1)
      (connect-signal window "destroy" (ilambda (w) (leave-gtk-main)))
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
      (let ((vokabeln (make-vokabel-entry-store))
            (modus :neu))
        ;; populate the store
        (setf (slot-value vokabeln 'gtk::items) (vokabeln lektion))
        ;; connect the model to the view
        (setf (tree-view-model view) vokabeln)
        ;; Neue Vokabel/Ändern
        (on-clicked save-button 
          ;; alter/insert data
          (if (eq modus :neu)
              ;; create new item
              (store-add-item vokabeln
                              (make-instance 'vokabel
                                             :dansk   #1=(entry-text dansk-entry)
                                             :deutsch #2=(entry-text deutsch-entry)))
              ;; store new values
              (setf (dansk modus) #1#
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
          (aif (tv-selected-row view)
               (setf modus (store-item vokabeln it)
                     ;; Change button label
                     (button-label save-button) "Speichern"
                     ;; load current values to text entries
                     #1# (dansk modus)
                     #2# (deutsch modus)))
          #3#)
        ;; Vokabel löschen
        (on-clicked del-button 
          (aif (tv-selected-row view)
               (store-remove-item vokabeln
                                  (store-item vokabeln it)))
          #3#)
        ;; on closing the window, move the edits back to the lektion.
        (connect-signal
         window "destroy"
         (ilambda (w)
           (setf (name lektion) (entry-text lektion-name)
                 (vokabeln lektion) (gtk::store-items vokabeln)
                 (widget-sensitive parent) t)
           (leave-gtk-main))))
      ;; setup the tree view
      (add-tv-column view "Dansk" 0)
      (add-tv-column view "Deutsch" 1)
      ;; focus input
      #3#
      (widget-show window))))
