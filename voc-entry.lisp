(defpackage :voc-entry
  (:use :cl :gtk :gdk :gobject :ol)
  (:shadowing-import-from :ol :range))

(in-package :voc-entry)

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

(defun make-vokabel-entry-store ()
  (let ((store (make-instance 'array-list-store)))
    (store-add-column store "gchararray" #'dansk)
    (store-add-column store "gchararray" #'deutsch)
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

(defun entry-ui (&optional lektion)
  "Gebe Vokabeln in eine Liste ein."
  (within-main-loop
    (unless lektion
      (setf lektion (make-instance 'lektion)))
    ;; the general layout of the entry ui
    (let-ui (gtk-window
             :type :toplevel
             :title "Vokabeleingabe"
             :default-width 500
             :default-height 700
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
        (connect-signal
         save-button "clicked"
         (ilambda (b)
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
           #3=(widget-grab-focus dansk-entry)))
        ;; Änderung einleiten
        (connect-signal
         edit-button "clicked"
         (ilambda (b)
           ;; find out which rows are selected
           (aif (tv-selected-row view)
                (setf modus (store-item vokabeln it)
                      ;; Change button label
                      (button-label save-button) "Speichern"
                      ;; load current values to text entries
                      #1# (dansk modus)
                      #2# (deutsch modus)))
           #3#))
        ;; Vokabel löschen
        (connect-signal
         del-button "clicked"
         (ilambda (b)
           (aif (tv-selected-row view)
                (store-remove-item vokabeln
                                   (store-item vokabeln it)))
           #3#))
        ;; on closing the window, move the edits back to the lektion.
        (connect-signal
         window "destroy"
         (ilambda (w)
           (setf (name lektion) (entry-text lektion-name)
                 (vokabeln lektion) (gtk::store-items vokabeln))
           ;; TODO remove when integrating into main app
           (leave-gtk-main))))
      ;; setup the tree view
      (add-tv-column view "Dansk" 0)
      (add-tv-column view "Deutsch" 1)
      ;; focus input
      #3#
      (widget-show window))))
