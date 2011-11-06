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

(create-standard-print-object vokabel dansk deutsch (richtig falsch))

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

(defun entry-ui ()
  "Gebe Vokabeln in eine Liste ein."
  (within-main-loop
    ;; the general layout of the entry ui
    (let-ui (gtk-window
             :type :toplevel
             :title "Vokabeleingabe"
             :default-width 500
             :default-height 700
             :var window
             (v-box
              (h-box
               (entry  :primary-icon-tooltip-text "Lektion" :text "Lektion ??" :var lektion-name)
               (label :label "  ")
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
           (dbug "~A" (gtk::store-items vokabeln))
           ;; reset inputs
           (setf #1# ""
                 #2# "")))
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
                      #2# (deutsch modus)))))
        ;; Vokabel löschen
        (connect-signal
         del-button "clicked"
         (ilambda (b)
           (aif (tv-selected-row view)
                (store-remove-item vokabeln
                                   (store-item vokabeln it))))))
      (add-tv-column view "Dansk" 0)
      (add-tv-column view "Deutsch" 1)
      (connect-signal window "destroy" (ilambda (w) (leave-gtk-main)))
      (widget-show window))))
