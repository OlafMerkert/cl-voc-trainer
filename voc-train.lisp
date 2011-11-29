(in-package :voc-entry)

(define-modify-macro minf (&rest args) min)

(setf *random-state* (make-random-state t))

(defparameter *group-size* 12)
(defparameter *group-iterations* 2)

(defclass training-session ()
  ((lektion :accessor lektion
            :initarg  :lektion)
   (voc-box :accessor voc-box)
   (run :accessor run
        :initform 0)
   (pointer :accessor pointer
            :initform 0)))

;;; TODO modelliere einen Karteikasten, in dem die Vokabeln sortiert werden.


(defun random-permute-subvector (vector start end)
  (unless (= start end)
   (let* ((n (- end start))
          (ra (random (ccb:factorial n)))
          (perm (ccb:permutation-unrank n ra))
          (tmp-arr (subseq vector start end)))
     (loop for i from start below end
        and j from 0 do
          (setf (aref vector i)
                (aref tmp-arr (1- (aref perm j)))))
     vector)))

(defun least-known-vocs (voc-list)
  (let* ((voc-vector        (list->array voc-list))
         (sorted-voc-vector (sort voc-vector #'< :key #'confidence))
         (size              (min *group-size* (length sorted-voc-vector))))
    ;; mix up the vocs with the lowest score
    (random-permute-subvector sorted-voc-vector 0 size)
    (subseq sorted-voc-vector 0 size)))

(defmethod reload-voc-box ((training-session training-session))
  (with-slots (voc-box lektion run pointer) training-session
    (setf voc-box
          (least-known-vocs (vokabeln lektion))
          pointer 0
          run 0)))

(defmethod initialize-instance :after ((training-session training-session) &key)
  (reload-voc-box training-session))

(defmethod confidence ((vokabel vokabel))
  (with-slots (richtig falsch) vokabel
    (- richtig
       falsch)))

(defmethod confidence ((lektion lektion))
  (loop
     for voc across (vokabeln lektion)
     minimize (confidence voc)))

;; so viel mehr richtige als falsch
(defparameter *conf-factor* 2.5)

(defun random-elt (seq)
  "return a random element from the sequence."
  (elt seq
       (random (length seq))))

;; todo use a more sophisticated strategy for asking vocs

(defun tillitstrue (fn)
  "Rufe FN auf, bis es nicht mehr NIL zurückgibt und liefere diese
Rückgabe."
  (do ((x (funcall fn) (funcall fn)))
      (x x)))

(defmethod next-voc ((training-session training-session))
  (with-slots (voc-box pointer run) training-session
    ;; teste auf pointer Überlauf
    (when (>= pointer (length voc-box))
      (setf pointer 0)
      (incf run))
    ;; teste auf run Überlauf
    (when (>= run *group-iterations*)
      (setf voc-box
            (least-known-vocs (vokabeln (lektion training-session)))
            pointer 0
            run 0))
    ;; Rückgabe der richtigen Vokabel
    (let ((voc (aref voc-box pointer)))
      (incf pointer)
      (format t "~&Confidence: ~A~%" (confidence voc))
      voc)))

(defun voc-train-ui (parent lektion)
  (within-main-loop
    (let-ui (gtk-window
             :type :toplevel
             :title "Vokabeltrainer - Abfrage"
             :default-width 400
             :default-height 200
             :var window
             (table :var tab))
      (let ((lname            (make-instance 'label            :label (name lektion)))
            (label-1          (make-instance 'label            :label "Abfragen"))
            (da-button        (make-instance 'radio-button     :label "Dansk"))
            (de-button        (make-instance 'radio-button     :label "Deutsch"))
            (label-2          (make-instance 'label            :label "Dansk:"))
            (label-3          (make-instance 'label            :label "Deutsch:"))
            (da-display       (make-instance 'label            :label ""
                                                               :use-markup t
                                                               :xalign 0.3))
            (de-display       (make-instance 'label            :label ""
                                                               :use-markup t
                                                               :xalign 0.3))
            (correct-button   (make-instance 'button           :label "korrekt"))
            (wrong-button     (make-instance 'button           :label "falsch"))
            (next-button      (make-instance 'button           :label "weiter"))
            (confidence-label (make-instance 'label            :label ""
                                                               :xalign 0.8))
            ;; zustand
            (current-voc nil)
            (training-session (make-instance 'training-session :lektion lektion)))
        ;; group the language selectors
        (setf (radio-button-group de-button) (list da-button))
        ;; Zeile 1
        (table-attach tab lname            0 2 0 1)
        (table-attach tab confidence-label 2 3 0 1)
        ;; Zeile 2
        (table-attach tab label-           1 0 1 1 2)
        (table-attach tab da-button        1 2 1 2)
        (table-attach tab de-button        2 3 1 2)
        (table-attach tab label-           2 0 1 3 4)
        (table-attach tab da-display       1 3 3 5)
        (table-attach tab label-           3 0 1 5 6)
        (table-attach tab de-display       1 3 5 7)
        ;; Button-Zeile
        (table-attach tab correct-button   0 1 7 8 :x-options nil :y-options nil)
        (table-attach tab wrong-button     1 2 7 8 :x-options nil :y-options nil)
        (table-attach tab next-button      2 3 7 8 :x-options nil :y-options nil)

        (labels ((display (label text)
                   (setf (label-label label)
                         (format nil "<span font=\"24\">~A</span>" text)))
                 (ask-dansk ()
                   (toggle-button-active da-button))
                 (ask-next-voc ()
                   (setf current-voc (next-voc training-session)
                         ;; reset confidence display
                         (label-label confidence-label) "")
                   (if (ask-dansk)
                       (progn
                         (display da-display (dansk current-voc))
                         (display de-display ""))
                       (progn
                         (display da-display "")
                         (display de-display (deutsch current-voc)))))
                 (show-solution ()
                   (when current-voc
                     (setf (label-label confidence-label)
                           (format nil "~A" (confidence current-voc)))
                     (display da-display (dansk current-voc))
                     (display de-display (deutsch current-voc))))
                 (correct-voc ()
                   (when current-voc
                     (incf (richtig current-voc))))
                 (wrong-voc ()
                   (when current-voc
                     (incf (falsch current-voc))))
                 (sens-buttons (s)
                   ;; enable/disable buttons
                   (setf (widget-sensitive correct-button) (not s)
                         (widget-sensitive wrong-button) (not s)
                         (widget-sensitive next-button) s)
                   ;; and focus accordingly
                   (widget-grab-focus (if s next-button
                                          correct-button))))
          ;; init button state
          (ask-next-voc)
          (sens-buttons t)
          (on-clicked correct-button
                      (correct-voc)
                      (ask-next-voc)
                      (sens-buttons t))
          (on-clicked wrong-button
                      (wrong-voc)
                      (ask-next-voc)
                      (sens-buttons t))
          (on-clicked next-button
                      (show-solution)
                      (sens-buttons nil))
          (on-clicked da-button
                      (ask-next-voc)
                      (sens-buttons t))
          (on-clicked de-button
                      (ask-next-voc)
                      (sens-buttons t))) 
        ;; anzeigen
        (connect-signal
         window "destroy"
         (ilambda (w)
           (setf (widget-sensitive parent) t)
           (leave-gtk-main))))
      (widget-show window))))
