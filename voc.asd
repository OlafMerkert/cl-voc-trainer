(defsystem :voc
  :depends-on (ol-utils
               cl-gtk2-gtk
               cl-prevalence
               cl-who
               cxml
               ccb
               ol-data-utils
               cl-gtk-utils)
  :serial t
  :components ((:file "voc-format")
               (:file "voc-entry")
               (:file "voc-train")))
