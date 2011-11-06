(defsystem :voc
  :depends-on (ol-utils
               cl-gtk2-gtk
               cl-who
               cxml)
  :serial t
  :components ((:file "voc-format")
               (:file "voc-entry")
               (:file "voc-train")))
