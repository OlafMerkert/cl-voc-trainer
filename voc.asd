(defsystem :voc
  :depends-on (ol-utils
               ltk
               cl-who
               cxml)
  :serial t
  :components ((:file "voc-format")
               (:file "voc-entry")
               (:file "voc-train")))
