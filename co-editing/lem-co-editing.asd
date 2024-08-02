(defsystem "lem-co-editing/common"
  :serial t
  :depends-on ("lem" "frugal-uuid" "jsonrpc")
  :components ((:file "utils")))

(defsystem "lem-co-editing/client"
  :serial t
  :depends-on ("lem-co-editing/common")
  :components ((:file "client")))

(defsystem "lem-co-editing/server"
  :serial t
  :depends-on ("lem-co-editing/common")
  :components ((:file "server")))

(defsystem "lem-co-editing"
  )
