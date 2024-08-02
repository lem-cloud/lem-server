(defsystem "lem-replica"
  :depends-on ("lem"
               "lem/extensions"
               "lem-co-editing/client"
               "jsonrpc"
               "trivial-utf-8"
               "jsonrpc/transport/stdio"
               "jsonrpc/transport/websocket"
               "jsonrpc/transport/local-domain-socket"
               "command-line-arguments"
               "ningle"
               "com.inuoe.jzon"
               "cl-ini")
  :serial t
  :components ((:file "jsonrpc-stdio-patch")
               (:file "utils")
               (:file "view")
               (:file "permission")
               (:file "user")
               (:file "idle-time")
               (:file "main")))

(defsystem "lem-replica/executable"
  :build-operation program-op
  :build-pathname "../lem-replica"
  :entry-point "lem-replica:main"
  :depends-on ("lem-replica"))
