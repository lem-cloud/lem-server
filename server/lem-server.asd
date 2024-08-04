(defsystem "lem-server"
  :depends-on ("ningle"
               "clack"
               "lem-co-editing/server"
               "jsonrpc/transport/websocket"
               "jsonrpc/transport/local-domain-socket"
               "com.inuoe.jzon"
               "lack-middleware-accesslog")
  :serial t
  :components ((:file "replica")
               (:file "http-server")
               (:file "main")))

(defsystem "lem-server/executable"
  :build-operation program-op
  :build-pathname "../lem-server"
  :entry-point "lem-server:main"
  :depends-on ("lem-server"))
