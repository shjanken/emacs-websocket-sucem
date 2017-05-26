(provide 'ewsw)

(require 'websocket)

;;; ws server connection
(setq ewsw-ws-server nil)

(defmacro when-server-status (condition &rest body)
  "exec body form when server-status is correct"
  'TODO)

(defmacro when-server-start (&rest body))
