(require 'websocket)

;;; Code:

;; save the server to list
(setq ewsw-ws-server nil)

;;; ws client list
(setq ewsw-ws-clients nil)

(defmacro when-server-status (condition &rest body)
  "Exec body form when server-status is correct.  CONDITION: nothing."
  `(if ,condition
       (progn ,@body)))

(defmacro when-server-open (&rest body)
  "exec body form when server is not nil"
  `(when-server-status (not (null ewsw-ws-server))
                       ,@body))

(defmacro when-server-close (&rest body)
  "execute when server is nil"
  `(when-server-status (null ewsw-ws-server)
                       ,@body))

;;; 将一个客户端的 ws 对象存入列表
(defun add-client-to-list (ws-client)
  "add client to list"
  (push ws ewsw-ws-clients))

(defun decode-json-string (json-msg)
  "read string from client msg and decode it to assoc list"
  (if (not (stringp json-msg))
      json-msg
    (progn
      (require 'json)
      (let ((json-key-type 'string))
        (json-read-from-string json-msg)))))


(defun client-msg-div (msg)
  "received msg from client and handler it by keyword"
  (let* ((json-list (decode-json-string msg))
         (op (assoc-default "op" json-list))
         (data (assoc-default "data" json-list)))
    (cond
     ((equal op "display-data") (ewsw-display-cgylr-search-result data))
     ((equal op "display-update-result") (ewsw-display-cgylr-update-result data))
     ((equal op "display-glgj-deleteyw-result") (ewsw-display-glgj-deleteyw-result data))
     )))

(defmacro ewsw-display-data-from-client (&rest body)
  "show the data message"
  `(save-excursion
     (set-buffer (get-buffer-create "*ewsw*"))
     (erase-buffer)
     (insert (format "author: Shjanken"))
     (insert "\n")
     (insert (format "janken.wang@hotmail.com"))
     (insert "\n\n")
     (display-buffer "*ewsw*")
     ,@body
     ))

(defun ewsw-close-ewsw-buffer ()
  "close the *ewsw* buffer"
  (interactive)
  (save-excursion
    (delete-windows-on "*ewsw*")
    (kill-buffer "*ewsw*")))

(defun ewsw-insert-data (format-str key value)
  "insert the text to current buffer,
the Text like: tmh: 1912309180293"
  (insert (format format-str key value)))

(defun ewsw-display-cgylr-search-result (data)
  (ewsw-display-data-from-client
   (ewsw-insert-data "\t%s\t%s\n" "条码号" (assoc-default "tmh" data))
   (ewsw-insert-data "\t%s\t%s\n" "流水号" (assoc-default "lsh" data))
   (ewsw-insert-data "\t%s\t%s\n" "号牌号码" (assoc-default "hphm" data))
   (ewsw-insert-data "\t%s\t%s\n" "公安预录入办理状态" (assoc-default "zt" data))
   (ewsw-insert-data "\t%s\t%s\n" "专网办理状态" (assoc-default "zw_blzt" data))
   (ewsw-insert-data "\t%s\t%s\n" "市场网预录入办理状态" (assoc-default "ylr_blzt" data))
   (let ((cg-ls (assoc-default "cg_ls" data))) ; 如果有公安状态的话，就显示公安状态
     (if (> (length cg-ls) 0)
         (ewsw-insert-data "\t%s\t%s\n" "公安网办理状态" (aref (assoc-default "cg_ls" data) 0)))))
  data)

(defun ewsw-display-cgylr-update-result (data)
  ""
  (ewsw-display-data-from-client
   (insert (format "update reuslt: %s\n" data))))

(defun ewsw-display-glgj-deleteyw-result (data)
  ""
  (ewsw-display-data-from-client
   (insert (format "业务删除: %s\n" data))))


(defun ewsw-get-current-input ()
  "get current line text as input"
                                        ; (interactive)
  (let ((input (string-trim-right ; ignore right withspace
                (thing-at-point 'line))))
    input))


(defun ewsw-send-command (command)
  "send command to all clients"
  (dolist (client ewsw-ws-clients)
    (websocket-send-text client command)))


(defun ewsw-send-cgylr-search ()
  "send search cgylr info command"
  (interactive)
  (let ((command (concat "cgylr search " (ewsw-get-current-input))))
    (ewsw-send-command command)
    (message "send command to cgylr: search")
    command))


(defun ewsw-send-cgylr-update ()
  "通知网页更新更新 cg_ylr_blzt 和 zs_ylr_blzt"xcf
  (interactive)
  (ewsw-send-command "cgylr update_ytb")
  (message "send command to cgylr: update_ytb")
  "cgylr update_ytb")


(defun ewsw-send-delete-yw ()
  "使用管理工具(glgj.jsp)中的删除业务功能"
  (interactive)
  (let ((command (concat "glgj deleteyw " (ewsw-get-current-input))))
    (ewsw-send-command command)
    (message "send command to glgj: delete yw")
    command))

;;; handler functions
(defun server-on-open-handler (ws)
  "when client conntect to the server"
  (message "client connected")
  (add-client-to-list ws))

(defun ewsw-start-server ()
  "start the web socket server when the server is not start"
  (let ((server (when-server-close (websocket-server
                                    9988
                                    :host 'local
                                    :on-message (lambda (ws frame)
                                                  (client-msg-div (websocket-frame-text frame)))
                                    :on-open 'server-on-open-handler
                                    :on-close (lambda (_websocket)
                                                (message "client closed!"))))))
    (setq ewsw-ws-server server) ; save the server connection to variable
    ))


(defun ewsw-close-server ()
  "close the server when the server is started.
if the server is nil, then do nothing"
  (when-server-open
   (websocket-server-close ewsw-ws-server)
   (setq ewsw-ws-server nil)
   (dolist (_client ewsw-ws-clients)
     (pop ewsw-ws-clients))))


(provide 'ewsw)
