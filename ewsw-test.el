(load (expand-file-name "ewsw.el"))

(require 'ewsw)
(require 'websocket)

(ert-deftest when-server-open-test ()
  "with server is not nil, the when-server-open function will execute body,
and the when-server-close will not"
  (let ((ewsw-ws-server t))
	(should (equal (when-server-open 'done) 'done))
	(should (equal (when-server-close 'close) nil))))

(ert-deftest when-server-close-test ()
  "with server is nil, the when-server-close function will execute body,
and when-server-open function will not"
  (let ((ewsw-ws-server nil))
	(should (equal (when-server-close 'close) 'close))
	(should (equal (when-server-open 'open) nil))))

(defun init-server-and-client-status (test-function)
  "clean server and client status when test function run end"
  (unwind-protect
	  (progn (funcall test-function))
	(progn ;; tear down
	  (setq ewsw-ws-server nil)
	  (setq ewsw-ws-clients nil))))

;;; 添加一个 websocket 的客户端到列表中
(ert-deftest add-client-to-list-test ()
	(init-server-and-client-status
	 (lambda ()
	   (let ((ws 'websocket-client)) ;; mock the client
		 (add-client-to-list ws)
		 (should (equal (length ewsw-ws-clients) 1)) ; 客户端列表的长度为1
		 (should (equal (car ewsw-ws-clients) 'websocket-client))))))


;;; 准备测试需要用到的json数据
(defun test-with-json-data (json-test-function)
  (require 'json)
  (let ((json-data (json-encode '(:foo "hello" :bar "world"))))
	(funcall json-test-function json-data)))

;;; 测试解码 json 字符串的方法
(ert-deftest decode-json-string-test ()
  (test-with-json-data
   (lambda (json-msg)
	 (let ((json-result (decode-json-string json-msg)))
	   (should (equal (assoc-default "foo" json-result) "hello"))
	   (should (equal (assoc-default "bar" json-result) "world"))))))


;;; 测试输入的 json 不是字符串
(ert-deftest decode-json-string-test-not-string ()
  (let ((result (decode-json-string 'not-string-msg)))
	(should (eq result 'not-string-msg))))


;;; 测试收到客户端消息以后的消息分发方法
;; (ert-deftest client-msg-div-test ()
;;   (require 'json)
;;   (require 'el-mock)
;;   (let ((client-datas (list
;; 					   (json-encode '(:op "display-data" :data "")
;; 									'(:op "default" :data "foo")))) ; 准备测试数据
;; 	(dolist (json-data client-datas)
;; 	  (with-mock
;; 		(stub display-data-from-client => 'display-data-from-client)
;; 		(should
;; 		 (eq (client-msg-div json-data) 'display-data-from-client))
;; 		(should
;; 		 (equal (client-msg-div))))))))

(defmacro client-msg-div-test (function-name json-data-lst mock-function)
  (require 'json)
  (require 'el-mock)
  `(ert-deftest ,function-name ()
	   (let ((json-data (json-encode ,json-data-lst)))
		 (with-mock
		   (stub ,mock-function => 1)
		   (should (eq (client-msg-div json-data) 1))))))

(client-msg-div-test
 test-display-data
 '(:op "display-data" :data ((tmh . "111"))) ewsw-display-cgylr-search-result)

(client-msg-div-test
 ewsw-display-cgylr-update-result-test
 '(:op "display-update-result" :data "update_success")
 ewsw-display-cgylr-update-result)

;; (insert (format "%s" (macroexpand-1 '(client-msg-div-test
;;   ewsw-display-cgylr-update-result-test
;;   '(:op "display-update-result" :data "update_success")
;;   ewsw-display-cgylr-update-result))))
