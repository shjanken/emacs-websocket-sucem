;;; or 可以用来检查 &optional 的参数
;;; 如果参数为空，则添加一个默认值

(defun hello-world (&optional name)
  (or name (setq name "foo"))
  (message "Hello, %s" name))

(hello-world)
"Hello, foo"

(hello-world "world")
"Hello, world"

;;; 在 elisp 中，所有的数据都是有类型的
;;; 包括： 数字，浮点数，cons, 符号，字符串，向量，hash-table, subr（内建函数，比如 cons, if, and 之类）, byte-function, 其他特殊类型（比如 buffer）


;;; int
#b101100
44
#o54 ;; 八进制
44

#x2c ;;十六进制
44

;;; 有 numberp integerp floatp 来进行测试

(/ 5 6.0)
0.8333333333333334


;;; 空字符串小于所有字符串，所以可以使用这个特性定义一个字符串的判空方法
(defun string-emtpyp (str)
  "return t while string is empty"
  (string< "" str))

;;; cons
(cons 1 '(1 . 2))

;;; 使用 cons 来追加值的时候时不改变原来的列表的
(setq lst '(1 2 3))
(cons 'x lst)
lst

;;; 使用 push 时原地改变的
(push 'x lst)
lst

;;; 取得一个列表的第 n 项
(defun my-nth (lst n)
  (let ((current 0)
		(current-cdr nil))
	(while (not (= current n))
	  (setq current-cdr (cdr lst))
	  (setq current (1+ current)))
	(car (cdr current-cdr))))

(my-nth '(1 2 3 4) 2)

(car '(1 2 3 4))
(cdr '(1 2 3 4))

;;; 用 列表 当作 关联列表使用
(setq assoc-lst '(("name" "shjanken") ("sex" "man")))
(assoc-default "name" assoc-lst)


;;; symbol

(setq foo (make-vector 10 0))
(intern-soft "abc" foo)
(intern "abc" foo)
foo

;;; play with buffer

(setq foo "I am global variable!") ; global
(make-local-variable 'foo) ; local the values is globa
foo
"I am global variable!"


(setq foo "I'm buffer-local variable!") ; local
foo
"I'm buffer-local variable!"
(with-current-buffer "*Messages*" foo) ; on anather buffer.no local var,so the value is global


(set-buffer "*Messages*")
(message (buffer-name)) ;; set-buffer dont switch the current buffer

(progn
  (set-buffer "*Messages*")
  (message (buffer-name))) ; if `set-buffer' in the `progn', then will switch the bufffer in backgground


;;; save-excursion

;; in save-excursion. when the function returned, the mark and point will set to original
(save-excursion
  (set-buffer (current-buffer))
  (goto-char (point-min)))

;; use save-current-buffer, when executed the end of function
;; the mark and point will not original
(save-current-buffer
  (set-buffer (current-buffer))
  (goto-char (point-min)))

(current-buffer)
#<buffer el-sinnpet.el>


;; other buffer function

(buffer-live-p (set-buffer "*Messages*"))
t

(buffer-live-p (current-buffer "shjanken"))
nil

(buffer-list)
(#<buffer el-sinnpet.el> #<buffer  *Minibuf-1*> #<buffer *scratch*> #<buffer ws.el> #<buffer *eshell*> #<buffer *Packages*> #<buffer  *Minibuf-0*> #<buffer *Messages*> #<buffer  *code-conversion-work*> #<buffer  *Echo Area 0*> #<buffer  *Echo Area 1*> #<buffer  *tip*> ...)

;; use with-temp-buffer to set the buffer what will close when the end
(with-temp-buffer
  (make-local-variable 'foo)
  (setq foo 'done)
  (print foo))
done

foo
nil

;;; mark and postion
(setq foo (make-marker)) ; 设置 mark 变量
(set-marker foo (point)) ; 给 mark 变量设置 point
foo
#<marker at 2819 in el-sinnpet.el>


(marker-position foo) ; 取得 mark 变量的内容
2819

(marker-buffer foo) ; mark 所在的 buffer 
#<buffer el-sinnpet.el>

;; insert
;; insert text from Message buffer

(let ((messages-buffer-string (with-current-buffer "*Messages*"
								(buffer-string))))
;;; or 可以用来检查 &optional 的参数
;;; 如果参数为空，则添加一个默认值

(defun hello-world (&optional name)
  (or name (setq name \\\\\\\"foo\\\\\\\"))
  (message \\\\\\\"Hello, %s\\\\\\\" name))

(hello-world)
\\\\\\\"Hello, foo\\\\\\\"

(hello-world \\\\\\\"world\\\\\\\")
\\\\\\\"Hello, world\\\\\\\"

;;; 在 elisp 中，所有的数据都是有类型的
;;; 包括： 数字，浮点数，cons, 符号，字符串，向量，hash-table, subr（内建函数，比如 cons, if, and 之类）, byte-function, 其他特殊类型（比如 buffer）


;;; int
#b101100
44
#o54 ;; 八进制
44

#x2c ;;十六进制
44

;;; 有 numberp integerp floatp 来进行测试

(/ 5 6.0)
0.8333333333333334


;;; 空字符串小于所有字符串，所以可以使用这个特性定义一个字符串的判空方法
(defun string-emtpyp (str)
  \\\\\\\"return t while string is empty\\\\\\\"
  (string< \\\\\\\"\\\\\\\" str))

;;; cons
(cons 1 '(1 . 2))

;;; 使用 cons 来追加值的时候时不改变原来的列表的
(setq lst '(1 2 3))
(cons 'x lst)
lst

;;; 使用 push 时原地改变的
(push 'x lst)
lst

;;; 取得一个列表的第 n 项
(defun my-nth (lst n)
  (let ((current 0)
		(current-cdr nil))
	(while (not (= current n))
	  (setq current-cdr (cdr lst))
	  (setq current (1+ current)))
	(car (cdr current-cdr))))

(my-nth '(1 2 3 4) 2)

(car '(1 2 3 4))
(cdr '(1 2 3 4))

;;; 用 列表 当作 关联列表使用
(setq assoc-lst '((\\\\\\\"name\\\\\\\" \\\\\\\"shjanken\\\\\\\") (\\\\\\\"sex\\\\\\\" \\\\\\\"man\\\\\\\")))
(assoc-default \\\\\\\"name\\\\\\\" assoc-lst)


;;; symbol

(setq foo (make-vector 10 0))
(intern-soft \\\\\\\"abc\\\\\\\" foo)
(intern \\\\\\\"abc\\\\\\\" foo)
foo

;;; play with buffer

(setq foo \\\\\\\"I am global variable!\\\\\\\") ; global
(make-local-variable 'foo) ; local the values is globa
foo
\\\\\\\"I am global variable!\\\\\\\"


(setq foo \\\\\\\"I'm buffer-local variable!\\\\\\\") ; local
foo
\\\\\\\"I'm buffer-local variable!\\\\\\\"
(with-current-buffer \\\\\\\"*Messages*\\\\\\\" foo) ; on anather buffer.no local var,so the value is global


(set-buffer \\\\\\\"*Messages*\\\\\\\")
(message (buffer-name)) ;; set-buffer dont switch the current buffer

(progn
  (set-buffer \\\\\\\"*Messages*\\\\\\\")
  (message (buffer-name))) ; if `set-buffer' in the `progn', then will switch the bufffer in backgground


;;; save-excursion

;; in save-excursion. when the function returned, the mark and point will set to original
(save-excursion
  (set-buffer (current-buffer))
  (goto-char (point-min)))

;; use save-current-buffer, when executed the end of function
;; the mark and point will not original
(save-current-buffer
  (set-buffer (current-buffer))
  (goto-char (point-min)))

(current-buffer)
#<buffer el-sinnpet.el>


;; other buffer function

(buffer-live-p (set-buffer \\\\\\\"*Messages*\\\\\\\"))
t

(buffer-live-p (current-buffer \\\\\\\"shjanken\\\\\\\"))
nil

(buffer-list)
(#<buffer el-sinnpet.el> #<buffer  *Minibuf-1*> #<buffer *scratch*> #<buffer ws.el> #<buffer *eshell*> #<buffer *Packages*> #<buffer  *Minibuf-0*> #<buffer *Messages*> #<buffer  *code-conversion-work*> #<buffer  *Echo Area 0*> #<buffer  *Echo Area 1*> #<buffer  *tip*> ...)

;; use with-temp-buffer to set the buffer what will close when the end
(with-temp-buffer
  (make-local-variable 'foo)
  (setq foo 'done)
  (print foo))
done

foo
nil

;;; mark and postion
(setq foo (make-marker)) ; 设置 mark 变量
(set-marker foo (point)) ; 给 mark 变量设置 point
foo
#<marker at 2819 in el-sinnpet.el>


(marker-position foo) ; 取得 mark 变量的内容
2819

(marker-buffer foo) ; mark 所在的 buffer 
#<buffer el-sinnpet.el>

;; insert
;; insert text from Message buffer
(with-current-buffer
	(set-buffer \\\\\\\"*Messages*\\\\\\\")
  (insert (buffer-string))) ; success, insert so many text. I clean that.

;; window.

(save-selected-window
  (select-window (next-window))
  (goto-char (point-min)))

(defun show-error-message (message)
  (display-buffer (get-buffer-create "*swsw-error*"))
  (progn
	(set-buffer "*swsw-error*")
	(insert "swsw-error: " message)))

(show-error-message "This is test message")
