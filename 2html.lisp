(require :cl-ppcre)

(defvar flag 0)
(defvar flag-ul 0)
(defvar flag-ul-1 0)
(defvar flag-ol 0)
(defvar flag-ol-1 0)

(setf flag 0)
(setf flag-ul 0)
(setf flag-ul-1 0)
(setf flag-ol 0)
(setf flag-ol-1 0)

(defun createfile (filename) (make-pathname
			      :directory (directory-namestring filename)
			      :name (pathname-name filename)
			      :type "html"))

(defmacro replace-match (var var2 var3)
  `(when (cl-ppcre:all-matches-as-strings ,var line)
     (format out ,var3  (cl-ppcre:regex-replace ,var2 (car (cl-ppcre:all-matches-as-strings ,var line)) "")) t))

(defmacro emphasis-replace (var0 var1 var2)
  `(cl-ppcre:regex-replace-all
    (concatenate 'string' "\\s[\\" ,var0 "]\\b")
    (cl-ppcre:regex-replace-all
     (concatenate 'string' "\\b[\\" ,var0 "]\\s")
     ,var1
     "</span> ")
    (concatenate 'string' " <span class=\"" ,var2 "\">")))
(defmacro emphasis-replace-1 (var0 var1 var2)
  `(cl-ppcre:regex-replace-all
    (concatenate 'string' "\\s" ,var0 "\\b")
    (cl-ppcre:regex-replace-all
     (concatenate 'string' "\\b" ,var0 "\\s")
     ,var1
     "</span> ")	     
    (concatenate 'string' " <span class=\"" ,var2 "\">")))

(defmacro emphasis-replace-2 (var0 var1 var2)
  `(cl-ppcre:regex-replace-all
    (concatenate 'string' "\\s" ,var0)
    (cl-ppcre:regex-replace-all
     (concatenate 'string' ,var0 "\\b\\s")
     ,var1
     "</span> ")	     
    (concatenate 'string' " <span class=\"" ,var2 "\">")))

(defun txt-to-html (f1) 
  (with-open-file (out (createfile f1) :direction :output :if-exists :supersede)
    (format out "<link rel=\"stylesheet\" href=\"/home/billy/css/style.css\" type=\"text/css\" />")
    (with-open-file (in f1)
      (loop for line = (read-line in nil)
	 while line do (progn
			 (setf line (emphasis-replace "*" line "bold"))
			 (setf line (emphasis-replace-2 "_" line "underline"))
			 (setf line (emphasis-replace-1 "/" line "it"))
			 (setf line (emphasis-replace "+" line "del"))
			 (cond ((replace-match "^#[\+]TITLE:\\s.*" "^#[\+]TITLE:\\s" "~%<h1>~a</h1>~%"))
			       ((replace-match "^#[\+]AUTHOR:\\s.*" "^#[\+]AUTHOR:\\s" "~%<span class=\"author\">~a</span>~%"))
			       ((replace-match "^#[\+]DATE:\\s.*" "^#[\+]DATE:\\s" "~%<span class=\"date\">~a</span>~%"))
			       ((replace-match "^[\*]\\s.*" "^[\*]\\s" "~%<h2>~a</h2>~%"))
			       ((replace-match "^[\*]{2}\\s.*" "^[\*]{2}\\s" "~%<h3>~a</h3>~%"))
			       ((replace-match "^[\*]{3}\\s.*" "^[\*]{3}\\s" "~%<h4>~a</h4>~%"))
			       ((replace-match "^#[\+]begin_src\\s.*" "^#[\+]begin_src\\s" "~%<pre class=\"~a\">~%") (setf flag 1))
			       ((replace-match "^#[\+]end_src" "^#[\+]end_src" "</pre>~a~%") (setf flag 0))
			       ((replace-match 
				 "^\\s{1}-\\s.*" 
				 "^\\s{1}-\\s" 
				 (if (= flag-ul 0) 
				     "~%<ul>~%<li>~a</li>~%" 
				     (if (= flag-ul-1 0) 
					 "<li>~a</li>~%" 
					 (progn  (setf flag-ul-1 0) "</ul>~%<li>~a</li>~%"))))
				(setf flag-ul 1))
			       ((replace-match 
				 "^\\s{2}-\\s.*" 
				 "^\\s{2}-\\s" 
				 (if (= flag-ul-1 0) 
				     "<ul>~%<li>~a</li>~%" 
				     "<li>~a</li>~%")) 
				(setf flag-ul-1 1))
			       ((= flag 1) (format out "~a~%" line))

			       ((replace-match 
				 "^\\s{1}#\\s.*" 
				 "^\\s{1}#\\s" 
				 (if (= flag-ol 0) 
				     "~%<ol>~%<li>~a</li>~%" 
				     (if (= flag-ol-1 0) 
					 "<li>~a</li>~%" 
					 (progn  (setf flag-ol-1 0) "</ol>~%<li>~a</li>~%"))))
				(setf flag-ol 1))
			       ((replace-match 
				 "^\\s{2}#\\s.*" 
				 "^\\s{2}#\\s" 
				 (if (= flag-ol-1 0) 
				     "<ol>~%<li>~a</li>~%" 
				     "<li>~a</li>~%")) 
				(setf flag-ol-1 1))
			       ((= flag 1) (format out "~a~%" line))
			       
			       ((replace-match 
				 "^[ ]*$" 
				 ".*" 
				 (cond ((= flag-ul 1) (if (= flag-ul-1 1) "</ul>~%</ul>~a~%" "</ul>~a~%"))
				       ((= flag-ol 1) (if (= flag-ul-1 1) "</ol>~%</ol>~a~%" "</ol>~a~%"))
				       (t "~a")))
				(progn (setf flag-ul 0) (setf flag-ul-1 0) (setf flag-ol 0) (setf flag-ol-1 0)))
			       ((= flag 0) (format out "~%<p>~a</p>~%" line))))))))
