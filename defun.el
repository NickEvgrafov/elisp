(defun count-words-in-defun()
  "Возвращает число слов и символов Лиспа в функции."

  (beginning-of-defun)
  (let ((count 0)
        (end (save-excursion (end-of-defun) (point))))
    (while (and (< (point) end)
                (re-search-forward "\\(\\w\\|\\s_\\)+[^ \t\n]*[ \t\n]*" end t))
      (setq count (1+ count)))
    count         
    ))

(defun lengths-list-file (filename)
  "Возвращает список длин функций содержащихся в ФАЙЛЕ.
Возвращаемый список --- это список чисел.
Каждое число --- количество слов или символов Лиспа
в одном определении функции."
  
  (message "Анализируем `%s' ... " filename)
  (save-excursion
    (let ((buffer (find-file-noselect filename))
          (lengths-list))

      (set-buffer buffer)
      (setq buffer-read-only t)
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "^.defun" nil t)
        (setq lengths-list
              (cons (count-words-in-defun) lengths-list)))
      
      (kill-buffer buffer)
      lengths-list)))

(defun lengths-list-many-files (list-of-files)
  "Возвращает список длин функций в LIST-OF-FILES."

  (let (lengths-list)
    (while list-of-files
      (setq lengths-list
            (append lengths-list (lengths-list-file (expand-file-name (car list-of-files)))))
      (setq list-of-files (cdr list-of-files)))

    lengths-list))


(defun recursive-lengths-list-many-files (list-of-files) 
  "Возвращает список длин функций из LIST-OF-FILES."
  (if list-of-files                     ; рекурсивная-проверка
      (append
       (lengths-list-file
        (expand-file-name (car list-of-files)))
       (recursive-lengths-list-many-files
        (cdr list-of-files)))))

(lengths-list-file "/usr/share/emacs/24.5/lisp/files.el.gz")
(lengths-list-file "/usr/share/emacs/24.5/lisp/ses.el.gz")

(setq lengths (recursive-lengths-list-many-files '("/usr/share/emacs/24.5/lisp/ses.el.gz" "/usr/share/emacs/24.5/lisp/files.el.gz"))
