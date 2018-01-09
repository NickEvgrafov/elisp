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

;;(length
;; (directory-files "c:/emacs/share/emacs/25.2/lisp" t "\\.el$"))

(setq max-lisp-eval-depth 3000)

;;(sort
;;  (recursive-lengths-list-many-files
;;    (directory-files "c:/emacs/share/emacs/25.2/lisp" t "\\.el$"))
;;  '<)

	
(defvar top-of-ranges
  '(10  20  30  40  50
    60  70  80  90 100
    110 120 130 140 150
    160 170 180 190 200
    210 220 230 240 250
    260 270 280 290 300)
  "Список задающий диапазоны для `defuns-per-range'.")

(defun defuns-per-range (sorted-lengths top-of-ranges)
  "Число функций в SORTED-LENGTHS в каждом диапазоне TOP-OF-RANGES."

  (let ((top-of-range (car top-of-ranges))
        (number-within-range 0)
        defuns-per-range-list)

    (while top-of-ranges

      (while (and
              (car sorted-lengths)
              (< (car sorted-lengths) top-of-range))
        (setq number-within-range (1+ number-within-range))
        (setq sorted-lengths (cdr sorted-lengths)))

      (setq defuns-per-range-list
            (cons number-within-range defuns-per-range-list))
      (setq number-within-range 0)

      (setq top-of-ranges (cdr top-of-ranges))
      (setq top-of-range (car top-of-ranges)))

    (setq defuns-per-range-list
          (cons
           (length sorted-lengths)
           defuns-per-range-list))

    (nreverse defuns-per-range-list)))

;; (В дальнейшем мы будем работать с большими списками.)
(setq top-of-ranges        
 '(110 120 130 140 150
   160 170 180 190 200))

(setq sorted-lengths
      '(85 86 110 116 122 129 154 176 179 200 265 300 300))

(defuns-per-range sorted-lengths top-of-ranges)
