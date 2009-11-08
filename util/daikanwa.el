;; daikanwa.el; (Emacs 23 専用)
;;
;; dkw2ucs.txt 利用例。
;; バッファの現在のポイントにある文字が、大漢和辞典にあれば、大漢和番号＠巻数をモードラインに表示します。
;; ない場合、その文字がJIS X 0213にある場合は、JISの面区点番号を表示します。

(defvar display-daikanwa-mode nil)

(defvar dkw2ucs-file (expand-file-name "~/cvs/kanji-database/data/dkw2ucs.txt"))

(defvar daikanwa-mode-line-string nil
  "String to display Daikanwa in the mode line.")

(defvar daikanwa-regexp 
  (concat "^\\(D.......\\) .* DS.. " ;; 1 .. 大漢和番号
          "\\(?:\\(\\(?:U\\+..... ?\\)+\\)\\|\\(?:→ \\(D.......\\)\\)\\)?" ;; 2 .. UCS番号群・3 .. 移動先
          "\\(?:# " ;; コメント部開始
             "\\([⿰-⿻][^ \n]+ ?\\)?"      ;; 4 .. IDS表現
             "\\(\\(?:D....... ?\\)*\\)?"  ;; 5 .. 大漢和異体
             "\\(\\(?:U\\+..... ?\\)*\\)?" ;; 6 .. UCS異体
             "\\(missing.*\\)?" ;; 7 missingの有無
          "\\)?$"
          ))

(defvar daikanwa-hash nil
  "Hash table from Daikanwa to UCS.")

(defun add-char-code-property (char propname value)
  (let ((x (get-char-code-property char propname)))
    (if (not (listp x)) (setq x nil))
    (add-to-list 'x value t 'equal)
    (put-char-code-property char propname x)))

(defun parse-dkw2ucs ()
  (interactive)
  (setq daikanwa-hash (make-hash-table :test 'equal)) ;; optional
  (while (re-search-forward daikanwa-regexp nil t)
    (let* ((dkw  (match-string 1))
           (ucs  (match-string 2))
           (ucv  (match-string 6)))
      (when ucs
        (mapcar
         #'(lambda (x)
             (let* ((ch (string-to-int (substring x 2) 16))
                    (cp (car (get-char-code-property ch 'decomposition))))
               (when ch
                 (puthash dkw (union (list ch) (gethash dkw daikanwa-hash) :test 'equal) ;; optional
                          daikanwa-hash)  ;; optional
                 (add-char-code-property ch 'daikanwa dkw))
               (when cp
                 (add-char-code-property cp 'daikanwa-compat dkw))))
         (split-string ucs " " t)))
      (when ucv
        (mapcar
         #'(lambda (x)
             (let ((ch (string-to-int (substring x 2) 16)))
               (when ch
                 (add-char-code-property ch 'daikanwa-variant dkw))))
         (split-string ucv " " t))))))

;;;###autoload
(define-minor-mode display-daikanwa-mode
  "Display Daikanwa number and vol in the mode line."
  :global t
  (or global-mode-string (setq global-mode-string '("")))
  (setq global-mode-string
        (delq 'daikanwa-mode-line-string global-mode-string))
  (if (not display-daikanwa-mode)
      (remove-hook 'post-command-hook 'daikanwa-mode-line-string-update)
    ;(add-to-list 'global-mode-string 'daikanwa-mode-line-string t)
    (setq global-mode-string
          (cons (car global-mode-string) 
                (cons 'daikanwa-mode-line-string
                      (cdr global-mode-string))))
    (add-hook 'post-command-hook 'daikanwa-mode-line-string-update)))

(defun daikanwa-mode-line-string-update ()
  (interactive)
  (if (and display-daikanwa-mode (char-valid-p (char-after (point))))
      (setq daikanwa-mode-line-string (daikanwa-mode-line-string (char-after (point))))
    (setq daikanwa-mode-string nil))
  (force-mode-line-update))

(defun daikanwa-mode-line-string (char)
  (let* ((dkw (get-char-code-property char 'daikanwa))
         (dkc (get-char-code-property char 'daikanwa-compat))
         (dkv (get-char-code-property char 'daikanwa-variant)))
    (if (or dkw dkc dkv)
        (concat " " (mapconcat 'daikanwa-format dkw ",")
                (if dkc 
                    (concat ":" (mapconcat 'daikanwa-format dkc ",")))
                (if dkv
                    (concat "v" (mapconcat 'daikanwa-format dkv ",")))
                " ")
      (let* ((j1 (encode-char  char 'japanese-jisx0213.2004-1))
             (j2 (encode-char  char 'japanese-jisx0213-2)))
        (if j1 (format " J1-%02d%02d " (- (/ j1 256) 32) (- (% j1 256) 32))
          (if j2 (format " J2-%02d%02d " (- (/ j2 256) 32) (- (% j2 256) 32))))))))

(defun daikanwa-format (dkw)
  (let ((num (string-to-int (substring dkw 1 6)))
        (dsh (string-to-int (substring dkw 7 8)))
        vol)
    (cond ((< num 01450) (setq vol  1))
          ((< num 04675) (setq vol  2))
          ((< num 07411) (setq vol  3))
          ((< num 11530) (setq vol  4))
          ((< num 14415) (setq vol  5))
          ((< num 17575) (setq vol  6))
          ((< num 22678) (setq vol  7))
          ((< num 28108) (setq vol  8))
          ((< num 32804) (setq vol  9))
          ((< num 38700) (setq vol 10))
          ((< num 42210) (setq vol 11))
          ((< num 48903) (setq vol 12))
          (t (setq vol 13)))
    (if (< num 00001) (format "H%s@15" (substring dkw 3 6))
      (format "%d%s@%d" num (make-string dsh ?') vol))))

;; Main Program

(with-temp-buffer
  (message "Now loading dkw2ucs...")
  (insert-file-contents dkw2ucs-file)
  (goto-char (point-min))
  (parse-dkw2ucs))
