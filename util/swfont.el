;;; swfont.el
;;;
;;; SW.ttf to shuowen.ttf 変換スクリプト
;;; SW.ttf の独自文字コードを、一般の文字コードに変換する。
;;;
;;; 参考：http://d.hatena.ne.jp/mashabow/20080520/1211290544

;; customization
(defvar swfont-original-ttf (expand-file-name "~/.fonts2/shuowen/SW.ttf"))
(defvar swfont-output-ttf   (expand-file-name "~/.fonts2/shuowen/shuowen.ttf"))
(defvar swfont-working-dir  "/tmp/shuowen")
(defvar swfont-fontforge    (executable-find "/usr/bin/fontforge"))
(defvar swfont-script-file  "/tmp/shuowen/tmp.script")
(defvar swfont-data-file    (expand-file-name "~/cvs/kanji-database/data/swfont.txt"))

;; misc variables
(defvar swfont-table (make-hash-table))
(defvar swfont-export-script 
  "Open($1);
   Select(0x4E00,0x796E);
   Export(\"%s/SW-%%U.svg\");
  ")
(defvar swfont-import-script
  "_fontname = \"%s\"
   _importfile = \"%s/u*.svg\"
   New()
   # .notdef作成
   Select(0x0000)
   SetWidth(1000)
   SetGlyphName(\".notdef\")
   # エンコードにUnicodeを指定
   Reencode(\"unicode\")
   # SVGをすべてインポート
   Print(\"import...\")
   Import(_importfile, 0)
   SelectAll()
   DontAutoHint()
   # 整数値に丸める
   RoundToInt()
   # 全角スペース作成
   # Select(0u3000)
   # SetWidth(1000)
   # フォント情報設定
   SetFontNames(\"Shuowen\",\
                \"Shuowen\",\
                \"Shuowen\",\
                \"Medium\",\
                \"Created from SW.ttf, with FontForge 2.0 (http://fontforge.sf.net)\",\
                \"1.00\") 
   SetTTFName(0x411, 1, \"Shuowen\")
   SetTTFName(0x411, 4, \"Shuowen\")
   # SFD書き出し
   Save(_fontname:t:r + \".sfd\")
   Print(_fontname:t:r + \".sfd\")
   # OTF生成
   Generate(_fontname, \"\", 0x94)
   Print(\"generated: \"+ _fontname)
   Close()
   Quit()
   ")

(defun swfont-generate-ttf ()
  (interactive)
  (if (not (file-directory-p swfont-working-dir))
      (make-directory swfont-working-dir))
  (if (not (file-exists-p swfont-fontforge))
      (error "Fontforge not found."))
  (if (not (file-exists-p swfont-data-file))
      (error "swfont.txt not found."))
  ;; swfont.txt
  (with-temp-buffer 
    (insert-file-contents swfont-data-file)
    (goto-char (point-min))
    (while (re-search-forward "^\\([0-9A-F]+\\)	@?[0-9]+A?	\\([㐀-𯿿]+\\)" nil t)
      (let ((sw (string-to-number (match-string 1) 16))
            (chars (string-to-list (match-string 2))))
      (dolist (char chars)
        (puthash char sw swfont-table)))))
  ;; generate svg files
  (with-temp-file swfont-script-file
    (insert (format swfont-export-script swfont-working-dir)))
  (call-process swfont-fontforge nil t t "-script" swfont-script-file swfont-original-ttf)
  ;; link svg files
  (maphash 
   (lambda (key val) 
     (call-process "/bin/ln" nil nil t 
                   (format "%s/SW-%X.svg" swfont-working-dir val)
                   (format "%s/u%X.svg" swfont-working-dir key)))
     ;;(make-symbolic-link (format "%s/SW-%X.svg" swfont-working-dir val)
     ;;                    (format "%s/u%X.svg" swfont-working-dir key)))
   swfont-table)
  ;; generate font files
  (with-temp-file swfont-script-file
    (insert (format swfont-import-script swfont-output-ttf swfont-working-dir)))
  (call-process swfont-fontforge nil t t "-script" swfont-script-file)
  )
