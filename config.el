;;; config.el -*- lexical-binding: t; -*-

;; Personal info
(setq user-full-name "hmanhng"
      user-mail-address (concat "hmanhng" "@" "icloud" "." "com"))

;; Set the default GPG key ID, see "gpg --list-secret-keys"
;; (setq-default epa-file-encrypt-to '("XXXX"))

;; Theme
(setq minemacs-theme 'doom-solarized-light) ; `doom-one' is a dark theme, `doom-one-light' is the light one

;; Fonts
(plist-put minemacs-fonts-plist
           :default ;; <- applies to the `default' face using `custom-theme-set-faces'
           '((:family "IBM Plex Mono" :height 170) ; <- priority 1
             (:family "Maple Mono" :height 160 :weight light) ; <- priority 2
             (:family "SF Pro Display" :height 150 :weight semi-light))) ; <- priority 3

(setq-default
 ;; Better support for files with long lines
 bidi-paragraph-direction 'left-to-right
 ;; Speeds redisplay, may break paranthesis rendering for bidirectional files
 bidi-inhibit-bpa t)

;; Format
;; Hack from Doom-Emacs
(cl-defun set-formatter! (name args &key modes)
  (declare (indent defun))
  (cl-check-type name symbol)
  (with-eval-after-load 'apheleia
    (if (null args)
        (progn
          (setq apheleia-formatters
                (assq-delete-all name apheleia-formatters))
          (while (rassoc name apheleia-mode-alist)
            (setq apheleia-mode-alist
                  (assq-delete-all (car (rassoc name apheleia-mode-alist)) apheleia-mode-alist))))
      (let ((formatter (cond
                        ((listp args) `(,@args))
                        (t args))))
        (setf (alist-get name apheleia-formatters) formatter))
      (when modes
        (dolist (mode modes)
          (setf (alist-get mode apheleia-mode-alist) name))))))

(with-eval-after-load 'nix-ts-mode
  (set-formatter! 'alejandra '("alejandra" "-q" "-") :modes '(nix-ts-mode)))
