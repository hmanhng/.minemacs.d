;;; modules.el -*- lexical-binding: t; -*-

;;; Ordered list of enabled core modules
(setq minemacs-core-modules
      '(me-splash         ; Simple splash screen
        me-keybindings    ; Keybinding (general, which-key, hydra, ...)
        me-evil           ; Emacs as Vim (evil, evil-collection, evil-snipe, evil-numbers, ...)
        me-core-ui        ; Core UI (doom-themes, modus-themes, doom-modeline, ...)
        me-completion))   ; Completion (vertico, marginalia, corfu, cape, consult, embark, ...)

;;; List of enabled modules
(setq minemacs-modules
      '(me-ui             ; User interface (svg-lib, focus, mixed-pitch, ...)
        me-editor         ; Editing (tempel, tempel-collection, rainbow-delimiters, expreg, drag-stuff, ...)
        ;; me-daemon         ; Emacs daemon tweaks
        me-undo           ; Better undoing (undo-fu, undo-fu-session, vundo, ...)
        ;; me-multi-cursors  ; Multi-cursors editing (iedit, evil-mc, evil-iedit-state, ...)
        me-vc             ; Version control (magit, forge, core-review, diff-hl, ...)
        me-project        ; Project management (consult-project-extra, ibuffer-project, ...)
        me-prog           ; Programming stuff (tree-sitter, eldoc-box, apheleia, editorconfig, ...)
        me-checkers       ; Static checkers (flymake-easy, ...)
        ;; me-debug          ; Debugging tools (realgud, disaster, ...)
        ;; me-lsp         ; LSP and DAP (lsp-mode, dap-mode, consult-lsp, lsp-pyright, ccls, ...)
        me-emacs-lisp     ; Emacs lisp development (parinfer-rust, macrostep, eros, helpful, ...)
        ;; me-common-lisp ; Common Lisp development (sly, sly-quicklisp, ...)
        ;; me-scheme      ; Scheme development (racket-mode, geiser, ...)
        ;; me-clojure     ; Clojure development (clojure-mode, cider, ...)
        ;; me-embedded    ; Embedded systems (arduino, openocd, bitbake, ...)
        ;; me-robot       ; Robotics stuff (ros, robot-mode, ...)
        me-data           ; Data file formats (csv, yaml, toml, json, plantuml-mode, ...)
        ;; me-math        ; Mathematics (maxima, ess, ein, julia-mode, ...)
        ;; me-modeling    ; Modeling tools (scad-mode, ...)
        me-org            ; Org-mode for life (org-contrib, org-modern, org-appear, ...)
        ;; me-extra          ; Extra features (better-jumper, crux, ...)
        ;; me-notes          ; Notes & Zettelkasten (denote, ...)
        ;; me-email       ; Email (mu4e, mu4e-alert, org-msg, ...)
        ;; me-rss         ; News feed (elfeed, ...)
        ;; me-lifestyle   ; *Very* opinionated lifestyle packages (awqat, ...)
        ;; me-docs           ; Documents (pdf-tools, nov, ...)
        ;; me-calendar    ; Calendar (calfw, calfw-org, calfw-ical, ...)
        ;; me-latex          ; LaTeX (auctex, auctex-latexmk, ...)
        ;; me-biblio      ; Bibliography & citations (citar, zotxt, ...)
        ;; me-natural-langs  ; Natural language stuff (spell-fu, go-translate, eglot-ltex, ...)
        ;; me-files          ; Files and directories (dirvish, treemacs, vlf, ...)
        me-tools          ; System tools (vterm, tldr, ssh-deploy, docker, ...)
        me-tty            ; Emacs from terminal (xt-mouse, xclip, ...)
        ;; me-fun            ; Games and funny packages (xkcd, speed-type, ...)
        ;; me-media          ; Multimedia (empv, emms, ...)
        ;; me-workspaces  ; Workspace separation (tabspaces, ...)
        ;; me-binary         ; Display binary files in hex or decompile them
        me-window))       ; Frame & window tweaks

;;; List of disabled packages
(setq minemacs-disabled-packages
      (append
       '(;; me-ui
         focus
         me-writing-mode
         visual-fill-column
         nix-ts-mode
         ;; me-tools
         bitwarden)))
