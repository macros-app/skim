;; skim-keys.scm — keymaps, hooks and command registration.
;;
;; `?` in *skim* opens the menu of every command (skim-menu).

(define-key "skim-mode" "enter"     'skim-follow)

(define-key "skim-mode" "mouse-1"   'skim-follow)

(define-key "skim-mode" "tab"       'skim-next-link)

(define-key "skim-mode" "shift-tab" 'skim-previous-link)

(define-key "skim-mode" "l"         'skim-back)

(define-key "skim-mode" "r"         'skim-forward)

(define-key "skim-mode" "g"         'skim-reload)

(define-key "skim-mode" "shift-g"   'skim-open)

(define-key "skim-mode" "o"         'skim-open)

(define-key "skim-mode" "e"         'skim-open-edit)

(define-key "skim-mode" "s"         'skim-search)

(define-key "skim-mode" "n"         'skim-next-page)

(define-key "skim-mode" "p"         'skim-previous-page)

(define-key "skim-mode" "u"         'skim-up)

(define-key "skim-mode" "w"         'skim-copy-url)

(define-key "skim-mode" "&"         'skim-browse-external)

(define-key "skim-mode" "d"         'skim-download)

(define-key "skim-mode" "v"         'skim-view-source)

(define-key "skim-mode" "shift-r"   'skim-toggle-reader)

(define-key "skim-mode" "shift-i"   'skim-toggle-images)

(define-key "skim-mode" "shift-c"   'skim-toggle-css)

(define-key "skim-mode" "b"         'skim-bookmark-add)

(define-key "skim-mode" "shift-b"   'skim-bookmarks)

(define-key "skim-mode" "h"         'skim-history)

(define-key "skim-mode" "?"         'skim-menu)

(define-key "skim-mode" "q"         'skim-quit)

(define-key "skim-source-mode" "q"  'skim-source-quit)

;; Motion: special buffers bypass the global evil maps, so bind per mode.
(for-each
 (lambda (mode)
   (define-key mode "down"   'next-line)
   (define-key mode "up"     'previous-line)
   (define-key mode "left"   'backward-char)
   (define-key mode "right"  'forward-char)
   (define-key mode "ctrl-n" 'next-line)
   (define-key mode "ctrl-p" 'previous-line)
   (define-key mode "ctrl-f" 'forward-char)
   (define-key mode "ctrl-b" 'backward-char)
   (define-key mode "ctrl-v" 'evil-scroll-down)
   (define-key mode "alt-v"  'evil-scroll-up)
   (define-key mode "space"  'evil-scroll-down)
   (define-key mode "shift-space" 'evil-scroll-up)
   (define-key mode "backspace" 'evil-scroll-up)
   (define-key mode "home"   'beginning-of-buffer)
   (define-key mode "end"    'end-of-buffer)
   (define-key mode "alt-<"  'beginning-of-buffer)
   (define-key mode "alt->"  'end-of-buffer)
   (define-key mode "ctrl-s" 'isearch-forward)
   (define-key mode "ctrl-r" 'isearch-backward))
 (list "skim-mode" "skim-source-mode"))

(for-each
 (lambda (prefix)
   (let ((km (string-append prefix "skim-mode")))
     (define-key km "j"       'next-line)
     (define-key km "k"       'previous-line)
     (define-key km "h"       'backward-char)
     (define-key km "l"       'forward-char)
     (define-key km "w"       'forward-word-start)
     (define-key km "b"       'backward-word-start)
     (define-key km "e"       'evil-forward-word-end)
     (define-key km "0"       'move-beginning-of-line)
     (define-key km "$"       'move-end-of-line)
     (define-key km "{"       'evil-backward-paragraph)
     (define-key km "}"       'evil-forward-paragraph)
     (define-key km "ctrl-d"  'evil-scroll-down)
     (define-key km "ctrl-u"  'evil-scroll-up)
     (define-key km "g g"     'evil-goto-first-line)
     (define-key km "shift-g" 'evil-goto-last-line)
     (define-key km "/"       'evil-search-forward)
     (define-key km "?"       'skim-menu)
     (define-key km "n"       'evil-search-next)
     (define-key km "shift-n" 'evil-search-previous)
     (define-key km "shift-h" 'skim-back)
     (define-key km "shift-l" 'skim-forward)
     (define-key km "o"       'skim-open)
     (define-key km "shift-o" 'skim-open-edit)
     (define-key km "r"       'skim-reload)
     (define-key km "g r"     'skim-reload)
     (define-key km "y y"     'skim-copy-url)
     (define-key km "] ]"     'skim-next-page)
     (define-key km "[ ["     'skim-previous-page)
     (define-key km "g u"     'skim-up)
     (define-key km "shift-b" 'skim-bookmarks)
     (define-key km "m"       'skim-bookmark-add)))
 (list "evil-normal:" "helix-normal:"))

(for-each
 (lambda (prefix)
   (let ((km (string-append prefix "skim-source-mode")))
     (define-key km "j" 'next-line)
     (define-key km "k" 'previous-line)
     (define-key km "ctrl-d" 'evil-scroll-down)
     (define-key km "ctrl-u" 'evil-scroll-up)
     (define-key km "g g" 'evil-goto-first-line)
     (define-key km "shift-g" 'evil-goto-last-line)))
 (list "evil-normal:" "helix-normal:"))

(set-option "line-numbers:skim-mode" "false")

(set-option "line-numbers:skim-source-mode" "false")

(add-hook "buffer-switch-hook" "skim--on-buffer-switch")

(for-each register-command
          '(skim skim-browse-url skim-open skim-open-edit skim-search skim-follow skim-next-link
            skim-previous-link skim-back skim-forward skim-reload skim-copy-url skim-browse-external
            skim-download skim-toggle-reader skim-toggle-images skim-toggle-css skim-view-source
            skim-next-page skim-previous-page skim-up skim-bookmark-add skim-bookmarks skim-history
            skim-quit skim-menu))

