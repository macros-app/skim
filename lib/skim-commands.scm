;; skim-commands.scm — what the keys do: links, forms, history, bookmarks.

;; ── point ──────────────────────────────────────────────────────────────
(define (skim--prop-index key)
  (let ((v (get-text-property (point) key)))
    (if (equal? v "") #f (string->number v))))

(define (skim--link-at-point)
  (let ((i (skim--prop-index "skim-link")) (links (skim--st 'links)))
    (and i links (< i (vector-length links)) (vector-ref links i))))

(define (skim--field-at-point)
  (let ((i (skim--prop-index "skim-field")) (fields (skim--st 'fields)))
    (and i fields (< i (vector-length fields)) i)))

;; ── forms ──────────────────────────────────────────────────────────────
(define (skim--field idx) (vector-ref (skim--st 'fields) idx))

(define (skim--fget idx key) (hash-try-get (skim--field idx) key))

(define (skim--current-value idx)
  (let ((vals (skim--st 'field-values)))
    (if (hash-contains? vals idx) (hash-try-get vals idx) (skim--fget idx 'default))))

(define (skim--set-value! idx v)
  (skim--st-put! 'field-values idx v))

(define (skim--form-fields form)
  (let loop ((i 0) (acc '()))
    (if (>= i (vector-length (skim--st 'fields)))
        (reverse acc)
        (loop (+ i 1) (if (equal? (skim--fget i 'form) form) (cons i acc) acc)))))

(define (skim--activate-field idx)
  (let ((kind (skim--fget idx 'kind)))
    (cond
      ((skim--fget idx 'disabled) (message "skim: field is disabled"))
      ((member kind '("text" "textarea"))
       (read-string (string-append (let ((l (skim--fget idx 'label))) (if (or (not l) (equal? l "")) "Input" l)) ": ")
                    (lambda (input)
                      (skim--set-value! idx input)
                      (let ((form (skim--fget idx 'form)))
                        (if (and form (equal? kind "text")
                                 (= 1 (length (filter (lambda (i) (equal? (skim--fget i 'kind) "text"))
                                                      (skim--form-fields form)))))
                            (skim--submit form #f)
                            (skim--render 'keep))))
                    (skim--current-value idx)))
      ((equal? kind "checkbox")
       (skim--set-value! idx (not (skim--current-value idx)))
       (skim--render 'keep))
      ((equal? kind "radio")
       (for-each (lambda (i) (when (and (equal? (skim--fget i 'kind) "radio") (equal? (skim--fget i 'name) (skim--fget idx 'name)))
                               (skim--set-value! i #f)))
                 (skim--form-fields (skim--fget idx 'form)))
       (skim--set-value! idx #t)
       (skim--render 'keep))
      ((equal? kind "select")
       (let ((opts (skim--fget idx 'options)))
         (completing-read (string-append (if (equal? (skim--fget idx 'name) "") "Choose" (skim--fget idx 'name)) ": ")
                          (map cadr opts)
                          (lambda (label)
                            (let ((o (filter (lambda (o) (equal? (cadr o) label)) opts)))
                              (when (pair? o)
                                (skim--set-value! idx (car (car o)))
                                (skim--render 'keep)))))))
      ((equal? kind "submit") (skim--submit (skim--fget idx 'form) idx))
      ((equal? kind "reset")
       (for-each (lambda (i) (skim--st! 'field-values (hash-remove (skim--st 'field-values) i)))
                 (skim--form-fields (skim--fget idx 'form)))
       (skim--render 'keep))
      (else (message "skim: this button needs JavaScript")))))

;; Submit `form` (index), optionally by button `submitter` (field index).
(define (skim--submit form submitter)
  (if (not form)
      (message "skim: field isn't inside a form")
      (let* ((f (vector-ref (skim--st 'forms) form))
             (pairs (filter (lambda (x) x)
                            (map (lambda (i)
                                   (let ((kind (skim--fget i 'kind)) (name (skim--fget i 'name)))
                                     (cond
                                       ((or (equal? name "") (skim--fget i 'disabled)) #f)
                                       ((member kind '("text" "textarea" "hidden" "select"))
                                        (cons name (skim--current-value i)))
                                       ((member kind '("checkbox" "radio"))
                                        (and (skim--current-value i) (cons name (skim--fget i 'value))))
                                       ((equal? kind "submit")
                                        (and (equal? i submitter) (cons name (skim--fget i 'value))))
                                       (else #f))))
                                 (skim--form-fields form))))
             (query (string-join (map (lambda (kv) (string-append (skim--form-encode (car kv)) "=" (skim--form-encode (cdr kv)))) pairs) "&"))
             (action (let ((a (hash-try-get f 'action))) (if (equal? a "") (skim--st 'url) a))))
        (if (equal? (hash-try-get f 'method) "post")
            (skim--navigate action "POST" query 'push)
            (skim--navigate (string-append (skim--strip-query action) "?" query) "GET" "" 'push)))))

;; ── opening pages ──────────────────────────────────────────────────────
(define (skim-browse-url url)
  (let ((u (skim--input->url url)))
    (when u (skim--navigate u "GET" "" 'push))))

(define (skim)
  (if (and (skim--st 'url) (not (equal? (current-buffer) skim--buffer)) (member skim--buffer (buffer-names)))
      (begin (skim--st! 'origin (current-buffer)) (switch-to-buffer skim--buffer))
      (read-string "URL or search: " (lambda (input) (skim-browse-url input))
                   (let ((h (get-option "skim-home"))) (if (string? h) h "")))))

(define (skim-open)
  (read-string "URL or search: " (lambda (input) (skim-browse-url input))))

(define (skim-open-edit)
  (read-string "URL: " (lambda (input) (skim-browse-url input)) (or (skim--st 'url) "")))

(define (skim-search)
  (read-string "Search: " (lambda (q) (when (not (equal? (skim--trim q) ""))
                                        (skim--navigate (string-append (skim--search-url) (skim--form-encode q)) "GET" "" 'push)))))

(define (skim-follow)
  (let ((link (skim--link-at-point)) (field (skim--field-at-point)))
    (cond
      (field (skim--activate-field field))
      (link (skim--navigate link "GET" "" 'push))
      (else (message "skim: nothing to follow here")))))

(define (skim--goto-target dir)
  (let* ((p (point))
         (ts (skim--st 'targets))
         (ts (if (> dir 0) ts (reverse ts)))
         (hit (let loop ((xs ts)) (cond ((null? xs) #f)
                                        ((if (> dir 0) (> (car (car xs)) p) (< (car (car xs)) p)) (car xs))
                                        (else (loop (cdr xs)))))))
    (if (null? ts)
        (message "skim: no links on this page")
        (let ((t (or hit (car ts))))
          (goto-char (car t))
          (message (if (eq? (cadr t) 'link)
                       (vector-ref (skim--st 'links) (caddr t))
                       (let ((f (vector-ref (skim--st 'fields) (caddr t))))
                         (string-append "[" (hash-try-get f 'kind) "] "
                                        (or (hash-try-get f 'name) "") " — RET to "
                                        (if (member (hash-try-get f 'kind) '("submit" "button")) "press" "edit")))))))))

(define (skim-next-link) (skim--goto-target 1))

(define (skim-previous-link) (skim--goto-target -1))

;; ── history ────────────────────────────────────────────────────────────
(define (skim--restore entry mode)
  (skim--st! 'pending-line (hash-try-get entry 'line))
  (skim--load-html (hash-try-get entry 'url) (hash-try-get entry 'html) (hash-try-get entry 'status) mode))

(define (skim-back)
  (let ((b (skim--st 'back)))
    (if (null? b)
        (message "skim: no previous page")
        (begin
          (skim--st! 'forward (cons (skim--snapshot) (skim--st 'forward)))
          (skim--st! 'back (cdr b))
          (skim--restore (car b) 'history)))))

(define (skim-forward)
  (let ((f (skim--st 'forward)))
    (if (null? f)
        (message "skim: no next page")
        (begin
          (skim--st! 'back (cons (skim--snapshot) (skim--st 'back)))
          (skim--st! 'forward (cdr f))
          (skim--restore (car f) 'history)))))

(define (skim-reload)
  (if (skim--st 'url)
      (begin (skim--st! 'pending-line (buffer-line))
             (skim--st! 'css-cache (hash))
             (skim--navigate (skim--st 'url) "GET" "" 'replace))
      (message "skim: nothing to reload")))

;; ── page actions ───────────────────────────────────────────────────────
(define (skim-copy-url)
  (let ((u (or (skim--link-at-point) (skim--st 'url))))
    (when u (clipboard-set u) (message (string-append "skim: copied " u)))))

(define (skim-browse-external)
  (let ((u (or (skim--link-at-point) (skim--st 'url))))
    (when u (open-url u) (message (string-append "skim: opened " u " externally")))))

(define (skim-download)
  (let ((u (skim--link-at-point)))
    (if (not u)
        (message "skim: no link at point")
        (begin
          (skim--st! 'download-url u)
          (message (string-append "skim: downloading " u " …"))
          (http-request-full "GET" u (list "User-Agent" (skim--user-agent)) "" 'skim--on-download)))))

(define (skim-toggle-reader)
  (skim--st! 'reader (not (skim--st 'reader)))
  (skim--render 'top))

(define (skim-toggle-images)
  (let ((on? (skim--toggle! "skim-images" #t)))
    (skim--render 'keep)
    (message (string-append "skim: images " (if on? "on" "off")))))

(define (skim-toggle-css)
  (let ((on? (skim--toggle! "skim-css" #t)))
    (skim--render 'keep)
    (when on? (skim--queue-stylesheets))
    (message (string-append "skim: author CSS " (if on? "on" "off")))))

(define (skim-view-source)
  (if (skim--st 'html)
      (render-buffer "*skim-source*" "skim-source-mode" (skim--st 'html) '())
      (message "skim: no page")))

(define (skim-source-quit) (switch-to-buffer skim--buffer))

;; rel=next / rel=prev from <link> or <a>.
(define (skim--rel-link rel)
  (let ((e (skim--find-el (skim--st 'doc)
                          (lambda (e) (and (member (skim--el-tag e) '("link" "a"))
                                           (skim--attr e "href")
                                           (member rel (skim--words (skim--lower (or (skim--attr e "rel") "")))))))))
    (and e (skim--resolve (skim--st 'base) (skim--attr e "href")))))

(define (skim-next-page)
  (let ((u (skim--rel-link "next"))) (if u (skim--navigate u "GET" "" 'push) (message "skim: no rel=next link"))))

(define (skim-previous-page)
  (let ((u (or (skim--rel-link "prev") (skim--rel-link "previous"))))
    (if u (skim--navigate u "GET" "" 'push) (message "skim: no rel=prev link"))))

(define (skim-up)
  (let ((u (skim--st 'url)))
    (when u
      (let* ((p (skim--strip-trailing-slash (skim--strip-query u)))
             (c (regexp-captures "^(.*://[^/]+)(/.*)?/[^/]*$" p)))
        (if (null? c)
            (message "skim: already at the top")
            (skim--navigate (string-append (nth c 1) (nth c 2) "/") "GET" "" 'push))))))

(define (skim-quit)
  (let ((o (skim--st 'origin)))
    (switch-to-buffer (if (and o (member o (buffer-names))) o "*scratch*"))))

;; ── bookmarks and history lists ────────────────────────────────────────
(define (skim--bookmarks)
  (if (file-exists? (skim--bookmarks-file))
      (filter (lambda (x) x)
              (map (lambda (l) (let ((p (split-many l "\t")))
                                 (if (and (pair? p) (pair? (cdr p))) (cons (car p) (cadr p)) #f)))
                   (split-many (read-file (skim--bookmarks-file)) "\n")))
      '()))

(define (skim-bookmark-add)
  (if (not (skim--st 'url))
      (message "skim: no page")
      (read-string "Bookmark title: "
                   (lambda (title)
                     (let* ((title (replace-regexp-in-string "[\\t\\n]" " " (skim--trim title)))
                            (bs (filter (lambda (b) (not (equal? (car b) (skim--st 'url)))) (skim--bookmarks)))
                            (bs (append bs (list (cons (skim--st 'url) title)))))
                       (write-file (skim--bookmarks-file)
                                   (string-append (string-join (map (lambda (b) (string-append (car b) "\t" (cdr b))) bs) "\n") "\n"))
                       (message (string-append "skim: bookmarked " title))))
                   (skim--st 'title))))

(define (skim--pick-url prompt entries)
  (if (null? entries)
      (message "skim: nothing to choose from")
      (let ((labels (map (lambda (e) (string-append (cdr e) "  —  " (car e))) entries)))
        (completing-read prompt labels
                         (lambda (choice)
                           (let ((hit (filter (lambda (e) (equal? (string-append (cdr e) "  —  " (car e)) choice)) entries)))
                             (if (pair? hit)
                                 (skim--navigate (car (car hit)) "GET" "" 'push)
                                 (skim-browse-url choice))))))))

(define (skim-bookmarks) (skim--pick-url "Bookmark: " (skim--bookmarks)))

(define (skim-history) (skim--pick-url "History: " (skim--st 'visited)))

;; ── menu ───────────────────────────────────────────────────────────────
(define (skim-menu)
  (transient ""                         ; no header row, just the columns
             (list (list "Navigate"
                         (list "o" "Open URL / search…" 'skim-open)
                         (list "e" "Edit current URL…" 'skim-open-edit)
                         (list "s" "Web search…"        'skim-search)
                         (list "l" "Back"               'skim-back)
                         (list "r" "Forward"            'skim-forward)
                         (list "g" "Reload"             'skim-reload)
                         (list "n" "Next page (rel)"    'skim-next-page)
                         (list "p" "Previous page (rel)" 'skim-previous-page)
                         (list "u" "Up a directory"     'skim-up))
                   (list "Page"
                         (list "w" "Copy URL"           'skim-copy-url)
                         (list "&" "Open externally"    'skim-browse-external)
                         (list "d" "Download link"      'skim-download)
                         (list "v" "View source"        'skim-view-source)
                         (list "R" "Reader mode"        'skim-toggle-reader)
                         (list "I" "Toggle images"      'skim-toggle-images)
                         (list "C" "Toggle author CSS"  'skim-toggle-css))
                   (list "Saved"
                         (list "b" "Bookmark page…"     'skim-bookmark-add)
                         (list "B" "Bookmarks…"         'skim-bookmarks)
                         (list "h" "History…"           'skim-history)
                         (list "q" "Quit"               'skim-quit)))))
