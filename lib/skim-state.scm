;; skim-state.scm — the browser's state, settings and small helpers.

;; ── state ──────────────────────────────────────────────────────────────
;; skim-main.scm — fetching, painting, interaction, history and keymaps.
;; (See skim.scm for the overview and options.)

(define skim--buffer "*skim*")

(define skim--header-lines 3)

;; ALL runtime state lives in one hash (the feeds pattern).
(define skim--state
  (hash 'url #f 'title "" 'html #f 'status 200 'doc #f 'css-texts '() 'links #f 'fields #f
        'forms #f 'field-values (hash) 'targets '() 'anchors (hash) 'back '() 'forward '()
        'inflight #f 'next #f 'origin #f 'reader #f 'img-lines '() 'img-queue '()
        'img-failed (hash) 'css-cache (hash) 'css-queue '()
        'dirty #f 'visited '() 'pending-line #f 'base #f 'confirm #f))

(define (skim--st key) (hash-try-get skim--state key))

(define (skim--st! key v) (set! skim--state (hash-insert skim--state key v)))

;; Insert k → v into the hash stored in state slot `key`.
(define (skim--st-put! key k v) (skim--st! key (hash-insert (skim--st key) k v)))

;; ── settings ───────────────────────────────────────────────────────────
(define (skim--width) (max 30 (skim--int (skim--opt-num "skim-width" 100))))

(define (skim--user-agent)
  (skim--opt "skim-user-agent"
             "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7; rv:128.0) Gecko/20100101 Firefox/128.0 skim/1.0"))

(define (skim--search-url) (skim--opt "skim-search-url" "https://html.duckduckgo.com/html/?q="))

(define (skim--cache-dir) (string-append (persistence-directory) "/skim/cache/"))

(define (skim--bookmarks-file) (string-append (persistence-directory) "/skim/bookmarks"))

;; ── URLs and text ──────────────────────────────────────────────────────
(define (skim--input->url input)
  (let ((s (skim--trim input)))
    (cond
      ((equal? s "") #f)
      ((not (null? (regexp-captures "^(https?|file|about|data):" s))) s)
      ((and (not (string-contains? s " "))
            (or (not (null? (regexp-captures "^[^\\s/]+\\.[a-zA-Z]{2,}(:[0-9]+)?(/.*)?$" s)))
                (not (null? (regexp-captures "^localhost(:[0-9]+)?(/.*)?$" s)))
                (not (null? (regexp-captures "^[0-9.]+(:[0-9]+)?(/.*)?$" s)))))
       (string-append (if (starts-with? s "localhost") "http://" "https://") s))
      (else (string-append (skim--search-url) (skim--form-encode s))))))

(define (skim--html-escape s)
  (string-replace (string-replace (string-replace s "&" "&amp;") "<" "&lt;") "\"" "&quot;"))

(define (skim--url-last-segment url)
  (let ((segs (filter (lambda (x) (not (equal? x ""))) (split-many (skim--strip-fragment url) "/"))))
    (if (null? segs) url (last segs))))

(define (skim--strip-query u) (car (split-many (skim--strip-fragment u) "?")))

(define (skim--strip-trailing-slash u) (if (ends-with? u "/") (substring u 0 (- (string-length u) 1)) u))

(define (skim--hex-prefix hex n) (if (> (string-length hex) (* 2 n)) (substring hex 0 (* 2 n)) hex))
