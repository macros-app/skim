;; skim-main.scm — fetching, painting, interaction, history and keymaps.
;; (See skim.scm for the overview and options.)

(define skim--buffer "*skim*")
(define skim--header-lines 3)

;; ── state ───────────────────────────────────────────────────────────────────
;; ALL runtime state lives in one hash (the feeds pattern).
(define skim--state
  (hash 'url #f 'title "" 'html #f 'status 200 'doc #f 'css-texts '() 'links #f 'fields #f
        'forms #f 'field-values (hash) 'targets '() 'anchors (hash) 'back '() 'forward '()
        'inflight #f 'next #f 'origin #f 'reader #f 'img-lines '() 'img-queue '()
        'img-inflight #f 'img-failed (hash) 'css-cache (hash) 'css-queue '() 'css-inflight #f
        'dirty #f 'visited '() 'pending-line #f 'base #f 'confirm #f))

(define (skim--st key) (hash-try-get skim--state key))
(define (skim--st! key v) (set! skim--state (hash-insert skim--state key v)))

(define (skim--width) (max 30 (skim--int (skim--opt-num "skim-width" 100))))
(define (skim--user-agent)
  (skim--opt "skim-user-agent"
             "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7; rv:128.0) Gecko/20100101 Firefox/128.0 skim/1.0"))
(define (skim--search-url) (skim--opt "skim-search-url" "https://html.duckduckgo.com/html/?q="))
(define (skim--cache-dir) (string-append (persistence-directory) "/skim/cache/"))
(define (skim--bookmarks-file) (string-append (persistence-directory) "/skim/bookmarks"))

;; ── input → URL ─────────────────────────────────────────────────────────────
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

;; ── fetching ────────────────────────────────────────────────────────────────
;; A request is #(url method body mode): mode 'push (new history entry),
;; 'replace (reload / redirect), or 'history (back/forward; html supplied).
(define (skim--request-headers)
  (list "User-Agent" (skim--user-agent)
        "Accept" "text/html,application/xhtml+xml,application/xml;q=0.9,text/plain;q=0.8,*/*;q=0.5"
        "Accept-Language" "en-US,en;q=0.7"))

(define (skim--navigate url method body mode)
  (let ((cur (skim--st 'url)))
    (cond
      ;; Same document, new fragment: just jump.
      ((and cur (equal? method "GET") (not (eq? mode 'replace))
            (string-contains? url "#")
            (equal? (skim--strip-fragment url) (skim--strip-fragment cur))
            (skim--st 'html))
       (when (eq? mode 'push) (skim--push-history))
       (skim--st! 'url url)
       (skim--jump-fragment (skim--url-fragment url)))
      ((starts-with? url "about:") (skim--show-about url mode))
      ((starts-with? url "file:") (skim--open-file-url url mode))
      ((skim--st 'inflight)
       ;; One page request at a time: the stale response is dropped on arrival.
       (skim--st! 'next (vector url method body mode))
       (message (string-append "skim: loading " url " …")))
      (else
       (skim--st! 'inflight (vector url method body mode))
       (message (string-append "skim: loading " url " …"))
       (http-request-full method url
                          (if (equal? method "POST")
                              (append (skim--request-headers)
                                      (list "Content-Type" "application/x-www-form-urlencoded"))
                              (skim--request-headers))
                          (or body "")
                          'skim--on-page)))))

;; "<status>\n<Header: v>…\n\n<hex>" → (status headers-alist hex).
(define (skim--parse-full payload)
  (let* ((parts (split-many payload "\n\n"))
         (head (car parts))
         (hex (if (null? (cdr parts)) "" (string-join (cdr parts) "")))
         (lines (split-many head "\n"))
         (status (or (string->number (skim--trim (car lines))) 0))
         (headers (filter (lambda (x) x)
                          (map (lambda (l)
                                 (let ((c (regexp-captures "^([^:]+):\\s*(.*)$" l)))
                                   (if (null? c) #f (cons (skim--lower (nth c 1)) (nth c 2)))))
                               (cdr lines)))))
    (list status headers hex)))

(define (skim--header headers name) (skim--aget headers name))

(define (skim--charset content-type)
  (let ((c (regexp-captures "(?i)charset=\"?([-A-Za-z0-9_:.]+)" (or content-type ""))))
    (if (null? c) #f (skim--lower (nth c 1)))))

;; Decode a page body: header charset, else <meta charset>, else UTF-8.
(define (skim--decode hex content-type)
  (let* ((cs (skim--charset content-type))
         (utf (decode-charset hex (or cs "utf-8"))))
    (if cs
        utf
        (let* ((head (if (> (string-length utf) 4096) (substring utf 0 4096) utf))
               (c (regexp-captures "(?i)<meta[^>]+charset\\s*=\\s*[\"']?([-A-Za-z0-9_:.]+)" head)))
          (if (or (null? c) (member (skim--lower (nth c 1)) '("utf-8" "utf8")))
              utf
              (decode-charset hex (skim--lower (nth c 1))))))))

(define (skim--on-page payload)
  (let ((req (skim--st 'inflight)) (next (skim--st 'next)))
    (skim--st! 'inflight #f)
    (cond
      ((not req) #t)
      (next
       (skim--st! 'next #f)
       (skim--navigate (vector-ref next 0) (vector-ref next 1) (vector-ref next 2) (vector-ref next 3)))
      (else
       (let* ((r (skim--parse-full payload))
              (status (car r)) (headers (cadr r)) (hex (caddr r))
              (ct (skim--lower (or (skim--header headers "content-type") "")))
              (url (vector-ref req 0)) (mode (vector-ref req 3)))
         (cond
           ((= status 0)
            (skim--show-error url (decode-charset hex "utf-8") mode))
           ((or (string-contains? ct "html") (string-contains? ct "xml")
                (and (equal? ct "") (not (null? (regexp-captures "(?i)^\\s*<" (decode-charset (skim--hex-prefix hex 200) "utf-8"))))))
            (skim--load-html url (skim--decode hex ct) status mode))
           ((or (starts-with? ct "text/") (string-contains? ct "json") (string-contains? ct "javascript"))
            (skim--load-html url (string-append "<html><head><title>" (skim--html-escape (skim--url-last-segment url))
                                                "</title></head><body><pre>"
                                                (skim--html-escape (skim--decode hex ct)) "</pre></body></html>")
                             status mode))
           ((starts-with? ct "image/")
            (let ((path (skim--save-image url hex ct)))
              (skim--load-html url (string-append "<html><head><title>" (skim--html-escape (skim--url-last-segment url))
                                                  "</title></head><body><img src=\"" (skim--html-escape url)
                                                  "\" alt=\"" (skim--html-escape ct) "\"></body></html>")
                               status mode)))
           (else
            (skim--offer-download url hex headers))))))))

(define (skim--hex-prefix hex n) (if (> (string-length hex) (* 2 n)) (substring hex 0 (* 2 n)) hex))

(define (skim--html-escape s)
  (string-replace (string-replace (string-replace s "&" "&amp;") "<" "&lt;") "\"" "&quot;"))

(define (skim--url-last-segment url)
  (let ((segs (filter (lambda (x) (not (equal? x ""))) (split-many (skim--strip-fragment url) "/"))))
    (if (null? segs) url (last segs))))

;; ── documents ───────────────────────────────────────────────────────────────
(define (skim--push-history)
  (when (skim--st 'url)
    (skim--st! 'back (cons (skim--snapshot) (skim--st 'back)))
    (skim--st! 'forward '())))

(define (skim--snapshot)
  (hash 'url (skim--st 'url) 'html (skim--st 'html) 'status (skim--st 'status)
        'title (skim--st 'title) 'line (if (equal? (current-buffer) skim--buffer) (buffer-line) skim--header-lines)))

;; Install a fetched/remembered document and render it.
(define (skim--load-html url html status mode)
  (when (eq? mode 'push) (skim--push-history))
  (when (not (eq? mode 'history)) (skim--st! 'pending-line #f))
  (let* ((parsed (skim--parse-html html))
         (doc (car parsed)))
    (skim--st! 'url url)
    (skim--st! 'html html)
    (skim--st! 'status status)
    (skim--st! 'doc doc)
    (skim--st! 'html-id (+ 1 (or (skim--st 'html-id) 0)))
    (skim--st! 'css-texts (cadr parsed))
    (skim--st! 'field-values (hash))
    (skim--st! 'reader #f)
    (skim--st! 'img-queue '())
    (skim--st! 'title (skim--doc-title doc url))
    (skim--st! 'base (skim--doc-base doc url))
    (skim--st! 'visited (cons (cons url (skim--st 'title))
                              (filter (lambda (v) (not (equal? (car v) url))) (skim--st 'visited))))
    (unless (member (current-buffer) (list skim--buffer "*skim-source*"))
      (skim--st! 'origin (current-buffer)))
    (let ((refresh (skim--meta-refresh doc)))
      (if (and refresh (not (eq? mode 'history)))
          (begin (message (string-append "skim: redirecting to " refresh))
                 (skim--navigate refresh "GET" "" 'replace))
          (begin
            (skim--render (or (skim--st 'pending-line) 'top))
            (skim--queue-stylesheets))))))

(define (skim--doc-title doc url)
  (let ((t (skim--find-el doc (skim--tag? "title"))))
    (let ((s (if t (skim--trim (replace-regexp-in-string "\\s+" " " (skim--text-content t))) "")))
      (if (equal? s "") url s))))

;; The base URL: <base href>, else the request URL — corrected to the
;; canonical URL when that only differs by a trailing slash (the usual
;; redirect skim can't see, which changes how relative links resolve).
(define (skim--doc-base doc url)
  (let* ((b (skim--find-el doc (lambda (e) (and (equal? (skim--el-tag e) "base") (skim--attr e "href")))))
         (canon (skim--find-el doc (lambda (e) (and (equal? (skim--el-tag e) "link")
                                                    (equal? (skim--lower (or (skim--attr e "rel") "")) "canonical")
                                                    (skim--attr e "href"))))))
    (cond
      (b (skim--resolve url (skim--attr b "href")))
      ((and canon
            (let ((c (skim--resolve url (skim--attr canon "href"))))
              (and (equal? (skim--url-host c) (skim--url-host url))
                   (equal? (string-append (skim--strip-trailing-slash (skim--strip-query url)) "/")
                           (skim--strip-query c)))))
       (skim--resolve url (skim--attr canon "href")))
      (else url))))

(define (skim--strip-query u) (car (split-many (skim--strip-fragment u) "?")))
(define (skim--strip-trailing-slash u) (if (ends-with? u "/") (substring u 0 (- (string-length u) 1)) u))

;; <meta http-equiv="refresh" content="0; url=…"> with a short delay → URL.
(define (skim--meta-refresh doc)
  (let ((m (skim--find-el doc (lambda (e) (and (equal? (skim--el-tag e) "meta")
                                               (equal? (skim--lower (or (skim--attr e "http-equiv") "")) "refresh"))))))
    (and m
         (let ((c (regexp-captures "(?i)^\\s*([0-9.]+)\\s*[;,]\\s*(?:url\\s*=\\s*)?['\"]?([^'\"]+)" (or (skim--attr m "content") ""))))
           (and (not (null? c))
                (<= (or (string->number (nth c 1)) 99) 2)
                (skim--resolve (skim--st 'url) (skim--trim (nth c 2))))))))

(define (skim--show-error url msg mode)
  (skim--load-html url (string-append "<html><head><title>Error</title></head><body><h1>Can't load page</h1><p><code>"
                                      (skim--html-escape url) "</code></p><pre>" (skim--html-escape msg)
                                      "</pre></body></html>")
                   0 mode))

(define (skim--show-about url mode)
  (skim--load-html url (string-append
                        "<html><head><title>skim</title></head><body><h1>skim</h1>"
                        "<p>A text web browser for macros. Press <kbd>G</kbd> (or <kbd>o</kbd> in evil) to open a URL or search, <kbd>?</kbd> for every command.</p>"
                        "<form action=\"" (skim--html-escape (skim--search-url)) "\"><input name=q size=40 placeholder=\"Search the web\"> <input type=submit value=Search></form>"
                        "</body></html>")
                   200 mode))

(define (skim--open-file-url url mode)
  (let* ((path (url-decode (substring url (if (starts-with? url "file://") 7 5) (string-length url))))
         (path (skim--strip-fragment path)))
    (cond
      ((not (file-exists? path)) (skim--show-error url "No such file" mode))
      ((file-directory? path)
       (skim--load-html url (string-append "<html><head><title>" (skim--html-escape path) "</title></head><body><h1>"
                                           (skim--html-escape path) "</h1><ul>"
                                           (string-join (map (lambda (p) (string-append "<li><a href=\"file://" (skim--html-escape p) "\">"
                                                                                        (skim--html-escape (basename p)) "</a>"))
                                                             (list-directory path)) "")
                                           "</ul></body></html>")
                        200 mode))
      ((not (null? (regexp-captures "(?i)\\.x?html?$" path))) (skim--load-html url (read-file path) 200 mode))
      (else (skim--load-html url (string-append "<html><body><pre>" (skim--html-escape (read-file path)) "</pre></body></html>") 200 mode)))))

;; ── stylesheets ─────────────────────────────────────────────────────────────
;; <link rel=stylesheet> URLs whose media applies.
(define (skim--stylesheet-urls doc)
  (map (lambda (e) (skim--resolve (skim--st 'base) (skim--attr e "href")))
       (filter (lambda (e)
                 (let ((rel (skim--lower (or (skim--attr e "rel") "")))
                       (media (or (skim--attr e "media") "")))
                   (and (member "stylesheet" (skim--words rel))
                        (not (member "alternate" (skim--words rel)))
                        (skim--attr e "href")
                        (skim--media-ok? media (skim--dark?)))))
               (skim--find-all-list doc (skim--tag? "link")))))

(define (skim--css-on?) (skim--opt-bool "skim-css" #t))

;; The cascade for the current document.
;; The cascade is cached per page: it only changes when a stylesheet arrives
;; or a CSS/palette option flips.
(define (skim--build-sheet)
  (let* ((key (list (skim--st 'url) (skim--st 'html-id) (skim--dark?) (skim--css-on?)
                    (skim--opt-bool "skim-external-css" #t)
                    (map (lambda (u) (hash-contains? (skim--st 'css-cache) u))
                         (if (skim--st 'doc) (skim--take (skim--stylesheet-urls (skim--st 'doc)) 32) '()))))
         (hit (skim--st 'sheet-cache)))
    (if (and hit (equal? (car hit) key))
        (cdr hit)
        (let ((sheet (skim--build-sheet-raw)))
          (skim--st! 'sheet-cache (cons key sheet))
          sheet))))

(define (skim--build-sheet-raw)
  (let* ((dark? (skim--dark?))
         (sheet (skim--ua-sheet)))
    (if (not (skim--css-on?))
        sheet
        (let* ((linked (if (skim--opt-bool "skim-external-css" #t)
                           (filter (lambda (x) x)
                                   (map (lambda (u) (hash-try-get (skim--st 'css-cache) u))
                                        (skim--take (skim--stylesheet-urls (skim--st 'doc)) 32)))
                           '()))
               ;; linked sheets usually come from <head> before inline <style>;
               ;; that's the common cascade order.
               (texts (append linked (skim--st 'css-texts))))
          (foldl (lambda (css sh) (skim--parse-css sh css 1 dark?)) sheet texts)))))

;; Fetch linked stylesheets not yet cached; re-render once they're all in.
(define (skim--queue-stylesheets)
  (when (and (skim--css-on?) (skim--opt-bool "skim-external-css" #t))
    (let ((missing (filter (lambda (u) (not (hash-contains? (skim--st 'css-cache) u)))
                           (skim--take (skim--stylesheet-urls (skim--st 'doc)) 32))))
      (for-each (lambda (u)
                  (when (starts-with? u "file:")
                    (let ((path (skim--strip-query (url-decode (substring u 7 (string-length u))))))
                      (skim--st! 'css-cache (hash-insert (skim--st 'css-cache) u
                                                         (if (file-exists? path) (read-file path) ""))))))
                missing)
      (let ((remote (filter (lambda (u) (starts-with? u "http")) missing)))
        (when (pair? remote)
          (skim--st! 'css-queue (append (skim--st 'css-queue) remote))
          (skim--next-css))
        (when (and (null? remote) (pair? missing)) (skim--render 'keep))))))

(define (skim--next-css)
  (when (not (skim--st 'css-inflight))
    (let ((q (skim--st 'css-queue)))
      (if (null? q)
          (begin
            (if (equal? (current-buffer) skim--buffer)
                (skim--render 'keep)
                (skim--st! 'dirty #t)))
          (begin
            (skim--st! 'css-queue (cdr q))
            (skim--st! 'css-inflight (car q))
            (skim--st! 'css-started (current-inexact-milliseconds))
            (run-with-timer 15000 "skim--css-watchdog")
            (http-request-full "GET" (car q) (list "User-Agent" (skim--user-agent) "Accept" "text/css,*/*;q=0.1")
                               "" 'skim--on-css))))))

;; A stylesheet that hasn't answered in 15s is given up on so the page still
;; gets its final render.
(define (skim--css-watchdog)
  (when (and (skim--st 'css-inflight)
             (> (- (current-inexact-milliseconds) (or (skim--st 'css-started) 0)) 14000))
    (skim--st! 'css-cache (hash-insert (skim--st 'css-cache) (skim--st 'css-inflight) ""))
    (skim--st! 'css-inflight #f)
    (skim--next-css)))

(define (skim--on-css payload)
  (let ((url (skim--st 'css-inflight)))
    (skim--st! 'css-inflight #f)
    (when url
      (let* ((r (skim--parse-full payload))
             (ok? (and (>= (car r) 200) (< (car r) 300))))
        (skim--st! 'css-cache (hash-insert (skim--st 'css-cache) url
                                           (if ok? (decode-charset (caddr r) "utf-8") "")))))
    (skim--next-css)))

;; ── rendering ───────────────────────────────────────────────────────────────
;; Reader mode: the main content element (article/main/role=main/#content),
;; else the element holding the most paragraph text.
(define (skim--main-content doc)
  (or (skim--find-el doc (skim--tag? "article"))
      (skim--find-el doc (skim--tag? "main"))
      (skim--find-el doc (lambda (e) (equal? (skim--attr e "role") "main")))
      (skim--find-el doc (lambda (e) (member (skim--attr e "id") '("content" "main-content" "main" "bodyContent" "mw-content-text"))))
      (let ((best (foldl (lambda (e acc)
                           (let ((n (foldl (lambda (p n) (+ n (string-length (skim--text-content p))))
                                           0 (filter (lambda (k) (and (skim--el? k) (equal? (skim--el-tag k) "p"))) (skim--el-kids e)))))
                             (if (> n (car acc)) (cons n e) acc)))
                         (cons 0 #f)
                         (skim--find-all-list doc (lambda (e) (member (skim--el-tag e) '("div" "section" "td")))))))
        (cdr best))
      doc))

(define (skim--title-face) (skim--fg-face (skim--pal 'title) #t #f))
(define (skim--url-face) (skim--fg-face (skim--pal 'url) #f #f))

;; Lay out the current document and paint *skim*. `where`: 'top, 'keep, a
;; line number, or a fragment jump (when the URL has one and where is 'top).
;; Where the last render put the cursor and when: a render following within
;; 2s (a stylesheet that arrived quickly) must not trust `buffer-line`, which
;; may not have caught up with that placement yet.
(define (skim--render where)
  (let* ((placed (skim--st 'placed))
         (prev-line (cond ((and placed (< (- (current-inexact-milliseconds) (cdr placed)) 2000)) (car placed))
                          ((equal? (current-buffer) skim--buffer) (buffer-line))
                          (else #f)))
         (doc (skim--st 'doc))
         (root (if (skim--st 'reader) (skim--el "#root" '() (list (skim--main-content doc))) doc))
         (sheet (skim--build-sheet))
         (L (skim--layout root sheet (skim--st 'base) (skim--width) (skim--st 'field-values)))
         (lines (hash-try-get L 'lines))
         (status (skim--st 'status))
         (head (list (skim--line (skim--st 'title) (list (list 0 (string-length (skim--st 'title)) 'fg (skim--title-face))) '())
                     (let ((u (string-append (skim--st 'url)
                                             (if (and (> status 0) (not (= status 200))) (string-append "  [" (number->string status) "]") "")
                                             (if (skim--st 'reader) "  [reader]" ""))))
                       (skim--line u (list (list 0 (string-length u) 'fg (skim--url-face))) '()))
                     skim--blank-line))
         (all (append head lines)))
    (skim--st! 'links (hash-try-get L 'links))
    (skim--st! 'fields (hash-try-get L 'fields))
    (skim--st! 'forms (hash-try-get L 'forms))
    (skim--st! 'dirty #f)
    (skim--paint all)
    (let* ((frag (skim--url-fragment (skim--st 'url)))
           (line (cond
                   ((number? where) (min where (- (length all) 1)))
                   ((and (eq? where 'keep) prev-line) (min prev-line (- (length all) 1)))
                   ((and (not (equal? frag "")) (hash-try-get (skim--st 'anchors) (url-decode frag)))
                    (hash-try-get (skim--st 'anchors) (url-decode frag)))
                   (else 0))))
      (goto-line line)
      (when (and (not (eq? where 'keep)) (not (equal? frag "")) (hash-try-get (skim--st 'anchors) (url-decode frag)))
        (scroll-cursor-to-row 0))
      (skim--st! 'placed (cons line (current-inexact-milliseconds))))
    (message (string-append "skim: " (skim--st 'title)))))

;; Paint lines into the buffer: text, faces, link/field properties, line
;; scales and images. Columns become byte offsets here.
(define (skim--paint lines)
  (skim--ensure-faces)
  (let* ((texts (map (lambda (l) (string-replace (skim--line-text l) skim--nbsp " ")) lines)))
    ((if (equal? (current-buffer) skim--buffer) render-buffer-quiet render-buffer)
     skim--buffer "skim-mode" (string-join texts "\n") '())
    (clear-overlays)
    (clear-line-scales)
    (clear-line-images)
    (let loop ((ls lines) (ts texts) (i 0) (base 0)
               (faces '()) (targets '()) (anchors (hash)) (imgs '()))
      (if (null? ls)
          (begin
            (overlay-set-faces (reverse faces))
            (skim--st! 'targets (reverse targets))
            (skim--st! 'anchors anchors)
            (skim--st! 'img-lines (reverse imgs))
            (skim--show-images))
          (let* ((l (car ls)) (t (car ts))
                 (ascii? (= (string-length t) (skim--bytelen t)))
                 (bpos (lambda (col) (+ base (if ascii? col (skim--bytelen (substring t 0 (min col (string-length t))))))))
                 (r (let span-loop ((ss (skim--line-spans l)) (faces faces) (targets targets))
                      (if (null? ss)
                          (cons faces targets)
                          (let* ((s (car ss)) (kind (caddr s)) (v (cadddr s))
                                 (b0 (bpos (car s))) (b1 (bpos (cadr s))))
                            (cond
                              ((>= b0 b1) (span-loop (cdr ss) faces targets))
                              ((or (eq? kind 'fg) (eq? kind 'bg))
                               (span-loop (cdr ss) (cons (list b0 b1 v) faces) targets))
                              ((eq? kind 'link)
                               (put-text-property b0 b1 "skim-link" (number->string v))
                               (span-loop (cdr ss) faces (cons (list b0 'link v) targets)))
                              ((eq? kind 'field)
                               (put-text-property b0 b1 "skim-field" (number->string v))
                               (span-loop (cdr ss) faces (cons (list b0 'field v) targets)))
                              (else (span-loop (cdr ss) faces targets)))))))
                 (scale (skim--meta-get l 'scale))
                 (img (skim--meta-get l 'img))
                 (ids (or (skim--meta-get l 'anchors) '())))
            (when (and scale (> scale 1)) (set-line-scale i scale))
            (loop (cdr ls) (cdr ts) (+ i 1) (+ base (skim--bytelen t) 1)
                  (car r) (cdr r)
                  (foldl (lambda (id h) (if (hash-contains? h id) h (hash-insert h id i))) anchors ids)
                  (if img (cons (list i (car img) (cdr img)) imgs) imgs)))))))

(define (skim--jump-fragment frag)
  (let ((line (hash-try-get (skim--st 'anchors) (url-decode frag))))
    (if line
        (begin (goto-line line) (scroll-cursor-to-row 0))
        (message (string-append "skim: no anchor #" frag)))))

;; ── images ──────────────────────────────────────────────────────────────────
(define (skim--image-ext ct)
  (cond ((string-contains? ct "png") "png") ((string-contains? ct "jpeg") "jpg")
        ((string-contains? ct "jpg") "jpg") ((string-contains? ct "gif") "gif")
        ((string-contains? ct "webp") "webp") ((string-contains? ct "bmp") "bmp")
        ((string-contains? ct "icon") "ico") ((string-contains? ct "tiff") "tiff")
        (else #f)))

(define (skim--image-path url ext)
  (string-append (skim--cache-dir) (sha256 url) "." ext))

;; Already-downloaded image for `url`, or #f.
(define (skim--cached-image url)
  (let loop ((exts '("png" "jpg" "gif" "webp" "bmp" "ico" "tiff")))
    (cond ((null? exts) #f)
          ((file-exists? (skim--image-path url (car exts))) (skim--image-path url (car exts)))
          (else (loop (cdr exts))))))

(define (skim--save-image url hex ct)
  (let ((ext (skim--image-ext ct)))
    (and ext (> (string-length hex) 0)
         (let ((p (skim--image-path url ext)))
           (write-file-hex p hex)
           p))))

;; data:image/png;base64,… → cached file path, or #f.
(define (skim--data-uri-image url)
  (or (skim--cached-image url)
      (let ((c (regexp-captures "^data:(image/[-a-z+.]+);base64,(.*)$" url)))
        (and (not (null? c))
             (skim--save-image url (base64url-hex (nth c 2)) (nth c 1))))))

;; Paint every image line whose file is cached; queue the rest.
(define (skim--show-images)
  (when (equal? (current-buffer) skim--buffer)
    (let loop ((xs (skim--st 'img-lines)) (missing '()))
      (if (null? xs)
          (begin
            (skim--st! 'img-queue (reverse missing))
            (skim--next-image))
          (let* ((x (car xs)) (url (cadr x))
                 (path (if (starts-with? url "data:") (skim--data-uri-image url) (skim--cached-image url))))
            (cond
              (path (set-line-image (car x) path (caddr x)) (loop (cdr xs) missing))
              ((or (hash-contains? (skim--st 'img-failed) url) (starts-with? url "data:") (member url missing))
               (loop (cdr xs) missing))
              (else (loop (cdr xs) (cons url missing)))))))))

(define (skim--next-image)
  (when (not (skim--st 'img-inflight))
    (let ((q (skim--st 'img-queue)))
      (when (pair? q)
        (skim--st! 'img-queue (cdr q))
        (skim--st! 'img-inflight (car q))
        (skim--st! 'img-started (current-inexact-milliseconds))
        (run-with-timer 15000 "skim--image-watchdog")
        (http-request-full "GET" (car q)
                           (list "User-Agent" (skim--user-agent) "Accept" "image/png,image/jpeg,image/gif,image/webp,image/*;q=0.8"
                                 "Referer" (or (skim--st 'url) ""))
                           "" 'skim--on-image)))))

(define (skim--image-watchdog)
  (when (and (skim--st 'img-inflight)
             (> (- (current-inexact-milliseconds) (or (skim--st 'img-started) 0)) 14000))
    (skim--st! 'img-failed (hash-insert (skim--st 'img-failed) (skim--st 'img-inflight) #t))
    (skim--st! 'img-inflight #f)
    (skim--next-image)))

(define (skim--on-image payload)
  (let ((url (skim--st 'img-inflight)))
    (skim--st! 'img-inflight #f)
    (when url
      (let* ((r (skim--parse-full payload))
             (ct (skim--lower (or (skim--header (cadr r) "content-type") "")))
             (ok? (and (>= (car r) 200) (< (car r) 300)))
             (path (and ok? (skim--save-image url (caddr r) (if (equal? ct "") (skim--guess-image-type url) ct)))))
        (if path
            (when (equal? (current-buffer) skim--buffer)
              (for-each (lambda (x) (when (equal? (cadr x) url) (set-line-image (car x) path (caddr x))))
                        (skim--st 'img-lines)))
            (skim--st! 'img-failed (hash-insert (skim--st 'img-failed) url #t)))))
    (skim--next-image)))

(define (skim--guess-image-type url)
  (let ((u (skim--lower url)))
    (cond ((string-contains? u ".png") "image/png") ((string-contains? u ".gif") "image/gif")
          ((string-contains? u ".webp") "image/webp") (else "image/jpeg"))))

;; Coming back to *skim*: re-apply what changed while it was hidden.
(define (skim--on-buffer-switch . _)
  (when (equal? (current-buffer) skim--buffer)
    (if (skim--st 'dirty)
        (skim--render 'keep)
        (skim--show-images))))

;; ── downloads ───────────────────────────────────────────────────────────────
(define (skim--download-path name)
  (let* ((dir (string-append (getenv "HOME") "/Downloads/"))
         (name (if (equal? name "") "download" name)))
    (let loop ((i 0))
      (let ((p (string-append dir (if (= i 0) name (string-append (number->string i) "-" name)))))
        (if (file-exists? p) (loop (+ i 1)) p)))))

(define (skim--offer-download url hex headers)
  (let* ((cd (or (skim--header headers "content-disposition") ""))
         (c (regexp-captures "filename\\*?=(?:UTF-8'')?\"?([^\";]+)" cd))
         (name (url-decode (if (null? c) (skim--url-last-segment (skim--strip-query url)) (nth c 1))))
         (path (skim--download-path name)))
    (skim--st! 'confirm (lambda () (write-file-hex path hex) (message (string-append "skim: saved " path))))
    (y-or-n-p (string-append "skim: " (or (skim--header headers "content-type") "file")
                             " — save to " path "? ")
              'skim--confirm-yes)))

(define (skim--confirm-yes ans)
  (let ((t (skim--st 'confirm)))
    (skim--st! 'confirm #f)
    (when (and t (equal? ans "yes")) (t))))

(define (skim--on-download payload)
  (let* ((r (skim--parse-full payload)))
    (if (and (>= (car r) 200) (< (car r) 300))
        (skim--offer-download (skim--st 'download-url) (caddr r) (cadr r))
        (message (string-append "skim: download failed — HTTP " (number->string (car r)))))))

;; ── point queries ───────────────────────────────────────────────────────────
(define (skim--prop-index key)
  (let ((v (get-text-property (point) key)))
    (if (equal? v "") #f (string->number v))))

(define (skim--link-at-point)
  (let ((i (skim--prop-index "skim-link")) (links (skim--st 'links)))
    (and i links (< i (vector-length links)) (vector-ref links i))))

(define (skim--field-at-point)
  (let ((i (skim--prop-index "skim-field")) (fields (skim--st 'fields)))
    (and i fields (< i (vector-length fields)) i)))

;; ── forms ───────────────────────────────────────────────────────────────────
(define (skim--field idx) (vector-ref (skim--st 'fields) idx))
(define (skim--fget idx key) (hash-try-get (skim--field idx) key))

(define (skim--current-value idx)
  (let ((vals (skim--st 'field-values)))
    (if (hash-contains? vals idx) (hash-try-get vals idx) (skim--fget idx 'default))))

(define (skim--set-value! idx v)
  (skim--st! 'field-values (hash-insert (skim--st 'field-values) idx v)))

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

;; ── commands ────────────────────────────────────────────────────────────────
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
  (set-option "skim-images" (if (skim--opt-bool "skim-images" #t) "off" "on"))
  (skim--render 'keep)
  (message (string-append "skim: images " (if (skim--opt-bool "skim-images" #t) "on" "off"))))

(define (skim-toggle-css)
  (set-option "skim-css" (if (skim--css-on?) "off" "on"))
  (skim--render 'keep)
  (when (skim--css-on?) (skim--queue-stylesheets))
  (message (string-append "skim: author CSS " (if (skim--css-on?) "on" "off"))))

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

;; ── bookmarks + history ─────────────────────────────────────────────────────
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

(define (skim-quit)
  (let ((o (skim--st 'origin)))
    (switch-to-buffer (if (and o (member o (buffer-names))) o "*scratch*"))))

(define (skim-menu)
  (transient "skim"
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

;; ── keymaps ─────────────────────────────────────────────────────────────────
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
