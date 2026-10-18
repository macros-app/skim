;; skim-page.scm — loading documents, stylesheets and images, and painting
;; *skim*.

;; ── history ────────────────────────────────────────────────────────────
(define (skim--push-history)
  (when (skim--st 'url)
    (skim--st! 'back (cons (skim--snapshot) (skim--st 'back)))
    (skim--st! 'forward '())))

(define (skim--snapshot)
  (hash 'url (skim--st 'url) 'html (skim--st 'html) 'status (skim--st 'status)
        'title (skim--st 'title) 'line (if (equal? (current-buffer) skim--buffer) (buffer-line) skim--header-lines)))

;; ── navigation ─────────────────────────────────────────────────────────
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

;; ── documents ──────────────────────────────────────────────────────────
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

;; ── stylesheets ────────────────────────────────────────────────────────
;; <link rel=stylesheet> URLs whose media applies.
(define (skim--stylesheet-urls doc)
  (skim--dedupe
   (map (lambda (e) (skim--resolve (skim--st 'base) (skim--attr e "href")))
       (filter (lambda (e)
                 (let ((rel (skim--lower (or (skim--attr e "rel") "")))
                       (media (or (skim--attr e "media") "")))
                   (and (member "stylesheet" (skim--words rel))
                        (not (member "alternate" (skim--words rel)))
                        (skim--attr e "href")
                        (skim--media-ok? media (skim--dark?)))))
               (skim--find-all-list doc (skim--tag? "link"))))))

;; The current document's stylesheet URLs skim will use (at most 120).
(define (skim--doc-stylesheets)
  (if (skim--st 'doc) (skim--take (skim--stylesheet-urls (skim--st 'doc)) 120) '()))

(define (skim--css-on?) (skim--setting "skim-css" #t))

;; The cascade for the current document.
;; The cascade is cached per page: it only changes when a stylesheet arrives
;; or a CSS/palette option flips.
(define (skim--build-sheet)
  (let* ((key (list (skim--st 'url) (skim--st 'html-id) (skim--dark?) (skim--css-on?)
                    (skim--opt-bool "skim-external-css" #t)
                    (map (lambda (u) (hash-contains? (skim--st 'css-cache) u))
                         (if (skim--st 'doc) (skim--doc-stylesheets) '()))))
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
                                        (skim--doc-stylesheets)))
                           '()))
               ;; linked sheets usually come from <head> before inline <style>;
               ;; that's the common cascade order.
               (texts (append linked (skim--st 'css-texts))))
          (foldl (lambda (css sh) (skim--parse-css sh css 1 dark?)) sheet texts)))))

;; Fetch linked stylesheets not yet cached; re-render once they're all in.
(define (skim--queue-stylesheets)
  (when (and (skim--css-on?) (skim--opt-bool "skim-external-css" #t))
    (let ((missing (filter (lambda (u) (not (hash-contains? (skim--st 'css-cache) u)))
                           (skim--doc-stylesheets))))
      (for-each (lambda (u)
                  (when (starts-with? u "file:")
                    (let ((path (skim--strip-query (url-decode (substring u 7 (string-length u))))))
                      (skim--st-put! 'css-cache u
                                                         (if (file-exists? path) (read-file path) "")))))
                missing)
      (let ((remote (filter (lambda (u) (starts-with? u "http")) missing)))
        (when (pair? remote)
          (skim--st! 'css-queue (append (skim--st 'css-queue) remote))
          (skim--next-css))
        (when (and (null? remote) (pair? missing)) (skim--render 'keep))))))

(define (skim--css-arrived url payload)
  (let* ((r (skim--parse-full payload))
         (ok? (and (>= (car r) 200) (< (car r) 300))))
    (skim--st-put! 'css-cache url
                                       (if ok? (decode-charset (caddr r) "utf-8") ""))))

;; Is *skim* on screen? "Visible" rather than "current": right after a
;; minibuffer read (skim-open) the snapshot can still report the previous
;; buffer while *skim* is on screen.
(define (skim--showing?)
  (or (equal? (current-buffer) skim--buffer) (>= (get-buffer-window skim--buffer) 0)))

;; Every stylesheet is in: re-render if *skim* is on screen. Fast servers
;; answer before the editor has even applied the first render, so otherwise
;; mark it dirty and look again shortly (and on the next switch to *skim*).
(define (skim--css-idle)
  (if (skim--showing?)
      (skim--render 'keep)
      (begin (skim--st! 'dirty #t)
             (run-with-timer 300 "skim--retry-dirty"))))

(define (skim--retry-dirty)
  (when (and (skim--st 'dirty) (skim--showing?))
    (skim--render 'keep)))

(define skim--css-pool
  (skim--make-pool 6 'css-queue "skim--on-css-" "text/css,*/*;q=0.1"
                   (lambda (u p) (skim--css-arrived u p)) (lambda () (skim--css-idle))))

(define (skim--next-css) (skim--pool-pump skim--css-pool))

(define (skim--on-css-0 p) (skim--pool-done skim--css-pool 0 p))

(define (skim--on-css-1 p) (skim--pool-done skim--css-pool 1 p))

(define (skim--on-css-2 p) (skim--pool-done skim--css-pool 2 p))

(define (skim--on-css-3 p) (skim--pool-done skim--css-pool 3 p))

(define (skim--on-css-4 p) (skim--pool-done skim--css-pool 4 p))

(define (skim--on-css-5 p) (skim--pool-done skim--css-pool 5 p))

;; ── images ─────────────────────────────────────────────────────────────
(define (skim--image-arrived url payload)
  (let* ((r (skim--parse-full payload))
         (ct (skim--lower (or (skim--header (cadr r) "content-type") "")))
         (ok? (and (>= (car r) 200) (< (car r) 300)))
         (path (and ok? (skim--save-image url (caddr r) (if (equal? ct "") (skim--guess-image-type url) ct)))))
    (if path
        (if (skim--showing?)
            (for-each (lambda (x) (when (equal? (cadr x) url) (set-line-image (car x) path (caddr x))))
                      (skim--st 'img-lines))
            (run-with-timer 300 "skim--show-images"))
        (skim--st-put! 'img-failed url #t))))

(define skim--img-pool
  (skim--make-pool 4 'img-queue "skim--on-image-"
                   "image/png,image/jpeg,image/gif,image/webp,image/*;q=0.8"
                   (lambda (u p) (skim--image-arrived u p)) (lambda () #t)))

(define (skim--next-image) (skim--pool-pump skim--img-pool))

(define (skim--on-image-0 p) (skim--pool-done skim--img-pool 0 p))

(define (skim--on-image-1 p) (skim--pool-done skim--img-pool 1 p))

(define (skim--on-image-2 p) (skim--pool-done skim--img-pool 2 p))

(define (skim--on-image-3 p) (skim--pool-done skim--img-pool 3 p))

;; Paint every image line whose file is cached; queue the rest.
(define (skim--show-images)
  (when (skim--showing?)
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

(define (skim--pools-watchdog)
  (skim--pool-expire skim--css-pool
                     (lambda (u) (skim--st-put! 'css-cache u "")))
  (skim--pool-expire skim--img-pool
                     (lambda (u) (skim--st-put! 'img-failed u #t))))

;; Is a page, stylesheet or image still loading?
(define (skim-loading?)
  (or (if (skim--st 'inflight) #t #f) (pair? (skim--st 'css-queue)) (skim--pool-busy? skim--css-pool)))

;; ── rendering ──────────────────────────────────────────────────────────
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

;; Coming back to *skim*: re-apply what changed while it was hidden.
(define (skim--on-buffer-switch . _)
  (when (equal? (current-buffer) skim--buffer)
    (if (skim--st 'dirty)
        (skim--render 'keep)
        (skim--show-images))))
