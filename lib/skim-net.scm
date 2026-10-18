;; skim-net.scm — HTTP responses, parallel fetch pools, the image cache and
;; downloads.

;; ── responses ──────────────────────────────────────────────────────────
;; A request is #(url method body mode): mode 'push (new history entry),
;; 'replace (reload / redirect), or 'history (back/forward; html supplied).
(define (skim--request-headers)
  (list "User-Agent" (skim--user-agent)
        "Accept" "text/html,application/xhtml+xml,application/xml;q=0.9,text/plain;q=0.8,*/*;q=0.5"
        "Accept-Language" "en-US,en;q=0.7"))

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

;; ── fetch pools ────────────────────────────────────────────────────────
;; `http-request-full` reports back through ONE named callback, so a request
;; can't say which URL it answers. A pool runs N requests at once by giving
;; each slot its own callback (skim--on-css-0 …): the slot remembers its URL.
;; A pool is #(slots queue-key callback-prefix on-done accept on-response).
;; A slot is #f (free), (url . started-ms), or 'dead — timed out and still
;; waiting for its late callback, so it can't be handed a new URL meanwhile.
(define (skim--make-pool n queue-key prefix accept on-response on-idle)
  (vector (make-vector n #f) queue-key prefix accept on-response on-idle))

(define (skim--pool-busy? pool)
  (let ((slots (vector-ref pool 0)))
    (let loop ((i 0))
      (cond ((>= i (vector-length slots)) #f)
            ((pair? (vector-ref slots i)) #t)
            (else (loop (+ i 1)))))))

(define (skim--pool-pump pool)
  (let ((slots (vector-ref pool 0)) (qk (vector-ref pool 1)))
    (let loop ((i 0))
      (when (< i (vector-length slots))
        (let ((q (skim--st qk)))
          (when (and (not (vector-ref slots i)) (pair? q))
            (skim--st! qk (cdr q))
            (vector-set! slots i (cons (car q) (current-inexact-milliseconds)))
            (run-with-timer 15000 "skim--pools-watchdog")
            (http-request-full "GET" (car q)
                               (list "User-Agent" (skim--user-agent) "Accept" (vector-ref pool 3)
                                     "Referer" (or (skim--st 'url) ""))
                               "" (string-append (vector-ref pool 2) (number->string i)))))
        (loop (+ i 1))))
    (when (and (null? (skim--st qk)) (not (skim--pool-busy? pool)))
      ((vector-ref pool 5)))))

;; Slot `i` answered: hand (url payload) to the pool's handler, free the slot.
(define (skim--pool-done pool i payload)
  (let* ((slots (vector-ref pool 0)) (slot (vector-ref slots i)))
    (vector-set! slots i #f)
    (when (pair? slot) ((vector-ref pool 4) (car slot) payload))
    (skim--pool-pump pool)))

;; Requests that haven't answered in 15s are given up on (their slot stays
;; 'dead until the late answer arrives) so pages still finish rendering.
(define (skim--pool-expire pool on-expire)
  (let ((slots (vector-ref pool 0)) (now (current-inexact-milliseconds)))
    (let loop ((i 0) (expired #f))
      (if (< i (vector-length slots))
          (let ((s (vector-ref slots i)))
            (if (and (pair? s) (> (- now (cdr s)) 14000))
                (begin (vector-set! slots i 'dead) (on-expire (car s)) (loop (+ i 1) #t))
                (loop (+ i 1) expired)))
          (when expired (skim--pool-pump pool))))))

;; ── image cache ────────────────────────────────────────────────────────
(define (skim--image-ext ct)
  (cond ((string-contains? ct "png") "png") ((string-contains? ct "jpeg") "jpg")
        ((string-contains? ct "jpg") "jpg") ((string-contains? ct "gif") "gif")
        ((string-contains? ct "webp") "webp") ((string-contains? ct "bmp") "bmp")
        ((string-contains? ct "icon") "ico") ((string-contains? ct "tiff") "tiff")
        (else #f)))

(define (skim--guess-image-type url)
  (let ((u (skim--lower url)))
    (cond ((string-contains? u ".png") "image/png") ((string-contains? u ".gif") "image/gif")
          ((string-contains? u ".webp") "image/webp") (else "image/jpeg"))))

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

;; ── downloads ──────────────────────────────────────────────────────────
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
