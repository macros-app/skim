;; skim-replaced.scm — elements that aren't text flow: rules, embedded
;; media, images and form controls.

;; ── rules and media ────────────────────────────────────────────────────
(define (skim--hr cs ctx st)
  (skim--flush! st ctx)
  (skim--out-push! st 1)
  (let ((w (hash-try-get ctx 'width)))
    (skim--out-push! st (skim--line (skim--repeat "─" w) (list (list 0 w 'fg (skim--dim-face))) '())))
  (skim--out-push! st 1))

;; An embedded iframe/video/audio/embed/object as a link line. Returns #f
;; when there's no source, so the walker renders the fallback content.
(define (skim--media el cs ctx st)
  (let* ((tag (skim--el-tag el))
         (src (or (skim--attr el "src") (skim--attr el "data")
                  (let ((s (skim--find-el el (skim--tag? "source")))) (and s (skim--attr s "src")))))
         (title (or (skim--attr el "title") (skim--attr el "aria-label") "")))
    (if (and src (not (equal? (skim--trim src) "")) (not (starts-with? src "about:")))
        (let* ((url (skim--resolve (skim--Rget 'base) src))
               (idx (skim--add-link url))
               (label (string-append "[" tag (if (equal? title "") "" (string-append ": " title)) "] "
                                     (skim--truncate url 70))))
          (skim--para-push! st (vector 'atom label (vector (skim--fg-face (skim--pal 'link) #f #t) #f idx #f)))
          (skim--para-push! st (vector 'br))
          #t)
        #f)))

;; ── images ─────────────────────────────────────────────────────────────
;; Pick the image URL: lazy-loading attributes first, then srcset's first
;; candidate, then src (skipping data: placeholders that are tiny GIFs).
(define (skim--image-src el)
  (let* ((lazy (or (skim--attr el "data-src") (skim--attr el "data-lazy-src") (skim--attr el "data-original")))
         (srcset (or (skim--attr el "srcset") (skim--attr el "data-srcset")))
         (src (skim--attr el "src")))
    (cond
      ((and lazy (not (equal? (skim--trim lazy) ""))) lazy)
      ((and src (not (equal? (skim--trim src) "")) (not (starts-with? src "data:"))) src)
      ((and srcset (not (equal? (skim--trim srcset) "")))
       (car (skim--words (car (split-many srcset ",")))))
      ((and src (starts-with? src "data:image/") (> (string-length src) 400) (< (string-length src) 2000000)) src)
      (else #f))))

(define (skim--image el cs ctx st)
  (let* ((alt (skim--trim (or (skim--attr el "alt") "")))
         (src (skim--image-src el))
         (url (and src (skim--resolve-image-url src)))
         (w (or (skim--num (skim--attr el "width")) (skim--px (skim--style cs "width"))))
         (h (or (skim--num (skim--attr el "height")) (skim--px (skim--style cs "height"))))
         (icon? (or (and w (< w 48)) (and h (< h 40))))
         (svg? (and url (or (string-contains? (skim--lower url) ".svg") (starts-with? url "data:image/svg"))))
         (link (hash-try-get ctx 'link))
         (show? (and url (not icon?) (not svg?) (skim--setting "skim-images" #t)
                     (< (skim--Rget 'images) (skim--opt-num "skim-max-images" 60)))))
    (cond
      (show?
       (set! skim--R (hash-insert skim--R 'images (+ 1 (skim--Rget 'images))))
       (skim--flush! st ctx)
       (let* ((label (if (equal? alt "") "▣" (string-append "▣ " (skim--truncate alt (max 10 (- (hash-try-get ctx 'width) 2))))))
              ;; only a height given (logos, avatars: usually square): cap the
              ;; width at it — the editor sizes images by width alone
              (maxw (skim--int (min (or w (and h (< h 200) h) 10000)
                                    (skim--opt-num "skim-image-max-width" 640))))
              (style (vector (skim--fg-face (if link (skim--pal 'link) (skim--pal 'dim)) #f #f) #f link #f))
              (line (car (skim--wrap (list (vector 'atom label style)) (max 8 (string-length label)) "left"))))
         (skim--out-push! st (skim--meta-put line 'img (cons url maxw)))))
      ((not (equal? alt ""))
       (skim--para-push! st (vector 'text (if icon? alt (string-append "[" alt "]"))
                                    (vector (skim--fg-face (if link (skim--pal 'link) (skim--pal 'dim)) #f #f) #f link #f)
                                    'normal)))
      ((and link (not icon?))
       (skim--para-push! st (vector 'atom (string-append "[" (skim--image-label el src) "]")
                                    (vector (skim--link-face) #f link #f))))
      ;; An image we don't draw still occupies its declared width, as in a
      ;; browser — this is how spacer GIFs indent (e.g. threaded comments).
      ((and w (>= w 4))
       (skim--para-push! st (vector 'atom (skim--spaces (min 80 (skim--int (/ (+ w 4) 8)))) skim--plain-style)))
      (else #t))))

;; A name for an image with no alt text: its title, an aria-label, else its
;; file name ("github" from /icons/github.svg), else "img".
(define (skim--image-label el src)
  (let ((t (or (skim--attr el "title") (skim--attr el "aria-label"))))
    (if (and t (not (equal? (skim--trim t) "")))
        (skim--trim t)
        (let ((c (and src (not (starts-with? src "data:"))
                      (regexp-captures "([^/?#]+?)(?:\\.[a-zA-Z0-9]+)?(?:[?#].*)?$" src))))
          (if (and c (pair? c) (> (string-length (nth c 1)) 1) (< (string-length (nth c 1)) 30))
              (nth c 1)
              "img")))))

(define (skim--resolve-image-url src)
  (if (starts-with? src "data:") src (skim--resolve (skim--Rget 'base) src)))

;; ── forms ──────────────────────────────────────────────────────────────
(define (skim--add-form el)
  (let ((v (skim--Rget 'forms)))
    (vector-push! v (hash 'action (skim--resolve (skim--Rget 'base) (or (skim--attr el "action") ""))
                          'method (skim--lower (or (skim--attr el "method") "get"))
                          'enctype (skim--lower (or (skim--attr el "enctype") ""))))
    (- (vector-length v) 1)))

;; Field value: the user's edit if any (keyed by field index), else the DOM's.
(define (skim--field-value idx default)
  (let ((vals (skim--Rget 'values)))
    (if (hash-contains? vals idx) (hash-try-get vals idx) default)))

(define (skim--widget el m cs ctx st)
  (let* ((tag (skim--el-tag el))
         (type (skim--lower (or (skim--attr el "type") (if (equal? tag "button") "submit" "text"))))
         (name (or (skim--attr el "name") ""))
         (form (hash-try-get ctx 'form))
         (disabled? (skim--has-attr? el "disabled"))
         (field-style (lambda (idx) (vector (skim--fg-face #f #f #f) (skim--bg-face (skim--pal 'field-bg)) #f idx)))
         (button-style (lambda (idx) (vector (skim--fg-face (skim--pal 'button) #t #f) (skim--bg-face (skim--pal 'button-bg)) #f idx))))
    (cond
      ((equal? tag "select")
       (let* ((opts (map (lambda (o)
                           (let ((txt (skim--trim (replace-regexp-in-string "\\s+" " " (skim--text-content o)))))
                             (list (or (skim--attr o "value") txt) txt (skim--has-attr? o "selected"))))
                         (skim--find-all-list el (skim--tag? "option"))))
              (sel (let ((s (filter caddr opts))) (if (pair? s) (car s) (if (pair? opts) (car opts) (list "" "" #f)))))
              (idx (skim--add-field (hash 'kind "select" 'name name 'form form 'options opts
                                          'default (car sel) 'disabled disabled?)))
              (val (skim--field-value idx (car sel)))
              (label (let ((o (filter (lambda (o) (equal? (car o) val)) opts))) (if (pair? o) (cadr (car o)) val))))
         (skim--para-push! st (vector 'atom (string-append "[" (skim--truncate label 40) " ▾]") (field-style idx)))))
      ((equal? tag "textarea")
       (let* ((default (skim--text-content el))
              (idx (skim--add-field (hash 'kind "textarea" 'name name 'form form 'default default
                                          'label (or (skim--attr el "placeholder") name) 'disabled disabled?)))
              (val (skim--field-value idx default))
              (cols (min 60 (max 20 (or (skim--num (skim--attr el "cols")) 40))))
              (first (skim--truncate (replace-regexp-in-string "\\s+" " " val) cols)))
         (skim--para-push! st (vector 'atom (string-append "[" (skim--pad-right first cols) "]") (field-style idx)))))
      ((equal? tag "button")
       (let* ((label (skim--trim (replace-regexp-in-string "\\s+" " " (skim--text-content el))))
              (label (if (equal? label "") (or (skim--attr el "aria-label") (skim--attr el "title") "") label))
              (idx (skim--add-field (hash 'kind (if (member type '("submit" "reset")) type "button")
                                          'name name 'value (or (skim--attr el "value") "")
                                          'form form 'disabled disabled?))))
         ;; an icon-only button with no accessible name shows nothing
         (unless (equal? (skim--trim label) "")
           (skim--para-push! st (vector 'atom (string-append "[ " label " ]") (button-style idx))))))
      ((equal? type "hidden")
       (skim--add-field (hash 'kind "hidden" 'name name 'form form 'default (or (skim--attr el "value") ""))))
      ((member type '("checkbox" "radio"))
       (let* ((idx (skim--add-field (hash 'kind type 'name name 'form form
                                          'value (or (skim--attr el "value") "on")
                                          'default (skim--has-attr? el "checked") 'disabled disabled?)))
              (on? (skim--field-value idx (skim--has-attr? el "checked"))))
         (skim--para-push! st (vector 'atom (if (equal? type "radio") (if on? "(•)" "( )") (if on? "[x]" "[ ]"))
                                      (field-style idx)))))
      ((member type '("submit" "reset" "button" "image"))
       (let* ((nonempty (lambda (v) (and v (not (equal? (skim--trim v) "")) v)))
              (label (or (nonempty (skim--attr el "value")) (nonempty (skim--attr el "alt"))
                         (nonempty (skim--attr el "aria-label")) (nonempty (skim--attr el "title"))
                         (if (equal? type "reset") "Reset" (if (equal? type "submit") "Submit" "button"))))
              (idx (skim--add-field (hash 'kind (if (equal? type "image") "submit" type) 'name name
                                          'value (or (skim--attr el "value") "") 'form form 'disabled disabled?))))
         (skim--para-push! st (vector 'atom (string-append "[ " label " ]") (button-style idx)))))
      ((equal? type "file")
       (skim--para-push! st (vector 'atom "[file upload unsupported]" (vector (skim--dim-face) #f #f #f))))
      (else
       (let* ((default (or (skim--attr el "value") ""))
              (placeholder (or (skim--attr el "placeholder") (skim--attr el "aria-label") ""))
              (idx (skim--add-field (hash 'kind "text" 'type type 'name name 'form form 'default default
                                          'label (if (equal? placeholder "") name placeholder)
                                          'disabled disabled?)))
              (val (skim--field-value idx default))
              (size (min 40 (max 8 (or (skim--num (skim--attr el "size")) 20))))
              (shown (if (equal? type "password") (make-string (string-length val) #\*) val))
              (text (if (equal? shown "") placeholder shown))
              (text (skim--pad-right (skim--truncate text size) size)))
         (skim--para-push! st (vector 'atom (string-append "[" text "]")
                                      (if (equal? shown "")
                                          (vector (skim--dim-face) (skim--bg-face (skim--pal 'field-bg)) #f idx)
                                          (field-style idx)))))))))
