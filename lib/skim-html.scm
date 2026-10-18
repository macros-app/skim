;; skim-html.scm — HTML → DOM.
;;
;; A forgiving HTML5-ish parser built for speed in Steel:
;;
;;   1. pre-clean with native regex passes (comments, <script>, <svg>, …),
;;      turning each <style> body into a fenced CSS token;
;;   2. fence every tag with S1 and cut the document with ONE native
;;      `split-many` — pieces alternate text / tag;
;;   3. walk the pieces with an open-element stack, applying the HTML5
;;      implied-end-tag rules that matter for rendering (p, li, dt/dd, table
;;      parts, option, headings, nested a).
;;
;; Text nodes are plain (entity-decoded) strings; elements are tagged
;; vectors (see skim--el).

;; An element is #('skim-el tag attrs kids id style-cache cell-cache): tag a
;; lowercase string, attrs an alist, kids a list, id unique; the two caches
;; are per-render (see skim--compute-cached / skim--cell-lines), stamped with
;; the render generation. Tagged vectors rather than a struct so reloading
;; the package in a running editor never mixes two struct types.
(define skim--el-counter 0)
(define (skim--el tag attrs kids)
  (set! skim--el-counter (+ skim--el-counter 1))
  (mutable-vector 'skim-el tag attrs kids skim--el-counter #f #f))
(define (skim--el? x) (and (vector? x) (= (vector-length x) 7) (eq? (vector-ref x 0) 'skim-el)))
(define (skim--el-tag e) (vector-ref e 1))
(define (skim--el-attrs e) (vector-ref e 2))
(define (skim--el-kids e) (vector-ref e 3))

(define (skim--attr el name)
  (let ((v (skim--aget (skim--el-attrs el) name))) v))

(define (skim--has-attr? el name)
  (if (assoc name (skim--el-attrs el)) #t #f))

(define skim--void-tags
  (hash "area" #t "base" #t "br" #t "col" #t "embed" #t "hr" #t "img" #t "input" #t
        "link" #t "meta" #t "param" #t "source" #t "track" #t "wbr" #t "keygen" #t "frame" #t))

;; Opening one of these closes an open <p> (HTML "closes a p element").
(define skim--p-closers
  (hash "address" #t "article" #t "aside" #t "blockquote" #t "center" #t "details" #t
        "dialog" #t "dir" #t "div" #t "dl" #t "fieldset" #t "figcaption" #t "figure" #t
        "footer" #t "form" #t "h1" #t "h2" #t "h3" #t "h4" #t "h5" #t "h6" #t "header" #t
        "hgroup" #t "hr" #t "main" #t "menu" #t "nav" #t "ol" #t "p" #t "pre" #t
        "section" #t "summary" #t "table" #t "ul" #t "li" #t "dd" #t "dt" #t "listing" #t
        "plaintext" #t "xmp" #t))

(define skim--headings (hash "h1" #t "h2" #t "h3" #t "h4" #t "h5" #t "h6" #t))

;; Elements that bound the search for an open element (HTML's "scope").
(define skim--scope-default (hash "table" #t "td" #t "th" #t "caption" #t "html" #t
                                  "template" #t "object" #t "marquee" #t "applet" #t))
(define skim--scope-button (hash "table" #t "td" #t "th" #t "caption" #t "html" #t
                                 "template" #t "object" #t "button" #t))
(define skim--scope-list (hash "table" #t "td" #t "th" #t "caption" #t "html" #t
                               "template" #t "ul" #t "ol" #t "menu" #t))
(define skim--scope-dl (hash "table" #t "td" #t "th" #t "caption" #t "html" #t "dl" #t))
(define skim--scope-table (hash "table" #t "html" #t "template" #t))
(define skim--scope-row (hash "tr" #t "table" #t "html" #t "template" #t))
(define skim--scope-select (hash "select" #t "datalist" #t "html" #t))

(define skim--table-tags (hash "td" #t "th" #t "tr" #t "tbody" #t "thead" #t "tfoot" #t
                               "table" #t "caption" #t "colgroup" #t "col" #t))

(define skim--h-p (hash "p" #t))
(define skim--h-li (hash "li" #t))
(define skim--h-dtdd (hash "dt" #t "dd" #t))
(define skim--h-cell (hash "td" #t "th" #t))
(define skim--h-tr (hash "tr" #t))
(define skim--h-tsect (hash "tbody" #t "thead" #t "tfoot" #t))
(define skim--h-option (hash "option" #t))
(define skim--h-optgroup (hash "optgroup" #t "option" #t))
(define skim--h-a (hash "a" #t))
(define skim--h-button (hash "button" #t))

;; ── pre-clean ───────────────────────────────────────────────────────────────
(define (skim--strip-element html tag)
  (replace-regexp-in-string
   (string-append "(?is)<" tag "\\b[^>]*>.*?</" tag "\\s*>") " " html))

(define (skim--preclean html)
  (let* ((s (replace-regexp-in-string "[\\x01-\\x04]" "" html))
         (s (replace-regexp-in-string "(?s)<!--.*?-->" "" s))
         (s (skim--strip-element s "script"))
         (s (skim--strip-element s "template"))
         (s (skim--strip-element s "svg"))
         (s (skim--strip-element s "math"))
         ;; <style> bodies become S1 S4 css S1 tokens.
         (s (replace-regexp-in-string "(?is)<style\\b[^>]*>(.*?)</style\\s*>"
                                      (string-append skim--S1 skim--S4 "${1}" skim--S1) s))
         (s (replace-regexp-in-string "(?s)<!\\[CDATA\\[(.*?)\\]\\]>" "${1}" s))
         (s (replace-regexp-in-string "(?s)<![^>]*>" "" s))
         (s (replace-regexp-in-string "(?s)<\\?[^>]*>" "" s)))
    s))

;; ── tokens ──────────────────────────────────────────────────────────────────
;; Cut the document into alternating text / tag pieces. A tag piece is
;; "name S2 attrs" (name keeps a leading "/" for end tags); a CSS piece starts
;; with S4.
(define (skim--tokenize html)
  (split-many
   (replace-regexp-in-string
    "<(/?[A-Za-z][A-Za-z0-9:_-]*)((?:[^>\"']|\"[^\"]*\"|'[^']*'|[\"'])*)>"
    (string-append skim--S1 "${1}" skim--S2 "${2}" skim--S1)
    (skim--preclean html))
   skim--S1))

;; Attribute strings for every tag are parsed in ONE native regex pass: the
;; strings are joined with S4, each `name=value` is fenced as S1 name S3 value
;; S1, and the result is split back per tag. Returns a list of alists aligned
;; with `attr-strings`.
(define skim--attr-re
  "([^\\s\"'>/=\\x01-\\x04]+)(?:\\s*=\\s*(?:\"([^\"\\x04]*)\"|'([^'\\x04]*)'|([^\\s\"'>\\x04]+)))?")

(define (skim--parse-attrs-batch attr-strings)
  (let* ((joined (string-join attr-strings skim--S4))
         (fenced (replace-regexp-in-string
                  skim--attr-re (string-append skim--S1 "${1}" skim--S3 "${2}${3}${4}" skim--S1) joined)))
    (map skim--attrs-from-fenced (split-many fenced skim--S4))))

(define (skim--attrs-from-fenced f)
  (if (not (string-contains? f skim--S1))
      '()
      (let loop ((ps (split-many f skim--S1)) (i 0) (acc '()))
        (if (null? ps)
            (reverse acc)
            (loop (cdr ps) (+ i 1)
                  (if (even? i)
                      acc
                      (let* ((kv (split-many (car ps) skim--S3))
                             (k (skim--lower (car kv)))
                             (v (if (null? (cdr kv)) "" (skim--entities (cadr kv)))))
                        (if (assoc k acc) acc (cons (cons k v) acc)))))))))

;; ── tree building ───────────────────────────────────────────────────────────
;; The stack holds mutable frames #(tag attrs kids-reversed); closing a frame
;; turns it into a `skim-el` appended to its parent.
(define (skim--frame tag attrs) (mutable-vector tag attrs '()))
(define (skim--frame-tag f) (vector-ref f 0))
(define (skim--frame-add! f node) (vector-set! f 2 (cons node (vector-ref f 2))))
(define (skim--frame->el f) (skim--el (vector-ref f 0) (vector-ref f 1) (reverse (vector-ref f 2))))

;; Is an element named in `names` (a hash) open before hitting a scope boundary?
(define (skim--in-scope? stack names boundary)
  (let loop ((st stack))
    (cond ((null? (cdr st)) #f)                       ; never pop the root
          ((hash-contains? names (skim--frame-tag (car st))) #t)
          ((hash-contains? boundary (skim--frame-tag (car st))) #f)
          (else (loop (cdr st))))))

;; Pop (closing) frames until one named in `names` has been popped.
(define (skim--pop-until stack names)
  (let loop ((st stack))
    (if (null? (cdr st))
        st
        (let* ((f (car st)) (rest (cdr st)))
          (skim--frame-add! (car rest) (skim--frame->el f))
          (if (hash-contains? names (skim--frame-tag f)) rest (loop rest))))))

(define (skim--close-if-open stack names boundary)
  (if (skim--in-scope? stack names boundary) (skim--pop-until stack names) stack))

;; Implied end tags before opening `tag`.
(define (skim--before-open stack tag)
  (let* ((st (if (hash-contains? skim--p-closers tag)
                 (skim--close-if-open stack skim--h-p skim--scope-button)
                 stack))
         (st (cond
               ((equal? tag "li") (skim--close-if-open st skim--h-li skim--scope-list))
               ((or (equal? tag "dt") (equal? tag "dd"))
                (skim--close-if-open st skim--h-dtdd skim--scope-dl))
               ((or (equal? tag "td") (equal? tag "th"))
                (skim--close-if-open st skim--h-cell skim--scope-row))
               ((equal? tag "tr")
                (skim--close-if-open st skim--h-tr skim--scope-table))
               ((or (equal? tag "tbody") (equal? tag "thead") (equal? tag "tfoot"))
                (skim--close-if-open st skim--h-tsect skim--scope-table))
               ((equal? tag "option")
                (skim--close-if-open st skim--h-option skim--scope-select))
               ((equal? tag "optgroup")
                (skim--close-if-open st skim--h-optgroup skim--scope-select))
               ((equal? tag "a") (skim--close-if-open st skim--h-a skim--scope-default))
               ((equal? tag "button") (skim--close-if-open st skim--h-button skim--scope-default))
               ((hash-contains? skim--headings tag)
                (if (hash-contains? skim--headings (skim--frame-tag (car st)))
                    (skim--pop-until st skim--headings)
                    st))
               (else st))))
    st))

;; Boundary for an END tag's search: table parts may cross anything up to
;; their table; everything else stops at cells so a stray </div> inside a
;; cell can't close the table around it.
(define (skim--end-boundary tag)
  (cond ((hash-contains? skim--table-tags tag) skim--scope-table)
        ((equal? tag "li") skim--scope-list)
        ((or (equal? tag "dd") (equal? tag "dt")) skim--scope-dl)
        (else skim--scope-default)))

;; Parse `html` → (list root-el css-texts). The root is a synthetic "#root"
;; element; css-texts are the <style> bodies in document order.
(define (skim--parse-html html)
  (let* ((toks (skim--tokenize html))
         (attr-strs (let collect ((ps toks) (i 0) (acc '()))
                      (if (null? ps)
                          (reverse acc)
                          (collect (cdr ps) (+ i 1)
                                   (if (or (even? i) (starts-with? (car ps) skim--S4)
                                           (starts-with? (car ps) "/"))
                                       acc
                                       (let ((x (split-many (car ps) skim--S2)))
                                         (cons (if (null? (cdr x)) "" (cadr x)) acc)))))))
         (attrs-all (skim--parse-attrs-batch attr-strs)))
   (let loop ((ps toks)
             (i 0)
             (stack (list (skim--frame "#root" '())))
             (css '())
             (attrs-all attrs-all))
    (if (null? ps)
        (let ((st (let close ((st stack))
                    (if (null? (cdr st)) st
                        (begin (skim--frame-add! (cadr st) (skim--frame->el (car st)))
                               (close (cdr st)))))))
          (list (skim--frame->el (car st)) (reverse css)))
        (let ((p (car ps)))
          (cond
            ;; text
            ((even? i)
             (when (not (equal? p ""))
               (skim--frame-add! (car stack) (skim--entities p)))
             (loop (cdr ps) (+ i 1) stack css attrs-all))
            ;; css
            ((starts-with? p skim--S4)
             (loop (cdr ps) (+ i 1) stack (cons (substring p 1 (string-length p)) css) attrs-all))
            (else
             (let* ((parts (split-many p skim--S2))
                    (name (skim--lower (car parts)))
                    (rest (if (null? (cdr parts)) "" (cadr parts))))
               (if (starts-with? name "/")
                   (loop (cdr ps) (+ i 1) (skim--end-tag stack (substring name 1 (string-length name))) css attrs-all)
                   (let* ((self-closing (ends-with? rest "/"))
                          (attrs (car attrs-all))
                          (st (skim--before-open stack name)))
                     (if (or self-closing (hash-contains? skim--void-tags name))
                         (begin (skim--frame-add! (car st) (skim--el name attrs '()))
                                (loop (cdr ps) (+ i 1) st css (cdr attrs-all)))
                         (loop (cdr ps) (+ i 1) (cons (skim--frame name attrs) st) css (cdr attrs-all)))))))))))))

(define (skim--end-tag stack name)
  (cond
    ((equal? name "br")
     (skim--frame-add! (car stack) (skim--el "br" '() '()))
     stack)
    ((and (equal? name "p") (not (skim--in-scope? stack skim--h-p skim--scope-button)))
     (skim--frame-add! (car stack) (skim--el "p" '() '()))
     stack)
    ((hash-contains? skim--headings name)
     (skim--close-if-open stack skim--headings skim--scope-default))
    (else
     (let ((names (hash name #t)))
       (skim--close-if-open stack names (skim--end-boundary name))))))

;; ── DOM queries ─────────────────────────────────────────────────────────────
;; Depth-first search for the first element satisfying `pred`.
(define (skim--find-el node pred)
  (cond ((string? node) #f)
        ((pred node) node)
        (else (let loop ((ks (skim--el-kids node)))
                (if (null? ks)
                    #f
                    (or (skim--find-el (car ks) pred) (loop (cdr ks))))))))

;; Every element satisfying `pred`, document order.
(define (skim--find-all node pred)
  (let walk ((n node) (acc '()))
    (if (string? n)
        acc
        (let ((acc (if (pred n) (cons n acc) acc)))
          (let loop ((ks (skim--el-kids n)) (acc acc))
            (if (null? ks) acc (loop (cdr ks) (walk (car ks) acc))))))))

(define (skim--find-all-list node pred) (reverse (skim--find-all node pred)))

;; Concatenated text content of a node.
(define (skim--text-content node)
  (if (string? node)
      node
      (string-join (map skim--text-content (skim--el-kids node)) "")))

(define (skim--tag? tag) (lambda (el) (equal? (skim--el-tag el) tag)))
