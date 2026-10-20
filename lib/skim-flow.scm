;; skim-flow.scm — the layout walk: DOM + CSS → items.
;;
;; A classic block/inline flow in character cells: block boxes stack and
;; their vertical margins become collapsed blank lines; inline content
;; collects in a paragraph that wraps when the block closes; list items get
;; hanging markers, blockquotes a bar, headings a larger line scale; short
;; floats sit beside the content; flex rows of short items stay on one line.

;; ── dispatch ───────────────────────────────────────────────────────────
;; Tables (skim-tables.scm) lay out cells by walking their content, and the
;; walk lays out tables: the walker calls tables through this slot, which
;; skim-tables.scm fills once it loads (Steel compiles each file on its own,
;; so a file can only name functions from files loaded before it). It's a
;; one-slot box, not a plain variable: Steel treats a top-level binding its
;; own file never reassigns as a constant, so a later `set!` wouldn't show.
(define skim--table-impl (mutable-vector #f))

(define skim--block-displays
  (hash "block" #t "flex" #t "grid" #t "list-item" #t "table" #t "flow-root" #t
        "table-caption" #t "table-row" #t "table-row-group" #t "table-header-group" #t
        "table-footer-group" #t "table-cell" #t "-webkit-box" #t "ruby-base-container" #f))

;; ── walking children ───────────────────────────────────────────────────
;; Children of `el` in flow `ctx`, appended to `st`.
(define (skim--walk-kids el m ctx st)
  (let* ((kids (skim--el-kids el))
         (nel (let count ((ks kids) (n 0)) (if (null? ks) n (count (cdr ks) (if (string? (car ks)) n (+ n 1)))))))
    (skim--walk-list kids 0 #f #t nel m ctx st)))

;; Walk `ks` (siblings from element index `i`, after match context `prev`).
(define (skim--walk-list ks i prev first-el nel m ctx st)
  (when (pair? ks)
    (let ((k (car ks)))
      (if (string? k)
          (begin (skim--add-text! st k ctx) (skim--walk-list (cdr ks) i prev first-el nel m ctx st))
          (let ((km (skim--mctx k m prev (+ i 1) (= (+ i 1) nel))))
            (if (and (skim--para-empty? (vector-ref st 0))
                     (skim--short-float? k km ctx)
                     ;; inside a flex row only out-of-flow badges qualify
                     (or (not (hash-try-get ctx 'flex-inline))
                         (equal? (skim--style (skim--style-of k km ctx) "position")
                                 "absolute")))
                ;; a run of adjacent short floats lines up as one group
                (let gather ((rest (cdr ks)) (j (+ i 1)) (last km) (group (list (cons k km))))
                  (let* ((next (let skip ((xs rest)) (if (and (pair? xs) (string? (car xs)) (equal? (skim--trim (car xs)) "")) (skip (cdr xs)) xs)))
                         (nkm (and (pair? next) (skim--el? (car next))
                                   (skim--mctx (car next) m last (+ j 1) (= (+ j 1) nel)))))
                    (cond
                      ((and nkm (skim--short-float? (car next) nkm ctx))
                       (gather (cdr next) (+ j 1) nkm (cons (cons (car next) nkm) group)))
                      ;; floats push aside BLOCK content; before inline
                      ;; content (or nothing) they simply flow inline
                      ((and nkm (skim--block-level? (car next) nkm ctx))
                       (skim--float-layout (reverse group) rest j last nel m ctx st))
                      (else
                       (for-each (lambda (g)
                                   (skim--walk-el (car g) (cdr g) (hash-insert ctx 'inline-box #t) st)
                                   (skim--para-push! st (vector 'sp " " skim--plain-style)))
                                 (reverse group))
                       (skim--walk-list rest j last #f nel m ctx st)))))
                (begin
                  (when (and (hash-try-get ctx 'flex-inline) (not first-el))
                    (skim--para-push! st (vector 'sp "  " skim--plain-style)))
                  (skim--walk-el k km ctx st)
                  (skim--walk-list (cdr ks) (+ i 1) km #f nel m ctx st))))))))

;; Text directly inside the element whose context is `ctx`.
(define (skim--add-text! st s ctx)
  (let ((cs (hash-try-get ctx 'cs)))
    (unless (skim--invisible? cs)          ; visibility: hidden text shows nothing
      (let* ((ws (skim--ws-mode cs))
             (s (skim--transform-text s cs))
             (script (hash-try-get ctx 'script))
             (s (cond ((eq? script 'sup) (skim--script-text s skim--sup-map))
                      ((eq? script 'sub) (skim--script-text s skim--sub-map))
                      (else s))))
        (skim--para-push! st (vector 'text s (hash-try-get ctx 'tstyle) ws))))))

;; Child context for an element with computed style `cs`.
(define (skim--child-ctx ctx el cs)
  (let* ((ctx (hash-insert ctx 'cs cs))
         (ctx (hash-insert ctx 'flex-inline #f))
         (ctx (hash-insert ctx 'inline-box #f))
         (tag (skim--el-tag el))
         (ctx (if (and (equal? tag "a") (skim--attr el "href")
                       (not (starts-with? (skim--lower (skim--trim (skim--attr el "href"))) "javascript:")))
                  (hash-insert ctx 'link (skim--add-link (skim--resolve (skim--Rget 'base) (skim--attr el "href"))))
                  ctx))
         (ctx (if (hash-contains? skim--headings tag) (hash-insert ctx 'heading tag) ctx))
         (ctx (cond ((equal? tag "sup") (hash-insert ctx 'script 'sup))
                    ((equal? tag "sub") (hash-insert ctx 'script 'sub))
                    (else ctx)))
         (ctx (if (eq? (skim--ws-mode cs) 'pre) (hash-insert ctx 'pre #t) ctx))
         ;; Code styling: inside code-ish elements, or where an inline element
         ;; switches to a monospace font — not a site whose body font is mono.
         (mono? (skim--mono? cs))
         (ctx (hash-insert ctx 'code
                           (or (hash-try-get ctx 'code)
                               (and mono?
                                    (or (member tag '("code" "kbd" "samp" "tt" "pre" "xmp" "listing"))
                                        (and (not (hash-contains? skim--block-displays (skim--style cs "display")))
                                             (not (hash-try-get ctx 'mono))))
                                    #t))))
         (ctx (hash-insert ctx 'mono mono?))
         ;; backgrounds paint inline boxes only: a block (or display:contents,
         ;; which has no box) neither paints one nor inherits an ancestor's
         (block? (or (hash-contains? skim--block-displays (skim--style cs "display"))
                     (equal? (skim--style cs "display") "contents")))
         (ctx (if block? (hash-insert (hash-insert ctx 'bg-face #f) 'bg-rgb #f) ctx))
         (ctx (let ((bg (and (not block?)
                             (skim--opt-bool "skim-css-colors" #t)
                             (skim--parse-color (skim--style cs "background-color")))))
                (if (and bg
                         (let ((fg (or (skim--parse-color (skim--style cs "color")) (skim--pal 'fg))))
                           (>= (skim--contrast fg bg) 3.0))
                         (>= (skim--contrast bg (skim--pal 'bg)) 1.6))
                    (hash-insert (hash-insert ctx 'bg-rgb bg) 'bg-face (skim--bg-face (skim--color->hex bg)))
                    ctx))))
    (hash-insert ctx 'tstyle (skim--text-style cs ctx))))

;; ── elements ───────────────────────────────────────────────────────────
(define (skim--walk-el el m ctx st)
  (let* ((r (skim--compute-cached el m (hash-try-get ctx 'cs) (hash-try-get ctx 'sheet)))
         (cs (car r))
         (tag (skim--el-tag el))
         (display (skim--style cs "display")))
    (cond
      ((skim--gone? cs) #t)
      ((skim--invisible? cs) (skim--walk-invisible el m cs ctx st))
      ((equal? tag "br") (skim--para-push! st (vector 'br)))
      ((equal? tag "wbr") #t)
      ((equal? tag "img") (skim--image el cs ctx st))
      ((member tag '("input" "select" "textarea" "button"))
       (skim--widget el m cs ctx st))
      ((and (member tag '("iframe" "video" "audio" "embed" "object"))
            (skim--media el cs ctx st))
       #t)
      ((equal? tag "hr") (skim--hr cs ctx st))
      (else
       (let* ((cctx (skim--child-ctx ctx el cs))
              (cctx (if (equal? tag "form") (hash-insert cctx 'form (skim--add-form el)) cctx))
              ;; a qualifying flex row spaces its items whether it ends up
              ;; laid out as a block or inline inside an outer flex row
              (cctx (if (skim--flex-inline? el cs) (hash-insert cctx 'flex-inline #t) cctx))
              (id (or (skim--attr el "id") (and (equal? tag "a") (skim--attr el "name")))))
         (cond
           ((equal? display "contents")
            (when id (skim--para-push! st (vector 'anchor id)))
            (skim--walk-inline el m cs r cctx st))
           ;; A <table> restyled display:block (responsive tables) still lays
           ;; its rows out as a table — the browser wraps them in an anonymous
           ;; table box.
           ((and (equal? tag "table") (member display '("table" "inline-table" "block" "inline-block")))
            (skim--flush! st ctx)
            (skim--block-margins-wrap st cs ctx el
                                      (lambda (w) ((vector-ref skim--table-impl 0) el m cs (hash-insert cctx 'width w)))
                                      id))
           ;; a short float that isn't wrapped (see skim--float-layout) flows inline
           ;; (menu items with hidden submenus included — their text counts)
           ((and (hash-contains? skim--block-displays display)
                 (member (skim--style cs "float") '("left" "right"))
                 (< (string-length (skim--squeeze (skim--text-content el))) 200)
                 (not (skim--find-el el (lambda (e) (member (skim--el-tag e) '("table" "p" "img"))))))
            (when id (skim--para-push! st (vector 'anchor id)))
            (skim--inline-box! el m cs r cctx st)
            (skim--para-push! st (vector 'sp " " skim--plain-style)))
           ((hash-contains? skim--block-displays display)
            (if (or (hash-try-get ctx 'flex-inline) (hash-try-get ctx 'inline-box))
                (skim--inline-box! el m cs r cctx st)
                (skim--block el m cs r cctx ctx st id)))
           ;; a short inline-block holding blocks (a <details> "caches" menu
           ;; in a byline) stays one unit in the line rather than breaking it
           ((and (member display '("inline-block" "inline-flex"))
                 (< (string-length (skim--squeeze (skim--text-content el))) 200)
                 (skim--holds-block? el m cctx))
            (when id (skim--para-push! st (vector 'anchor id)))
            (skim--inline-box! el m cs r cctx st))
           (else
            (when id (skim--para-push! st (vector 'anchor id)))
            (skim--walk-inline el m cs r cctx st))))))))

;; A visibility: hidden element: its own text is suppressed (see
;; skim--add-text!), but descendants that set visibility: visible show, so
;; its content is laid out aside and kept only if any of it is visible.
(define (skim--walk-invisible el m cs ctx st)
  (let ((sub (skim--new-st))
        (cctx (skim--child-ctx ctx el cs)))
    (skim--walk-kids el m cctx sub)
    (skim--flush! sub cctx)
    (let ((items (skim--st-items sub)))
      (when (let any ((xs items))
              (and (pair? xs)
                   (or (and (skim--line? (car xs)) (not (equal? (skim--trim (skim--line-text (car xs))) "")))
                       (any (cdr xs)))))
        (skim--flush! st ctx)
        (skim--out-append! st items)))))

;; Inline element: ::before, kids, ::after into the current paragraph.
;; Horizontal margin/padding of 4px or more becomes a breakable space.
(define (skim--walk-inline el m cs r cctx st)
  (when (skim--inline-gap? cs "left") (skim--para-push! st (vector 'sp " " skim--plain-style)))
  (when (cadr r) (skim--para-push! st (vector 'text (cadr r) (hash-try-get cctx 'tstyle) 'nowrap)))
  (skim--walk-kids el m cctx st)
  (when (caddr r) (skim--para-push! st (vector 'text (caddr r) (hash-try-get cctx 'tstyle) 'nowrap)))
  (when (skim--inline-gap? cs "right") (skim--para-push! st (vector 'sp " " skim--plain-style))))

(define (skim--inline-gap? cs side)
  (let ((m (skim--px (skim--style cs (string-append "margin-" side))))
        (p (skim--px (skim--style cs (string-append "padding-" side)))))
    (>= (+ (if (and m (> m 0)) m 0) (if (and p (> p 0)) p 0)) 4)))

;; ── floats and compact boxes ───────────────────────────────────────────
;; A short box beside the content: a left float, or a small absolutely
;; positioned badge (a date in the margin). Vote counts, dates, breadcrumb
;; items, icon captions.
(define (skim--short-float? k km ctx)
  (let* ((r (skim--compute-cached k km (hash-try-get ctx 'cs) (hash-try-get ctx 'sheet)))
         (cs (car r)))
    (and (or (equal? (skim--style cs "float") "left")
             (equal? (skim--style cs "position") "absolute"))
         (not (skim--hidden? cs))
         (hash-contains? skim--block-displays (skim--style cs "display"))
         (let ((t (skim--squeeze (skim--text-content k))))
           (and (> (string-length t) 0) (< (string-length t) 24)))
         (not (let loop ((ks (skim--el-kids k)))
                (and (pair? ks)
                     (or (and (skim--el? (car ks))
                              (skim--find-el (car ks) (lambda (e) (member (skim--el-tag e) '("table" "ul" "ol" "p" "img")))))
                         (loop (cdr ks)))))))))

;; Is `k` a visible, non-floated block-level box?
(define (skim--block-level? k km ctx)
  (let ((cs (skim--style-of k km ctx)))
    (and (not (skim--hidden? cs))
         (hash-contains? skim--block-displays (skim--style cs "display"))
         (not (member (skim--style cs "float") '("left" "right"))))))

;; Does `el` have a block-level child? `ctx` is el's child context.
(define (skim--holds-block? el m ctx)
  (let* ((kids (filter skim--el? (skim--el-kids el))) (n (length kids)))
    (let loop ((ks kids) (i 0) (prev #f))
      (and (pair? ks)
           (let ((km (skim--mctx (car ks) m prev (+ i 1) (= (+ i 1) n))))
             (or (skim--block-level? (car ks) km ctx)
                 (loop (cdr ks) (+ i 1) km)))))))

;; Lay `el` out as a compact unit (a float or flex item whose content may
;; hold blocks), flatten it to one line and flow that line into the
;; paragraph. Boxes holding images keep normal inline flow (an image needs a
;; line of its own).
(define (skim--inline-box! el m cs r cctx st)
  (let* ((tmp (skim--new-st))
         (bctx (hash-insert (hash-insert cctx 'width 200) 'align-left #t)))
    (skim--walk-inline el m cs r bctx tmp)
    (skim--flush! tmp bctx)
    (let ((lines (filter skim--line? (skim--resolve-items (skim--st-items tmp)))))
      (cond
        ((null? lines) #t)
        ((let any ((ls lines)) (and (pair? ls) (or (skim--meta-get (car ls) 'img) (any (cdr ls)))))
         (skim--walk-inline el m cs r cctx st))
        (else
         (for-each (lambda (it) (skim--para-push! st it))
                   (skim--line->items (skim--join-lines lines))))))))

;; Lay out the siblings after a float group, then put the group's text in
;; the first line's left margin (shifting the content right only as far as
;; needed), the way content wraps beside floats.
(define (skim--float-layout group rest i last nel m ctx st)
  (skim--flush! st ctx)                 ; only whitespace (and anchors) pending
  (let* ((fctx (hash-insert (hash-insert ctx 'width 200) 'align-left #t))
         (fst (skim--new-st))
         (_ (for-each (lambda (g)
                        (skim--walk-el (car g) (cdr g) fctx fst)
                        (skim--para-push! fst (vector 'sp " " skim--plain-style)))
                      group))
         (_ (skim--flush! fst fctx))
         (fline (skim--join-lines (filter skim--line? (skim--resolve-items (skim--st-items fst)))))
         (ftext (skim--line-text fline))
         (fspans (skim--line-spans fline))
         (css-cols (foldl (lambda (g n)
                            (let ((px (skim--px (skim--style (skim--style-of (car g) (cdr g) ctx) "width"))))
                              (+ n (if px (skim--int (/ (+ px 4) 8)) 0))))
                          0 group))
         (fw (max (+ (string-length ftext) 1) (min 16 css-cols)))
         (sub (skim--new-st)))
    (skim--walk-list rest i last #f nel m ctx sub)
    (skim--flush! sub ctx)
    (let* ((items0 (skim--st-items sub))
           (first-text (let find-first ((xs items0)) (cond ((null? xs) #f) ((skim--line? (car xs)) (skim--line-text (car xs))) (else (find-first (cdr xs))))))
           (lead (if first-text (skim--lead-spaces first-text) 0))
           (d (max 0 (- fw lead)))
           (items (if (> d 0) (skim--prefix items0 (skim--spaces d) (skim--spaces d) #f) items0)))
      (if (not first-text)
          (skim--out-push! st (skim--line ftext fspans '()))
          (let loop ((xs items) (done #f))
            (when (pair? xs)
              (let ((x (car xs)))
                (if (and (not done) (skim--line? x))
                    (let ((t (skim--line-text x)))
                      (skim--out-push! st (skim--line (string-append ftext (substring t (min (string-length t) (string-length ftext)) (string-length t)))
                                                      (append fspans (skim--line-spans x))
                                                      (skim--line-meta x)))
                      (loop (cdr xs) #t))
                    (begin (skim--out-push! st x) (loop (cdr xs) done))))))))))

;; ── blocks ─────────────────────────────────────────────────────────────
(define skim--spacing-floor (hash "p" 1 "h1" 1 "h2" 1 "h3" 1 "h4" 1 "h5" 1 "h6" 1 "pre" 1
                                  "blockquote" 1 "table" 1 "figure" 1 "hr" 0 "dl" 1))

(define (skim--gap-lines cs side tag ctx)
  (let* ((m (skim--px (skim--style cs (string-append "margin-" side))))
         (p (skim--px (skim--style cs (string-append "padding-" side))))
         (px (+ (if (and m (> m 0)) m 0) (if (and p (> p 0)) p 0)))
         (n (cond ((>= px 48) 2) ((>= px 10) 1) (else 0)))
         (floor (if (and (skim--opt-bool "skim-spacing-floor" #t) (not (hash-try-get ctx 'in-cell)))
                    (or (hash-try-get skim--spacing-floor tag) 0) 0)))
    (max n floor)))

(define (skim--indent-cols cs tag)
  (let* ((m (skim--px (skim--style cs "margin-left")))
         (p (skim--px (skim--style cs "padding-left")))
         (px (+ (if (and m (> m 0)) m 0) (if (and p (> p 0)) p 0))))
    (min (if (member tag '("ul" "ol" "menu" "dir" "blockquote")) 2 4) (skim--int (/ (+ px 4) 8)))))

;; Wrap a block renderer with its margins, indentation and anchor:
;; `render` takes the inner width and returns items.
(define (skim--block-margins-wrap st cs ctx el render id)
  (let* ((tag (skim--el-tag el))
         (width (hash-try-get ctx 'width))
         (ind (skim--indent-cols cs tag))
         (ind (if (< (- width ind) 24) 0 ind))
         ;; max-width narrows the box; margin auto on both sides centers it
         (mw (let ((px (skim--px (skim--style cs "max-width"))))
               (and px (> px 0) (max 20 (skim--int (/ px 8))))))
         (inner (if (and mw (< mw (- width ind))) mw (- width ind)))
         (ind (if (and mw (< mw (- width ind))
                       (equal? (skim--style cs "margin-left") "auto")
                       (equal? (skim--style cs "margin-right") "auto"))
                  (+ ind (quotient (- width ind inner) 2))
                  ind))
         (items (render inner))
         (items (if (> ind 0) (skim--prefix items (skim--spaces ind) (skim--spaces ind) #f) items)))
    (when id (skim--out-push! st (vector 'anchor id)))
    (skim--out-push! st (skim--gap-lines cs "top" tag ctx))
    (skim--out-append! st items)
    (skim--out-push! st (skim--gap-lines cs "bottom" tag ctx))))

(define (skim--heading-scale tag)
  (if (not (skim--opt-bool "skim-heading-scale" #t))
      1
      (cond ((equal? tag "h1") 1.6) ((equal? tag "h2") 1.35) ((equal? tag "h3") 1.15) (else 1))))

;; A flex row whose element children are all short and simple renders on
;; one line, like the navigation bars it usually is.
(define skim--flex-blockers (hash "p" #t "ul" #t "ol" #t "table" #t "h1" #t "h2" #t "h3" #t
                                  "h4" #t "section" #t "article" #t "pre" #t "form" #t
                                  "blockquote" #t "figure" #t))

(define (skim--flex-inline? el cs)
  (and (member (skim--style cs "display") '("flex" "inline-flex"))
       (not (string-contains? (skim--style cs "flex-direction") "column"))
       (let ((kids (filter skim--el? (skim--el-kids el))))
         (and (pair? kids) (<= (length kids) 16)
              (let loop ((ks kids))
                (or (null? ks)
                    (and (< (string-length (skim--squeeze (skim--text-content (car ks)))) 80)
                         (not (skim--find-el (car ks) (lambda (e) (hash-contains? skim--flex-blockers (skim--el-tag e)))))
                         (loop (cdr ks)))))))))

;; Render a block element into `st`.
(define (skim--block el m cs r cctx ctx st id)
  (skim--flush! st ctx)
  (let* ((tag (skim--el-tag el))
         (display (skim--style cs "display"))
         (list-item? (equal? display "list-item"))
         (cctx (if (member tag '("ul" "ol" "menu" "dir"))
                   (hash-insert cctx 'list (skim--new-counter el cs))
                   cctx))
         (cctx (if (skim--flex-inline? el cs) (hash-insert cctx 'flex-inline #t) cctx)))
    (skim--block-margins-wrap
     st cs ctx el
     (lambda (w)
       (let* ((scale (skim--heading-scale tag))
              (marker (and list-item? (skim--list-marker el cs ctx)))
              (quote? (equal? tag "blockquote"))
              (summary? (and (equal? tag "summary") (not (equal? (skim--style cs "list-style-type") "none"))))
              (pw (cond (marker (string-length marker)) (quote? 2) (summary? 2) (else 0)))
              (inner (max 8 (skim--int (/ (- w pw) scale))))
              (sub (skim--new-st))
              (ictx (hash-insert cctx 'width inner)))
         (when (cadr r) (skim--para-push! sub (vector 'text (cadr r) (hash-try-get ictx 'tstyle) 'nowrap)))
         (skim--walk-kids el m ictx sub)
         (when (caddr r) (skim--para-push! sub (vector 'text (caddr r) (hash-try-get ictx 'tstyle) 'nowrap)))
         (skim--flush! sub ictx)
         (let* ((items (skim--st-items sub))
                (items (if (> scale 1)
                           (map (lambda (x) (if (skim--line? x) (skim--meta-put x 'scale scale) x)) items)
                           items)))
           (cond
             (marker (skim--prefix items marker (skim--spaces pw) #f))
             (quote? (skim--prefix items "│ " "│ " (skim--dim-face)))
             (summary? (skim--prefix items
                                     (let ((p (vector-ref m 2)))
                                       (if (and p (skim--has-attr? (vector-ref p 0) "open")) "▾ " "▸ "))
                                     "  " #f))
             (else items)))))
     id)))

;; ── lists ──────────────────────────────────────────────────────────────
;; A counter box #(next step type) shared by a list's items.
(define (skim--new-counter el cs)
  (let* ((reversed? (skim--has-attr? el "reversed"))
         (n-items (length (filter (lambda (k) (and (skim--el? k) (equal? (skim--el-tag k) "li"))) (skim--el-kids el))))
         (start (or (skim--num (skim--attr el "start")) (if reversed? n-items 1))))
    (mutable-vector start (if reversed? -1 1) (max start n-items (+ start n-items -1)))))

(define (skim--roman n upper?)
  (let loop ((n n) (pairs '((1000 . "m") (900 . "cm") (500 . "d") (400 . "cd") (100 . "c") (90 . "xc")
                            (50 . "l") (40 . "xl") (10 . "x") (9 . "ix") (5 . "v") (4 . "iv") (1 . "i")))
             (acc ""))
    (cond ((or (<= n 0) (null? pairs)) (if upper? (string-upcase acc) acc))
          ((>= n (car (car pairs))) (loop (- n (car (car pairs))) pairs (string-append acc (cdr (car pairs)))))
          (else (loop n (cdr pairs) acc)))))

(define (skim--alpha n upper?)
  (let loop ((n n) (acc '()))
    (if (<= n 0)
        (let ((s (list->string acc))) (if upper? (string-upcase s) s))
        (let ((q (quotient (- n 1) 26)) (r (remainder (- n 1) 26)))
          (loop q (cons (integer->char (+ 97 r)) acc))))))

(define (skim--format-counter n type)
  (cond ((member type '("lower-alpha" "lower-latin")) (string-append (skim--alpha n #f) "."))
        ((member type '("upper-alpha" "upper-latin")) (string-append (skim--alpha n #t) "."))
        ((equal? type "lower-roman") (string-append (skim--roman n #f) "."))
        ((equal? type "upper-roman") (string-append (skim--roman n #t) "."))
        ((equal? type "decimal-leading-zero") (string-append (if (< n 10) "0" "") (number->string n) "."))
        ((equal? type "lower-greek") (string-append (string (integer->char (+ 944 (min n 24)))) "."))
        (else (string-append (number->string n) "."))))

;; The marker prefix for a list item (padded so its text hangs), or "" for
;; list-style none.
(define (skim--list-marker el cs ctx)
  (let* ((type (skim--style cs "list-style-type"))
         (counter (hash-try-get ctx 'list))
         (value (skim--num (skim--attr el "value"))))
    (when (and counter value) (vector-set! counter 0 value))
    (let ((n (if counter (vector-ref counter 0) 1)))
      (when counter (vector-set! counter 0 (+ n (vector-ref counter 1))))
      (cond
        ((equal? type "none") "")
        ((equal? type "disc") "• ")
        ((equal? type "circle") "◦ ")
        ((equal? type "square") "▪ ")
        (else
         (let* ((s (skim--format-counter n type))
                (w (if counter (string-length (skim--format-counter (vector-ref counter 2) type)) (string-length s))))
           (string-append (skim--spaces (- w (string-length s))) s " ")))))))

;; ── document ───────────────────────────────────────────────────────────
;; Lay out `root` → hash: 'lines (list) 'links 'fields 'forms 'title.
(define (skim--layout root sheet base width field-values)
  (skim--R-reset! base field-values)
  (set! skim--R (hash-insert skim--R 'doc-text (string-length (skim--squeeze (skim--text-content root)))))
  (let* ((st (skim--new-st))
         (ctx (hash 'width width 'cs skim--root-style 'sheet sheet 'tstyle skim--plain-style))
         (m (skim--mctx root #f #f 1 #t)))
    (skim--walk-kids root m ctx st)
    (skim--flush! st ctx)
    (hash 'lines (skim--resolve-items (skim--st-items st))
          'links (skim--Rget 'links)
          'fields (skim--Rget 'fields)
          'forms (skim--Rget 'forms))))
