;; skim-css.scm — a small CSS engine for skim.
;;
;; Parses stylesheets (<style>, linked sheets, style="" attributes and a
;; built-in user-agent sheet), matches selectors, runs the cascade and
;; computes the handful of properties a text renderer can honour:
;;
;;   display visibility font-weight font-style font-family font-variant
;;   text-decoration text-align text-transform white-space list-style-type
;;   margin-{top,bottom,left} padding-{top,bottom,left} color background-color
;;   content (::before/::after) position clip clip-path width height overflow
;;   flex-direction opacity — plus custom properties and var().
;;
;; Selectors: type, *, .class, #id, [attr], [attr=|~=|^=|$=|*=||=v (i)],
;; descendant, >, +, ~, :first-child :last-child :only-child :root :empty
;; :link :any-link :checked :disabled :enabled :not() :is() :where(),
;; ::before ::after. Anything else (:hover, :nth-child, …) never matches.
;; @media is evaluated against a nominal viewport; @supports and @layer
;; contents apply; @font-face/@keyframes/@page are skipped.

;; ── property tables ─────────────────────────────────────────────────────────
(define skim--inherited-props
  (hash "color" #t "font-weight" #t "font-style" #t "font-family" #t "font-variant" #t
        "text-align" #t "text-transform" #t "white-space" #t "visibility" #t
        "list-style-type" #t "text-decoration" #t))

(define skim--own-props
  (hash "display" #t "margin-top" #t "margin-bottom" #t "margin-left" #t
        "padding-top" #t "padding-bottom" #t "padding-left" #t "background-color" #t
        "content" #t "position" #t "clip" #t "clip-path" #t "width" #t "height" #t
        "overflow" #t "flex-direction" #t "opacity" #t "left" #t "top" #t "font-size" #t
        "max-width" #t "margin-right" #t "padding-right" #t "float" #t
        "transition" #t "transition-property" #t "animation" #t "animation-name" #t))

(define skim--prop-defaults
  (hash "display" "inline" "visibility" "visible" "font-weight" "normal"
        "font-style" "normal" "font-family" "" "font-variant" "normal"
        "text-decoration" "none" "text-align" "start" "text-transform" "none"
        "white-space" "normal" "list-style-type" "disc" "margin-top" "0"
        "margin-bottom" "0" "margin-left" "0" "padding-top" "0" "padding-bottom" "0"
        "padding-left" "0" "color" "" "background-color" "" "content" ""
        "position" "static" "clip" "" "clip-path" "" "width" "" "height" ""
        "overflow" "visible" "flex-direction" "row" "opacity" "1" "left" "" "top" ""
        "font-size" "" "max-width" "" "margin-right" "0" "padding-right" "0" "float" "none"
        "transition" "" "transition-property" "" "animation" "" "animation-name" ""))

;; ── declarations ────────────────────────────────────────────────────────────
;; The four sides of a margin/padding shorthand → (top right bottom left).
(define (skim--four-sides v)
  (let ((ws (skim--words v)))
    (cond ((null? ws) (list "0" "0" "0" "0"))
          ((= (length ws) 1) (list (car ws) (car ws) (car ws) (car ws)))
          ((= (length ws) 2) (list (car ws) (cadr ws) (car ws) (cadr ws)))
          ((= (length ws) 3) (list (car ws) (cadr ws) (caddr ws) (cadr ws)))
          (else (skim--take ws 4)))))

(define skim--list-style-types
  (hash "disc" #t "circle" #t "square" #t "decimal" #t "decimal-leading-zero" #t
        "lower-alpha" #t "upper-alpha" #t "lower-latin" #t "upper-latin" #t
        "lower-roman" #t "upper-roman" #t "lower-greek" #t "none" #t))

;; One declaration → list of (prop . value) after shorthand expansion; only
;; properties skim uses (and custom properties) survive.
(define (skim--expand-decl prop val)
  (cond
    ((starts-with? prop "--") (list (cons prop val)))
    ((hash-contains? skim--inherited-props prop) (list (cons prop val)))
    ((hash-contains? skim--own-props prop) (list (cons prop val)))
    ((equal? prop "margin")
     (let ((s (skim--four-sides val)))
       (list (cons "margin-top" (car s)) (cons "margin-right" (cadr s))
             (cons "margin-bottom" (caddr s)) (cons "margin-left" (cadddr s)))))
    ((equal? prop "padding")
     (let ((s (skim--four-sides val)))
       (list (cons "padding-top" (car s)) (cons "padding-right" (cadr s))
             (cons "padding-bottom" (caddr s)) (cons "padding-left" (cadddr s)))))
    ((equal? prop "margin-block") (let ((w (skim--words val)))
                                    (if (null? w) '()
                                        (list (cons "margin-top" (car w))
                                              (cons "margin-bottom" (if (null? (cdr w)) (car w) (cadr w)))))))
    ((equal? prop "margin-block-start") (list (cons "margin-top" val)))
    ((equal? prop "margin-block-end") (list (cons "margin-bottom" val)))
    ((equal? prop "margin-inline-start") (list (cons "margin-left" val)))
    ((equal? prop "margin-inline-end") (list (cons "margin-right" val)))
    ((equal? prop "margin-inline") (let ((w (skim--words val)))
                                     (if (null? w) '()
                                         (list (cons "margin-left" (car w))
                                               (cons "margin-right" (if (null? (cdr w)) (car w) (cadr w)))))))
    ((equal? prop "padding-inline-start") (list (cons "padding-left" val)))
    ((equal? prop "text-decoration-line") (list (cons "text-decoration" val)))
    ((equal? prop "list-style")
     (let ((t (filter (lambda (w) (hash-contains? skim--list-style-types w)) (skim--words val))))
       (if (null? t) '() (list (cons "list-style-type" (car t))))))
    ((equal? prop "background")
     (let ((c (filter (lambda (w) (skim--parse-color w))
                      (skim--words (replace-regexp-in-string "url\\([^)]*\\)" "" val)))))
       (cond ((not (null? c)) (list (cons "background-color" (car c))))
             ((string-contains? val "var(") (list (cons "background-color" val)))
             ((or (equal? val "none") (equal? val "transparent")) (list (cons "background-color" "transparent")))
             (else '()))))
    ((equal? prop "font")
     (let ((ws (skim--words val)))
       (append (if (or (member "bold" ws) (member "bolder" ws) (member "700" ws) (member "600" ws) (member "800" ws) (member "900" ws))
                   (list (cons "font-weight" "bold")) (list (cons "font-weight" "normal")))
               (if (member "italic" ws) (list (cons "font-style" "italic")) (list (cons "font-style" "normal")))
               (if (string-contains? val "mono") (list (cons "font-family" "monospace")) '()))))
    (else '())))

;; "a: b; c: d !important" → (normal-decls . important-decls).
(define (skim--parse-decls text)
  (let loop ((ds (split-many text ";")) (norm '()) (imp '()))
    (if (null? ds)
        (cons (reverse norm) (reverse imp))
        (let ((c (regexp-captures "(?s)^\\s*([-A-Za-z0-9_]+)\\s*:\\s*(.*?)\\s*$" (car ds))))
          (if (null? c)
              (loop (cdr ds) norm imp)
              (let* ((prop (skim--lower (nth c 1)))
                     (raw (nth c 2))
                     (important? (not (null? (regexp-captures "!\\s*important\\s*$" raw))))
                     (raw (if important? (replace-regexp-in-string "\\s*!\\s*important\\s*$" "" raw) raw))
                     (val (if (or (starts-with? prop "--") (equal? prop "content") (equal? prop "font-family"))
                              (skim--trim raw)
                              (skim--lower (skim--trim raw))))
                     (ex (skim--expand-decl prop val)))
                (if important?
                    (loop (cdr ds) norm (append (reverse ex) imp))
                    (loop (cdr ds) (append (reverse ex) norm) imp))))))))

;; ── selectors ───────────────────────────────────────────────────────────────
;; compound: #(tag id classes attr-tests pseudo-classes) ; tag/id #f = any
;; selector: #(compounds specificity pseudo-element) where compounds is a list
;;   ((compound . combinator) …) from the RIGHTMOST compound leftwards; the
;;   combinator relates that compound to the next one ('desc 'child 'adj 'sib),
;;   'none on the last.

(define skim--sel-token-re
  "\\[[^\\]]*\\]|::?[-A-Za-z0-9_]+(?:\\((?:[^()]|\\([^()]*\\))*\\))?|[.#][-A-Za-z0-9_\\\\]+|\\*|[-A-Za-z0-9_]+|\\s*[>+~]\\s*|\\s+")

(define (skim--sel-tokens s)
  (let ((pieces (split-many (replace-regexp-in-string
                             skim--sel-token-re (string-append skim--S1 "${0}" skim--S1) s)
                            skim--S1)))
    ;; Even pieces are unmatched leftovers: anything there makes it unsupported.
    (let loop ((ps pieces) (i 0) (acc '()))
      (cond ((null? ps) (reverse acc))
            ((even? i) (if (equal? (car ps) "") (loop (cdr ps) (+ i 1) acc) #f))
            (else (loop (cdr ps) (+ i 1) (cons (car ps) acc)))))))

(define skim--never 'never)

;; Attribute test "[name op value i]" → (name op value ci?) or #f.
(define (skim--parse-attr-test tok)
  (let ((c (regexp-captures "^\\[\\s*([-A-Za-z0-9_:]+)\\s*(?:([~|^$*]?=)\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s\\]]*))\\s*(i|s)?)?\\s*\\]$" tok)))
    (if (null? c)
        #f
        (list (skim--lower (nth c 1)) (nth c 2)
              (string-append (nth c 3) (nth c 4) (nth c 5))
              (equal? (nth c 6) "i")))))

;; Parse a compound from tokens; returns (compound spec pseudo-element rest-tokens).
;; Stops at a combinator token.
(define (skim--parse-compound toks)
  (let loop ((ts toks) (tag #f) (id #f) (classes '()) (attrs '()) (pcs '()) (spec 0) (pe #f))
    (if (or (null? ts) (skim--combinator-token? (car ts)))
        (list (vector tag id classes attrs pcs) spec pe ts)
        (let ((t (car ts)))
          (cond
            ((equal? t "*") (loop (cdr ts) tag id classes attrs pcs spec pe))
            ((starts-with? t "#")
             (loop (cdr ts) tag (substring t 1 (string-length t)) classes attrs pcs (+ spec 10000) pe))
            ((starts-with? t ".")
             (loop (cdr ts) tag id (cons (replace-regexp-in-string "\\\\" "" (substring t 1 (string-length t))) classes)
                   attrs pcs (+ spec 100) pe))
            ((starts-with? t "[")
             (let ((a (skim--parse-attr-test t)))
               (if a
                   (loop (cdr ts) tag id classes (cons a attrs) pcs (+ spec 100) pe)
                   (loop (cdr ts) tag id classes attrs (cons skim--never pcs) spec pe))))
            ((starts-with? t "::")
             (let ((n (skim--lower (substring t 2 (string-length t)))))
               (loop (cdr ts) tag id classes attrs
                     (if (or (equal? n "before") (equal? n "after")) pcs (cons skim--never pcs))
                     (+ spec 1) (if (or (equal? n "before") (equal? n "after")) n pe))))
            ((starts-with? t ":")
             (let* ((p (skim--parse-pseudo-class (substring t 1 (string-length t)))))
               (if (or (equal? (car p) "before") (equal? (car p) "after"))
                   (loop (cdr ts) tag id classes attrs pcs (+ spec 1) (car p))
                   (loop (cdr ts) tag id classes attrs (cons (car p) pcs) (+ spec (cdr p)) pe))))
            (else (loop (cdr ts) (skim--lower t) id classes attrs pcs (+ spec 1) pe)))))))

(define (skim--combinator-token? t)
  (let ((c (string-ref t 0)))
    (or (char=? c #\space) (char=? c #\>) (char=? c #\+) (char=? c #\~)
        (char=? c #\tab) (char=? c #\newline) (char=? c #\return))))

;; A pseudo-class → (test . specificity). Tests: a symbol, or
;; (list 'not compounds) / (list 'is compounds).
(define (skim--parse-pseudo-class s)
  (let* ((c (regexp-captures "^([-A-Za-z0-9_]+)(?:\\((.*)\\))?$" s))
         (name (if (null? c) "" (skim--lower (nth c 1))))
         (arg (if (null? c) "" (nth c 2))))
    (cond
      ((member name '("first-child" "last-child" "only-child" "root" "empty" "link"
                      "any-link" "checked" "disabled" "enabled"))
       (cons (string->symbol name) 100))
      ((equal? name "before") (cons "before" 1))
      ((equal? name "after") (cons "after" 1))
      ((member name '("not" "is" "where" "matches"))
       (let ((alts (map (lambda (a)
                          (let ((ts (skim--sel-tokens (skim--trim a))))
                            (if (or (not ts) (null? ts)) #f
                                (let ((pc (skim--parse-compound ts)))
                                  (if (null? (cadddr pc)) (cons (car pc) (cadr pc)) #f)))))
                        (split-many arg ","))))
         (if (member #f alts)
             (cons skim--never 0)
             (cons (list (if (equal? name "not") 'not 'is) (map car alts))
                   (if (equal? name "where") 0
                       (foldl (lambda (a m) (max m (cdr a))) 0 alts))))))
      (else (cons skim--never 100)))))

;; Selector text (no commas) → #(compounds spec pseudo-element) or #f.
(define (skim--parse-selector text)
  (let ((ts (skim--sel-tokens (skim--trim text))))
    (if (or (not ts) (null? ts))
        #f
        (let loop ((ts ts) (acc '()) (spec 0) (pe #f))
          (let* ((r (skim--parse-compound ts))
                 (comp (car r)) (rest (cadddr r))
                 (spec (+ spec (cadr r)))
                 (pe (or (caddr r) pe)))
            (if (null? rest)
                ;; acc holds (compound . combinator-to-its-RIGHT); flip to
                ;; right-to-left with combinator-to-the-LEFT.
                (let ((chain (skim--rtl (cons (cons comp 'none) acc))))
                  (vector chain spec pe (skim--required-ancestors chain)))
                (let* ((ct (car rest))
                       (comb (cond ((string-contains? ct ">") 'child)
                                   ((string-contains? ct "+") 'adj)
                                   ((string-contains? ct "~") 'sib)
                                   (else 'desc))))
                  (if (null? (cdr rest))
                      #f
                      (loop (cdr rest) (cons (cons comp comb) acc) spec pe)))))))))

;; acc = ((cN . none) (cN-1 . comb) … (c0 . comb)) where each comb sits
;; between that compound and the one to its right. Rebuild as
;; ((cN . combN-1) (cN-1 . combN-2) … (c0 . none)).
(define (skim--rtl acc)
  (let loop ((xs acc) (out '()))
    (if (null? (cdr xs))
        (reverse (cons (cons (car (car xs)) 'none) out))
        (loop (cdr xs) (cons (cons (car (car xs)) (cdr (cadr xs))) out)))))

;; Tokens ("t:tag" "#id" ".class") every match's ancestors must carry: those
;; of each compound reached from the right through descendant/child
;; combinators (a compound joined by + or ~ is a sibling, not an ancestor).
(define (skim--required-ancestors chain)
  (let loop ((cs chain) (acc '()))
    (if (or (null? cs) (null? (cdr cs)))
        acc
        (let ((comb (cdr (car cs))) (next (car (car (cdr cs)))))
          (loop (cdr cs)
                (if (memq comb '(desc child)) (append (skim--compound-tokens next) acc) acc))))))

(define (skim--compound-tokens c)
  (append (if (vector-ref c 0) (list (string-append "t:" (vector-ref c 0))) '())
          (if (vector-ref c 1) (list (string-append "#" (vector-ref c 1))) '())
          (map (lambda (k) (string-append "." k)) (vector-ref c 2))))

;; Index key of a selector: its rightmost compound's id, first class, or tag.
(define (skim--sel-key sel)
  (let ((c (car (car (vector-ref sel 0)))))
    (cond ((vector-ref c 1) (string-append "#" (vector-ref c 1)))
          ((pair? (vector-ref c 2)) (string-append "." (car (vector-ref c 2))))
          ((vector-ref c 0) (vector-ref c 0))
          (else "*"))))

;; ── matching ────────────────────────────────────────────────────────────────
;; A match context for one element: #(el classes parent prev index last?)
;;   parent/prev are match contexts (or #f); index is 1-based among element
;;   siblings.
(define (skim--mctx el parent prev index last?)
  (mutable-vector el
          (let ((c (skim--attr el "class"))) (if c (skim--words c) '()))
          parent prev index last?
          (if parent (skim--mctx-tokens parent) (hash))
          #f))

;; The ancestor-token set for children of match context `m`: m's own
;; ancestors plus m's tag/id/classes.
(define (skim--mctx-tokens m)
  (or (vector-ref m 7)
      (let ((h (skim--mctx-tokens-raw m)))
        (vector-set! m 7 h)
        h)))

(define (skim--mctx-tokens-raw m)
  (let* ((el (vector-ref m 0))
         (h (hash-insert (vector-ref m 6) (string-append "t:" (skim--el-tag el)) #t))
         (id (skim--attr el "id"))
         (h (if id (hash-insert h (string-append "#" id) #t) h)))
    (foldl (lambda (c h) (hash-insert h (string-append "." c) #t)) h (vector-ref m 1))))

(define (skim--ancestors-ok? sel m)
  (let ((need (vector-ref sel 3)) (have (vector-ref m 6)))
    (let loop ((ns need))
      (or (null? ns) (and (hash-contains? have (car ns)) (loop (cdr ns)))))))

(define (skim--attr-test-ok? test el)
  (let ((v (skim--attr el (car test))))
    (and v
         (let* ((op (cadr test))
                (want (caddr test))
                (ci (cadddr test))
                (v (if ci (skim--lower v) v))
                (want (if ci (skim--lower want) want)))
           (cond ((equal? op "") #t)
                 ((equal? op "=") (equal? v want))
                 ((equal? op "~=") (if (member want (skim--words v)) #t #f))
                 ((equal? op "|=") (or (equal? v want) (starts-with? v (string-append want "-"))))
                 ((equal? op "^=") (and (not (equal? want "")) (starts-with? v want)))
                 ((equal? op "$=") (and (not (equal? want "")) (ends-with? v want)))
                 ((equal? op "*=") (and (not (equal? want "")) (string-contains? v want)))
                 (else #f))))))

(define (skim--pseudo-ok? p m)
  (let ((el (vector-ref m 0)))
    (cond
      ((eq? p 'first-child) (= (vector-ref m 4) 1))
      ((eq? p 'last-child) (vector-ref m 5))
      ((eq? p 'only-child) (and (= (vector-ref m 4) 1) (vector-ref m 5)))
      ((eq? p 'root) (equal? (skim--el-tag el) "html"))
      ((eq? p 'empty) (null? (skim--el-kids el)))
      ((or (eq? p 'link) (eq? p 'any-link))
       (and (member (skim--el-tag el) '("a" "area")) (skim--has-attr? el "href")))
      ((eq? p 'checked) (or (skim--has-attr? el "checked") (skim--has-attr? el "selected")))
      ((eq? p 'disabled) (skim--has-attr? el "disabled"))
      ((eq? p 'enabled) (not (skim--has-attr? el "disabled")))
      ((pair? p)
       (let ((any (let loop ((cs (cadr p)))
                    (cond ((null? cs) #f)
                          ((skim--compound-ok? (car cs) m) #t)
                          (else (loop (cdr cs)))))))
         (if (eq? (car p) 'not) (not any) any)))
      (else #f))))

(define (skim--compound-ok? c m)
  (let ((el (vector-ref m 0)))
    (and (or (not (vector-ref c 0)) (equal? (vector-ref c 0) (skim--el-tag el)))
         (or (not (vector-ref c 1)) (equal? (vector-ref c 1) (skim--attr el "id")))
         (let loop ((cls (vector-ref c 2)))
           (or (null? cls) (and (member (car cls) (vector-ref m 1)) (loop (cdr cls)))))
         (let loop ((as (vector-ref c 3)))
           (or (null? as) (and (skim--attr-test-ok? (car as) el) (loop (cdr as)))))
         (let loop ((ps (vector-ref c 4)))
           (or (null? ps) (and (skim--pseudo-ok? (car ps) m) (loop (cdr ps))))))))

;; Does compound chain `cs` (right-to-left) match context `m`?
(define (skim--chain-ok? cs m)
  (and m
       (skim--compound-ok? (car (car cs)) m)
       (let ((comb (cdr (car cs))) (rest (cdr cs)))
         (cond
           ((eq? comb 'none) #t)
           ((eq? comb 'child) (skim--chain-ok? rest (vector-ref m 2)))
           ((eq? comb 'adj) (skim--chain-ok? rest (vector-ref m 3)))
           ((eq? comb 'desc)
            (let loop ((a (vector-ref m 2)))
              (cond ((not a) #f)
                    ((skim--chain-ok? rest a) #t)
                    (else (loop (vector-ref a 2))))))
           ((eq? comb 'sib)
            (let loop ((s (vector-ref m 3)))
              (cond ((not s) #f)
                    ((skim--chain-ok? rest s) #t)
                    (else (loop (vector-ref s 3))))))
           (else #f)))))

;; ── stylesheets ─────────────────────────────────────────────────────────────
;; A sheet is a hash: key → list of rules, plus 'count. A rule is
;; #(selector order origin normal-decls important-decls).
(define (skim--empty-sheet) (hash 'count 0 'pseudo #f))

;; All reads happen before the inserts so `sheet` has no other live reference
;; when it's updated — Steel then updates in place instead of copying.
(define (skim--sheet-add sheet sel decls origin)
  (let* ((n (hash-try-get sheet 'count))
         (rule (vector sel n origin (car decls) (cadr decls) (caddr decls)))
         (key (skim--sel-key sel))
         (bucket (cons rule (or (hash-try-get sheet key) '())))
         (pseudo (or (hash-try-get sheet 'pseudo) (if (vector-ref sel 2) #t #f))))
    (hash-insert (hash-insert (hash-insert sheet key bucket) 'count (+ n 1)) 'pseudo pseudo)))

;; Length → px (number) or #f. `em`/`rem` assume 16px.
(define (skim--px v)
  (let ((c (regexp-captures "^(-?[0-9]*\\.?[0-9]+)(px|em|rem|pt|ch|ex|vw|vh|%)?$" (skim--trim v))))
    (if (null? c)
        #f
        (let ((n (string->number (nth c 1))) (u (nth c 2)))
          (and n
               (cond ((equal? u "") (if (= n 0) 0 #f))
                     ((equal? u "px") n)
                     ((or (equal? u "em") (equal? u "rem")) (* n 16))
                     ((equal? u "pt") (* n 1.333))
                     ((or (equal? u "ch") (equal? u "ex")) (* n 8))
                     ((equal? u "vw") (* n (/ (skim--viewport-px) 100)))
                     ((equal? u "vh") (* n 8))
                     (else #f)))))))

(define (skim--viewport-px)
  (let ((v (get-option "skim-viewport-width")))
    (cond ((number? v) v) ((and (string? v) (string->number v)) (string->number v)) (else 1024))))

;; One media-query feature "(min-width: 600px)" → #t/#f.
(define (skim--media-feature-ok? f dark?)
  (let* ((f (skim--trim f))
         (c (regexp-captures "^\\(?\\s*([-a-z]+)\\s*(?::\\s*([^)]*))?\\)?$" f))
         (vw (skim--viewport-px)))
    (if (null? c)
        ;; Range syntax: (width >= 600px) / (600px <= width)
        (let ((r (regexp-captures "^\\(\\s*width\\s*([<>]=?)\\s*([^)]*)\\)$" f)))
          (if (null? r)
              #f
              (let ((px (skim--px (nth r 2))) (op (nth r 1)))
                (and px (cond ((equal? op ">=") (>= vw px)) ((equal? op ">") (> vw px))
                              ((equal? op "<=") (<= vw px)) (else (< vw px)))))))
        (let ((name (nth c 1)) (val (skim--trim (nth c 2))))
          (cond
            ((equal? name "min-width") (let ((px (skim--px val))) (and px (>= vw px))))
            ((equal? name "max-width") (let ((px (skim--px val))) (and px (<= vw px))))
            ((equal? name "prefers-color-scheme") (equal? val (if dark? "dark" "light")))
            ((equal? name "prefers-reduced-motion") #t)
            ((equal? name "orientation") (equal? val "landscape"))
            ((equal? name "color") #t)
            ((equal? name "hover") (equal? val "none"))
            ((equal? name "pointer") (equal? val "fine"))
            (else #f))))))

;; "@media screen and (max-width: 600px), print" → #t/#f.
(define (skim--media-ok? prelude dark?)
  (let ((q (skim--trim (replace-regexp-in-string "(?i)^@media" "" prelude))))
    (if (equal? q "")
        #t
        (let loop ((alts (split-many (skim--lower q) ",")))
          (cond
            ((null? alts) #f)
            ((skim--media-alt-ok? (skim--trim (car alts)) dark?) #t)
            (else (loop (cdr alts))))))))

(define (skim--media-alt-ok? alt dark?)
  (let* ((neg? (starts-with? alt "not "))
         (alt (replace-regexp-in-string "^(not|only)\\s+" "" alt))
         (parts (map skim--trim (split-many alt " and ")))
         (ok (let loop ((ps parts))
               (cond ((null? ps) #t)
                     ((equal? (car ps) "") (loop (cdr ps)))
                     ((member (car ps) '("screen" "all")) (loop (cdr ps)))
                     ((member (car ps) '("print" "speech" "tv" "handheld" "projection")) #f)
                     ((skim--media-feature-ok? (car ps) dark?) (loop (cdr ps)))
                     (else #f)))))
    (if neg? (not ok) ok)))

;; Parse stylesheet `text` into `sheet` (a hash) with `origin` (0 UA, 1 author).
;; Braces are fenced and the text cut once; a stack tracks nesting:
;;   'rule (collecting declarations) 'keep (@media match/@supports/@layer)
;;   'skip (unmatched @media, @font-face, @keyframes, nested rules, …).
(define (skim--parse-css sheet text origin dark?)
  (let* ((text (replace-regexp-in-string "(?s)/\\*.*?\\*/" "" text))
         (toks (split-many (replace-regexp-in-string "([{}])" (string-append skim--S1 "${1}" skim--S1) text)
                           skim--S1)))
    ;; `prelude` holds the selector text of the rule being collected; the
    ;; sheet is threaded uniquely (a second reference would make Steel copy
    ;; the whole hash on every insert).
    (let loop ((ts toks) (stack '()) (pending "") (sheet sheet) (prelude ""))
      (if (null? ts)
          sheet
          (let ((t (car ts)))
            (cond
              ((equal? t "{")
               (let* ((pre (skim--trim (skim--after-last-semicolon pending)))
                      (skipping? (and (pair? stack) (memq (car stack) '(skip rule))))
                      (kind (cond (skipping? 'skip)
                                  ((starts-with? pre "@media")
                                   (if (skim--media-ok? pre dark?) 'keep 'skip))
                                  ((or (starts-with? pre "@supports") (starts-with? pre "@layer")
                                       (starts-with? pre "@container")
                                       (starts-with? pre "@document"))
                                   'keep)
                                  ((starts-with? pre "@") 'skip)
                                  (else 'rule))))
                 (loop (cdr ts) (cons kind stack) "" sheet (if (eq? kind 'rule) pre prelude))))
              ((equal? t "}")
               (cond
                 ((null? stack) (loop (cdr ts) stack "" sheet prelude))
                 ((eq? (car stack) 'rule)
                  (loop (cdr ts) (cdr stack) "" (skim--add-rules sheet prelude pending origin) ""))
                 (else (loop (cdr ts) (cdr stack) "" sheet prelude))))
              ((and (pair? stack) (eq? (car stack) 'skip)) (loop (cdr ts) stack "" sheet prelude))
              (else (loop (cdr ts) stack (string-append pending t) sheet prelude))))))))

(define (skim--after-last-semicolon s)
  (if (string-contains? s ";")
      (last (split-many s ";"))
      s))

;; Add one rule block (comma-separated selectors) to the sheet.
(define (skim--add-rules sheet prelude decl-text origin)
  (let* ((parsed (skim--parse-decls decl-text))
         ;; custom properties become one prebuilt map per rule (pushed as a
         ;; layer by every matching element — see skim--cascade)
         (vars (foldl (lambda (d h) (if (starts-with? (car d) "--") (hash-insert h (car d) (cdr d)) h))
                      (hash) (car parsed)))
         (decls (list (filter (lambda (d) (not (starts-with? (car d) "--"))) (car parsed))
                      (cdr parsed)
                      vars)))
    (if (and (null? (car parsed)) (null? (cdr parsed)))
        sheet
        (let loop ((sels (skim--split-selectors prelude)) (sheet sheet))
          (if (null? sels)
              sheet
              (let ((sel (skim--parse-selector (car sels))))
                (loop (cdr sels) (if sel (skim--sheet-add sheet sel decls origin) sheet))))))))

;; Split "a, b:is(c, d)" at top-level commas only.
(define (skim--split-selectors s)
  (if (not (string-contains? s "("))
      (split-many s ",")
      (let loop ((cs (string->list s)) (depth 0) (cur '()) (acc '()))
        (cond ((null? cs) (reverse (cons (list->string (reverse cur)) acc)))
              ((char=? (car cs) #\() (loop (cdr cs) (+ depth 1) (cons (car cs) cur) acc))
              ((char=? (car cs) #\)) (loop (cdr cs) (max 0 (- depth 1)) (cons (car cs) cur) acc))
              ((and (char=? (car cs) #\,) (= depth 0))
               (loop (cdr cs) depth '() (cons (list->string (reverse cur)) acc)))
              (else (loop (cdr cs) depth (cons (car cs) cur) acc))))))

;; ── the user-agent stylesheet ───────────────────────────────────────────────
(define skim--ua-css "
html, body, address, blockquote, center, div, figure, figcaption, footer, form,
header, hr, legend, listing, main, p, plaintext, pre, xmp, article, aside, h1, h2,
h3, h4, h5, h6, hgroup, nav, section, dir, dd, dl, dt, menu, ol, ul, details,
summary, fieldset, dialog, search, optgroup { display: block }
details:not([open]) > :not(summary) { display: none }
head, link, meta, script, style, title, base, template, noembed, datalist, param,
source, track, area, map, dialog:not([open]), [hidden], input[type=hidden],
option, rp, embed, object, audio:not([controls]) { display: none }
li { display: list-item }
table { display: table; text-align: start; white-space: normal; font-weight: normal; font-style: normal }
caption { display: table-caption; text-align: center }
tr { display: table-row } td, th { display: table-cell }
thead { display: table-header-group } tbody { display: table-row-group }
tfoot { display: table-footer-group } col, colgroup { display: none }
p, blockquote, figure, dl, pre, xmp, listing, plaintext, fieldset { margin-top: 1em; margin-bottom: 1em }
h1 { margin-top: 0.67em; margin-bottom: 0.67em }
h2, h3, h4, h5, h6 { margin-top: 1em; margin-bottom: 1em }
ul, ol, menu, dir { margin-top: 1em; margin-bottom: 1em; padding-left: 40px }
ul ul, ul ol, ol ul, ol ol, menu ul, ul menu { margin-top: 0; margin-bottom: 0 }
ol { list-style-type: decimal }
ul ul, ol ul, menu ul { list-style-type: circle }
ul ul ul, ol ul ul, ul ol ul, ol ol ul { list-style-type: square }
blockquote, figure { margin-left: 40px }
dd { margin-left: 40px }
hr { margin-top: 0.5em; margin-bottom: 0.5em }
h1, h2, h3, h4, h5, h6, b, strong, th, dt, summary, legend, optgroup { font-weight: bold }
th { text-align: center }
i, em, cite, var, dfn, address { font-style: italic }
u, ins { text-decoration: underline }
s, strike, del { text-decoration: line-through }
a:link, a:any-link { text-decoration: underline }
pre, xmp, listing, plaintext { white-space: pre; font-family: monospace }
code, kbd, samp, tt { font-family: monospace }
textarea { white-space: pre-wrap }
nobr { white-space: nowrap }
center { text-align: center }
q::before { content: open-quote } q::after { content: close-quote }
mark { background-color: #fff3a0; color: #000000 }
small, sub, sup { font-size: smaller }
")

;; Parsed once per palette darkness (it has no @media, but keep the shape).
(define skim--ua-sheet-cache #f)
(define (skim--ua-sheet)
  (or skim--ua-sheet-cache
      (begin (set! skim--ua-sheet-cache (skim--parse-css (skim--empty-sheet) skim--ua-css 0 #t))
             skim--ua-sheet-cache)))

;; ── the cascade ─────────────────────────────────────────────────────────────
;; Computed style = #(inherited own vars):
;;   inherited — inherited properties (color, font-weight, …), shared with the
;;               parent until an element changes one;
;;   own       — non-inherited properties, this element only;
;;   vars      — custom properties (--x), inherited, as a chain of layers
;;               (see skim--var-lookup): pages define thousands of them, and
;;               copying them per element was the dominant cost.
;; Each element's declarations go into small fresh maps first and are merged
;; into the parent's maps once, so a shared map is copied at most once.
(define skim--root-style (vector (hash) (hash) '()))

;; Custom properties are a chain of layers (newest first): an element that
;; declares variables pushes one small map instead of copying the inherited
;; set, which on design-system pages runs to thousands of entries.
(define (skim--var-lookup layers name)
  (if (null? layers)
      #f
      (or (hash-try-get (car layers) name) (skim--var-lookup (cdr layers) name))))

(define (skim--style cs prop)
  (let ((v (if (hash-contains? skim--own-props prop)
               (hash-try-get (vector-ref cs 1) prop)
               (hash-try-get (vector-ref cs 0) prop))))
    (or v (hash-try-get skim--prop-defaults prop) "")))

(define (skim--candidates sheet m)
  (let* ((el (vector-ref m 0))
         (id (skim--attr el "id"))
         (lists (append
                 (list (hash-try-get sheet (skim--el-tag el)) (hash-try-get sheet "*"))
                 (if id (list (hash-try-get sheet (string-append "#" id))) '())
                 (map (lambda (c) (hash-try-get sheet (string-append "." c))) (vector-ref m 1)))))
    (let loop ((ls lists) (acc '()))
      (cond ((null? ls) acc)
            ((not (car ls)) (loop (cdr ls) acc))
            (else (loop (cdr ls) (append (car ls) acc)))))))

;; Rules whose selector matches `m`, straight off the index buckets.
(define (skim--matching-rules sheet m)
  (let* ((el (vector-ref m 0))
         (id (skim--attr el "id"))
         (have (vector-ref m 6))
         (keys (append (list (skim--el-tag el) "*")
                       (if id (list (string-append "#" id)) '())
                       (map (lambda (c) (string-append "." c)) (vector-ref m 1)))))
    (let kloop ((ks keys) (acc '()))
      (if (null? ks)
          acc
          (let rloop ((rs (or (hash-try-get sheet (car ks)) '())) (acc acc))
            (if (null? rs)
                (kloop (cdr ks) acc)
                (let* ((r (car rs)) (sel (vector-ref r 0)))
                  (rloop (cdr rs)
                         (if (and (let need ((ns (vector-ref sel 3)))
                                    (or (null? ns) (and (hash-contains? have (car ns)) (need (cdr ns)))))
                                  (skim--chain-ok? (vector-ref sel 0) m))
                             (cons r acc)
                             acc)))))))))

;; Sort key: origin, then specificity, then source order.
(define (skim--rule-key r)
  (+ (* (vector-ref r 2) 1000000000000)
     (* (vector-ref (vector-ref r 0) 1) 1000000)
     (vector-ref r 1)))

;; Presentational attributes → author-level declarations (lowest priority).
(define (skim--presentational el)
  (let* ((tag (skim--el-tag el))
         (acc '())
         (acc (let ((a (skim--attr el "align")))
                (if (and a (not (member tag '("img" "table" "iframe"))))
                    (cons (cons "text-align" (skim--lower a)) acc) acc)))
         (acc (let ((c (skim--attr el "color")))
                (if (and c (equal? tag "font")) (cons (cons "color" (skim--lower c)) acc) acc)))
         (acc (let ((c (skim--attr el "bgcolor")))
                (if c (cons (cons "background-color" (skim--lower c)) acc) acc)))
         (acc (if (and (skim--has-attr? el "nowrap") (member tag '("td" "th")))
                  (cons (cons "white-space" "nowrap") acc) acc))
         (acc (let ((t (skim--attr el "type")))
                (if (and t (member tag '("ol" "ul" "li")))
                    (cons (cons "list-style-type"
                                (cond ((equal? t "1") "decimal") ((equal? t "a") "lower-alpha")
                                      ((equal? t "A") "upper-alpha") ((equal? t "i") "lower-roman")
                                      ((equal? t "I") "upper-roman") (else (skim--lower t))))
                          acc)
                    acc))))
    (reverse acc)))

;; Resolve var(--x, fallback) references; `lookup` maps a name to its value.
(define (skim--resolve-vars val lookup depth)
  (if (or (> depth 8) (not (string-contains? val "var(")))
      val
      (let ((c (regexp-captures "var\\(\\s*(--[-A-Za-z0-9_]+)\\s*(?:,\\s*([^()]*(?:\\([^()]*\\))?[^()]*))?\\)" val)))
        (if (null? c)
            val
            (let* ((name (nth c 1))
                   (fallback (skim--trim (nth c 2)))
                   (v (or (lookup name) fallback))
                   (whole (nth c 0)))
              (skim--resolve-vars (string-replace val whole v) lookup (+ depth 1)))))))

(define (skim--merge base delta)
  (if (= (hash-length delta) 0)
      base
      (foldl (lambda (k h) (hash-insert h k (hash-try-get delta k))) base (hash-keys->list delta))))

;; Apply `decls` ((prop . value) …, lowest priority first) over `parent`.
(define (skim--cascade decls parent . layers)
  (let* ((pinh (vector-ref parent 0)) (pown (vector-ref parent 1))
         ;; rules' prebuilt variable maps, lowest priority first → pushed so
         ;; the highest-priority map is looked up first
         (pvars (foldl (lambda (l acc) (if (= (hash-length l) 0) acc (cons l acc)))
                       (vector-ref parent 2) (if (null? layers) '() (car layers)))))
    (let loop ((ds decls) (d (hash)) (own (hash)) (vd (hash)))
      (if (null? ds)
          (vector (skim--merge pinh d) own (if (= (hash-length vd) 0) pvars (cons vd pvars)))
          (let ((prop (car (car ds))) (val (cdr (car ds))))
            (if (starts-with? prop "--")
                (loop (cdr ds) d own (hash-insert vd prop val))
                (let* ((val (if (string-contains? val "var(")
                                (skim--lower (skim--resolve-vars
                                              val (lambda (n) (or (hash-try-get vd n) (skim--var-lookup pvars n))) 0))
                                val))
                       (own? (hash-contains? skim--own-props prop))
                       (val (cond ((equal? val "inherit")
                                   (or (hash-try-get (if own? pown pinh) prop) ""))
                                  ((or (equal? val "initial") (equal? val "unset") (equal? val "revert"))
                                   (hash-try-get skim--prop-defaults prop))
                                  (else val))))
                  (if own?
                      (loop (cdr ds) d (hash-insert own prop val) vd)
                      (loop (cdr ds) (hash-insert d prop val) own vd)))))))))

;; Compute an element's style. `sheet` holds UA + author rules; returns
;; (cs before-content after-content) where the contents are strings or #f.
(define (skim--compute el m parent-cs sheet)
  (let* ((cands (skim--matching-rules sheet m))
         (sorted (sort-by skim--rule-key < cands))
         (main (filter (lambda (r) (not (vector-ref (vector-ref r 0) 2))) sorted))
         (ua (filter (lambda (r) (= (vector-ref r 2) 0)) main))
         (author (filter (lambda (r) (= (vector-ref r 2) 1)) main))
         (inline (let ((s (skim--attr el "style"))) (if s (skim--parse-decls s) (cons '() '()))))
         (cs (skim--cascade
              (append (apply append (map (lambda (r) (vector-ref r 3)) ua))
                      (skim--presentational el)
                      (apply append (map (lambda (r) (vector-ref r 3)) author))
                      (car inline)
                      (apply append (map (lambda (r) (vector-ref r 4)) main))
                      (cdr inline))
              parent-cs
              (map (lambda (r) (vector-ref r 5)) main)))
         (pseudos (filter (lambda (r) (vector-ref (vector-ref r 0) 2)) sorted)))
    (list cs
          (skim--pseudo-content pseudos "before" cs)
          (skim--pseudo-content pseudos "after" cs))))

;; The `content` of ::before/::after, decoded to display text, or #f.
(define (skim--pseudo-content rules which cs)
  (let ((rs (filter (lambda (r) (equal? (vector-ref (vector-ref r 0) 2) which)) rules)))
    (if (null? rs)
        #f
        (let* ((pcs (skim--cascade (apply append (map (lambda (r) (append (vector-ref r 3) (vector-ref r 4))) rs))
                                   cs))
               (content (skim--style pcs "content"))
               (display (skim--style pcs "display")))
          (if (equal? display "none") #f (skim--content->text content))))))

;; CSS `content` value → text. Strings and quotes only; counters/attr()/
;; images/icon-font glyphs (private use area) yield nothing.
(define (skim--content->text v)
  (cond
    ((member v '("" "none" "normal")) #f)
    ((equal? v "open-quote") "“")
    ((equal? v "close-quote") "”")
    (else
     (let ((strs (regexp-captures "^\\s*\"((?:[^\"\\\\]|\\\\.)*)\"\\s*$|^\\s*'((?:[^'\\\\]|\\\\.)*)'\\s*$" v)))
       (if (null? strs)
           #f
           (let* ((raw (string-append (nth strs 1) (nth strs 2)))
                  (txt (skim--css-unescape raw))
                  (txt (list->string (filter (lambda (c) (let ((i (char->integer c)))
                                                           (not (or (and (>= i 57344) (<= i 63743)) (>= i 983040)))))
                                             (string->list txt)))))
             (if (equal? (skim--trim txt) "") #f txt)))))))

;; "\201C" / "\a" CSS escapes → characters.
(define (skim--css-unescape s)
  (if (not (string-contains? s "\\"))
      s
      (let* ((fenced (replace-regexp-in-string "\\\\([0-9a-fA-F]{1,6}) ?"
                                               (string-append skim--S1 "${1}" skim--S1) s))
             (pieces (split-many fenced skim--S1)))
        (let loop ((ps pieces) (i 0) (acc '()))
          (if (null? ps)
              (replace-regexp-in-string "\\\\(.)" "${1}" (string-join (reverse acc) ""))
              (loop (cdr ps) (+ i 1)
                    (cons (if (even? i) (car ps)
                              (let ((cp (string->number (car ps) 16)))
                                (if (= cp 10) " " (skim--codepoint->string cp))))
                          acc)))))))

;; Content in normal flow that starts invisible but transitions/animates its
;; opacity or visibility is waiting for a script to reveal it (fade-in on
;; scroll, slide decks). skim runs no script, so it shows such content.
;; Positioned boxes (dropdowns, popovers) stay hidden.
(define (skim--script-revealed? cs)
  (and (member (skim--style cs "position") '("static" "relative" "sticky" ""))
       (let ((tr (string-append (skim--style cs "transition") " " (skim--style cs "transition-property")))
             (an (string-append (skim--style cs "animation") " " (skim--style cs "animation-name"))))
         (or (string-contains? tr "opacity") (string-contains? tr "visibility") (string-contains? tr "all")
             (and (not (equal? (skim--trim an) "")) (not (equal? (skim--trim an) "none")))))))

;; ── style predicates used by layout ─────────────────────────────────────────
(define (skim--bold? cs)
  (let ((w (skim--style cs "font-weight")))
    (or (equal? w "bold") (equal? w "bolder")
        (let ((n (string->number w))) (and n (>= n 600))))))

(define (skim--italic? cs)
  (let ((s (skim--style cs "font-style"))) (or (equal? s "italic") (equal? s "oblique"))))

(define (skim--mono? cs)
  (let ((f (skim--lower (skim--style cs "font-family"))))
    (or (string-contains? f "mono") (string-contains? f "courier") (string-contains? f "consolas")
        (string-contains? f "menlo") (string-contains? f "code"))))

(define (skim--underline? cs) (string-contains? (skim--style cs "text-decoration") "underline"))
(define (skim--strike? cs) (string-contains? (skim--style cs "text-decoration") "line-through"))

;; Is the element hidden for a text renderer? (display:none, visibility,
;; the "visually hidden" clip pattern, off-screen positioning, opacity 0.)
(define (skim--hidden? cs)
  (or (equal? (skim--style cs "display") "none")
      (and (or (member (skim--style cs "visibility") '("hidden" "collapse"))
               (member (skim--style cs "opacity") '("0" "0.0" "0%")))
           (not (skim--script-revealed? cs)))
      (let ((pos (skim--style cs "position")))
        (and (member pos '("absolute" "fixed"))
             (or (string-contains? (skim--style cs "clip") "rect(0")
                 (string-contains? (skim--style cs "clip") "rect(1px")
                 (string-contains? (skim--style cs "clip-path") "inset(50%")
                 (member (skim--style cs "width") '("1px" "0" "0px"))
                 (member (skim--style cs "height") '("1px" "0" "0px"))
                 (let ((l (skim--px (skim--style cs "left"))) (t (skim--px (skim--style cs "top"))))
                   (or (and l (or (< l -500) (> l (skim--viewport-px))))
                       (and t (< t -500))
                       ;; left: 100% (or more) parks it just off the right edge
                       (let ((c (regexp-captures "^([0-9.]+)%$" (skim--style cs "left"))))
                         (and (not (null? c)) (>= (string->number (nth c 1)) 100))))))))))
