;; skim-selectors.scm — CSS selectors: parsing and matching.
;;
;; Supported: type, *, .class, #id, [attr], [attr=|~=|^=|$=|*=||=v (i)],
;; descendant, >, +, ~, :first-child :last-child :only-child :root :empty
;; :link :any-link :checked :disabled :enabled :not() :is() :where(),
;; ::before ::after. Anything else (:hover, :nth-child, …) never matches, and
;; such rules are dropped when a stylesheet is parsed.

;; ── parsing ────────────────────────────────────────────────────────────
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
       (let* ((alts (map (lambda (a)
                          (let ((ts (skim--sel-tokens (skim--trim a))))
                            (if (or (not ts) (null? ts)) #f
                                (let ((pc (skim--parse-compound ts)))
                                  (if (null? (cadddr pc)) (cons (car pc) (cadr pc)) #f)))))
                        (skim--split-selectors arg)))
             ;; Complex alternatives (with combinators) are dropped: in
             ;; :is()/:where() they just can't match, in :not() we assume the
             ;; excluded case doesn't apply. Rejecting the whole rule instead
             ;; loses e.g. every Tailwind Typography (.prose) style.
             (simple (filter (lambda (a) a) alts)))
         (cond
           ((and (null? simple) (not (equal? name "not"))) (cons skim--never 0))
           (else
            (cons (list (if (equal? name "not") 'not 'is) (map car simple))
                  (if (equal? name "where") 0
                      (foldl (lambda (a m) (max m (cdr a))) 0 simple)))))))
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

;; A selector with a part skim can never match (:hover, :nth-child, …) is
;; dropped at parse time rather than tested against every element.
(define (skim--sel-never? sel)
  (let loop ((cs (vector-ref sel 0)))
    (and (pair? cs)
         (or (memq skim--never (vector-ref (car (car cs)) 4))
             (loop (cdr cs))))))

;; ── indexing ───────────────────────────────────────────────────────────
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
;; Index keys of a selector: its rightmost compound's id, first class or tag.
;; A compound with none of those but an :is()/:where() whose alternatives all
;; have one (`:where(p)`, `:is(h1, h2)`) is indexed under each of those, so
;; it isn't tested against every element.
(define (skim--compound-key c)
  (cond ((vector-ref c 1) (string-append "#" (vector-ref c 1)))
        ((pair? (vector-ref c 2)) (string-append "." (car (vector-ref c 2))))
        ((vector-ref c 0) (vector-ref c 0))
        (else #f)))

(define (skim--sel-keys sel)
  (let* ((c (car (car (vector-ref sel 0))))
         (k (skim--compound-key c)))
    (if k
        (list k)
        (let ((is (filter (lambda (p) (and (pair? p) (eq? (car p) 'is))) (vector-ref c 4))))
          (if (null? is)
              (list "*")
              (let ((keys (map skim--compound-key (cadr (car is)))))
                (if (or (null? keys) (member #f keys)) (list "*") (skim--dedupe keys))))))))

;; ── matching ───────────────────────────────────────────────────────────
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
