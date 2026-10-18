;; skim-text.scm — lines, paragraphs and word wrapping.
;;
;; Output lines are #(text spans meta): `spans` are (start end kind value) in
;; CHAR columns with kind ∈ fg bg link field; `meta` an alist (scale, img,
;; anchors). Items are lines, gap requests (blank-line counts that collapse
;; like CSS margins) and anchor markers. A paragraph is a list of inline
;; items that `skim--flush!` word-wraps into lines.

;; ── render state ───────────────────────────────────────────────────────
;; One render's accumulators: links, form fields, forms, image count.
(define skim--R (hash))

(define (skim--R-reset! base field-values)
  (set! skim--R (hash 'links (mutable-vector) 'fields (mutable-vector) 'forms (mutable-vector)
                      'images 0 'base base 'values field-values))
  (set! skim--gen (+ skim--gen 1)))

(define (skim--Rget key) (hash-try-get skim--R key))

(define (skim--add-link url)
  (let ((v (skim--Rget 'links)))
    (vector-push! v url)
    (- (vector-length v) 1)))

(define (skim--add-field f)
  (let ((v (skim--Rget 'fields)))
    (vector-push! v f)
    (- (vector-length v) 1)))

;; Within one render an element always gets the same parent style and match
;; context, so its computed style can be reused (tables lay cells out more
;; than once); likewise a cell rendered at the same width. The caches live
;; in the element's own mutable slots, stamped with the render generation —
;; growing a global immutable hash costs O(n) per insert in Steel.
(define skim--gen 0)

(define (skim--compute-cached el m parent-cs sheet)
  (let ((c (vector-ref el 5)))
    (if (and c (= (car c) skim--gen))
        (cdr c)
        (let ((r (skim--reveal el (skim--compute el m parent-cs sheet))))
          (vector-set! el 5 (cons skim--gen r))
          r))))

;; Content hidden only until a script shows it is made visible here (so its
;; descendants inherit `visibility: visible` too), since skim runs no
;; script. Two shapes of it:
;;   * fade-in content: an opacity/visibility transition (skim--script-
;;     revealed?) on real content — tiny hover affordances (a "#" permalink,
;;     an icon button) fade in too, but on :hover, so ≥ 20 chars of text;
;;   * a curtain: one hidden box holding at least half the page's text,
;;     uncovered once fonts/scripts load. Menus and dialogs are a small
;;     share of a page, so they stay hidden.
(define (skim--reveal el r)
  (let ((cs (car r)))
    (if (not (or (member (skim--style cs "visibility") '("hidden" "collapse"))
                 (member (skim--style cs "opacity") '("0" "0.0" "0%"))))
        r
        (let ((len (string-length (skim--squeeze (skim--text-content el)))))
          (if (or (and (skim--script-revealed? cs) (>= len 20))
                  (and (>= len 200) (>= (* 2 len) (or (skim--Rget 'doc-text) 0))))
              (cons (vector (hash-insert (vector-ref cs 0) "visibility" "visible")
                            (hash-insert (vector-ref cs 1) "opacity" "1")
                            (vector-ref cs 2))
                    (cdr r))
              r)))))

;; Computed style of element `el` (match context `m`) inside flow `ctx`.
(define (skim--style-of el m ctx)
  (car (skim--compute-cached el m (hash-try-get ctx 'cs) (hash-try-get ctx 'sheet))))

;; ── lines and items ────────────────────────────────────────────────────
(define (skim--line text spans meta) (vector text spans meta))

(define (skim--line? x) (and (vector? x) (string? (vector-ref x 0))))

(define (skim--line-text l) (vector-ref l 0))

(define (skim--line-spans l) (vector-ref l 1))

(define (skim--line-meta l) (vector-ref l 2))

(define skim--blank-line (vector "" '() '()))

(define (skim--meta-get l key) (skim--aget (skim--line-meta l) key))

(define (skim--meta-put l key val)
  (vector (skim--line-text l) (skim--line-spans l)
          (cons (cons key val) (filter (lambda (kv) (not (equal? (car kv) key))) (skim--line-meta l)))))

;; Items are lines, gap requests (non-negative integers) and anchor markers
;; #('anchor id).
(define (skim--anchor-item? x) (and (vector? x) (eq? (vector-ref x 0) 'anchor)))

(define (skim--shift-spans spans dx)
  (if (= dx 0)
      spans
      (map (lambda (s) (list (+ (car s) dx) (+ (cadr s) dx) (caddr s) (cadddr s))) spans)))

;; Attach pending anchor ids to a line.
(define (skim--attach-anchors l ids)
  (if (null? ids) l (skim--meta-put l 'anchors (append ids (or (skim--meta-get l 'anchors) '())))))

;; Resolve gaps (max of a run, capped at 2) and anchors into a plain line list.
;; `keep-edges?` #f drops leading/trailing gaps.
(define (skim--resolve-items items)
  (let loop ((xs items) (gap 0) (anchors '()) (started #f) (out '()))
    (cond
      ((null? xs)
       (let ((out (if (and (pair? anchors) (pair? out))
                      (cons (skim--attach-anchors (car out) anchors) (cdr out))
                      (if (pair? anchors) (list (skim--attach-anchors skim--blank-line anchors)) out))))
         (reverse out)))
      ((number? (car xs)) (loop (cdr xs) (max gap (car xs)) anchors started out))
      ((skim--anchor-item? (car xs)) (loop (cdr xs) gap (cons (vector-ref (car xs) 1) anchors) started out))
      (else
       (let* ((out (if started
                       (let add ((n (min gap 2)) (out out)) (if (<= n 0) out (add (- n 1) (cons skim--blank-line out))))
                       out)))
         (loop (cdr xs) 0 '() #t (cons (skim--attach-anchors (car xs) anchors) out)))))))

;; Leading / trailing gap of an item list (for margin collapsing through a
;; prefixed block).
(define (skim--edge-gap items)
  (let loop ((xs items) (g 0))
    (cond ((null? xs) g)
          ((number? (car xs)) (loop (cdr xs) (max g (car xs))))
          ((skim--anchor-item? (car xs)) (loop (cdr xs) g))
          (else g))))

;; Prefix every line of `items`: the first line gets `first`, the rest `rest`.
;; Interior gaps become prefixed blank lines; edge gaps pass through so
;; margins still collapse with the outside.
(define (skim--prefix items first rest face)
  (let* ((lead (skim--edge-gap items))
         (trail (skim--edge-gap (reverse items)))
         (lines (skim--resolve-items items))
         (rest-blank (skim--rtrim rest)))
    (if (null? lines)
        (if (equal? (skim--trim first) "") '() (list (skim--prefix-line skim--blank-line first face)))
        (append
         (list lead)
         (let loop ((ls lines) (i 0) (acc '()))
           (if (null? ls)
               (reverse acc)
               (let* ((l (car ls))
                      (p (if (= i 0) first (if (equal? (skim--line-text l) "") rest-blank rest))))
                 (loop (cdr ls) (+ i 1) (cons (skim--prefix-line l p face) acc)))))
         (list trail)))))

(define (skim--prefix-line l p face)
  (let ((n (string-length p)))
    (if (= n 0)
        l
        (skim--line (string-append p (skim--line-text l))
                    (append (if (and face (not (equal? (skim--trim p) "")))
                                (list (list 0 n 'fg face)) '())
                            (skim--shift-spans (skim--line-spans l) n))
                    (skim--line-meta l)))))

(define (skim--rtrim s) (replace-regexp-in-string "\\s+$" "" s))

(define (skim--lines-width lines)
  (foldl (lambda (l n) (max n (string-length (skim--line-text l)))) 0 lines))

(define (skim--longest-word s)
  (foldl (lambda (w n) (max n (string-length w))) 0 (skim--words s)))

;; Displayed text: whitespace runs collapsed, ends trimmed.
(define (skim--squeeze s) (skim--trim (replace-regexp-in-string "[ \\t\\r\\n]+" " " s)))

;; Lines → one line, joined with single spaces (spans kept).
(define (skim--join-lines lines)
  (let loop ((ls lines) (text "") (spans '()))
    (if (null? ls)
        (skim--line text spans '())
        (let* ((raw (skim--line-text (car ls)))
               (t (skim--trim raw))
               (lead (skim--lead-spaces raw))
               (sep (if (or (equal? text "") (equal? t "")) "" " "))
               (off (- (+ (string-length text) (string-length sep)) lead)))
          (loop (cdr ls) (string-append text sep t)
                (append spans (skim--shift-spans (skim--line-spans (car ls)) off)))))))

;; A line back into paragraph text items, one per run of identical styling.
(define (skim--line->items l)
  (let* ((t (skim--line-text l)) (n (string-length t)) (spans (skim--line-spans l))
         (cuts (sort (skim--dedupe (map (lambda (c) (max 0 (min n c)))        ; spans may overhang
                                         (append (list 0 n) (map car spans) (map cadr spans)))) <))
         (style-at (lambda (pos kind)
                     (let loop ((ss spans))
                       (cond ((null? ss) #f)
                             ((and (eq? (caddr (car ss)) kind) (<= (car (car ss)) pos) (< pos (cadr (car ss)))) (cadddr (car ss)))
                             (else (loop (cdr ss))))))))
    (let loop ((cs cuts) (acc '()))
      (if (or (null? cs) (null? (cdr cs)))
          (reverse acc)
          (let ((a (car cs)) (b (cadr cs)))
            (loop (cdr cs)
                  (if (>= a b) acc
                      (cons (vector 'text (substring t a b)
                                    (vector (style-at a 'fg) (style-at a 'bg) (style-at a 'link) (style-at a 'field))
                                    'normal)
                            acc))))))))

;; ── paragraph state ────────────────────────────────────────────────────
;; st = #(para-reversed out-reversed): para holds inline items
;;   #('text string style ws) #('atom string style) #('br) #('anchor id)
(define (skim--new-st) (mutable-vector '() '()))

(define (skim--para-push! st item) (vector-set! st 0 (cons item (vector-ref st 0))))

(define (skim--out-push! st item) (vector-set! st 1 (cons item (vector-ref st 1))))

(define (skim--out-append! st items)
  (for-each (lambda (x) (skim--out-push! st x)) items))

(define (skim--st-items st) (reverse (vector-ref st 1)))

(define (skim--ws-mode cs)
  (let ((ws (skim--style cs "white-space")))
    (cond ((member ws '("pre" "break-spaces")) 'pre)
          ((equal? ws "pre-wrap") 'pre-wrap)
          ((equal? ws "pre-line") 'pre-line)
          ((equal? ws "nowrap") 'nowrap)
          (else 'normal))))

;; Flush the paragraph into wrapped lines.
(define (skim--flush! st ctx)
  (let ((para (reverse (vector-ref st 0))))
    (vector-set! st 0 '())
    (when (pair? para)
      (if (skim--para-empty? para)
          (for-each (lambda (it) (when (eq? (vector-ref it 0) 'anchor) (skim--out-push! st it))) para)
          (for-each (lambda (l) (skim--out-push! st l))
                    (skim--wrap para (hash-try-get ctx 'width)
                                (if (hash-try-get ctx 'align-left) "left"
                                    (skim--style (hash-try-get ctx 'cs) "text-align"))))))))

(define (skim--para-empty? para)
  (let loop ((ps para))
    (cond ((null? ps) #t)
          ((memq (vector-ref (car ps) 0) '(atom br)) #f)
          ((and (eq? (vector-ref (car ps) 0) 'text)
                (not (equal? (skim--trim (vector-ref (car ps) 1)) "")))
           #f)
          (else (loop (cdr ps))))))

;; ── wrapping ───────────────────────────────────────────────────────────
;; CSS min-content width: lay the cell out with every unbreakable unit (word,
;; image, spacer, form control) on a line of its own — no hard-splitting —
;; and take the widest. Width 0 keys this pass apart in the cell cache.
(define skim--no-chop #f)

;; Units: #('w pieces len) words (pieces: list of (string . style)),
;; #('s string style) breakable space, 'nl forced break, #('a id) anchor.
(define (skim--units para)
  (let loop ((ps para) (acc '()) (word '()) (wlen 0) (after-space #t))
    (if (null? ps)
        (reverse (skim--close-word acc word wlen))
        (let* ((it (car ps)) (kind (vector-ref it 0)))
          (cond
            ((eq? kind 'br) (loop (cdr ps) (cons 'nl (skim--close-word acc word wlen)) '() 0 #t))
            ((eq? kind 'anchor) (loop (cdr ps) (cons (vector 'a (vector-ref it 1)) acc) word wlen after-space))
            ((eq? kind 'sp)
             (loop (cdr ps) (cons (vector 's (vector-ref it 1) (vector-ref it 2)) (skim--close-word acc word wlen)) '() 0 #t))
            ((eq? kind 'atom)
             (let ((s (vector-ref it 1)))
               (loop (cdr ps) acc (cons (cons s (vector-ref it 2)) word) (+ wlen (string-length s)) #f)))
            (else
             (let ((s (vector-ref it 1)) (style (vector-ref it 2)) (ws (vector-ref it 3)))
               (cond
                 ((eq? ws 'pre)
                  (let seg ((lines (split-many (skim--expand-tabs s) "\n")) (first #t)
                            (acc acc) (word word) (wlen wlen))
                    (if (null? lines)
                        (loop (cdr ps) acc word wlen #f)
                        (let* ((acc (if first acc (cons 'nl (if (null? word) acc (cons (vector 'w (reverse word) wlen) acc)))))
                               (word (if first word '()))
                               (wlen (if first wlen 0))
                               (t (car lines)))
                          (seg (cdr lines) #f acc
                               (if (equal? t "") word (cons (cons t style) word))
                               (+ wlen (string-length t)))))))
                 ((eq? ws 'pre-wrap)
                  (let seg ((lines (split-many (skim--expand-tabs s) "\n")) (first #t) (acc acc) (word word) (wlen wlen))
                    (if (null? lines)
                        (loop (cdr ps) acc word wlen #f)
                        (let* ((acc (if first acc (cons 'nl (if (null? word) acc (cons (vector 'w (reverse word) wlen) acc)))))
                               (word (if first word '()))
                               (wlen (if first wlen 0))
                               (r (skim--split-spaces (car lines) style acc word wlen #f)))
                          (seg (cdr lines) #f (car r) (cadr r) (caddr r))))))
                 ((eq? ws 'pre-line)
                  (let seg ((lines (split-many s "\n")) (first #t) (acc acc) (word word) (wlen wlen) (sp after-space))
                    (if (null? lines)
                        (loop (cdr ps) acc word wlen sp)
                        (let* ((acc (if first acc (cons 'nl (if (null? word) acc (cons (vector 'w (reverse word) wlen) acc)))))
                               (word (if first word '()))
                               (wlen (if first wlen 0))
                               (r (skim--split-spaces (replace-regexp-in-string "[ \\t\\r]+" " " (car lines))
                                                      style acc word wlen #t)))
                          (seg (cdr lines) #f (car r) (cadr r) (caddr r) (cadddr r))))))
                 ((eq? ws 'nowrap)
                  (let* ((t (replace-regexp-in-string "[ \\t\\r\\n]+" " " s))
                         (t (if (and after-space (starts-with? t " ")) (substring t 1 (string-length t)) t)))
                    (loop (cdr ps) acc (if (equal? t "") word (cons (cons t style) word))
                          (+ wlen (string-length t)) (ends-with? t " "))))
                 (else
                  (let ((r (skim--split-spaces (replace-regexp-in-string "[ \\t\\r\\n]+" " " s)
                                               style acc word wlen after-space)))
                    (loop (cdr ps) (car r) (cadr r) (caddr r) (cadddr r))))))))))))

(define (skim--close-word acc word wlen)
  (if (null? word) acc (cons (vector 'w (reverse word) wlen) acc)))

;; Split collapsed text at single spaces into words and space units.
;; Returns (acc word wlen after-space).
(define (skim--split-spaces t style acc word wlen after-space)
  (let loop ((parts (split-many t " ")) (first #t) (acc acc) (word word) (wlen wlen) (sp after-space))
    (if (null? parts)
        (list acc word wlen sp)
        (let* ((p (car parts))
               ;; every part after the first was preceded by a space
               (r (if first
                      (list acc word wlen sp)
                      (let ((acc (if (null? word) acc (cons (vector 'w (reverse word) wlen) acc))))
                        (if sp
                            (list acc '() 0 #t)
                            (list (cons (vector 's " " style) acc) '() 0 #t)))))
               (acc (car r)) (word (cadr r)) (wlen (caddr r)) (sp (cadddr r)))
          (if (equal? p "")
              (loop (cdr parts) #f acc word wlen sp)
              (loop (cdr parts) #f acc (cons (cons p style) word) (+ wlen (string-length p)) #f))))))

(define (skim--expand-tabs s)
  (if (string-contains? s "\t") (string-replace s "\t" "        ") s))

;; Greedy fill of units into lines of at most `width` chars.
(define (skim--wrap para width align)
  (let ((width (max 1 width)))
    (let loop ((us (skim--units para))
               (line '()) (col 0) (pending #f) (anchors '()) (out '()))
      (define (emit line col anchors out)
        (cons (skim--build-line (reverse line) col width align anchors) out))
      (if (null? us)
          (reverse (if (or (pair? line) (pair? anchors)) (emit line col anchors out) out))
          (let ((u (car us)))
            (cond
              ((eq? u 'nl) (loop (cdr us) '() 0 #f '() (emit line col anchors out)))
              ((eq? (vector-ref u 0) 'a) (loop (cdr us) line col pending (cons (vector-ref u 1) anchors) out))
              ((eq? (vector-ref u 0) 's)
               (if (null? line)
                   (loop (cdr us) line col #f anchors out)
                   (loop (cdr us) line col (cons (vector-ref u 1) (vector-ref u 2)) anchors out)))
              (else
               (let* ((pieces (vector-ref u 1)) (len (vector-ref u 2))
                      (sp (if pending (string-length (car pending)) 0)))
                 (cond
                   ((null? line)
                    (if (and (> len width) (not skim--no-chop))
                        (let* ((chunks (skim--chop-pieces pieces width))
                               (out (let add ((cs chunks) (out out) (anchors anchors))
                                      (if (null? (cdr cs))
                                          out
                                          (add (cdr cs) (emit (reverse (car cs)) (skim--pieces-len (car cs)) anchors out) '())))))
                          (let ((last-chunk (last chunks)))
                            (loop (cdr us) (reverse last-chunk) (skim--pieces-len last-chunk) #f '() out)))
                        (loop (cdr us) (reverse pieces) len #f anchors out)))
                   ((<= (+ col sp len) width)
                    (loop (cdr us) (append (reverse pieces) (if pending (cons pending line) line))
                          (+ col sp len) #f anchors out))
                   (else
                    (loop us '() 0 #f '() (emit line col anchors out))))))))))))

(define (skim--pieces-len ps) (foldl (lambda (p n) (+ n (string-length (car p)))) 0 ps))

;; Hard-split an over-long word into chunks of `width` chars.
(define (skim--chop-pieces pieces width)
  (let loop ((ps pieces) (cur '()) (n 0) (out '()))
    (if (null? ps)
        (reverse (if (null? cur) out (cons (reverse cur) out)))
        (let* ((s (car (car ps))) (style (cdr (car ps))) (len (string-length s)) (room (- width n)))
          (cond
            ((<= len room) (loop (cdr ps) (cons (car ps) cur) (+ n len) out))
            ((= room 0) (loop ps '() 0 (cons (reverse cur) out)))
            (else
             (loop (cons (cons (substring s room len) style) (cdr ps))
                   '() 0 (cons (reverse (cons (cons (substring s 0 room) style) cur)) out))))))))

;; Pieces → a line with merged spans, aligned within `width`.
(define (skim--build-line pieces col width align anchors)
  (let* ((pad (cond ((member align '("center" "-webkit-center")) (quotient (max 0 (- width col)) 2))
                    ((member align '("right" "end" "-webkit-right")) (max 0 (- width col)))
                    (else 0)))
         (body (string-join (map car pieces) ""))
         ;; a line of only (non-breaking) spaces — an &nbsp; paragraph — is blank
         ;; (only nbsp lines: a spacer image's reserved plain spaces must stay)
         (blank? (and (string-contains? body skim--nbsp)
                      (equal? (skim--trim (string-replace body skim--nbsp " ")) "")))
         (pad (if blank? 0 pad))
         (text (if blank? "" (string-append (skim--spaces pad) body))))
    (let loop ((ps (if blank? '() pieces)) (c pad) (fg #f) (bg #f) (lk #f) (fd #f) (spans '()))
      (if (null? ps)
          (skim--line text (reverse (skim--close-spans c fg bg lk fd spans))
                      (if (null? anchors) '() (list (cons 'anchors anchors))))
          (let* ((s (car (car ps))) (st (cdr (car ps))) (e (+ c (string-length s))))
            (loop (cdr ps) e
                   (skim--span-step fg (vector-ref st 0) c e)
                   (skim--span-step bg (vector-ref st 1) c e)
                   (skim--span-step lk (vector-ref st 2) c e)
                   (skim--span-step fd (vector-ref st 3) c e)
                   (skim--span-emit (skim--span-emit (skim--span-emit (skim--span-emit spans fg (vector-ref st 0) c 'fg)
                                                                      bg (vector-ref st 1) c 'bg)
                                                     lk (vector-ref st 2) c 'link)
                                    fd (vector-ref st 3) c 'field)))))))

;; An open span is (start end value). Continue it when the value matches and
;; the new piece is contiguous, else start a new one (or none for #f).
(define (skim--span-step open val c e)
  (cond ((not val) #f)
        ((and open (equal? (caddr open) val) (= (cadr open) c)) (list (car open) e val))
        (else (list c e val))))

;; When an open span stops continuing at column `c`, emit it.
(define (skim--span-emit spans open val c kind)
  (if (and open (not (and val (equal? (caddr open) val) (= (cadr open) c))))
      (cons (list (car open) (cadr open) kind (caddr open)) spans)
      spans))

(define (skim--close-spans c fg bg lk fd spans)
  (let* ((spans (if fg (cons (list (car fg) (cadr fg) 'fg (caddr fg)) spans) spans))
         (spans (if bg (cons (list (car bg) (cadr bg) 'bg (caddr bg)) spans) spans))
         (spans (if lk (cons (list (car lk) (cadr lk) 'link (caddr lk)) spans) spans))
         (spans (if fd (cons (list (car fd) (cadr fd) 'field (caddr fd)) spans) spans)))
    spans))
