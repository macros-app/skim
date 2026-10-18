;; skim-layout.scm — DOM + CSS → styled text lines.
;;
;; The layout is a classic block/inline flow, done in character cells:
;;
;;   * block boxes stack; their vertical margins become collapsed blank lines,
;;     left margins/padding become indentation (capped so text keeps room);
;;   * inline content of a block accumulates in a "paragraph" of runs that is
;;     word-wrapped (greedy, honouring white-space) when the block closes;
;;   * list items get hanging markers, blockquotes a bar, headings a larger
;;     line scale, <hr> a rule, tables real column layout (box-drawn when the
;;     table holds data, borderless when it's page layout), form controls
;;     interactive widgets, and images a placeholder line the editor paints
;;     the picture under.
;;
;; Output lines are #(text spans meta): `spans` are (start end kind value) in
;; CHAR columns with kind ∈ fg bg link field; `meta` an alist (scale, img,
;; anchors). The paint step in skim.scm converts columns to bytes.

;; ── palette + faces ─────────────────────────────────────────────────────────
(define skim--palette-dark
  (hash 'bg '(26 26 26) 'fg '(216 222 233)
        'link "#6cb6ff" 'heading "#e3c893" 'code "#e394dc" 'code-bg "#2b2b2b"
        'em "#c8b8a0" 'dim "#7a7a7a" 'button "#83d6c5" 'button-bg "#263238"
        'field-bg "#2e2e2e" 'border "#555555" 'error "#f14c4c" 'title "#e3c893"
        'url "#8a8a8a" 'focus-bg "#3a3d41"))

(define skim--palette-light
  (hash 'bg '(252 252 252) 'fg '(20 20 20)
        'link "#1a5fa0" 'heading "#8f3f71" 'code "#4a7d3a" 'code-bg "#eeeeee"
        'em "#5a4a2a" 'dim "#8a8a8a" 'button "#206595" 'button-bg "#e3edf5"
        'field-bg "#ececec" 'border "#b5b5b5" 'error "#e51400" 'title "#8f3f71"
        'url "#6f6f6f" 'focus-bg "#e0e0e0"))

(define (skim--dark?)
  (not (equal? (get-option "skim-palette") "light")))

(define (skim--pal key)
  (hash-try-get (if (skim--dark?) skim--palette-dark skim--palette-light) key))

;; Faces are made on demand from (color bold underline); every made face is
;; remembered so `skim--ensure-faces` can re-seed them after a theme switch
;; (load-theme wipes face attributes).
(define skim--faces-made (hash))

(define (skim--fg-face color bold? underline?)
  (if (and (not color) (not bold?) (not underline?))
      #f
      (let ((name (string-append "skim:" (or color "d") ":" (if bold? "b" "") (if underline? "u" ""))))
        (unless (hash-contains? skim--faces-made name)
          (set! skim--faces-made (hash-insert skim--faces-made name (list 'fg color bold? underline?)))
          (skim--seed-face name (list 'fg color bold? underline?)))
        name)))

(define (skim--bg-face color)
  (if (not color)
      #f
      (let ((name (string-append "skim-bg:" color)))
        (unless (hash-contains? skim--faces-made name)
          (set! skim--faces-made (hash-insert skim--faces-made name (list 'bg color)))
          (skim--seed-face name (list 'bg color)))
        name)))

(define (skim--seed-face name spec)
  (if (eq? (car spec) 'bg)
      (set-face-background name (cadr spec))
      (begin
        (when (cadr spec) (set-face name (cadr spec)))
        (set-face-bold name (if (caddr spec) #t #f))
        (set-face-underline name (if (cadddr spec) #t #f)))))

(define (skim--ensure-faces)
  (for-each (lambda (name) (skim--seed-face name (hash-try-get skim--faces-made name)))
            (hash-keys->list skim--faces-made)))

;; ── options ─────────────────────────────────────────────────────────────────
(define (skim--opt name default)
  (let ((v (get-option name)))
    (cond ((eq? v #f) default) (else v))))

(define (skim--opt-bool name default)
  (let ((v (get-option name)))
    (cond ((eq? v #f) default)        ; unset reads as #f — use "off" to disable
          ((eq? v #t) #t)
          ((string? v) (not (member (skim--lower v) '("false" "no" "off" "0" "nil"))))
          ((number? v) (not (= v 0)))
          (else default))))

(define (skim--opt-num name default)
  (let ((v (get-option name)))
    (cond ((number? v) v)
          ((and (string? v) (string->number v)) (string->number v))
          (else default))))

;; ── render-wide state ───────────────────────────────────────────────────────
;; One render's accumulators: links, form fields, forms, image count.
(define skim--R (hash))
(define (skim--R-reset! base field-values)
  (set! skim--R (hash 'links (mutable-vector) 'fields (mutable-vector) 'forms (mutable-vector)
                      'images 0 'base base 'values field-values))
  (set! skim--gen (+ skim--gen 1)))

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
        (let ((r (skim--compute el m parent-cs sheet)))
          (vector-set! el 5 (cons skim--gen r))
          r))))
(define (skim--Rget key) (hash-try-get skim--R key))

(define (skim--add-link url)
  (let ((v (skim--Rget 'links)))
    (vector-push! v url)
    (- (vector-length v) 1)))

(define (skim--add-field f)
  (let ((v (skim--Rget 'fields)))
    (vector-push! v f)
    (- (vector-length v) 1)))

;; ── lines and items ─────────────────────────────────────────────────────────
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

;; ── text styling ────────────────────────────────────────────────────────────
;; A run style is #(fg-face bg-face link-index field-index).
(define skim--plain-style (vector #f #f #f #f))

(define (skim--css-color cs prop bg-rgb)
  (and (skim--opt-bool "skim-css-colors" #t)
       (let ((rgb (skim--parse-color (skim--style cs prop))))
         (and rgb (>= (skim--contrast rgb bg-rgb) 3.0) (skim--color->hex rgb)))))

;; Style for text directly inside an element with computed style `cs`.
(define (skim--text-style cs ctx)
  (let* ((link (hash-try-get ctx 'link))
         (bg-rgb (or (hash-try-get ctx 'bg-rgb) (skim--pal 'bg)))
         (css-fg (skim--css-color cs "color" bg-rgb))
         (heading? (hash-try-get ctx 'heading))
         (mono? (hash-try-get ctx 'code))
         (color (cond (link (skim--pal 'link))
                      ((skim--strike? cs) (skim--pal 'dim))
                      (css-fg css-fg)
                      (heading? (skim--pal 'heading))
                      (mono? (skim--pal 'code))
                      ((skim--italic? cs) (skim--pal 'em))
                      (else #f)))
         (bold? (skim--bold? cs))
         (under? (skim--underline? cs))
         (bg (or (hash-try-get ctx 'bg-face)
                 (and mono? (not (hash-try-get ctx 'pre)) (skim--bg-face (skim--pal 'code-bg))))))
    (vector (skim--fg-face color bold? under?) bg link (hash-try-get ctx 'field))))

(define (skim--transform-text s cs)
  (let ((tt (skim--style cs "text-transform")))
    (cond ((equal? tt "uppercase") (string-upcase s))
          ((equal? tt "lowercase") (string-downcase s))
          ((equal? tt "capitalize") (skim--capitalize s))
          ((equal? (skim--style cs "font-variant") "small-caps") (string-upcase s))
          (else s))))

(define (skim--capitalize s)
  (let loop ((cs (string->list s)) (start #t) (acc '()))
    (if (null? cs)
        (list->string (reverse acc))
        (let ((c (car cs)))
          (loop (cdr cs) (char-whitespace? c)
                (cons (if start (char-upcase c) c) acc))))))

;; ── paragraph state ─────────────────────────────────────────────────────────
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

;; ── wrapping ────────────────────────────────────────────────────────────────
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
                    (if (> len width)
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
         (text (string-append (skim--spaces pad) (string-join (map car pieces) ""))))
    (let loop ((ps pieces) (c pad) (fg #f) (bg #f) (lk #f) (fd #f) (spans '()))
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

;; ── the walk ────────────────────────────────────────────────────────────────
(define skim--block-displays
  (hash "block" #t "flex" #t "grid" #t "list-item" #t "table" #t "flow-root" #t
        "table-caption" #t "table-row" #t "table-row-group" #t "table-header-group" #t
        "table-footer-group" #t "table-cell" #t "-webkit-box" #t "ruby-base-container" #f))

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
            (if (and (skim--para-empty? (vector-ref st 0)) (pair? (cdr ks))
                     (not (hash-try-get ctx 'flex-inline))
                     (skim--short-float? k km ctx))
                (skim--float-layout k km (cdr ks) (+ i 1) nel m ctx st)
                (begin
                  (when (and (hash-try-get ctx 'flex-inline) (not first-el))
                    (skim--para-push! st (vector 'sp "  " skim--plain-style)))
                  (skim--walk-el k km ctx st)
                  (skim--walk-list (cdr ks) (+ i 1) km #f nel m ctx st))))))))

;; A short left float (a vote count, a date, an icon caption) that the
;; following content should wrap beside.
(define (skim--short-float? k km ctx)
  (let* ((r (skim--compute-cached k km (hash-try-get ctx 'cs) (hash-try-get ctx 'sheet)))
         (cs (car r)))
    (and (equal? (skim--style cs "float") "left")
         (not (skim--hidden? cs))
         (hash-contains? skim--block-displays (skim--style cs "display"))
         (let ((t (skim--trim (skim--text-content k))))
           (and (> (string-length t) 0) (< (string-length t) 16)))
         (not (let loop ((ks (skim--el-kids k)))
                (and (pair? ks)
                     (or (and (skim--el? (car ks))
                              (skim--find-el (car ks) (lambda (e) (member (skim--el-tag e) '("table" "ul" "ol" "p" "img" "div")))))
                         (loop (cdr ks)))))))))

;; Lay out the siblings after float `k`, then put the float's text in the
;; first line's left margin when it fits there, else as a hanging column.
(define (skim--float-layout k km rest i nel m ctx st)
  (skim--flush! st ctx)                 ; only whitespace (and anchors) pending
  (let* ((fst (skim--new-st))
         (_ (skim--walk-el k km (hash-insert ctx 'width 40) fst))
         (_ (skim--flush! fst (hash-insert (hash-insert ctx 'width 40) 'align-left #t)))
         (flines (filter skim--line? (skim--resolve-items (skim--st-items fst))))
         (fline0 (if (pair? flines) (car flines) skim--blank-line))
         (raw (skim--rtrim (skim--line-text fline0)))
         (flead (- (string-length raw) (string-length (replace-regexp-in-string "^ +" "" raw))))
         (ftext (substring raw flead (string-length raw)))
         (fspans (skim--shift-spans (skim--line-spans fline0) (- 0 flead)))
         (fcss (skim--px (skim--style (car (skim--compute-cached k km (hash-try-get ctx 'cs) (hash-try-get ctx 'sheet))) "width")))
         (fw (max (+ (string-length ftext) 1) (if fcss (min 16 (skim--int (/ (+ fcss 4) 8))) 0)))
         (sub (skim--new-st)))
    (skim--walk-list rest i km #f nel m ctx sub)
    (skim--flush! sub ctx)
    (let* ((items0 (skim--st-items sub))
           (first-text (let find-first ((xs items0)) (cond ((null? xs) #f) ((skim--line? (car xs)) (skim--line-text (car xs))) (else (find-first (cdr xs))))))
           (lead (if first-text (- (string-length first-text) (string-length (replace-regexp-in-string "^ +" "" first-text))) 0))
           ;; content wraps beside the float: shift right only as far as the
           ;; float needs beyond the content's own left margin
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

;; Text directly inside the element whose context is `ctx`.
(define (skim--add-text! st s ctx)
  (let* ((cs (hash-try-get ctx 'cs))
         (ws (skim--ws-mode cs)))
    (skim--para-push! st (vector 'text (skim--transform-text s cs) (hash-try-get ctx 'tstyle) ws))))

;; Child context for an element with computed style `cs`.
(define (skim--child-ctx ctx el cs)
  (let* ((ctx (hash-insert ctx 'cs cs))
         (ctx (hash-insert ctx 'flex-inline #f))
         (tag (skim--el-tag el))
         (ctx (if (and (equal? tag "a") (skim--attr el "href")
                       (not (starts-with? (skim--lower (skim--trim (skim--attr el "href"))) "javascript:")))
                  (hash-insert ctx 'link (skim--add-link (skim--resolve (skim--Rget 'base) (skim--attr el "href"))))
                  ctx))
         (ctx (if (hash-contains? skim--headings tag) (hash-insert ctx 'heading tag) ctx))
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
         (ctx (let ((bg (and (not (hash-contains? skim--block-displays (skim--style cs "display")))
                             (skim--opt-bool "skim-css-colors" #t)
                             (skim--parse-color (skim--style cs "background-color")))))
                (if (and bg
                         (let ((fg (or (skim--parse-color (skim--style cs "color")) (skim--pal 'fg))))
                           (>= (skim--contrast fg bg) 3.0))
                         (>= (skim--contrast bg (skim--pal 'bg)) 1.6))
                    (hash-insert (hash-insert ctx 'bg-rgb bg) 'bg-face (skim--bg-face (skim--color->hex bg)))
                    ctx))))
    (hash-insert ctx 'tstyle (skim--text-style cs ctx))))

(define (skim--walk-el el m ctx st)
  (let* ((r (skim--compute-cached el m (hash-try-get ctx 'cs) (hash-try-get ctx 'sheet)))
         (cs (car r))
         (tag (skim--el-tag el))
         (display (skim--style cs "display")))
    (cond
      ((skim--hidden? cs) #t)
      ((equal? tag "br") (skim--para-push! st (vector 'br)))
      ((equal? tag "wbr") #t)
      ((equal? tag "img") (skim--image el cs ctx st))
      ((member tag '("input" "select" "textarea" "button"))
       (skim--widget el m cs ctx st))
      ((member tag '("iframe" "video" "audio" "embed" "object"))
       (skim--media el cs ctx st))
      ((equal? tag "hr") (skim--hr cs ctx st))
      (else
       (let* ((cctx (skim--child-ctx ctx el cs))
              (cctx (if (equal? tag "form") (hash-insert cctx 'form (skim--add-form el)) cctx))
              (id (or (skim--attr el "id") (and (equal? tag "a") (skim--attr el "name")))))
         (cond
           ((equal? display "contents")
            (when id (skim--para-push! st (vector 'anchor id)))
            (skim--walk-inline el m cs r cctx st))
           ((and (equal? tag "table") (equal? display "table"))
            (skim--flush! st ctx)
            (skim--block-margins-wrap st cs ctx el
                                      (lambda (w) (skim--table el m cs (hash-insert cctx 'width w)))
                                      id))
           ;; a short float that isn't wrapped (see skim--float-layout) flows inline
           ((and (hash-contains? skim--block-displays display)
                 (member (skim--style cs "float") '("left" "right"))
                 (< (string-length (skim--trim (skim--text-content el))) 40)
                 (not (skim--find-el el (lambda (e) (member (skim--el-tag e) '("table" "ul" "ol" "p" "img"))))))
            (when id (skim--para-push! st (vector 'anchor id)))
            (skim--walk-inline el m cs r cctx st)
            (skim--para-push! st (vector 'sp " " skim--plain-style)))
           ((hash-contains? skim--block-displays display)
            (if (hash-try-get ctx 'flex-inline)
                (skim--walk-inline el m cs r cctx st)
                (skim--block el m cs r cctx ctx st id)))
           (else
            (when id (skim--para-push! st (vector 'anchor id)))
            (skim--walk-inline el m cs r cctx st))))))))

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

;; ── blocks ──────────────────────────────────────────────────────────────────
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
(define skim--flex-blockers (hash "div" #t "p" #t "ul" #t "ol" #t "table" #t "h1" #t "h2" #t "h3" #t
                                  "h4" #t "section" #t "article" #t "img" #t "pre" #t "form" #t
                                  "blockquote" #t "figure" #t "header" #t "footer" #t))

(define (skim--flex-inline? el cs)
  (and (member (skim--style cs "display") '("flex" "inline-flex"))
       (not (string-contains? (skim--style cs "flex-direction") "column"))
       (let ((kids (filter skim--el? (skim--el-kids el))))
         (and (pair? kids) (<= (length kids) 16)
              (let loop ((ks kids))
                (or (null? ks)
                    (and (< (string-length (skim--trim (skim--text-content (car ks)))) 50)
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
              (summary? (equal? tag "summary"))
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
             (quote? (skim--prefix items "│ " "│ " (skim--fg-face (skim--pal 'dim) #f #f)))
             (summary? (skim--prefix items
                                     (let ((p (vector-ref m 2)))
                                       (if (and p (skim--has-attr? (vector-ref p 0) "open")) "▾ " "▸ "))
                                     "  " #f))
             (else items)))))
     id)))

;; ── lists ───────────────────────────────────────────────────────────────────
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

;; ── rules, media, images ────────────────────────────────────────────────────
(define (skim--hr cs ctx st)
  (skim--flush! st ctx)
  (skim--out-push! st 1)
  (let ((w (hash-try-get ctx 'width)))
    (skim--out-push! st (skim--line (skim--repeat "─" w) (list (list 0 w 'fg (skim--fg-face (skim--pal 'dim) #f #f))) '())))
  (skim--out-push! st 1))

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
          (skim--para-push! st (vector 'br)))
        ;; no source: render fallback content
        (let ((cctx (skim--child-ctx ctx el cs)))
          (skim--walk-kids el (skim--mctx el #f #f 1 #t) cctx st)))))

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
      ((and src (starts-with? src "data:image/") (> (string-length src) 400)) src)
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
         (show? (and url (not icon?) (not svg?) (skim--opt-bool "skim-images" #t)
                     (< (skim--Rget 'images) (skim--opt-num "skim-max-images" 60)))))
    (cond
      (show?
       (set! skim--R (hash-insert skim--R 'images (+ 1 (skim--Rget 'images))))
       (skim--flush! st ctx)
       (let* ((label (if (equal? alt "") "▣" (string-append "▣ " (skim--truncate alt (max 10 (- (hash-try-get ctx 'width) 2))))))
              (maxw (skim--int (min (or w 10000) (skim--opt-num "skim-image-max-width" 640))))
              (style (vector (skim--fg-face (if link (skim--pal 'link) (skim--pal 'dim)) #f #f) #f link #f))
              (line (car (skim--wrap (list (vector 'atom label style)) (max 8 (string-length label)) "left"))))
         (skim--out-push! st (skim--meta-put line 'img (cons url maxw)))))
      ((not (equal? alt ""))
       (skim--para-push! st (vector 'text (if icon? alt (string-append "[" alt "]"))
                                    (vector (skim--fg-face (if link (skim--pal 'link) (skim--pal 'dim)) #f #f) #f link #f)
                                    'normal)))
      ((and link (not icon?))
       (skim--para-push! st (vector 'atom "[img]" (vector (skim--fg-face (skim--pal 'link) #f #f) #f link #f))))
      (else #t))))

(define (skim--resolve-image-url src)
  (if (starts-with? src "data:") src (skim--resolve (skim--Rget 'base) src)))

;; ── forms ───────────────────────────────────────────────────────────────────
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
       (let* ((label (or (skim--attr el "value") (skim--attr el "alt")
                         (if (equal? type "reset") "Reset" (if (equal? type "submit") "Submit" "button"))))
              (idx (skim--add-field (hash 'kind (if (equal? type "image") "submit" type) 'name name
                                          'value (or (skim--attr el "value") "") 'form form 'disabled disabled?))))
         (skim--para-push! st (vector 'atom (string-append "[ " label " ]") (button-style idx)))))
      ((equal? type "file")
       (skim--para-push! st (vector 'atom "[file upload unsupported]" (vector (skim--fg-face (skim--pal 'dim) #f #f) #f #f #f))))
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
                                          (vector (skim--fg-face (skim--pal 'dim) #f #f) (skim--bg-face (skim--pal 'field-bg)) #f idx)
                                          (field-style idx)))))))))

;; ── tables ──────────────────────────────────────────────────────────────────
;; Rows of the table: direct <tr>s and those inside thead/tbody/tfoot (thead
;; first, tfoot last), as (tr-el . mctx-parent) pairs.
(define (skim--table-rows tbl)
  (let loop ((ks (skim--el-kids tbl)) (head '()) (body '()) (foot '()))
    (if (null? ks)
        (append (reverse head) (reverse body) (reverse foot))
        (let ((k (car ks)))
          (cond
            ((string? k) (loop (cdr ks) head body foot))
            ((equal? (skim--el-tag k) "tr") (loop (cdr ks) head (cons k body) foot))
            ((member (skim--el-tag k) '("thead" "tbody" "tfoot"))
             (let ((rows (filter (lambda (x) (and (skim--el? x) (equal? (skim--el-tag x) "tr"))) (skim--el-kids k))))
               (cond ((equal? (skim--el-tag k) "thead") (loop (cdr ks) (append (reverse rows) head) body foot))
                     ((equal? (skim--el-tag k) "tfoot") (loop (cdr ks) head body (append (reverse rows) foot)))
                     (else (loop (cdr ks) head (append (reverse rows) body) foot)))))
            (else (loop (cdr ks) head body foot)))))))

(define (skim--data-table? tbl rows)
  (let ((border (skim--attr tbl "border"))
        (cls (skim--lower (or (skim--attr tbl "class") ""))))
    (or (and border (not (member (skim--trim border) '("0" ""))))
        (string-contains? cls "table") (string-contains? cls "data") (string-contains? cls "grid")
        (skim--find-el tbl (lambda (e) (member (skim--el-tag e) '("th" "caption" "thead"))))
        #f)))

;; Render one cell's content at `width` → resolved lines.
(define (skim--cell-lines cell m cs ctx width)
  (let* ((c (vector-ref cell 6))
         (entries (if (and c (= (car c) skim--gen)) (cdr c) '()))
         (hit (assv width entries)))
    (if hit
        (cdr hit)
        (let ((lines (skim--cell-lines-raw cell m cs ctx width)))
          (vector-set! cell 6 (cons skim--gen (cons (cons width lines) entries)))
          lines))))

(define (skim--cell-lines-raw cell m cs ctx width)
  (let* ((sub (skim--new-st))
         (cctx (skim--child-ctx ctx cell cs))
         (cctx (hash-insert (hash-insert (hash-insert cctx 'width width) 'in-cell #t) 'align-left #t)))
    (skim--walk-kids cell m cctx sub)
    (skim--flush! sub cctx)
    (map (lambda (l) (skim--meta-put l 'scale 1)) (skim--resolve-items (skim--st-items sub)))))

(define (skim--lines-width lines)
  (foldl (lambda (l n) (max n (string-length (skim--line-text l)))) 0 lines))

(define (skim--longest-word s)
  (foldl (lambda (w n) (max n (string-length w))) 0 (skim--words s)))

(define (skim--table tbl m cs ctx)
  (let* ((width (hash-try-get ctx 'width))
         (sheet (hash-try-get ctx 'sheet))
         (rows (skim--table-rows tbl))
         (data? (skim--data-table? tbl rows))
         (caption (let ((c (filter (lambda (k) (and (skim--el? k) (equal? (skim--el-tag k) "caption"))) (skim--el-kids tbl))))
                    (if (pair? c) (car c) #f)))
         ;; Place cells: grid rows of (cell-el cs mctx col span header?)
         (placed (skim--place-cells rows m cs sheet))
         (cells (car placed)) (nrows (cadr placed)) (ncols (caddr placed)))
    (if (or (= ncols 0) (null? cells))
        '()
        (let* ((avail (if data? (- width (+ (* 3 ncols) 1)) (- width (* 2 (- ncols 1)))))
               (measured (map (lambda (c) (skim--measure-cell c ctx avail)) cells))
               (widths (skim--column-widths (map cons cells measured) ncols (max ncols avail)))
               (rendered (map (lambda (c meas)
                                (let ((w (skim--span-width widths (vector-ref c 4) (vector-ref c 5) data?)))
                                  (vector c (if (and (vector-ref meas 2) (<= (vector-ref meas 1) w))
                                                (vector-ref meas 2)
                                                (skim--cell-lines (vector-ref c 0) (vector-ref c 2) (vector-ref c 1) ctx w))
                                          w)))
                              cells measured))
               (body (skim--compose-rows rendered widths nrows ncols data?))
               (cap (if caption
                        (let ((ccs (car (skim--compute caption (skim--mctx caption m #f 1 #f) cs sheet))))
                          (skim--cell-lines caption (skim--mctx caption m #f 1 #f) ccs ctx width))
                        '())))
          (append cap body)))))

;; Grid placement honouring colspan/rowspan. Returns (cells nrows ncols)
;; where each cell is #(el cs mctx row col span rowspan header?).
(define (skim--place-cells rows tm tcs sheet)
  (let loop ((rs rows) (r 0) (occupied (hash)) (acc '()) (ncols 0) (prev-row #f))
    (if (null? rs)
        (list (reverse acc) r ncols)
        (let* ((tr (car rs))
               (trm (skim--mctx tr tm prev-row (+ r 1) (null? (cdr rs))))
               (trcs (car (skim--compute-cached tr trm tcs sheet))))
          (if (skim--hidden? trcs)
              (loop (cdr rs) r occupied acc ncols trm)
              (let* ((tds (filter (lambda (k) (and (skim--el? k) (member (skim--el-tag k) '("td" "th")))) (skim--el-kids tr)))
                     (nt (length tds)))
                (let cell-loop ((ts tds) (i 0) (c 0) (occupied occupied) (acc acc) (ncols ncols) (prev #f))
                  (if (null? ts)
                      (loop (cdr rs) (+ r 1) occupied acc (max ncols c) trm)
                      (let ((c (let skip ((c c)) (if (hash-contains? occupied (cons r c)) (skip (+ c 1)) c))))
                        (let* ((td (car ts))
                               (tdm (skim--mctx td trm prev (+ i 1) (= (+ i 1) nt)))
                               (tdcs (car (skim--compute-cached td tdm trcs sheet)))
                               (span (max 1 (min 50 (or (skim--num (skim--attr td "colspan")) 1))))
                               (rspan (max 1 (min 50 (or (skim--num (skim--attr td "rowspan")) 1))))
                               (occupied (let occ ((k 1) (o occupied))
                                           (if (>= k rspan) o
                                               (occ (+ k 1) (let col ((j 0) (o o))
                                                              (if (>= j span) o (col (+ j 1) (hash-insert o (cons (+ r k) (+ c j)) #t)))))))))
                          (if (skim--hidden? tdcs)
                              (cell-loop (cdr ts) (+ i 1) c occupied acc ncols tdm)
                              (cell-loop (cdr ts) (+ i 1) (+ c span) occupied
                                         (cons (vector td tdcs tdm r c span rspan (equal? (skim--el-tag td) "th")) acc)
                                         (max ncols (+ c span)) tdm))))))))))))

;; #(min max lines-at-max-or-#f) for a cell.
(define (skim--measure-cell c ctx avail)
  (let* ((el (vector-ref c 0))
         (text (skim--text-content el))
         (big? (> (string-length text) 2500)))
    (if big?
        (vector (min 30 (skim--longest-word text)) (max 30 avail) #f)
        (let* ((lines (skim--cell-lines el (vector-ref c 2) (vector-ref c 1) ctx 1000))
               (mx (skim--lines-width lines))
               (mn (min 24 (max (skim--longest-word text) (if (> mx 0) 1 0)))))
          (vector (min mn mx) mx lines)))))

;; Distribute `avail` columns over `ncols` from per-cell min/max widths.
(define (skim--column-widths pairs ncols avail)
  (let* ((mins (make-vector ncols 0))
         (maxs (make-vector ncols 0)))
    ;; single-column cells first
    (for-each (lambda (pair)
                (let ((c (car pair)) (m (cdr pair)))
                  (when (= (vector-ref c 5) 1)
                    (let ((col (vector-ref c 4)))
                      (vector-set! mins col (max (vector-ref mins col) (vector-ref m 0)))
                      (vector-set! maxs col (max (vector-ref maxs col) (vector-ref m 1)))))))
              pairs)
    (let* ((summin (foldl + 0 (vector->list mins)))
           (summax (foldl + 0 (vector->list maxs))))
      (cond
        ((<= summax avail) (vector->list maxs))
        ((>= summin avail) (vector->list mins))
        (else
         (let* ((extra (- avail summin))
                (slack (- summax summin)))
           (map (lambda (mn mx) (+ mn (skim--int (/ (* (- mx mn) extra) slack))))
                (vector->list mins) (vector->list maxs))))))))

(define (skim--span-width widths col span data?)
  (let loop ((i col) (n 0) (sum 0))
    (if (or (>= n span) (>= i (length widths)))
        (+ sum (* (- n 1) (if data? 3 2)))
        (loop (+ i 1) (+ n 1) (+ sum (list-ref widths i))))))

(define (skim--pad-line l w align)
  (let* ((t (skim--line-text l)) (n (string-length t)) (gap (max 0 (- w n)))
         (left (cond ((equal? align "center") (quotient gap 2)) ((equal? align "right") gap) (else 0))))
    (skim--line (string-append (skim--spaces left) t (skim--spaces (- gap left)))
                (skim--shift-spans (skim--line-spans l) left) (skim--line-meta l))))

;; Compose rendered cells into table lines.
(define (skim--compose-rows rendered widths nrows ncols data?)
  (let* ((border (skim--fg-face (skim--pal 'border) #f #f))
         (by-row (make-vector nrows '()))
         (_ (for-each (lambda (rc) (let ((r (vector-ref (vector-ref rc 0) 3)))
                                     (vector-set! by-row r (cons rc (vector-ref by-row r)))))
                      rendered))
         (row-height (lambda (rcs) (foldl (lambda (rc n) (max n (length (vector-ref rc 1)))) 0 rcs)))
         (multi? (let loop ((r 0)) (cond ((>= r nrows) #f) ((> (row-height (vector-ref by-row r)) 1) #t) (else (loop (+ r 1))))))
         (hline (lambda (l m r)
                  (let ((t (string-append l (string-join (map (lambda (w) (skim--repeat "─" (+ w 2))) widths) m) r)))
                    (skim--line t (list (list 0 (string-length t) 'fg border)) '()))))
         (header-row? (lambda (rcs) (and (pair? rcs) (let loop ((xs rcs)) (or (null? xs) (and (vector-ref (vector-ref (car xs) 0) 7) (loop (cdr xs))))))))
         (sep-after (lambda (r) (and (< r (- nrows 1))
                                     (or multi? (and (header-row? (vector-ref by-row r))
                                                     (not (header-row? (vector-ref by-row (+ r 1))))))))))
    (let loop ((r 0) (out (if data? (list (hline "┌" "┬" "┐")) '())))
      (if (>= r nrows)
          (reverse (if data? (cons (hline "└" "┴" "┘") out) out))
          (let* ((rcs (sort-by (lambda (rc) (vector-ref (vector-ref rc 0) 4)) < (vector-ref by-row r)))
                 ;; an all-empty row (a spacer) takes no line in a layout table
                 (h (if data? (max 1 (row-height rcs)) (row-height rcs)))
                 (lines (skim--row-lines rcs widths h ncols data? border))
                 (out (append (reverse lines) out))
                 (out (if (and data? (sep-after r)) (cons (hline "├" "┼" "┤") out) out)))
            (loop (+ r 1) out))))))

;; The `h` text lines of one table row.
(define (skim--row-lines rcs widths h ncols data? border)
  (let line-loop ((k 0) (acc '()))
    (if (>= k h)
        (reverse acc)
        (let cell-loop ((col 0) (rs rcs) (text '()) (spans '()) (meta '()) (pos 0))
          (if (>= col ncols)
              (let* ((tail (if data? "│" ""))
                     (t (string-join (reverse (cons tail text)) ""))
                     (t (if data? t (skim--rtrim t)))
                     (spans (if data? (cons (list pos (+ pos 1) 'fg border) spans) spans)))
                (line-loop (+ k 1) (cons (skim--line t spans meta) acc)))
              (let* ((rc (and (pair? rs) (= (vector-ref (vector-ref (car rs) 0) 4) col) (car rs)))
                     (span (if rc (vector-ref (vector-ref rc 0) 5) 1))
                     (w (skim--span-width widths col span data?))
                     (cell-line (and rc (< k (length (vector-ref rc 1))) (list-ref (vector-ref rc 1) k)))
                     (align (if rc (let ((a (skim--style (vector-ref (vector-ref rc 0) 1) "text-align")))
                                     (cond ((member a '("center" "-webkit-center")) "center")
                                           ((member a '("right" "end")) "right")
                                           (else "left")))
                                "left"))
                     (cl (skim--pad-line (or cell-line skim--blank-line) w align))
                     (lead (cond (data? "│ ") ((= col 0) "") ((= w 0) "") (else "  ")))
                     (start (+ pos (string-length lead)))
                     (spans (append (skim--shift-spans (skim--line-spans cl) start)
                                    (if data? (cons (list pos (+ pos 1) 'fg border) spans) spans)))
                     (cell-text (string-append lead (skim--line-text cl) (if data? " " "")))
                     (meta (if (and cell-line (null? meta)) (filter (lambda (kv) (not (eq? (car kv) 'scale))) (skim--line-meta cell-line)) meta)))
                (cell-loop (+ col span) (if rc (cdr rs) rs) (cons cell-text text) spans meta
                           (+ pos (string-length cell-text)))))))))

;; ── document ────────────────────────────────────────────────────────────────
;; Lay out `root` → hash: 'lines (list) 'links 'fields 'forms 'title.
(define (skim--layout root sheet base width field-values)
  (skim--R-reset! base field-values)
  (let* ((st (skim--new-st))
         (ctx (hash 'width width 'cs skim--root-style 'sheet sheet 'tstyle skim--plain-style))
         (m (skim--mctx root #f #f 1 #t)))
    (skim--walk-kids root m ctx st)
    (skim--flush! st ctx)
    (hash 'lines (skim--resolve-items (skim--st-items st))
          'links (skim--Rget 'links)
          'fields (skim--Rget 'fields)
          'forms (skim--Rget 'forms))))
