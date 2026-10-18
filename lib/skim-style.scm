;; skim-style.scm — how text looks: palette, faces, options, run styles.
;;
;; skim owns its colours (a dark and a light palette) because theme face
;; colours can't be read from Scheme and overlays don't merge; faces combining
;; colour + bold + underline are made on demand.

;; ── palette and faces ──────────────────────────────────────────────────
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

(define (skim--dim-face) (skim--fg-face (skim--pal 'dim) #f #f))
(define (skim--link-face) (skim--fg-face (skim--pal 'link) #f #f))

;; ── options ────────────────────────────────────────────────────────────
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

;; Session toggles (I / C in *skim*) win over the option. They live here, not
;; in the option, because `set-option` only takes effect after the command
;; returns — the re-render right after a toggle would still see the old value.
(define skim--toggles (hash))

(define (skim--setting name default)
  (if (hash-contains? skim--toggles name)
      (hash-try-get skim--toggles name)
      (skim--opt-bool name default)))

(define (skim--toggle! name default)
  (set! skim--toggles (hash-insert skim--toggles name (not (skim--setting name default))))
  (skim--setting name default))

(define (skim--opt-num name default)
  (let ((v (get-option name)))
    (cond ((number? v) v)
          ((and (string? v) (string->number v)) (string->number v))
          (else default))))

;; ── run styles ─────────────────────────────────────────────────────────
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

;; <sup>/<sub> text drawn with Unicode super/subscript characters when every
;; character has one (footnote numbers, exponents, chemical formulas);
;; otherwise left as is.
(define skim--sup-map
  (hash #\0 "⁰" #\1 "¹" #\2 "²" #\3 "³" #\4 "⁴" #\5 "⁵" #\6 "⁶" #\7 "⁷" #\8 "⁸" #\9 "⁹"
        #\+ "⁺" #\- "⁻" #\= "⁼" #\( "⁽" #\) "⁾" #\n "ⁿ" #\i "ⁱ" #\space " "))

(define skim--sub-map
  (hash #\0 "₀" #\1 "₁" #\2 "₂" #\3 "₃" #\4 "₄" #\5 "₅" #\6 "₆" #\7 "₇" #\8 "₈" #\9 "₉"
        #\+ "₊" #\- "₋" #\= "₌" #\( "₍" #\) "₎" #\space " "))

(define (skim--script-text s table)
  (let ((cs (string->list s)))
    (if (and (pair? cs) (< (length cs) 12)
             (let loop ((xs cs)) (or (null? xs) (and (hash-contains? table (car xs)) (loop (cdr xs))))))
        (string-join (map (lambda (c) (hash-try-get table c)) cs) "")
        s)))
