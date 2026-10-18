;; skim-util.scm — string, entity, URL and color helpers for skim.
;;
;; Everything here is pure (no editor state). Positions are CHAR indexes unless
;; a name says bytes; regex results are only ever used as captured STRINGS or
;; via sentinel-split, never as byte offsets into char-indexed strings.

;; ── sentinels ───────────────────────────────────────────────────────────────
;; Control characters that never survive into rendered text (the tokenizer
;; strips them from input first), used to fence regex matches so a native
;; `split-many` can cut a big string in one call.
(define skim--S1 (string (integer->char 1)))
(define skim--S2 (string (integer->char 2)))
(define skim--S3 (string (integer->char 3)))
(define skim--S4 (string (integer->char 4)))
(define skim--nbsp (string (integer->char 160)))

;; ── strings ─────────────────────────────────────────────────────────────────
(define (skim--trim s) (replace-regexp-in-string "^[ \\t\\r\\n\\f]+|[ \\t\\r\\n\\f]+$" "" s))
(define (skim--lower s) (string-downcase s))
(define (skim--bytelen s) (bytes-length (string->bytes s)))
(define (skim--spaces n) (if (<= n 0) "" (make-string n #\space)))

;; `s` repeated `n` times (multi-char strings such as "─").
(define (skim--repeat s n)
  (let loop ((i 0) (acc '()))
    (if (>= i n) (string-join acc "") (loop (+ i 1) (cons s acc)))))

(define (skim--words s) (split-whitespace s))

(define (skim--aget alist key)
  (cond ((null? alist) #f)
        ((equal? (car (car alist)) key) (cdr (car alist)))
        (else (skim--aget (cdr alist) key))))

(define (skim--take xs n)
  (let loop ((xs xs) (n n) (acc '()))
    (if (or (<= n 0) (null? xs)) (reverse acc) (loop (cdr xs) (- n 1) (cons (car xs) acc)))))

(define (skim--index-of x xs)
  (let loop ((xs xs) (i 0))
    (cond ((null? xs) #f) ((equal? (car xs) x) i) (else (loop (cdr xs) (+ i 1))))))

;; Pad `s` on the right to `w` chars (never truncates).
(define (skim--pad-right s w) (string-append s (skim--spaces (- w (string-length s)))))

(define (skim--truncate s w)
  (if (> (string-length s) w) (string-append (substring s 0 (max 0 (- w 1))) "…") s))

(define (skim--num s)
  (and (string? s) (string->number (skim--trim s))))

(define (skim--int x) (inexact->exact (floor x)))

;; ── entities ────────────────────────────────────────────────────────────────
(define skim--entity-table
  (let loop ((xs
              '(("nbsp" . " ") ("amp" . "&") ("lt" . "<") ("gt" . ">") ("quot" . "\"")
                ("apos" . "'") ("copy" . "©") ("reg" . "®") ("trade" . "™") ("hellip" . "…")
                ("mdash" . "—") ("ndash" . "–") ("lsquo" . "‘") ("rsquo" . "’") ("ldquo" . "“")
                ("rdquo" . "”") ("sbquo" . "‚") ("bdquo" . "„") ("laquo" . "«") ("raquo" . "»")
                ("lsaquo" . "‹") ("rsaquo" . "›") ("bull" . "•") ("middot" . "·") ("deg" . "°")
                ("plusmn" . "±") ("times" . "×") ("divide" . "÷") ("frac12" . "½") ("frac14" . "¼")
                ("frac34" . "¾") ("sup1" . "¹") ("sup2" . "²") ("sup3" . "³") ("micro" . "µ")
                ("para" . "¶") ("sect" . "§") ("cent" . "¢") ("pound" . "£") ("yen" . "¥")
                ("euro" . "€") ("curren" . "¤") ("larr" . "←") ("rarr" . "→") ("uarr" . "↑")
                ("darr" . "↓") ("harr" . "↔") ("lArr" . "⇐") ("rArr" . "⇒") ("hArr" . "⇔")
                ("crarr" . "↵") ("le" . "≤") ("ge" . "≥") ("ne" . "≠") ("asymp" . "≈")
                ("equiv" . "≡") ("infin" . "∞") ("sum" . "∑") ("prod" . "∏") ("minus" . "−")
                ("radic" . "√") ("part" . "∂") ("nabla" . "∇") ("isin" . "∈") ("notin" . "∉")
                ("cap" . "∩") ("cup" . "∪") ("and" . "∧") ("or" . "∨") ("not" . "¬")
                ("forall" . "∀") ("exist" . "∃") ("empty" . "∅") ("prime" . "′") ("Prime" . "″")
                ("alpha" . "α") ("beta" . "β") ("gamma" . "γ") ("delta" . "δ") ("epsilon" . "ε")
                ("zeta" . "ζ") ("eta" . "η") ("theta" . "θ") ("iota" . "ι") ("kappa" . "κ")
                ("lambda" . "λ") ("mu" . "μ") ("nu" . "ν") ("xi" . "ξ") ("omicron" . "ο")
                ("pi" . "π") ("rho" . "ρ") ("sigma" . "σ") ("sigmaf" . "ς") ("tau" . "τ")
                ("upsilon" . "υ") ("phi" . "φ") ("chi" . "χ") ("psi" . "ψ") ("omega" . "ω")
                ("Alpha" . "Α") ("Beta" . "Β") ("Gamma" . "Γ") ("Delta" . "Δ") ("Theta" . "Θ")
                ("Lambda" . "Λ") ("Pi" . "Π") ("Sigma" . "Σ") ("Phi" . "Φ") ("Psi" . "Ψ")
                ("Omega" . "Ω") ("aacute" . "á") ("eacute" . "é") ("iacute" . "í") ("oacute" . "ó")
                ("uacute" . "ú") ("yacute" . "ý") ("Aacute" . "Á") ("Eacute" . "É") ("Iacute" . "Í")
                ("Oacute" . "Ó") ("Uacute" . "Ú") ("agrave" . "à") ("egrave" . "è") ("igrave" . "ì")
                ("ograve" . "ò") ("ugrave" . "ù") ("Agrave" . "À") ("Egrave" . "È") ("acirc" . "â")
                ("ecirc" . "ê") ("icirc" . "î") ("ocirc" . "ô") ("ucirc" . "û") ("auml" . "ä")
                ("euml" . "ë") ("iuml" . "ï") ("ouml" . "ö") ("uuml" . "ü") ("yuml" . "ÿ")
                ("Auml" . "Ä") ("Ouml" . "Ö") ("Uuml" . "Ü") ("szlig" . "ß") ("ntilde" . "ñ")
                ("Ntilde" . "Ñ") ("ccedil" . "ç") ("Ccedil" . "Ç") ("aring" . "å") ("Aring" . "Å")
                ("aelig" . "æ") ("AElig" . "Æ") ("oslash" . "ø") ("Oslash" . "Ø") ("atilde" . "ã")
                ("otilde" . "õ") ("oelig" . "œ") ("scaron" . "š") ("Scaron" . "Š") ("zcaron" . "ž")
                ("eth" . "ð") ("thorn" . "þ") ("iexcl" . "¡") ("iquest" . "¿") ("ordf" . "ª")
                ("ordm" . "º") ("macr" . "¯") ("uml" . "¨") ("cedil" . "¸") ("acute" . "´")
                ("circ" . "ˆ") ("tilde" . "˜") ("shy" . "") ("zwj" . "") ("zwnj" . "")
                ("lrm" . "") ("rlm" . "") ("ensp" . " ") ("emsp" . " ") ("thinsp" . " ")
                ("dagger" . "†") ("Dagger" . "‡") ("permil" . "‰") ("loz" . "◊") ("spades" . "♠")
                ("clubs" . "♣") ("hearts" . "♥") ("diams" . "♦") ("check" . "✓") ("star" . "☆")
                ("starf" . "★") ("hyphen" . "‐") ("dash" . "‐") ("excl" . "!") ("num" . "#")
                ("dollar" . "$") ("percnt" . "%") ("lpar" . "(") ("rpar" . ")") ("ast" . "*")
                ("plus" . "+") ("comma" . ",") ("period" . ".") ("sol" . "/") ("colon" . ":")
                ("semi" . ";") ("equals" . "=") ("quest" . "?") ("commat" . "@") ("lsqb" . "[")
                ("rsqb" . "]") ("bsol" . "\\") ("lowbar" . "_") ("grave" . "`") ("lcub" . "{")
                ("rcub" . "}") ("verbar" . "|") ("vert" . "|") ("Hat" . "^") ("NewLine" . "\n")
                ("Tab" . "\t") ("rang" . "⟩") ("lang" . "⟨") ("rceil" . "⌉") ("lceil" . "⌈")
                ("rfloor" . "⌋") ("lfloor" . "⌊") ("sim" . "∼") ("cong" . "≅") ("sub" . "⊂")
                ("sup" . "⊃") ("oplus" . "⊕") ("otimes" . "⊗") ("perp" . "⊥") ("sdot" . "⋅")))
             (h (hash)))
    (if (null? xs) h (loop (cdr xs) (hash-insert h (car (car xs)) (cdr (car xs)))))))

;; Code point → one-char string; junk (controls, surrogates, out of range) → "".
(define (skim--codepoint->string cp)
  (cond ((or (not cp) (< cp 9)) "")
        ((and (>= cp 55296) (<= cp 57343)) "")
        ((>= cp 1114112) "")
        ((= cp 160) skim--nbsp)
        (else (string (integer->char cp)))))

;; Decode HTML character references. Each `&…;` is fenced with S1 and the
;; pieces alternate text / reference, so one native split does the walk.
(define (skim--entities s)
  (if (not (string-contains? s "&"))
      s
      (let* ((fenced (replace-regexp-in-string
                      "&(#[xX][0-9a-fA-F]{1,6}|#[0-9]{1,7}|[A-Za-z][A-Za-z0-9]{1,31});"
                      (string-append skim--S1 "${1}" skim--S1) s))
             (pieces (split-many fenced skim--S1)))
        (let loop ((ps pieces) (i 0) (acc '()))
          (if (null? ps)
              (string-join (reverse acc) "")
              (loop (cdr ps) (+ i 1)
                    (cons (if (even? i)
                              (car ps)
                              (let ((r (car ps)))
                                (cond
                                  ((starts-with? r "#x") (skim--codepoint->string (string->number (substring r 2 (string-length r)) 16)))
                                  ((starts-with? r "#X") (skim--codepoint->string (string->number (substring r 2 (string-length r)) 16)))
                                  ((starts-with? r "#") (skim--codepoint->string (string->number (substring r 1 (string-length r)))))
                                  (else (or (hash-try-get skim--entity-table r)
                                            (string-append "&" r ";"))))))
                          acc)))))))

;; ── URLs ────────────────────────────────────────────────────────────────────
;; (scheme host path query fragment) — scheme lowercased with its colon
;; dropped, path "" when absent, query/fragment keep their leading ?/#.
(define (skim--url-parts url)
  (let ((c (regexp-captures "^([a-zA-Z][a-zA-Z0-9+.-]*):(//([^/?#]*))?([^?#]*)(\\?[^#]*)?(#.*)?$" url)))
    (if (null? c)
        #f
        (list (skim--lower (nth c 1)) (nth c 3) (nth c 4) (nth c 5) (nth c 6)))))

(define (skim--url-scheme url) (let ((p (skim--url-parts url))) (and p (car p))))
(define (skim--url-host url) (let ((p (skim--url-parts url))) (if p (skim--lower (cadr p)) "")))

(define (skim--url-origin url)
  (let ((p (skim--url-parts url)))
    (if p (string-append (car p) "://" (cadr p)) "")))

(define (skim--strip-fragment url)
  (let ((i (skim--char-index url #\#)))
    (if i (substring url 0 i) url)))

(define (skim--url-fragment url)
  (let ((i (skim--char-index url #\#)))
    (if i (substring url (+ i 1) (string-length url)) "")))

(define (skim--char-index s ch)
  (let loop ((cs (string->list s)) (i 0))
    (cond ((null? cs) #f) ((char=? (car cs) ch) i) (else (loop (cdr cs) (+ i 1))))))

;; Resolve "." / ".." segments of an absolute path.
(define (skim--normalize-path path)
  (if (not (or (string-contains? path "/.") (starts-with? path ".")))
      path
      (let* ((segs (split-many path "/"))
             (trailing? (or (ends-with? path "/") (ends-with? path "/.") (ends-with? path "/.."))))
        (let loop ((ss segs) (acc '()))
          (if (null? ss)
              (let ((p (string-join (reverse acc) "/")))
                (string-append (if (starts-with? p "/") "" "/") p
                               (if (and trailing? (not (ends-with? p "/"))) "/" "")))
              (let ((s (car ss)))
                (cond ((equal? s ".") (loop (cdr ss) acc))
                      ((equal? s "..")
                       (loop (cdr ss) (if (and (pair? acc) (not (equal? (car acc) ""))) (cdr acc) acc)))
                      (else (loop (cdr ss) (cons s acc))))))))))

;; Resolve `href` against the absolute URL `base` (RFC 3986, the common cases).
(define (skim--resolve base href)
  (let ((href (skim--trim (replace-regexp-in-string "[\t\r\n]" "" href))))
    (cond
      ((equal? href "") (skim--strip-fragment base))
      ((skim--has-scheme? href) href)
      (else
       (let ((p (skim--url-parts base)))
         (if (not p)
             href
             (let ((scheme (nth p 0)) (host (nth p 1)) (path (nth p 2)) (query (nth p 3)))
               (cond
                 ((starts-with? href "//") (string-append scheme ":" href))
                 ((starts-with? href "#") (string-append (skim--strip-fragment base) href))
                 ((starts-with? href "?") (string-append scheme "://" host (if (equal? path "") "/" path) href))
                 ((starts-with? href "/")
                  (string-append scheme "://" host (skim--resolve-path-query href)))
                 (else
                  (let* ((dir (let ((c (regexp-captures "^(.*/)" path)))
                                (if (null? c) "/" (nth c 1)))))
                    (string-append scheme "://" host
                                   (skim--resolve-path-query (string-append dir href)))))))))))))

;; Normalize only the path part of "path?query#frag".
(define (skim--resolve-path-query s)
  (let ((c (regexp-captures "^([^?#]*)(.*)$" s)))
    (if (null? c) s (string-append (skim--normalize-path (nth c 1)) (nth c 2)))))

(define (skim--has-scheme? s)
  (not (null? (regexp-captures "^[a-zA-Z][a-zA-Z0-9+.-]*:" s))))

;; Percent-encode for application/x-www-form-urlencoded (space → +).
(define (skim--form-encode s)
  (let loop ((bs (bytes->list (string->bytes s))) (acc '()))
    (if (null? bs)
        (string-join (reverse acc) "")
        (let ((b (car bs)))
          (loop (cdr bs)
                (cons (cond ((or (and (>= b 48) (<= b 57)) (and (>= b 65) (<= b 90))
                                 (and (>= b 97) (<= b 122)) (= b 45) (= b 46) (= b 95) (= b 42))
                             (string (integer->char b)))
                            ((= b 32) "+")
                            (else (string-append "%" (if (< b 16) "0" "")
                                                 (string-upcase (number->string b 16)))))
                      acc))))))

;; ── colors ──────────────────────────────────────────────────────────────────
(define skim--named-colors
  (let loop ((xs '(("black" 0 0 0) ("white" 255 255 255) ("red" 255 0 0) ("green" 0 128 0)
                   ("blue" 0 0 255) ("yellow" 255 255 0) ("orange" 255 165 0) ("purple" 128 0 128)
                   ("gray" 128 128 128) ("grey" 128 128 128) ("silver" 192 192 192)
                   ("maroon" 128 0 0) ("navy" 0 0 128) ("teal" 0 128 128) ("olive" 128 128 0)
                   ("lime" 0 255 0) ("aqua" 0 255 255) ("cyan" 0 255 255) ("fuchsia" 255 0 255)
                   ("magenta" 255 0 255) ("brown" 165 42 42) ("pink" 255 192 203)
                   ("darkred" 139 0 0) ("darkgreen" 0 100 0) ("darkblue" 0 0 139)
                   ("darkgray" 169 169 169) ("darkgrey" 169 169 169) ("lightgray" 211 211 211)
                   ("lightgrey" 211 211 211) ("dimgray" 105 105 105) ("crimson" 220 20 60)
                   ("gold" 255 215 0) ("indigo" 75 0 130) ("violet" 238 130 238)
                   ("steelblue" 70 130 180) ("royalblue" 65 105 225) ("tomato" 255 99 71)
                   ("firebrick" 178 34 34) ("forestgreen" 34 139 34) ("seagreen" 46 139 87)
                   ("slategray" 112 128 144) ("darkorange" 255 140 0) ("goldenrod" 218 165 32)
                   ("darkslategray" 47 79 79) ("midnightblue" 25 25 112) ("cornflowerblue" 100 149 237)
                   ("dodgerblue" 30 144 255) ("deepskyblue" 0 191 255) ("lightblue" 173 216 230)
                   ("whitesmoke" 245 245 245) ("gainsboro" 220 220 220) ("beige" 245 245 220)
                   ("ivory" 255 255 240) ("linen" 250 240 230) ("lightyellow" 255 255 224)
                   ("khaki" 240 230 140) ("salmon" 250 128 114) ("coral" 255 127 80)
                   ("chocolate" 210 105 30) ("sienna" 160 82 45) ("tan" 210 180 140)
                   ("orchid" 218 112 214) ("plum" 221 160 221) ("darkviolet" 148 0 211)
                   ("limegreen" 50 205 50) ("darkcyan" 0 139 139) ("cadetblue" 95 158 160)))
             (h (hash)))
    (if (null? xs) h (loop (cdr xs) (hash-insert h (car (car xs)) (cdr (car xs)))))))

(define (skim--hex2 s) (string->number s 16))

;; CSS color → (r g b) ints, or #f (transparent, currentColor, var(), junk).
(define (skim--parse-color v)
  (let* ((v (skim--lower (skim--trim v)))
         (c (regexp-captures "^#([0-9a-f])([0-9a-f])([0-9a-f])([0-9a-f])?$" v)))
    (if (null? c)
        (skim--parse-color-2 v)
        (list (* 17 (skim--hex2 (nth c 1))) (* 17 (skim--hex2 (nth c 2))) (* 17 (skim--hex2 (nth c 3)))))))

(define (skim--parse-color-2 v)
  (let ((c6 (regexp-captures "^#([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})?$" v)))
    (if (not (null? c6))
        (if (and (> (length c6) 4) (not (equal? (nth c6 4) "")) (< (skim--hex2 (nth c6 4)) 64))
            #f
            (list (skim--hex2 (nth c6 1)) (skim--hex2 (nth c6 2)) (skim--hex2 (nth c6 3))))
        (let ((rgb (regexp-captures "^rgba?\\(\\s*([0-9.]+)%?[\\s,]+([0-9.]+)%?[\\s,]+([0-9.]+)%?\\s*(?:[,/]\\s*([0-9.]+)(%?))?\\s*\\)$" v)))
          (if (not (null? rgb))
              (let ((a (and (> (length rgb) 4) (skim--num (nth rgb 4)))))
                (if (and a (< (if (equal? (nth rgb 5) "%") (/ a 100) a) 0.25))
                    #f
                    (list (skim--clamp255 (skim--num (nth rgb 1)))
                          (skim--clamp255 (skim--num (nth rgb 2)))
                          (skim--clamp255 (skim--num (nth rgb 3))))))
              (hash-try-get skim--named-colors v))))))

(define (skim--clamp255 x) (if x (max 0 (min 255 (skim--int x))) 0))

(define (skim--color->hex rgb)
  (string-append "#" (string-join (map (lambda (x) (let ((h (number->string x 16)))
                                                      (if (< x 16) (string-append "0" h) h)))
                                       rgb) "")))

;; WCAG relative luminance and contrast ratio.
(define (skim--lum rgb)
  (let ((ch (lambda (x)
              (let ((c (/ (exact->inexact x) 255.0)))
                (if (<= c 0.03928) (/ c 12.92) (expt (/ (+ c 0.055) 1.055) 2.4))))))
    (+ (* 0.2126 (ch (car rgb))) (* 0.7152 (ch (cadr rgb))) (* 0.0722 (ch (caddr rgb))))))

(define (skim--contrast a b)
  (let ((la (skim--lum a)) (lb (skim--lum b)))
    (/ (+ (max la lb) 0.05) (+ (min la lb) 0.05))))
