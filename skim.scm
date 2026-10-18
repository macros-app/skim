;; skim — a text web browser for the macros editor, in Steel.
;;
;; skim renders real web pages as faithfully as a text browser can: an
;; HTML5-ish parser, a CSS engine (UA sheet + <style> + linked stylesheets +
;; style="" with selectors, specificity, @media, var()), and a block/inline
;; layout engine with wrapped paragraphs, lists, real tables, forms, headings
;; at a larger line scale, and inline images painted by the editor.
;;
;;   M-x skim               open a URL or search (prompt)
;;   M-x skim-browse-url    same, but takes the URL as an argument
;;
;;   in *skim*:
;;     RET / click     follow the link (or edit/press the form field) at point
;;     TAB / S-TAB     next / previous link or field
;;     l / r           back / forward              (evil: H / L)
;;     g               reload                       (evil: g r)
;;     G / o           open a URL or search         (evil: o, O edits the URL)
;;     s               web search
;;     n / p           rel=next / rel=prev page     (evil: ] ] / [ [)
;;     u               up one directory in the URL
;;     w               copy the URL of the link at point (else the page)
;;     &               open the page (or link) in the system browser
;;     d               download the link at point to ~/Downloads
;;     R               reader mode (main content only)
;;     I / C           toggle images / author CSS
;;     v               view source
;;     b / B           bookmark this page / open a bookmark
;;     h               history
;;     ?               menu of every command
;;     q               quit back to where skim was opened from
;;
;; Configuration is through options (settable from init.scm):
;;   "skim-width"            wrap width in columns (default 100)
;;   "skim-home"             page `skim` offers first (default about:blank → prompt)
;;   "skim-search-url"       search prefix (default DuckDuckGo HTML)
;;   "skim-images"           "on"/"off" inline images (default on)
;;   "skim-image-max-width"  px cap for images (default 640)
;;   "skim-external-css"     "on"/"off" fetch <link rel=stylesheet> (default on)
;;   "skim-css"              "on"/"off" honour author CSS at all (default on)
;;   "skim-css-colors"       "on"/"off" honour author colors (default on;
;;                           colors failing contrast vs the editor are dropped)
;;   "skim-palette"          "dark" / "light" (default dark)
;;   "skim-viewport-width"   px width @media queries see (default 1024)
;;   "skim-user-agent"       User-Agent header
;;   "skim-heading-scale"    "on"/"off" larger h1–h3 lines (default on)

;; Steel compiles each loaded file as a unit and a file can only name what
;; earlier files defined, so the pieces load in dependency order:
;;
;;   util       strings, entities, URLs, colours
;;   html       HTML → DOM
;;   selectors  CSS selector parsing and matching
;;   css        stylesheets, @media, the cascade
;;   style      palette, faces, options, run styles
;;   text       lines, paragraphs, word wrapping
;;   replaced   rules, media, images, form controls
;;   flow       the layout walk (blocks, inline, floats, lists)
;;   tables     table layout (reached from flow through skim--table-impl)
;;   state      browser state and settings
;;   net        responses, fetch pools, image cache, downloads
;;   page       loading pages and stylesheets, painting *skim*
;;   commands   links, forms, history, bookmarks, the ? menu
;;   keys       keymaps and command registration
(define skim--dir *package-directory*)
(for-each (lambda (piece) (load (path-join skim--dir (string-append "lib/skim-" piece ".scm"))))
          '("util" "html" "selectors" "css" "style" "text" "replaced" "flow" "tables"
            "state" "net" "page" "commands" "keys"))
