# skim

A text web browser for the [macros](https://github.com/macros-app/macros)
editor, written entirely in Steel. Think Emacs's `eww`, but it tries much harder
to render pages the way a real browser lays them out.

- **HTML5-ish parser.** Implied end tags (`<p>`, `<li>`, table parts, …),
  character references, and `<script>`/`<svg>` stripped. It's built on native
  regex passes and one `split-many`, so a 400 KB Wikipedia article parses in
  about 100 ms.
- **A CSS engine.** The cascade covers a user-agent sheet, `<style>`, linked
  stylesheets (fetched in the background, then the page re-renders) and
  `style=""`. It handles type/class/id/attribute selectors, descendant, `>`,
  `+` and `~` combinators, `:not()`/`:is()`/`:where()`, `:first-child`, and
  `::before`/`::after` content, with specificity and `!important`. `@media` is
  evaluated against a nominal viewport, and custom properties work with
  `var()`.
- **Layout in character cells.** Block margins become blank lines and left
  margins/padding become indentation. `max-width` + `margin: auto` gives a
  centred column, and short `float: left` boxes (vote counts, dates) sit beside
  the content they float next to. Flex rows of short items stay on one line, and
  visually hidden content (`display:none`, `.sr-only` clip patterns, off-screen
  boxes) is skipped. Content that starts invisible and waits for JavaScript to
  fade it in (an opacity/visibility transition on an in-flow element with real
  text) is shown, since skim never runs the script. Images skim doesn't draw
  still take up their declared width (spacer GIFs indent HN comment threads).
- **Text.** Paragraphs are word-wrapped and honour `white-space`. `text-align`,
  `text-transform`, bold, italic (emphasis color), underline, strike, inline
  code, and author colours are applied when they have enough contrast against
  your editor theme.
- **Lists, tables, headings.** Lists get hanging markers (disc/circle/square,
  decimal/alpha/roman, `start`, `value`, `reversed`), and blockquotes get a bar.
  Tables use real column layout with colspan and rowspan: box-drawn when they
  hold data, borderless when they're page layout. `h1`–`h3` render at a larger
  line scale.
- **Fast enough on heavy sites.** Stylesheets (up to 120 per page) and images
  are fetched six and four at a time. Custom properties are kept as prebuilt
  per-rule layers, so design-system pages with thousands of `--vars` stay fast.
- **Images inline.** PNG/JPEG/GIF/WebP are downloaded in the background,
  cached on disk, and painted under their line. `data:` URIs, `srcset`, and
  lazy `data-src` are supported.
- **Forms.** Text fields, textareas, checkboxes, radios, selects and buttons,
  submitted as GET or POST. Pressing RET on a search box asks for the text and
  submits.
- **Browsing.** History with back/forward (cursor restored), `#fragment` jumps,
  `rel=next/prev`, reader mode, view source, bookmarks, downloads, and opening
  the page in the system browser.

No JavaScript, by design.

## Install

```scheme
(package-install-local "~/repos/skim")   ; or package-install with a git URL
(evil-leader-set "m e" 'skim)             ; optional
```

## Use

`M-x skim` asks for a URL or search terms (anything that isn't URL-shaped
goes to the search engine). `M-x skim-browse-url` takes the URL as an argument.
Press `?` in `*skim*` for a menu of every command.

| key             | evil            | action                                        |
|-----------------|-----------------|-----------------------------------------------|
| `RET` / click   | `RET`           | follow link / edit or press the form field    |
| `TAB` / `S-TAB` | same            | next / previous link or field                 |
| `l` / `r`       | `H` / `L`       | back / forward                                |
| `G` / `o`       | `o`             | open a URL or search                          |
| `e`             | `O`             | edit the current URL                          |
| `g`             | `r`, `g r`      | reload                                        |
| `s`             |                 | web search                                    |
| `n` / `p`       | `] ]` / `[ [`   | `rel=next` / `rel=prev` page                  |
| `u`             | `g u`           | up one directory                              |
| `w`             | `y y`           | copy the link at point (else the page URL)    |
| `&`             |                 | open in the system browser                    |
| `d`             |                 | download the link at point to ~/Downloads     |
| `R`             |                 | reader mode (main content only)               |
| `I` / `C`       |                 | toggle images / author CSS                    |
| `v`             |                 | view source                                   |
| `b` / `B`       | `m` / `B`       | bookmark this page / open a bookmark          |
| `h`             |                 | history                                       |
| `q`             | `q`             | quit back to where skim was opened            |

Evil users also get `j`/`k`/`w`/`b`/`gg`/`G`/`C-d`/`C-u` and `/` `n` `N` search.

## Options

All are strings set with `set-option` (from an init.scm `'config` thunk).
Booleans take `"on"` / `"off"`.

| option                 | default                            | meaning                              |
|------------------------|------------------------------------|--------------------------------------|
| `skim-width`           | `100`                              | wrap width in columns                |
| `skim-search-url`      | `https://html.duckduckgo.com/html/?q=` | search prefix                    |
| `skim-home`            | (empty)                            | text the `skim` prompt starts with   |
| `skim-images`          | `on`                               | inline images                        |
| `skim-image-max-width` | `640`                              | image width cap (px)                 |
| `skim-max-images`      | `60`                               | images per page                      |
| `skim-css`             | `on`                               | honour author CSS at all             |
| `skim-external-css`    | `on`                               | fetch `<link rel=stylesheet>`        |
| `skim-css-colors`      | `on`                               | author colours (low contrast dropped) |
| `skim-palette`         | `dark`                             | `dark` / `light` to match your theme |
| `skim-viewport-width`  | `1024`                             | px width `@media` queries see        |
| `skim-heading-scale`   | `on`                               | larger h1–h3 lines                   |
| `skim-spacing-floor`   | `on`                               | keep a blank line around paragraphs even under CSS resets |
| `skim-user-agent`      | Firefox-like                       | User-Agent header                    |

Bookmarks live in `<config>/skim/bookmarks`; downloaded images in
`<config>/skim/cache/`.

## Security

skim treats every page as untrusted and runs no page code (no JavaScript, no
`eval`, no processes). On top of that:

- **Local files:** a web page can't read or open `file:` URLs — not as a
  stylesheet, a link, a form target or a redirect. Only a local page (or a
  URL you type, a bookmark, history) leads to local files.
- **Downloads** are saved under ~/Downloads with a sanitized name (no `../`,
  path separators, control or bidi characters, or leading dots), and only
  after you confirm.
- **Passwords** in forms are read masked and never enter minibuffer history
  (which savehist writes to disk). Other form fields use the normal prompt,
  so their text is kept in history like anything else you type there.
- **External opening** (`&`): `http`, `https` and `mailto` go to the system
  directly; any other scheme asks first.
- **Referer:** subresource requests send the full page URL only within the
  same origin, just the origin across sites, and nothing from https to http.
- **Displayed URLs** have bidi-override and control characters stripped.
- **Limits** keep a hostile page from freezing the editor: element nesting is
  capped at 128, HTML at 4 MB, stylesheets at 2 MB each / 40 000 rules, tables
  past 4 000 cells lay out as plain blocks, images over 15 MB aren't shown.

## Layout of the code

| file                    | what                                                       |
|-------------------------|------------------------------------------------------------|
| `skim.scm`              | entry point: loads the pieces below in dependency order    |
| `lib/skim-util.scm`     | strings, entities, URL resolution, colours                 |
| `lib/skim-html.scm`     | tokenizer + tree builder → DOM                             |
| `lib/skim-selectors.scm`| CSS selector parsing, indexing and matching                |
| `lib/skim-css.scm`      | stylesheets, `@media`, UA sheet, the cascade               |
| `lib/skim-style.scm`    | palette, faces, options, text run styles                   |
| `lib/skim-text.scm`     | lines, paragraphs, word wrapping, per-render caches        |
| `lib/skim-replaced.scm` | rules, embedded media, images, form controls               |
| `lib/skim-flow.scm`     | the layout walk: blocks, inline, floats, flex rows, lists  |
| `lib/skim-tables.scm`   | table layout                                               |
| `lib/skim-state.scm`    | browser state and settings                                 |
| `lib/skim-net.scm`      | responses, parallel fetch pools, image cache, downloads    |
| `lib/skim-page.scm`     | loading pages/stylesheets/images, painting `*skim*`        |
| `lib/skim-commands.scm` | links, forms, history, bookmarks, the `?` menu             |
| `lib/skim-keys.scm`     | keymaps and command registration                           |

Two Steel performance notes for anyone hacking on it. First, never grow an
immutable hash that something else (a global, a pair) still references:
every insert then copies the whole map. The per-render caches live in mutable
slots on the elements instead. Second, `load` compiles a file before running
it, so a file can only call functions from files loaded before it (the one
cycle, flow ⇄ tables, goes through the `skim--table-impl` box).

## License

MIT
