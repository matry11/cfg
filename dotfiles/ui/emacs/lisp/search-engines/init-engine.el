;;; Engine mode

(engine-mode)
(require 'eww)

(defun ambrevar/engine-eww-function (url &optional _)
  (interactive)
  (eww url))

(defengine arch-wiki
  "https://wiki.archlinux.org/index.php?title=Special%%3ASearch&search=%s&go=Go"
  :keybinding "archw"
  :browser 'ambrevar/engine-eww-function)

(defengine cookbook
  "https://en.wikibooks.org/wiki/Special:Search?search=%s&prefix=Cookbook%3A&fulltext=Search+Cookbook&fulltext=Search&ns0=1&ns4=1&ns102=1&ns110=1&ns112=1"
  :keybinding "cb")

(defengine ctan
  "https://www.ctan.org/search?phrase=%s"
  :keybinding "ctan")

(defengine devdocs
  "https://devdocs.io/#q=%s"
  :keybinding "dd")

(defengine github
  "https://github.com/search?ref=simplesearch&q=%s"
  :keybinding "gh")

(defengine musicbrainz-artists
  "https://musicbrainz.org/search?query=%s&type=artist&method=indexed"
  :keybinding "mba")

(defengine musicbrainz-releases
  "https://musicbrainz.org/search?query=%s&type=release&method=indexed"
  :keybinding "mbr")

(defengine nyxt
  "https://github.com/atlas-engineer/nyxt/issues?q=%s"
  :keybinding "nyxt")

(defengine openstreetmap
  "https://www.openstreetmap.org/search?query=%s"
  :keybinding "osm")

(defengine stackoverflow
  "https://stackoverflow.com/search?q=%s"
  :keybinding "so")

(defengine wikipedia
  "https://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
  :keybinding "wp"
  :docstring "Search Wikipedia!")

(defengine wikibooks
  "https://en.wikibooks.org/wiki/Special:Search?search=%s"
  :keybinding "wb")

(defengine wine-appdb
  "https://www.winehq.org/search/?q=%s"
  :keybinding "wine")

(load (expand-file-name "bookmarks/engines.el" (getenv "PERSONAL")) t)

(provide 'init-engine)
