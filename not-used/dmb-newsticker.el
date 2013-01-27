
(setq 
 newsticker-url-list '(("/dev/websphere"  "http://feeds.feedburner.com/dev/websphere")
                       ("Planet JDK" "http://planetjdk.org/feed.rss")
                       ("Discover physics and math" "http://discovermagazine.com/topics/physics-math/rss.xml")
                       ("Scientific American" "http://rss.sciam.com/ScientificAmerican-Global?format=xml")))

(autoload 'newsticker-start "newsticker" "Emacs Newsticker" t)
(autoload 'newsticker-show-news "newsticker" "Emacs Newsticker" t)


