(setq w3m-search-default-engine "duckduckgo"
      w3m-search-engine-alist
      '(("debian-pkg"
         "http://packages.debian.org/cgi-bin/search_contents.pl?directories=yes&arch=i386&version=unstable&case=insensitive&word=%s"
         nil)
        ("debian-bts"
         "http://bugs.debian.org/cgi-bin/pkgreport.cgi?archive=yes&pkg=%s" nil)
        ("emacswiki" "http://www.emacswiki.org/cgi-bin/wiki?search=%s" nil)
        ("wikipedia-en"
         "http://en.wikipedia.org/wiki/Special:Search?search=%s" nil)
        ("wikipedia-fr"
         "http://fr.wikipedia.org/wiki/Special:Search?search=%s" utf-8)
        ("duckduckgo" "https://duckduckgo.com/?q=%s" utf-8)
        ("wiki"
         "http://wiki.crans.org/?action=fullsearch&value=%s&titlesearch=Titres"
         utf-8)
        ("wikoeur"
         "http://pimeys.fr/wikoeur/?action=fullsearch&value=%s&titlesearch=Titres"
         utf-8)))

(provide 'w3m-conf)
