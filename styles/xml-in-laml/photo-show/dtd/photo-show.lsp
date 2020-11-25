((element "photo-show" "-" "-" (element-content (seq one (name one "title-of-show") (name one "upper-caption") (name one "lower-caption") (choice one (name one "photo-list") (name one "photos-in-current-directory")))) "") (element "title-of-show" "-" "-" (mixed-content pcdata) "") (element "upper-caption" "-" "-" (mixed-content pcdata) "") (element "lower-caption" "-" "-" (mixed-content pcdata) "") (element "photo-list" "-" "-" (element-content (seq one (name zero-or-more "photo"))) "") (element "photo" "-" "-" (element-content (seq one (name optional "upper-caption") (name optional "lower-caption"))) "") (element "photos-in-current-directory" "-" "-" empty "") (attribute "photo-show" (("time" "CDATA" "#REQUIRED") ("background-color" "CDATA" "#IMPLIED") ("text-color" "CDATA" "#IMPLIED") ("home-path" "CDATA" "#IMPLIED") ("previous-film" "CDATA" "#IMPLIED") ("next-film" "CDATA" "#IMPLIED") ("before-first" ("last-picture" "previous-film" "none") "last-picture") ("after-last" ("first-picture" "next-film" "stop-page" "none") "first-picture") ("transition" ("blend" "none") "none") ("verbosity-level" "CDATA" "2") ("language" ("danish" "english") "english") ("html-mode" ("raw" "pp") "raw") ("index-linking" ("run" "stop") "stop") ("default-size" ("original" "auto" "explicit") "auto") ("default-orientation" ("portrait" "landscape") "landscape") ("portrait-percentage" "CDATA" "90") ("landscape-percentage" "CDATA" "95") ("photo-displacement-left" "CDATA" "0") ("photo-displacement-right" "CDATA" "0") ("tabular-overview-dimension" "CDATA" "3") ("photo-tabular-window-width" "CDATA" "870") ("photo-tabular-window-height" "CDATA" "740") ("copy-film" ("true" "false") "false") ("copy-destination" "CDATA" "#IMPLIED") ("shave-resize-and-copy" ("true" "false") "false") ("shave-width" "CDATA" "0") ("shave-height" "CDATA" "0") ("resize-width" "CDATA" "0") ("resize-height" "CDATA" "0") ("camera" "CDATA" "Canon-G3") ("image-transformation-software" ("image-magick" "none") "none") ("make-thumbnails-of" ("all" "non-existing" "none") "none") ("time-stamp" ("auto" "from-file-system" "from-jpg-file" "none") "auto") ("time-stamp-presentation" ("date" "weekday-date" "time" "date-time" "weekday-date-time" "none") "date-time") ("rotate" "CDATA" "#IMPLIED"))) (attribute "upper-caption" (("size" "CDATA" "#IMPLIED"))) (attribute "lower-caption" (("size" "CDATA" "#IMPLIED"))) (attribute "photo" (("file" "CDATA" "#REQUIRED") ("path" "CDATA" "") ("time" "CDATA" "#IMPLIED") ("transition" ("blend" "none") "none") ("size" ("original" "auto" "explicit") "auto") ("orientation" ("portrait" "landscape") "landscape") ("width" "CDATA" "#IMPLIED") ("height" "CDATA" "#IMPLIED"))) (attribute "photos-in-current-directory" ()) (attribute "photo-list" ()) (attribute "title-of-show" ()) (entity "Number" "CDATA") (entity "Color" "CDATA") (entity "Symbol" "CDATA") (entity "URI" "CDATA") (entity "File-path" "CDATA"))