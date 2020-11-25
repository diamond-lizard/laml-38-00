((element "album" "-" "-" (element-content (seq one (name one "title") (name one "artist") (name optional "recordingdate") (name one "coverart") (seq one-or-more (name one "catalogno")) (name one "personnel") (name one "tracks") (name one "notes"))) "") (element "title" "-" "-" (mixed-content pcdata) "") (element "artist" "-" "-" (mixed-content pcdata) "") (element "recordingdate" "-" "-" empty "") (element "coverart" "-" "-" (element-content (seq optional (name one "location"))) "") (element "location" "-" "-" empty "") (element "catalogno" "-" "-" empty "") (element "personnel" "-" "-" (element-content (seq one-or-more (name one "player"))) "") (element "player" "-" "-" empty "") (element "tracks" "-" "-" (element-content (seq zero-or-more (name one "track"))) "") (element "track" "-" "-" empty "") (element "notes" "-" "-" (mixed-content (choice pcdata "albumref" "trackref")) "") (element "albumref" "-" "-" (mixed-content pcdata) "") (element "trackref" "-" "-" (mixed-content pcdata) "") (attribute "recordingdate" (("date" "CDATA" "#IMPLIED") ("place" "CDATA" "#IMPLIED"))) (attribute "coverart" (("style" "CDATA" "#REQUIRED"))) (attribute "location" (("thumbnail" "CDATA" "#IMPLIED") ("fullsize" "CDATA" "#IMPLIED"))) (attribute "catalogno" (("label" "CDATA" "#REQUIRED") ("number" "CDATA" "#REQUIRED") ("format" ("CD" "LP" "MiniDisc") "#IMPLIED") ("releasedate" "CDATA" "#IMPLIED") ("country" "CDATA" "#IMPLIED"))) (attribute "player" (("name" "CDATA" "#REQUIRED") ("instrument" "CDATA" "#REQUIRED"))) (attribute "track" (("title" "CDATA" "#REQUIRED") ("credit" "CDATA" "#IMPLIED") ("timing" "CDATA" "#IMPLIED"))) (attribute "notes" (("author" "CDATA" "#IMPLIED"))) (attribute "albumref" (("link" "CDATA" "#REQUIRED"))) (attribute "trackref" (("link" "CDATA" "#IMPLIED"))) (attribute "tracks" ()) (attribute "personnel" ()) (attribute "artist" ()) (attribute "title" ()) (attribute "album" ()))