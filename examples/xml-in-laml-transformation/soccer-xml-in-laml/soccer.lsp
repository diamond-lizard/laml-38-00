((element "results" "-" "-" (element-content (seq zero-or-more (name one "match"))) "") (element "match" "-" "-" (element-content (seq one (name one "date") (name one "team") (name one "team"))) "") (element "date" "-" "-" (mixed-content pcdata) "") (element "team" "-" "-" (mixed-content pcdata) "") (attribute "results" (("group" "CDATA" "#REQUIRED"))) (attribute "team" (("score" "CDATA" "#REQUIRED"))) (attribute "date" ()) (attribute "match" ()) (entity "Number" "CDATA"))