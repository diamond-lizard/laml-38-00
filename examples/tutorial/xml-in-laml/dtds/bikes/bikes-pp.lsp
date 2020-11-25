(
 (element
   "bikes"
   "-"
   "-"
   (element-content (seq zero-or-more (name one "bike")))
   ""
 )

 (element
  "bike"
  "-"
  "-"
  (element-content
   (seq
    one
    (name one "frame")
    (name one-or-more "wheel")
    (name zero-or-more "brake")
    (name zero-or-more "lock")))
  ""
  )

  (element "frame" "-" "-" empty "")

  (element "wheel" "-" "-" empty "")

  (element "brake" "-" "-" empty "")

  (element "lock" "-" "-" empty "")

  (attribute "bikes" ())

  (attribute
     "bike"
     (("kind" ("mountain-bike, racer-bike, tourist-bike, other") "tourist-bike")))

  (attribute "frame" (("frame-number" "CDATA" "#REQUIRED")))

  (attribute
     "wheel"
     (("size" "CDATA" "#REQUIRED") ("tube-kind" "CDATA" "#IMPLIED")))

  (attribute
     "brake"
     (("kind" "CDATA" "#IMPLIED") ("brand" "CDATA" "#IMPLIED")))

  (attribute
     "lock"
     (("brand" "CDATA" "#IMPLIED")
        ("insurance-approved" ("true" "false") "#REQUIRED")))

  (entity "Number" "CDATA")

  (entity "Boolean" "(true | false)"))

