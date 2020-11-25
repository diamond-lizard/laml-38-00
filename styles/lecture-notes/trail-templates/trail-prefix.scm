(load (string-append laml-dir "laml.scm"))       
(laml-style "xml-in-laml/lecture-notes-trail/lecture-note-trail")

(leno-trail-front-matters
  (front-title       "TRAIL-TITLE")           
  (front-author      "TRAIL-AUTHOR")           
  (front-affiliation "TRAIL-AFFILIATION")  
  (front-abstract    "TRAIL-ABSTRACT")

  'trail-id          "TRAIL-ID"
  'scheme-prefix     "TRAIL-PREFIX"
  'scheme-suffix     "TRAIL-SUFFIX"
  'course-home-url   "HOME-URL"
  'language          "english"
)

(begin-trail)

