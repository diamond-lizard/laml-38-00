(define multi-author "Kurt Nørmark")
(define multi-affiliation "Department of Computer Science, Aalborg University, Denmark")

(define common-shared-front-matters-attributes
 (list 
  'scheme-prefix "pre-notes.scm"
  'scheme-suffix "post-notes.scm"

  'course-home-url "../../../index.html"   
  'author-home-url "http://www.cs.auc.dk/~normark/"   
  'logo-url "http://www.auc.dk/"  

  'show-and-speak "false" 
  'sound-source "real-audio" 
  'speak-url-prefix "../../speak/"
  'speak-file-prefix "???"

  'make-print-page "true"
 )
)

(define shared-themes-front-matters-attributes
 (append common-shared-front-matters-attributes
  (list
   'author-mode "true"
   'presentation-medium "web"
  )
 )
)
  
  

(define shared-front-matters-attributes
 (append common-shared-front-matters-attributes
  (list

   'theme-view "true"
   'theme-source "none"  ; none/new/overwrite
   'theme-auto-process "true"
 
   'slide-view "true"  
   'annotated-slide-view "true"
   'aggregated-view "true" 
   'primary-view "slide-view"
 
   'note-download-url ""   
 
   'note-contents-description ""    
   'slide-header "normal"  ; minimal/normal/none 
   'trail-of-lecture "false"  
   'language "english"      
 
   'exercise-model "none"   
   'mouse-advancement "double-press" 
   'word-index "true"  
   'css-stylesheet ""
   'news-flash-string  ""  
   'news-flash-level   "2"
   'quiz-support "false"   
   'verbosity-level "1"
  )
 )
)