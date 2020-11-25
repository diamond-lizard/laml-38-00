(((kind "manual-page") (title "elucidator-verbose-mode") (form elucidator-verbose-mode) (description "If #t a number of messages are written on the output when processing is done. 
If #f, nothing is written. 
")) ((kind "manual-section") (section-title "Directory setup") (section-body "  
A number of variables and functions which gives information about relevant directories. ")) ((kind "manual-page") (title "software-base-directory") (form software-base-directory) (description "The directory in which all Scheme programs and libraries are located 
")) ((kind "manual-page") (title "scheme-library") (form scheme-library) (description "The scheme library relative to software-base-library 
")) ((kind "manual-page") (title "the-library") (form the-library) (description "The directory in which the libraries are located 
")) ((kind "manual-page") (title "software-directory") (form software-directory) (description "The directory in which the elucidator is located 
")) ((kind "manual-page") (title "source-directory") (form source-directory) (description "The directory in which the documentation source is locacted. 
Must be redefined. Ends with a slash. 
")) ((kind "manual-page") (title "html-directory") (form (html-directory)) (description "The directory in which the generated html files are located. Depends on source-directory 
")) ((kind "manual-section") (section-title "Controlling the amount of processing") (section-body "  
There are a number of variables which control the amount of processing. 
The user of the eludicator does not set these directly via set!. Rather 
he or she uses a function interface, which in turn manipulates the variables. ")) ((kind "manual-page") (title "make-all-indexes") (form (make-all-indexes)) (description "Make both duplicate, cross-reference and defining-name indexes 
")) ((kind "manual-page") (title "make-no-indexes") (form (make-no-indexes)) (description "Make neither duplicate, cross-reference or defining-name indexes 
")) ((kind "manual-page") (title "process-only") (form (process-only . source-keys)) (description "Only process the sources whose keys are given in the parameter. 
If no parameteres are given, process no sources 
If this form does not appear, process all sources. 
")) ((kind "manual-page") (title "minimum-processing") (form (minimum-processing)) (description "Set variables such that minimum processing is called for 
")) ((kind "manual-page") (title "maximum-processing") (form (maximum-processing)) (description "Set variables such that maximum processing is called for 
")) ((kind "manual-page") (title "set-source-directory") (form (set-source-directory dir)) (description "Defines the source directory to be dir. The source directory is the directory which 
contains the documentation laml file, and the path typically ends in doc. Ends in a slash. 
")) ((kind "manual-page") (title "set-documentation-name") (form (set-documentation-name name)) (description "Define the name of the documentation. Per convention, this is the same 
as the file name of the laml file, without extension. 
")) ((kind "manual-page") (title "documentation-intro") (form (documentation-intro title author affiliation-list abstract)) (description "Define the title, affiliation, author, affiliation-list, and the abstract 
")) ((kind "manual-page") (title "end-file-empty-lines") (form end-file-empty-lines) (description "The number of empty lines in the bottom of an html file, 
in order to allow navigation to bottom stuf 
")) ((kind "manual-page") (title "browser-pixel-width") (form browser-pixel-width) (description "The width (in pixels) of the browser 
")) ((kind "manual-page") (title "control-frame-pixel-height") (form control-frame-pixel-height) (description "The height of the top control frame in pixels 
")) ((kind "manual-section") (section-title "Top level functions ") (section-body "")) ((kind "manual-page") (title "program-source") (form (program-source . elements)) (description "Define a documentation source in terms of a number of elements.  
")) ((kind "manual-page") (title "documentation-section") (form (documentation-section . elements)) (description "Define a documentation-section.  
Internally, this function collect information about a documentation section 
")) ((kind "manual-page") (title "documentation-entry") (form (documentation-entry . elements)) (description "Define a documentation entry. 
Internally, this function collects information about a documentation entry. 
")) ((kind "manual-page") (title "begin-documentation") (form (begin-documentation)) (description "Begin the documentation part. This ends the preamble section. 
")) ((kind "manual-page") (title "end-documentation") (form (end-documentation)) (description "End of documentation part. 
Makes all the html stuff. Until now we have collected stuff. Here we generate html files 
based on the collected stuff. 
")) ((kind "manual-section") (section-title "Scheme source file reading") (section-body "  ")) ((kind "manual-page") (title "read-source") (form (read-source file)) (description "Read the file (a lisp source file) and return a list of the lisp expressions at the source file 
")) ((kind "manual-section") (section-title "Extraction of top level defined names from parsed Scheme expressions") (section-body "  ")) ((kind "manual-page") (title "defined-names") (form (defined-names source-list)) (description "Return the list of top-level defined names in the source list 
Source list may be as returned by read-source. 
")) ((kind "manual-section") (section-title "Scheme dependent elucidator") (section-body " ")) ((kind "manual-page") (title "elucidate-program-source") (form (elucidate-program-source source-path destination-path source-list defined-names documented-names size source-key)) (description "Decorate the Scheme source-file with anchors and links. 
Source-path is the name of the file with the Scheme source text (full path and extension). 
Destination-path is the name of the html file with where the decorated Scheme source is to be written (full path and extension). 
Source-list is the list of, read Scheme expressions on source-file. 
Defined-names is a list of name-definitions to which we link applied names. 
A name-definition is a list of the form (name . source-key), where source-key identifies the 
source file, in which name is a defining name occurence 
Documented names is a list of name descriptors, which are documented in the elucidated program. 
In this context, a name descriptor is a pair of the form (documented-name documentation-id). 
documented-name is a program name which occurs (in curly brackets) in the documentation. 
documentation-id is the id of the subsection, in which the name occurs. 
")) ((kind "manual-page") (title "linking-from") (form (linking-from word doc-id)) (description "This function is called during the traversal of a documentation body. 
It returns the a-tag'ed and fonted link word. 
As a side-effect, it collects the documented names in the list documented-name-occurences. 
")) ((kind "manual-page") (title "present-defined-name-index") (form (present-defined-name-index)) (description "Index support: total index of all defining name occurences. 
")) ((kind "manual-page") (title "applied-names-multiple-sources") (form (applied-names-multiple-sources source-list-list)) (description "Index support: cross references involving both applied and defining name occurences 
")))