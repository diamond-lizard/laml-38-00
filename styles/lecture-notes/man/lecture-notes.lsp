(((kind "manual-section") (section-title "Overall organization") (section-body "We start with a description of the overall organization of a set of lecture notes. <p>

A set of lecture notes, say  <em>n</em>  chapters, reside on  <em>n</em>  laml files in a single directory. An laml file
has extension 'laml'. <p> There is a single file, which provides common description of the set of
chapters in a lecture note document. The name of this file is typicaly <b>notes.scm</b>, and it should reside
in the same directory as all the laml files. The notes.scm file is loaded before the LENO software.
It is often convenient, in addition to notes.scm, to load a file after the LENO software. Such a file, which may
be called <b>post-notes.scm</b>, can be used to redefine LENO default values.
It is intended that the notes.scm and post-notes.scm files contain information
which is common to a number of lecture notes file (information which we will avoid being replicated in
a number of lecture note files).
Notes.scm contain a list, <kbd>lecture-sections</kbd>, which describes the identity and the titles of all chapters in 
the set of lectures notes. We refer to an annotated example of a notes.scm files later in this manual.
Notice that notes.scm must be loaded explicitly by each laml lecture note file, just before 
the style clause (which loads the LENO software). If a post-notes file is used, it must be loaded explicitly just after the style clause.<p>

Here follows the subdirectory structure of the directory with LENO lecture notes: 
<pre>
notes
  html
    graphics
      small
    images
    javascript
    applets
    external-html
  graphics
    small
  images
  includes
  internal
  latex
</pre>
All of the names above are directory names. <p>
The notes directory contains the laml files (one laml file per chapter in the notes), notes.scm, and post-notes.scm. <p>

The graphics directory in notes contains the sources of the user defined graphics used in the notes. The small subdirectory
contains manually reduced copies. If the variable copy-note-graphics? is #t, the graphics file from notes/graphics are copied
to the html/graphics. If this variable is #f (false) you are supposed to put the graphics file
into the html/graphics directory yourselves. Similarly, the notes/graphics/small files are copied to html/graphics/small. <p>

The html directory contais all the generated html files, and as can be seen, it contains
a number of sub-directories. This organization is handy, because you can pack the html directory
in order to obtain a total, complete and self contained set of information at HTML level. In other words, the contents
of the html directory is enough to use the material in an Internet browser. The javascript directory contains
a tiny amount of javascript stuff used by Lene. When LAML processing a note file, this directory is populated automatically.<p>

It is possible to organize that the html directory is placed outside the notes directory. 
In order to do that you should redefine the function relative-source-html-destination-path-fragment, which defines
the path 'difference' between the note source directory and the HTML destination directory. In the default case,
this function just returns \"html/\".
<p>

The notes/graphics directory contains gif files for all the figures of the notes. The author of the notes is responsible
for the population of this directory, as well as the subdirectory called small.
If f.gif is a file in html/graphics, f.fig must in addition be found in html/graphics/small.
The idea is to reduce the figure in f, and to place the reduced figure in the small directory.
This turns out to be necessary, because a browser makes very rough and poor reductions of pictures.
The variable copy-note-graphics? controls the copying of files from notes/graphics to html/graphics.

<p>

The images directory contains note-specific icons that are used throughout the LENO systems.
(The LENO software support its own images, which are organized in the LAML directories).
When we run LENO (thus generating the HTML version of the 
slides etc) the relevant image icons are copied from the notes/images directoy to the html/images directory.
Therefore we do not need to copy the images into this directory on beforehand. <p>

In the applets directory you should place java class files with applets.<p>

The directory external-html is intended to contain html files, which are considered an integrated
part of a set of lecture notes, but being outside the LAML domain. In other words, here you can put
html files, which somehow are relevant for the lecture notes.<p>

The internal directory contains helping files in Lisp format with extensions aux, crs, exc, and idx. 
Most users of LENO do not need to care about these files. They are generated when a lecture on an LAML file
is processsed. The aux file contains the name of the lecture together with an 'record' for each page
in the lecture. The crs file contains information about all cross references found in a lecture.
The exc file contains information about all the exercises of a lecture. And finally, an idx file
contains all contributions to the index file from a given section.   <p>

The includes directory can be used to material which is included
(copied into) the notes. In other words, the material in the include directory
is source material, which is addressed from the LAML note files. Such included material can also come from other
places in your file system.<p>

The latex directory is intended to contain a latex version of the lecture notes, generated by
means of the style \"lecture-notes/lecture-notes-latex\".<p>

")) ((kind "manual-section") (section-title "Preamble stuff") (section-body "Here we describe the various declarations which are possible to include before the (begin-notes) clause.
Many of the declarations are carried out as (re)definition of Scheme variables. <p>

The notes.scm files, introduced above, is an integral part of the preamble stuff. Notice that the contents
of the notes.scm can be included inline in the preamble. This is, however, only recommended for single chapter
lecture notes. For all the clauses of the preamble we will state whether to place the clause in the notes.scm,
or in the laml chapter file. Most useful, perhaps, we have an example template of a lecture note laml file,
where you can see a typical preamble.

")) ((kind "manual-page") (title "style") (form (style which-style)) (description "States the style of this LAML document. According to the organization 
               of the LAML software, which-style should be the string \"lecture-notes/lecture-notes\".
               (meaning the lecture-notes.scm file in the lecture-note directory in the styles directory). 
               This causes loading of the LENO Scheme software.")) ((kind "manual-page") (title "include-front-index") (form (include-front-index)) (description "Arrange that an index of all chapters are generated. This is the absolute top-level index in a set LENO lecture notes.
               Include-front-index is the default in case neither include-front-index nor exclude-front-index is called ") (cross-references (internal-references "Related clauses" "exclude-front-index")) (misc "Internally, sets the boolean variable front-index?")) ((kind "manual-page") (title "exclude-front-index") (form (exclude-front-index)) (description "Arrange that no index of chapters is generated. This is moste often used in case there is only one chapter.
               Include-front-index is the default in case neither include-front-index nor exclude-front-index is called.") (cross-references (internal-references "Related clauses" "include-front-index")) (misc "Internally, sets the boolean variable front-index?")) ((kind "manual-page") (title "include-word-index") (form (include-word-index)) (description "Arrange that an index of distinquished keywords will be generated with links to the slides, where the words occur.
               More detailed LENO makes it possible to put words into the index explictly via the form (word-index ...).
               Concept names and slide titles also go the index") (cross-references (internal-references "Related clauses" "exclude-word-index" "index-words")) (misc "Internally, sets the boolean variable word-index?")) ((kind "manual-page") (title "exclude-word-index") (form (exclude-word-index)) (description "Arrange that no index of keywords will be generated. This may be useful during the development in order to save
               a little processing time") (cross-references (internal-references "Related clauses" "include-word-index" "index-words")) (misc "Internally, sets the boolean variable word-index?")) ((kind "manual-page") (title "exercise-model") (form (exercise-model model)) (description "Set the exercise model to model. This controls the indexing and support of exercises in a particular lecture
               in a set of notes. ") (parameters (parameter "model" "Which exercise model to use. The value must be a symbol. <p> none: No exercises at all in this chapter.
make-indexes: Make exercise indexes in this chapter. <p>
make-indexes-and-solutions-per-lecture: Make exercise indexes and lecture-specific solution indexes. <p>
make-indexes-and-solutions-lecture-wide: Make exercise indexes, lecture-specific solution indexes, and a global solution index. <p>
synchronous-exercises: Make exercise indexes, solution indexes at a/after a certain point in time,
plus framed exercises with room for the exercise manager. <p>
asynchronous-exercises: Make exercise indexes, NO solution indexes all,
plus framed exercises with room for the asynchronous distance education tool.")) (cross-references (internal-references "See also" "exercise")) (misc "This procedure assigns a number of variables, which control the various aspects of exercise handling in LENO. These
        variables are exercise-indes?, exercise-manager-support?, distance-education-support?, reveal-exercise-solutions?,
        reveal-all-solutions?. Use exercise-model after the style clause but before (begin-notes).")) ((kind "manual-page") (title "set-lecture-name") (form (set-lecture-name name)) (description "Define the internal name of a chapter of the lecture notes (the chapter on the current file).
               This name is used to identify a chapter in relation to other chapters. The name 
               is part of thefile names, where the slides will be represented in HTML. 
               This name should be the same as the name of the laml file, disregarding file extension.
               In some Scheme systems (SCM) it is possible to use the name of the file instad of
               using the set-lecture-name clause. However, we recomment the use of set-lecture-name
               even if it is a bit redundant.") (parameters (parameter "name" "a string defining the name intenal of the lecture notes.")) (pre-condition "Must be placed before the begin-notes clause") (misc "Internally, sets the variable lecture-id")) ((kind "manual-page") (title "set-lecture-number") (form (set-lecture-number n)) (description "Defines the chapter number of the current chapter. As the default, chapter numbers are not used. Only
               if the lecture number is set to an integer, the chapter will be numbered.") (parameters (parameter "n" "the chapter number")) (misc "Internally, sets the variable lecture-number. Lecture-number is a boolean variable, the initial value of which is #f (false)")) ((kind "manual-page") (title "note-abtract") (form note-abtract) (description "A variable the value of which is the overall abstract of the lecture notes (top level, covering all chapters).
               You must define this variable, or the sytem will break.
               The value must be defined before the style clause. A good place to define note-abstract is the first
               item in the notes.scm file. In case you do not want an abstract, define note-abstract to be the empty string") (misc "Use the Scheme define form to define the value of the variable: (define note-abstract ...) ")) ((kind "manual-page") (title "note-source-directory") (form note-source-directory) (description "A mandatory variable the value of which must be a string holding the full path of the directory,
               in which the LAML note files are located. Again, this may seem a bit redundant, but it
               is not possible in all Scheme systems to extract this information automatically. This 
               variable must be define before the style clause, typically in notes.scm. Ends in a '/'.") (misc "Use the Scheme define form to define the value of the variable: (define note-source-directory ...) ")) ((kind "manual-page") (title "note-specific-image-files") (form note-specific-image-files) (description "This optional variable is a list of image file names (without path, with extension such a gif) used specifically in these notes.
               The images must reside in the images directory of note-source-directory. This list of files is copied automatically
               to the html directory during processing together with a set of general images files, defined together with the LENO software.
               In simple notes, just define this variable to the empty list: '()") (misc "Use a define form to set this variable. Define it post-notes.scm if you need it. If not, the default value is the empty list.")) ((kind "manual-page") (title "notes-title") (form notes-title) (description "A variable which holds the title of the lecture notes (a string). You must define this variable
               before the style clause.") (misc "Use the Scheme define form to define the value of the variable: (define notes-title ...) ")) ((kind "manual-page") (title "lecture-sections") (form lecture-sections) (description "A variable which keep track of the names and titles of all lecture chapters in these lecture notes.
               As such, this variable holds very important bookkeeping information. The value of the variable 
               must be a list of list. In other words, it must be a list of lecture-chapter descriptions.
               A lecture chapter description is, in turn, a list of two strings: a lecture-name and a lecture-title.
               This variable must always be defined. In case there is only one chapter in the notes, define
               this variable to be a list of one lecture-chapter description.
             ") (misc "Use the Scheme define form to define the value of the variable: (define lecture-sections ...) ")) ((kind "manual-page") (title "current-lecture") (form current-lecture) (description "An integer variable defining the chapter number of the last lecture to be generated.
               The number is relative to the lecture-sections list (see above). If current-lecture is 1,
               only the first lecture-chapter is taken into consideration.
               This variable is useful if the collection consist of, say. 10 chapters, but we currently
               working at chapter 3. In that case, the value of current-lecture is 3. Lecuture 4 - 10
               are not processed") (misc "Use the Scheme define form to define the value of the variable: (define current-lecture ...) ")) ((kind "manual-page") (title "course-home-url") (form course-home-url) (description "A variable the value of which must be the URL of the course home page (a string). You must define
               this variable before the style clause") (misc "Use the Scheme define form to define the value of the variable: (define course-home-url ...) ")) ((kind "manual-page") (title "author-home-url") (form author-home-url) (description "A variable the value of which must the URL of the author home page. This variable must be defined
               before the style clause") (misc "Use the Scheme define form to define the value of the variable: (define author-home-url ...) ")) ((kind "manual-page") (title "note-download-url") (form note-download-url) (description "A variable the value of which must the URL of page, from which download of the lecture notes is possible. This variable must be defined
               before the style clause. In case you do not want to support download, define this variable to to the empty string") (misc "Use the Scheme define form to define the value of the variable: (define note-download-url ...) ")) ((kind "manual-page") (title "set-language") (form (set-language language)) (description "Defines which language to use in the LENO defined text of the lecture note system. Currently Danish and English is supported.
               The default language is Danish.") (parameters (parameter "language" "a symbol, either 'danish or 'english")) (misc "Internally, sets the variable lecture-note-language-preference to the symbol")) ((kind "manual-page") (title "lecture-note-verbose-mode") (form lecture-note-verbose-mode) (description "A booelan Scheme variable that determines whether some feedback on progress etc is written out while the LENO 
               system processes a LAML file. Default value is #t") (misc "Use the Scheme define form to define the value of the variable: (define lecture-note-verbose-mode ...) ")) ((kind "manual-page") (title "cross-reference-location-hints") (form (cross-reference-location-hints location-hints)) (description "A function which may return a hint of the location of a cross reference. 
The parameter is a list of symbols. This function must be defined in notes.scm. It is legal
always to return the empty string from this function, in which case no location hints will be given.
") (parameters (parameter "location-list" "a list of symbols which depends on the lectures notes as such, not on LENO"))) ((kind "manual-page") (title "meta-tag-clauses") (form (meta-tag-clauses)) (description "Returns meta information about these notes in terms of a list of list. 
In other words, this function is required to return a list of list. Each inner list must
be an even length list of key (symbol) value (string) pairs.
Each list generates a single HTML meta tag.
It is intended that this function returns the general meta information,
such as the charset used, the generator applied, the language used, etc.
Page specific meta information can be supplied as parameters to functions such
as html:page. This function is, in fact, called by html:page and by html:page-with-keypress-script
in the html-v1 library") (misc "Must be defined in notes.scm")) ((kind "manual-page") (title "java-scripting") (form java-scripting) (description "A boolean valued variable telling whether or not to use Javascript scripts.
If the value is #f, no Javascripts are generated in the lecture notes. If the value is
#t java scripts can be applied. Currently, we only use Javascripts for keyboard navigation shortcuts,
and it only works in IE4. The default value is #t")) ((kind "manual-page") (title "javascript-loading") (form javascript-loading) (description "A variable which determines how Javascript scripts are loaded.
               The value must be one of the symbols 'static or 'dynamic.
               If static, the javascript is included inline into the generated HTML page.
               if dynamic, the javascript is loaded from the javascript sub-directory of the html directory.
               In this case the javascript navigation file (lenonavigate.js) is copied from the software
               directory to the notes directory. The copying takes place in the end-notes function.
               This variable is only relevant if java-scripting is #t") (cross-references (internal-references "See also" "end-notes"))) ((kind "manual-page") (title "copy-image-files?") (form copy-image-files?) (description "A variable which controls whether to copy image icons from the software directory to the note source directory.
               The copying is done in the end-notes function.
               The value of the variable must be of type boolean. The default value is true.") (cross-references (internal-references "See also" "end-notes")) (misc "Use the Scheme define form to define the value of the variable: (define copy-image-files? ...) ")) ((kind "manual-page") (title "copy-note-graphics?") (form copy-note-graphics?) (description "A variable which controls whether to copy author defined graphics from the software graphics directory to the HTML destination directory.
               The copying is done in the end-notes function.
               The value of the variable must be of type boolean. The default value is true.") (cross-references (internal-references "See also" "end-notes")) (misc "Use the Scheme define form to define the value of the variable: (define copy-image-files? ...) ")) ((kind "manual-page") (title "preferred-maximum-filename-length") (form preferred-maximum-filename-length) (description "The preferred maximum length of file names.
               The length includes the including extension, but excludes the initial path.
               This size may be important for CD file systems.
               A warning is issed if the preferred maxiumum length is exceeded.
               The type is integer. The default value is 64.") (misc "Use the Scheme define form to change the value of the variable: (define preferred-maximum-filename-length ...) ")) ((kind "manual-page") (title "generating-system") (form (generating-system)) (description "A parameter less function which is supposed to return a string describing the system, which has generated these notes.
               This information is useful of you use LENO on more than one machine in slightly different contexts. The default
               implementation returns the empty string (no information)") (misc "If needed, define this function in post-notes.scm")) ((kind "manual-page") (title "course-dir") (form course-dir) (description "A variable which defines the course directory, in which the course plan files resides.") (misc "Define this variable using a Scheme define form. ")) ((kind "manual-page") (title "solution-passwords") (form solution-passwords) (description "A variable the of which is to define a number of password for exercise solution files. 
               We do not want users to be able to guess the names of solution files. Therefore we can
               add a component to solution file names, which cannot be guessed: a password component.
               Such a component is defined pr. lecture. The value of the variable is a list of 
               password entries. A password entry is a cons pair (lecture-id-symbol . password-string).
               The default value is an empty list, meaning that no passwords are mixed into the solution files.") (misc "Define this variable via a Scheme define form in the post-notes.scm")) ((kind "manual-page") (title "slide-header?") (form slide-header?) (description "Define the style of headers to use in slide style. The value must be one of the symbols normal, none, or minimal.
               Normal implies a range of navigation icons, none means nothing, and minal means an informative text including
               lecture and page numbers. Use minimal if you want maximum screen real estate for slide contents, and if
               you use keyboard navigation") (misc "Define this variabel via a Scheme define form in the individual LAML files.")) ((kind "manual-page") (title "trail-of-lecture?") (form trail-of-lecture?) (description "A boolean variable which controls whether to make a parallel trail of the lecture.
               This may be useful if you want to take advantage of the convenient return to the original slide after
               having followed cross references from a slide.
               Turning this variable to true makes a frameset for each slide, cause many more files in the html directory.
               Consider to set slide-header? to either none or minimal if trail-of-lecture? is true.") (cross-references (internal-references "Related clause" "slide-header?")) (misc "Define this variabel via a Scheme define form in the individual LAML files.")) ((kind "manual-page") (title "relative-source-html-destination-path-fragment") (form (relative-source-html-destination-path-fragment)) (description "A non-argument function which returns the difference between the note-source-directory and the note destination HTML directory.
               The default value is \"html/\". Redefine this function in post-notes.scm in case you wish another placement of the
               destination HTML directory ")) ((kind "manual-page") (title "lecture-title-page?") (form lecture-title-page?) (description "A variable that controls the generation of a title page and an abstract page.") (misc "Use the Scheme define form to change the value of the variable: (define lecture-title-page? ...) ")) ((kind "manual-page") (title "mouse-advancement") (form mouse-advancement) (description "It is possible to advance from one slide to the next via either single or double click.
               The value of this variable determines whether to use single or double click for page advancement. Possible
               values are single-press or double-press (Scheme symbols).
               The value double-press is default and recommended. If you use single page advancement this collides with text selection.") (misc "Use the Scheme define form to change the value of the variable: (define lecture-title-page? ...) ")) ((kind "manual-page") (title "show-and-speak?") (form show-and-speak?) (description "A boolean variable that controls the generation of an automatic progressing slide show with speaker sound,
               briefly referred to as 'show-and-speak'.
               The value #t calls for such a show.") (misc "Set this variable with the Scheme define form.")) ((kind "manual-page") (title "default-showing-time") (form default-showing-time) (description "The number of seconds to show a slide page with show-and-speak. This variable is
               used in the situation where show-and-speak? is #t, but no show-and-speak clause is 
               specified on a note page.") (cross-references (internal-references "Related definitions" "show-and-speak?" "show-and-speak")) (misc "Set this variable with the Scheme define form.")) ((kind "manual-page") (title "additional-showing-time") (form additional-showing-time) (description "An additional number of seconds added to the showing times of each show-and-speak page.") (cross-references (internal-references "Related definitions" "default-showing-time" "show-and-speak")) (misc "")) ((kind "manual-page") (title "speak-url-prefix") (form speak-url-prefix) (description "The prefix of the URL to speaker sounds. Must be ended with a forward slash character. Similar
               to speak-file-prefix. This variable is used to address the sound files.") (cross-references (internal-references "similar definition" "speak-file-prefix")) (misc "Set this variable with the Scheme define form.")) ((kind "manual-page") (title "speak-file-prefix") (form speak-file-prefix) (description "The file prefix to the speaker sound. Similar to speak-url-prefix.
               This variable is used to check the existence of sound files.") (cross-references (internal-references "similar definition" "speak-url-prefix"))) ((kind "manual-section") (section-title "Top level functions") (section-body "Besides the preamble stuff there is only a few top level function. The most
                import of these is  <kbd>note-page</kbd> . Each note-page describes the contents of a slide, together
                with possible annotations. There are Emacs commands of the form <kbd>insert-f</kbd> for each function f described in this section.
                ")) ((kind "manual-page") (title "begin-notes") (form (begin-notes)) (description "This form marks the ending of the preamble and the beginning of the note page definition part. It must be matched by exactly one (end-notes)") (pre-condition "Must occur before lecture-intro and before the first note-page form") (cross-references (internal-references "Related clauses" "end-notes"))) ((kind "manual-page") (title "end-notes") (form (end-notes)) (description "States the ending of this lecture note file. This form matches the (begin-notes) form. The end-note form must be the last clause in a lecture note file.") (pre-condition "Must occur after the last note-page in this lecture file.") (cross-references (internal-references "Related clauses" "begin-notes"))) ((kind "manual-page") (title "lecture-intro") (form (lecture-intro title author-list . abstract)) (description "Defines the title, authors and the abstract of this lecture note") (parameters (parameter "title" "the string defining the title of this chapter of the lecture notes") (parameter "author-list" "a list of strings, the first of which must be the author name. The rests can be affiliation information") (parameter "abstract" "the string with the abstract (optional)") (parameter "date" "the date (a string) at which this material is published (optional)")) (pre-condition "Should occur right after the (begin-notes) form")) ((kind "manual-page") (title "note-page") (form (note-page page-id . note-page-elements)) (description "The outer level description of a page. A page corresponds to a slide in slide view. The ordering of note-page-elements
               is significant for all elements appart from title, section-title, long-slide. The mutual ordering determines the sequence of
               element presentations in all views.") (parameters (parameter "page-id" "A symbol which in a unique way identifies this note page in the notes defined in the current file") (parameter "note-page-elements" "A list of one or more of the LENO forms documented in this manual")) (cross-references (internal-references "Reference to" "slide-url" "note-url" "book-url")) (pre-condition "Must be a top level form") (misc "Must at least contain a title or a section-title form. The Emacs commands <kbd>insert-notepage</kbd> and <kbd>insert-notesection</kbd>
            insert templates of this form.")) ((kind "manual-section") (section-title "Clauses of the note-page form") (section-body "In this section follows all the possible note-page clauses. All of these must appear within a note-page form.
      Some of the clauses must appear at the top level within a note-page form. Others must be further nested.
      Please consult the preconditions of the clauses for information about this")) ((kind "manual-page") (title "title") (form (title title-string title-comment)) (description "Defines the title of a note-page ") (parameters (parameter "title-string" "the title itself") (parameter "title-comment" "remarks to the title which are used on the annotated slides and in the lecture notes, but not in the slide view")) (pre-condition "Must be a constituent of a note-page form") (cross-references (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-title.html") (internal-references "Related clauses" "section-title"))) ((kind "manual-page") (title "section-title") (form (section-title title-string)) (description "Defines the title of a section of slides. In the lecture note view this can be thought of as a starting a new section in a chapter. 
               In this context, a chapter is a single collection oflecture notes") (parameters (parameter "title-string" "The title of the section")) (pre-condition "Must be a constituent of a note-page form. Also, this clause should be the only clause of the enclosing note-page form") (cross-references (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-section1.html") (internal-references "Related clauses" "title"))) ((kind "manual-page") (title "text") (form (text primary-text secondary-text)) (description "Defines a text clause of a note page. The primary text is shown in all views, whereas the secondary text is shown
               in the annotated slides and in the note page views only. The secondary text is used for additional comments to the primary text.
               The second parameter is optional") (parameters (parameter "primary-text" "The text to be shown in slide views (of type string)") (parameter "secondary-text" "The additional comments to the primary text (of type string)")) (pre-condition "Must be a constituent of a note-page form") (cross-references (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-text.html") (internal-references "Related clauses" "note-text" "slide-text" "items"))) ((kind "manual-page") (title "slide-text") (form (slide-text text)) (description "Defines a text clause to appear in slides views only. Does not affect slide annotations nor note page views at all") (parameters (parameter "text" "the text to appear in slide view")) (pre-condition "Must be a constituent of a note-page form") (cross-references (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-slide-text.html") (internal-references "Related clauses" "note-text" "text"))) ((kind "manual-page") (title "note-text") (form (note-text text)) (description "Defines a text clause to appear in note page view and annotated slide view only. Does not affect slide view at all") (parameters (parameter "text" "the text to appear in annotated slide and note page view")) (pre-condition "Must be a constituent of a note-page form") (cross-references (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-note-text.html") (internal-references "Related clauses" "slide-text" "text"))) ((kind "manual-page") (title "items") (form (items . item-list)) (description "Defines one of more items, to be presented as a bullet list") (parameters (parameter "item-list" "a list of item forms or strings")) (pre-condition "Must be a constituent of a note-page form") (cross-references (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-items.html") (internal-references "Related clauses" "Constituent form" "item"))) ((kind "manual-page") (title "item") (form (item primary-text secondary-text sub-items)) (description "Describes a single item. Secondary-text and sub-items are optional parameters") (parameters (parameter "primary-text" "The item text to be shown in all views (a string)") (parameter "secondary-text" "The item text to be shown in the annotated slide view and the note page views (a string)") (parameter "sub-items" "Items which act as subitems (an items form, recursively)")) (pre-condition "Must be a constituent of an items form") (cross-references (internal-references "Enclosing form" "items")) (misc "An item-form with only one parameter corresponds to the parameter<p>Secondary-text parameters of an item is only taken into consideration at level one and two in an items form")) ((kind "manual-page") (title "point") (form (point primary-point secondary-point)) (description "A point is a distinguished observation which is presented with special emphasis in all views. The second parameter is optional") (parameters (parameter "primary-point" "The observation presented in slide view (a string)") (parameter "secondary-point" "Acts as a comment to the primary-point (a string). Is only shown in the annonated slide view and in note page view")) (pre-condition "Must be a constituent of a note-page form") (cross-references (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-point.html"))) ((kind "manual-page") (title "example") (form (example example-text about-the-example)) (description "Defines an example which is shown in all views. The second parameter is optional") (parameters (parameter "example-text" "the example itself (a string)") (parameter "about-the-example" "an explanation of the example (a string)")) (pre-condition "Must be a constituent of a note-page form") (cross-references (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-example.html") (internal-references "Related clause" "text")) (comment "It is doubtful whether this clause is worthwhile, although of course, it defines a semantic difference between
           an example and a more general text clause")) ((kind "manual-page") (title "opposing") (form (opposing . item-list)) (description "Presents a number of opposing items. A two column presentation is used.") (parameters (parameter "item-list" "a list of item forms")) (pre-condition "Must be a constituent of a note-page form") (cross-references (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-opposing.html") (internal-references "Constituent forms" "item")) (misc "Each item form in an opposing clause must have exactly two textual parameters. No sub-items are allowed") (comment "We should probably not use item as constituent form, but rather a new form called opposing-items, or something similar")) ((kind "manual-page") (title "comment") (form (comment comment-text)) (description "Present an internal comment. A comment is only intended as a remark, which should be removed in the final version") (parameters (parameter "comment-text" "The comment itself (a string)")) (pre-condition "Must be a constituent of a note-page form") (cross-references (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-comment.html")) (comment "As an idea, comments could be suppressed by a boolean flag")) ((kind "manual-page") (title "long-slide") (form (long-slide)) (description "Declares that this slide is a long. Long slides have a bottom navigation bar similar to the top nativation bar.
               This clause may appear anywhere in the list of elements of a note-page") (pre-condition "Must be a constituent of a note-page form") (cross-references (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-long-slide.html"))) ((kind "manual-page") (title "index-words") (form (index-words . words)) (description "Defines a number of index words of this page. The index words are added to the central lecture note index with a link
               to the enclosing note page. This clause leaves no trace in any of the views. The index-words clause may be 
               given at an arbitrary position within a note-page form") (parameters (parameter "words" "the list of words, each of which are strings")) (pre-condition "Must be a constituent of a note-page form") (cross-references (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-index-words.html") (internal-references "Related clause" "concept"))) ((kind "manual-page") (title "concept-list") (form (concept-list . concepts)) (description "Defines a number of concepts. Each concept is defined via a separate concept clause") (parameters (parameter "concepts" "a list of concept forms")) (pre-condition "Must be a constituent of a note-page form") (cross-references (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-concept-list.html") (internal-references "Constituent form" "concept"))) ((kind "manual-page") (title "concept") (form (concept concept-name concept-definition concept-explanation)) (description "Defines a concept in terms of its name, a concept definition, and a concept explanation. The concept name goes into the index.
               The concept definition is shown in slide view. The concept explanation is, together with the concept definition, shown in the two other views") (parameters (parameter "concept-name" "the name of the concept (a string)") (parameter "concept-definition" "the primary description and definition of the concept (a string)") (parameter "concept-explanation" "an explanation of the concept, typically explained broader and less formal than in the concept definition")) (pre-condition "Must be a constituent of a concept-list form") (cross-references (internal-references "Enclosing form" "concept") (internal-references "Related clause" "index-words") (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-concept-list.html"))) ((kind "manual-page") (title "quotation") (form (quotation quotation-text quotation-comment)) (description "Defines a quotation, which is presented within quote characters. The second parameter is optional") (parameters (parameter "quotation-text" "the text to be quoted (a string)") (parameter "quotation-comment" "comments to the quotation text (a string).
                                  This parameter is only used in the annotated slide view and in the note page view")) (pre-condition "Must be a constituent of a note-page form") (cross-references (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-quotation.html"))) ((kind "manual-page") (title "image") (form (image filename explanation formatter)) (description "Inserts a gif image from an external file. The last two parameters are optional.") (parameters (parameter "filename" "the name of the file in the graphics and the graphics/small directories. The file name must include the file extension (.gif),
              but not directory information. The type of filename is string.") (parameter "explanation" "a textual explanation of the image (a string)") (parameter "formatter" "a list which defines how the image and the explanation are formatted relative to each other in slide view. 
                          This parameter is a list of symbols. The following forms are supported:
                         
                           <font size = 2><pre>
'(vertical picture)                 just show picture          DEFAULT
'(vertical text)                    just show text
'(vertical picture text)            show picuture above text
'(vertical text picture)            show picture below text 
'(horizontal text picture)          show picture to the right of text
'(horizontal picture text)          show picture to the left of text</pre></font> 
                         ")) (pre-condition "Must be a constituent of a note-page form") (cross-references (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-image.html") (internal-references "Related clause" "image-series")) (misc "All images must be stored in a directory named  <kbd>graphics</kbd>  which is a subdirectory of the catalogue
        in which the generated html files reside.
        The graphics directory, in turn, must have a sub directory called  <kbd>small</kbd>  in which a smaller version of all images reside. This is in order
        to ensure good reduction of images in the annotated slide view and in the lecture note view")) ((kind "manual-page") (title "slide-image") (form (slide-image filename)) (description "Show a gif image in slide view and in the annotated slide view. The note page view is not affected.
               This clause is useful to duplicate an already shown picture in a slide. The first occurence of the picture is shown via the image clause") (parameters (parameter "filename" "the filename where the gif image is stored. See the manual entry of  <a href = \"#image\">image</a>  for further explanations of filename")) (pre-condition "Must be a constituent of a note-page form") (misc "This clause is much simpler, and less important than image. It's sole purpose is duplication of already shown images")) ((kind "manual-page") (title "image-series") (form (image-series series-title image-descriptions how-to-show)) (description "Defines a series of images, which may define a number of steps. Each step can be explained separately via the second parameter.
              Via the last parameter it is possible to control how the images are presented in the three different views") (parameters (parameter "series-title" "a text describing the series as such (a string)") (parameter "image-description" "a list of individual image-descriptors. 
                                  An image-descriptor is, in turn, a list of the form  <kbd>(filename image-explanation)</kbd> .
                                  filename plays the same role as the filename parameter in the  <a href = \"#image\">image</a>  clause.
                                  image-explanation is a string which explains an individual image in the series.
                                  In summary, this parameter is of the type list of lists") (parameter "how-to-show" "a list of length two, which describes how the image series is presented in the various views.
                            The following possibilities exist: <font size = 2><pre>
'(slide-external book-inline)      Link to material on slides and annotated,
                                   but inline material in the note page view
'(slide-inline book-inline)        Inline material in all views
'(slide-external book-external)    Link to material in all views
'(slide-inline book-external)      Inline material on the slide and annotated slides,
                                   but link to material in the note page view</pre></font> 
                            The type of the parameter is list of symbols.")) (pre-condition "Must be a constituent of a note-page form") (cross-references (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-image-series.html") (internal-references "Related clause" "image")) (misc "The most useful configuration of  <kbd>how-to-show</kbd>  is  <kbd>'(slide-external book-inline)</kbd> because we  
        in that way use minimal space on slides. In this configuration, the full image series is shown inline in the note-page view")) ((kind "manual-page") (title "cross-references") (form (cross-references reference-list)) (description "Define a number of cross references to other material. The references may be to Internet stuff (urls), to other places in the current
               collection of lecture notes, or to off line paper material") (parameters (parameter "reference-list" "a list of reference clauses. The reference clauses can be note-reference, internet-reference or informal-reference forms.
                               Each of these are documented elsewhere in this manual")) (pre-condition "Must be a constituent of a note-page form") (cross-references (internal-references "Constituent clauses" "note-reference" "internet-reference" "informal-reference") (internal-references "See also" "slide-url" "note-url" "book-url" "exercise-formulation-url" "exercise-formulation-url-given-id" "exercise-solution-url"))) ((kind "manual-page") (title "note-reference") (form (note-reference anchor-text section-id slide-id . location-hints)) (description "Describes a reference to another part (possibly in another section) of the current lecture notes") (parameters (parameter "anchor-text" "the textual description used as anchor text to this reference (a string)") (parameter "section-id" "the symbol which identifies the section of this reference (a symbol)") (parameter "slide-id" "the symbol which identifies the slide within a section of this reference (a symbol)") (parameter "location-hints" "hints on where and how to find the reference (a list of symbols, not defined in LENO)")) (pre-condition "Must be a constituent of a cross-references form") (cross-references (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-cross-references.html") (internal-references "Enclosing clause" "cross-references") (internal-references "Related clauses" "internet-reference" "informal-reference"))) ((kind "manual-page") (title "internet-reference") (form (internet-reference anchor-text url . location-hints)) (description "Defines a reference to an Internet resource") (parameters (parameter "anchor-text" "the textual description used as anchor text to this reference (a string)") (parameter "url" "the WWW url of the reference (a string)") (parameter "location-hints" "hints on where and how to find the reference (a list of symbols, not defined in LENO)")) (pre-condition "Must be a constituent of a cross-references form") (cross-references (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-cross-references.html") (internal-references "Enclosing clause" "cross-references") (internal-references "Related clauses" "note-reference" "informal-reference")) (misc "It is often very useful to generate urls via Scheme functions. This is the case if several references are made to information
        located at the same server and/or directory. In that way only the function needs redefintion in case we want a set of 
        internet references to be moved")) ((kind "manual-page") (title "informal-reference") (form (informal-reference anchor-text reference-text . location-hints)) (description "Defines a reference to an off line resource, such that a paper or a book") (parameters (parameter "anchor-text" "the textual description used as anchor text to this reference (a string)") (parameter "reference-text" "details about the reference material") (parameter "location-hints" "hints on where and how to find the reference (a list of symbols, not defined in LENO)")) (pre-condition "Must be a constituent of a cross-references form") (cross-references (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-cross-references.html") (internal-references "Enclosing clause" "cross-references") (internal-references "Related clauses" "note-reference" "internet-reference")) (misc "It would be useful to define Scheme functions which produce the reference text form from structurede, bibliographic information")) ((kind "manual-page") (title "syntax") (form (syntax syntax-rule syntax-explanation)) (description "Defines a syntax description in terms of a syntax rule and an explanation") (parameters (parameter "syntax-rule" "the sytax rule, such as a grammar production (or it's right hand side). White space and line shifts are significant
                            in this string. The type is a string.") (parameter "syntax-explanation" "the intuitive explanation of the syntax-rule and it's constituents (a string)")) (pre-condition "Must be a constituent of a note-page form") (cross-references (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-syntax.html")) (misc "It is often most convenient to read the syntax-rule string from an external file. The meta example referred to above shows an example.
        The function read-text-file-between-marks is useful for reading a substring from an external file. In order to decorate the syntax-rule
        with fonting and coloring the function colorize-substrings is useful.")) ((kind "manual-page") (title "source-program") (form (source-program filename region-marks decorations how-to-show comment)) (description "Shows a source program (or a similar text of an artificial language) in 'verbatim style'. 
               The source program text is taken from an external file. 
               It is possible to extract a substring from the external file via the region-marks parameter.
               Substrings of the extracted source program (part) can be decorated with colors and font-faces.
               The first four parameters are mandatory. The comment parameter is optional.
               ") (parameters (parameter "filename" "the name of the file where the source program is located. The name is relative to the directory,
              in which the LENO laml source file is located. The name may, alternatively, be a full file path. The type is string") (parameter "region-marks" "describes the substring of the source file to include.
                             If it is the empty string, all of the source file is included.
                             If it is a list, it must be of length two, and the elements must both be strings.
                             In that case the first string determines the starting point of the sub-string and 
                             the second string determines the ending points of the sub-string (both ends are included).
                             In summary the type is either a string (empty) or a list of strings (of length two)
                             ") (parameter "decorations" "describes the colors and fonts of substrings of the included source program.
                            decorations is a list of decorator lists. A decorator list is of the form
                             <pre>(from-delimiter to-delimiter color font-face repeat-count)</pre> 
                            where the first three elements are mandatory.
                             <ul><li type = disc>The from-delimiter and to-delimiter designate the substring to docorate (both parts are included).
                                   These two parameters are strings.</li><li type = disc>The color parameter is a list of rgb colors, such as  <kbd>'(255 255 255)</kbd>  for white.</li><li type = disc>The font-face is one of the symbols   <kbd>italic</kbd> ,  <kbd>bold</kbd> ,  <kbd>typewriter</kbd> ,  <kbd>underlined</kbd> , or  <kbd>plain</kbd> . Plain is the default.</li><li type = disc>repeat-count is a number which makes it possible to instruct colorizing and fontifying further occurences of
                                          the desingnated substring.</li></ul>") (parameter "how-to-show" "a list of list of two symbols, which describes how show the source program on the note page. Possible 
                            values are:
                             <font size = 2><pre>
'(slide-external book-inline)      Link to material on slides and annotated,
                                   but inline material in the note page view
'(slide-inline book-inline)        Inline material in all views
'(slide-external book-external)    Link to material in all views
'(slide-inline book-external)      Inline material on the slide and annotated slides,
                                   but link to material in the note page view</pre></font>") (parameter "comment" "a textual comment (of type string) which explains the details of the source program")) (pre-condition "Must be a constituent of a note-page form") (cross-references (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-source-program.html")) (misc "It will be very useful to consult the meta examples regarding the source-program clause. The meta examples show how
        portions of the laml source file can be included in its own html presentation. This is an advanced application, but 
        illustrative and useful")) ((kind "manual-page") (title "exercise") (form (exercise exercise-id exercise-title exercise-formulation . exercise-solution)) (description "Defines an exercise in terms of an id, a title, a formulation and an optional solution. 
               A link will be provided to a separate page where the title and formulation
               appear. The exercise-solution parameter is optional") (parameters (parameter "exercise-id" "a symbol identifying the exercise for reference purposes") (parameter "exercise-title" "the title of the exercise (a string)") (parameter "exercise-formulation" "the text formulating the exercise in terms of a formulation clause") (parameter "exercise-solution" "the text formulating the exercise solution in terms of a solution clause")) (pre-condition "Must be a constituent of a note-page form") (cross-references (internal-references "Constituent forms" "formulation" "solution") (internal-references "References to" "exercise-formulation-url" "exercise-formulation-url-given-id" "exercise-solution-url") (internal-references "See also" "exercise-model") (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-exercise.html"))) ((kind "manual-page") (title "formulation") (form (formulation exercise-formulation-text)) (description "Defines the exercise formulation text") (parameters (parameter "exercise-formulation-text" "A string defining the exercise formulation")) (pre-condition "Must occur as part of an exercise form") (cross-references (internal-references "Enclosing form" "exercise") (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-exercise.html") (internal-references "Reference to" "exercise-formulation-url" "exercise-formulation-url-given-id"))) ((kind "manual-page") (title "solution") (form (formulation exercise-solution-text)) (description "Defines the exercise solution text") (parameters (parameter "exercise-solution-text" "A string defining the exercise solution")) (pre-condition "Must occur as part of an exercise form") (cross-references (internal-references "Enclosing form" "exercise") (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-exercise.html") (internal-references "Reference to" "exercise-solution-url"))) ((kind "manual-page") (title "tabular") (form (tabular border column-widths table-contents table-explanation)) (description "Defines a tabular presentation of table-contents. The last parameter is optional.") (parameters (parameter "border" "an integer designating the frame width of the table. 0 means no frame.") (parameter "column-widths" "a list of integers or strings which determine the widths of the table columns.
                              Integers measure points. Strings are either html-link percent measueres or a html-link star character") (parameter "table-contents" "a list of lists. The inner lists are table rows. Each of the inner lists must be of equal length, and this
                               length must be the same as the length of column-widths") (parameter "table-explanation " "an intuitive explanation of the of the tabular information")) (pre-condition "Must be a constituent of a note-page form") (cross-references (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-tabular.html")) (misc "This clause resemble the set of Scheme table functions, on which much of the LAML formatting work is based")) ((kind "manual-page") (title "applet-program") (form (applet-program class-file codebase explanation width height)) (description "Defines and inserts a Java applet") (parameters (parameter "class-file" "the name of the file where the applet is located. Must include the class file extension. The location of the file
                           is relative to codebase (the next parameter). The type of this parameter is string") (parameter "code-base" "the directory or url in which the class file is located. The code base is taken relative to the directory in which 
                         the generated html files reside") (parameter "explanation" "the textual explanation of the applet (a string)") (parameter "width" "an integer describing the width of the applet (integer, in pixels)") (parameter "height" "an integer describing the height of the applet (integer, in pixels)")) (pre-condition "Must be a constituent of a note-page form") (cross-references (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-applet-program.html"))) ((kind "manual-page") (title "synopsis") (form (synopsis . synopsis-item-list)) (description "Defines a synopsis in terms of a list of synopsis item elements. A synopsis is useful to summarize a number of points
               in a graphically distinguished manner") (parameters (parameter "synopsis-item-list" "a list of synopsis-item forms")) (pre-condition "Must be a constituent of a note-page form") (cross-references (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-synopsis.html") (internal-references "Constituent clause" "synopsis-item"))) ((kind "manual-page") (title "synopsis-item") (form (synopsis-item synopsis-text synopsis-comment)) (description "Defines a single item in a synopsis clause") (parameters (parameter "synopsis-text" "the text of an synopsis item, which goes into all views (a string)") (parameter "synopsis-comment" "an additional comment to the synopsis text. Goes into the annotated side view and the note page view")) (pre-condition "Must be a constituent of a synopsis form") (cross-references (reference "Meta example" "example with access to the underlying note-page form" "html/meta-lecture-notes-slide-synopsis.html") (internal-references "Enclosing clause" "synopsis"))) ((kind "manual-page") (title "quiz") (form (quiz question answer-list)) (description "Define a contribution to a quiz about the current lecture.
               Quizzes are managed by a separate side server program, which is not part of the LAML distribution.
               As of now, the quiz facility can only be used at www.cs.auc.dk because it depends on additional software,
               including server side checking software.") (parameters (parameter "question" "A question clause with a formulation of the question") (parameter "answer-list" "An answer-list clause with individual answering possibilities ")) (cross-references (internal-references "Constituent clauses" "question" "answers" "answer"))) ((kind "manual-page") (title "question") (form (question question-formulation)) (description "Defines the question of a quiz clause") (parameters (parameter "question-formulation" "The question string")) (cross-references (internal-references "Surrounding clause" "quiz") (internal-references "Sibling clause" "answers"))) ((kind "manual-page") (title "answers") (form (answers . answer-list)) (description "Define a number of possible answers of a quiz clause") (parameters (parameter "answer-list" "A list of answer clauses")) (cross-references (internal-references "Surrounding clause" "quiz") (internal-references "Sibling clause" "question"))) ((kind "manual-page") (title "answer") (form (answer possible-answer correctness-point clarification)) (description "Define a single answering possibility side by side with other possible answers") (parameters (parameter "possible-answer" "A possible answer presented for the user (a string).") (parameter "correctness-point" "A measure of correctness. A number between 0 and 100. 0 means wrong, 100 means correct.
                                  Numbers in between 0 and 100 means partially correct.") (parameter "clarification" "An explanation of why - or why not - the answer possibility is right or wrong.")) (cross-references (internal-references "Surrounding clauses" "answers" "quiz"))) ((kind "manual-page") (title "show-and-speak") (form (show-and-speak . show-speak-clauses)) (description "Define a number of 'show and speak' clause of the surrounding note page. 
               A show and speak clause controls the speaker's sound and the sequencing in an automatically progressing
               slide show. ") (parameters (parameter "show-and-speak-clauses" "Slide-part, program-part, and image-series-part clauses")) (cross-references (internal-references "Constituent clauses" "slide-part" "program-part" "image-series-part") (internal-references "About show and speak" "SECTION8"))) ((kind "manual-page") (title "slide-part") (form (slide-part number time)) (description "Define a slide-part of a surrounding show and speak clause. A slide-part asks for presentation of the
               slide for a certain amount of time.") (parameters (parameter "number" "A slide page can be show a number of time an an automatically progressing slide show. Each time there
                       will be a different speaker sound related to the slide. This parameter defines the number associated with
                       this presentation.") (parameter "time" "The number of seconds (an integer) to show this page")) (cross-references (internal-references "Sibling clauses" "program-part" "image-series-part") (internal-references "Surrounding clause" "show-and-speak"))) ((kind "manual-page") (title "program-part") (form (program-part number time)) (description "Define a program-part of the surrounding show and speak clause. A program-part presents one of the external source programs
               on this page.") (parameters (parameter "number" "Which program to show. The programs are numbered on each note page, starting from 1, in presentation order.") (parameter "time" "The number of seconds (an integer) to show this page")) (cross-references (internal-references "Sibling clauses" "slide-part" "image-series-part") (internal-references "Surrounding clause" "show-and-speak"))) ((kind "manual-page") (title "image-series-part") (form (image-series-part number time-list)) (description "Define an image-series part of the surrounding show and speak clause.
               An image-series-part presents one of the image-series
               on this page.") (parameters (parameter "number" "Which image series to show. The images are numbered on each note page, starting from 1, in presentation order.") (parameter "time-list" "The number of seconds (an integer) to each image in the series. A list of integers.")) (cross-references (internal-references "Sibling clauses" "slide-part" "program-part") (internal-references "Surrounding clause" "show-and-speak"))) ((kind "manual-page") (title "lecturer-photos") (form (lecturer-photos start-photo-number number-of-photos)) (description "Present a number of lecturer photos on the current note page") (parameters (parameter "start-photo-number" "Refers to the first photo to show (an integer).") (parameter "number-of-photos" "Tells how many photos to show (an integer)")) (pre-condition "Depends on the list lecturer-photo-list, which must enumerate the available photos. Defined it in post-notes.
                 The lecturer photos must be organized in the sub-directory lecturer-photos in the lecture note source directory
                 (the directory in which your lecture laml files are found). The photos will be copied to the html destination 
                 directory as part of LENO processing.") (cross-references (internal-references "Related clause" "lecturer-photos-and-logo"))) ((kind "manual-page") (title "lecturer-photos-and-logo") (form (lecturer-photos-and-logo start-photo-number)) (description "Show two lecturer photos and a logo. The parameter determines which photos to show. ") (parameters (parameter "start-photo-number" "Refers to the first photo to show (an integer). This quantity is automatically
                                   normalized to an appropriate range.")) (cross-references (internal-references "Related clause with necessary information" "lecturer-photos")) (misc "The logo is a gif file organized together with the other images which are important for LENO.
        You should define your logo in the images directory of your lecture note directory (the note specific images files).
        The Aalborg University logo comes together with LENO, however, so it is automatically there.
        The logo image must have the
        name 'logo.gif'. In case you want to link from the logo, define the variable logo-url to an appropriate
        WWW address.")) ((kind "manual-page") (title "elucidate") (form (elucidate description url target)) (description "Insert a link to an elucidator (accompanied with an eluciator icon) which explains a program.
               Only available for Schem and Java.") (parameters (parameter "description" "The description of the elucidator. The first sentence is rendered after the elucidator icon.") (parameter "url" "The url with the WWW address of the top level elucidator page.") (parameter "target" "An optional parameter which gives the anchor target. Defaults to the value of elucidator-default-target."))) ((kind "manual-page") (title "splice-page-with") (form "(splice-page-with tag-list lecture-id page-id [id])") (description "Splice the elements of another page from a specified lecture-id including certain kinds of elements. 
    With this, it is possible to
    include selected elements from another page without copying its elements. Only the type of elements mentioned in 
    tag-list are included. If the optional id parameter is supplied, only include an element (of the 
    given type) with the given id. In practice, this implies that only a single element is included (because
    ids of subclauses have to be unique in a note page).
    In case a certain kind of element appears more than once, all occurrences are included.
    Only pages from the current lecture or a sibling lecture can be include. In other words,
    you cannot address pages in lectures located in other directories.") (parameters (parameter "tag-list" "A list of LENO element tag names (a list of symbols)") (parameter "lecture-id" "The lecture id of the lecture from which to splice a page - a symbol") (parameter "page-id" "The page id of the page from which to splice") (parameter "id" "The id of an element on the specified note page (optional parameter)")) (cross-references (internal-references "Related clause" "splice-page-without")) (misc "The internal lsp representation of the lecture contains the splice-page-with and splice-page-without clauses in contrast to
        the spliced elements. It is not possible to splice (part of) a page into itself recursively (you get an error).
        Mutual splicing of two pages into each other should be avoided - the result is undefined and it will break LENO.
        (We could easily support mutual recursive splicing by a very minial change in
        the information saved on the -.lsp file, but it would cause confusing results - ever expanding pages - which are
        not interesting for practical purposes).")) ((kind "manual-page") (title "splice-page-without") (form (splice-page-without tag-list lecture-id page-id)) (description "Splice the elements of another page from a specified lecture-id excluding certain types of elements.
    With this, it is possible to
    include selected elements from another page without copying its elements. The type of elements mentioned in 
    tag-list are not included; All others than the tags in tag-list are incuded.
    Only pages from the current lecture or a sibling lecture can be include. In other words,
    you cannot address pages in lectures located in other directories.") (parameters (parameter "tag-list" "A list of LENO element tag names (a list of symbols)") (parameter "lecture-id" "The lecture id of the lecture from which to splice a page - a symbol") (parameter "page-id" "The page id of the page from which to splice")) (cross-references (internal-references "Related clause" "splice-page-with") (internal-references "See also note of" "splice-page-with"))) ((kind "manual-page") (title "side-track") (form (side-track title track-lecture-id track-explanation)) (description "Define a side track. A side track is meant to explain a given topic in some additional detail.
               In LENO terminology, a side track is a lecture of its own. This lecture is dedicated to the side track.") (parameters (parameter "title" "The main text of the side track - its title (a string)") (parameter "track-lecture-id" "The lecture id of the lecture that make up the side track (a symbol)") (parameter "track-exaplanation" "Some further explanation of the role of this side track (a string)"))) ((kind "manual-section") (section-title "LENO Emacs Support") (section-body "There is a number of useful Emacs commands that make it easy and conveninent to author LENO material. 
        Most important, perhaps, is the function which help initiate a new set of slides (a single lecture only).
        This command is called <kbd>make-lecture-notes</kbd>.<p>The table below lists the other LENO specific Emacs Lisp commands. These will all be available
        if you have installed the LAML system appropriately.<p><table border = 0><tr><td width = 10 valign = top ></td><td width = * valign = top ><table border = 1><tr><td width = 160 valign = top bgcolor = \"#ffffff\"><b>Command</b></td><td width = 100 valign = top bgcolor = \"#ffffff\"><b>Key binding</b></td><td width = 500 valign = top bgcolor = \"#ffffff\"><b>Effect</b></td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">make-lecture-notes</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">make the directory structure of a LENO set of slides. You are prompted for directory and
                                              the name of slides. You are adviced to organize each single-chaper slide set in a new 
                                              directory.</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-notepage</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert a note-page form in a set of slides</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-notesection</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert a note-page form in a set of slides</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-title</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert a title form in a note page</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-items</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert an item form in a note page. This command prompts for the number of items at top level,
                                        and the number of subitems of each item. A very useful function if you use many items
                                        on your slides.</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-opposing</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert an opposing form in a note page. 
                                           This command prompts for the number of opposing items.</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-concept-list</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert a concept-list form in a note page.
                                               This command prompts for the number of concepts to define.</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-synopsis</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert a synopsis form in a note page. 
                                           This command promps for the number of synopsis items.</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-title</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert a title form in a note page</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-text</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert a text form in a note page</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-point</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert a point form in a note page</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-source-program</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert a source-program form in a note page</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-image</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert a title form in a note page</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-example</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert an example form in a note page</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-syntax</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert a syntax form in a note page</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-slide-space</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert a slide-space form in a note page</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-tabular</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert a tabular form in a note page</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-note-text</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert a note-text form in a note page</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-slide-text</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert a slide-text form in a note page</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-cross-references</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert a cross-references form in a note page</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-internet-reference</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert an internet-reference form in a cross-references form</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-inforal-reference</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert an informal-reference form in a cross-references form</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-note-reference</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert a note-reference form in a cross-references form</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-exercise</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert an exercise form in note page</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-long-slide</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert a long-slide form in note page</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-index-words</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert an index-words form in note page</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-image-series</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert an image-series form in note page</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-section-title</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert a section-titel form in note section page</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-slide-image</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert a slide-image form in note page page</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-applet-program</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert an applet-program form in note page page</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">leno-insert-quotation</td><td width = 100 valign = top bgcolor = \"#ffffff\">none</td><td width = 500 valign = top bgcolor = \"#ffffff\">insert a quotation form in note page page</td></tr></table></td></tr></table><p>The emacs lisp variable <kbd>laml-pp-helpful</kbd> (of type boolean) controls the detailed working of most of the leno-insert-... functions from above.
        If the value of  <kbd>laml-pp-helpful</kbd> is <kbd>t</kbd> (for true) it shows the roles of the individual constituents. If <kbd>nil</kbd> (for false) just
        empty string placeholders are presented.<p>The <kbd>leno-insert-..</kbd>. commands are found in the LENO section of the LAML menu in Emacs.<p>The variable <kbd>leno-version</kbd> controls which version of LENO to use. As the default, the value is <kbd>xml-in-laml</kbd> (a symbol).
        The old version of LENO is supported by the value <kbd>original</kbd> (a symbol). You can change <kbd>leno-version</kbd> via LAML customization.<p>Please also notice the following generally useful LAML Emacs commands:<p><table border = 0><tr><td width = 10 valign = top ></td><td width = * valign = top ><table border = 1><tr><td width = 160 valign = top bgcolor = \"#ffffff\"><b>Command</b></td><td width = 100 valign = top bgcolor = \"#ffffff\"><b>Key binding</b></td><td width = 500 valign = top bgcolor = \"#ffffff\"><b>Effect</b></td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">embed</td><td width = 100 valign = top bgcolor = \"#ffffff\">C-x C-e</td><td width = 500 valign = top bgcolor = \"#ffffff\">Embeds the current selection into a Lisp form. Handles the necessary string splitting and surrounding concatenation.
                                    The variables laml-single-string-approach, smart-word-determination, concatenate-form controls the behavior of 
                                    the embed command</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">unembed</td><td width = 100 valign = top bgcolor = \"#ffffff\">C-x C-r</td><td width = 500 valign = top bgcolor = \"#ffffff\">The opposite command of embed</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">split</td><td width = 100 valign = top bgcolor = \"#ffffff\">C-x C-q</td><td width = 500 valign = top bgcolor = \"#ffffff\">Split a string in two pieces</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">unsplit</td><td width = 100 valign = top bgcolor = \"#ffffff\">C-x C-a</td><td width = 500 valign = top bgcolor = \"#ffffff\">Join two strings - the opposite of split.</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">nest</td><td width = 100 valign = top bgcolor = \"#ffffff\">C-x C-n</td><td width = 500 valign = top bgcolor = \"#ffffff\">Nest a lisp form into another. A generic and generally useful Lisp programming primitive</td></tr><tr><td width = 160 valign = top bgcolor = \"#ffffff\">unnest</td><td width = 100 valign = top bgcolor = \"#ffffff\">C-x C-m</td><td width = 500 valign = top bgcolor = \"#ffffff\">The opposite command of nest</td></tr></table></td></tr></table>")) ((kind "manual-section") (section-title "Useful templates") (section-body "<em>This section is obsolete with respect to single chapter lecture notes. Use the Emacs command make-lecture-notes, as described
       in the previous section.
       If, however, you are going to initiate a multi chapter material, this section may still be
       a good starting point.</em> <p> Here we show a LENO lecture note source file of a chapter, which is supposed to be
       part of a set of chapters in a material. Notice that the notes.scm file is loaded.
       The capitalized words need to be filled in 
       by the author of the material. Most of the variables and functions seen in the template
       are documented above. <p> <table border = 0><tr><td width = 10 valign = top ></td><td width = * valign = top ><font size = 2 color = \"#a64700\"><pre>                                         ; Start of preamble section
(load (string-append laml-dir \"laml.scm\"))

(load \"notes.scm\")                       ; Load the common set up for all chapters from notes.scm
(style \"lecture-notes/lecture-notes\")    ; Causes loadning the LENO software
(load \"post-notes.scm\")                  ; Loads the post part of the common set up

(define front-index? #t)                 ; Set LENO up to make a top-level front index 
(define word-index? #t)                  ; Set LENO up make word, cross reference index

(set-lecture-name \"LECTURE-NAME\")        ; Define the name of this lecture, normally the same as the file name
(set-lecture-number N)                   ; Define the chapter number (a number)

(exercise-model 'make-indexes)           ; One of the symbols none, make-indexes, make-indexes-and-solutions-per-lecture,
                                         ; make-indexes-and-solutions-lecture-wide, synchronous-exercises, asynchronous-exercises

(define slide-header? 'normal)           ; one of the symbols normal, none, or minimal


(lecture-intro \"TITLE\"                   ; Define the title, author, affiliation, and the abstract
  (list 
    (copyright-owner \"AUTHOR\")
    \"AFFILIATION-1\"
    \"AFFILIATION-2\"
    )
  \"ABSTRACT\"
)

(begin-notes)                            ; End of preamble section, start of note page section


(note-page 'ID-OF-FIRST-SLIDE
  (section-title \"SECTION-TITLE\")
)

; A NUMBER OF SUBSEQUENT NOTE PAGES


(end-notes)                              ; End of note page section

</pre></font></td></tr></table> <p> Next, we give a template of the notes.scm file, which contains the common stuff in a multi-chapter
      set of lecture notes. <p> <table border = 0><tr><td width = 10 valign = top ></td><td width = * valign = top ><font size = 2 color = \"#a64700\"><pre>(define note-abstract
  \"ABSTRACT AT TOP LEVEL\")


; The directory in which this file is located.
; Must be ended with a slash
(define note-source-directory \"DIRECTORY\")             ; The directory where the note sources are located. 
                                                       ; Full, absolute path. Must be terminated by a slash.

(define notes-title \"NOTE TITLE\")                      ; The top level title of these notes.
 
(define current-lecture N)                             ; The number of the last section processed


(define lecture-sections                               ; A list with one entry per lecture section
  (list
    (list  \"CHAPTER-1-ID\"   \"CHAPTER ONE TITLE\")
    (list  \"CHAPTER-2-ID\"   \"CHAPTER TWO TITLE\")
    ...
   )
)
                                                       ; A parameter-less function returns a list of meta-tag contribution                                     
(define (meta-tag-clauses)
   (list (list 'HTTP-EQUIV \"Content-Type\" 'content \"text/html; charset = ISO-8859-1\")  
         (list 'name \"Generator\" 'content \"laml\")
         (list 'name \"description\" 
                     'lang (meta-language language-preference)
                     'content \"CONTENTS DESCRIPTION\")
   )
)


(define course-home-url \"URL\")                       ; The full course home URL.

(define author-home-url \"URL\")                       ; The full author's home URL.

(define note-download-url \"URL\")                     ; The url from which to download the set of notes. If empty string no download

(define (cross-reference-location-hints location-list)  ; No location hints 
  \"\")


; --------------------------------------------------------------------------------------------------------

; Here it is possible to define useful scheme functions,
; for instance functions which abbreviate URL's to outside
; material. Such functions are highly recommended.

</pre></font></td></tr></table> <p> Here is a template of the post-notes.scm file, which is loaded after the LENO software: <p> <table border = 0><tr><td width = 10 valign = top ></td><td width = * valign = top ><font size = 2 color = \"#a64700\"><pre>;; This is the portion of the notes.scm file which must
;; be loaded after the lecture notes style has been loaded.

;; In the long run, all the material in notes.scm should go here.
;; But there are some technical difficulties which must be overcome before
;; it can happen.

; On which system have we generated LENO. The variable computer-system may be defined
; in laml.scm at the basic LAML level.
(define (generating-system)
  (con (text-choice \"Genereret for: \" \"on the system \") (as-string computer-system)))

; A list of lecture specific passwords. A list of pairs. See the manual
(define solution-passwords
  (list
    (cons  'CHAPTER-1-ID  \"CRYPTIC-STRING-1\")
    (cons  'CHAPTER-2-ID  \"CRYPTIC-STRING-2\")
  ))

(set-language 'english)    ; set the language of the fixed texts in LENO. 'danish og 'english</pre></font></td></tr></table> <p> Finally, we give a template of a single chapter lecture note. In this template
      we have inlined the notes.scm file and the post-notes.scm file, and as a consequence no such file is needed in this case. <p> <table border = 0><tr><td width = 10 valign = top ></td><td width = * valign = top ><font size = 2 color = \"#a64700\"><pre>(load (string-append laml-dir \"laml.scm\"))       
                                                       ; Start of preamble section


(define note-source-directory \"DIRECTORY\")          ;  The directory where the note sources are located. 
                                                       ; Full, absolute path. Must be terminated by a slash.

(define current-lecture 1)

(define lecture-sections
  (list
    (list  \"CHAPTER-ID\" \"CHAPTER TITLE\")              ; The id and title of the one and only chapter.
   )
)

(define notes-title (cadr (car lecture-sections)))   ; Set the notes title to the chapter title, defined above.

(define course-home-url \"URL\")                       ; The full course home URL.

(define author-home-url \"URL\")                       ; The full author's home URL.

(define note-download-url \"URL\")                     ; The url from which to download the set of notes. If empty string no download

(define (cross-reference-location-hints location-list)  ; No location hints 
  \"\")

                                                       ; A parameter-less function returns A list of meta-tag contribution       
(define (meta-tag-clauses)
   (list (list 'HTTP-EQUIV \"Content-Type\" 'content \"text/html; charset = ISO-8859-1\")  
         (list 'name \"Generator\" 'content \"LAML\")
         (list 'name \"description\" 
                     'lang (meta-language language-preference)
                     'content \"CONTENTS DESCRIPTION\")
   )
)


(style \"lecture-notes/lecture-notes\")

(define front-index? #f)                               ; No front index; not needed because there is only one chaper.

(set-language 'english)                                 ; The fixed, system defined text are in English

(set-lecture-name (car (car lecture-sections)))         ; Define the name of this lecture to the chapter-id of lecture-sections (def. earlier)
(set-lecture-number 1)                                  ; Define the lecture numerb

(lecture-intro \"TITLE\"                                  ; Define the title, author, affiliation, and the abstract
  (list 
    (copyright-owner \"AUTHOR\")
    \"AFFILIATION-1\"
    \"AFFILIATION-2\"
    )
  \"ABSTRACT\"
)

(begin-notes)                                          ; End of preamble section, start of note page section


(note-page 'ID-OF-FIRST-SLIDE
  (section-title \"SECTION-TITLE\")
)

(end-notes)












</pre></font></td></tr></table> <p> The templates from above are all found in the zip files, which represents the LENO directory structure.
       The zip file is discussed below. <p> For templates of note pages, please consult the meta examples referred under the individual
       note page forms. <p> We have made a  <a href = \"template-zip-dir.zip\">template of the LENO directory structure</a>  (zip file)
       This is a very useful starting point of a new set of lectures. Enjoy! <p>")) ((kind "manual-section") (section-title "Cross Reference Support") (section-body "It is often useful and necessary to refer from one piece of LENO information to another.
             In this section we will document the various cross reference facilities supported by LENO. 
             The functions in this section return URLs to other pages generated by LENO.<p>Recall the <a href = \"#cross-references\">cross-references</a> clause of LENO, the purpose of which is to define cross references
             to other note pages, or to external material. The cross-references clause is a constituent of a note-page.")) ((kind "manual-page") (title "slide-url") (form (slide-url lecture-id slide-id)) (description "Return a URL of particular slide page in a particular lecture") (parameters (parameter "lecture-id" "A symbol which identifies a lecture") (parameter "slide-id" "A symbol which identifies a note pages in the lecture - corresponds to the first parameter of note-page")) (misc "Use this form in an anchor tag, such as the LAML a-tag")) ((kind "manual-page") (title "note-url") (form (note-url lecture-id slide-id)) (description "Return a URL of particular note page in a particular lecture") (parameters (parameter "lecture-id" "A symbol which identifies a lecture") (parameter "slide-id" "A symbol which identifies a note pages in the lecture - corresponds to the first parameter of note-page")) (misc "Use this form in an anchor tag, such as the LAML a-tag")) ((kind "manual-page") (title "book-url") (form (slide-url lecture-id . slide-id)) (description "Return a URL of particular aggregated lecture. The slide-id parameter is optional. 
              If it is supplied,
              return a URL to a particular place in the aggregated lecture") (parameters (parameter "lecture-id" "A symbol which identifies a lecture") (parameter "slide-id" "A symbol which identifies a note pages in the lecture - corresponds to the first parameter of note-page")) (misc "Use this form in an anchor tag, such as the first parameter of an LAML a-tag")) ((kind "manual-page") (title "exercise-formulation-url") (form (exercise-formulation-url lecture-id page-id presentation-kind exercise-number)) (description "Return a URL to an exercise formulation in a given lecture, and on a given page. 
               It is possible to return a URL to either the slide formulation or the note formulation of the exercise (depending
               on the presentation-kind parameter). Exercise-number refers to a particular exercise number on the 
               page. If there is only one exercise on this page, exercise-number will be 1.
               Notice that this function does not use the exercise-id parameter; Rather it identifies the exercise
               by means of the lecture, page (slide-id) and which number of exercise on the page. 
              ") (parameters (parameter "lecture-id" "A symbol which identifies the lecture") (parameter "page-id" "A symbol which identifies a page in a lecture - corresponds to the first parameter of note-page") (parameter "presentation-kind" "One of the symbols 'slide or 'note, giving the kind of presentation addressed by the URL") (parameter "exercise-number" "An integer giving the number of the exericise to address with the resulting URL")) (cross-references (internal-references "Relevant LENO clauses" "exercise" "formulation") (internal-references "Similar function" "exercise-formulation-url-given-id")) (misc "Use this form in an anchor tag, such as the first parameter of an LAML a-tag")) ((kind "manual-page") (title "exercise-formulation-url-given-id") (form (exercise-formulation-url-given-id lecture-id exercise-id . presentation-kind)) (description "Return a URL to an exercise formulation identified with exercise-id, located in a particular lecture. 
               Presentation-kind determines the kind of presentation addressed by the URL (optional parameter which defaults to note).
               This function finds the page and the exercise-number on the page via the internally generated -.exc files.
               Thus, this function assumes that the -.exc file of lecture-id exists. If not, this function
               returns \"???\" and issues a warning.") (parameters (parameter "lecture-id" "A symbol which identifies the lecture") (parameter "exercise-id" "A symbol identifying the lecture - correspond to the first parameter of the exercise clause") (parameter "presentation-kind" "One of the symbols 'slide or 'note, giving the kind of presentation addressed by the URL")) (pre-condition "") (cross-references (internal-references "Relevant LENO clauses" "exercise" "formulation") (internal-references "Similar function" "exercise-formulation-url")) (misc "Use this form in an anchor tag, such as the first parameter of an LAML a-tag")) ((kind "manual-page") (title "exercise-solution-url") (form (exercise-solution-url lecture-id page-id exercise-number)) (description "Return a URL to an exercise solution in a particular lecture, on a particular page.  
               The URL always addresses a note-size solution - there is no slide style solution page generated.
               The parameter exericse-number addresses a particular exercise on a note page. If there is only one
               exercise on the page, exercise-number will be one") (parameters (parameter "lecture-id" "A symbol which identifies the lecture") (parameter "page-id" "A symbol which identifies a page in a lecture - corresponds to the first parameter of note-page") (parameter "exercise-number" "An integer giving the number of the exericise to address with the resulting URL")) (cross-references (internal-references "Relevant LENO clauses" "exercise" "solution")) (misc "Use this form in an anchor tag, such as the first parameter of an LAML a-tag")) ((kind "manual-section") (section-title "Show and speak functionality") (section-body "In this section we describe the show and speak functionality of LENO. This facility provides
    for an automatically progressing lectures with the lecturer's speaker sound.<p>The LENO clauses related to the facility is described elsewhere. Here we will focus
    on the practical procedure for recording and arranging the sound clips in reation to
    a LENO material.<p>At the overall level, we can observe that the show and speak facility is based on 
    the following:<p><ol><li>To each note page there is one or more corresponding sound clips<li>On each note page we specify the sequencing and timing of the sound clips using a show-and-speak clause<li>The sound clips are brought to the appropriate sound files by means of a LENO utility tool, 
      found in the LENO source program directory's util brach.<li>A number of variables control the show and speak facility: show-and-speak?, default-showing-time, sound-source, 
      default-show-and-speak-clause, speak-url-prefix, and speak-file-prefix.</ol><p>The practical procedure for making and arranging a set of sound files is the following:<p><ol><li>Record the sounds, preferably on files 1.wav, 2.wav, etc.<li>Specify the sounds in the leno source, using (show-and-speak ...) clauses<li>Be sure that (define show-and-speak? #t)<li>Process the leno file, thus defining the internal/&lt;lecture-id&gt;.spk file.<li>Arrange that the sound clips are properly named. This is done by running a copying program on the sound clips which
     implements the naming conventions. An <a href = \"../util/sound-copy.laml\">example</a> of the program is found in the util directory in the leno directory of LAML.<li>Process the leno file again.</ol><p>As another practical information, the processing of a LENO laml file with show-and-speak clauses (and with show-and-speak? set
    to #t) generates a -.spk file in the internal directory. This file holds all the book keeping information about
    the show-and-speak clauses in a LENO laml file.")) ((kind "manual-section") (section-title "Some relevant functions from the LENO implementation") (section-body "In the rest of this document we describe a number of useful functions from the LENO implementation, including a number of practical utility functions.
     This portion of the manual has been extracted automatically from the Scheme source file of the LENO implementation
     by means of the Schemedoc tool.
  ")))