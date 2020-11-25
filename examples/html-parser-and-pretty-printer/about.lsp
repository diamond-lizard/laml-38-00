(html-tree
   (declaration
      "DOCTYPE"
      "HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"")
   (tree
      (tag start "html" lang "en")
      (tree
         (tag start "head")
         (tag
            start-end
            "meta"
            http-equiv
            "Content-Type"
            content
            "text/html; charset=ISO-8859-1")
         (tree (tag start "title") "About the HTML 4 Specification")
         (tag start-end "link" rel "previous" href "cover.html")
         (tag start-end "link" rel "next" href "intro/intro.html")
         (tag start-end "link" rel "contents" href "cover.html#toc")
         (tag
            start-end
            "link"
            rel
            "STYLESHEET"
            href
            "style/default.css"
            type
            "text/css"))
      (tree
         (tag start "body")
         (tree
            (tag start "div" class "navbar" align "center")
            "&nbsp;"
            (tree (tag start "a" href "cover.html") "previous")
            "&nbsp; "
            (tree (tag start "a" href "intro/intro.html") "next")
            "&nbsp; "
            (tree (tag start "a" href "cover.html#minitoc") "contents")
            "&nbsp; "
            (tree (tag start "a" href "index/elements.html") "elements")
            "&nbsp; "
            (tree (tag start "a" href "index/attributes.html") "attributes")
            "&nbsp; "
            (tree (tag start "a" href "index/list.html") "index")
            (tag start-end "hr"))
         (tree
            (tag start "h1" align "center")
            (tree (tag start "a" name "h-1") "1")
            (tree (tag start "a" name "about") "About the HTML 4 Specification"))
         (tree
            (tag start "div" class "subtoc")
            (tree (tag start "p") (tree (tag start "strong") "Contents"))
            (tree
               (tag start "ol")
               (tree
                  (tag start "li")
                  (tree
                     (tag start "a" class "tocxref" href "#h-1.1")
                     "How the specification is organized"))
               (tree
                  (tag start "li")
                  (tree
                     (tag start "a" class "tocxref" href "#h-1.2")
                     "Document conventions")
                  (tree
                     (tag start "ol")
                     (tree
                        (tag start "li")
                        (tree
                           (tag start "a" class "tocxref" href "#h-1.2.1")
                           "Elements and attributes"))
                     (tree
                        (tag start "li")
                        (tree
                           (tag start "a" class "tocxref" href "#h-1.2.2")
                           "Notes and examples"))))
               (tree
                  (tag start "li")
                  (tree
                     (tag start "a" class "tocxref" href "#h-1.3")
                     "Acknowledgments")
                  (tree
                     (tag start "ol")
                     (tree
                        (tag start "li")
                        (tree
                           (tag start "a" class "tocxref" href "#h-1.3.1")
                           "Acknowledgments for the current revision"))))
               (tree
                  (tag start "li")
                  (tree
                     (tag start "a" class "tocxref" href "#h-1.4")
                     "Copyright Notice"))))
         (tree
            (tag start "h2")
            (tree (tag start "a" name "h-1.1") "1.1")
            "How the specification is organized")
         (tree
            (tag start "p")
            "This specification is divided into the following sections:")
         (tree
            (tag start "dl")
            (tree
               (tag start "dt")
               (tree
                  (tag start "strong")
                  "Sections 2 and 3: Introduction to HTML 4"))
            (tree
               (tag start "dd")
               "The introduction describes HTML's place in the scheme of the World Wide Web, provides a brief history of the development of HTML, highlights what can be done with HTML 4, and provides some HTML authoring tips. "
               (tree
                  (tag start "p")
                  "The brief SGML tutorial gives readers some understanding of HTML's relationship to SGML and gives summary information on how to read the HTML Document Type Definition (DTD)."))
            (tree
               (tag start "dt")
               (tree
                  (tag start "strong")
                  "Sections 4 - 24: HTML 4 reference manual"))
            (tree
               (tag start "dd")
               "The bulk of the reference manual consists of the HTML language reference, which defines all elements and attributes of the language. "
               (tree
                  (tag start "p")
                  "This document has been organized by topic rather than by the grammar of HTML. Topics are grouped into three categories: structure, presentation, and interactivity. Although it is not easy to divide HTML constructs perfectly into these three categories, the model reflects the HTML Working Group's experience that separating a document's structure from its presentation produces more effective and maintainable documents.")
               (tree
                  (tag start "p")
                  "The language reference consists of the following information:")
               (tree
                  (tag start "ul")
                  (tree
                     (tag start "li")
                     (tree
                        (tag start "p")
                        "What "
                        (tree
                           (tag start "a" href "charset.html#doc-char-set")
                           "characters")
                        "may appear in an HTML document."))
                  (tree
                     (tag start "li")
                     (tree
                        (tag start "p")
                        "Basic "
                        (tree (tag start "a" href "types.html") "data types")
                        "of an HTML document."))
                  (tree
                     (tag start "li")
                     (tree
                        (tag start "p")
                        "Elements that govern the structure of an HTML document, including "
                        (tree (tag start "a" href "./struct/text.html") "text")
                        ", "
                        (tree
                           (tag start "a" href "./struct/lists.html")
                           "lists")
                        ", "
                        (tree
                           (tag start "a" href "./struct/tables.html")
                           "tables")
                        ", "
                        (tree
                           (tag start "a" href "./struct/links.html")
                           "links")
                        ", and "
                        (tree
                           (tag start "a" href "./struct/objects.html")
                           "included objects, images, and applets")
                        "."))
                  (tree
                     (tag start "li")
                     (tree
                        (tag start "p")
                        "Elements that govern the presentation of an HTML document, including "
                        (tree
                           (tag start "a" href "./present/styles.html")
                           "style sheets")
                        ", "
                        (tree
                           (tag start "a" href "./present/graphics.html")
                           "fonts, colors, rules, and other visual presentation")
                        ", and "
                        (tree
                           (tag start "a" href "./present/frames.html")
                           "frames for multi-windowed presentations")
                        "."))
                  (tree
                     (tag start "li")
                     (tree
                        (tag start "p")
                        "Elements that govern interactivity with an HTML document, including "
                        (tree
                           (tag start "a" href "./interact/forms.html")
                           "forms for user input")
                        "and "
                        (tree
                           (tag start "a" href "./interact/scripts.html")
                           "scripts for active documents")
                        "."))
                  (tree
                     (tag start "li")
                     (tree
                        (tag start "p")
                        "The SGML formal definition of HTML:")
                     (tree
                        (tag start "ul")
                        (tree
                           (tag start "li")
                           "The "
                           (tree
                              (tag start "a" href "./sgml/sgmldecl.html")
                              "SGML declaration of HTML")
                           ".")
                        (tree
                           (tag start "li")
                           "Three DTDs: "
                           (tree
                              (tag start "a" href "./sgml/dtd.html")
                              "strict")
                           ", "
                           (tree
                              (tag start "a" href "./sgml/loosedtd.html")
                              "transitional")
                           ", and "
                           (tree
                              (tag start "a" href "./sgml/framesetdtd.html")
                              "frameset")
                           ".")
                        (tree
                           (tag start "li")
                           "The "
                           (tree
                              (tag start "a" href "./sgml/entities.html")
                              "list of character references")
                           ".")))))
            (tree (tag start "dt") (tree (tag start "strong") "Appendixes"))
            (tree
               (tag start "dd")
               "The first appendix contains information about "
               (tree
                  (tag start "a" href "appendix/changes.html")
                  "changes from HTML 3.2")
               "to help authors and implementors with the transition to HTML 4, and "
               (tree
                  (tag start "a" href "appendix/changes.html#19980424")
                  "changes from the 18 December 1997 specification")
               ". The second appendix contains "
               (tree
                  (tag start "a" href "appendix/notes.html")
                  "performance and implementation notes")
               ", and is primarily intended to help implementors create user agents for HTML 4.")
            (tree (tag start "dt") (tree (tag start "strong") "References"))
            (tree
               (tag start "dd")
               "A list of normative and informative references.")
            (tree (tag start "dt") (tree (tag start "strong") "Indexes"))
            (tree
               (tag start "dd")
               "Three indexes give readers rapid access to the definition of key "
               (tree (tag start "a" href "./index/list.html") "concepts")
               ", "
               (tree (tag start "a" href "./index/elements.html") "elements")
               "and "
               (tree
                  (tag start "a" href "./index/attributes.html")
                  "attributes")
               "."))
         (tree
            (tag start "h2")
            (tree (tag start "a" name "h-1.2") "1.2")
            "Document conventions")
         (tree
            (tag start "p")
            "This document has been written with two types of readers in mind: authors and implementors. We hope the specification will provide authors with the tools they need to write efficient, attractive, and accessible documents, without over-exposing them to HTML's implementation details. Implementors, however, should find all they need to build conforming user agents.")
         (tree
            (tag start "p")
            "The specification may be approached in several ways:")
         (tree
            (tag start "ul")
            (tree
               (tag start "li")
               (tree
                  (tag start "p")
                  (tree (tag start "strong") "Read from beginning to end.")
                  "The specification begins with a general presentation of HTML and becomes more and more technical and specific towards the end."))
            (tree
               (tag start "li")
               (tree (tag start "strong") "Quick access to information.")
               "In order to get information about syntax and semantics as quickly as possible, the online version of the specification includes the following features: "
               (tree
                  (tag start "ol")
                  (tree
                     (tag start "li")
                     "Every reference to an element or attribute is linked to its definition in the specification. Each element or attribute is defined in only one location.")
                  (tree
                     (tag start "li")
                     "Every page includes links to the indexes, so you never are more than two links away from finding the definition of an "
                     (tree (tag start "a" href "index/elements.html") "element")
                     "or "
                     (tree
                        (tag start "a" href "index/attributes.html")
                        "attribute")
                     ".")
                  (tree
                     (tag start "li")
                     (tree
                        (tag start "p")
                        "The front pages of each section of the language reference manual extend the initial table of contents with more detail about that section.")))))
         (tree
            (tag start "h3")
            (tree (tag start "a" name "h-1.2.1") "1.2.1")
            "Elements and attributes")
         (tree
            (tag start "p")
            "Element names are written in uppercase letters (e.g., BODY). Attribute names are written in lowercase letters (e.g., lang, onsubmit). Recall that in HTML, element and attribute names are case-insensitive; the convention is meant to encourage readability.")
         (tree
            (tag start "p")
            "Element and attribute names in this document have been marked up and may be rendered specially by some user agents.")
         (tree
            (tag start "p")
            "Each attribute definition specifies the type of its value. If the type allows a small set of possible values, the definition lists the set of values, separated by a bar (|).")
         (tree
            (tag start "p")
            "After the type information, each attribute definition indicates the case-sensitivity of its values, between square brackets (\"[]\"). See the section on "
            (tree (tag start "a" href "types.html#case") "case information")
            "for details.")
         (tree
            (tag start "h3")
            (tree (tag start "a" name "h-1.2.2") "1.2.2")
            "Notes and examples")
         (tree
            (tag start "p")
            "Informative notes are emphasized to stand out from surrounding text and may be rendered specially by some user agents.")
         (tree
            (tag start "p")
            "All examples illustrating "
            (tree (tag start "a" href "conform.html#deprecated") "deprecated")
            "usage are marked as \"DEPRECATED EXAMPLE\". Deprecated examples also include recommended alternate solutions. All examples that illustrates illegal usage are clearly marked \"ILLEGAL EXAMPLE\".")
         (tree
            (tag start "p")
            "Examples and notes have been marked up and may be rendered specially by some user agents.")
         (tree
            (tag start "h2")
            (tree (tag start "a" name "h-1.3") "1.3")
            (tree (tag start "a" name "acknowledgments") "Acknowledgments"))
         (tree
            (tag start "p")
            "Thanks to everyone who has helped to author the working drafts that went into the HTML 4 specification, and to all those who have sent suggestions and corrections.")
         (tree
            (tag start "p")
            "Many thanks to the Web Accessibility Initiative task force (WAI HC group) for their work on improving the accessibility of HTML and to T.V. Raman (Adobe) for his early work on developing accessible forms.")
         (tree
            (tag start "p")
            "The authors of this specification, the members of the "
            (tree
               (tag
                  start
                  "span"
                  class
                  "index-inst"
                  title
                  "HTML Working Group::members of")
               (tree
                  (tag start "a" name "idx-HTML_Working_Group")
                  "W3C HTML Working Group"))
            ", deserve much applause for their diligent review of this document, their constructive comments, and their hard work: John D. Burger (MITRE), Steve Byrne (JavaSoft), Martin J. D&uuml;rst (University of Zurich), Daniel Glazman (Electricit&eacute; de France), Scott Isaacs (Microsoft), Murray Maloney (GRIF), Steven Pemberton (CWI), Robert Pernett (Lotus), Jared Sorensen (Novell), Powell Smith (IBM), Robert Stevahn (HP), Ed Tecot (Microsoft), Jeffrey Veen (HotWired), Mike Wexler (Adobe), Misha Wolf (Reuters), and Lauren Wood (SoftQuad).")
         (tree
            (tag start "p")
            "Thank you Dan Connolly (W3C) for rigorous and bountiful input as part-time editor and thoughtful guidance as chairman of the HTML Working Group. Thank you Sally Khudairi (W3C) for your indispensable work on press releases.")
         (tree
            (tag start "p")
            "Thanks to David M. Abrahamson and Roger Price for their careful reading of the specification and constructive comments.")
         (tree
            (tag start "p")
            "Thanks to Jan K&auml;rrman, author of "
            (tree
               (tag start "a" href "http://www.tdb.uu.se/~jan/html2ps.html")
               "html2ps")
            "for helping so much in creating the Postscript version of the specification.")
         (tree
            (tag start "p")
            "Of particular help from the W3C at Sophia-Antipolis were Janet Bertot, Bert Bos, Stephane Boyera, Daniel Dardailler, Yves Lafon, H&aring;kon Lie, Chris Lilley, and Colas Nahaboo (Bull).")
         (tree
            (tag start "p")
            "Lastly, thanks to Tim Berners-Lee without whom none of this would have been possible.")
         (tree
            (tag start "h3")
            (tree (tag start "a" name "h-1.3.1") "1.3.1")
            "Acknowledgments for the current revision")
         (tree
            (tag start "p")
            "Many thanks to Shane McCarron for tracking errata for this revision of the specification.")
         (tree
            (tag start "h2")
            (tree (tag start "a" name "h-1.4") "1.4")
            (tree (tag start "a" name "copyright") "Copyright Notice"))
         (tree
            (tag start "p")
            "For information about copyrights, please refer to the "
            (tree
               (tag
                  start
                  "a"
                  href
                  "http://www.w3.org/Consortium/Legal/ipr-notice#Copyright")
               "W3C Intellectual Property Notice")
            ", the "
            (tree
               (tag
                  start
                  "a"
                  href
                  "http://www.w3.org/Consortium/Legal/copyright-documents-19990405.html")
               "W3C Document Notice")
            ", and the "
            (tree
               (tag
                  start
                  "a"
                  href
                  "http://www.w3.org/Consortium/Legal/copyright-software-19980720.html")
               "W3C IPR Software Notice")
            ".")
         (tree
            (tag start "div" class "navbar" align "center")
            (tag start-end "hr")
            (tree (tag start "a" href "cover.html") "previous")
            "&nbsp; "
            (tree (tag start "a" href "intro/intro.html") "next")
            "&nbsp; "
            (tree (tag start "a" href "cover.html#minitoc") "contents")
            "&nbsp; "
            (tree (tag start "a" href "index/elements.html") "elements")
            "&nbsp; "
            (tree (tag start "a" href "index/attributes.html") "attributes")
            "&nbsp; "
            (tree (tag start "a" href "index/list.html") "index")))))

