;=>man/svg-extensions.sdoc

;; .schemedoc-dependencies  "man/svg-extensions" "xml-in-laml/mirrors/man/svg11-mirror"

;;;; .title       The SVG Extension Library
;;;; .author      Kurt NÅ¯rmark
;;;; .affiliation Department of Computer Science, Aalborg University
;;;; This library provides a number of SVG abstractions on top of the SVG mirror library.
;;;; The primary abstractions are related to drawing of graph-like structures, composed of
;;;; nodes and edges.  
;;;; There exists a set of <a href="../../examples/svg-extensions/index.html">examples</a> which accompany
;;;; the paper <a href = "http://www.cs.auc.dk/~normark/laml/papers/svg-open-2007/paper.html"> A Graph Library Extension of SVG</a>.

; General XML-in-LAML settings:
(set! compact-end-tag-rendering? #f)
(set! use-empty-tags-for-elements-without-contents #t)


; Determines the currently used animation type.
; One of the symbols none, step-buttons-reveal, step-buttons-walk-through, node-emphasize, or edge-emphasize.
; May also be a list of these symbols.
(define current-animation-type 'none)

; An additional propertype for the auto animation type.
; The time before the first step.
; (define auto-animation-type-start 0)

; An additional propertype for the auto animation type.
; The number of seconds per step.
; (define auto-animation-type-seconds-per-step 1)


;;; SVG configuration constants.
;;; A number of constants. Most of the constant details of the graph library extension of SVG.

;; The version of SVG to be used in this library.
;; Either svg10 or svg11 (a symbol). The given version must be supported by LAML 
;; (the DTD must be parsed, and the mirror of the XML in Scheme must be generated).
;; Use the value svg11.
(define svg-language 'svg11)

;; Recommended list of attributes of the svg element in SVG 1.1 which should always be used.
;; The list contains the attributes version, baseProfile, xmlsn, and xmlsn:xlink. 
(define standard-svg-1-1-element-attributes
  (list 'version "1.1"
        'baseProfile "full"
        'xmlns "http://www.w3.org/2000/svg"
        'xmlns:xlink "http://www.w3.org/1999/xlink"))

;; The color used to emphasize a node or edge, during the supported animations.
(define emphasis-color "red")

;; The color used for control buttons.
(define button-color "grey")

;; The time periode used to reveal an explanation during animation.
(define expl-dur "1s")

;; The time period used to reveal a node during animation
(define node-dur "1s")

;; The time period used to reveal an edge during animation
(define edge-dur "1s")

;; The time period used to move the token on an edge during animation
(define edge-move-dur "3s")

;; The time periode for disappearance of some element during animation.
(define disappear-dur "0.5s")

(define infinite 1000000)

;;; Graph Abstractions.
;;; The major SVG abstractions of this library allows convenient drawing of graphs, in terms of (text) nodes and edges.

;; Draw an SVG graph.  
;; The outer abstraction of an an SVG graph.
;; The contentes of this elements is the svg-node and svg-edge elements together with an optional explanations clause. 
;; (Notice plural form of explanations. Explanations are only used together with animated graphs).
;; The nodes, edges, and the explanations form can be given in arbitrary order.
;; It is good style, however, to pass two separate lists of nodes and edges to svg-graph.
;; This abstraction is mainly syntactic sugar for an SVG g (group) element.
;; .form (svg-graph . xml-contents-and-attributes)
;; .attribute from-step implied The initial step number. Used only when the graph is animated.
;; .attribute to-step implied The final step number. Used only when the graph is animated.
;; .attribute button-x implied The x coordinate of the animation control buttons. Used only when the graph is animated.
;; .attribute button-y implied The y coordinate of the animation control buttons (bottom of triangle). Used only when the graph is animated.
;; .internal-references "possible embedded forms" "svg-node" "empty-svg-node"  "svg-composite-node" "svg-edge" "svg-edge-broken" "explanations"
(define svg-graph 
 (xml-in-laml-abstraction
   (lambda (cont attr)
     (let* ((from-step (as-number (defaulted-get-prop 'from-step attr 0)))   ; svg-graph attributes
            (to-step (as-number (defaulted-get-prop 'to-step attr 0)))
            (button-x (as-number (defaulted-get-prop 'button-x attr 0)))
            (button-y (as-number (defaulted-get-prop 'button-y attr 24)))
            
            ; Information from explanations:
            (explanations-ast (traverse-and-collect-first-from-ast cont (ast-of-type? 'element-name "explanations") id-1))
            (explanation-font-size (if explanations-ast (ast-attribute explanations-ast 'font-size 20) #f))
            (explanation-x (if explanations-ast (as-number (ast-attribute explanations-ast 'x 100)) #f))
            (explanation-y (if explanations-ast (as-number (ast-attribute explanations-ast 'y 24 )) #f))
            (explanation-width (if explanations-ast (as-number (ast-attribute explanations-ast 'width 500)) #f))
            (explanation-height (if explanations-ast (as-number (ast-attribute explanations-ast 'height 50)) #f))
            (explanation-list 
             (if explanations-ast 
              (traverse-and-collect-all-from-ast explanations-ast (ast-of-type? 'element-name "explanation")
                 (lambda (expl-ast)
                   (list (as-number (ast-attribute expl-ast 'step)) (ast-subtrees expl-ast))))
              '()))
            (explanation-clause 
              (cond ((and explanations-ast 
                       (or (animation-includes? 'step-buttons-reveal) (animation-includes? 'step-buttons-walk-through) 
                           (animation-includes? 'step-buttons-walk-through-edge-motion)))
                        (make-explanation-clause explanation-list explanation-x explanation-y explanation-width explanation-height
                                           explanation-font-size from-step to-step))
                    ((and explanations-ast (animation-includes? 'auto))
                        (make-explanation-clause-auto explanation-list explanation-x explanation-y explanation-width explanation-height
                                           explanation-font-size from-step to-step))
                   (else '())))

            (animation-control 
              (cond ((or (animation-includes? 'step-buttons-reveal) (animation-includes? 'step-buttons-walk-through) (animation-includes? 'step-buttons-walk-through-edge-motion))
                       (make-animation-control-clause button-x button-y from-step to-step))
                    (else '())))

            (g-attributes (property-subset attr '()))  ; the empty list of attribute
            (g-cont   ; Content without explanations
              (filter (lambda (cnt) (not (and (ast? cnt) (equal? (ast-element-name cnt) "explanations")))) cont))
           )
    (g (open-arrow-def 20 15)  (open-diamond-def 30 22.5) (filled-arrow-def 20 15)  (filled-diamond-def 30 22.5)
       animation-control explanation-clause
       g-cont g-attributes)
   ))
   (required-implied-attributes '() '(from-step to-step button-x button-y)
                                      "svg-graph"
    )
   "svg-graph"
   svg-language))




; Define the triangular backward and forward control. 
; Place first button at x,y.
(define (make-animation-control-clause x y from-step to-step)
   (let ((y (- y 8)))  ; for nicer alignment with explanation
    (list 
     (g   ; FIRST BUTTON

       ; forward triangle:
      (let ((x (+ x 50))
           )

         (triangle x (- y 12)  (+ x 24) y x (+ y 12) 
           'fill button-color 'id (animation-forward-button-name from-step) 'css:visibility "visible"
           (show-setting-upon (+ from-step 1) 'backward) ; click on (+ from-step 1) button in backward direction will
                                                         ; make this triangle visible
           (hide-setting-upon from-step 'forward)        ; click on from-step (this) button in forward direction will
                                                         ; hide this button 
          )))

     ; MIDDLE SECTION BUTTONS - initially all hidden
     (map (lambda (step)
             (g
               ; backward triangle:
               (triangle x y  (+ x 24) (- y 12) (+ x 24) (+ y 12) 
                         'fill button-color 'id (animation-backward-button-name step) 'css:visibility "hidden"
                 (show-setting-upon (- step 1) 'forward) (show-setting-upon (+ step 1) 'backward)                         
                 (hide-setting-upon step 'forward) (hide-setting-upon step 'backward) 
              )

              ; forward triangle:
              (let ((x (+ x 50)))
                 (triangle x (- y 12)  (+ x 24) y x (+ y 12) 
                           'fill button-color 'id (animation-forward-button-name step) 'css:visibility "hidden"
                   (show-setting-upon (- step 1) 'forward) (show-setting-upon (+ step 1) 'backward)                         
                   (hide-setting-upon step 'forward) (hide-setting-upon step 'backward)                         
               ))

            ))
           (number-interval (+ from-step 1) to-step)) ; (- to-step 1)

     (g ; LAST BUTTON
        ; backward triangle:
        (triangle x y  (+ x 24) (- y 12) (+ x 24) (+ y 12) 
                 'fill button-color 'id (animation-backward-button-name (+ to-step 1)) 'css:visibility "hidden"
                 (show-setting-upon to-step 'forward) ; (- to-step 1)
                 (hide-setting-upon (+ to-step 1) 'backward)
        )

     )
   )))

; set clause that causes controls to be shown when at step in direction.
(define (show-setting-upon step direction)
  (set 'attributeType "CSS" 
                'attributeName "visibility" 'to "visible"
                'begin 
                (string-append
                    (cond ((eq? direction 'forward) (animation-forward-button-name step))
                          ((eq? direction 'backward) (animation-backward-button-name step))
                          (else (laml-error "show-setting-upon: direction must be either forward or backward" direction)))
                     "." "click") 'fill "freeze"))

(define (hide-setting-upon step direction) 
  (set 'attributeType "CSS" 
                'attributeName "visibility" 'to "hidden"
                'begin 
                (string-append
                    (cond ((eq? direction 'forward) (animation-forward-button-name step))
                          ((eq? direction 'backward) (animation-backward-button-name step))
                          (else (laml-error "show-setting-upon: direction must be either forward or backward" direction)))
                     "." "click") 'fill "freeze"))


;; Draws a graph node with embedded textual contents within a given rectangular bounding box.
;; The graph node is placed at (x,y) relative to the lc locator attribute.
;; More specifically, the point determined by the lc locator attribute (defaulted to the center of the node) is placed at (x,y).
;; The size of the rectangular bounding box is, in part, controlled by the min-width and min-height attributes, and in part by the required size of the embedded textual node contents. 
;; The drawn shape is controlled by shape-path-function (which must draw a closed figure inside the given bounding box).  
;; There exists a number of predefined shape path functions (see below).
;; The element contents passed to svg-node becomes the textual contents (the label) of the node.
;; .form (svg-node shape-path-function x y . xml-contents-and-attributes)
;; .parameter x the x coordinate of the node (in pixels, without units)
;; .parameter y the y coordinate of the node (in pixels, without units)
;; .parameter shape-path-function A function that draws the boundary of the svg node. A shape function accepts five required parameters: (shape-path-function x y w h . rest-attributes). (x,y) is the upper left corner of the bounding box. w is the horizontal width and h is the vertical height. The rest-attributes is SVG attributes passed on to the path. See more about shape path functions in a separate section of the documentation.
;; .attribute lc implied A locator (a two letter string) which determines the node's position relative to the given x and y parameters.\
;;                       The first letter defines the horizontal location (l,c,r), left, center, or right.\
;;                       The second leter defines the vertical location (t,c,b), top, center, or bottom.\
;;                       Notice that in the vertical dimension, top has a lower y coordinate that bottom. (This may be slightly confusing).
;;                       Defaults to "cc", which means that the given (x,y) coordinate denotes the center.\  
;;                       As an example, the locator "lt" places the left top corner of the rectangle at (x,y).
;; .attribute id implied An id to be part of the underlying rect element instance. The id can also be used in svg-edge for exact identification of the from-node and to-node.
;; .attribute font-size implied The font size of the text which is embeded in the node.
;; .attribute font-style implied      Native SVG attribute propagated to the node text.
;; .attribute text-color implied The color of the text in the rectangle
;; .attribute text-align implied A locator that defines the alignment of the textual label within the rectangular bounding box of the node. A two letter string. Defaults to "cc".\
;;                               The first letter defines the horizontal alignment (l,c,r). The second leter defines the vertical alingment (t,c,b). 
;; .attribute bg-color implied    The background color of the rectangle.
;; .attribute min-width implied   The minimum width (pixels, no units) of the rectangle. You can supply this attribute if the automatically computed width of the node turns out to be inappropriate.
;; .attribute min-height implied  The minimum height (pixels, no units) of the rectangle. You can supply this attribute if the automatically computed height of the node turns out to be inappropriate.
;; .attribute delta-width implied   A compensating delta width (pixels, no units) added to the computed width. Use this attribute to fine tune the node width.
;; .attribute delta-height implied  A compensating delta height (pixels, no units) added to the computed height. Use this attribute to fine tune the node height.
;; .attribute stroke implied      Native SVG attribute propagated to the boundary shape (including the mandatory visible or invisible rectangle)
;; .attribute stroke-width implied      Native SVG attribute propagated to the boundary shape (including the mandatory visible or invisible rectangle)
;; .attribute stroke-dasharray implied      Native SVG attribute propagated to the boundary shape (including the mandatory visible or invisible rectangle)
;; .attribute stroke-offset implied      Native SVG attribute propagated to the boundary shape (including the mandatory visible or invisible rectangle)
;; .attribute rx implied      Native SVG attribute propagated to the boundary shape (including the mandatory visible or invisible rectangle)
;; .attribute ry implied      Native SVG attribute propagated to the boundary shape (including the mandatory visible or invisible rectangle)
;; .attribute ldx implied     A delta x used to move the label text relative to its default position. The default value of ldx is 0.
;; .attribute ldy implied     A delta y used to move the label text relative to its default position. The default value of ldy is 0.
;; .attribute step implied    The step number of this node. Only used when the surrounding graph is animated.
;; .attribute steps implied    The step numbers of this node. Comma separated. Used instead of singular step. 
;; .internal-references "simple alternative" "empty-svg-node"
;; .internal-references "shape path functions" "shape-path-functions"
(define svg-node
 (xml-in-laml-positional-abstraction 3 0
   (lambda (shape-path-function x y cont attr)
     (let* ((id (defaulted-get-prop 'id attr #f))
            (font-size (as-number (defaulted-get-prop 'font-size attr 30)))     ; Abstraction specific attributes
            (font-family (defaulted-get-prop 'font-family attr "times-roman"))  ; "courier-new"
            (text-color (defaulted-get-prop 'text-color attr "black"))
            (text-align (defaulted-get-prop 'text-align attr "cc"))
            (bg-color (defaulted-get-prop 'bg-color attr "white"))
            (locator (defaulted-get-prop 'lc attr "cc"))
            (min-width (as-number (defaulted-get-prop 'min-width attr 0)))
            (min-height (as-number (defaulted-get-prop 'min-height attr 0)))
            (delta-width (as-number (defaulted-get-prop 'delta-width attr 0)))
            (delta-height (as-number (defaulted-get-prop 'delta-height attr 0)))
            (step (as-number (defaulted-get-prop 'step attr 0)))
            (rect-attributes (property-subset attr '(stroke-width stroke stroke-dasharray stroke-offset opacity stroke-opacity rx ry)))   ; SVG attributes
            (text-attributes (property-subset attr '(font-style)))

            (label-dx (as-number (defaulted-get-prop 'ldx attr 0)))
            (label-dy (as-number (defaulted-get-prop 'ldy attr 0)))

            (w (+ (max (measured-text-width cont font-size font-family) min-width) delta-width))     ; Width and height
            (h (+ (max (measured-text-height cont font-size font-family) min-height) delta-height))

            (displacement-vector (rectangle-adjustment locator w h))
            (dx (car displacement-vector))
            (dy (cdr displacement-vector))

            (cr-x (+ x dx))    ; Calculated x and y coordinates of rectangle
            (cr-y (+ y dy))

            (text-x-y-clause (text-x-y cr-x cr-y w h font-size text-align label-dx label-dy))
            (group-animation-clause
             (let* ((step-from (as-number (defaulted-get-prop 'step-from attr step)))    ; step-from .. step-to:  reaveal in this interval
                    (step-to   (as-number (defaulted-get-prop 'step-to attr infinite)))  
                   )
              (if (> step-from step-to) (laml-error "step-from must be less than or equal to step-to" step-from step-to))
              (cond ((animation-includes? 'step-buttons-reveal) ; only step, not steps
                       (list
                        (if (> step-from 0) 
                            (list 'css:visibility "visible" 'css:opacity 0)
                            (list 'css:visibility "visible" 'css:opacity 1))
                        ; going forward:
                       (animate 'attributeType "CSS" 'attributeName "opacity" ; APPEARING
                                (list 'from 0 'to 1) 'dur edge-dur  'fill "freeze" 
                                'begin (string-append (animation-forward-button-name step-from) "." "click"))
                       (animate 'attributeType "CSS" 'attributeName "opacity" ; DISAPPEARING
                                (list 'from 1 'to 0) 'dur edge-dur  'fill "freeze" 
                                'begin (string-append (animation-forward-button-name step-to) "." "click"))

                       ; going backward:
                       (animate 'attributeType "CSS" 'attributeName "opacity" ; APPEARING
                                (list 'from 0 'to 1) 'dur disappear-dur  'fill "freeze" 
                                'begin (string-append (animation-backward-button-name (+ step-to 1)) "." "click"))
                       (animate 'attributeType "CSS" 'attributeName "opacity" ; DISAPPEARING
                                (list 'from 1 'to 0) 'dur disappear-dur  'fill "freeze" 
                                'begin (string-append (animation-backward-button-name (+ step-from 1)) "." "click"))
                        ))

                     ((animation-includes? 'auto)
                       (let ((start-time (as-number (second current-animation-type)))
                             (seconds-pr-step (as-number (third current-animation-type))))
                        (list
			 (if (> step-from 0) 
			     (list 'css:visibility "visible" 'css:opacity 0)
			     (list 'css:visibility "visible" 'css:opacity 1))
					; going forward:
			 (animate 'attributeType "CSS" 'attributeName "opacity"	; APPEARING
				  (list 'from 0 'to 1) 'dur edge-dur  'fill "freeze" 
				  'begin (+ start-time (* step-from seconds-pr-step)))

			 )))
                    (else '()))))
            (rect-animation-clause-node-emphasize 
              (cond ((animation-includes? 'node-emphasize)
                       (list
                         (animate 'attributeType "XML" 'attributeName "fill" 'from bg-color 'to emphasis-color 'dur node-dur 
                                  'begin "mouseover" 'fill "freeze")
                         (animate 'attributeType "XML" 'attributeName "fill" 'from emphasis-color 'to bg-color 'dur disappear-dur
                                  'begin "mouseout" 'fill "freeze")
                       ))
                    (else '())))
            (rect-animation-clause-buttons-walk-through
             (let* ((steps-given (as-number-list (defaulted-get-prop 'steps attr ""))) ; the empty list if steps not supplied.
                    (steps (cond ((not (null? steps-given)) steps-given)
                                (else (list step)))))
               (cond ((or (animation-includes? 'step-buttons-walk-through) (animation-includes? 'step-buttons-walk-through-edge-motion))
                      (map (lambda (step)
                             (list
                              (node-emphasize bg-color (animation-forward-button-name step))

                              (if (>= step 1) 
                                  (node-deemphasize bg-color (animation-forward-button-name (+ step 1)))
                                  '())

                              (node-deemphasize bg-color (animation-backward-button-name (+ step 1)))

                              (if (>= step 1) 
                                  (node-emphasize bg-color (animation-backward-button-name (+ step 2)))
                                  '())
                              )) steps))
                     (else '())))
            
           ))
       (g group-animation-clause
          (shape-path-function cr-x cr-y w h 
             rect-attributes                           ; LAML flattens all lists passed as rest parameters to shape-path-function
             'stroke-width "1" 'stroke "black" 'fill bg-color (if id (list 'id id) '()) 
              rect-animation-clause-node-emphasize rect-animation-clause-buttons-walk-through)
          (text  text-attributes 'font-family font-family 'font-size font-size
                 'stroke text-color 'color text-color 'fill text-color
                 text-x-y-clause 
                 cont
                 )
          )))
    (required-implied-attributes '() '(id font-size font-family text-color text-align bg-color lc min-width min-height delta-width delta-height
                                       stroke-width stroke stroke-dasharray stroke-offset rx ry font-style step steps step-from step-to)
                                      "svg-node"
    )
    "svg-node"
    svg-language))



;(define shaped-svg-node 
; (xml-in-laml-positional-abstraction 3 0
;   (lambda (x y shape-path-fn cont attr)
;     ...)))

;; An invisible SVG node with empty textual content. Empty nodes are often useful for source or target of svg-edges.
;; A list of empty svg nodes can also be used as the fifth parameter of svg-edge-broken. 
;; .internal-references "non empty svg node" "svg-node" 
;; .internal-references "broken edges" "svg-edge-broken" 
;; .form (empty-svg-node x y . xml-contents-and-attributes)
(define empty-svg-node
 (xml-in-laml-positional-abstraction 2 0
   (lambda (x y cont attr)
      (svg-node rectangular x y "" 'stroke "none" attr))))

; Convert a string of the form "n-m" to the list '(n m). n and m are both positive integers.
; (define (string-to-number-interval str)
;  (map as-number (split-on #\- str)))
  


(define (node-emphasize bg-color-before but)
  (animate 'attributeType "XML" 'attributeName "fill" 
           'from bg-color-before 'to emphasis-color 'dur node-dur 'fill "freeze" 
           'begin (string-append but "." "click")))

(define (node-deemphasize bg-color-after but)
  (animate 'attributeType "XML" 'attributeName "fill" 
           'to bg-color-after 'from emphasis-color 'dur disappear-dur 'fill "freeze" 
           'begin (string-append but "." "click")))

; Given a comma separated string of numbers, such as "1,2,3" return a list of numbers, such as (1 2 3).
(define (as-number-list comma-string)
  (map as-number (string-to-list comma-string (list #\,))))

;; Draw a node with an embeded svg graph.
;; You can think of a composite node as a node that consists of an entire graph.
;; This is a convenient way to compose nodes of nodes.
;; Per default, the surrounding node is rectangular, but you can use the rx and ry attributes to obtain elliplic or circular boundaries.
;; .form (svg-composite-node x y inner-graph . xml-contents-and-attributes)
;; .parameter x x coordinate (in pixels, without units)
;; .parameter y y coordinate (in pixels, without units)
;; .parameter inner-graph The graph that plays the role as the node contents. An instance of svg-graph (underlying an SVG g (group) element).
;; .attribute lc implied A locator (a two letter string) which determines the node's position relative to x and y. First letter defines the horizontal location (l,c,r). The second leter defines the vertical location (t,c,b). 
;; .attribute bg-color implied    The background color of the rectangle.
;; .attribute padding implied A number of blank pixels between the outer rectangle boundary and the embedded graph. 
;; .attribute width implied The width (pixels, no units) of the composite graph node. The height is automatically determined (preserving the aspect ratio of the embedded graph).
;; .attribute stroke implied      Native SVG attribute propagated to the rectangle
;; .attribute stroke-width implied      Native SVG attribute propagated to the rectangle
;; .attribute stroke-dasharray implied      Native SVG attribute propagated to the rectangle
;; .attribute stroke-offset implied      Native SVG attribute propagated to the rectangle
;; .attribute rx implied      Native SVG attribute propagated to the rectangle
;; .attribute ry implied      Native SVG attribute propagated to the rectangle
;; .attribute step implied    The step number of this node. Used only when surrounding graph is animated.
;; .attribute steps implied    The step numbers of this node. Comma separated. Used instead of singular step. 
(define svg-composite-node
 (xml-in-laml-positional-abstraction 3 0
   (lambda (x y inner-graph cont attr)
     (let* (
            (bg-color (defaulted-get-prop 'bg-color attr "white"))      ; Abstraction specific attributes
            (locator (defaulted-get-prop 'lc attr "cc"))
            (padding (as-number (defaulted-get-prop 'padding attr 0)))
            (step (as-number (defaulted-get-prop 'step attr 0)))
            (steps-given (as-number-list (defaulted-get-prop 'steps attr "")))  ; the empty list if steps not supplied.
            (steps (cond ((not (null? steps-given)) steps-given)
                         (else (list step))))

            (rect-attributes (property-subset attr '(stroke-width stroke stroke-dasharray stroke-offset rx ry)))     ; SVG attributes

            (min-max-x-y (find-min-max-x-y inner-graph)) ; a list of four numbers: the minimum left-top coordinate and the maximum right-bottom coordinates
            (inner-x (first min-max-x-y))
            (inner-y (second min-max-x-y))
            (width-of-inner-graph (- (third min-max-x-y) (first min-max-x-y)))     ; Width and height
            (height-of-inner-graph (- (fourth min-max-x-y) (second min-max-x-y)))

            (width-of-composite (as-number (defaulted-get-prop 'width attr width-of-inner-graph)))      ; Given attribute. The exact width of the composite node. Not required.
            (height-of-composite (* height-of-inner-graph (divide width-of-composite width-of-inner-graph)))  ; Calcuated - preserves the aspect ratio.

            (displacement-vector (rectangle-adjustment locator (+ width-of-composite (* 2 padding)) (+ height-of-composite (* 2 padding))))
            (dx (car displacement-vector))
            (dy (cdr displacement-vector))

            (cr-x (+ x dx))    ; Calculated x and y coordinates of rectangle
            (cr-y (+ y dy))

            (group-animation-clause
              (cond ((animation-includes? 'step-buttons-reveal) ; only step, not steps
                       (list
                        (if (> step 0) 
                            (list 'css:visibility "visible" 'css:opacity 0)
                            (list 'css:visibility "visible" 'css:opacity 1))
                        (animate 'attributeType "CSS" 'attributeName "opacity" ; APPEARING
                                 'from 0 'to 1 'dur node-dur 'fill "freeze" 
                                 'begin (string-append (animation-forward-button-name step) "." "click"))
                        (animate 'attributeType "CSS" 'attributeName "opacity" ; DISAPPEARING
                                 'from 1 'to 0 'dur disappear-dur  'fill "freeze"
                                 'begin (string-append (animation-backward-button-name (+ step 1)) "." "click"))
                        ))
                    (else '())))
            (rect-animation-clause-buttons-walk-through
              (cond ((or (animation-includes? 'step-buttons-walk-through) (animation-includes? 'step-buttons-walk-through-edge-motion))
                      (map (lambda (step)
                       (list
                        (node-emphasize bg-color (animation-forward-button-name step))

                        (if (>= step 1) 
                            (node-deemphasize bg-color (animation-forward-button-name (+ step 1)))
                            '())

                        (node-deemphasize bg-color (animation-backward-button-name (+ step 1)))

                        (if (>= step 1) 
                            (node-emphasize bg-color (animation-backward-button-name (+ step 2)))
                            '())
                        )) steps)   
                    )
                    (else '())))

           )
         (g group-animation-clause
          (rect rect-attributes 'x cr-x 'y cr-y
                'width (+ width-of-composite (* 2 padding)) 'height (+ height-of-composite (* 2 padding))
                'stroke-width "1" 'stroke "black" 'fill bg-color
                rect-animation-clause-buttons-walk-through
               )
          (g 'transform (svg-translate (+ cr-x padding) (+ cr-y padding) )
            (g 'transform (svg-scale (divide width-of-composite width-of-inner-graph))  
               (g 'transform (svg-translate (- inner-x) (- inner-y)) inner-graph))))))
          

    (required-implied-attributes '() '(step steps padding width bg-color lc min-width min-height 
                                       stroke-width stroke stroke-dasharray stroke-offset rx ry font-style)
                                      "svg-compositie-node"
    )
    "svg-composite-node"
    svg-language))


;; Draw a line or arrow from one svg-node to another. The contents of svg-edge is used as the textual label of the edge.
;; As a special property, svg-edge either accepts two or four required, positional parameters. 
;; If two parameters are supplied, it must be two svg-nodes (from and two node resp).
;; If four parameters are supplied, it must be (1) the svg from node, (2) the from connector,
;; (3) the svg to node, and (4) the svg to connector.
;; .form (svg-edge from-node from-locator to-node to-locator . xml-contents-and-attributes)
;; .form (svg-edge from-node to-node . xml-contents-and-attributes)
;; .parameter from-node The source node of the edge. An AST as returned by svg-node or svg-composite-node.
;; .parameter from-connector A locator (a two letter string) which determines where the edge leaves the from-node. First letter defines the horizontal location (l,c,r). The second leter defines the vertical location (t,c,b). 
;; .parameter to-node The destination node of the edge. An AST as returned by svg-node or svg-composite-node.
;; .parameter to-connector A locator (a two letter string) which determines where the edge enters the to-node. First letter defines the horizontal location (l,c,r). The second leter defines the vertical location (t,c,b). 
;; .attribute from-connector implied A locator (a two letter string) which determines where the edge leaves the from-node. First letter defines the horizontal location (l,c,r). The second leter defines the vertical location (t,c,b).\
;; Only applicable in the variant of svg-edge that takes two required parameters.\
;; The from-connector defaults to a connector calculated from the mutual positions of from-node and to-node.\
;; The default connector may turn out to be surprising in case the widths of the involved nodes are large.
;; .attribute to-connector implied A locator (a two letter string) which determines where the edge enters the to-node. First letter defines the horizontal location (l,c,r). The second leter defines the vertical location (t,c,b).\ 
;; Only applicable in the variant of svg-edge that takes two required parameters.\
;; The to-connector defaults to a connector calculated from the mutual positions of from-node and to-node.\
;; The default connector may turn out to be surprising in case the widths of the involved nodes are large.
;; .attribute break-path implied A path relative to the source node, which makes it possible to break the edge in a number of segments. The final segment is implicitly given, and it goes from the end of the given break-path to the designated attachment point of the destination node. 
;; .attribute style implied The edge style. Either straight, hv, or hv. Straight means a straight lined edge. hv gives a broken, right angeled edge, first horizontal then vertical. vh gives a broken, right angeled edge, first vertical then horizontal. If break-path is given it overrules the style attribute.
;; .attribute arrow implied Draw an arrow (yes or no). Default is no.
;; .attribute from-id implied The id of the from-node. Useful in case the from-node is a group of two or more svg-nodes. In this case, a particular of these svg-nodes can be addressed.
;; .attribute to-id implied The id of the to-node. Useful in case the to-node is a group of two or more svg-nodes. In this case, a particular of these svg-nodes can be addressed.
;; .attribute step implied    The step number of this edge. Used only when surrounding graph is animated.
;; .attribute steps implied    The step numbers of this edge. Comma separated. Used instead of singular step. 
;; .attribute dx implied      An value to be added to both the source and destination x coordinates of the edge connection points. Defaults to 0.
;; .attribute dy implied      An value to be added to both the source and destination y coordinates of the edge connection points. Defaults to 0.
;; .attribute ldx implied     A delta x used to move the label text relative to its default position. The default value of ldx is 0.
;; .attribute ldy implied     A delta y used to move the label text relative to its default position. The default value of ldy is 0.
;; .attribute font-size implied       The font size of the label text.
;; .attribute font-style implied      The font style of the label text.
;; .attribute text-color implied       The color of the edge label text.
;; .attribute stroke implied  The stroke attribute of the edge, passed directly to the underlying edge path. 
;; .attribute stroke-width implied  The stroke-width attribute of the edge, passed directly to the underlying edge path. 
;; .attribute stroke-dasharray implied  The stroke-dasharray attribute of the edge, passed directly to the underlying edge path. 
;; .attribute stroke-dashoffset implied  The stroke-dashoffset attribute of the edge, passed directly to the underlying edge path. 
;; .attribute stroke-linecap implied  The stroke-line attribute of the edge, passed directly to the underlying edge path. 
;; .attribute dur implied  The duration of an animation of the edge. Only applied for step-buttons-walk-through-edge-motion animation.\
;;    Either a string or an integer (implicitly given as a number of seconds).
;; .internal-references "similar function" "svg-edge-broken"
(define (svg-edge . parameters)
  (cond ((and (>= (length parameters) 4)
                  (ast? (first parameters)) (string? (second parameters))
                  (ast? (third parameters)) (string? (fourth parameters)))
           (apply svg-edge-original parameters))
        ((and (>= (length parameters) 2) (ast? (first parameters)) (ast? (second parameters)))
           (apply svg-edge-new parameters))
        (else (laml-error "svg-edge: Either (svg-edge node con node con ...) or (svg-edge node node ...)."))))

; The variant of svg-edge, with four required, position parameters. No optional connectors can be supplied in attributes.
(define svg-edge-original
 (xml-in-laml-positional-abstraction 4 0
   (lambda (from-node from-connector to-node to-connector cont attr)
     (let* ((arrow (defaulted-get-prop 'arrow attr "no"))   ; Abstraction specific attributes
            (from-id (defaulted-get-prop 'from-id attr #f))
            (to-id (defaulted-get-prop 'to-id attr #f))
            (step (as-number (defaulted-get-prop 'step attr 0)))

            (font-size (as-number (defaulted-get-prop 'font-size attr 30)))     ; Label attributes
            (font-family (defaulted-get-prop 'font-family attr "times-roman"))
            (font-style (defaulted-get-prop 'font-style attr "normal"))  
            (text-color (defaulted-get-prop 'text-color attr "black")) 

            (label-dx (as-number (defaulted-get-prop 'ldx attr 0)))
            (label-dy (as-number (defaulted-get-prop 'ldy attr 0)))

            (edge-style (as-symbol (defaulted-get-prop 'style attr "straight")))
            (dx (as-number (defaulted-get-prop 'dx attr 0)))
            (dy (as-number (defaulted-get-prop 'dy attr 0)))

            (from-pair (x-y-of-node from-node from-id from-connector))
            (x1 (+ (car from-pair) dx)) (y1 (+ (cdr from-pair) dy))
            (to-pair (x-y-of-node to-node to-id to-connector))
            (x2 (+ (car to-pair) dx)) (y2 (+ (cdr to-pair) dy))
            (break-path (defaulted-get-prop 'break-path attr #f))
            (edge-break-segment (defaulted-get-prop 'break-path attr (edge-break-segment edge-style x1 y1 x2 y2)))
            (line-attr (property-subset attr '(stroke stroke-width stroke-dasharray stroke-dashoffset stroke-linecap)))   ; 
            (forward-line-id (unique-symbol "line"))
            (reverse-line-id (unique-symbol "line"))
            (stroke-width (as-number (defaulted-get-prop 'stroke-width attr 1)))
            (stroke (defaulted-get-prop 'stroke attr "black"))
            (duration (ensure-as-seconds (defaulted-get-prop 'dur attr edge-move-dur))) 

            (arrow-clause
               (cond ((or (equal? arrow "yes") (equal? arrow "true") (equal? arrow "filled-triangle") (equal? arrow "triangle")) (list 'marker-end "url(#FilledArrowhead)"))
                     ((equal? arrow "open-triangle") (list 'marker-end "url(#OpenArrowhead)") )
                     ((or (equal? arrow "diamond") (equal? arrow "filled-diamond")) (list 'marker-end "url(#FilledDiamond)"))
                     ((equal? arrow "open-diamond") (list 'marker-end "url(#OpenDiamond)"))
                     (else '())) )

            (group-animation-clause
             (let ((step-from (as-number (defaulted-get-prop 'step-from attr step)))    ; step-from .. step-to:  reaveal in this interval
                   (step-to   (as-number (defaulted-get-prop 'step-to attr infinite)))    
                  )
              (if (> step-from step-to) (laml-error "step-from must be less than or equal to step-to" step-from step-to))
              (cond ((animation-includes? 'step-buttons-reveal)  ; only step, not steps
                      (list  
                       (if (> step-from 0)
                           (list 'css:visibility "visible" 'css:opacity 0)
                           (list 'css:visibility "visible" 'css:opacity 1))

                       ; going forward:
                       (animate 'attributeType "CSS" 'attributeName "opacity" ; APPEARING
                                (list 'from 0 'to 1) 'dur edge-dur  'fill "freeze" 
                                'begin (string-append (animation-forward-button-name step-from) "." "click"))
                       (animate 'attributeType "CSS" 'attributeName "opacity" ; DISAPPEARING
                                (list 'from 1 'to 0) 'dur edge-dur  'fill "freeze" 
                                'begin (string-append (animation-forward-button-name step-to) "." "click"))

                       ; going backward:
                       (animate 'attributeType "CSS" 'attributeName "opacity" ; APPEARING
                                (list 'from 0 'to 1) 'dur disappear-dur  'fill "freeze" 
                                'begin (string-append (animation-backward-button-name (+ step-to 1)) "." "click"))
                       (animate 'attributeType "CSS" 'attributeName "opacity" ; DISAPPEARING
                                (list 'from 1 'to 0) 'dur disappear-dur  'fill "freeze" 
                                'begin (string-append (animation-backward-button-name (+ step-from 1)) "." "click"))

                       ))
                    ((animation-includes? 'auto)
                     (let ((start-time (as-number (second current-animation-type)))
                           (seconds-pr-step (as-number (third current-animation-type))))
                       (list
                        (if (> step-from 0) 
                            (list 'css:visibility "visible" 'css:opacity 0)
                            (list 'css:visibility "visible" 'css:opacity 1))
                       (animate 'attributeType "CSS" 'attributeName "opacity" ; APPEARING
                                (list 'from 0 'to 1) 'dur edge-dur  'fill "freeze" 
                                'begin (+ start-time (* step-from seconds-pr-step))))

                        ))
                    (else '()))))
            (line-animation-clause-edge-emphasize
              (cond ((animation-includes? 'edge-emphasize)
                      (list
                         (animate 'attributeType "XML" 'attributeName "stroke-width" 'from stroke-width 'to (* 4 stroke-width) 
                                  'dur edge-dur 'begin "mouseover" 'fill "freeze")
                         (animate 'attributeType "XML" 'attributeName "stroke-width" 'to stroke-width 'from (* 4 stroke-width)
                                  'dur disappear-dur 'begin "mouseout" 'fill "freeze")

                         (animate 'attributeType "XML" 'attributeName "stroke" 'from stroke 'to emphasis-color 
                                  'dur edge-dur 'begin "mouseover" 'fill "freeze")
                         (animate 'attributeType "XML" 'attributeName "stroke" 'to stroke 'from emphasis-color
                                  'dur disappear-dur 'begin "mouseout" 'fill "freeze")
                       ))
                    (else '())))
            (line-animation-clause-buttons-walk-through
             (let* ((steps-given (as-number-list (defaulted-get-prop 'steps attr ""))) ; the empty list if steps not supplied.
                    (steps (cond ((not (null? steps-given)) steps-given)
                                (else (list step))))
                  )
               (cond ((animation-includes? 'step-buttons-walk-through)
                      (map (lambda (step)
                             (list 
                              (edge-emphasize stroke stroke-width (animation-forward-button-name step))
                              (if (>= step 1) 
                                  (edge-deemphasize stroke stroke-width (animation-forward-button-name (+ step 1)))
                                  '())
                              (edge-deemphasize stroke stroke-width (animation-backward-button-name (+ step 1)))
                              (if (>= step 1)
                                  (edge-emphasize stroke stroke-width (animation-backward-button-name (+ step 2)))
                                  '())
                              )) steps))
                     (else '()))))
            (group-animation-clause-edge-motion
             (let* ((steps-given (as-number-list (defaulted-get-prop 'steps attr ""))) ; the empty list if steps not supplied.
                    (steps (cond ((not (null? steps-given)) steps-given)
                                (else (list step))))
                  )
              (cond ((animation-includes? 'step-buttons-walk-through-edge-motion)
                      (map (lambda (step)
                       (list 
                        (edge-move forward-line-id  (animation-forward-button-name step) duration)
                        (edge-move reverse-line-id  (animation-backward-button-name (+ step 1)) duration)
                        )) steps)
                     )
                    (else '()))))

           )
       (g group-animation-clause group-animation-clause-edge-motion
          (path 'id forward-line-id line-attr 'fill "none"
                'd (am-p x1 y1 (append-path edge-break-segment (al-p x2 y2 (e-p))))
                'stroke stroke 'stroke-width stroke-width                                                               ; defaults due to keep-first attributes in SVG
                arrow-clause line-animation-clause-edge-emphasize line-animation-clause-buttons-walk-through)
          (path 'id reverse-line-id line-attr 'fill "none" 'css:visibility "hidden"
                'd (am-p x2 y2 (append-path edge-break-segment (al-p x1 y1 (e-p)))) 
                'stroke stroke 'stroke-width stroke-width
                arrow-clause line-animation-clause-edge-emphasize line-animation-clause-buttons-walk-through)
          (text 'font-family font-family 'font-size font-size 'font-style font-style
                'stroke text-color 'color text-color 'fill text-color
                'x (+ (+ x1 (divide (- x2 x1) 2)) label-dx) 'y (+ (+ y1 (divide (- y2 y1) 2)) label-dy)
                cont)
       )))

   (required-implied-attributes '() '(from-id to-id arrow stroke-width stroke stroke-dasharray
                                      stroke-linecap stroke-dashoffset step steps step-from step-to
                                      dx dy ldx ldy break-path style font-size font-style text-color dur)
                                "svg-edge"
   )
   "svg-edge"
   svg-language))

; The variant of svg-edge, with two required, position parameters and optional connectors supplied in attributes.
(define svg-edge-new
 (xml-in-laml-positional-abstraction 2 0
   (lambda (from-node to-node cont attr)
    (let* ((from-id (defaulted-get-prop 'from-id attr #f))   ; ids: only in case of composite nodes. Not central. 
           (to-id (defaulted-get-prop 'to-id attr #f))
           (from-connector (defaulted-get-prop 'from-connector attr (default-connection-between from-node from-id to-node to-id)))
           (to-connector   (defaulted-get-prop 'to-connector attr   (default-connection-between to-node to-id from-node from-id))) )
       (svg-edge from-node from-connector to-node to-connector cont attr)))))

; Return a connection point of node, calculated in relation to the (x,y) coordinate of other-node.
; Redefined below.
(define (default-connection-between node node-id other-node other-id)
  (let* ((node-xy (basis-x-y-of-node node node-id))
         (x (car node-xy))  ; (x,y) of bounding rectangle
         (y (cdr node-xy))

         (other-xy (basis-x-y-of-node other-node other-id))
         (ox (car other-xy))
         (oy (cdr other-xy))
        )
    (string-append
      (cond ((= x ox) "c")
            ((> x ox) "l")
            ((< x ox) "r"))
      (cond ((= y oy) "c")
            ((> y oy) "t")
            ((< y oy) "b")))))

; Redefined.
; Return a connection point of node, calculated in relation to the (x,y) coordinate of other-node.
; 45 degree version of the version above.
(define (default-connection-between node node-id other-node other-id)
  (let* ((node-xy (basis-x-y-of-node node node-id))
         (x (car node-xy))
         (y (cdr node-xy))

         (other-xy (basis-x-y-of-node other-node other-id))
         (ox (car other-xy))
         (oy (cdr other-xy))
 
         (xot (- ox x))  ; (ox,oy) translated such that (x,y) is origo
         (yot (- oy y))
        )
    (string-append
      (cond ((and (>= xot (- yot)) (>= xot yot))     "rc")  ; 1
            ((and (<= xot yot)     (>= xot (- yot))) "cb")  ; 2
            ((and (<= xot yot)     (<= xot (- yot))) "lc")  ; 3
            ((and (<= xot (- yot)) (>= xot yot))     "ct")  ; 4
            (else (laml-error "default-connection-between: Should not happen"))))))
   

;; Almost identical with svg-edge. The only difference is that this function requires an extra fifth parameter, name a list of (empty) svg nodes used for edge breaking.
;; Causes a call the svg-edge with an appropriate break-path attribute. Do not pass any explicit break-path attribute to this function. 
;; .form (svg-edge-broke from-node from-locator to-node to-locator node-break-list . xml-contents-and-attributes)
;; .internal-references "similar function" "svg-edge"
(define svg-edge-broken
 (xml-in-laml-positional-abstraction 5 0
   (lambda (from-node from-connector to-node to-connector node-break-list cont attr)
     (let ((break-path (node-list-to-edge-break-path node-break-list)))
       (svg-edge from-node from-connector to-node to-connector cont attr 'break-path break-path)))))


; Given edge-style (a symbol: straight, hv, or vh) return an appropriate edge-break-segment
(define (edge-break-segment edge-style x1 y1 x2 y2)
  (cond ((eq? edge-style 'straight) (e-p))
        ((eq? edge-style 'hv) (rh-p (- x2 x1) (e-p)))
        ((eq? edge-style 'vh) (rv-p (- y2 y1) (e-p)))
        (else (laml-error "edge-break-segment: Unknown edge style" edge-style))))

; Transform a list nodes to an (absolute) SVG path through the center points of the nodes in svg-node-list.
(define (node-list-to-edge-break-path node-list)
  (cond ((null? node-list) (e-p))
        (else (let* ((node (car node-list))
                     (x-y (x-y-of-node node #f "cc")))
                (al-p (car x-y) (cdr x-y)
                      (node-list-to-edge-break-path (cdr node-list)))))))

(define (edge-move line-id but-name duration)
 (let ((anim-id (string-append "anim-" line-id)))
  (circle 'r 8 'cx 0 'cy 0 'fill emphasis-color 'stroke emphasis-color 'css:visibility "hidden"
    (set 'attributeType "CSS" 'attributeName "visibility" 'to "visible" 'begin (string-append but-name "." "click"))
    (animateMotion 'id anim-id 'dur duration 'rotate "auto" ; 'repeatCount "indefinite"  'fill "freeze"
      'begin (string-append but-name "." "click")  ;  
      (mpath 'xlink:href (string-append "#" line-id)))
    (set 'attributeType "CSS" 'attributeName "visibility" 'to "hidden" 'begin (string-append anim-id ".end")))))


(define (edge-emphasize stroke-before stroke-width-before but-name)
 (list
  (animate 'attributeType "XML" 'attributeName "stroke" 'from stroke-before 'to emphasis-color 
           'dur edge-dur 'fill "freeze" 
           'begin (string-append but-name "." "click"))
  (animate 'attributeType "XML" 'attributeName "stroke-width" 'from stroke-width-before 'to (max 5 (* 2 stroke-width-before)) 
           'dur edge-dur 'fill "freeze" 
           'begin (string-append but-name "." "click"))))

(define (edge-deemphasize stroke-before stroke-width-before but-name)
 (list
   (animate 'attributeType "XML"  'attributeName "stroke" 'to stroke-before 
            'from emphasis-color 'dur disappear-dur 'fill "freeze" 
            'begin (string-append but-name "." "click"))
   (animate 'attributeType "XML"  'attributeName "stroke-width" 'to stroke-width-before 
            'from (max 5 (* 2 stroke-width-before)) 'dur disappear-dur 'fill "freeze" 
            'begin (string-append but-name "." "click"))
 )
)
  


; This function positions the text in a rect-node.
; Calculcate the text x, y coordinate relative to the rectangles x,y coordinates (cr-x, cr-y), width w, and height h,
; and the text alignment locator.
; ldx and ldy reflex manual fine tuning of the text position
; Return an svg attribute list of x, y and text-anchor.
(define (text-x-y cr-x cr-y w h font-size text-align-locator ldx ldy)
  (let* ((hl (horizontal-locator text-align-locator))
         (vl (vertical-locator text-align-locator))
         (wh (divide w 2))
         (hh (divide h 2))
         (hor-contribution 
           (cond ((eq? hl 'c) (list 'x (+ (+ cr-x wh) ldx) 'text-anchor "middle"))
                 ((eq? hl 'l) (list 'x (+ (+ cr-x 5) ldx) 'text-anchor "start"))
                 ((eq? hl 'r) (list 'x (+ (+ cr-x w (- 5)) ldx) 'text-anchor "end"))))
         (ver-contribution 
           (cond ((eq? vl 'c) (list 'y (+ (+ cr-y hh (+ (quotient font-size 2))  (- 5)) ldy)))
                 ((eq? vl 't) (list 'y (+ (+ cr-y font-size) ldy)))
                 ((eq? vl 'b) (list 'y (+  (+ cr-y h (- 5)) ldy)))))
        )
    (append hor-contribution ver-contribution)))

; -----------------------------------------------------------------------------------------------------
; defs clauses with various arrow definition.
; Not a good solution to the arrow problem, because all arrows will be identical (and of the same color).  

(define (open-arrow-def w h)
  (defs
    (marker 'id "OpenArrowhead"
            'viewBox "0 0 10 10" 'refX "10" 'refY "5"
            'stroke "black" 'stroke-width "1" 'fill "white"
            'markerUnits "userSpaceOnUse" 
            'markerWidth w 'markerHeight h
            'orient "auto" 'preserveAspectRatio "none"
            (path 'd "M 0 0 L 10 5 L 0 10 z"))))

(define (filled-arrow-def w h)
  (defs
    (marker 'id "FilledArrowhead"
            'viewBox "0 0 10 10" 'refX "10" 'refY "5" 'fill "black"
            'fill "black"
            'markerUnits "userSpaceOnUse" ; "strokeWidth"   
            'markerWidth w 'markerHeight h
            'orient "auto" 'preserveAspectRatio "none"
            (path 'd "M 0 0 L 10 5 L 0 10 z"))))

(define (open-diamond-def w h)
  (defs
    (marker 'id "OpenDiamond"
            'viewBox "0 -5 10 10" 'refX "10" 'refY "0" 
            'stroke "black" 'stroke-width "1" 'fill "white"
            'markerUnits "userSpaceOnUse" ; "strokeWidth"   
            'markerWidth w 'markerHeight h
            'orient "auto" 'preserveAspectRatio "none"
            (path 'd "M 0 0 L 5 -5 L 10 0 L 5 5 z"))))

(define (filled-diamond-def w h)
  (defs
    (marker 'id "FilledDiamond"
            'viewBox "0 -5 10 10" 'refX "10" 'refY "0" 
            'fill "black"
            'markerUnits "userSpaceOnUse" ; "strokeWidth"   
            'markerWidth w 'markerHeight h
            'orient "auto" 'preserveAspectRatio "none"
            (path 'd "M 0 0 L 5 -5 L 10 0 L 5 5 z"))))

; -----------------------------------------------------------------------------------------------------

; Extract the x, y, width, and height attributes of a rect AST.
; Useful in relation to definition of edges of SVG graphs.
; Return the cons pair of x and y coordinates of the connection point of node-ast relative to the connector con
(define (x-y-of-node node-ast id con)
 (letrec ((node-interesting? (lambda (node-ast) 
                               (and ((ast-of-type? 'element-name "rect") node-ast) 
                                    (equal? (ast-attribute node-ast 'id #f) id))))) 
  (let* ((rect-ast-1 (find-first-ast node-ast "rect"))
         (rect-ast-2 (traverse-and-collect-first-from-ast node-ast node-interesting? id-1))
         (rect-ast (if (and id rect-ast-2) ; still experimental
                       rect-ast-2
                       rect-ast-1))
         (rect-attr (ast-attributes rect-ast))

         (x (as-number (get-prop 'x rect-attr)))
         (y (as-number (get-prop 'y rect-attr)))
         (w (as-number (get-prop 'width rect-attr)))
         (h (as-number (get-prop 'height rect-attr)))
 
         (hl (horizontal-locator con))
         (vl (vertical-locator con))
        )

       (cons
        (cond ((eq? hl 'c) (+ x (divide w 2)))
              ((eq? hl 'l) x)
              ((eq? hl 'r) (+ x w)))
        (cond ((eq? vl 'c) (+ y (divide h 2)))
              ((eq? vl 't) y)
              ((eq? vl 'b) (+ y h)))))))

; A variant of x-y-of-node without use of connectors. 
; Returns a cons pair of (x,y) of node-ast
(define (basis-x-y-of-node node-ast id)
 (letrec ((node-interesting? (lambda (node-ast) 
                               (and ((ast-of-type? 'element-name "rect") node-ast) 
                                    (equal? (ast-attribute node-ast 'id #f) id))))) 
  (let* ((rect-ast-1 (find-first-ast node-ast "rect"))
         (rect-ast-2 (traverse-and-collect-first-from-ast node-ast node-interesting? id-1))
         (rect-ast (if (and id rect-ast-2) ; still experimental
                       rect-ast-2
                       rect-ast-1))
         (rect-attr (ast-attributes rect-ast))

         (x (as-number (get-prop 'x rect-attr)))
         (y (as-number (get-prop 'y rect-attr)))
         (w (as-number (get-prop 'width rect-attr)))
         (h (as-number (get-prop 'height rect-attr)))
        )
     (cons (+ x (divide w 2)) (+ y (divide h 2))))))

; Return delta adjustment to x and y from given-x and given-y relative to the locator.
(define (rectangle-adjustment locator-string width height)
  (let ((hl (horizontal-locator locator-string))
        (vl (vertical-locator locator-string))
       )
    (cons
     (cond ((eq? hl 'c) (- (divide width 2)))
           ((eq? hl 'l) 0)
           ((eq? hl 'r) (- width)))
     (cond ((eq? vl 'c) (- (divide height 2)))
           ((eq? vl 't) 0)
           ((eq? vl 'b) (- height))))))

; Locate all rectangles in svg-graph and find the minimal bounding box that surrounds it.
; Return a list of four coordinates (top-left-x, top-left-y, bottom-right-x, bottom-right-y).
(define (find-min-max-x-y svg-graph-ast)
  (letrec ((reduce-right (lambda (f lst)
                           (if (null? (cdr lst))
                               (car lst)
                               (f (car lst) 
                                  (reduce-right f (cdr lst)))))) 

           (x-y-w-h (lambda (rect-ast)
                      (let ((rect-attr (ast-attributes rect-ast)))
                        (list (as-number (get-prop 'x rect-attr)) (as-number (get-prop 'y rect-attr)) (as-number (get-prop 'width rect-attr)) (as-number (get-prop 'height rect-attr))))))

           (min-list (lambda (lst) (reduce-right min lst)))
           (max-list (lambda (lst) (reduce-right max lst)))
           )
    (let* ((rect-list (find-asts svg-graph-ast "rect"))
           (x-y-w-h-list (map x-y-w-h rect-list))
           (x1-y1-x2-y2-list (map (lambda (x-y-w-y-entry) (list (first x-y-w-y-entry) (second x-y-w-y-entry)
                                                           (+ (first x-y-w-y-entry) (third x-y-w-y-entry)) (+ (second x-y-w-y-entry) (fourth x-y-w-y-entry))))
                                  x-y-w-h-list))

           )
      (list 
       (min-list (map first x1-y1-x2-y2-list)) ; min left top x,y
       (min-list (map second x1-y1-x2-y2-list))
       (max-list (map third x1-y1-x2-y2-list)) ; max right bottom x, y
       (max-list (map fourth x1-y1-x2-y2-list))))))

; ------------------------------------------------------------------
; Locator functions.

(define (horizontal-locator locator-string)
  (let ((ls (as-string locator-string)))
    (check-locator-string! ls)
    (as-symbol (string-ref ls 0))))

(define (vertical-locator locator-string)
  (let ((ls (as-string locator-string)))
    (check-locator-string! ls)
    (as-symbol (string-ref ls 1))))

(define (locator-string? x)
  (and (string? x)
       (= 2 (string-length x))
       (let ((a (string-ref x 0))
             (b (string-ref x 1)))
         (and (or (eqv? a #\c) (eqv? a #\l) (eqv? a #\r))
              (or (eqv? b #\c) (eqv? b #\t) (eqv? b #\b))))))

(define (check-locator-string! ls)
  (if (not (locator-string? ls))
      (laml-error "Invalid locator string:" ls ". " "First char either c, l, or t. Second char either c, t, or b.")))


; Does animation, as defined in the global variable current-animation-type, prescribe animation-kind (a symbol)
(define (animation-includes? animation-kind)
  (cond ((symbol? current-animation-type)
          (eq? animation-kind current-animation-type))
        ((list? current-animation-type)
          (memq animation-kind current-animation-type))
        (else (laml-error "animation-includes?: animation-kind must be a symbol or a list of symbols:" animation-kind))))


;; A container of a number of explanation clauses. The explanations clause is a possible constituent of an svg-graph.
;; .form (explanations explanation-list)
;; .attribute x implied The x coordinate of the explanation text
;; .attribute y implied The y coordinate of the explanation text
;; .attribute font-size implied The font size of the explanation text
(define explanations
  (xml-in-laml-abstraction
    (lambda (cont attr)
      (make-ast "explanations" cont attr 'double svg-language))
    (required-implied-attributes '() '(x y font-size width height)
                                      "explanations"
    )
    "explanations"
    svg-language))

;; A single explanation clause. Holds a step attribute and the textual explanational content.
;; A given step should be explained at most once within an explanations clause.
;; A list of explanation clauses can be nested in an explanations clause, which is a possible constituent of svg-graph.
;; Defined as an XML-in-LAML abstraction.
;; .form (explanation 'step s explanation-text)
(define explanation
  (xml-in-laml-abstraction
    (lambda (cont attr)
      (make-ast "explanation" cont attr 'double svg-language))
    (required-implied-attributes '() '(step)
                                      "explanation"
    )
    "explanation"
    svg-language))

; Make the (overlapping) explanations at x,y. Explanation-list is a list of (step explanation) entries.
(define (make-explanation-clause explanation-list x y width height font-size from-step to-step)
 (let ((explanation-list-completed (complete-explanation-list explanation-list to-step)))
  (map 
   (lambda (step-expl)
    (let ((step (car step-expl))
          (expl (cadr step-expl))
          (text-color "black")
         )
     (g 'css:visibility "visible" 'css:opacity (if (= step 0) 1 0)
        (show-explanation-upon step 'forward)
        (show-explanation-upon (+ 2 step) 'backward)
        (hide-explanation-upon (+ step 1) 'forward)
        (hide-explanation-upon (+ step 1) 'backward)

;        (text-box 'x x 'y y 'width ? 'height ?
;                  'font-family "times-roman" 'font-size font-size
;                  'stroke text-color 'color text-color 'fill text-color
;                  expl)

        (text  'font-family "times-roman" 'font-size font-size
                 'stroke text-color 'color text-color 'fill text-color
                 'x x 'y y expl))))
   explanation-list-completed)))

(define (make-explanation-clause-auto explanation-list x y width height font-size from-step to-step)
 (let ((explanation-list-completed (complete-explanation-list explanation-list to-step)))
  (map 
   (lambda (step-expl)
    (let ((step (car step-expl))
          (expl (cadr step-expl))
          (text-color "black")
          (start-time (as-number (second current-animation-type)))
          (seconds-pr-step (as-number (third current-animation-type)))
         )
     (g 'css:visibility "visible" 'css:opacity (if (= step 0) 1 0)
       (animate 'attributeType "CSS" 'attributeName "opacity" 'from 0 'to 1 'dur expl-dur 'fill "freeze" 
		'begin (+ start-time (* step seconds-pr-step)))
       (animate 'attributeType "CSS" 'attributeName "opacity" 'from 1 'to 0 'dur "0.1s" 'fill "freeze" 
                 'begin (+ start-time (* (+ step 1) seconds-pr-step)))
       (text  'font-family "times-roman" 'font-size font-size
                 'stroke text-color 'color text-color 'fill text-color
                 'x x 'y y expl))))
   explanation-list-completed)))

(define (complete-explanation-list explanation-list to-step)
 (let ((sorted-explanation-list (sort-list explanation-list (lambda (x y) (<= (car x) (car y))))))
   (complete-explanation-list-1 sorted-explanation-list 0 to-step)))

(define (complete-explanation-list-1 sorted-explanation-list i to-step)
 (let ((empty-expl ""))
  (cond ((and (> i to-step) (null? sorted-explanation-list)) '())
        ((and (<= i to-step) (null? sorted-explanation-list))
           (cons (list i empty-expl) (complete-explanation-list-1 sorted-explanation-list (+ i 1) to-step)))
        ((= i (car (car sorted-explanation-list)))
           (cons (car sorted-explanation-list) (complete-explanation-list-1 (cdr sorted-explanation-list) (+ i 1) to-step)))
        (else
           (cons (list i empty-expl) (complete-explanation-list-1 sorted-explanation-list (+ i 1) to-step))))))

(define  (show-explanation-upon step direction)
  (animate 'attributeType "CSS" 'attributeName "opacity" 'from 0 'to 1 'dur expl-dur 'fill "freeze" 
                 'begin (string-append 
                         (cond ((eq? direction 'forward) (animation-forward-button-name step))
                               ((eq? direction 'backward) (animation-backward-button-name step))
                               (else (laml-error "show-explanation-upon: direction must be either forward or backward" direction)))
                         "." "click")))

(define  (hide-explanation-upon step direction)
  (animate 'attributeType "CSS" 'attributeName "opacity" 'from 1 'to 0 'dur "0.1s" 'fill "freeze" 
                 'begin (string-append
                         (cond ((eq? direction 'forward) (animation-forward-button-name step))
                               ((eq? direction 'backward) (animation-backward-button-name step))
                               (else (laml-error "hide-explanation-upon: direction must be either forward or backward" direction)))
                         "." "click")))


;;; SVG node shape path functions.
;;; The shape path functions are used as the first parameter to the svg-node function.
;;; The svg-node function calls the shape path function, which is passed as the first parameter to svg-node.
;;; A shape path function is supposed to draw a path that serves as the boundary around the svg node.
;;; The first four parameters of an SVG node shape path function receives a bounding box in terms of x, y, w, and h.
;;; (x,y) is the upper left corner of the bounding box. w is the width (pixels), and h the height (pixels).
;;; Internally a shape path function MUST draw a visible or hidden SVG rect, possibly together with another path inside a group element.
;;; .section-id shape-path-functions

;; A rectangular svg node shape path function.
;; .internal-references "relevant for" "svg-node"
(define (rectangular x y w h . attributes)
  (rect  'x x 'y y 'width w 'height h
          attributes  ; earlier attributes overwrites later attributes - controlled by SVG mirror
         'stroke-width "1" 'stroke "black"))

;; A circular svg node shape path function.
;; .internal-references "relevant for" "svg-node"
(define (circular x y w h . attributes)
  (rect  'x x 'y y 'width w 'height h   'rx (divide w 2) 'ry (divide h 2)   attributes  'stroke-width "1" 'stroke "black"))

;; A diamond svg node shape path function.
;; .internal-references "relevant for" "svg-node"
(define (diamond x y w h . attributes)
  (let* ((hh (divide h 2)) (wh (divide w 2))          ; half height and width
         (sx x) (sy (+ y hh)))       ; diamond start (x,y) coordinates
    (g
      (rect 'css:visibility "hidden" 'x x 'y y 'width w 'height h  attributes  'stroke-width "1" 'stroke "black")  ; hidden boundary rect
      (path attributes 'd (am-p sx sy (rl-p wh (- hh) (rl-p wh hh (rl-p (- wh) hh (rl-p (- wh) (- hh) (e-p))))))))))

;; A triangle svg node shape path function.
;; .internal-references "relevant for" "svg-node"
(define (triangular x y w h . attributes)
  (let* ((hh (divide h 2)) (wh (divide w 2))          ; half height and width
         (sx x) (sy (+ y h)))       ; diamond start (x,y) coordinates
    (g
      (rect 'css:visibility "hidden"  'x x 'y y 'width w 'height h  attributes  'stroke-width "1" 'stroke "black")  ; hidden boundary rect
      (path attributes 'd (am-p sx sy (rl-p wh (- h) (rl-p wh h (rl-p (- w) 0 (e-p)))))))))

;; A cloud svg node shape path function.
;; .internal-references "relevant for" "svg-node"
(define (cloud x y w h . attributes)
  (let* ((h2 (divide h 2)) (w2 (divide w 2))          ; half height and width
         (h4 (divide h2 2)) (w4 (divide w2 2))        ; quarts height and width
         (sx x) (sy (+ y h2))       ; cloud start (x,y) coordinates
         (c (divide (+ w h) 8)) (c2 (divide c 2)) (cm (- c)) (cm2 (- c2))
        )
    (g
      (rect 'css:visibility "hidden" 'x x 'y y 'width w 'height h  attributes  'stroke-width "1" 'stroke "black")  ; hidden boundary rect
      (path attributes
        'd (am-p sx sy 
            (rq-p 0 cm  w4 (- h4)
             (rq-p 0 cm w4 (- h4)
              (rq-p c cm2 w4 h4
               (rq-p c cm w4 h4
                (rq-p c c (- w4) h4
                 (rq-p c c (- w4) h4
                  (rq-p cm2 c (- w4) (- h4)
                   (rq-p cm2 c (- w4) (- h4) 
                    (e-p))))))))))))))

;;; Graph Animations.
;;; Svg graphs can be animated in a number of different ways. 
;;; Use the syntactic form with-animation around an svg-graph form to specify the kind of animation to use.
;;; The following kinds are supported:
;;; <ul>
;;;   <li> none. Do not use any animation at all.
;;;   <li> node-emphasize: Emphasize the node with a particular color when it gets focus with the mouse.
;;;   <li> edge-emphasize: Emphasize the edge with a particular color and thickness when it gets focus with the mouse.
;;;   <li> step-buttons-reveal: 
;;;         The nodes and edges are revealed one after the other, controlled by triangular shaped buttons.
;;;         The step attribute tells when to reveal the graph node or edge.
;;;         A given step attribute value should only appear once with this kind of animation.
;;;         As an alternative to the step attribute, you can use the step-from and step-to attributes to given step interval.
;;;         The node or edge is revealed at step step-to and and hidden at step step-from.
;;;         Several nodes or edges can have the same step value. This leads to simultaneous revealing of these nodes and edges.
;;;   <li> step-buttons-walk-through: The nodes and edges are highlighted in a given order, controlled by triangular shaped buttons. 
;;;        By means of the steps attribute (plural) a given node or edge can be highlighted more than once during the walk through.
;;;        Several nodes or edges can have the same step value. 
;;;   <li> step-buttons-walk-through-edge-motion: The nodes are highlighted in a given order. The edges are animated with a moving token.
;;;        The animation is controlled by triangular shaped buttons. 
;;; </ul>
;;; Within with-animation, you can use node-emphasize and edge-emphasize together. 
;;; You can also use node-emphasize or edge-emphasize (or both) together with step-buttons-reveal and step-buttons-walk-through.
;;; step-buttons-reveal and step-buttons-walk-through cannot be used together.<p>
;;;
;;; The non-animated part of the graph has implicitly assigned step number 0. 
;;; You can Assign step numbers higher than 0 to selected nodes and edges.
;;; Use the svg-graph attributes to from-step and to-step to control the animated step interval. 


;; Set the variable current-animation-type fluidly while evaluating forms.
;; The animation taking place in the svg graph forms depend on animation-type.
;; A syntactic abstraction (macro) intended to be used around an svg-graph form.
;; .form (with-animation animation-type . forms)
;; .parameter animation-type A symbol, or a list of symbols. The following types of animation are supported: \
;;    none,  node-emphasize, edge-emphasize, step-buttons-reveal, step-buttons-walk-through, and step-buttons-walk-through-edge-motion.
(define-syntax with-animation
 (syntax-rules ()
   ((with-animation animation-type form ...)
    (let ((old-animation-type current-animation-type))
      (set! current-animation-type animation-type) 
      (let ((result (begin form ...)))
        (set! current-animation-type old-animation-type)
        result)))))




; ---------------------------------------------------------------------------------------------------
;;; Transform attribute functions.
;;; SVG uses a little language for values of transform attributes. In this section you will find Scheme
;;; counterparts of such expresssions.

;; The translate expression.
(define (svg-translate tx ty)
  (string-append "translate" "(" (as-string tx) "," (as-string ty) ")"))

;; The scale expression.
;; .form (svg-scale sx [sy])
(define (svg-scale sx . optional-parameter-list)
 (let ((sy (optional-parameter 1 optional-parameter-list sx)))
  (string-append "scale" "(" (as-string sx) "," (as-string sy) ")")))

;; The rotate expression.
;; .form (svg-rotate angle [cx cy])
(define (svg-rotate angle . optional-parameter-list)
 (let ((cx (optional-parameter 1 optional-parameter-list #f))
       (cy (optional-parameter 2 optional-parameter-list #f)))
  (if (and cx cy)
      (string-append "rotate" "(" (as-string angle) "," (as-string cx) "," (as-string cy) ")")
      (string-append "rotate" "(" (as-string angle) ")"))))

;; The skewX expression.
(define (svg-skewX angle)
  (string-append "skewX" "(" (as-string angle) ")"))

;; The skewY expression.
(define (svg-skewY angle)
  (string-append "skewY" "(" (as-string angle) ")"))



; ---------------------------------------------------------------------------------------------------
;;; Path functions.
;;; Functions for definition of SVG paths. 
;;; You can think of the functions as constructors of SVG paths.
;;; The functions in this section can be used as the value of d attributes of the SVG path element. 
;;; The functions model a path as a linear recursive structures for instance in the style of lists in Lisp.
;;; All functions return strings.
;;; As a naming convention, the first letter in the prefix tells if we draw in absolute or relative mode ('a' or 'r').
;;; The next letter in the prefix mimics the type of the path ('l' for line, 'm' for move). This letter corresponds to the (lower case) SVG path letter name.
;;; The suffix of the name is always "-p", which is a short name for "-path".
;;; If you dislike the functions you can use native SVG path strings, or you can program your own set of path constructors.

;; The empty path.
(define (e-p) "")

;; Absolute line path to x,y continued in path.
(define (al-p x y path) (p-exp "L" path x y))

;; Relative line path to x,y continued in path.
(define (rl-p x y path) (p-exp "l" path x y))

;; Absolute horizontal path to x continued in path.
(define (ah-p x path) (p-exp "H" path x))

;; Relative horizontal path to x continued in path.
(define (rh-p x path) (p-exp "h" path x))

;; Absolute vertical path to y continued in path.
(define (av-p y path) (p-exp "V" path y))

;; Relative vertical path to y continued in path.
(define (rv-p y path) (p-exp "v" path y))

;; Relative move to x, y - without drawing. The path is continued in path.
(define (rm-p x y path) (p-exp "m" path x y))

;; Absolute move to x,y - without drawing.  The path is continued in path.
(define (am-p x y path) (p-exp "M" path x y))

;; An elliptic arc from the current point to the point with the relative coordinates (x,y).
;; The path is continued in path.
;; .parameter x the target x coordinate (absolute)
;; .parameter y the target y coordinate (absolute)
;; .parameter x-axis-rotation the rotation of the x axis (degrees).
;; .parameter large-arc? controls if the large arc or small arc is drawn. Boolean or one of 0 or 1.
;; .parameter sweep? if true, draw in positive direction, else draw in negative direction. Boolean or one of 0 or 1.
;; .parameter path The continuation of the arc path.
(define (ra-p rx ry x-axis-rotation large-arc? sweep? x y path)
  (let ((large-arc-number (as-01-boolean large-arc?))
        (sweep-number (as-01-boolean sweep?)))
    (p-exp "a" path rx ry x-axis-rotation large-arc-number sweep-number x y)))

;; An elliptic arc from the current point to the point with the absolute coordinates (x,y).
;; The path is continued in path.
;; See the similar relative function for description of the parameters.
;; .internal-references "similar function" "ra-p"
(define (aa-p rx ry x-axis-rotation large-arc? sweep? x y path)
  (let ((large-arc-number (as-01-boolean large-arc?))
        (sweep-number (as-01-boolean sweep?)))
    (p-exp "A" path rx ry x-axis-rotation large-arc-number sweep-number x y)))

;; Relative quadratic bezier curve from the implicit starting point to the ending point (x,y).
;; (cx, cy) control point of the curve
(define (rq-p cx cy x y path)
  (p-exp "q" path cx cy x y))

;; Absolute quadratic bezier curve from the implicit starting point to the ending point (x,y).
;; (cx, cy) control point of the curve
(define (aq-p cx cy x y path)
  (p-exp "Q" path cx cy x y))

;; Relative quadratic bezier curve from the implicit starting point to the ending point (x,y).
;; The control point is the reflection of the quadratic bezier curve.
(define (rt-p x y path)
  (p-exp "t" path x y))

;; Absolute quadratic bezier curve from the implicit starting point to the ending point (x,y).
;; The control point is the reflection of the quadratic bezier curve.
(define (at-p x y path)
  (p-exp "T" path x y))

;; Relative cubic bezier curve from the implicity starting point to the ending point (x,y).
;; (cx1, cy1) controls the curve at the starting point.
;; (cx2, cy2) controls the curve at the ending point.
(define (rc-p cx1 cy1 cx2 cy2 x y path)
  (p-exp "c" path cx1 cy1 cx2 cy2 x y))

;; Absolute cubic bezier curve from the implicity starting point to the ending point (x,y).
;; (cx1, cy1) controls the curve at the starting point.
;; (cx2, cy2) controls the curve at the ending point.
(define (ac-p cx1 cy1 cx2 cy2 x y path)
  (p-exp "C" path cx1 cy1 cx2 cy2 x y))

;; Relative cubic bezier curve from the implicity starting point to the ending point (x,y). Thus, (x,y) is given relative to the implicit starting point of this curve. 
;; The control point of the implicit starting point is the reflection of the previous cubic bezier curve. 
;; (cx2, cy2) controls the curve at the ending point.
(define (rs-p cx2 cy2 x y path)
  (p-exp "s" path cx2 cy2 x y))

;; Absolute cubic bezier curve from the implicity starting point to the ending point (x,y). 
;; The control point of the implicit starting point is the reflection of the previous cubic bezier curve. 
;; (cx2, cy2) controls the curve at the ending point.
(define (as-p cx2 cy2 x y path)
  (p-exp "S" path cx2 cy2 x y))

;; Closing of path. With this alternative to the (e-p) closing, the path returns to its starting point.
(define (z-p) "Z")



;; Append path p2 to path p1
(define (append-path p1 p2)
  (string-append p1 p2))

; SVG render path
(define (p-exp letter path . coordinates)
  (string-append 
    letter " " 
    (list-to-string (map as-string coordinates) " ") 
    " " path))

; Temporary definition. Strengthen it.
(define (svg-path? x) (string? x))


; ---------------------------------------------------------------------------------------------------

; Misc functions

; Used to prevent rational numbers to appear in SVG, for instance.
(define (divide x y)
  (/ (exact->inexact x) (exact->inexact y)))


(define unique-number 0)

(define (unique-symbol prefix)
  (set! unique-number (+ unique-number 1)) 
  (string-append prefix "-" (as-string unique-number)))


; Generate and return an id of the button that activates step in a button controlled animation.
(define (animation-forward-button-name step-number)
  (string-append "forward-button-id" "-" (as-string step-number)))

(define (animation-backward-button-name step-number)
  (string-append "backward-button-id" "-" (as-string step-number)))

; Generate and return an id of the button text that activates step in a button controlled animation. NOT USED.
(define (animation-button-text-name step-number)
  (string-append "button-text-id" "-" (as-string step-number)))

; ---------------------------------------------------------------------------------------------------

;;; Aditional basic shapes

;; A triangle defined as a path through three points x1, y1, x2, y2, x3, and y3.
;; Attributes and additional content is passed to the underlying path.
;; .form (triangle x1 y1 x2 y2 x3 y3 . cont-and-attr)
(define triangle 
  (xml-in-laml-positional-abstraction 6 0
    (lambda (x1 y1 x2 y2 x3 y3 cont attr)
     (path 'd (am-p x1 y1 (al-p x2 y2 (al-p x3 y3 (z-p)))) cont attr))))


;;; Extended text management.
;;; Not yet completed. Please disregard this function.

;; Type set the textual content of textbox within the box formed by the x, y, width and height attributes.
;; Break the text into lines. It is possible to use em, kbd, and b to achieve italic, keyboard, and bold effects.
;; The elements em, kbd, and b are ad hoc SVG elements, but similar to the XHTML elements of the same name. 
;; .attribute x required  x coordiate of the text box  
;; .attribute y required  y coordiate of the text box  
;; .attribute width required  the with of the text box (pixels)
;; .attribute height required  the height of the text box
;; .attribute font-family implied  the name of the font family. Defaults to times-roman.
;; .attribute font-size implied  the size of the font (in user units). Defaults to 30.
(define text-box
  (xml-in-laml-abstraction
   (lambda (cont attr)
     (let* ((x (get-prop 'x attr))
            (y (get-prop 'y attr))
            (width (get-prop 'width attr))
            (height (get-prop 'width attr))
            (font-family (defaulted-get-prop 'font-family attr "times-roman"))
            (font-size   (as-number (defaulted-get-prop 'font-size attr "30")))
           )
        (do-text-box x y width height font-family font-size cont)
     )
   )
   (required-implied-attributes '(x y width height) '(text-color font-family font-size)
                                 "text-box"
   )
   "text-box"
   svg-language))


(define (do-text-box x y width height font-family font-size text-list)
  (laml-error "STOP")
)

(define text-width-factor 1.9)  ; maybe 1.7

; Determines the width of text-contents, relative to a given font size and familiy.
; Conservative and approximate
; The font-size is given in points.
(define (measured-text-width text-contents font-size font-family)
 (let* ((textual? (textual-contents? text-contents))
        (txt (if textual? (string-of-textual-contents text-contents) #f))
        (basis-width (as-number font-size))  ; A width added as the basis width.
                                             ; Without it nodes with short labels become too narrow.
       )
 
   (if textual? 
       (+ (* (/ font-size text-width-factor) (string-length txt)) basis-width)    ; earlier (quotient font-size 2)
       0)  ; rely on min-width in this case
   ))

; Determines the height of text-contents, relative to a given font size and familiy.
; Conservative and approximate
; The font-size is given in points.
(define (measured-text-height text-contents font-size font-family)
  (+ font-size 10))

; A predicate which determines if x is considered as textual contents. 
(define (textual-contents? x)
  (cond ((string? x) #t)
        ((list? x)
           (not (find-in-list ast? x)))
        (else (laml-error "textual-contents?: Unknown type of parameter:" x))))

; Return the string from x. x can be a string or the content of an XML-in-LAML AST node.
; Precondition: x satisfies the predicate textual-contents?
(define (string-of-textual-contents x)
 (if (string? x)
     x
    (aggregated-ast-cdata-contents-1 x "") ; undocumented function from xml-in-laml
 ))

; ---------------------------------------------------------------------------------------------------------------

(define (ensure-as-seconds x)
  (cond ((number? x) (string-append (as-string x) "s"))
        ((and (string? x) (eqv? #\s (string-ref x (- (string-length x) 1)))) x)
        ((string? x) (string-append x "s"))
        (else (laml-error "ensure-as-seconds: Cannot ensure x as seconds:" x))))