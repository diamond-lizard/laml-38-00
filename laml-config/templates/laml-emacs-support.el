
; In Windows the .emacs file is normally in c:/
; In Unix the .emacs file is in normally located in the root of your home directory.

; Loads the LAML emacs support via the file laml.el. 
; Also load some dired extensions and structural regular expressions:

(load "@laml-dir@laml") 
(load "@laml-dir@emacs-support/structural-regular-expressions") 
(load "@laml-dir@emacs-support/dired-extensions") 


; -----------------------------------------------------------------------------------
; Top level contribution to the Emacs tools menu:

(define-key global-map [menu-bar tools menu-forms-sep-tools]
    '("----"))

(define-key global-map [menu-bar tools laml]
    (cons "Laml" (make-sparse-keymap "Laml")))

; ------------------------------------------------------------------------------------
; Loading of Emacs support of LAML subsystems and tools.
; You can eliminate the Emacs support by putting a semicolon comment in front of
; one or several of the following load commands.


; Loads the Scheme Elucidator 2 Emacs support.
; Establishes the Elucidator keyboard shortcuts (C-e ...) and the elucidator menu items
; in Tools > LAML > Elucidator.
@elucidator-comment-glyph@(load "@laml-dir@styles/xml-in-laml/elucidator-2/elucidator")  

; Load the Emacs support for LAML SchemeDoc
@schemedoc-comment-glyph@(load "@laml-dir@styles/xml-in-laml/schemedoc-2/schemedoc") 

; Loads the LENO emacs support.
; Establishes the LENO keyboard shortcuts (C-l ...) and the LENO menu item.
@leno-comment-glyph@(load "@laml-dir@styles/lecture-notes/leno")

; Load the LAML photo show emacs support.
@photoshow-comment-glyph@(load "@laml-dir@styles/xml-in-laml/photo-show/photo-show")

; Load the LAML chords emacs support. This supports a song format with chords.
@chords-comment-glyph@(load "@laml-dir@styles/xml-in-laml/chords/chords")

; Load the MIDI LAML emacs support. This supports MIDI editing and MIDI
; programming in Scheme with LAML.
@midi-comment-glyph@(load "@laml-dir@styles/xml-in-laml/midi/midi")

; Load some miscellaneous tools - just loosely connected with LAML.
@laml-misc-tools-glyph@(load "@laml-dir@emacs-support/laml-misc-tools")

; Load the LAML keybinding.
(load "@laml-dir@emacs-support/laml-key-menu-bindings") 