; LAML Midi.
; Parsing of midi file to XML-in-LAML.
; Unparsing XML-in-LAML midi files to binary midi.
; Relies in a cyclic way on midi-laml-processing-lib.scm.
; schemedoc-dependencies  "man/midi-mirror.manlsp" "man/midi-laml-processing-lib.manlsp" 
; .schemedoc-dependencies  "man/midi-mirror.manlsp"
; .source-file-dependencies "midi-laml-processing-lib.scm" 

; Uncomment for debugging and profiling purposes:
; (require (lib "errortrace.ss" "errortrace"))
; (profiling-enabled #t)                                  ; To report profiling results: (output-profile-results #f #t)

; ===============================================================================================================
; Configuration part.

; Which instrument do you prefer.
; Affects currently voice info attributes.
; One of the symbols tyros, motif-xs or pc-sound-card.
; pc-sound-card stands for general-midi.
(define preferred-instrument #f)  ; call read-and-set-preferred-instrument-from-environment before first use.

; ===============================================================================================================
; Template stuff. Temporary.
; Emacs  (setq interactive-laml-mirror-library nil)

; ===============================================================================================================
; CONSTANTS

; A boolean variable that controls if the midi LAML software prints (a few) messages during processing
(define midi-reporting? #f)


; ===============================================================================================================
; Global variables 

(define midi-software-dir (string-append laml-dir "styles/xml-in-laml/midi/"))

(define global-ppqn 1920)
(define global-signature (list 4 4))

; Give info about drum sounds in NoteOn messages
(define info-about-drum-sounds? #f)

; A list of channels considered as drum channels
(define drum-channels '(9 10))

(define (drum-sound-channel? channel-number)
  (memq channel-number drum-channels))

; Of backward compatibility I keep old, erroneous calculations of pitch bend.
; As long as both parsing and unparsing is erroneous, this will give correct midi files.
; Needless to say, you should always use the value #t
(define use-correct-pitch-bend-calculations #t)

; ===============================================================================================================
; LOADING SECTION

(load (string-append laml-dir "lib/" "file-read.scm"))

(lib-load "xml-in-laml/xml-in-laml.scm")
(load (string-append midi-software-dir "mirror/midi.scm"))

; Convenient if you load this software repeatedly into a Scheme prompt:
(set! xml-check-language-overlap? #f)

; ---------------------------------------------------------------------------------------------------------------

(define xml-check-attributes? #f)
(define xml-validate-contents? #f)
(define xml-link-checking #f)
(define xml-check-language-overlap? #f)

; -----------------------------------------------------------------------------
; Integer handling functions.

; Return n. If n > ma, return ma. If n < mi return mi.
(define (between mi ma n)
  (max (min n ma) mi))

; Earlier version:
; (define (to-int x)
;  (truncate (exact->inexact x)))

(define (to-int x)
  (inexact->exact (round (exact->inexact x))))

; Old version. Redefined next.
;(define (as-int-string x)
;  (cond ((number? x) (remove-decimals (as-string x)))
;        (else (as-string x))))

; new as of july 29, 2007:
(define (as-int-string x)
  (as-string (to-int (as-number x))))

(define (remove-decimals num-string)
  (let ((point-pos (find-in-string num-string #\.)))
    (if point-pos
        (if (= point-pos 0) 
            "0"
            (substring num-string 0 point-pos))
        num-string)))



; ===============================================================================================================
; Overall processing functions.

;; Read and parse midi file in midi-file-path and make a corresponding laml source file in laml-file-path. 
;; midi-file-path is assumed to be a full path to the midi file (including file extension).
;; laml-file-path is assumed to be a full path to the generated laml file (in a preexisting directory).
;; Saves the intermediate AST in the variable the-ast for programmatic access.
;; mode is either raw, absTime or deltaTime (a symbol).
;; raw: delta times with NoteOn and NoteOff.
;; absTime: absolute times with durations.
;; deltaTime: delta times with duration.
;; counter-transp is the amount of transposition done to compensate for the transposition made at the keyboard. Provides for recognizable note names. An integer.
;; track-separation can be used on format 0 midi file, not pad files. A boolean.
(define (midi-file-to-laml midi-file-path laml-file-path mode counter-transp track-separation)
  (midi-report "This program converts a binary midi file to an XML-in-LAML source file.")
  (let ((input-file-extension (file-name-extension midi-file-path))
        (transformed-ast (midi-file-to-laml-ast midi-file-path mode counter-transp track-separation)))
    (midi-report "LAML AST has been made. Now rendering AST to a XML-in-LAML source file...")
    (set! the-ast transformed-ast)
    
    (make-midi-laml-file transformed-ast laml-file-path)
  )
)

; A variant of midi-to-laml which constructs a midi laml file for step recording purposes.
; Quantification is not used in this version. The quantification depends on the duration of the recorded notes:
; Recording happens at 100 BPM = 100 quarter notes / 60 sek = 100*480 ticks / 60 s = 800 ticks pr. sec with PPQN = 480.
; Thus, per convention, a single quarter note lasts 800 ticks.
(define (midi-file-to-laml-step-recording midi-file-path laml-file-path mode counter-transp track-separation quantification)
  (midi-report "This program converts a binary midi file to an XML-in-LAML source file.")
  (let ((input-file-extension (file-name-extension midi-file-path))
        (transformed-ast (as-step-recording-on-motif-nested-variable-timing (midi-file-to-laml-ast midi-file-path mode counter-transp track-separation))))
    (midi-report "LAML AST has been made. Now rendering AST to a XML-in-LAML source file...")
    (set! the-ast transformed-ast)
    
    (make-midi-laml-file transformed-ast laml-file-path)
  )
)


;; Read and parse midi file in midi-file-path and return the corresponding AST.
;; The file extension controls the result, which is either a Standard Midi file, a Yamaha Tyros Multipad format, og a Yamaha Style format.
;; If, however, the last optional parameter enable-transformation is #f, never transform to pad or style format, and never track separate.
;; .parameter mode Either raw, absTime or deltaTime (a symbol).\
;; raw: delta times with NoteOn and NoteOff.\
;; absTime: absolute times with durations.\
;; deltaTime: delta times with duration.\
;; .form (midi-file-to-laml-ast midi-file-path mode counter-transp track-separation [enable-transformation])
;; .parameter midi-file-path The full path to the midi file (including file extension).
;; .parameter mode Either absTime or deltaTime. A symbol.
;; .parameter counter-transp The amount of transposition done to compensate for the transposition made at the keyboard. Provides for recognizable note names. An integer.
;; .parameter track-separation Can be used on format 0 midi file, not pad files. A boolean.
;; .parameter enable-transformation If false, the initial AST of the midi parsing is to transformed subsequently. Defaults to true.
;; .returns An midi LAML AST.
(define (midi-file-to-laml-ast midi-file-path mode counter-transp track-separation . optional-parameter-list)
  (let ((enable-transformation (optional-parameter 1 optional-parameter-list #t)))
    (set! counter-transposition counter-transp)
    (let ((input-file-extension (file-name-extension midi-file-path))
          (midi-str (read-text-file midi-file-path)))
      (if (not (equal? "MThd" (substring midi-str 0 4)))
          (laml-error "You are NOT reading a midi file.  Every midi files starts with 'MThd'.  Processing interrupted."))
      (estimate-processing-time! (string-length midi-str))
      (midi-report "Parsing the binary midi file...")
      (let* ((lst (parse-midi-string midi-str))   ; PARSING binary midi to intermediate (non-AST) list format.
             (non-midi-suffix-str (get-non-midi-suffix-string midi-str lst))
            )
        (midi-time-report "Parsing done")
        (midi-report "Midi parsing done. Now constructing and transforming the XML AST...")
        (let* ((ast (midi-file-as-list-to-ast lst mode non-midi-suffix-str))  ; various requested TRANSFORMATIONS done by midi-file-as-list-to-ast
               (ast-header (ast-subtree ast "MidiHeader"))
               (midi-format (as-number (ast-attribute ast-header 'format)))
               (transformed-ast 
                  (cond ((not enable-transformation) ast)

                        ; Midi file format 0
                        ((and (= midi-format 0) (or (equal? (downcase-string input-file-extension) "mid") (equal? (downcase-string input-file-extension) "midi")))
                            (if track-separation
                                (track-separated-standard-midi-file-ast ast standard-track-separation-scheme mode)  
                                ast)
                        )
  
                        ; Midi file format 1.   Disregard track-separation
                        ((and (= midi-format 1) (or (equal? (downcase-string input-file-extension) "mid") (equal? (downcase-string input-file-extension) "midi")))
                            ast
                        )
  
                        ; Yamaha Tyros Multipad
                        ((and (equal? (downcase-string input-file-extension) "pad") (or (equal? (as-string mode) "absTime") (equal? (as-string mode) "deltaTime")))  
                            (standard-midi-file-ast-to-multipad-ast ast midi-file-path))
                        ((and (equal? (downcase-string input-file-extension) "pad") (not (equal? (as-string mode) "absTime"))) 
                            (laml-error "Can only deal with multiPads in absTime mode or deltaTime mode" ))
  
                        ; Yamaha Tyros Styles
                        ((and (= midi-format 0) 
                              (member (downcase-string input-file-extension) (list "sty" "pst" "psc" "sst" "prs" "bcs")))
                           (standard-midi-file-ast-to-style-ast ast midi-file-path)
                        )
  
                        (else (laml-error "Unknown file extension of input file." (downcase-string input-file-extension)))))
              )
          (midi-time-report "Transformation done")
          transformed-ast)))))



; Return the part of the binary midi-string which comes after the standard midi contents.
; This may, for instance, be the CASM part of a Yamaha style file.
; Returns a (binary) string.
(define (get-non-midi-suffix-string midi-string parsed-midi-list)
  (let* ((midi-string-length (string-length midi-string))
         (header-length (byte-length-of-header (header-of-smf parsed-midi-list)))
         (track-length-list (map byte-length-of-track  (tracks-of-smf parsed-midi-list)))
         (track-length-sum (accumulate-right + 0 track-length-list)))
    (substring midi-string (+ header-length track-length-sum) midi-string-length)))

(define (byte-length-of-header parsed-header)
  (+ 4  ; "MThd"
     4  ; 4 byte length, always the number 6
     6  ; Header data length
  ))

(define (byte-length-of-track parsed-track)
  (+ 4                                    ; "MTrk"
     4                                    ; 4 byte length
     (length-of-midi-track parsed-track)  ; track data length
  ))



;; Bulk Generation of LAML files from Midi files.
;; Make laml files of all files in midi-file-list (a list of proper file names with extension). 
;; Each file in midi-file-list is assume to reside input-dir (full path, ends in forward slash).
;; Place output files in output-dir (full path, ends in forward slash).
;; mode is either raw, absTime or deltaTime (a symbol).
;; raw: delta times with NoteOn and NoteOff.
;; absTime: absolute times with durations.
;; deltaTime: delta times with duration.
(define (midi-files-to-laml-files input-dir midi-file-list output-dir mode)
  (for-each
    (lambda (f)
       (display-message f)
       (midi-file-to-laml 
         (string-append input-dir f)
         (string-append output-dir (file-name-proper f) "." "midl")
         mode
         0
         #f)
       (display-message "") 
       )
    midi-file-list))

(define (midi-file? path)
  (let ((ext (file-name-extension path)))
    (or (equal? (downcase-string ext) "mid") (equal? (downcase-string ext) "midi"))))

(define (estimate-processing-time! byte-lgt-midi-str)
  (midi-report
    (string-append
      "Length of binary midi-file: " (as-string byte-lgt-midi-str) " bytes" "."))
  (midi-report
    (string-append
      "Estimated processing time: " (as-string (max (to-int (* (/ 2 3) (quotient byte-lgt-midi-str 1000))) 2)) " seconds.")))


; A global variable which references the latest AST processed.
(define the-ast #f)


; ===============================================================================================================
; Standard XML-in-LAML standard action procedures: Make midi file in the midi-temp directory.

(define (StandardMidiFile! ast)
 (let ((mode-string (ast-attribute (ast-subtree ast "MidiHeader") 'mode))
       (source-file-name (source-filename-without-extension))  ; file-name-proper is used to strip y in file names x.y.ext.  source-file-name becomes x.
      )
  (ensure-directory-existence! (startup-directory) "midi-temp") 
; (file-write ast (in-startup-directory "midi-temp/" (string-append source-file-name "." "ast")))   ; commented out December 21, 2010
  (write-text-file
     (standard-midi-file-ast-to-bin (expand-procedural-content-items-in-ast ast))
     (in-startup-directory "midi-temp/" (string-append source-file-name "." "mid")))))

(define (TyrosMultiPad! ast)
 (let* ((mode-string (ast-attribute ast 'mode))                                           ; mode not used in this version.
        (image (ast-attribute ast 'image "S910"))
        (transformed-ast (multipad-ast-to-standard-midi-file-ast (expand-procedural-content-items-in-ast ast))))  ; multipad ast -> standard midi ast
  (ensure-directory-existence! (startup-directory) "midi-temp") 
  (write-text-file
     (standard-midi-file-ast-to-bin transformed-ast)
     (in-startup-directory "midi-temp/"
                           (string-append (source-filename-without-extension)    ; With possible middle name, such as "xx.S384"
                                           "." "mid"))                           ; we use -.mid to provide for playing of the pad
  )))

(define (TyrosStyle! ast)
 (let* ((mode-string (ast-attribute ast 'mode))                                           ; mode not used in this version.
        (image (ast-attribute ast 'image "S910"))
        (extension (ast-attribute ast 'extension "sty"))
        (transformed-ast (style-ast-to-standard-midi-file-ast (expand-procedural-content-items-in-ast ast)))  ; style ast -> standard midi ast
        (extension-path (in-startup-directory "midi-temp/" "extensions"))
       )
  (file-write transformed-ast "c:/lookat-it.lsp")
  (ensure-directory-existence! (startup-directory) "midi-temp") 
  (if (file-exists? extension-path) (delete-file extension-path))
  (write-text-file (string-it (string-append image "." extension)) extension-path)
  (write-text-file   
     (standard-midi-file-ast-to-bin transformed-ast)
     (in-startup-directory "midi-temp/"
                           (string-append (source-filename-without-extension)     ; With possible middle name, such as "xx.S384"
                                           "." "mid")))                           ; we use -.mid to provide for playing of the style
 )
)

; ----------------------------------------------------------------------------------------------------------------------------------------------------------
; Alternative action procedures:

; Action: generate a LAML AST in midi-temp of relative to the startup directory. Extension: ast
(define (StandardMidiFile-laml-ast! ast)
 (let ((source-file-name (source-filename-without-extension))  ; file-name-proper is used to strip y in file names x.y.ext.  source-file-name becomes x.
      )
  (ensure-directory-existence! (startup-directory) "midi-temp")
  (file-write ast  (in-startup-directory "midi-temp/" (string-append source-file-name "." "ast")))))

(define (TyrosMultiPad-laml-ast! ast)
 (let ((source-file-name (source-filename-without-extension))  ; file-name-proper is used to strip y in file names x.y.ext.  source-file-name becomes x.
      )
  (ensure-directory-existence! (startup-directory) "midi-temp")
  (file-write ast  (in-startup-directory "midi-temp/" (string-append source-file-name "." "ast")))))

(define (TyrosStyle-laml-ast! ast)
 (let ((source-file-name (source-filename-without-extension))  ; file-name-proper is used to strip y in file names x.y.ext.  source-file-name becomes x.
      )
  (ensure-directory-existence! (startup-directory) "midi-temp")
  (file-write ast  (in-startup-directory "midi-temp/" (string-append source-file-name "." "ast")))))



; Action: generate a pretty printed source text file in midi-temp of relative to the startup directory. Extension: src
(define (StandardMidiFile-laml-source! ast)
 (let ((source-file-name (source-filename-without-extension))  ; file-name-proper is used to strip y in file names x.y.ext.  source-file-name becomes x.
      )
  (ensure-directory-existence! (startup-directory) "midi-temp")
  (make-midi-laml-file ast (in-startup-directory "midi-temp/" (string-append source-file-name "." "src")))))

(define (TyrosMultiPad-laml-source! ast)
 (let ((source-file-name (source-filename-without-extension))  ; file-name-proper is used to strip y in file names x.y.ext.  source-file-name becomes x.
      )
  (ensure-directory-existence! (startup-directory) "midi-temp")
  (make-midi-laml-file ast (in-startup-directory "midi-temp/" (string-append source-file-name "." "src")))))


(define (TyrosStyle-laml-source! ast)
 (let ((source-file-name (source-filename-without-extension))  ; file-name-proper is used to strip y in file names x.y.ext.  source-file-name becomes x.
      )
  (ensure-directory-existence! (startup-directory) "midi-temp")
  (make-midi-laml-file ast (in-startup-directory "midi-temp/" (string-append source-file-name "." "src")))))


; Action: generate a number of single NoteOn midi files in midi-temp/single-notes relative to the startup directory.
(define (StandardMidiFile-laml-single-notes! ast)
  (ensure-directory-existence! (startup-directory) "single-notes")  ; the startup-directory IS midi-temp
  (make-single-note-midi-file ast (string-append (startup-directory) "single-notes/")))

(define (TyrosMultiPad-laml-single-notes! ast)
  (laml-error "TyrosMultiPad-laml-single-notes!  Not yet supported"))

(define (TyrosStyle-laml-single-notes! ast)
  (laml-error "TyrosStyle-laml-single-notes!  Not yet supported"))


; Action: Extract voices and write to a file in voice collection format.
(define (StandardMidiFile-laml-voice-collection-extract! ast)
  (do-extract-voice-collection-from-ast ast))

(define (TyrosMultiPad-laml-voice-collection-extract! ast)
  (laml-error "TyrosMultiPad-laml-voice-collection-extract! Not yet supported"))

(define (TyrosStyle-laml-voice-collection-extract! ast)
  (laml-error "TyrosStyle-laml-voice-collection-extract!  Not yet supported"))


; Action: Extract channel statistics and present it in a text file.
(define (StandardMidiFile-laml-channel-stat! ast)
  (do-extract-channel-stat-from-ast ast))

(define (TyrosMultiPad-laml-channel-stat! ast)
  (laml-error "TyrosMultiPad-laml-channel-stat! Not yet supported"))

(define (TyrosStyle-laml-channel-stat! ast)
  (laml-error "TyrosStyle-laml-channel-stat!  Not yet supported"))



; ----------------------------------------------------------------------------------------------------------------------------------------------------------

;; Use an alternative set of action procedures.
(define (midl-action action-symbol)
  (cond
         ((eq? action-symbol 'laml-ast)
           (set! StandardMidiFile! StandardMidiFile-laml-ast!)
           (set! TyrosMultiPad! TyrosMultiPad-laml-ast!)
           (set! TyrosStyle! TyrosStyle-laml-ast!))

         ((eq? action-symbol 'laml-source)
           (set! StandardMidiFile! StandardMidiFile-laml-source!)
           (set! TyrosMultiPad! TyrosMultiPad-laml-source!)
           (set! TyrosStyle! TyrosStyle-laml-source!))

         ((eq? action-symbol 'laml-single-notes)
           (set! StandardMidiFile! StandardMidiFile-laml-single-notes!)
           (set! TyrosMultiPad! TyrosMultiPad-laml-single-notes!)
           (set! TyrosStyle! TyrosStyle-laml-single-notes!))

         ((eq? action-symbol 'voice-collection-extract)
           (set! StandardMidiFile! StandardMidiFile-laml-voice-collection-extract!)
           (set! TyrosMultiPad! TyrosMultiPad-laml-voice-collection-extract!)
           (set! TyrosStyle! TyrosStyle-laml-voice-collection-extract!))

         ((eq? action-symbol 'channel-stat)
           (set! StandardMidiFile! StandardMidiFile-laml-channel-stat!)
           (set! TyrosMultiPad! TyrosMultiPad-laml-channel-stat!)
           (set! TyrosStyle! TyrosStyle-laml-channel-stat!))

        (else 'do-nothing))
)

; Switch action procedures based on the first LAML program parameter: 
(let ((prog-pars (laml-program-parameters)))
  (if (not (null? prog-pars))
      (begin
         (cond ((equal? (car prog-pars) "laml-ast")
                  (midl-action 'laml-ast))
               ((equal? (car prog-pars) "laml-source")
                  (midl-action 'laml-source))
               ((equal? (car prog-pars) "laml-single-notes")
                  (midl-action 'laml-single-notes))
               ((equal? (car prog-pars) "voice-collection-extract")
                  (midl-action 'voice-collection-extract))
               ((equal? (car prog-pars) "channel-stat")
                  (midl-action 'channel-stat))
               (else 'do-nothing)))))

; ===============================================================================================================
; Processing reportation stuff

(define (midi-report . mes)
  (if midi-reporting? 
      (apply display-message mes)))

(define (midi-time-report . mes)
  (if midi-reporting? 
      (apply display-message (cons (current-process-milliseconds) mes))))


; ===============================================================================================================
; OVERALL STANDARD MIDI FILE PARSING.
; The result of this parsing is an intermediate midi list structure.


; ----------------------------------------------------------------------------------------------------------
; CONSTRUCTORS AND SELECTORS - RELATED TO THE INTERMEDIATE LIST STRUCTURE REPRESENTATIONS

; Make a standard midi file list structure given a parsed header and parsed tracks.
(define (make-smf header tracks)
  (cons 'smf (cons header tracks)))

; Select and return the header of parsed standard midi file list structure
(define header-of-smf (make-selector-function 2 "header-of-smf"))

; Select and return the tracks of parsed standard midi file list structure
(define (tracks-of-smf smf)
  (cdr (cdr smf))) 



; Make a midi header chuck given format, number-of-tracks and division 
(define (make-midi-header-chunck format number-of-tracks division)
  (list 'mthd (as-number format) (as-number number-of-tracks) division))

; Select and return the format of a parsed midi file header chunck.
(define format-of-midi-header (make-selector-function 2 "format-of-midi-header"))

; Select and return the number of tracks of a parsed midi file header chunck.
(define number-of-tracks-of-midi-header (make-selector-function 3 "number-of-tracks-of-midi-header"))

; Select and return the division (always in ppqn - pulses per quarter note) of a parsed midi file header chunck.
(define (ppqn-of-midi-header header)
  (let ((division (list-ref header 3)))
    (if (eq? (first division) 'ppqn)
        (second division)
        #f)))



; Make a single midi track chuck. data is a list of track events, each constructed by make-track-event.
(define (make-midi-track-chunck length data)
  (list 'mtrk (as-number length) data))

; Select and return the length of a parsed track
(define length-of-midi-track (make-selector-function 2 "length-of-midi-track"))

; Select and return the data of a parsed track. A list of track-events, each as constructed by make-track-event.
(define data-of-midi-track (make-selector-function 3 "data-of-midi-track"))



(define (make-track-event delta-time midi-event)
  (list 'track-event delta-time midi-event))

(define delta-time-of-track-event (make-selector-function 2 "delta-time-of-track-event"))
(define midi-event-of-track-event (make-selector-function 3 "midi-event-of-track-event")) 



(define (make-midi-event status-byte rest-bytes)
  (list 'midi-event status-byte rest-bytes))

(define status-byte-of-midi-event (make-selector-function 2 "status-byte-of-midi-event"))
(define rest-bytes-of-midi-event (make-selector-function 3 "rest-bytes-of-midi-event"))

; ---------------------------------------------------------------------------------------------------------

; Parse a midi string return a smf list structure.
(define (parse-midi-string midi-string)
  (let* ((header (parse-header-of-midi-string midi-string))
         (number-of-tracks (number-of-tracks-of-midi-header header)))
    (make-smf header (parse-tracks-of-midi-string midi-string 14 number-of-tracks))))

; Parse a header of standard midi file, as represented by the byte string str.
(define (parse-header-of-midi-string str)
  (if (and (equal? (substring str 0 4) "MThd")                   ; Byte 0-3
           (= (byte-string-to-integer (substring str 4 8)) 6))   ; Byte 4-7
      (make-midi-header-chunck 
         (byte-string-to-integer (substring str 8 10))           ; Byte 8-9
         (byte-string-to-integer (substring str 10 12))          ; Byte 10-11
         (parse-division (substring str 12 14))                  ; Byte 12-13
      )
      #f))

; Temporary - to find out where midi part ends
(define length-info '())

; Parse a single track from string which starts at start-pos.
; str is the overall byte-string that represents the whole midi file.
(define (parse-track-of-midi-string str start-pos)
  (if (equal? (substring str start-pos (+ start-pos 4)) "MTrk")
      (let ((track-length (byte-string-to-integer (substring str (+ start-pos 4) (+ start-pos 8)))))
        (set! length-info (cons (+ start-pos 8 track-length) length-info))
        (make-midi-track-chunck 
         track-length 
         (parse-track-data (substring str (+ start-pos 8) (+ start-pos 8 track-length)))
         ))
      #f))

; Parse n tracks from str. The first one starts at position start-pos.
; Return a list of tracks.
; str is the overall byte-string that representins the whole midi file.
(define (parse-tracks-of-midi-string str start-pos n)
  (if (= n 0)
      '()
      (let* ((track (parse-track-of-midi-string str start-pos))
             (track-length (length-of-midi-track track)))
        (cons track (parse-tracks-of-midi-string str (+ start-pos 8 track-length) (- n 1))))))

; Parse a division from the header chunck.
(define (parse-division two-byte-string)
  (if (>= (as-number (string-ref two-byte-string 0)) 128)        ; "first byte negative"
      (error "parse-division: only PPQN is supported as of now")
      (list 'ppqn (as-number (byte-string-to-integer two-byte-string)))))

; ---------------------------------------------------------------------------------------------------------------
; Track data parsing.

; Parse track data in string. Str contains all data pertaining to the track. Nothing more and nothing less.
; Returns a list of track events, each constructed by the function make-track-event.
(define (parse-track-data str)
  (parse-track-data-1 str (string-length str) 0 #f))

(define (parse-track-data-1 str str-lgt start-pos running-status-byte)
  (if (>= start-pos str-lgt)
      '()
      (let* ((track-event-data (parse-track-event str str-lgt start-pos running-status-byte))
             (track-event-length (car track-event-data))
             (the-track-event (cdr track-event-data))
             (new-running-status-byte (status-byte-of-midi-event (midi-event-of-track-event the-track-event)))
            )
        (cons
          the-track-event
          (parse-track-data-1 str str-lgt (+ start-pos track-event-length) new-running-status-byte)))))

; Parse a track event (delta time and midi event).
; Return cons cell of the byte length of the track event and and the track event as such.
; running-status-byte is the status byte (if available) of the previous midi event.
(define (parse-track-event str str-lgt start-pos running-status-byte)
;  (midi-report "parse-track-event" start-pos)
  (let* ((vlq-info (parse-variable-length-quantity str start-pos))   ; First delta time, as variable length quantity:  vlq
         (vlq-length (car vlq-info))
         (vlq (cdr vlq-info))
        )
    (let* ((midi-event-info (parse-midi-event str (+ start-pos vlq-length) running-status-byte))    ; Next Midi event: midi-event
           (midi-length (car midi-event-info))
           (midi-event (cdr midi-event-info))
          )
      (cons (+ vlq-length midi-length) (make-track-event vlq midi-event)))))


; Parse a midi event, starting at position start-pos.
; Runnning-status-byte is a byte, or #f if not available.
; Return a cons-cell of the length of the midi-event and the midi-event itself, as constructed by the function make-midi-event.
(define (parse-midi-event str start-pos running-status-byte)
  (let* ((first-byte-of-event (as-number (string-ref str start-pos)))
         (status-byte-of-midi-event (if (is-status-byte? first-byte-of-event) first-byte-of-event running-status-byte))
         (data-bytes-of-midi-event  (read-data-bytes-of-midi-event 
                                       str
                                       (if (is-status-byte? first-byte-of-event) (+ start-pos 1) start-pos) 
                                       status-byte-of-midi-event))
        )
    ; (if (not (is-status-byte? first-byte-of-event)) (midi-report "running status used"))
    (let ((lgt-data-bytes (string-length data-bytes-of-midi-event)))
     (cons 
      (if (is-status-byte? first-byte-of-event) (+ 1 lgt-data-bytes) lgt-data-bytes)
      (make-midi-event status-byte-of-midi-event data-bytes-of-midi-event)
     )
    )
  )
)

; Read the data bytes of a midi event. The data bytes start at start-pos.
; status-byte describes the type of the data-bytes to be parsed, and the amount of reading is done according to data-byte.
; Return a string of data bytes (everything after the status byte).
(define (read-data-bytes-of-midi-event str start-pos status-byte)
                                                                                                    ; STATUS BYTE
  (cond ((note-off-event? status-byte)                (substring str start-pos (+ start-pos 2)))    ; 1000 cccc  + two data bytes (the 4 cccc bits encode the channel)
        ((note-on-event? status-byte)                 (substring str start-pos (+ start-pos 2)))    ; 1001 cccc  + two data bytes
        ((polyphonic-aftertouch-event? status-byte)   (substring str start-pos (+ start-pos 2)))    ; 1010 cccc  + two data bytes
        ((control-change-event? status-byte)          (substring str start-pos (+ start-pos 2)))    ; 1011 cccc  + two data bytes
        ((program-change-event? status-byte)          (substring str start-pos (+ start-pos 1)))    ; 1100 cccc  + one data byte
        ((channel-pressure-event? status-byte)        (substring str start-pos (+ start-pos 1)))    ; 1101 cccc  + one data byte
        ((pitch-wheel-event? status-byte)             (substring str start-pos (+ start-pos 2)))    ; 1110 cccc  + two data bytes
        (else                                                                                       ; 1111 ****  + a variable number of bytes, where * is one bit.
                                                                                                    ; or in hex:   F*  where * is four bits.

                                                         ; Now notating in hexadecimal notation:
          (cond
            ((system-exclusive-event? status-byte)       ; F0 <vlq-length> <bytes> F7       Variable length data bytes.
                                                         ; F7 <vlq-length> <bytes> F7       Relies on trailing F7
               (let* ((f7-char (make-char-2-hex 15 7))   ; F = 15 (decimal).
                      (f7-end-pos (find-in-string str f7-char (+ start-pos 1))))
                 (substring str start-pos (+ f7-end-pos 1)))   ; including ending f7-char ! 

            )
            ((mtc-quater-frame-message-event? status-byte)   (substring str start-pos (+ start-pos 1)))   ; 1111 0001    or in Hex:  F1  +  One data byte.
            ((song-position-pointer-event? status-byte)      (substring str start-pos (+ start-pos 2)))   ; 1111 0010    or in Hex:  F2  +  Two data bytes.
            ((song-select-event? status-byte)                (substring str start-pos (+ start-pos 1)))   ; 1111 0011    or in Hex:  F3  +  One data byte.
            ((tune-request-event? status-byte)               "")                                          ; 1111 0110    or in Hex:  F6  +  NO data.
            ((midi-clock-event? status-byte)                 "")                                          ; 1111 1000    or in Hex:  F8  +  NO data.            
            ((midi-tick-event? status-byte)                  "")                                          ; 1111 1001    or in Hex:  F9  +  NO data.            
            ((midi-start-event? status-byte)                 "")                                          ; 1111 1010    or in Hex:  FA  +  NO data.            
            ((midi-continue-event? status-byte)              "")                                          ; 1111 1011    or in Hex:  FB  +  NO data.            
            ((midi-stop-event? status-byte)                  "")                                          ; 1111 1100    or in Hex:  FC  +  NO data.            
            ((midi-active-sense? status-byte)                "")                                          ; 1111 1110    or in Hex:  FE  +  NO data.            

            ; Midi reset - will not happen. Will never be sent. Therefore interpreted as midi-meta event:
            ((meta-event? status-byte)                       ; FF <extra-status-byte> <vlq-length> <bytes>
                                                             ; thus variable length data bytes.
               (let* ((vlq-info (parse-variable-length-quantity str (+ start-pos 1)))
                      (vlq-length (car vlq-info))
                      (vlq (cdr vlq-info)))
                  (substring str start-pos (+ start-pos 1 vlq-length vlq)))
            )

            (else (error (string-append "Non-existing system common, real-time and system exclusive event. Status-byte: " (as-string status-byte))))
          )
        )
  )
)

; ---------------------------------------------------------------------------------------------------------------
; Status byte predicates.

(define (note-off-event? status-byte)                ; 1000 cccc   (the 4 cccc bits encode the channel) 
  (and (>= status-byte 128) (<= status-byte 143)))

(define (note-on-event? status-byte)                 ; 1001 cccc
  (and (>= status-byte 144) (<= status-byte 159)))

(define (polyphonic-aftertouch-event? status-byte)   ; 1010 cccc
  (and (>= status-byte 160) (<= status-byte 175)))

(define (control-change-event? status-byte)          ; 1011 cccc
  (and (>= status-byte 176) (<= status-byte 191)))

(define (program-change-event? status-byte)          ; 1100 cccc
  (and (>= status-byte 192) (<= status-byte 207)))

(define (channel-pressure-event? status-byte)        ; 1101 cccc
  (and (>= status-byte 208) (<= status-byte 223)))

(define (pitch-wheel-event? status-byte)             ; 1110 cccc
  (and (>= status-byte 224) (<= status-byte 239)))

(define (system-exclusive-event? status-byte)        ; 1111 ...  ???
  (or (= status-byte 240) (= status-byte 247)))

(define (mtc-quater-frame-message-event? status-byte)
  (= status-byte 241))

(define (song-position-pointer-event? status-byte)
  (= status-byte 242))

(define (song-select-event? status-byte)
  (= status-byte 243))

(define (tune-request-event? status-byte)
  (= status-byte 246))

(define (midi-clock-event? status-byte)
  (= status-byte 248))

(define (midi-tick-event? status-byte)
  (= status-byte 249))

(define (midi-start-event? status-byte)
  (= status-byte 250))

(define (midi-continue-event? status-byte)
  (= status-byte 251))

(define (midi-stop-event? status-byte)
  (= status-byte 252))

(define (midi-active-sense? status-byte)
  (= status-byte 254))

(define (meta-event? status-byte)
  (= status-byte 255))

; --------------------------------------------------------------------------------
; Other byte predicates.

(define (is-status-byte? byte)
  (>= byte 128))


; ===============================================================================================================
; VARIABLE LENGTH QUANTITIES. PARSING AND UNPARSING.
; Variable length quantities, according to  http://home.roadrunner.com/~jgglatt/tech/midifile/vari.htm
; Variable length quantities are variable length, binary representation of integers.
; The first (most significant) bit in each byte encodes status: 1 means not last byte. 0 means last byte.
; Most significant byte at start-pos.
; Return a cons cell of the form (n . val) where n is the number of bytes processed, 
; and val is the integer value aggregated from these bytes.
(define (parse-variable-length-quantity str start-pos)
 (let* ((str-lgt (string-length str))
        (byte-1 (if (< start-pos str-lgt) (as-number (string-ref str start-pos)) #f))   ; Feb 23, 2008: more conservative programming, due to style parsing difficulties.
        (stop-1 (if byte-1 (< byte-1 128) #t))
        (byte-2 (if (< (+ start-pos 1) str-lgt) (as-number (string-ref str (+ start-pos 1))) #f))
        (stop-2 (if byte-2 (< byte-2 128) #t))
        (byte-3 (if (< (+ start-pos 2) str-lgt) (as-number (string-ref str (+ start-pos 2))) #f))
        (stop-3 (if byte-3 (< byte-3 128) #t))
        (byte-4 (if (< (+ start-pos 3) str-lgt) (as-number (string-ref str (+ start-pos 3))) #f))
        (stop-4 (if byte-4 (< byte-4 128) #t))
       )
   (cond ((and (not byte-1)) (cons 0 0))
         ((and stop-1 byte-1) (cons 1 byte-1))
         ((and stop-2 byte-1 byte-2) (cons 2 (+ (* (- byte-1 128) 128) byte-2)))
         ((and stop-3 byte-1 byte-2 byte-3) (cons 3 (+ (* (- byte-1 128) 128 128) (* (- byte-2 128) 128) byte-3))) 
         ((and stop-4 byte-1 byte-2 byte-3 byte-4) (cons 4 (+ (* (- byte-1 128) 128 128 128) (* (- byte-2 128) 128 128) (* (- byte-3 128) 128) byte-4))) 
         (else (error "parse-variable-length-quantity: str does not encode a valid variable length quantity. At most four bytes are allowed")))))

; The inverse function of parse-variable-length-quantity.
; Return a byte string which represents n as a variable length quantiy.
; n is non-negative.
(define (unparse-integer-as-variable-length-quantity n)
  (let ((low (remainder n 128))
        (rest (quotient n 128)))
    (string-append (unparse-integer-as-variable-length-quantity-highs rest) (as-string (as-char low)))))

; Produce bytes in which the high bits are all set to 1.  
(define (unparse-integer-as-variable-length-quantity-highs n)
  (if (= n 0)
      ""
      (let ((low (remainder n 128))
            (rest (quotient n 128)))
        (string-append (unparse-integer-as-variable-length-quantity-highs rest) (as-string (as-char (+ 128 low)))) ;    + 128  enforces one as first bit.
        )))



; ===============================================================================================================
; TEXTUAL REPORTING FUNCTIONS.
; Obsolete. Only used during development.

(define (present-midi-file file-path)
  (let ((parsed-midi-structure (parse-midi-string (read-text-file file-path))))
    (present-smf parsed-midi-structure)))

(define (present-smf smf-structure)
  (let ((header (header-of-smf smf-structure))
        (tracks (tracks-of-smf smf-structure)))
    (present-midi-header header)
    (display (string-append CR))
    (display (string-append  " -------------------------" CR))
    (for-each (lambda (n dtr)
                (display (string-append (as-string n)  " -------------------------" CR)) 
                (present-track-data (data-of-midi-track dtr)))
              (number-interval 1 (length tracks))
              tracks)))

(define (present-midi-header header)
  (display (string-append 
              "HEADER " CR
              "Format: " (as-string (format-of-midi-header header)) CR
              "Number of tracks: " (as-string (number-of-tracks-of-midi-header header)) CR
              "Pulses per quater note: " (as-string (ppqn-of-midi-header header)))))


(define (present-track-data track-event-list)
  (for-each
    (lambda (track-event)
       (display (string-append (track-event-as-string track-event) CR)))
    track-event-list))

; Produce a pretty string from a track event.
(define (track-event-as-string track-event)
  (string-append "TRE: " (as-string (delta-time-of-track-event track-event)) " "
                (midi-event-as-string (midi-event-of-track-event track-event))))

(define (midi-event-as-string midi-event)
  (let ((status-byte (status-byte-of-midi-event midi-event))
        (rest-bytes (rest-bytes-of-midi-event midi-event)))
    (present-midi-event status-byte rest-bytes)))

(define (present-midi-event status-byte rest-bytes)
  (cond ((note-off-event? status-byte)   (string-append "Note-off #" (as-string (channel-of-status-byte status-byte)) " " (present-rest-bytes rest-bytes))  )
        ((note-on-event? status-byte)    (string-append "Note-on #" (as-string (channel-of-status-byte status-byte)) " " (present-rest-bytes rest-bytes))  )
        ((polyphonic-aftertouch-event? status-byte) (string-append "Poly Aftertch #" (as-string (channel-of-status-byte status-byte)) " " (present-rest-bytes rest-bytes)) ) 
        ((control-change-event? status-byte)      (string-append "Control change #" (as-string (channel-of-status-byte status-byte)) " " (present-rest-bytes rest-bytes))   ) 
        ((program-change-event? status-byte)      (string-append "Program change #" (as-string (channel-of-status-byte status-byte)) " " (present-rest-bytes rest-bytes))   )
        ((channel-pressure-event? status-byte)    (string-append "Channel pressure #" (as-string (channel-of-status-byte status-byte)) " " (present-rest-bytes rest-bytes))   )
        ((pitch-wheel-event? status-byte)         (string-append "Pitch wheel #" (as-string (channel-of-status-byte status-byte)) " " (present-rest-bytes rest-bytes))   ) ; not correct.    
        (else                                                                             
          (cond
            ((system-exclusive-event? status-byte)     
                (string-append "System Exclusive " (as-string (string-length rest-bytes))) 

            )
            ((mtc-quater-frame-message-event? status-byte) "mtc-quarter" )  
            ((song-position-pointer-event? status-byte)    "song pos" )   
            ((song-select-event? status-byte)              "song select" )
            ((tune-request-event? status-byte)             "tune req" )   
            ((midi-clock-event? status-byte)               "midi-clock")  
            ((midi-tick-event? status-byte)                "midi-tick" )  
            ((midi-start-event? status-byte)               "midi-start" ) 
            ((midi-continue-event? status-byte)            "midi-continue" )
            ((midi-stop-event? status-byte)                "midi-stop" )    
            ((midi-active-sense? status-byte)              "active-sense" ) 

            ((meta-event? status-byte)                      
               (string-append "Meta event " (as-string (string-length rest-bytes)))
               
            )
            (else (string-append "NON-EXISTING!" (as-string status-byte)))
          )
        )
  )
)  

; status-byte is a normal, decimal integer, between 0x80 and 0xFF (between 128 and 255).
; Return the least four significant bits, as a decimal number.
(define (channel-of-status-byte status-byte)
  (+ (remainder status-byte 16) 1)  ; Add 1, because the lowest channel is 1, not 0.
)

; Extract byte number n from str, which represents binary data (as a string of bytes).
; Returns an integer.
(define (get-byte-number str n)
  (as-number (string-ref str n)))

; Extract byte number n from str, which represents binary data (as a string of bytes).
; Returns a string.
(define (get-byte-number-as-string str n)
  (as-string (get-byte-number str n)))

(define (present-rest-bytes rest-bytes)
  (let ((lgt (string-length rest-bytes)))
    (cond ((= 1 lgt)  (as-string (as-number (string-ref rest-bytes 0))))
          ((= 2 lgt)  (string-append (get-byte-number-as-string rest-bytes 0) " " (get-byte-number-as-string rest-bytes 1) ))
          (else "..."))))
  
    
    
; ===============================================================================================================
; TRANSFORMATION OF INTERMEDIATE LIST STRUCTURE TO XML-IN-LAML AST.
; ...-as-list-to-ast functions.

(define (midi-file-as-list-to-ast midi-file-list mode non-midi-suffix-string)
  (set! global-ppqn (ppqn-of-midi-header (header-of-smf midi-file-list)))
  (set! global-signature (find-time-signature-in-midi-list midi-file-list))
  (StandardMidiFile 'internal:run-action-procedure "false"
    (midi-header-chunck-as-list-to-ast (header-of-smf midi-file-list) mode)
    (map (lambda (track) (midi-track-chunck-as-list-to-ast track mode)) (tracks-of-smf midi-file-list))    ; midi-track-chunck-as-list-to-ast transforms track data
    (if (empty-string? non-midi-suffix-string)
        '()
        (NonMidiSuffix (binary-to-hex-string non-midi-suffix-string)))))

; Attempt to find the time signature in midi-file-list (we use it in the info strings).
; Return a list, such as (4 4).
; If the time signature is not found, return the value of the first optional parameter.
; .form (find-time-signature-in-midi-list midi-file-list [default-value])
(define (find-time-signature-in-midi-list midi-file-list . optional-parameters)
 (let ((default-value (optional-parameter 1 optional-parameters (list 4 4))))
  (let ((tracks (tracks-of-smf midi-file-list)))
    (if (not (null? tracks))
      (let* ((first-track (first tracks))
             (meta-time-sign-events (filter time-signature-event? (data-of-midi-track first-track))))
        (if (not (null? meta-time-sign-events))
            (extract-time-signature-of-track-event (first meta-time-sign-events))
            default-value))
      default-value))))

(define (time-signature-event? x)
  (if (and (list? x) (= (length x) 3) (eq? (first x) 'track-event))
      (let* ((me (midi-event-of-track-event x))
             (sb (status-byte-of-midi-event me))
             (dt (rest-bytes-of-midi-event me))
            )
        (if (= sb 255) ; meta event
            (let ((type (get-byte-number dt 0)))
               (if (= type 88) ; time signature
                   #t
                   #f))
            #f))
      #f))

(define (extract-time-signature-of-track-event track-event)
  (let* ((me (midi-event-of-track-event track-event))
         (dt (rest-bytes-of-midi-event me))
         (num (as-number (string-ref dt 2)))
         (denom (as-number (string-ref dt 3))))
    (list num (power 2 denom))))


; Convenient top-level function for transformation of intermediate midi list file structure to an AST.
(define midi-list-to-ast midi-file-as-list-to-ast)

(define laml-midi-file-prefix
  "(load (string-append laml-dir \"laml.scm\"))\n(laml-style \"xml-in-laml/midi/midi\")\n(load (string-append laml-dir \"styles/xml-in-laml/midi/midi-laml-processing-lib.scm\"))\n\n"
)

;; Given a midi ast, render it as XML-in-LAML at a file named output-file-path.
;; output-file-path is the full path the output file, including extension.
;; An optional parameter pretty-print? controls if the rendered XML-in-LAML source file is pretty printed. Pr. defaults pretty printing is done.
;; form (make-midi-laml-file midi-ast output-file-path [pretty-print?])
(define (make-midi-laml-file midi-ast output-file-path . optional-parameter-list)
 (let ((pretty-print? (optional-parameter 1 optional-parameter-list #t)))
  (cond ((and (equal? "StandardMidiFile" (ast-element-name midi-ast)) (not (ast-subtree midi-ast "NonMidiSuffix")))  ; A typical and conventional Standard Midi file AST.
           (make-midi-laml-file-smf midi-ast output-file-path #t))                                                   ; Fast version - always pretty print.
        (else 
           (make-midi-laml-file-generic midi-ast output-file-path pretty-print?)))))

; ---------------------------------------------------------------------------------------------------------------
; Fast stuff:

; Given a midi ast, render it as XML-in-LAML at a file named output-file-path.
; Assume that it is a StandardMidiFile AST (format 0 or 1).
; output-file-path is the full path the output file, including extension.
(define (make-midi-laml-file-smf midi-ast output-file-path pretty-print?)
  (if (file-exists? output-file-path) (delete-file output-file-path))
  (let ((op (open-output-file output-file-path)))
    (display laml-midi-file-prefix op)
    (if (equal? (ast-element-name midi-ast) "StandardMidiFile") 
        (begin
          (display "(StandardMidiFile\n" op)
          (display-midi-header (ast-subtree midi-ast "MidiHeader") op)
          (for-each (lambda (midi-track-ast) (display-midi-track midi-track-ast op)) (filter (lambda (x) (and (ast? x) (equal? (ast-element-name x) "MidiTrack"))) (ast-subtrees midi-ast))) 
          (display ")\n" op))
        (laml-error "A StandardMidiFile is expected"))

    (close-output-port op)
    (midi-time-report "AST written to file done")
  )
)

(define (display-midi-track midi-track-ast op)
   (display "  (MidiTrack\n" op)
   (for-each (lambda (midi-event-ast) (if (ast? midi-event-ast) (display-midi-event midi-event-ast op))) (ast-subtrees midi-track-ast))
   (display "  )\n" op))

(define (display-midi-header midi-header-ast op)
  (display "  (MidiHeader" op)
  (do ((attributes (ast-attributes midi-header-ast) (cddr attributes)))
      ((null? attributes) 'done)
    (let ((attr-name (car attributes))
          (attr-val (cadr attributes)))
      (display " '" op) (display attr-name op) (display " " op)
      (display "\"" op) (display attr-val op) (display "\"" op)))
  (display ")\n" op))

; A boolean variable that controls the layout of NoteOn events
(define use-aligned-noteon-layout #t)

; A variable that controls the place of white space filling, used in case use-aligned-noteon-layout is true.
; Either before, between, or after. before is before the attribtue name. between is between the attribue name and value. after inserts the spaces after the attribute value.
; The Emacs MIDI mode depends on the value after of this variable!
(define white-space-fill-mode 'after)

; The max width of an info attribute string (excluding string quotes). Assume that last 7 of these are for note name and counter transposition.
(define info-max-width 18)

(define (display-midi-event midi-event-ast op)
 (let ((abs-time-max-width 7)     ; widths are excluding string quotes.
       (delta-time-max-width 5))
  (cond ((and use-aligned-noteon-layout (equal? (ast-element-name midi-event-ast) "NoteOn"))      ; special aligned layout of NoteOn
         (let (
               (duration-max-width 5)    ; other max widths are fixed according to the MIDI specification (typically 3 due to 0..127 ranges).
              )

           (display "     (NoteOn       " op)

           ; time attribute:
           (cond ((has-ast-attribute? midi-event-ast 'absTime)
                    (let* ((attr-val (ast-attribute midi-event-ast 'absTime))
                           (attr-val-lgt (string-length attr-val))
                           (fill-str (make-string (max 0 (- abs-time-max-width attr-val-lgt)) #\space)))
                      (if (eq? white-space-fill-mode 'before) (display fill-str op))
                      (display " 'absTime " op)
                      (if (eq? white-space-fill-mode 'between) (display fill-str op))
                      (display "\"" op) (display attr-val op) (display "\"" op)
                      (if (eq? white-space-fill-mode 'after) (display fill-str op))))
                 ((has-ast-attribute? midi-event-ast 'deltaTime)
                    (let* ((attr-val (ast-attribute midi-event-ast 'deltaTime))
                           (attr-val-lgt (string-length attr-val))
                           (fill-str (make-string (max 0 (- delta-time-max-width attr-val-lgt)) #\space)))
                      (if (eq? white-space-fill-mode 'before) (display fill-str op))
                      (display " 'deltaTime " op)
                      (if (eq? white-space-fill-mode 'between) (display fill-str op))
                      (display "\"" op) (display attr-val op) (display "\"" op)
                      (if (eq? white-space-fill-mode 'after) (display fill-str op))))
                 (else (laml-error "display-midi-event: NoteOn: Either absTime og deltaTime expected")))

           ; info attribute
           (let* ((attr-val (ast-attribute midi-event-ast 'info))
                  (attr-val-lgt (string-length attr-val))
                  (fill-str (make-string (max 0 (- info-max-width attr-val-lgt)) #\space)))
             (if (eq? white-space-fill-mode 'before) (display fill-str op))
             (display " 'info " op)
             (if (eq? white-space-fill-mode 'between) (display fill-str op))
             (display "\"" op) (display attr-val op) (display "\"" op)
             (if (eq? white-space-fill-mode 'after) (display fill-str op))
           )

           ; channel
           (let* ((attr-val (ast-attribute midi-event-ast 'channel))
                  (attr-val-lgt (string-length attr-val))
                  (fill-str (make-string (max 0 (- 2 attr-val-lgt)) #\space)))
             (if (eq? white-space-fill-mode 'before) (display fill-str op))
             (display " 'channel " op)
             (if (eq? white-space-fill-mode 'between) (display fill-str op))
             (display "\"" op) (display attr-val op) (display "\"" op)
             (if (eq? white-space-fill-mode 'after) (display fill-str op))
           )

           ; note
           (let* ((attr-val (ast-attribute midi-event-ast 'note))
                  (attr-val-lgt (string-length attr-val))
                  (fill-str (make-string (max 0 (- 3 attr-val-lgt)) #\space)))
             (if (eq? white-space-fill-mode 'before) (display fill-str op))
             (display " 'note " op)
             (if (eq? white-space-fill-mode 'between) (display fill-str op))
             (display "\"" op) (display attr-val op) (display "\"" op)
             (if (eq? white-space-fill-mode 'after) (display fill-str op))
           )

           ; velocity
           (let* ((attr-val (ast-attribute midi-event-ast 'velocity))
                  (attr-val-lgt (string-length attr-val))
                  (fill-str (make-string (max 0 (- 3 attr-val-lgt)) #\space)))
             (if (eq? white-space-fill-mode 'before) (display fill-str op))
             (display " 'velocity " op)
             (if (eq? white-space-fill-mode 'between) (display fill-str op))
             (display "\"" op) (display attr-val op) (display "\"" op)
             (if (eq? white-space-fill-mode 'after) (display fill-str op))
            )

           ; duration
           (let* ((attr-val (ast-attribute midi-event-ast 'duration))
                  (attr-val-lgt (string-length attr-val))
                  (fill-str (make-string (max 0 (- duration-max-width attr-val-lgt)) #\space)))
             (if (eq? white-space-fill-mode 'before) (display fill-str op))
             (display " 'duration " op)
             (if (eq? white-space-fill-mode 'between) (display fill-str op))
             (display "\"" op) (display attr-val op) (display "\"" op)
             (if (eq? white-space-fill-mode 'after) (display fill-str op))
           )

           (display ")\n" op)
          )
        )

        ((and use-aligned-noteon-layout (equal? (ast-element-name midi-event-ast) "ControlChange"))      ; special aligned layout of ControlChange
           (display "     (ControlChange" op)

           ; time attribute:
           (cond ((has-ast-attribute? midi-event-ast 'absTime)
                    (let* ((attr-val (ast-attribute midi-event-ast 'absTime))
                           (attr-val-lgt (string-length attr-val))
                           (fill-str (make-string (max 0 (- abs-time-max-width attr-val-lgt)) #\space)))
                      (if (eq? white-space-fill-mode 'before) (display fill-str op))
                      (display " 'absTime " op)
                      (if (eq? white-space-fill-mode 'between) (display fill-str op))
                      (display "\"" op) (display attr-val op) (display "\"" op)
                      (if (eq? white-space-fill-mode 'after) (display fill-str op))))
                 ((has-ast-attribute? midi-event-ast 'deltaTime)
                    (let* ((attr-val (ast-attribute midi-event-ast 'deltaTime))
                           (attr-val-lgt (string-length attr-val))
                           (fill-str (make-string (max 0 (- delta-time-max-width attr-val-lgt)) #\space)))
                      (if (eq? white-space-fill-mode 'before) (display fill-str op))
                      (display " 'deltaTime " op)
                      (if (eq? white-space-fill-mode 'between) (display fill-str op))
                      (display "\"" op) (display attr-val op) (display "\"" op)
                      (if (eq? white-space-fill-mode 'after) (display fill-str op))))
                 (else (laml-error "display-midi-event: ControlChange: Either absTime og deltaTime expected")))

           ; info attribute
           (let* ((attr-val (ast-attribute midi-event-ast 'info))
                  (attr-val-lgt (string-length attr-val))
                  (fill-str (make-string (max 0 (- info-max-width attr-val-lgt)) #\space)))
             (if (eq? white-space-fill-mode 'before) (display fill-str op))
             (display " 'info " op)
             (if (eq? white-space-fill-mode 'between) (display fill-str op))
             (display "\"" op) (display attr-val op) (display "\"" op)
             (if (eq? white-space-fill-mode 'after) (display fill-str op))
           )

           ; channel
           (let* ((attr-val (ast-attribute midi-event-ast 'channel))
                  (attr-val-lgt (string-length attr-val))
                  (fill-str (make-string (max 0 (- 2 attr-val-lgt)) #\space)))
             (if (eq? white-space-fill-mode 'before) (display fill-str op))
             (display " 'channel " op)
             (if (eq? white-space-fill-mode 'between) (display fill-str op))
             (display "\"" op) (display attr-val op) (display "\"" op)
             (if (eq? white-space-fill-mode 'after) (display fill-str op))
           )

           ; control
           (let* ((attr-val (ast-attribute midi-event-ast 'control))
                  (attr-val-lgt (string-length attr-val))
                  (fill-str (make-string (max 0 (- 3 attr-val-lgt)) #\space)))
             (if (eq? white-space-fill-mode 'before) (display fill-str op))
             (display " 'control " op)
             (if (eq? white-space-fill-mode 'between) (display fill-str op))
             (display "\"" op) (display attr-val op) (display "\"" op)
             (if (eq? white-space-fill-mode 'after) (display fill-str op))
           )

           ; value
           (let* ((attr-val (ast-attribute midi-event-ast 'value))
                  (attr-val-lgt (string-length attr-val))
                  (fill-str (make-string (max 0 (- 3 attr-val-lgt)) #\space)))
             (if (eq? white-space-fill-mode 'before) (display fill-str op))
             (display " 'value " op)
             (if (eq? white-space-fill-mode 'between) (display fill-str op))
             (display "\"" op) (display attr-val op) (display "\"" op)
             (if (eq? white-space-fill-mode 'after) (display fill-str op))
            )

           (display ")\n" op)
        )

        ((and use-aligned-noteon-layout (equal? (ast-element-name midi-event-ast) "Meta"))      ; special aligned layout of Meta
           (display "     (Meta         " op)

           ; time attribute:
           (cond ((has-ast-attribute? midi-event-ast 'absTime)
                    (let* ((attr-val (ast-attribute midi-event-ast 'absTime))
                           (attr-val-lgt (string-length attr-val))
                           (fill-str (make-string (max 0 (- abs-time-max-width attr-val-lgt)) #\space)))
                      (if (eq? white-space-fill-mode 'before) (display fill-str op))
                      (display " 'absTime " op)
                      (if (eq? white-space-fill-mode 'between) (display fill-str op))
                      (display "\"" op) (display attr-val op) (display "\"" op)
                      (if (eq? white-space-fill-mode 'after) (display fill-str op))))
                 ((has-ast-attribute? midi-event-ast 'deltaTime)
                    (let* ((attr-val (ast-attribute midi-event-ast 'deltaTime))
                           (attr-val-lgt (string-length attr-val))
                           (fill-str (make-string (max 0 (- delta-time-max-width attr-val-lgt)) #\space)))
                      (if (eq? white-space-fill-mode 'before) (display fill-str op))
                      (display " 'deltaTime " op)
                      (if (eq? white-space-fill-mode 'between) (display fill-str op))
                      (display "\"" op) (display attr-val op) (display "\"" op)
                      (if (eq? white-space-fill-mode 'after) (display fill-str op))))
                 (else (laml-error "display-midi-event: Meta: Either absTime og deltaTime expected")))

           ; info attribute
           (let* ((attr-val (ast-attribute midi-event-ast 'info))
                  (attr-val-lgt (string-length attr-val))
                  (fill-str (make-string (max 0 (- info-max-width attr-val-lgt)) #\space)))
             (if (eq? white-space-fill-mode 'before) (display fill-str op))
             (display " 'info " op)
             (if (eq? white-space-fill-mode 'between) (display fill-str op))
             (display "\"" op) (display attr-val op) (display "\"" op)
             (if (eq? white-space-fill-mode 'after) (display fill-str op))
           )

           ; type
           (let* ((attr-val (ast-attribute midi-event-ast 'type))
                  (attr-val-lgt (string-length attr-val))
                  (fill-str (make-string (max 0 (- 2 attr-val-lgt)) #\space)))
             (if (eq? white-space-fill-mode 'before) (display fill-str op))
             (display " 'type " op)
             (if (eq? white-space-fill-mode 'between) (display fill-str op))
             (display "\"" op) (display attr-val op) (display "\"" op)
             (if (eq? white-space-fill-mode 'after) (display fill-str op))
           )

           ; CONTENT
           (let ((ast-contents (ast-text midi-event-ast)))
             (if (empty-string? ast-contents)
                 (display ")\n" op)
                 (begin (display " " op) (write ast-contents op) (display ")\n" op))))
        )

        (else                                ; Default layout
         (display "     (" op)
         (display (ast-element-name midi-event-ast) op) 
         (do ((attributes (ast-attributes midi-event-ast) (cddr attributes)))
             ((null? attributes) 'done)
           (let ((attr-name (car attributes))
                 (attr-val (cadr attributes)))
             (display " '" op) (display attr-name op) (display " " op)
             (display "\"" op) (display attr-val op) (display "\"" op)))
         (let ((ast-contents (ast-text midi-event-ast)))
           (if (empty-string? ast-contents)
               (display ")\n" op)
               (begin (display " " op) (write ast-contents op) (display ")\n" op))))))))


; End fast stuff
; ---------------------------------------------------------------------------------------------------------------


; Given a midi ast, render it as XML-in-LAML at a file named output-file-path.
; A generic version that relies on the general LAML rendering to an output port and subsequent pretty printing (if requested).
; output-file-path is the full path the output file, including extension.
(define (make-midi-laml-file-generic midi-ast output-file-path pretty-print?)
  (if (file-exists? output-file-path) (delete-file output-file-path))
  (let* ((op (open-output-file output-file-path)))
    (render-as-laml-to-output-port midi-ast op laml-midi-file-prefix)
    (close-output-port op))
  (midi-time-report "AST written to file done")

  (if pretty-print?
      (begin
	(midi-report "Linearized LAML source string has been written to file. Now pretty printing...")
	(lib-load "file-read.scm") ; ikke nødvendig - er allerede loaded.
	(lib-load "scheme-pretty-printing.scm") 
	(set! use-single-lining #t)
	(set! indentation-delta 3)
	(set! prefered-maximum-width 180)
	(pretty-print-lisp-file output-file-path output-file-path)
	(midi-time-report "Pretty printing done")
	(midi-report (string-append "The file " output-file-path " has been created.")  )
	(midi-report "You can edit the LAML file, process it, and reconstruct a midi file from it."))))


(define (midi-header-chunck-as-list-to-ast midi-header-chunck mode)
  (let ((format (format-of-midi-header midi-header-chunck))
        (number-of-tracks (number-of-tracks-of-midi-header midi-header-chunck))
        (ppqn (ppqn-of-midi-header midi-header-chunck)))
    (MidiHeader 'format (as-string format) 'numberOfTracks (as-string number-of-tracks)
                'pulsesPerQuarterNote (as-string ppqn) 'mode (as-string mode) 'counterTransposition (as-string counter-transposition))))


; Convert a single track (on list form) to a MidiTrack AST.
; mode is either raw, absTime or deltaTime (a symbol).
; raw: delta times with NoteOn and NoteOff.
; absTime: absolute times with durations.
; deltaTime: delta times with duration.
(define (midi-track-chunck-as-list-to-ast midi-track-chunck mode)
 (cond ((eq? mode 'raw)
          (MidiTrack
           (map midi-track-event-as-list-to-ast (data-of-midi-track midi-track-chunck))))
       ((eq? mode 'absTime)
          (track-abs-time-on-off-to-abs-time-with-duration
           (track-delta-to-abs-time
            (MidiTrack
             (map midi-track-event-as-list-to-ast (data-of-midi-track midi-track-chunck))))))
       ((eq? mode 'absTime-on-off)
         (track-delta-to-abs-time
            (MidiTrack
             (map midi-track-event-as-list-to-ast (data-of-midi-track midi-track-chunck)))))
       ((eq? mode 'deltaTime)
          (track-abs-time-with-duration-to-delta-time-with-duration
           (track-abs-time-on-off-to-abs-time-with-duration
            (track-delta-to-abs-time
             (MidiTrack
              (map midi-track-event-as-list-to-ast (data-of-midi-track midi-track-chunck))))) 0))
       (else (laml-error "midi-track-chunck-as-list-to-ast: Unknown mode:" mode))))
       

; Convert a single track (on list form) to an AST.
; Sample transformations back and forth:
; (define (midi-track-chunck-as-list-to-ast midi-track-chunck)
;    (track-abs-to-delta-time
;     (track-abs-time-with-duration-to-abs-time-on-off
;      (track-delta-with-duration-to-abs-time-with-duration
;       (track-abs-time-with-duration-to-delta-time-with-duration
;        (track-abs-time-on-off-to-abs-time-with-duration
;         (track-delta-to-abs-time
;          (MidiTrack
;           (map midi-track-event-as-list-to-ast (data-of-midi-track midi-track-chunck))))) 0 ))) 0))

; An amount of transposition introduced to show note info that corresponds to actual playing at the keyboard.
(define counter-transposition 0)


; Convert at single track-event, as constructed by make-track-event, to an AST.
; There are many different forms of the resulting AST!
(define (midi-track-event-as-list-to-ast track-event)
  (let* ((delta-time (delta-time-of-track-event track-event))    ; int
         (midi-event (midi-event-of-track-event track-event))    
         (status-byte (status-byte-of-midi-event midi-event))    ; int
         (rest-bytes (rest-bytes-of-midi-event midi-event))      ; binary data - byte string
        )
   (cond ((note-off-event? status-byte)                (make-midi-ast "NoteOff" 'deltaTime (as-string delta-time) 'channel (as-string (channel-of-status-byte status-byte)) 
                                                               'note (get-byte-number-as-string rest-bytes 0) 'velocity (get-byte-number-as-string rest-bytes 1) 
                                                               'info (note-number-to-note-name (as-number (get-byte-number-as-string rest-bytes 0)))
                                                       ))

         ((note-on-event? status-byte)                 (let ((note-number (get-byte-number rest-bytes 0))
                                                             (channel-number (channel-of-status-byte status-byte)))
                                                         (make-midi-ast "NoteOn" 'deltaTime (as-string delta-time) 'channel (as-string channel-number) 
                                                                 'note (as-string note-number) 'velocity (get-byte-number-as-string rest-bytes 1)
                                                                 'info (string-append
                                                                        (note-number-to-note-name (as-number (get-byte-number-as-string rest-bytes 0)))
                                                                        (if (and info-about-drum-sounds? (drum-sound-channel? channel-number))
                                                                            (string-append " " (drum-info-string note-number))
                                                                            "")))
                                                       ))
         ((polyphonic-aftertouch-event? status-byte)   (make-midi-ast "PolyKeyPressure" 'deltaTime (as-string delta-time) 'channel (as-string (channel-of-status-byte status-byte)) 
                                                               'note (get-byte-number-as-string rest-bytes 0) 'pressure (get-byte-number-as-string rest-bytes 1))) 
         
         ((control-change-event? status-byte)          
             (let ((control-number (get-byte-number rest-bytes 0))
                   (control-value  (get-byte-number rest-bytes 1)))
               (cond ((= control-number 123)       (make-midi-ast "AllSoundOff" 'deltaTime (as-string delta-time) 'channel (as-string (channel-of-status-byte status-byte)) ))
                     ((= control-number 124)       (make-midi-ast "OmniOff" 'deltaTime (as-string delta-time) 'channel (as-string (channel-of-status-byte status-byte)) ))
                     ((= control-number 125)       (make-midi-ast "OmniOn" 'deltaTime (as-string delta-time) 'channel (as-string (channel-of-status-byte status-byte)) ))
                     ((= control-number 126)       (make-midi-ast "MonoMode" 'deltaTime (as-string delta-time) 'channel (as-string (channel-of-status-byte status-byte)) ))  ; ??
                     ((= control-number 127)       (make-midi-ast "PolyMode" 'deltaTime (as-string delta-time) 'channel (as-string (channel-of-status-byte status-byte)) ))  ; ??
                     (else                         (begin
                                                      (do-remember-control-data! (channel-of-status-byte status-byte) control-number control-value)
                                                      (make-midi-ast "ControlChange" 'deltaTime (as-string delta-time) 'channel (as-string (channel-of-status-byte status-byte))
                                                                  'control (as-string control-number) 'value (as-string control-value) 
                                                                  'info (control-info-string control-number control-value))))
               )
             )              
         )
 
         ((program-change-event? status-byte)          (let ((res-ast 
                                                              (make-midi-ast "ProgramChange" 'deltaTime (as-string delta-time) 
                                                                'info (program-change-info (channel-of-status-byte status-byte) (get-byte-number rest-bytes 0))
                                                                'channel (as-string (channel-of-status-byte status-byte)) 
                                                                'number (get-byte-number-as-string rest-bytes 0))))
                                                          (do-reset-control-data!)
                                                          res-ast
                                                        ))
 
         ((channel-pressure-event? status-byte)        (make-midi-ast "ChannelKeyPressure" 'deltaTime (as-string delta-time) 
                                                               'channel (as-string (channel-of-status-byte status-byte)) 
                                                               'pressure (get-byte-number-as-string rest-bytes 0)))

         ; Corrected January 31, 2008:
         ; Tricky conversion: The two data bytes are 7 bit quantities (high-order bit is 0).
         ; In addition, the first data byte is the most significant.
         ((pitch-wheel-event? status-byte)               (if use-correct-pitch-bend-calculations
                                                             (make-midi-ast "PitchBendChange" 'deltaTime (as-string delta-time) 
                                                               'channel (as-string (channel-of-status-byte status-byte)) 
                                                               'value (as-string (+ (* (get-byte-number rest-bytes 1) 128) (get-byte-number rest-bytes 0))) )
                                                             (make-midi-ast "PitchBendChange" 'deltaTime (as-string delta-time) 
                                                               'channel (as-string (channel-of-status-byte status-byte)) 
                                                               'value (as-string (+ (* (get-byte-number rest-bytes 0) 256) (get-byte-number rest-bytes 1))) )
                                                         )
         )

         (else                        
          (cond
           ((system-exclusive-event? status-byte)           (make-midi-ast-contents "SysEx" (binary-to-hex-string rest-bytes) 'deltaTime (as-string delta-time)  
                                                                                    'info (sys-ex-info-string status-byte rest-bytes)
                                                                   ) )
                                                                                                           ; most probably: Take the last byte of rest-bytes away.

           ((mtc-quater-frame-message-event? status-byte)   (make-midi-ast "MTCQuarterFrame" 'deltaTime (as-string delta-time) 'value (get-byte-number-as-string rest-bytes 0)) ) 
           ((song-position-pointer-event? status-byte)      (SongPositionPointer 'deltaTime (as-string delta-time) 'position "???") )  ; Not complete!
           ((song-select-event? status-byte)                (SongSelect 'deltaTime (as-string delta-time) 'number "???") )             ; Not complete!
           ((tune-request-event? status-byte)               (TuneRequest 'deltaTime (as-string delta-time)) ) 
           ((midi-clock-event? status-byte)                 (TimingClock 'deltaTime (as-string delta-time)) ) 
           ((midi-tick-event? status-byte)                  (laml-error "Encountered problematic midi-tick-event") )  ; !!??
           ((midi-start-event? status-byte)                 (Start  'deltaTime (as-string delta-time)) ) 
           ((midi-continue-event? status-byte)              (Continue 'deltaTime (as-string delta-time)) ) 
           ((midi-stop-event? status-byte)                  (Stop  'deltaTime (as-string delta-time))) 
           ((midi-active-sense? status-byte)                (ActiveSensing 'deltaTime (as-string delta-time)) ) 
           ((meta-event? status-byte)                       
                                                            (let* ((type (get-byte-number-as-string rest-bytes 0))
                                                                   (vlq-info (parse-variable-length-quantity rest-bytes 1))
                                                                   (vlq-length (car vlq-info))
                                                                   (vlq (cdr vlq-info))
                                                                   (rest-bytes-lgt (string-length rest-bytes))
                                                                   (rest-bytes (substring rest-bytes (+ 1 vlq-length) rest-bytes-lgt))
                                                                  )
                                                              (make-midi-ast-contents "Meta" (if (member (as-number type) textual-meta-types)
                                                                                               rest-bytes
                                                                                               (binary-to-hex-string 
                                                                                                rest-bytes))
                                                                    'deltaTime (as-string delta-time) 
                                                                    'type type
                                                                     'info (meta-info-string (as-number type) rest-bytes)
                                                                     )) )
           (else (laml-error "midi-track-event-as-list-to-ast: Non-existing system common, real-time and system exclusive event. Status-byte: " (as-string status-byte)))
          )
         )
   )
  )
)

; ----------------------------------------------------------------------------------------
; The following two functions are introduced to avoid calling time-consming
; mirror functions from non-user-related contexts:

(define (make-midi-ast kind-string . attributes) 
  (make-ast kind-string '() attributes 'single 'midi '()))

(define (make-midi-ast-contents kind-string contents . attributes) 
  (make-ast kind-string (list contents) attributes 'double 'midi '()))

; ---------------------------------------------------------------------------------------------------
; Imperative stuff:

; Global variables used for remembering certain control changes: Bank select MSB/LSB.
; Contain a cons pair (channel . value). Thus the information pertain to a given channel.
; Default (#t . 0) leads the way to General Midi (GM) voices. #t means any channel (really, disregard channel).
(define previous-msb-control-channel-value '(#t . 0))
(define previous-lsb-control-channel-value '(#t . 0))

(define (do-remember-control-data! channel-number control-number control-value)
  (cond ((= 0 control-number) (set! previous-msb-control-channel-value (cons channel-number control-value)))
        ((= 32 control-number) (set! previous-lsb-control-channel-value (cons channel-number control-value)))
        (else 'do-nothing)))

(define (do-reset-control-data!)
  (set! previous-msb-control-channel-value '(#t . 0))
  (set! previous-lsb-control-channel-value '(#t . 0)) )

; ---------------------------------------------------------------------------------------------------
  

(define (drum-info-string note)
  (case note
     (( 13)  "Latin Perc. Surdo Mute")
     (( 14)  "Latin Perc. Surdo open")
     (( 15)  "Hi Q")
     (( 16)  "Whip Slap ")
     (( 17)  "Scratch H")
     (( 18)  "Scratch L")
     (( 19)  "Finger Snap")
     (( 20)  "Click noice")
     (( 21)  "Metronome Click")
     (( 22)  "Metronome Bell")
     (( 23)  "Seq Click L")
     (( 24)  "Seq Click H")
     (( 25)  "Snare. Brush tab")
     (( 26)  "Snare. Brush swirl")
     (( 27)  "Snare. Brush slap")
     (( 28)  "Snare. Brush tap swirl")
     (( 29)  "Snare Rool")
     (( 30)  "Castanet")
     (( 31)  "Snare soft")
     (( 32)  "Sticks")
     (( 33)  "Bass drum. Kick soft")
     (( 34)  "Snare drum. Open rim shot")
     (( 35)  "Acoustic Bass Drum ")
     (( 36)  "Bass Drum 1")
     (( 37)  "Side Stick")
     (( 38)  "Acoustic Snare")
     (( 39)  "Hand Clap")
     (( 40)  "Electric Snare")
     (( 41)  "Low Floor Tom")
     (( 42)  "Closed Hi-Hat")
     (( 43)  "High Floor Tom")
     (( 44)  "Pedal Hi-Hat")
     (( 45)  "Low Tom")
     (( 46)  "Open Hi-Hat")
     (( 47)  "Low-Mid Tom")
     (( 48)  "Hi-Mid Tom")
     (( 49)  "Crash Cymbal 1")
     (( 50)  "High Tom")
     (( 51)  "Ride Cymbal 1")
     (( 52)  "Chinese Cymbal")
     (( 53)  "Ride Bell")
     (( 54)  "Tambourine")
     (( 55)  "Splash Cymbal")
     (( 56)  "Cowbell")
     (( 57)  "Crash Cymbal 2")
     (( 58)  "Vibraslap")
     (( 59)  "Ride Cymbal 2")
     (( 60)  "Hi Bongo")
     ((61)  "Low Bongo")
     ((62)  "Mute Hi Conga")
     ((63)  "Open Hi Conga")
     ((64)  "Low Conga")
     ((65)  "High Timbale")
     ((66)  "Low Timbale")
     ((67)  "High Agogo")
     ((68)  "Low Agogo")
     ((69)  "Cabasa")
     ((70)  "Maracas")
     ((71)  "Short Whistle")
     ((72)  "Long Whistle")
     ((73)  "Short Guiro")
     ((74)  "Long Guiro")
     ((75)  "Claves")
     ((76)  "Hi Wood Block")
     ((77)  "Low Wood Block")
     ((78)  "Mute Cuica")
     ((79)  "Open Cuica")
     ((80)  "Mute Triangle")
     ((81)  "Open Triangle")
     ((82)  "Shaker")
     ((83)  "Jingle Bells")
     ((84)  "Bell tree") 
     (else "")))
    

(define (meta-info-string type rest-bytes)
  (cond ((= type 0) "Sequence number")
        ((= type 1) "Text") 
        ((= type 2) "Copyright") 
        ((= type 3) "Sequence/track name") 
        ((= type 4) "Instrument name") 
        ((= type 5) "Lyric") 
        ((= type 6) "Marker")         
        ((= type 7) "Cue point") 
        ((= type 8) "Program name (patch/voice)")         
        ((= type 9) "Devise/port name") 
        ((= type 47) "End of track")         
        ((= type 81)
           (let ((tempo-microsec-per-qn (byte-string-to-integer rest-bytes)))
               (string-append "Tempo: " (as-string tempo-microsec-per-qn) " microsec. per quarter note = " 
                               (as-int-string (to-int (/ 60000000 tempo-microsec-per-qn))) " BPM. Setter: (tempo= BPM)")))
        ((= type 84) "SMPTE Offset: hh mm se fr ff")         
        ((= type 88) (string-append "Time signature " (let* ((tsil (time-signature-info-list rest-bytes))
                                                             (num (first tsil))
                                                             (denom (second tsil)))
                                                        (string-append 
                                                         (as-string num) ":" (as-string denom)))))
        ((= type 89) (string-append "Key Signature " (key-signature-info rest-bytes)))

        ; Chord Name meta
        ((and (= type 127) (= (string-length rest-bytes) 7)
              (= 67 (get-byte-number rest-bytes 0))  ; 43 hex
              (= 123 (get-byte-number rest-bytes 1)) ; 7B hex
              (= 1 (get-byte-number rest-bytes 2)))   ; 01 hex     
           (chord-name-info (get-byte-number rest-bytes 3) (get-byte-number rest-bytes 4)))

        ((= type 127) "Proprietary event")         
        (else "???")         
  )
)

; cr is chord root (encoded) and ct is chord type number
; Return chord name. Takes counter transposition into account.
(define (chord-name-info cr ct)
 (let ((chord-type-name (lookup-chord-type-short-name ct)))
  (string-append "Chord: " 
    (chord-root-info cr)
    (if (equal? chord-type-name "M") ; major is default, and it is not shown explicitly
        ""
        chord-type-name)
    (if (not (= counter-transposition 0))   ; display counter transposition
            (if (> (- counter-transposition) 0)
                (string-append "+" (as-string (- counter-transposition)))
                (as-string (- counter-transposition)))
            ""))))


(define (chord-root-info cr)
  (let* ((decoration  (quotient cr 16))                       ; most significant HALF byte
         (natural-note (remainder cr 16))                     ; least significant HALF byte
         (note-number (calculate-half-step-note-number natural-note decoration))   ; C = 0, C# = 1, ... B = 11
         (note-number-after-counter-transposition (remainder (+ note-number counter-transposition) 12))
         (note-number-after-counter-transposition-positive      ; Correct?
             (if (< note-number-after-counter-transposition 0)
                 (+ note-number-after-counter-transposition 12)
                 note-number-after-counter-transposition))
        )
    (name-of-note note-number-after-counter-transposition-positive)))

  ; Conditional in earlier version - not well-suited for counter transposition:
  ;    (string-append
  ;      (cond ((= natural-note 0) "??")
  ;            ((= natural-note 1) "C")
  ;            ((= natural-note 2) "D")
  ;            ((= natural-note 3) "E")
  ;            ((= natural-note 4) "F")
  ;            ((= natural-note 5) "G")
  ;            ((= natural-note 6) "A")
  ;            ((= natural-note 7) "B")
  ;            (else "??"))
  ;      (cond ((= decoration 0) "bbb")
  ;            ((= decoration 1) "bb")
  ;            ((= decoration 2) "b")
  ;            ((= decoration 3) "")   ; natural
  ;            ((= decoration 4) "#")
  ;            ((= decoration 5) "##")
  ;            ((= decoration 6) "###")
  ;            (else "??")))


(define (calculate-half-step-note-number natural-note decoration)
  (let ((note-half-step-based
           (cond ((= natural-note 1) 0)  ; C 
                 ((= natural-note 2) 2)  ; D
                 ((= natural-note 3) 4)  ; E
                 ((= natural-note 4) 5)  ; F
                 ((= natural-note 5) 7)  ; G
                 ((= natural-note 6) 9)  ; A
                 ((= natural-note 7) 11) ; B
                 (else (laml-error "calculate-half-step-note-number: Unexpected natural note"))))
        (decoration-offset 
           (cond ((= decoration 0) -3)   ; bbb
                 ((= decoration 1) -2)   ; bb
                 ((= decoration 2) -1)   ; b
                 ((= decoration 3)  0)   ; Natural
                 ((= decoration 4)  1)   ; #
                 ((= decoration 5)  2)   ; ##
                 ((= decoration 6)  3) ; ###
                 (else (laml-error "calculate-half-step-note-number: Unexpected decoration - b or #")))))
    (+ note-half-step-based decoration-offset)))
                 


(define (key-signature-info rest-bytes)
  (let* ((sf (as-number (string-ref rest-bytes 0)))
         (sf-signed (as-two-complement-signed-number sf 8))
         (mi (as-number (string-ref rest-bytes 1)))
        ) 
   (string-append
       (cond ((< sf-signed 0)   ; flat   b'er
                 (make-string (- sf-signed) #\b))
             ((> sf-signed 0)   ; flat   b'er
                 (make-string sf-signed #\#))
             ((= sf-signed 0) "neutral (C)"))
       " "
       (cond ((= mi 1) "minor (mol)")
             ((= mi 0) "major (dur)")
             (else "???")) 
       ". "
       "Setter: (key-signature= 'flat/neutral/sharp number major/minor)")))
       
; Return a list of two integers, such as '(4 4) or '(3 4)
(define (time-signature-info-list str)
  (let* ((num (as-number (string-ref str 0)))
         (denom (as-number (string-ref str 1)))
         (number-of-midi-clock-in-metronome-click (as-number (string-ref str 2))) ; not shown
                                        ; one additional byte is present
        )
    (list num (power 2 denom))))


; Return short informal, intuitive description of a control change message with a given
; control-number and control-value.
(define (control-info-string control-number control-value)
  (cond ((= control-number 0) "Bank select MSB")
        ((= control-number 1)  "Modulation wheel MSB")
        ((= control-number 2)  "Breath control MSB")
        ((= control-number 3)  "Undefined")
        ((= control-number 4)  "Foot control MSB")
        ((= control-number 5)  "Portamento time MSB")
        ((= control-number 6)  "Data entry MSB")
        ((= control-number 7)  "Channel volume MSB")
        ((= control-number 8)  "Balance MSB")
        ((= control-number 9)  "Undefined")
        ((= control-number 10)  "Pan MSB")
        ((= control-number 11)  "Expression controller MSB")
        ((= control-number 12)  "Effect control 1 MSB")
        ((= control-number 13)  "Effect control 2 MSB")
        ((= control-number 14)  "Undefined")
        ((= control-number 15)  "Undefined")
        ((= control-number 16)  "General purpose controller 1 MSB")
        ((= control-number 17)  "General purpose controller 2 MSB")
        ((= control-number 18)  "General purpose controller 3 MSB")
        ((= control-number 19)  "General purpose controller 4 MSB")
        ((and (>= control-number 20) (<= control-number 31))  "Undefined")
        ((= control-number 32) "Bank select LSB")
        ((= control-number 33)  "Modulation wheel LSB")
        ((= control-number 34)  "Breath control LSB")
        ((= control-number 35)  "Undefined")
        ((= control-number 36)  "Foot control LSB")
        ((= control-number 37)  "Portamento time LSB")
        ((= control-number 38)  "Data entry LSB")
        ((= control-number 39)  "Channel volume LSB")
        ((= control-number 40)  "Balance LSB")
        ((= control-number 41)  "Undefined")
        ((= control-number 42)  "Pan LSB")
        ((= control-number 43)  "Expression controller LSB")
        ((= control-number 44)  "Effect control 1 LSB")
        ((= control-number 45)  "Effect control 2 LSB")
        ((= control-number 46)  "Undefined")
        ((= control-number 47)  "Undefined")
        ((= control-number 48)  "General purpose controller 1 LSB")
        ((= control-number 49)  "General purpose controller 2 LSB")
        ((= control-number 50)  "General purpose controller 3 LSB")
        ((= control-number 51)  "General purpose controller 4 LSB")
        ((and (>= control-number 52) (<= control-number 63))  "Undefined")
        ((= control-number 64)  (string-append "Sustain") )   ; Earlier also "Hold pedal" together with (on-off control-value)
        ((= control-number 65)  (string-append "Portamento " (on-off control-value)))
        ((= control-number 66)  (string-append "Sustenuto " (on-off control-value)))
        ((= control-number 67)  (string-append "Soft pedal " (on-off control-value))"")
        ((= control-number 69)  "Hold 2")
        ((= control-number 70)  "Sound controller 1: Sound Variation")
        ((= control-number 71)  "Sound controller 2: Timbre")
        ((= control-number 72)  "Sound controller 3: Release time")
        ((= control-number 73)  "Sound controller 4: Attack time")
        ((= control-number 74)  "Sound controller 5: Brightness")
        ((= control-number 75)  "Sound controller 6")
        ((= control-number 77)  "Sound controller 7")
        ((= control-number 78)  "Sound controller 8")
        ((= control-number 79)  "Sound controller 9")
        ((= control-number 80)  (string-append "General purpose button 5 " (on-off control-value)))
        ((= control-number 81)  (string-append "General purpose button 6 ") (on-off control-value))
        ((= control-number 82)  (string-append "General purpose button 7 ") (on-off control-value))
        ((= control-number 83)  (string-append "General purpose button 8 ") (on-off control-value))
        ((= control-number 84)  "Portamento Control")
        ((and (>= control-number 85) (<= control-number 90))  "Undefined")
        ((= control-number 91)  "Reverb send level")
        ((= control-number 92)  "Effects 2 depth")
        ((= control-number 93)  "Chorus send level ")
        ((= control-number 94)  "Effects 4 depth")
        ((= control-number 95)  "Effects 5 depth")
        ((= control-number 96)  "Data entry +1")
        ((= control-number 97)  "Data entry -1")
        ((= control-number 98)  "Non-registered parameter number LSB")
        ((= control-number 99)  "Non-registered parameter number MSB")
        ((= control-number 100)  "Registered parameter number LSB")                   ; 100 and
        ((= control-number 101)  "Registered parameter number MSB")                   ; 101 encodes for pitch bend range. Depends on following data entry messages.
        ((and (>= control-number 102) (<= control-number 119))  "Undefined")
        ; The rest have special ASTs.
        (else "???")
   )
)

(define (on-off control-value)
  (if (< control-value 63) "off" "on"))

; The meta types for which we do NOT convert the contents to a hex string.
(define textual-meta-types
  (list 1 2 3 4 5 6 7 8 9))



(define (sys-ex-info-string status-byte rest-bytes)
  (let* ((lgt (string-length rest-bytes))
         (var-length-quantity (parse-variable-length-quantity rest-bytes 0))   ; After the status-byte (F7 or F0) comes a variable-length quantity which
                                                                               ; (somewhat redundantly) measures the length of rest-bytes.
         (vlq-number-of-bytes (car var-length-quantity))
         (sys-ex-rest-bytes (substring rest-bytes vlq-number-of-bytes lgt))
        )
   (cond ((tyros-style-no-sysex-code? rest-bytes) (string-append "Tyros style: "
                                                                (lookup-tyros-style (as-number (string-ref rest-bytes 7)) (as-number (string-ref rest-bytes 8)))))

         ((gm-reset-sysex-code? sys-ex-rest-bytes) (string-append "GM Reset: " (binary-to-hex-string sys-ex-rest-bytes)))

         (else  ; The system exclusive bytes without the leading variable length quantity which gives the length of rest-bytes.
             (string-append "  " (binary-to-hex-string sys-ex-rest-bytes)))
   )
  )
)

(define (gm-reset-sysex-code? byte-str)
  (and (= 5 (string-length byte-str))
       (eqv? (string-ref byte-str 0) (make-char-2-hex 7 14))  ; 7E
       (eqv? (string-ref byte-str 1) (make-char-2-hex 7 15))  ; 7F
       (eqv? (string-ref byte-str 2) (make-char-2-hex 0  9))  ; 09
       (eqv? (string-ref byte-str 3) (make-char-2-hex 0  1))  ; 01
       (eqv? (string-ref byte-str 4) (make-char-2-hex 15 7))  ; F7
  ))


(define (tyros-style-no-sysex-code? byte-str)
  (and  (= 10 (string-length byte-str))
        (eqv? (string-ref byte-str 1) (make-char-2-hex 4 3))    ; first byte after vlq is hex 43
        (eqv? (string-ref byte-str 2) (make-char-2-hex 7 3))    ; etc...
        (eqv? (string-ref byte-str 3) (make-char-2-hex 7 12))
        (eqv? (string-ref byte-str 4) (make-char-2-hex 1 1))
        (eqv? (string-ref byte-str 5) (make-char-2-hex 0 0))
        (eqv? (string-ref byte-str 6) (make-char-2-hex 1 6))))

; Cache for lookup-tyros-style, to prevent multiple reading of the same file.
(define style-list '())       ; assigned later
(define style-list-length 0)  ; assigned later

; Style number is (+ (* x 128) y) in  c:/Program Files/MidiPlayer/Tyrosstyles.dat
(define (lookup-tyros-style x y)
  (if (null? style-list)
      (begin
        (set! style-list (file-read (string-append midi-software-dir "data/Tyrosstyles.dat")))
        (set! style-list-length (length style-list))))
  
  ; Assume that the style-list is sorted by the numbers (+ (* x 128) y)

  (let ((idx (+ (* x 128) y)))
     (if (and (>= idx 0) (< idx style-list-length))
         (as-string (list-ref style-list (+ (* x 128) y)))
         "??")))



(define (program-change-info channel-number program-number)
  (if (and previous-msb-control-channel-value
           previous-lsb-control-channel-value
           (or (eq? (car previous-msb-control-channel-value) #t) (= (car previous-msb-control-channel-value) channel-number))
           (or (eq? (car previous-lsb-control-channel-value) #t) (= (car previous-lsb-control-channel-value) channel-number)))
      (let ((voice-info (find-voice-info-string (cdr previous-msb-control-channel-value) (cdr previous-lsb-control-channel-value) program-number)))
         (if voice-info
             (string-append "**" (as-string voice-info))
             ""))
      "??"))

; A list of voice entries. Each entry is of the format (msb lsb program-number "explanation").
(define voice-list '())

(define (find-voice-info-string msb lsb pn)
  (if (and (boolean? preferred-instrument) (not preferred-instrument)) (read-and-set-preferred-instrument-from-environment))
  (cond ((eq? preferred-instrument 'tyros)    (find-tyros-voice msb lsb pn))
        ((eq? preferred-instrument 'motif-xs) (find-motif-xs-voice msb lsb pn))
        ((eq? preferred-instrument 'pc-sound-card) (find-general-midi-voice msb lsb pn))
        (else (laml-error "find-voice-info-string: Unsupported instrument" preferred-instrument))))


; msb, lsb, and pn are all numbers
; Lookup the name of the voice in the Tyros1 voice data file.
; Return #f if a voice name cannot be located.
; Should use selectors to the representation in "data/Tyrosvoices.dat"
(define (find-tyros-voice msb lsb pn)
  (if (null? voice-list)
      (set! voice-list (file-read (string-append midi-software-dir "data/Tyrosvoices.dat"))))

  (let ((res (find-in-list
              (lambda (voice-entry)
                (and
                 (= msb (car voice-entry))
                 (= lsb (cadr voice-entry))
                 (= pn (caddr voice-entry))))
              voice-list)))
    (if res
        (string-append "T1 " (cadddr res))
        #f)))

; Voice data selectors - relative the representation of voice data lists

(define get-xs-voice-collection (make-selector-function 1 "get-xs-voice-collection"))
(define get-xs-voice-msb (make-selector-function 2 "get-xs-voice-msb"))
(define get-xs-voice-lsb (make-selector-function 3 "get-xs-voice-msb"))
(define get-xs-voice-pc  (make-selector-function 4 "get-xs-voice-msb"))
(define get-xs-voice-name  (make-selector-function 5 "get-xs-voice-name"))
(define get-xs-voice-main-cat-1  (make-selector-function 6 "get-xs-voice-main-cat-1"))
(define get-xs-voice-sub-cat-1  (make-selector-function 7 "get-xs-voice-sub-cat-1"))
(define get-xs-voice-main-cat-2  (make-selector-function 8 "get-xs-voice-main-cat-2"))
(define get-xs-voice-sub-cat-2  (make-selector-function 9 "get-xs-voice-sub-cat-2"))
(define get-xs-voice-element  (make-selector-function 10 "get-xs-voice-element"))
(define get-xs-voice-comment  (make-selector-function 11 "get-xs-voice-comment"))

(define (get-gm-voice-msb e) 0)
(define (get-gm-voice-lsb e) 0)
(define get-gm-voice-pc (make-selector-function 3 "get-gm-voice-pc"))
(define get-gm-voice-name (make-selector-function 4 "get-gm-voice-name")) 
(define get-gm-voice-cat (make-selector-function 5 "get-gm-voice-cat")) 



; msb, lsb, and pn are all numbers
; Lookup the name of the voice in the Motif XS voice data file via the selectors from above.
; Return #f if a voice name cannot be located.
(define (find-motif-xs-voice msb lsb pn)
  (if (null? voice-list)
      (set! voice-list (file-read (string-append midi-software-dir "data/Motif-xs-voices.dat"))))

  (cond ((and (= msb 63) (= lsb 8))  "XS User1 Voice")
        ((and (= msb 63) (= lsb 9))  "XS User2 Voice")
        ((and (= msb 63) (= lsb 10)) "XS User3 Voice")
        ((and (= msb 63) (= lsb 40)) "XS Drum user Voice")
        ((and (= msb 63) (= lsb 64)) "XS Performance user 1 Voice")
        ((and (= msb 63) (= lsb 65)) "XS Performance user 2 Voice")
        ((and (= msb 63) (= lsb 66)) "XS Performance user 3 Voice")
        ((and (= msb 63) (= lsb 50)) "XS Sample Voice")
        ((and (= msb 63) (= lsb 60)) "XS Mixing Voice")
        ((or (and (= msb 63) (= lsb 32))          ; Drums
             (and (= msb 127) (= lsb 0)))
           (let ((res (find-in-list
                       (lambda (voice-entry)
                         (and
                          (= msb (get-xs-voice-msb voice-entry))
                          (= lsb (get-xs-voice-lsb voice-entry))
                          (= pn (get-xs-voice-pc voice-entry))))
                       voice-list)))
             (if res
                 (render-motif-xs-voice-string (get-xs-voice-name res))
                 #f))
        )
        (else (let ((res (find-in-list
                          (lambda (voice-entry)
                            (and
                             (= msb (get-xs-voice-msb voice-entry))
                             (= lsb (get-xs-voice-lsb voice-entry))
                             (= pn (get-xs-voice-pc voice-entry))))
                          voice-list)))
                (if res
                    (render-motif-xs-voice-string (get-xs-voice-name res) (get-xs-voice-main-cat-1 res) (get-xs-voice-sub-cat-1 res))
                    #f)))))

(define (find-general-midi-voice msb lsb pn)
  (if (null? voice-list)
      (set! voice-list (file-read (string-append midi-software-dir "data/general-midi-voices.dat"))))

  (let ((res (find-in-list
                       (lambda (gm-voice-entry)
                         (and
                          ; disregard msb and lsb - until further at least
                          (= pn (get-gm-voice-pc gm-voice-entry))))
                       voice-list)))
             (if res
                 (string-append "GM " (get-gm-voice-cat res) ": " (get-gm-voice-name res))
                 #f)))  


(define (render-motif-xs-voice-string voice-name . optional-parameter-list)
 (let ((main-cat (optional-parameter 1 optional-parameter-list #f))
       (sub-cat  (optional-parameter 2 optional-parameter-list #f))
      )
  (if (and main-cat sub-cat)
      (string-append "XS " voice-name " - " (as-string main-cat) " " (as-string sub-cat))
      (string-append "XS " voice-name))))


; ===============================================================================================================
; TRANSFORMATION OF XML-IN-LAML ASTs to binary MIDI format. 
; Not done via the intermediate list format. Functions return strings of bytes (binary information).
; ...-ast-to-bin functions.

; Top level function that transforms an overall standard midi file AST to the binary midi format.
; Returns a string (of bytes) - binary data. 
(define (standard-midi-file-ast-to-bin smf-ast)
  (let* (
         (dummy-0 (begin (midi-time-report "Start standard-midi-file-ast-to-bin")))
         (header-ast (ast-subtree smf-ast "MidiHeader"))
         (format (as-number (ast-attribute header-ast 'format)))
         (target-format (as-number (ast-attribute header-ast 'target-format format)))

         (target-ppqn (as-number (ast-attribute header-ast 'target-pulsesPerQuarterNote (ast-attribute header-ast 'pulsesPerQuarterNote))))
         (ppqn (as-number (ast-attribute header-ast 'pulsesPerQuarterNote)))
         (ppqn-factor (/ target-ppqn ppqn))

         (smf-ast-1 (cond 
                      ((= format target-format) smf-ast)
                      ((and (= format 1) (= target-format 0)) (track-joined-standard-midi-file-ast smf-ast))
                      (else (laml-error "standard-midi-file-ast-to-bin: Illegal target-format relative to format:" target-format format))))

         (header-ast-1 (ast-subtree smf-ast-1 "MidiHeader"))
         (header-ast-2 (copy-ast-mutate-attributes header-ast-1 'pulsesPerQuarterNote (as-string target-ppqn)))
         (mode-symbol (as-symbol (ast-attribute header-ast-2 'mode)))
                           
         (counter-transp (as-number (ast-attribute header-ast-2 'counterTransposition "0")))
         (track-asts (filter (lambda (x) (and (ast? x) (equal? (ast-element-name x) "MidiTrack"))) (ast-subtrees smf-ast-1)))  ; a list of MidiTrack asts
         (dummy-1 (begin (midi-time-report "Before transformation")))
         (transformed-track-asts   ; TRANSFORMATIONS: To raw mode ast.
             (map (lambda (track-ast) 
                     (generalized-transform-track-to-raw-mode track-ast mode-symbol ppqn-factor))
                   track-asts))  
         (dummy-2 (begin (midi-time-report "After transformation")))
 
         (non-midi-suffix-ast (ast-subtree smf-ast "NonMidiSuffix"))
         (non-midi-suffix-hex-string (if non-midi-suffix-ast 
                                     (ast-text non-midi-suffix-ast)
                                     ""))
        )
    (set! counter-transposition counter-transp)  ; global variable!
    (let ((res (string-append
		(midi-header-ast-to-bin header-ast-2)
		(midi-tracks-ast-to-bin transformed-track-asts)	; transform raw ast to binary form
		(hex-to-binary-string-relaxed non-midi-suffix-hex-string)
		)))
       (midi-time-report "After conversion to binary format")
       res)))        

; Transform a track ast to raw mode from whatever mode mode-system prescribes.
(define (transform-track-to-raw-mode midi-track-ast mode-symbol ppqn-factor)
  (let* ((first-event-ast-in-track (first-ast-in-list (ast-subtrees midi-track-ast)))
         (start-time (if first-event-ast-in-track (as-number (ast-attribute first-event-ast-in-track 'absTime "0")) 0))
        )
  (cond ((eq? mode-symbol 'raw)
            midi-track-ast)                                                             ; the identity transformation
        ((eq? mode-symbol 'absTime)
            (track-abs-to-delta-time
              (track-abs-time-with-duration-to-abs-time-on-off midi-track-ast) start-time ppqn-factor ))
        ((eq? mode-symbol 'absTime-on-off)
            (track-abs-to-delta-time
              midi-track-ast start-time ppqn-factor ))
        ((eq? mode-symbol 'deltaTime)
            (track-abs-to-delta-time
              (track-abs-time-with-duration-to-abs-time-on-off
                (track-delta-with-duration-to-abs-time-with-duration midi-track-ast)) start-time ppqn-factor))
        (else (laml-error "transform-track-to-raw-mode: Unknown mode" mode-symbol)))))

; Return the first ast in x-lst. If no ast is in the list, return #f.
(define (first-ast-in-list x-lst)
  (cond ((null? x-lst) #f)
        ((not (ast? (first x-lst))) (first-ast-in-list (cdr x-lst)))
        (else (first x-lst))))

(define (midi-header-ast-to-bin header-ast)
  (let ((format (ast-attribute header-ast 'format))
        (number-of-tracks (ast-attribute header-ast 'numberOfTracks))
        (ppqn (ast-attribute header-ast 'pulsesPerQuarterNote))
       )
    (string-append "MThd" 
                   (int10-to-binary 6 4)                ; Length of remaining: always 6
                   (int10-to-binary (as-number format) 2)           ; First 2 bytes of the 6 bytes
                   (int10-to-binary (as-number number-of-tracks) 2) ; Next 2 bytes of the 6 bytes
                   (int10-to-binary (as-number ppqn) 2)             ; Final 2 bytes of the 6 bytes
    )))

(define (midi-tracks-ast-to-bin track-ast-list)  ; plural - a list of MidiTrack asts
  (let* ((track-string-list (map midi-one-track-ast-to-bin track-ast-list)))
     (apply string-append track-string-list)))

(define (midi-one-track-ast-to-bin track-ast)   ; track-ast is a MidiTrack ast.
  (let* ((messages-asts (filter ast? (ast-subtrees track-ast)))
         (track-string-list (map single-midi-track-event-ast-to-bin (filter legal-channel? messages-asts)))
         (track-string (apply string-append track-string-list))
        )
    (string-append "MTrk"
                   (int10-to-binary (string-length track-string) 4) ; 4 bytes length of data
                   track-string)))

; mes-ast is legal if it is a non-channel message, or a channel message assigned to a channel between 1 and 16.
(define (legal-channel? mes-ast)
  (let ((ch (ast-attribute mes-ast 'channel #f)))
    (if ch  ; a channel message
        (let ((ch-number (as-number ch)))
           (and (>= ch-number 1) (<= ch-number 16)))
        #t    ; Non-channel messages are considered legal!
    )
  )
)


; Convert a single midi track event ast (such as a NoteOn ast) to binary form.
; Return a string (binary data).
(define (single-midi-track-event-ast-to-bin ast)
 (letrec ((ann (lambda (ast name) (as-number (ast-attribute ast name)))))  ; attribute name as number
  (string-append
   (unparse-integer-as-variable-length-quantity (ann ast 'deltaTime))                  ; deltaTime first, as variable length quantity
   (cond  ((equal? (ast-element-name ast) "NoteOff") 
            (string-append 
               (make-byte-string-from-hex-2 8 (- (ann ast 'channel) 1))                ; status byte: status half byte + channel number
               (int10-to-binary (ann ast 'note) 1)                                     ; one byte, note
               (int10-to-binary (between 0 127 (ann ast 'velocity)) 1)                 ; one byte, velocity
            ))
         ((equal? (ast-element-name ast) "NoteOn")  
            (string-append 
               (make-byte-string-from-hex-2 9 (- (ann ast 'channel) 1))                ; status byte: status half byte + channel number
               (int10-to-binary (ann ast 'note) 1)                                     ; one byte, note
               (int10-to-binary (between 0 127 (ann ast 'velocity)) 1)                 ; one byte, velocity
            )
         ) 
         ((equal? (ast-element-name ast) "PolyKeyPressure") 
            (string-append 
               (make-byte-string-from-hex-2 10 (- (ann ast 'channel) 1))               ; status byte: status half byte + channel number
               (int10-to-binary (ann ast 'note) 1)                                     ; one byte, note
               (int10-to-binary (ann ast 'pressure) 1)                                 ; one byte
            )
         ) 
         ((equal? (ast-element-name ast) "ControlChange")    
            (string-append 
               (make-byte-string-from-hex-2 11 (- (ann ast 'channel) 1))               ; status byte: status half byte + channel number
               (int10-to-binary (ann ast 'control) 1)                                  ; one byte
               (int10-to-binary (ann ast 'value) 1)                                    ; one byte
            )
         ) 
         ((equal? (ast-element-name ast) "ProgramChange") 
            (string-append 
               (make-byte-string-from-hex-2 12 (- (ann ast 'channel) 1))               ; status byte: status half byte + channel number
               (int10-to-binary (ann ast 'number) 1)                                   ; one byte
            )
         ) 
         ((equal? (ast-element-name ast) "ChannelKeyPressure") 
            (string-append 
               (make-byte-string-from-hex-2 13 (- (ann ast 'channel) 1))               ; status byte: status half byte + channel number
               (int10-to-binary (ann ast 'pressure) 1)                                 ; one byte
            )
         ) 
         ((equal? (ast-element-name ast) "PitchBendChange")
            (string-append 
               (make-byte-string-from-hex-2 14 (- (ann ast 'channel) 1))                ; status byte: status half byte + channel number
               (if use-correct-pitch-bend-calculations
                  (pitch-bend-int-to-binary (ann ast 'value))                           ; the value attribute is large integer, represented by two bytes!
                  (int10-to-binary (ann ast 'value) 2))                                 ; Is the two bytes correctly constructed. Yes, as of Janunary 31, 2008.
                                                                                        ; if use-correct-pitch-bend-calculations has the value #t
            )
         )
         ((equal? (ast-element-name ast) "AllSoundOff")    
            (string-append 
               (make-byte-string-from-hex-2 11 (- (ann ast 'channel) 1))                ; status byte: status half byte + channel number
               (int10-to-binary 120 1)                                                  ; one byte. Fixed control number
               (int10-to-binary 0 1)                                                    ; one byte. Not used, defaults to 0
            )
         )
         ((equal? (ast-element-name ast) "ResetAllControllers") 
            (string-append 
               (make-byte-string-from-hex-2 11 (- (ann ast 'channel) 1))                ; status byte: status half byte + channel number
               (int10-to-binary 121 1)                                                  ; one byte. Fixed control number
               (int10-to-binary 0 1)                                                    ; one byte. Not used, defaults to 0
            )
         )
         ((equal? (ast-element-name ast) "LocalControl")  
            (string-append 
               (make-byte-string-from-hex-2 11 (- (ann ast 'channel) 1))                ; status byte: status half byte + channel number
               (int10-to-binary 122 1)                                                  ; one byte. Fixed control number
               (int10-to-binary (if (equal? (ast-attribute ast 'value) "off") 0 127))   ; one byte. 
            )
         )
         ((equal? (ast-element-name ast) "AllNotesOff")
            (string-append 
               (make-byte-string-from-hex-2 11 (- (ann ast 'channel) 1))                ; status byte: status half byte + channel number
               (int10-to-binary 123 1)                                                  ; one byte. Fixed control number
               (int10-to-binary 0 1)                                                    ; one byte. Not used, defaults to 0
            )
         )
         ((equal? (ast-element-name ast) "OmniOff")   
            (string-append 
               (make-byte-string-from-hex-2 11 (- (ann ast 'channel) 1))                ; status byte: status half byte + channel number
               (int10-to-binary 124 1)                                                  ; one byte. Fixed control number
               (int10-to-binary 0 1)                                                    ; one byte. Not used, defaults to 0
            )
         )
         ((equal? (ast-element-name ast) "OmniOn") 
            (string-append 
               (make-byte-string-from-hex-2 11 (- (ann ast 'channel) 1))                ; status byte: status half byte + channel number
               (int10-to-binary 125 1)                                                  ; one byte. Fixed control number
               (int10-to-binary 0 1)                                                    ; one byte. Not used, defaults to 0
            )
         )
         ((equal? (ast-element-name ast) "MonoMode")  
            (string-append 
               (make-byte-string-from-hex-2 11 (- (ann ast 'channel) 1))                ; status byte: status half byte + channel number
               (int10-to-binary 126 1)                                                  ; one byte. Fixed control number
               (int10-to-binary (ann ast 'value) 1)                                     ; one byte.
            )
         )
         ((equal? (ast-element-name ast) "PolyMode") 
            (string-append 
               (make-byte-string-from-hex-2 11 (- (ann ast 'channel) 1))                ; status byte: status half byte + channel number
               (int10-to-binary 127 1)                                                  ; one byte. Fixed control number
               (int10-to-binary 0 1)                                                    ; one byte. Not used, defaults to 0
            )
         )
         ((equal? (ast-element-name ast) "ControlChange14") 
            (laml-error "Cannot emit ControlChange14 AST to binary midi string.") 
         ) 
         ((equal? (ast-element-name ast) "RPNChange")
            (laml-error "Cannot emit RPNChange AST to binary midi string.") 
         )
         ((equal? (ast-element-name ast) "SysEx")    
            (string-append
               (make-byte-string-from-hex-2 15 0)                                       ; one byte  F0. Start sys ex.
               (hex-to-binary-string (ast-text ast))                                    ; includes trailing F7.  Thus next line is not necessary. Therefore comment.
               ; (make-byte-string-from-hex-2 15 7)                                                   ; one byte  F7. End sys ex.
            )
         ) 
         ((equal? (ast-element-name ast) "MTCQuarterFrame")
            (string-append
               (make-byte-string-from-hex-2 15 1)                                       ; one byte  F1.
               (int10-to-binary (ann ast 'value) 1)                                     ; one data byte.               
            )
         )
         ((equal? (ast-element-name ast) "SongPositionPointer") 
            (string-append
               (make-byte-string-from-hex-2 15 2)                                       ; one byte  F2.
               (int10-to-binary (ann ast 'position) 2)                                  ; the value attribute is large integer, represented by two bytes!
                                                                                        ; Is the two bytes correctly constructed???
            )
         )
         ((equal? (ast-element-name ast) "SongSelect")   
            (string-append
               (make-byte-string-from-hex-2 15 3)                                       ; one byte  F3.
               (int10-to-binary (ann ast 'number) 1)                                    ; one data byte.               
            )
         )
         ((equal? (ast-element-name ast) "TuneRequest")  
            (string-append
               (make-byte-string-from-hex-2 15 6)                                       ; one byte  F6.
            )

         )
         ((equal? (ast-element-name ast) "TimingClock") 
            (string-append
               (make-byte-string-from-hex-2 15 8)                                       ; one byte  F8.
            )
         )
         ((equal? (ast-element-name ast) "Start")  
            (string-append
               (make-byte-string-from-hex-2 15 10)                                      ; one byte  FA.
            )
         )
         ((equal? (ast-element-name ast) "Continue") 
            (string-append
               (make-byte-string-from-hex-2 15 11)                                      ; one byte  FB.
            )
         )
         ((equal? (ast-element-name ast) "Stop") 
            (string-append
               (make-byte-string-from-hex-2 15 12)                                      ; one byte  FC.
            )
         )
         ((equal? (ast-element-name ast) "ActiveSensing")
            (string-append
               (make-byte-string-from-hex-2 15 14)                                      ; one byte  FE.
            )
         )         
         ((equal? (ast-element-name ast) "Meta") 
           (let* ((type-number (ann ast 'type))
                  (contents (ast-text ast))
                  (meta-bytes (if (member type-number textual-meta-types) contents (hex-to-binary-string contents)))   ; either ascii string or hex presented string 
                  (vlq-length (unparse-integer-as-variable-length-quantity (string-length meta-bytes)))
                 )
                  
            (string-append 
               (make-byte-string-from-hex-2 15 15)                                      ; one byte  FF.
               (int10-to-binary (ann ast 'type) 1)                                      ; one data byte. The type of the meta event
               vlq-length                                                               ; length of meta-bytes, as a variable length quantity
               meta-bytes                                                               ; the meta bytes as such
            )
           )
         )
         (else (laml-error "single-midi-track-event-ast-to-bin: Unknown element name of AST:" (ast-element-name ast)))

   ))))

; Used for pich bend - decimal to binary conversion
; It is given n is between 0 and 16384 = (power 2 14). 
; Split in two bytes, each with zero as high order bit. Thus, in reality we split the in two 7-bits bytes.
; The least significant byte comes first.
(define (pitch-bend-int-to-binary n)
  (let ((high (remainder n 128))
        (low (quotient n 128)))
    (list->string (list (as-char high) (as-char low)))))



; ===============================================================================================================
; Testing stuff.

; Compare two binary strings.
(define (binary-compare str1 str2)
 (let ((lgt1 (string-length str1))
       (lgt2 (string-length str2)))
  (if (not (= lgt1 lgt2)) (display-message "Not same length" lgt1 lgt2))
  (binary-compare-1 str1 str2 0 (string-length str1) (string-length str2))))
  

(define (binary-compare-1 str1 str2 i lgt1 lgt2)
  (if (< i (min lgt1 lgt2))
    (if (eqv? (string-ref str1 i) (string-ref str2 i))
        (binary-compare-1 str1 str2 (+ i 1) lgt1 lgt2)
        (begin (display-message "Difference at index" i (binary-to-hex-string (as-string (string-ref str1 i))) (binary-to-hex-string (as-string (string-ref str2 i))))
               (binary-compare-1 str1 str2 (+ i 1) lgt1 lgt2)))))



; ===============================================================================================================
; AST Transformations, related to deltaTime, absTime, and duration.


; #1

; Convert raw deltaTime to absTime attributes in the MidiTrack ast midi-track-ast. 
; Removes deltaTime attributes and adds absTime attributes (with both NoteOn and NoteOff messages).
; Does not add durations.
; Returns a revised copy of midi-track-ast (a MidiTrack ast).
; Inverse function: track-abs-to-delta-time.
(define (track-delta-to-abs-time midi-track-ast)
  (let ((message-asts (filter ast? (ast-subtrees midi-track-ast))))
    (MidiTrack 
      (message-list-delta-time-to-abs-time message-asts 0))))

(define (message-list-delta-time-to-abs-time message-ast-list start-time)
  (cond ((null? message-ast-list) '())
        (else (let* ((message-ast (car message-ast-list))
                     (delta-time (ast-attribute message-ast 'deltaTime))
                     (new-abs-time (+ start-time (as-number delta-time)))
                    )
                (cons (single-message-ast-delta-to-abs-time message-ast new-abs-time)
                      (message-list-delta-time-to-abs-time (cdr message-ast-list) new-abs-time))))))

; A mutating alternative to the copying function next to this function. OK, but does not really affect the total running time
; (define (single-message-ast-delta-to-abs-time ast abs-time)
;  (let ((existing-info (ast-attribute ast 'info "")))
;   (set-ast-attributes! ast
;      (append (remove-props! '(deltaTime 'info) (ast-attributes ast))
;              (list
;               'absTime (as-string abs-time) 
;               'info (string-append (bar-beat-clock-string abs-time global-ppqn (first global-signature) (second global-signature)) " " existing-info)
;               )))
;   ast))

(define (single-message-ast-delta-to-abs-time ast abs-time)
 (let* ((existing-info (ast-attribute ast 'info ""))
        (bbcs (bar-beat-clock-string abs-time global-ppqn (first global-signature) (second global-signature)))
        (bbcs-lgt (string-length bbcs))
        (filler (make-string (max 1 (- (- info-max-width 7) bbcs-lgt)) #\space))
       )
  (make-ast (ast-element-name ast) 
            (ast-subtrees ast) 
            (append (list
                       'absTime (as-string abs-time) 
                       'info (string-append bbcs filler existing-info)
                    )
                    (but-props (ast-attributes ast) (list 'deltaTime 'info)))
            (ast-kind ast)
            (ast-language ast)
            (ast-internal-attributes ast))))


; ----------------------------------------------------------------------------------------

; #6
; Convert absTime to low-level deltaTime (without duration) attributes in the MidiTrack ast midi-track-ast.
; midi-track-ast has absTime on/off events.
; Returns a revised copy of midi-track-ast (a MidiTrack ast).
; Inverse function: track-delta-to-abs-time.
; The parameter initial-delta-time is typically 0.
(define (track-abs-to-delta-time midi-track-ast initial-delta-time ppqn-factor)
  (let ((message-asts (filter ast? (ast-subtrees midi-track-ast))))
    (cond ((null? message-asts)
             (MidiTrack))
          (else 
             (MidiTrack
               (let* ((first-message-ast (first message-asts))
                      (first-abs-time (as-number (ast-attribute first-message-ast 'absTime))))
                 (cons (single-message-ast-abs-to-delta-time first-message-ast initial-delta-time ppqn-factor)
                       (message-list-abs-to-delta-time (cdr message-asts) first-abs-time ppqn-factor))))))))
      

; previous-abs-time is the absolute time of the previous message ast.
(define (message-list-abs-to-delta-time message-ast-list previous-abs-time ppqn-factor)
  (cond ((null? message-ast-list) '())
        (else (let* ((message-ast (car message-ast-list))
                     (this-abs-time (as-number (ast-attribute message-ast 'absTime)))
                     (new-delta-time (- this-abs-time previous-abs-time))
                    )
                (cons (single-message-ast-abs-to-delta-time message-ast new-delta-time ppqn-factor)
                      (message-list-abs-to-delta-time (cdr message-ast-list) this-abs-time ppqn-factor))))))

(define (single-message-ast-abs-to-delta-time ast delta-time ppqn-factor)
  (make-ast (ast-element-name ast) 
            (ast-subtrees ast) 
            (append (list 'deltaTime 
                           (if (= ppqn-factor 1)
                               (as-string delta-time)
                               (as-int-string (to-int (* ppqn-factor delta-time)))))
                    (but-props (ast-attributes ast) (list 'absTime)))
            (ast-kind ast)
            (ast-language ast)
            (ast-internal-attributes ast)))

; ----------------------------------------------------------------------------------------

; #2
; Convert absTime with NoteOn and NoteOff messages to absTime with duration.
; Removes all NoteOff messages.
; Returns a revised copy of midi-track-ast (a MidiTrack ast).
(define (track-abs-time-on-off-to-abs-time-with-duration midi-track-ast)
  (let ((message-asts (filter ast? (ast-subtrees midi-track-ast))))
    (MidiTrack 
      (messages-abs-time-on-off-to-abs-time-with-duration message-asts))))

(define (messages-abs-time-on-off-to-abs-time-with-duration message-ast-list)
  (cond ((null? message-ast-list) '())
        ((note-on-ast? (first message-ast-list)) 
           (let* ((current-ast (first message-ast-list))
                  (current-channel (ast-attribute current-ast 'channel))
                  (current-note (ast-attribute current-ast 'note))
                  (note-off-ast (find-in-list 
                                  (lambda (ast ) (and ast
                                                      (note-off-ast? ast) 
                                                      (equal? (ast-attribute ast 'channel) current-channel)
                                                      (equal? (ast-attribute ast 'note) current-note)
                                                      (not (equal? "true" (ast-internal-attribute ast 'seen "false")))))
                                 message-ast-list))
                  (duration (if note-off-ast 
                                (- (as-number (ast-attribute note-off-ast 'absTime)) (as-number (ast-attribute current-ast 'absTime)))
                                0))  ; happens if we cannot find the note-off. Should not happen.
                 )
              (if note-off-ast (set-internal-ast-attribute! note-off-ast 'seen "true"))
              (if (not note-off-ast)  
                  (display-warning "NoteOn without corresponding NoteOff"))
              (cons (single-message-ast-abs-time-add-duration (first message-ast-list) duration)
                    (messages-abs-time-on-off-to-abs-time-with-duration (cdr message-ast-list)))))
 
        ((note-off-ast? (first message-ast-list))   ; drop it. Has alreadby been taken into account.
                    (messages-abs-time-on-off-to-abs-time-with-duration (cdr message-ast-list)))
        (else (cons (first message-ast-list)
                    (messages-abs-time-on-off-to-abs-time-with-duration (cdr message-ast-list))))))

(define (note-on-ast? x)
 (and (ast? x) (equal? "NoteOn" (ast-element-name x)) (not (equal? (ast-attribute x 'velocity) "0"))))

(define (note-off-ast? x)
 (and (ast? x)
      (let ((el-name (ast-element-name x)))
        (or
         (equal? "NoteOff" el-name)
         (and (equal? "NoteOn" el-name) (equal? (ast-attribute x 'velocity) "0"))))))

(define (meta-chord-ast? x)
  (if (and (ast? x) (equal? "Meta" (ast-element-name x)) (= 127 (as-number (ast-attribute x 'type -1))))
      (let* ((meta-contents (ast-text x))
             (binary-meta-contents (hex-to-binary-string-relaxed meta-contents)))
         (and (= (string-length binary-meta-contents) 7)
              (= 67 (get-byte-number binary-meta-contents 0))  ; 43 hex
              (= 123 (get-byte-number binary-meta-contents 1)) ; 7B hex
              (= 1 (get-byte-number binary-meta-contents 2))) )
      #f))

; Given that meta-ast is a meta chord ast (satisfying the predicate meta-chord-ast?) 
; return the cons of root and chord-type, as a cons pair of strings. 
; Example ("C" . "7")
(define (meta-chord-root-and-chordtype meta-chord-ast)
 (let* ((meta-contents (ast-text meta-chord-ast))
        (binary-meta-contents (hex-to-binary-string-relaxed meta-contents))
        (root-number (get-byte-number binary-meta-contents 3))
        (chord-type-number (get-byte-number binary-meta-contents 4)))
   (cons (chord-root-info root-number)                             ; maybe a b chord. We only support # chords.
         (lookup-chord-type-short-name chord-type-number))))


(define (single-message-ast-abs-time-add-duration ast duration)
  (make-ast (ast-element-name ast) 
            (ast-subtrees ast) 
            (append (ast-attributes ast) (list 'duration (as-string duration)))
            (ast-kind ast)
            (ast-language ast)
            (ast-internal-attributes ast)))


; ----------------------------------------------------------------------------------------

; #5
; Convert absTime with duration to absTime with NoteOn and NoteOff attributes.
; In between NoteOn with absTime it is allowed to have NoteOn with deltaTime.
; A NoteOn with deltaTime is relative to either the previous NoteOn with deltaTime, or the previous NoteOn with absTime.
; Re-introduces NoteOff messages.
; Makes sure that the resulting messages are properly sorted.
(define (track-abs-time-with-duration-to-abs-time-on-off midi-track-ast)
  (let ((message-asts (filter ast? (ast-subtrees midi-track-ast))))
    (MidiTrack 
      (stable-sort-list    ; sort-list happens not to be stable. In fact it is systematically unstable. Therefore we use stable-sort-list.
        (messages-abs-time-with-duration-to-abs-time-on-off (get-rid-of-delta-times message-asts 0))
        (lambda (ast1 ast2)
          (<= (as-number (ast-attribute ast1 'absTime)) (as-number (ast-attribute ast2 'absTime))))))))


; Eliminate deltaTime, keep absTime.
; (define (get-rid-of-delta-times message-ast-list previous-abs-time)
;   (cond ((null? message-ast-list) '())
;         ((and (note-on-ast? (first message-ast-list)) (ast-attribute (first message-ast-list)  'absTime))       ; absTime NoteOn
;           (let ((abs-time (as-number (ast-attribute (first message-ast-list) 'absTime))))
;            (cons (first message-ast-list) 
;                  (get-rid-of-delta-times (cdr message-ast-list) abs-time))))
;         ((and (note-on-ast? (first message-ast-list)) (ast-attribute (first message-ast-list) 'deltaTime))      ; deltaTime  NoteOn
;           (let ((delta-time (as-number (ast-attribute (first message-ast-list) 'deltaTime))))
;            (cons (single-message-ast-delta-to-abs-time (first message-ast-list) (+ delta-time previous-abs-time)) 
;                  (get-rid-of-delta-times (cdr message-ast-list) (+ delta-time previous-abs-time)))))
;         (else (let ((abs-time (as-number (ast-attribute (first message-ast-list) 'absTime))))
;                       (cons (first message-ast-list)
;                             (get-rid-of-delta-times (cdr message-ast-list) abs-time)) ))))

; Eliminate deltaTime, keep absTime.
(define (get-rid-of-delta-times message-ast-list previous-abs-time)
  (cond ((null? message-ast-list) '())
        ((ast-attribute (first message-ast-list) 'deltaTime)      ; any event with deltaTime
          (let ((delta-time (as-number (ast-attribute (first message-ast-list) 'deltaTime))))
           (cons (single-message-ast-delta-to-abs-time (first message-ast-list) (+ delta-time previous-abs-time) ) 
                 (get-rid-of-delta-times (cdr message-ast-list) (+ delta-time previous-abs-time)))))
        (else (let ((abs-time (as-number (ast-attribute (first message-ast-list) 'absTime))))
                      (cons (first message-ast-list)
                            (get-rid-of-delta-times (cdr message-ast-list) abs-time)) ))))
                    
  

(define (messages-abs-time-with-duration-to-abs-time-on-off message-ast-list)
  (cond ((null? message-ast-list) '())
        ((note-on-ast? (first message-ast-list)) 
           (let* ((current-ast (first message-ast-list))
                  (current-channel (ast-attribute current-ast 'channel))
                  (current-note (ast-attribute current-ast 'note))
                  (current-abstime (as-number (ast-attribute current-ast 'absTime)))
                  (current-duration (as-number (ast-attribute current-ast 'duration)))
                 )
            (cons 
               (single-message-ast-abs-time-remove-duration current-ast)     ; NoteOn without duration
               (cons
                 (make-midi-ast "NoteOn" 'absTime (as-string (+ current-abstime current-duration)) 'channel current-channel
                         'note current-note 'velocity "0") ; NoteOff = NoteOn with zero velocity
                   (messages-abs-time-with-duration-to-abs-time-on-off (cdr message-ast-list))))))
        (else (cons (first message-ast-list)
                    (messages-abs-time-with-duration-to-abs-time-on-off (cdr message-ast-list))))))
    

(define (single-message-ast-abs-time-remove-duration ast)
  (make-ast (ast-element-name ast) 
            (ast-subtrees ast) 
            (but-props (ast-attributes ast) (list 'duration))
            (ast-kind ast)
            (ast-language ast)
            (ast-internal-attributes ast)))

; ----------------------------------------------------------------------------------------

; #3
; Convert a track, absTime with duration to deltaTime with duration.
; Returns a revised copy of midi-track-ast (a MidiTrack ast).
; The parameter initial-delta-time is typically 0.
; Very similar to message-list-abs-to-delta-time.
(define (track-abs-time-with-duration-to-delta-time-with-duration midi-track-ast initial-delta-time)
  (let ((message-asts (filter ast? (ast-subtrees midi-track-ast))))  ; Inefficient, and not necessary.
    (cond ((null? message-asts)
             (MidiTrack))
          (else 
             (MidiTrack
               (let* ((first-message-ast (first message-asts))
                      (first-abs-time (as-number (ast-attribute first-message-ast 'absTime))))
                 (cons (single-message-ast-abs-to-delta-time first-message-ast initial-delta-time 1)
                       (message-list-abs-with-duration-to-delta-with-duration (cdr message-asts) first-abs-time))))))))
      

; previous-abs-time is the absolute time of the previous message ast.
(define (message-list-abs-with-duration-to-delta-with-duration message-ast-list previous-abs-time)
  (cond ((null? message-ast-list) '())
        (else (let* ((message-ast (car message-ast-list))
                     (this-abs-time (as-number (ast-attribute message-ast 'absTime)))
                     (new-delta-time (- this-abs-time previous-abs-time))
                    )
                (cons (single-message-ast-abs-to-delta-time message-ast new-delta-time 1)
                      (message-list-abs-with-duration-to-delta-with-duration (cdr message-ast-list) this-abs-time))))))

; single-message-ast-abs-to-delta-time defined earlier.

; ----------------------------------------------------------------------------------------

; #4 
; Convert deltaTime with duration to absTime with duration attributes in the MidiTrack ast midi-track-ast. 
; Removes deltaTime attributes and adds absTime attributes.
; Does not add duration.
; Returns a revised copy of midi-track-ast (a MidiTrack ast).
; Inverse function: track-abs-to-delta-time.
; Very similar to track-delta-to-abs-time.
(define (track-delta-with-duration-to-abs-time-with-duration midi-track-ast)
  (let ((message-asts (filter ast? (ast-subtrees midi-track-ast))))
    (MidiTrack 
      (message-list-delta-time-with-duration-to-abs-time-with-duration message-asts 0))))

(define (message-list-delta-time-with-duration-to-abs-time-with-duration message-ast-list start-time)
  (cond ((null? message-ast-list) '())
        (else (let* ((message-ast (car message-ast-list))
                     (delta-time (ast-attribute message-ast 'deltaTime))
                     (new-abs-time (+ start-time (as-number delta-time)))
                    )
                (cons (single-message-ast-delta-to-abs-time message-ast new-abs-time)
                      (message-list-delta-time-with-duration-to-abs-time-with-duration (cdr message-ast-list) new-abs-time))))))

; single-message-ast-delta-to-abs-time define before

; ===============================================================================================================
; Note names and note numbers

; Depends on the global variable counter-transposition
(define (note-number-to-note-name n0)
 (let ((n (+ n0 counter-transposition)))
  (let ((root-number (remainder n 12))
        (octave-number (- (quotient n 12) 2))  ; yamaha convention
       )
   (string-append (name-of-note root-number) (as-string octave-number) 
        (if (not (= counter-transposition 0))   ; display counter transposition
            (if (> (- counter-transposition) 0)
                (string-append "+" (as-string (- counter-transposition)))
                (as-string (- counter-transposition)))
            "")))))

;; Return the note number of note name, either a string or a symbol.
(define (note-name-to-note-number nm)
  (let* ((nm-str (as-string nm))
         (lgt (string-length nm-str))
         (pos (find-in-string-by-predicate (lambda (ch) (memv (as-number ch) (list 48 49 50 51 52 53 54 55 56 57  45))) nm-str))   ; pos of '-' (negation sign) or ciffer.
         (root-name (upcase-string (substring nm-str 0 pos)))
         (octave-number (as-number (substring nm-str pos lgt))))
     (+ (* (+ octave-number 2) 12) (number-of-note-name (upcase-string root-name)))))

(define (name-of-note n)
  (cond ((= n 0) "C")
        ((= n 1) "C#")
        ((= n 2) "D")
        ((= n 3) "D#")
        ((= n 4) "E")
        ((= n 5) "F")
        ((= n 6) "F#")
        ((= n 7) "G")
        ((= n 8) "G#")
        ((= n 9) "A")
        ((= n 10) "A#")
        ((= n 11) "B")
        (else (laml-error "name-of-note: Invalid number:" n))
;        (else (string-append "?" (as-string n) "?"))   ; case for tracing of error
  )
)

(define (number-of-note-name nm)
  (cond ((equal? nm "C") 0)
        ((equal? nm "C#") 1)
        ((equal? nm "D") 2)
        ((equal? nm "D#") 3)
        ((equal? nm "E") 4)
        ((equal? nm "F") 5)
        ((equal? nm "F#") 6)
        ((equal? nm "G") 7)
        ((equal? nm "G#") 8)
        ((equal? nm "A") 9)
        ((equal? nm "A#") 10)
        ((or (equal? nm "B") (equal? nm "H"))  11)
        (else (laml-error "number-of-note-name: Invalid name:" nm))))

;; A function reserved for used in XML-in-LAML midi source files.
;; In reality an alias of note-name-to-note-number, which compensates for counter transposition.
;; It means that you can use (nn "note-name"), where note-name is as shown in the info attribute.
(define (nn nm)
  (- (note-name-to-note-number nm) counter-transposition))
 

; ===============================================================================================================
; Bar, beat and clock

; A number of bars added to the bar number. Use 1 if bar numbers are zero-based, and 1 if bar numbering starts with one.
(define bar-displacement 1)

; Return a list of three integers: bar, beat and clock.
; ppqn is the pulses per quarter note.
; The signature of the song is n:m
; ppgn, n and m are all integer numbers.
(define (bar-beat-clock abs-time-0 ppqn n m)
 (let ((abs-time (to-int abs-time-0)))   ; experimental
  (let* ((quarter-notes-per-bar (quotient (* n 4) m))
         (pp-per-bar (* quarter-notes-per-bar ppqn))
         (pp-per-beat (quotient pp-per-bar m))

         (bar (quotient abs-time pp-per-bar))
         (rest1 (remainder abs-time pp-per-bar))
         (beat (quotient rest1 pp-per-beat))
         (rest2 (remainder rest1 pp-per-beat)))
    (list bar beat rest2))))

(define (bar-beat-clock-string abs-time ppqn n m)
  (let ((bbc (bar-beat-clock abs-time ppqn n m)))
    (string-append (as-string (+ bar-displacement (first bbc))) ":" (as-string (second bbc)) ":" (as-string (third bbc))))) 


; ---------------------------------------------------------------------------------------------------------------
; Tyros Multipad stuff.
; It is assumed that we deal with multipad stuff in absTime mode.

(define (multipad-ast-to-standard-midi-file-ast ast)
  (let* ((mode (ast-attribute ast 'mode))
         (default-image-name "S809")

         (name-1    (ast-attribute ast 'name-1))
         (repeat-1  (as-boolean (ast-attribute ast 'repeat-1 "false")))
         (chord-match-1 (as-boolean (ast-attribute ast 'chord-match-1 "false")))
         (image-1 (ast-attribute ast 'image-1 default-image-name))

         (name-2 (ast-attribute ast 'name-2))
         (repeat-2  (as-boolean (ast-attribute ast 'repeat-2 "false")))
         (chord-match-2 (as-boolean (ast-attribute ast 'chord-match-2 "false")))
         (image-2 (ast-attribute ast 'image-2 default-image-name))

         (name-3 (ast-attribute ast 'name-3))
         (repeat-3  (as-boolean (ast-attribute ast 'repeat-3 "false")))
         (chord-match-3 (as-boolean (ast-attribute ast 'chord-match-3 "false")))
         (image-3 (ast-attribute ast 'image-3 default-image-name))

         (name-4 (ast-attribute ast 'name-4))
         (repeat-4  (as-boolean (ast-attribute ast 'repeat-4 "false")))
         (chord-match-4 (as-boolean (ast-attribute ast 'chord-match-4 "false")))
         (image-4 (ast-attribute ast 'image-4 default-image-name))

         (play-only-track-number (ast-attribute ast 'play-only #f))   ; play-only attribute on root construct
         (counter-transp (ast-attribute ast 'counterTransposition "0"))

         (midi-tracks (find-asts ast "MidiTrack"))
         (play-only-conditions
            (if play-only-track-number
                (play-only-track-number-to-play-only-condition play-only-track-number)
                (map find-play-only-condition midi-tracks)))

         (filtered-tracks 
            (cond ((equal? play-only-conditions (list #f #f #f #f))    ; no play only conditions specified
                     midi-tracks)
                  ((= 1 (count-true play-only-conditions))
                     (list (car (car (filter                        ; Tricky stuff: inner car: select from pair. Outer car: first element only.
                                 (lambda (tr-play)
                                   (let ((tr (car tr-play))
                                         (play (cdr tr-play)))
                                     play))
                                 (pair-up midi-tracks play-only-conditions))))))
                  (else (laml-error "Only a single track is allowed to be marked with 'play-only \"true\""))))

         (timing (if (equal? mode "absTime") (list 'absTime "0") (list 'deltaTime "0")))
        )
    (make-ast "StandardMidiFile"    ; Avoid calling (StandardMidiFile  ...), because it activates action procedure
      (cons
        (MidiHeader 'format "1" 'numberOfTracks "5" 'pulsesPerQuarterNote "1920" 'mode mode 'counterTransposition counter-transp)
        (cons
         (MidiTrack
          (Meta timing  'type "1" (multipad-repeat-string repeat-1 repeat-2 repeat-3 repeat-4))
          (Meta timing  'type "1" (multipad-chord-match-string chord-match-1 chord-match-2 chord-match-3 chord-match-4))
          (Meta timing  'type "1" (multipad-name "N1" name-1))
          (Meta timing  'type "1" (multipad-name "N2" name-2))
          (Meta timing  'type "1" (multipad-name "N3" name-3))
          (Meta timing  'type "1" (multipad-name "N4" name-4))
          (Meta timing  'type "1" (string-append "I1" image-1))
          (Meta timing  'type "1" (string-append "I2" image-2))
          (Meta timing  'type "1" (string-append "I3" image-3))
          (Meta timing  'type "1" (string-append "I4" image-4))
          (Meta timing  'type "47" "") ; end of track
          )
         filtered-tracks))
     '()  ; attributes
     'double
     "midi")))

(define (play-only-track-number-to-play-only-condition n)
  (let ((nn (as-number n)))
    (cond ((= nn 1) (list #t #f #f #f)) 
          ((= nn 2) (list #f #t #f #f))
          ((= nn 3) (list #f #f #t #f))
          ((= nn 4) (list #f #f #f #t))
          (else     (list #f #f #f #f))   ; all tracks
    )))


(define (find-play-only-condition track)
  (as-boolean (ast-attribute track 'play-only "false")))

(define (count-true bool-lst)
  (length (filter (lambda (x) x) bool-lst)))

(define (standard-midi-file-ast-to-multipad-ast ast input-file-path)
  (let* ((header (ast-subtree ast "MidiHeader"))
         (track-1 (ast-subtree ast "MidiTrack" 1))
         (remaining-tracks (cdr (find-asts ast "MidiTrack"))))
    (if (not (equal? "1" (ast-attribute header 'format))) (display-warning "Expecting a format 1 midi file"))
    (if (not (= 4 (length remaining-tracks))) (display-warning "Expecting 5 tracks in midi file"))
    (if (not (equal? "1920" (ast-attribute header 'pulsesPerQuarterNote) )) (display-warning "Expecting 1920 pulses per quarter note"))
;    (if (not (equal? "absTime" (ast-attribute header 'mode) )) (display-warning "Expecting mode absTime"))

    (let* ((meta-ast-list (find-asts track-1 "Meta"))
           (meta-ast-list-1 (filter (lambda (meta-ast) (equal? "1" (ast-attribute meta-ast 'type))) meta-ast-list))
           (meta-text-list (map ast-text meta-ast-list-1))
           (rp (find-meta "RP" meta-text-list))
           (cm (find-meta "CM" meta-text-list))
           (n1 (find-meta "N1" meta-text-list))
           (n2 (find-meta "N2" meta-text-list))
           (n3 (find-meta "N3" meta-text-list))
           (n4 (find-meta "N4" meta-text-list))
           (i1 (find-meta "I1" meta-text-list))
           (i2 (find-meta "I2" meta-text-list))
           (i3 (find-meta "I3" meta-text-list))
           (i4 (find-meta "I4" meta-text-list))
           
           (input-file-name-proper (file-name-proper input-file-path))   ; expected form x.S123.pad
           (image-file-name (file-name-extension input-file-name-proper))
           (default-image-file-name "S910")
          )
      (TyrosMultiPad
       'mode (ast-attribute header 'mode)
       'image (if (empty-string? image-file-name) default-image-file-name image-file-name)
       'name-1 (strip-trailing-null-chars n1)  
       'repeat-1 (if (eqv? (string-ref rp 0) #\0) "false" "true")
       'chord-match-1 (if (eqv? (string-ref cm 0) #\0) "false" "true")
       'image-1 i1

       'name-2 (strip-trailing-null-chars n2)  
       'repeat-2 (if (eqv? (string-ref rp 1) #\0) "false" "true")
       'chord-match-2 (if (eqv? (string-ref cm 1) #\0) "false" "true")
       'image-2 i2

       'name-3 (strip-trailing-null-chars n3)
       'repeat-3 (if (eqv? (string-ref rp 2) #\0) "false" "true")
       'chord-match-3 (if (eqv? (string-ref cm 2) #\0) "false" "true")
       'image-3 i3

       'name-4 (strip-trailing-null-chars n4)
       'repeat-4 (if (eqv? (string-ref rp 3) #\0) "false" "true")
       'chord-match-4 (if (eqv? (string-ref cm 3) #\0) "false" "true")
       'image-4 i4

       remaining-tracks))))


(define (find-meta prefix str-lst)
  (let ((res (find-in-list 
                (lambda (str) (equal? prefix (substring str 0 (string-length prefix))))
                str-lst)))
    (if res
        (substring res (string-length prefix) (string-length res))
        (laml-error "find-meta: Cannot find prefix" prefix "in" str-lst))))

(define (strip-trailing-null-chars str)
  (substring str 0 (find-in-string str #\nul)))


(define (multipad-repeat-string r1 r2 r3 r4)
  (string-append "RP"
    (if r1 "1" "0")    (if r2 "1" "0")     (if r3 "1" "0")     (if r4 "1" "0")))

(define (multipad-chord-match-string cm1 cm2 cm3 cm4)
  (string-append "CM"
    (if cm1 "1" "0")    (if cm2 "1" "0")     (if cm3 "1" "0")     (if cm4 "1" "0")))

 
(define (multipad-name prefix name)
 (let ((padding-length (- 52 (+ (string-length prefix) (string-length name)))))
  (string-append prefix name (make-string padding-length #\nul))))

; ---------------------------------------------------------------------------------------------------------------
; Chords

; A list of all chord formula - taken from the Yahama Tyros data list (page 23).
(define chord-types
  '(
    ("M"        "major"                                0 4 7)                ; 0
    ("6"        "sixth"                                0 4 7 9)              ; 1
    ("M7"       "major seventh"                        0 4 7 11)             ; 2
    ("M7(#11)"  "major seventh add sharp eleventh"     0 2 4 6 7 11)         ; 3
    ("9"        "add ninth"                            0 2 4 7)              ; 4
    ("M7_9"     "major seventh ninth"                  0 2 4 7 11)           ; 5
    ("6_9"      "sixht ninth"                          0 2 4 7 9)            ; 6
    ("aug"      "augmented"                            0 4 8)                ; 7
  
    ("m"        "minor"                                0 3 7)                ; 8
    ("m6"       "minor sixth"                          0 3 7 9)              ; 9
    ("m7"       "minor seventh"                        0 3 7 10)             ; 10
    ("m7b5"     "minor seventh flat fifth"             0 3 6 10)             ; 11
    ("m(9)"     "minor add ninth"                      0 2 3 7)              ; 12
    ("m7(9)"    "minor seventh ninth"                  0 2 3 7 10)           ; 13
    ("m7(11)"   "minor seventh eleventh"               0 2 3 5 7 10)         ; 14
    ("mM7"      "minor major seventh"                  0 3 7 11)             ; 15
    ("mM7(9)"   "minor major seventh ninth"            0 2 3 7 11)           ; 16
  
    ("dim"       "diminished"                          0 3 6)                ; 17
    ("dim7"      "diminished seventh"                  0 3 6 9)              ; 18
    ("7"         "seventh"                             0 4 7 10)             ; 19
    ("7sus4"     "seventh suspended fourth"            0 5 7 10)             ; 20
    ("7b5"       "sevent flatted fifth"                0 4 6 10)             ; 21
    ("7(9)"      "seventh ninth"                       0 2 4 7 10)           ; 22
    ("7(#11)"    "seventh add sharp eleventh"          0 2 4 6 7 10)         ; 23
    ("7(13)"     "seventh add thirteenth"              0 4 7 9 10)           ; 24
    ("7(b9)"     "sevent flatted ninth"                0 1 4 7 10)           ; 25
    ("7(b13)"    "seventh add flatted thirteenth"      0 4 7 8 10)           ; 26
    ("7(#9)"     "seventh sharp ninth"                 0 3 4 7 10)           ; 27
    ("M7aug"    "major seventh augmented"              0 4 8 11)             ; 28
    ("7aug"     "seventh augmented"                    0 4 8 10)             ; 29
    ("1+8"      "1+8"                                  0 1 3)                ; 30
    ("1+5"      "1+5"                                  0 7)                  ; 31
    ("sus4"      "suspended fourth"                    0 5 7)                ; 32
    ("1+2+5"     "???"                                 0 2 5)                ; 33   ; wrong!
    ("cancel"    "cancel"                              0 1 2)                ; 34

    ("sus2"      "suspended plus two plus five"        0 2 7)                ; 35   ; non-yamaha
    ("M7b5"     "major seventh flat fifth"             0 4 6 11)             ; 36   ; non-yamaha
    ("b5"       "flatted fifth"                        0 4 6)                ; 37   ; non-yamaha
    ("mM7b5"    "minor major seventh flatted fifth"    0 3 6 11)             ; 38   ; non-yamaha
    
  ))

; Return the chord given type. 
; This function accept both short chord name (a string) and a number
; The numering correspond the Yamaha/Tyros ordering.
(define (lookup-chord-type-formula type)
  (cond ((string? type)
          (let ((res (assoc type chord-types)))
            (if res
                (cddr res)
                (laml-error "lookup-chord-type-formula: Unknown chord type:" type))))

        ((integer? type)
          (cddr (list-ref chord-types type)))

        (else (laml-error "lookup-chord-type-formula: Input must be string (short chord name) or an index number (between 0 and 38)"))))

(define (lookup-chord-type-short-name chord-number)
  (if (and (>= chord-number 0) (<= chord-number 38))
      (let ((res (list-ref chord-types chord-number)))
        (car res))
      "??"))
  
      
  


; Return a chord list for a given chord.
; Root is a name (string): C, C#, D, D#, E, F, F#, G, G#, A, A#, B.
; Chord-type is a chord type name, as appearing in the list chord-types.
; start-octave is an octave number. Octave number 3 contains the middle C (note 60) - according the Yamaha convention. Within the interval [-2..8]
; Generate it through number-of-octaves (a non-negative integer), allways ended by the root note.
; .form (chord-note-list root chord-type start-octave number-of-octaves)
(define (chord-note-list root chord-type start-octave number-of-octaves)
  (let* ((relative-chord-formula (lookup-chord-type-formula (as-string chord-type)))
         (root-number (number-of-note-name (as-string root)))
         (relative-root-chord-formula (map (lambda (n) (+ n root-number)) relative-chord-formula))
         (relative-repeated-root-chord-formula (repeat-chord-formula relative-root-chord-formula number-of-octaves))
         (start-octave-number (start-of-octave-number start-octave))
         (absolute-repeated-root-chord-formula (map (lambda (n) (+ n start-octave-number)) relative-repeated-root-chord-formula))
        )
    absolute-repeated-root-chord-formula 
  ))

; Return a chord list (a list of integers) of length lgt, ending in note, played according to chord-type with a given root.
; Notice that  note  always will be the last element of the list (if a list is returned, of course).
; If it is not possible to form a chor note list of the given chord-type and root, return #f.
(define (chord-note-list-ending-at note lgt root chord-type)
  (let* ((total-long-chord-list (chord-note-list root chord-type -2 10))   ; all notes in given chord-type with given root
         (idx-of-note (index-in-list-by-predicate total-long-chord-list note =)))
    (if idx-of-note
        (let* ((to-idx (+ idx-of-note 1))
               (from-idx (- to-idx (- lgt 1)))) 
          (list-part from-idx to-idx total-long-chord-list))
        #f)))

(define (repeat-chord-formula formula n)
  (if (= n 0) 
      (list (car formula))   ; ending at root
      (append formula 
              (repeat-chord-formula 
               (map (lambda (n) (+ n 12)) formula) (- n 1)))))

(define (start-of-octave-number n)
  (cond 
        ((< n -2) (laml-error "start-of-octave-number: Octave numbers must be within [-2..8]" n))
        ((= n -2) 0)
        ((= n -1) 12)
        ((= n 0) 24)
        ((= n 1) 36)
        ((= n 2) 48)
        ((= n 3) 60)
        ((= n 4) 72)
        ((= n 5) 84)
        ((= n 6) 96)
        ((= n 7) 108)
        ((= n 8) 120)
        ((> n 8) (laml-error "start-of-octave-number: Octave numbers must be within [-2..8]" n))))

    
; ---------------------------------------------------------------------------------------------------------------
; Track separation.
; Transformation of MIDI format 0 AST to MIDI format 1 AST.

; Defines how to separate tracks when making a format 1 AST.
(define standard-track-separation-scheme 
  '((system meta) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))

; Create a format 1 ast from a format 0 StandardMidiFile ast.
; The parameter mode describes the mode of the source (format 0) AST: either deltaTime or absTime (a symbol).
(define (track-separated-standard-midi-file-ast format-0-ast track-separation-scheme mode)
  (make-ast "StandardMidiFile"    ; use make-ast to avoid calling the mirror function StandardMidiFile. We do not wish to perfom the action procedure.
    (cons 
      (MidiHeader 'format "1" 
                  'numberOfTracks (as-string (length track-separation-scheme)) 
                  'pulsesPerQuarterNote (unique-ast-attribute format-0-ast 'pulsesPerQuarterNote)
                  'mode (unique-ast-attribute format-0-ast 'mode)
                  'counterTransposition (unique-ast-attribute format-0-ast 'counterTransposition "0"))
      (make-multiple-level-1-tracks-from-single-track
                 (ast-subtree format-0-ast "MidiTrack")  ; only one such "MidiTrack" AST in format 0 StandardMidiFile AST
                 track-separation-scheme
                 mode
      )
    )
    '()  ; attributes
    'double
    "midi"))

; mode is the mode of the midi-track-ast
(define (make-multiple-level-1-tracks-from-single-track midi-track-ast track-separation-scheme mode)
  (cond 
        ; Convert track to absTime with duration, do track separation, and convert back to deltaTime with duration.
        ; Slightly inefficient. 
        ((eq? mode 'deltaTime)
           (let ((midi-track-ast-abstime (track-delta-with-duration-to-abs-time-with-duration midi-track-ast)))
             (map 
              (lambda (track-separation-item)
                (let* ((event-ast-list (ast-subtrees midi-track-ast-abstime))
                       (event-ast-list-track (filter (track-predicate track-separation-item) event-ast-list))
                       (first-event-ast-in-track (first-ast-in-list event-ast-list-track))
                       (start-time (if first-event-ast-in-track (as-number (ast-attribute first-event-ast-in-track 'absTime "0")) 0))
                      )
                  (track-abs-time-with-duration-to-delta-time-with-duration 
                    (MidiTrack event-ast-list-track)
                    start-time)))
              track-separation-scheme))
        )

        ((eq? mode 'absTime)
           (map 
            (lambda (track-separation-item)
              (let ((event-ast-list (ast-subtrees midi-track-ast)))
                (MidiTrack (filter (track-predicate track-separation-item) event-ast-list))))
            track-separation-scheme))

        (else (laml-error "make-multiple-level-1-tracks-from-single-track: Can only handle absTime or deltaTime mode. Received: " mode))
   )
)

(define system-meta-element-names
  (list "SysEx" "MTCQuarterFrame" "SongPositionPointer" "SongSelect" "TuneRequest" "TimingClock" "Start" "Continue" "Stop" "ActiveSensing"))

; Return an AST predicate that recognizes an item from the track-separation-scheme  (an element in that list).
; Legal track-items:   (system meta), (meta system), meta, system, an integer, a non-empty integer list. 
(define (track-predicate track-item) 
  (cond ((and (pair? track-item) (or (equal? track-item '(system meta))  (equal? track-item '(meta system))))
           (lambda (x)
              (and (ast? x) (member (ast-element-name x) (cons "Meta" system-meta-element-names)))))
        ((int-list? track-item)
           (lambda (x)
              (and (ast? x) (member (as-number (ast-attribute x 'channel)) track-item))))
        ((and (symbol? track-item) (equal? track-item 'system))
           (lambda (x)
              (and (ast? x) (member (ast-element-name x) system-meta-element-names))))
        ((and (symbol? track-item) (equal? track-item 'meta))
           (lambda (x)
              (and (ast? x) (equal? (ast-element-name x) "Meta"))))
        ((integer? track-item)
           (lambda (x)
              (and (ast? x) (= track-item (as-number (ast-attribute x 'channel))))))
        (else
           (lambda (x)
               #f))))

(define (int-list? x)
  (and (pair? x) (list? x) (>= (length x) 1)
       (accumulate-right and-fn #t (map integer? x))))
       
; ---------------------------------------------------------------------------------------------------------------   
; Track joining
; Transformation of MIDI format 1 AST to MIDI format 0 AST.

; (define (track-joined-standard-midi-file-ast format-1-ast)
;   (make-ast "StandardMidiFile"    ; use make-ast to avoid calling the mirror function StandardMidiFile. We do not wish to perfom the action procedure.
;     (cons 
;       (MidiHeader 'format "0" 
;                   'numberOfTracks "1" 
;                   'pulsesPerQuarterNote (unique-ast-attribute format-1-ast 'pulsesPerQuarterNote)
;                   'mode (unique-ast-attribute format-1-ast 'mode)
;                   'counterTransposition (unique-ast-attribute format-1-ast 'counterTransposition "0"))
;       (cons #t (list 
;                 (make-single-track-from-multiple-tracks 
;                   (filter 
;                     (lambda (x) (and (ast? x) (equal? "MidiTrack" (ast-element-name x))))
;                     (ast-subtrees format-1-ast))
;                   mode
;                 ))
;       )
;     )
;     '()  ; attributes
;     'double
;     "midi"))

(define (track-joined-standard-midi-file-ast format-1-ast)
  (let ((mode (unique-ast-attribute format-1-ast 'mode)))
      (StandardMidiFile 'internal:run-action-procedure "false"
      (MidiHeader 'format "0" 
                  'numberOfTracks "1" 
                  'pulsesPerQuarterNote (unique-ast-attribute format-1-ast 'pulsesPerQuarterNote)
                  'mode mode
                  'counterTransposition (unique-ast-attribute format-1-ast 'counterTransposition "0"))
      (make-single-track-from-multiple-tracks 
                    (filter 
                      (lambda (x) (and (ast? x) (equal? "MidiTrack" (ast-element-name x))))
                      (ast-subtrees format-1-ast))
                    (as-symbol mode)
                  )
      ; We assume that there is no non-midi suffix of a format 1 AST.
     ))
)

; mode is the mode of the tracks in midi-track-ast-list. Either absTime og deltaTime (a symbol).
(define (make-single-track-from-multiple-tracks midi-track-ast-list mode)
 (cond ((eq? mode 'absTime)   ; flattening and sorting is sufficient in absTime mode. This is easy.
           (MidiTrack
            (stable-sort-list 
             (flatten
              (map 
               (lambda (midi-track-ast)
                 (get-rid-of-delta-times (filter ast? (ast-subtrees midi-track-ast)) 0))
               midi-track-ast-list))
             abs-time-ast-leq?)))

        ((eq? mode 'deltaTime)   ; slightly more complicated. Cannot just 'concatenate' as above. 
                                 ; convert temporarily each track to absTime, flatten & sort, and convert back to deltaTime.
                                 ; This is inefficient - but probably not used very much - therefore OK.
          (track-abs-time-with-duration-to-delta-time-with-duration
            (MidiTrack
             (stable-sort-list 
              (flatten
               (map 
                (lambda (midi-track-ast)
                  (let ((midi-track-ast-abs-time (track-delta-with-duration-to-abs-time-with-duration midi-track-ast)))
                    (filter ast? (ast-subtrees midi-track-ast)))
                  )
                midi-track-ast-list))
              abs-time-ast-leq?))
            0)
        )

        (else (laml-error "make-single-track-from-multiple-tracks: Unknown mode: " mode))
 )
)

; Earlier simple version:
(define (abs-time-ast-leq? ast1 ast2)
  (<= (as-number (ast-attribute ast1 'absTime)) (as-number (ast-attribute ast2 'absTime))))

; A list sorting function which is stable.
(define (stable-sort-list lst compare-fn)
  (sort-list (reverse lst) compare-fn))

; ---------------------------------------------------------------------------------------------------------------

(define (meta-division-event? x)
  (and x
       (ast? x)
       (equal? (ast-element-name x) "Meta")
       (equal? "6" (ast-attribute x 'type))))

(define (no-spaces-in-string str)
  (transliterate str #\space "-"))

; Return a consequtive subset of the events in track-events.
; from and to are names (strings) of type 6 meta events (markers).
; from and to can also be booleans. 
; If from is #t, it means from beginning.
; If to is #t, it means to end.
(define (midi-event-ast-subsequence track-events from to)
  (sublist-of-list 
    track-events
    (if (and (boolean? from) from) 
        (lambda (x) #t)
        (lambda (x)
          (and
           (ast? x)
           (equal? (ast-element-name x) "Meta")
           (equal? "6" (ast-attribute x 'type))
           (equal? from (ast-text x)))))
    (if (and (boolean? to) to) 
        (lambda (x) #f)
        (lambda (x)
          (and
           (ast? x)
           (equal? (ast-element-name x) "Meta")
           (equal? "6" (ast-attribute x 'type))
           (equal? to (ast-text x)))))))                   

; ---------------------------------------------------------------------------------------------------------------
; Input functions.

; Return a hex tempo string, suitable for the contents of a type 81 tempo meta message.
; The bpm is meassured in beat per quarter note, and it is not required to be a an integer.
(define (tempo= bpm)
  (binary-to-hex-string (int10-to-binary (to-int (/ 60000000 bpm)) 3)))

; Return a hex key signature string, suitable for the contents of a type 89 key signature meta message
; .parameter flat-neutral-sharp One of the symbols flat, neutral, or sharp (a symbol).
; .parameter number The number of flats or sharps (a non-negative integer)
; .parameter major-minor One of the symbols major or minor (a symbol)
(define (key-signature= flat-neutral-sharp number major-minor)
  (string-append
   (binary-to-hex-string
     (int10-to-binary
       (cond ((and (eq? flat-neutral-sharp 'neutral) (= number 0)) 0)
             ((and (eq? flat-neutral-sharp 'flat) (>= number 1) (<= number 7)) (- 256 number))    ; the two's complement negative number!
             ((and (eq? flat-neutral-sharp 'sharp) (>= number 1) (<= number 7)) number)
             (else (laml-error "key-signature= first par: expected neutral/flat/sharp (symbols). Second par: expected an integer between 0 and 7"))
       )
       1))
   " "
   (binary-to-hex-string
     (int10-to-binary
       (cond ((eq? major-minor 'major) 0)
             ((eq? major-minor 'minor) 1)
             (else (laml-error "key-signature= third parameter: expected either the symbol major or minor"))
       )
     1))))
   

; ----------------------------------------------------------------------------------------------------
; New transformation stuff ##

(define (generalized-transform-track-to-raw-mode midi-track-ast mode-symbol ppqn-factor)
  (let* ((first-event-ast-in-track (first-ast-in-list (ast-subtrees midi-track-ast)))
         (start-time (if first-event-ast-in-track (as-number (ast-attribute first-event-ast-in-track 'absTime "0")) 0))
        )
  (cond ((eq? mode-symbol 'raw)
            midi-track-ast)    ; the identity transformation

        ((eq? mode-symbol 'absTime)  ; absTime with duration. If necessary, intervening deltaTime messages are first converted to absTime messages. Followed by a stable sort.
            (let* ((fixed-part-file-name (ast-attribute midi-track-ast 'fixed-part #f))
                   (fixed-part-track-messages (if fixed-part-file-name
                                                  (read-and-pre-process-messages (string-append (startup-directory) (source-filename-without-extension) "/" "fixed-parts" "/" fixed-part-file-name) fixed-part-file-name)
                                                  '()))
                   (message-ast-list (append fixed-part-track-messages (ast-subtrees midi-track-ast)))  ; simple append sufficient - sorting below fixes the ordering.
                  )
              (if (has-intervening-delta-time-messages? message-ast-list)  ; of efficiency reasons - to avoid unnecessary sorting - ALWAYS TRUE!
                  (let* ((pure-abs-time-message-ast-list (eliminate-intervening-delta-time-messages message-ast-list))
                         (sorted-abs-time-message-ast-list (stable-sort-list pure-abs-time-message-ast-list abs-time-ast-leq?)))
                    (generalizied-track-transform-to-raw-delta-time sorted-abs-time-message-ast-list 0 ppqn-factor))
                  (generalizied-track-transform-to-raw-delta-time midi-track-ast 0 ppqn-factor))))                            ; else branch never selelected.

        ((eq? mode-symbol 'deltaTime)  ; deltaTime with duration. No absTime midi messages are allowed.
            (generalizied-track-transform-to-raw-delta-time (ast-subtrees midi-track-ast) start-time ppqn-factor ))

        ((eq? mode-symbol 'absTime-on-off)  ; old-style transformation 
            (track-abs-to-delta-time
              midi-track-ast start-time ppqn-factor ))

        (else (laml-error "generalized-transform-track-to-raw-mode: Unknown mode" mode-symbol)))))

(define (has-intervening-delta-time-messages? message-ast-list)
  #t)

(define (read-and-pre-process-messages full-path part-name)
  (if (file-exists? full-path)
      (map uncompact-midi-laml-entry (file-read full-path))
      (laml-error "MidiTrack fixed-part: Cannot find the fixed part" part-name "in the project directory")))

(define (compact-midi-laml-ast ast)
  (cond ((NoteOn? ast) (list 'no (midi 'absTime ast) (midi 'channel ast) (midi 'note ast) (midi 'velocity ast) (midi 'duration ast)))
        ((ControlChange? ast) (list 'cc (midi 'absTime ast) (midi 'channel ast) (midi 'control ast) (midi 'value ast)))
        ((PitchBendChange? ast) (list 'pb (midi 'absTime ast) (midi 'channel ast) (midi 'value ast)))
        (else ast)))

(define (uncompact-midi-laml-entry x)
 (cond ((ast? x) x)
       ((and (pair? x) (eq? (first x) 'no)) (NoteOn 'absTime (second x) 'channel (third x) 'note (fourth x) 'velocity (fifth x) 'duration (sixth x)))
       ((and (pair? x) (eq? (first x) 'cc)) (ControlChange 'absTime (second x) 'channel (third x) 'control (fourth x) 'value (fifth x)))
       ((and (pair? x) (eq? (first x) 'pb)) (PitchBendChange 'absTime (second x) 'channel (third x) 'value (fourth x)))
       (else (laml-error "uncompact-midi-laml-entry: Should not happen"))))



; Generation of the predicate used by the Elisp channel zoom function process-laml-file-save-unzoom-and-zoom-on-channel.
(define (make-zoom-on-channel-predicate ch)
  (lambda (m)
    (or (and (NoteOn? m) (= ch (midi 'channel m)))
        (Meta? m)
        (ControlChange? m 0 ch) (ControlChange? m 32 ch)  ; Bank select
        (ProgramChange? m ch))))

; Generation of a slightly broader predicate than one generated by make-zoom-on-channel-predicate. Used in the Elisp zoom function process-laml-file-all-mes-save-unzoom-and-zoom-on-channel.
(define (make-zoom-on-channel-predicate-all ch)
  (lambda (m)
    (or (and (channel-message? m) (= ch (midi 'channel m)))
        (Meta? m))))


; Convert deltaTime messages to absTime messages. 
; Return a list of absTime messages. Only ASTs remain in the resulting list.
; Assume that the very first message is an absTime message.
(define (eliminate-intervening-delta-time-messages message-list)
  (get-rid-of-delta-times (filter ast? message-list) 0)   ; a rude implementation - but it works
)


; --------------------------------------------------------------------------------------------------------------------

; A priority queue ADT of delta events which avait insertion into the resulting raw deltaTime midi event message list.

(define priority-queue '())
(define leq-function #f)
(define eq-function #f)
(define value-function #f)
(define set-value-function #f)

(define (init-priority-queue capacity leq-fn eq-fn val-fn set-val-fn)
  (set! leq-function leq-fn)
  (set! eq-function eq-fn)
  (set! value-function val-fn)
  (set! set-value-function set-val-fn)  
  (set! priority-queue '())
)

; Insert element into the priority queue
(define (insert-in-priority-queue element)
  (set! priority-queue (cons element priority-queue))
)

; Remove the element from the priority queue with the smallest
; value, and return it. If the queue is empty, return #f.
; Reduce the values of all remaining elements correspondingly. 
(define (remove-from-priority-queue)
  (if (null? priority-queue)
      #f
      (let* ((smallest-element (find-minimum-element-in-priority-queue))
             (smallest-value (value-function smallest-element)))
        (remove-element-from-priority-queue smallest-element)
        (reduce-values-in-priority-queue smallest-value)
        smallest-element)))

; Return the element with the minimum value. 
; Do not mutate the queue.
(define (find-minimum-element-in-priority-queue)
  (if (null? priority-queue)
      #f
      (find-minimum-element-1 (cdr priority-queue) (car priority-queue))))

(define (find-minimum-element-1 lst min-el)
 (cond ((null? lst) min-el)
       ((leq-function (car lst) min-el)
          (find-minimum-element-1 (cdr lst) (car lst)))
       (else (find-minimum-element-1 (cdr lst) min-el))))

(define (remove-element-from-priority-queue element)
  (set! priority-queue
        (filter (lambda (el) (not (eq? element el)))
                priority-queue)))

; Reduce all values in the priority queue by delta.
; Thus, subtract delta from all values in the elements of the queue.
(define (reduce-values-in-priority-queue delta)
  (map (lambda (el) 
         (let ((val (value-function el)))
           (set-value-function el (- val delta)))
       )
       priority-queue))

; --------------------------------------------------------------------------------------------------------------------

; The AST midi messages can either be absTime or deltaTime messages.
; absTime messages (including possible deltaTime messages in between) are supposed to be sorted.
; Return a list of raw on/off delta time messages without durations.
; ppqn-factor is not used in this version!
; message-ast-list-0 is the midi messages of the midi-track-ast
(define (generalizied-track-transform-to-raw-delta-time message-ast-list-1 start-time ppqn-factor)
 (let ((message-ast-list-0 (map ast-copy message-ast-list-1)))  ; May 15, 2008: copies the ast-list, because of mutations done in this procedure
  (init-priority-queue 1000 delta-mes-leq delta-mes-eq mes-value-function set-mes-value-function)
  (let* ((midi-deltatime-ast-lst '())
         (latest-reduction #f)  ; either message-list or priority-queue (a symbol)
         (cur-abs-time start-time)
        )

    (do ((message-ast-list message-ast-list-0 (if (eq? latest-reduction 'message-list) (cdr message-ast-list) message-ast-list))
        )
      ((and (null? message-ast-list) (null? priority-queue))   ; Done - both lists exhausted
           (MidiTrack
             (reverse midi-deltatime-ast-lst))
      )
      (let* ((mes-ast (if (not (null? message-ast-list)) (first message-ast-list) #f))  ; first message in message-ast-list
            )
            (let* ((delta-time? (if mes-ast (ast-attribute mes-ast 'deltaTime #f) #f))  ; is mes-ast with deltaTime?
                   (abs-time? (if mes-ast (ast-attribute mes-ast 'absTime #f) #f))      ; is mes-ast with   absTime?
                   (delta-time-of-mes-ast (if mes-ast (find-effective-delta-time-of mes-ast cur-abs-time) #f))
                   (min-event-of-queue-0 (find-minimum-element-in-priority-queue))
                   (min-event-of-queue   (if min-event-of-queue-0 (delta-time-unwrap min-event-of-queue-0) #f))
                   (min-delta-of-queue (if min-event-of-queue 
                                           (car min-event-of-queue-0)
                                           #f))
                   (note-on? (if mes-ast (note-on-ast? mes-ast) #f))
                  )
               (cond 
                     ((and (not mes-ast) min-event-of-queue)   ; message-ast-list empty, priority queue not empty
                        (let ((event-from-queue (delta-time-unwrap (remove-from-priority-queue))))
                           (set! midi-deltatime-ast-lst (cons min-event-of-queue midi-deltatime-ast-lst))
                           (set! cur-abs-time (+ cur-abs-time min-delta-of-queue ))
                           (set! latest-reduction 'priority-queue)
                        )
                     )

                     ((and mes-ast min-event-of-queue                         ; priority queue is not empty. message-ast-list is not empty
                           (<= min-delta-of-queue delta-time-of-mes-ast))      ; the smallest event is found in the priority queue   ; 28.4.2008: CHANGED < TO <=. IMPORTANT
                        (let ((event-from-queue (delta-time-unwrap (remove-from-priority-queue))))
                           (if delta-time? (reduce-delta-ast-attribute mes-ast min-delta-of-queue))
                           (set! midi-deltatime-ast-lst (cons min-event-of-queue midi-deltatime-ast-lst))
                           (set! cur-abs-time (+ cur-abs-time min-delta-of-queue ))
                           (set! latest-reduction 'priority-queue)
                        )
                     )

                     ((and (not (null? message-ast-list)) delta-time? note-on?)       ; deltaTime.  The first is a NoteOn message
                          (set! midi-deltatime-ast-lst (cons mes-ast midi-deltatime-ast-lst))
                          (reduce-values-in-priority-queue delta-time-of-mes-ast)
                          (insert-in-priority-queue (delta-time-wrap (similar-noteoff-ast mes-ast)))
                          (set! cur-abs-time (+ cur-abs-time delta-time-of-mes-ast))
                          (set! latest-reduction 'message-list)
                     )

                     ((and (not (null? message-ast-list)) abs-time? note-on?)         ; absTime.  The first is a NoteOn message
                          (set! midi-deltatime-ast-lst (cons (as-delta-time-message mes-ast delta-time-of-mes-ast) midi-deltatime-ast-lst))
                          (reduce-values-in-priority-queue delta-time-of-mes-ast)
                          (insert-in-priority-queue 
                              (delta-time-wrap (similar-noteoff-ast (as-delta-time-message mes-ast delta-time-of-mes-ast))))
                          (set! cur-abs-time (+ cur-abs-time delta-time-of-mes-ast))
                          (set! latest-reduction 'message-list)
                     )

                     ((and (not (null? message-ast-list)) delta-time?)                 ; deltaTime.  The first is a NOT NoteOn message
                          (set! midi-deltatime-ast-lst (cons mes-ast midi-deltatime-ast-lst))
                          (reduce-values-in-priority-queue delta-time-of-mes-ast)
                          (set! cur-abs-time (+ cur-abs-time delta-time-of-mes-ast))                       
                          (set! latest-reduction 'message-list)
                     )

                     ((and (not (null? message-ast-list)) abs-time?)                   ; absTime.  The first is a NOT NoteOn message
                          (set! midi-deltatime-ast-lst (cons (as-delta-time-message mes-ast delta-time-of-mes-ast)  midi-deltatime-ast-lst))
                          (reduce-values-in-priority-queue delta-time-of-mes-ast)   
                          (set! cur-abs-time (+ cur-abs-time delta-time-of-mes-ast))                       
                          (set! latest-reduction 'message-list)
                     )

                     (else (laml-error "generalizied-track-transform-to-raw-delta-time: Should not happen."))
               )
            )
       )
    )
 )
))




; Returns a number
(define (find-effective-delta-time-of ast cur-abs-time)
  (let ((delta-time-of-mes-ast (ast-attribute ast 'deltaTime #f))
        (abs-time-of-mes-ast   (ast-attribute ast 'absTime   #f))
       )
    (if (and delta-time-of-mes-ast abs-time-of-mes-ast)
        (laml-error "find-effective-delta-time-of: An ast should not have both a deltaTime attribute and an absTime attribute" ast))
    (cond (delta-time-of-mes-ast (as-number delta-time-of-mes-ast))
          (abs-time-of-mes-ast   (- (as-number abs-time-of-mes-ast) cur-abs-time))
          (else (laml-error "find-effective-delta-time-of: An ast should either have a deltaTime attribute or an absTime attribute:" ast)))))

; Wrap the deltaTime of mes-ast around mes-ast in a cons cell. For efficiency purposes.
(define (delta-time-wrap mes-ast)
  (let ((delta-time (as-number (ast-attribute mes-ast 'deltaTime))))
    (cons delta-time mes-ast)))

; Put (car consed-mes-ast) into the deltaTime of the wrapped message ast, and return the mutated message ast.
(define (delta-time-unwrap wrapped-mes-ast)
 (let ((delta-time (car wrapped-mes-ast))
       (mes-ast (cdr wrapped-mes-ast)))
   (set-ast-attribute! mes-ast 'deltaTime delta-time)
   mes-ast))


(define (reduce-delta-ast-attribute mes-ast delta)
  (set-ast-attribute! mes-ast 'deltaTime
     (- (as-number (ast-attribute mes-ast 'deltaTime)) delta)))


; ---------------------------------------------------------------------------------------------
; Actual function parameter to priority queue:  OLD VERSION

; (define (delta-mes-leq m1 m2)
;   (<= (as-number (ast-attribute m1 'deltaTime)) (as-number (ast-attribute m2 'deltaTime))))
; 
; (define (delta-mes-eq m1 m2)
;   (= (as-number (ast-attribute m1 'deltaTime)) (as-number (ast-attribute m2 'deltaTime))))
; 
; (define (mes-value-function mes)
;   (as-number (ast-attribute mes 'deltaTime)))
; 
; (define (set-mes-value-function mes new-val)
;   (set-ast-attribute! mes 'deltaTime new-val))

; ---------------------------------------------------------------------------------------------
; Actual function parameter to priority queue - Wrapped versions.

(define (delta-mes-leq m1 m2)
  (<= (as-number (car m1)) (as-number (car m2))))

(define (delta-mes-eq m1 m2)
  (= (as-number (car m1)) (as-number (car m2))))

(define (mes-value-function mes)
  (as-number (car mes)))

(define (set-mes-value-function mes new-val)
  (set-car! mes new-val))
; ---------------------------------------------------------------------------------------------

; Convert an absTime mes-ast to a deltaTime mes-ast with deltaTime new-delta-time
(define (as-delta-time-message mes-ast new-delta-time)
  (make-ast (ast-element-name mes-ast) 
            (ast-subtrees mes-ast) 
            (extend-prop-list 'deltaTime (as-string new-delta-time) (remove-props! '(deltaTime absTime) (ast-attributes mes-ast)))
            (ast-kind mes-ast)
            (ast-language mes-ast)
            (ast-internal-attributes mes-ast)))

; Makes mutated copy with zero velocity (= NoteOff).
; mes-ast is with deltaTime. 
(define (similar-noteoff-ast mes-ast)
  (let ((delta-time (as-number (ast-attribute mes-ast 'deltaTime)))
        (duration   (as-number (ast-attribute mes-ast 'duration))))
    (copy-ast-mutate-attributes mes-ast 'deltaTime duration  'velocity "0")))   ; Serious error corrected May 16, 2008: 
                                                                                ; duration was before (+ delta-time duration).

; ---------------------------------------------------------------------------------------------------
; For testing purposes:

(define (compare-tracks tr1 tr2)
  (compare-ast-lists (ast-subtrees tr1) (ast-subtrees tr2)))

(define (compare-ast-lists ast-list-1 ast-list-2)
  (if (= (length ast-list-1) (length ast-list-2))
      (compare-ast-lists-1 ast-list-1 ast-list-2 1)
      (display-message "Lists are of UNEQUAL LENGTHS:" (length ast-list-1) (length ast-list-2))))

(define (compare-ast-lists-1 ast-list-1 ast-list-2 i)
  (cond ((null? ast-list-1) (display-message "EQUAL"))
        ((equal? (car ast-list-1) (car ast-list-2))
            (compare-ast-lists-1 (cdr ast-list-1) (cdr ast-list-2) (+ i 1)))
        (else (display-message (string-append "Difference at element number " (as-string i) (as-string (car ast-list-1)) (as-string (car ast-list-2)))))))


; ---------------------------------------------------------------------------------------------------------------
; Midi AST selection

; Return the time signature of a meta ast, of type 88.
; The information is taken from the contents of of the meta AST.
; It is assumed, as a preconditions, that ast is of the appropriate type.
; Returns a list of two integers, such as '(3 4) or (4 4).
(define (time-signature-of-meta-type-88-ast ast)
  (time-signature-info-list (hex-to-binary-string-relaxed (ast-text ast))))


; =======================================================================================================================================================================
; MIDI STYLE EXTENSION.
; A language on top of the Standard Midi language for description of the Yamaha Accompaniment Style Language.

; From Standard Midi to Style. In the slipstream of midi PARSING.
; The reverse function is style-ast-to-standard-midi-file-ast.
(define (standard-midi-file-ast-to-style-ast ast input-file-path)
  (let* ((header (ast-subtree ast "MidiHeader"))
         (track (ast-subtree ast "MidiTrack" 1))
         (remaining-tracks (cdr (find-asts ast "MidiTrack")))
         (input-file-name-proper (file-name-proper input-file-path))      ; expected form such as x.S123.sty
         (image-file-name (file-name-extension input-file-name-proper))
         (default-image-file-name "S910")
        )

    ; Legal structure checking:
    (if (not (equal? "0" (ast-attribute header 'format))) (laml-error "A style file must be a format 0 midi file"))
    (if (not (null? remaining-tracks)) (laml-error "A Style file is only allowed to have one track"))

    ; Discovering structure in the Format 0 midi sequence, followed by assembly:
    (let* (
           (track-messages (eliminate-events-1 meta-type-1-non-fn-txt (ast-subtrees track) 1 0))  ; Meta type 1 events can confuse subsequent structuring. Thus elimiated.
           (non-midi-suffix-ast (find-first-ast ast "NonMidiSuffix"))  ; finding trailing non-midi part 
           (track-section-a-list (structure-style-track track-messages))   ; finding style-related meta events and cut the midi events into pieces (sections)
           (non-init-sections (if (> (length track-section-a-list) 3)    ; The three first sections (the initial, SFF1, and SInt) are treated specially.
                                     (cdr (cdr (cdr track-section-a-list)))
                                     (laml-error "The style must contain more than just initial sections.")))
          )

      ; Synthesis:
      (TyrosStyle 'internal:run-action-procedure "false"
        'mode (ast-attribute header 'mode)
        'image (if (empty-string? image-file-name) default-image-file-name image-file-name)
        'pulsesPerQuarterNote (ast-attribute header 'pulsesPerQuarterNote)
        (MidiSection
          (StyleInit (get-style-track-section-of-name "Init" track-section-a-list))
          (StyleId (get-style-track-section-of-name "SFF1" track-section-a-list))
          (StyleSetup (get-style-track-section-of-name "SInt" track-section-a-list)) 

          ; Introduce mirror functions instead meta events. This introduces more structure. Preserves the order of style sections.
          (map 
            (lambda (section) 
              (let* ((meta-section-name (car section))
                     (mirror-function (mirror-function-of-meta-section-name meta-section-name)))
                (mirror-function (cdr section)))
            )
            non-init-sections)

        )
        (let* ((non-midi-suffix-binary-string (hex-to-binary-string (ast-text non-midi-suffix-ast)))
               (casm-list-structure (parse-casm-string non-midi-suffix-binary-string))
               (casm-length (length-of-casm-list casm-list-structure))
               (remaining-non-midi-suffix-hex-string 
                  (binary-to-hex-string (substring non-midi-suffix-binary-string casm-length (string-length non-midi-suffix-binary-string))))
               (remaining-non-midi-suffix-ast (NonMidiSuffix remaining-non-midi-suffix-hex-string))
               )
          (list
            (CasmSection  
              (map cseg-to-cseg-ast
                   (cseg-part-of-casm-list casm-list-structure))
            )
            remaining-non-midi-suffix-ast
          )
        )
      )
    )
  )
)

(define meta-type-1-non-fn-txt
  (lambda (x) 
    (if (Meta? x 1)
        (let ((meta-txt (ast-text x)))
           (if (>= 3 (string-length meta-txt))
               (not (equal "fn:" (substring meta-txt 0 3)))
               #t))
        #f)))


; Looking up in a trac-section-alist with entries of the form (meta-name . events).
(define (get-style-track-section-of-name name track-section-alist)
  (let ((res (assoc name track-section-alist)))
    (if res (cdr res) #f)))

; -----------------------------------------------------------------------------------
; A list which describe the mutual relationships between of style section meta events,
; names of mirror functions, and the mirror functions as such.
(define style-section-naming
 (list
    (list "SFF1" "StyleId" StyleId)
    (list "Sint" "StyleSetup" StyleSetup)

    (list "Intro A" "IntroA" IntroA )
    (list "Intro B" "IntroB" IntroB )
    (list "Intro C" "IntroC" IntroC)
  
    (list "Main A" "MainA" MainA )
    (list "Main B" "MainB" MainB )
    (list "Main C" "MainC" MainC )
    (list "Main D" "MainD" MainD )
  
    (list "Ending A" "EndingA" EndingA )
    (list "Ending B" "EndingB" EndingB ) 
    (list "Ending C" "EndingC" EndingC )
  
    (list "Fill In AA" "FillInAA" FillInAA )
    (list "Fill In BB" "FillInBB" FillInBB )
    (list "Fill In CC" "FillInCC" FillInCC )
    (list "Fill In DD" "FillInDD" FillInDD )
    (list "Fill In BA" "FillInBA" FillInBA )          
    (list "Fill In AB" "FillInAB" FillInAB )
  ))


; Functions that lookup in the relationships defined in the list style-section-naming from above:

(define (mirror-function-of-meta-section-name meta-name)
  (let ((res (find-in-list (lambda (entry) (equal? (car entry) meta-name)) style-section-naming)))
    (if res (third res) #f)))

(define (meta-name-of-mirror-section-name mirror-name)
  (let ((res (find-in-list (lambda (entry) (equal? (cadr entry) mirror-name)) style-section-naming)))
    (if res (first res) #f)))


; -----------------------------------------------------------------------------------

; Traverse message-list and collect the meta sections of the style.
(define (structure-style-track message-list)
  (structure-style-track-1 message-list '() '() "Init"))

; Tail recursive helping function doing the real structuring work
(define (structure-style-track-1 message-list collected-list res-a-list current-section-name)
  (if (null? message-list)
      (reverse   ; reverse to maintain the order
        (if (null? collected-list)      ; probably false
            res-a-list
            (cons
              (cons current-section-name (no-end-of-track-ad-hoc collected-list)) ; the last section which not ended by Meta type 6 event,
                                                                                  ; but instead of Meta type 47 end of track event. Eliminate end of track event.
              res-a-list))
      )
      (let ((mes (car message-list)))
         (cond ((and (ast? mes) (equal? "Meta" (ast-element-name mes)) (= 6 (as-number (ast-attribute mes 'type))))  ; encountered a Meta type 6 message
                   (let ((new-section-name (ast-text mes)))
                     (structure-style-track-1 
                      (cdr message-list) ; eat meta message
                      '()   ; start from fresh on next collection
                      (cons  ; add contribution to res-alist
                       (cons current-section-name (reverse collected-list))
                       res-a-list)
                      new-section-name)))
               ((ast? mes)
                  (structure-style-track-1 (cdr message-list) (cons mes collected-list) res-a-list current-section-name))
               (else  ; should not happen. Skip possible boolean separators
                  (structure-style-track-1 (cdr message-list) collected-list res-a-list current-section-name))))))

; Precondition: reversed-message-list is not empty.
; Takes a reversed message-list. Checks if first (meaning: last) is an end of track event.
; If so, elminate it. Else just return reverse of input.
(define (no-end-of-track-ad-hoc reversed-message-list)
  (let ((last (first reversed-message-list)))
     (if (Meta? last 47) ; end of track
         (reverse (cdr reversed-message-list))
         (reverse reversed-message-list))))



; ------------------------------------------------------------------------------------

; The reverse of standard-midi-file-ast-to-style-ast.
; From Style to back to Standard Midi. 
; Works on both ASTs with and without CasmSection. 
; (Style ASTS without a CasmSection may exist in medium old midl files).
; Typically In the beginning of an UNPARSING process, which ultimately will lead to a binary midi stream.
; Rebuild the standard format 0 midi file from the TyrosStyle ast.
; May involve retiming if editing has modified the timing of style. This is the most tricky part of this function.
(define (style-ast-to-standard-midi-file-ast style-ast)
  (let* ((mode (ast-attribute style-ast 'mode))
         (default-image-name "S809")
         (image (ast-attribute style-ast 'image default-image-name))
         (ppqn (as-number (ast-attribute style-ast 'pulsesPerQuarterNote)))

         (time-signature (time-signature-of-style-ast style-ast))   ; such as the list (4 4) or (6 8)
         (can-time-signature (canonicalize-time-signature time-signature))  ; such as (4 4) or (3 4)
         (bar-length (if (= 4 (second can-time-signature))     ; if not quarters, enforce (4 4)
                         (* ppqn (first can-time-signature))
                         (* ppqn 4)))

         (midi-section-ast (find-first-ast style-ast "MidiSection"))
         (delete-and-add-attributes (ast-attributes midi-section-ast))       ; pass this attribute list to constituents - for inheritance purposes
         (midi-section-parts (filter ast? (ast-subtrees midi-section-ast)))  ; The list of StyleInit, StyleId, ... asts. Filtering as a precaution only...
         (casm-section-ast (find-first-ast style-ast "CasmSection"))
         (binary-casm-section (if casm-section-ast (unparse-casm-ast casm-section-ast) ""))
         (non-midi-non-casm-suffix-ast (find-first-ast style-ast "NonMidiSuffix"))    ; The non-midi and (typically) non-casm suffix
        )
     (StandardMidiFile 'internal:run-action-procedure "false"
       (MidiHeader 'format "0" 'numberOfTracks "1" 'pulsesPerQuarterNote ppqn 'mode mode 'counterTransposition "0")
       (MidiTrack

         ; Initial three parts: OK for both absTime (!) and deltaTime (??)
         (ast-subtrees (first midi-section-parts))                                                        ; Messages in StyleInit part
         (Meta 'deltaTime "0" 'type "6" "SFF1") (ast-subtrees (second midi-section-parts))                ; Messages in StyleId part
         (Meta 'deltaTime "0" 'type "6" "SInt") (ast-subtrees (third midi-section-parts))                 ; Messages in StyleSetup part

         (make-flat-retimed-and-processed-style-sections 
              delete-and-add-attributes
              mode ppqn
              bar-length
              (cdr (cdr (cdr midi-section-parts))))   ; retime remaining sections in case the style has been edited.

         ; End of meta track is added by make-flat-retimed-and-processed-style-sections

       )
       (if casm-section-ast
           (NonMidiSuffix ; Add unparsed CASM to NonMidiSuffix at the Standard Midi file level.
            (string-append (binary-to-hex-string binary-casm-section) ; back from binary to hex string - to stay compatible with the StandardMidiFile level
                           " "
                           (ast-text non-midi-non-casm-suffix-ast)
                           )
           )
           (NonMidiSuffix (ast-text non-midi-non-casm-suffix-ast))
       )
     )
  )
)

; Returns the time signature of the style-ast, as extracted from a first encountered Meta type 88 event.
; Returns (4 4) as default value.
(define (time-signature-of-style-ast style-ast)
  (let ((candiate-ast (traverse-and-collect-first-from-ast 
                         style-ast
                         (lambda (x) (Meta? x 88))
                         id-1)))
    (if candiate-ast
        (time-signature-of-meta-type-88-ast candiate-ast)
        '(4 4))))

; If at all possible, return a list of quarters. Else return time-signature unchanged.
(define (canonicalize-time-signature time-signature)
  (let ((a (first time-signature))
        (b (second time-signature)))
    (cond ((= b 4) time-signature)
          ((= 0 (remainder b 4))      ;  b is multiplum of 4: 8 for instance
             (let ((q (quotient b 4)))
               (if (= (remainder a q) 0) ; quotient (such as 2) divides a
                   (list (quotient a q) (quotient b q))
                   time-signature)))
          (else time-signature))))


; NOT USED
(define (get-section-of-style-ast style-ast section-name)
 (let ((res (find-first-ast style-ast section-name ast-subtrees)))
   (if res res '())))

; Make a flat list of messages from the list of style sections asts.
; Insert meta messages in between.
; The length of two first parameters must be the same.
; Enforce that the meta messages starts at bar boundaries. This is actually the most serious concern.
(define (make-flat-retimed-and-processed-style-sections delete-and-add-attributes-of-midi-section mode ppqn bar-length list-of-style-section-asts)
  (cond ((equal? mode "absTime") 
           (make-flat-retimed-style-sections-abs-time delete-and-add-attributes-of-midi-section ppqn bar-length bar-length         ; thus at boundary of bar 1 
                                                      list-of-style-section-asts))
        ((equal? mode "deltaTime") 
           (make-flat-retimed-style-sections-delta-time delete-and-add-attributes-of-midi-section ppqn bar-length ??? list-of-style-section-asts))
        (else (laml-error "make-flat-retimed-style-sections: Unsupported mode" mode)))
)

; The first entry in list-of-style-section-asts must start at init-abs-time.
; It is assumed, as a precondition, that init-abs-time is at a bar boundary.
; Process delete and add attributes, and do retiming if necessary.
(define (make-flat-retimed-style-sections-abs-time delete-and-add-attributes-of-midi-section ppqn bar-length init-abs-time list-of-style-section-asts)
   (if (null? list-of-style-section-asts)
       (list (Meta 'absTime (next-multiplum-of bar-length init-abs-time) 'type "47"))  ; termination of list - end of track Meta message
       (let* ((first-style-section-ast (car list-of-style-section-asts))
              (delete-and-add-attributes-this-section (ast-attributes first-style-section-ast))
              (delete-and-add-attributes (join-delete-and-add-attributes delete-and-add-attributes-of-midi-section delete-and-add-attributes-this-section))
              (mirror-name (ast-element-name first-style-section-ast))
              (section-limit (limit-of-section mirror-name delete-and-add-attributes ppqn bar-length)) ; in pulses, or #f
              (meta-name (meta-name-of-mirror-section-name mirror-name))
              (midi-events (ast-subtrees first-style-section-ast))  
              (sorted-midi-events (stable-sort-list midi-events abs-time-ast-leq?))   ; Sorting - primarily a precaution
              (first-abs-time (if (not (null? sorted-midi-events)) (as-number (ast-attribute (first sorted-midi-events) 'absTime)) #f))
             )
         (if first-abs-time  ; Non-empty section. The case to watch
             (let* ((midi-events-with-deletions (effectuate-deletes sorted-midi-events delete-and-add-attributes))   ; Do deletions
                    (delta-time-adds (find-stuff-to-add ppqn delete-and-add-attributes))  ; Find stuff to be added - already merged at this point
                    (midi-events-with-additions 
                      (add-delta-time-events-to-style-events
                           delta-time-adds
                           midi-events-with-deletions
                           init-abs-time meta-name))
                    (displaced-midi-events (time-displace-1 (- init-abs-time first-abs-time) midi-events-with-additions))  ; enforce that first event starts at
                    (truncated-midi-events 
                       (if section-limit
                           (truncate-midi-sequence-beyond (+ init-abs-time section-limit) displaced-midi-events)
                           displaced-midi-events))
                    (last-event-ast (last truncated-midi-events))
                    (last-abs-time (as-number (ast-attribute last-event-ast 'absTime)))
                    (next-first-time-0 (next-multiplum-of bar-length last-abs-time))  ; calculating the bar boundary of next section
                    (next-first-time (if (= next-first-time-0 init-abs-time)   ; Enforce that next part starts at least one bar later than this part
                                         (next-multiplum-of bar-length (+ next-first-time-0 1))
                                         next-first-time-0))
                   )
               (cons (list (Meta 'absTime init-abs-time 'type "6" meta-name)
                           truncated-midi-events
                     )
                     (make-flat-retimed-style-sections-abs-time           ; Recursive processing of remaining sections
                         delete-and-add-attributes-of-midi-section ppqn bar-length next-first-time   ; at next bar boundary
                         (cdr list-of-style-section-asts))
               )
             )
             (let* ((delta-time-adds (find-stuff-to-add ppqn delete-and-add-attributes))  ; Find stuff to be added - already merged at this point
                    (midi-events-with-additions 
                      (add-delta-time-events-to-style-events-1 
                            delta-time-adds
                            (Meta 'absTime init-abs-time 'type "6" meta-name)  
                            init-abs-time))
                    (first-abs-time (if (not (null? midi-events-with-additions)) (as-number (ast-attribute (first midi-events-with-additions) 'absTime)) #f))
                    (displaced-midi-events 
                       (if first-abs-time
                           (time-displace-1 (- init-abs-time first-abs-time) midi-events-with-additions)
                           midi-events-with-additions))   ; no displacement necessary (???)
                    (truncated-midi-events 
                       (if section-limit
                           (truncate-midi-sequence-beyond (+ init-abs-time section-limit) displaced-midi-events)
                           displaced-midi-events))
                    (last-event-ast (last truncated-midi-events))
                    (last-abs-time (as-number (ast-attribute last-event-ast 'absTime)))
                    (next-first-time-0 (next-multiplum-of bar-length last-abs-time))  ; calculating the bar boundary of next section
                    (next-first-time (if (= next-first-time-0 init-abs-time)   ; Enforce that next part starts at leas one bar later than this part
                                         (next-multiplum-of bar-length (+ next-first-time-0 1))
                                         next-first-time-0))
                   )
                (cons
                    truncated-midi-events
                    (make-flat-retimed-style-sections-abs-time   ; Recursive processing of remaining sections
                        delete-and-add-attributes-of-midi-section ppqn bar-length next-first-time     ; at next bar boundary
                        (cdr list-of-style-section-asts)))
             )
         ))))

; Return upper limit of number of pulses of mirror name, as reflected by the attributes-property-list of the section (looking in particular of limitQN attribute
; in attributes-property-list). Enforce that fills have an enforced (maximum) length of bar-length.
; If no upper limit is specified - explicitly or implicitly - return #f
(define (limit-of-section mirror-name attributes-property-list ppqn bar-length)
  (if (member mirror-name (list "FillInAA" "FillInBB" "FillInCC" "FillInDD" "FillInBA" "FillInAB"))
      bar-length
      (let ((limit (defaulted-get-prop 'limitQN attributes-property-list #f)))
        (if limit
            (* (as-number limit) ppqn)
            #f))))

; displaced-midi-events are abs-time. Discard all events with absTime larger than pulse-limit.
; Truncate the durations such that the sum of absTime and duration do not get larger than pulse-limit. 
(define (truncate-midi-sequence-beyond pulse-limit displaced-midi-events)
  (truncate-midi-sequence-beyond-1 pulse-limit displaced-midi-events '()))

(define (truncate-midi-sequence-beyond-1 pulse-limit midi-events res)
  (if (null? midi-events)
      (reverse res)
      (let* ((mes-ast (first midi-events)))
         (if (ast? mes-ast) ; precaution: always true
             (let ((abs-time-of-mes (ast-attribute mes-ast 'absTime)))
                (if (>= (as-number abs-time-of-mes) pulse-limit)
                    (truncate-midi-sequence-beyond-1 pulse-limit (cdr midi-events) res) ; remove
                    (truncate-midi-sequence-beyond-1 pulse-limit (cdr midi-events)
                        (cons (truncate-duration mes-ast pulse-limit) res)
                    ) ; keep
                )
             )
             (truncate-midi-sequence-beyond-1 pulse-limit (cdr midi-events) res)))))

(define (truncate-duration mes-ast pulse-limit)
  (let ((abs-time (as-number (ast-attribute mes-ast 'absTime)))
        (dur (ast-attribute mes-ast 'duration #f)))
     (if dur
         (let* ((dur-number (as-number dur))
                (new-duration (min dur-number (- pulse-limit abs-time))))
           (copy-ast-mutate-attributes mes-ast 'duration new-duration))
         mes-ast)))  

(define (join-delete-and-add-attributes prop-list-father prop-list-son)
  (let* ((delete-channels-string-father (defaulted-get-prop 'deleteChannels prop-list-father ""))
         (delete-channels-string-son    (defaulted-get-prop 'deleteChannels prop-list-son ""))
         (joined-delete-string (string-append delete-channels-string-son "," delete-channels-string-father)))
    (append
       (list 'deleteChannels joined-delete-string)  
       (but-props prop-list-son    (list 'deleteChannels))  ; son attributes before father attributes - natural inheritance.
       (but-props prop-list-father (list 'deleteChannels)))))

(define (effectuate-deletes midi-event-list delete-and-add-attributes)
  (let* ((delete-channels-str (defaulted-get-prop 'deleteChannels delete-and-add-attributes ""))
         (delete-channels-str-lst (comma-separated-string-to-string-list delete-channels-str))
         (channels-to-delete (map as-number delete-channels-str-lst)))
    (delete-channels-in-message-list channels-to-delete midi-event-list)))

(define (add-delta-time-events-to-style-events delta-time-events-to-add abs-time-style-events first-abs-time meta-name)
 (let ((abs-time-style-events-with-base-and-inserted-relative-events
         (append
            (list (Meta 'absTime first-abs-time 'type 1 (string-append "fn:" meta-name (as-string #\nul))))  ; plays double role...
            delta-time-events-to-add
            abs-time-style-events  
         )))
   (get-rid-of-delta-times 
      abs-time-style-events-with-base-and-inserted-relative-events
      first-abs-time)))

; Variant for a single abs-time event (the type 6 meta event):
(define (add-delta-time-events-to-style-events-1 delta-time-events-to-add single-abs-meta-type-6-event first-abs-time)
 (let ((abs-time-style-events-with-base-and-inserted-relative-events
         (cons
            single-abs-meta-type-6-event
            delta-time-events-to-add)))
   (get-rid-of-delta-times 
      abs-time-style-events-with-base-and-inserted-relative-events
      first-abs-time)))
            
   
(define (find-stuff-to-add ppqn delete-and-add-attributes)
  (find-stuff-to-add-1 (number-interval 1 16) ppqn delete-and-add-attributes))

(define (find-stuff-to-add-1 channel-list ppqn delete-and-add-attributes)
  (if (null? channel-list) 
      '()
      (let* ((channel-number (first channel-list))
             (part-attr-name (as-symbol (string-append "addMidiPart" (as-string channel-number))))
             (piece-file-path (defaulted-get-prop part-attr-name delete-and-add-attributes #f))

             (channel-attr-name (as-symbol (string-append "addAsChannel" (as-string channel-number))))
             (target-channel (defaulted-get-prop channel-attr-name  delete-and-add-attributes #f))

             (repetition-attr-name (as-symbol (string-append "repetitions" (as-string channel-number))))
             (repetitions (as-number (defaulted-get-prop repetition-attr-name  delete-and-add-attributes 1)))

             (part-length-qn-attr-name (as-symbol (string-append "partLengthQN" (as-string channel-number))))
             (part-length-qn (as-number (defaulted-get-prop part-length-qn-attr-name delete-and-add-attributes 1))) 
                                                                                             ; Interpreted as quarter notes length of the stuff to add
                                                                                             ; Only relevant - and only used - if repetitions is greater than one.

             (delta-time-list (get-midi-part piece-file-path ppqn))  ; get-midi-part adapts the pieces to the styles ppqn rate.
             
             (repeated-delta-time-list   ; repeat delta-time-list, but enforce periode length of (* part-length-qn ppqn) pulses
                (if (or (= repetitions 1) (null? delta-time-list)) 
                    delta-time-list
                    (repeat-messages-enforce-periode-length-1 repetitions (* part-length-qn ppqn) delta-time-list)))
             

             (delta-time-list-rech (rechannel-part repeated-delta-time-list target-channel))  ; Move all events in delta-time-list to channel
            )
         (delta-merge delta-time-list-rech
                      (find-stuff-to-add-1 (cdr channel-list) ppqn delete-and-add-attributes)))))

; Rechanel message-list if to-channel is non-false.
(define (rechannel-part message-list to-channel)
  (if to-channel       
      (let ((ch-from (as-number (ast-attribute (first message-list) 'channel))))
        (join-channels-1 (list ch-from) (as-number to-channel) message-list))
      message-list))  

; Read and parse the midi file part in abs-file-path, and stretch it to style-ppqn.
; At least one NoteOn event must be returned.
; A list starting with a deltaTimed NoteOn events is returned.
(define (get-midi-part abs-file-path style-ppqn)
  (if abs-file-path
      (let* ((ast (midi-file-to-laml-ast abs-file-path 'deltaTime 0 #f))
             (ast-header (find-first-ast ast "MidiHeader"))
             (insertion-ppqn (as-number (ast-attribute ast-header 'pulsesPerQuarterNote)))
             (track (find-first-ast ast "MidiTrack"))
             (track-events (ast-subtrees track))  ; ppqn adaption. The inserted stuff must be converted to the style's ppqn resolution
             (ppqn-adapted-track-events (time-stretch-1 (/ style-ppqn insertion-ppqn) track-events))
             (noteon-track-events (find-tail-in-list NoteOn? ppqn-adapted-track-events))  ; start from first NoteOn
             (noteon-no-meta-events (eliminate-events Meta? noteon-track-events))  ; eliminates meta events, most important the end of track event.
            )
         noteon-no-meta-events)
      '()))
          

(define (delete-channels-in-message-list channels-to-delete midi-event-list)
  (if (null? channels-to-delete)
      midi-event-list
      (delete-channels-in-message-list
         (cdr channels-to-delete)
         (delete-channel-abs-time (first channels-to-delete) midi-event-list))))

(define (comma-separated-string-to-string-list str)
  (map (lambda (str) (strip-trailing-characters white-space-char-list (strip-initial-characters white-space-char-list str)))
       (split-string-by-predicate str (lambda (ch) (eqv? ch #\,)))))


(define (make-flat-retimed-style-sections-delta-time delete-and-add-attributes-of-midi-section bar-length init-abs-time list-of-style-section-asts)
  (laml-error "NOT implemented yet.")
)

; Return the next multiplum of the integer b which is equal to or larger than the integer a.
(define (next-multiplum-of b a)
 (let ((rem (remainder a b)))
  (if (= rem 0)
      a
      (+ (- a rem) b))))

(define (midl-project-file suffix)
  (string-append "c:/users/kurt/Media/midi-laml-projects/" suffix))

(define (midi-piece-file suffix)
  (string-append "c:/users/kurt/Media/Tyros/Styles/midi/MIDI-PIECES/All-pieces/" suffix))


; ---------------------------------------------------------------------------------------------------------------
; CASM parsing - binary to intermediate list structure

; Parse the casm data in str and return a list structure.
; Return a list of the form (casm n ...) where casm is a tag, n is the total length of the casm section, and ... is the parsed casm data. 
(define (parse-casm-string str)
  (if (and (>= (string-length str) 4) (equal? (substring str 0 4) "CASM"))     ; Byte 0-3
      (let* ((number-of-bytes (byte-string-to-integer (substring str 4 8)))    ; Byte 4-8
             (csegs (parse-cseg-sections (substring str 8 (string-length str)))))
        (list 'casm (+ number-of-bytes 8) csegs)) 
      #f))

; Casm selectors:
(define length-of-casm-list (make-selector-function 2 "length-of-casm-list"))
(define cseg-part-of-casm-list (make-selector-function 3 "cseg-part-of-casm-list"))

; Parse one or more cseg sections. Return a list of parsed cseg sections.
; The first starts at the beginning of str.
(define (parse-cseg-sections str)
  (let* ((res (parse-cseg-section str))
         (cseg-lgt (first res))
         (cseg (second res)))
    (if cseg
        (cons cseg
              (parse-cseg-sections (substring str cseg-lgt (string-length str))) ; recurse to extract rest
              )
        '())))

; Parse one cseg section.
; Return a list of length 2 of length of parsed portion of str and the parsed cseg contents.
; Return (list 0 #f) if there are no more csegs in str.
(define (parse-cseg-section str)
  (if (and (>= (string-length str) 4) (equal? (substring str 0 4) "CSEG"))   ; Byte 0-3
      (let* ((number-of-bytes (byte-string-to-integer (substring str 4 8)))  ; Byte 4-8
            )
         (list (+ number-of-bytes 8)
               (let* ((res (parse-sdec (substring str 8 (string-length str))))
                      (sdec-lgt (first res))
                      (sdec (second res)))
                 (list 'cseg sdec 
                    (let* ((ctabs (parse-ctab-or-cntt-sections (substring str (+ 4 (+ 8 sdec-lgt)) (string-length str)))))
                       ctabs
                    )
                 )                   
               )))
      (list 0 #f)))

; Selectors
(define sdec-of-cseg-list (make-selector-function 2 "sdec-of-cseg-list"))
(define ctabs-and-cntts-of-cseg-list (make-selector-function 3 "ctabs-and-cntts-of-cseg-list"))


; Parse one or more ctab sections. Return a list of parsed ctab sections.
; The first starts at the beginning of str.
(define (parse-ctab-or-cntt-sections str)
  (let* ((res (parse-ctab-or-cntt-section str))
         (ctab-lgt (first res))
         (ctab (second res)))
    (if ctab
        (cons ctab
              (parse-ctab-or-cntt-sections (substring str ctab-lgt (string-length str))) ; recurse to extract rest
              )
        '())))

; Parse one ctab section or one cntt section. String starts with ctab or cntt
; Return a list of length 2 of length of parsed portion of str and the parsed ctab/cntt contents.
; Return (list 0 #f) if there are no more ctabs/cntt in str.
(define (parse-ctab-or-cntt-section str)
  (cond ((and (>= (string-length str) 4) (equal? (substring str 0 4) "Ctab")) ; Byte 0-3
         (let* ((number-of-bytes (byte-string-to-integer (substring str 4 8)))) ; Byte 4-8
           (list (+ number-of-bytes 8)
                 (let* ((ctab-contents (substring str 8 (+ 8 number-of-bytes)))
                        (source-channel (byte-string-to-integer (substring ctab-contents 0 1)))
                        (name (substring ctab-contents 1 9))
                        (destination-channel (byte-string-to-integer (substring ctab-contents 9 10)))
                        (editable (byte-string-to-integer (substring ctab-contents 10 11))) ; 0 or 1
                        (note-mute (byte-string-to-bit-list (substring ctab-contents 11 13) 16))
                        (chord-mute (byte-string-to-bit-list (substring ctab-contents 13 18) 40))
                        (source-chord (byte-string-to-integer (substring ctab-contents 18 19)))
                        (source-chord-type (byte-string-to-integer (substring ctab-contents 19 20)))
                        (ntr (byte-string-to-integer (substring ctab-contents 20 21))) ; 0 or 1
                        (ntt (byte-string-to-integer (substring ctab-contents 21 22))) ; 0 .. 5
                        (high-key (byte-string-to-integer (substring ctab-contents 22 23))) ; 0 .. 11
                        (note-low-limit (byte-string-to-integer (substring ctab-contents 23 24)))
                        (note-high-limit (byte-string-to-integer (substring ctab-contents 24 25)))
                        (retrigger-rule (byte-string-to-integer (substring ctab-contents 25 26))) ; 0 .. 5
                        (special-feature-bytes (substring ctab-contents 26 (string-length ctab-contents))) 
                        )
                   (list 'ctab source-channel name destination-channel editable note-mute chord-mute source-chord source-chord-type
                         ntr ntt high-key note-low-limit note-high-limit retrigger-rule special-feature-bytes)
                   )
                 )))
        ((and (>= (string-length str) 4) (equal? (substring str 0 4) "Cntt")) ; Byte 0-3
           (let* ((number-of-bytes (byte-string-to-integer (substring str 4 8)))) ; Byte 4-8
             (list (+ number-of-bytes 8)
                   (let* ((cntt-contents (substring str 8 (+ 8 number-of-bytes)))
                          (source-channel (byte-string-to-integer (substring cntt-contents 0 1)))
                          (ntt (byte-string-to-integer (substring cntt-contents 1 2))) ; 0 .. 10 in 7 lower order bits  - extension of ntt in ctab
                         )
                     (list 'cntt source-channel ntt)
                   )
             )
           )
        )
        (else  
           (list 0 #f))))

; Selectors ctab/cntt:
(define ctab-or-cntt-tag-of                   (make-selector-function 1 "ctab-or-cntt-tag"))

; Selectors - ctab:
(define source-channel-of-ctab-list           (make-selector-function 2 "source-channel-of-ctab-list"))               ;  
(define name-of-ctab-list                     (make-selector-function 3 "name-of-ctab-list"))
(define destination-channel-of-ctab-list      (make-selector-function 4 "destination-channel-of-ctab-list"))
(define editable-of-ctab-list                 (make-selector-function 5 "editable-of-ctab-list"))
(define note-mute-bit-list-of-ctab-list       (make-selector-function 6 "note-mute-bit-list-of-ctab-list"))
(define chord-mute-bit-list-of-ctab-list      (make-selector-function 7 "chord-mute-bit-list-of-ctab-list"))
(define source-chord-of-ctab-list             (make-selector-function 8 "source-chord-of-ctab-list"))
(define source-chord-type-of-ctab-list        (make-selector-function 9 "source-chord-type-of-ctab-list"))
(define note-transposition-rule-of-ctab-list  (make-selector-function 10 "note-transposition-rule-of-ctab-list"))
(define note-transposition-table-of-ctab-list (make-selector-function 11 "note-transposition-table-of-ctab-list"))
(define high-key-of-ctab-list                 (make-selector-function 12 "high-key-of-ctab-list"))
(define note-low-limit-of-ctab-list           (make-selector-function 13 "note-low-limit-of-ctab-list"))
(define note-high-limit-of-ctab-list          (make-selector-function 14 "note-high-limit-of-ctab-list"))
(define retrigger-rule-of-ctab-list           (make-selector-function 15 "retrigger-rule-of-ctab-list" ))
(define special-feature-bytes-of-ctab-list    (make-selector-function 16 "sepcial-features-of-ctab-list"))

; Selectors - cntt:a
(define source-channel-of-cntt-list           (make-selector-function 2 "source-channel-of-cntt-list"))
(define note-transposition-table-of-cntt-list (make-selector-function 3 "note-transposition-table-of-cntt-list"))


; Parse a sdec section. The parameter str starts with Sdec
; Return a list of length 2 of length of parsed portion of str and the parsed contents.
; Return (list #f 0) if there are problems.
(define (parse-sdec str)
  (if (and (>= (string-length str) 4) (equal? (substring str 0 4) "Sdec"))   ; Byte 0-3
      (let* ((number-of-bytes (byte-string-to-integer (substring str 4 8))))  ; Byte 4-8
         (list (+ number-of-bytes 4)
               (list 'sdec 
                     (string-to-list (substring str 8 (+ 8 number-of-bytes)) (list #\,))))   
      )
      (list 0 #f)))

; Selectors
(define style-part-list-of-sdec (make-selector-function 2 "style-part-list-of-sdec"))

; ---------------------------------------------------------------------------------------------------------------
; CASM parsing - list structure to AST:

(define (cseg-to-cseg-ast cseg-list)
   (cseg
     (sdec-list-to-sdec-ast (sdec-of-cseg-list cseg-list)) 
     (map ctab-or-cntt-list-to-ast
          (ctabs-and-cntts-of-cseg-list cseg-list))))

(define (sdec-list-to-sdec-ast sdec-lst)
   (sdec (list-to-string (style-part-list-of-sdec sdec-lst) ",")))   

(define (ctab-or-cntt-list-to-ast ctab-or-cntt-list)
  (let ((kind (ctab-or-cntt-tag-of ctab-or-cntt-list)))
    (cond ((eq? kind 'ctab) 
             (ctab
                'source-channel (as-string (+ 1 (as-number (source-channel-of-ctab-list ctab-or-cntt-list))))
                'name (name-of-ctab-list ctab-or-cntt-list)
                'destination-channel (as-string (+ 1 (as-number (destination-channel-of-ctab-list ctab-or-cntt-list))))                
                'editable (if (= 0 (editable-of-ctab-list ctab-or-cntt-list)) "true" "false")
                (note-mute (note-mute-string-of-bit-list (note-mute-bit-list-of-ctab-list ctab-or-cntt-list)))
                'autostart-enable (if (= 1 (sixth (chord-mute-bit-list-of-ctab-list ctab-or-cntt-list))) "true" "false")
                (chord-mute (chord-mute-string-of-bit-list (chord-mute-bit-list-of-ctab-list ctab-or-cntt-list)))
                'source-chord-key (source-key-name-of-number (source-chord-of-ctab-list ctab-or-cntt-list))
                'source-chord-type (source-chord-type-of-number (source-chord-type-of-ctab-list ctab-or-cntt-list))
                'note-transposition-rule (cond ((= (note-transposition-rule-of-ctab-list ctab-or-cntt-list) 0)
                                                  "root-transposition")
                                               ((= (note-transposition-rule-of-ctab-list ctab-or-cntt-list) 1)
                                                  "root-fixed")
                                               (else (laml-error "ctab-or-cntt-list-to-ast: note-transposition-rule value must be 0 or 1" 
                                                                 (note-transposition-rule-of-ctab-list ctab-or-cntt-list))))
                'note-transposition-table (note-transposition-table-number-to-symbolic-name (note-transposition-table-of-ctab-list ctab-or-cntt-list))
                'high-key (source-key-name-of-number (high-key-of-ctab-list ctab-or-cntt-list))
                'note-low-limit (note-number-to-note-name (note-low-limit-of-ctab-list ctab-or-cntt-list))
                'note-high-limit (note-number-to-note-name (note-high-limit-of-ctab-list ctab-or-cntt-list))
                'retrigger-rule (retrigger-number-to-symbolic-name (retrigger-rule-of-ctab-list ctab-or-cntt-list))
                'special-feature-bytes (binary-to-hex-string (special-feature-bytes-of-ctab-list ctab-or-cntt-list))  ; not parsed yet
             )
          )
          ((eq? kind 'cntt) 
            (cntt
              'source-channel (as-string (+ 1 (as-number  (source-channel-of-cntt-list ctab-or-cntt-list))))
              'bass-on-off    (if (> (note-transposition-table-of-cntt-list ctab-or-cntt-list) 127) "on" "off")
              'note-transposition-table (note-transposition-table-number-to-symbolic-name
                                          (remainder (note-transposition-table-of-cntt-list ctab-or-cntt-list) 128))
            )
          )
          (else (laml-error "ctab-or-cntt-list-to-ast: Either a ctab og cntt list structure expected" ctab-or-cntt-list)))))

(define note-name-list (list "C" "CS" "D" "Eb" "E" "F" "FS" "G" "GS" "A" "Bb" "B"))

(define chord-name-list (list "maj" "maj6" "maj7" "maj7s11" "maj9" "maj7-9" "maj6-9" "aug" "min" "min6" "min7" "min7b5" "min-9" "min7-9" 
                              "min7-11" "min-maj7" "min-maj7-9" "dim" "dim7" "7th" "7sus4" "7b5" "7-9" "7s11" "7-13" "7-b9" "7-b13" "7-s9" "maj7aug" "7aug"
                              "1-plus-8" "1-plus-5" "sus4" "1-plus-2-plus-5" "cancel"))

; Return a comma separated list of of those symbolic note names which should be muted according to note-mute-bit-list.
; 0 = mute, 1 = play.
(define (note-mute-string-of-bit-list note-mute-bit-list)
  (let ((used-note-mute-bit-list (cdr (cdr (cdr (cdr note-mute-bit-list))))))
    (list-to-string (select-via-bit-list note-name-list (inverse-bit-list (reverse used-note-mute-bit-list))) ",")
  ))

; Return a comma separated list of of those symbolic chords which should be muted according to note-mute-bit-list.
; 0 = mute, 1 = play.
; The first six bits are not used. "cancel" is not relevant here.
(define (chord-mute-string-of-bit-list chord-mute-bit-list)
  (let ((used-chord-mute-bit-list (cdr (cdr (cdr (cdr (cdr (cdr chord-mute-bit-list))))))))
    (list-to-string (select-via-bit-list (cdr (reverse chord-name-list)) (inverse-bit-list used-chord-mute-bit-list)) ",")))


(define (inverse-bit-list b-lst)
  (map (lambda (b) (if (= b 0) 1 0)) b-lst))

(define (select-via-bit-list selection-list bit-list)
  (filter (lambda (x) x) (map (lambda (sel bit) (if (= bit 1) sel #f)) selection-list bit-list)))

(define (source-key-name-of-number number)
  (if (and (>= number 0) (<= number 11))
      (list-ref note-name-list number)
      (laml-error "source-key-name-of-number: number out of range [0..11]" number)))

(define (source-chord-key-number-of-name name)
 (let ((res (member (upcase-string name) (map upcase-string note-name-list))))
   (if res
       (- 12 (length res))
       (laml-error "source-chord-key-number-of-name: Unknown note name" name))))
  
(define (source-chord-type-of-number number)
  (if (and (>= number 0) (<= number 34))
      (list-ref chord-name-list number)
      (laml-error "source-chord-type-of-number: number out of range [0..34]" number)))

(define (source-chord-type-number-of-name name)
   (let ((res (member (downcase-string name) chord-name-list)))
     (if res
         (- 35 (length res))
         (laml-error "source-chord-type-number-of-name: Unknown chord name" name))))
     

(define ntt-entry-list (list "bypass" "melody" "chord"  "bass" "melodic-minor" "harmonic-minor" "harmonic-minor-5" "natural-minor"
                             "natural-minor-5" "dorian-minor" "dorian-minor-5"))

(define (note-transposition-table-number-to-symbolic-name number)
  (if (and (>= number 0) (<= number 10))
      (list-ref ntt-entry-list number)
      (laml-error "note-transposition-table-number-to-symbolic-name: number out of range [0..10]" number)))

(define retrigger-entry-list (list "stop" "pitch-shift" "pitch-shift-to-root" "retrigger" "retrigger-to-root" "note-generator"))

(define (retrigger-number-to-symbolic-name number)
  (if (and (>= number 0) (<= number 5))
      (list-ref retrigger-entry-list number)
      (laml-error "retrigger-number-to-symbolic-name: number out of range [0..5]" number)))

; ---------------------------------------------------------------------------------------------------------------
; CASM AST Unparsing - from AST to binary data.

; Assume that casemsection-ast is a valid CASM section AST.
(define (unparse-casm-ast casm-section-ast)
  (let ((casm-data-bytes (unparse-list-of-cseg-asts (ast-subtrees casm-section-ast))))
   (string-append
      "CASM" 
      (int10-to-binary (string-length casm-data-bytes) 4)
      casm-data-bytes)))


(define (unparse-list-of-cseg-asts cseg-ast-list)
   (accumulate-right string-append "" (map unparse-cseg-ast cseg-ast-list)))

(define (unparse-cseg-ast cseg-ast)
  (let ((cseg-data-bytes 
         (string-append
          (unparse-sdec-ast (ast-subtree cseg-ast "sdec"))
          (unparse-list-of-ctab-asts (filter (lambda (x) (and (ast? x) (equal? "ctab" (ast-element-name x)))) (ast-subtrees cseg-ast)))
          (unparse-list-of-cntt-asts (filter (lambda (x) (and (ast? x) (equal? "cntt" (ast-element-name x)))) (ast-subtrees cseg-ast))))))
    (string-append
     "CSEG"
     (int10-to-binary (string-length cseg-data-bytes) 4)
     cseg-data-bytes)))

(define (unparse-sdec-ast sdec-ast)
  (let ((sdec-data-bytes (ast-text sdec-ast)))
    (string-append
     "Sdec"
     (int10-to-binary (string-length sdec-data-bytes) 4)
     sdec-data-bytes
     )))

(define (unparse-list-of-ctab-asts ctab-ast-list)
  (accumulate-right string-append "" (map unparse-ctab-ast ctab-ast-list)))

(define (unparse-list-of-cntt-asts cntt-ast-list)
  (accumulate-right string-append "" (map unparse-cntt-ast cntt-ast-list)))

(define (unparse-ctab-ast ctab-ast)
  (let ((ctab-data-bytes
          (string-append
             ; source channel:
             (int10-to-binary (- (as-number (ast-attribute ctab-ast 'source-channel)) 1) 1)

             ; name
             (ensure-string-length (ast-attribute ctab-ast 'name) 8 #\space)

             ; destination channel:
             (int10-to-binary (- (as-number (ast-attribute ctab-ast 'destination-channel)) 1) 1)

             ; editable:
             (int10-to-binary (cond ((equal? (ast-attribute ctab-ast 'editable) "true") 0)
                                    ((equal? (ast-attribute ctab-ast 'editable) "false") 1)
                                    (else (laml-error "The editable attribute must be 'true' or 'false'"))) 1)

             ; note-mute:
             (bit-list-to-byte-string (bit-list-of-note-mute-string (ast-text (ast-subtree ctab-ast "note-mute"))))   

             ; chord-mute:
             (bit-list-to-byte-string (bit-list-of-chord-mute-string 
                                        (ast-text (ast-subtree ctab-ast "chord-mute"))
                                        (cond ((equal? (ast-attribute ctab-ast 'autostart-enable) "true") #t)
                                              ((equal? (ast-attribute ctab-ast 'autostart-enable) "false") #f)
                                              (else (laml-error "unparse-ctab-ast: The attribute autostart-enable must be either 'true' or 'false'")))))
             ; source chord key
             (int10-to-binary (source-chord-key-number-of-name (ast-attribute ctab-ast 'source-chord-key)) 1)

             ; source chord type
             (int10-to-binary (source-chord-type-number-of-name (ast-attribute ctab-ast 'source-chord-type)) 1)

             ; note transposition rule (NTR)
             (int10-to-binary (cond ((equal? (ast-attribute ctab-ast 'note-transposition-rule) "root-transposition") 0)
                                    ((equal? (ast-attribute ctab-ast 'note-transposition-rule) "root-fixed") 1)
                                    (else (laml-error "unparse-ctab-ast: Unknown value of note-transposition-rule" (ast-attribute ctab-ast 'note-transposition-rule)))) 1)

             ; note transposition table (NTT)
             (int10-to-binary (cond ((equal? (ast-attribute ctab-ast 'note-transposition-table) "bypass") 0)
                                    ((equal? (ast-attribute ctab-ast 'note-transposition-table) "melody") 1)
                                    ((equal? (ast-attribute ctab-ast 'note-transposition-table) "chord") 2)
                                    ((equal? (ast-attribute ctab-ast 'note-transposition-table) "bass") 3)
                                    ((equal? (ast-attribute ctab-ast 'note-transposition-table) "melodic-minor") 4)
                                    ((equal? (ast-attribute ctab-ast 'note-transposition-table) "harmonic-minor") 5)
                                    (else (laml-error "unparse-ctab-ast: Unknown value of note-transposition-table" (ast-attribute ctab-ast 'note-transposition-table)))) 1)

             ; high key
             (int10-to-binary (source-chord-key-number-of-name (ast-attribute ctab-ast 'high-key)) 1)

             ; note low limit
             (int10-to-binary (note-name-to-note-number (ast-attribute ctab-ast 'note-low-limit)) 1)

             ; note hight limit
             (int10-to-binary (note-name-to-note-number (ast-attribute ctab-ast 'note-high-limit)) 1)

             ; retrigger rule
             (int10-to-binary (cond ((equal? (ast-attribute ctab-ast 'retrigger-rule) "stop") 0)
                                    ((equal? (ast-attribute ctab-ast 'retrigger-rule) "pitch-shift") 1)
                                    ((equal? (ast-attribute ctab-ast 'retrigger-rule) "pitch-shift-to-root") 2)
                                    ((equal? (ast-attribute ctab-ast 'retrigger-rule) "retrigger") 3)
                                    ((equal? (ast-attribute ctab-ast 'retrigger-rule) "retrigger-to-root") 4)
                                    ((equal? (ast-attribute ctab-ast 'retrigger-rule) "note-generator") 5)
                                    (else (laml-error "unparse-ctab-ast: Unknown value of retrigger-rule" (ast-attribute ctab-ast 'retrigger-rule)))) 1)

             ; special feature
             (hex-to-binary-string (ast-attribute ctab-ast 'special-feature-bytes))
          )
        ))
    (string-append
     "Ctab"
     (int10-to-binary (string-length ctab-data-bytes) 4)
     ctab-data-bytes
     ))  
)

(define (unparse-cntt-ast cntt-ast)
  (let ((cntt-data-bytes
          (string-append
             ; source chord key
             (int10-to-binary (- (as-number (ast-attribute cntt-ast 'source-channel)) 1) 1)

             ; note transposition table (NTT)
             (int10-to-binary (cond ((equal? (ast-attribute cntt-ast 'note-transposition-table) "bypass") 0)
                                    ((equal? (ast-attribute cntt-ast 'note-transposition-table) "melody") 1)
                                    ((equal? (ast-attribute cntt-ast 'note-transposition-table) "chord") 2)
                                    ((equal? (ast-attribute cntt-ast 'note-transposition-table) "bass") 3)
                                    ((equal? (ast-attribute cntt-ast 'note-transposition-table) "melodic-minor") 4)
                                    ((equal? (ast-attribute cntt-ast 'note-transposition-table) "harmonic-minor") 5)
                                    ((equal? (ast-attribute cntt-ast 'note-transposition-table) "harmonic-minor-5") 6)
                                    ((equal? (ast-attribute cntt-ast 'note-transposition-table) "natural-minor") 7)
                                    ((equal? (ast-attribute cntt-ast 'note-transposition-table) "natural-minor-5") 8)
                                    ((equal? (ast-attribute cntt-ast 'note-transposition-table) "dorian-minor") 9)
                                    ((equal? (ast-attribute cntt-ast 'note-transposition-table) "dorian-minor-5") 10)
                                    (else (laml-error "unparse-cntt-ast: Unknown value of note-transposition-table" (ast-attribute cntt-ast 'note-transposition-table)))) 1)
          )))
    (string-append
     "Cntt"
     (int10-to-binary (string-length cntt-data-bytes) 4)
     cntt-data-bytes
     )))  

; If str is shorter than required length, pad str with a number of padding-char characters at the rear end.
; Else just return str.
(define (ensure-string-length str required-length padding-char)
  (cond ((< (string-length str) required-length)
          (string-append str (make-string (- required-length (string-length str)) padding-char)))
        (( = (string-length str) required-length)
          str)
        (else (substring str 0 8))))


; The reverse function of chord-mute-string-of-bit-list.
(define (bit-list-of-chord-mute-string str autostart-enable)
  (let ((chord-mute-list (map upcase-string (string-to-list (transliterate str #\space "") (list #\,)))))
     (list
         0 0 0 0 
         0        ; ??
         (if autostart-enable 1 0)
         (if (member "1-PLUS-2-PLUS-5" chord-mute-list) 0 1)
         (if (member "SUS4" chord-mute-list) 0 1)

         (if (member "1-PLUS-5" chord-mute-list) 0 1)
         (if (member "1-PLUS-8" chord-mute-list) 0 1)
         (if (member "7AUG" chord-mute-list) 0 1)
         (if (member "MAJ7AUG" chord-mute-list) 0 1)
         (if (member "7-S9" chord-mute-list) 0 1)
         (if (member "7-B13" chord-mute-list) 0 1)
         (if (member "7-B9" chord-mute-list) 0 1)
         (if (member "7-13" chord-mute-list) 0 1)

         (if (member "7S11" chord-mute-list) 0 1)
         (if (member "7-9" chord-mute-list) 0 1)
         (if (member "7B5" chord-mute-list) 0 1)
         (if (member "7SUS4" chord-mute-list) 0 1)
         (if (member "7TH" chord-mute-list) 0 1)
         (if (member "DIM7" chord-mute-list) 0 1)
         (if (member "DIM" chord-mute-list) 0 1)
         (if (member "MIN-MAJ7-9" chord-mute-list) 0 1)

         (if (member "MIN-MAJ7" chord-mute-list) 0 1)
         (if (member "MIN7-11" chord-mute-list) 0 1)
         (if (member "MIN7-9" chord-mute-list) 0 1)
         (if (member "MIN-9" chord-mute-list) 0 1)
         (if (member "MIN7B5" chord-mute-list) 0 1)
         (if (member "MIN7" chord-mute-list) 0 1)
         (if (member "MIN6" chord-mute-list) 0 1)
         (if (member "MIN" chord-mute-list) 0 1)

         (if (member "AUG" chord-mute-list) 0 1)
         (if (member "MAJ6-9" chord-mute-list) 0 1)
         (if (member "MAJ7-9" chord-mute-list) 0 1)
         (if (member "MAJ9" chord-mute-list) 0 1)
         (if (member "MAJ7S11" chord-mute-list) 0 1)
         (if (member "MAJ7" chord-mute-list) 0 1)
         (if (member "MAJ6" chord-mute-list) 0 1)
         (if (member "MAJ" chord-mute-list) 0 1)
  )))


; The reverse function of note-mute-string-of-bit-list.
(define (bit-list-of-note-mute-string str)
  (let ((note-mute-list (map upcase-string (string-to-list (transliterate str #\space "") (list #\,)))))
     (list 0 0 0 0
           (if (member "B" note-mute-list) 0 1)
           (if (member "BB" note-mute-list) 0 1)
           (if (member "A" note-mute-list) 0 1)
           (if (member "GS" note-mute-list) 0 1)

           (if (member "G" note-mute-list) 0 1)
           (if (member "FS" note-mute-list) 0 1)
           (if (member "F" note-mute-list) 0 1)
           (if (member "E" note-mute-list) 0 1)

           (if (member "EB" note-mute-list) 0 1)
           (if (member "D" note-mute-list) 0 1)
           (if (member "CS" note-mute-list) 0 1)
           (if (member "C" note-mute-list) 0 1)
     )))



; ---------------------------------------------------------------------------------------------------------------
; More elaborate equal function. For ad hoc testing

(define (eee s1 s2)
  (eee1 s1 s2 0))

(define (eee1 s1 s2 i)
  (cond ((>= i (string-length s1)) "OK")
        ((>= i (string-length s2)) "OK")
        ((eqv? (string-ref s1 i) (string-ref s2 i))
           (eee1 s2 s2 (+ i 1)))
        (laml-error (string-append "Diff at " (as-string i)))))

; ---------------------------------------------------------------------------------------------------------------

; Make a single-note midi file from all NoteOn messages in ast, and write these midi files to output-dir
; in files formed systematically from the attributes of the NoteOn messages.
; First primtive version, which is now replaced by make-single-note-midi-file, see next.
; May produce incorrect ControlChange and ProgramChange, alwasy taken from the prefix part of the events.
; Not used.
(define (make-single-note-midi-file-original ast output-dir)
  (let* ((header (ast-subtree ast "MidiHeader"))
         (format (ast-attribute header 'format))
         (mode (ast-attribute header 'mode))
         (tracks (ast-attribute header 'numberOfTracks)))
   (cond ((equal? mode "absTime")
            (let* ((midi-track (ast-subtree ast "MidiTrack"))
                   (midi-track-messages (filter ast? (ast-subtrees midi-track)))
                   (midi-lgt (length midi-track-messages))
                   (midi-track-prefix-messages (front-sublist-while midi-track-messages (lambda (ast) (not (NoteOn? ast))) midi-lgt))
                   (note-on-messages (filter NoteOn? (rear-sublist midi-track-messages (- midi-lgt (length midi-track-prefix-messages)))))
                   (midi-track-prefix-messages-displaced (map (lambda (m) (copy-ast-mutate-attributes m 'absTime "0")) midi-track-prefix-messages))
                  )
              (if (and (equal? format "0") (equal? tracks "1") (equal? mode "absTime"))
                  (for-each 
                   (lambda (note-on-mes)
                     (let* ((channel (midi 'channel note-on-mes))
                            (abs-time (midi 'absTime note-on-mes))
                            (vel (midi 'velocity note-on-mes))
                            (note (midi 'note note-on-mes))
                            (dur (midi 'duration note-on-mes))

                            (res-ast (StandardMidiFile 'internal:run-action-procedure "false"
                                                       header
                                                       (MidiTrack
                                                        (filter (lambda (m)
                                                                  (and (or (ControlChange? m #t channel) (ProgramChange? m channel))))
                                                                midi-track-prefix-messages-displaced)
                                                        (copy-ast-mutate-attributes note-on-mes 'absTime "1")
                                                        (Meta 'absTime (+ 1 dur) 'type "47" ""))))
                           )
                        (write-text-file 
                          (standard-midi-file-ast-to-bin res-ast)
                          (string-append output-dir (single-midi-note-file-name abs-time channel note vel dur) ".mid")
                        )
                       ))
                     note-on-messages)
                  (laml-error "make-single-note-asts: Expected format 0 ast with a single track"))))
          (else (laml-error "make-single-note-asts:: Expected absTime mode")))))

; Make a single-note midi file from all NoteOn messages in ast, and write these midi files to output-dir
; in files formed systematically from the attributes of the NoteOn messages.
; Produces correct ControlChange and ProgramChange, taken from closest preceding events. 
(define (make-single-note-midi-file ast output-dir)
 (letrec ((find-immediately-preceding-control-program-changes
             (lambda (lst abs-time)
               (let ((res (find-in-list (lambda (e) (<= (first e) abs-time)) lst)))
                 (if res (cdr res) (laml-error "find-immediately-preceding-control-program-changes of make-single-note-midi-file: Should not happen."))))))

  (let* ((header (ast-subtree ast "MidiHeader"))
         (format (ast-attribute header 'format))
         (mode (ast-attribute header 'mode))
         (tracks (ast-attribute header 'numberOfTracks)))
   (cond ((equal? mode "absTime")
            (let* ((midi-track (ast-subtree ast "MidiTrack"))
                   (midi-track-messages (filter ast? (ast-subtrees midi-track)))
                   (midi-lgt (length midi-track-messages))
                   (control-program-change-collection (reverse (find-control-program-changes midi-track-messages)))
                   (midi-track-prefix-messages (front-sublist-while midi-track-messages (lambda (ast) (not (NoteOn? ast))) midi-lgt))
                   (note-on-messages (filter NoteOn? (rear-sublist midi-track-messages (- midi-lgt (length midi-track-prefix-messages)))))
                  )
              (if (and (equal? format "0") (equal? tracks "1") (equal? mode "absTime"))
                  (for-each 
                   (lambda (note-on-mes)
                     (let* ((channel (midi 'channel note-on-mes))
                            (abs-time (midi 'absTime note-on-mes))
                            (vel (midi 'velocity note-on-mes))
                            (note (midi 'note note-on-mes))
                            (dur (midi 'duration note-on-mes))

                            (res-ast (StandardMidiFile 'internal:run-action-procedure "false"
                                                       header
                                                       (MidiTrack
;                                                         (map (lambda (m) (copy-ast-mutate-attributes m 'absTime "0"))
;                                                           (find-immediately-preceding-control-program-changes control-program-change-collection abs-time))
                                                        (copy-ast-mutate-attributes note-on-mes 'absTime "1")
;                                                       (Meta 'absTime (+ 1 dur) 'type "47" "")
                                                       )
                                     )
                            )
                           )
                        (write-text-file 
                          (standard-midi-file-ast-to-bin res-ast)
                          (string-append output-dir (single-midi-note-file-name abs-time channel note vel dur) ".mid")
                        )
                       ))
                     note-on-messages)
                  (laml-error "make-single-note-asts: Expected format 0 ast with a single track"))))
          (else (laml-error "make-single-note-asts:: Expected absTime mode"))))))

; Collect all ControlChange/ProgramChange tripples in midi-track-messages.
; Associate these with the absolute time they occur.
; Returns an association list of entries of the form (abs-time control-msb control-lsb program-ch),
; ordered by increasing abs-time.
(define (find-control-program-changes midi-track-messages)
  (find-control-program-changes-1 midi-track-messages #f '() '())
)

; local-res is the local collection of relevant same-time ControlChange/ProgramChange messages.
; global-res is the overall collection of associations of absTime and ControlChange/ProgramChange tripples. 
(define (find-control-program-changes-1 midi-track-messages abs-time local-res global-res)
 (letrec ((make-entry (lambda (lst) (if (>= (length local-res) 3) (cons abs-time (reverse local-res)) #f)))
          (interesting? (lambda (x) (or (ProgramChange? x) (ControlChange? x 0) (ControlChange? x 32))))
         )
  (cond ((and (null? midi-track-messages) (null? local-res))        (reverse (filter (lambda (x) x) global-res)))
        ((and (null? midi-track-messages) (not (null? local-res)))  (reverse (filter (lambda (x) x) (cons (make-entry local-res) global-res))))
        ((not (null? midi-track-messages))
           (let ((m (first midi-track-messages)))
              (cond ((and (interesting? m) (not abs-time))
                       (find-control-program-changes-1 (cdr midi-track-messages) (midi 'absTime m) (list m) global-res))
                    ((and (interesting? m) abs-time (= abs-time (midi 'absTime m)))
                       (find-control-program-changes-1 (cdr midi-track-messages) abs-time (cons m local-res) global-res))
                    ((and (interesting? m) abs-time (not (= abs-time (midi 'absTime m))))
                       (find-control-program-changes-1 (cdr midi-track-messages) (midi 'absTime m) '() (cons (make-entry local-res) global-res)))
                    ((and (not (interesting? m)) abs-time)
                       (find-control-program-changes-1 (cdr midi-track-messages) #f '() (cons (make-entry local-res) global-res)))
                    ((and (not (interesting? m)) (not abs-time))
                       (find-control-program-changes-1 (cdr midi-track-messages) #f '() global-res))
               ))))))


(define (single-midi-note-file-name abs-time channel note vel dur)
  (string-append (as-string channel) "-" (as-string note) "-" (as-string vel) "-" (as-string dur)))

; ---------------------------------------------------------------------------------------------------------------

; Set preferred-instrument to i (a symbol).
; Can/should be called just after loading.
; Currently the preferred instrument controls some info attributes in the MIDI LAML expressions.
(define (set-instrument i)
 (let ((known-instruments '(tyros motif-xs)))
  (if (memq i known-instruments)
      (set! preferred-instrument i)
      (laml-error "Instrument must be one of:" (list-to-string (map as-string known-instruments) ", ")))))

(define (read-and-set-preferred-instrument-from-environment)
  (set! preferred-instrument (file-read (string-append midi-software-dir "preferred-instrument"))))
            
; ---------------------------------------------------------------------------------------------------------------

; Writes voices.voices file in the LAML temp dir that contains the voices used in ast.
; Assume that ast is SMF format 0 - thus, a single track
(define (do-extract-voice-collection-from-ast ast)
  (let ((header (ast-subtree ast "MidiHeader")))
     (cond ((equal? "0" (ast-attribute header 'format))
               (do-extract-voice-collection-from-smf-0-ast ast))
           ((equal? "1" (ast-attribute header 'format))
               (do-extract-voice-collection-from-smf-1-ast ast))
           (else (laml-error "do-extract-voice-collection-from-ast: Unknown format of SMF AST")))))

  ; Extract voices and write it to file in the laml temp directory.
(define (do-extract-voice-collection-from-smf-0-ast ast)
  (let* ((track-ast (ast-subtree ast "MidiTrack" 1))
         (fixed-part-file-name (ast-attribute track-ast 'fixed-part #f))
         (fixed-part-track-messages (if fixed-part-file-name
                                        (read-and-pre-process-messages (string-append (startup-directory) (source-filename-without-extension) "/" "fixed-parts" "/" fixed-part-file-name) fixed-part-file-name)
                                        '()))
         (messages (abs-merge-two-lists (ast-subtrees track-ast) fixed-part-track-messages))
         (track-messages (msb-lsb-to-internal-pc-attributes messages '(#t . 0) '(#t . 0) '() ))  ; with internal attributes 
         (voices-file-path (string-append (laml-temp-file-path) "voices" "." "voices"))
         (voice-col (make-voice-collection track-messages)))
    (if (file-exists? voices-file-path) (delete-file voices-file-path))
    (file-write
       (cons 'nil
           (map (lambda (ch) 
                   (let ((m (member-by-predicate ch voice-col (lambda (x y) (= x (first y))))))
                      (if m (list (second (car m)) (third (car m)) (fourth (car m)) (fifth (car m))) 'nil)))
                (number-interval 1 16)))
       voices-file-path)
  )
)

; Extract voices and write it to file in the laml temp directory.
(define (do-extract-voice-collection-from-smf-1-ast ast)
  (let* ((track-ast-list (filter (lambda (x) (and (ast? x) (equal? "MidiTrack" (ast-element-name x)))) (ast-subtrees ast)))
         (track-messages-list (map (lambda (track-ast) 
                                     (msb-lsb-to-internal-pc-attributes (ast-subtrees track-ast) '(#t . 0) '(#t . 0) '() ))
                                   track-ast-list))
         (track-messages (flatten track-messages-list))
         (voices-file-path (string-append (laml-temp-file-path) "voices" "." "voices"))
         (voice-col (make-voice-collection track-messages)))
    (if (file-exists? voices-file-path) (delete-file voices-file-path))
    (file-write
       (cons 'nil
           (map (lambda (ch) 
                   (let ((m (member-by-predicate ch voice-col (lambda (x y) (= x (first y))))))
                      (if m (list (second (car m)) (third (car m)) (fourth (car m)) (fifth (car m))) 'nil)))
                (number-interval 1 16)))
       voices-file-path)
  )
)

(define (make-voice-collection messages)
   (map (lambda (lst) (apply make-voice-collection-entry lst))
        (filter pair?
                 (map
                  (lambda (ast)
                    (if (ProgramChange? ast)
                        (if (and (ast-internal-attribute ast 'msb) (ast-internal-attribute ast 'lsb))
                            (let* ((ch (midi 'channel ast))
                                   (msb (ast-internal-attribute ast 'msb))
                                   (lsb (ast-internal-attribute ast 'lsb))
                                   (pc (midi 'number ast)))
                              (list ch msb lsb pc))
                            #f)
                        #f))
                  messages))))

(define (make-voice-collection-entry ch msb lsb pc)
 (let ((voice-name (find-voice-info-string msb lsb pc)))
  (list ch msb lsb pc (if voice-name voice-name "??")) ))


; CHANNEL STATISTICS:

(define (do-extract-channel-stat-from-ast ast)
  (let ((header (ast-subtree ast "MidiHeader")))
     (cond ((equal? "0" (ast-attribute header 'format))
               (do-extract-channel-stat-from-smf-0-ast ast))
           ((equal? "1" (ast-attribute header 'format))
               (do-extract-channel-stat-from-smf-1-ast ast))
           (else (laml-error "do-extract-channel-stat-from-ast: Unknown format of SMF AST")))))

; Extract channel statistics from the format 0 MIDI LAML ast.
; Do also extract a possible fixed-part, and include the fixed-part messages in the statistics.
(define (do-extract-channel-stat-from-smf-0-ast ast)
  (let* ((midi-track-ast (ast-subtree ast "MidiTrack"))
         (fixed-part-file-name (ast-attribute midi-track-ast 'fixed-part #f))
         (fixed-part-track-messages (if fixed-part-file-name
                                        (read-and-pre-process-messages (string-append (startup-directory) (source-filename-without-extension) "/" "fixed-parts" "/" fixed-part-file-name) fixed-part-file-name)
                                        '()))
         (midi-track-messages (ast-subtrees midi-track-ast))
         (messages (abs-merge-two-lists midi-track-messages fixed-part-track-messages))
         (channel-messsages (make-vector 17 '()))  ; a vector from 0 .. 16, where we use 1..16. Indexed by channels.
        )

   ; Distribute message in the channel-indexed vector channel-messsages:
   (for-each (lambda (mes) 
                (let ((ch (midi 'channel mes)))
                   (if ch
                       (vector-set! channel-messsages ch (cons mes (vector-ref channel-messsages ch))))))
             messages)

   ; Present statistics:
   (let ((file-stat-path (string-append (laml-temp-file-path) "channel-stat" "." "txt"))
         (channel-stat-list
             (map (lambda (ch) (single-channel-stat ch (vector-ref channel-messsages ch))) (reverse (number-interval 1 16)))))
      (write-text-file
         (string-append
            "CHANNEL STATISTICS:      " (as-string (length (filter (lambda (m) (midi 'channel m)) messages)))" channel messages and " 
                      (as-string (length (filter (lambda (m) (not (midi 'channel m))) messages)))" non channel messages." ; CR
            (accumulate-right (lambda (txt res) (string-append res CR txt)) "" channel-stat-list)
            CR 
         )
         file-stat-path
      ))))

(define (single-channel-stat ch channel-messages)
 (letrec ((pad-ch (lambda (ch) (if (< ch 10) (string-append " " (as-string ch)) (as-string ch))))) 
  (let* ((channel-note-messages (filter NoteOn? channel-messages))
         (sorted-channel-note-messages (sort-list channel-note-messages (lambda (m1 m2) (<= (midi 'note m1) (midi 'note m2))))))
    (if (null? channel-messages)
        (string-append "  Channel " (pad-ch ch) ": " "    -")
        (let ()
              (string-append "  Channel " (pad-ch ch) ": " (pad-string-to-length 5 (length channel-messages) 'right) " messages" " "
                             " " (pad-string-to-length 5 (length channel-note-messages) 'right) " NoteOn" 

                             (if (null? sorted-channel-note-messages)
                                 "          "
                                 (string-append
                                  " [" (pad-string-to-length 3 (midi 'note (first sorted-channel-note-messages))) "-"
                                  (pad-string-to-length 3 (midi 'note (last sorted-channel-note-messages)) 'right) "]"))
                             
                             "." "    "

                             (report-stat "Sus" (filter (lambda (m) (ControlChange? m 64 ch)) channel-messages))
                             (report-stat "Pan" (filter (lambda (m) (ControlChange? m 10 ch)) channel-messages))
                             (report-stat "PB " (filter (lambda (m) (equal? "PitchBendChange" (ast-element-name m))) channel-messages))
                             (report-stat "Vol" (filter (lambda (m) (ControlChange? m 7 ch)) channel-messages))
                             (report-stat "Exp" (filter (lambda (m) (ControlChange? m 11 ch)) channel-messages))
                             (report-stat "Mod" (filter (lambda (m) (ControlChange? m 1 ch)) channel-messages))
                             (report-stat "Por" (filter (lambda (m) (ControlChange? m 5 ch)) channel-messages))
              ))))))

(define (report-stat text relevant-messages)
  (if (null? relevant-messages) 
      "" ; "            "
      (string-append text ": " (pad-string-to-length 5 (length relevant-messages) 'right) "   ")))


(define (do-extract-channel-stat-from-smf-1-ast ast-fmt-1)
  (do-extract-channel-stat-from-smf-0-ast (smf-to-format-0 'nested ast-fmt-1))
)