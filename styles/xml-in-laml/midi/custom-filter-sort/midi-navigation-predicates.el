(defun form-navigation-predicate ()
  "Point is at the start parenthesis of a midi message form, with name equal to the value of the global variable target-form-of-navigation,
and matching the channel specifications of the gobal variabel channels-to-play.
Return true if this message is a stop point in the navigation"
  (and (= (attribute-of-current-form 'channel 'number) 8) (= (attribute-of-current-form 'note 'number) 49)))


(defun form-navigation-predicate ()
  "Point is at the start parenthesis of a midi message form, with name equal to the value of the global variable target-form-of-navigation,
and matching the channel specifications of the gobal variabel channels-to-play.
Return true if this message is a stop point in the navigation"
  (> (attribute-of-current-form 'note 'number) 100))


(defun form-navigation-predicate () 
  (= (attribute-of-current-form 'value 'number) 0))

(defun form-navigation-predicate ()
  (< (attribute-of-current-form 'note 'number) 60))


(defun form-navigation-predicate () t)

(defun form-navigation-predicate ()
  (and (> (attribute-of-current-form 'note 'number) 100)
  ))

(defun form-navigation-predicate ()
  (and (<= (attribute-of-current-form 'note 'number) 50)
  ))



