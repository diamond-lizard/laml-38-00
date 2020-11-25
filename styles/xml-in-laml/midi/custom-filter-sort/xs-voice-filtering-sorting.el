; GM Voice
(defun xs-voice-filter-fn (entry)
  (and (= (xs-voice-msb entry) 0) (= (xs-voice-lsb entry) 0)))



