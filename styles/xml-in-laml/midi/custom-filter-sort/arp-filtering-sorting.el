; ONE OF THE MAIN PARTS:
(defun arp-filter-fn (entry)
  (memq (arp-role entry) (list 'MA 'MB 'MC 'MD)))

; ONE OF THE MAIN ENDINGS:
(defun arp-filter-fn (entry)
  (memq (arp-role entry) (list 'FA 'FB 'FC)))

; AN ARP OF LENGTH 8:
(defun arp-filter-fn (entry)
  (= (arp-length entry) 8))

; AN ARP OF SOME PARTICULAR TIME SIGNATURE
(defun arp-filter-fn (entry)
  (let* ((a-time-sig-str (arp-time-sig entry))
         (a-time-sig-lst (parse-arp-time-signature a-time-sig-str)) ; to such as (4  4)
         (nom (car a-time-sig-lst))
         (denom (cadr a-time-sig-lst)))
    (= denom 8)))  

; A FAVORITE ARP:
(defun arp-filter-fn (entry)
  (let ((number (arp-number entry)))
    (arp-is-entry-in-favorites number)))


; AUX:
(defun parse-arp-time-signature (time-sig-str)
  (mapcar (function as-number) (split-string time-sig-str "/")))

