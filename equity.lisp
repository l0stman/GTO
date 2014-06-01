(in-package :equity)

(utils:defconstant +preflop-combos-file+
  (asdf/system:system-relative-pathname 'gto "preflop-combos.txt"))

(utils:defconstant +preflop-equity-file+
  (asdf/system:system-relative-pathname 'gto "preflop-matchups.txt"))

(defun put (dist hero vill val)
  (let ((table (gethash hero dist)))
    (unless table
      (setf table               (make-hash-table :test 'equal)
            (gethash hero dist) table))
    (setf (gethash vill table) val)))

(defun parse-float (str &key (start 0))
  "Return a float read from STR, and the index to the remainder of STR."
  (multiple-value-bind (integer i)
      (parse-integer str :start start :junk-allowed t)
    (multiple-value-bind (fraction j)
        (parse-integer str :start (1+ i) :junk-allowed t)
      (values (float (+ integer (/ fraction (expt 10 (- j i 1)))))))))

(defvar *equity-table*
  (let ((table (make-hash-table :test 'equal)))
    (with-open-file (in +preflop-equity-file+ :direction :input)
      (loop
        (let ((line (read-line in NIL)))
          (unless line (return))
          (let* ((end1 (position #\space line))
                 (start2 (position-if-not #'(lambda (c)
                                              (or (char= c #\space)
                                                  (char= c #\v)
                                                  (char= c #\s)
                                                  (char= c #\.)))
                                          line
                                          :start end1))
                 (end2 (position #\space line :start start2))
                 (start3 (position-if-not #'(lambda (c)
                                              (or (char= c #\space)
                                                  (char= c #\:)))
                                          line
                                          :start end2)))
            (put table
                 (subseq line 0 end1)
                 (subseq line start2 end2)
                 (parse-float line :start start3))))))
    table))

(defvar *combos-table*
  (let ((table (make-hash-table :test 'equal)))
    (with-open-file (in +preflop-combos-file+ :direction :input)
      (loop
        (let ((line (read-line in NIL)))
          (unless line (return))
          (let* ((end1 (position #\tab line))
                 (end2 (position #\tab line :start (1+ end1))))
            (put table
                 (subseq line 0 end1)
                 (subseq line (1+ end1) end2)
                 (parse-integer line :start (1+ end2)))))))
    table))



(defun value (hero vill)
  "Return the equity HERO's hand against VILL's."
  (flet ((get-val (h v)
           (utils:when-bind (table (gethash h *equity-table*))
             (gethash v table))))
    (if (string= hero vill)
        0.5
        (or (get-val hero vill)
            (utils:when-bind (val (get-val vill hero))
              (- 1 val))))))

(defun matchup-combo (hero vill)
  "Return the number of possible match-ups for a preflop hand
  represented as HERO against all the hands represented as VILL if
  suits were considered."
  (utils:aif (gethash hero *combos-table*)
             (or (gethash vill it)
                 (error "Unknown hand ~A" vill))
             (error "Unknown hand ~A" hero)))
