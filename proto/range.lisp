(in-package :range)

(utils:defconstant +ranks+ "23456789TJQKA")

(defun rankp (c)
  (integerp (position c +ranks+)))

(defun handp (o)
  "Return true if O is a preflop representation of a hand."
  (and (stringp o)
       (or (and (= (length o) 2) (char= (char o 0) (char o 1)))
           (and (= (length o) 3)
                (or (char= (char o 2) #\s) (char= (char o 2) #\o))
                (char/= (char o 0) (char o 1))))
       (rankp (char o 0))
       (rankp (char o 1))))

(deftype hand () '(satisfies handp))

(defstruct range
  (hands (make-hash-table :test 'equal) :type hash-table))

(defun memberp (range hand)
  "Return true if HAND is a member of RANGE."
  (check-type range range)
  (gethash hand (range-hands range)))

(defun add (range hand)
  "Add HAND to RANGE."
  (check-type range range)
  (check-type hand hand)
  (setf (gethash hand (range-hands range)) T))

(defun remove (range hand)
  "Remove HAND from RANGE."
  (check-type range range)
  (check-type hand hand)
  (remhash hand (range-hands range)))

(defun fill (range)
  "Fill the range will all possible hands."
  (loop for c across +ranks+
        do (add range (format nil "~C~C" c c)))
  (dotimes (r1 (1- (length +ranks+)))
    (loop for r2 from (1+ r1) below (length +ranks+)
          do (add range
                  (format nil "~C~Cs" (char +ranks+ r2) (char +ranks+ r1)))
             (add range
                  (format nil "~C~Co" (char +ranks+ r2) (char +ranks+ r1)))))
  range)

(defun size (range)
  "Return the number of hands type in RANGE."
  (hash-table-count (range-hands range)))

(defmacro iterate ((hand range) &body body)
  "Iterate over all the hands in RANGE and execute BODY."
  (let ((memberp (gensym)))
    `(maphash #'(lambda (,hand ,memberp)
                  (declare (ignore ,memberp))
                  ,@body)
              (range-hands ,range))))

(defun new (&optional formats)
  "Return a new range build from FORMATS which is a string of formats
separated by ','.  Supported formats are:
AKo,AKs,AK,33+,22-77,A2o-A5o,A2s-A5s,A2s+,A2o+."
  (let ((fmts (remove-if #'(lambda (c)
                             (or (char= c #\tab)
                                 (char= c #\newline)
                                 (char= c #\space)
                                 (char= c #\linefeed)
                                 (char= c #\return)
                                 (char= c #\vt)))
                         formats))
        (range (make-range))
        (start 0))
    (dotimes (end (1+ (length fmts)))
      (when (and (or (= end (length fmts)) (char= (char fmts end) #\,))
                 (< start end))
        (let ((fmt (subseq fmts start end)))
          (flet ((err () (error "Unknown format ~A" fmt))
                 (handp (pos)
                   (and (rankp (char fmt pos)) (rankp (char fmt (1+ pos)))))
                 (minmax (a b)
                   (if (< a b)
                       (values a b)
                       (values b a))))
            (unless (handp 0) (err))
            (case (- end start)
              (2
               (cond ((char= (char fmt 0) (char fmt 1)) (add range fmt))
                     (t
                      (add range (format nil "~As" fmt))
                      (add range (format nil "~Ao" fmt)))))
              (3
               (case (char fmt 2)
                 ((#\s #\o)
                  (when (char= (char fmt 0) (char fmt 1))
                    (err))
                  (add range fmt))
                 (#\+
                  (if (char= (char fmt 0) (char fmt 1))
                      (loop for r from (position (char fmt 0) +ranks+)
                            below (length +ranks+)
                            do (let ((c (char +ranks+ r)))
                                 (add range (format nil "~C~C" c c))))
                      (err)))
                 (t (err))))
              (4
               (unless (and (char= (char fmt 3) #\+)
                            (or (char= (char fmt 2) #\s)
                                (char= (char fmt 2) #\o))
                            (char/= (char fmt 0) (char fmt 1)))
                 (err))
               (multiple-value-bind (min max)
                   (minmax (position (char fmt 0) +ranks+)
                           (position (char fmt 1) +ranks+))
                 (loop for r from min below max
                       do (add range (format nil "~C~C~C"
                                             (char +ranks+ max)
                                             (char +ranks+ r)
                                             (char fmt 2))))))
              (5
               (unless (and (handp 3)
                            (char= (char fmt 2) #\-)
                            (char= (char fmt 0) (char fmt 1))
                            (char= (char fmt 3) (char fmt 4)))
                 (err))
               (multiple-value-bind (min max)
                   (minmax (position (char fmt 0) +ranks+)
                           (position (char fmt 3) +ranks+))
                 (loop for r from min to max
                       do (let ((c (char +ranks+ r)))
                            (add range (format nil "~C~C" c c))))))
              (7
               (unless (and (handp 4)
                            (char= (char fmt 2) (char fmt 6))
                            (char/= (char fmt 0) (char fmt 1))
                            (char/= (char fmt 4) (char fmt 5))
                            (or (char= (char fmt 2) #\s)
                                (char= (char fmt 2) #\o))
                            (char= (char fmt 3) #\-))
                 (err))
               (multiple-value-bind (min1 max1)
                   (minmax (position (char fmt 0) +ranks+)
                           (position (char fmt 1) +ranks+))
                 (multiple-value-bind (min2 max2)
                     (minmax (position (char fmt 4) +ranks+)
                             (position (char fmt 5) +ranks+))
                   (unless (= max1 max2) (err))
                   (multiple-value-bind (r1 r2) (minmax min1 min2)
                     (loop for r from r1 to r2
                           do (add range
                                   (format nil "~C~C~C"
                                           (char +ranks+ max1)
                                           (char +ranks+ r)
                                           (char fmt 2))))))))
              (t (err))))
          (setf start (1+ end)))))
    range))
