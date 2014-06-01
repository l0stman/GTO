(in-package :range)

(utils:defconstant +ranks+ "23456789TJQKA")

(defun rankp (c)
  (integerp (position c +ranks+)))

(defstruct range
  (hands (make-hash-table :test 'equal) :type hash-table))

(defun memberp (range hand)
  "Return true if HAND is a member of RANGE."
  (gethash hand (range-hands range)))

(defun add (range hand)
  "Add HAND to RANGE."
  (setf (gethash hand (range-hands range)) T))

(defun remove (range hand)
  "Remove HAND from RANGE."
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
                   (and (rankp (char fmts pos)) (rankp (char fmts (1+ pos)))))
                 (minmax (a b)
                   (if (< a b)
                       (values a b)
                       (values b a))))
            (unless (handp start) (err))
            (case (- end start)
              (2
               (add range (format nil "~As" fmt))
               (add range (format nil "~Ao" fmt)))
              (3
               (case (char fmts (+ start 2))
                 ((#\s #\o) (add range fmt))
                 (#\+ (if (char= (char fmts start) (char fmts (1+ start)))
                          (loop for r from (position (char fmts start) +ranks+)
                                below (length +ranks+)
                                do (let ((c (char +ranks+ r)))
                                     (add range (format nil "~C~C" c c))))
                          (err)))
                 (t (err))))
              (4
               (unless (and (char= (char fmts (+ start 3)) #\+)
                            (or (char= (char fmts (+ start 2)) #\s)
                                (char= (char fmts (+ start 2)) #\o)))
                 (err))
               (multiple-value-bind (min max)
                   (minmax (position (char fmts start) +ranks+)
                           (position (char fmts (+ start 1)) +ranks+))
                 (loop for r from min below max
                       do (add range (format nil "~C~C~C"
                                             (char +ranks+ max)
                                             (char +ranks+ r)
                                             (char fmts (+ start 2)))))))
              (5
               (unless (and (handp (+ start 3))
                            (char= (char fmts (+ start 2)) #\-)
                            (char= (char fmts start) (char fmts (1+ start)))
                            (char= (char fmts (+ start 3))
                                   (char fmts (+ start 4))))
                 (err))
               (multiple-value-bind (min max)
                   (minmax (position (char fmts start) +ranks+)
                           (position (char fmts (+ start 3)) +ranks+))
                 (loop for r from min to max
                       do (let ((c (char +ranks+ r)))
                            (add range (format nil "~C~C" c c))))))
              (7
               (unless (and (handp (+ start 4))
                            (char= (char fmts (+ start 2))
                                   (char fmts (+ start 6)))
                            (or (char= (char fmts (+ start 2)) #\s)
                                (char= (char fmts (+ start 2)) #\o))
                            (char= (char fmts (+ start 3)) #\-))
                 (err))
               (multiple-value-bind (min1 max1)
                   (minmax (position (char fmts start) +ranks+)
                           (position (char fmts (1+ start)) +ranks+))
                 (multiple-value-bind (min2 max2)
                     (minmax (position (char fmts (+ start 4)) +ranks+)
                             (position (char fmts (+ start 5)) +ranks+))
                   (unless (= max1 max2) (err))
                   (multiple-value-bind (r1 r2) (minmax min1 min2)
                     (loop for r from r1 to r2
                           do (add range
                                   (format nil "~C~C~C"
                                           (char +ranks+ max1)
                                           (char +ranks+ r)
                                           (char fmts (+ start 2)))))))))
              (t (err))))
          (setf start (1+ end)))))
    range))
