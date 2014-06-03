(in-package :gto)

(defun make-root (stack blinds raise three-bet four-bet utg-hands btn-hands)
  (declare (optimize speed)
           ((simple-array simple-string) utg-hands btn-hands)
           (double-float stack blinds raise three-bet four-bet)
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((lut (make-array (list (length utg-hands) (length btn-hands))
                         :element-type 'double-float)))
    (dotimes (v (length utg-hands))
      (dotimes (h (length btn-hands))
        (setf (aref lut v h)
              (coerce (the single-float (equity:value (aref utg-hands v)
                                                      (aref btn-hands h)))
                      'double-float))))
    (labels ((err (player name)
               (error "Don't have utility for ~S at ~A." player name))
             (4bet-fold-util (player pid oid)
               (declare (ignore pid oid))
               (case player
                 (cfr:villain (- stack four-bet))
                 (cfr:hero (+ stack four-bet blinds))
                 (t (err player "4-bet bluff"))))
             (4bet-call-util (player pid oid)
               (case player
                 (cfr:villain (* (aref lut pid oid) (+ blinds (* 2 stack))))
                 (cfr:hero (* (- 1 (aref lut oid pid)) (+ blinds (* 2 stack))))
                 (t (err player "4-bet call"))))
             (3bet-fold-util (player pid oid)
               (declare (ignore pid oid))
               (case player
                 (cfr:villain (+ stack blinds three-bet))
                 (cfr:hero (- stack three-bet))
                 (t (err player "3-bet bluff"))))
             (raise-fold-util (player pid oid)
               (declare (ignore pid oid))
               (case player
                 (cfr:villain (- stack raise))
                 (cfr:hero (+ stack blinds raise))
                 (t (err player "Open raise bluff"))))
             (flat-util (player pid oid)
               (let* ((pot (+ blinds (* 2 raise)))
                      (bet (/ (* 2 pot) 3)))
                 (case player
                   (cfr:villain (+ (- stack raise bet)
                                   (* (aref lut pid oid) (+ pot (* 2 bet)))))
                   (cfr:hero (+ (- stack raise bet)
                                (* (- 1 (aref lut oid pid)) (+ pot (* 2 bet)))))
                   (t (err player "Flat call")))))
             (fold-util (player pid oid)
               (declare (ignore pid oid))
               (case player
                 (cfr:villain (+ stack blinds))
                 (cfr:hero stack)
                 (t (err player "Fold")))))
      (let* ((4bet-fold (cfr:make-leaf "4-bet bluff" #'4bet-fold-util))
             (4bet-call (cfr:make-leaf  "4-bet call" #'4bet-call-util))
             (5bet (cfr:make-node "5-bet"
                                  'cfr:villain
                                  (length utg-hands)
                                  (vector 4bet-fold 4bet-call)))
             (3bet-fold (cfr:make-leaf "3-bet bluff" #'3bet-fold-util))
             (4bet (cfr:make-node "4-bet"
                                  'cfr:hero
                                  (length btn-hands)
                                  (vector 3bet-fold 5bet)))
             (raise-fold (cfr:make-leaf "Open raise bluff" #'raise-fold-util))
             (3bet (cfr:make-node "3-bet"
                                  'cfr:villain
                                  (length utg-hands)
                                  (vector raise-fold 4bet)))
             (flat (cfr:make-leaf "Flat call" #'flat-util))
             (fold (cfr:make-leaf "Fold" #'fold-util))
             (root (cfr:make-node "Root"
                                  'cfr:hero
                                  (length btn-hands)
                                  (vector fold flat 3bet))))
        root))))

(defun print-tree (root hutil vutil hhands vhands)
  (labels ((iter (node player hands)
             (unless (cfr:leafp node)
               (when (eq (cfr:active-player node) player)
                 (let ((avg (cfr:average-strategy node)))
                   (destructuring-bind (nstates nactions)
                       (array-dimensions avg)
                     (princ "Hand")
                     (dotimes (a nactions)
                       (format t " | ~A"
                               (cfr:name (aref (cfr:children node) a))))
                     (terpri)
                     (dotimes (s nstates)
                       (princ (aref hands s))
                       (dotimes (a nactions)
                         (format t " ~,4F" (aref avg s a)))
                       (terpri)))))
               (loop for c across (cfr:children node)
                     do (iter c player hands)))))
    (format t "UTG: ~,4F~%" vutil)
    (iter root 'cfr:villain vhands)
    (format t "~%BTN: ~,4F~%" hutil)
    (iter root 'cfr:hero hhands)))

(declaim (inline suit-combo))
(defun suit-combo (hand)
  (declare (optimize speed)
           (simple-string hand))
  (cond ((= (length hand) 2) 6)
        ((char= (char hand 2) #\s) 4)
        (t 12)))

(defun print-flat (root hutil vutil hhands vhands)
  (labels ((string-vector ()
             (make-array 0 :adjustable t
                           :fill-pointer t
                           :element-type 'simple-string))
           (leaf-names (n hnames vnames)
             (unless (cfr:leafp n)
               (loop with terminalp = T
                     for c across (cfr:children n)
                     do (if (cfr:leafp c)
                            (vector-push-extend
                             (cfr:name c)
                             (if (eq (cfr:active-player n) 'cfr:hero)
                                 hnames
                                 vnames))
                            (setf terminalp NIL))
                        (leaf-names c hnames vnames)
                     finally (when terminalp
                               (vector-push-extend (cfr:name n)
                                                   (if (eq (cfr:active-player n)
                                                           'cfr:hero)
                                                       vnames
                                                       hnames)))))
             (values hnames vnames))
           (iter (n id idx player p probs)
             (unless (cfr:leafp n)
               (let ((avg (when (eq (cfr:active-player n) player)
                            (cfr:average-strategy n)))
                     (terminalp T))
                 (dotimes (a (length (cfr:children n)))
                   (let ((c (aref (cfr:children n) a)))
                     (if (cfr:leafp c)
                         (when avg
                           (setf (aref probs id idx) (* p (aref avg id a)))
                           (incf idx))
                         (setf terminalp NIL))
                     (setf idx (iter c
                                     id
                                     idx
                                     player
                                     (if avg (* p (aref avg id a)) p)
                                     probs))))
                 (when (and terminalp (not avg))
                   (setf (aref probs id idx) p)
                   (incf idx))))
             idx)
           (prob-array (player hands names)
             (let ((probs (make-array (list (length hands) (length names))
                                      :element-type 'double-float)))
               (dotimes (id (length hands) probs)
                 (iter root id 0 player 1.0d0 probs))))
           (pr (player hands probs names util)
             (format t "~A: ~,4F~%" player util)
             (let ((records (make-array (length hands) :fill-pointer t))
                   (total 0))
               (dotimes (n (length names))
                 (setf (fill-pointer records) 0
                       total                  0)
                 (dotimes (id (length hands))
                   (when (>= (aref probs id n) 0.05)
                     (vector-push (cons (aref hands id) (aref probs id n))
                                  records)
                     (incf total (* (aref probs id n)
                                    (suit-combo (aref hands id))))))
                 (setf records (sort records #'> :key #'cdr))
                 (format t "~A range: ~,2F hand~:P~%" (aref names n) total)
                 (format t "Hand~CProb~%" #\tab)
                 (loop for (hand . prob) across records
                       do (format t "~A~C~,4F~%" hand #\tab prob))))))
    (multiple-value-bind (hnames vnames)
        (leaf-names root (string-vector) (string-vector))
      (let ((hprobs (prob-array 'cfr:hero hhands hnames))
            (vprobs (prob-array 'cfr:villain vhands vnames)))
        (pr "UTG" vhands vprobs vnames vutil)
        (terpri)
        (pr "BTN" hhands hprobs hnames hutil)))))

(defun range-to-array (range)
  (let ((hands (make-array (range:size range) :element-type 'simple-string))
        (i 0))
    (range:iterate (hand range)
      (setf (aref hands i) hand
            i              (1+ i)))
    hands))

(defun make-dealer (hhands vhands)
  (declare (optimize speed)
           ((simple-array simple-string) hhands vhands)
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (flet ((sample-id (hands)
           (declare ((simple-array simple-string) hands))
           (let ((weights (make-array (length hands)
                                      :element-type 'single-float))
                 (total 0.0))
             (dotimes (i (length hands))
               (incf (the single-float total) (suit-combo (aref hands i)))
               (setf (aref weights i) total))
             (when (plusp total)
               (dotimes (i (length weights))
                 (setf (aref weights i) (/ (aref weights i) total))))
             (lambda ()
               (loop with r = (random 1.0) and
                     min = 0 and
                     max = (1- (length weights))
                     while (<= min max)
                     do (let ((mid (ash (+ min max) -1)))
                          (cond ((< r (aref weights mid))
                                 (setf max (1- mid)))
                                ((< r (aref weights (1+ mid)))
                                 (return (1+ mid)))
                                (t (setf min (1+ mid)))))
                     finally (return min))))))
    (let ((hid (sample-id hhands))
          (vid (sample-id vhands))
          (lut (make-array (list (length hhands) (length vhands))
                           :element-type 'fixnum)))
      (dotimes (h (length hhands))
        (dotimes (v (length vhands))
          (setf (aref lut h v)
                (equity:matchup-combo (aref hhands h) (aref vhands v)))))
      (lambda ()
        (loop
          (let* ((h (funcall hid))
                 (v (funcall vid))
                 (m (aref lut h v))
                 (s (suit-combo (aref vhands v))))
            (when (or (= m s) (< (random s) m)) ; no conflict
              (return (values h v)))))))))

(defun train (&key (niter 1000000) (print-tree-p NIL) (verbosep T))
  (let* ((vhands (range-to-array
                  (range:new "77+,A7s+,K9s+,QTs+,JTs,ATo+,KTo+,QJo")))
         (hhands (range-to-array (range:fill (range:new))))
         (root (make-root 100d0 1.5d0 3d0 9d0 27d0 vhands hhands))
         (dealer (make-dealer hhands vhands))
         (hutil 0d0)
         (vutil 0d0))
    (dotimes (i niter)
      (multiple-value-bind (hid vid) (funcall dealer)
        (incf hutil (cfr:cfr root
                             'cfr:hero
                             1d0
                             1d0
                             hid
                             vid)))
      (multiple-value-bind (hid vid) (funcall dealer)
        (incf vutil (cfr:cfr root 'cfr:villain
                             1d0
                             1d0
                             vid
                             hid)))
      (when (and verbosep (zerop (mod i 100000)))
        (format t "~D UTG: ~,8F BTN: ~,8F~%"
                i
                (/ vutil (1+ i))
                (/ hutil (1+ i)))
        (force-output)))
    (let ((fn (if print-tree-p #'print-tree #'print-flat)))
     (funcall fn root (/ hutil niter) (/ vutil niter) hhands vhands))))
