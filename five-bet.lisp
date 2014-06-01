(in-package :gto)

(defstruct (infoset (:copier NIL))
  "Information set for a given node."
  (stack 0.0d0 :type double-float :read-only t)
  (blinds 0.0d0 :type double-float :read-only t)
  (raise 0.0d0 :type double-float :read-only t)
  (three-bet 0.0d0 :type double-float :read-only t)
  (four-bet 0.0d0 :type double-float :read-only t)
  (utg-hands #() :type (simple-array simple-string) :read-only t)
  (btn-hands #() :type (simple-array simple-string) :read-only t))

(defclass root (cfr:node) ())
(defclass utg-open-raise (cfr:node) ())
(defclass btn-fold (cfr:node) ())
(defclass btn-flat-call (cfr:node) ())
(defclass btn-3bet (cfr:node) ())
(defclass utg-raise-fold (cfr:node) ())
(defclass utg-4bet (cfr:node) ())
(defclass btn-3bet-fold (cfr:node) ())
(defclass btn-5bet (cfr:node) ())
(defclass utg-4bet-fold (cfr:node) ())
(defclass utg-4bet-call (cfr:node) ())

(defun make-root (stack
                  blinds
                  raise
                  three-bet
                  four-bet
                  utg-hands
                  btn-hands)
  (labels ((check-floats (args)
             (unless (null args)
               (destructuring-bind (name obj &rest rest) args
                 (unless (or (null obj)
                             (and (typep obj 'double-float) (plusp obj)))
                   (error "~A should be a positive double-float, not ~S."
                          name obj))
                 (check-floats rest))))
           (make-leaf (class name infoset)
             (make-instance class
                            :name name
                            :active-player 'cfr:nobody
                            :infoset infoset))
           (make-node (class name player num-states children)
             (make-instance class
                            :name name
                            :active-player player
                            :num-states num-states
                            :children children)))
    (check-floats (list "STACK" stack
                        "FOUR-BET" four-bet
                        "BLINDS" blinds
                        "THREE-BET" three-bet
                        "FOUR-BET" four-bet
                        "RAISE" raise))
    (let* ((info (make-infoset :stack stack
                               :raise raise
                               :three-bet three-bet
                               :four-bet four-bet
                               :utg-hands utg-hands
                               :btn-hands btn-hands))
           (4bet-fold (make-leaf 'utg-4bet-fold "4-bet bluff" info))
           (4bet-call (make-leaf 'utg-4bet-call "4-bet call"  info))
           (5bet (make-node 'btn-5bet
                            "5-bet"
                            'cfr:villain
                            (length utg-hands)
                            (vector 4bet-fold 4bet-call)))
           (3bet-fold (make-leaf 'btn-3bet-fold "3-bet bluff" info))
           (4bet (make-node 'utg-4bet
                            "4-bet"
                            'cfr:hero
                            (length btn-hands)
                            (vector 3bet-fold 5bet)))
           (raise-fold (make-leaf 'utg-raise-fold "Open raise bluff" info))
           (3bet (make-node 'btn-3bet
                            "3-bet"
                            'cfr:villain
                            (length utg-hands)
                            (vector raise-fold 4bet)))
           (flat (make-leaf 'btn-flat-call "Flat call" info))
           (fold (make-leaf 'btn-fold "Fold" info))
           (root (make-node 'root
                            "Root"
                            'cfr:hero
                            (length btn-hands)
                            (vector fold flat 3bet))))
      root)))

(defmethod cfr:utility ((n btn-fold) player pid oid)
  (declare (ignore pid oid))
  (with-slots ((i cfr:infoset) (name cfr:name)) n
    (case player
      (cfr:villain (+ (infoset-stack i) (infoset-blinds i)))
      (cfr:hero (infoset-stack i))
      (t (error "Don't have utility for ~S at ~A." player name)))))

(defmethod cfr:utility ((n btn-flat-call) player pid oid)
  (with-slots ((i cfr:infoset) (name cfr:name)) n
    (let* ((pot (+ (infoset-blinds i) (* 2 (infoset-raise i))))
           (bet (/ (* 2 pot) 3)))
      (case player
        (cfr:villain (+ (- (infoset-stack i) (infoset-raise i) bet)
                        (* (equity:value (aref (infoset-utg-hands i) pid)
                                         (aref (infoset-btn-hands i) oid))
                           (+ pot (* 2 bet)))))
        (cfr:hero (+ (- (infoset-stack i) (infoset-raise i) bet)
                     (* (equity:value (aref (infoset-btn-hands i) pid)
                                      (aref (infoset-utg-hands i) oid))
                        (+ pot (* 2 bet)))))
        (t (error "Don't have utility for ~S at ~A." player name))))))

(defmethod cfr:utility ((n utg-raise-fold) player pid oid)
  (declare (ignore pid oid))
  (with-slots ((i cfr:infoset) (name cfr:name)) n
    (case player
      (cfr:villain (- (infoset-stack i) (infoset-raise i)))
      (cfr:hero (+ (infoset-stack i) (infoset-blinds i) (infoset-raise i)))
      (t (error "Don't have utility for ~S at ~A." player name)))))

(defmethod cfr:utility ((n btn-3bet-fold) player pid oid)
  (declare (ignore pid oid))
  (with-slots ((i cfr:infoset) (name cfr:name)) n
    (case player
      (cfr:villain (+ (infoset-stack i)
                      (infoset-blinds i)
                      (infoset-three-bet i)))
      (cfr:hero (- (infoset-stack i) (infoset-three-bet i)))
      (t (error "Don't have utility for ~S at ~A." player name)))))

(defmethod cfr:utility ((n utg-4bet-fold) player pid oid)
  (declare (ignore pid oid))
  (with-slots ((i cfr:infoset) (name cfr:name)) n
    (case player
      (cfr:villain (- (infoset-stack i) (infoset-four-bet i)))
      (cfr:hero (+ (infoset-stack i) (infoset-four-bet i) (infoset-blinds i)))
      (t (error "Don't have utility for ~S at ~A." player name)))))

(defmethod cfr:utility ((n utg-4bet-call) player pid oid)
  (with-slots ((i cfr:infoset) (name cfr:name)) n
    (case player
      (cfr:villain (* (equity:value (aref (infoset-utg-hands i) pid)
                                    (aref (infoset-btn-hands i) oid))
                      (+ (infoset-blinds i) (* 2 (infoset-stack i)))))
      (cfr:hero (* (equity:value (aref (infoset-btn-hands i) pid)
                                 (aref (infoset-utg-hands i) oid))
                   (+ (infoset-blinds i) (* 2 (infoset-stack i)))))
      (t (error "Don't have utility for ~S at ~A." player name)))))

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

(defun suit-combo (hand)
  (cond ((= (length hand) 2) 6)
        ((char= (char hand 2) #\s) 4)
        (t 12)))

(defun sample (weights)
  (let ((r (random 1.0))
        (min 0)
        (max (1- (length weights))))
    (loop while (<= min max)
          do (let ((mid (ash (+ min max) -1)))
               (cond ((< r (aref weights mid))
                      (setf max (1- mid)))
                     ((< r (aref weights (1+ mid)))
                      (return-from sample (1+ mid)))
                     (t (setf min (1+ mid))))))
    min))

(defun sample-id (hands)
  (let ((weights (make-array (length hands) :element-type 'single-float))
        (total 0.0))
    (dotimes (i (length hands))
      (incf total (suit-combo (aref hands 1)))
      (setf (aref weights i) total))
    (when (plusp total)
      (dotimes (i (length weights))
        (setf (aref weights i) (/ (aref weights i) total))))
    (lambda () (sample weights))))

(defun make-dealer (hhands vhands)
  (let ((hid (sample-id hhands))
        (vid (sample-id vhands)))
    (lambda ()
      (loop
        (let* ((h (funcall hid))
               (v (funcall vid))
               (m (equity:matchup-combo (aref hhands h) (aref vhands v)))
               (s (suit-combo (aref vhands v))))
          (when (or (= m s) (< (random s) m)) ; no conflict
            (return (values h v))))))))

(defun train (&key (niter 1000000) (print-tree-p NIL) (verbosep T))
  (let* ((vhands (range-to-array
                  (range:new "77+,A7s+,K9s+,QTs+,JTs,ATo+,KTo+,QJo")))
         (hhands (range-to-array (range:fill (range:new))))
         (root (make-root 75d0 1.5d0 3d0 9d0 27d0 vhands hhands))
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
