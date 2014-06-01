(in-package :cfr)

(deftype player ()
  "Represents the players in the game: HERO, VILLAIN or NOBODY."
  '(member hero villain nobody))

(utils:defconstant +empty-array+ (vector))

(defclass node ()
  ((name
    :type simple-string
    :initarg :name
    :reader name
    :initform (error "Need to supply a name")
    :documentation "Name of the node.")
   (active-player
    :type player
    :initarg :active-player
    :reader active-player
    :initform (error "Need to supply an active player.")
    :documentation "The player that should play at the given node.")
   (children
    :type (simple-array node)
    :initarg :children
    :accessor children
    :initform +empty-array+
    :documentation "Array containing the children of the node.")
   (infoset
    :type T
    :initarg :infoset
    :reader infoset
    :documentation "Optional information set that can be added to the node.")
   (regret-sum
    :type (simple-array double-float)
    :reader regret-sum
    :documentation "Array containing the sum of regrets of each action.")
   (strategy
    :type (simple-array double-float)
    :reader strategy
    :documentation "Array of probabilities of taking each action.")
   (strategy-sum
    :type (simple-array double-float)
    :reader strategy-sum
    :documentation "Array of sum of all the previous strategies.")))

(defmethod initialize-instance :before ((n node) &key name
                                                      active-player
                                                      num-states
                                                      children)
  (unless (typep name 'simple-string)
    (error "NAME should be a string, not ~S" name))
  (unless (typep active-player 'player)
    (error "ACTIVE-PLAYER should be a PLAYER, not ~S" active-player))
  (unless (or (null children) (typep children '(simple-array node)))
    (error "CHILDREN should an array of node, not ~S" children))
  (unless (or (null num-states) (typep num-states '(integer 0)))
    (error "NUM-STATES should be a non-negative integer, not ~S"
           num-states)))

(defmethod initialize-instance :after ((n node)
                                       &key num-states
                                       &aux (len (length (children n))))
  (with-slots (regret-sum strategy-sum strategy) n
    (if (plusp len)
        (let ((dims (list num-states len)))
          (setf regret-sum   (make-double-array dims)
                strategy-sum (make-double-array dims)
                strategy     (make-double-array
                              dims :initial-element (/ 1.0d0 len))))
        (let ((arr (make-double-array 0)))
          (setf regret-sum   arr
                strategy-sum arr
                strategy     arr)))))

(defun make-node (name &optional (active-player 'nobody)
                                 (num-states 0)
                                 (children +empty-array+)
                                 (infoset NIL))
  "Create a new node with name NAME and an active player
ACTIVE-PLAYER.  CHILDREN is an array of nodes that are the children of
the current one. NUM-STATES is the number of states of the current
active player.  Each state for the active player is referred to by an
unique id between 0 and NUM-OPP-STATES - 1. INFOSET is an optional
information set to be stored at the node."
  (make-instance 'node
                 :name name
                 :active-player active-player
                 :children children
                 :num-states num-states
                 :infoset infoset))

(defun make-double-array (dims &key (initial-element 0.0d0))
  (make-array dims
              :element-type 'double-float
              :initial-element initial-element))

(defun leafp (n)
  "Return true if N is a leaf node."
  (and (typep n 'node) (eq (children n) +empty-array+)))

(defun average-strategy (node)
  "Return the average info set mixed strategy across all iterations.
The returned value is a two-dimensional array.  The first dimension
corresponds to the state id of the active player and the second one to
the action taken at the node."
  (declare (node node))
  (with-slots (strategy-sum) node
    (unless (eq strategy-sum +empty-array+)
      (let* ((dims (array-dimensions strategy-sum))
             (avg (make-double-array dims)))
        (destructuring-bind (nstates nactions) dims
          (dotimes (s nstates)
            (dotimes (a nactions)
              (let ((norm (loop for a below nactions sum
                                   (aref strategy-sum s a))))
                (setf (aref avg s a)
                      (if (plusp norm)
                          (/ (aref strategy-sum s a) norm)
                          (/ 1.0d0 nactions)))))))
        avg))))

(defgeneric utility (node player pid oid)
  (:documentation
   "Return the utility of PLAYER on a given leaf NODE. PID is the
   state id of the active player and OID the state id of his
   opponent."))

(defun cfr (node player pprob oprob pid oid)
  "Return the utility of NODE for a given PLAYER using the
counterfactual regret minimization algorithm. PPROB and OPROB are the
reaching probabilities of the current node for PLAYER and his opponent
respectively. PID is the state id of PLAYER and OID the state id of
his opponent."
  (declare (optimize speed)
           (double-float pprob oprob)
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (with-slots (children strategy regret-sum strategy-sum) node
    (declare ((simple-array node) children)
             ((simple-array double-float) strategy regret-sum strategy-sum))
    (when (eq children +empty-array+)
      (return-from cfr (utility node player pid oid)))
    (let ((utils (the (simple-array double-float)
                      (make-double-array (length children))))
          (id (if (eq (active-player node) player) pid oid))
          (util 0.0d0))
      (dotimes (a (length children))
        (setf (aref utils a)
              (if (eq (active-player node) player)
                  (cfr (aref children a)
                       player
                       (* (aref strategy id a) pprob)
                       oprob
                       pid
                       oid)
                  (cfr (aref children a)
                       player
                       pprob
                       (* (aref strategy id a) oprob)
                       pid
                       oid)))
        (incf util (* (aref strategy id a) (aref utils a))))
      (when (eq (active-player node) player)
        (let ((norm 0.0d0))
          (dotimes (a (length children))
            (incf (aref regret-sum pid a)
                  (* oprob (- (aref utils a) util)))
            (incf (aref strategy-sum pid a)
                  (* pprob (aref strategy pid a))))
          (dotimes (a (length children))
            (setf (aref strategy pid a)
                  (if (plusp (aref regret-sum pid a))
                      (aref regret-sum pid a)
                      0.0d0))
            (incf norm (aref strategy pid a)))
          (dotimes (a (length children))
            (setf (aref strategy pid a)
                  (if (plusp norm)
                      (/ (aref strategy pid a) norm)
                      (/ 1.0d0 (length children)))))))
      util)))
