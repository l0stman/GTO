(in-package :cfr)

(deftype player ()
  "Represents the players in the game: HERO, VILLAIN or NOBODY."
  '(member hero villain nobody))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-double-array (dims &key (initial-element 0.0d0))
    (make-array dims
                :element-type 'double-float
                :initial-element initial-element)))

(utils:defconstant +empty-children+ (vector))
(utils:defconstant +empty-double-array+ (make-double-array 0))

(defstruct (node (:constructor %make-node) (:copier) (:conc-name))
  "Represents a node in the game-tree.
NAME            : Name of the node.
ACTIVE-PLAYER   : The player that should play at the given node.
CHILDREN        : Array containing the children of the node.
REGRET-SUM      : Array containing the sum of regrets of each action.
STRATEGY        : Array of probabilities of taking each action.
STRATEGY-SUM	: Array containing the sum of all the previous strategies.
UTILITY         : Utility function for a leaf node."
  (name (error "Need to supply a name.")
   :type simple-string
   :read-only t)
  (active-player (error "Need to supply an active player.")
   :type player
   :read-only t)
  (children +empty-children+
   :type (simple-array node)
   :read-only t)
  (utility NIL
   :type (or null (function (player integer integer) double-float))
   :read-only t)
  (regret-sum +empty-double-array+
   :type (simple-array double-float)
   :read-only t)
  (strategy +empty-double-array+
   :type (simple-array double-float)
   :read-only t)
  (strategy-sum +empty-double-array+
   :type (simple-array double-float)
   :read-only t))

(defun make-node (name active-player num-states children)
  "Create a new node with name NAME and an active player
ACTIVE-PLAYER.  CHILDREN is a non-empty array of nodes that are the
children of the current one. NUM-STATES is the number of states of the
current active player.  Each state for the active player is referred
to by an unique id between 0 and NUM-STATES - 1."
  (check-type name simple-string)
  (check-type active-player (member hero villain))
  (check-type num-states (integer 0))
  (check-type children (simple-array node))
  (when (zerop (length children))
    (error "CHILDREN should be a non-empty array of nodes."))
  (let ((dims (list num-states (length children))))
    (%make-node :name name
                :active-player active-player
                :children children
                :regret-sum (make-double-array dims)
                :strategy-sum (make-double-array dims)
                :strategy (make-double-array
                           dims :initial-element (/ 1.0d0 (length children))))))

(defun make-leaf (name utility)
  "Like `make-node', create a new leaf with name NAME. UTILITY is a
  function that takes three arguments -- PLAYER, PID, OID -- and
  returns the utility of PLAYER at this terminal node.  PID is the
  state id of PLAYER and OID the state id of his opponent."
  (check-type name simple-string)
  (check-type utility function)
  (%make-node :name name :active-player 'nobody :utility utility))

(defun leafp (n)
  "Return true if N is a leaf node."
  (and (typep n 'node) (eq (children n) +empty-children+)))

(defun average-strategy (node)
  "Return the average info set mixed strategy across all iterations.
The returned value is a two-dimensional array.  The first dimension
corresponds to the state id of the active player and the second one to
the action taken at the node."
  (check-type node node)
  (let ((strategy-sum (strategy-sum node)))
    (unless (eq strategy-sum +empty-double-array+)
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

(defun cfr (node player pprob oprob pid oid)
  "Return the utility of NODE for a given PLAYER using the
counterfactual regret minimization algorithm. PPROB and OPROB are the
reaching probabilities of the current node for PLAYER and his opponent
respectively. PID is the state id of PLAYER and OID the state id of
his opponent."
  (declare (optimize speed)
           (double-float pprob oprob)
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((children (children node))
        (strategy (strategy node))
        (regret-sum (regret-sum node))
        (strategy-sum (strategy-sum node)))
    (declare ((simple-array node) children)
             ((simple-array double-float) strategy regret-sum strategy-sum))
    (when (eq children +empty-children+)
      (return-from cfr (funcall (the function (utility node)) player pid oid)))
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
