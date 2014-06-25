(in-package :cl-user)

(defpackage #:utils
  (:use :cl)
  (:shadow :defconstant)
  (:export #:defconstant
           #:when-bind
           #:aif))

(defpackage #:range
  (:use :cl)
  (:shadow :remove :fill)
  (:export #:memberp
           #:add
           #:remove
           #:iterate
           #:new
           #:fill
           #:size
           #:handp))

(defpackage #:equity
  (:use :cl)
  (:export #:value
           #:matchup-combo))

(defpackage #:cfr
  (:use :cl)
  (:export #:player
           #:hero
           #:villain
           #:nobody
           #:node
           #:average-strategy
           #:utility
           #:cfr
           #:make-node
           #:make-leaf
           #:name
           #:children
           #:leafp
           #:infoset
           #:active-player))

(defpackage #:gto
  (:use :cl)
  (:export #:train))

