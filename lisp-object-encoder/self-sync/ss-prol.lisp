
(in-package :prolog)

(defun int< (a b)
  (and (integerp a)
       (integerp b)
       (< a b)))

(progn
  (clear-db)

  #|
  (<- (trans ?A ?B ?C)
      (eqv ?C ?D)
      (trans ?A ?B ?D))
  |#
  #|
  (<- (eqv ?A ?B)
      (eqv ?A ?C)
      !
      (eqv ?C ?B))
  |#
  
  (<- (trans (state :start) ?b ?new-state)
      (trans (state :read-version) ?b ?new-state))
  
  
  ;; -------------------------------
  (<- (trans (state :read-version) #x01 (state :read-short-count))
      !)
  (<- (trans (state :read-version) ?x   ?new-state)
      (eqv (state :sync ?x) ?new-state))
  
  
  ;; -------------------------------
  (<- (eqv (state :sync #xFE) (state :check-start-fd))
      !)
  (<- (eqv (state :sync :EOF) (state :start))
      !)
  (<- (eqv (state :sync ?b)   (state :sync)))
  
  
  (<- (trans (state :sync) ?b ?new-state)
      (eqv (state :sync ?b) ?new-state))
  

  ;; -------------------------------
  (<- (trans (state :check-start-fd) #xFD (state :read-short-count))
      !)
  (<- (trans (state :check-start-fd) ?x   ?new-state)
      (eqv (state :sync ?x) ?new-state))


  ;; -------------------------------
  (<- (trans (state :read-short-count) #x00 (state :read-long-count))
      !)
  (<- (trans (state :read-short-count) ?b (state :read-frag ?b))
      (lt ?b 253 t)
      !)
  (<- (trans (state :read-short-count) ?b ?new-state)
      (eqv (state :sync ?b) ?new-state))


  ;; -------------------------------
  (<- (trans (state :read-long-count) ?b (state :read-long-count-2 ?b))
      (lt ?b 253 t)
      !)
  (<- (trans (state :read-long-count) ?b ?new-state)
      (eqv (state :sync ?b) ?new-state))

  ;; -------------------------------
  (<- (trans (state :read-long-count-2 ?b0) ?b1 (state :read-frag ?ct))
      (lt ?b1 253 t)
      !
      (lisp ?ct ((lambda (lo hi)
                   (+ lo (* hi 253)))
                 ?b0 ?b1)))
  (<- (trans (state :read-long-count-2 ?b0) ?b1 ?new-state)
      (eqv (state :sync ?b1) ?new-state))


  ;; -------------------------------
  (<- (trans (state :read-frag 0) ?b ?new-state)
      !
      (trans (state :read-long-count) ?b ?new-state))
  
  (<- (trans (state :read-frag ?ct) :EOF ?new-state)
      !
      (eqv (state :sync :EOF) ?new-state))

  (<- (trans (state :read-frag 1) ?b (state :read-long-count))
      !
      ;; stuff ?b
      )

  (<- (trans (state :read-frag ?ct) #xFE (state :check-frag-fd ?ct))
      !)

  (<- (trans (state :read-frag ?ct) ?b (state :read-frag ?ct-new))
      ;; stuff ?b
      (lisp ?x (- ?ct 1))
      (is ?ct-new ?x))


  ;; -------------------------------
  (<- (trans (state :check-frag-fd ?ct) #xFD ?new-state)
      !
      (eqv (state :sync #xFD) ?new-state))
  (<- (trans (state :check-frag-fd ?ct) ?b ?new-state)
      ;; stuff #xFE
      (is ?ctm1 (lisp (1- ?ct)))
      (trans (state :read-frag ?ctm1) ?b ?new-state))
      
  
  
  (<- (lt ?a ?b ?x)
      (lisp ?x (int< ?a ?b)))
  )

















(?- (eqv (state :start) ?x))
(?- (eqv (state :sync 'a) ?x))
(?- (trans (state :sync) #xfe ?x))
(?- (trans (state :read-short-count) #xFE ?x))
(?- (trans (state :read-short-count) #x00 ?x))

(?- (is ?x nil))

(?- (lisp (unless (< 3 2)
            (fail))))

(?- (lt 1 2 ?x))
(?- (lt 1 2 t))
