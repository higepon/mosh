;;;; This is a Scheme library that support Finite State Automatons, both non-deterministic and deterministic.
;;;; The library includes a function that generates a deterministic automaton from a non-deterministic automaton.
;;;; Another central function is automaton-accepts? which accepts or rejects a list of input symbols in a DFA.
;;;; The generated automata are both compact and fast, because they make apply a rather efficient transition lookup in a sorted vector.
;;;; .title Reference Manual of the LAML Finite State Automaton library

; ---------------------------------------------------------------------------------------------------

;;; Automaton construction and selection.

;; Make a finite state automaton with a given start-state, a list of accept states, and a set of transitions - a list.
;; The optional parameter is a symbol map. In case it is passed, use this as symbol map for the resulting automaton,
;; and do not in this case compact the transitions. In normal use, do not pass any such symbol map. The function
;; make-finite-state-automaton will make the symbol map for you.
;; .form (make-finite-state-automaton start-state accept-state-list transitions [given-symbol-map])
;; .parameter start-state The start state of the automaton - an integer
;; .parameter accept-state-list The list of accepting states - a list of integers
;; .parameter transitions A list of transitions. Transitions are made by the function make-automaton-transition
;; .parameter given-symbol-map A symbol map - a sorted vector - ala an association list that maps real input symbols to terse symbols
;; .returns Returns a self-contained list structure that represents the finite state automaton.
(define (make-finite-state-automaton start-state accept-state-list transitions . optional-parameter-list)
 (let ((given-symbol-map (optional-parameter 1 optional-parameter-list #f)))
  (let* ((automaton-alphabet (alphabet-of-automaton-transitions transitions))
	 (symbol-map (if given-symbol-map given-symbol-map (make-automaton-symbol-map automaton-alphabet)))
        )
    (list
     'finite-state-automaton
     start-state
     accept-state-list
     (list->vector (sort-list (if given-symbol-map transitions (compacted-transitions transitions symbol-map)) transition-leq?))
     symbol-map))))

;; Return the start state of an automaton.
;; .form (start-state-of-finite-state-automaton aut)
(define start-state-of-finite-state-automaton (make-selector-function 2 "start-state-of-finite-state-automaton"))

;; Return the list of final states of an automaton.
;; .form (final-states-of-finite-state-automaton aut)
(define final-states-of-finite-state-automaton (make-selector-function 3 "final-states-of-finite-state-automaton"))

;; Return the transitions - a sorted vector - of an automaton.
;; .form (transitions-of-finite-state-automaton aut)
(define transitions-of-finite-state-automaton (make-selector-function 4 "transitions-of-finite-state-automaton"))

;; Return the transitions - as a list - of an automaton.
(define (transition-list-of-finite-state-automaton aut)
  (let ((trans-vec (transitions-of-finite-state-automaton aut)))
    (vector->list trans-vec)))

;; Return the symbol map of an automaton. The symbol is an internal device which maps real input symbol to
;; terse internal input symbols (for the sake of compact automaton representation).
;; .form (symbol-map-of-finite-state-automaton aut)
(define symbol-map-of-finite-state-automaton (make-selector-function 5 "symbol-map-of-finite-state-automaton"))

;;; State and transition predicates.

;; The equality used for states in a finite state automaton. Normally, the states are integers.
;; .form (state-equal state1 state2).
(define state-equal? =)

;; A leq? function on states. The function is used to normalize subset states in the subset construction.
;; .form (state-leq? state1 state2)
(define state-leq? <=)

;; A less than function on states.
;; .form (state-lt state1 state2)
(define state-lt? <)

;; A leq? function on transitions. The function is used for binary search among transitions for efficient lookup.
(define (transition-leq? trans1 trans2)
 (let ((from1 (from-state-of-transition trans1))
       (from2 (from-state-of-transition trans2)))
  (cond ((state-lt? from1 from2)
          #t)
        ((state-equal? from1 from2)
          (let ((sym1 (symbol-of-transition trans1))
                (sym2 (symbol-of-transition trans2)))
            (cond ((and (epsilon-symbol? sym1) (epsilon-symbol? sym2))
                     #t)
                  ((and (epsilon-symbol? sym1) (not (epsilon-symbol? sym2)))
                      #t)
                  ((and (symbol? sym1) (symbol? sym2))
                     (symbol-leq? sym1 sym2))
                  (else #f))))
        (else #f))))

;; The equality used for symbols in a finite state automaton. Normally finite state automaton symbols are Scheme symbols.
;; .form (symbol-equal? sym1 sym2).
(define symbol-equal? eq?)


; ---------------------------------------------------------------------------------------------------
;;; Transitions

;; Make a finite state transition
(define (make-automaton-transition in-state symbol out-state)
  (list in-state symbol out-state))

;; Return the from-state of the transition.
;; .form (from-state-of-transition trans)
(define from-state-of-transition (make-selector-function 1 "from-state-of-transition"))

;; Return the symbol of the transtion.
;; .form (symbol-of-transition trans)
(define symbol-of-transition (make-selector-function 2 "symbol-of-transition"))

;; Return the to-state of the transition.
;; .form (to-state-of-transition trans)
(define to-state-of-transition (make-selector-function 3 "to-state-of-transition"))

;; The denotation for the empty symbol.
(define epsilon-symbol #f)

;; Is the symbol s an epsilon symbol?
(define (epsilon-symbol? s) (and (boolean? s) (not s)))

;; Is the transition trans an epsilon transition?
(define (epsilon-transition? trans) (epsilon-symbol? (symbol-of-transition trans)))

; ---------------------------------------------------------------------------------------------------
; TRANSITION MOVING:

; The last tried automaton symbol input.
; Used for error message purposes.
(define last-automaton-input-symbol #f)

; The number of the input symbol just consumed
(define automaton-input-number 0)


; Return a single state S for which there is a move M in automaton for which
; M(from-state, symbol) = S. If more than one such state exist, just return one of them.
; If no such move is possible, return #f.
; This function is designed to work well for deterministic automata.
(define (deterministic-automaton-move automaton from-state symbol)
 (let* ((transitions (transitions-of-finite-state-automaton automaton))
        (symbol-map (symbol-map-of-finite-state-automaton automaton))
        (matching-transition (search-transitions transitions from-state (get-compact-automata-symbol symbol symbol-map unknown-symbol))))
  (set! last-automaton-input-symbol symbol)
  (set! automaton-input-number (+ 1 automaton-input-number))
  (if matching-transition
      (to-state-of-transition matching-transition)
      #f)))

; Run the automaton a far as possible, deterministically, starting in state
; with symbol-list as input. Return the final state, or #f if the automaton goes dead without
; consuming all input.
(define (deterministic-automaton-move* automaton state symbol-list)
  (if (null? symbol-list) 
      state
      (let ((next-state (deterministic-automaton-move automaton state (car symbol-list))))
        (if next-state
            (deterministic-automaton-move* automaton next-state (cdr symbol-list))
            #f))))

(define (trans-sel trans)
  (cons (from-state-of-transition trans) (symbol-of-transition trans)))

(define (trans-eq? cell1 cell2)
  (and (=   (car cell1) (car cell2)) ; in-states
       (eq? (cdr cell1) (cdr cell2)) ; symbols
  ))

(define (trans-leq? cell1 cell2)
  (cond ((< (car cell1) (car cell2))
          #t)
        ((= (car cell1) (car cell2))
          (symbol-leq? (cdr cell1) (cdr cell2)))
        (else #f)))

; Return a single transition among transitions that match from-state and symbol.
; If no such transition is found, return #f.
; transitions is a sorted vector of transitions
(define (search-transitions transitions from-state symbol)
  (let ((search-res (binary-search-in-vector transitions (cons from-state symbol) trans-sel trans-eq? trans-leq?)))
   (if search-res
       search-res         
       #f)))

; ---------------------------------------------------------------------------------------------------------------

;;; Acceptance predicate.

;; Does automaton accepts the input symbol-list. 
;; This predicate should only be used if automaton is a DFA. If a NFA, first use the function subset-construction to construct a DFA from it.
;; This function outputs either #t of #f.
(define (automaton-accepts? automaton symbol-list)
 (set! last-automaton-input-symbol #f)
 (set! automaton-input-number 0)
 (let ((end-state 
        (deterministic-automaton-move* 
         automaton (start-state-of-finite-state-automaton automaton) symbol-list)))
   (if end-state
       (turn-into-boolean (member-by-predicate end-state (final-states-of-finite-state-automaton automaton) state-equal?))
       #f)))

   

; ---------------------------------------------------------------------------------------------------

;;; DFA construction from NFA.
;;; Construction of deterministic finite automaton af non-deterministic finite state automaton.

;; Return an equivalent DFA from the non-deterministic NFA passed as first parameter.
;; .form (subset-construction nfa [support-epsilon-moves?])
;; .parameter nfa A non-deterministic finite state automaton
;; .parameter support-epsilon-moves? A boolean parameter, defaulted to #f, which makes it possible to carry out the construction with epsilon moves in nfa
(define (subset-construction nfa . optional-parameter-list)
 (let ((support-epsilon-moves? (optional-parameter 1 optional-parameter-list #f)))
  (letrec ((set-of-elements list))
    (let* ((input-symbols 
            (remove-duplicates-by-predicate
             (filter (lambda (s) (not (epsilon-symbol? s))) (map symbol-of-transition (transition-list-of-finite-state-automaton nfa))) symbol-equal?))
	   (dfa-start-state (if support-epsilon-moves?
                                (epsilon-closure-single-state (start-state-of-finite-state-automaton nfa) nfa)
                                (set-of-elements (start-state-of-finite-state-automaton nfa))
                            ))
	   (unmarked-dstates (set-of-elements dfa-start-state))
	   (dstates (set-of-elements dfa-start-state)) ; a set of states, each a set (represented as a list).
	   (dtrans (set-of-elements))
	   )
      (do ()
	  ((null? unmarked-dstates) (make-subset-dfa (reverse dstates) (reverse dtrans) nfa))
	(let ((first-unmarked-dstate (first unmarked-dstates)))
	  (set! unmarked-dstates (cdr unmarked-dstates))
	  (for-each 
           (lambda (input-symbol) 
             (let ((u (if support-epsilon-moves? 
                          (epsilon-closure-set (subset-move nfa first-unmarked-dstate input-symbol) nfa)
                          (subset-move nfa first-unmarked-dstate input-symbol))))
	       (if (not (null? u))
		   (begin
		     (if (not (member-by-predicate u dstates subset-state-equal?)) ; u is a set, dstates is a set of sets. 
			 (begin
			   (set! dstates (cons u dstates))
			   (set! unmarked-dstates (cons u unmarked-dstates))))
		     (let ((new-transition 
			    (make-automaton-transition first-unmarked-dstate input-symbol u)))
		       (set! dtrans (cons new-transition dtrans)))))))  
           input-symbols)))))))

; Given states (in terms of nfa subset states), and transition-list (in terms of transitions between subset states),
; Return a (deterministic) finite state automaton with new equivalent numbered states.
; The start state of the automaton is the new state corresponding to (first states).
; The final states of the automaton are those states (subsets) that holds a final state in the nfa - properly transformed.
(define (make-subset-dfa states transition-list nfa)
 (let* ((nfa-final-states (final-states-of-finite-state-automaton nfa))
        (number-of-states (length states))
        (new-states (number-interval 0 (- number-of-states 1)))
        (old-new-state-map (map (lambda (old-state new-state) (cons old-state new-state)) states new-states)))
  (letrec ((old-to-new 
            (lambda (old-state)
              (get-by-predicate old-state old-new-state-map subset-state-equal?))))
   (make-finite-state-automaton
     (first new-states)
     (map old-to-new (filter (lambda (old-state) (not (null? (intersection-by-predicate old-state nfa-final-states state-equal?))))  states))
     (map 
      (lambda (subset-transition) 
        (make-automaton-transition 
          (old-to-new (from-state-of-transition subset-transition))
          (symbol-of-transition subset-transition)
          (old-to-new (to-state-of-transition subset-transition))))
      transition-list)
    (symbol-map-of-finite-state-automaton nfa)
))))

; Return the set of states in the nfa reachable from nfa-state by epsilon moves.
; (Not really needed for our purposes, but useful in general.)
(define (epsilon-closure-single-state nfa-state nfa)
  (epsilon-closure-1 nfa-state nfa (list nfa-state)))

; Return the set of states in the nfa reachable from some nfa-state in nfa-state-set by epsilon moves.
; (Not really needed for our purposes, but useful in general.)
(define (epsilon-closure-set nfa-state-set nfa)
  (state-subset-normalize (flatten (map (lambda (nfa-state) (epsilon-closure-single-state nfa-state nfa)) nfa-state-set))))

(define (epsilon-closure-1 nfa-state nfa diregarded-end-states)
 (let* ((relevant-transitions 
         (filter 
          (lambda (trans) (and (state-equal? (from-state-of-transition trans) nfa-state) (epsilon-transition? trans))) 
          (transition-list-of-finite-state-automaton nfa)))
        (end-states (state-subset-normalize (map to-state-of-transition relevant-transitions)))
        (end-states-filtered (filter (lambda (dis-state) (not (member-by-predicate dis-state diregarded-end-states state-equal?))) end-states))
       )
  (state-subset-normalize
   (append 
     (list nfa-state)
     end-states-filtered
     (flatten (map (lambda (state) (epsilon-closure-1 state nfa (append diregarded-end-states end-states-filtered))) end-states-filtered)))
   )))
  

; Return the list of those elements in lst which are also found in lst2. 
; Membership is measured by member-by-predicate
(define (intersection-by-predicate lst1 lst2 pred)
  (cond ((null? lst1) '())
        ((member-by-predicate (car lst1) lst2 pred)
           (cons (car lst1) 
                 (intersection-by-predicate (cdr lst1) lst2 pred)))
        (else (intersection-by-predicate (cdr lst1) lst2 pred))))

(define (get-by-predicate key a-list pred)
 (let ((res (assq-by-predicate key a-list pred)))
   (if (pair? res) 
       (cdr res)
       (error (string-append "Get: Cannot find " (as-string key) " in " (as-string a-list))))))

(define (assq-by-predicate key a-list pred)
  (cond ((null? a-list) #f)
        ((pred key (caar a-list)) (car a-list))
        (else (assq-by-predicate key (cdr a-list) pred))))

; Aho, Sethi, Ullman page 118: move(T,a). nfa-state-set: T. symbol: a.
(define (subset-move nfa nfa-state-set symbol)
  (let* ((nfa-transitions (transition-list-of-finite-state-automaton nfa))
         (relevant-transitions
           (filter 
             (lambda (trans) 
               (and (member-by-predicate (from-state-of-transition trans) nfa-state-set state-equal?)
                    (symbol-equal? symbol (symbol-of-transition trans))))
             nfa-transitions))
        )
    (state-subset-normalize (map to-state-of-transition relevant-transitions))))

; Sort states and remove duplicates
(define (state-subset-normalize state-subset)
  (remove-duplicates-by-predicate 
      (sort-list state-subset state-leq?)
      state-equal?))

; state-set1 and state-set1 are two sets of states, each represented as a list
; Is state-set1 and state-set2 considered equal?
; Assume that both state sets are normalized, meaning that the states in the list are sorted by state-leq?
(define (subset-state-equal? state-set1 state-set2)
  (if (= (length state-set1) (length state-set2))
      (let ((eq-pairs (map (lambda (s1 s2) (state-equal? s1 s2))
                             state-set1 state-set2)))
         (accumulate-right and-fn #t eq-pairs))
      #f))

(define (and-fn x y) (and x y))

; ---------------------------------------------------------------------------------------------------
; Unique symbols

; Return a list of lgt unique symbols named systematically by the letters a .. z.
; Made via numbers in base 26 (the number of letters the English alphabet), using ciffers
; a..z, instead of 0..9a..z.
(define (make-unique-symbol-list lgt)
  (map (lambda (n) (as-symbol (special-number-in-base n 26))) (number-interval 1 lgt)))


(define (special-number-in-base n base)
 (if (= n 0) "a"
  (let ((ciffer-list (reverse (special-ciffers-in-base n base))))
     (special-ciffer-output ciffer-list))))

(define (special-ciffers-in-base n base)
  (if (= n 0)
      '()
      (let ((rem (modulo n base))
            (newn (quotient n base)))
        (cons rem (special-ciffers-in-base newn base)))))

(define (special-ciffer-output ciffer-list)
 (apply string-append
    (map special-ciffer-translation ciffer-list)))

(define (special-ciffer-translation c)
  (cond ((and (>= c 0) (< c 26)) (make-string 1 (integer->char (+ c 97))))
        (t "?")))

; A (presuably) never symbol never used in any automaton.
(define unknown-symbol (as-symbol (special-number-in-base 100000 26)))


; ---------------------------------------------------------------------------------------------------

; Return the list of input symbols, as deduced from the automata transitions.
(define (alphabet-of-automaton-transitions transition-list)
 (remove-duplicates-by-predicate
  (filter 
    (lambda (s) (not (epsilon-symbol? s)))
    (map symbol-of-transition transition-list)) symbol-equal?))

; Return the automaton symbol map, which maps a real input symbol to a very short input symbol.
; The symbol map is a sorted vector, suitable for binary search.
; Both input and output is Scheme symbols.
(define (make-automaton-symbol-map alphabet-symbols)
 (let ((unique-symbol-list (make-unique-symbol-list (length alphabet-symbols))))
  (list->vector
   (sort-list 
     (map (lambda (s us) (cons s us)) alphabet-symbols unique-symbol-list)
     (lambda (pair1 pair2) (string<=? (as-string (car pair1)) (as-string (car pair2))))))))

; Transform transitions (a list of transitions) such that the ordinary symbols are replaced by the compact symbols,
; which are the target symbols symbol-map
(define (compacted-transitions transitions symbol-map)
 (map 
   (lambda (trans) 
    (let ((fr (from-state-of-transition trans))
          (sy (symbol-of-transition trans))
          (to (to-state-of-transition trans)))
      (make-automaton-transition fr (get-compact-automata-symbol sy symbol-map) to)))
   transitions))

(define (symbol-leq? s1 s2)
 (string<=? (symbol->string s1) (symbol->string s2)))

(define problem-map #f)

(define (get-compact-automata-symbol sy symbol-map . optional-parameter-list)
 (let ((default-result (optional-parameter 1 optional-parameter-list #f)))
  (if (epsilon-symbol? sy)
      epsilon-symbol
      (let ((search-res (binary-search-in-vector symbol-map sy car eq? symbol-leq?)))
	(if search-res
	    (cdr search-res)
	    (if default-result
	        default-result
	        (laml-error "get-compact-automata-symbol: Cannot find value of symbol" sy "in" symbol-map)))))))





