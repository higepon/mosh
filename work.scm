;(call-with-values (lambda () (let1 x (and (display "before[x]")(g$70$13650 '() g$70$22851 g$70$22852)) (display "***")(display x) (display "***>>" )x)) (lambda (g$70$22855 g$70$22856 g$70$22857 g$70$22858 g$70$22859 g$70$22860 g$70$22861) (begin (display '"<after>") (values g$70$22856 g$70$22858))))
(define (x)
  (values 1 2 3 4))
(print (call-with-values (lambda () (x)) (lambda (a b c d) (values b c))))


;(print (apply symbol? '(3)))

;; ; mosh only
;; (define (assertion-violation a b c)
;;   (raise (format "~a ~a ~a" a b c)))
;; ; mosh only end


;;   (define make-field-spec cons)

;;   (define field-spec-mutable? car)
;;   (define field-spec-name cdr)

;;   (define (field-spec=? spec-1 spec-2)
;;     (and (eq? (field-spec-mutable? spec-1)
;;           (field-spec-mutable? spec-2))
;;      (eq? (field-spec-name spec-1)
;;           (field-spec-name spec-2))))

;;   (define :record-type-data (make-vector-type 'record-type-descriptor
;;                           #f ; no supertype
;;                           #f ; no per-type data
;;                           '(#f #f #f #f #f #f) ; all fields immutable
;;                           #t)) ; opaque

;;   (define (make-record-type-data name uid sealed? opaque? field-specs parent)
;;     ((typed-vector-constructor :record-type-data)
;;      name uid sealed? opaque? field-specs parent))

;;   (define (record-type-data? thing)
;;     ((vector-type-predicate :record-type-data) thing))

;;   (define (typed-vector-ref t v pos)
;;     ((typed-vector-accessor t pos) v))

;;   (define (record-type-name rtd)
;;     (typed-vector-ref :record-type-data (vector-type-data rtd) 0))
;;   (define (record-type-uid rtd)
;;     ;; this is #f in the generative case
;;     (typed-vector-ref :record-type-data (vector-type-data rtd) 1))
;;   (define (record-type-sealed? rtd)
;;     (typed-vector-ref :record-type-data (vector-type-data rtd) 2))
;;   (define (record-type-opaque? rtd)
;;     (typed-vector-ref :record-type-data (vector-type-data rtd) 3))
;;   (define (record-type-field-specs rtd)
;;     (typed-vector-ref :record-type-data (vector-type-data rtd) 4))
;;   (define (record-type-parent rtd)
;;     (typed-vector-ref :record-type-data (vector-type-data rtd) 5))

;;   (define (record-type-descriptor=? rtd-1 rtd-2)
;;     (and (eq? (record-type-parent rtd-1) (record-type-parent rtd-2))
;;      (eq? (record-type-uid rtd-1) (record-type-uid rtd-2))
;;      (for-all field-spec=?
;;           (record-type-field-specs rtd-1)
;;           (record-type-field-specs rtd-2))))

;;   (define (uid->record-type-descriptor uid)
;;     (find (lambda (rtd)
;;         (eq? (record-type-uid rtd) uid))
;;       *nongenerative-record-types*))

;;   (define (record-type-generative? rtd)
;;     (not (record-type-uid rtd)))

;;   (define *nongenerative-record-types* '())

;;   (define (append-field-mutable-specs parent)
;;     (if parent
;;     (append (append-field-mutable-specs (record-type-parent parent))
;;         (map field-spec-mutable? (record-type-field-specs parent)))
;;     '()))

;;   (define (make-record-type-descriptor name parent uid sealed? opaque? field-specs)
;;     (if (not (symbol? name))
;;         (assertion-violation 'make-record-type-descriptor "not a symbol for record type name" name))
;;     (if (not (or (not parent)
;;                  (record-type-descriptor? parent)))
;;     (assertion-violation 'make-record-type-descriptor "parent not #f or a record type descriptor" parent))
;;     (if (not (or (not uid)
;;                  (symbol? uid)))
;;     (assertion-violation 'make-record-type-descriptor "uid must be #f or a symbol" parent))
;;     (if (and parent
;;          (record-type-sealed? parent))
;;     (assertion-violation 'make-record-type-descriptor "can't extend a sealed parent type" parent))
;;     (if (not (list? field-specs))
;;         (assertion-violation 'make-record-type-descriptor "field specification must be a list" field-specs))
;;     (let ((opaque? (if parent
;;                (or (record-type-opaque? parent)
;;                opaque?)
;;                opaque?))
;;       (field-specs (map parse-field-spec field-specs)))
;;       (let ((rtd
;;          (make-vector-type name
;;                    parent
;;                    (make-record-type-data name uid (and sealed? #t) (and opaque? #t) field-specs parent)
;;                    (append (append-field-mutable-specs parent)
;;                        (map field-spec-mutable? field-specs))
;;                    opaque?)))
;;     (if uid
;;         (cond
;;          ((uid->record-type-descriptor uid)
;;           => (lambda (old-rtd)
;;            (if (record-type-descriptor=? rtd old-rtd)
;;                old-rtd
;;                (assertion-violation 'make-record-type
;;                                             "mismatched nongenerative record types with identical uids"
;;                                             old-rtd rtd))))
;;          (else
;;           (set! *nongenerative-record-types*
;;             (cons rtd *nongenerative-record-types*))
;;           rtd))
;;         rtd))))

;;   (define (record-type-descriptor? thing)
;;     (and (vector-type? thing)
;;      (record-type-data? (vector-type-data thing))))

;;   (define (ensure-rtd thing)
;;     (if (not (record-type-descriptor? thing))
;;     (assertion-violation 'make-record-type "not a record-type descriptor" thing)))

;;   (define (parse-field-spec spec)
;;     (if (not (and (list? spec)
;;                   (= 2 (length spec))))
;;         (assertion-violation 'make-record-type
;;                              "field spec list element is not a list of two elements"
;;                              spec))
;;     (apply (lambda (mutability name)
;;              (if (not (symbol? name))
;;                  (assertion-violation 'make-record-type
;;                                       "field spec name is not a symbol"
;;                                       name))
;;          (make-field-spec
;;           (case mutability
;;         ((mutable) #t)
;;         ((immutable) #f)
;;         (else (assertion-violation 'make-record-type
;;                                            "field spec with invalid mutability specification" spec)))
;;           name))
;;        spec))

;;   (define (record-type-field-names rtd)
;;     (map field-spec-name (record-type-field-specs rtd)))

;;   (define (field-count rtd)
;;     (let loop ((rtd rtd)
;;            (count 0))
;;       (if (not rtd)
;;       count
;;       (loop (record-type-parent rtd)
;;         (+ count (length (record-type-field-specs rtd)))))))

;;   (define (record? thing)
;;     (and (typed-vector? thing)
;;      (let ((rtd (typed-vector-type thing)))
;;        (and (record-type-descriptor? rtd)
;;         (not (record-type-opaque? rtd))))))

;;   (define (record-rtd rec)
;;     (if (record? rec)
;;     (typed-vector-type rec)
;;     (assertion-violation 'record-rtd "cannot extract rtd of a non-record or opaque record" rec)))

;;   ;; Constructing constructors

;;   (define :record-constructor-descriptor (make-vector-type 'record-constructor-descriptor #f #f '(#f #f #f #f) #t))

;;   (define record-type-constrctor-descriptor?
;;     (vector-type-predicate :record-constructor-descriptor))

;;   (define (make-record-constructor-descriptor rtd previous protocol)
;;     (if (not (record-type-descriptor? rtd))
;;         (assertion-violation 'make-record-constructor-descriptor
;;                              "not a record type descriptor" rtd))
;;     (if (not (or (not previous)
;;                  (record-type-constrctor-descriptor? previous)))
;;         (assertion-violation 'make-record-constructor-descriptor
;;                              "not #f or a parent record type constructor descriptor" previous))
;;     (if (not (or (not protocol)
;;                  (procedure? protocol)))
;;         (assertion-violation 'make-record-constructor-descriptor
;;                              "not #f or procedure for protocol" protocol))
;;     (let ((parent (record-type-parent rtd)))
;;       (if (and previous (not parent))
;;       (assertion-violation 'make-record-constructor-descriptor
;;                                "mismatch between rtd and constructor descriptor" rtd previous))
;;       (if (and protocol parent (not previous))
;;           (assertion-violation 'make-record-constructor-descriptor
;;                                "non-default protocol requested, but no parent constrcutor descriptor given" rtd previous))
;;       (if (and previous
;;            (not protocol)
;;            (record-constructor-descriptor-custom-protocol? previous))
;;       (assertion-violation 'make-record-constructor-descriptor
;;                                "default protocol requested when parent constructor descriptor has custom one"
;;                                protocol previous))

;;       (let ((custom-protocol? (and protocol #t))
;;         (protocol (or protocol (default-protocol rtd)))
;;         (previous
;;          (if (or previous
;;              (not parent))
;;          previous
;;          (make-record-constructor-descriptor parent #f #f))))

;;     ((typed-vector-constructor :record-constructor-descriptor)
;;      rtd protocol custom-protocol? previous))))

;;   (define (split-at l n)
;;     (if (zero? n)
;;     (values '() l)
;;     (let-values (((a b) (split-at (cdr l) (- n 1))))
;;       (values (cons (car l) a) b))))

;;   (define (default-protocol rtd)
;;     (let ((parent (record-type-parent rtd)))
;;       (if (not parent)
;;       (lambda (p)
;;         (lambda field-values
;;           (apply p field-values)))
;;       (let ((parent-field-count (field-count parent)))
;;         (lambda (p)
;;           (lambda all-field-values
;;         (call-with-values
;;             (lambda () (split-at all-field-values parent-field-count))
;;           (lambda (parent-field-values this-field-values)
;;             (apply (apply p parent-field-values) this-field-values)))))))))

;;   (define (record-constructor-descriptor-rtd desc)
;;     (typed-vector-ref :record-constructor-descriptor desc 0))
;;   (define (record-constructor-descriptor-protocol desc)
;;     (typed-vector-ref :record-constructor-descriptor desc 1))
;;   ;; this field is for error checking
;;   (define (record-constructor-descriptor-custom-protocol? desc)
;;     (typed-vector-ref :record-constructor-descriptor desc 2))
;;   (define (record-constructor-descriptor-previous desc)
;;     (typed-vector-ref :record-constructor-descriptor desc 3))

;;   ;; A "seeder" is the procedure passed to the cons conser, used to seed
;;   ;; the initial field values.

;;   (define (make-make-seeder real-rtd wrap for-desc)
;;     (let recur ((for-desc for-desc))
;;       (let* ((for-rtd (record-constructor-descriptor-rtd for-desc))
;;          (for-rtd-field-count (length (record-type-field-specs for-rtd))))
;;     (cond
;;      ((record-constructor-descriptor-previous for-desc)
;;       => (lambda (parent-desc)
;;            (let ((parent-protocol (record-constructor-descriptor-protocol parent-desc))
;;              (parent-make-seeder (recur parent-desc)))
;;          (lambda extension-field-values
;;            (lambda parent-protocol-args
;;              (lambda for-rtd-field-values
;;                (if (not (= (length for-rtd-field-values) for-rtd-field-count))
;;                (assertion-violation 'make-record-constructor
;;                                                 "wrong number of arguments to record constructor"
;;                                                 for-rtd for-rtd-field-values))
;;                (apply (parent-protocol
;;                    (apply parent-make-seeder
;;                       (append for-rtd-field-values extension-field-values)))
;;                   parent-protocol-args)))))))
;;      (else
;;       (lambda extension-field-values
;;         (lambda for-rtd-field-values
;;           (if (not (= (length for-rtd-field-values) for-rtd-field-count))
;;           (assertion-violation 'make-record-constructor
;;                                        "wrong number of arguments to record constructor"
;;                                        for-rtd for-rtd-field-values))
;;           (wrap
;;            (apply (typed-vector-constructor real-rtd)
;;               (append for-rtd-field-values extension-field-values))))))))))

;;   ;; does RTD-1 represent an ancestor of RTD-2?

;;   ;; This suggests the corresponding procedure in VECTOR-TYPES should be
;;   ;; abstracted out.

;;   (define (rtd-ancestor? rtd-1 rtd-2)
;;     (let loop ((rtd-2 rtd-2))
;;       (or (eq? rtd-1 rtd-2)
;;       (and rtd-2
;;            (loop (record-type-parent rtd-2))))))

;;   (define (record-constructor desc)
;;     (let* ((rtd (record-constructor-descriptor-rtd desc)))
;;       ((record-constructor-descriptor-protocol desc)
;;        ((make-make-seeder rtd (lambda (r) r) desc)))))

;;   (define (record-predicate rtd)
;;     (vector-type-predicate rtd))

;;   (define (check-field-id who rtd field-id)
;;     (if (not (record-type-descriptor? rtd))
;;         (assertion-violation who
;;                              "not a record type descriptor" rtd))
;;     (if (not (and (integer? field-id)
;;                   (exact? field-id)
;;                   (>= field-id 0)
;;                   (< field-id (length (record-type-field-specs rtd)))))
;;         (assertion-violation who
;;                              "invalid index (not a non-negative exact integer less than the field count)" field-id)))

;;   (define (record-accessor rtd field-id)
;;     (check-field-id 'record-accessor rtd field-id)
;;     (typed-vector-accessor rtd (field-id-index rtd field-id)))

;;   (define (record-mutator rtd field-id)
;;     (check-field-id 'record-mutator rtd field-id)
;;     (if (not (record-field-mutable? rtd field-id))
;;     (assertion-violation 'record-mutator
;;                              "record-mutator called on immutable field" rtd field-id))
;;     (typed-vector-mutator rtd (field-id-index rtd field-id)))

;;   ;; A FIELD-ID is an index, which refers to a field in RTD itself.
;;   (define (field-id-index rtd field-id)
;;     (+ (field-count (record-type-parent rtd))
;;        field-id))

;;   (define (record-field-mutable? rtd field-id)
;;     (field-spec-mutable? (list-ref (record-type-field-specs rtd) field-id)))

;; (print "\n\n")


;; ;;   (define-syntax record-type-descriptor
;; ;;     (syntax-rules ()
;; ;;       ((record-type-descriptor ?record-type)
;; ;;        (?record-type descriptor))))

;;   (define-macro (record-type-descriptor record-type)
;;     `(,record-type descriptor))

;; ;;   (define-syntax no-record-type
;; ;;     (syntax-rules (descriptor constructor-descriptor)
;; ;;       ((?name descriptor) #f)
;; ;;       ((?name constructor-descriptor) #f)))

;;   (define-macro (no-record-type x) #f)


;; (define-macro (define-alist-extractor ?name ?name/cps ?tag ?default)
;;   `(begin
;; ;;   (define-syntax ?name/cps
;; ;;     (syntax-rules (?tag)
;; ;;       ((?name/cps () ?k . ?rands)
;; ;;        (?k ?default . ?rands))
;; ;;       ((?name/cps ((?tag ?val) . ?rest) ?k . ?rands)
;; ;;        (?k ?val . ?rands))
;; ;;       ((?name/cps ((?another-tag ?val) . ?rest) ?k . ?rands)
;; ;;        (?name/cps ?rest ?k . ?rands))))


;;      (define-macro (,?name/cps . args)
;;        (match args
;;          ((() ?k . ?rands)
;;           (append (list ?k ',?default) ?rands))
;;          ((((',?tag ?val) . ?rest) ?k . ?rands)
;;           `(,?k ,?val ,@?rands))
;;          ((((?another-tag ?val) . ?rest) ?k . ?rands)
;;           (append (list ',?name/cps ?rest ?k) ?rands))))
;;      (define-macro (,?name . args)
;;        (match args
;;          ((())
;;           ',?default)
;;          ((((',?tag ?val) . ?rest))
;;           ?val)
;;          ((((?another-tag ?val) . ?rest))
;;           (list ',?name ?rest))))))

;;   (define-alist-extractor extract-parent extract-parent/cps parent no-record-type)
;;   (define-alist-extractor extract-sealed extract-sealed/cps sealed #f)
;;   (define-alist-extractor extract-opaque extract-opaque/cps opaque #f)
;;   (define-alist-extractor extract-protocol extract-protocol/cps
;;     protocol #f)
;;   (define-alist-extractor extract-nongenerative extract-nongenerative/cps nongenerative #f)

;;   (define-alist-extractor extract-record-name extract-record-name/cps record-name cant-happen)
;;   (define-alist-extractor extract-constructor-name extract-constructor-name/cps
;;     constructor-name cant-happen)
;;   (define-alist-extractor extract-predicate-name extract-predicate-name/cps
;;     predicate-name cant-happen)


;; ;(write (macroexpand '(define-alist-extractor extract-parent extract-parent/cps parent no-record-type)))
;; ;(write (macroexpand '(extract-parent/cps () record-type-descriptor)))
;; ;;      (define $rtd
;; ;;        (make-record-type-descriptor (extract-record-name/cps (spec hage) quote)
;; ;;                     (extract-parent/cps () record-type-descriptor)
;; ;;                     (extract-nongenerative ())
;; ;;                     (extract-sealed ())
;; ;;                     (extract-opaque ())
;; ;;                     '((() ()) ...)))

;; ;; これを達成したい
;; ;; (define-record-type (point make-point point?)
;; ;;   (fields (immutable x point-x)
;; ;;           (mutable y point-y set-point-y!))
;; ;;   (nongenerative
;; ;;     point-4893d957-e00b-11d9-817f-00111175eb9e))

;; ;; (define p1 (make-point 1 2))
;; ;; (print (point? p1))

;; ;; まず define-record-type で
;; ;; (define-record-type-1
;; ;;      ((record-name point)
;; ;;       (constructor-name make-point)
;; ;;       (predicate-name point?))
;; ;;      ()             ; fields
;; ;;      (fields (immutable x point-x)
;; ;;              (mutable y point-y set-point-y!))
;; ;;      (nongenerative
;; ;;       point-4893d957-e00b-11d9-817f-00111175eb9e))


;; (begin (define $rtd (make-record-type-descriptor (extract-record-name/cps ((nongenerative 'point-4893d957-e00b-11d9-817f-00111175eb9e) (record-name point) (constructor-name make-point) (predicate-name point?)) quote) (extract-parent/cps ((nongenerative 'point-4893d957-e00b-11d9-817f-00111175eb9e) (record-name point) (constructor-name make-point) (predicate-name point?)) record-type-descriptor) (extract-nongenerative ((nongenerative 'point-4893d957-e00b-11d9-817f-00111175eb9e) (record-name point) (constructor-name make-point) (predicate-name point?))) (extract-sealed ((nongenerative 'point-4893d957-e00b-11d9-817f-00111175eb9e) (record-name point) (constructor-name make-point) (predicate-name point?))) (extract-opaque ((nongenerative 'point-4893d957-e00b-11d9-817f-00111175eb9e) (record-name point) (constructor-name make-point) (predicate-name point?))) '((immutable x) (mutable y)))) (define $constructor-descriptor (make-record-constructor-descriptor $rtd (extract-parent/cps ((nongenerative 'point-4893d957-e00b-11d9-817f-00111175eb9e) (record-name point) (constructor-name make-point) (predicate-name point?)) record-constructor-descriptor) (extract-protocol ((nongenerative 'point-4893d957-e00b-11d9-817f-00111175eb9e) (record-name point) (constructor-name make-point) (predicate-name point?))))) (extract-record-name/cps ((nongenerative 'point-4893d957-e00b-11d9-817f-00111175eb9e) (record-name point) (constructor-name make-point) (predicate-name point?)) define-record-type-name $rtd $constructor-descriptor) (extract-constructor-name/cps ((nongenerative 'point-4893d957-e00b-11d9-817f-00111175eb9e) (record-name point) (constructor-name make-point) (predicate-name point?)) define (record-constructor $constructor-descriptor)) (extract-predicate-name/cps ((nongenerative 'point-4893d957-e00b-11d9-817f-00111175eb9e) (record-name point) (constructor-name make-point) (predicate-name point?)) define (record-predicate $rtd)) (define-record-fields $rtd 0 (x (point-x)) (y (point-y set-point-y!))))

;; ;; test
;; ;; (write (macroexpand '(define-alist-extractor extract-parent extract-parent/cps parent no-record-type)))
;; ;; (print "\n\n")
;; ;; (define-alist-extractor extract-parent extract-parent/cps parent no-record-type)
;; ;; (write (extract-parent ((hoge hige) (parent 'moge))))
