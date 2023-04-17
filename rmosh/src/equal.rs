use std::collections::HashMap;

use rand::Rng;

use crate::{
    gc::Gc,
    objects::{Object, Vox}, numbers::ObjectExt,
};

//  Copied and ported to Rust from
//  Efficient Nondestructive Equality Checking for Trees and Graphs
//  Michael D. Adams and R. Kent Dybvig
//  ICFP 2008

pub struct Equal {
    k0: Object,
    kb: Object,
}
impl Default for Equal {
    fn default() -> Self {
        Self::new()
    }
}

impl Equal {
    pub fn new() -> Self {
        Self {
            k0: Object::Fixnum(400),
            kb: Object::Fixnum(-40),
        }
    }
    pub fn is_equal(&self, gc: &mut Box<Gc>, x: &Object, y: &Object) -> bool {
        self.is_precheck_interleave_equal(gc, x, y)
    }

    //   (define (precheck/interleave-equal? x y)
    //     (let ([k (pre? x y k0)])
    //       (and k (or (> k 0) (interleave? x y 0)))))
    fn is_precheck_interleave_equal(&self, gc: &mut Box<Gc>, x: &Object, y: &Object) -> bool {
        let k = Self::is_pre(gc, x, y, self.k0);
        if k.is_false() {
            return false;
        }
        if k.to_isize() > 0 {
            true
        } else {
            self.is_interleave(gc, x, y, Object::Fixnum(0))
        }
    }

    // (define (pre? x y k)
    //     (import UNSAFE)
    //     (cond
    //       [(eq? x y) k]
    //       [(pair? x)
    //        (and (pair? y)
    //             (if (<= k 0)
    //                 k
    //                 (let ([k (pre? (car x) (car y) (- k 1))])
    //                   (and k (pre? (cdr x) (cdr y) k)))))]
    //       [(vector? x)
    //        (and (vector? y)
    //             (let ([n (vector-length x)])
    //               (and (= (vector-length y) n)
    //                    (let f ([i 0] [k k])
    //                      (if (or (= i n) (<= k 0))
    //                          k
    //                          (let ([k (pre?
    //                                     (vector-ref x i)
    //                                     (vector-ref y i)
    //                                     (- k 1))])
    //                            (and k (f (+ i 1) k))))))))]
    //       [(string? x) (and (string? y) (string=? x y) k)]
    //       [(bytevector? x) (and (bytevector? y) (bytevector=? x y) k)]
    //       [else (and (eqv? x y) k)]))
    fn is_pre(_gc: &mut Box<Gc>, x: &Object, y: &Object, k: Object) -> Object {
        if x == y {
            return k;
        }
        match (x, y) {
            (Object::Pair(pair1), Object::Pair(pair2)) => {
                if k.to_isize() <= 0 {
                    k
                } else {
                    let k2 = Self::is_pre(
                        _gc,
                        &pair1.car,
                        &pair2.car,
                        Object::Fixnum(k.to_isize() - 1),
                    );
                    if k2.is_false() {
                        return Object::False;
                    }
                    Self::is_pre(_gc, &pair1.cdr, &pair2.cdr, k2)
                }
            }
            (Object::Bytevector(bv1), Object::Bytevector(bv2)) => {
                if bv1.equal(bv2) {
                    k
                } else {
                    Object::False
                }
            }
            (Object::Vector(v1), Object::Vector(v2)) => {
                let n = v1.len();
                if v2.len() != n {
                    return Object::False;
                }
                let mut i: usize = 0;
                let mut k = k;
                loop {
                    if i == n || k.to_isize() <= 0 {
                        return k;
                    } else {
                        let k2 = Self::is_pre(
                            _gc,
                            &v1.data[i],
                            &v2.data[i],
                            Object::Fixnum(k.to_isize() - 1),
                        );
                        if k2.is_false() {
                            return Object::False;
                        }
                        i += 1;
                        k = k2;
                    }
                }
            }
            (Object::Regexp(r1), Object::Regexp(r2)) => {
                if r1.pattern.eq(&r2.pattern) {
                    k
                } else {
                    Object::False
                }
            }
            (Object::String(s1), Object::String(s2)) => {
                if s1.string.eq(&s2.string) {
                    k
                } else {
                    Object::False
                }
            }
            (Object::Procedure(p1), Object::Procedure(p2)) => {
                if p1.func as usize == p2.func as usize {
                    k
                } else {
                    Object::False
                }
            }
            _ => {
                if x.eqv(y) {
                    k
                } else {
                    Object::False
                }
            }
        }
    }

    //     (define (interleave? x y k)
    //       (and (e? x y k) #t))
    fn is_interleave(&self, gc: &mut Box<Gc>, x: &Object, y: &Object, k: Object) -> bool {
        let mut hashmap: HashMap<Object, Object> = HashMap::new();
        !self.is_e(gc, &mut hashmap, x, y, k).is_false()
    }

    //       (define (call-union-find x y)
    //         (unless ht (set! ht (make-eq-hashtable)))
    //         (union-find ht x y))
    fn call_union_find(
        &self,
        gc: &mut Box<Gc>,
        hashmap: &mut HashMap<Object, Object>,
        x: &Object,
        y: &Object,
    ) -> Object {
        self.union_find(gc, hashmap, x, y)
    }

    // (define (find b)
    //       (let ([n (box-content b)])
    //         (if (box? n)
    //             (let loop ([b b] [n n])
    //               (let ([nn (box-content n)])
    //                 (if (box? nn) (begin (set-box-content! b nn) (loop n nn)) n)))
    //             b)))

    fn find(&self, b: Object) -> Object {
        let mut n = b.to_vox().value;
        if n.is_vox() {
            let mut b = b;
            loop {
                let nn = n.to_vox().value;
                if nn.is_vox() {
                    b.to_vox().value = nn;
                    b = n;
                    n = nn;
                } else {
                    return n;
                }
            }
        } else {
            b
        }
    }

    //   (define (union-find ht x y)
    //     (let ([bx (eq-hashtable-ref ht x #f)]
    //           [by (eq-hashtable-ref ht y #f)])
    //       (if (not bx)
    //           (if (not by)
    //               (let ([b (make-box 1)])
    //                 (eq-hashtable-set! ht x b)
    //                 (eq-hashtable-set! ht y b)
    //                 #f)
    //               (let ([ry (find by)]) (eq-hashtable-set! ht x ry) #f))
    //           (if (not by)
    //               (let ([rx (find bx)]) (eq-hashtable-set! ht y rx) #f)
    //               (let ([rx (find bx)] [ry (find by)])
    //                 (or (eq? rx ry)
    //                     (let ([nx (box-content rx)] [ny (box-content ry)])
    //                       (if (> nx ny)
    //                           (begin
    //                             (set-box-content! ry rx)
    //                             (set-box-content! rx (+ nx ny))
    //                             #f)
    //                           (begin
    //                             (set-box-content! rx ry)
    //                             (set-box-content! ry (+ ny nx))
    //                             #f)))))))))
    fn union_find(
        &self,
        gc: &mut Box<Gc>,
        hashmap: &mut HashMap<Object, Object>,
        x: &Object,
        y: &Object,
    ) -> Object {
        let bx = match hashmap.get(x) {
            Some(v) => *v,
            None => Object::False,
        };
        let by = match hashmap.get(y) {
            Some(v) => *v,
            None => Object::False,
        };
        if bx.is_false() {
            if by.is_false() {
                let b = Object::Vox(gc.alloc(Vox::new(Object::Fixnum(1))));
                hashmap.insert(*x, b);
                hashmap.insert(*y, b);
                Object::False
            } else {
                let ry = self.find(by);
                hashmap.insert(*x, ry);
                Object::False
            }
        } else if by.is_false() {
            let rx = self.find(bx);
            hashmap.insert(*y, rx);
            return Object::False;
        } else {
            let rx = self.find(bx);
            let ry = self.find(by);
            if rx == ry {
                return Object::True;
            }
            let nx = rx.to_vox().value;
            let ny = ry.to_vox().value;
            if nx.to_isize() > ny.to_isize() {
                ry.to_vox().value = rx;
                rx.to_vox().value = Object::Fixnum(nx.to_isize() + ny.to_isize());
                return Object::False;
            } else {
                rx.to_vox().value = ry;
                ry.to_vox().value = Object::Fixnum(ny.to_isize() + nx.to_isize());
                return Object::False;
            }
        }
    }

    //       (define (e? x y k)
    //         (if (<= k 0)
    //             (if (= k kb) (fast? x y (random (* 2 k0))) (slow? x y k))
    //             (fast? x y k)))
    fn is_e(
        &self,
        gc: &mut Box<Gc>,
        hashmap: &mut HashMap<Object, Object>,
        x: &Object,
        y: &Object,
        k: Object,
    ) -> Object {
        if k.to_isize() <= 0 {
            if k == self.kb {
                let mut rng = rand::thread_rng();
                let next_k = Object::Fixnum(rng.gen::<isize>() % (2 * self.k0.to_isize()));
                self.is_fast(gc, hashmap, x, y, next_k)
            } else {
                self.is_slow(gc, hashmap, x, y, k)
            }
        } else {
            self.is_fast(gc, hashmap, x, y, k)
        }
    }

    // (define (slow? x y k)
    //         (cond
    //           [(eq? x y) k]
    //           [(pair? x)
    //            (and (pair? y)
    //                 (if (call-union-find x y)
    //                     0
    //                     (let ([k (e? (car x) (car y) (- k 1))])
    //                       (and k (e? (cdr x) (cdr y) k)))))]
    //           [(vector? x)
    //            (and (vector? y)
    //                 (let ([n (vector-length x)])
    //                   (and (= (vector-length y) n)
    //                        (if (call-union-find x y)
    //                            0
    //                            (let f ([i 0] [k (- k 1)])
    //                              (if (= i n)
    //                                  k
    //                                  (let ([k (e? (vector-ref x i)
    //                                               (vector-ref y i)
    //                                               k)])
    //                                    (and k (f (+ i 1) k)))))))))]
    //           [(string? x) (and (string? y) (string=? x y) k)]
    //           [(bytevector? x) (and (bytevector? y) (bytevector=? x y) k)]
    //           [else (and (eqv? x y) k)]))
    fn is_slow(
        &self,
        gc: &mut Box<Gc>,
        hashmap: &mut HashMap<Object, Object>,
        x: &Object,
        y: &Object,
        k: Object,
    ) -> Object {
        if x == y {
            return k;
        }
        match (x, y) {
            (Object::Pair(pair1), Object::Pair(pair2)) => {
                if !self.call_union_find(gc, hashmap, x, y).is_false() {
                    Object::Fixnum(0)
                } else {
                    let k = self.is_e(
                        gc,
                        hashmap,
                        &pair1.car,
                        &pair2.car,
                        Object::Fixnum(k.to_isize() - 1),
                    );
                    if k.is_false() {
                        return Object::False;
                    }
                    self.is_e(gc, hashmap, &pair1.cdr, &pair2.cdr, k)
                }
            }
            (Object::Bytevector(bv1), Object::Bytevector(bv2)) => {
                if bv1.equal(bv2) {
                    k
                } else {
                    Object::False
                }
            }
            (Object::Vector(v1), Object::Vector(v2)) => {
                let n = v1.len();
                if v2.len() != n {
                    return Object::False;
                }
                if !self.call_union_find(gc, hashmap, x, y).is_false() {
                    return Object::Fixnum(0);
                }
                let mut i: usize = 0;
                let mut k = k;
                loop {
                    if i == n {
                        return k;
                    }
                    k = self.is_e(gc, hashmap, &v1.data[i], &v2.data[i], k);
                    if k.is_false() {
                        return Object::False;
                    }
                    i += 1;
                }
            }
            (Object::String(s1), Object::String(s2)) => {
                if s1.string.eq(&s2.string) {
                    k
                } else {
                    Object::False
                }
            }
            (Object::Procedure(p1), Object::Procedure(p2)) => {
                if p1.func as usize == p2.func as usize {
                    Object::True
                } else {
                    Object::False
                }
            }
            _ => {
                if x.eqv(y) {
                    k
                } else {
                    Object::False
                }
            }
        }
    }

    // (define (fast? x y k)
    //         (let ([k (- k 1)])
    //           (cond
    //             [(eq? x y) k]
    //             [(pair? x)
    //              (and (pair? y)
    //                   (let ([k (e? (car x) (car y) k)])
    //                     (and k (e? (cdr x) (cdr y) k))))]
    //             [(vector? x)
    //              (and (vector? y)
    //                   (let ([n (vector-length x)])
    //                     (and (= (vector-length y) n)
    //                          (let f ([i 0] [k k])
    //                            (if (= i n)
    //                                k
    //                                (let ([k (e? (vector-ref x i)
    //                                             (vector-ref y i)
    //                                             k)])
    //                                  (and k (f (+ i 1) k))))))))]
    //             [(string? x) (and (string? y) (string=? x y) k)]
    //             [(bytevector? x) (and (bytevector? y) (bytevector=? x y) k)]
    //             [else (and (eqv? x y) k)])))
    fn is_fast(
        &self,
        gc: &mut Box<Gc>,
        hashmap: &mut HashMap<Object, Object>,
        x: &Object,
        y: &Object,
        k: Object,
    ) -> Object {
        let mut k = Object::Fixnum(k.to_isize() - 1);
        if x == y {
            return k;
        }
        match (x, y) {
            (Object::Pair(pair1), Object::Pair(pair2)) => {
                k = self.is_e(gc, hashmap, &pair1.car, &pair2.car, k);
                if k.is_false() {
                    return Object::False;
                }
                self.is_e(gc, hashmap, &pair1.cdr, &pair2.cdr, k)
            }
            (Object::Bytevector(bv1), Object::Bytevector(bv2)) => {
                if bv1.equal(bv2) {
                    k
                } else {
                    Object::False
                }
            }
            (Object::Regexp(r1), Object::Regexp(r2)) => {
                (r1.pattern.eq(&r2.pattern)).to_obj()
            }            
            (Object::Vector(v1), Object::Vector(v2)) => {
                let n = v1.len();
                if v2.len() != n {
                    return Object::False;
                }
                let mut i: usize = 0;
                let mut k = k;
                loop {
                    if i == n {
                        return k;
                    }
                    k = self.is_e(gc, hashmap, &v1.data[i], &v2.data[i], k);
                    if k.is_false() {
                        return Object::False;
                    }
                    i += 1;
                }
            }
            (Object::String(s1), Object::String(s2)) => {
                if s1.string.eq(&s2.string) {
                    k
                } else {
                    Object::False
                }
            }
            (Object::Procedure(p1), Object::Procedure(p2)) => {
                if p1.func as usize == p2.func as usize {
                    k
                } else {
                    Object::False
                }
            }
            _ => {
                if x.eqv(y) {
                    k
                } else {
                    Object::False
                }
            }
        }
    }
}
