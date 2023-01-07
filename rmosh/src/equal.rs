use std::{collections::HashMap, ptr::null_mut};

use rand::Rng;

use crate::{
    gc::Gc,
    objects::{Object, Vox},
};

//  Copied and ported to Rust from
//  Efficient Nondestructive Equality Checking for Trees and Graphs
//  Michael D. Adams and R. Kent Dybvig
//  ICFP 2008

pub struct Equal {
    k0: Object,
    kb: Object,
}
impl Equal {
    pub fn new() -> Self {
        Self {
            k0: Object::Number(400),
            kb: Object::Number(-40),
        }
    }
    pub fn is_equal(&self, gc: &mut Box<Gc>, x: &Object, y: &Object) -> bool {
        self.is_precheck_interleave_equal(gc, x, y)
    }

    //   (define (precheck/interleave-equal? x y)
    //     (let ([k (pre? x y k0)])
    //       (and k (or (> k 0) (interleave? x y 0)))))
    fn is_precheck_interleave_equal(&self, gc: &mut Box<Gc>, x: &Object, y: &Object) -> bool {
        let k = self.is_pre(gc, x, y, self.k0);
        if k.is_false() {
            return false;
        }
        if k.to_number() > 0 {
            return true;
        } else {
            self.is_interleave(gc, x, y, Object::Number(0))
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
    fn is_pre(&self, gc: &mut Box<Gc>, x: &Object, y: &Object, k: Object) -> Object {
        if x == y {
            return k;
        }
        match (x, y) {
            (Object::Pair(pair1), Object::Pair(pair2)) => {
                if k.to_number() <= 0 {
                    return k;
                } else {
                    let k2 = self.is_pre(
                        gc,
                        &pair1.car,
                        &pair2.car,
                        Object::Number(k.to_number() - 1),
                    );
                    if k2.is_false() {
                        return Object::False;
                    }
                    return self.is_pre(gc, &pair1.cdr, &pair2.cdr, k2);
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
                    if i == n || k.to_number() <= 0 {
                        return k;
                    } else {
                        let k2 = self.is_pre(
                            gc,
                            &v1.data[i],
                            &v2.data[i],
                            Object::Number(k.to_number() - 1),
                        );
                        if k2.is_false() {
                            return Object::False;
                        }
                        i += 1;
                        k = k2;
                    }
                }
            }
            (Object::String(s1), Object::String(s2)) => {
                if s1.string.eq(&s2.string) {
                    return k;
                } else {
                    return Object::False;
                }
            }
            (Object::Procedure(p1), Object::Procedure(p2)) => {
                if p1.func as isize == p2.func as isize {
                    return k;
                } else {
                    return Object::False;
                }
            }
            _ => {
                if x.eqv(y) {
                    return k;
                } else {
                    Object::False
                }
            }
        }
    }

    //     (define (interleave? x y k)
    //       (and (e? x y k) #t))
    fn is_interleave(&self, gc: &mut Box<Gc>, x: &Object, y: &Object, k: Object) -> bool {
        let hashmap: *mut *mut HashMap<Object, Object> = null_mut();
        if self.is_e(gc, hashmap, x, y, k).is_false() {
            false
        } else {
            true
        }
    }

    //       (define (call-union-find x y)
    //         (unless ht (set! ht (make-eq-hashtable)))
    //         (union-find ht x y))
    fn call_union_find(
        &self,
        gc: &mut Box<Gc>,
        hashmap: *mut *mut HashMap<Object, Object>,
        x: &Object,
        y: &Object,
    ) -> Object {
        if unsafe { *hashmap == null_mut() } {
            unsafe { *hashmap = &mut HashMap::new().to_owned() as *mut HashMap<Object, Object> };
        }
        return self.union_find(gc, unsafe { *hashmap }, x, y);
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
            return b;
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
        hashmap: *mut HashMap<Object, Object>,
        x: &Object,
        y: &Object,
    ) -> Object {
        let hm: &mut HashMap<Object, Object> = unsafe { hashmap.as_mut().unwrap() };
        let bx = match hm.get(x) {
            Some(v) => *v,
            None => Object::False,
        };
        let by = match hm.get(y) {
            Some(v) => *v,
            None => Object::False,
        };
        if bx.is_false() {
            if by.is_false() {
                let b = Object::Vox(gc.alloc(Vox::new(Object::Number(1))));
                hm.insert(*x, b);
                hm.insert(*y, b);
                return Object::False;
            } else {
                let ry = self.find(by);
                hm.insert(*x, ry);
                return Object::False;
            }
        } else if by.is_false() {
            let rx = self.find(bx);
            hm.insert(*y, rx);
            return Object::False;
        } else {
            let rx = self.find(bx);
            let ry = self.find(by);
            if rx == rx {
                return Object::True;
            }
            let nx = rx.to_vox().value;
            let ny = ry.to_vox().value;
            if nx.to_number() > ny.to_number() {
                ry.to_vox().value = rx;
                rx.to_vox().value = Object::Number(nx.to_number() + ny.to_number());
                return Object::False;
            } else {
                rx.to_vox().value = ry;
                ry.to_vox().value = Object::Number(ny.to_number() + nx.to_number());
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
        hashmap: *mut *mut HashMap<Object, Object>,
        x: &Object,
        y: &Object,
        k: Object,
    ) -> Object {
        if k.to_number() <= 0 {
            if k == self.kb {
                let mut rng = rand::thread_rng();
                let next_k = Object::Number(rng.gen::<isize>() % (2 * self.k0.to_number()));
                return self.is_fast(gc, hashmap, &x, &y, next_k);
            } else {
                return self.is_slow(gc, hashmap, x, y, k);
            }
        } else {
            return self.is_fast(gc, hashmap, x, y, k);
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
        hashmap: *mut *mut HashMap<Object, Object>,
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
                    return Object::Number(0);
                } else {
                    let k = self.is_e(
                        gc,
                        hashmap,
                        &pair1.car,
                        &pair2.car,
                        Object::Number(k.to_number() - 1),
                    );
                    if k.is_false() {
                        return Object::False;
                    }
                    return self.is_e(gc, hashmap, &pair1.cdr, &pair2.cdr, k);
                }
            }
            (Object::Vector(v1), Object::Vector(v2)) => {
                let n = v1.len();
                if v2.len() != n {
                    return Object::False;
                }
                if !self.call_union_find(gc, hashmap, x, y).is_false() {
                    return Object::Number(0);
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
                    return k;
                } else {
                    return Object::False;
                }
            }
            (Object::Procedure(p1), Object::Procedure(p2)) => {
                if p1.func as isize == p2.func as isize {
                    return Object::True;
                } else {
                    return Object::False;
                }
            }
            _ => {
                if x.eqv(&y) {
                    return k;
                } else {
                    return Object::False;
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
        hashmap: *mut *mut HashMap<Object, Object>,
        x: &Object,
        y: &Object,
        k: Object,
    ) -> Object {
        let mut k = Object::Number(k.to_number() - 1);
        if x == y {
            return k;
        }
        match (x, y) {
            (Object::Pair(pair1), Object::Pair(pair2)) => {
                k = self.is_e(gc, hashmap, &pair1.car, &pair2.car, k);
                if k.is_false() {
                    return Object::False;
                }
                return self.is_e(gc, hashmap, &pair1.cdr, &pair2.cdr, k);
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
                    return k;
                } else {
                    return Object::False;
                }
            }
            (Object::Procedure(p1), Object::Procedure(p2)) => {
                if p1.func as isize == p2.func as isize {
                    return k;
                } else {
                    return Object::False;
                }
            }
            _ => {
                if x.eqv(y) {
                    return k;
                } else {
                    return Object::False;
                }
            }
        }
    }
}
