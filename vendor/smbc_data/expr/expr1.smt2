
; SAT

; find an arithmetic expression that is not properly simplified

; courtesy of Koen Classen and Dan Rosen

(declare-datatypes () ((Nat (Z) (S (p Nat)))))
(declare-datatypes ()
  ((list (Nil) (Cons (Cons_0 Nat) (Cons_1 list)))))
(declare-datatypes ()
  ((E (N (N_0 Nat))
     (Add (Add_0 E) (Add_1 E)) (Mul (Mul_0 E) (Mul_1 E))
     (Div (Div_0 E) (Div_1 E)) (Eq (Eq_0 E) (Eq_1 E)) (V (V_0 Nat)))))
(define-fun-rec
  plus
    ((x Nat) (y Nat)) Nat
    (match x
      (case Z y)
      (case (S n) (S (plus n y)))))
(define-fun-rec
  mult
    ((x Nat) (y Nat)) Nat
    (match x
      (case Z Z)
      (case (S n) (plus y (mult n y)))))
(define-fun-rec
  minus
    ((x Nat) (y Nat)) Nat
    (match x
      (case Z Z)
      (case (S n)
        (match y
          (case Z x)
          (case (S m) (minus n m))))))
(define-fun
  isVar
    ((x E)) Bool
    (match x
      (case default false)
      (case (V y) true)))
(define-fun-rec
  fetch
    ((x list) (y Nat)) Nat
    (match x
      (case Nil Z)
      (case (Cons n st)
        (match y
          (case Z n)
          (case (S z) (fetch st z))))))
(define-fun-rec
  equal
    ((x Nat) (y Nat)) Bool
    (match x
      (case Z
        (match y
          (case Z true)
          (case (S z) false)))
      (case (S x2)
        (match y
          (case Z false)
          (case (S y2) (equal x2 y2))))))
(define-fun-rec
  eqE
    ((x E) (y E)) Bool
    (match x
      (case (N a)
        (match y
          (case default false)
          (case (N b) (equal a b))))
      (case (Add a1 a2)
        (match y
          (case default false)
          (case (Add b1 b2) (and (eqE a1 b1) (eqE a2 b2)))))
      (case (Mul a12 a22)
        (match y
          (case default false)
          (case (Mul b12 b22) (and (eqE a12 b12) (eqE a22 b22)))))
      (case (Div z x2) false)
      (case (Eq a13 a23)
        (match y
          (case default false)
          (case (Eq b13 b23) (and (eqE a13 b13) (eqE a23 b23)))))
      (case (V c)
        (match y
          (case default false)
          (case (V b3) (equal c b3))))))
(define-fun
  step1
    ((x E)) E
    (match x
      (case default x)
      (case (Add y b)
        (let
          ((z (let
                ((x2
                    (ite
                      (eqE y b) (Mul (N (S (S Z))) y)
                      (match y
                        (case default x)
                        (case (Add a c) (Add a (Add c b)))
                        (case (V x3)
                          (match b
                            (case default x)
                            (case (V y2) (ite (equal x3 x3) (Mul (N (S (S Z))) y) x))))))))
                (match b
                  (case default x2)
                  (case (N x4)
                    (match x4
                      (case Z y)
                      (case (S x5) x2)))))))
          (match y
            (case default z)
            (case (N x6)
              (match x6
                (case Z b)
                (case (S x7) z))))))
      (case (Mul x8 b2)
        (let
          ((x9
              (let
                ((x10
                    (let
                      ((x11
                          (let
                            ((x12
                                (match x8
                                  (case default x)
                                  (case (Mul a2 b3) (Mul a2 (Mul b3 b2))))))
                            (match b2
                              (case default x12)
                              (case (N x13)
                                (match x13
                                  (case Z x12)
                                  (case (S x14)
                                    (match x14
                                      (case Z x8)
                                      (case (S x15) x12)))))))))
                      (match x8
                        (case default x11)
                        (case (N x16)
                          (match x16
                            (case Z x11)
                            (case (S x17)
                              (match x17
                                (case Z b2)
                                (case (S x18) x11)))))))))
                (match b2
                  (case default x10)
                  (case (N x19)
                    (match x19
                      (case Z (N Z))
                      (case (S x20) x10)))))))
          (match x8
            (case default x9)
            (case (N x21)
              (match x21
                (case Z (N Z))
                (case (S x22) x9))))))
      (case (Eq a3 b4) (ite (eqE a3 b4) (N (S Z)) x))))
(define-fun-rec
  simp1
    ((x E)) E
    (match x
      (case default (step1 x))
      (case (Add a b) (step1 (Add (simp1 a) (simp1 b))))
      (case (Mul c b2) (step1 (Mul (simp1 c) (simp1 b2))))
      (case (Eq a2 b3) (step1 (Eq (simp1 a2) (simp1 b3))))))
(define-fun
  step2
    ((x E)) E
    (match x
      (case default x)
      (case (Add y b)
        (let
          ((z (let
                ((x2
                    (ite
                      (eqE y b) (Mul (N (S (S Z))) y)
                      (match y
                        (case default x)
                        (case (Add a c) (Add a (Add c b)))))))
                (match b
                  (case default x2)
                  (case (N x3)
                    (match x3
                      (case Z y)
                      (case (S x4) x2)))))))
          (match y
            (case default z)
            (case (N x5)
              (match x5
                (case Z b)
                (case (S x6) z))))))
      (case (Mul x7 b2)
        (let
          ((x8
              (let
                ((x9
                    (let
                      ((x10
                          (let
                            ((x11
                                (match x7
                                  (case default x)
                                  (case (Mul a2 b3) (Mul a2 (Mul b3 b2))))))
                            (match b2
                              (case default x11)
                              (case (N x12)
                                (match x12
                                  (case Z x11)
                                  (case (S x13)
                                    (match x13
                                      (case Z x7)
                                      (case (S x14) x11)))))))))
                      (match x7
                        (case default x10)
                        (case (N x15)
                          (match x15
                            (case Z x10)
                            (case (S x16)
                              (match x16
                                (case Z b2)
                                (case (S x17) x10)))))))))
                (match b2
                  (case default x9)
                  (case (N x18)
                    (match x18
                      (case Z (N Z))
                      (case (S x19) x9)))))))
          (match x7
            (case default x8)
            (case (N x20)
              (match x20
                (case Z (N Z))
                (case (S x21) x8))))))
      (case (Div a3 b4) (ite (eqE a3 b4) (N (S Z)) x))
      (case (Eq a4 b5) (ite (eqE a4 b5) (N (S Z)) x))))
(define-fun-rec
  simp2
    ((x E)) E
    (match x
      (case default (step2 x))
      (case (Add a b) (step2 (Add (simp2 a) (simp2 b))))
      (case (Mul c b2) (step2 (Mul (simp2 c) (simp2 b2))))
      (case (Eq a2 b3) (step2 (Eq (simp2 a2) (simp2 b3))))))
(define-fun
  step3
    ((x E)) E
    (match x
      (case default x)
      (case (Add y b)
        (let
          ((z (let
                ((x2
                    (ite
                      (eqE y b) (Mul (N (S (S Z))) y)
                      (match y
                        (case default x)
                        (case (Add a c) (Add a (Add c b)))))))
                (match b
                  (case default x2)
                  (case (N x3)
                    (match x3
                      (case Z y)
                      (case (S x4) x2)))))))
          (match y
            (case default z)
            (case (N x5)
              (match x5
                (case Z b)
                (case (S x6) z))))))
      (case (Mul x7 b2)
        (let
          ((x8
              (let
                ((x9
                    (let
                      ((x10
                          (let
                            ((x11
                                (match x7
                                  (case default x)
                                  (case (Mul a2 b3) (Mul a2 (Mul b3 b2))))))
                            (match b2
                              (case default x11)
                              (case (N x12)
                                (match x12
                                  (case Z x11)
                                  (case (S x13)
                                    (match x13
                                      (case Z x7)
                                      (case (S x14) x11)))))))))
                      (match x7
                        (case default x10)
                        (case (N x15)
                          (match x15
                            (case Z x10)
                            (case (S x16)
                              (match x16
                                (case Z b2)
                                (case (S x17) x10)))))))))
                (match b2
                  (case default x9)
                  (case (N x18)
                    (match x18
                      (case Z (N Z))
                      (case (S x19) x9)))))))
          (match x7
            (case default x8)
            (case (N x20)
              (match x20
                (case Z (N Z))
                (case (S x21) x8))))))
      (case (Div x22 c2)
        (match x22
          (case default x)
          (case (Mul a3 b4)
            (ite (and (eqE b4 c2) (not (eqE c2 (N Z)))) a3 x))))
      (case (Eq a4 b5) (ite (eqE a4 b5) (N (S Z)) x))))
(define-fun-rec
  simp3
    ((x E)) E
    (match x
      (case default (step3 x))
      (case (Add a b) (step3 (Add (simp3 a) (simp3 b))))
      (case (Mul c b2) (step3 (Mul (simp3 c) (simp3 b2))))
      (case (Eq a2 b3) (step3 (Eq (simp3 a2) (simp3 b3))))))
(define-fun
  step4
    ((x E)) E
    (match x
      (case default x)
      (case (Add y b)
        (let
          ((z (let
                ((x2
                    (ite
                      (eqE y b) (Mul (N (S (S Z))) y)
                      (match y
                        (case default x)
                        (case (Add a c) (Add a (Add c b)))))))
                (match b
                  (case default x2)
                  (case (N x3)
                    (match x3
                      (case Z y)
                      (case (S x4) x2)))))))
          (match y
            (case default z)
            (case (N x5)
              (match x5
                (case Z b)
                (case (S x6) z))))))
      (case (Mul x7 b2)
        (let
          ((x8
              (let
                ((x9
                    (let
                      ((x10
                          (let
                            ((x11
                                (match x7
                                  (case default x)
                                  (case (Mul a2 b3) (Mul a2 (Mul b3 b2))))))
                            (match b2
                              (case default x11)
                              (case (N x12)
                                (match x12
                                  (case Z x11)
                                  (case (S x13)
                                    (match x13
                                      (case Z x7)
                                      (case (S x14) x11)))))))))
                      (match x7
                        (case default x10)
                        (case (N x15)
                          (match x15
                            (case Z x10)
                            (case (S x16)
                              (match x16
                                (case Z b2)
                                (case (S x17) x10)))))))))
                (match b2
                  (case default x9)
                  (case (N x18)
                    (match x18
                      (case Z (N Z))
                      (case (S x19) x9)))))))
          (match x7
            (case default x8)
            (case (N x20)
              (match x20
                (case Z (N Z))
                (case (S x21) x8))))))
      (case (Div x22 c2)
        (match x22
          (case default x)
          (case (Mul a3 b4)
            (ite
              (and (eqE b4 c2) (and (not (eqE c2 (N Z))) (not (isVar c2)))) a3
              x))))
      (case (Eq a4 b5) (ite (eqE a4 b5) (N (S Z)) x))))
(define-fun-rec
  simp4
    ((x E)) E
    (match x
      (case default (step4 x))
      (case (Add a b) (step4 (Add (simp4 a) (simp4 b))))
      (case (Mul c b2) (step4 (Mul (simp4 c) (simp4 b2))))
      (case (Eq a2 b3) (step4 (Eq (simp4 a2) (simp4 b3))))))
(define-fun-rec
  div_divv
    ((x Nat) (y Nat) (z Nat)) Nat
    (match x
      (case Z z)
      (case (S x2) (div_divv (minus x y) y (S z)))))
(define-fun
  div2
    ((x Nat) (y Nat)) Nat
    (match y
      (case Z Z)
      (case (S z) (div_divv x y Z))))
(define-fun-rec
  eval
    ((x list) (y E)) Nat
    (match y
      (case (N n) n)
      (case (Add a b) (plus (eval x a) (eval x b)))
      (case (Mul c b2) (mult (eval x c) (eval x b2)))
      (case (Div a2 b3) (div2 (eval x a2) (eval x b3)))
      (case (Eq a3 b4) (ite (equal (eval x a3) (eval x b4)) (S Z) Z))
      (case (V z) (fetch x z))))
(assert-not
  (forall ((st list) (a E)) (equal (eval st a) (eval st (simp2 a)))))
(check-sat)
