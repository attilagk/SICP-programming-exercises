;;;;CODE FROM CHAPTER 1 OF STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;; Examples from the book are commented out with ;: so that they
;;;  are easy to find and so that they will be omitted if you evaluate a
;;;  chunk of the file (programs with intervening examples) in Scheme.

;;; BEWARE: Although the whole file can be loaded into Scheme,
;;;  don't expect the programs to work if you do so.  For example,
;;;  the redefinition of + in exercise 1.9 wreaks havoc with the
;;;  last version of square defined here.

;;;SECTION 1.1.1

;; interpreter examples

;: 486

;: (+ 137 349)
;: (- 1000 334)
;: (* 5 99)

(/ 10 5)
(+ 2.7 10)

;: (+ 21 35 12 7)
;: (* 25 4 12)

;: (+ (* 3 5) (- 10 6))

;: (+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))

;: (+ (* 3
;:       (+ (* 2 4)
;:          (+ 3 5)))
;:    (+ (- 10 7)
;:       6))


;;;SECTION 1.1.2

;: (define size 2)
;: size
;: (* 5 size)

(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))
(define circumference (* 2 pi radius))
circumference


;;;SECTION 1.1.3

;: (* (+ 2 (* 4 6))
;:    (+ 3 5 7))


;;;SECTION 1.1.4

(define (square x) (* x x))

;: (square 21)
;: (square (+ 2 5))
;: (square (square 3))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

;: (sum-of-squares 3 4)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

;: (f 5)


;;;SECTION 1.1.5

;: (f 5)
;: (sum-of-squares (+ 5 1) (* 5 2))
;: (+ (square 6) (square 10))
;: (+ (* 6 6) (* 10 10))
;: (+ 36 100)

;: (f 5)
;: (sum-of-squares (+ 5 1) (* 5 2))
;: (+    (square (+ 5 1))      (square (* 5 2))  )
;: (+    (* (+ 5 1) (+ 5 1))   (* (* 5 2) (* 5 2)))
;: (+         (* 6 6)             (* 10 10))
;: (+           36                   100)
;:                     136


;;;SECTION 1.1.6

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

;: (and (> x 5) (< x 10))

(define (>= x y)
  (or (> x y) (= x y)))

(define (>= x y)
  (not (< x y)))


;;EXERCISE 1.1
;: 10

;: (+ 5 3 4)

;: (- 9 1)

;: (/ 6 2)

;: (+ (* 2 4) (- 4 6))

;: (define a 3)

;: (define b (+ a 1))

;: (+ a b (* a b))

;: (= a b)

;: (if (and (> b a) (< b (* a b)))
;:     b
;:     a)

;: (cond ((= a 4) 6)
;:       ((= b 4) (+ 6 7 a))
;:       (else 25))

;: (+ 2 (if (> b a) b a))

;: (* (cond ((> a b) a)
;: 	 ((< a b) b)
;: 	 (else -1))
;:    (+ a 1))

;;EXERCISE 1.2

(/
  ;; enumerator
  (+
    5
    4
    (-
      2
      (-
        3
        (+
          6
          (/ 4 5 ))))
    )
  ;; denominator
  (*
    3
    (- 6 2)
    (- 2 7))
  )


;;EXERCISE 1.3
;; Define a procedure that takes three numbers as arguments and returns the
;; sum of the squares of the two larger numbers. 
(define (sum-of-sq-two-larger a b c)
  (cond
    ((and (< a b) (< a c)) (sum-of-squares b c))
    ((and (< b a) (< b c)) (sum-of-squares a c))
    (else (sum-of-squares a b))))

;;EXERCISE 1.4
;; Observe that our model of evaluation allows for combinations whose operators are compound expressions. Use this observation to describe the behavior of the following procedure: 
(define
  ;; the preocedure's syntax
  (a-plus-abs-b ;; procedure name
          a b) ;; formal parameters
  ;; the procedure's body specifies the semantics
  ( ;; the operator is a compound expression that evaluates to either '+' or '-' depending on the sign of 'b'
   (if (> b 0) + -) a b))

;;EXERCISE 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;: (test 0 (p))

;; Evaluating '(p)' raises an infinite recursion since procedure 'p' was defined
;; recursively without base case(s).  So, under applicative order the 
;; evaluation of the expression '(test 0 (p))' is never completed because
;; both arguments of 'test'---including '(p)'---must be evaluated before the
;; body of 'test'.  In contrast, under normal order the argument '(p)' would
;; only be evaluated if the '(= x 0)' predicate were false, which is not the
;; case, so the evaluation of '(test 0 (p))' is completed after the '0'
;; consequent is evaluated.

;;;SECTION 1.1.7

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))


;: (sqrt 9)
;: (sqrt (+ 100 37))
;: (sqrt (+ (sqrt 2) (sqrt 3)))
;: (square (sqrt 1000))


;;EXERCISE 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;: (new-if (= 2 3) 0 5)

;: (new-if (= 1 1) 0 5)

;; EXERCISE 1.6
;;
;; Alyssa P. Hacker doesn't see why if needs to be provided as a special form.
;; ``Why can't I just define it as an ordinary procedure in terms of cond?''
;; she asks. Alyssa's friend Eva Lu Ator claims this can indeed be done, and
;; she defines a new version of if:

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; The shortcoming of 'new-if' is that it is a procedure and hence---under
;; applicative order of evaluation---all of its arguments must be evaluated
;; first, therefore if a new procedure is defined recursively by calling
;; itselv in 'else-clause' then evaluating that procedure starts an infinitely
;; recursive process.  To see this redefine 'sqrt-iter' as follows:

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))


;; EXERCISE 1.7
;;
;; The good-enough? test used in computing square roots will not be very
;; effective for finding the square roots of very small numbers. Also, in real
;; computers, arithmetic operations are almost always performed with limited
;; precision. This makes our test inadequate for very large numbers. Explain
;; these statements, with examples showing how the test fails for small and
;; large numbers. An alternative strategy for implementing good-enough? is to
;; watch how guess changes from one iteration to the next and to stop when the
;; change is a very small fraction of the guess. Design a square-root
;; procedure that uses this kind of end test. Does this work better for small
;; and large numbers?

(define (good-enough? guess x)
  (> 0.001
    (abs (/ (- (improve guess x) guess) guess))))

;; EXERCISE 1.8
;;
;; Newton's method for cube roots is based on the fact that if y is an
;; approximation to the cube root of x, then a better approximation is given
;; by the value (x / y^2 + 2 y) / 3

(define (improve-cube guess x)
  (/
    (+
      (/ x (square guess))
      (* 2 guess))
    3))

(define (good-enough-cube? guess x)
  (> 0.001
    (abs (/ (- (improve-cube guess x) guess) guess))))

(define (cubert-iter guess x)
  (if (good-enough-cube? guess x)
          guess
          (cubert-iter (improve-cube guess x)
                     x)))

(define (cubert x) (cubert-iter 1.0 x))

;: my Newton method (Attila)

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))


;: procedures for cube root estimation (Attila)

(define (cubert x)
  (cubert-iter 1.0 x))

(define (cubert-iter guess x)
  (if (cube-good-enough? guess x)
    guess
    (cubert-iter (cube-improve guess x)
               x)))

(define (cube-improve guess x)
  (/ (+ (* 2 guess) (/ x (square guess))) 3))

(define (cube x) (* x x x))

(define (cube-good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))


;;;SECTION 1.1.8

(define (square x) (* x x))

(define (square x) 
  (exp (double (log x))))

(define (double x) (+ x x))


;; As in 1.1.7
(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))


;; Block-structured
(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

;; Taking advantage of lexical scoping
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;;;SECTION 1.2.1

;; Recursive

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;; Factorial (Attila)

(define (my-factorial x)
  (cond ((= x 1) 1)
        (else (* x (my-factorial (- x 1))))))
               

;; Iterative

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

;; Iterative, block-structured (from footnote)
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))


;;EXERCISE 1.9
;; procedure A
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

;; procedure B
(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

;; The first procedure (A) is recursive with deferred operation 'inc',
;; wherease procedure B is iterative with state variables 'a' and 'b'.
;;
;; The processes generated when evaluating (+ 4 5)
;;
;; Under A:
(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9
;;
;; Under B:
(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9

;;EXERCISE 1.10
;; The following procedure computes a mathematical function called Ackermann's
;; function.
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; What are the values of the following expressions?

;; TODO
;; 2^10 because...
(A 1 10)

(A 2 4)

;; 0 because there are three steps of dereference
(A 3 3)

(define (f n) (A 0 n))
;; f(n) = 2 * n    (Attila)

(define (g n) (A 1 n))
;; g(n) = 2 ^ n    (Attila)

(define (h n) (A 2 n))
;; h(n) = ?    (Attila)

(define (k n) (* 5 n n))


;;;SECTION 1.2.2

;; Recursive

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;; Iterative

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))


;; Counting change

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;: (count-change 100)


;; EXERCISE 1.11
;;
;; A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n -
;; 1) + 2f(n - 2) + 3f(n - 3) if n> 3. Write a procedure that computes f by
;; means of a recursive process. Write a procedure that computes f by means of
;; an iterative process.  

;; recursive process
(define (f n)
  (if (< n 3)
    n
    (+
      (f (- n 1))
      (f (- n 2))
      (f (- n 3)))))

;; iterative process
(define (f-it n)
  (define (f-iter cnt f2 f1 f0)
    (if (= cnt n)
      f2
      (f-iter (+ cnt 1) (+ f2 f1 f0) f2 f1)))
  (if (< n 3)
    n
    (f-iter 3 3 2 1)))

;; test both procedures: f-it is much faster at large n
(f 3)
(f-it 3)
(f 4)
(f-it 4)
(f 5)
(f-it 5)
(f 20)
(f-it 20)
(f 30)
(f-it 30)






;; EXERCISE 1.12
;;
;; The numbers at the edge of the triangle are all 1, and each number inside
;; the triangle is the sum of the two numbers above it.35 Write a procedure
;; that computes elements of Pascal's triangle by means of a recursive
;; process. 
;;
;; Binomial formula: n choose k; n indexes rows in Pascal's triangle and k
;; horozontal positions from either side of the triangle.

(define (pascal-tri n k)
  (cond ((or (= 1 k) (= n k))
         1)
        ((or (> 1 k) (< n k))
         0)
        (else (+
                (pascal-tri (- n 1) (- k 1))
                (pascal-tri (- n 1) k)))))


;;;SECTION 1.2.3

;;EXERCISE 1.15
;; The sine of an angle (specified in radians) can be computed by making use
;; of the approximation sin x x if x is sufficiently small, and the
;; trigonometric identity:
;; sin x = 3 sin(x/3) - 4 sin^3(x/3)
;; to reduce the size of the argument of sin. (For purposes of this exercise
;; an angle is considered ``sufficiently small'' if its magnitude is not
;; greater than 0.1 radians.) These ideas are incorporated in the following
;; procedures:

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

;; a.  How many times is the procedure p applied when (sine 12.15) is
;; evaluated?
;;
;; Answer:
;; 5 times because of the logarithmic growth (see below) combined
;; with 12.15 and 0.1 yields approximately 4.37:
(/ (log (/ 12.15 0.1)) (log 3))

;; b.  What is the order of growth in space and number of steps (as a function
;; of a) used by the process generated by the sine procedure when (sine a) is
;; evaluated? 
;;
;; Answer:
;; The process is recursive without branching so the order of growth in space is the
;; same as that in the number of steps.  And the latter is logarithmic because
;; at each step the input angle is divided by the constant 3.

;;;SECTION 1.2.4

;; Linear recursion
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))


;; Linear iteration
(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                (- counter 1)
                (* b product)))) 

;; Logarithmic iteration
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))


;;EXERCISE 1.17
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

;;EXERCISE 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   ??FILL-THIS-IN?? ; compute p'
                   ??FILL-THIS-IN?? ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))


;;;SECTION 1.2.5

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


;;;SECTION 1.2.6

;; prime?

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))


;; fast-prime?

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))        

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))


;;EXERCISE 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;;EXERCISE 1.25
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

;;EXERCISE 1.26
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;;;SECTION 1.3

(define (cube x) (* x x x))

;;;SECTION 1.3.1

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (sum term a next b) ;; Attila
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
    (iter a (term a)))

(define (inc n) (+ n 1))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


;; Using sum

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

;: (sum-cubes 1 10)


(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

;: (sum-integers 1 10)


(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

;: (* 8 (pi-sum 1 1000))


(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

;: (integral cube 0 1 0.01)

;: (integral cube 0 1 0.001)

;; EXERCISE 1.29
;;
;; Simpson's Rule is a more accurate method of numerical integration than the
;; method illustrated above. Using Simpson's Rule, the integral of a function
;; f between a and b is approximated as [...] where h = (b - a)/n, for some
;; even integer n, and yk = f(a + kh). (Increasing n increases the accuracy of
;; the approximation.) Define a procedure that takes as arguments f, a, b, and
;; n and returns the value of the integral, computed using Simpson's Rule. Use
;; your procedure to integrate cube between 0 and 1 (with n = 100 and n =
;; 1000), and compare the results to those of the integral procedure shown
;; above. 

(define (integral-s f a b n)
  (define h (/ (- b a) n))
  (define (add-2h x) (+ x (* 2 h)))
  (* (/ h 3)
     (+
       (f a)
       (* 4 (sum f (+ a h) add-2h b))
       (* 2 (sum f (+ a (* 2 h)) add-2h b))
       (f b))))
    
;; test precision of integral and integral-s
(integral cube 0 1.0 (/ 1.0 10))
(integral-s cube 0 1.0 10) ;; pretty bad
(integral cube 0 1.0 (/ 1.0 100))
(integral-s cube 0 1.0 100) ;; much better
(integral cube 0 1.0 (/ 1.0 1000))
(integral-s cube 0 1.0 1000) ;; still prima
(integral cube 0 1.0 (/ 1.0 10000))
(integral-s cube 0 1.0 10000) ;; strange decline
(integral cube 0 1.0 (/ 1.0 100000))
(integral-s cube 0 1.0 100000) ;; good again


;; EXERCISE 1.30
;;
;; The sum procedure above generates a linear recursion. The procedure can be
;; rewritten so that the sum is performed iteratively. Show how to do this by
;; filling in the missing expressions in the following definition:

(define (sum-iter term a next b)
  (define (iter a result)
    (if (< a b)
        (iter (next a)
              (+
                result
                (term (next a))))
        result))
  (iter a (term a)))

;; test
(sum cube 0 inc 4)
(sum-iter cube 0 inc 4)


;; EXERCISE 1.31
;;
;; a.
;; The sum procedure is only the simplest of a vast number of similar
;; abstractions that can be captured as higher-order procedures.51 Write an
;; analogous procedure called product that returns the product of the values
;; of a function at points over a given range. Show how to define factorial in
;; terms of product. Also use product to compute approximations to using the
;; formula[...]

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

;; b.
;; If your product procedure generates a recursive process, write one that
;; generates an iterative process. If it generates an iterative process, write
;; one that generates a recursive process. 

(define (product-iter term a next b)
  (define (iter a result)
    (if (< a b)
        (iter (next a)
              (*
                result
                (term (next a))))
        result))
  (iter a (term a)))

(define (factorial-iter n)
  (product-iter identity 1 inc n))



;;EXERCISE 1.32
;: (accumulate combiner null-value term a next b)

;;;SECTION 1.3.2

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

(define (plus4 x) (+ x 4))

(define plus4 (lambda (x) (+ x 4)))

;: ((lambda (x y z) (+ x y (square z))) 1 2 3)


;; Using let

(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y)) 
            (- 1 y)))

(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(+ (let ((x 3))
     (+ x (* x 10)))
   x)

(let ((x 3)
      (y (+ x 2)))
  (* x y))

(define (f x y)
  (define a (+ 1 (* x y)))
  (define b (- 1 y))
  (+ (* x (square a))
     (* y b)
     (* a b)))


;;EXERCISE 1.34
;;
;; Suppose we define the procedure

(define (f g)
  (g 2))

;; What happens if we (perversely) ask the interpreter to evaluate the
;; combination (f f)? Explain. 
;;
;; Answer:
;; The interpreter throws the following error message:
;;
; application: not a procedure;
;  expected a procedure that can be applied to arguments
;   given: 2
; [,bt for context]
;;
;; The following happened
;; 1. argument 'f' was bound to formal parameter 'g' in the body of procedure 'f'
;; 2. the definition of 'f' prescribes that the local 'g' procedure is applied to 2
;; 3. now 'f' is bound to 'g', so 'f' is attempted to be applied to 2
;; 4. but this fails because 'f' requires a procedure as argument instead of a number

;: (f square)

;: (f (lambda (z) (* z (+ z 1))))


;;;SECTION 1.3.3

;; Half-interval method

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))


;: (half-interval-method sin 2.0 4.0)

;: (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
;:                       1.0
;:                       2.0)


;; Fixed points

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


;: (fixed-point cos 1.0)

;: (fixed-point (lambda (y) (+ (sin y) (cos y)))
;:              1.0)


(define (sqrt x)
  (fixed-point (lambda (y) (/ x y))
               1.0))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))


;;EXERCISE 1.37
;: (cont-frac (lambda (i) 1.0)
;:            (lambda (i) 1.0)
;:            k)


;;;SECTION 1.3.4

(define (average-damp f)
  (lambda (x) (average x (f x))))

;: ((average-damp square) 10)

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))


;; Newton's method

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)


(define (cube x) (* x x x))

;: ((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))


(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))


;; Fixed point of transformed function

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))


;;EXERCISE 1.40
;: (newtons-method (cubic a b c) 1)


;;EXERCISE 1.41
;: (((double (double double)) inc) 5)


;;EXERCISE 1.42
;: ((compose square inc) 6)


;;EXERCISE 1.43
;: ((repeated square 2) 5)
