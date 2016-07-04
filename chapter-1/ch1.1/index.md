---
layout: page
title: Chapter 1.1
---

### Exercise 1.1

>Below is a sequence of expressions. What is the result printed by the interpreter in response to each expression? Assume that the sequence is to be evaluated in the order in which it is presented.

{% highlight scheme %}
10
{% endhighlight %}
`10`

{% highlight scheme %}
(+ 5 3 4)
{% endhighlight %}
`12`

{% highlight scheme %}
(- 9 1)
{% endhighlight %}
`8`

{% highlight scheme %}
(/ 6 2)
{% endhighlight %}
`3`

{% highlight scheme %}
(+ (* 2 4) (- 4 6))
{% endhighlight %}
`6`

{% highlight scheme %}
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
{% endhighlight %}
`19`

{% highlight scheme %}
(= a b)
{% endhighlight %}
`#f`

{% highlight scheme %}
(if (and (> b a) (< b (* a b)))
    b
    a)
{% endhighlight %}
`4`

{% highlight scheme %}
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
{% endhighlight %}
`16`

{% highlight scheme %}
(+ 2 (if (> b a) b a))
{% endhighlight %}
`6`

{% highlight scheme %}
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
{% endhighlight %}
`16`


### Exercise 1.2

>Translate the following expression into prefix form
$$
\begin{equation}
\frac{5 + 4 + (2 - (3 - (6 + \frac{4}{3})))}{3 (6 - 2) (2 - 7)}
\end{equation}
$$

{% highlight scheme %}
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
{% endhighlight %}
Evaluating the expression yields `-37/150`.  This is because the enumerator evaluates to `74/5` and the denominator to `-60`.  All numerals are specified as integer as if `(/ (- 37) 150)` had been given.  In contrast, the expression `(/ (- 37.0) 150)` evaluates to `0.24666666666666667`.


### Exercise 1.3

>Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers. 

Prerequisites---as in the book
{% highlight scheme %}
(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))
{% endhighlight %}

A solution
{% highlight scheme %}
(define (sum-of-sq-two-larger a b c)
  (cond
    ((and (< a b) (< a c)) (sum-of-squares b c))
    ((and (< b a) (< b c)) (sum-of-squares a c))
    (else (sum-of-squares a b))))
{% endhighlight %}