---
layout: page
title: Info
permalink: /info/
---

This is a compendium of *solved problems* centered on the *principles computer programming*.  All programming exercises were taken from [Structure and Interpretation of Computer Programs][book] (MIT Press), "widely considered a classic text in computer science"---[Wikipedia][wikipedia].  The book, [as it states][ch1], uses the *Scheme* dialect of the *Lisp* language "as the framework for [...] discussion of programming" because "the language possesses unique features that make it an excellent medium for studying important programming constructs and data structures and for relating them to the linguistic features that support them".

### Notation

#### 1. Prose

>The text of problems, copied from the book, appear in "blockquotes" such as this one.  One kind of exception is the $$\LaTeX$$ `equation` environment, another is the code block (see below).

Solutions are typeset as normal text.

#### 2. Code

{% highlight scheme %}
;; The code appears in blocks like this one, both for problems and solutions.
;; Sometimes a -> prompt is written to distinguish between an <expression>
;; given to the interpreter and the <answer> resulting from the evaluation of
;; <expression>:
-> <expression>
<answer>
{% endhighlight %}

`Inline code` is used to refer to computational objects outside code blocks.

[book]: https://www.mitpress.mit.edu/sicp/full-text/book/book.html
[wikipedia]: https://en.wikipedia.org/wiki/Structure_and_Interpretation_of_Computer_Programs
[ch1]: https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-9.html#%_chap_1
