# properties-types-tests
This is an introduction to the property based view 
of a code base. 

Look around!  Let me know if I should change an explanation.
## Usage

`stack build ` will create the code, but generating the context
stuff is harder.  The main file is [slidex.tex](./presentation/slidex.tex)

I build it with texworks or auctex mode.  It uses MetaPost.  

[slidex.pdf](./presentation/slidex.pdf) is the output file.


## How to run tests

```
stack test 
```

Will run the tests that the presentation is discussing.  

## Other places to find info about properties.

* [Haskell as a Theorem Proover](http://goto.ucsd.edu/~rjhala/liquid/haskell/blog/blog/2016/09/18/refinement-reflection.lhs/)
* [Deep Look into Properties and Types with SPJ](https://pdfs.semanticscholar.org/8dd2/c7c64451e6dda5d72351ef5c75d091352e05.pdf)
  * I really like the typed printf!
* [Programming Foundations] (https://www.cs.cmu.edu/~rwh/pfpl/2nded.pdf)
  * He is no Haskell fan but who cares, Harper has a great introduction to the notation, terminology and techniques
    that you will see over and over in papers and thesis.
* [Hasochism](https://personal.cis.strath.ac.uk/conor.mcbride/pub/hasochism.pdf)
  * Learn all the Haskell type tricks in one paper
