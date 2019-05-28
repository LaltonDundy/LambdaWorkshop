id    :- lambda x . x                                 -: 

zero  :- lambda f . lambda x . x                      -:

succ  :- lambda g . lambda f . lambda x . f ( g f x ) -:

one   :- @succ @zero                                  -:

two   :- @succ @one                                   -:

three :- @succ @two                                   -:

plus  :- lambda a . lambda b . a @succ b              -: 

mult  :- lambda a . lambda b . a (@plus b) @zero      -:

exp   :- lambda a . lambda b . b a                    -:

inf   :- (lambda x . x x) (lambda x . x x)            -:

const :- (lambda c . lambda x . c)                    -:

main  :- ! (@const x '(@inf))                         -:
