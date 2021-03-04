#lang hyper-literate racket/base #:no-require-lang #:no-auto-require
@(require scribble-enhanced/doc
          racket/require
          (for-label (subtract-in typed/racket/base type-expander)
                     type-expander
                     phc-toolkit
                     (subtract-in racket/syntax phc-toolkit)
                     phc-toolkit/untyped-only
                     syntax/parse
                     syntax/parse/experimental/template
                     (only-in type-expander prop:type-expander)))
@doc-lib-setup

@title[#:style manual-doc-style
       #:tag "remember"
       #:tag-prefix "(lib multi-id/multi-id.hl.rkt)"
       ]{Implementation of the 
 @racket[multi-id] library}

@(chunks-toc-prefix
  '("(lib multi-id/multi-id.hl.rkt)"))

@author[@author+email["Suzanne Soy" "racket@suzanne.soy"]]

This document describes the implementation of the 
@racketmodname[multi-id] library, using literate
programming. For the library's documentation, see the 
@other-doc['(lib "multi-id/scribblings/multi-id.scrbl")]
document instead.

@section{Syntax properties implemented by the defined @racket[multi-id]}

@chunk[#:save-as prop-te <props>
       (?? (?@ #:property prop:type-expander p-type))]

@chunk[#:save-as prop-me <props>
       (?? (?@ #:property prop:match-expander p-match))
       (?? (?@ #:property prop:match-expander
               (λ (stx) (syntax-case stx ()
                          [(_ . rest) #'(p-match-id . rest)]))))]

@chunk[#:save-as prop-cw <props>
       (?? (?@ #:property prop:custom-write p-write))]

@chunk[#:save-as prop-set! <props>
       #:property prop:set!-transformer
       (?? p-set!
           (λ (_ stx)
             (syntax-case stx (set!)
               [(set! self . rest) (?? p-set! <fail-set!>)]
               (?? [(_ . rest) p-just-call])
               (?? [_ p-just-id]))))]

@chunk[#:save-as maybe-define-type-noexpand <maybe-define-type>
       (?? (tr:define-type name p-type-noexpand #:omit-define-syntaxes))]

@chunk[#:save-as maybe-define-type-expand-once <maybe-define-type>
       (?? (define-type name p-type-expand-once #:omit-define-syntaxes))]

@chunk[#:save-as prop-fallback <props>
       (?@ #:property fallback.prop fallback-value)
       …]

@(module orig racket/base
   (require scribble/manual
            (for-label typed/racket/base))
   (define orig:tr:define-type @racket[define-type])
   (provide orig:tr:define-type))
@(require 'orig)

The multi-id macro defines the identifier @tc[_name] as a
struct with several properties:
@itemlist[
 @item{@racket[prop:type-expander], so that the identifier
  acts as a 
  @tech[#:doc '(lib "type-expander/scribblings/type-expander.scrbl")]{
   type expander}
  
  @(prop-te)

  Optionally, the user can request the type to not be
  expanded, in which case we bind the type expression to a
  temporary type name, using the original
  @orig:tr:define-type from @racketmodname[typed/racket]:

  @(maybe-define-type-noexpand)

  The user can otherwise request that the type expression be
  expanded once and for all. This can be used for
  performance reasons, to cache the expanded type, instead
  of re-computing it each time the @racket[name] identifier
  is used as a type. To achieve that, we bind the expanded
  type to a temporary type name using @racket[define-type]
  as provided by the @racketmodname[type-expander] library:

  @(maybe-define-type-expand-once)

  The two keywords @racket[#:type-noexpand] and 
  @racket[#:type-expand-once] can also be used to circumvent
  issues with recursive types (the type expander would
  otherwise go in an infinite loop while attempting to
  expand them). This behaviour may be fixed in the future,
  but these options should stay so that they can still be
  used for performance reasons.}
  
 @item{@racket[prop:match-expander], so that the identifier
  acts as a 
  @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{
   match expander}
  
  @(prop-me)}
 
 @item{@racket[prop:custom-write], so that the identifier
  can be printed in a special way. Note that this does not
  affect instances of the data structure defined using
  multi-id. It is even possible that this property has no
  effect, as no instances of the structure should ever be
  created, in practice. This feature is therefore likely to
  change in the future.
  
  @(prop-cw)}
 
 @item{@racket[prop:set!-transformer], so that the
  identifier can act as a regular 
  @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{macro},
  as an
  @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{identifier macro}
  and as a
  @seclink["set__Transformers" #:doc '(lib "scribblings/guide/guide.scrbl")]{
   set! transformer}.
  
  @(prop-set!)}

 @item{Any @racket[prop:xxx] identifier can be defined with @racket[#:xxx], if
  so long as the @racket[prop:xxx] identifier is a
  @racket[struct-type-property?].

  @(prop-fallback)}]

The multi-id macro therefore defines @racket[_name] as follows:

@chunk[<multi-id-body>
       (template
        (begin
          <maybe-define-type>
          (define-syntax name
            (let ()
              (struct tmp ()
                <props>)
              (tmp)))))]

@section{Signature of the @racket[multi-id] macro}


@chunk[#:save-as type-expander-kws <type-expander-kws>
       (~optional (~or (~seq #:type-expander p-type:expr)
                       (~seq #:type-noexpand p-type-noexpand:expr)
                       (~seq #:type-expand-once p-type-expand-once:expr)))]

@chunk[#:save-as match-expander-kws <match-expander-kws>
       (~optional (~or (~seq #:match-expander p-match:expr)
                       (~seq #:match-expander-id p-match-id:id)))]

@chunk[#:save-as custom-write-kw <custom-write-kw>
       (~optional (~seq #:custom-write p-write:expr))]

@chunk[#:save-as set!-transformer-kws <set!-transformer-kws>
       (~optional (~or (~seq #:set!-transformer p-set!:expr)
                       :kw-else
                       :kw-set!+call+id))]

@; TODO: maybe we should cache @tc[p-else] and @tc[p-get].

@CHUNK[#:save-as stx-class-kw-else <stx-class-kw-else>
       (define-splicing-syntax-class kw-else
         #:attributes (p-just-set! p-just-call p-just-id)
         (pattern (~seq #:mutable-else p-else)
                  #:with p-just-set! #'#'(set! p-else . rest)
                  #:with p-just-call #'#'(p-else . rest)
                  #:with p-just-id #'#'p-else)
         (pattern (~seq #:else p-else)
                  #:with p-just-set! <fail-set!>
                  #:with p-just-call #'#`(#,p-else . rest)
                  #:with p-just-id #'p-else)
         (pattern (~seq #:mutable-else-id p-else-id)
                  #:with (:kw-else) #'(#:mutable-else #'p-else-id))
         (pattern (~seq #:else-id p-else-id)
                  #:with (:kw-else) #'(#:else #'p-else-id)))]

@; TODO: add #:pattern-expander with prop:pattern-expander, see
@; http://docs.racket-lang.org/syntax/stxparse-patterns.html
@;   #%28def._%28%28lib._syntax%2Fparse..rkt%29._prop~3apattern-expander%29%29
@chunk[#:save-as stx-class-kw-set!+call+id <stx-class-kw-set!+call+id>
       (define-splicing-syntax-class kw-set!+call+id
         (pattern (~seq (~or
                         (~optional (~seq #:set! p-user-set!:expr))
                         (~optional (~or (~seq #:call p-user-call:expr)
                                         (~seq #:call-id p-user-call-id:id)))
                         (~optional (~or (~seq #:id p-user-id:expr)
                                         (~seq #:id-id p-user-id-id:expr))))
                        …)
                  #:attr p-just-set!
                  (and (attribute p-user-set!) #'(p-user-set! stx))
                  #:attr p-just-call
                  (cond [(attribute p-user-call)
                         #'(p-user-call stx)]
                        [(attribute p-user-call-id)
                         #'(syntax-case stx ()
                             [(_ . rest) #'(p-user-call-id . rest)])]
                        [else #f])
                  #:attr p-just-id
                  (cond [(attribute p-user-id) #'(p-user-id stx)]
                        [(attribute p-user-id-id) #'#'p-user-id-id]
                        [else #f])))]

@chunk[#:save-as fail-set! <fail-set!>
       #'(raise-syntax-error
          'self
          (format "can't set ~a" (syntax->datum #'self)))]
@chunk[#:save-as prop-keyword <prop-keyword-syntax-class>
       (define-syntax-class prop-keyword
         (pattern keyword:keyword
                  #:with prop (datum->syntax #'keyword
                                             (string->symbol
                                              (string-append
                                               "prop:"
                                               (keyword->string
                                                (syntax-e #'keyword))))
                                             #'keyword
                                             #'keyword)
                  #:when (eval #'(struct-type-property? prop))))]

@chunk[#:save-as fallback-kw <fallback-kw>
       (~seq fallback:prop-keyword fallback-value:expr)]

The @tc[multi-id] macros supports many options, although
not all combinations are legal. The groups of options
specify how the @racket[_name] identifier behaves as a type
expander, match expander, how it is printed with 
@racket[prop:custom-write] and how it acts as a 
@racket[prop:set!-transformer], which covers usage as a
macro, identifier macro and actual @racket[set!]
transformer.

@chunk[<multi-id>
       (begin-for-syntax
         <stx-class-kw-else>
         <stx-class-kw-set!+call+id>
         <prop-keyword-syntax-class>)
       (define-syntax/parse (define-multi-id name:id
                              (~or <type-expander-kws>
                                   <match-expander-kws>
                                   <custom-write-kw>
                                   <set!-transformer-kws>
                                   <fallback-kw>)
                              …)
         <multi-id-body>)]

These groups of options are detailed below:

@itemlist[
 @item{The @racket[#:type-expander], 
  @racket[#:type-noexpand] and @racket[#:type-expand-once]
  options are mutually exclusive.
  
  @(type-expander-kws)}
  
 @item{The @racket[#:match-expander] and @racket[#:match-expander-id]
  options are mutually exclusive.
  
  @(match-expander-kws)}

 @item{The @racket[#:custom-write] keyword can always be used

  @(custom-write-kw)}

 @item{The @racket[prop:set!-transformer] can be specified
  as a whole using @racket[#:set!-transformer], or using one
  of @racket[#:else], @racket[#:else-id], 
  @racket[#:mutable-else] or @racket[#:mutable-else-id], or
  using some combination of @racket[#:set!], 
  @racket[#:call] (or @racket[#:call-id]) and
  @racket[#:id].

  @(set!-transformer-kws)

  More precisely, the @racket[kw-else] syntax class accepts
  one of the mutually exclusive options @racket[#:else], 
  @racket[#:else-id], @racket[#:mutable-else] and 
  @racket[#:mutable-else-id]:
  
  @(stx-class-kw-else)

  The @racket[kw-set!+call+id] syntax class accepts
  optionally the @racket[#:set!] keyword, optionally one of
  @racket[#:call] or @racket[#:call-id], and optionally the
  @racket[#:id] keyword.

  @(stx-class-kw-set!+call+id)

  When neither the @racket[#:set!] option nor 
  @racket[#:set!-transformer] are given, the @racket[_name]
  identifier acts as an immutable object, and
  cannot be used in a @racket[set!] form. If it appears as
  the second element of a @racket[set!] form, it raises a
  syntax error:

  @(fail-set!)}

 @item{As a fallback, for any @racket[#:xxx] keyword, we check whether a
  corresponding @racket[prop:xxx] exists, and whether it is a
  @racket[struct-type-property?]:

  @(fallback-kw)

  The check is implemented as a syntax class:

  @(prop-keyword)}]

@section{Tests for @racket[multi-id]}

@chunk[<test-multi-id>
       (define (p1 [x : Number]) (+ x 1))
       
       (define-type-expander (Repeat stx)
         (syntax-case stx ()
           [(_ t n) #`(List #,@(map (λ (x) #'t)
                                    (range (syntax->datum #'n))))]))
       
       (define-multi-id foo
         #:type-expander
         (λ (stx) #'(List (Repeat Number 3) 'x))
         #:match-expander
         (λ (stx) #'(vector _ _ _))
         #:custom-write
         (λ (self port mode) (display "custom-write for foo" port))
         #:set!-transformer
         (λ (_ stx)
           (syntax-case stx (set!)
             [(set! self . _)
              (raise-syntax-error 'foo (format "can't set ~a"
                                               (syntax->datum #'self)))]
             [(_ . rest) #'(+ . rest)]
             [_ #'p1])))
       
       (check-equal? (ann (ann '((1 2 3) x) foo)
                          (List (List Number Number Number) 'x))
                     '((1 2 3) x))
       
       (code:comment "(set! foo 'bad) should throw an error here")
       
       (let ([test-match (λ (val) (match val [(foo) #t] [_ #f]))])
         (check-equal? (test-match #(1 2 3)) #t)
         (check-equal? (test-match '(1 x)) #f))
       
       (check-equal? (foo 2 3) 5)
       (check-equal? (map foo '(1 5 3 4 2)) '(2 6 4 5 3))]

It would be nice to test the @tc[(set! foo 'bad)] case, but grabbing the
compile-time error is a challenge (one could use @tc[eval], but it's a bit heavy
to configure).

Test with @tc[#:else]:

@chunk[<test-multi-id>
       (begin-for-syntax
         (define-values
           (prop:awesome-property awesome-property? get-awesome-property)
           (make-struct-type-property 'awesome-property)))
       
       (define-multi-id bar-id
         #:type-expander
         (λ (stx) #'(List `,(Repeat 'x 2) Number))
         #:match-expander
         (λ (stx) #'(cons _ _))
         #:custom-write
         (λ (self port mode) (display "custom-write for foo" port))
         #:else-id p1
         #:awesome-property 42)

       (check-equal? (ann (ann '((x x) 79) bar)
                          (List (List 'x 'x) Number))
                     '((x x) 79))
       
       (code:comment "(set! bar 'bad) should throw an error here")
       
       (let ([test-match (λ (val) (match val [(bar-id) #t] [_ #f]))])
         (check-equal? (test-match '(a . b)) #t)
         (check-equal? (test-match #(1 2 3)) #f))

       (let ([f-bar-id bar-id])
         (check-equal? (f-bar-id 6) 7))
       (check-equal? (bar-id 6) 7)
       (check-equal? (map bar-id '(1 5 3 4 2)) '(2 6 4 5 3))

       (require (for-syntax rackunit))
       (define-syntax (check-awesome-property stx)
         (syntax-case stx ()
           [(_ id val)
           (begin (check-pred awesome-property?
                              (syntax-local-value #'id (λ _ #f)))
                  (check-equal? (get-awesome-property
                                 (syntax-local-value #'id (λ _ #f)))
                                (syntax-e #'val))
                  #'(void))]))
       (check-awesome-property bar-id 42)]

@chunk[<test-multi-id>
       (define-multi-id bar
         #:type-expander
         (λ (stx) #'(List `,(Repeat 'x 2) Number))
         #:match-expander
         (λ (stx) #'(cons _ _))
         #:custom-write
         (λ (self port mode) (display "custom-write for foo" port))
         #:else #'p1)
       
       (check-equal? (ann (ann '((x x) 79) bar)
                          (List (List 'x 'x) Number))
                     '((x x) 79))
       
       (code:comment "(set! bar 'bad) should throw an error here")
       
       (let ([test-match (λ (val) (match val [(bar) #t] [_ #f]))])
         (check-equal? (test-match '(a . b)) #t)
         (check-equal? (test-match #(1 2 3)) #f))
       
       (check-equal? (bar 6) 7)
       (check-equal? (map bar '(1 5 3 4 2)) '(2 6 4 5 3))]

@section{Conclusion}

@chunk[<*>
       (require (only-in type-expander prop:type-expander define-type)
                (only-in typed/racket [define-type tr:define-type])
                phc-toolkit/untyped
                (for-syntax phc-toolkit/untyped
                            racket/base
                            racket/syntax
                            syntax/parse
                            syntax/parse/experimental/template
                            (only-in type-expander prop:type-expander)))
       (provide define-multi-id)
           
       <multi-id>
         
       (module* test-syntax racket/base
         (provide tests)
         (define tests #'(begin <test-multi-id>)))]
