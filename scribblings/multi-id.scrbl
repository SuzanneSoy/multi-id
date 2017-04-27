#lang scribble/manual
@require[@for-label[multi-id
                    racket/base
                    racket/contract/base]
         scribble-enhanced]

@title{Polyvalent identifiers with @racket[multi-id]}
@author{Georges Dupéron}

@defmodule[multi-id]

This library is implemented using literate programming. The
implementation details are presented in the 
@other-doc['(lib "multi-id/multi-id.hl.rkt")]
document.

This package helps defining identifiers with many different meanings in
different contexts. An identifier can be given a meaning:

@itemlist[
 @item{As a type expander @racket[(: foo (Listof (ident arg …)))]
  (see @racketmodname[type-expander #:indirect])}
 @item{As a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{
   match expander}}
 @item{As a @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{macro}
  (i.e. when it appears in the first position of a form)}
 @item{As a simple identifier (i.e. used as a variable, via an
  @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{identifier macro})}
 @item{As a @racket[set!] subform}]

@defform[(define-multi-id name
           maybe-type-expander
           maybe-match-expander
           maybe-maybe-set!+call+id
           fallback-clause ...)
         #:grammar ([maybe-type-expander
                     (code:line)
                     (code:line #:type-expander proc)]
                    [maybe-match-expander
                     (code:line)
                     (code:line #:match-expander proc)]
                    [maybe-set!+call+id
                     (code:line)
                     (code:line #:set!-transformer proc)
                     (code:line else)
                     (code:line maybe-set! maybe-call maybe-id)]
                    [maybe-set!
                     (code:line #:set! proc)]
                    [maybe-call
                     (code:line #:call proc)
                     (code:line #:call-id identifier)]
                    [maybe-id
                     (code:line #:id proc)
                     (code:line #:id-id identifier)]
                    [else
                     (code:line #:else-id identifier)
                     (code:line #:mutable-else-id identifier)
                     (code:line #:else identifier-expression)
                     (code:line #:mutable-else identifier-expression)]
                    [fallback-clause
                     (code:line #:??? expression)]
                    [??? "any struct-type-property?, without the prop:"])]{
 Defines @racket[name] as a 
 @tech[#:doc '(lib "type-expander/scribblings/type-expander.scrbl")]{
  type expander}, 
 @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{
  match expander}, 
 @seclink["set__Transformers" #:doc '(lib "scribblings/guide/guide.scrbl")]{
  set! transformer},
 @tech[#:doc '(lib
"scribblings/guide/guide.scrbl")]{identifier macro}, a
 regular 
 @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{macro},
 some other concepts, each implemented with an arbitrary
 @racket[struct-type-property?],
 or combinations of those.

 In the syntax described above, each @racket[proc] should
 be a transformer procedure accepting a single 
 @racket[syntax?] argument and returning a @racket[syntax?]
 value, i.e. the signature of each @racket[proc] should be 
 @racket[(syntax? . -> . syntax?)]. Each 
 @racket[identifier] should be an identifier, and each 
 @racket[identifier-expression] should be a compile-time
 expression producing an identifier.

 The following options are currently supported:
 @specsubform[#:unwrap (#:??? expression)
              #:grammar
              ([??? "any struct-type-property?, without the prop:"])]{
  The identifier @racket[name] is a struct with the @racket[prop:???] struct
  type property, using the given @racket[_expression]

  In the syntax above, @racket[#:???] is only a placeholder; any keyword can be
  used, so long as prefixing the keyword's name with @racket[prop:] gives an
  identifier which is a @racket[struct-type-property?].}
 @specsubform[#:unwrap (#:type-expander proc)]{ The
  identifier @racket[name] acts as a 
  @tech[#:doc '(lib "type-expander/scribblings/type-expander.scrbl")]{
   type expander}, using the given @racket[proc] which
  should return the syntax for a type.}
 @specsubform[#:unwrap (#:match-expander proc)]{
  The identifier @racket[name] acts as a 
  @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{
   match expander}, using the given @racket[proc] which
  should return the syntax for a match pattern.}
 @specsubform[#:unwrap (#:set!-transformer proc)]{
  The identifier @racket[name] acts as a 
  @seclink["set__Transformers" #:doc '(lib "scribblings/guide/guide.scrbl")]{
   set! transformer}, using the given @racket[proc] which
  should return a @racket[syntax?] value. The @racket[proc]
  is used both when @racket[name] is used in a 
  @racket[set!] form, and when it is used as a macro or
  identifier macro.}
 @specsubform[#:unwrap (#:set! proc)]{
  The identifier @racket[name] acts as a 
  @seclink["set__Transformers" #:doc '(lib "scribblings/guide/guide.scrbl")]{
   set! transformer} when it is used in a @racket[set!]
  form, using the given @racket[proc] which should return a
  @racket[syntax?] value.

  The @racket[proc] is used only when @racket[name] is used
  in a @racket[set!] form, but not when it is used as a
  macro or identifier macro. In these cases, @racket[#:call] and 
  @racket[#:id], respectively, are used instead.

  If @racket[#:id] is not specified, but @racket[name] is used
  as an identifier macro, the @racket[exn:fail:syntax]
  exception is raised. If @racket[#:call] is not specified,
  but @racket[name] is used as a regular macro, the 
  @racket[exn:fail:syntax] exception is raised.}
 @specsubform[#:unwrap (#:call proc)]{
  The identifier @racket[name]
  acts as a macro when it appears in the first position of
  a form, using the given @racket[proc] which should return
  a @racket[syntax?] value.

  The @racket[proc] is used only when @racket[name] is used
  as a regular macro, but not when it is used as an
  identifier macro or when it is used in a @racket[set!]
  form. In these cases, @racket[#:id] and @racket[#:set!],
  respectively, are used instead.

  If @racket[#:set!] is not specified, but @racket[name] is
  used in a @racket[set!] form, the @racket[exn:fail:syntax]
  exception is raised. If @racket[#:id] is not specified, but
  @racket[name] is used as an identifier macro, the
  @racket[exn:fail:syntax] exception is raised.}
 @specsubform[#:unwrap (#:call-id identifier)]{
  The identifier @racket[name]
  acts as a macro when it appears in the first position of a
  form. The occurrence of @racket[name] is replaced by the
  given @racket[identifier], which should either be a
  function or a macro.

  When @racket[name] is used as a macro, i.e. in a form
  like @racket[(name . args)], the whole form is replaced
  by @racket[(identifier . args)]. If the identifier is
  itself a regular macro, then the whole 
  @racket[(identifier . args)] form is expanded.

  The @racket[identifier] is used only when @racket[name]
  is used as a regular macro, not when it is used as an
  identifier macro or as a @racket[set!] transformer.
  In these cases, @racket[#:id] and @racket[#:set!],
  respectively, are used instead.

  If @racket[#:set!] is not specified, but @racket[name] is
  used in a @racket[set!] form, the @racket[exn:fail:syntax]
  exception is raised. If @racket[#:id] is not specified, but
  @racket[name] is used as an identifier macro, the
  @racket[exn:fail:syntax] exception is raised.}
 
 @specsubform[#:unwrap (#:id proc)]{
  The identifier @racket[name] acts as an 
  @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{identifier macro},
  using the given @racket[proc] which should return a 
  @racket[syntax?] value.
   
  The  @racket[proc] is used only when @racket[name] is used
  as an identifier macro, but not when it appears in the
  first position of a form, nor when it is used in a 
  @racket[set!] form. In these cases, @racket[#:call] and
  @racket[#:set!], respectively, are used instead.

  If @racket[#:set!] is not specified, but @racket[name]
  is used in a @racket[set!] form, the @racket[exn:fail:syntax]
  exception is raised. If @racket[#:call] is not specified, but
  @racket[name] is used as a regular macro, the
  @racket[exn:fail:syntax] exception is raised.}

 @specsubform[#:unwrap (#:id-id proc)]{
  The identifier @racket[name] acts as an 
  @tech[#:doc '(lib
"scribblings/guide/guide.scrbl")]{identifier macro}. The
  occurrence of @racket[name] is replaced by the given 
  @racket[identifier]. If the @racket[identifier] is itself
  an identifier macro, it is expanded again.
  
  The  @racket[identifier] is used only when @racket[name]
  is used as an identifier macro, but not when it appears
  in the first position of a form, nor when it is used in a 
  @racket[set!] form. In these cases, @racket[#:call] and
  @racket[#:set!], respectively, are used instead.

  If @racket[#:set!] is not specified, but @racket[name] is
  used in a @racket[set!] form, the @racket[exn:fail:syntax]
  exception is raised. If @racket[#:call] is not specified,
  but @racket[name] is used as a regular macro,
  the @racket[exn:fail:syntax] exception is raised.}
 
 @specsubform[#:unwrap (#:else-id identifier)]{
  The identifier @racket[name]
  acts as a regular macro and as an identifier macro. The
  occurrence of @racket[name] is replaced by the given 
  @racket[identifier], which should either be a function, or
  be both a macro and an identifier macro at the same time.

  When @racket[name] is used as an identifier macro, it is
  replaced by @racket[identifier]. If the 
  @racket[identifier] is itself an identifier macro, then it
  is expanded.

  When @racket[name] is used as a macro, i.e. in a form
  like @racket[(name . args)], the whole form is replaced by
  @racket[(identifier . args)]. If the identifier is itself
  a regular macro, then the whole 
  @racket[(identifier . args)] form is expanded.
   
  The @racket[identifier] is not used when @racket[name] is
  used in a @racket[set!] form, instead the 
  @racket[exn:fail:syntax] exception is raised.}
 
 @specsubform[#:unwrap (#:mutable-else-id identifier)]{
  The identifier @racket[name]
  acts as a regular macro, as an identifier macro and as a 
  @racket[set!] transformer. In all three cases, the
  occurrence of @racket[name] is replaced by the given 
  @racket[identifier], which should either be a function, or
  be a macro and an identifier macro and a @racket[set!]
  transformer at the same time.

  This option works like @racket[#:else-id], except that 
  @racket[name] can also be used in a @racket[set!] form.

  When @racket[name] is used in a @racket[set!] form like 
  @racket[(set! name . vals)], the whole form is replaced
  by @racket[(set! identifier . vals)]. If the identifier is
  itself a @racket[set!] transformer, then the whole 
  @racket[(set! identifier . vals)] form is expanded.}
 
 @specsubform[#:unwrap (#:else identifier-expression)]{
  The identifier @racket[name]
  acts as a regular macro and as an identifier macro. The
  occurrence of @racket[name] is replaced by the result of
  the compile-time expression 
  @racket[identifier-expression], which should either
  produce the syntax for a function, or the syntax for an
  identifier which is both a macro and an identifier macro
  at the same time.

  It is equivalent to @racket[#:else-id], except that the
  identifier is not constant, but is instead produced by 
  @racket[identifier-expression]. Note that 
  @racket[identifier-expression] is not a transformer
  function (it will not be able to access the original
  syntax). In other words, the compile-time contract for 
  @racket[identifier-expression] is @racket[syntax?], not 
  @racket[(syntax? . -> . syntax?)].

  The @racket[identifier-expression] is not used when 
  @racket[name] is used in a @racket[set!] form, instead the
  @racket[exn:fail:syntax] exception is raised.}
 
 @specsubform[#:unwrap (#:mutable-else identifier-expression)]{
  The identifier @racket[name] acts as a regular macro, as
  an identifier macro and as a @racket[set!] transformer. In
  all three cases, the occurrence of @racket[name] is
  replaced by the result of the compile-time expression 
  @racket[identifier-expression], which should either
  produce the syntax for a mutable identifier containing a
  function, or the syntax for an identifier which is a macro
  and an identifier macro and a @racket[set!] transformer at
  the same time.

  It is equivalent to @racket[#:mutable-else-id], except
  that the identifier is not constant, but is instead
  produced by @racket[identifier-expression]. Note that 
  @racket[identifier-expression] is not a transformer
  function (it will not be able to access the original
  syntax). In other words, the compile-time contract for 
  @racket[identifier-expression] is @racket[syntax?], not 
  @racket[(syntax? . -> . syntax?)].}}