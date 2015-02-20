(* Buster Testacles and his infeasibly large Monad ... or is it 
   Testacles Monad and his infeasibly large Buster ... or is it
   Monad Buster and his infeasibly large Testicles ... ?

   You see, it's worth taking some trouble to get the arguments in the
   right order, so that we can use currying and type-directed partial
   evaluation to compose monadic representations at the same time as
   we compose parsers for those types. And, if we can compose the
   types, then we should be able to compose the semantics too.

   There are also "fringe benefits" to be had --- decidable
   Higher-Order Unification, for example. That sounds useful. ["Higher
   Order Unification Revisited: Complete Sets of Transformations", by
   Wayne Snyder and Jean H. Gallier] It might make HOU decidable for
   any language we interpret in a monad. If so, then that ought to
   make quite a lot of stuff decidable. Peano Arithmetic (PA) for
   example. Then Ladies will be able to understand the proofs in the
   Arithmetica of Diophantus.

   The Arithmetica of Diophantus was written by a Woman. The reason I
   think I know this is that little men, e.g. Heath, don't seem to
   understand it. I know this because Heath asserts as a fact, some
   statement to the effect that ``Diophantus only used a single
   variable in his problems because he couldn't conceive of the notion
   "more than one variable"''.  _Even though_ he observes that
   Diophantus takes great pains to cast problems in two or more
   unknowns into monadic form.

   Heath apparently sees no reason to explain to his readers this
   utterly incredible discovery he has made: which is, how it is that
   the "man" who had apparently invented the notion of using a
   non-numerical symbol, i.e., a _variable,_ to represent an unknown,
   and who had proved dozens and dozens of theorems in arithmetic and
   whose examples frequently used six digit numbers, couldn't
   _conceive_ of the notion of "one variable" generalisng to "two
   variables" or "three variables".

   Heath thus seems to think Diophantus stupid. Yet Heath knows at the
   same time that he (Heath, I mean!) doesn't know how Diophantus
   proved "the porisms" which are referred to throughout the text. It
   doesn't seem to occur to Heath that the author might have had a
   _reason_ for not using more than one unknown value in the
   equations, and that that reason might have something to do with the
   unknown (to Heath) proof methods in the porisms. Maybe the machine,
   "for technical reasons", couldn't automatically generate proofs for
   propositions with more than one hypothetical?

   This is what I see as typical of how little men treat the thought
   of Women when it is so far advanced of their own that they cannot
   even recognise it as thought. They are at best patronising (see
   e.g. Babbage's comments on the work of Ada Augusta Lovelace), or
   they ridicule it. Perhaps that's better than them recognising that
   it _is_ thought, but not being able to understand it, because then
   they get angry and all they can think of doing is trying to insult
   her.

   Now if there is any reader who is scoffing "... and of course both
   Diophantus and Heath would have been completely familiar with
   Eilenberg and Moore, not to mention Moggi!" Well, I won't complain.
   At least you're not insulting me. But if you want to learn
   something, perhaps for the first time in your life, then look up
   what Aristotle writes on the theory of proportionals, in particular
   on the proposition Proportionals Alternate (PA) which is Euclid's
   Prop. V.16. It seems to be about using translations between
   languages to apply the same proof in three different domains:
   logic, arithmetic and geometry.

   But I have to agree with Wadler on one thing: I'm also a fan of
   John Reynolds. What I love about the man is that he writes to
   explain things, not just because he thinks he can make himself look
   clever. And he doesn't make himself look clever, because he
   explains things so well that he makes what he's writing about seem
   utterly trivial. Perhaps that's why so few seem to have heard of
   him, and also why hardly anyone seems to have really read anything
   he's written.

   This is a bit of a problem, because it's _systemic_. There's a
   mechanism in "academia" which consistently acts against anyone who
   thinks and writes clearly, and that means _anyone_ who does work of
   any real lasting value. As Leonard Cohen never dared to say,
   everybody knows that the mediocre is the enemy of the best, but how
   can they know that, really, when even the mediocre is perpetually
   swamped by the utterly useless?

   It doesn't take an academic "genius" to explain what is the
   mechanism either. The problem is _trade_. Human reason is not a
   least-fixedpoint, so it's not effective reason: it's co-effective
   reason, and a greatest-fixedpoint. So actual Human knowledge is
   inherently, necessarily, co-operative. Therefore, if you take
   people whose responsibility is the acquisition and dissemination of
   knowledge, and you force them to compete against each other in "the
   race whose prize is `Daily Bread'" then they are unable to
   co-operate without losing "the prize". Consequently, the good ones,
   who co-operate, and who actually know something, drop out, and the
   winners are ... can you guess?

   Well, by a truly remarkable co-incidence, they all turn out to be
   men who don't have time to read what other men write because
   they're far too busy writing things for those other men to not read
   ...  Now how long can this sort of thing go on before somebody
   cottons on to the fact that what's being published is, well, less
   than mediocre, shall we say? And what's going to happen then? I
   daresay there are a lot of very clever schemes that have been
   thought up to deal with it, so we needn't worry about anything,
   need we ...?

*)

signature Monad =
sig
   type 'a M
   val unit : ('a -> 'b) -> 'a -> 'b M
   val bind : 'a M -> ('a -> 'b M) -> 'b M
   val show : ('a -> 'b) -> 'a M -> 'b
end

signature Interpreter =
sig
   type term
   type result
   val eval : term -> result
end

signature Value =
sig
   type value
   type result
   val showval : value -> result
   val errval : string * string -> value
end

signature Evaluator =
sig
   type environment
   type term
   type value
   type 'a M
   val interp : term -> environment -> value M
end

signature Environment =
sig
   eqtype name
   type value
   type environment
   val lookup : environment -> name -> value
   val bind : environment -> name * value -> environment
   val null : environment
end

functor ListEnvironment
   (eqtype name
    type value
    val error : name -> value)
   :> Environment
      where type name = name
        and type value = value =
struct
   type name = name
   type value = value
   type environment = (name * value) list
   local fun lookup e n =
            case List.find (fn (n',_) => n' = n) e
              of NONE => error n
               | SOME (_,v) => v
         fun bind e p = p::e
   in
      val lookup : environment -> name -> value =
           lookup
      val bind : environment -> name * value -> environment =
           bind
      val null = []
   end
end

structure InterpI =
struct
   type name = string
   structure MonadI :> Monad =
   struct (* This way you see more clearly that the Monad is just a type function *)
      type 'a M = 'a
      val unit : ('a -> 'b) -> 'a -> 'b M
          = fn f => fn x => f x
      val bind : 'a M -> ('a -> 'b M) -> 'b M
          = fn x => fn f => f x
      val show : ('a -> 'b) -> 'a M -> 'b
          = fn f => fn x => f x
   end
   datatype value =
       Wrong
     | Num of int
     | Fun of value -> value MonadI.M
   datatype term =
       Var of name
     | Con of int
     | Add of term * term
     | Lam of name * term
     | App of term * term
   structure Value
      :> Value
          where type value = value
            and type result = string =
   struct
      local
         fun showval Wrong = "<wrong>"
           | showval (Num i) = Int.toString i
           | showval (Fun _) = "<fn>"
         fun errval (_,_) = Wrong
      in
         type value = value
         type result = string
         val showval : value -> result =
            showval
         val errval : string * string -> value =
            errval
      end
   end
   structure Env : Environment = 
      ListEnvironment (type name = name
                       type value = Value.value
                       val error : name -> value =
                          fn n => Value.errval ("bind",n))
   structure Evaluator
      :> Evaluator
            where type value = value 
              and type 'a M = 'a MonadI.M
              and type environment = Env.environment
              and type term = term =
   struct
      type environment = Env.environment
      type term = term
      type value = value
      type 'a M = 'a MonadI.M
      local
         open MonadI
         fun add (Num i) (Num j) = Num (i + j)
           | add _ _ = Value.errval ("Add","wrong type(s)")
         fun app (Fun k) a = k a
           | app _ _ = unit Value.errval ("App","wrong type")
         fun interp (Var x) e = unit (Env.lookup e) x
           | interp (Con i) e = unit Num i
           | interp (Add (u,v)) e =
                         bind (interp u e) (fn a =>
                         bind (interp v e) (fn b =>
                         unit (add a) b))
           | interp (Lam (x,v)) e =
                          unit Fun (fn a =>
                                      interp v (Env.bind e (x,a)))
           | interp (App (u,v)) e = 
                          bind (interp u e) (fn a =>
                          bind (interp v e) (fn b =>
                          app a b))
      in
         val interp : term -> environment -> value M
            = interp
      end
   end
   fun eval t =
      let val env = Env.null
          val m = Evaluator.interp t  env
      in MonadI.show  Value.showval m
      end
end


(*             Wadler calls this "the standard meta-circular
               interpreter". But Reynolds, who coined the phrase, might
               not agree, because this interpreter doesn't
               interpret itself. This is because it doesn't
               represent the abstract syntax and deconstruct it.

               It might be argued that this is merely a nicety, but
               consider the questions Wadler asks, such as "How do we
               compose interpreters in a monad?" And "Can we interpret
               a call-by-need interpreter in a monad?" If the
               interpreters really were meta-circular then the answers
               to these questions would obviously be positive.

               And Wadlers interpreters aren't modular, as he
               claims. They're all one big amorphous blob.

               If you now go and carefully read Reynolds' paper
               "Definitional Interpreters for Higher-Order Programming
               Languages" in Higher-Order and Symbolic Computation, 11,
               363–397 (1998), you will see that the treatment he
               gives there is far, far superior to the one Wadler
               gives, some twenty years later.

               Note, for example, how Reynolds uses recursive symbolic
               _environments_ to implement fixedpoint combinators, and
               how they dissolve in the first-order translation (p.
               381).  The resulting first-order meta-circular
               interpreter is one of the most beautiful 20 lines of
               code I've ever seen: it is just three lambda
               expressions (excluding the trivial wrapper function
               interpret) and one pair of these---eval and apply---are
               mutually recursive. The function eval also calls the
               function get, which is (simply) recursive. One final
               interesting point to note is that eval takes _two_
               arguments: a value, and an environment, which is a kind
               of _state._

               If anyone can show me more recent work that uses this
               idea, with or without attribution, I would be very
               interested to hear about it. I have never come across
               the idea mentioned anywhere else, and it is what a
               mathematician might call "highly non-obvious".

               Another notable feature of Reynolds' treatment is that
               he uses abstract syntax as an informal type
               discipline. All the values used in definitional
               interpreters are for all practical purposes,
               typed. This is from "Definitional Interpreters
               Revisited" in Higher-Order and Symbolic Computation,
               11, 355–361 (1998):

                  In “Definitional Interpreters”, however, closures do
                  not contain lambda expressions, but merely unique
                  tags that are in one-to-one correspondence with
                  occurrences of lambda expressions in the program
                  being defunctionalized. The computations described
                  by these occurrences are moved to interpretive
                  functions associated with the points where closures
                  are applied to arguments. Moreover, within each
                  interpretive function the case selection on tags of
                  closures is limited to those tags that might be seen
                  at the point of application

            .     I’ve been told that this was an early example of
                  control flow analysis in a functional setting, which
                  has inspired some of the extensive development of
                  this area [23]. In fact, however, the limiting of
                  the case selections was not determined by control
                  flow analysis, but by the informal abstract type
                  declarations (called abstract syntax equations) that
                  guided the construction of the original interpreter.

               Something which any reader of the full paper will find
               curious is the fact that Reynolds' for some reason
               implements the successor function and the equality
               relation as bound values in the first four
               interpreters, even though none of the interpreters
               actually use these functions. The answer is perhaps
               that in the last interpreter, which implements
               memories: which are essentially lists of references,
               the successor and equality are the only two primitive
               constants that are needed, And coincidentally this is
               also enough to bootstrap PA.

               So why would anyone want to implement the last
               interpreter in the first one? Perhaps because the first
               one can be implemented pretty easily in any language.

               And that leads to the question "Why would anyone want
               memories in the first interpreter?" Well, memories are
               essentially lists of references to values
               (cf. Reynold's comment quoted above, regarding how
               closures are implemented, and the role of abstract
               syntax as an informal type discipline). So memories
               allow one to implement abstract syntax in the
               interpreter. And that, I think, is why the first
               interpreter is called meta-circular: because it can
               interpret the last interpreter, which can interpret the
               first one. So those first 20 lines of code are enough
               to bootstrap any language which can be described by a
               grammar, i.e. in terms of abstract syntax equations on
               records, and a formal semantics described in terms of
               untyped lambda calculus.

               That would make quite a neat API for the LLVM JIT
               engine, wouldn't it? There's not much that one would
               have to write ad-hoc, and then from any scripting
               language, one could interpret interpreters with a
               full-on assembler, capable of optimising tail-recursive
               calls, and handling and throwing exceptions, and all
               this running in native machine code on half a dozen
               different processor architectures. And an API like that
               would be an awful lot easier to use than all that macho
               hairy stuff with templates and abstract classes and
               what-not. Are there _really_ no simpler ways to
               implement abstract syntax in c++?

               Now if we have memories representing abstract syntax,
               then we have (informally) typed values, one of which is
               the type of memories. And those memories can hold any
               sort of object that could be described by a recursive
               set of abstract syntax equations. 

               Now take a look at the type system that MacQueen,
               Plotkin and Sethi describe in "An ideal model for
               recursive polymorphic types" (1983) The ideal model is
               just such a set of recursive equations. Note that they
               define a least-fixedpoint type variable binder, and a
               pair of rules that can be included in a type inference
               algorithm W, with a circular unification algorithm. As
               MacQueen et al. point out, type checking is undecidable
               in general, but this can be used in practice, and the
               denotational semantics handle the possibility, because
               there is the error value W for _dynamic_ type errors. So
               much for mu, the least fixedpoint. There is also the
               type nu, of type environments, which are functions from
               variables to ... well, memory values, I suppose. Like
               the algorithm W would be, implemented meta-circularly:
               a function from lambda expressions to memory values,
               i.e. abstract syntax representing types.

               Now look at the 1982 paper "Principal Typeschemes for
               Functional Programs" by Damas and Milner, and see the
               curious comment they make in the section describing the
               denotational semantics, to the effect that "A free type
               variable is implicitly universally quantified across
               the _whole_ of the expression in which it appears, and
               so it is sufficient to verify just the instantiation of
               type variables by any monotype ... " The whole
               expression in this case includes the "modelled by"
               turnstile, so they seem to be referring to some sort of
               inner model of the type system ... one that could be
               described in terms of memories, perhaps?

               And since untyped lambda expressions can be represented
               by abstract syntax, and since the successor and an
               equality predicate can be implemented in untyped lambda
               calculus, there you have it: operational semantics from
               hot aehr! (This Aristotle's term, meaning
               "information", as far as I can tell.)

               Which brings us to "Proofs and Bloody Types" by Girard
               et al. Perhaps the missing intuition (all of it is
               missing, from that book!) is to be found in this
               "operational denotational semantics" idea. Look perhaps
               at their "denotational" model of system-T expressions
               as untyped lambda expressions implemented as an
               operator algebra and written in the language of ZF set
               theory, then at their reducibility proofs for System F,
               and then at the proofs in MacQueen et al. on the
               contractive/non-expansiveness of the type operators
               when they are under least-fixedpoints: these have an
               eerie familiarity (so much so that it makes me feel bit
               sick to think about it, but I hope that I'll soon be
               able to face opening that book again, and enjoying it
               --- once I have some idea what it's about.)
 *)

local
   open InterpI
   val term0 = (App (Lam ("x", Add (Var "x", Var "x")),
                     Add (Con 10, Con 11)))
in
   val rI = eval term0
end

