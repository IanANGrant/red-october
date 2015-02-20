(* Reynolds' "Definitional Interpreters for Higher Order 
   Programming Languages" in HOSC. *)

(* Record types, those in [], are selected upon, so they carry tags,
   and have associated constructors and deconstructors.

   They can make a sort of operators and arities style language. *)

(* Interpreter II --- first order meta-circular interpreter *)

(* We have added a LET expression so that we can more easily translate
   the other interpreters in this one. The changes are the new LET
   record in EXP, and the extra case in eval.

EXP = CONST ∪ VAR ∪ APPL ∪ LAMBDA ∪ COND ∪ LET ∪ LETREC
APPL = [opr: EXP, opnd: EXP]
LAMBDA = [fp: VAR, body: EXP]
COND = [prem: EXP, conc: EXP, altr: EXP]
LET = [lvar: VAR, lexp: EXP, lbody: EXP]
LETREC = [dvar: VAR, dexp: LAMBDA, body: EXP]

VAL = INTEGER ∪ BOOLEAN ∪ FUNVAL

FUNVAL = CLOSR ∪ SC ∪ EQ1 ∪ EQ2
CLOSR = [lam: LAMBDA, en: ENV]
SC = []
EQ1 = []
EQ2 = [arg1: VAL]

ENV = INIT ∪ SIMP ∪ REC
INIT = []
SIMP = [bvar: VAR, bval: VAL, old: ENV]
REC = [letx: LETREC, old: ENV]

(* interpret : EXP → VAL *)
interpret = λr.eval(r,mk-init())

(* eval : EXP, ENV → VAL *)
eval = λ(r,e).
  (const?(r) → evcon(r),
   var?(r) → get(e,r),
   appl?(r) → apply(eval(opr(r),e),eval(opnd(r),e)),
   lambda?(r) → mk-closr(r,e),
   cond?(r) → if eval(prem(r),e)
                 then eval(conc(r),e)
                 else eval(altr(r),e),
   let?(r) → apply(mk-closr(mk-lambda(lvar(r),lbody(r)),e),eval(lexp(r),e)),
   letrec?(r) → eval(body(r),mk-rec(r,e)))

(* apply : FUNVAL, VAL → VAL *)
apply = λ(f,a).
   (closr?(f) → eval(body(lam(f)),
                     mk-simp(fp(lam(f)),a,en(f))),
   (lclosr?(f) → eval(body(clam(f)),
                     mk-lbind(fp(clam(f)),a,cen(f))),
    sc?(f) → succ(a),
    eq1?(f) → mk-eq2(a),
    eq2?(f) → equal(arg1(f),a))

(* get : ENV, VAR → VAL *)
get = λ(e,x).
   (init?(e) → (x = “succ” → mk-sc(),
                x = “equal” → mk-eq1()),
    simp?(e) → if x = bvar(e)
                  then bval(e)
                  else get(old(e),x),
    lbind?(e) → if x = lvar(e)
                  then lexp(e)
                  else get(lold(e),x),
    rec?(e) → if x = dvar(letx(e))
                 then mk-closr(dexp(letx(e)),e)
                 else get(old(e),x))
*)

type CONST = string
 and VAR = string

abstype EXP =
    CONST of CONST
  | VAR of VAR
  | APPL of {opr: EXP, opnd: EXP}
  | LAMBDA of LAMBDA
  | COND of {prem: EXP, conc: EXP, altr: EXP}
  | LET of {lvar: VAR, lexp: EXP, lbody: EXP}
  | LETREC of LETREC
and VAL =
    INTEGER of int
  | BOOLEAN of bool
  | CLOSR of {lam: LAMBDA, en: ENV}
  | SC of {}
  | EQ1 of {}
  | EQ2 of {arg1: VAL}
and ENV =
    INIT of {}
  | SIMP of {bvar: VAR, bval: VAL, old: ENV}
  | REC of {letx: LETREC, old: ENV}
withtype LAMBDA = {fp: VAR, body: EXP}
     and LETREC = {dvar: VAR, dexp: {fp: VAR, body: EXP}, body: EXP}
with
   fun evcon s =
      case Int.fromString s
        of SOME n => INTEGER n
         | NONE => (case s
                      of "true" => BOOLEAN true
                       | "false" => BOOLEAN false
                       | _ =>  raise Fail ("evcon: don't know "^s))
   fun show (INTEGER n) = Int.toString n
     | show (BOOLEAN t) = if t then "true" else "false"
     | show _ = "<fn>"
   fun equal (INTEGER i,INTEGER j) = BOOLEAN (i = j)
     | equal (BOOLEAN p,BOOLEAN q) = BOOLEAN (p = q)
     | equal _ = raise Fail "equal: both operands not either INTEGER or BOOLEAN"
   fun succ (INTEGER n) = INTEGER (n+1)
     | succ _ =  raise Fail "succ: operand not INTEGER"
   fun mk_const s = CONST s
   fun mk_var s = VAR s
   fun mk_appl (e,e') = APPL {opr = e, opnd = e'}
   fun mk_lambda (v,e) = LAMBDA {fp = v, body = e}
   fun mk_cond (e,e',e'') = COND {prem = e, conc = e', altr = e''}
   fun mk_let (v,e,b) = LET {lvar = v, lexp = e, lbody = b}
   fun mk_lam (v,e) = {fp = v, body = e}
   fun mk_letrec (v,(v',e),b) = LETREC {dvar = v, dexp = mk_lam(v',e), body = b}
   fun mk_closr (l,e) = CLOSR {lam = l, en = e}
   fun mk_sc () = SC {}
   fun mk_eq1 () = EQ1 {}
   fun mk_eq2 a = EQ2 {arg1 = a}
   fun mk_init () = INIT {}
   fun mk_simp (v,l,e) = SIMP {bvar = v, bval = l, old = e}
   fun mk_rec (l,e) = REC {letx = l, old = e}
   fun eval (CONST r,_) = evcon(r)
     | eval (VAR r,e) = get(e,r)
     | eval (APPL r,e) = apply(eval(#opr r,e),eval(#opnd r,e))
     | eval (LAMBDA r,e) = mk_closr(r,e)
     | eval (COND r,e) =
              if (case eval(#prem r,e)
                    of BOOLEAN t => t
                     | INTEGER n => not (n = 0)
                     | _ => raise Fail "COND: Premiss is neither BOOLEAN nor INTEGER")
                 then eval(#conc r,e)
                 else eval(#altr r,e)
     | eval (LET r,e) = apply(mk_closr(mk_lam(#lvar r,#lbody r),e),(eval(#lexp r,e)))
     | eval (LETREC r,e) = eval(#body r, mk_rec(r,e))
   and apply (CLOSR f,a) = eval(#body (#lam f),
                                mk_simp(#fp (#lam f),a,#en f))
     | apply (SC _,a) = succ a
     | apply (EQ1 _,a) = mk_eq2 a
     | apply (EQ2 f,a) = equal(#arg1 f,a)
     | apply _ = raise Fail "APPLY: wrong type"
   and get (INIT _,"succ") = mk_sc()
     | get (INIT _,"equal") = mk_eq1()
     | get (INIT _,s) = raise Fail ("INIT: free variable "^s)
     | get (SIMP e,x) =
          if x = #bvar e
             then #bval e
             else get (#old e,x)
     | get (r as (REC e),x) =
          if x = #dvar (#letx e)
             then mk_closr (#dexp (#letx e),r)
             else get (#old e,x)
   fun interpret r = show (eval(r,mk_init()))
end

(* Define some ML to make expressions a little easier to construct. *)

infix 5 :@;

fun lambda var body = mk_lambda (var,body)
fun letrec var (var',dexp) body = mk_letrec (var,(var',dexp),body)
fun let' var dexp body = mk_let (var,dexp,body)
fun cond prem concl altr = mk_cond (prem, concl, altr)
fun lambdas [] body = body
  | lambdas (v::vs) body = lambda v (lambdas vs body)
fun op :@ (exp1, exp2) = mk_appl (exp1,exp2)
fun equal a1 a2 = mk_var "equal" :@ a1 :@ a2
fun succ a = mk_var "succ" :@ a
fun integer n = mk_const (Int.toString n)

(* By way of example, let's program some arithmetic functions.

   They are the usual suspects: the addition, predecessor, fibonacci
       number, multiplication and factorial functions.

letrec
 add = λm.λn.
   letrec
     loop = λx.λa.
       if x = m
          then a 
          else loop (succ x) (succ a)
   in loop 0 n
in letrec
 pred = λm.
   letrec
     loop = λx.λa.λb.
       if x = m
          then b
          else loop (succ x) b (succ b)
   in loop 0 0 0
in letrec
 fibs = λm.
   letrec
     loop = λx.λa.λb.
       if x = m
          then b
          else loop (succ x) b (add a b)
   in loop 0 0 1
in letrec
 mult = λm.λn.
   letrec
     loop = λx.λa.
       if x = m
          then a
          else loop (succ x) (add a n)
   in loop 0 0
in letrec
 factorial = λm.
   letrec 
     loop = λx.λa.
       if x = m
          then a
          else loop (succ x) (mult a (succ x))
   in loop 0 1
in
  factorial 8
*)

(* They're all the same, really. So we can make a primitive recursion
   template which allows us to specify just the essential details,
   which are the non-recursion variables, the initial values of the
   accumulator(s), and the subsequent accumulator calculation(s). *)

fun prec name loopv mv nvs xv nextx itpred initx avs steps exit accs expr =
     let val loop = mk_var loopv
         fun appaccs [] a = a
           | appaccs (ac::acs) e =
                appaccs acs (e :@ ac)
     in
      letrec name
        (mv,
         (lambdas nvs
           (letrec loopv
              (xv,
               (lambdas avs
                (cond itpred exit (appaccs steps (loop :@ nextx)))))
             (appaccs accs (loop :@ initx)))))
       expr
     end

fun revargs e = (lambdas ["m","n"] (e :@ (mk_var "n") :@ (mk_var "m")))

fun add m n  = mk_var "add"  :@ m :@ n
fun sub m n  = revargs (mk_var "sub") :@ m :@ n
fun pred n   = mk_var "pred" :@ n
fun fib n    = mk_var "fib"  :@ n
fun mult m n = mk_var "mult" :@ m :@ n
fun fact n   = mk_var "fact" :@ n
fun expt m n = revargs (mk_var "expt") :@ m :@ n

fun arithdef next =
  let val a = mk_var "a"
      val b = mk_var "b"
      val m = mk_var "m"
      val n = mk_var "n"
      val x = mk_var "x"
      val zero = integer 0
      val one = integer 1
      fun precn name vars = prec name "loop" "m" vars "x" (succ x) (equal x m) zero
      fun prec1 name = precn name []
      fun prec2 name = precn name ["n"]
  in
    prec2 "add"  ["a"]     [(succ a)]          a [n]
   (prec1 "pred" ["a","b"] [b,(succ b)]        a [zero,zero]
   (prec2 "sub"  ["a"]     [(pred a)]          a [n]
   (prec1 "fib"  ["a","b"] [b,(add a b)]       b [zero,one]
   (prec2 "mult" ["a"]     [(add a n)]         a [zero]
   (prec1 "fact" ["a"]     [(mult a (succ x))] a [one]
   (prec2 "expt" ["a"]     [(mult a n)]        a [one]
    next))))))
  end

fun test e = interpret (arithdef e)

val tests =
  [add  (integer 100)  (integer 2),
   pred (integer 202),
   sub  (integer 103)  (integer 53),
   fib  (integer 6),
   mult (integer 105)  (integer 2),
   fact (integer 5),
   expt (integer 7)    (integer 2)];

fun from a b =
   let val s = b - a
       val d = if s < 0 then ~1 else 1
       fun iter n acc =
         if d*n > d*b
            then List.rev acc
            else iter (n + d) (n::acc)
   in iter a []
   end

val facts = List.map (test o fact o integer) (from 1 6);
val fibs = List.map (test o fib o integer) (from 1 16);

val results = List.map test tests;

(* And that shows, we think, that using metaprogramming means there's
   less code. So it's easier to understand, and there's less to go
   wrong, and there's less to audit. 

   It's also much harder to subvert. For example: if you wanted to
   make the results of just one exponent calculation be different, how
   would you do it?  If you change the definition of the exponent
   function, then (a) your change will be obvious, and (b) you'll
   change more than one result. If you change the definition of prec
   then you'll change more than one function. If you change the ML
   definition of expt then it's going to be obvious. The only
   remaining possibility is changing the definition of eval, but how
   are you going to identify what is the exponential function? And
   what if we use a different evaluator and interpret it inside the
   one you changed? *)

(* Interpreter III --- evaluation-order independent, first-order,
   and all co-recursion is via tail calls.

EXP = CONST ∪ VAR ∪ APPL ∪ LAMBDA ∪ COND ∪ LETREC
APPL = [opr: EXP, opnd: EXP]
LAMBDA = [fp: VAR, body: EXP]
COND = [prem: EXP, conc: EXP, altr: EXP]
LETREC = [dvar: VAR, dexp: LAMBDA, body: EXP]

CONT = FIN ∪ EVOPN ∪ APFUN ∪ BRANCH
FIN = []
EVOPN = [ap: APPL, en: ENV, next: CONT]
APFUN = [fun: VAL, next: CONT]
BRANCH = [cn: COND, en: ENV, next: CONT]

VAL = INTEGER ∪ BOOLEAN ∪ FUNVAL

FUNVAL = CLOSR ∪ SC ∪ EQ1 ∪ EQ2
CLOSR = [lam: LAMBDA, en: ENV]
SC = []
EQ1 = []
EQ2 = [arg1: VAL]

ENV = INIT ∪ SIMP ∪ REC
INIT = []
SIMP = [bvar: VAR, bval: VAL, old: ENV]
REC = [letx: LETREC, old: ENV]

interpret = λr.eval(r,mk-init(),mk-fin())

eval = λ(r,e,c).
  (const?(r)  → cont(c,evcon(r)),
   var?(r)    → cont(c,get(e,r)),
   appl?(r)   → eval(opr(r),e,mk-evopn(r,e,c)),
   lambda?(r) → cont(c,mk-closr(r,e)),
   cond?(r)   → eval(prem(r),e,mk-branch(r,e,c)),
   letrec?(r) → eval(body(r),mk-rec(r,e),c))

apply = λ(f,a,c).
   (closr?(f) → eval(body(lam(f)),mk-simp(fp(lam(f)),a,en(f)),c),
    sc?(f)    → cont(c,succ(a)),
    eq1?(f)   → cont(c,mk-eq2(a)),
    eq2?(f)   → cont(c,equal(arg1(f),a)))

cont = λ(c,a).
  (fin?(c)    → a,
   evopn?(c)  → let f = a
                and r = ap(c)
                and c = next(c)
                and e = en(c)
                in eval(opnd(r),e,mk-apfun(f,c)),
   apfun?(c)  → let f = fun(c)
                and c = next(c)
                in apply(f,a,c),
   branch?(c) → let b = a
                and r = cn(c)
                and e = en(c)
                and c = next(c)
                in if b then eval(conc(r),e,c)
                        else eval(altr(r),e,c))

(* get : ENV, VAR → VAL *)
get = λ(e,x).
   (init?(e) → (x = “succ” → mk-sc(),
                x = “equal” → mk-eq1()),
    simp?(e) → let v = bvar(e)
               in if x = v
                  then bval(e)
                  else get(old(e),x),
    rec?(e)  → let r = letx(e)
               and s = dvar()
               in if x = s
                  then mk-closr(dexp(r),e)
                  else get(old(e),x))
*)

(* Interpreter V --- higher-order, evaluation-order independent, with
   escapes and memory state. *)

(*
EXP = CONST ∪ VAR ∪ APPL ∪ LAMBDA ∪ COND ∪ LETREC ∪ ESCP
APPL = [opr: EXP, opnd: EXP]
LAMBDA = [fp: VAR, body: EXP]
COND = [prem: EXP, conc: EXP, altr: EXP]
LETREC = [dvar: VAR, dexp: LAMBDA, body: EXP]
ESCP = [escv: VAR, body: EXP]

VAL = INTEGER ∪ BOOLEAN ∪ FUNVAL ∪ REF
FUNVAL = VAL, MEM, CONT → VAL
ENV = VAR → VAL
CONT = MEM, VAL → VAL

REF = [number: INTEGER]
MEM = [count: INTEGER, possess: INTEGER → VAL]

(* If MEM were implemented as a distributed RB map (see the comments
   in PolyRedBlackMap.sml) then we could use distributed memories and
   references to represent abstract syntax records, and then do
   non-deterministic, redundant, distributed expression evaluation
   across the net ... So possess() would look up one or more
   distributed, redundant RB dictionaries to find out where to go to
   follow the references. These couldn't be two separate layers: the
   actual reference lookup and reply, or update, would have to be the
   continuation of the RB tree lookup to avoid an infinite regress. If
   say, each of 20 nodes had three neighours, in a dodecahedral
   topology, then each RB lookup and update originating at that node
   could be multicast to all three peers. Each peer receiving a
   message would process it itself, and also multicast it to two
   others (not the one it came from).

   These messages would be just short lists of integers. Now if all
   these processes multiplexed their messages over, say UDP datagrams,
   or ATM packets, then it would be awfully hard for _anyone_ to be
   able to work out what was actually going on, never mind interfering
   with it without being detected. *)

initmem = mk-mem(0,λn.0)
nextref = λm.mk-ref(succ(count(m)))
augment = λ(m,a).
            mk-mem(succ(count(m)),
                   λn.if equal(n, succ(count(m)))
                         then a
                         else (possess(m))(n))
update = λ(m,rf,a). 
            mk-mem(count(m),
                   λn.if equal(n,number(rf))
                         then a
                         else (possess(m))(n))

lookup = λ(m,rf).(possess(m))(number(rf))

initenv = λx.(equal(x, “succ”) → λ(a,m,c).c(m,succ(a)),
              equal(x, “equal”) → λ(a,m,c).c(m,λ(b,m',c').c'(m',equal(a,b)))
              equal(x, “ref”) → λ(a,m,c).c(augment(m,a),nextref(m)),
              equal(x, “set”) → λ(rf,m,c).c(m,λ(a,m',c').c'(update(m',rf,a),a)),
              equal(x, “val”) → λ(rf,m,c).c(m,lookup(m,rf)))

interpret = λr.eval(r,initenv,initmem,λ(m,a).a)

eval = λ(r,e,m,c).
  (const?(r) → c(m,evcon(r)),
   var?(r) → c(m,e(r)),
   appl?(r) → eval(opr(r),e,m,λ(m',f).eval(opnd(r),e,m',λ(m'',a).f(a,m'',c))),
   lambda?(r) → c(m,evlambda(r,e)),
   cond?(r) → eval(prem(r),e,m,λ(m',b).if b
                                          then eval(conc(r),e,m',c)
                                          else eval(altr(r),e,m',c)),
   letrec?(r) → letrec e' = λx.if equal(x,dvar(r))
                                  then evlambda(dexp(r),e')
                                  else e(x)
                in eval(body(r),e',m,c),
   escp?(r) → eval(body(r),ext(escv(r),λ(a,m',c').c(m',a),e),m,c))

evlambda = λ(l,e).λ(a,m,c).eval(body(l),ext(fp(l),a,e),m,c)

ext = λ(z,a,e).λx.if equal(x,z)
                     then a
                     else e(x)

*)
