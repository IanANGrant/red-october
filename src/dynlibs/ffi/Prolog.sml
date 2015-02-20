(* (Apart from the != predicate) This is a transliteration of the
   Prolog interpreter in John Harrison's Cambridge lecture notes. *)

open PrologSyntax

datatype outcome =
   Yes of (string * term) list
 | No

local
   fun occurs_in x =
      fn (Var y) => x = y
       | (Fn(_,l)) => List.exists (occurs_in x) l
   fun assoc i =
      (Option.map (fn (_,v) => v)) o (List.find (fn (k,_) => k = i))
   fun subs insts =
     fn (tm as (Var y)) =>
           (case assoc y insts of NONE => tm | SOME v => v)
      | (Fn(s,l)) =>
           Fn(s,List.map (subs insts) l)
   fun augment1 theta (x,s) =
      let val s' = subs theta s
      in if occurs_in x s andalso not (s = Var(x))
            then raise Fail "Occurs check."
            else (x,s')
      end
   fun raw_augment p insts = p::(List.map (augment1 [p]) insts)
   fun augment (v,t) insts =
      let val t' = subs insts t
      in case t'
           of Var (w) => 
                if w <= v 
                  then if w = v
                          then insts
                          else raw_augment (v,t') insts
                  else raw_augment (w,Var(v)) insts 
            | _ => if occurs_in v t' 
                      then raise Fail "Occurs check."
                      else raw_augment (v,t') insts
      end
    fun itlist2 f =
      let fun iter [] [] = (fn b => b)
            | iter (h::t) (h'::t') = (fn b => f h h' (itlist2 f t t' b))
            | iter _ _ = raise Fail "Arity."
      in iter
      end
   fun unify tm1 tm2 insts =
      case tm1
        of Var(x) => 
           (case assoc x insts
              of NONE => augment (x,tm2) insts
               | SOME tm1' => unify tm1' tm2 insts)
         | Fn(f1,args1) =>
           (case tm2
              of (Var(y)) => 
                 (case assoc y insts
                    of NONE => augment (y,tm1) insts
                     | SOME tm2' => unify tm1 tm2' insts)
               | Fn(f2,args2) =>
                   if f1 = f2 then itlist2 unify args1 args2 insts
                              else raise Fail ("Constants: mismatch: "^f1^"<>"^f2))
   fun rename s =
      fn (Var v) => Var("~"^v^s)
       | (Fn(f,args)) => Fn(f,List.map (rename s) args)
   fun rename_rule s (conc,assums) =
       (rename s conc,List.map (rename s) assums)
   fun expand n rules insts goals =
      let fun first f =
                (fn [] => raise Fail "No rules apply."
                  | (h::t) => (f h handle Fail _ => first f t))
          fun search rule = 
             if goals = []
                then insts
                else
                   let fun eqgoal (Fn("!=",[a,b])) = not (a = b)
                         | eqgoal _ = false
                       val (conc,assums) = rename_rule (Int.toString n) rule
                       val goal = hd goals
                       val insts' = if eqgoal goal
                                       then insts (* CHECK! This is probably wrong. *)
                                       else unify conc goal insts
                       fun occurs (v,_) = occurs_in v conc
                                   orelse List.exists (occurs_in v) assums
                       val (loc,glob) = List.partition occurs insts'
                       val goals' = (List.map (subs loc) assums) @ (tl goals)
                   in expand (n+1) rules glob goals'
                   end
      in
         first search rules
      end
in
   fun prolog rules goal =
      let val insts = expand 0 rules [] [goal]
      in Yes (List.filter (fn (v,_) => occurs_in v goal) insts)
      end handle Fail _ => No
end

(* DATALOG

   Change Answer format to a list of terms.

   Change search strategy to allow all or first n answers. 

   Implement Earley deduction search strategy with reduction rule.

   Add negation:

      1. Add check for inconsistent programs? (I'm not sure what it is though!)
      2. Pre-compile subsumption check when adding new derived terms
      3. Change evaluation strategy to only evaluate negated terms when
         the fixedpoint has been reached for positive terms.

   Add built-in predicates:

      1. Add constant_term predicate for checking a term for uninstantiated logical variables
      2. Add syntax for built-ins
      3. Add evaluation_check predicate for terms:

             check that all free logical variables which are arguments
             to built-in predicates/functions are either safe
             (i.e. they appear in the arguments to one or more
             ordinary predicates) or are linked by a chain of equality
             predicates to some safe variable. This must take into
             account lazy evaluation on andalso and orelse and ? :
      4. Add semantics for evaluating built-ins
      5. Change evaluation strategy to 

             (a) only evaluate non-equality built-ins when they are constant terms.  
             (b) evaluate equality built-ins when either side is a constant term.

   Add relational algebra and data description language (DDL) to allow
   the extensional databases to be specified with a goal.

   The syntax and semantics of built-ins should be typed, and
   "pluggable", so that different domain languages can be used for
   different types of program, or even in differents parts of one
   program, see below for more on types/modules.

*)
