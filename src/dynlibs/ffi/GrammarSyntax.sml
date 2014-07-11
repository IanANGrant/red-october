datatype Tree = NonTerm of string * Tree list
              | Term of string;

(* A functor for handling abstract syntax.  Walks the tree, applying f
   before the subtree is traversed, and g afterwards. *)

val nilTree = Term("");

fun fold f g acc =
   let fun iterouter acc (t as (Term(_))) = g(t,f (t,acc))
         | iterouter acc (nt as (NonTerm(_,l))) =
           let fun iterinner acc [] = acc
                 | iterinner acc (t::ts) =
                      iterinner (iterouter acc t) ts
           in
              g (nt,iterinner (f(nt,acc)) l)
           end
    in
       iterouter acc
    end;

val I = fn x => x;
val K = fn x => fn _ => x;
fun lift f = fn (x,acc) => f acc x;
fun foldr f = fold f (lift K);
fun foldl g = fold (lift K) g;

(* For example, we can use this functor to implement a trivial
   abstract syntax printer. *)

val ilevel = 2; (* Indentation on successive levels *)
fun spaces i = CharVector.tabulate (i, K #" ");

(* We use the fold breadth function to increase the indentation, then
   the depth function reduces it after the subtree has been printed *)

fun toString m t =
   let fun prefix s i n = s^(if s = "" then "" else "\n")^(spaces (ilevel * i))^n
       fun ffn (NonTerm(n,_),(s,i)) =
               if m> 0 andalso i >= m 
                  then (if i = m then prefix s i (n^": ...") else s,i+1)
                  else (prefix s i (n^":"),i+1)
         | ffn (Term(t),(s,i)) = 
                if m > 0 andalso i > m then (s,i) else (s^" "^t,i)
       fun gfn (NonTerm _,(s,i)) = (s,i-1)
         | gfn (_,x) = x
       val (s,_) = fold ffn gfn ("",0) t
   in s
   end;

fun printTreeDirect m t =
   let fun prefix s i n = (if s then "\n" else "")^(spaces (ilevel * i))^n
       fun ffn (NonTerm(n,_),(s,i)) =
               if m> 0 andalso i >= m 
                  then (if i = m then (TextIO.print(prefix s i (n^": ..."));true) else s,i+1)
                  else ((TextIO.print (prefix s i (n^":"));true),i+1)
         | ffn (Term(t),(s,i)) = 
                if m > 0 andalso i > m then (s,i) else ((TextIO.print(" "^t);true),i)
       fun gfn (NonTerm _,(s,i)) = (s,i-1)
         | gfn (_,x) = x
       val (_,_) = fold ffn gfn (false,0) t
   in print "\n";
      ()
   end;

(* Debugging printer *)

datatype debugState =
    Off
  | On of int
  | Cond of int * (Tree -> bool);

val debug_switches : (string * debugState) list list ref = ref [[]];

fun debug_switch f =
   let fun iter [] = Off
         | iter ((f',s)::fs) =
              if f'=f then s else iter fs
   in case !debug_switches
        of l::_ => iter l 
         | _ => Off
   end;

fun debug_state_off f =
   case debug_switch f 
     of Off => true
      | _ => false;

fun debug_state_on f t =
   case debug_switch f 
     of Off => false
      | On(_) => true
      | Cond (_,p) => p t;

fun debug_debug_push cf f s t = 
   if (cf = "" orelse debug_state_on cf t)
   then debug_switches := 
          (case !debug_switches
             of (lls as (l::_)) => ((f,s)::l)::lls
              | [] => [[(f,s)]])
   else ();

fun debug_debug_pop cf = 
   if cf = "" orelse not (debug_state_off cf) then
      debug_switches := 
        (case !debug_switches
           of (lls as (_::ls)) => ls
         | [] => raise Fail "debug_pop: stack empty")
   else ();

fun debug_on f s =
   debug_debug_push "" f s nilTree;

fun debug_off f =
   if debug_state_off f
   then ()
   else
      debug_switches := 
        (case !debug_switches
           of (lls as (l::_)) => ((f,Off)::l)::lls
            | [] => [[(f,Off)]]);

fun debug_reset () =
      debug_switches := [[]];   

fun debug_tree_printer f m t d =
    let val _ = TextIO.print ("debug: "^f^" "^m^":\n"^(toString d t)^"\n")
    in t end;

fun debug_print printer f m t =
     case debug_switch f
       of On(d) => printer f m t d
        | Cond(d,p) => 
            if p t then printer f m t d else t
        | Off => t;

val debug = debug_print debug_tree_printer;
fun debug_list f m = List.map (debug f ("["^m^"]"));

fun add_nl s = s^"\n";
val printTree = TextIO.print o add_nl o toString 0;
fun printTreeToDepth m = TextIO.print o add_nl o toString m;
val printTreeList = app printTree;
fun printTreeListToDepth m = app (printTreeToDepth m);

(* A 'leaf' is a tree of the form NonTerm(name,[Term(value)]) *)

fun tree_leaves name f =
   let fun itfn (NonTerm(n, [Term(value)]),acc)
                  = if n = name then f (value,acc) else acc
         | itfn (_,acc) = acc
   in itfn 
   end;

(* A predicate which holds if a tree has a leaf with a particular name
   and value. *)

fun tree_has_leaf name value = 
   let fun f (v,acc) = if v=value then true else acc
   in tree_leaves name f
   end;

(* Substitutes each node with the result of the function f applied to
   that node. If the result is 'epsilon': NonTerm("",[]) then it is ignored.
   This is done depth-first, and left-to-right along the branches. *)

fun tree_subst f t =
   let val recur = tree_subst f
       fun elim_epsilon (t,a) =
            case (recur t)
              of NonTerm("",[])
                   => a
               | t => t::a
       fun iter t =
           case t
             of Term _
                  => f t
              | NonTerm (n,l)
                  => f (NonTerm(n,List.foldr elim_epsilon [] l))
       val r = iter t
   in r
   end;

(* Substitutes a singleton branch of a certain name with the result of
   the function f applied to the sole immediate subtree of that
   branch. *)

fun tree_subst_branch n f =
   tree_subst (fn (nt as (NonTerm(n',[t]))) =>
                             if n'=n then f t else nt
                | t => t);

(* Substitutes particular leaves with the result of the function f
   applied to just the value of the leaf. *)

fun tree_subst_leaves name f =
   tree_subst_branch name
        (fn (Term (t)) => f t
          | _ => raise Fail ("Bind: tree_subst_leaves: "^name));

(* Another simiar function specialised to substituting from a list of
   leaves representing name/value pairs. *)

local
   fun assq name l =
      let val fnname = "tree_tree_list_subst_leaves"
          fun iter [] k = raise Fail (fnname^": no var "^name^"."^k)
            | iter (NonTerm(k',[v])::kvps) k = if k' = k then v else (iter kvps) k
            | iter ((t as (NonTerm(s,_)))::_) _ = 
                          (printTreeDirect 0 t;raise Fail (fnname^": arity "^name^"."^s))
            | iter (Term(s)::_) _ = raise Fail (fnname^": terminal "^name^"."^s)
      in iter l
      end
   val fname = "tree_tree_list_subst_leaves"
in
   fun tree_tree_list_subst_leaves name ss =
          (debug fname "") o (tree_subst_leaves name (assq name (debug_list fname "" ss)))
end;

(* Applies the list fs of functions element-wise to the subtrees of a
   given branch of arity equal to length of fs. *)

fun tree_unify_branch name fs = 
  fn (x as (t,a)) =>
     let val _ = debug "tree_unify_branch" "" t
         fun itfn (_,x as (_,false)) = x 
           | itfn (NonTerm(n, l),(a,m)) = if not m then (a,m) else
               if name = "*" orelse n = name andalso length fs = length l
                  then List.foldr (fn ((f,t),a) => f (t,a)) (a,m) (ListPair.zip (fs,l))
                  else (a,false)
           | itfn (_,(a,_)) = (a,false)
     in itfn x
     end;

(* Breadth-first traverse. Accumulates the results of the function f
   applied to each node. *)

fun tree_foldl f =
   let fun iter (t as (Term _),a) = f (t,a)
         | iter (t as (NonTerm (_,l)),a) =
             List.foldr iter (f (t,a)) l
   in iter
   end;

(* Depth-first traverse. Accumulates the results of the function f
   applied to each node. *)

fun tree_foldr f =
   let fun iter (t as (Term _),a) = f (t,a)
         | iter (t as (NonTerm (_,l)),a) =
             f (t,List.foldr iter a l)
   in iter
   end;

(* Applies f to the first positive result of successive attempts to
   unify a given branch with one of a list ps of patterns. *)

fun tree_unify_match f ps =
   let fun iter (t,(a,m)) =
       let val dflg = "tree_unify_match"
           val _ = debug dflg "" t
           val _ = debug_debug_push dflg "tree_unify_branch" (On 10) t
           fun alternates (_,r as (_,true)) = r
             | alternates (((n,fs),tgt),(a',m')) = 
                 let val (a'',m'') = tree_unify_branch n fs (t,(a',true))
                 in (if m'' then f (t,tgt,a'') else a', m' orelse m'')
                 end
           val (a',m') = List.foldr alternates ([],false) ps
           val _ = debug_list dflg ("a=") a
           val _ = debug_list dflg ("alternates (m'="^(if m' then "t" else "f")^")gave: ") a'
           val _ = debug_debug_pop dflg
       in (if m' then a'@a else a, m andalso m')
       end
   in iter
   end;

fun tree_match quote pattern =
   let fun matches (_,t',a) =
          let val _ = debug "tree_match" "matches" t'
              val r = debug "tree_match" "matches returning"
                                   (tree_tree_list_subst_leaves quote a t')
          in [r] end
   in fn t => 
        let val (r,_) = tree_foldl (tree_unify_match matches pattern) (t,([],false)) 
        in r end
   end;

(* Like tree_match, but substitutes according to a target which is
   paired with each match rule in ps. *)

fun tree_unify_subst f ps =
  fn t => 
    let val dflg = "tree_unify_subst"
        val _ = debug dflg "" t
        fun alternates (_,r as (_,true)) = r
          | alternates (((n,fs),tgt),(t,m')) = 
                let val (t',m'') = tree_unify_branch n fs (t,([t],true))
                    val t'' = if m'' then f (tgt,t') else t
                in (t'',m'')
                end
        val (t',m') = List.foldl alternates (t,false) ps
        val _ = debug dflg "after List.foldl alternates" t' 
    in (if m' then t' else t)
    end;

(* Recursively rewrite using tree_unify_subst, rewriting anything that
   appears within anti-quotes in the substituted pattern. When
   anti-quotes are nested, the rewrites are done depth-first. *)

local
   fun matches quote (t,a) =
      let val _ = debug "matches" "" t
          val r = debug "matches" "returning" (tree_tree_list_subst_leaves quote a t)
      in r end
   fun rewrite quote pattern t =
         tree_subst (tree_unify_subst (matches quote) pattern) (debug "rewrite" "" t)
   fun substitute quote pattern t =
         tree_unify_subst (matches quote) pattern (debug "substitute" "" t)
   fun tree_subst_epsilon t =
      tree_subst (fn (NonTerm("%",[])) => (NonTerm("",[])) | t => t) t
   fun tree_rewrite_ rewrite aquote quote pattern =
      let 
      in fn t => 
            let val rewrite' = rewrite quote pattern
                val recursive_rewrite = tree_rewrite_ rewrite aquote quote pattern
                val _ = debug "tree_rewrite" "" t
                val t = rewrite' t
                val _ = debug "tree_rewrite" "rewrite" t
                val t = tree_subst_branch aquote recursive_rewrite t
                val _ = debug "tree_rewrite" "tree_subst_branch: aquote" t
                val t = tree_subst_epsilon t
                val _ = debug "tree_rewrite" "tree_subst_epsilon" t
            in 
               t
            end
      end
   fun tree_rewrite_param_ rewrite aquote quote pattern fns =
      let fun assq l =
               let fun iter [] k = raise Fail ("no function: "^k)
                     | iter ((k',f)::kvps) k = if k' = k then f else (iter kvps) k
          in iter l
          end
          fun fn_rewrite fns =
                     tree_subst (fn (nt as (NonTerm("%funsubs",[Term(fnm),t]))) => assq fns fnm t
                                  | t => t);
      in fn t => 
            let val rewrite' = rewrite quote pattern
                val recursive_rewrite = tree_rewrite_param_ rewrite aquote quote pattern fns
                val _ = debug "tree_rewrite_param" "" t
                val t = rewrite' t
                val _ = debug "tree_rewrite_param" "rewrite" t
                val t = fn_rewrite fns t
                val _ = debug "tree_rewrite_param" "tree_subst_branch: fnsubs" t
                val t = tree_subst_branch aquote recursive_rewrite t
                val _ = debug "tree_rewrite_param" "tree_subst_branch: aquote" t
                val t = tree_subst_epsilon t
                val _ = debug "tree_rewrite_param" "tree_subst_epsilon" t
            in 
               t
            end
      end
in
   val tree_rewrite = tree_rewrite_ rewrite 
   val tree_rewrite_param = tree_rewrite_param_ rewrite 
   val tree_rewrite_td = tree_rewrite_ substitute
   val tree_rewrite_param_td = tree_rewrite_param_ substitute 
end;

(* Interpreter semantics. *)

fun mkNonTerm nt = NonTerm nt;

fun mkTerm v = Term v;
