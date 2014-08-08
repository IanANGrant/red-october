open GrammarSyntax

val locations = ref [] : (string * int * int) list ref;

fun location (s as (s',_,_)) =
   ((*print ("location: "^s'^"\n");*)
    locations := (s::(!locations)));   

val clear_locations = fn () => locations := [];

(* A similar list of comments received from the CPP. The function
   comment is called by CPPLexer *)

val comments = ref [] : (string * int * int) list ref;

fun comment s =
   comments := (s::(!comments));   

fun clear_comments () = comments := [];

(* The table of typedef names that the lexer uses. The function
   is_typedef_name is called by CPPLexer.Token. *)

val typedef_table = (Hasht.new 50 : (string,(Tree * Tree)) Hasht.t);

fun clear_typedefs () =
 (Hasht.clear typedef_table;
  Hasht.insert typedef_table "__builtin_va_list" (Term(""),Term("")));

fun is_typedef_name s =
   let val _ = Hasht.find typedef_table s
   in true end 
   handle Subscript => false;

fun typedef_decl s =
   let val (t,_) = Hasht.find typedef_table s
   in t end;

fun new_typedef_name s t =
   Hasht.insert typedef_table s (t,t);

fun typedef_names () = 
   Hasht.fold (fn n => fn _ => fn l => n::l) [] typedef_table;

(* Similarly for struct/union and enum tags *)

val tag_table = (Hasht.new 50 : (string,Tree) Hasht.t);

fun clear_tags () =
 Hasht.clear tag_table;

fun is_tag_name s =
   let val _ = Hasht.find tag_table s
   in true end 
   handle Subscript => false;

fun tag_decl s =
   Hasht.find tag_table s;

fun new_tag_name s t =
   Hasht.insert tag_table s t;

fun tag_names () = 
   Hasht.fold (fn n => fn _ => fn l => n::l) [] tag_table;

(* The table of macro names *)

val macro_table = (Hasht.new 50 : (string,(Tree * (string option * string))) Hasht.t);

fun clear_macros () = Hasht.clear macro_table;

fun is_macro_name s =
   let val _ = Hasht.find macro_table s
   in true end 
   handle Subscript => false;

fun macro_decl s =
   Hasht.find macro_table s;

fun new_macro_name s t =
   Hasht.insert macro_table s t;

fun macro_names () = 
   Hasht.fold (fn n => fn _ => fn l => n::l) [] macro_table;

fun reset_parser () = 
(   clear_locations();
    clear_comments();
    clear_typedefs();
    clear_tags();
    clear_macros()
);

(* The semantics of type definitions. The syntax of C is not
   context-free. A typedef name can only be recognised during the
   parse by keeping account of the types defined so far. So every time
   we add a new non-terminal, we check to see whether it is a typedef
   declaration, and if so, we add all the names of the new types to a
   symbol table which the lexer uses to distinguish between IDENTIFIER
   and TYPEDEF_NAME tokens.

   We need to re-write the declarations so that we don't store the
   entire init-declarator-list against each typedef name: it is not
   common practice (which is probably no bad thing) but the following
   is valid C syntax which defines three types with one use of
   typedef:

      unsigned short typedef int usi, *usip, ( *pusifusip ) (usip);     *)

local
   val declrwt = 
         tree_rewrite "%aquote" "%quote" 
           (Rewrite.compileRewrites
             (Rewrite.parse_rewrites_string
                "⟦direct-declarator ⌜x:direct-declarator⌝ \
               \                    ⌜y:parameter-type-list⌝⟧ = (⌜x⌝) \
               \ ⟦direct-declarator ⌜x:direct-declarator⌝ \
               \                    ⌜y:identifier-list⌝⟧ = (⌜x⌝)"))
   val initdeclmatch = tree_match "%quote" 
          (Rewrite.compileRewrites
            (Rewrite.parse_rewrites_string
                "⟦declaration \
               \    ⌜x:declaration-specifiers⌝ \
               \    (init-declarator-list ⌜y:init-declarator⌝)⟧ = \
               \ (⌜y⌝)"))
   val idmatch = tree_match "%quote" 
          (Rewrite.compileRewrites
            (Rewrite.parse_rewrites_string
                "⟦direct-declarator (identifier ⌜x⌝)⟧ = (⌜x⌝)"))
   val decllistmatch = tree_match "%quote" 
          (Rewrite.compileRewrites
            (Rewrite.parse_rewrites_string
                "⟦declaration-list \
               \    ⌜x:declaration-list⌝ \
               \    ⌜y:declaration⌝⟧ = \
               \ (⌜y⌝) \
               \ ⟦declaration-list \
               \    ⌜x:declaration⌝⟧ = \
               \ (⌜x⌝)"))
   val decl_is_typedef =
        foldr (tree_has_leaf "storage-class-specifier" "typedef")
              false 
   val rewrite_typedef =
         tree_rewrite "%aquote" "%quote" 
            (Rewrite.compileRewrites
               (Rewrite.parse_rewrites_string
                  "⟦declaration \
                 \    ⌜w:declaration-specifiers⌝ \
                 \    (init-declarator-list \
                 \       ⌜x:init-declarator-list⌝ \
                 \       ⌜y:init-declarator⌝)⟧ = \
                 \ (declaration-list \
                 \    (declaration-list \
                 \       ⟦declaration ⌜w⌝ ⌜x⌝⟧) \
                 \    (declaration ⌜w⌝ (init-declarator-list ⌜y⌝)))"))
in
   fun insertNewTypedefNames nt =
      let fun add_typedef t (Term(name),acc) =
                 let val _ = () (* print ("add_typedef: adding "^name^"\n") *)
                     val () = new_typedef_name name t
                 in name::acc end
            | add_typedef _ (NonTerm(n,_),_) =
                 raise Fail ("Internal error: add_typedef: unexpected non-terminal "^n)
          fun add_typedef_names t =
               let val l = initdeclmatch (declrwt t)
               in
                  List.foldl (add_typedef t) [] 
                     (List.foldl (fn (t,a) => List.revAppend (idmatch t, a)) [] l)
               end
      in
         case nt
           of ("declaration",[specs,_]) =>
                if decl_is_typedef specs
                   then let val tdefs = rewrite_typedef
                                           (NonTerm ("declaration-list",[(NonTerm nt)]))
                            val tdl = decllistmatch tdefs
                            val ns = List.foldl
                                       (fn (t,a) => List.revAppend (add_typedef_names t,a))
                                       [] tdl
                        in rev ns end
                   else []
            | _ => []
      end
end;

local
   val idmatch = Rewrite.tree_match "%quote" 
          (Rewrite.compileRewrites
            (Rewrite.parse_rewrites_string
                "⟦struct-or-union-specifier ⌜x⌝ (identifier ⌜y⌝) ⌜z⌝⟧ = (⌜y⌝)\
               \ ⟦enum-specifier (identifier ⌜x⌝) ⌜y⌝⟧ = (⌜x⌝)"))
   val declmatch = Rewrite.tree_match "%quote" 
          (Rewrite.compileRewrites
            (Rewrite.parse_rewrites_string
                "⟦struct-or-union-specifier ⌜x⌝ (identifier ⌜y⌝) ⌜z⌝⟧ = (⌜z⌝)\
               \ ⟦enum-specifier (identifier ⌜y⌝) ⌜z⌝⟧ = (⌜z⌝)"))
in fun insertNewTags nt =
       case nt
         of ("declaration",_) =>
               let val t = NonTerm nt
                   val ids = idmatch t
                   val ds = declmatch t
               in case ds 
                    of [d] => List.app (fn (Term(name)) => new_tag_name name d
                                         | _ => ()) ids
                     | _ => ()
               end
          | _ => ()
end;

(* Interpreter semantics. Here we interpret the meaning of the
   abstract syntax. *)

fun mkNonTerm (nt as (s,_)) =
   let val tds = insertNewTypedefNames nt
       val _ = insertNewTags nt
       val _ = () (* print ("mkNonTerm: "^s^"\n") *)
   in
      NonTerm nt
   end;

fun mkTerm v = Term v;

(* Here we define some functions for processing abstract syntax
   representations of value declarations. *)


fun assqi n l =
  let fun lookup k =
     let fun iter [] = raise Fail ("Ffi.assqi: internal error: no value in "^
                                      n^" table for key "^(Int.toString k))
           | iter ((k',v)::ps) = if k' = k then v else iter ps
     in iter l
     end
  in lookup
  end;

fun assq n l =
  let fun lookup k =
     let fun iter [] = raise Fail ("Ffi.assq: internal error: no value in "^
                                               n^" table for key "^k)
           | iter ((k',v)::ps) = if k' = k then v else iter ps
     in iter l
     end
  in lookup
  end;

fun eval env e =
   let val lookup = assq "env" env
       fun ev (NonTerm("expression",[e])) = ev e
         | ev (NonTerm("assignment-expression",[e])) = ev e
         | ev (NonTerm("constant-expression",[e])) = ev e
         | ev (NonTerm("conditional-expression",[e])) = ev e
         | ev (NonTerm("conditional-expression",[c,e,e'])) = if (ev c = 0w0) then (ev e') else (ev e)
         | ev (NonTerm("logical-or-expression",[e])) = ev e
         | ev (NonTerm("logical-or-expression",[e,e'])) = if ev e <> 0w0 
                                                              then 0w1
                                                              else if ev e' = 0w0
                                                                      then 0w0
                                                                      else 0w1 
         | ev (NonTerm("logical-and-expression",[e])) = ev e
         | ev (NonTerm("logical-and-expression",[e,e'])) = if ev e = 0w0 
                                                              then 0w0 
                                                              else if ev e' = 0w0
                                                                      then 0w0
                                                                      else 0w1 
         | ev (NonTerm("inclusive-or-expression",[e])) = ev e
         | ev (NonTerm("inclusive-or-expression",[e,e'])) = Word.orb(ev e,ev e')
         | ev (NonTerm("exclusive-or-expression",[e])) = ev e
         | ev (NonTerm("exclusive-or-expression",[e,e'])) = Word.xorb(ev e,ev e')
         | ev (NonTerm("and-expression",[e])) = ev e
         | ev (NonTerm("and-expression",[e,e'])) = Word.andb(ev e,ev e')
         | ev (NonTerm("equality-expression",[e])) = ev e
         | ev (NonTerm("equality-expression",[e,NonTerm("equality-operator",[Term(s)]),e'])) =
                             (case s of "==" => if Word.compare(ev e,ev e') = EQUAL then 0w1 else 0w0
                                      | "!=" => if Word.compare(ev e,ev e') = EQUAL then 0w0 else 0w1
                                      | _ => raise raise Fail ("eval: no case for equality-operator "^s))
         | ev (NonTerm("relational-expression",[e])) = ev e
         | ev (NonTerm("relational-expression",[e,Term(s),e'])) =
                          let val (n,n') = (ev e, ev e')
                              val r = Word.compare(n,n')
                          in case s of "<" =>  if r = LESS    then 0w1 else 0w0
                                     | "<=" => if r = GREATER then 0w0 else 0w1
                                     | ">" =>  if r = GREATER then 0w1 else 0w0
                                     | ">=" => if r = LESS    then 0w0 else 0w1
                                     | _ => raise raise Fail ("eval: no case for relational-operator "^s)
                          end
         | ev (NonTerm("shift-expression",[e])) = ev e
         | ev (NonTerm("shift-expression",
                 [e as NonTerm("shift-expression",_),
                    Term(oper),
                  e' as NonTerm("additive-expression",_)])) =
                  (let val (n,n') = (ev e, ev e')
                   in case oper 
                        of "<<" => (Word.<<(n, n'))
                         | ">>" => (Word.>>(n, n'))
                         | s => raise Fail ("eval: no case for shift-operator "^s)
                   end handle Overflow => raise Fail ("eval: shift-expression: "^oper^" overflow"))
         | ev (NonTerm("additive-expression",[e])) = ev e
         | ev (NonTerm("additive-expression",
                 [e as NonTerm("additive-expression",_),
                    Term(oper),
                  e' as NonTerm("multiplicative-expression",_)])) =
                   let val (n,n') = (ev e, ev e')
                   in case oper 
                        of "+" => (Word.+(n, n'))
                         | "-" => (Word.-(n, n'))
                         | s => raise Fail ("eval: no case for additive-operator "^s)
                   end
         | ev (NonTerm("multiplicative-expression",[e])) = ev e
         | ev (NonTerm("multiplicative-expression",
                 [e as NonTerm("multiplicative-expression",_),
                    Term(oper),
                  e' as NonTerm("cast-expression",_)])) =
                   let val (n,n') = (ev e, ev e')
                   in case oper 
                        of "*" => (Word.*(n, n'))
                         | "/" => (Word.div(n, n'))
                         | "%" => (Word.mod(n, n'))
                         | s => raise Fail ("eval: no case for multiplicative-operator "^s)
                   end
         | ev (NonTerm("cast-expression",[e])) = ev e
         | ev (NonTerm("unary-expression",[e])) = ev e
         | ev (NonTerm("unary-expression",[NonTerm("unary-operator",[Term(oper)]),e])) =
                   let val n = ev e
                   in case oper (* FIXME: Moscow ML's 31 bit Word.word cannot represent > 0x7FFFFFFF *)
                        of "-" => Word.xorb(Word.-(n,0w1),0wx7FFFFFFF)
                         | "~" => Word.notb(n)
                         | "!" => if n <> 0w0 then 0w0 else 0w1
                         | s => raise Fail ("eval: no case for unary-operator "^s)
                   end
         | ev (NonTerm("postfix-expression",[e])) = ev e
         | ev (NonTerm("primary-expression",[e])) = ev e
         | ev (NonTerm("character-literal",[Term s])) = 
                  (case String.explode s
                     of [c] => Word.fromInt(Char.ord c)
                      | _ => raise Fail ("eval: invalid character literal #\""^s^"\"")) 
         | ev (NonTerm("integer-constant",[n,_])) = ev n
         | ev (NonTerm("integer-constant",[n])) = ev n
         | ev (NonTerm("integer",[NonTerm("decimal-integer",[Term s])])) =
                 (Option.valOf (StringCvt.scanString (Word.scan StringCvt.DEC) s) 
                         handle Overflow => raise Fail ("eval: decimal "^s^" is too big"))
         | ev (NonTerm("integer",[NonTerm("hexadecimal-integer",[Term s])])) =
                 (Option.valOf (StringCvt.scanString (Word.scan StringCvt.HEX) s)
                         handle Overflow => raise Fail ("eval: hexadecimal "^s^" is too big"))
         | ev (NonTerm("integer",[NonTerm("octal-integer",[Term s])])) =
                 (Option.valOf (StringCvt.scanString (Word.scan StringCvt.OCT) s) 
                         handle Overflow => raise Fail ("eval: octal "^s^" is too big"))
         | ev (NonTerm("identifier",[Term s])) = lookup s
         | ev (t as (NonTerm(s,_))) = (Rewrite.printTreeDirect 0 t;
                                       raise Fail ("eval: no case for non-terminal "^s))
         | ev (Term s) = raise Fail ("eval: no case for terminal "^s)
   in
      ev e
   end;

fun enumdec e =
   let fun enum (NonTerm("enumerator",
                    [NonTerm("enumeration-constant",
                        [NonTerm("identifier",[Term(s)])])]),a) = ((s,NONE)::a)
         | enum (NonTerm("enumerator",
                    [NonTerm("enumeration-constant",
                        [NonTerm("identifier",[Term(s)])]),
                     NonTerm("constant-expression",[c])]),a) = ((s,SOME c)::a)
         | enum _ = raise Fail "enum_dec: enum: no case"
       fun iter r (NonTerm("enumerator-list",
                       [e as (NonTerm("enumerator",_))]))
                          = enum (e,r)
         | iter r (NonTerm("enumerator-list",
                       [es as (NonTerm("enumerator-list",_)),
                         e as (NonTerm("enumerator",_))]))
                          = iter (enum (e,r)) es
         | iter _ _ = raise Fail "enum_dec: iter: no case"
       fun assign e =
           List.foldl (fn ((s,SOME c),a) => (s,eval a c)::a
                        | ((s,NONE),[]) => [(s,0w0)]
                        | ((s,NONE),a as ((_,n)::_)) => (s,Word.+(n,0w1))::a) 
                      [] e 
       val (e',_) = e
       val a = iter [] e'
   in rev (assign a) handle Overflow => raise Fail ("enum_dec: iter: Overflow")
   end;

fun decl_specs def =
   let fun add_sc {sc=sc,ts=ts,tq=tq,ls=ls,fs=fs} t = {sc=(t::sc),ts=ts,tq=tq,ls=ls,fs=fs}
       fun add_ts {sc=sc,ts=ts,tq=tq,ls=ls,fs=fs} t = {sc=sc,ts=(t::ts),tq=tq,ls=ls,fs=fs}
       fun add_tq {sc=sc,ts=ts,tq=tq,ls=ls,fs=fs} t = {sc=sc,ts=ts,tq=(t::tq),ls=ls,fs=fs}
       fun add_ls {sc=sc,ts=ts,tq=tq,ls=ls,fs=fs} t = {sc=sc,ts=ts,tq=tq,ls=(t::ls),fs=fs}
       fun add_fs {sc=sc,ts=ts,tq=tq,ls=ls,fs=fs} t = {sc=sc,ts=ts,tq=tq,ls=ls,fs=(t::fs)}
       fun decl r (NonTerm("storage-class-specifier",[t])) = add_sc r t
         | decl r (NonTerm("type-specifier",[t])) = add_ts r t
         | decl r (NonTerm("type-qualifier",[t])) = add_tq r t
         | decl r (NonTerm("alignment-specifier",[t])) = add_ls r t
         | decl r (NonTerm("function-specifier",[t])) = add_fs r t
         | decl _ t =  (Rewrite.printTreeDirect 0 t;
                        raise Fail "decl_specs: decl: no case")
       fun iter r (NonTerm("declaration-specifiers",
                           [ds,
                            ss as (NonTerm("declaration-specifiers",_))])) =
                                    iter (decl r ds) ss
         | iter r (NonTerm("declaration-specifiers",
                           [ds])) = decl r ds
         | iter r (NonTerm("specifier-qualifier-list",
                           [ss as (NonTerm("specifier-qualifier-list",_)),
                            ds])) = iter (decl r ds) ss
         | iter r (NonTerm("specifier-qualifier-list",
                           [ds])) = decl r ds
         | iter _ t =  (Rewrite.printTreeDirect 0 t;
                        raise Fail "decl_specs: iter: no case")
   in
      iter {sc=[],ts=[],tq=[],ls=[],fs=[]} def
   end;

fun decl_type {sc=sc,ts=[t],tq=tq,ls=ls,fs=fs} = SOME t 
  | decl_type _ = NONE;

fun decl_types {sc=sc,ts=ts,tq=tq,ls=ls,fs=fs} = ts;

fun decl_sc_p s {sc=[Term(s')],ts=ts,tq=tq,ls=ls,fs=fs} = s'=s
  | decl_sc_p _ _ = false;

val decl_is_typedef : {fs : Tree list, ls : Tree list, sc : Tree list, tq : Tree list,
                       ts : Tree list} -> bool = decl_sc_p "typedef"

val decl_is_extern : {fs : Tree list, ls : Tree list, sc : Tree list, tq : Tree list,
                       ts : Tree list} -> bool  = decl_sc_p "extern"

val decl_is_static : {fs : Tree list, ls : Tree list, sc : Tree list, tq : Tree list,
                       ts : Tree list} -> bool = decl_sc_p "static"

fun decl_tq_p s {sc=sc,ts=ts,tq=tq,ls=ls,fs=fs} =
       List.exists (fn s' => s' = Term(s)) tq

val decl_is_const : {fs : Tree list, ls : Tree list, sc : Tree list, tq : Tree list,
                       ts : Tree list} -> bool = decl_tq_p "const"

fun split_decls t = 
   let fun iter r (NonTerm("external-declarations",
                           [ts as (NonTerm("external-declarations",_)),
                            NonTerm("external-declaration",[t])])) = iter (t::r) ts
         | iter r (NonTerm("external-declarations",
                           [NonTerm("external-declaration",[t])])) = t::r

         | iter r _ = (Rewrite.printTreeDirect 0 t;
                       raise Fail "split_decls:iter: no case")
  in iter [] t
  end;

local
   val declrwt = 
         tree_rewrite "%aquote" "%quote" 
           (Rewrite.compileRewrites
             (Rewrite.parse_rewrites_string
                "⟦direct-declarator ⌜x:direct-declarator⌝             \
               \                    ⌜y:parameter-type-list⌝⟧ = (⌜x⌝)  \
               \ ⟦direct-declarator ⌜x:direct-declarator⌝             \
               \                    ⌜y:identifier-list⌝⟧ = (⌜x⌝)      \
               \ ⟦struct-or-union-specifier ⌜x⌝ ⌜y:identifier⌝ ⌜z⌝⟧ = \
               \               (struct-or-union-specifier ⌜x⌝ ⌜y⌝)"))
   val initdeclmatch = tree_match "%quote"
          (Rewrite.compileRewrites
            (Rewrite.parse_rewrites_string
                "⟦declaration \
               \    ⌜x:declaration-specifiers⌝ \
               \    (init-declarator-list ⌜y:init-declarator⌝)⟧ = \
               \ (⌜y⌝) \
               \ ⟦declaration ⌜x:declaration-specifiers⌝⟧ = \
               \ (⌜x⌝)"))
   val idmatch = tree_match "%quote" 
          (Rewrite.compileRewrites
            (Rewrite.parse_rewrites_string
                "⟦direct-declarator (identifier ⌜x⌝)⟧ = (⌜x⌝) \
               \ ⟦struct-or-union-specifier ⌜x⌝ (identifier ⌜y⌝)⟧ = (⌜y⌝)\
               \ ⟦struct-or-union-specifier ⌜x⌝ (identifier ⌜y⌝) ⌜z⌝⟧ = (⌜y⌝)\
               \ ⟦enum-specifier (identifier ⌜x⌝) ⌜y⌝⟧ = (⌜x⌝)"))
   val decllistmatch = tree_match "%quote"
          (Rewrite.compileRewrites
            (Rewrite.parse_rewrites_string
                "⟦declaration-list \
               \    ⌜x:declaration-list⌝ \
               \    ⌜y:declaration⌝⟧ = (⌜y⌝) \
               \ ⟦declaration-list \
               \    ⌜x:declaration⌝⟧ = (⌜x⌝)"))
   val rewrite_typedef =
         tree_rewrite "%aquote" "%quote" 
            (Rewrite.compileRewrites
               (Rewrite.parse_rewrites_string
                  "⟦declaration \
                 \    ⌜w:declaration-specifiers⌝ \
                 \    (init-declarator-list \
                 \       ⌜x:init-declarator-list⌝ \
                 \       ⌜y:init-declarator⌝)⟧ = \
                 \ (declaration-list \
                 \    (declaration-list \
                 \       ⟦declaration ⌜w⌝ ⌜x⌝⟧) \
                 \    (declaration ⌜w⌝ (init-declarator-list ⌜y⌝)))"))
in
   fun decl_id t =
         List.foldl (fn (t',a) => List.revAppend (idmatch t', a))
                     []
                    (initdeclmatch (declrwt t))
   fun declFoldl f a =
      fn t =>
            (case t
               of (NonTerm("declaration",_)) =>
                    let val tdefs = rewrite_typedef (NonTerm ("declaration-list",[t]))
                        val tdl = decllistmatch tdefs
                    in List.foldl f a tdl
                    end
                | _ => a)
end;

fun declsFoldl f =
   let fun iter r [] = r
         | iter r (t::ts) = 
             (case t
                of (NonTerm("declaration",_)) => iter (declFoldl f r t) ts
                 | _ => iter r ts)
   in fn a => fn t => iter a (split_decls t)
   end;

fun typedef def =
   let val e = case def 
                 of NonTerm("declaration",
                      [dss as (NonTerm("declaration-specifiers",_)),
                       e'' as (NonTerm("init-declarator-list",_))])
                      => (let val r = decl_specs dss
                              val e' = Option.valOf (decl_type r)
                          in if decl_is_typedef r 
                                then SOME (e',e'')
                                else NONE
                          end handle _ => NONE)
                  | _ => NONE
   in e
   end;

fun enum_typedef edef =
   let val e = case typedef edef
                 of SOME (NonTerm("enum-specifier",[e']),
                          e'') => SOME (e',e'')
                  | _ => NONE
   in e
   end;

val enumdeclmatch = tree_match "%quote"
          (Rewrite.compileRewrites
            (Rewrite.parse_rewrites_string
                "⟦declaration \
               \    (declaration-specifiers \
               \       (type-specifier \
               \          (enum-specifier ⌜y⌝)))⟧ = \
               \ (⌜y⌝)"));

fun compound_typedef struct_or_union def =
   let val e =
          case typedef def
            of SOME (NonTerm("type-specifier",
                             [e' as (NonTerm("struct-or-union-specifier",
                                             (NonTerm("struct-or-union",[Term(s)])::ts)))]),
                     e'') => if s = struct_or_union
                                then SOME (NonTerm(s,ts),e'')
                                else NONE
             | _ => NONE
   in e
   end;

fun struct_or_union_typedef struct_or_union def =
   let val e =
       case compound_typedef struct_or_union def
         of NONE => (NONE,NONE,def)
          | SOME (t,t') => 
              (case t  
                  of NonTerm(_,
                      [NonTerm("identifier", [Term(s)]),
                       NonTerm("struct-contents",[e'])]) => (SOME s, SOME e',t')
                   | NonTerm(_,
                      [NonTerm("struct-contents",[e'])]) => (NONE, SOME e',t')
                   | NonTerm(_,
                      [NonTerm("identifier", [Term(s)])]) => (SOME s,NONE,t')
                   | NonTerm(_,
                      [NonTerm("typedef-name", [Term(s)])]) => (SOME s,NONE,t')
                   | _ => (NONE,NONE,t'))
   in e
   end;

fun load_macros t =
   let fun plist param r (NonTerm("hash-define-parameter-list",
                       [e as (NonTerm("identifier",_))]))
                          = param (e,r)
         | plist param r (NonTerm("hash-define-parameter-list",
                       [es as (NonTerm("hash-define-parameter-list",_)),
                         e as (NonTerm("identifier",_))]))
                          = let val r = param (e,r) in plist param r es end
         | plist _ _ t = (Rewrite.printTreeDirect 0 t;
                          raise Fail "plist: no case")
       fun pdef (NonTerm("hash-definition",
                         [NonTerm("hash-token",
                                  [NonTerm(ttyp,
                                           [Term(s)])])])) =
                    if ttyp = "identifier" orelse ttyp = "text-frag"
                       then s
                       else (Rewrite.printTreeDirect 0 t;
                             raise Fail "pdef: no case")
         | pdef t = (Rewrite.printTreeDirect 0 t;
                     raise Fail "pdef: no case")
       fun plist_str l = plist
               (fn (NonTerm("identifier",[Term(s)]),r) =>
                    let val space = if r = "" then "" else ", "
                    in r^space^s
                    end
                 | _ => raise Fail "plist_str: no case") "" l
       fun hash_define (t as (NonTerm("hash-define",
                                 [NonTerm("identifier",[Term(s)])]))) =
                                  new_macro_name s (t,(NONE,""))
         | hash_define (NonTerm("hash-define",
                          [NonTerm("identifier",[Term(s)]),
                           t as (NonTerm("hash-definition",_))])) =
                                 new_macro_name s (t,(NONE,pdef t))
         | hash_define (t as (NonTerm("hash-define",
                                 [NonTerm("parametric-identifier",[Term(s)]),
                                  l as (NonTerm("hash-define-parameter-list",_)),
                                  t' as (NonTerm("hash-definition",_))]))) =
                                        new_macro_name s (t,(SOME (plist_str l),pdef t'))
         | hash_define (t as (NonTerm("hash-define",
                                 [NonTerm("parametric-identifier",[Term(s)]),
                                  l as (NonTerm("hash-define-parameter-list",
                                                [(NonTerm("identifier",[Term(s')]))]))]))) =
                                        new_macro_name s (t,(SOME (s'),""))
         | hash_define (t as (NonTerm("hash-define",
                                 [NonTerm("parametric-identifier",[Term(s)]),
                                  l as (NonTerm("hash-define-parameter-list",
                                                [NonTerm("hash-define-parameter-list",_),
                                                 NonTerm("identifier",_)]))]))) =
                                        new_macro_name s (t,(SOME (plist_str l),""))
         | hash_define (t as (NonTerm("hash-define",
                                 [NonTerm("parametric-identifier",[Term(s)]),
                                  l as (NonTerm("hash-define-parameter-list",
                                        [NonTerm("identifier",_)]))]))) =
                                        new_macro_name s (t,(SOME (plist_str l),""))
         | hash_define (t as (NonTerm("hash-define",
                                [NonTerm("parametric-identifier",[Term(s)]),
                                 (t' as NonTerm("hash-definition",_))]))) = 
                                 new_macro_name s (t,(SOME "",pdef t'))
         | hash_define t = (Rewrite.printTreeDirect 0 t;
                            raise Fail "hash_define: no case")
       fun iter (NonTerm("cpp-lines",[t as (NonTerm("hash-define",_))])) = hash_define t
         | iter (NonTerm("cpp-lines",[l as (NonTerm("cpp-lines",_)),
                                      t as (NonTerm("hash-define",_))])) = (iter l;
                                                                            hash_define t)
         | iter t = (Rewrite.printTreeDirect 0 t;
                     raise Fail "load_macros: iter: no case")
   in iter t
   end;

fun mlvar s =
   let fun iter [] = "us'"
         | iter (l as (c::cs)) = if c = #"_" then iter cs else implode l
   in iter (explode s)
   end 

fun enum_datatype tds s =
   let fun dedup l =
              List.foldr 
                 (fn (p as (s,n),a) => 
                     (case List.find (fn (_,n') => n'=n) a
                        of NONE => p::a
                         | SOME _ => a)) [] l
       val lut_ = assq "EnumTypedef" tds s
       val lut = List.map (fn (s,n) => (mlvar s,n)) lut_
       val dtl = List.foldl (fn ((s,n),a) => a^(if a = "" then "\n   [" else ",\n   ")^s) "" lut
       val dtdef = List.foldl (fn ((s,n),a) => a^(if a = "" then "\n    " else "\n  | ")^s) "" lut
       val ludef = List.foldl (fn ((s,n),a) => a^(if a = "" then "\n         " else "\n       | ")^
                                  "0wx"^(Word.toString n)^" => "^s) "" (dedup lut)
       val lusdef = List.foldl (fn ((s,n),a) => a^(if a = "" then "\n         " else "\n       | ")^
                                  "\""^s^"\" => "^s) "" (dedup lut)
       val uldef = List.foldl (fn ((s,n),a) => a^(if a = "" then "\n         " else "\n       | ")^
                                  s^" => "^"0wx"^(Word.toString n)) "" lut
       val ulsdef = List.foldl (fn ((s,n),a) => a^(if a = "" then "\n         " else "\n       | ")^
                                  s^" => \""^s^"\"") "" lut
   in "structure "^s^" = \nstruct\n"^
      "datatype "^s^"_enum ="^dtdef^"\n"^
      "type enum = "^s^"_enum\n"^
      "val flags = "^dtl^"]\n"^
      "\nfun fromWord n =\n    case n of"^ludef^"\n       | w => raise Fail (\""^s
                     ^".fromWord: 0wx\"^(Word.toString w)^\" is not a defined enumeration constant\")\n"^
      "\nfun toWord e =\n    case e of"^uldef^"\n"^
      "\nfun fromString s =\n    case s of"^lusdef^"\n       | w => raise Fail (\""^s
                     ^".fromString: \\\"\"^s^\"\\\" is not a defined enumeration constant\")\n"^
      "\nfun toString e =\n    case e of"^ulsdef^"\n"^
      "\nend\n"
   end;

fun lcprefix s1 s2 = 
   let fun iter r [] _ = r
         | iter r _ [] = r
         | iter r (c1::c1s) (c2::c2s) = if c1 = c2 then iter (c1::r) c1s c2s else r
   in String.implode (List.rev (iter [] (String.explode s1) (String.explode s2)))
   end;

fun lcprefixl [] = ""
  | lcprefixl (h::t) = List.foldl (fn (s,p) => lcprefix s p) h t;
