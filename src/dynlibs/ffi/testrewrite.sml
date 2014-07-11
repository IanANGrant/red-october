load "RewriteMain";
load "Rewrite";
load "CSyntax";

Meta.quotation := true;

fun match s = Rewrite.tree_match "%aquote" 
                   (Rewrite.compileRewrites
                      (Rewrite.parse_rewrites_string s));

fun rewrite s = Rewrite.tree_rewrite "%aquote" "%quote" 
                   (Rewrite.compileRewrites
                      (Rewrite.parse_rewrites_string s));

fun rewritetd s = Rewrite.tree_rewrite_td "%aquote" "%quote" 
                   (Rewrite.compileRewrites
                      (Rewrite.parse_rewrites_string s));

fun rewrite_param fns s = Rewrite.tree_rewrite_param "%aquote" "%quote" 
                   (Rewrite.compileRewrites
                      (Rewrite.parse_rewrites_string s)) fns;

fun rewrite_paramtd fns s = Rewrite.tree_rewrite_param_td "%aquote" "%quote" 
                   (Rewrite.compileRewrites
                      (Rewrite.parse_rewrites_string s)) fns;

fun printTree t = Rewrite.printTreeDirect 0 t;

fun cstring s = RewriteMain.parse_c_string s;

fun qlToString l =
   let fun iter r [] = r
         | iter r ((QUOTE s)::fs) = iter (r^s) fs
         | iter r ((ANTIQUOTE s)::fs) = iter (r^s) fs
   in iter "" l
   end;

local open GrammarSyntax
in
   type decls = {define_enum : string -> string, get_decl : string -> Tree,
                 get_macro : string -> Tree * (string option * string),
                 get_tag : string -> Tree, get_typedef : string -> Tree,
                 print_decl : string -> unit}
end

fun parse fname cppflags trunit =
let 
   open GrammarSyntax;
   open CSyntax;
   val cppcmd = "cpp -undef -std=iso9899:1999 "^cppflags;
   val cppmcmd = cppcmd^" -dM"
   val cdeft = RewriteMain.parse_cpp_pipe cppmcmd fname trunit;
   val _ = load_macros cdeft
   val nCMacros = List.length (macro_names());
   val cdeclt = RewriteMain.parse_c_pipe cppcmd fname trunit
   val (cdecls,canondecls) =
          declsFoldl
             (fn (t,(a,a')) =>
                    (case decl_id t
                       of [Term(s)] => ((s,t)::a,a')
                        | _ => (a,t::a')))
         ([],[]) cdeclt;
   val nCDecls = List.length cdecls
   val nCAnonDecls = List.length canondecls
   val CEnumTypedefs =
       List.foldl (fn ((n, SOME l),r) => (n,l)::r | ((_,NONE),r) => r)  []
                  (List.map  
                      (fn n => (n, Option.map enumdec ((enum_typedef o typedef_decl) n))) 
                      (typedef_names()));
   val (nCEnumTypedefs,
        nCEnumTypedefConsts) =
           List.foldl (fn ((_,l),(m,n)) => (m+1,n+List.length l)) (0,0) CEnumTypedefs;
   val CAnonEnums =
       let fun name [] = ""
             | name l = (case lcprefixl (List.map (fn (s,_) => s) l)
                           of "" => (#1 o hd) l
                            | n => n)
       in 
          rev (List.foldl
            (fn (t,a) => 
                case enumdeclmatch t
                  of [e] =>
                     let val l=enumdec (e,())
                         val ename = (name l)^"Enum"
                         val ename' = 
                            let fun iter n =
                                  if List.exists (fn (s,_) => s = (ename^(Int.toString n))) a
                                     then iter (n+1)
                                     else ename^(Int.toString n)
                            in if List.exists (fn (s,_) => s = ename) a
                                  then iter 2
                                  else ename
                            end 
                     in ((ename',l)::a)
                     end
                   | _ => a)
            []
            canondecls)
       end;
     val _ = print ("CPP Macro definitions: "^(Int.toString nCMacros))
     val _ = print ("\nNamed Declarations: "^(Int.toString nCDecls))
     val _ = print ("\nEnumeration Typedefs: "^(Int.toString nCEnumTypedefs))
     val _ = print (" (defining "^(Int.toString nCEnumTypedefConsts)^" constants)")
     val _ = print ("\nAnonymous Enumeration Declarations: "^(Int.toString nCAnonDecls))
     val _ = print "\n"
     fun hashtToList t = Hasht.fold (fn a => fn b => fn c => (a,b)::c) [] t
     val tags = hashtToList CSyntax.tag_table
     val macros = hashtToList CSyntax.macro_table
     val typedefs = List.map (fn (s,(t,_)) => (s,t)) (hashtToList CSyntax.typedef_table)
     fun find_cdecl ds = fn s => List.find (fn (s',t) => s'=s) ds;
     fun exists optn = case optn of SOME v => v | NONE => raise Subscript
     val cdecl = fn ds => #2 o exists o (find_cdecl ds)
     val print_cdecl = fn ds => (Rewrite.printTreeDirect 20) o (cdecl ds) 
     fun define_enum s = enum_datatype (CEnumTypedefs@CAnonEnums) s;
in {get_decl=cdecl cdecls, print_decl=print_cdecl cdecls, define_enum=define_enum,
    get_tag=cdecl tags, get_macro=cdecl macros, get_typedef=cdecl typedefs}
end;

fun parseq fname cppflags trunit =
   parse fname cppflags (qlToString trunit);

val rewriteq = rewrite o qlToString;

val rewritepq = fn params => (rewrite_param params) o qlToString;

val rewritetdq = rewritetd o qlToString;

val rewritetdpq = fn params => (rewrite_paramtd params) o qlToString;

val matchq = match o qlToString;

val _ = Rewrite.printTreeDirect 0
                (RewriteMain.parse_c_string "typedef unsigned int ui;\
                                           \ typedef ui *pui;\
                                           \ typedef unsigned int *pui2;");

(* Now we take the declaration-specifiers list of a typedef
   declaration and turn it into the implied specifier-qualifier-list
   of a structure declaration. This is a little involved, because we
   must eliminate all the declaration-specifiers other than
   type-specifiers (int, typedef-name, etc.) and type-qualifiers
   (const etc.), and we simultaneously turn the
   right-associative declaration-specifiers list into a
   left-associative specifier-qualifier-list.

   To be able to understand a set of rewrites like this it is
   important to know the order in which things happen. The rewrites
   are applied, depth-first (i.e. starting from the leaves,) to each
   leaf and branch of the abstract syntax tree. Each node (a branch or
   a leaf) is tested against each of the rules in the list, starting
   with the first rule. When one of the LHSs matches, the matching
   parameters ⌜x⌝ are substituted in the RHS of that rule, and then
   the function abstractions ⌜fn(⌜x⌝)⌝ are made. Finally the recursive
   rewrites (in ⟦semantic brackets⟧) are applied. When each branch is
   complete, the resulting term is substituted in the original tree,
   and the set of rewrites are then applied to that new branch,
   starting again with the first rule. So the rules below start out at
   the leaves, the first two of which rules introduce new types of
   branch which in turn trigger the introduction of more new branch
   types via the subsequent rules.

   It may be difficult to learn to read these rules before one has
   learned to write them. If so, then some writing practice will
   help. One might try implementing some simple functors like
   list.map, list.fold, list.rev etc. which act on abstract syntax
   representations of lists using cons and nil non-terminals. *)

val specqualrw = rewriteq `
   ⟦declaration-specifiers ⌜x:type-qualifier⌝⟧
    = (specifier-qualifier-list ⌜x⌝) 
   ⟦declaration-specifiers ⌜x:type-specifier⌝⟧
    = (specifier-qualifier-list ⌜x⌝)
   ⟦declaration-specifiers ⌜_⌝⟧
    = (⌜ε⌝)
   ⟦declaration-specifiers
      ⌜x:type-specifier⌝
      ⌜y:specifier-qualifier-list⌝⟧
    = (specifier-qualifier-list ⌜x⌝ ⌜y⌝) 
   ⟦declaration-specifiers
      ⌜x:type-qualifier⌝
      ⌜y:specifier-qualifier-list⌝⟧
    = (specifier-qualifier-list ⌜x⌝ ⌜y⌝) 
   ⟦declaration-specifiers
      ⌜x⌝
      ⌜y:specifier-qualifier-list⌝⟧
    = (⌜y⌝) 
   ⟦declaration (specifier-qualifier-list ⌜x⌝ ⌜y⌝) 
                (init-declarator-list 
                    (init-declarator ⌜z⌝))⟧
    = (struct-declaration
         ⟦specifier-qualifier-list
           (specifier-qualifier-list ⌜x⌝)
           ⌜y⌝⟧
         (struct-declarator-list
            (struct-declarator ⌜z⌝)))
   ⟦declaration (specifier-qualifier-list ⌜x⌝) 
                (init-declarator-list 
                    (init-declarator ⌜z⌝))⟧
    = (struct-declaration
         (specifier-qualifier-list ⌜x⌝)
         (struct-declarator-list
            (struct-declarator ⌜z⌝)))
   ⟦specifier-qualifier-list
      ⌜x:specifier-qualifier-list⌝
      (specifier-qualifier-list ⌜y⌝)⟧
    = (specifier-qualifier-list ⌜x⌝ ⌜y⌝)
   ⟦specifier-qualifier-list
      ⌜x:specifier-qualifier-list⌝
      (specifier-qualifier-list ⌜y⌝ ⌜z⌝)⟧
   = (⟦specifier-qualifier-list
         (specifier-qualifier-list ⌜x⌝ ⌜y⌝)
         ⌜z⌝⟧)`;

val split_structdecl_list = rewriteq `
    ⟦struct-declaration-list
        (struct-declaration
           ⌜w:specifier-qualifier-list⌝ 
           (struct-declarator-list
              ⌜x:struct-declarator-list⌝ 
              ⌜y:struct-declarator⌝))⟧ =
    (struct-declaration-list
       ⟦struct-declaration-list (struct-declaration ⌜w⌝ ⌜x⌝)⟧
       (struct-declaration ⌜w⌝ (struct-declarator-list ⌜y⌝)))
    ⟦struct-declaration-list
        ⌜v:struct-declaration-list⌝
        (struct-declaration
           ⌜w:specifier-qualifier-list⌝ 
           (struct-declarator-list
              ⌜x:struct-declarator-list⌝ 
              ⌜y:struct-declarator⌝))⟧ =
    (struct-declaration-list
       ⟦struct-declaration-list ⌜v⌝ (struct-declaration ⌜w⌝ ⌜x⌝)⟧
       (struct-declaration ⌜w⌝ (struct-declarator-list ⌜y⌝)))
    ⟦struct-declaration-list
       ⌜x:struct-declaration-list⌝⟧ = (⌜x⌝)`

fun foldStructDecl f = rewritepq [("f",f)] `
      ⟦struct-declaration-list
                 ⌜x:struct-declaration-list⌝
                 ⌜y:struct-declaration⌝⟧ =
      (struct-declaration-list ⌜x⌝ ⌜f(⌜y⌝)⌝)
      ⟦struct-declaration-list
                 ⌜x:struct-declaration⌝⟧ =
      (struct-declaration-list ⌜f(⌜x⌝)⌝)`;

fun foldParameterDecl f = rewritepq [("f",f)] `
      ⟦parameter-list
                 ⌜x:parameter-list⌝
                 ⌜y:parameter-declaration⌝⟧ =
      (parameter-list ⌜x⌝ ⌜f(⌜y⌝)⌝)
      ⟦parameter-list
                 ⌜x:parameter-declaration⌝⟧ =
      (parameter-list ⌜f(⌜x⌝)⌝)`;

val elimtdrw = rewriteq `                 
      ⟦declaration-specifiers
                 (storage-class-specifier typedef)
                 ⌜x:declaration-specifiers⌝⟧
      = (⟦⌜x⌝⟧)
      ⟦declaration-specifiers
            ⌜x⌝
            (declaration-specifiers
               (storage-class-specifier typedef))⟧
      = (⟦declaration-specifiers ⌜x⌝⟧)`;

val mergepointer = rewriteq `
      ⟦merge ⌜x:type-qualifier⌝
            (pointer ⌜w:type-qualifier-list⌝ ⌜z⌝)⟧
          = (pointer (type-qualifier-list ⌜w⌝ ⌜x⌝) ⌜z⌝)
      ⟦merge ⌜x:type-qualifier⌝
            (pointer ⌜z⌝)⟧
          = (pointer (type-qualifier-list ⌜x⌝) ⌜z⌝)
      ⟦merge ⌜x:type-qualifier⌝
            (pointer)⟧
          = (pointer (type-qualifier-list ⌜x⌝))`;

val pointer = ("pf",mergepointer)

fun mergedecltq dss tds = rewritepq [pointer] `
      ⟦merge (declaration-specifiers
                 ⌜x⌝
                 ⌜y:declaration-specifiers⌝)
              ⌜z⌝⟧
      = (⟦merge ⌜y⌝ ⟦merge (declaration-specifiers ⌜x⌝) ⌜z⌝⟧⟧)
      ⟦merge (declaration-specifiers
                 ⌜x:type-qualifier⌝)
             (declaration
                 ⌜y:declaration-specifiers⌝
                 (init-declarator-list 
                    (init-declarator
                       (declarator ⌜w:pointer⌝ ⌜z⌝))))⟧
      = (declaration
           ⌜y⌝
           (init-declarator-list
              (init-declarator
                 (declarator ⌜pf(merge ⌜x⌝ ⌜w⌝)⌝ ⌜z⌝))))
      ⟦merge (declaration-specifiers
                 ⌜x:type-qualifier⌝)
             (declaration
                 ⌜y:declaration-specifiers⌝
                 (init-declarator-list 
                    (init-declarator
                       (declarator
                          (direct-declarator
                             ⌜w:direct-declarator⌝)))))⟧
      = (declaration
           ⌜y⌝
           (init-declarator-list
              (init-declarator
                 (declarator
                    (direct-declarator
                       ⟦merge
                          (declaration-specifiers ⌜x⌝)
                          ⌜w⌝⟧)))))
      ⟦merge (declaration-specifiers
                 ⌜x:type-qualifier⌝)
             (declaration
                 ⌜y:declaration-specifiers⌝
                 (init-declarator-list 
                    (init-declarator
                       (declarator
                          (direct-declarator
                              ⌜w:direct-declarator⌝
                              ⌜z:parameter-type-list⌝)))))⟧
      = (declaration
           ⌜y⌝
           (init-declarator-list
              (init-declarator
                 (declarator
                    (direct-declarator
                       ⟦merge (declaration-specifiers ⌜x⌝) ⌜w⌝⟧ ⌜z⌝)))))
      ⟦merge (declaration-specifiers
                 ⌜x:type-qualifier⌝)
             (parameter-declaration
                 ⌜y:declaration-specifiers⌝
                 (declarator
                    (direct-declarator
                       ⌜w:direct-declarator⌝)))⟧
      = (declaration
           ⌜y⌝
           (declarator
              (direct-declarator
                 ⟦merge
                    (declaration-specifiers ⌜x⌝)
                    ⌜w⌝⟧)))
      ⟦merge (declaration-specifiers
                 ⌜_:type-specifier⌝)
             ⌜x⌝⟧
      = (⌜x⌝)
      ⟦merge (declaration-specifiers
                 ⌜x:type-qualifier⌝)
             (parameter-declaration
                 ⌜y:declaration-specifiers⌝
                 (declarator ⌜w:pointer⌝ ⌜z⌝))⟧
      = (parameter-declaration
           ⌜y⌝
           (declarator ⌜pf(merge ⌜x⌝ ⌜w⌝)⌝ ⌜z⌝))
      ⟦merge (declaration-specifiers
                 ⌜x:type-qualifier⌝)
             (direct-declarator ⌜w⌝ ⌜z:parameter-type-list⌝)⟧
      = (direct-declarator
           ⟦merge (declaration-specifiers ⌜x⌝) ⌜w⌝⟧ ⌜z⌝)
      ⟦merge (declaration-specifiers
                 ⌜x:type-qualifier⌝)
             (direct-declarator
                 (declarator ⌜w:pointer⌝ ⌜z⌝))⟧
      = (direct-declarator
           (declarator ⌜pf(merge ⌜x⌝ ⌜w⌝)⌝ ⌜z⌝))
      ⟦merge (declaration-specifiers
                 ⌜x⌝)
             (parameter-declaration
                 ⌜y:declaration-specifiers⌝
                 ⌜z⌝)⟧
      = (parameter-declaration
                 (declaration-specifiers ⌜x⌝ ⌜y⌝)
                 ⌜z⌝)
      ⟦merge (declaration-specifiers
                 ⌜x⌝)
             (declaration
                 ⌜y:declaration-specifiers⌝
                 ⌜z⌝)⟧
      = (declaration
                 (declaration-specifiers ⌜x⌝ ⌜y⌝)
                 ⌜z⌝)` (GrammarSyntax.NonTerm("merge",[dss,tds]));

(* merge type-qualifiers with structure declarations *)

fun mergestructtq sqs dss = rewritepq [pointer] `
      ⟦merge (specifier-qualifier-list
                 ⌜y:specifier-qualifier-list⌝
                 ⌜x⌝)
              ⌜z⌝⟧
      = (⟦merge ⌜y⌝ ⟦merge (specifier-qualifier-list ⌜x⌝) ⌜z⌝⟧⟧)
      ⟦merge (specifier-qualifier-list
                 ⌜x:type-qualifier⌝)
             (declaration
                 ⌜y:declaration-specifiers⌝
                 (init-declarator-list 
                    (init-declarator
                       (declarator ⌜w:pointer⌝ ⌜z⌝))))⟧
      = (declaration
           ⌜y⌝
           (init-declarator-list
              (init-declarator
                 (declarator ⌜pf(merge ⌜x⌝ ⌜w⌝)⌝ ⌜z⌝))))
      ⟦merge (specifier-qualifier-list
                 ⌜x:type-qualifier⌝)
             (struct-declaration
                 ⌜y:declaration-specifiers⌝
                 (struct-declarator-list 
                    (struct-declarator
                       (declarator
                          (direct-declarator
                             ⌜w:direct-declarator⌝)))))⟧
      = (struct-declaration
           ⌜y⌝
           (struct-declarator-list
              (struct-declarator
                 (declarator
                    (direct-declarator
                       ⟦merge (specifier-qualifier-list ⌜x⌝) ⌜w⌝⟧)))))
      ⟦merge (specifier-qualifier-list
                 ⌜_:type-specifier⌝)
             ⌜x⌝⟧
      = (⌜x⌝)
      ⟦merge (specifier-qualifier-list
                 ⌜x⌝)
             (declaration
                 ⌜y:declaration-specifiers⌝
                 ⌜z⌝)⟧
      = (declaration
                 (declaration-specifiers ⌜x⌝ ⌜y⌝)
                 ⌜z⌝)
      ⟦merge (specifier-qualifier-list
                 ⌜x:type-qualifier⌝)
             (struct-declaration
                 ⌜y:specifier-qualifier-list⌝
                 (struct-declarator-list 
                    (struct-declarator
                       (declarator ⌜w:pointer⌝ ⌜z⌝))))⟧
      = (struct-declaration
            ⌜y⌝
            (struct-declarator-list
               (struct-declarator
                  (declarator ⌜pf(merge ⌜x⌝ ⌜w⌝)⌝ ⌜z⌝))))
      ⟦merge (specifier-qualifier-list
                 ⌜x⌝)
             (struct-declaration
                 ⌜y:specifier-qualifier-list⌝
                 ⌜z⌝)⟧
      = (struct-declaration
                 (specifier-qualifier-list ⌜x⌝ ⌜y⌝)
                 ⌜z⌝)` (GrammarSyntax.NonTerm("merge",[sqs,dss]));

fun mergedss dss tds = rewriteq `
    ⟦merge (declaration-specifiers ⌜_:type-specifier⌝)
       ⌜t:=(declaration-specifiers ⌜_:type-specifier⌝)⌝⟧
    = (⌜t⌝)
    ⟦merge (declaration-specifiers ⌜_:type-specifier⌝ ⌜dss:declaration-specifiers⌝)
           (declaration-specifiers ⌜t:type-specifier⌝ ⌜tds⌝)⟧
         = (declaration-specifiers ⌜t⌝ ⟦merge ⌜dss⌝ ⌜tds⌝⟧)
    ⟦merge (declaration-specifiers ⌜ds⌝ ⌜dss:declaration-specifiers⌝)
       ⌜tds:declaration-specifiers⌝⟧
         = (declaration-specifiers ⌜ds⌝ ⟦merge ⌜dss⌝ ⌜tds⌝⟧)`
 (GrammarSyntax.NonTerm("merge",[dss,tds]));

fun dstDecl t =
  let open GrammarSyntax
  in case t
       of (NonTerm(s as "declaration",[t as (NonTerm("declaration-specifiers",_)),
                                  t' as (NonTerm("init-declarator-list",_))]))
            => (s,(t,t'))
        | (NonTerm(s as "struct-declaration",[t as (NonTerm("specifier-qualifier-list",_)),
                                         t' as (NonTerm("struct-declarator-list",_))]))
            => (s,(t,t'))
        | (NonTerm(s as "parameter-declaration",[t as (NonTerm("declaration-specifiers",_)),
                                            t']))
            => (s,(t,t'))
        | _ => (printTree t;raise Fail "dstDecl: bad decl")
  end;

fun mkDecl s (dss,idl) =
  let open GrammarSyntax
  in (NonTerm(s,[dss,idl]))
  end;

fun resolve substruct (decls : decls) t =
   let open CSyntax
       val typedef_decl = #get_typedef decls
       val tag_decl = #get_tag decls
       fun resolve_decl tags t =
           rewritetdpq [("dss",resolve_decl_specs tags),
                        ("scs",resolve_struct_contents tags),
                        ("fail",fn t => (Rewrite.printTree t; raise Fail "resolve_decl: no case"))] `
                             ⟦declaration ⌜x⌝ ⌜y⌝⟧ = (⌜dss(declaration ⌜x⌝ ⌜y⌝)⌝)
                             ⟦declaration ⌜t⌝⟧ = (declaration ⌜scs(⌜t⌝)⌝)
                             ⟦parameter-declaration ⌜x⌝ ⌜y⌝⟧ = (⌜dss(parameter-declaration ⌜x⌝ ⌜y⌝)⌝)
                             ⟦parameter-declaration ⌜t⌝⟧ = (parameter-declaration ⌜scs(⌜t⌝)⌝)
                             ⟦struct-declaration ⌜x⌝ ⌜y⌝⟧ = (⌜dss(struct-declaration ⌜x⌝ ⌜y⌝)⌝)
                             ⟦⌜t⌝⟧ = (⌜fail(⌜t⌝)⌝)` t
       and resolve_struct_contents tags t =
          let fun resolve_sc (NonTerm("sc",[t])) = foldStructDecl (resolve_decl tags) t
                | resolve_sc (NonTerm("sc",[Term(name),t])) = foldStructDecl (resolve_decl (name::tags)) t
                | resolve_sc t = (Rewrite.printTree t; raise Fail "resolve_sc: no case")
           in rewritetdpq [("resolve",resolve_sc),
                           ("split",split_structdecl_list),
                           ("fail",fn t => (Rewrite.printTree t;
                                            raise Fail "resolve_struct_contents: no case"))] `
                             ⟦declaration-specifiers ⌜t⌝⟧ = (declaration-specifiers ⟦⌜t⌝⟧)
                             ⟦type-specifier ⌜t⌝⟧ = (type-specifier ⟦⌜t⌝⟧)
                             ⟦struct-or-union-specifier
                                 ⌜su:struct-or-union⌝
                                 ⌜id:=(identifier ⌜n⌝)⌝
                                 (struct-contents ⌜t⌝)⟧ =
                             (struct-or-union-specifier
                                 ⌜su⌝
                                 ⌜id⌝
                                (struct-contents ⌜resolve(sc ⌜n⌝ ⌜split(⌜t⌝)⌝)⌝))
                             ⟦struct-or-union-specifier
                                 ⌜su:struct-or-union⌝
                                 (struct-contents ⌜t⌝)⟧ =
                             (struct-or-union-specifier
                                 ⌜su⌝
                                (struct-contents ⌜resolve(sc ⌜split(⌜t⌝)⌝)⌝))
                             ⟦⌜t⌝⟧ = (⌜fail(⌜t⌝)⌝)` t
           end
       and resolve_decl_specs tags t =
       let val fname = "resolve_decl_specs"
           val (decl_class,(tdd,tdid)) = dstDecl (debug fname "" t)
       in case decl_types (decl_specs tdd)
            of [NonTerm("typedef-name",[Term(n)])] =>
                  let val tddecl = (typedef_decl n)
                          handle Subscript => raise Fail (fname^": can't find typedef "^n)
                      val td = debug fname "resolve_decl returned" (resolve_decl tags tddecl)
                      val td' = if decl_class = "struct-declaration"
                                   then debug fname "mergestructtq returned"
                                           (mergestructtq tdd
                                                (debug fname "specqualrw returned" (specqualrw td)))
                                   else debug fname "mergedecltq returned" (mergedecltq tdd td)
                      val (_,(tdd',tdid')) = dstDecl (td')
                      val rtd = debug fname "resolve_decls returned" (resolve_decls tags tdid tdid')
                   in elimtdrw (mkDecl decl_class (tdd', rtd))
                  end
             | [NonTerm("struct-or-union-specifier",
                        [su,id as NonTerm("identifier",
                                          [Term(name)])])] =>
                  let val tdopt = SOME (tag_decl name) handle Subscript => NONE
                      val nop = fn () => mkDecl 
                                           decl_class 
                                           (mergedss tdd
                                                  (NonTerm("declaration-specifiers",
                                                           [NonTerm("type-specifier",
                                                               [NonTerm("struct-or-union-specifier",
                                                                        [su,id])])])),
                                            tdid)
                  in if not substruct then nop () else
                   case tdopt
                       of NONE => nop ()
                        | SOME td =>
                            if List.exists (fn s => s = name) tags
                               then nop () 
                               else resolve_decl (name::tags)
                                        (mkDecl 
                                           decl_class 
                                           (mergedss tdd
                                                  (NonTerm("declaration-specifiers",
                                                           [NonTerm("type-specifier",
                                                               [NonTerm("struct-or-union-specifier",
                                                                        [su,id,td])])])),
                                            tdid))
                  end
             | [NonTerm("struct-or-union-specifier",
                        [su,id as NonTerm("identifier",
                                          [Term(name)]) ,NonTerm("struct-contents",[sc])])] =>
                  let val sc' = foldStructDecl
                                     (resolve_decl (name::tags))
                                     (split_structdecl_list sc)
                      val td = NonTerm("struct-contents",[sc']) 
                  in mkDecl
                       decl_class
                       (mergedss tdd (NonTerm("declaration-specifiers",
                                          [NonTerm("type-specifier",
                                                   [NonTerm("struct-or-union-specifier",
                                                            [su,id,td])])])),
                        tdid)
                  end
             | [NonTerm("enum-specifier",_)] =>
                  let
                  in mkDecl
                       decl_class
                       (mergedss tdd (NonTerm("declaration-specifiers",
                                          [NonTerm("type-specifier",
                                                   [Term("int")])])),
                        tdid)
                  end
             | _ => t
       end
    and new_resolve_decls tags t t' =
          let val fold_pl = foldParameterDecl (resolve_decl tags)
          in rewritetdpq [("fold",fold_pl),
                          ("fail",fn t => (Rewrite.printTree t;
                                            raise Fail "resolve_decls: no case"))] `
                             ⟦resolve (init-declarator-list ⌜t1⌝)
                                      (init-declarator-list ⌜t2⌝)⟧ =
                             (init-declarator-list ⟦resolve ⌜t1⌝ ⌜t2⌝⟧)
                             ⟦resolve ⌜t1:declarator⌝
                                  (init-declarator-list
                                     (init-declarator ⌜t2:declarator⌝))⟧ =
                             (⟦resolve ⌜t1⌝ ⌜t2⌝⟧)
                             ⟦resolve
                                  (init-declarator ⌜t1⌝)
                                  (init-declarator ⌜t2⌝)⟧ =
                             (init-declarator ⟦resolve ⌜t1⌝ ⌜t2⌝⟧)
                             ⟦resolve
                                  (struct-declarator-list ⌜t1⌝)
                                  (struct-declarator-list ⌜t2⌝)⟧ =
                             (struct-declarator-list ⟦resolve ⌜t1⌝ ⌜t2⌝⟧)
                             ⟦resolve
                                  (struct-declarator ⌜t1⌝)
                                  (struct-declarator ⌜t2⌝)⟧ =
                             (struct-declarator ⟦resolve ⌜t1⌝ ⌜t2⌝⟧)
                             ⟦resolve
                                   (declarator
                                     (direct-declarator
                                         ⌜dd:direct-declarator⌝
                                         (parameter-type-list ⌜l:parameter-list⌝)))
                                   (declarator
                                      (direct-declarator ⌜_:identifier⌝))⟧ =
                             (declarator
                                 (direct-declarator
                                     ⌜dd⌝
                                     (parameter-type-list ⌜fold(⌜l⌝)⌝)))
                             ⟦resolve
                                   (declarator
                                     ⌜p⌝
                                     (direct-declarator
                                         ⌜dd:direct-declarator⌝
                                         (parameter-type-list ⌜l:parameter-list⌝)))
                                   (declarator
                                      (direct-declarator ⌜_:identifier⌝))⟧ =
                             (declarator
                                 ⌜p⌝
                                 (direct-declarator
                                     ⌜dd⌝
                                     (parameter-type-list ⌜fold(⌜l⌝)⌝)))
                             ⟦resolve ⌜t⌝
                                  (declarator
                                     (direct-declarator ⌜_:identifier⌝))⟧ = (⌜t⌝)
                             ⟦resolve 
                                   (declarator
                                     ⌜p1:pointer⌝
                                     ⌜d1:direct-declarator⌝)
                                   (declarator
                                     (pointer ⌜p2:pointer⌝)
                                     ⌜d2:direct-declarator⌝)⟧ =
                             (⟦resolve (declarator
                                          ⌜p1⌝ ⌜d1⌝)
                                       (declarator
                                          (pointer ⌜p2⌝ ⌜d2⌝))⟧)
`        end 
   and fun mergepointer t = rewriteq `
      ⟦merge ⌜x:type-qualifier⌝
            (pointer ⌜w:type-qualifier-list⌝ ⌜z⌝)⟧
          = (pointer (type-qualifier-list ⌜w⌝ ⌜x⌝) ⌜z⌝)
      ⟦merge ⌜x:type-qualifier⌝
            (pointer ⌜z⌝)⟧
          = (pointer (type-qualifier-list ⌜x⌝) ⌜z⌝)
      ⟦merge ⌜x:type-qualifier⌝
            (pointer)⟧
          = (pointer (type-qualifier-list ⌜x⌝))` t
    and resolve_decls tags t t' =
       let fun iter (NonTerm("init-declarator-list",[t]))
                    (NonTerm("init-declarator-list",[t'])) =
                     NonTerm("init-declarator-list",[iter t t'])
             | iter (t as NonTerm("declarator",_))
                    (NonTerm("init-declarator-list",
                             [NonTerm("init-declarator",
                                      [t' as NonTerm("declarator",_)])])) =
                     iter t t'
             | iter (NonTerm("init-declarator",[t]))
                    (NonTerm("init-declarator",[t'])) =
                     NonTerm("init-declarator",[iter t t'])
             | iter (NonTerm("struct-declarator-list",[t]))
                    (NonTerm("struct-declarator-list",[t'])) =
                     NonTerm("struct-declarator-list",[iter t t'])
             | iter (NonTerm("struct-declarator",[t]))
                    (NonTerm("struct-declarator",[t'])) =
                     NonTerm("struct-declarator",[iter t t'])
             | iter (NonTerm("declarator",
                             [NonTerm("direct-declarator",
                                      [dd as NonTerm("direct-declarator",_),
                                       NonTerm("parameter-type-list",
                                               [pl as NonTerm("parameter-list",_)])])]))
                    (NonTerm("declarator",
                             [NonTerm("direct-declarator",
                                      [NonTerm("identifier",_)])])) =
                    (NonTerm("declarator",
                             [NonTerm("direct-declarator",
                                      [dd,NonTerm("parameter-type-list",
                                                  [foldParameterDecl (resolve_decl tags) pl])])]))
             | iter (NonTerm("declarator",
                             [p,
                              NonTerm("direct-declarator",
                                      [dd as NonTerm("direct-declarator",_),
                                       NonTerm("parameter-type-list",
                                               [pl as NonTerm("parameter-list",_)])])]))
                    (NonTerm("declarator",
                             [NonTerm("direct-declarator",
                                      [NonTerm("identifier",_)])])) =
                    (NonTerm("declarator",
                             [p, NonTerm("direct-declarator",
                                      [dd,NonTerm("parameter-type-list",
                                                  [foldParameterDecl (resolve_decl tags) pl])])]))
             | iter t (NonTerm("declarator",
                               [NonTerm("direct-declarator",
                                  [NonTerm("identifier",_)])])) = t
(*             | iter t (NonTerm("declarator",
                                 [NonTerm("direct-declarator",
                                          [d as NonTerm("declarator",_)])])) = 
                         NonTerm("declarator",
                                 [NonTerm("direct-declarator",
                                          [iter t d])]) *)
             | iter (NonTerm("declarator",
                             [p as NonTerm("pointer",_),
                              d as NonTerm("direct-declarator",_)]))
                    (NonTerm("declarator",
                             [NonTerm("pointer",l as [NonTerm("pointer",_)]),
                              d' as NonTerm("direct-declarator",_)])) =
                iter (NonTerm("declarator",[NonTerm("pointer",[p]),d]))
                     (NonTerm("declarator",l@[d']))
             | iter (NonTerm("declarator",
                             [p as NonTerm("pointer",_),
                              d as NonTerm("direct-declarator",_)]))
                    (NonTerm("declarator",
                             [NonTerm("pointer",l),
                              d' as NonTerm("direct-declarator",_)])) =
                iter (NonTerm("declarator",[NonTerm("pointer",l@[p]),d]))
                     (NonTerm("declarator",[d']))
             | iter (NonTerm("declarator",
                             [d as NonTerm("direct-declarator",_)]))
                    (NonTerm("declarator", [NonTerm("pointer",l as [NonTerm("pointer",_)]),
                                            d' as NonTerm("direct-declarator", _)])) =
                iter (NonTerm("declarator",[NonTerm("pointer",[]),d]))
                     (NonTerm("declarator",l@[d']))
             | iter (NonTerm("declarator",
                             [d as NonTerm("direct-declarator",_)]))
                    (NonTerm("declarator", [p as NonTerm("pointer",_),
                                            d' as NonTerm("direct-declarator", _)])) =
                iter (NonTerm("declarator",[p,d]))
                     (NonTerm("declarator",[d']))
             | iter (NonTerm("declarator",
                             [d as NonTerm("direct-declarator",_)]))
                    (NonTerm("declarator",
                             [NonTerm("direct-declarator",
                                      [dd,ad as NonTerm("array-declarator",_)])])) =
                iter (NonTerm("declarator",[NonTerm("direct-declarator",[d,ad])]))
                     (NonTerm("declarator",[dd]))
             | iter (d as (NonTerm("declarator",
                             [NonTerm("direct-declarator",_)])))
                    (NonTerm("declarator",
                             [NonTerm("direct-declarator",
                                      [NonTerm("direct-declarator",[d']),
                                       ptl as NonTerm("parameter-type-list",_)])])) =
                (NonTerm("declarator",
                         [NonTerm("direct-declarator",
                                  [NonTerm("direct-declarator",
                                           [iter d d'])]),ptl]))
             | iter t t' = (Rewrite.printTreeDirect 0 t; Rewrite.printTreeDirect 0 t';
                            raise Fail "resolve_decls: no case")
       in iter t t'
       end
   in resolve_decl [] t
   end;

val td = parseq "structdef" "" `
  typedef unsigned int *pui;
  typedef unsigned int ui;
  struct tag {const pui puiv, *ppui; const ui *pcui;};
  extern ui func (const ui *pcui);
  static const ui *puiv;  
  static const pui *ppuiv;  
  typedef struct tag *ps;`;

val _ = printTree (#get_tag td "tag");
val _ = printTree (resolve true td (#get_decl td "ps"));
val _ = printTree (resolve true td (#get_decl td "puiv"));
val _ = printTree (resolve true td (#get_decl td "ppuiv"));
val _ = printTree (resolve true td (#get_decl td "func"));

val fdecl = parseq "fdecl" "" `
   typedef unsigned int ui;
   typedef ui ( *funcp ) (ui i);
   extern ui *func (ui i, const funcp cfp[]);`;

val _ = printTree (#get_decl fdecl "func");
val _ = printTree (resolve true fdecl (#get_decl fdecl "func"));
val _ = printTree (#get_typedef fdecl "ui");
val _ = printTree (resolve true fdecl (#get_decl fdecl "funcp"));

val cpdecl = parseq "cpdecl" "" `
   typedef unsigned int *pui;
   typedef const pui *cppui;
   typedef unsigned int * const *cppui2;`;

val _ = printTree (resolve true cpdecl (#get_typedef cpdecl "cppui"));
val _ = printTree (#get_typedef cpdecl "cppui2");

val ctdecl = parseq "ctdecl" "" `
   typedef unsigned int ui;
   typedef const ui cui;`;

val _ = printTree (resolve true ctdecl (#get_typedef ctdecl "cui"));

val gdkdecls = parse "gdk/gdk.h" "$(pkg-config --cflags-only-I gdk-3.0)" "#include \"gdk/gdk.h\"\n";
val _ = #print_decl gdkdecls "gdk_window_new";
val _ = Meta.exec (#define_enum gdkdecls "GdkEventType");

val _ = printTree (resolve true gdkdecls (#get_decl gdkdecls "_GdkWindowAttr"));
val _ = printTree (resolve true gdkdecls (#get_typedef gdkdecls "GdkWindowAttr"));
val _ = printTree (resolve true gdkdecls (#get_typedef gdkdecls "GdkWindow"));
val _ = printTree (resolve true gdkdecls (#get_decl gdkdecls "gdk_window_new"));
val _ = printTree (resolve false gdkdecls (#get_decl gdkdecls "gdk_window_new"));


(*           val _ = GrammarSyntax.debug_on "tree_rewrite_param" (GrammarSyntax.On 10)
             val _ = GrammarSyntax.debug_on "resolve_decl_specs" (GrammarSyntax.On 10)
             val _ = GrammarSyntax.debug_on "matches" (GrammarSyntax.On 10)
             val _ = GrammarSyntax.debug_on "tree_tree_list_subst_leaves" (GrammarSyntax.On 10) *)
