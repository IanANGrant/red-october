val () = app Meta.load
   ["Int", "Real", "Mosml", "Substring", "Regex", "Listsort", "Jit",
    "BitSet", "RewriteMain", "CSyntax", "Prolog"];

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
                 get_macro : string -> Tree * (string list option * string),
                 get_tag : string -> Tree, get_typedef : string -> Tree,
                 print_decl : string -> unit, define_enum_hash : string -> string, 
                 macros : unit -> (string * (Tree * (string list option * string))) list}
end

fun parse fname cppflags trunit =
let 
   open GrammarSyntax
   open CSyntax
   val cppcmd = "LD_LIBRARY_PATH=/home/ian3/usr/lib cpp -undef -std=iso9899:1999 "^cppflags
   val cppmcmd = cppcmd^" -dM"
   val cdeft = RewriteMain.parse_cpp_pipe cppmcmd fname trunit
   val _ = load_macros cdeft
   val nCMacros = List.length (macro_names())
     fun hashtToList t = Hasht.fold (fn a => fn b => fn c => (a,b)::c) [] t
     val macros = hashtToList CSyntax.macro_table
   val cdeclt = RewriteMain.parse_c_pipe cppcmd fname trunit
   val (cdecls,canondecls) =
          declsFoldl
             (fn (t,(a,a')) =>
                    (case decl_id t
                       of [Term(s)] => ((s,t)::a,a')
                        | _ => (a,t::a')))
         ([],[]) cdeclt
   val nCDecls = List.length cdecls
   val nCAnonDecls = List.length canondecls
   val CEnumTypedefs =
       List.foldl (fn ((n, SOME l),r) => (n,l)::r | ((_,NONE),r) => r)  []
                  (List.map  
                      (fn n => (n, Option.map enumdec ((enum_typedef o typedef_decl) n))) 
                      (typedef_names()));
   val (nCEnumTypedefs,
        nCEnumTypedefConsts) =
           List.foldl (fn ((_,l),(m,n)) => (m+1,n+List.length l)) (0,0) CEnumTypedefs
   val CAnonEnums =
       let fun name [] = ""
             | name l = (case lcprefixl (List.map (fn (s,_) => mlvar s) l)
                           of "" => (#1 o hd) l
                            | n => n)
       in 
          rev (List.foldl
            (fn (t,a) => 
                case enumdeclmatch t
                  of [e] =>
                     let val l=enumdec (e,())
                         val ename = (name l)^"Consts"
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
       end
     val _ = print ("CPP Macro definitions: "^(Int.toString nCMacros))
     val _ = print ("\nNamed Declarations: "^(Int.toString nCDecls))
     val _ = print ("\nEnumeration Typedefs: "^(Int.toString nCEnumTypedefs))
     val _ = print (" (defining "^(Int.toString nCEnumTypedefConsts)^" constants)")
     val _ = print ("\nAnonymous Enumeration Declarations: "^(Int.toString nCAnonDecls))
     val _ = print "\n"
     val tags = hashtToList CSyntax.tag_table
     val typedefs = List.map (fn (s,(t,_)) => (s,t)) (hashtToList CSyntax.typedef_table)
     fun find_cdecl ds = fn s => List.find (fn (s',t) => s'=s) ds
     fun exists optn = case optn of SOME v => v | NONE => raise Subscript
     val cdecl = fn ds => #2 o exists o (find_cdecl ds)
     val print_cdecl = fn ds => (Rewrite.printTreeDirect 20) o (cdecl ds) 
     fun define_enum s = enum_datatype (CEnumTypedefs@CAnonEnums) s
     fun define_enum_hash s = enum_hash (CEnumTypedefs@CAnonEnums) s
in {get_decl=cdecl cdecls, print_decl=print_cdecl cdecls, define_enum=define_enum,
    define_enum_hash=define_enum_hash,
    get_tag=cdecl tags, get_macro=cdecl macros, get_typedef=cdecl typedefs, macros=(fn () => macros)}
end

fun parseq fname cppflags trunit =
   parse fname cppflags (qlToString trunit)

val rewriteq = rewrite o qlToString

val rewritepq = fn params => (rewrite_param params) o qlToString

val rewritetdq = rewritetd o qlToString

val rewritetdpq = fn params => (rewrite_paramtd params) o qlToString

val matchq = match o qlToString

val _ = Rewrite.printTreeDirect 0
                (RewriteMain.parse_c_string "typedef unsigned int ui;\
                                           \ typedef ui *pui;\
                                           \ typedef unsigned int *pui2;")

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
      (parameter-list ⌜f(⌜x⌝)⌝)`

val elimtdrw = rewriteq `                 
      ⟦declaration-specifiers
                 (storage-class-specifier typedef)
                 ⌜x:declaration-specifiers⌝⟧
      = (⟦⌜x⌝⟧)
      ⟦declaration-specifiers
            ⌜x⌝
            (declaration-specifiers
               (storage-class-specifier typedef))⟧
      = (⟦declaration-specifiers ⌜x⌝⟧)`

val mergepointer = rewriteq `
      ⟦merge ⌜x:type-qualifier⌝
            (pointer ⌜w:type-qualifier-list⌝ ⌜z⌝)⟧
          = (pointer (type-qualifier-list ⌜w⌝ ⌜x⌝) ⌜z⌝)
      ⟦merge ⌜x:type-qualifier⌝
            (pointer ⌜z⌝)⟧
          = (pointer (type-qualifier-list ⌜x⌝) ⌜z⌝)
      ⟦merge ⌜x:type-qualifier⌝
            (pointer)⟧
          = (pointer (type-qualifier-list ⌜x⌝))`

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
                 ⌜z⌝)` (GrammarSyntax.NonTerm("merge",[dss,tds]))

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
                 ⌜z⌝)` (GrammarSyntax.NonTerm("merge",[sqs,dss]))

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
 (GrammarSyntax.NonTerm("merge",[dss,tds]))

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
  end

fun mkDecl s (dss,idl) =
  let open GrammarSyntax
  in (NonTerm(s,[dss,idl]))
  end

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
             | ((Term_)::_) =>
                  let val rtd = debug fname "resolve_decls returned" 
                                  (resolve_decls tags tdid 
                                     (NonTerm("init-declarator-list",
                                              [NonTerm("init-declarator",
                                                       [NonTerm("declarator",
                                                                [NonTerm("direct-declarator",
                                                                         [NonTerm("identifier",
                                                                                  [Term("fred")])])])])])))
                   in mkDecl decl_class (tdd, rtd)
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
   and mergepointer t = rewriteq `
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
   end

val td = parseq "structdef" "" `
  typedef unsigned int *pui;
  typedef unsigned int ui;
  struct tag {const pui puiv, *ppui; const ui *pcui;};
  extern ui func (const ui *pcui);
  static const ui *puiv;  
  static const pui *ppuiv;  
  typedef struct tag *ps;`

val _ = printTree (#get_tag td "tag")
val _ = printTree (resolve true td (#get_decl td "ps"))
val _ = printTree (resolve true td (#get_decl td "puiv"))
val _ = printTree (resolve true td (#get_decl td "ppuiv"))
val _ = printTree (resolve true td (#get_decl td "func"))

val fdecl = parseq "fdecl" "" `
   typedef unsigned int ui;
   typedef ui ( *funcp ) (ui i);
   extern ui *func (ui i, const funcp cfp[]);
`;

val _ = printTree (#get_decl fdecl "func")
val _ = printTree (resolve true fdecl (#get_decl fdecl "func"))
val _ = printTree (#get_typedef fdecl "ui")
val _ = printTree (resolve true fdecl (#get_decl fdecl "funcp"))

val cpdecl = parseq "cpdecl" "" `
   typedef unsigned int *pui;
   typedef const pui *cppui;
   typedef unsigned int * const *cppui2;
`;

val _ = printTree (resolve true cpdecl (#get_typedef cpdecl "cppui"))
val _ = printTree (#get_typedef cpdecl "cppui2")

val ctdecl = parseq "ctdecl" "" `
   typedef long int size_t;
   typedef long unsigned int __off_t;
   typedef void *ptr_t;
   extern void *mmap (void *__addr, size_t __len, int __prot, int __flags, int __fd, __off_t __offset);
`;

val _ = printTree (resolve true ctdecl (#get_typedef ctdecl "size_t"))
val _ = printTree (resolve true ctdecl (#get_decl ctdecl "mmap"))

val enumdecl = parseq "enumdecl" "" `
typedef enum scm_t_foreign_type
  {
    SCM_FOREIGN_TYPE_VOID,
    SCM_FOREIGN_TYPE_FLOAT,
    SCM_FOREIGN_TYPE_DOUBLE,
    SCM_FOREIGN_TYPE_UINT8,
    SCM_FOREIGN_TYPE_INT8,
    SCM_FOREIGN_TYPE_UINT16,
    SCM_FOREIGN_TYPE_INT16,
    SCM_FOREIGN_TYPE_UINT32,
    SCM_FOREIGN_TYPE_INT32,
    SCM_FOREIGN_TYPE_UINT64,
    SCM_FOREIGN_TYPE_INT64,
    SCM_FOREIGN_TYPE_LAST = SCM_FOREIGN_TYPE_INT64
  } scm_t_foreign_type;
`;

val _ = printTree ((#get_decl enumdecl "scm_t_foreign_type"))
val _ = printTree ((#get_tag enumdecl "scm_t_foreign_type"))
val _ = print (#define_enum enumdecl "scm_t_foreign_type")

(*
val _ = Meta.quietdec := true

val gdkdecls = parse "gdk/gdk.h" "$(pkg-config --cflags-only-I gdk-3.0)" "#include \"gdk/gdk.h\"\n"

val _ = Meta.quietdec := false;

val _ = #print_decl gdkdecls "gdk_window_new";
val _ = Meta.exec (#define_enum gdkdecls "GdkEventType");

val _ = printTree (resolve true  gdkdecls (#get_decl    gdkdecls "_GdkWindowAttr"))
val _ = printTree (resolve true  gdkdecls (#get_typedef gdkdecls "GdkWindowAttr"))
val _ = printTree (resolve true  gdkdecls (#get_typedef gdkdecls "GdkWindow"))
val _ = printTree (resolve true  gdkdecls (#get_decl    gdkdecls "gdk_window_new"))
val _ = printTree (resolve false gdkdecls (#get_decl    gdkdecls "gdk_window_new"))
*)

(*           val _ = GrammarSyntax.debug_on "tree_rewrite_param" (GrammarSyntax.On 10)
             val _ = GrammarSyntax.debug_on "resolve_decl_specs" (GrammarSyntax.On 10)
             val _ = GrammarSyntax.debug_on "matches" (GrammarSyntax.On 10)
             val _ = GrammarSyntax.debug_on "tree_tree_list_subst_leaves" (GrammarSyntax.On 10) *)

fun findmacros (pat,pat') =
    let val regex = Regex.regcomp pat [Regex.Extended]
        val regex' = Regex.regcomp pat' [Regex.Extended]
    in List.filter (fn (n,d) => Regex.regexecBool regex [] n andalso
                                Regex.regexecBool regex' [] d)
    end

val evalStringInEnv = fn env => (CSyntax.eval env) o RewriteMain.parse_c_cexp_string
val evalString = evalStringInEnv []
val ascval = fn ((n,d),(n',d')) => (Word.compare (evalString d, evalString d'))
val ascnm =  fn ((n,d),(n',d')) => (String.compare (n, n'))
val sortByAscVal =  Listsort.sort ascval
val sortByAscName = Listsort.sort ascnm
val IntegerConstant = "^(0x[0-9a-fA-F]+|[0-9]+)[LUlu]*$"

val mmandecls = parse "sys/mman.h" "" "#include <unistd.h>\n#include <sys/mman.h>\n"
val simple =     List.filter (fn (_,(_,(NONE,_))) => true | _ => false)
val mmapmacros = List.map (fn (n,(_,(a,d))) => (n,d)) (simple ((#macros mmandecls)()))

val mmapraw =     printTree                          (#get_decl mmandecls "mmap")
val mmap =        printTree (resolve false mmandecls (#get_decl mmandecls "mmap"))
val getpagesize = printTree (resolve false mmandecls (#get_decl mmandecls "getpagesize"))
val munmap =      printTree (resolve true  mmandecls (#get_decl mmandecls "munmap"))
val msync =       printTree (resolve true  mmandecls (#get_decl mmandecls "msync"))
val mprotect =    printTree (resolve false mmandecls (#get_decl mmandecls "mprotect"))

val macs = (sortByAscVal o (findmacros ("^MAP_.*",IntegerConstant))) mmapmacros
val macs' = List.map (fn (n,d) => (n,evalString d)) macs
val dt = CSyntax.enum_datatype [("MMap",macs')] "MMap"
val _ = Meta.exec dt

val macs'' = (sortByAscVal o (findmacros ("^PROT_.*",IntegerConstant))) mmapmacros
val macs''' = List.map (fn (n,d) => (n,evalString d)) macs''
val dt'' = CSyntax.enum_datatype [("MMapProt",macs''')] "MMapProt"
val _ = Meta.exec dt''

val smacs'' = (sortByAscVal o (findmacros ("^MS_.*",IntegerConstant))) mmapmacros
val smacs''' = List.map (fn (n,d) => (n,evalString d)) smacs''
val sdt'' = CSyntax.enum_datatype [("MSync",smacs''')] "MSync"
val _ = Meta.exec sdt'';

structure MMapProtBits = BitSet(structure Enum = MMapProt)
structure MMapBits     = BitSet(structure Enum = MMap)
structure MSyncBits    = BitSet(structure Enum = MSync)

val scetd = #define_enum mmandecls "SC_Consts"
val _ = Meta.exec scetd

val csetd = #define_enum mmandecls "CS_Consts"
val _ = Meta.exec csetd

val pcetd = #define_enum mmandecls "PC_Consts"
val _ = Meta.exec pcetd;

val posix_consts'' = (sortByAscName o (findmacros ("^_POSIX_.*",".*"))) mmapmacros
val smacs''' = List.map (fn (n,d) => (n,evalString d)) posix_consts''
val pcnstdt'' = CSyntax.enum_datatype [("Posix",smacs''')] "Posix"
val _ = Meta.exec pcnstdt'';
val pcs = List.map (fn v => (Posix.toString v,Word.toInt (Posix.toWord v)
                                                handle Overflow => (~1))) Posix.flags

val posix2_consts = (sortByAscName o (findmacros ("^__POSIX2.*",".*"))) mmapmacros
val posix2_consts' = List.map (fn (n,v) => (CSyntax.mlvar n, evalString v)) posix2_consts
val posix2_consts'' = (sortByAscName o (findmacros ("^_POSIX2.*",".*"))) mmapmacros
val posix2_consts''' = List.map (fn (n,v) => (n,CSyntax.mlvar v)) posix2_consts''
val smacs''' = List.map (fn (n,d) => (n,evalStringInEnv posix2_consts' d)) (posix2_consts''')
val p2cnstdt'' = CSyntax.enum_datatype [("Posix2",smacs''')] "Posix2"
val _ = Meta.exec p2cnstdt'';

val p2cs = List.map (fn v => (Posix2.toString v,Word.toInt (Posix2.toWord v))) Posix2.flags;

val dlxh = Dynlib.dlopen {lib = "", flag = Dynlib.RTLD_LAZY, global = false}

(*
#define Push_roots(name, size)						      \
   value name [(size) + 2];						      \
   { long _; for (_ = 0; _ < (size); name [_++] = Val_long (0)); }	      \
   name [(size)] = (value) (size);					      \
   name [(size) + 1] = (value) c_roots_head;				      \
   c_roots_head = &(name [(size)]);

#define Pop_roots() {c_roots_head = (value* ) c_roots_head [1]; }
*)

val c_roots_headp =  Jit.Pointer (Ffi.svec_getcptrvalue (Dynlib.cptr (Dynlib.dlsym dlxh "c_roots_head")))
val sysconfp =       Jit.Pointer (Ffi.svec_getcptrvalue (Dynlib.cptr (Dynlib.dlsym dlxh "sysconf")))
val pathconfp =      Jit.Pointer (Ffi.svec_getcptrvalue (Dynlib.cptr (Dynlib.dlsym dlxh "pathconf")))
val confstrp =       Jit.Pointer (Ffi.svec_getcptrvalue (Dynlib.cptr (Dynlib.dlsym dlxh "confstr")))
val string_lengthp = Jit.Pointer (Ffi.svec_getcptrvalue (Dynlib.cptr (Dynlib.dlsym dlxh "string_length")))

val sysconfdecl =  printTree (resolve false mmandecls (#get_decl mmandecls "sysconf"))
val confstrdecl =  printTree (resolve false mmandecls (#get_decl mmandecls "confstr"))
val pathconfdecl = printTree (resolve false mmandecls (#get_decl mmandecls "pathconf"))

val _ = Jit.jit_set_memory_functions Ffi.my_alloc Ffi.my_realloc Ffi.my_free

val () = Jit.init_jit Jit.argv0

local open Jit
   val jit_ = Jit.jit_new_state ()
   val () = jit_prolog (jit_)
   val v = jit_arg (jit_)
   val () = jit_getarg (jit_, V0, v)         (* long int sysconf (struct {int __name;} *v) *)
   val _ = jit_rshi (jit_, V1, V0, 1)        (* V1 := Long_val(v) *)
   val _ = jit_prepare (jit_)
   val _ = jit_pushargr (jit_, V1)
   val _ = jit_finishi (jit_, sysconfp)
   val _ = jit_retval (jit_, R0)
   val _ = jit_lshi (jit_, R0, R0, 1)
   val _ = jit_addi (jit_, R0, R0, 1)        (* R0 := Val_long(R0) *)
   val _ = jit_retr (jit_, R0)
   val sysconfcallptr = jit_emit (jit_)
in
   val sysconf : word -> int = Ffi.app1 sysconfcallptr
end

val sysconf_settings =
       List.map
          (fn v => (SC_Consts.toString v,
                    sysconf(SC_Consts.toWord v)))

fun findconfnames toString names =
   fn pat =>
    let val regex = Regex.regcomp pat [Regex.Extended]
    in List.filter (fn w => Regex.regexecBool regex [] (toString w)) names
    end

val findsysconfnames = findconfnames SC_Consts.toString SC_Consts.flags

fun findconfvals pat =
   let val regex = Regex.regcomp pat [Regex.Extended]
   in List.filter (fn (n,d) => Regex.regexecBool regex [] n)
   end

val stuff' = sysconf_settings
               (findsysconfnames "SC_NPROCESSORS|SC_LEVEL[12]_.?CACHE|SC_(AV)?PHYS_PAGES");

local open Jit
   val wsz = Jit.WORDSIZE div 8
   val jit_ = Jit.jit_new_state () (* SML      : Int.int * Word8Array.array -> Word.word               *)
   val () = jit_prolog (jit_)      (* Moscow ML: Int.int * Word8Vector.vector ref -> Word.word         *)
                                   (* CAMLrt   : value confstr (value v[2])                            *)
   val v = jit_arg (jit_)          (* CAMLrt C : unsigned long int confstr (struct {int __name;
                                                                             (char ** ) __bufp;} *vp)  *)
   val () = jit_getarg (jit_, V0, v)          (* V0 = v             =  v                               *)
   val _ = jit_ldxi (jit_, V1, V0, wsz * 0)   (* V1 = Field(v,0)    =  v[0] = vp->__name               *)
   val _ = jit_rshi (jit_, V1, V1, 1)         (* V1 = Long_val(V1)  = __name                           *)
   val _ = jit_ldxi (jit_, V2, V0, wsz * 1)   (* V2 = Field(v,1)    =  v[1] = vp->__bufp               *)
   val _ = jit_ldxi (jit_, R1, V2, 0)         (* R1 = Field(V2,0)   = *(vp->__bufp) = __buf            *)
   val _ = jit_prepare (jit_)                 (* ------------ call string_length --------------------- *)
   val _ = jit_pushargr (jit_, R1)            (* __buf                                                 *)
   val _ = jit_finishi (jit_, string_lengthp) (* C: unsigned long int ( *string_lengthp) (char *__buf) *)
   val _ = jit_retval (jit_, R0)              (* R0 = string_length ( __buf ) = __len                  *)
   val _ = jit_prepare (jit_)                 (* ------------ call confstr --------------------------- *)
   val _ = jit_pushargr (jit_, V1)            (* __name                                                *)
   val _ = jit_pushargr (jit_, R1)            (* __buf                                                 *)
   val _ = jit_pushargr (jit_, R0)            (* __len                                                 *)
   val _ = jit_finishi (jit_, confstrp)       (* C: unsigned long int ( *confstrp) (int __name,
                                                                                    char *__buf,
                                                                      unsigned long int __len)         *)
                                              (* Posix: size_t confstr (int __name,
                                                                        char *__buf, size_t __len)     *)
   val _ = jit_retval (jit_, R0)              (* R0 = confstr (__name, __buf, __len) = __rlen          *)
   val _ = jit_lshi (jit_, R0, R0, 1)
   val _ = jit_addi (jit_, R0, R0, 1)         (* R0 = Val_long(__rlen) = __res : value                 *)
   val _ = jit_retr (jit_, R0)                (* return __res                                          *)
   val confstrcallptr = jit_emit (jit_)
in
   val confstr : int * Word8Array.array -> word = Ffi.app1 confstrcallptr
end

fun stringToWord8Array s =
      Word8Array.tabulate
        (String.size s,
         fn n => Word8.fromInt
                   (Char.ord
                      (String.sub(s,n)))) 

fun word8ArrayToString a =
      CharVector.tabulate
        (Word8Array.length a,
         fn n => Char.chr
                   (Word8.toInt
                      (Word8Array.sub(a,n)))) 

val confstr = fn c =>
   let val i = Word.toInt (CS_Consts.toWord c)
       val n = confstr (i,Word8Array.array (0,0w0));
       val a = Word8Array.array (Word.toInt n,0w0)
       val n' = confstr(i,a)
   in  if n' = n
          then
            String.extract (word8ArrayToString a,0,SOME (Word.toInt (n-0w1)))
          else
            raise Fail ("Unistd.confstr: Internal error loading "^
                         (CS_Consts.toString c)^".")
   end

val findconfstrnames = findconfnames CS_Consts.toString CS_Consts.flags

val confstr_settings =
       List.map
            (fn v => (CS_Consts.toString v, confstr v))

val info = confstr_settings (findconfstrnames "CS_GNU|CS_PATH");

local open Jit
   val wsz = Jit.WORDSIZE div 8
   val jit_ = Jit.jit_new_state ()
   val () = jit_prolog (jit_)
   val v = jit_arg (jit_)
   val () = jit_getarg (jit_, V0, v)        (* long int pathconf (struct {char **s; long int i;} *v) *)
   val _ = jit_ldxi (jit_, R0, V0, wsz * 0) (* R0 = Field(v,0)   *)
   val _ = jit_ldxi (jit_, V1, R0, 0)       (* V1 = Field(R0,0)  *)
   val _ = jit_ldxi (jit_, V2, V0, wsz * 1) (* V2 = Field(v,1)   *)
   val _ = jit_rshi (jit_, V2, V2, 1)       (* V2 = Long_val(V2) *)
   val _ = jit_prepare (jit_)
   val _ = jit_pushargr (jit_, V1)
   val _ = jit_pushargr (jit_, V2)
   val _ = jit_finishi (jit_, pathconfp)
   val _ = jit_retval (jit_, R0)
   val _ = jit_lshi (jit_, R0, R0, 1)
   val _ = jit_addi (jit_, R0, R0, 1)        (* R0 := Val_long(R0) *)
   val _ = jit_retr (jit_, R0)
   val pathconfcallptr = jit_emit (jit_)
in
   val pathconf : Word8Array.array * word -> int
          = Ffi.app1 pathconfcallptr
end

val pathconf =
  fn s => fn c =>
    let val w = PC_Consts.toWord c
        val a = stringToWord8Array s
    in pathconf (a,w)
    end
(*
val guiledecls = parse "libguile.h" "$(pkg-config guile-2.0 --cflags-only-I)" "#include <libguile.h>\n"
val simple =     List.filter (fn (_,(_,(NONE,_))) => true | _ => false)
val guilemacros = List.map (fn (n,(_,(a,d))) => (n,d)) ( ((#macros guiledecls)()))
val scmmacs = findmacros ("^SCM_(UNDEFINED|TRUE|FALSE|EOL|MAKIFLAG_BITS|MAKE_ITAG8_BITS)$",".*") guilemacros
val scmmacs' = findmacros ("tc8",".*") guilemacros
*)

fun findpmacros (pat,pat') =
    let val regex = Regex.regcomp pat [Regex.Extended]
        val regex' = Regex.regcomp pat' [Regex.Extended]
    in List.filter (fn (n,_,d) => Regex.regexecBool regex [] n andalso
                                 Regex.regexecBool regex' [] d)
    end

val ltdecls = parse "lightning.h" "$(pkg-config lightning --cflags-only-I)" "#include <lightning.h>\n"
val simple =     List.filter (fn (_,(_,(NONE,_))) => true | _ => false)
val ltmacros = List.map (fn (n,(_,(SOME a,d))) => (n,a,d)
                          | (n,(_,(NONE,d))) => (n,[],d)) (#macros ltdecls ())
val nnmacs = findpmacros ("^jit_new_node_",".*") ltmacros
val nncmacs = findpmacros ("^jit_","^jit_new_node_[a-z]+ *(.+)$") ltmacros
val notnncmacs = findpmacros ("^jit_","^jit_[^n]") ltmacros
fun matches pat = 
      let val regex =  Regex.regcomp pat [Regex.Extended]
      in fn n => Regex.regexecBool regex [] n
      end

val codehash = #define_enum_hash ltdecls "jit_code_t"
val _ = print codehash;

val exclmacs = matches "^(jit_set|jit_get_|jit_name|jit_link|jit_callee|jit_data|jit_pointer|jit_new_node_)"
val othermacs = List.filter (fn (n,_,_) => not (exclmacs n)) (findpmacros ("^jit_","^_jit") ltmacros)

val ltshowdecl = fn n => printTree (#get_decl ltdecls n)

val delim = Regex.regcomp " *, *" []
fun params regex s = List.rev(Regex.fold regex (op ::, #2) [] s)
fun split s = List.map Substring.string (params delim s)

val mkr = fn ([_,n,ps,_]) => (n,split ps) | _ => ("",[])

val defopt = fn d => fn mo => Option.getOpt (mo,d) 

val macdef = Regex.regcomp "^([a-zA-Z_0-9]+)\\(([^)]+)\\)$" [Regex.Extended]
val strlist = (List.map Substring.string) o (Vector.foldr op :: []) o Option.valOf 
val res = mkr o strlist o ((Regex.regnexec macdef []) o Substring.full)

val dblq = List.foldr (fn (s,l) => if s = "q" then ("w"::"w"::l) else s::l) []
val typed = Regex.regcomp "_([qpwdf]+)$" [Regex.Extended]
val splitt = dblq o 
             (List.map String.str) o 
             List.rev o 
             String.explode o 
             (fn [_,s] => s | _ => raise Bind) o 
             strlist o
             (Regex.regnexec typed []) o
             Substring.full

val ascnm = (fn ((a,_,_),(b,_,_)) => String.compare (a,b))
val sortm = Listsort.sort ascnm

val nncmacs' = sortm (List.map (fn (n,ps,d) => (n,ps,res d)) nncmacs)
val nnmacs' = sortm (List.map (fn (n,ps,d) => (n,ps,res d)) nnmacs)
val notnncmacs' = sortm (List.map (fn (n,ps,d) => (n,ps,res d)) notnncmacs)
val aliases = List.map (fn (n,_,(n',_)) =>(n',n)) notnncmacs'
val othermacs' = sortm (List.map (fn (n,ps,d) => (n,"_jit"::((if ps = [""] then [] else ps)),res d)) othermacs)

val selectms = fn r => 
     let val cr = Regex.regexecBool (Regex.regcomp r [Regex.Extended]) []
     in List.filter (fn (s,_,_) => cr s)
     end;

val tmap = [("q","quad"),("p","pointer"),("f","float"),("d","double"),("w","word")];
val lookup = fn s => (#2) o (defopt ("",s^"?") o (List.find (fn (c,t) => c = s)))
val tparams = ListPair.foldr (fn (p,t,l) => if p = "NULL" then l else (p,lookup t tmap)::l) [] 

val types = fn (n,fps,(cn,cps)) => 
               (n,List.length fps + 1,
                  [("_jit","state")]@(List.rev (tparams (List.rev cps,splitt cn 
                            handle Option => raise Fail ("option: "^cn)))),
               (cn,"_jit"::cps))
val ltrest = List.map types (selectms "jit_" nncmacs')

val types' = fn (n,fps,(cn,cps)) => 
                (n,List.length fps + 1,
                   [("_jit","state"),("c","code")] @ (List.rev (tparams (List.rev cps,splitt cn 
                             handle Option => raise Fail ("option: "^cn)))),
                (cn,cps))
val ltrest' = List.map types' (selectms "jit_" nnmacs')

val typemap = [("f","fpr"),("d","fpr"),("i","gpr"),("u","gpr"),
               ("l","gpr"),("c","gpr"),("s","gpr"),("us","gpr"),
               ("uc","gpr"),("ul","gpr")]

val itypemap = [("f","float"),("d","double"),("i","int"),("u","unsigned"),
                ("l","long"),("c","char"),("s","short"),("us","unsigned short"),
                ("uc","unsigned char"),("ul","unsigned long")]

val lookupt = fn s => ((#2) o (defopt ("","word") o (List.find (fn (c,t) => c = s)))) typemap
val lookupit = fn s => ((#2) o (defopt ("","int") o (List.find (fn (c,t) => c = s)))) itypemap

val modargs = [
   ("jit_(un)?(lt|gt|le|ge|eq|ne|ltgt|ord)(r|i)_(f|d|u)",
            fn [_,_,insn,ri,tc] => [(1,"gpr"),(2,lookupt tc)]@(if ri="r" then [(3,lookupt tc)] else [])
             | _ => []),
   ("jit_b(un)?(lt|gt|le|ge|eq|ne|ltgt|ord)(r|i)_(f|d|u)",
            fn [_,_,insn,ri,tc] => [(1,lookupt tc)]@(if ri="r" then [(2,lookupt tc)] else [])
             | _ => []),
   ("jit_b(lt|gt|le|ge|eq|ne)(r|i)$",
            fn [_,insn,ri] => [(1,"gpr")]@(if ri="r" then [(2,"gpr")] else [])
             | _ => []),
   ("jit_(lt|gt|le|ge|eq|ne)(r|i)$",
            fn [_,insn,ri] => [(1,"gpr"),(2,"gpr")]@(if ri="r" then [(3,"gpr")] else [])
             | _ => []),
   ("jit_(add|sub|mul|div|rem|rsh)(r|i)_(us|uc|ul|f|d|s|c|i|l|u)",
            fn [_,insn,ri,tc] =>
               let val res = if ri="i"
                                then []
                                else [(3,lookupt tc)]
               in (1,lookupt tc)::(2,lookupt tc)::res
               end
             | _ => []),
   ("jit_(abs|sqrt|neg)r_(f|d)",
            fn [_,_,tc] => [(1,"fpr"),(2,"fpr")]
             | _ => []),
   ("jit_mov(r|i)_(f|d|w|d_w|f_w|w_d|w_f|d_ww|ww_d)$",
            fn [_,ri,tc] => 
               (case tc of "d" => (1,"fpr")::(if ri="r" then [(2,"fpr")] else [])
                         | "f" =>  (1,"fpr")::(if ri="r" then [(2,"fpr")] else [])
                         | "d_w" =>  (1,"gpr")::(if ri="r" then [(2,"fpr")] else [])
                         | "d_ww" =>  (1,"gpr")::(2,"gpr")::(if ri="r" then [(3,"fpr")] else [])
                         | "f_w" =>  (1,"gpr")::(if ri="r" then [(2,"fpr")] else [])
                         | "w_d" =>  (1,"fpr")::(if ri="r" then [(2,"gpr")] else [])
                         | "w_f" =>  (1,"fpr")::(if ri="r" then [(2,"gpr")] else [])
                         | "ww_d" =>  (1,"fpr")::(if ri="r" then [(2,"gpr"),(3,"gpr")] else [])
                         | _ => [])
             | _ => []),
   ("jit_b(un)?ord(i|r)_(f|d)",
            fn [_,_,ir,_] => (1,"fpr")::(if ir = "r" then [(2,"fpr")] else [])
             | _ => []),
   ("jit_extr_(f|d|c|s|uc|us)",
            fn [_,t] => (2,"fpr")::[(1,lookupt t)]
             | _ => []),
   ("jit_(add|sub)(x|c)(r|i)",
            fn [_,insn,xc,ri] =>
               let val res = if ri="i"
                                then []
                                else [(3,"gpr")]
               in (1,"gpr")::(2,"gpr")::res
               end
             | l => List.map (fn s => (0,s)) l),
   ("jit_bm(s|c)(r|i)",
            fn [_,sc,ri] =>
               let val res = if ri="i"
                                then []
                                else [(2,"gpr")]
               in (1,"gpr")::res
               end
             | l => List.map (fn s => (0,s)) l),
   ("jit_call(r|i)",
            fn [_,ri] =>
               let val res = if ri="i"
                                then []
                                else [(1,"gpr")]
               in res
               end
             | l => List.map (fn s => (0,s)) l),
   ("jit_b[ox](add|sub)(r|i)(_u)?",
            fn [_,insn,ri,u] =>
               let val res = if ri="i"
                                then []
                                else [(2,"gpr")]
               in (1,"gpr")::res
               end
             | l => List.map (fn s => (0,s)) l),
   ("jit_q(div|mul)(r|i)(_u)?",
            fn [_,insn,ri,u] =>
               let val res = if ri="i"
                                then []
                                else [(4,"gpr")]
               in (1,"gpr")::(2,"gpr")::(3,"gpr")::res
               end
             | l => List.map (fn s => (0,s)) l),
   ("jit_(add|sub|mul|div|rem|lsh|and|rsh|or|xor)(r|i)$",
            fn [_,insn,ri] =>
               let val res = if ri="i"
                                then []
                                else [(3,"gpr")]
               in (1,"gpr")::(2,"gpr")::res
               end
             | l => List.map (fn s => (0,s)) l),
   ("jit_(comr|htonr|ntohr|jmpr|live|negr|movr|movi|truncr_d_i|truncr_f_i)$",
            fn [_,insn] =>
               (case insn of "comr" => [(1,"gpr"),(2,"gpr")]
                           | "htonr" => [(1,"gpr"),(2,"gpr")]
                           | "ntohr" => [(1,"gpr"),(2,"gpr")]
                           | "movr" => [(1,"gpr"),(2,"gpr")]
                           | "jmpr" => [(1,"gpr")]
                           | "live" => [(1,"gpr")]
                           | "movi" => [(1,"gpr")]
                           | "negr" => [(1,"gpr"),(2,"gpr")]
                           | "truncr_d_i" => [(1,"gpr"),(2,"fpr")]
                           | "truncr_f_i" => [(1,"gpr"),(2,"fpr")]
                           | _ => [])
             | l => List.map (fn s => (0,s)) l),
   ("jit_(ld|st)(x?)(r|i)_(f|d|i|u?c|u?s)",
            fn [_,insn,"x",ri,tc] =>
               let val (srcp,dstp) = if insn="st" then (3,1) else (1,3)
               in [(srcp,lookupt tc),(2,"gpr"),(dstp,if ri="i" then "pointer" else "gpr")]
               end
             | [_,insn,"",ri,tc] =>
               let val (srcp,dstp) = if insn="st" then (2,1) else (1,2)
               in [(srcp,lookupt tc),(dstp,if ri="i" then "pointer" else "gpr")]
               end
             | l => List.map (fn s => (0,s)) l)
];

val df = [
   ("jit_(un)?(lt|gt|le|ge|eq|ne|ltgt|ord)(r|i)_(f|d|u)",
            fn [_,_,insn,ri,tc] => ([1],if ri="r" then [2,3] else [2])
             | _ => raise Fail "df: regexec match: no case."),
   ("jit_b(un)?(lt|gt|le|ge|eq|ne|ltgt|ord)(r|i)_(f|d|u)",
            fn [_,_,insn,ri,tc] => ([],if ri="r" then [1,2] else [1])
             | _ => raise Fail "df: regexec match: no case."),
   ("jit_b(lt|gt|le|ge|eq|ne)(r|i)$",
            fn [_,insn,ri] => ([],if ri="r" then [1,2] else [1])
             | _ => raise Fail "df: regexec match: no case."),
   ("jit_(lt|gt|le|ge|eq|ne)(r|i)$",
            fn [_,insn,ri] => ([1],if ri="r" then [2,3] else [2]) 
             | _ => raise Fail "df: regexec match: no case."),
   ("jit_(add|sub|mul|div|rem|rsh)(r|i)_(us|uc|ul|f|d|s|c|i|l|u)",
            fn [_,insn,ri,tc] => ([1],if ri="r" then [2,3] else [2]) 
             | _ => raise Fail "df: regexec match: no case."),
   ("jit_(abs|sqrt|neg)r_(f|d)",
            fn [_,_,tc] => ([1],[2])
             | _ => raise Fail "df: regexec match: no case."),
   ("jit_mov(r|i)_(f|d|w|d_w|f_w|w_d|w_f|d_ww|ww_d)$",
            fn [_,ri,tc] => 
               (case tc of "d" => ([1],if ri="r" then [2] else [])
                         | "f" => ([1],if ri="r" then [2] else [])
                         | "d_w" => ([1],if ri="r" then [2] else [])
                         | "d_ww" => ([1,2],if ri="r" then [3] else [])
                         | "f_w" => ([1],if ri="r" then [2] else [])
                         | "w_d" => ([1],if ri="r" then [2] else [])
                         | "w_f" => ([1],if ri="r" then [2] else [])
                         | "ww_d" => ([1],if ri="r" then [2,3] else [])
                         | t => raise Fail ("df: regexec match: type "^t^": no case."))
             | _ => raise Fail "df: regexec match: no case."),
   ("jit_b(un)?ord(i|r)_(f|d)",
            fn [_,_,ri,_] => ([1],if ri="r" then [2] else [])
             | _ => raise Fail "df: regexec match: no case."),
   ("jit_extr_(f|d|c|s|uc|us)",
            fn [_,t] => ([1],[2])
             | _ => raise Fail "df: regexec match: no case."),
   ("jit_(add|sub)(x|c)(r|i)",
            fn [_,insn,xc,ri] => ([1],if ri="r" then [2,3] else [2])
             | _ => raise Fail "df: regexec match: no case."),
   ("jit_bm(s|c)(r|i)",
            fn [_,sc,ri] => ([1],if ri="r" then [2] else [])
             | _ => raise Fail "df: regexec match: no case."),
   ("jit_call(r|i)",
            fn [_,ri] => ([],if ri="r" then [1] else [])
             | _ => raise Fail "df: regexec match: no case."),
   ("jit_b[ox](add|sub)(r|i)(_u)?",
            fn [_,insn,ri,u] => ([1],if ri="r" then [2] else [])
             | _ => raise Fail "df: regexec match: no case."),
   ("jit_q(div|mul)(r|i)(_u)?",
            fn [_,insn,ri,u] => ([1,2],if ri="r" then [3,4] else [3])
             | _ => raise Fail "df: regexec match: no case."),
   ("jit_(add|sub|mul|div|rem|lsh|and|rsh|or|xor)(r|i)$",
            fn [_,insn,ri] => ([1],if ri="r" then [2,3] else [2])
             | _ => raise Fail "df: regexec match: no case."),
   ("jit_(comr|htonr|ntohr|jmpr|live|negr|movr|movi|truncr_d_i|truncr_f_i)$",
            fn [_,insn] =>
               (case insn of "comr" => ([1],[2])
                           | "htonr" => ([1],[2])
                           | "ntohr" => ([1],[2])
                           | "movr" => ([1],[2])
                           | "jmpr" => ([],[1])
                           | "live" => ([],[1])
                           | "movi" => ([1],[])
                           | "negr" => ([1],[2])
                           | "truncr_d_i" => ([1],[2])
                           | "truncr_f_i" => ([1],[2])
                           | t => raise Fail ("df: regexec match: type: "^t^"no case."))
             | _ => raise Fail "df: regexec match: no case."),
   ("jit_(ld|st)(x?)(r|i)_(f|d|i|u?c|u?s)",
            fn [_,insn,"x",ri,tc] => if insn="st" then ([],if ri="i" then [1,2,3] else [1,2])
                                                  else ([1],if ri="i" then [2,3] else [2])
             | [_,insn,"",ri,tc] => if insn="st" then ([],if ri="i" then [1,2] else [1])
                                                  else ([1],if ri="i" then [2,3] else [2])
             | _ => raise Fail "df: regexec match: no case.")
];

val rematchlst = (List.map Substring.string) o (Vector.foldr op :: []) o (defopt #[])
val compm = fn m => Regex.regcomp m [Regex.Extended]
val modargs' = List.map (fn (re,modfn) => (compm re,modfn)) modargs
val df' = List.map (fn (re,rdf) => (compm re,rdf)) df
val mods = fn s =>
             List.foldl 
                (fn ((re,modfn),a) =>
                     List.@ (modfn ((rematchlst o (Regex.regnexec re []) o Substring.full) s),a))
             [] modargs'

val dfs = fn l => fn s => 
             List.foldl 
                (fn ((re,modfn),a) =>
                     (List.:: (modfn ((rematchlst o (Regex.regnexec re []) o Substring.full) s),a)
                             handle Fail _ => a))
             [] l;

val othermodargs = [
   ("jit_retval_(f|d|i|c|s|uc|us)",
            fn [_,tc] => [(0,"state"),(1,lookupt tc)]
             | _ => []),
   ("jit_getarg_(f|d|i|c|s|uc|us)$",
            fn [_,tc] => [(0,"state"),(1,lookupt tc),(2,"noderef")]
             | _ => []),
   ("jit_patch$",
            fn [_] => [(0,"state"),(1,"noderef")]
             | _ => []),
   ("jit_patch_abs$",
            fn [_] => [(0,"state"),(1,"noderef"),(2,"pointer")]
             | _ => []),
   ("jit_patch_at$",
            fn [_] => [(0,"state"),(1,"noderef"),(2,"noderef")]
             | _ => []),
   ("jit_address$",
            fn [_] => [(0,"state"),(1,"noderef")]
             | _ => []),
   ("jit_ret(r|i)_(f|d)",
            fn [_,ri,tc] => [(0,"state"),(1,if ri = "r" then lookupt tc else lookupit tc)]
             | _ => []),
   ("jit_ret(r|i)$",
            fn [_,ri] => [(0,"state"),(1,if ri = "r" then "gpr" else "word")]
             | _ => []),
   ("jit_pusharg(r|i)_(f|d)",
            fn [_,ri,tc] => [(0,"state"),(1,if ri = "r" then lookupt tc else lookupit tc)]
             | _ => []),
   ("jit_pusharg(r|i)$",
            fn [_,ri] => [(0,"state"),(1,if ri = "r" then "gpr" else "word")]
             | _ => []),
   ("jit_allocai$",
            fn [_] => [(0,"state"),(1, "word")]
             | _ => []),
   ("jit_new_node$",
            fn [_] => [(0,"state"),(1, "code")]
             | _ => []),
   ("jit_note$",
            fn [_] => [(0,"state"),(1, "pointer"),(2, "word")]
             | _ => []),
   ("jit_finish(r|i)$",
            fn [_,ri] => [(0,"state"),(1,if ri = "r" then "gpr" else "pointer")]
             | _ => []),
   ("jit_(print|prolog|prepare|realize|ret|epilog|emit|forward|clear_state \
             \ |destroy_state|disassemble|ellipsis|arg|arg_d|arg_f|indirect|label)$",
            fn [_,cmd] => [(0,"state")]
             | _ => [])]

val otherdf = [
   ("jit_retval_(f|d|i|c|s|uc|us)",
            fn [_,_] => ([1],[])
             | _ => raise Fail "otherdf: regexec match: no case."),
   ("jit_getarg_(f|d|i|c|s|uc|us)$",
            fn [_,_] => ([1],[])
             | _ => raise Fail "otherdf: regexec match: no case."),
   ("jit_ret(r|i)(_(f|d))?",
            fn (_::ri::_) => ([],if ri = "r" then [1] else [])
             | _ => raise Fail "otherdf: regexec match: no case."),
   ("jit_pusharg(r|i)(_(f|d))?",
            fn (_::ri::_) => ([],if ri = "r" then [1] else [])
             | _ => raise Fail "otherdf: regexec match: no case."),
   ("jit_finish(r|i)$",
            fn [_,ri] => ([],if ri = "r" then [1] else [])
             | _ => raise Fail "otherdf: regexec match: no case.")]

val othermodargs' = List.map (fn (re,modfn) => (compm re,modfn)) othermodargs

val othermods = fn s => 
             List.foldl 
                (fn ((re,modfn),a) =>
                     List.@ (modfn ((rematchlst o (Regex.regnexec re []) o Substring.full) s),a))
             [] othermodargs'

val lookupa = fn (n,t) => ((#2) o (defopt (0,t) o (List.find (fn (n',_) => n = n'))))
val lookupoa = fn (n) => ((#2) o (defopt (0,"") o (List.find (fn (n',t) => n = n'))))

val do_modargs = 
     fn amods =>
        List.rev o #2 o (List.foldl (fn ((s,t),(i,args)) => (i+1,(s,lookupa (i,t) amods)::args)) (0,[]))

val do_modotherargs = 
     fn amods =>
        List.rev o #2 o (List.foldl (fn (s,(i,args)) => (i+1,(s,lookupoa (i) amods)::args)) (0,[]))

val otherdf' = List.map (fn (re,rdf) => (compm re,rdf)) otherdf

val thedfs = List.map (fn (name,_,_,_) => (name,dfs df' name)) ltrest;
val theotherdfs = List.filter (fn (_,[]) => false | (_,[([],[])]) => false | _ => true) (List.map (fn (name,_,_) => (name,dfs otherdf' name)) othermacs');
val dataflow = thedfs @ theotherdfs

val modded = List.map (fn (name,nargs,args,dst as (dstnm,dstargs)) =>
                          ("noderef",name, do_modargs (mods name) args,dst))
                       ltrest;

val modded' = List.map (fn (name,nargs,args,dst as (dstnm,dstargs)) =>
                           ("noderef",name, args,dst))
                        ltrest';

val retvals = [
   ("jit_address","pointer"),
   ("jit_emit","pointer"),
   ("jit_new_state","state"),
   ("jit_indirect","noderef"),
   ("jit_finishi","noderef"),
   ("jit_arg(_[df])?","noderef"),
   ("jit_finishi","noderef"),
   ("jit_new_node$","noderef"),
   ("jit_note","noderef"),
   ("jit_allocai","int")
]

val retvals' = List.map (fn (re,rv) => (compm re,rv)) retvals

val findretvals = fn s => 
             (case List.find
                     (fn (re,rv) => Regex.regexecBool re [] s)
                     retvals'
                of SOME (_,t) => t
                 | _ => "void")

val othermodded = List.map (fn (name,args,dst as (dstnm,dstargs)) =>
                               (findretvals name,
                                name,
                                do_modotherargs (othermods name) args,dst))
                           othermacs';

val lookupalias = fn s => ((#2) o (defopt ("","") o (List.find (fn (c,t) => c = s)))) aliases

fun printargs args = 
   (List.foldl (fn (s,a) => (if a = "" then s else a^", "^s)) "" (List.map (fn (a,t) => t^" "^a) args))

fun printdargs args = 
   (List.foldl (fn (s,a) => (if a = "" then s else a^", "^s)) "" args)

fun typemap m = fn s => ((#2) o (defopt (s,s) o (List.find (fn (c,t) => c = s)))) m

val maptypes = [("state","lgt_state_t"),("code","lgt_code_t"),("gpr","lgt_gpr_t"),
                ("fpr","lgt_fpr_t"),("pointer","lgt_ptr_t"),("word","lgt_word_t"),
                ("void","void"),("noderef","lgt_noderef_t")];

fun replacejit s = fn t => Regex.replace (Regex.regcomp "^jit_" [Regex.Extended]) [Regex.Str t] s

fun chgjit s = fn t => Regex.replace (Regex.regcomp "^_jit$" [Regex.Extended]) [Regex.Str t] s

val printdef = List.app (fn (rv,n,args,(dstnm,dstargs)) => 
          let val extra = lookupalias n
              val args = List.map (fn (a,t) => (chgjit a "st",(typemap maptypes t))) args
              val dstargs = List.map (fn a => chgjit a "st") dstargs
              fun printit n = print ((typemap maptypes rv)^" "^(replacejit n "lgt_")^"("^(printargs args)^") {\n   "^(if rv <> "void" then "return " else "")^(replacejit dstnm "lgt_")^"("^(printdargs dstargs)^");\n}\n\n")
          in printit n; if extra <> "" then printit extra else ()
          end)

val _ = printdef othermodded;
val _ = printdef modded';
val _ = printdef modded;

val printdef' = List.app (fn (rv,n,args,(dstnm,dstargs)) => 
          let val extra = lookupalias n
              fun printit n = print ((rv)^" "^(n)^"("^(printargs args)^") = "^(dstnm)^"("^(printdargs dstargs)^")\n")
          in printit n; if extra <> "" then printit extra else ()
          end)

val _ = printLength := Option.valOf Int.maxInt;
val _ = (print "val infra = ";printVal othermodded;print "\n");
val _ = (print "val mknodes = ";printVal modded';print "\n");
val _ = (print "val insns = ";printVal modded;print "\n");
val _ = (print "val dataflow = ";printVal dataflow;print "\n");


