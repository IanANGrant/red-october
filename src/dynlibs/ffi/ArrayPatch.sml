functor ArrayPatch
   (structure ArrayStruct : GenericArray
    structure ArraySliceStruct : GenericArraySlice
           where type array = ArrayStruct.array
             and type elem = ArrayStruct.elem
    val zero : ArrayStruct.elem)
      :> ArrayPatch
           where type elem = ArrayStruct.elem
             and type array = ArrayStruct.array
             and type slice = ArraySliceStruct.slice
             and type vector = ArrayStruct.vector =
struct
   type elem = ArrayStruct.elem
   type array = ArrayStruct.array
   type slice = ArraySliceStruct.slice
   type vector = ArrayStruct.vector

   datatype patch_ =
        Slice of slice
      | Branch of patch_ list
      | Patch of patch
      | Tab of int * (int -> elem)
   withtype patch = patch_ ref

   local

      fun mkPatch l : patch = ref (Branch l) 

      fun fromArray a =
             mkPatch [Slice (ArraySliceStruct.full a)]

      fun fromSlice s =
             mkPatch [Slice s]

      fun fromList l =
            mkPatch
              (List.foldr 
                 (fn (s,a) => (Slice s)::a)
                 [] l)

      fun concat l =
            mkPatch
              (List.foldr
                 (fn (ref (Branch l),a) => (List.@(l,a))
                   | (ref x,a) => x::a)
                 [] l)

      fun foldl_ acyclic limit f acc (p as (ref p_)) =
         let fun iter (arg as (Slice s),((v,n),r)) =
                         f (arg,((v,n+(ArraySliceStruct.length s)),r))
               | iter (arg as (Tab (m,_)),((v,n),r)) =
                         f (arg,((v,m+n),r))
               | iter (Branch [],vnr) = vnr
               | iter (Branch (p_::ps),acc as ((v,n),r)) =
                    let val ((_,n),r) = iter (p_,acc)
                    in iter (Branch ps,((v,n),r))
                    end
               | iter (arg as (Patch (p as (ref p_)),vnr as ((v,n),r))) =
                    if acyclic andalso List.exists
                         (fn p' => p' = p) v
                    then f arg
                    else if n < limit
                            then iter (p_,((p::v,n),r))
                            else vnr
         in iter (p_,(([p],0),acc))
         end

  local
      exception Done of elem
      exception DoneUnit
      fun loop f limit =
         let fun iter (arg as (Slice s),((b,a,n),r)) =
                      let val m = ArraySliceStruct.length s
                          val n' = n + m
                      in if n = limit 
                            then f ((b,arg::a,n'),r)
                            else if n' > limit
                                    then let val subslice = ArraySliceStruct.subslice
                                             val brk = limit - n
                                             val part1 = Slice (subslice(s,0,SOME(brk)))
                                             val part2 = Slice (subslice(s,brk,NONE))
                                         in f ((part1::b,part2::a,n'),r)
                                         end
                                    else ((arg::b,a,n'),r)
                      end
               | iter (arg as (Tab (m,f')),((b,a,n),r)) =
                      let val n' = n + m
                      in if n = limit 
                            then f ((b,arg::a,n'),r)
                            else if n' > limit
                                    then let val brk = limit - n

                                             val part1 = Tab (brk,f')
                                             val part2 = Tab (m-brk,fn i => f'(i+brk))
                                         in f ((part1::b,part2::a,n'),r)
                                         end
                                    else ((arg::b,a,n'),r)
                      end
               | iter (Branch [],banr) = banr
               | iter (Branch (p_::ps),
                       banr as ((b,a,n),r)) =
                    let val ((b,a,n),r) = iter (p_,banr)
                    in if n <= limit
                          then iter (Branch ps,((b,a,n),r))
                          else ((b,List.revAppend(ps,a),n),r)
                    end
               | iter (arg as (Patch (p as (ref p_))),
                       banr as ((b,a,n),r)) =
                    if n <= limit
                       then case iter (p_,banr)
                              of rvs as ((l::ls,r::rs,n'),r') =>
                                    if n' <= limit
                                       then rvs
                                       else (((Patch (ref l))::ls,
                                              (Patch (ref r))::rs,n'),r')
                               | rvs => rvs 
                       else ((b,arg::a,n),r)
      in fn a => fn (p as (ref p_)) => iter (p_,(([],[],0),a))
      end
   in
      fun split_ limit p =
            let val ((p1,p2,_),_) = loop (fn x => x) limit 0 p
            in (mkPatch (rev p1),mkPatch (rev p2))
            end
      fun sub_ (p,limit) =
            let fun subfn (arg as ((b,(Slice s)::a,n),_)) =
                      if true
                         then raise Done (ArraySliceStruct.sub(s,0))
                         else arg
                  | subfn (arg as ((_,(Tab (m,f))::_,_),_)) =
                      if true
                         then raise Done (f 0)
                         else arg
                  | subfn _ = raise Fail "sub_: impossible"
            in (case loop subfn limit zero p
                  of _ => if true
                             then raise Subscript
                             else zero)
                handle Done v => v
            end
      fun update_ (p,limit,e) =
            let fun updfn (arg as ((b,(Slice s)::a,n),_)) =
                      if true
                         then (ArraySliceStruct.update(s,0,e); raise DoneUnit)
                         else arg
                  | updfn (arg as ((_,(Tab (m,f))::_,_),_)) =
                      if true
                         then raise Fail "update of tabulated patch"
                         else arg
                  | updfn _ = raise Fail "update_: impossible"
            in (case loop updfn limit zero p
                  of _ => if true
                             then raise Subscript
                             else ())
                handle DoneUnit => ()
            end
   end

      fun partition_ start2 start3 p =
          let val (p1,p2') =
                  if start2 >= start3 
                     then raise Fail "partition_ backwards"
                     else if start2 = 0
                             then raise Fail "partition_ zero"
                             else split_ start2 p
              val (p2,p3) = split_ (start3 - start2) p2'
          in (p1,p2,p3)
          end

      fun walk_patches_ acyclic limit set get =
        fn f =>
         let val g = f o get
             fun foldfn x = set (x,g x)
         in foldl_ acyclic limit foldfn
         end

      fun walk_patches1_ acyclic limit = fn f => fn acc => fn p =>
             let fun set ((_,(v,_)),r) = (v,r)
                 fun get (p,(_,acc)) = (p,acc)
                 val (_,r) = walk_patches_ acyclic limit set get f acc p
             in r
             end

      val walk_patches = walk_patches_ true 0

      val cycle_patches = walk_patches_ false

      val length =
         let val walk_patches1 = walk_patches1_ true 0
             val length = ArraySliceStruct.length
             fun lenfn (Slice s,r)   = r + length s
               | lenfn (Tab (n,_),r) = r + n
               | lenfn (_,r)         = r
         in walk_patches1 lenfn 0
         end

      fun tabulate nf = mkPatch [Tab nf]

      val slices =
         let val walk_patches1 = walk_patches1_ true 0
             val tabslc =
                    ArraySliceStruct.full
                      o ArrayStruct.tabulate
             fun slcfn (Slice s,r) = s::r
               | slcfn (Tab p,r) = (tabslc p)::r
               | slcfn (_,r) = r
         in rev o (walk_patches1 slcfn [])
         end

      fun array p =
         let val walk_patches1 = walk_patches1_ true 0
             val patchlen = length p
             val arr = ArrayStruct.array (patchlen,zero)
             fun slcfn (Slice s,r) =
                      let val (slcarr,offs,len) = ArraySliceStruct.base s
                      in ArraySliceStruct.appi
                             (fn (i,w) => (ArrayStruct.update (arr,r+i-offs,w))) s;
                         r + len
                      end
               | slcfn (Tab (n,f),r) =
                        let val slc = ArraySliceStruct.slice (arr,r,SOME n)
                        in (ArraySliceStruct.appi 
                               (fn (i,_) =>
                                   (ArraySliceStruct.update
                                      (slc,i - r,f (i - r))))
                               slc;
                            r + n)
                        end
               | slcfn (_,r) = r
            val len = walk_patches1 slcfn 0 p
         in arr
         end

      val vector =  ArrayStruct.vector o array

      fun subpatch (p, 0, NONE) = p
        | subpatch (p, n, NONE) =
            let val (_,e) = split_ n p
            in e end
        | subpatch (p, 0, SOME len) =
            let val (m,_) = if len < 0
                               then raise Fail "subpatch: length < 0"
                               else split_ len p
            in m end
        | subpatch (p, n, SOME len) =
            if len < 0
               then raise Fail "subpatch: length < 0"
               else if len = 0
                       then let val (_,m) = split_ n p
                            in m end
                       else let val (_,m,_) = partition_ n (n+len) p
                            in m end

      fun slice (p, i, lopt) = 
         let val sp = subpatch (p, i, lopt)
         in case slices sp
              of [slc] => slc
               | _ => ArraySliceStruct.full (array sp)
         end

      fun patch (p, q, i) =
            let val () = if length p < i then raise Fail "patch off end" else ()
                val lenq = length q
                val ps = if i = 0 
                            then let val (_,e) = split_ lenq p
                                 in [q,e] end
                            else let val (b,_,e) = partition_ i (i + lenq) p
                                 in [b,q,e] end
            in concat ps
            end

      fun superpatch (p, q, i) =
            let val len = length q
                val (b,_,e) = partition_ i (i + (length q) + 1) p
            in concat [b,mkPatch [Patch q],e] end

      fun appi f =
         let val walk_patches1 = walk_patches1_ true 0
             fun slcfn (Slice s,r) =
                      let val (slcarr,offs,len) = ArraySliceStruct.base s
                          val () = ArraySliceStruct.appi
                                     (fn (i,w) => (f (r+i-offs,w))) s
                      in r + len
                      end
               | slcfn (Tab (n,tf),r) =
                        let val stop = r + n
                             fun lr j = 
                                 if j < stop
                                    then (f(j, tf (j-r));
                                          lr (j+1)) 
                                    else ()
                        in lr r;r+n end
                | slcfn (_,r) = r
         in fn p => ignore (walk_patches1 slcfn 0 p)
         end

      val app = fn f => appi (fn (_,w) => f w)

      fun foldl f =
         let val walk_patches1 = walk_patches1_ true 0
             fun slcfn (Slice s,(acc,r)) =
                      let val (slcarr,offs,len) = ArraySliceStruct.base s
                          val acc' = ArraySliceStruct.foldl f acc s
                      in (acc',r + len)
                      end
               | slcfn (Tab (n,tf),(acc,r)) =
                        let val stop = r + n
                            fun lr j res = 
                                if j < stop
                                   then lr (j+1) (f (tf (j-r),res))
                                   else res
                        in (lr r acc,r+n) end
                | slcfn (_,(acc,r)) = (acc,r)
         in fn acc => (fn p => let val (acc,_) = walk_patches1 slcfn (acc,0) p in acc end)
         end

      fun foldr f =
         let val walk_patches1 = walk_patches1_ true 0
             fun slcfn (Slice s,(acc,r)) =
                      let val (slcarr,offs,len) = ArraySliceStruct.base s
                          val acc' = ArraySliceStruct.foldr f acc s
                      in (acc',r + len)
                      end
               | slcfn (Tab (n,tf),(acc,r)) =
                         let val stop = r + n
                             fun rl j res = 
                                 if j >= r
                                    then rl (j-1) (f (tf (j-r),res))
                                    else res
                        in (rl r acc,r+n-1) end
                | slcfn (_,(acc,r)) = (acc,r)
         in (* This is bad. We have to realize all the Tab patches. We need
               a "flatten" operation to return a list of sub-patches that we
               can then reverse and concat to get a reversed list to do
               foldr on. Since we need to re-write all this with indexes 
               anyway, we leave it like this for now. *)
            fn acc => (fn p => let val rp = (fromList o rev o slices) p
                                   val (acc,_) = walk_patches1 slcfn (acc,0) rp
                               in acc end)
         end
   in
      val fromArray : array -> patch
          = fromArray

      val fromSlice : slice -> patch
          = fromSlice

      val fromList : slice list -> patch
          = fromList

      val concat : patch list -> patch
          = concat

      val tabulate : int * (int -> elem) -> patch
          = tabulate

      val length : patch -> int
          = length

      val slices : patch -> slice list
          = slices

      val array : patch -> array
          = array

      val vector : patch -> ArrayStruct.vector
          = vector

      val slice : patch * int * int option -> slice
          = slice

      val patch : patch * patch * int -> patch
          = patch

      val subpatch : patch * int * int option -> patch
          = subpatch

      val superpatch : patch * patch * int -> patch
          = superpatch

      val update   : patch * int * elem  -> unit
          = update_

      val sub      : patch * int -> elem
          = sub_

      val foldr    : (elem * 'b -> 'b) -> 'b -> patch -> 'b   
          = foldr

      val foldl    : (elem * 'b -> 'b) -> 'b -> patch -> 'b
          = foldl

      val appi     : (int * elem -> unit) -> patch -> unit
          = appi

      val app      : (elem -> unit) -> patch -> unit
          = app

  end
end
