
fun atomicPrinter print_elt consumer =
   fn (acc,x) => consumer (acc,print_elt x)

fun listPrinter print_delim print_elt nilv decons consumer =
   let fun iter first (acc, l) =
         if l = nilv
            then acc
            else let val (elt,rest) = decons l
                     val acc' = if not first then print_delim consumer (acc, ()) else acc
                     val acc'' = print_elt consumer (acc',elt)
                 in iter false (acc'', rest)
                 end
   in iter true
   end

fun seqPrinter print_elt1 print_elt2 decons consumer =
   let fun printer (acc, l) =
          let val (elt1,elt2) = decons l
              val acc' =  print_elt1 consumer (acc, elt1)
              val acc'' = print_elt2 consumer (acc',elt2)
          in acc''
          end
   in printer
   end

fun altPrinter print_elt1 print_elt2 decons consumer =
   let fun printer (acc, l) =
          let val elt1p = fn v => print_elt1 consumer (acc, v)
              val elt2p = fn v => print_elt2 consumer (acc, v)
              val acc' = decons elt1p elt2p l
          in acc'
          end
   in printer
   end

fun optPrinter print_elt print_none decons consumer =
   let fun printer (acc, l) =
          let val eltp = fn v => print_elt consumer (acc, v)
              val nonep = fn () => print_none consumer (acc, ())
              val acc' = decons eltp nonep l
          in acc'
          end
   in printer
   end
