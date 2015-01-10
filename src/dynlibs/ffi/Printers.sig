val atomicPrinter :
  ('a -> 'b) -> ('c * 'b -> 'd) -> 'c * 'a -> 'd
val listPrinter :
  ('a -> 'b * unit -> 'b) -> ('a -> 'b * 'c -> 'b) -> ''d ->
  (''d -> 'c * ''d) -> 'a -> 'b * ''d -> 'b
val seqPrinter :
  ('a -> 'b * 'c -> 'd) -> ('a -> 'd * 'e -> 'f) -> ('g -> 'c * 'e) -> 'a ->
  'b * 'g -> 'f
val altPrinter :
  ('a -> 'b * 'c -> 'd) -> ('a -> 'b * 'e -> 'f) ->
  (('c -> 'd) -> ('e -> 'f) -> 'g -> 'h) -> 'a -> 'b * 'g -> 'h
val optPrinter :
  ('a -> 'b * 'c -> 'd) -> ('a -> 'b * unit -> 'e) ->
  (('c -> 'd) -> (unit -> 'e) -> 'f -> 'g) -> 'a -> 'b * 'f -> 'g
