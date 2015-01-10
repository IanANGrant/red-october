val atomicScanner : ('b -> ('c * 'd) option) -> ('c -> 'e option) -> 'b -> ('e * 'd) option
val  listScanner :
  ('b -> ('c * 'd) option) -> ('d -> ('e * 'b) option) -> ('e -> 'f option) ->
  'g -> ('f * 'g -> 'g) -> ('g -> 'h option) -> 'd -> ('h * 'b) option
val  seqScanner :
  ('g -> ('h * 'i) option) -> ('i -> ('j * 'k) option) -> ('h -> 'l option) ->
  ('j -> 'm option) -> ('l * 'm -> 'n) -> ('n -> 'o option) -> 'g ->
  ('o * 'k) option
val  altScanner :
  ('b -> ('c * 'd) option) -> ('b -> ('e * 'd) option) -> ('c -> 'f option) ->
  ('e -> 'f option) -> ('f -> 'g option) -> 'b -> ('g * 'd) option
val optScanner :
  ('de -> ('df * 'de) option) -> ('df -> 'dg option) -> 'de ->
  ('dg option * 'de) option
