signature IPv4Packet =
sig
    type hdr
    type tos 
    type length 
    type id 
    type frag
    type ttl 
    type protocol 
    type checksum 
    type address 
    type options 
    type payload 
    type packetrep = hdr * 
                  tos *
                  length *
                  id *
                  frag *
                  ttl *
                  protocol *
                  checksum *
                  address *
                  address *
                  options *
                  payload
      type packet
      val get_hdr : packet -> hdr
      val get_tos : packet -> tos
      val get_length : packet -> length
      val get_id : packet -> id
      val get_frag : packet -> frag
      val get_ttl : packet -> ttl
      val get_protocol : packet -> protocol
      val get_checksum : packet -> checksum
      val get_srcaddress : packet -> address
      val get_dstaddress : packet -> address
      val get_options : packet -> options
      val get_payload : packet -> payload
      val set_hdr : packet * hdr -> unit
      val set_tos : packet * tos -> unit
      val set_length : packet * length -> unit
      val set_id : packet * id -> unit
      val set_frag : packet * frag -> unit
      val set_ttl : packet * ttl -> unit
      val set_protocol : packet * protocol -> unit
      val set_checksum : packet * checksum -> unit
      val set_srcaddress : packet * address -> unit
      val set_dstaddress : packet * address -> unit
      val set_options : packet * options -> unit
      val set_payload : packet * payload -> unit
      val new : packetrep -> packet
      val get : packet -> packetrep
      val set : packet * packetrep -> unit
end

functor ConcIPv4Packet
   (structure ConcRep : ConcRep) :> IPv4Packet
        where type hdr = ConcRep.X
          and type tos = ConcRep.X
          and type length = ConcRep.X
          and type id = ConcRep.X
          and type frag = ConcRep.X
          and type ttl = ConcRep.X
          and type protocol = ConcRep.X
          and type checksum = ConcRep.X
          and type address = ConcRep.X
          and type options = ConcRep.X
          and type payload = ConcRep.X =
struct
    type hdr = ConcRep.X
    type tos = ConcRep.X
    type length = ConcRep.X
    type id = ConcRep.X
    type frag = ConcRep.X
    type ttl = ConcRep.X
    type protocol = ConcRep.X
    type checksum = ConcRep.X
    type address = ConcRep.X
    type options = ConcRep.X
    type payload = ConcRep.X
    type packetrep =
                  hdr *
                  tos *
                  length *
                  id *
                  frag *
                  ttl *
                  protocol *
                  checksum *
                  address *
                  address *
                  options *
                  payload
    abstype packet = PACKET
     of {get : unit -> packetrep,
         set : packetrep -> unit,
         get_hdr : unit -> hdr,
         get_tos : unit -> tos,
         get_length : unit -> length,
         get_id : unit -> id,
         get_frag : unit -> frag,
         get_ttl : unit -> ttl,
         get_protocol : unit -> protocol,
         get_checksum : unit -> checksum,
         get_srcaddress : unit -> address,
         get_dstaddress : unit -> address,
         get_options : unit -> options,
         get_payload : unit -> payload,
         set_hdr : hdr -> unit,
         set_tos : tos -> unit,
         set_length : length -> unit,
         set_id : id -> unit,
         set_frag : frag -> unit,
         set_ttl : ttl -> unit,
         set_protocol : protocol -> unit,
         set_checksum : checksum -> unit,
         set_srcaddress : address -> unit,
         set_dstaddress : address -> unit,
         set_options : options -> unit,
         set_payload : payload -> unit,
         new : packetrep -> packet}
   with
      fun new (hdr, tos, len, id, frag, ttl, prot, csum, src, dst, opts, payld) =
         let 
             fun self (hdr, tos, len, id, frag,
                       ttl, prot, csum, src, dst, opts, payld) =
                let val (rhdr, rtos, rlen, rid, rfrag, 
                         rttl, rprot, rcsum, rsrc, rdst, ropts, rpayld) =
                        (ref hdr, ref tos, ref len, ref id, ref frag,
                         ref ttl, ref prot, ref csum, ref src, ref dst, ref opts, ref payld)
                in PACKET
                    {get = fn () =>
                             (!rhdr, !rtos, !rlen, !rid, !rfrag, !rttl,
                              !rprot, !rcsum, !rsrc, !rdst, !ropts, !rpayld),
                     set =
                           fn (hdr, tos, len, id, frag, ttl,
                              prot, csum, src, dst, opts, payld) =>
                                 (rhdr := hdr; rtos := tos; rlen := len;
                                  rid := id; rfrag := frag; rttl := ttl;
                                  rprot := prot; rcsum := csum; rsrc := src; rdst := dst;
                                  ropts := opts; rpayld := payld),
                     get_hdr = fn () => !rhdr,
                     get_tos = fn () => !rtos,
                     get_length = fn () => !rlen,
                     get_id = fn () => !rid,
                     get_frag = fn () => !rfrag,
                     get_ttl = fn () => !rttl,
                     get_protocol = fn () => !rprot,
                     get_checksum = fn () => !rcsum,
                     get_srcaddress = fn () => !rsrc,
                     get_dstaddress = fn () => !rdst,
                     get_options = fn () => !ropts,
                     get_payload = fn () => !rpayld,
                     set_hdr = fn x => rhdr := x,
                     set_tos = fn x => rtos := x,
                     set_length = fn x => rlen := x,
                     set_id = fn x => rid := x,
                     set_frag = fn x => rfrag := x,
                     set_ttl = fn x => rttl := x,
                     set_protocol = fn x => rprot := x,
                     set_checksum = fn x => rcsum := x,
                     set_srcaddress = fn x => rsrc := x,
                     set_dstaddress = fn x => rdst := x,
                     set_options = fn x => ropts := x,
                     set_payload = fn x => rpayld := x,
                     new = fn (hdr, tos, len, id, frag, ttl,
                               prot, csum, src, dst, opts, payld)
                                 => self (hdr, tos, len, id, frag, ttl,
                                          prot, csum, src, dst, opts, payld)}
                end
         in self (hdr, tos, len, id, frag, ttl,
                  prot, csum, src, dst, opts, payld)
         end
      fun get_hdr (PACKET pr) = (#get_hdr pr) ()
      fun get_tos (PACKET pr) = (#get_tos pr) ()
      fun get_length (PACKET pr) = (#get_length pr) ()
      fun get_id (PACKET pr) = (#get_id pr) ()
      fun get_frag (PACKET pr) = (#get_frag pr) ()
      fun get_ttl (PACKET pr) = (#get_ttl pr) ()
      fun get_protocol (PACKET pr) = (#get_protocol pr) ()
      fun get_checksum (PACKET pr) = (#get_checksum pr) ()
      fun get_srcaddress (PACKET pr) = (#get_srcaddress pr) ()
      fun get_dstaddress (PACKET pr) = (#get_dstaddress pr) ()
      fun get_options (PACKET pr) = (#get_options pr) ()
      fun get_payload (PACKET pr) = (#get_payload pr) ()
      fun set_hdr (PACKET pr, x) = (#set_hdr pr) x
      fun set_tos (PACKET pr, x) = (#set_tos pr) x
      fun set_length (PACKET pr, x) = (#set_length pr) x
      fun set_id (PACKET pr, x) = (#set_id pr) x
      fun set_frag (PACKET pr, x) = (#set_frag pr) x
      fun set_ttl (PACKET pr, x) = (#set_ttl pr) x
      fun set_protocol (PACKET pr, x) = (#set_protocol pr) x
      fun set_checksum (PACKET pr, x) = (#set_checksum pr) x
      fun set_srcaddress (PACKET pr, x) = (#set_srcaddress pr) x
      fun set_dstaddress (PACKET pr, x) = (#set_dstaddress pr) x
      fun set_options (PACKET pr, x) = (#set_options pr) x
      fun set_payload (PACKET pr, x) = (#set_payload pr) x
      fun get (PACKET pr) = (#get pr) ()
      fun set (PACKET pr, y) = (#set pr) y
   end
end

functor AbstIPv4Packet
   (structure ConcRep : ConcRep
    structure Packet : IPv4Packet 
    type hdr
    type tos
    type length
    type id
    type frag
    type ttl
    type protocol
    type checksum
    type address
    type options
    type payload
    structure Hdr : AbstRep
      where type X = Packet.hdr
        and type Y = hdr
    structure Tos : AbstRep 
     where type X = Packet.tos
       and type Y = tos
    structure Length : AbstRep 
     where type X = Packet.length
       and type Y = length
    structure Id : AbstRep 
     where type X = Packet.id
       and type Y = id
    structure Frag : AbstRep 
     where type X = Packet.frag
       and type Y = frag
    structure Ttl : AbstRep 
     where type X = Packet.ttl
       and type Y = ttl
    structure Protocol : AbstRep 
     where type X = Packet.protocol
       and type Y = protocol
    structure Checksum : AbstRep 
     where type X = Packet.checksum
       and type Y = checksum
    structure Address : AbstRep 
     where type X = Packet.address
       and type Y = address
    structure Options : AbstRep 
     where type X = Packet.options
       and type Y = options
    structure Payload : AbstRep
     where type X = Packet.payload
       and type Y = payload) : IPv4Packet = 
struct
    type hdr = hdr
    type tos = tos
    type length = length
    type id = id
    type frag = frag
    type ttl = ttl
    type protocol = protocol
    type checksum = checksum
    type address = address
    type options = options
    type payload = payload
    type packetrep = hdr *
                  tos *
                  length *
                  id *
                  frag *
                  ttl *
                  protocol *
                  checksum *
                  address *
                  address *
                  options *
                  payload
      type packet = Packet.packet
      val get_hdr = fn p => Hdr.get(Packet.get_hdr p)
      val get_tos = fn p => Tos.get(Packet.get_tos p)
      val get_length = fn p => Length.get(Packet.get_length p)
      val get_id = fn p => Id.get(Packet.get_id p)
      val get_frag = fn p => Frag.get(Packet.get_frag p)
      val get_ttl = fn p => Ttl.get(Packet.get_ttl p)
      val get_protocol = fn p => Protocol.get(Packet.get_protocol p)
      val get_checksum = fn p => Checksum.get(Packet.get_checksum p)
      val get_srcaddress = fn p => Address.get(Packet.get_srcaddress p)
      val get_dstaddress = fn p => Address.get(Packet.get_dstaddress p)
      val get_options = fn p => Options.get(Packet.get_options p)
      val get_payload = fn p => Payload.get(Packet.get_payload p)
      val set_hdr = fn (p,x) => Packet.set_hdr (p,Hdr.set x)
      val set_tos = fn (p,x) => Packet.set_tos (p,Tos.set x)
      val set_length = fn (p,x) => Packet.set_length (p,Length.set x)
      val set_id = fn (p,x) => Packet.set_id(p,Id.set x)
      val set_frag = fn (p,x) => Packet.set_frag(p,Frag.set x)
      val set_ttl = fn (p,x) => Packet.set_ttl(p, Ttl.set x)
      val set_protocol = fn (p,x) => Packet.set_protocol(p,Protocol.set x)
      val set_checksum = fn (p,x) => Packet.set_checksum(p,Checksum.set x)
      val set_srcaddress = fn (p,x) => Packet.set_srcaddress(p,Address.set x)
      val set_dstaddress = fn (p,x) => Packet.set_dstaddress(p,Address.set x)
      val set_options = fn (p,x) => Packet.set_options(p,Options.set x)
      val set_payload = fn (p,x) => Packet.set_payload(p,Payload.set x)
      fun new (hdr, tos, len, id, frag, ttl,
                   prot, csum, src, dst, opts, payld)
                =  Packet.new
                 (Hdr.set (hdr), Tos.set (tos),
                  Length.set (len), Id.set (id), Frag.set (frag),
                  Ttl.set (ttl), Protocol.set (prot), Checksum.set (csum), 
                  Address.set (src), Address.set (dst), Options.set (opts),
                  Payload.set (payld))
      fun get p =
              let val (hdr, tos, len, id, frag, ttl,
                           prot, csum, src, dst, opts, payld) = Packet.get p
              in (Hdr.get hdr, Tos.get tos,
                  Length.get len, Id.get id, Frag.get frag,
                  Ttl.get ttl, Protocol.get prot, Checksum.get csum, 
                  Address.get src, Address.get dst, Options.get opts,
                  Payload.get payld)
              end
      fun set (p, (hdr, tos, len, id, frag, ttl,
                   prot, csum, src, dst, opts, payld)) =
               Packet.set (p,
                 (Hdr.set (hdr), Tos.set (tos),
                  Length.set (len), Id.set (id), Frag.set (frag),
                  Ttl.set (ttl), Protocol.set (prot), Checksum.set (csum), 
                  Address.set (src), Address.set (dst), Options.set (opts),
                  Payload.set (payld)))
end

functor IPv4PacketStrm
  (structure ConcRep : ConcRep
    type hdr
    type tos
    type length
    type id
    type frag
    type ttl
    type protocol
    type checksum
    type address
    type options
    type payload
    structure Hdr : StrmRep
      where type S = ConcRep.X
        and type T = hdr
    structure Tos : StrmRep 
     where type S = ConcRep.X
       and type T = tos
    structure Length : StrmRep 
     where type S = ConcRep.X
       and type T = length
    structure Id : StrmRep 
     where type S = ConcRep.X
       and type T = id
    structure Frag : StrmRep 
     where type S = ConcRep.X
       and type T = frag
    structure Ttl : StrmRep 
     where type S = ConcRep.X
       and type T = ttl
    structure Protocol : StrmRep 
     where type S = ConcRep.X
       and type T = protocol
    structure Checksum : StrmRep 
     where type S = ConcRep.X
       and type T = checksum
    structure Address : StrmRep 
     where type S = ConcRep.X
       and type T = address
    structure Options : StrmRep 
     where type S = ConcRep.X
       and type T = options
    structure Payload : StrmRep 
     where type S = ConcRep.X
       and type T = payload) : StrmRep where type S = ConcRep.X =
struct
   type S = ConcRep.X
   type T = hdr *
                  tos *
                  length *
                  id *
                  frag *
                  ttl *
                  protocol *
                  checksum *
                  address *
                  address *
                  options *
                  payload
      fun getItem s =
            case Hdr.getItem s
              of SOME (hdr,s) =>
                 (case Tos.getItem s
                    of SOME (tos,s) =>
                       (case Length.getItem s
                          of SOME (len,s) =>
                             (case Id.getItem s
                                of SOME (id,s) =>
                                   (case Frag.getItem s
                                      of SOME (frag,s) =>
                                         (case Ttl.getItem s
                                            of SOME (ttl,s) =>
                                               (case Protocol.getItem s
                                                  of SOME (prot,s) =>
                                                     (case Checksum.getItem s
                                                        of SOME (csum,s) =>
                                                           (case Address.getItem s
                                                              of SOME (src,s) =>
                                                           (case Address.getItem s
                                                              of SOME (dst,s) =>
                                                           (case Options.getItem s
                                                              of SOME (opts,s) =>
                                                           (case Payload.getItem s
                                                              of SOME (payld,s) =>
                                                                  SOME ((hdr, tos, len,
                                                                         id, frag, ttl,
                                                                         prot, csum, src,
                                                                         dst, opts, payld),s)
                                                               | NONE => NONE)
                                                               | NONE => NONE)
                                                               | NONE => NONE)
                                                               | NONE => NONE)
                                                         | NONE => NONE)
                                                   | NONE => NONE)
                                             | NONE => NONE)
                                       | NONE => NONE)
                                 | NONE => NONE)
                           | NONE => NONE)
                     | NONE => NONE)
               | NONE => NONE
      fun setItem (s, (hdr, tos, len, id, frag, ttl,
                       prot, csum, src, dst, opts, payld)) =
                  let val s = Hdr.setItem(s,hdr)
                      val s = Tos.setItem(s,tos)
                      val s = Length.setItem(s,len)
                      val s = Id.setItem(s,id)
                      val s = Frag.setItem(s,frag)
                      val s = Ttl.setItem(s,ttl)
                      val s = Protocol.setItem(s,prot)
                      val s = Checksum.setItem(s,csum) 
                      val s = Address.setItem(s,src)
                      val s = Address.setItem(s,dst)
                      val s = Options.setItem(s,opts)
                      val s = Payload.setItem(s,payld)
                  in s
                  end
end
