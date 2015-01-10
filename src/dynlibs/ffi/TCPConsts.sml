structure TCP_Consts = 
struct
datatype TCP_Consts_enum =
    TCP_ESTABLISHED
  | TCP_SYN_SENT
  | TCP_SYN_RECV
  | TCP_FIN_WAIT1
  | TCP_FIN_WAIT2
  | TCP_TIME_WAIT
  | TCP_CLOSE
  | TCP_CLOSE_WAIT
  | TCP_LAST_ACK
  | TCP_LISTEN
  | TCP_CLOSING

type enum = TCP_Consts_enum

val flags = 
   [TCP_ESTABLISHED,
   TCP_SYN_SENT,
   TCP_SYN_RECV,
   TCP_FIN_WAIT1,
   TCP_FIN_WAIT2,
   TCP_TIME_WAIT,
   TCP_CLOSE,
   TCP_CLOSE_WAIT,
   TCP_LAST_ACK,
   TCP_LISTEN,
   TCP_CLOSING]

fun fromWord n =
    case n of
         0wx1 => TCP_ESTABLISHED
       | 0wx2 => TCP_SYN_SENT
       | 0wx3 => TCP_SYN_RECV
       | 0wx4 => TCP_FIN_WAIT1
       | 0wx5 => TCP_FIN_WAIT2
       | 0wx6 => TCP_TIME_WAIT
       | 0wx7 => TCP_CLOSE
       | 0wx8 => TCP_CLOSE_WAIT
       | 0wx9 => TCP_LAST_ACK
       | 0wxA => TCP_LISTEN
       | 0wxB => TCP_CLOSING
       | w => raise Fail ("TCP_Consts.fromWord: 0wx"^(Word.toString w)^" is not a defined enumeration constant")

fun toWord e =
    case e of
         TCP_ESTABLISHED => 0wx1
       | TCP_SYN_SENT => 0wx2
       | TCP_SYN_RECV => 0wx3
       | TCP_FIN_WAIT1 => 0wx4
       | TCP_FIN_WAIT2 => 0wx5
       | TCP_TIME_WAIT => 0wx6
       | TCP_CLOSE => 0wx7
       | TCP_CLOSE_WAIT => 0wx8
       | TCP_LAST_ACK => 0wx9
       | TCP_LISTEN => 0wxA
       | TCP_CLOSING => 0wxB

fun fromString s =
    case s of
         "TCP_ESTABLISHED" => TCP_ESTABLISHED
       | "TCP_SYN_SENT" => TCP_SYN_SENT
       | "TCP_SYN_RECV" => TCP_SYN_RECV
       | "TCP_FIN_WAIT1" => TCP_FIN_WAIT1
       | "TCP_FIN_WAIT2" => TCP_FIN_WAIT2
       | "TCP_TIME_WAIT" => TCP_TIME_WAIT
       | "TCP_CLOSE" => TCP_CLOSE
       | "TCP_CLOSE_WAIT" => TCP_CLOSE_WAIT
       | "TCP_LAST_ACK" => TCP_LAST_ACK
       | "TCP_LISTEN" => TCP_LISTEN
       | "TCP_CLOSING" => TCP_CLOSING
       | w => raise Fail ("TCP_Consts.fromString: \""^s^"\" is not a defined enumeration constant")

fun toString e =
    case e of
         TCP_ESTABLISHED => "TCP_ESTABLISHED"
       | TCP_SYN_SENT => "TCP_SYN_SENT"
       | TCP_SYN_RECV => "TCP_SYN_RECV"
       | TCP_FIN_WAIT1 => "TCP_FIN_WAIT1"
       | TCP_FIN_WAIT2 => "TCP_FIN_WAIT2"
       | TCP_TIME_WAIT => "TCP_TIME_WAIT"
       | TCP_CLOSE => "TCP_CLOSE"
       | TCP_CLOSE_WAIT => "TCP_CLOSE_WAIT"
       | TCP_LAST_ACK => "TCP_LAST_ACK"
       | TCP_LISTEN => "TCP_LISTEN"
       | TCP_CLOSING => "TCP_CLOSING"
end

structure TH = 
struct
datatype TH_enum =
    TH_FIN
  | TH_SYN
  | TH_RST
  | TH_PUSH
  | TH_ACK
  | TH_URG

type enum = TH_enum

val flags = 
   [TH_FIN,
   TH_SYN,
   TH_RST,
   TH_PUSH,
   TH_ACK,
   TH_URG]

fun fromWord n =
    case n of
         0wx1 => TH_FIN
       | 0wx2 => TH_SYN
       | 0wx4 => TH_RST
       | 0wx8 => TH_PUSH
       | 0wx10 => TH_ACK
       | 0wx20 => TH_URG
       | w => raise Fail ("TH.fromWord: 0wx"^
                         (Word.toString w)^
             " is not a defined enumeration constant")

fun toWord e =
    case e of
         TH_FIN => 0wx1
       | TH_SYN => 0wx2
       | TH_RST => 0wx4
       | TH_PUSH => 0wx8
       | TH_ACK => 0wx10
       | TH_URG => 0wx20

fun fromString s =
    case s of
         "TH_FIN" => TH_FIN
       | "TH_SYN" => TH_SYN
       | "TH_RST" => TH_RST
       | "TH_PUSH" => TH_PUSH
       | "TH_ACK" => TH_ACK
       | "TH_URG" => TH_URG
       | w => raise Fail ("TH.fromString: \""^
                          s^"\" is not a defined \
                          \enumeration constant")

fun toString e =
    case e of
         TH_FIN => "TH_FIN"
       | TH_SYN => "TH_SYN"
       | TH_RST => "TH_RST"
       | TH_PUSH => "TH_PUSH"
       | TH_ACK => "TH_ACK"
       | TH_URG => "TH_URG"
end
