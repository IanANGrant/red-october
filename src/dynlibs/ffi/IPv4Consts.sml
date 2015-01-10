structure IPv4Consts = 
struct

(*
#define	IP_RF 0x8000			/* reserved fragment flag */
#define	IP_DF 0x4000			/* dont fragment flag */
#define	IP_MF 0x2000			/* more fragments flag */
#define	IP_OFFMASK 0x1fff		/* mask for fragmenting bits */ *)

datatype enum_t =
    IP_RF
  | IP_DF
  | IP_MF

type enum = enum_t

val flags = 
   [IP_RF,
    IP_DF,
    IP_MF]

fun fromWord n =
    case n of
         0wx8000 => IP_RF
       | 0wx4000 => IP_DF
       | 0wx2000 => IP_MF
       | w => raise Fail ("IPv4.fromWord: 0wx"^
                         (Word.toString w)^
             " is not a defined enumeration constant")

fun toWord e =
    case e of
         IP_RF => 0wx8000 
       | IP_DF => 0wx4000
       | IP_MF => 0wx2000

fun fromString s =
    case s of
         "RF" => IP_RF
       | "DF" => IP_DF
       | "MF" => IP_MF
       | w => raise Fail ("IPv4.fromString: \""^
                          s^"\" is not a defined \
                          \enumeration constant")

fun toString e =
    case e of
         IP_RF => "RF"
       | IP_DF => "DF"
       | IP_MF => "MF"
end
