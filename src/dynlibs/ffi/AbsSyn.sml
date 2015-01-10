signature AbsSyn =
sig
   datatype ('a,'b) abssyn =
       Term of 'a
     | NonTerm of 'b * ('a,'b) abssyn list
end

structure AbsSyn : AbsSyn =
struct
   datatype ('a,'b) abssyn =
       Term of 'a
     | NonTerm of 'b * ('a,'b) abssyn list   
end
