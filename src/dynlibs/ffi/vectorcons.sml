fun w8pat (#[0w0 : Word8.word,0w1]) = 0
  | w8pat (#[0w0, 0w0]) = 1
  | w8pat _  = 2

fun w8pat' ([0w0 : Word8.word]) = 0
  | w8pat' ([0w1, 0w0]) = 1
  | w8pat' (0w2::0w0::_) = 2
  | w8pat' _  = 3

datatype ('a,'b) pair =
     <% of 'a * ('a,'b) pair
   | %> of ('a,'b) pair * 'b
   | << of 'a
   | >>

infixr 6 <% 
infix 5 %>
nonfix <<
nonfix >>

val t =    (1 <% 2 <% 3 <% >>
        %>  1 <% 2      <% >>
        %>  1 <% 2 <% 3 <% >>)
 <%
           (1 <% 2 <% 3 <% >>
        %>  1 <% 2      <% >>
        %>  1 <% 2 <% 3 <% >>)
 <% >>
