(* A channel is not a thing in itself. It is a relation between its
   endpoints. And the endpoints are not things in themselves either,
   they are processes. Processes are not substantive things, because
   they are abstract patterns of behaviour with definite intensional
   descriptions. One substantial thing, such as a person, for example,
   may at any moment be engaged in one or more processes. So what we
   model as "things," call them objects if you like, are collections
   of state. And processes, then, are the causal patterns which govern
   the way the state changes. So a channel is a connection between
   processes which establishes a certain relation, more properly
   called a _correlation,_ between the patterns of state changes in
   those processes which we call the endpoints of the channel. Clearly
   then, a channel need not connect two distinct "things," it could
   just as usefully connect two sub-processes which are each part of
   the same "thing." So channels and processes are abstract
   intensional descriptions of the logical relations that hold between
   the changes of state in the various parts of a system.  Dually, we
   can think of the state of the system as being determined by the
   abstract channels and processes. That is to say that we need not
   necessarily represent the state as some particular concrete object
   in a definite place. Provided we have a means to interpret the
   state changes as representing the state of the system at any
   moment, the whole system can remain abstract, its only
   representation being in terms of state changes. This will allow us
   to multiply-represent logical systems, and to know them to be the
   same logical system because we observe perfect correlations of
   state between two physically independent representations. The two
   independent process co-verify each other. *)

structure Process =
struct
   abstype 'a process =
      Process of {event : 'a -> 'a process}
    | Stop
   with
      fun new evfn =
         fn s => 
            let fun self s =
                Process {event =
                          fn e => case evfn (s,e)
                                    of SOME s' => self s'
                                     | NONE => Stop}
            in self s
            end
      fun event (Process s) = #event s
        | event Stop = fn _ => Stop
   end
end

structure State =
struct
   abstype 'a state =
      State of {get : 'a,
                set : 'a -> 'a state}
   with
      fun new getfn setfn =
         fn s =>
            let fun self s =
                State {get = getfn s,
                       set =
                         fn s' =>
                           self(setfn (s,s'))}
            in self s
            end
      fun get (State s) = #get s
      fun set (State s) = #set s
   end
end

structure Channel =
struct
   abstype ('a,'b) channel =
      Dec
       of 'a * ('a,'b) channel Process.process
    | Rec
       of 'b * ('a,'b) channel Process.process
   with
      
   end
end

fun induction deconstruct reconstruct =
   let fun reconstructor argl = fn argr =>
                  reconstruct deconstructor argl argr
       and deconstructor argl = fn argr =>
                  deconstruct reconstructor argl argr
   in
      deconstructor
   end
