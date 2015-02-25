(* 
   "Select a machine with sound basic characteristics (e.g. an
    interrupt system to fall in love with is certainly an inspiring
    feature); from then on try to keep the specific properties of the
    configuration for which you are preparing the system out of your
    considerations as long as possible."      --- Edsger W. Dijkstra

   A channel is not a thing in itself. It is a relation between its
   endpoints. And the endpoints are not things in themselves either,
   they are processes. Processes are not substantive things, they are
   abstract patterns of behaviour with definite intensional
   descriptions. One substantial thing, a Woman, for example, may at
   any moment be consciously engaged in a dozen or more processes:

  "The board was a vast expanse of eyes, with, at the base, a dozen or
   so pairs of plugs on cords for connecting and an equal number of
   keys for talking, listening, and ringing. On a busy day these cords
   were woven across the board in a constantly changing, confusing
   pattern; half the people using telephones were convinced that
   Central was incompetent or hated them, and Central—flipping plugs
   into holes, ringing numbers, trying to remember whether 44 wanted
   170-K or 170-L, because if she went back and asked him, he’d be
   sure she was stupid—was close to hysterics. It was every operator’s
   dream that when her ship came in she would open all the keys on a
   busy board, yell “To hell with you,” pull all the plugs, and march
   out in triumph, leaving everything in total chaos. Nobody ever
   did. We felt an awful responsibility toward our little corner of
   the world. We really helped keep it running, one girl at a time all
   by herself at the board." http://montanawomenshistory.org/number-please

   After six weeks of training, the women consigned their boyfriends
   to memory archives and returned to Penn, where they were given
   poster-size diagrams and charts describing ENIAC. “Somebody gave us
   a whole stack of blueprints, and these were the wiring diagrams for
   all the panels, and they said, ‘Here, figure out how the machine
   works and then figure out how to program it,’” explained
   McNulty. That required analyzing the differential equations and
   then determining how to patch the cables to connect to the correct
   electronic circuits. “The biggest advantage of learning the ENIAC
   from the diagrams was that we began to understand what it could and
   could not do,” said Jennings. “As a result we could diagnose
   troubles almost down to the individual vacuum tube.” She and Snyder
   devised a system to figure out which of the 18,000 vacuum tubes had
   burned out. “Since we knew both the application and the machine, we
   learned to diagnose troubles as well as, if not better than, the
   engineers. I tell you, those engineers loved it. They could leave
   the debugging to us.”                     Fortune. October 6 2014

   So what we model as "things," call them objects if you like, are
   collections of state. And processes, then, are the causal patterns
   which govern the way the state changes: they're the way the things
   behave. So a channel is a connection between processes which
   establishes a certain relation, more properly called a
   _correlation,_ between the patterns of state changes in those
   processes which we call the endpoints of the channel. Clearly then,
   a channel need not connect two distinct "things," it could just as
   usefully connect two sub-processes which are each part of the same
   "thing." So channels and processes are abstract intensional
   descriptions of the logical relations that hold between the changes
   of state in the various loosely-coupled parts of a whole system. 

   Dually, we can think of the state of the system as being determined
   by the abstract channels and processes. That is to say that we need
   not necessarily represent the state as some particular concrete
   object in a definite place. Provided we have a means to interpret
   observable events as the state changes and _infer_ the state of the
   system at any moment, the whole system can remain abstract, its
   only representation being in terms of _state changes._ This will
   allow us to multiply-represent logical systems, and to know them to
   be the same logical system because we infer perfect correlations of
   state between two physically independent representations. The two
   independent process co-verify one another.  Then these ongoing
   process of co-verification will be what we call the state. This is
   an idea of Karen Sparck-Jones':

   "And then the lab became a home for Julia Galliers, who got a
    post-doc research fellowship, I think it was—EPSRC post-doc
    research fellowship. She’d done some very interesting work on
    modeling changes of belief. If we have a dialogue with one
    another, all the time you’re changing your beliefs: about what it
    is the other person’s saying; what the state of the world is; all
    of these kind of things. And what you want to do is to model what
    beliefs people have got and how they change, and what you’re
    particularly interested in is which ones they change, and why they
    change them. Supposing somebody says something to me, and it can
    be interpreted in different ways, and some of these ways will be
    compatible with one lot of beliefs, and the others will be
    compatible with another lot of beliefs. Which ones do I choose,
    and what are the general criteria I apply for choosing one rather
    than the other? So this was the kind of thing, and she’d done some
    rather interesting work for her thesis—elsewhere, not in
    Cambridge—on that.

    So we thought we’d have a project which was going to put these
    ideas to work in the context of information retrieval, because if
    you have a dialogue with a librarian: [Let’s say] I come along and
    I say, “Have you got any books on Michelangelo?” and the librarian
    says, “The books on sculpture are over there.” Well, you see she’s
    made some assumption, on the basis of what she knows and what she
    believes I’m interested in, that I’m interested in Michelangelo’s
    sculpture. In fact, I might be interested in the “Last Judgment”;
    so we’d go through a whole lot of this dialogue. And we thought it
    would be a rather interesting test context for this, because you
    can constrain it a little bit; also, there were some people at
    City University who’d done some rather interesting work on the
    sorts of dialogues that people have with librarians—task analysis,
    you know, and stuff like that. Anyway, then she got this research
    fellowship, so she joined me as P.I. on this project, and three of
    the Research Councils had had a joint initiative on supporting
    research in cognitive science, so this was funded under this
    cognitive science initiative; this was at the beginning of the
    ‘90s. It was quite interesting; it was a rather difficult project,
    [but] it was rather interesting to do. Julia eventually left and
    went away and did something else, but it was kind of interesting
    at the time."

   http://www.ieeeghn.org/wiki/index.php/Oral-History:Karen_Sp%C3%A4rck_Jones

   So let's see if we can't use processes to model states. This would
   give us a way to implement, e.g. semaphores as channels, the
   channel being correlated state, and the endpoints therefore
   nonlocal state.

   The Stop state is Tony's idea. I'm not at all sure about it
   though. He doesn't give any semantics for it, you see. He says "it
   doesn't take part in any events." Well, if it doesn't take part in
   events then it's not a process, is it? Is this state observable in
   any way at all then? If not, then how do we know the Stop state is
   real, and not just a figment of our imagination?

   The way we've done it here is to say that the process still takes
   part in _all_ events it would have taken part in before it was
   stopped, it's just that now you can't observe it.

   Tony says the Stop state is "broken". Well, I suppose that is the
   case if you abstract away all the irrelevant detail of machines,
   such as who makes them, what they make them for, how they make
   them, and why they break, who puts more chocs and biscuits in when
   they're empty, and who takes all the coins out when they're full,
   etc. etc. So you study machines that are like the Global Economy:
   they're uncreated, maintenance free, they don't consume resources
   and they don't produce any waste; and they operate perfectly, until
   they break. But really, what is left of the abstract machine that's
   going to be interesting?  Maybe the question "how can such a
   perfectly deterministic machine break?"?  And dear old Bishop
   Berkely might well ask: "Well now, Mister Hoare, tell me if you
   please: is a machine which you describe as one which 'sells two
   chocs and then breaks' really broken, if it then goes and does
   _exactly_ that very thing?  I mean, is that not what _somebody_
   _meant_ it to do?"

   And when they break, _real_ machines do so for what is always a
   particular, though more or less well known _reason._ The coin
   slot's jammed with an old Irish 5p piece, or somebody ram-raided it
   in a white van and stole all the chocs, or Robin blew it to
   smithereens with a block of semtex one Saturday afternoon because
   he was fed up with the constant humming noise, etc. etc. I say
   "more or less well-known" because sometimes _we_ just _think_
   they're broken, until it dawns on us that they're actually behaving
   exactly as specified, and the problem was just that we didn't
   understand our own bloody Z specification!

   I personally feel intuitively much happer with Humberto Maturana
   and Fancisco Varela's definition of mechanism, which is something
   like "a mechanism is the ongoing effective operation of the machine
   in its environment."  And they go on to produce three levels of
   what they call "autopoesis" (self creation or causation of self). I
   hope it's three, then they correspond nicely to Aristotle's three
   souls of man: cellular, organic and linguistic in Maturana and
   Varela's terms, vegetable, animal and rational in Aristotle's.

   Anyone interested in the "underlying" metaphor should join the SPR
   and browse their archives, I think they're still in Cambridge,
   aren't they? You will very quickly find yourself in the very good
   company of people like Thomson Lord Kelvin, Venn, my dear old
   friend and tutor Augustus and, dare we say, a whole _host_ of
   _terribly_ rational men, who've all been _literaly dying_ to talk
   to you for ages!  And do read my 'Notes'. They were nine months in
   production, and that was before I went into labour!

   Sorry to pull the plugs on you all like this, but I've got to dash;
   my ship's just come in, you see!

   Lots of love A.A.L.

 *)

structure Process =
struct
   abstype 'a process =
      Process of {set : 'a -> 'a process}
    | Stop    of {get : 'a }
   with
      fun new evfn =
         fn s => 
            let fun self s =
                Process {set =
                          fn e => case evfn (s,e)
                                    of SOME s' => self s'
                                     | NONE => Stop {get = e}}
            in self s
            end
      fun event (Process s) = #set s
        | event (Stop _) = fn e => Stop {get = e}
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
                           self(setfn s s')}
            in self s
            end
      fun get (State s) = #get s
      fun set (State s) = #set s
   end
end

structure BinTree =
struct
   abstype 'a bintree =
       Branch of 'a bintree * 'a bintree
     | Leaf of 'a
   with
      fun branch (l,r) = Branch (l,r)
      fun leaf s = Leaf s
      fun deconstruct (bf,lf) =
         fn (Branch (l,r)) => bf (l,r)
          | (Leaf s) => lf s
   end
end

structure BinList =
struct
      fun cons (v,l) = BinTree.branch (BinTree.leaf "cons",BinTree.branch (v,l))
      fun null () = BinTree.leaf "null"
      fun deconstruct (cf,nf) =
         BinTree.deconstruct
            (fn (l,r) =>
               let val () = BinTree.deconstruct
                             (fn _ => raise Fail "BinList.internal error. expected leaf",
                              fn "cons" => ()
                               | _ => raise Fail "BinList.internal error. expected cons") l
                   val res = BinTree.deconstruct
                                 (fn x => x,
                                  fn _ => raise Fail "BinList.internal error. expected branch") r
               in cf res end,
             fn "null" => nf () | _ => raise Fail "BinList.internal error. expected null")
end

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

structure BinAbsSyn =
struct
   local fun fail s = raise Fail ("BinAbsSyn.deconstruct internal error "^s)
   in
      fun term s = BinTree.branch (BinTree.leaf "term",BinTree.leaf s)
      fun nonterm (s,l) =
         BinTree.branch (BinTree.leaf "nonterm",BinTree.branch (s,l))
      fun deconstruct (ntf,tf) =
         BinTree.deconstruct
            (fn (l,r) =>
                BinTree.deconstruct
                  (fn _ => fail "expected leaf", 
                   fn "term" => 
                      BinTree.deconstruct (fn _ => fail "term: expected leaf", tf) r
                    | "nonterm" => 
                      BinTree.deconstruct (ntf , fn _ => fail "nonterm: expected branch") r
                    | _ => fail "nonterm: expected term or nonterm") l,
             fn _ => fail "expected branch")
   end
end

structure Channel =
struct
   abstype ('a,'b) ichan =
       Dec of (('a,'b) ochan * 'b) Process.process * 'a
   and ('b,'a) ochan = 
       Rec of (('b,'a) ichan * 'a) Process.process * 'b
   with
      
   end
end
