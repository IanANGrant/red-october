load "Socket";
load "Unix";
load "Process";
load "Time";
load "Regex";
load "Polyhash";

open Socket;

val debug = ref false;

fun dprint s =
   if !debug then print s else ();

fun bugon () = debug := true;
fun bugoff () = debug := false;

val bufStr = String.implode o
             (List.map (Char.chr o Word8.toInt)) o
             (Word8Vector.foldr op :: [])
val strBuf = Word8Vector.fromList o
             (List.map (Word8.fromInt o Char.ord)) o
             String.explode

fun writeStr fd s = (writeVec (fd, {buf = strBuf s, ofs = 0, size = NONE}));

fun readBuf buf n =
  fn fd => 
   let val fdd = fdDesc fd 
       val {rds = rds, ...} = 
             Socket.pselect {rds = [fdd],
                             wrs = [], exs = [],
                             timeout = SOME (Time.fromMilliseconds 0),
                             signals = []}
       val nr = case rds
                  of [_] => readVec (fd, {buf = buf, ofs = 0, size = SOME n})
                   | _ => 0
   in nr
   end

fun readStr n =
   let val buf = Word8Vector.tabulate (n,fn _ => 0w0)
   in fn fd =>
      let val nr = readBuf buf n fd
      in String.substring(bufStr buf,0,nr)
      end
   end

datatype readst = CHAR | CR
datatype linemode = LF | CRLF

fun mkInputStream pid fd bufsiz mode =
   let val wr = ref (fn _ => ())
       val sr = ref CHAR
       val ar = ref ""
       val buf = Word8Vector.tabulate (bufsiz,fn _ => 0w0)
       fun curline () = !ar
       fun watch f = wr := f
       fun state () = !sr
       fun update () = 
          let val ncs = (readBuf buf bufsiz fd)
              fun iter s a 0 = (s,a)
                | iter s a n =
                  let val c = Char.chr (Word8.toInt (Word8Vector.sub (buf,ncs-n)))
                  in case (mode,s,c)
                       of (CRLF,CHAR,#"\r") => iter CR a (n-1)
                        | (_,CHAR,c) => iter s (a^(String.str c)) (n-1)
                        | (_,CR,#"\n") => ((!wr) (pid,a^"\n"); iter CHAR "" (n-1))
                        | (CRLF,CR,_) => iter CHAR (a^"\r") n
                        | (LF,CR,_) => raise Fail "Internal error: update: invalid state."
                  end
              val (s,a) = iter (!sr) (!ar) ncs
          in sr := s; ar := a; ncs
          end
      fun flush () =
         if (!ar) <> "" then ((!wr) (pid,!ar); ar := "") else ()
   in { update = update,
         flush = flush,
         watch = watch,
       curline = curline,
         state = state }
   end

fun mkMatcher () =
   let val lr = ref []
       fun clear () = lr := []
       fun add (mid : string,p,f) =
             lr := ((mid,p,f)::(!lr))
       fun del mid =
             lr := List.filter
                     (fn (mid',_,_) => mid <> mid')
                     (!lr)
       fun watch (pid,s) =
           List.app (fn (mid,p,f) =>
                        if p s
                           then f (pid,mid,s)
                           else ()) (!lr)
   in {clear = clear,
         add = add,
         del = del,
       watch = watch}
   end

fun mkReMatcher ()  =
   let val {add : string *
                  (string -> bool) *
                  (string * string * string -> unit) -> unit,
            clear : unit -> unit,
            del : string -> unit,
            watch : string * string -> unit} = mkMatcher ()
       fun add' (mid : string,pat,f) =
          let val re = Regex.regcomp pat []
              val p = Regex.regexecBool re []
          in add (mid,p,f) end
   in {clear = clear,
         add = add',
         del = del,
       watch = watch}
   end

fun mkFilter (pfn : 'a -> 'b option)
             {add : 'b -> unit,
              addentries : 'b list -> unit,
              entries,
              clear} =
   let fun add' s = Option.app add (pfn s)
       fun addentries' l = 
             let fun iter a [] = rev a
                   | iter a (l::ls) = 
                        let val a' = 
                               case pfn l
                                 of NONE => a
                                  | SOME v => v::a
                        in iter a' ls
                        end
             in addentries (iter [] l)
             end
   in {     add = add',
     addentries = addentries',
        entries = entries,
          clear = clear }
   end

fun mkBuffer () =
   let val lr : 'a list ref = ref []
       fun entries () = !lr
       fun clear () = lr := []
       fun add s = lr := (s::(!lr))
       fun addentries l = 
             let fun iter a [] = a
                   | iter a (l::ls) = iter (l::a) ls
             in lr := (iter (!lr) (rev l))
             end
   in { entries = entries,
          clear = clear,
            add = add,
     addentries = addentries }
   end

datatype argtype =
    INT of int
  | STR of string
  | RECD of argtype list

datatype typet =
    INTt
  | STRt
  | RECDt of typet list

fun typecheck (INTt, INT _) = true
  | typecheck (STRt, STR _) = true
  | typecheck (RECDt [], RECD []) = true
  | typecheck (RECDt (h::t), RECD (h'::t')) =
       typecheck (h,h') andalso
       typecheck (RECDt t,RECD t')
  | typecheck _ = false

(*
   val test = typecheck (RECDt [INTt,RECDt [STRt,INTt]],RECD [INT 1,RECD [STR "a",INT 2]])
*)

fun mkMachine () =
   let val states = Polyhash.mkPolyTable (10,Subscript);
       fun add st = Polyhash.insert states st
       fun state id = Polyhash.find states id
   in { add = add, state = state }
   end

fun mkState () =
   let val events = Polyhash.mkPolyTable (10,Subscript);
       fun add ev = Polyhash.insert events ev
       fun event id = Polyhash.find events id
   in { add = add, event = event }
   end

fun mkStateMachine states =
   let val mc = mkMachine ()
       fun addstate (id : string,l) = 
           let val st = mkState ()
               val _ = List.app 
                         (fn (id : string,m,a) => (#add st) (id,(m,a)))
                         l
           in (#add mc) (id,st)
           end
       val _ = List.app addstate states
   in mc
   end

fun mkProcess
   {add :
     string *
     {add :
        string *
        ((argtype list -> bool) *
         (string * string * string * argtype list -> string)) -> unit,
      event :
        string ->
        (argtype list -> bool) *
        (string * string * string * argtype list -> string)} -> unit,
   state :
     string ->
     {add :
        string *
        ((argtype list -> bool) *
         (string * string * string * argtype list -> string)) -> unit,
      event :
        string ->
        (argtype list -> bool) *
        (string * string * string * argtype list -> string)}}
         (pid : string) (sid : string) =
   let val sidr = ref sid
       fun procid () = pid
       fun stateid () = !sidr
       fun event (pid' : string, id : string, args : argtype list) =
            let val state = state (!sidr)
                val (match, action) = (#event state) id
                                        handle Subscript =>
                                          (fn _ => false,
                                           fn _ => !sidr)
                val _ = dprint ("mkProcess.event: pid="^pid^" pid'="^pid'^" id="^id^"\n")
            in sidr := (if pid' = pid andalso match args
                          then action (pid, !sidr, id, args)
                          else !sidr)
            end
   in { event = event, stateid = stateid, procid = procid }
   end 

fun mkEvent (id : string) ty 
            (fnc : string * string * string * (argtype list) -> string) =
   (id, fn l => typecheck (RECDt ty, RECD l), fnc)

fun mkDispatcher () =
   let val procs = Polyhash.mkPolyTable (10,Subscript)
       fun add (pr : string * {event : string * string * argtype list -> unit,
                               procid : unit -> string,
                               stateid : unit -> string}) = Polyhash.insert procs pr
       fun del id = Polyhash.remove procs id
       fun proc id = Polyhash.find procs id
       fun dispatch ev = Polyhash.apply (fn (pid,p) => (#event p) ev) procs
   in { add = add, del = del, proc = proc, dispatch = dispatch }
   end

fun mkClock (pid : string) (id : string) ms n act =
   let val msr = ref ms
       val nr = ref (SOME n)
       fun reset (now) = (Time.+ (now,Time.fromMilliseconds (!msr)))
       val expr = ref (reset(Time.now()))
       fun timeout ms = msr := ms
       fun expire () = (!expr)
       fun shot () = (!nr)
       fun trigger () =
           let val now = Time.now()
           in if not (Time.< (now,!expr))
                 then let val _ = dprint ("mkClock.trigger: pid="^pid^" clock id="^id^"\n")
                      in
                      case !nr
                        of SOME 0 => (act(pid,id,0,Time.-(now,!expr));
                                      expr := (reset(!expr)))
                         | SOME 1 => (act(pid,id,1,Time.-(now,!expr));
                                      nr := NONE;
                                      expr := (reset(!expr)))
                         | SOME n => (act(pid,id,n,Time.-(now,!expr));
                                      expr := (reset(!expr));
                                      nr := (SOME (n-1)))
                         | NONE => ()
                      end
                 else ()
           end
   in { expire = expire,
       timeout = timeout,
          shot = shot,
       trigger = trigger }
   end

fun mkTimer () =
   let val clocks = Polyhash.mkPolyTable (10,Subscript)
       fun add (cr as (id,_) : string * {expire : unit -> Time.time,
                                         shot : unit -> int option,
                                         timeout : int -> unit,
                                         trigger : unit -> unit}) =
                    let val _ = dprint ("mkTimer.add: clock id="^id^"\n")
                    in Polyhash.insert clocks cr end
       fun del id = let val _ = dprint ("mkTimer.del: clock id="^id^"\n")
                    in Polyhash.remove clocks id end
       fun clock id = Polyhash.find clocks id
       fun process (id,c) =
                    let val _ = dprint ("mkTimer.process: clock id="^id^"\n")
                    in case (#shot c) ()
                         of NONE => ignore (del id)
                          | _ => (#trigger c) ()
                    end
       fun dispatch () = Polyhash.apply process clocks
   in { add = add, del = del, clock = clock, dispatch = dispatch }
   end

fun tstamp s = SOME (Time.fmt 9 (Time.now()),s);

(*
open Socket;

 val fdout = fdopen ("/tmp/test.out", [O_WRONLY, O_CREAT, O_TEXT], S_IRALL + S_IWALL);
 val fds = FDSet.fdset ();
 val _ = FDSet.fd_set(fdout,fds);
 val res = FDSet.fd_isset(fdout,fds);
 val _ = FDSet.fd_clr(fdout,fds);
 val res' = FDSet.fd_isset(fdout,fds);
 val _ = FDSet.fd_set(fdout,fds);
 val res'' = FDSet.fd_isset(fdout,fds);
 val _ = FDSet.fd_zero(fds);
 val res''' = FDSet.fd_isset(fdout,fds);

   val nw = writeStr fdout "Hello, World\r\nlogin: \r\npassword: \r\nshell $"; 
   val 0 = fdclose (fdout);
 *)

fun lastcis (s,p) =
   let val n = String.size s - 1
   in if n >= 0
         then p(String.sub(s,n))
         else false
   end

fun maybeaddcr s =
  s^(if lastcis (s,fn c => c = #"\n") then "" else "\n");

    val lbuf = mkFilter (tstamp : string -> (string * string) option) (mkBuffer ());
    val lbuf = mkFilter (fn s => (print (maybeaddcr("rsh1: >> "^s));SOME s)) lbuf;
    val matches = mkReMatcher ();
    val lbuflog = mkFilter (tstamp : string -> (string * string) option) (mkBuffer ());
    val lbuflog = mkFilter (fn s => (print (maybeaddcr("log1: >> "^s));SOME s)) lbuflog;
    val logmatches = mkReMatcher ();
    val fdin = fdopen ("/home/ian3/.cua01.1.in", [O_WRONLY, O_TEXT], 0);
    val fdout = fdopen ("/home/ian3/.cua01.1.out", [O_RDONLY, O_TEXT], 0);
    val is = mkInputStream "shell1.in" fdout 1024 CRLF;
    val fdoutlog = fdopen ("/home/ian3/.cua00.1.out", [O_RDONLY, O_BINARY], 0);
    val islog = mkInputStream "log1.in" fdoutlog 1024 CRLF;
    val timer = mkTimer ();
    val disp1 = mkDispatcher ();
    val dispatch = (#dispatch disp1);
    val d = ref false;
    val () = (#clear lbuf)();
    val () = (#clear matches)();
    val () = (#add matches) ("all", ".*", (fn (_,_,s) => (#add lbuf) s));
    val () = (#add logmatches) ("all", ".*", (fn (_,_,s) => (#add lbuflog) s));
    val clk1 = fn pid => ("c1", (mkClock pid "c1" 200 1
                                   (fn (pid,id,n,ov) => dispatch (pid, "flush",[STR id]))))
    val clk2 = fn pid => ("c2", (mkClock pid "c2" 200 1
                                   (fn (pid,id,n,ov) => dispatch (pid, "update",[STR id]))))
    val clk3 = fn pid => ("c3", (mkClock pid "c3" 200 1
                                   (fn (pid,id,n,ov) => dispatch (pid, "update",[STR id]))))
    val reader = [("read", [mkEvent "update" [STRt]
                            (fn (p,s,e,[STR id]) =>
                                let val nr = (#update is) ()
                                    val _= dprint ("read.update: read "^(Int.toString nr)^" bytes\n")
                                in if nr = 0
                                      then ((#add timer) (clk1 p); "delay")
                                      else ((#add timer) (clk2 p); "read")
                                end
                              | _ => raise Fail "read.update type error.")]),
                  ("delay", [mkEvent "flush" [STRt]
                            (fn (p,s,e,[STR id]) =>
                                 let val _= dprint ("delay.flush\n")
                                 in (#flush is) ();
                                    (#add timer) (clk2 p);
                                    "read"
                                 end
                              | _ => raise Fail "read.delay type error.")])];
    val login = [("login", [mkEvent "login-prompt" [STRt]
                            (fn (p,s,e,[STR arg]) =>
                                 let val _= dprint ("login.login-prompt: "^p^"."^s^"\n")
                                 in writeStr fdin "root\n";
                                    "passwd"
                                 end
                              | _ => raise Fail "read.ev1 type error.")]),
                 ("passwd", [mkEvent "password-prompt" [STRt]
                            (fn (p,s,e,[STR arg]) =>
                                 let val _= dprint ("passwd.password-prompt: "^p^"."^s^"\n")
                                 in writeStr fdin "w41rpw4\n";
                                    "shell"
                                 end
                              | _ => raise Fail "read.ev1 type error.")]),
                 ("shell", [mkEvent "shell-prompt" [STRt]
                            (fn (p,s,e,[STR arg]) => 
                                 let val _= dprint ("shell.shell-prompt: "^p^"."^s^"\n")
                                 in "shell"
                                 end
                              | _ => raise Fail "read.ev1 type error.")])];
    val _ = (#add disp1) ("shell1.out", mkProcess (mkStateMachine reader) "shell1.out" "read");
    val _ = (#add disp1) ("shell1.in", mkProcess (mkStateMachine login) "shell1.in" "login");
    val logreader = [("read", [mkEvent "update" [STRt]
                            (fn (p,s,e,[STR id]) =>
                                let val nr = (#update islog) ()
                                    val _= dprint ("read.update: read "^(Int.toString nr)^" bytes\n")
                                in ((#add timer) (clk3 p); "read")
                                end
                              | _ => raise Fail "read.update type error.")])];
    val _ = (#add disp1) ("log1.out", mkProcess (mkStateMachine logreader) "log1.out" "read");
    fun watchev (pid,mid,s) = dispatch (pid,mid,[STR s]);
    val () = (#add matches) ("login-prompt", "^login: ", watchev);
    val () = (#add matches) ("password-prompt", "^Password:", watchev);
    val () = (#add matches) ("shell-prompt", "^# ", watchev);
    val () = (#watch is) (#watch matches);
    val () = (#watch islog) (#watch logmatches);
    val _ = dispatch ("shell1.out","update",[STR "ck0"]);
    val _ = dispatch ("log1.out","update",[STR "ck0"]);
    val slp = (Time.fromMilliseconds 50); 
    val _ = while not (!d)
              do (Process.sleep slp;
                  dprint "loop: woke up\n";
                  (#dispatch timer) ());
    val res = rev ((#entries lbuf) ());
    val 0 = fdclose (fdin);
    val 0 = fdclose (fdout);
    val 0 = fdclose (fdoutlog);

fun pause ms = Process.sleep (Time.fromMilliseconds ms);

fun shellescstr s =
   let val cs = String.explode s
   in List.foldl
        (fn (c,l) => 
           case c
             of #"\\" => l^"\\\\"
              | #"\"" => l^"\\\""
              | #"\n" => l^"\\n"
              | c => l^(String.str c))
        "" cs
   end

fun logcmd t m =
  let val t = if t = "" then "" else " -t \""^(shellescstr t)^"\""
  in "logger -p local7.info"^t^" \""^(shellescstr m)^"\"" 
  end
