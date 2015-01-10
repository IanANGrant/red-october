structure Fcntl :> Fcntl =
struct
   datatype flags =
     O_APPEND
   | O_BINARY
   | O_CREAT
   | O_EXCL
   | O_RDONLY
   | O_RDWR
   | O_TEXT
   | O_TRUNC
   | O_WRONLY
  datatype mode =
     S_IRWXU
   | S_IRUSR
   | S_IWUSR
   | S_IXUSR
   | S_IRWXG
   | S_IRGRP
   | S_IWGRP
   | S_IXGRP
   | S_IRWXO
   | S_IROTH
   | S_IWOTH
   | S_IXOTH
   local
      val dlih = Dynlib.dlopen {lib = "libcinfo.so",
                       flag = Dynlib.RTLD_LAZY,
                       global = false }
      fun syminf s = Dynlib.dlsym dlih s;
      val get_mode = 
           Dynlib.app1
              (syminf "fcntl_mode")
                 : unit -> (word * word * word * word * word * word *
                            word * word * word * word * word * word)
      val get_flags = 
           Dynlib.app1
              (syminf "fcntl_flags")
                 : unit -> (word * word * word * word * 
                            word * word * word * word * word)
      val (O_APPEND_val,
           O_BINARY_val,
           O_CREAT_val,
           O_EXCL_val,
           O_RDONLY_val,
           O_RDWR_val,
           O_TEXT_val,
           O_TRUNC_val,
           O_WRONLY_val) = get_flags ()
      val (S_IRWXU_val,
           S_IRUSR_val,
           S_IWUSR_val,
           S_IXUSR_val,
           S_IRWXG_val,
           S_IRGRP_val,
           S_IWGRP_val,
           S_IXGRP_val,
           S_IRWXO_val,
           S_IROTH_val,
           S_IWOTH_val,
           S_IXOTH_val) = get_mode ()
      val flagvals =
             [(O_APPEND_val,O_APPEND),
              (O_BINARY_val,O_BINARY),
              (O_CREAT_val,O_CREAT),
              (O_EXCL_val,O_EXCL),
              (O_RDONLY_val,O_RDONLY),
              (O_RDWR_val,O_RDWR),
              (O_TEXT_val,O_TEXT),
              (O_TRUNC_val,O_TRUNC),
              (O_WRONLY_val,O_WRONLY)]
      val modevals =
             [(S_IRWXU_val,S_IRWXU),
              (S_IRUSR_val,S_IRUSR),
              (S_IWUSR_val,S_IWUSR),
              (S_IXUSR_val,S_IXUSR),
              (S_IRWXG_val,S_IRWXG),
              (S_IRGRP_val,S_IRGRP),
              (S_IWGRP_val,S_IWGRP),
              (S_IXGRP_val,S_IXGRP),
              (S_IRWXO_val,S_IRWXO),
              (S_IROTH_val,S_IROTH),
              (S_IWOTH_val,S_IWOTH),
              (S_IXOTH_val,S_IXOTH)]
      fun lookup_ vs =
          Option.valOf o
            (fn w => 
               List.foldl
                  (fn (_,acc as (SOME n)) => acc
                    | ((n,w'),_) =>
                        if w' = w
                           then SOME n
                           else NONE) NONE vs)
      val flagWord = lookup_ flagvals
      val modeWord = lookup_ modevals
      fun toWord conv =
         let fun iter w [] = w
               | iter w (f::fs) =
                   iter (Word.orb(conv f,w)) fs
         in iter 0w0
         end
   in
      val flagsWord = toWord flagWord
      val modesWord = toWord modeWord
   end
end

(*
val _ = load "Fcntl";

Fcntl.modesWord[Fcntl.S_IRWXU];
Fcntl.flagsWord[Fcntl.O_CREAT,Fcntl.O_RDONLY];
*)
