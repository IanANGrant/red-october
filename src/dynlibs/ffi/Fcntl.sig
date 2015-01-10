signature Fcntl =
sig
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

   val flagsWord : flags list -> Word.word
   val modesWord : mode list -> Word.word
end
