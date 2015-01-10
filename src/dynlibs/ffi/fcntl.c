#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>

#include "mosml/mlvalues.h"
#include "mosml/alloc.h"
#include "mosml/memory.h"
#include "mosml/fail.h"
#include "mosml/str.h"

#ifndef O_BINARY
#define O_BINARY 0
#endif
#ifndef O_TEXT
#define O_TEXT 0
#endif

value fcntl_mode(value unit) {
  value res = alloc_tuple(12);
     Field(res, 0) = Val_long(S_IRWXU);
     Field(res, 1) = Val_long(S_IRUSR);
     Field(res, 2) = Val_long(S_IWUSR);
     Field(res, 3) = Val_long(S_IXUSR);
     Field(res, 4) = Val_long(S_IRWXG);
     Field(res, 5) = Val_long(S_IRGRP);
     Field(res, 6) = Val_long(S_IWGRP);
     Field(res, 7) = Val_long(S_IXGRP);
     Field(res, 8) = Val_long(S_IRWXO);
     Field(res, 9) = Val_long(S_IROTH);
     Field(res, 10) = Val_long(S_IWOTH);
     Field(res, 11) = Val_long(S_IXOTH);
  return res;
}

value fcntl_flags(value unit) {
  value res = alloc_tuple(9);
     Field(res, 0) = Val_long(O_APPEND);
     Field(res, 1) = Val_long(O_BINARY);
     Field(res, 2) = Val_long(O_CREAT);
     Field(res, 3) = Val_long(O_EXCL);
     Field(res, 4) = Val_long(O_RDONLY);
     Field(res, 5) = Val_long(O_RDWR);
     Field(res, 6) = Val_long(O_TEXT);
     Field(res, 7) = Val_long(O_TRUNC);
     Field(res, 8) = Val_long(O_WRONLY);
  return res;
}

