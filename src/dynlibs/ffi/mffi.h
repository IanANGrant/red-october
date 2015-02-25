#include <string.h>
#include <stdlib.h>

/* The GNU lightning headers. */
#include <lightning.h>

/* The GNU ffi headers. */
#include <ffi.h>
#include <ffitarget.h>

/* Access to the camlrunm/Moscow ML runtime data representation: */

#include "mlvalues.h"
#include "fail.h"
#include "memory.h"
#include "alloc.h"
#include "callback.h"
#include "globals.h"
#include "interp.h"		/* For callback                            */
#include "str.h"		/* For string_length                       */

/* At most 50 Mb of FFI garbage to collect .... */
#define MAX_FFI_ALLOC 50000000

#define FFI_MEM_WITH_REALLOC
