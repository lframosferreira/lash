######################################################################
##                                                                  ##
##  LASH package -- v0.92                                           ##
##  ============                                                    ##
##                                                                  ##
##     Makefile.                                                    ##
##                                                                  ##
##     03/13/98  :  Creation. (BB)                                  ##
##     06/09/98  :  Minor corrections. (BB)                         ##
##     07/27/98  :  Addition of 'auto-io'. (BB)                     ##
##     07/28/98  :  Addition of 'auto-basic'. (BB)                  ##
##     08/05/98  :  Addition of 'datastruct'. (BB)                  ##
##     09/04/98  :  Reorganization. (BB)                            ##
##     09/18/98  :  Basic operations. (BB)                          ##
##     09/22/98  :  NDD package. (BB)                               ##
##     09/28/98  :  Reorganization. (BB)                            ##
##     09/29/98  :  New module. (BB)                                ##
##     10/06/98  :  New module. (BB)                                ##
##     10/27/98  :  New module. (BB)                                ##
##     01/05/99  :  New modules. (BB)                               ##
##     01/15/99  :  New module. (BB)                                ##
##     01/20/99  :  New module. (BB)                                ##
##     02/18/99  :  Compilation flag '-ansi'. (BB)                  ##
##     02/22/99  :  New module. (BB)                                ##
##     08/01/00  :  New module. (JMF)                               ##
##     12/10/00  :  New module. (JMF)                               ##
##     18/10/00  :  New module. (LL) (count & biguint)              ##
##     02/24/01  :  Reorganization. (BB)                            ##
##     05/15/01  :  Removed '-Wtraditional' flag. (BB)              ##
##     08/14/01  :  New source tree. (BB)                           ##
##     09/26/01  :  Reorganization of 'auto-serialize.c'. (SJ)      ##
##     06/28/02  :  New module. (SJ)                                ##
##     07/15/02  :  Reorganization. (BB)                            ##
##                                                                  ##
##  Copyright (c) 1998-2002, Universite de Liege (ULg). All rights  ##
##  reserved. This package is provided for evaluation purposes and  ##
##  educational use only. No guarantee is expressed or implied by   ## 
##  the distribution of this software. The commercial use and the   ##
##  redistribution of this product or of any part of it are not     ##
##  permitted without our prior, express, and written consent.      ##
##                                                                  ##
######################################################################

CC = 		gcc

LASH-LIB-DIR =	../../lib/

SRCS-CORE =	lash.c \
		diag.c \
		resource.c \
		datastruct.c \
		biguint.c \
		arithmetic.c \
		sort.c \
		datastruct-io.c \
		graph-scc.c \
		io-ops.c \
		auto.c \
		auto-io.c \
		auto-io-print.c \
		auto-io-dots.c \
		auto-determinize.c \
		auto-minimize.c \
		auto-product.c \
		auto-sequential.c \
		auto-difference.c \
		auto-normalize.c \
		auto-quotient.c \
		auto-operations.c \
		auto-count.c \
		auto-serialize.c \
		auto-weak-convert.c \
		auto-depth.c \
		auto-sort.c

SRCS-ARITH =	cyclotomic.c \
		matrix.c

OBJS-CORE =	${SRCS-CORE:.c=.o}

OBJS-ARITH =	${SRCS-ARITH:.c=.o}

CFLAGS =   -c -g -O3 -ansi -Wall -Wpointer-arith \
         -Waggregate-return -Wstrict-prototypes \
         -Wmissing-prototypes -Wmissing-declarations

all:  lash-core lash-arith

lash-core: $(OBJS-CORE)
	ld -r -o lash-core.o $(OBJS-CORE)
	mv lash-core.o $(LASH-LIB-DIR)

lash-arith: $(OBJS-ARITH)
	ld -r -o lash-arith.o $(OBJS-ARITH) 
	mv lash-arith.o $(LASH-LIB-DIR)

clean:
	-rm -f $(OBJS-CORE) $(OBJS-ARITH) lash-core.o lash-arith.o
	-rm -f $(LASH-LIB-DIR)/lash-core.o
	-rm -f $(LASH-LIB-DIR)/lash-arith.o

depend: 
	makedepend $(SRCS-CORE) $(SRCS-ARITH)

# DO NOT DELETE THIS LINE -- make depend depends on it.

lash.o: lash.h lash-types.h diag.h lash-diag.h
diag.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
diag.o: /usr/include/features.h /usr/include/features-time64.h
diag.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
diag.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
diag.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
diag.o: /usr/include/stddef.h /usr/include/stdarg.h /usr/include/bits/types.h
diag.o: /usr/include/bits/typesizes.h /usr/include/bits/time64.h
diag.o: /usr/include/bits/types/__fpos_t.h
diag.o: /usr/include/bits/types/__mbstate_t.h
diag.o: /usr/include/bits/types/__fpos64_t.h /usr/include/bits/types/__FILE.h
diag.o: /usr/include/bits/types/FILE.h /usr/include/bits/types/struct_FILE.h
diag.o: /usr/include/bits/types/cookie_io_functions_t.h
diag.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
diag.o: /usr/include/bits/floatn-common.h diag.h lash-diag.h lash-types.h
diag.o: lash.h
resource.o: /usr/include/stdlib.h /usr/include/bits/libc-header-start.h
resource.o: /usr/include/features.h /usr/include/features-time64.h
resource.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
resource.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
resource.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
resource.o: /usr/include/stddef.h /usr/include/bits/waitflags.h
resource.o: /usr/include/bits/waitstatus.h /usr/include/bits/floatn.h
resource.o: /usr/include/bits/floatn-common.h /usr/include/sys/types.h
resource.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
resource.o: /usr/include/bits/time64.h /usr/include/bits/types/clock_t.h
resource.o: /usr/include/bits/types/clockid_t.h
resource.o: /usr/include/bits/types/time_t.h
resource.o: /usr/include/bits/types/timer_t.h /usr/include/bits/stdint-intn.h
resource.o: /usr/include/endian.h /usr/include/bits/endian.h
resource.o: /usr/include/bits/endianness.h /usr/include/bits/byteswap.h
resource.o: /usr/include/bits/uintn-identity.h /usr/include/sys/select.h
resource.o: /usr/include/bits/select.h /usr/include/bits/types/sigset_t.h
resource.o: /usr/include/bits/types/__sigset_t.h
resource.o: /usr/include/bits/types/struct_timeval.h
resource.o: /usr/include/bits/types/struct_timespec.h
resource.o: /usr/include/bits/pthreadtypes.h
resource.o: /usr/include/bits/thread-shared-types.h
resource.o: /usr/include/bits/pthreadtypes-arch.h
resource.o: /usr/include/bits/atomic_wide_counter.h
resource.o: /usr/include/bits/struct_mutex.h
resource.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
resource.o: /usr/include/bits/stdlib-float.h /usr/include/string.h
resource.o: /usr/include/bits/types/locale_t.h
resource.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
resource.o: resource.h lash-types.h lash.h diag.h lash-diag.h
datastruct.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
datastruct.o: /usr/include/features.h /usr/include/features-time64.h
datastruct.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
datastruct.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
datastruct.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
datastruct.o: /usr/include/stddef.h /usr/include/stdarg.h
datastruct.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
datastruct.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
datastruct.o: /usr/include/bits/types/__mbstate_t.h
datastruct.o: /usr/include/bits/types/__fpos64_t.h
datastruct.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
datastruct.o: /usr/include/bits/types/struct_FILE.h
datastruct.o: /usr/include/bits/types/cookie_io_functions_t.h
datastruct.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
datastruct.o: /usr/include/bits/floatn-common.h /usr/include/string.h
datastruct.o: /usr/include/bits/types/locale_t.h
datastruct.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
datastruct.o: /usr/include/stdlib.h /usr/include/bits/waitflags.h
datastruct.o: /usr/include/bits/waitstatus.h /usr/include/sys/types.h
datastruct.o: /usr/include/bits/types/clock_t.h
datastruct.o: /usr/include/bits/types/clockid_t.h
datastruct.o: /usr/include/bits/types/time_t.h
datastruct.o: /usr/include/bits/types/timer_t.h
datastruct.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
datastruct.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
datastruct.o: /usr/include/bits/byteswap.h /usr/include/bits/uintn-identity.h
datastruct.o: /usr/include/sys/select.h /usr/include/bits/select.h
datastruct.o: /usr/include/bits/types/sigset_t.h
datastruct.o: /usr/include/bits/types/__sigset_t.h
datastruct.o: /usr/include/bits/types/struct_timeval.h
datastruct.o: /usr/include/bits/types/struct_timespec.h
datastruct.o: /usr/include/bits/pthreadtypes.h
datastruct.o: /usr/include/bits/thread-shared-types.h
datastruct.o: /usr/include/bits/pthreadtypes-arch.h
datastruct.o: /usr/include/bits/atomic_wide_counter.h
datastruct.o: /usr/include/bits/struct_mutex.h
datastruct.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
datastruct.o: /usr/include/bits/stdlib-float.h resource.h lash-types.h lash.h
datastruct.o: sort.h datastruct.h
biguint.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
biguint.o: /usr/include/features.h /usr/include/features-time64.h
biguint.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
biguint.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
biguint.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
biguint.o: /usr/include/stddef.h /usr/include/stdarg.h
biguint.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
biguint.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
biguint.o: /usr/include/bits/types/__mbstate_t.h
biguint.o: /usr/include/bits/types/__fpos64_t.h
biguint.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
biguint.o: /usr/include/bits/types/struct_FILE.h
biguint.o: /usr/include/bits/types/cookie_io_functions_t.h
biguint.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
biguint.o: /usr/include/bits/floatn-common.h /usr/include/string.h
biguint.o: /usr/include/bits/types/locale_t.h
biguint.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
biguint.o: biguint.h lash-types.h lash.h arithmetic.h resource.h diag.h
biguint.o: lash-diag.h
arithmetic.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
arithmetic.o: /usr/include/features.h /usr/include/features-time64.h
arithmetic.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
arithmetic.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
arithmetic.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
arithmetic.o: /usr/include/stddef.h /usr/include/stdarg.h
arithmetic.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
arithmetic.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
arithmetic.o: /usr/include/bits/types/__mbstate_t.h
arithmetic.o: /usr/include/bits/types/__fpos64_t.h
arithmetic.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
arithmetic.o: /usr/include/bits/types/struct_FILE.h
arithmetic.o: /usr/include/bits/types/cookie_io_functions_t.h
arithmetic.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
arithmetic.o: /usr/include/bits/floatn-common.h /usr/include/string.h
arithmetic.o: /usr/include/bits/types/locale_t.h
arithmetic.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
arithmetic.o: lash-diag.h lash-types.h lash.h resource.h arithmetic.h
sort.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
sort.o: /usr/include/features.h /usr/include/features-time64.h
sort.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
sort.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
sort.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
sort.o: /usr/include/stddef.h /usr/include/stdarg.h /usr/include/bits/types.h
sort.o: /usr/include/bits/typesizes.h /usr/include/bits/time64.h
sort.o: /usr/include/bits/types/__fpos_t.h
sort.o: /usr/include/bits/types/__mbstate_t.h
sort.o: /usr/include/bits/types/__fpos64_t.h /usr/include/bits/types/__FILE.h
sort.o: /usr/include/bits/types/FILE.h /usr/include/bits/types/struct_FILE.h
sort.o: /usr/include/bits/types/cookie_io_functions_t.h
sort.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
sort.o: /usr/include/bits/floatn-common.h /usr/include/string.h
sort.o: /usr/include/bits/types/locale_t.h
sort.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
sort.o: /usr/include/stdlib.h /usr/include/bits/waitflags.h
sort.o: /usr/include/bits/waitstatus.h /usr/include/sys/types.h
sort.o: /usr/include/bits/types/clock_t.h /usr/include/bits/types/clockid_t.h
sort.o: /usr/include/bits/types/time_t.h /usr/include/bits/types/timer_t.h
sort.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
sort.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
sort.o: /usr/include/bits/byteswap.h /usr/include/bits/uintn-identity.h
sort.o: /usr/include/sys/select.h /usr/include/bits/select.h
sort.o: /usr/include/bits/types/sigset_t.h
sort.o: /usr/include/bits/types/__sigset_t.h
sort.o: /usr/include/bits/types/struct_timeval.h
sort.o: /usr/include/bits/types/struct_timespec.h
sort.o: /usr/include/bits/pthreadtypes.h
sort.o: /usr/include/bits/thread-shared-types.h
sort.o: /usr/include/bits/pthreadtypes-arch.h
sort.o: /usr/include/bits/atomic_wide_counter.h
sort.o: /usr/include/bits/struct_mutex.h /usr/include/bits/struct_rwlock.h
sort.o: /usr/include/alloca.h /usr/include/bits/stdlib-float.h resource.h
sort.o: lash-types.h lash.h datastruct.h sort.h
datastruct-io.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
datastruct-io.o: /usr/include/features.h /usr/include/features-time64.h
datastruct-io.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
datastruct-io.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
datastruct-io.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
datastruct-io.o: /usr/include/stddef.h /usr/include/stdarg.h
datastruct-io.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
datastruct-io.o: /usr/include/bits/time64.h
datastruct-io.o: /usr/include/bits/types/__fpos_t.h
datastruct-io.o: /usr/include/bits/types/__mbstate_t.h
datastruct-io.o: /usr/include/bits/types/__fpos64_t.h
datastruct-io.o: /usr/include/bits/types/__FILE.h
datastruct-io.o: /usr/include/bits/types/FILE.h
datastruct-io.o: /usr/include/bits/types/struct_FILE.h
datastruct-io.o: /usr/include/bits/types/cookie_io_functions_t.h
datastruct-io.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
datastruct-io.o: /usr/include/bits/floatn-common.h /usr/include/string.h
datastruct-io.o: /usr/include/bits/types/locale_t.h
datastruct-io.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
datastruct-io.o: lash-io.h lash-auto.h /usr/include/stdlib.h
datastruct-io.o: /usr/include/bits/waitflags.h /usr/include/bits/waitstatus.h
datastruct-io.o: /usr/include/sys/types.h /usr/include/bits/types/clock_t.h
datastruct-io.o: /usr/include/bits/types/clockid_t.h
datastruct-io.o: /usr/include/bits/types/time_t.h
datastruct-io.o: /usr/include/bits/types/timer_t.h
datastruct-io.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
datastruct-io.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
datastruct-io.o: /usr/include/bits/byteswap.h
datastruct-io.o: /usr/include/bits/uintn-identity.h /usr/include/sys/select.h
datastruct-io.o: /usr/include/bits/select.h
datastruct-io.o: /usr/include/bits/types/sigset_t.h
datastruct-io.o: /usr/include/bits/types/__sigset_t.h
datastruct-io.o: /usr/include/bits/types/struct_timeval.h
datastruct-io.o: /usr/include/bits/types/struct_timespec.h
datastruct-io.o: /usr/include/bits/pthreadtypes.h
datastruct-io.o: /usr/include/bits/thread-shared-types.h
datastruct-io.o: /usr/include/bits/pthreadtypes-arch.h
datastruct-io.o: /usr/include/bits/atomic_wide_counter.h
datastruct-io.o: /usr/include/bits/struct_mutex.h
datastruct-io.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
datastruct-io.o: /usr/include/bits/stdlib-float.h lash-types.h lash.h
datastruct-io.o: datastruct.h datastruct-io.h lash-auto-io.h auto-io-print.h
datastruct-io.o: auto-io-dots.h diag.h lash-diag.h resource.h
graph-scc.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
graph-scc.o: /usr/include/features.h /usr/include/features-time64.h
graph-scc.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
graph-scc.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
graph-scc.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
graph-scc.o: /usr/include/stddef.h /usr/include/stdarg.h
graph-scc.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
graph-scc.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
graph-scc.o: /usr/include/bits/types/__mbstate_t.h
graph-scc.o: /usr/include/bits/types/__fpos64_t.h
graph-scc.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
graph-scc.o: /usr/include/bits/types/struct_FILE.h
graph-scc.o: /usr/include/bits/types/cookie_io_functions_t.h
graph-scc.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
graph-scc.o: /usr/include/bits/floatn-common.h graph-scc.h lash-auto.h
graph-scc.o: /usr/include/stdlib.h /usr/include/bits/waitflags.h
graph-scc.o: /usr/include/bits/waitstatus.h /usr/include/sys/types.h
graph-scc.o: /usr/include/bits/types/clock_t.h
graph-scc.o: /usr/include/bits/types/clockid_t.h
graph-scc.o: /usr/include/bits/types/time_t.h
graph-scc.o: /usr/include/bits/types/timer_t.h
graph-scc.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
graph-scc.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
graph-scc.o: /usr/include/bits/byteswap.h /usr/include/bits/uintn-identity.h
graph-scc.o: /usr/include/sys/select.h /usr/include/bits/select.h
graph-scc.o: /usr/include/bits/types/sigset_t.h
graph-scc.o: /usr/include/bits/types/__sigset_t.h
graph-scc.o: /usr/include/bits/types/struct_timeval.h
graph-scc.o: /usr/include/bits/types/struct_timespec.h
graph-scc.o: /usr/include/bits/pthreadtypes.h
graph-scc.o: /usr/include/bits/thread-shared-types.h
graph-scc.o: /usr/include/bits/pthreadtypes-arch.h
graph-scc.o: /usr/include/bits/atomic_wide_counter.h
graph-scc.o: /usr/include/bits/struct_mutex.h
graph-scc.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
graph-scc.o: /usr/include/bits/stdlib-float.h lash-types.h lash.h
graph-scc.o: datastruct.h /usr/include/string.h
graph-scc.o: /usr/include/bits/types/locale_t.h
graph-scc.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
graph-scc.o: resource.h diag.h lash-diag.h
io-ops.o: /usr/include/stdlib.h /usr/include/bits/libc-header-start.h
io-ops.o: /usr/include/features.h /usr/include/features-time64.h
io-ops.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
io-ops.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
io-ops.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
io-ops.o: /usr/include/stddef.h /usr/include/bits/waitflags.h
io-ops.o: /usr/include/bits/waitstatus.h /usr/include/bits/floatn.h
io-ops.o: /usr/include/bits/floatn-common.h /usr/include/sys/types.h
io-ops.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
io-ops.o: /usr/include/bits/time64.h /usr/include/bits/types/clock_t.h
io-ops.o: /usr/include/bits/types/clockid_t.h
io-ops.o: /usr/include/bits/types/time_t.h /usr/include/bits/types/timer_t.h
io-ops.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
io-ops.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
io-ops.o: /usr/include/bits/byteswap.h /usr/include/bits/uintn-identity.h
io-ops.o: /usr/include/sys/select.h /usr/include/bits/select.h
io-ops.o: /usr/include/bits/types/sigset_t.h
io-ops.o: /usr/include/bits/types/__sigset_t.h
io-ops.o: /usr/include/bits/types/struct_timeval.h
io-ops.o: /usr/include/bits/types/struct_timespec.h
io-ops.o: /usr/include/bits/pthreadtypes.h
io-ops.o: /usr/include/bits/thread-shared-types.h
io-ops.o: /usr/include/bits/pthreadtypes-arch.h
io-ops.o: /usr/include/bits/atomic_wide_counter.h
io-ops.o: /usr/include/bits/struct_mutex.h /usr/include/bits/struct_rwlock.h
io-ops.o: /usr/include/alloca.h /usr/include/bits/stdlib-float.h
io-ops.o: /usr/include/stdio.h /usr/include/stdarg.h
io-ops.o: /usr/include/bits/types/__fpos_t.h
io-ops.o: /usr/include/bits/types/__mbstate_t.h
io-ops.o: /usr/include/bits/types/__fpos64_t.h
io-ops.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
io-ops.o: /usr/include/bits/types/struct_FILE.h
io-ops.o: /usr/include/bits/types/cookie_io_functions_t.h
io-ops.o: /usr/include/bits/stdio_lim.h /usr/include/string.h
io-ops.o: /usr/include/bits/types/locale_t.h
io-ops.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h diag.h
io-ops.o: lash-diag.h lash-types.h lash.h resource.h io-ops.h lash-auto.h
io-ops.o: datastruct.h lash-auto-io.h lash-io.h datastruct-io.h
io-ops.o: auto-io-print.h auto-io-dots.h
auto.o: /usr/include/unistd.h /usr/include/features.h
auto.o: /usr/include/features-time64.h /usr/include/bits/wordsize.h
auto.o: /usr/include/bits/timesize.h /usr/include/stdc-predef.h
auto.o: /usr/include/sys/cdefs.h /usr/include/bits/long-double.h
auto.o: /usr/include/gnu/stubs.h /usr/include/bits/posix_opt.h
auto.o: /usr/include/bits/environments.h /usr/include/bits/types.h
auto.o: /usr/include/bits/typesizes.h /usr/include/bits/time64.h
auto.o: /usr/include/stddef.h /usr/include/bits/confname.h
auto.o: /usr/include/bits/getopt_posix.h /usr/include/bits/getopt_core.h
auto.o: /usr/include/bits/unistd_ext.h /usr/include/stdlib.h
auto.o: /usr/include/bits/libc-header-start.h /usr/include/bits/waitflags.h
auto.o: /usr/include/bits/waitstatus.h /usr/include/bits/floatn.h
auto.o: /usr/include/bits/floatn-common.h /usr/include/sys/types.h
auto.o: /usr/include/bits/types/clock_t.h /usr/include/bits/types/clockid_t.h
auto.o: /usr/include/bits/types/time_t.h /usr/include/bits/types/timer_t.h
auto.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
auto.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
auto.o: /usr/include/bits/byteswap.h /usr/include/bits/uintn-identity.h
auto.o: /usr/include/sys/select.h /usr/include/bits/select.h
auto.o: /usr/include/bits/types/sigset_t.h
auto.o: /usr/include/bits/types/__sigset_t.h
auto.o: /usr/include/bits/types/struct_timeval.h
auto.o: /usr/include/bits/types/struct_timespec.h
auto.o: /usr/include/bits/pthreadtypes.h
auto.o: /usr/include/bits/thread-shared-types.h
auto.o: /usr/include/bits/pthreadtypes-arch.h
auto.o: /usr/include/bits/atomic_wide_counter.h
auto.o: /usr/include/bits/struct_mutex.h /usr/include/bits/struct_rwlock.h
auto.o: /usr/include/alloca.h /usr/include/bits/stdlib-float.h
auto.o: /usr/include/string.h /usr/include/bits/types/locale_t.h
auto.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h auto.h
auto.o: lash-auto.h lash-types.h lash.h datastruct.h diag.h lash-diag.h
auto.o: resource.h sort.h
auto-io.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
auto-io.o: /usr/include/features.h /usr/include/features-time64.h
auto-io.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
auto-io.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
auto-io.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
auto-io.o: /usr/include/stddef.h /usr/include/stdarg.h
auto-io.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
auto-io.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
auto-io.o: /usr/include/bits/types/__mbstate_t.h
auto-io.o: /usr/include/bits/types/__fpos64_t.h
auto-io.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
auto-io.o: /usr/include/bits/types/struct_FILE.h
auto-io.o: /usr/include/bits/types/cookie_io_functions_t.h
auto-io.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
auto-io.o: /usr/include/bits/floatn-common.h datastruct-io.h lash-auto.h
auto-io.o: /usr/include/stdlib.h /usr/include/bits/waitflags.h
auto-io.o: /usr/include/bits/waitstatus.h /usr/include/sys/types.h
auto-io.o: /usr/include/bits/types/clock_t.h
auto-io.o: /usr/include/bits/types/clockid_t.h
auto-io.o: /usr/include/bits/types/time_t.h /usr/include/bits/types/timer_t.h
auto-io.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
auto-io.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
auto-io.o: /usr/include/bits/byteswap.h /usr/include/bits/uintn-identity.h
auto-io.o: /usr/include/sys/select.h /usr/include/bits/select.h
auto-io.o: /usr/include/bits/types/sigset_t.h
auto-io.o: /usr/include/bits/types/__sigset_t.h
auto-io.o: /usr/include/bits/types/struct_timeval.h
auto-io.o: /usr/include/bits/types/struct_timespec.h
auto-io.o: /usr/include/bits/pthreadtypes.h
auto-io.o: /usr/include/bits/thread-shared-types.h
auto-io.o: /usr/include/bits/pthreadtypes-arch.h
auto-io.o: /usr/include/bits/atomic_wide_counter.h
auto-io.o: /usr/include/bits/struct_mutex.h /usr/include/bits/struct_rwlock.h
auto-io.o: /usr/include/alloca.h /usr/include/bits/stdlib-float.h
auto-io.o: lash-types.h lash.h datastruct.h /usr/include/string.h
auto-io.o: /usr/include/bits/types/locale_t.h
auto-io.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
auto-io.o: lash-io.h lash-auto-io.h auto-io-print.h auto-io-dots.h auto.h
auto-io.o: io-ops.h diag.h lash-diag.h resource.h auto-io.h
auto-io-print.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
auto-io-print.o: /usr/include/features.h /usr/include/features-time64.h
auto-io-print.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
auto-io-print.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
auto-io-print.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
auto-io-print.o: /usr/include/stddef.h /usr/include/stdarg.h
auto-io-print.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
auto-io-print.o: /usr/include/bits/time64.h
auto-io-print.o: /usr/include/bits/types/__fpos_t.h
auto-io-print.o: /usr/include/bits/types/__mbstate_t.h
auto-io-print.o: /usr/include/bits/types/__fpos64_t.h
auto-io-print.o: /usr/include/bits/types/__FILE.h
auto-io-print.o: /usr/include/bits/types/FILE.h
auto-io-print.o: /usr/include/bits/types/struct_FILE.h
auto-io-print.o: /usr/include/bits/types/cookie_io_functions_t.h
auto-io-print.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
auto-io-print.o: /usr/include/bits/floatn-common.h diag.h lash-diag.h
auto-io-print.o: lash-types.h lash.h auto.h lash-auto.h /usr/include/stdlib.h
auto-io-print.o: /usr/include/bits/waitflags.h /usr/include/bits/waitstatus.h
auto-io-print.o: /usr/include/sys/types.h /usr/include/bits/types/clock_t.h
auto-io-print.o: /usr/include/bits/types/clockid_t.h
auto-io-print.o: /usr/include/bits/types/time_t.h
auto-io-print.o: /usr/include/bits/types/timer_t.h
auto-io-print.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
auto-io-print.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
auto-io-print.o: /usr/include/bits/byteswap.h
auto-io-print.o: /usr/include/bits/uintn-identity.h /usr/include/sys/select.h
auto-io-print.o: /usr/include/bits/select.h
auto-io-print.o: /usr/include/bits/types/sigset_t.h
auto-io-print.o: /usr/include/bits/types/__sigset_t.h
auto-io-print.o: /usr/include/bits/types/struct_timeval.h
auto-io-print.o: /usr/include/bits/types/struct_timespec.h
auto-io-print.o: /usr/include/bits/pthreadtypes.h
auto-io-print.o: /usr/include/bits/thread-shared-types.h
auto-io-print.o: /usr/include/bits/pthreadtypes-arch.h
auto-io-print.o: /usr/include/bits/atomic_wide_counter.h
auto-io-print.o: /usr/include/bits/struct_mutex.h
auto-io-print.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
auto-io-print.o: /usr/include/bits/stdlib-float.h datastruct.h
auto-io-print.o: /usr/include/string.h /usr/include/bits/types/locale_t.h
auto-io-print.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
auto-io-print.o: auto-io-print.h
auto-io-dots.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
auto-io-dots.o: /usr/include/features.h /usr/include/features-time64.h
auto-io-dots.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
auto-io-dots.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
auto-io-dots.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
auto-io-dots.o: /usr/include/stddef.h /usr/include/stdarg.h
auto-io-dots.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
auto-io-dots.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
auto-io-dots.o: /usr/include/bits/types/__mbstate_t.h
auto-io-dots.o: /usr/include/bits/types/__fpos64_t.h
auto-io-dots.o: /usr/include/bits/types/__FILE.h
auto-io-dots.o: /usr/include/bits/types/FILE.h
auto-io-dots.o: /usr/include/bits/types/struct_FILE.h
auto-io-dots.o: /usr/include/bits/types/cookie_io_functions_t.h
auto-io-dots.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
auto-io-dots.o: /usr/include/bits/floatn-common.h /usr/include/string.h
auto-io-dots.o: /usr/include/bits/types/locale_t.h
auto-io-dots.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
auto-io-dots.o: lash-auto.h /usr/include/stdlib.h
auto-io-dots.o: /usr/include/bits/waitflags.h /usr/include/bits/waitstatus.h
auto-io-dots.o: /usr/include/sys/types.h /usr/include/bits/types/clock_t.h
auto-io-dots.o: /usr/include/bits/types/clockid_t.h
auto-io-dots.o: /usr/include/bits/types/time_t.h
auto-io-dots.o: /usr/include/bits/types/timer_t.h
auto-io-dots.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
auto-io-dots.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
auto-io-dots.o: /usr/include/bits/byteswap.h
auto-io-dots.o: /usr/include/bits/uintn-identity.h /usr/include/sys/select.h
auto-io-dots.o: /usr/include/bits/select.h /usr/include/bits/types/sigset_t.h
auto-io-dots.o: /usr/include/bits/types/__sigset_t.h
auto-io-dots.o: /usr/include/bits/types/struct_timeval.h
auto-io-dots.o: /usr/include/bits/types/struct_timespec.h
auto-io-dots.o: /usr/include/bits/pthreadtypes.h
auto-io-dots.o: /usr/include/bits/thread-shared-types.h
auto-io-dots.o: /usr/include/bits/pthreadtypes-arch.h
auto-io-dots.o: /usr/include/bits/atomic_wide_counter.h
auto-io-dots.o: /usr/include/bits/struct_mutex.h
auto-io-dots.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
auto-io-dots.o: /usr/include/bits/stdlib-float.h lash-types.h lash.h
auto-io-dots.o: datastruct.h lash-auto-io.h lash-io.h datastruct-io.h
auto-io-dots.o: auto-io-print.h auto-io-dots.h diag.h lash-diag.h resource.h
auto-io-dots.o: io-ops.h
auto-determinize.o: /usr/include/stdio.h
auto-determinize.o: /usr/include/bits/libc-header-start.h
auto-determinize.o: /usr/include/features.h /usr/include/features-time64.h
auto-determinize.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
auto-determinize.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
auto-determinize.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
auto-determinize.o: /usr/include/stddef.h /usr/include/stdarg.h
auto-determinize.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
auto-determinize.o: /usr/include/bits/time64.h
auto-determinize.o: /usr/include/bits/types/__fpos_t.h
auto-determinize.o: /usr/include/bits/types/__mbstate_t.h
auto-determinize.o: /usr/include/bits/types/__fpos64_t.h
auto-determinize.o: /usr/include/bits/types/__FILE.h
auto-determinize.o: /usr/include/bits/types/FILE.h
auto-determinize.o: /usr/include/bits/types/struct_FILE.h
auto-determinize.o: /usr/include/bits/types/cookie_io_functions_t.h
auto-determinize.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
auto-determinize.o: /usr/include/bits/floatn-common.h /usr/include/string.h
auto-determinize.o: /usr/include/bits/types/locale_t.h
auto-determinize.o: /usr/include/bits/types/__locale_t.h
auto-determinize.o: /usr/include/strings.h /usr/include/stdlib.h
auto-determinize.o: /usr/include/bits/waitflags.h
auto-determinize.o: /usr/include/bits/waitstatus.h /usr/include/sys/types.h
auto-determinize.o: /usr/include/bits/types/clock_t.h
auto-determinize.o: /usr/include/bits/types/clockid_t.h
auto-determinize.o: /usr/include/bits/types/time_t.h
auto-determinize.o: /usr/include/bits/types/timer_t.h
auto-determinize.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
auto-determinize.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
auto-determinize.o: /usr/include/bits/byteswap.h
auto-determinize.o: /usr/include/bits/uintn-identity.h
auto-determinize.o: /usr/include/sys/select.h /usr/include/bits/select.h
auto-determinize.o: /usr/include/bits/types/sigset_t.h
auto-determinize.o: /usr/include/bits/types/__sigset_t.h
auto-determinize.o: /usr/include/bits/types/struct_timeval.h
auto-determinize.o: /usr/include/bits/types/struct_timespec.h
auto-determinize.o: /usr/include/bits/pthreadtypes.h
auto-determinize.o: /usr/include/bits/thread-shared-types.h
auto-determinize.o: /usr/include/bits/pthreadtypes-arch.h
auto-determinize.o: /usr/include/bits/atomic_wide_counter.h
auto-determinize.o: /usr/include/bits/struct_mutex.h
auto-determinize.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
auto-determinize.o: /usr/include/bits/stdlib-float.h auto.h lash-auto.h
auto-determinize.o: lash-types.h lash.h datastruct.h auto-determinize.h
auto-determinize.o: lash-auto-operations.h auto-minimize.h auto-product.h
auto-determinize.o: auto-sequential.h auto-difference.h auto-normalize.h
auto-determinize.o: auto-quotient.h auto-count.h biguint.h diag.h lash-diag.h
auto-determinize.o: resource.h graph-scc.h
auto-minimize.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
auto-minimize.o: /usr/include/features.h /usr/include/features-time64.h
auto-minimize.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
auto-minimize.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
auto-minimize.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
auto-minimize.o: /usr/include/stddef.h /usr/include/stdarg.h
auto-minimize.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
auto-minimize.o: /usr/include/bits/time64.h
auto-minimize.o: /usr/include/bits/types/__fpos_t.h
auto-minimize.o: /usr/include/bits/types/__mbstate_t.h
auto-minimize.o: /usr/include/bits/types/__fpos64_t.h
auto-minimize.o: /usr/include/bits/types/__FILE.h
auto-minimize.o: /usr/include/bits/types/FILE.h
auto-minimize.o: /usr/include/bits/types/struct_FILE.h
auto-minimize.o: /usr/include/bits/types/cookie_io_functions_t.h
auto-minimize.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
auto-minimize.o: /usr/include/bits/floatn-common.h /usr/include/string.h
auto-minimize.o: /usr/include/bits/types/locale_t.h
auto-minimize.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
auto-minimize.o: /usr/include/stdlib.h /usr/include/bits/waitflags.h
auto-minimize.o: /usr/include/bits/waitstatus.h /usr/include/sys/types.h
auto-minimize.o: /usr/include/bits/types/clock_t.h
auto-minimize.o: /usr/include/bits/types/clockid_t.h
auto-minimize.o: /usr/include/bits/types/time_t.h
auto-minimize.o: /usr/include/bits/types/timer_t.h
auto-minimize.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
auto-minimize.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
auto-minimize.o: /usr/include/bits/byteswap.h
auto-minimize.o: /usr/include/bits/uintn-identity.h /usr/include/sys/select.h
auto-minimize.o: /usr/include/bits/select.h
auto-minimize.o: /usr/include/bits/types/sigset_t.h
auto-minimize.o: /usr/include/bits/types/__sigset_t.h
auto-minimize.o: /usr/include/bits/types/struct_timeval.h
auto-minimize.o: /usr/include/bits/types/struct_timespec.h
auto-minimize.o: /usr/include/bits/pthreadtypes.h
auto-minimize.o: /usr/include/bits/thread-shared-types.h
auto-minimize.o: /usr/include/bits/pthreadtypes-arch.h
auto-minimize.o: /usr/include/bits/atomic_wide_counter.h
auto-minimize.o: /usr/include/bits/struct_mutex.h
auto-minimize.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
auto-minimize.o: /usr/include/bits/stdlib-float.h auto.h lash-auto.h
auto-minimize.o: lash-types.h lash.h datastruct.h auto-minimize.h
auto-minimize.o: lash-auto-operations.h auto-determinize.h auto-product.h
auto-minimize.o: auto-sequential.h auto-difference.h auto-normalize.h
auto-minimize.o: auto-quotient.h auto-count.h biguint.h diag.h lash-diag.h
auto-minimize.o: resource.h sort.h
auto-product.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
auto-product.o: /usr/include/features.h /usr/include/features-time64.h
auto-product.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
auto-product.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
auto-product.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
auto-product.o: /usr/include/stddef.h /usr/include/stdarg.h
auto-product.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
auto-product.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
auto-product.o: /usr/include/bits/types/__mbstate_t.h
auto-product.o: /usr/include/bits/types/__fpos64_t.h
auto-product.o: /usr/include/bits/types/__FILE.h
auto-product.o: /usr/include/bits/types/FILE.h
auto-product.o: /usr/include/bits/types/struct_FILE.h
auto-product.o: /usr/include/bits/types/cookie_io_functions_t.h
auto-product.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
auto-product.o: /usr/include/bits/floatn-common.h /usr/include/string.h
auto-product.o: /usr/include/bits/types/locale_t.h
auto-product.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
auto-product.o: /usr/include/stdlib.h /usr/include/bits/waitflags.h
auto-product.o: /usr/include/bits/waitstatus.h /usr/include/sys/types.h
auto-product.o: /usr/include/bits/types/clock_t.h
auto-product.o: /usr/include/bits/types/clockid_t.h
auto-product.o: /usr/include/bits/types/time_t.h
auto-product.o: /usr/include/bits/types/timer_t.h
auto-product.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
auto-product.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
auto-product.o: /usr/include/bits/byteswap.h
auto-product.o: /usr/include/bits/uintn-identity.h /usr/include/sys/select.h
auto-product.o: /usr/include/bits/select.h /usr/include/bits/types/sigset_t.h
auto-product.o: /usr/include/bits/types/__sigset_t.h
auto-product.o: /usr/include/bits/types/struct_timeval.h
auto-product.o: /usr/include/bits/types/struct_timespec.h
auto-product.o: /usr/include/bits/pthreadtypes.h
auto-product.o: /usr/include/bits/thread-shared-types.h
auto-product.o: /usr/include/bits/pthreadtypes-arch.h
auto-product.o: /usr/include/bits/atomic_wide_counter.h
auto-product.o: /usr/include/bits/struct_mutex.h
auto-product.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
auto-product.o: /usr/include/bits/stdlib-float.h auto.h lash-auto.h
auto-product.o: lash-types.h lash.h datastruct.h auto-product.h
auto-product.o: lash-auto-operations.h auto-determinize.h auto-minimize.h
auto-product.o: auto-sequential.h auto-difference.h auto-normalize.h
auto-product.o: auto-quotient.h auto-count.h biguint.h diag.h lash-diag.h
auto-product.o: resource.h
auto-sequential.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
auto-sequential.o: /usr/include/features.h /usr/include/features-time64.h
auto-sequential.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
auto-sequential.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
auto-sequential.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
auto-sequential.o: /usr/include/stddef.h /usr/include/stdarg.h
auto-sequential.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
auto-sequential.o: /usr/include/bits/time64.h
auto-sequential.o: /usr/include/bits/types/__fpos_t.h
auto-sequential.o: /usr/include/bits/types/__mbstate_t.h
auto-sequential.o: /usr/include/bits/types/__fpos64_t.h
auto-sequential.o: /usr/include/bits/types/__FILE.h
auto-sequential.o: /usr/include/bits/types/FILE.h
auto-sequential.o: /usr/include/bits/types/struct_FILE.h
auto-sequential.o: /usr/include/bits/types/cookie_io_functions_t.h
auto-sequential.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
auto-sequential.o: /usr/include/bits/floatn-common.h /usr/include/string.h
auto-sequential.o: /usr/include/bits/types/locale_t.h
auto-sequential.o: /usr/include/bits/types/__locale_t.h
auto-sequential.o: /usr/include/strings.h /usr/include/stdlib.h
auto-sequential.o: /usr/include/bits/waitflags.h
auto-sequential.o: /usr/include/bits/waitstatus.h /usr/include/sys/types.h
auto-sequential.o: /usr/include/bits/types/clock_t.h
auto-sequential.o: /usr/include/bits/types/clockid_t.h
auto-sequential.o: /usr/include/bits/types/time_t.h
auto-sequential.o: /usr/include/bits/types/timer_t.h
auto-sequential.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
auto-sequential.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
auto-sequential.o: /usr/include/bits/byteswap.h
auto-sequential.o: /usr/include/bits/uintn-identity.h
auto-sequential.o: /usr/include/sys/select.h /usr/include/bits/select.h
auto-sequential.o: /usr/include/bits/types/sigset_t.h
auto-sequential.o: /usr/include/bits/types/__sigset_t.h
auto-sequential.o: /usr/include/bits/types/struct_timeval.h
auto-sequential.o: /usr/include/bits/types/struct_timespec.h
auto-sequential.o: /usr/include/bits/pthreadtypes.h
auto-sequential.o: /usr/include/bits/thread-shared-types.h
auto-sequential.o: /usr/include/bits/pthreadtypes-arch.h
auto-sequential.o: /usr/include/bits/atomic_wide_counter.h
auto-sequential.o: /usr/include/bits/struct_mutex.h
auto-sequential.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
auto-sequential.o: /usr/include/bits/stdlib-float.h auto.h lash-auto.h
auto-sequential.o: lash-types.h lash.h datastruct.h auto-sequential.h
auto-sequential.o: lash-auto-operations.h auto-determinize.h auto-minimize.h
auto-sequential.o: auto-product.h auto-difference.h auto-normalize.h
auto-sequential.o: auto-quotient.h auto-count.h biguint.h diag.h lash-diag.h
auto-sequential.o: resource.h
auto-difference.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
auto-difference.o: /usr/include/features.h /usr/include/features-time64.h
auto-difference.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
auto-difference.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
auto-difference.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
auto-difference.o: /usr/include/stddef.h /usr/include/stdarg.h
auto-difference.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
auto-difference.o: /usr/include/bits/time64.h
auto-difference.o: /usr/include/bits/types/__fpos_t.h
auto-difference.o: /usr/include/bits/types/__mbstate_t.h
auto-difference.o: /usr/include/bits/types/__fpos64_t.h
auto-difference.o: /usr/include/bits/types/__FILE.h
auto-difference.o: /usr/include/bits/types/FILE.h
auto-difference.o: /usr/include/bits/types/struct_FILE.h
auto-difference.o: /usr/include/bits/types/cookie_io_functions_t.h
auto-difference.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
auto-difference.o: /usr/include/bits/floatn-common.h /usr/include/string.h
auto-difference.o: /usr/include/bits/types/locale_t.h
auto-difference.o: /usr/include/bits/types/__locale_t.h
auto-difference.o: /usr/include/strings.h /usr/include/stdlib.h
auto-difference.o: /usr/include/bits/waitflags.h
auto-difference.o: /usr/include/bits/waitstatus.h /usr/include/sys/types.h
auto-difference.o: /usr/include/bits/types/clock_t.h
auto-difference.o: /usr/include/bits/types/clockid_t.h
auto-difference.o: /usr/include/bits/types/time_t.h
auto-difference.o: /usr/include/bits/types/timer_t.h
auto-difference.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
auto-difference.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
auto-difference.o: /usr/include/bits/byteswap.h
auto-difference.o: /usr/include/bits/uintn-identity.h
auto-difference.o: /usr/include/sys/select.h /usr/include/bits/select.h
auto-difference.o: /usr/include/bits/types/sigset_t.h
auto-difference.o: /usr/include/bits/types/__sigset_t.h
auto-difference.o: /usr/include/bits/types/struct_timeval.h
auto-difference.o: /usr/include/bits/types/struct_timespec.h
auto-difference.o: /usr/include/bits/pthreadtypes.h
auto-difference.o: /usr/include/bits/thread-shared-types.h
auto-difference.o: /usr/include/bits/pthreadtypes-arch.h
auto-difference.o: /usr/include/bits/atomic_wide_counter.h
auto-difference.o: /usr/include/bits/struct_mutex.h
auto-difference.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
auto-difference.o: /usr/include/bits/stdlib-float.h auto.h lash-auto.h
auto-difference.o: lash-types.h lash.h datastruct.h auto-difference.h
auto-difference.o: lash-auto-operations.h auto-determinize.h auto-minimize.h
auto-difference.o: auto-product.h auto-sequential.h auto-normalize.h
auto-difference.o: auto-quotient.h auto-count.h biguint.h diag.h lash-diag.h
auto-difference.o: resource.h graph-scc.h
auto-normalize.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
auto-normalize.o: /usr/include/features.h /usr/include/features-time64.h
auto-normalize.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
auto-normalize.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
auto-normalize.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
auto-normalize.o: /usr/include/stddef.h /usr/include/stdarg.h
auto-normalize.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
auto-normalize.o: /usr/include/bits/time64.h
auto-normalize.o: /usr/include/bits/types/__fpos_t.h
auto-normalize.o: /usr/include/bits/types/__mbstate_t.h
auto-normalize.o: /usr/include/bits/types/__fpos64_t.h
auto-normalize.o: /usr/include/bits/types/__FILE.h
auto-normalize.o: /usr/include/bits/types/FILE.h
auto-normalize.o: /usr/include/bits/types/struct_FILE.h
auto-normalize.o: /usr/include/bits/types/cookie_io_functions_t.h
auto-normalize.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
auto-normalize.o: /usr/include/bits/floatn-common.h /usr/include/string.h
auto-normalize.o: /usr/include/bits/types/locale_t.h
auto-normalize.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
auto-normalize.o: /usr/include/stdlib.h /usr/include/bits/waitflags.h
auto-normalize.o: /usr/include/bits/waitstatus.h /usr/include/sys/types.h
auto-normalize.o: /usr/include/bits/types/clock_t.h
auto-normalize.o: /usr/include/bits/types/clockid_t.h
auto-normalize.o: /usr/include/bits/types/time_t.h
auto-normalize.o: /usr/include/bits/types/timer_t.h
auto-normalize.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
auto-normalize.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
auto-normalize.o: /usr/include/bits/byteswap.h
auto-normalize.o: /usr/include/bits/uintn-identity.h
auto-normalize.o: /usr/include/sys/select.h /usr/include/bits/select.h
auto-normalize.o: /usr/include/bits/types/sigset_t.h
auto-normalize.o: /usr/include/bits/types/__sigset_t.h
auto-normalize.o: /usr/include/bits/types/struct_timeval.h
auto-normalize.o: /usr/include/bits/types/struct_timespec.h
auto-normalize.o: /usr/include/bits/pthreadtypes.h
auto-normalize.o: /usr/include/bits/thread-shared-types.h
auto-normalize.o: /usr/include/bits/pthreadtypes-arch.h
auto-normalize.o: /usr/include/bits/atomic_wide_counter.h
auto-normalize.o: /usr/include/bits/struct_mutex.h
auto-normalize.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
auto-normalize.o: /usr/include/bits/stdlib-float.h auto.h lash-auto.h
auto-normalize.o: lash-types.h lash.h datastruct.h auto-normalize.h
auto-normalize.o: lash-auto-operations.h auto-determinize.h auto-minimize.h
auto-normalize.o: auto-product.h auto-sequential.h auto-difference.h
auto-normalize.o: auto-quotient.h auto-count.h biguint.h diag.h lash-diag.h
auto-normalize.o: resource.h graph-scc.h
auto-quotient.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
auto-quotient.o: /usr/include/features.h /usr/include/features-time64.h
auto-quotient.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
auto-quotient.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
auto-quotient.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
auto-quotient.o: /usr/include/stddef.h /usr/include/stdarg.h
auto-quotient.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
auto-quotient.o: /usr/include/bits/time64.h
auto-quotient.o: /usr/include/bits/types/__fpos_t.h
auto-quotient.o: /usr/include/bits/types/__mbstate_t.h
auto-quotient.o: /usr/include/bits/types/__fpos64_t.h
auto-quotient.o: /usr/include/bits/types/__FILE.h
auto-quotient.o: /usr/include/bits/types/FILE.h
auto-quotient.o: /usr/include/bits/types/struct_FILE.h
auto-quotient.o: /usr/include/bits/types/cookie_io_functions_t.h
auto-quotient.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
auto-quotient.o: /usr/include/bits/floatn-common.h /usr/include/string.h
auto-quotient.o: /usr/include/bits/types/locale_t.h
auto-quotient.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
auto-quotient.o: /usr/include/stdlib.h /usr/include/bits/waitflags.h
auto-quotient.o: /usr/include/bits/waitstatus.h /usr/include/sys/types.h
auto-quotient.o: /usr/include/bits/types/clock_t.h
auto-quotient.o: /usr/include/bits/types/clockid_t.h
auto-quotient.o: /usr/include/bits/types/time_t.h
auto-quotient.o: /usr/include/bits/types/timer_t.h
auto-quotient.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
auto-quotient.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
auto-quotient.o: /usr/include/bits/byteswap.h
auto-quotient.o: /usr/include/bits/uintn-identity.h /usr/include/sys/select.h
auto-quotient.o: /usr/include/bits/select.h
auto-quotient.o: /usr/include/bits/types/sigset_t.h
auto-quotient.o: /usr/include/bits/types/__sigset_t.h
auto-quotient.o: /usr/include/bits/types/struct_timeval.h
auto-quotient.o: /usr/include/bits/types/struct_timespec.h
auto-quotient.o: /usr/include/bits/pthreadtypes.h
auto-quotient.o: /usr/include/bits/thread-shared-types.h
auto-quotient.o: /usr/include/bits/pthreadtypes-arch.h
auto-quotient.o: /usr/include/bits/atomic_wide_counter.h
auto-quotient.o: /usr/include/bits/struct_mutex.h
auto-quotient.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
auto-quotient.o: /usr/include/bits/stdlib-float.h auto.h lash-auto.h
auto-quotient.o: lash-types.h lash.h datastruct.h auto-quotient.h
auto-quotient.o: lash-auto-operations.h auto-determinize.h auto-minimize.h
auto-quotient.o: auto-product.h auto-sequential.h auto-difference.h
auto-quotient.o: auto-normalize.h auto-count.h biguint.h diag.h lash-diag.h
auto-quotient.o: resource.h
auto-operations.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
auto-operations.o: /usr/include/features.h /usr/include/features-time64.h
auto-operations.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
auto-operations.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
auto-operations.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
auto-operations.o: /usr/include/stddef.h /usr/include/stdarg.h
auto-operations.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
auto-operations.o: /usr/include/bits/time64.h
auto-operations.o: /usr/include/bits/types/__fpos_t.h
auto-operations.o: /usr/include/bits/types/__mbstate_t.h
auto-operations.o: /usr/include/bits/types/__fpos64_t.h
auto-operations.o: /usr/include/bits/types/__FILE.h
auto-operations.o: /usr/include/bits/types/FILE.h
auto-operations.o: /usr/include/bits/types/struct_FILE.h
auto-operations.o: /usr/include/bits/types/cookie_io_functions_t.h
auto-operations.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
auto-operations.o: /usr/include/bits/floatn-common.h /usr/include/string.h
auto-operations.o: /usr/include/bits/types/locale_t.h
auto-operations.o: /usr/include/bits/types/__locale_t.h
auto-operations.o: /usr/include/strings.h /usr/include/stdlib.h
auto-operations.o: /usr/include/bits/waitflags.h
auto-operations.o: /usr/include/bits/waitstatus.h /usr/include/sys/types.h
auto-operations.o: /usr/include/bits/types/clock_t.h
auto-operations.o: /usr/include/bits/types/clockid_t.h
auto-operations.o: /usr/include/bits/types/time_t.h
auto-operations.o: /usr/include/bits/types/timer_t.h
auto-operations.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
auto-operations.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
auto-operations.o: /usr/include/bits/byteswap.h
auto-operations.o: /usr/include/bits/uintn-identity.h
auto-operations.o: /usr/include/sys/select.h /usr/include/bits/select.h
auto-operations.o: /usr/include/bits/types/sigset_t.h
auto-operations.o: /usr/include/bits/types/__sigset_t.h
auto-operations.o: /usr/include/bits/types/struct_timeval.h
auto-operations.o: /usr/include/bits/types/struct_timespec.h
auto-operations.o: /usr/include/bits/pthreadtypes.h
auto-operations.o: /usr/include/bits/thread-shared-types.h
auto-operations.o: /usr/include/bits/pthreadtypes-arch.h
auto-operations.o: /usr/include/bits/atomic_wide_counter.h
auto-operations.o: /usr/include/bits/struct_mutex.h
auto-operations.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
auto-operations.o: /usr/include/bits/stdlib-float.h auto.h lash-auto.h
auto-operations.o: lash-types.h lash.h datastruct.h lash-auto-operations.h
auto-operations.o: auto-determinize.h auto-minimize.h auto-product.h
auto-operations.o: auto-sequential.h auto-difference.h auto-normalize.h
auto-operations.o: auto-quotient.h auto-count.h biguint.h diag.h lash-diag.h
auto-operations.o: resource.h
auto-count.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
auto-count.o: /usr/include/features.h /usr/include/features-time64.h
auto-count.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
auto-count.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
auto-count.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
auto-count.o: /usr/include/stddef.h /usr/include/stdarg.h
auto-count.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
auto-count.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
auto-count.o: /usr/include/bits/types/__mbstate_t.h
auto-count.o: /usr/include/bits/types/__fpos64_t.h
auto-count.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
auto-count.o: /usr/include/bits/types/struct_FILE.h
auto-count.o: /usr/include/bits/types/cookie_io_functions_t.h
auto-count.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
auto-count.o: /usr/include/bits/floatn-common.h datastruct.h
auto-count.o: /usr/include/string.h /usr/include/bits/types/locale_t.h
auto-count.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
auto-count.o: lash-types.h lash.h lash-auto.h /usr/include/stdlib.h
auto-count.o: /usr/include/bits/waitflags.h /usr/include/bits/waitstatus.h
auto-count.o: /usr/include/sys/types.h /usr/include/bits/types/clock_t.h
auto-count.o: /usr/include/bits/types/clockid_t.h
auto-count.o: /usr/include/bits/types/time_t.h
auto-count.o: /usr/include/bits/types/timer_t.h
auto-count.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
auto-count.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
auto-count.o: /usr/include/bits/byteswap.h /usr/include/bits/uintn-identity.h
auto-count.o: /usr/include/sys/select.h /usr/include/bits/select.h
auto-count.o: /usr/include/bits/types/sigset_t.h
auto-count.o: /usr/include/bits/types/__sigset_t.h
auto-count.o: /usr/include/bits/types/struct_timeval.h
auto-count.o: /usr/include/bits/types/struct_timespec.h
auto-count.o: /usr/include/bits/pthreadtypes.h
auto-count.o: /usr/include/bits/thread-shared-types.h
auto-count.o: /usr/include/bits/pthreadtypes-arch.h
auto-count.o: /usr/include/bits/atomic_wide_counter.h
auto-count.o: /usr/include/bits/struct_mutex.h
auto-count.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
auto-count.o: /usr/include/bits/stdlib-float.h biguint.h auto.h diag.h
auto-count.o: lash-diag.h resource.h lash-auto-io.h lash-io.h datastruct-io.h
auto-count.o: auto-io-print.h auto-io-dots.h lash-auto-operations.h
auto-count.o: auto-determinize.h auto-minimize.h auto-product.h
auto-count.o: auto-sequential.h auto-difference.h auto-normalize.h
auto-count.o: auto-quotient.h auto-count.h
auto-serialize.o: /usr/include/string.h /usr/include/bits/libc-header-start.h
auto-serialize.o: /usr/include/features.h /usr/include/features-time64.h
auto-serialize.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
auto-serialize.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
auto-serialize.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
auto-serialize.o: /usr/include/stddef.h /usr/include/bits/types/locale_t.h
auto-serialize.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
auto-serialize.o: auto.h lash-auto.h /usr/include/stdlib.h
auto-serialize.o: /usr/include/bits/waitflags.h
auto-serialize.o: /usr/include/bits/waitstatus.h /usr/include/bits/floatn.h
auto-serialize.o: /usr/include/bits/floatn-common.h /usr/include/sys/types.h
auto-serialize.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
auto-serialize.o: /usr/include/bits/time64.h
auto-serialize.o: /usr/include/bits/types/clock_t.h
auto-serialize.o: /usr/include/bits/types/clockid_t.h
auto-serialize.o: /usr/include/bits/types/time_t.h
auto-serialize.o: /usr/include/bits/types/timer_t.h
auto-serialize.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
auto-serialize.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
auto-serialize.o: /usr/include/bits/byteswap.h
auto-serialize.o: /usr/include/bits/uintn-identity.h
auto-serialize.o: /usr/include/sys/select.h /usr/include/bits/select.h
auto-serialize.o: /usr/include/bits/types/sigset_t.h
auto-serialize.o: /usr/include/bits/types/__sigset_t.h
auto-serialize.o: /usr/include/bits/types/struct_timeval.h
auto-serialize.o: /usr/include/bits/types/struct_timespec.h
auto-serialize.o: /usr/include/bits/pthreadtypes.h
auto-serialize.o: /usr/include/bits/thread-shared-types.h
auto-serialize.o: /usr/include/bits/pthreadtypes-arch.h
auto-serialize.o: /usr/include/bits/atomic_wide_counter.h
auto-serialize.o: /usr/include/bits/struct_mutex.h
auto-serialize.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
auto-serialize.o: /usr/include/bits/stdlib-float.h lash-types.h lash.h
auto-serialize.o: datastruct.h auto-serialize.h lash-auto-operations.h
auto-serialize.o: auto-determinize.h auto-minimize.h auto-product.h
auto-serialize.o: auto-sequential.h auto-difference.h auto-normalize.h
auto-serialize.o: auto-quotient.h auto-count.h biguint.h resource.h diag.h
auto-serialize.o: lash-diag.h
auto-weak-convert.o: auto-weak-convert.h lash-auto.h /usr/include/stdlib.h
auto-weak-convert.o: /usr/include/bits/libc-header-start.h
auto-weak-convert.o: /usr/include/features.h /usr/include/features-time64.h
auto-weak-convert.o: /usr/include/bits/wordsize.h
auto-weak-convert.o: /usr/include/bits/timesize.h /usr/include/stdc-predef.h
auto-weak-convert.o: /usr/include/sys/cdefs.h /usr/include/bits/long-double.h
auto-weak-convert.o: /usr/include/gnu/stubs.h /usr/include/stddef.h
auto-weak-convert.o: /usr/include/bits/waitflags.h
auto-weak-convert.o: /usr/include/bits/waitstatus.h
auto-weak-convert.o: /usr/include/bits/floatn.h
auto-weak-convert.o: /usr/include/bits/floatn-common.h
auto-weak-convert.o: /usr/include/sys/types.h /usr/include/bits/types.h
auto-weak-convert.o: /usr/include/bits/typesizes.h /usr/include/bits/time64.h
auto-weak-convert.o: /usr/include/bits/types/clock_t.h
auto-weak-convert.o: /usr/include/bits/types/clockid_t.h
auto-weak-convert.o: /usr/include/bits/types/time_t.h
auto-weak-convert.o: /usr/include/bits/types/timer_t.h
auto-weak-convert.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
auto-weak-convert.o: /usr/include/bits/endian.h
auto-weak-convert.o: /usr/include/bits/endianness.h
auto-weak-convert.o: /usr/include/bits/byteswap.h
auto-weak-convert.o: /usr/include/bits/uintn-identity.h
auto-weak-convert.o: /usr/include/sys/select.h /usr/include/bits/select.h
auto-weak-convert.o: /usr/include/bits/types/sigset_t.h
auto-weak-convert.o: /usr/include/bits/types/__sigset_t.h
auto-weak-convert.o: /usr/include/bits/types/struct_timeval.h
auto-weak-convert.o: /usr/include/bits/types/struct_timespec.h
auto-weak-convert.o: /usr/include/bits/pthreadtypes.h
auto-weak-convert.o: /usr/include/bits/thread-shared-types.h
auto-weak-convert.o: /usr/include/bits/pthreadtypes-arch.h
auto-weak-convert.o: /usr/include/bits/atomic_wide_counter.h
auto-weak-convert.o: /usr/include/bits/struct_mutex.h
auto-weak-convert.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
auto-weak-convert.o: /usr/include/bits/stdlib-float.h lash-types.h lash.h
auto-weak-convert.o: datastruct.h /usr/include/string.h
auto-weak-convert.o: /usr/include/bits/types/locale_t.h
auto-weak-convert.o: /usr/include/bits/types/__locale_t.h
auto-weak-convert.o: /usr/include/strings.h diag.h lash-diag.h graph-scc.h
auto-depth.o: auto-depth.h lash-types.h lash.h datastruct.h
auto-depth.o: /usr/include/string.h /usr/include/bits/libc-header-start.h
auto-depth.o: /usr/include/features.h /usr/include/features-time64.h
auto-depth.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
auto-depth.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
auto-depth.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
auto-depth.o: /usr/include/stddef.h /usr/include/bits/types/locale_t.h
auto-depth.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
auto-depth.o: lash-auto.h /usr/include/stdlib.h /usr/include/bits/waitflags.h
auto-depth.o: /usr/include/bits/waitstatus.h /usr/include/bits/floatn.h
auto-depth.o: /usr/include/bits/floatn-common.h /usr/include/sys/types.h
auto-depth.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
auto-depth.o: /usr/include/bits/time64.h /usr/include/bits/types/clock_t.h
auto-depth.o: /usr/include/bits/types/clockid_t.h
auto-depth.o: /usr/include/bits/types/time_t.h
auto-depth.o: /usr/include/bits/types/timer_t.h
auto-depth.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
auto-depth.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
auto-depth.o: /usr/include/bits/byteswap.h /usr/include/bits/uintn-identity.h
auto-depth.o: /usr/include/sys/select.h /usr/include/bits/select.h
auto-depth.o: /usr/include/bits/types/sigset_t.h
auto-depth.o: /usr/include/bits/types/__sigset_t.h
auto-depth.o: /usr/include/bits/types/struct_timeval.h
auto-depth.o: /usr/include/bits/types/struct_timespec.h
auto-depth.o: /usr/include/bits/pthreadtypes.h
auto-depth.o: /usr/include/bits/thread-shared-types.h
auto-depth.o: /usr/include/bits/pthreadtypes-arch.h
auto-depth.o: /usr/include/bits/atomic_wide_counter.h
auto-depth.o: /usr/include/bits/struct_mutex.h
auto-depth.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
auto-depth.o: /usr/include/bits/stdlib-float.h auto.h diag.h lash-diag.h
auto-depth.o: resource.h auto-sort.h /usr/include/stdio.h
auto-depth.o: /usr/include/stdarg.h /usr/include/bits/types/__fpos_t.h
auto-depth.o: /usr/include/bits/types/__mbstate_t.h
auto-depth.o: /usr/include/bits/types/__fpos64_t.h
auto-depth.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
auto-depth.o: /usr/include/bits/types/struct_FILE.h
auto-depth.o: /usr/include/bits/types/cookie_io_functions_t.h
auto-depth.o: /usr/include/bits/stdio_lim.h auto-determinize.h
auto-depth.o: lash-auto-operations.h auto-minimize.h auto-product.h
auto-depth.o: auto-sequential.h auto-difference.h auto-normalize.h
auto-depth.o: auto-quotient.h auto-count.h biguint.h
auto-sort.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
auto-sort.o: /usr/include/features.h /usr/include/features-time64.h
auto-sort.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
auto-sort.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
auto-sort.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
auto-sort.o: /usr/include/stddef.h /usr/include/stdarg.h
auto-sort.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
auto-sort.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
auto-sort.o: /usr/include/bits/types/__mbstate_t.h
auto-sort.o: /usr/include/bits/types/__fpos64_t.h
auto-sort.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
auto-sort.o: /usr/include/bits/types/struct_FILE.h
auto-sort.o: /usr/include/bits/types/cookie_io_functions_t.h
auto-sort.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
auto-sort.o: /usr/include/bits/floatn-common.h /usr/include/string.h
auto-sort.o: /usr/include/bits/types/locale_t.h
auto-sort.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
auto-sort.o: /usr/include/stdlib.h /usr/include/bits/waitflags.h
auto-sort.o: /usr/include/bits/waitstatus.h /usr/include/sys/types.h
auto-sort.o: /usr/include/bits/types/clock_t.h
auto-sort.o: /usr/include/bits/types/clockid_t.h
auto-sort.o: /usr/include/bits/types/time_t.h
auto-sort.o: /usr/include/bits/types/timer_t.h
auto-sort.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
auto-sort.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
auto-sort.o: /usr/include/bits/byteswap.h /usr/include/bits/uintn-identity.h
auto-sort.o: /usr/include/sys/select.h /usr/include/bits/select.h
auto-sort.o: /usr/include/bits/types/sigset_t.h
auto-sort.o: /usr/include/bits/types/__sigset_t.h
auto-sort.o: /usr/include/bits/types/struct_timeval.h
auto-sort.o: /usr/include/bits/types/struct_timespec.h
auto-sort.o: /usr/include/bits/pthreadtypes.h
auto-sort.o: /usr/include/bits/thread-shared-types.h
auto-sort.o: /usr/include/bits/pthreadtypes-arch.h
auto-sort.o: /usr/include/bits/atomic_wide_counter.h
auto-sort.o: /usr/include/bits/struct_mutex.h
auto-sort.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
auto-sort.o: /usr/include/bits/stdlib-float.h auto-sort.h lash-auto.h
auto-sort.o: lash-types.h lash.h datastruct.h auto.h diag.h lash-diag.h
auto-sort.o: resource.h
cyclotomic.o: cyclotomic.h lash-types.h lash.h
matrix.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
matrix.o: /usr/include/features.h /usr/include/features-time64.h
matrix.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
matrix.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
matrix.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
matrix.o: /usr/include/stddef.h /usr/include/stdarg.h
matrix.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
matrix.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
matrix.o: /usr/include/bits/types/__mbstate_t.h
matrix.o: /usr/include/bits/types/__fpos64_t.h
matrix.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
matrix.o: /usr/include/bits/types/struct_FILE.h
matrix.o: /usr/include/bits/types/cookie_io_functions_t.h
matrix.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
matrix.o: /usr/include/bits/floatn-common.h /usr/include/string.h
matrix.o: /usr/include/bits/types/locale_t.h
matrix.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
matrix.o: arithmetic.h lash-types.h lash.h resource.h matrix.h lash-diag.h
