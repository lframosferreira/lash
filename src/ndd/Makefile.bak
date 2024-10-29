######################################################################
##                                                                  ##
##  LASH NDD package -- v0.9                                        ##
##  ========================                                        ##
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
##     08/13/01  :  New source tree. (BB)                           ##
##     07/09/02  :  Reorganization. (BB)                            ##
##     02/22/05  :  New module. (LL)                                ##
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

LASH-CORE-DIR =	../lash-core/

SRCS-NDD =	ndd.c \
		ndd-sets.c \
		ndd-operations.c \
		ndd-tests.c \
		ndd-equations.c \
		ndd-projection.c \
		ndd-transformations.c \
		ndd-iterations.c \
		ndd-machines.c \
		ndd-io.c \
		ndd-count.c \
		ndd-congruence.c

OBJS-NDD =	${SRCS-NDD:.c=.o}

CFLAGS =   -c -g -O3 -ansi -Wall -Wpointer-arith \
         -Waggregate-return -Wstrict-prototypes \
         -Wmissing-prototypes -Wmissing-declarations \
	 -I$(LASH-CORE-DIR)

all:  ndd-package

ndd-package: $(OBJS-NDD)
	ld -r -o ndd-package.o $(OBJS-NDD) 
	mv ndd-package.o $(LASH-LIB-DIR)

clean:
	-rm -f $(OBJS-NDD) ndd-package.o
	-rm -f $(LASH-LIB-DIR)/ndd-package.o

depend: 
	makedepend -I$(LASH-CORE-DIR) $(SRCS-NDD)

# DO NOT DELETE THIS LINE -- make depend depends on it.

ndd.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
ndd.o: /usr/include/features.h /usr/include/features-time64.h
ndd.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
ndd.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
ndd.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
ndd.o: /usr/include/stddef.h /usr/include/stdarg.h /usr/include/bits/types.h
ndd.o: /usr/include/bits/typesizes.h /usr/include/bits/time64.h
ndd.o: /usr/include/bits/types/__fpos_t.h
ndd.o: /usr/include/bits/types/__mbstate_t.h
ndd.o: /usr/include/bits/types/__fpos64_t.h /usr/include/bits/types/__FILE.h
ndd.o: /usr/include/bits/types/FILE.h /usr/include/bits/types/struct_FILE.h
ndd.o: /usr/include/bits/types/cookie_io_functions_t.h
ndd.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
ndd.o: /usr/include/bits/floatn-common.h ndd.h lash-ndd.h
ndd.o: ../lash-core/lash-auto.h /usr/include/stdlib.h
ndd.o: /usr/include/bits/waitflags.h /usr/include/bits/waitstatus.h
ndd.o: /usr/include/sys/types.h /usr/include/bits/types/clock_t.h
ndd.o: /usr/include/bits/types/clockid_t.h /usr/include/bits/types/time_t.h
ndd.o: /usr/include/bits/types/timer_t.h /usr/include/bits/stdint-intn.h
ndd.o: /usr/include/endian.h /usr/include/bits/endian.h
ndd.o: /usr/include/bits/endianness.h /usr/include/bits/byteswap.h
ndd.o: /usr/include/bits/uintn-identity.h /usr/include/sys/select.h
ndd.o: /usr/include/bits/select.h /usr/include/bits/types/sigset_t.h
ndd.o: /usr/include/bits/types/__sigset_t.h
ndd.o: /usr/include/bits/types/struct_timeval.h
ndd.o: /usr/include/bits/types/struct_timespec.h
ndd.o: /usr/include/bits/pthreadtypes.h
ndd.o: /usr/include/bits/thread-shared-types.h
ndd.o: /usr/include/bits/pthreadtypes-arch.h
ndd.o: /usr/include/bits/atomic_wide_counter.h
ndd.o: /usr/include/bits/struct_mutex.h /usr/include/bits/struct_rwlock.h
ndd.o: /usr/include/alloca.h /usr/include/bits/stdlib-float.h
ndd.o: ../lash-core/lash-types.h ../lash-core/lash.h
ndd.o: ../lash-core/datastruct.h /usr/include/string.h
ndd.o: /usr/include/bits/types/locale_t.h
ndd.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h ndd-sets.h
ndd.o: ndd-operations.h ndd-tests.h ndd-equations.h ndd-projection.h
ndd.o: ndd-transformations.h ndd-iterations.h ndd-machines.h
ndd.o: ../lash-core/auto.h ../lash-core/diag.h ../lash-core/lash-diag.h
ndd.o: ../lash-core/resource.h ../lash-core/lash-auto-operations.h
ndd.o: ../lash-core/auto-determinize.h ../lash-core/auto-minimize.h
ndd.o: ../lash-core/auto-product.h ../lash-core/auto-sequential.h
ndd.o: ../lash-core/auto-difference.h ../lash-core/auto-normalize.h
ndd.o: ../lash-core/auto-quotient.h ../lash-core/auto-count.h
ndd.o: ../lash-core/biguint.h
ndd-sets.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
ndd-sets.o: /usr/include/features.h /usr/include/features-time64.h
ndd-sets.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
ndd-sets.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
ndd-sets.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
ndd-sets.o: /usr/include/stddef.h /usr/include/stdarg.h
ndd-sets.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
ndd-sets.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
ndd-sets.o: /usr/include/bits/types/__mbstate_t.h
ndd-sets.o: /usr/include/bits/types/__fpos64_t.h
ndd-sets.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
ndd-sets.o: /usr/include/bits/types/struct_FILE.h
ndd-sets.o: /usr/include/bits/types/cookie_io_functions_t.h
ndd-sets.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
ndd-sets.o: /usr/include/bits/floatn-common.h ndd.h lash-ndd.h
ndd-sets.o: ../lash-core/lash-auto.h /usr/include/stdlib.h
ndd-sets.o: /usr/include/bits/waitflags.h /usr/include/bits/waitstatus.h
ndd-sets.o: /usr/include/sys/types.h /usr/include/bits/types/clock_t.h
ndd-sets.o: /usr/include/bits/types/clockid_t.h
ndd-sets.o: /usr/include/bits/types/time_t.h
ndd-sets.o: /usr/include/bits/types/timer_t.h /usr/include/bits/stdint-intn.h
ndd-sets.o: /usr/include/endian.h /usr/include/bits/endian.h
ndd-sets.o: /usr/include/bits/endianness.h /usr/include/bits/byteswap.h
ndd-sets.o: /usr/include/bits/uintn-identity.h /usr/include/sys/select.h
ndd-sets.o: /usr/include/bits/select.h /usr/include/bits/types/sigset_t.h
ndd-sets.o: /usr/include/bits/types/__sigset_t.h
ndd-sets.o: /usr/include/bits/types/struct_timeval.h
ndd-sets.o: /usr/include/bits/types/struct_timespec.h
ndd-sets.o: /usr/include/bits/pthreadtypes.h
ndd-sets.o: /usr/include/bits/thread-shared-types.h
ndd-sets.o: /usr/include/bits/pthreadtypes-arch.h
ndd-sets.o: /usr/include/bits/atomic_wide_counter.h
ndd-sets.o: /usr/include/bits/struct_mutex.h
ndd-sets.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
ndd-sets.o: /usr/include/bits/stdlib-float.h ../lash-core/lash-types.h
ndd-sets.o: ../lash-core/lash.h ../lash-core/datastruct.h
ndd-sets.o: /usr/include/string.h /usr/include/bits/types/locale_t.h
ndd-sets.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
ndd-sets.o: ndd-sets.h ndd-operations.h ndd-tests.h ndd-equations.h
ndd-sets.o: ndd-projection.h ndd-transformations.h ndd-iterations.h
ndd-sets.o: ndd-machines.h ../lash-core/auto.h ../lash-core/diag.h
ndd-sets.o: ../lash-core/lash-diag.h ../lash-core/resource.h
ndd-sets.o: ../lash-core/lash-auto-operations.h
ndd-sets.o: ../lash-core/auto-determinize.h ../lash-core/auto-minimize.h
ndd-sets.o: ../lash-core/auto-product.h ../lash-core/auto-sequential.h
ndd-sets.o: ../lash-core/auto-difference.h ../lash-core/auto-normalize.h
ndd-sets.o: ../lash-core/auto-quotient.h ../lash-core/auto-count.h
ndd-sets.o: ../lash-core/biguint.h
ndd-operations.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
ndd-operations.o: /usr/include/features.h /usr/include/features-time64.h
ndd-operations.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
ndd-operations.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
ndd-operations.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
ndd-operations.o: /usr/include/stddef.h /usr/include/stdarg.h
ndd-operations.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
ndd-operations.o: /usr/include/bits/time64.h
ndd-operations.o: /usr/include/bits/types/__fpos_t.h
ndd-operations.o: /usr/include/bits/types/__mbstate_t.h
ndd-operations.o: /usr/include/bits/types/__fpos64_t.h
ndd-operations.o: /usr/include/bits/types/__FILE.h
ndd-operations.o: /usr/include/bits/types/FILE.h
ndd-operations.o: /usr/include/bits/types/struct_FILE.h
ndd-operations.o: /usr/include/bits/types/cookie_io_functions_t.h
ndd-operations.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
ndd-operations.o: /usr/include/bits/floatn-common.h ndd.h lash-ndd.h
ndd-operations.o: ../lash-core/lash-auto.h /usr/include/stdlib.h
ndd-operations.o: /usr/include/bits/waitflags.h
ndd-operations.o: /usr/include/bits/waitstatus.h /usr/include/sys/types.h
ndd-operations.o: /usr/include/bits/types/clock_t.h
ndd-operations.o: /usr/include/bits/types/clockid_t.h
ndd-operations.o: /usr/include/bits/types/time_t.h
ndd-operations.o: /usr/include/bits/types/timer_t.h
ndd-operations.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
ndd-operations.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
ndd-operations.o: /usr/include/bits/byteswap.h
ndd-operations.o: /usr/include/bits/uintn-identity.h
ndd-operations.o: /usr/include/sys/select.h /usr/include/bits/select.h
ndd-operations.o: /usr/include/bits/types/sigset_t.h
ndd-operations.o: /usr/include/bits/types/__sigset_t.h
ndd-operations.o: /usr/include/bits/types/struct_timeval.h
ndd-operations.o: /usr/include/bits/types/struct_timespec.h
ndd-operations.o: /usr/include/bits/pthreadtypes.h
ndd-operations.o: /usr/include/bits/thread-shared-types.h
ndd-operations.o: /usr/include/bits/pthreadtypes-arch.h
ndd-operations.o: /usr/include/bits/atomic_wide_counter.h
ndd-operations.o: /usr/include/bits/struct_mutex.h
ndd-operations.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
ndd-operations.o: /usr/include/bits/stdlib-float.h ../lash-core/lash-types.h
ndd-operations.o: ../lash-core/lash.h ../lash-core/datastruct.h
ndd-operations.o: /usr/include/string.h /usr/include/bits/types/locale_t.h
ndd-operations.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
ndd-operations.o: ndd-sets.h ndd-operations.h ndd-tests.h ndd-equations.h
ndd-operations.o: ndd-projection.h ndd-transformations.h ndd-iterations.h
ndd-operations.o: ndd-machines.h ../lash-core/auto.h ../lash-core/diag.h
ndd-operations.o: ../lash-core/lash-diag.h ../lash-core/resource.h
ndd-operations.o: ../lash-core/lash-auto-operations.h
ndd-operations.o: ../lash-core/auto-determinize.h
ndd-operations.o: ../lash-core/auto-minimize.h ../lash-core/auto-product.h
ndd-operations.o: ../lash-core/auto-sequential.h
ndd-operations.o: ../lash-core/auto-difference.h
ndd-operations.o: ../lash-core/auto-normalize.h ../lash-core/auto-quotient.h
ndd-operations.o: ../lash-core/auto-count.h ../lash-core/biguint.h
ndd-tests.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
ndd-tests.o: /usr/include/features.h /usr/include/features-time64.h
ndd-tests.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
ndd-tests.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
ndd-tests.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
ndd-tests.o: /usr/include/stddef.h /usr/include/stdarg.h
ndd-tests.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
ndd-tests.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
ndd-tests.o: /usr/include/bits/types/__mbstate_t.h
ndd-tests.o: /usr/include/bits/types/__fpos64_t.h
ndd-tests.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
ndd-tests.o: /usr/include/bits/types/struct_FILE.h
ndd-tests.o: /usr/include/bits/types/cookie_io_functions_t.h
ndd-tests.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
ndd-tests.o: /usr/include/bits/floatn-common.h ndd.h lash-ndd.h
ndd-tests.o: ../lash-core/lash-auto.h /usr/include/stdlib.h
ndd-tests.o: /usr/include/bits/waitflags.h /usr/include/bits/waitstatus.h
ndd-tests.o: /usr/include/sys/types.h /usr/include/bits/types/clock_t.h
ndd-tests.o: /usr/include/bits/types/clockid_t.h
ndd-tests.o: /usr/include/bits/types/time_t.h
ndd-tests.o: /usr/include/bits/types/timer_t.h
ndd-tests.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
ndd-tests.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
ndd-tests.o: /usr/include/bits/byteswap.h /usr/include/bits/uintn-identity.h
ndd-tests.o: /usr/include/sys/select.h /usr/include/bits/select.h
ndd-tests.o: /usr/include/bits/types/sigset_t.h
ndd-tests.o: /usr/include/bits/types/__sigset_t.h
ndd-tests.o: /usr/include/bits/types/struct_timeval.h
ndd-tests.o: /usr/include/bits/types/struct_timespec.h
ndd-tests.o: /usr/include/bits/pthreadtypes.h
ndd-tests.o: /usr/include/bits/thread-shared-types.h
ndd-tests.o: /usr/include/bits/pthreadtypes-arch.h
ndd-tests.o: /usr/include/bits/atomic_wide_counter.h
ndd-tests.o: /usr/include/bits/struct_mutex.h
ndd-tests.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
ndd-tests.o: /usr/include/bits/stdlib-float.h ../lash-core/lash-types.h
ndd-tests.o: ../lash-core/lash.h ../lash-core/datastruct.h
ndd-tests.o: /usr/include/string.h /usr/include/bits/types/locale_t.h
ndd-tests.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
ndd-tests.o: ndd-sets.h ndd-operations.h ndd-tests.h ndd-equations.h
ndd-tests.o: ndd-projection.h ndd-transformations.h ndd-iterations.h
ndd-tests.o: ndd-machines.h ../lash-core/auto.h ../lash-core/diag.h
ndd-tests.o: ../lash-core/lash-diag.h ../lash-core/resource.h
ndd-tests.o: ../lash-core/lash-auto-operations.h
ndd-tests.o: ../lash-core/auto-determinize.h ../lash-core/auto-minimize.h
ndd-tests.o: ../lash-core/auto-product.h ../lash-core/auto-sequential.h
ndd-tests.o: ../lash-core/auto-difference.h ../lash-core/auto-normalize.h
ndd-tests.o: ../lash-core/auto-quotient.h ../lash-core/auto-count.h
ndd-tests.o: ../lash-core/biguint.h
ndd-equations.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
ndd-equations.o: /usr/include/features.h /usr/include/features-time64.h
ndd-equations.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
ndd-equations.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
ndd-equations.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
ndd-equations.o: /usr/include/stddef.h /usr/include/stdarg.h
ndd-equations.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
ndd-equations.o: /usr/include/bits/time64.h
ndd-equations.o: /usr/include/bits/types/__fpos_t.h
ndd-equations.o: /usr/include/bits/types/__mbstate_t.h
ndd-equations.o: /usr/include/bits/types/__fpos64_t.h
ndd-equations.o: /usr/include/bits/types/__FILE.h
ndd-equations.o: /usr/include/bits/types/FILE.h
ndd-equations.o: /usr/include/bits/types/struct_FILE.h
ndd-equations.o: /usr/include/bits/types/cookie_io_functions_t.h
ndd-equations.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
ndd-equations.o: /usr/include/bits/floatn-common.h ndd.h lash-ndd.h
ndd-equations.o: ../lash-core/lash-auto.h /usr/include/stdlib.h
ndd-equations.o: /usr/include/bits/waitflags.h /usr/include/bits/waitstatus.h
ndd-equations.o: /usr/include/sys/types.h /usr/include/bits/types/clock_t.h
ndd-equations.o: /usr/include/bits/types/clockid_t.h
ndd-equations.o: /usr/include/bits/types/time_t.h
ndd-equations.o: /usr/include/bits/types/timer_t.h
ndd-equations.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
ndd-equations.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
ndd-equations.o: /usr/include/bits/byteswap.h
ndd-equations.o: /usr/include/bits/uintn-identity.h /usr/include/sys/select.h
ndd-equations.o: /usr/include/bits/select.h
ndd-equations.o: /usr/include/bits/types/sigset_t.h
ndd-equations.o: /usr/include/bits/types/__sigset_t.h
ndd-equations.o: /usr/include/bits/types/struct_timeval.h
ndd-equations.o: /usr/include/bits/types/struct_timespec.h
ndd-equations.o: /usr/include/bits/pthreadtypes.h
ndd-equations.o: /usr/include/bits/thread-shared-types.h
ndd-equations.o: /usr/include/bits/pthreadtypes-arch.h
ndd-equations.o: /usr/include/bits/atomic_wide_counter.h
ndd-equations.o: /usr/include/bits/struct_mutex.h
ndd-equations.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
ndd-equations.o: /usr/include/bits/stdlib-float.h ../lash-core/lash-types.h
ndd-equations.o: ../lash-core/lash.h ../lash-core/datastruct.h
ndd-equations.o: /usr/include/string.h /usr/include/bits/types/locale_t.h
ndd-equations.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
ndd-equations.o: ndd-sets.h ndd-operations.h ndd-tests.h ndd-equations.h
ndd-equations.o: ndd-projection.h ndd-transformations.h ndd-iterations.h
ndd-equations.o: ndd-machines.h ../lash-core/auto.h ../lash-core/diag.h
ndd-equations.o: ../lash-core/lash-diag.h ../lash-core/resource.h
ndd-equations.o: ../lash-core/lash-auto-operations.h
ndd-equations.o: ../lash-core/auto-determinize.h ../lash-core/auto-minimize.h
ndd-equations.o: ../lash-core/auto-product.h ../lash-core/auto-sequential.h
ndd-equations.o: ../lash-core/auto-difference.h ../lash-core/auto-normalize.h
ndd-equations.o: ../lash-core/auto-quotient.h ../lash-core/auto-count.h
ndd-equations.o: ../lash-core/biguint.h
ndd-projection.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
ndd-projection.o: /usr/include/features.h /usr/include/features-time64.h
ndd-projection.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
ndd-projection.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
ndd-projection.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
ndd-projection.o: /usr/include/stddef.h /usr/include/stdarg.h
ndd-projection.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
ndd-projection.o: /usr/include/bits/time64.h
ndd-projection.o: /usr/include/bits/types/__fpos_t.h
ndd-projection.o: /usr/include/bits/types/__mbstate_t.h
ndd-projection.o: /usr/include/bits/types/__fpos64_t.h
ndd-projection.o: /usr/include/bits/types/__FILE.h
ndd-projection.o: /usr/include/bits/types/FILE.h
ndd-projection.o: /usr/include/bits/types/struct_FILE.h
ndd-projection.o: /usr/include/bits/types/cookie_io_functions_t.h
ndd-projection.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
ndd-projection.o: /usr/include/bits/floatn-common.h /usr/include/string.h
ndd-projection.o: /usr/include/bits/types/locale_t.h
ndd-projection.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
ndd-projection.o: ndd.h lash-ndd.h ../lash-core/lash-auto.h
ndd-projection.o: /usr/include/stdlib.h /usr/include/bits/waitflags.h
ndd-projection.o: /usr/include/bits/waitstatus.h /usr/include/sys/types.h
ndd-projection.o: /usr/include/bits/types/clock_t.h
ndd-projection.o: /usr/include/bits/types/clockid_t.h
ndd-projection.o: /usr/include/bits/types/time_t.h
ndd-projection.o: /usr/include/bits/types/timer_t.h
ndd-projection.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
ndd-projection.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
ndd-projection.o: /usr/include/bits/byteswap.h
ndd-projection.o: /usr/include/bits/uintn-identity.h
ndd-projection.o: /usr/include/sys/select.h /usr/include/bits/select.h
ndd-projection.o: /usr/include/bits/types/sigset_t.h
ndd-projection.o: /usr/include/bits/types/__sigset_t.h
ndd-projection.o: /usr/include/bits/types/struct_timeval.h
ndd-projection.o: /usr/include/bits/types/struct_timespec.h
ndd-projection.o: /usr/include/bits/pthreadtypes.h
ndd-projection.o: /usr/include/bits/thread-shared-types.h
ndd-projection.o: /usr/include/bits/pthreadtypes-arch.h
ndd-projection.o: /usr/include/bits/atomic_wide_counter.h
ndd-projection.o: /usr/include/bits/struct_mutex.h
ndd-projection.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
ndd-projection.o: /usr/include/bits/stdlib-float.h ../lash-core/lash-types.h
ndd-projection.o: ../lash-core/lash.h ../lash-core/datastruct.h ndd-sets.h
ndd-projection.o: ndd-operations.h ndd-tests.h ndd-equations.h
ndd-projection.o: ndd-projection.h ndd-transformations.h ndd-iterations.h
ndd-projection.o: ndd-machines.h ../lash-core/auto.h ../lash-core/diag.h
ndd-projection.o: ../lash-core/lash-diag.h ../lash-core/resource.h
ndd-projection.o: ../lash-core/lash-auto-operations.h
ndd-projection.o: ../lash-core/auto-determinize.h
ndd-projection.o: ../lash-core/auto-minimize.h ../lash-core/auto-product.h
ndd-projection.o: ../lash-core/auto-sequential.h
ndd-projection.o: ../lash-core/auto-difference.h
ndd-projection.o: ../lash-core/auto-normalize.h ../lash-core/auto-quotient.h
ndd-projection.o: ../lash-core/auto-count.h ../lash-core/biguint.h
ndd-transformations.o: /usr/include/stdio.h
ndd-transformations.o: /usr/include/bits/libc-header-start.h
ndd-transformations.o: /usr/include/features.h /usr/include/features-time64.h
ndd-transformations.o: /usr/include/bits/wordsize.h
ndd-transformations.o: /usr/include/bits/timesize.h
ndd-transformations.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
ndd-transformations.o: /usr/include/bits/long-double.h
ndd-transformations.o: /usr/include/gnu/stubs.h /usr/include/stddef.h
ndd-transformations.o: /usr/include/stdarg.h /usr/include/bits/types.h
ndd-transformations.o: /usr/include/bits/typesizes.h
ndd-transformations.o: /usr/include/bits/time64.h
ndd-transformations.o: /usr/include/bits/types/__fpos_t.h
ndd-transformations.o: /usr/include/bits/types/__mbstate_t.h
ndd-transformations.o: /usr/include/bits/types/__fpos64_t.h
ndd-transformations.o: /usr/include/bits/types/__FILE.h
ndd-transformations.o: /usr/include/bits/types/FILE.h
ndd-transformations.o: /usr/include/bits/types/struct_FILE.h
ndd-transformations.o: /usr/include/bits/types/cookie_io_functions_t.h
ndd-transformations.o: /usr/include/bits/stdio_lim.h
ndd-transformations.o: /usr/include/bits/floatn.h
ndd-transformations.o: /usr/include/bits/floatn-common.h
ndd-transformations.o: /usr/include/string.h
ndd-transformations.o: /usr/include/bits/types/locale_t.h
ndd-transformations.o: /usr/include/bits/types/__locale_t.h
ndd-transformations.o: /usr/include/strings.h ndd.h lash-ndd.h
ndd-transformations.o: ../lash-core/lash-auto.h /usr/include/stdlib.h
ndd-transformations.o: /usr/include/bits/waitflags.h
ndd-transformations.o: /usr/include/bits/waitstatus.h
ndd-transformations.o: /usr/include/sys/types.h
ndd-transformations.o: /usr/include/bits/types/clock_t.h
ndd-transformations.o: /usr/include/bits/types/clockid_t.h
ndd-transformations.o: /usr/include/bits/types/time_t.h
ndd-transformations.o: /usr/include/bits/types/timer_t.h
ndd-transformations.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
ndd-transformations.o: /usr/include/bits/endian.h
ndd-transformations.o: /usr/include/bits/endianness.h
ndd-transformations.o: /usr/include/bits/byteswap.h
ndd-transformations.o: /usr/include/bits/uintn-identity.h
ndd-transformations.o: /usr/include/sys/select.h /usr/include/bits/select.h
ndd-transformations.o: /usr/include/bits/types/sigset_t.h
ndd-transformations.o: /usr/include/bits/types/__sigset_t.h
ndd-transformations.o: /usr/include/bits/types/struct_timeval.h
ndd-transformations.o: /usr/include/bits/types/struct_timespec.h
ndd-transformations.o: /usr/include/bits/pthreadtypes.h
ndd-transformations.o: /usr/include/bits/thread-shared-types.h
ndd-transformations.o: /usr/include/bits/pthreadtypes-arch.h
ndd-transformations.o: /usr/include/bits/atomic_wide_counter.h
ndd-transformations.o: /usr/include/bits/struct_mutex.h
ndd-transformations.o: /usr/include/bits/struct_rwlock.h
ndd-transformations.o: /usr/include/alloca.h /usr/include/bits/stdlib-float.h
ndd-transformations.o: ../lash-core/lash-types.h ../lash-core/lash.h
ndd-transformations.o: ../lash-core/datastruct.h ndd-sets.h ndd-operations.h
ndd-transformations.o: ndd-tests.h ndd-equations.h ndd-projection.h
ndd-transformations.o: ndd-transformations.h ndd-iterations.h ndd-machines.h
ndd-transformations.o: ../lash-core/auto.h ../lash-core/diag.h
ndd-transformations.o: ../lash-core/lash-diag.h ../lash-core/resource.h
ndd-transformations.o: ../lash-core/matrix.h ../lash-core/arithmetic.h
ndd-transformations.o: ../lash-core/lash-auto-operations.h
ndd-transformations.o: ../lash-core/auto-determinize.h
ndd-transformations.o: ../lash-core/auto-minimize.h
ndd-transformations.o: ../lash-core/auto-product.h
ndd-transformations.o: ../lash-core/auto-sequential.h
ndd-transformations.o: ../lash-core/auto-difference.h
ndd-transformations.o: ../lash-core/auto-normalize.h
ndd-transformations.o: ../lash-core/auto-quotient.h ../lash-core/auto-count.h
ndd-transformations.o: ../lash-core/biguint.h
ndd-iterations.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
ndd-iterations.o: /usr/include/features.h /usr/include/features-time64.h
ndd-iterations.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
ndd-iterations.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
ndd-iterations.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
ndd-iterations.o: /usr/include/stddef.h /usr/include/stdarg.h
ndd-iterations.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
ndd-iterations.o: /usr/include/bits/time64.h
ndd-iterations.o: /usr/include/bits/types/__fpos_t.h
ndd-iterations.o: /usr/include/bits/types/__mbstate_t.h
ndd-iterations.o: /usr/include/bits/types/__fpos64_t.h
ndd-iterations.o: /usr/include/bits/types/__FILE.h
ndd-iterations.o: /usr/include/bits/types/FILE.h
ndd-iterations.o: /usr/include/bits/types/struct_FILE.h
ndd-iterations.o: /usr/include/bits/types/cookie_io_functions_t.h
ndd-iterations.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
ndd-iterations.o: /usr/include/bits/floatn-common.h /usr/include/string.h
ndd-iterations.o: /usr/include/bits/types/locale_t.h
ndd-iterations.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
ndd-iterations.o: /usr/include/math.h /usr/include/bits/math-vector.h
ndd-iterations.o: /usr/include/bits/libm-simd-decl-stubs.h
ndd-iterations.o: /usr/include/bits/flt-eval-method.h
ndd-iterations.o: /usr/include/bits/fp-logb.h /usr/include/bits/fp-fast.h
ndd-iterations.o: /usr/include/bits/mathcalls-helper-functions.h
ndd-iterations.o: /usr/include/bits/mathcalls.h
ndd-iterations.o: /usr/include/bits/mathcalls-narrow.h
ndd-iterations.o: /usr/include/bits/iscanonical.h ndd.h lash-ndd.h
ndd-iterations.o: ../lash-core/lash-auto.h /usr/include/stdlib.h
ndd-iterations.o: /usr/include/bits/waitflags.h
ndd-iterations.o: /usr/include/bits/waitstatus.h /usr/include/sys/types.h
ndd-iterations.o: /usr/include/bits/types/clock_t.h
ndd-iterations.o: /usr/include/bits/types/clockid_t.h
ndd-iterations.o: /usr/include/bits/types/time_t.h
ndd-iterations.o: /usr/include/bits/types/timer_t.h
ndd-iterations.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
ndd-iterations.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
ndd-iterations.o: /usr/include/bits/byteswap.h
ndd-iterations.o: /usr/include/bits/uintn-identity.h
ndd-iterations.o: /usr/include/sys/select.h /usr/include/bits/select.h
ndd-iterations.o: /usr/include/bits/types/sigset_t.h
ndd-iterations.o: /usr/include/bits/types/__sigset_t.h
ndd-iterations.o: /usr/include/bits/types/struct_timeval.h
ndd-iterations.o: /usr/include/bits/types/struct_timespec.h
ndd-iterations.o: /usr/include/bits/pthreadtypes.h
ndd-iterations.o: /usr/include/bits/thread-shared-types.h
ndd-iterations.o: /usr/include/bits/pthreadtypes-arch.h
ndd-iterations.o: /usr/include/bits/atomic_wide_counter.h
ndd-iterations.o: /usr/include/bits/struct_mutex.h
ndd-iterations.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
ndd-iterations.o: /usr/include/bits/stdlib-float.h ../lash-core/lash-types.h
ndd-iterations.o: ../lash-core/lash.h ../lash-core/datastruct.h ndd-sets.h
ndd-iterations.o: ndd-operations.h ndd-tests.h ndd-equations.h
ndd-iterations.o: ndd-projection.h ndd-transformations.h ndd-iterations.h
ndd-iterations.o: ndd-machines.h ../lash-core/auto.h ../lash-core/diag.h
ndd-iterations.o: ../lash-core/lash-diag.h ../lash-core/resource.h
ndd-iterations.o: ../lash-core/arithmetic.h ../lash-core/cyclotomic.h
ndd-iterations.o: ../lash-core/matrix.h ../lash-core/lash-auto-operations.h
ndd-iterations.o: ../lash-core/auto-determinize.h
ndd-iterations.o: ../lash-core/auto-minimize.h ../lash-core/auto-product.h
ndd-iterations.o: ../lash-core/auto-sequential.h
ndd-iterations.o: ../lash-core/auto-difference.h
ndd-iterations.o: ../lash-core/auto-normalize.h ../lash-core/auto-quotient.h
ndd-iterations.o: ../lash-core/auto-count.h ../lash-core/biguint.h
ndd-machines.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
ndd-machines.o: /usr/include/features.h /usr/include/features-time64.h
ndd-machines.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
ndd-machines.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
ndd-machines.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
ndd-machines.o: /usr/include/stddef.h /usr/include/stdarg.h
ndd-machines.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
ndd-machines.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
ndd-machines.o: /usr/include/bits/types/__mbstate_t.h
ndd-machines.o: /usr/include/bits/types/__fpos64_t.h
ndd-machines.o: /usr/include/bits/types/__FILE.h
ndd-machines.o: /usr/include/bits/types/FILE.h
ndd-machines.o: /usr/include/bits/types/struct_FILE.h
ndd-machines.o: /usr/include/bits/types/cookie_io_functions_t.h
ndd-machines.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
ndd-machines.o: /usr/include/bits/floatn-common.h /usr/include/string.h
ndd-machines.o: /usr/include/bits/types/locale_t.h
ndd-machines.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
ndd-machines.o: ../lash-core/lash-auto-operations.h ../lash-core/lash-auto.h
ndd-machines.o: /usr/include/stdlib.h /usr/include/bits/waitflags.h
ndd-machines.o: /usr/include/bits/waitstatus.h /usr/include/sys/types.h
ndd-machines.o: /usr/include/bits/types/clock_t.h
ndd-machines.o: /usr/include/bits/types/clockid_t.h
ndd-machines.o: /usr/include/bits/types/time_t.h
ndd-machines.o: /usr/include/bits/types/timer_t.h
ndd-machines.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
ndd-machines.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
ndd-machines.o: /usr/include/bits/byteswap.h
ndd-machines.o: /usr/include/bits/uintn-identity.h /usr/include/sys/select.h
ndd-machines.o: /usr/include/bits/select.h /usr/include/bits/types/sigset_t.h
ndd-machines.o: /usr/include/bits/types/__sigset_t.h
ndd-machines.o: /usr/include/bits/types/struct_timeval.h
ndd-machines.o: /usr/include/bits/types/struct_timespec.h
ndd-machines.o: /usr/include/bits/pthreadtypes.h
ndd-machines.o: /usr/include/bits/thread-shared-types.h
ndd-machines.o: /usr/include/bits/pthreadtypes-arch.h
ndd-machines.o: /usr/include/bits/atomic_wide_counter.h
ndd-machines.o: /usr/include/bits/struct_mutex.h
ndd-machines.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
ndd-machines.o: /usr/include/bits/stdlib-float.h ../lash-core/lash-types.h
ndd-machines.o: ../lash-core/lash.h ../lash-core/datastruct.h
ndd-machines.o: ../lash-core/auto-determinize.h ../lash-core/auto-minimize.h
ndd-machines.o: ../lash-core/auto-product.h ../lash-core/auto-sequential.h
ndd-machines.o: ../lash-core/auto-difference.h ../lash-core/auto-normalize.h
ndd-machines.o: ../lash-core/auto-quotient.h ../lash-core/auto-count.h
ndd-machines.o: ../lash-core/biguint.h ndd.h lash-ndd.h ndd-sets.h
ndd-machines.o: ndd-operations.h ndd-tests.h ndd-equations.h ndd-projection.h
ndd-machines.o: ndd-transformations.h ndd-iterations.h ndd-machines.h
ndd-machines.o: ../lash-core/auto.h ../lash-core/diag.h
ndd-machines.o: ../lash-core/lash-diag.h ../lash-core/resource.h
ndd-io.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
ndd-io.o: /usr/include/features.h /usr/include/features-time64.h
ndd-io.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
ndd-io.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
ndd-io.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
ndd-io.o: /usr/include/stddef.h /usr/include/stdarg.h
ndd-io.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
ndd-io.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
ndd-io.o: /usr/include/bits/types/__mbstate_t.h
ndd-io.o: /usr/include/bits/types/__fpos64_t.h
ndd-io.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
ndd-io.o: /usr/include/bits/types/struct_FILE.h
ndd-io.o: /usr/include/bits/types/cookie_io_functions_t.h
ndd-io.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
ndd-io.o: /usr/include/bits/floatn-common.h /usr/include/string.h
ndd-io.o: /usr/include/bits/types/locale_t.h
ndd-io.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
ndd-io.o: ../lash-core/lash-io.h ../lash-core/lash-auto.h
ndd-io.o: /usr/include/stdlib.h /usr/include/bits/waitflags.h
ndd-io.o: /usr/include/bits/waitstatus.h /usr/include/sys/types.h
ndd-io.o: /usr/include/bits/types/clock_t.h
ndd-io.o: /usr/include/bits/types/clockid_t.h
ndd-io.o: /usr/include/bits/types/time_t.h /usr/include/bits/types/timer_t.h
ndd-io.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
ndd-io.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
ndd-io.o: /usr/include/bits/byteswap.h /usr/include/bits/uintn-identity.h
ndd-io.o: /usr/include/sys/select.h /usr/include/bits/select.h
ndd-io.o: /usr/include/bits/types/sigset_t.h
ndd-io.o: /usr/include/bits/types/__sigset_t.h
ndd-io.o: /usr/include/bits/types/struct_timeval.h
ndd-io.o: /usr/include/bits/types/struct_timespec.h
ndd-io.o: /usr/include/bits/pthreadtypes.h
ndd-io.o: /usr/include/bits/thread-shared-types.h
ndd-io.o: /usr/include/bits/pthreadtypes-arch.h
ndd-io.o: /usr/include/bits/atomic_wide_counter.h
ndd-io.o: /usr/include/bits/struct_mutex.h /usr/include/bits/struct_rwlock.h
ndd-io.o: /usr/include/alloca.h /usr/include/bits/stdlib-float.h
ndd-io.o: ../lash-core/lash-types.h ../lash-core/lash.h
ndd-io.o: ../lash-core/datastruct.h ../lash-core/datastruct-io.h
ndd-io.o: ../lash-core/lash-auto-io.h ../lash-core/auto-io-print.h
ndd-io.o: ../lash-core/auto-io-dots.h lash-ndd-io.h lash-ndd.h ndd-sets.h
ndd-io.o: ndd.h ../lash-core/auto.h ndd-operations.h ndd-tests.h
ndd-io.o: ndd-equations.h ndd-projection.h ndd-transformations.h
ndd-io.o: ndd-iterations.h ndd-machines.h ndd-io.h ../lash-core/io-ops.h
ndd-io.o: ../lash-core/diag.h ../lash-core/lash-diag.h
ndd-io.o: ../lash-core/resource.h
ndd-count.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
ndd-count.o: /usr/include/features.h /usr/include/features-time64.h
ndd-count.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
ndd-count.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
ndd-count.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
ndd-count.o: /usr/include/stddef.h /usr/include/stdarg.h
ndd-count.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
ndd-count.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
ndd-count.o: /usr/include/bits/types/__mbstate_t.h
ndd-count.o: /usr/include/bits/types/__fpos64_t.h
ndd-count.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
ndd-count.o: /usr/include/bits/types/struct_FILE.h
ndd-count.o: /usr/include/bits/types/cookie_io_functions_t.h
ndd-count.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
ndd-count.o: /usr/include/bits/floatn-common.h ndd.h lash-ndd.h
ndd-count.o: ../lash-core/lash-auto.h /usr/include/stdlib.h
ndd-count.o: /usr/include/bits/waitflags.h /usr/include/bits/waitstatus.h
ndd-count.o: /usr/include/sys/types.h /usr/include/bits/types/clock_t.h
ndd-count.o: /usr/include/bits/types/clockid_t.h
ndd-count.o: /usr/include/bits/types/time_t.h
ndd-count.o: /usr/include/bits/types/timer_t.h
ndd-count.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
ndd-count.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
ndd-count.o: /usr/include/bits/byteswap.h /usr/include/bits/uintn-identity.h
ndd-count.o: /usr/include/sys/select.h /usr/include/bits/select.h
ndd-count.o: /usr/include/bits/types/sigset_t.h
ndd-count.o: /usr/include/bits/types/__sigset_t.h
ndd-count.o: /usr/include/bits/types/struct_timeval.h
ndd-count.o: /usr/include/bits/types/struct_timespec.h
ndd-count.o: /usr/include/bits/pthreadtypes.h
ndd-count.o: /usr/include/bits/thread-shared-types.h
ndd-count.o: /usr/include/bits/pthreadtypes-arch.h
ndd-count.o: /usr/include/bits/atomic_wide_counter.h
ndd-count.o: /usr/include/bits/struct_mutex.h
ndd-count.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
ndd-count.o: /usr/include/bits/stdlib-float.h ../lash-core/lash-types.h
ndd-count.o: ../lash-core/lash.h ../lash-core/datastruct.h
ndd-count.o: /usr/include/string.h /usr/include/bits/types/locale_t.h
ndd-count.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
ndd-count.o: ndd-sets.h ndd-operations.h ndd-tests.h ndd-equations.h
ndd-count.o: ndd-projection.h ndd-transformations.h ndd-iterations.h
ndd-count.o: ndd-machines.h ../lash-core/auto.h ../lash-core/biguint.h
ndd-count.o: ../lash-core/auto-count.h ../lash-core/diag.h
ndd-count.o: ../lash-core/lash-diag.h ../lash-core/resource.h
ndd-count.o: ../lash-core/lash-auto-io.h ../lash-core/lash-io.h
ndd-count.o: ../lash-core/datastruct-io.h ../lash-core/auto-io-print.h
ndd-count.o: ../lash-core/auto-io-dots.h ../lash-core/lash-auto-operations.h
ndd-count.o: ../lash-core/auto-determinize.h ../lash-core/auto-minimize.h
ndd-count.o: ../lash-core/auto-product.h ../lash-core/auto-sequential.h
ndd-count.o: ../lash-core/auto-difference.h ../lash-core/auto-normalize.h
ndd-count.o: ../lash-core/auto-quotient.h ../lash-core/arithmetic.h
ndd-count.o: ../lash-core/auto-depth.h ../lash-core/auto-sort.h ndd-count.h
ndd-congruence.o: ndd-congruence.h ndd.h lash-ndd.h ../lash-core/lash-auto.h
ndd-congruence.o: /usr/include/stdlib.h /usr/include/bits/libc-header-start.h
ndd-congruence.o: /usr/include/features.h /usr/include/features-time64.h
ndd-congruence.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
ndd-congruence.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
ndd-congruence.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
ndd-congruence.o: /usr/include/stddef.h /usr/include/bits/waitflags.h
ndd-congruence.o: /usr/include/bits/waitstatus.h /usr/include/bits/floatn.h
ndd-congruence.o: /usr/include/bits/floatn-common.h /usr/include/sys/types.h
ndd-congruence.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
ndd-congruence.o: /usr/include/bits/time64.h
ndd-congruence.o: /usr/include/bits/types/clock_t.h
ndd-congruence.o: /usr/include/bits/types/clockid_t.h
ndd-congruence.o: /usr/include/bits/types/time_t.h
ndd-congruence.o: /usr/include/bits/types/timer_t.h
ndd-congruence.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
ndd-congruence.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
ndd-congruence.o: /usr/include/bits/byteswap.h
ndd-congruence.o: /usr/include/bits/uintn-identity.h
ndd-congruence.o: /usr/include/sys/select.h /usr/include/bits/select.h
ndd-congruence.o: /usr/include/bits/types/sigset_t.h
ndd-congruence.o: /usr/include/bits/types/__sigset_t.h
ndd-congruence.o: /usr/include/bits/types/struct_timeval.h
ndd-congruence.o: /usr/include/bits/types/struct_timespec.h
ndd-congruence.o: /usr/include/bits/pthreadtypes.h
ndd-congruence.o: /usr/include/bits/thread-shared-types.h
ndd-congruence.o: /usr/include/bits/pthreadtypes-arch.h
ndd-congruence.o: /usr/include/bits/atomic_wide_counter.h
ndd-congruence.o: /usr/include/bits/struct_mutex.h
ndd-congruence.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
ndd-congruence.o: /usr/include/bits/stdlib-float.h ../lash-core/lash-types.h
ndd-congruence.o: ../lash-core/lash.h ../lash-core/datastruct.h
ndd-congruence.o: /usr/include/string.h /usr/include/bits/types/locale_t.h
ndd-congruence.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
ndd-congruence.o: ndd-sets.h ndd-operations.h ndd-tests.h ndd-equations.h
ndd-congruence.o: ndd-projection.h ndd-transformations.h ndd-iterations.h
ndd-congruence.o: ndd-machines.h ../lash-core/auto.h ../lash-core/diag.h
ndd-congruence.o: ../lash-core/lash-diag.h ../lash-core/resource.h
ndd-congruence.o: ../lash-core/lash-auto-operations.h
ndd-congruence.o: ../lash-core/auto-determinize.h
ndd-congruence.o: ../lash-core/auto-minimize.h ../lash-core/auto-product.h
ndd-congruence.o: ../lash-core/auto-sequential.h
ndd-congruence.o: ../lash-core/auto-difference.h
ndd-congruence.o: ../lash-core/auto-normalize.h ../lash-core/auto-quotient.h
ndd-congruence.o: ../lash-core/auto-count.h ../lash-core/biguint.h
ndd-congruence.o: ../lash-core/arithmetic.h ../lash-core/lash-auto-io.h
ndd-congruence.o: ../lash-core/lash-io.h /usr/include/stdio.h
ndd-congruence.o: /usr/include/stdarg.h /usr/include/bits/types/__fpos_t.h
ndd-congruence.o: /usr/include/bits/types/__mbstate_t.h
ndd-congruence.o: /usr/include/bits/types/__fpos64_t.h
ndd-congruence.o: /usr/include/bits/types/__FILE.h
ndd-congruence.o: /usr/include/bits/types/FILE.h
ndd-congruence.o: /usr/include/bits/types/struct_FILE.h
ndd-congruence.o: /usr/include/bits/types/cookie_io_functions_t.h
ndd-congruence.o: /usr/include/bits/stdio_lim.h ../lash-core/datastruct-io.h
ndd-congruence.o: ../lash-core/auto-io-print.h ../lash-core/auto-io-dots.h
