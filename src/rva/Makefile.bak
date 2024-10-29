######################################################################
##                                                                  ##
##  RVA package -- v0.9                                             ##
##  ============                                                    ##
##                                                                  ##
##     Makefile.                                                    ##
##                                                                  ##
##     05/04/01  :  Creation. (SJ)                                  ##
##     08/06/01  :  Reorganization. (SJ)                            ##
##     08/20/01  :  Addition of 'rva-io-print'. (SJ)                ##
##     10/19/01  :  Usage of the NDD package. (SJ)                  ##
##     11/02/01  :  Addition of 'rva-shifting'. (SJ)                ##
##     11/07/01  :  Addition of 'rva-tools'. (SJ)                   ##
##     06/28/02  :  'auto-weak-convert' removed. (SJ)               ##
##     07/10/02  :  Reorganization. (BB)                            ##
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

SRCS-RVA =	rva-equations.c \
		rva-io.c \
		rva-io-print.c \
		rva-operations.c \
		rva-projection.c \
		rva-sets.c \
		rva-shifting.c \
		rva-tests.c \
		rva-tools.c \
		rva-transformations.c \
		rva.c

OBJS-RVA =	${SRCS-RVA:.c=.o}

CFLAGS =  -g -c -O3 -ansi -Wall -Wtraditional -Wpointer-arith \
         -Waggregate-return -Wstrict-prototypes \
         -Wmissing-prototypes -Wmissing-declarations \
         -I$(LASH-CORE-DIR)

all:  rva-package

rva-package: $(OBJS-RVA)
	ld -r -o rva-package.o $(OBJS-RVA)
	mv rva-package.o $(LASH-LIB-DIR)

clean:
	-rm -f $(OBJS-RVA) rva-package.o
	-rm -f $(LASH-LIB-DIR)/rva-package.o

depend: 
	makedepend -I$(LASH-CORE-DIR) $(SRCS-RVA)

# DO NOT DELETE THIS LINE -- make depend depends on it.

rva-equations.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
rva-equations.o: /usr/include/features.h /usr/include/features-time64.h
rva-equations.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
rva-equations.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
rva-equations.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
rva-equations.o: /usr/include/stddef.h /usr/include/stdarg.h
rva-equations.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
rva-equations.o: /usr/include/bits/time64.h
rva-equations.o: /usr/include/bits/types/__fpos_t.h
rva-equations.o: /usr/include/bits/types/__mbstate_t.h
rva-equations.o: /usr/include/bits/types/__fpos64_t.h
rva-equations.o: /usr/include/bits/types/__FILE.h
rva-equations.o: /usr/include/bits/types/FILE.h
rva-equations.o: /usr/include/bits/types/struct_FILE.h
rva-equations.o: /usr/include/bits/types/cookie_io_functions_t.h
rva-equations.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
rva-equations.o: /usr/include/bits/floatn-common.h rva.h lash-rva.h
rva-equations.o: ../lash-core/lash-auto.h /usr/include/stdlib.h
rva-equations.o: /usr/include/bits/waitflags.h /usr/include/bits/waitstatus.h
rva-equations.o: /usr/include/sys/types.h /usr/include/bits/types/clock_t.h
rva-equations.o: /usr/include/bits/types/clockid_t.h
rva-equations.o: /usr/include/bits/types/time_t.h
rva-equations.o: /usr/include/bits/types/timer_t.h
rva-equations.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
rva-equations.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
rva-equations.o: /usr/include/bits/byteswap.h
rva-equations.o: /usr/include/bits/uintn-identity.h /usr/include/sys/select.h
rva-equations.o: /usr/include/bits/select.h
rva-equations.o: /usr/include/bits/types/sigset_t.h
rva-equations.o: /usr/include/bits/types/__sigset_t.h
rva-equations.o: /usr/include/bits/types/struct_timeval.h
rva-equations.o: /usr/include/bits/types/struct_timespec.h
rva-equations.o: /usr/include/bits/pthreadtypes.h
rva-equations.o: /usr/include/bits/thread-shared-types.h
rva-equations.o: /usr/include/bits/pthreadtypes-arch.h
rva-equations.o: /usr/include/bits/atomic_wide_counter.h
rva-equations.o: /usr/include/bits/struct_mutex.h
rva-equations.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
rva-equations.o: /usr/include/bits/stdlib-float.h ../lash-core/lash-types.h
rva-equations.o: ../lash-core/lash.h ../lash-core/datastruct.h
rva-equations.o: /usr/include/string.h /usr/include/bits/types/locale_t.h
rva-equations.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
rva-equations.o: ../lash-core/auto-weak-convert.h rva-equations.h
rva-equations.o: rva-operations.h rva-projection.h rva-sets.h rva-shifting.h
rva-equations.o: rva-tests.h rva-tools.h rva-transformations.h
rva-equations.o: ../lash-core/auto.h ../lash-core/diag.h
rva-equations.o: ../lash-core/lash-diag.h ../lash-core/resource.h
rva-equations.o: ../lash-core/arithmetic.h
rva-equations.o: ../lash-core/lash-auto-operations.h
rva-equations.o: ../lash-core/auto-determinize.h ../lash-core/auto-minimize.h
rva-equations.o: ../lash-core/auto-product.h ../lash-core/auto-sequential.h
rva-equations.o: ../lash-core/auto-difference.h ../lash-core/auto-normalize.h
rva-equations.o: ../lash-core/auto-quotient.h ../lash-core/auto-count.h
rva-equations.o: ../lash-core/biguint.h
rva-io.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
rva-io.o: /usr/include/features.h /usr/include/features-time64.h
rva-io.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
rva-io.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
rva-io.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
rva-io.o: /usr/include/stddef.h /usr/include/stdarg.h
rva-io.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
rva-io.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
rva-io.o: /usr/include/bits/types/__mbstate_t.h
rva-io.o: /usr/include/bits/types/__fpos64_t.h
rva-io.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
rva-io.o: /usr/include/bits/types/struct_FILE.h
rva-io.o: /usr/include/bits/types/cookie_io_functions_t.h
rva-io.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
rva-io.o: /usr/include/bits/floatn-common.h /usr/include/string.h
rva-io.o: /usr/include/bits/types/locale_t.h
rva-io.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
rva-io.o: ../lash-core/lash-io.h ../lash-core/lash-auto.h
rva-io.o: /usr/include/stdlib.h /usr/include/bits/waitflags.h
rva-io.o: /usr/include/bits/waitstatus.h /usr/include/sys/types.h
rva-io.o: /usr/include/bits/types/clock_t.h
rva-io.o: /usr/include/bits/types/clockid_t.h
rva-io.o: /usr/include/bits/types/time_t.h /usr/include/bits/types/timer_t.h
rva-io.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
rva-io.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
rva-io.o: /usr/include/bits/byteswap.h /usr/include/bits/uintn-identity.h
rva-io.o: /usr/include/sys/select.h /usr/include/bits/select.h
rva-io.o: /usr/include/bits/types/sigset_t.h
rva-io.o: /usr/include/bits/types/__sigset_t.h
rva-io.o: /usr/include/bits/types/struct_timeval.h
rva-io.o: /usr/include/bits/types/struct_timespec.h
rva-io.o: /usr/include/bits/pthreadtypes.h
rva-io.o: /usr/include/bits/thread-shared-types.h
rva-io.o: /usr/include/bits/pthreadtypes-arch.h
rva-io.o: /usr/include/bits/atomic_wide_counter.h
rva-io.o: /usr/include/bits/struct_mutex.h /usr/include/bits/struct_rwlock.h
rva-io.o: /usr/include/alloca.h /usr/include/bits/stdlib-float.h
rva-io.o: ../lash-core/lash-types.h ../lash-core/lash.h
rva-io.o: ../lash-core/datastruct.h ../lash-core/datastruct-io.h
rva-io.o: ../lash-core/lash-auto-io.h ../lash-core/auto-io-print.h
rva-io.o: ../lash-core/auto-io-dots.h lash-rva-io.h lash-rva.h
rva-io.o: ../lash-core/auto-weak-convert.h rva-equations.h rva.h
rva-io.o: ../lash-core/auto.h rva-operations.h rva-projection.h rva-sets.h
rva-io.o: rva-shifting.h rva-tests.h rva-tools.h rva-transformations.h
rva-io.o: rva-io-print.h rva-io.h ../lash-core/io-ops.h ../lash-core/diag.h
rva-io.o: ../lash-core/lash-diag.h ../lash-core/resource.h
rva-io-print.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
rva-io-print.o: /usr/include/features.h /usr/include/features-time64.h
rva-io-print.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
rva-io-print.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
rva-io-print.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
rva-io-print.o: /usr/include/stddef.h /usr/include/stdarg.h
rva-io-print.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
rva-io-print.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
rva-io-print.o: /usr/include/bits/types/__mbstate_t.h
rva-io-print.o: /usr/include/bits/types/__fpos64_t.h
rva-io-print.o: /usr/include/bits/types/__FILE.h
rva-io-print.o: /usr/include/bits/types/FILE.h
rva-io-print.o: /usr/include/bits/types/struct_FILE.h
rva-io-print.o: /usr/include/bits/types/cookie_io_functions_t.h
rva-io-print.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
rva-io-print.o: /usr/include/bits/floatn-common.h rva-io-print.h lash-rva.h
rva-io-print.o: ../lash-core/lash-auto.h /usr/include/stdlib.h
rva-io-print.o: /usr/include/bits/waitflags.h /usr/include/bits/waitstatus.h
rva-io-print.o: /usr/include/sys/types.h /usr/include/bits/types/clock_t.h
rva-io-print.o: /usr/include/bits/types/clockid_t.h
rva-io-print.o: /usr/include/bits/types/time_t.h
rva-io-print.o: /usr/include/bits/types/timer_t.h
rva-io-print.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
rva-io-print.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
rva-io-print.o: /usr/include/bits/byteswap.h
rva-io-print.o: /usr/include/bits/uintn-identity.h /usr/include/sys/select.h
rva-io-print.o: /usr/include/bits/select.h /usr/include/bits/types/sigset_t.h
rva-io-print.o: /usr/include/bits/types/__sigset_t.h
rva-io-print.o: /usr/include/bits/types/struct_timeval.h
rva-io-print.o: /usr/include/bits/types/struct_timespec.h
rva-io-print.o: /usr/include/bits/pthreadtypes.h
rva-io-print.o: /usr/include/bits/thread-shared-types.h
rva-io-print.o: /usr/include/bits/pthreadtypes-arch.h
rva-io-print.o: /usr/include/bits/atomic_wide_counter.h
rva-io-print.o: /usr/include/bits/struct_mutex.h
rva-io-print.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
rva-io-print.o: /usr/include/bits/stdlib-float.h ../lash-core/lash-types.h
rva-io-print.o: ../lash-core/lash.h ../lash-core/datastruct.h
rva-io-print.o: /usr/include/string.h /usr/include/bits/types/locale_t.h
rva-io-print.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
rva-io-print.o: ../lash-core/auto-weak-convert.h rva-equations.h rva.h
rva-io-print.o: ../lash-core/auto.h rva-operations.h rva-projection.h
rva-io-print.o: rva-sets.h rva-shifting.h rva-tests.h rva-tools.h
rva-io-print.o: rva-transformations.h ../lash-core/diag.h
rva-io-print.o: ../lash-core/lash-diag.h ../lash-core/auto-io-print.h
rva-operations.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
rva-operations.o: /usr/include/features.h /usr/include/features-time64.h
rva-operations.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
rva-operations.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
rva-operations.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
rva-operations.o: /usr/include/stddef.h /usr/include/stdarg.h
rva-operations.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
rva-operations.o: /usr/include/bits/time64.h
rva-operations.o: /usr/include/bits/types/__fpos_t.h
rva-operations.o: /usr/include/bits/types/__mbstate_t.h
rva-operations.o: /usr/include/bits/types/__fpos64_t.h
rva-operations.o: /usr/include/bits/types/__FILE.h
rva-operations.o: /usr/include/bits/types/FILE.h
rva-operations.o: /usr/include/bits/types/struct_FILE.h
rva-operations.o: /usr/include/bits/types/cookie_io_functions_t.h
rva-operations.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
rva-operations.o: /usr/include/bits/floatn-common.h rva.h lash-rva.h
rva-operations.o: ../lash-core/lash-auto.h /usr/include/stdlib.h
rva-operations.o: /usr/include/bits/waitflags.h
rva-operations.o: /usr/include/bits/waitstatus.h /usr/include/sys/types.h
rva-operations.o: /usr/include/bits/types/clock_t.h
rva-operations.o: /usr/include/bits/types/clockid_t.h
rva-operations.o: /usr/include/bits/types/time_t.h
rva-operations.o: /usr/include/bits/types/timer_t.h
rva-operations.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
rva-operations.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
rva-operations.o: /usr/include/bits/byteswap.h
rva-operations.o: /usr/include/bits/uintn-identity.h
rva-operations.o: /usr/include/sys/select.h /usr/include/bits/select.h
rva-operations.o: /usr/include/bits/types/sigset_t.h
rva-operations.o: /usr/include/bits/types/__sigset_t.h
rva-operations.o: /usr/include/bits/types/struct_timeval.h
rva-operations.o: /usr/include/bits/types/struct_timespec.h
rva-operations.o: /usr/include/bits/pthreadtypes.h
rva-operations.o: /usr/include/bits/thread-shared-types.h
rva-operations.o: /usr/include/bits/pthreadtypes-arch.h
rva-operations.o: /usr/include/bits/atomic_wide_counter.h
rva-operations.o: /usr/include/bits/struct_mutex.h
rva-operations.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
rva-operations.o: /usr/include/bits/stdlib-float.h ../lash-core/lash-types.h
rva-operations.o: ../lash-core/lash.h ../lash-core/datastruct.h
rva-operations.o: /usr/include/string.h /usr/include/bits/types/locale_t.h
rva-operations.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
rva-operations.o: ../lash-core/auto-weak-convert.h rva-equations.h
rva-operations.o: rva-operations.h rva-projection.h rva-sets.h rva-shifting.h
rva-operations.o: rva-tests.h rva-tools.h rva-transformations.h
rva-operations.o: ../lash-core/auto.h ../lash-core/diag.h
rva-operations.o: ../lash-core/lash-diag.h ../lash-core/resource.h
rva-operations.o: ../lash-core/lash-auto-operations.h
rva-operations.o: ../lash-core/auto-determinize.h
rva-operations.o: ../lash-core/auto-minimize.h ../lash-core/auto-product.h
rva-operations.o: ../lash-core/auto-sequential.h
rva-operations.o: ../lash-core/auto-difference.h
rva-operations.o: ../lash-core/auto-normalize.h ../lash-core/auto-quotient.h
rva-operations.o: ../lash-core/auto-count.h ../lash-core/biguint.h
rva-projection.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
rva-projection.o: /usr/include/features.h /usr/include/features-time64.h
rva-projection.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
rva-projection.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
rva-projection.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
rva-projection.o: /usr/include/stddef.h /usr/include/stdarg.h
rva-projection.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
rva-projection.o: /usr/include/bits/time64.h
rva-projection.o: /usr/include/bits/types/__fpos_t.h
rva-projection.o: /usr/include/bits/types/__mbstate_t.h
rva-projection.o: /usr/include/bits/types/__fpos64_t.h
rva-projection.o: /usr/include/bits/types/__FILE.h
rva-projection.o: /usr/include/bits/types/FILE.h
rva-projection.o: /usr/include/bits/types/struct_FILE.h
rva-projection.o: /usr/include/bits/types/cookie_io_functions_t.h
rva-projection.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
rva-projection.o: /usr/include/bits/floatn-common.h /usr/include/string.h
rva-projection.o: /usr/include/bits/types/locale_t.h
rva-projection.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
rva-projection.o: rva.h lash-rva.h ../lash-core/lash-auto.h
rva-projection.o: /usr/include/stdlib.h /usr/include/bits/waitflags.h
rva-projection.o: /usr/include/bits/waitstatus.h /usr/include/sys/types.h
rva-projection.o: /usr/include/bits/types/clock_t.h
rva-projection.o: /usr/include/bits/types/clockid_t.h
rva-projection.o: /usr/include/bits/types/time_t.h
rva-projection.o: /usr/include/bits/types/timer_t.h
rva-projection.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
rva-projection.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
rva-projection.o: /usr/include/bits/byteswap.h
rva-projection.o: /usr/include/bits/uintn-identity.h
rva-projection.o: /usr/include/sys/select.h /usr/include/bits/select.h
rva-projection.o: /usr/include/bits/types/sigset_t.h
rva-projection.o: /usr/include/bits/types/__sigset_t.h
rva-projection.o: /usr/include/bits/types/struct_timeval.h
rva-projection.o: /usr/include/bits/types/struct_timespec.h
rva-projection.o: /usr/include/bits/pthreadtypes.h
rva-projection.o: /usr/include/bits/thread-shared-types.h
rva-projection.o: /usr/include/bits/pthreadtypes-arch.h
rva-projection.o: /usr/include/bits/atomic_wide_counter.h
rva-projection.o: /usr/include/bits/struct_mutex.h
rva-projection.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
rva-projection.o: /usr/include/bits/stdlib-float.h ../lash-core/lash-types.h
rva-projection.o: ../lash-core/lash.h ../lash-core/datastruct.h
rva-projection.o: ../lash-core/auto-weak-convert.h rva-equations.h
rva-projection.o: rva-operations.h rva-projection.h rva-sets.h rva-shifting.h
rva-projection.o: rva-tests.h rva-tools.h rva-transformations.h
rva-projection.o: ../lash-core/auto.h ../lash-core/diag.h
rva-projection.o: ../lash-core/lash-diag.h ../lash-core/resource.h
rva-projection.o: ../lash-core/lash-auto-operations.h
rva-projection.o: ../lash-core/auto-determinize.h
rva-projection.o: ../lash-core/auto-minimize.h ../lash-core/auto-product.h
rva-projection.o: ../lash-core/auto-sequential.h
rva-projection.o: ../lash-core/auto-difference.h
rva-projection.o: ../lash-core/auto-normalize.h ../lash-core/auto-quotient.h
rva-projection.o: ../lash-core/auto-count.h ../lash-core/biguint.h
rva-sets.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
rva-sets.o: /usr/include/features.h /usr/include/features-time64.h
rva-sets.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
rva-sets.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
rva-sets.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
rva-sets.o: /usr/include/stddef.h /usr/include/stdarg.h
rva-sets.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
rva-sets.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
rva-sets.o: /usr/include/bits/types/__mbstate_t.h
rva-sets.o: /usr/include/bits/types/__fpos64_t.h
rva-sets.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
rva-sets.o: /usr/include/bits/types/struct_FILE.h
rva-sets.o: /usr/include/bits/types/cookie_io_functions_t.h
rva-sets.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
rva-sets.o: /usr/include/bits/floatn-common.h rva.h lash-rva.h
rva-sets.o: ../lash-core/lash-auto.h /usr/include/stdlib.h
rva-sets.o: /usr/include/bits/waitflags.h /usr/include/bits/waitstatus.h
rva-sets.o: /usr/include/sys/types.h /usr/include/bits/types/clock_t.h
rva-sets.o: /usr/include/bits/types/clockid_t.h
rva-sets.o: /usr/include/bits/types/time_t.h
rva-sets.o: /usr/include/bits/types/timer_t.h /usr/include/bits/stdint-intn.h
rva-sets.o: /usr/include/endian.h /usr/include/bits/endian.h
rva-sets.o: /usr/include/bits/endianness.h /usr/include/bits/byteswap.h
rva-sets.o: /usr/include/bits/uintn-identity.h /usr/include/sys/select.h
rva-sets.o: /usr/include/bits/select.h /usr/include/bits/types/sigset_t.h
rva-sets.o: /usr/include/bits/types/__sigset_t.h
rva-sets.o: /usr/include/bits/types/struct_timeval.h
rva-sets.o: /usr/include/bits/types/struct_timespec.h
rva-sets.o: /usr/include/bits/pthreadtypes.h
rva-sets.o: /usr/include/bits/thread-shared-types.h
rva-sets.o: /usr/include/bits/pthreadtypes-arch.h
rva-sets.o: /usr/include/bits/atomic_wide_counter.h
rva-sets.o: /usr/include/bits/struct_mutex.h
rva-sets.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
rva-sets.o: /usr/include/bits/stdlib-float.h ../lash-core/lash-types.h
rva-sets.o: ../lash-core/lash.h ../lash-core/datastruct.h
rva-sets.o: /usr/include/string.h /usr/include/bits/types/locale_t.h
rva-sets.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
rva-sets.o: ../lash-core/auto-weak-convert.h rva-equations.h rva-operations.h
rva-sets.o: rva-projection.h rva-sets.h rva-shifting.h rva-tests.h
rva-sets.o: rva-tools.h rva-transformations.h ../lash-core/auto.h
rva-sets.o: ../lash-core/diag.h ../lash-core/lash-diag.h
rva-sets.o: ../lash-core/resource.h ../lash-core/lash-auto-operations.h
rva-sets.o: ../lash-core/auto-determinize.h ../lash-core/auto-minimize.h
rva-sets.o: ../lash-core/auto-product.h ../lash-core/auto-sequential.h
rva-sets.o: ../lash-core/auto-difference.h ../lash-core/auto-normalize.h
rva-sets.o: ../lash-core/auto-quotient.h ../lash-core/auto-count.h
rva-sets.o: ../lash-core/biguint.h
rva-shifting.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
rva-shifting.o: /usr/include/features.h /usr/include/features-time64.h
rva-shifting.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
rva-shifting.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
rva-shifting.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
rva-shifting.o: /usr/include/stddef.h /usr/include/stdarg.h
rva-shifting.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
rva-shifting.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
rva-shifting.o: /usr/include/bits/types/__mbstate_t.h
rva-shifting.o: /usr/include/bits/types/__fpos64_t.h
rva-shifting.o: /usr/include/bits/types/__FILE.h
rva-shifting.o: /usr/include/bits/types/FILE.h
rva-shifting.o: /usr/include/bits/types/struct_FILE.h
rva-shifting.o: /usr/include/bits/types/cookie_io_functions_t.h
rva-shifting.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
rva-shifting.o: /usr/include/bits/floatn-common.h rva.h lash-rva.h
rva-shifting.o: ../lash-core/lash-auto.h /usr/include/stdlib.h
rva-shifting.o: /usr/include/bits/waitflags.h /usr/include/bits/waitstatus.h
rva-shifting.o: /usr/include/sys/types.h /usr/include/bits/types/clock_t.h
rva-shifting.o: /usr/include/bits/types/clockid_t.h
rva-shifting.o: /usr/include/bits/types/time_t.h
rva-shifting.o: /usr/include/bits/types/timer_t.h
rva-shifting.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
rva-shifting.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
rva-shifting.o: /usr/include/bits/byteswap.h
rva-shifting.o: /usr/include/bits/uintn-identity.h /usr/include/sys/select.h
rva-shifting.o: /usr/include/bits/select.h /usr/include/bits/types/sigset_t.h
rva-shifting.o: /usr/include/bits/types/__sigset_t.h
rva-shifting.o: /usr/include/bits/types/struct_timeval.h
rva-shifting.o: /usr/include/bits/types/struct_timespec.h
rva-shifting.o: /usr/include/bits/pthreadtypes.h
rva-shifting.o: /usr/include/bits/thread-shared-types.h
rva-shifting.o: /usr/include/bits/pthreadtypes-arch.h
rva-shifting.o: /usr/include/bits/atomic_wide_counter.h
rva-shifting.o: /usr/include/bits/struct_mutex.h
rva-shifting.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
rva-shifting.o: /usr/include/bits/stdlib-float.h ../lash-core/lash-types.h
rva-shifting.o: ../lash-core/lash.h ../lash-core/datastruct.h
rva-shifting.o: /usr/include/string.h /usr/include/bits/types/locale_t.h
rva-shifting.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
rva-shifting.o: ../lash-core/auto-weak-convert.h rva-equations.h
rva-shifting.o: rva-operations.h rva-projection.h rva-sets.h rva-shifting.h
rva-shifting.o: rva-tests.h rva-tools.h rva-transformations.h
rva-shifting.o: ../lash-core/auto.h ../lash-core/diag.h
rva-shifting.o: ../lash-core/lash-diag.h ../lash-core/resource.h
rva-shifting.o: ../lash-core/auto-determinize.h
rva-shifting.o: ../lash-core/lash-auto-operations.h
rva-shifting.o: ../lash-core/auto-minimize.h ../lash-core/auto-product.h
rva-shifting.o: ../lash-core/auto-sequential.h ../lash-core/auto-difference.h
rva-shifting.o: ../lash-core/auto-normalize.h ../lash-core/auto-quotient.h
rva-shifting.o: ../lash-core/auto-count.h ../lash-core/biguint.h
rva-tests.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
rva-tests.o: /usr/include/features.h /usr/include/features-time64.h
rva-tests.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
rva-tests.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
rva-tests.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
rva-tests.o: /usr/include/stddef.h /usr/include/stdarg.h
rva-tests.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
rva-tests.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
rva-tests.o: /usr/include/bits/types/__mbstate_t.h
rva-tests.o: /usr/include/bits/types/__fpos64_t.h
rva-tests.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
rva-tests.o: /usr/include/bits/types/struct_FILE.h
rva-tests.o: /usr/include/bits/types/cookie_io_functions_t.h
rva-tests.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
rva-tests.o: /usr/include/bits/floatn-common.h rva.h lash-rva.h
rva-tests.o: ../lash-core/lash-auto.h /usr/include/stdlib.h
rva-tests.o: /usr/include/bits/waitflags.h /usr/include/bits/waitstatus.h
rva-tests.o: /usr/include/sys/types.h /usr/include/bits/types/clock_t.h
rva-tests.o: /usr/include/bits/types/clockid_t.h
rva-tests.o: /usr/include/bits/types/time_t.h
rva-tests.o: /usr/include/bits/types/timer_t.h
rva-tests.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
rva-tests.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
rva-tests.o: /usr/include/bits/byteswap.h /usr/include/bits/uintn-identity.h
rva-tests.o: /usr/include/sys/select.h /usr/include/bits/select.h
rva-tests.o: /usr/include/bits/types/sigset_t.h
rva-tests.o: /usr/include/bits/types/__sigset_t.h
rva-tests.o: /usr/include/bits/types/struct_timeval.h
rva-tests.o: /usr/include/bits/types/struct_timespec.h
rva-tests.o: /usr/include/bits/pthreadtypes.h
rva-tests.o: /usr/include/bits/thread-shared-types.h
rva-tests.o: /usr/include/bits/pthreadtypes-arch.h
rva-tests.o: /usr/include/bits/atomic_wide_counter.h
rva-tests.o: /usr/include/bits/struct_mutex.h
rva-tests.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
rva-tests.o: /usr/include/bits/stdlib-float.h ../lash-core/lash-types.h
rva-tests.o: ../lash-core/lash.h ../lash-core/datastruct.h
rva-tests.o: /usr/include/string.h /usr/include/bits/types/locale_t.h
rva-tests.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
rva-tests.o: ../lash-core/auto-weak-convert.h rva-equations.h
rva-tests.o: rva-operations.h rva-projection.h rva-sets.h rva-shifting.h
rva-tests.o: rva-tests.h rva-tools.h rva-transformations.h
rva-tests.o: ../lash-core/auto.h ../lash-core/diag.h ../lash-core/lash-diag.h
rva-tests.o: ../lash-core/resource.h ../lash-core/lash-auto-operations.h
rva-tests.o: ../lash-core/auto-determinize.h ../lash-core/auto-minimize.h
rva-tests.o: ../lash-core/auto-product.h ../lash-core/auto-sequential.h
rva-tests.o: ../lash-core/auto-difference.h ../lash-core/auto-normalize.h
rva-tests.o: ../lash-core/auto-quotient.h ../lash-core/auto-count.h
rva-tests.o: ../lash-core/biguint.h
rva-tools.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
rva-tools.o: /usr/include/features.h /usr/include/features-time64.h
rva-tools.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
rva-tools.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
rva-tools.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
rva-tools.o: /usr/include/stddef.h /usr/include/stdarg.h
rva-tools.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
rva-tools.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
rva-tools.o: /usr/include/bits/types/__mbstate_t.h
rva-tools.o: /usr/include/bits/types/__fpos64_t.h
rva-tools.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
rva-tools.o: /usr/include/bits/types/struct_FILE.h
rva-tools.o: /usr/include/bits/types/cookie_io_functions_t.h
rva-tools.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
rva-tools.o: /usr/include/bits/floatn-common.h rva.h lash-rva.h
rva-tools.o: ../lash-core/lash-auto.h /usr/include/stdlib.h
rva-tools.o: /usr/include/bits/waitflags.h /usr/include/bits/waitstatus.h
rva-tools.o: /usr/include/sys/types.h /usr/include/bits/types/clock_t.h
rva-tools.o: /usr/include/bits/types/clockid_t.h
rva-tools.o: /usr/include/bits/types/time_t.h
rva-tools.o: /usr/include/bits/types/timer_t.h
rva-tools.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
rva-tools.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
rva-tools.o: /usr/include/bits/byteswap.h /usr/include/bits/uintn-identity.h
rva-tools.o: /usr/include/sys/select.h /usr/include/bits/select.h
rva-tools.o: /usr/include/bits/types/sigset_t.h
rva-tools.o: /usr/include/bits/types/__sigset_t.h
rva-tools.o: /usr/include/bits/types/struct_timeval.h
rva-tools.o: /usr/include/bits/types/struct_timespec.h
rva-tools.o: /usr/include/bits/pthreadtypes.h
rva-tools.o: /usr/include/bits/thread-shared-types.h
rva-tools.o: /usr/include/bits/pthreadtypes-arch.h
rva-tools.o: /usr/include/bits/atomic_wide_counter.h
rva-tools.o: /usr/include/bits/struct_mutex.h
rva-tools.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
rva-tools.o: /usr/include/bits/stdlib-float.h ../lash-core/lash-types.h
rva-tools.o: ../lash-core/lash.h ../lash-core/datastruct.h
rva-tools.o: /usr/include/string.h /usr/include/bits/types/locale_t.h
rva-tools.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
rva-tools.o: ../lash-core/auto-weak-convert.h rva-equations.h
rva-tools.o: rva-operations.h rva-projection.h rva-sets.h rva-shifting.h
rva-tools.o: rva-tests.h rva-tools.h rva-transformations.h
rva-tools.o: ../lash-core/auto.h ../lash-core/diag.h ../lash-core/lash-diag.h
rva-tools.o: ../lash-core/resource.h ../lash-core/auto-serialize.h
rva-tools.o: ../lash-core/lash-auto-operations.h
rva-tools.o: ../lash-core/auto-determinize.h ../lash-core/auto-minimize.h
rva-tools.o: ../lash-core/auto-product.h ../lash-core/auto-sequential.h
rva-tools.o: ../lash-core/auto-difference.h ../lash-core/auto-normalize.h
rva-tools.o: ../lash-core/auto-quotient.h ../lash-core/auto-count.h
rva-tools.o: ../lash-core/biguint.h
rva-transformations.o: /usr/include/stdio.h
rva-transformations.o: /usr/include/bits/libc-header-start.h
rva-transformations.o: /usr/include/features.h /usr/include/features-time64.h
rva-transformations.o: /usr/include/bits/wordsize.h
rva-transformations.o: /usr/include/bits/timesize.h
rva-transformations.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
rva-transformations.o: /usr/include/bits/long-double.h
rva-transformations.o: /usr/include/gnu/stubs.h /usr/include/stddef.h
rva-transformations.o: /usr/include/stdarg.h /usr/include/bits/types.h
rva-transformations.o: /usr/include/bits/typesizes.h
rva-transformations.o: /usr/include/bits/time64.h
rva-transformations.o: /usr/include/bits/types/__fpos_t.h
rva-transformations.o: /usr/include/bits/types/__mbstate_t.h
rva-transformations.o: /usr/include/bits/types/__fpos64_t.h
rva-transformations.o: /usr/include/bits/types/__FILE.h
rva-transformations.o: /usr/include/bits/types/FILE.h
rva-transformations.o: /usr/include/bits/types/struct_FILE.h
rva-transformations.o: /usr/include/bits/types/cookie_io_functions_t.h
rva-transformations.o: /usr/include/bits/stdio_lim.h
rva-transformations.o: /usr/include/bits/floatn.h
rva-transformations.o: /usr/include/bits/floatn-common.h
rva-transformations.o: /usr/include/string.h
rva-transformations.o: /usr/include/bits/types/locale_t.h
rva-transformations.o: /usr/include/bits/types/__locale_t.h
rva-transformations.o: /usr/include/strings.h rva.h lash-rva.h
rva-transformations.o: ../lash-core/lash-auto.h /usr/include/stdlib.h
rva-transformations.o: /usr/include/bits/waitflags.h
rva-transformations.o: /usr/include/bits/waitstatus.h
rva-transformations.o: /usr/include/sys/types.h
rva-transformations.o: /usr/include/bits/types/clock_t.h
rva-transformations.o: /usr/include/bits/types/clockid_t.h
rva-transformations.o: /usr/include/bits/types/time_t.h
rva-transformations.o: /usr/include/bits/types/timer_t.h
rva-transformations.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
rva-transformations.o: /usr/include/bits/endian.h
rva-transformations.o: /usr/include/bits/endianness.h
rva-transformations.o: /usr/include/bits/byteswap.h
rva-transformations.o: /usr/include/bits/uintn-identity.h
rva-transformations.o: /usr/include/sys/select.h /usr/include/bits/select.h
rva-transformations.o: /usr/include/bits/types/sigset_t.h
rva-transformations.o: /usr/include/bits/types/__sigset_t.h
rva-transformations.o: /usr/include/bits/types/struct_timeval.h
rva-transformations.o: /usr/include/bits/types/struct_timespec.h
rva-transformations.o: /usr/include/bits/pthreadtypes.h
rva-transformations.o: /usr/include/bits/thread-shared-types.h
rva-transformations.o: /usr/include/bits/pthreadtypes-arch.h
rva-transformations.o: /usr/include/bits/atomic_wide_counter.h
rva-transformations.o: /usr/include/bits/struct_mutex.h
rva-transformations.o: /usr/include/bits/struct_rwlock.h
rva-transformations.o: /usr/include/alloca.h /usr/include/bits/stdlib-float.h
rva-transformations.o: ../lash-core/lash-types.h ../lash-core/lash.h
rva-transformations.o: ../lash-core/datastruct.h
rva-transformations.o: ../lash-core/auto-weak-convert.h rva-equations.h
rva-transformations.o: rva-operations.h rva-projection.h rva-sets.h
rva-transformations.o: rva-shifting.h rva-tests.h rva-tools.h
rva-transformations.o: rva-transformations.h ../lash-core/auto.h
rva-transformations.o: ../lash-core/diag.h ../lash-core/lash-diag.h
rva-transformations.o: ../lash-core/resource.h ../lash-core/matrix.h
rva-transformations.o: ../lash-core/arithmetic.h
rva-transformations.o: ../lash-core/lash-auto-operations.h
rva-transformations.o: ../lash-core/auto-determinize.h
rva-transformations.o: ../lash-core/auto-minimize.h
rva-transformations.o: ../lash-core/auto-product.h
rva-transformations.o: ../lash-core/auto-sequential.h
rva-transformations.o: ../lash-core/auto-difference.h
rva-transformations.o: ../lash-core/auto-normalize.h
rva-transformations.o: ../lash-core/auto-quotient.h ../lash-core/auto-count.h
rva-transformations.o: ../lash-core/biguint.h
rva.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
rva.o: /usr/include/features.h /usr/include/features-time64.h
rva.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
rva.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
rva.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
rva.o: /usr/include/stddef.h /usr/include/stdarg.h /usr/include/bits/types.h
rva.o: /usr/include/bits/typesizes.h /usr/include/bits/time64.h
rva.o: /usr/include/bits/types/__fpos_t.h
rva.o: /usr/include/bits/types/__mbstate_t.h
rva.o: /usr/include/bits/types/__fpos64_t.h /usr/include/bits/types/__FILE.h
rva.o: /usr/include/bits/types/FILE.h /usr/include/bits/types/struct_FILE.h
rva.o: /usr/include/bits/types/cookie_io_functions_t.h
rva.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
rva.o: /usr/include/bits/floatn-common.h rva.h lash-rva.h
rva.o: ../lash-core/lash-auto.h /usr/include/stdlib.h
rva.o: /usr/include/bits/waitflags.h /usr/include/bits/waitstatus.h
rva.o: /usr/include/sys/types.h /usr/include/bits/types/clock_t.h
rva.o: /usr/include/bits/types/clockid_t.h /usr/include/bits/types/time_t.h
rva.o: /usr/include/bits/types/timer_t.h /usr/include/bits/stdint-intn.h
rva.o: /usr/include/endian.h /usr/include/bits/endian.h
rva.o: /usr/include/bits/endianness.h /usr/include/bits/byteswap.h
rva.o: /usr/include/bits/uintn-identity.h /usr/include/sys/select.h
rva.o: /usr/include/bits/select.h /usr/include/bits/types/sigset_t.h
rva.o: /usr/include/bits/types/__sigset_t.h
rva.o: /usr/include/bits/types/struct_timeval.h
rva.o: /usr/include/bits/types/struct_timespec.h
rva.o: /usr/include/bits/pthreadtypes.h
rva.o: /usr/include/bits/thread-shared-types.h
rva.o: /usr/include/bits/pthreadtypes-arch.h
rva.o: /usr/include/bits/atomic_wide_counter.h
rva.o: /usr/include/bits/struct_mutex.h /usr/include/bits/struct_rwlock.h
rva.o: /usr/include/alloca.h /usr/include/bits/stdlib-float.h
rva.o: ../lash-core/lash-types.h ../lash-core/lash.h
rva.o: ../lash-core/datastruct.h /usr/include/string.h
rva.o: /usr/include/bits/types/locale_t.h
rva.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
rva.o: ../lash-core/auto-weak-convert.h rva-equations.h rva-operations.h
rva.o: rva-projection.h rva-sets.h rva-shifting.h rva-tests.h rva-tools.h
rva.o: rva-transformations.h ../lash-core/auto.h ../lash-core/diag.h
rva.o: ../lash-core/lash-diag.h ../lash-core/resource.h
rva.o: ../lash-core/lash-auto-operations.h ../lash-core/auto-determinize.h
rva.o: ../lash-core/auto-minimize.h ../lash-core/auto-product.h
rva.o: ../lash-core/auto-sequential.h ../lash-core/auto-difference.h
rva.o: ../lash-core/auto-normalize.h ../lash-core/auto-quotient.h
rva.o: ../lash-core/auto-count.h ../lash-core/biguint.h
