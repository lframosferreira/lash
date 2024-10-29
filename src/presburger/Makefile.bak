######################################################################
##                                                                  ##
##  LASH Presburger compiler -- v0.9                                ##
##  ========================                                        ##
##                                                                  ##
##     Makefile.                                                    ##
##                                                                  ##
##     12/14/99  :  Creation. (LL)                                  ##
##     02/12/01  :  Reorganization. (BB)                            ##
##     08/29/02  :  Reorganization. (BB)                            ##
##     05/22/03  :  Added flag for mpatrol. (LL)                    ##
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

LASH-BIN-DIR = 	../../bin
LASH-LIB-DIR = 	../../lib

LASH-CORE-DIR =	../lash-core/
LASH-NDD-DIR  =	../ndd/

LASH-CORE   =	$(LASH-LIB-DIR)/lash-core.o
LASH-ARITH  =   $(LASH-LIB-DIR)/lash-arith.o
NDD-PACKAGE = 	$(LASH-LIB-DIR)/ndd-package.o

SRCS-PRESB =	lexical.c \
		semantic.c \
		expression.c \
		output.c \
		presburger.c

OBJS-PRESB =	${SRCS-PRESB:.c=.o}

CFLAGS =  -g -c -O3 -ansi -Wall -Wtraditional -Wpointer-arith \
         -Waggregate-return -Wstrict-prototypes \
         -Wmissing-prototypes -Wmissing-declarations \
         -I$(LASH-CORE-DIR) -I$(LASH-NDD-DIR) 

all: presburger

presburger: $(OBJS-PRESB)
	$(CC) -g -o presburger $(OBJS-PRESB)\
	 $(LASH-CORE) $(LASH-ARITH) $(NDD-PACKAGE)
	mv presburger $(LASH-BIN-DIR)

clean:
	-rm -f $(OBJS-PRESB) presburger $(LASH-BIN-DIR)/presburger

depend: 
	makedepend -I$(LASH-CORE-DIR) -I$(LASH-NDD-DIR) $(SRCS-PRESB)

# DO NOT DELETE THIS LINE -- make depend depends on it.

lexical.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
lexical.o: /usr/include/features.h /usr/include/features-time64.h
lexical.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
lexical.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
lexical.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
lexical.o: /usr/include/stddef.h /usr/include/stdarg.h
lexical.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
lexical.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
lexical.o: /usr/include/bits/types/__mbstate_t.h
lexical.o: /usr/include/bits/types/__fpos64_t.h
lexical.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
lexical.o: /usr/include/bits/types/struct_FILE.h
lexical.o: /usr/include/bits/types/cookie_io_functions_t.h
lexical.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
lexical.o: /usr/include/bits/floatn-common.h /usr/include/stdlib.h
lexical.o: /usr/include/bits/waitflags.h /usr/include/bits/waitstatus.h
lexical.o: /usr/include/sys/types.h /usr/include/bits/types/clock_t.h
lexical.o: /usr/include/bits/types/clockid_t.h
lexical.o: /usr/include/bits/types/time_t.h /usr/include/bits/types/timer_t.h
lexical.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
lexical.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
lexical.o: /usr/include/bits/byteswap.h /usr/include/bits/uintn-identity.h
lexical.o: /usr/include/sys/select.h /usr/include/bits/select.h
lexical.o: /usr/include/bits/types/sigset_t.h
lexical.o: /usr/include/bits/types/__sigset_t.h
lexical.o: /usr/include/bits/types/struct_timeval.h
lexical.o: /usr/include/bits/types/struct_timespec.h
lexical.o: /usr/include/bits/pthreadtypes.h
lexical.o: /usr/include/bits/thread-shared-types.h
lexical.o: /usr/include/bits/pthreadtypes-arch.h
lexical.o: /usr/include/bits/atomic_wide_counter.h
lexical.o: /usr/include/bits/struct_mutex.h /usr/include/bits/struct_rwlock.h
lexical.o: /usr/include/alloca.h /usr/include/bits/stdlib-float.h
lexical.o: /usr/include/string.h /usr/include/bits/types/locale_t.h
lexical.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
lexical.o: ../lash-core/lash-types.h ../lash-core/lash.h
lexical.o: ../lash-core/lash-diag.h ../lash-core/resource.h presburger.h
lexical.o: lexical.h expression.h ../lash-core/datastruct.h ../ndd/ndd.h
lexical.o: ../ndd/lash-ndd.h ../lash-core/lash-auto.h ../ndd/ndd-sets.h
lexical.o: ../ndd/ndd-operations.h ../ndd/ndd-tests.h ../ndd/ndd-equations.h
lexical.o: ../ndd/ndd-projection.h ../ndd/ndd-transformations.h
lexical.o: ../ndd/ndd-iterations.h ../ndd/ndd-machines.h ../lash-core/auto.h
lexical.o: ../lash-core/biguint.h
semantic.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
semantic.o: /usr/include/features.h /usr/include/features-time64.h
semantic.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
semantic.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
semantic.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
semantic.o: /usr/include/stddef.h /usr/include/stdarg.h
semantic.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
semantic.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
semantic.o: /usr/include/bits/types/__mbstate_t.h
semantic.o: /usr/include/bits/types/__fpos64_t.h
semantic.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
semantic.o: /usr/include/bits/types/struct_FILE.h
semantic.o: /usr/include/bits/types/cookie_io_functions_t.h
semantic.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
semantic.o: /usr/include/bits/floatn-common.h ../lash-core/lash-types.h
semantic.o: ../lash-core/lash.h ../lash-core/resource.h
semantic.o: ../lash-core/datastruct.h /usr/include/string.h
semantic.o: /usr/include/bits/types/locale_t.h
semantic.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
semantic.o: presburger.h lexical.h expression.h ../ndd/ndd.h
semantic.o: ../ndd/lash-ndd.h ../lash-core/lash-auto.h /usr/include/stdlib.h
semantic.o: /usr/include/bits/waitflags.h /usr/include/bits/waitstatus.h
semantic.o: /usr/include/sys/types.h /usr/include/bits/types/clock_t.h
semantic.o: /usr/include/bits/types/clockid_t.h
semantic.o: /usr/include/bits/types/time_t.h
semantic.o: /usr/include/bits/types/timer_t.h /usr/include/bits/stdint-intn.h
semantic.o: /usr/include/endian.h /usr/include/bits/endian.h
semantic.o: /usr/include/bits/endianness.h /usr/include/bits/byteswap.h
semantic.o: /usr/include/bits/uintn-identity.h /usr/include/sys/select.h
semantic.o: /usr/include/bits/select.h /usr/include/bits/types/sigset_t.h
semantic.o: /usr/include/bits/types/__sigset_t.h
semantic.o: /usr/include/bits/types/struct_timeval.h
semantic.o: /usr/include/bits/types/struct_timespec.h
semantic.o: /usr/include/bits/pthreadtypes.h
semantic.o: /usr/include/bits/thread-shared-types.h
semantic.o: /usr/include/bits/pthreadtypes-arch.h
semantic.o: /usr/include/bits/atomic_wide_counter.h
semantic.o: /usr/include/bits/struct_mutex.h
semantic.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
semantic.o: /usr/include/bits/stdlib-float.h ../ndd/ndd-sets.h
semantic.o: ../ndd/ndd-operations.h ../ndd/ndd-tests.h ../ndd/ndd-equations.h
semantic.o: ../ndd/ndd-projection.h ../ndd/ndd-transformations.h
semantic.o: ../ndd/ndd-iterations.h ../ndd/ndd-machines.h ../lash-core/auto.h
semantic.o: ../lash-core/biguint.h semantic.h
expression.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
expression.o: /usr/include/features.h /usr/include/features-time64.h
expression.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
expression.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
expression.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
expression.o: /usr/include/stddef.h /usr/include/stdarg.h
expression.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
expression.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
expression.o: /usr/include/bits/types/__mbstate_t.h
expression.o: /usr/include/bits/types/__fpos64_t.h
expression.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
expression.o: /usr/include/bits/types/struct_FILE.h
expression.o: /usr/include/bits/types/cookie_io_functions_t.h
expression.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
expression.o: /usr/include/bits/floatn-common.h ../lash-core/lash.h
expression.o: ../lash-core/lash-types.h ../lash-core/resource.h
expression.o: ../lash-core/datastruct.h /usr/include/string.h
expression.o: /usr/include/bits/types/locale_t.h
expression.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
expression.o: presburger.h expression.h ../ndd/ndd.h ../ndd/lash-ndd.h
expression.o: ../lash-core/lash-auto.h /usr/include/stdlib.h
expression.o: /usr/include/bits/waitflags.h /usr/include/bits/waitstatus.h
expression.o: /usr/include/sys/types.h /usr/include/bits/types/clock_t.h
expression.o: /usr/include/bits/types/clockid_t.h
expression.o: /usr/include/bits/types/time_t.h
expression.o: /usr/include/bits/types/timer_t.h
expression.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
expression.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
expression.o: /usr/include/bits/byteswap.h /usr/include/bits/uintn-identity.h
expression.o: /usr/include/sys/select.h /usr/include/bits/select.h
expression.o: /usr/include/bits/types/sigset_t.h
expression.o: /usr/include/bits/types/__sigset_t.h
expression.o: /usr/include/bits/types/struct_timeval.h
expression.o: /usr/include/bits/types/struct_timespec.h
expression.o: /usr/include/bits/pthreadtypes.h
expression.o: /usr/include/bits/thread-shared-types.h
expression.o: /usr/include/bits/pthreadtypes-arch.h
expression.o: /usr/include/bits/atomic_wide_counter.h
expression.o: /usr/include/bits/struct_mutex.h
expression.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
expression.o: /usr/include/bits/stdlib-float.h ../ndd/ndd-sets.h
expression.o: ../ndd/ndd-operations.h ../ndd/ndd-tests.h
expression.o: ../ndd/ndd-equations.h ../ndd/ndd-projection.h
expression.o: ../ndd/ndd-transformations.h ../ndd/ndd-iterations.h
expression.o: ../ndd/ndd-machines.h ../lash-core/auto.h
expression.o: ../lash-core/biguint.h output.h ../ndd/ndd-count.h
expression.o: ../ndd/ndd-congruence.h ../lash-core/auto-minimize.h
expression.o: ../lash-core/lash-auto-operations.h
expression.o: ../lash-core/auto-determinize.h ../lash-core/auto-product.h
expression.o: ../lash-core/auto-sequential.h ../lash-core/auto-difference.h
expression.o: ../lash-core/auto-normalize.h ../lash-core/auto-quotient.h
expression.o: ../lash-core/auto-count.h
output.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
output.o: /usr/include/features.h /usr/include/features-time64.h
output.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
output.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
output.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
output.o: /usr/include/stddef.h /usr/include/stdarg.h
output.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
output.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
output.o: /usr/include/bits/types/__mbstate_t.h
output.o: /usr/include/bits/types/__fpos64_t.h
output.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
output.o: /usr/include/bits/types/struct_FILE.h
output.o: /usr/include/bits/types/cookie_io_functions_t.h
output.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
output.o: /usr/include/bits/floatn-common.h ../lash-core/lash.h
output.o: ../lash-core/lash-types.h ../lash-core/resource.h
output.o: ../lash-core/datastruct.h /usr/include/string.h
output.o: /usr/include/bits/types/locale_t.h
output.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
output.o: presburger.h expression.h ../ndd/ndd.h ../ndd/lash-ndd.h
output.o: ../lash-core/lash-auto.h /usr/include/stdlib.h
output.o: /usr/include/bits/waitflags.h /usr/include/bits/waitstatus.h
output.o: /usr/include/sys/types.h /usr/include/bits/types/clock_t.h
output.o: /usr/include/bits/types/clockid_t.h
output.o: /usr/include/bits/types/time_t.h /usr/include/bits/types/timer_t.h
output.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
output.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
output.o: /usr/include/bits/byteswap.h /usr/include/bits/uintn-identity.h
output.o: /usr/include/sys/select.h /usr/include/bits/select.h
output.o: /usr/include/bits/types/sigset_t.h
output.o: /usr/include/bits/types/__sigset_t.h
output.o: /usr/include/bits/types/struct_timeval.h
output.o: /usr/include/bits/types/struct_timespec.h
output.o: /usr/include/bits/pthreadtypes.h
output.o: /usr/include/bits/thread-shared-types.h
output.o: /usr/include/bits/pthreadtypes-arch.h
output.o: /usr/include/bits/atomic_wide_counter.h
output.o: /usr/include/bits/struct_mutex.h /usr/include/bits/struct_rwlock.h
output.o: /usr/include/alloca.h /usr/include/bits/stdlib-float.h
output.o: ../ndd/ndd-sets.h ../ndd/ndd-operations.h ../ndd/ndd-tests.h
output.o: ../ndd/ndd-equations.h ../ndd/ndd-projection.h
output.o: ../ndd/ndd-transformations.h ../ndd/ndd-iterations.h
output.o: ../ndd/ndd-machines.h ../lash-core/auto.h ../lash-core/biguint.h
output.o: output.h ../lash-core/auto-io-print.h ../ndd/lash-ndd-io.h
output.o: ../lash-core/datastruct-io.h ../lash-core/lash-io.h
output.o: ../lash-core/lash-auto-io.h ../lash-core/auto-io-dots.h
presburger.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
presburger.o: /usr/include/features.h /usr/include/features-time64.h
presburger.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
presburger.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
presburger.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
presburger.o: /usr/include/stddef.h /usr/include/stdarg.h
presburger.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
presburger.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
presburger.o: /usr/include/bits/types/__mbstate_t.h
presburger.o: /usr/include/bits/types/__fpos64_t.h
presburger.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
presburger.o: /usr/include/bits/types/struct_FILE.h
presburger.o: /usr/include/bits/types/cookie_io_functions_t.h
presburger.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
presburger.o: /usr/include/bits/floatn-common.h /usr/include/stdlib.h
presburger.o: /usr/include/bits/waitflags.h /usr/include/bits/waitstatus.h
presburger.o: /usr/include/sys/types.h /usr/include/bits/types/clock_t.h
presburger.o: /usr/include/bits/types/clockid_t.h
presburger.o: /usr/include/bits/types/time_t.h
presburger.o: /usr/include/bits/types/timer_t.h
presburger.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
presburger.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
presburger.o: /usr/include/bits/byteswap.h /usr/include/bits/uintn-identity.h
presburger.o: /usr/include/sys/select.h /usr/include/bits/select.h
presburger.o: /usr/include/bits/types/sigset_t.h
presburger.o: /usr/include/bits/types/__sigset_t.h
presburger.o: /usr/include/bits/types/struct_timeval.h
presburger.o: /usr/include/bits/types/struct_timespec.h
presburger.o: /usr/include/bits/pthreadtypes.h
presburger.o: /usr/include/bits/thread-shared-types.h
presburger.o: /usr/include/bits/pthreadtypes-arch.h
presburger.o: /usr/include/bits/atomic_wide_counter.h
presburger.o: /usr/include/bits/struct_mutex.h
presburger.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
presburger.o: /usr/include/bits/stdlib-float.h /usr/include/string.h
presburger.o: /usr/include/bits/types/locale_t.h
presburger.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
presburger.o: ../lash-core/lash-types.h ../lash-core/lash.h
presburger.o: ../lash-core/lash-diag.h ../lash-core/datastruct.h presburger.h
presburger.o: lexical.h expression.h ../ndd/ndd.h ../ndd/lash-ndd.h
presburger.o: ../lash-core/lash-auto.h ../ndd/ndd-sets.h
presburger.o: ../ndd/ndd-operations.h ../ndd/ndd-tests.h
presburger.o: ../ndd/ndd-equations.h ../ndd/ndd-projection.h
presburger.o: ../ndd/ndd-transformations.h ../ndd/ndd-iterations.h
presburger.o: ../ndd/ndd-machines.h ../lash-core/auto.h
presburger.o: ../lash-core/biguint.h grammar.h semantic.h
