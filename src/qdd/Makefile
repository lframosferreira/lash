######################################################################
##                                                                  ##
##  LASH QDD package -- v0.9                                        ##
##  ========================                                        ##
##                                                                  ##
##     Makefile.                                                    ##
##                                                                  ##
##     06/30/00  :  Creation. (BB+JMF)                              ##
##     02/12/01  :  Reorganization. (BB)                            ##
##     08/13/01  :  New source tree. (BB)                           ##
##     07/09/02  :  Reorganization. (BB)                            ##
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

SRCS-QDD =	qdd-one-iterations.c \
                qdd-iterations.c \
                queue-operations.c \
                qdd.c \
                qdd-automaton.c \
                qdd-symbol.c \
                qdd-one-queue.c \
                qdd-perform.c \
                qdd-operations.c \
                qdd-iterations-utils.c \
                qdd-machines.c \
                qdd-io.c

OBJS-QDD =	${SRCS-QDD:.c=.o}

CFLAGS =  -g -c -O3 -ansi -Wall -Wtraditional -Wpointer-arith \
         -Waggregate-return -Wstrict-prototypes \
         -Wmissing-prototypes -Wmissing-declarations \
         -I$(LASH-CORE-DIR)

all:  qdd-package

qdd-package: $(OBJS-QDD)
	ld -r -o qdd-package.o $(OBJS-QDD)
	mv qdd-package.o $(LASH-LIB-DIR)

clean:
	-rm -f $(OBJS-QDD) qdd-package.o
	-rm -f $(LASH-LIB-DIR)/qdd-package.o

depend: 
	makedepend -I$(LASH-CORE-DIR) $(SRCS-QDD)

# DO NOT DELETE THIS LINE -- make depend depends on it.

qdd-one-iterations.o: /usr/include/stdlib.h
qdd-one-iterations.o: /usr/include/bits/libc-header-start.h
qdd-one-iterations.o: /usr/include/features.h /usr/include/features-time64.h
qdd-one-iterations.o: /usr/include/bits/wordsize.h
qdd-one-iterations.o: /usr/include/bits/timesize.h /usr/include/stdc-predef.h
qdd-one-iterations.o: /usr/include/sys/cdefs.h
qdd-one-iterations.o: /usr/include/bits/long-double.h
qdd-one-iterations.o: /usr/include/gnu/stubs.h /usr/include/stddef.h
qdd-one-iterations.o: /usr/include/bits/waitflags.h
qdd-one-iterations.o: /usr/include/bits/waitstatus.h
qdd-one-iterations.o: /usr/include/bits/floatn.h
qdd-one-iterations.o: /usr/include/bits/floatn-common.h
qdd-one-iterations.o: /usr/include/sys/types.h /usr/include/bits/types.h
qdd-one-iterations.o: /usr/include/bits/typesizes.h
qdd-one-iterations.o: /usr/include/bits/time64.h
qdd-one-iterations.o: /usr/include/bits/types/clock_t.h
qdd-one-iterations.o: /usr/include/bits/types/clockid_t.h
qdd-one-iterations.o: /usr/include/bits/types/time_t.h
qdd-one-iterations.o: /usr/include/bits/types/timer_t.h
qdd-one-iterations.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
qdd-one-iterations.o: /usr/include/bits/endian.h
qdd-one-iterations.o: /usr/include/bits/endianness.h
qdd-one-iterations.o: /usr/include/bits/byteswap.h
qdd-one-iterations.o: /usr/include/bits/uintn-identity.h
qdd-one-iterations.o: /usr/include/sys/select.h /usr/include/bits/select.h
qdd-one-iterations.o: /usr/include/bits/types/sigset_t.h
qdd-one-iterations.o: /usr/include/bits/types/__sigset_t.h
qdd-one-iterations.o: /usr/include/bits/types/struct_timeval.h
qdd-one-iterations.o: /usr/include/bits/types/struct_timespec.h
qdd-one-iterations.o: /usr/include/bits/pthreadtypes.h
qdd-one-iterations.o: /usr/include/bits/thread-shared-types.h
qdd-one-iterations.o: /usr/include/bits/pthreadtypes-arch.h
qdd-one-iterations.o: /usr/include/bits/atomic_wide_counter.h
qdd-one-iterations.o: /usr/include/bits/struct_mutex.h
qdd-one-iterations.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
qdd-one-iterations.o: /usr/include/bits/stdlib-float.h
qdd-one-iterations.o: ../lash-core/lash-types.h ../lash-core/lash.h
qdd-one-iterations.o: ../lash-core/diag.h ../lash-core/lash-diag.h
qdd-one-iterations.o: ../lash-core/resource.h ../lash-core/datastruct.h
qdd-one-iterations.o: /usr/include/string.h
qdd-one-iterations.o: /usr/include/bits/types/locale_t.h
qdd-one-iterations.o: /usr/include/bits/types/__locale_t.h
qdd-one-iterations.o: /usr/include/strings.h ../lash-core/arithmetic.h
qdd-one-iterations.o: queue-operations.h qdd-symbol.h qdd.h
qdd-one-iterations.o: ../lash-core/lash-auto-operations.h
qdd-one-iterations.o: ../lash-core/lash-auto.h
qdd-one-iterations.o: ../lash-core/auto-determinize.h
qdd-one-iterations.o: ../lash-core/auto-minimize.h
qdd-one-iterations.o: ../lash-core/auto-product.h
qdd-one-iterations.o: ../lash-core/auto-sequential.h
qdd-one-iterations.o: ../lash-core/auto-difference.h
qdd-one-iterations.o: ../lash-core/auto-normalize.h
qdd-one-iterations.o: ../lash-core/auto-quotient.h ../lash-core/auto-count.h
qdd-one-iterations.o: ../lash-core/biguint.h lash-qdd.h qdd-operations.h
qdd-one-iterations.o: qdd-perform.h qdd-one-queue.h qdd-one-iterations.h
qdd-one-iterations.o: qdd-iterations.h qdd-machines.h qdd-automaton.h
qdd-one-iterations.o: qdd-iterations-utils.h
qdd-iterations.o: ../lash-core/lash-types.h ../lash-core/lash.h
qdd-iterations.o: ../lash-core/diag.h ../lash-core/lash-diag.h
qdd-iterations.o: ../lash-core/resource.h ../lash-core/arithmetic.h qdd.h
qdd-iterations.o: ../lash-core/lash-auto-operations.h
qdd-iterations.o: ../lash-core/lash-auto.h /usr/include/stdlib.h
qdd-iterations.o: /usr/include/bits/libc-header-start.h
qdd-iterations.o: /usr/include/features.h /usr/include/features-time64.h
qdd-iterations.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
qdd-iterations.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
qdd-iterations.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
qdd-iterations.o: /usr/include/stddef.h /usr/include/bits/waitflags.h
qdd-iterations.o: /usr/include/bits/waitstatus.h /usr/include/bits/floatn.h
qdd-iterations.o: /usr/include/bits/floatn-common.h /usr/include/sys/types.h
qdd-iterations.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
qdd-iterations.o: /usr/include/bits/time64.h
qdd-iterations.o: /usr/include/bits/types/clock_t.h
qdd-iterations.o: /usr/include/bits/types/clockid_t.h
qdd-iterations.o: /usr/include/bits/types/time_t.h
qdd-iterations.o: /usr/include/bits/types/timer_t.h
qdd-iterations.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
qdd-iterations.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
qdd-iterations.o: /usr/include/bits/byteswap.h
qdd-iterations.o: /usr/include/bits/uintn-identity.h
qdd-iterations.o: /usr/include/sys/select.h /usr/include/bits/select.h
qdd-iterations.o: /usr/include/bits/types/sigset_t.h
qdd-iterations.o: /usr/include/bits/types/__sigset_t.h
qdd-iterations.o: /usr/include/bits/types/struct_timeval.h
qdd-iterations.o: /usr/include/bits/types/struct_timespec.h
qdd-iterations.o: /usr/include/bits/pthreadtypes.h
qdd-iterations.o: /usr/include/bits/thread-shared-types.h
qdd-iterations.o: /usr/include/bits/pthreadtypes-arch.h
qdd-iterations.o: /usr/include/bits/atomic_wide_counter.h
qdd-iterations.o: /usr/include/bits/struct_mutex.h
qdd-iterations.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
qdd-iterations.o: /usr/include/bits/stdlib-float.h ../lash-core/datastruct.h
qdd-iterations.o: /usr/include/string.h /usr/include/bits/types/locale_t.h
qdd-iterations.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
qdd-iterations.o: ../lash-core/auto-determinize.h
qdd-iterations.o: ../lash-core/auto-minimize.h ../lash-core/auto-product.h
qdd-iterations.o: ../lash-core/auto-sequential.h
qdd-iterations.o: ../lash-core/auto-difference.h
qdd-iterations.o: ../lash-core/auto-normalize.h ../lash-core/auto-quotient.h
qdd-iterations.o: ../lash-core/auto-count.h ../lash-core/biguint.h lash-qdd.h
qdd-iterations.o: qdd-symbol.h queue-operations.h qdd-operations.h
qdd-iterations.o: qdd-perform.h qdd-one-queue.h qdd-one-iterations.h
qdd-iterations.o: qdd-iterations.h qdd-machines.h qdd-automaton.h
qdd-iterations.o: qdd-iterations-utils.h
queue-operations.o: /usr/include/stdlib.h
queue-operations.o: /usr/include/bits/libc-header-start.h
queue-operations.o: /usr/include/features.h /usr/include/features-time64.h
queue-operations.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
queue-operations.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
queue-operations.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
queue-operations.o: /usr/include/stddef.h /usr/include/bits/waitflags.h
queue-operations.o: /usr/include/bits/waitstatus.h /usr/include/bits/floatn.h
queue-operations.o: /usr/include/bits/floatn-common.h
queue-operations.o: /usr/include/sys/types.h /usr/include/bits/types.h
queue-operations.o: /usr/include/bits/typesizes.h /usr/include/bits/time64.h
queue-operations.o: /usr/include/bits/types/clock_t.h
queue-operations.o: /usr/include/bits/types/clockid_t.h
queue-operations.o: /usr/include/bits/types/time_t.h
queue-operations.o: /usr/include/bits/types/timer_t.h
queue-operations.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
queue-operations.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
queue-operations.o: /usr/include/bits/byteswap.h
queue-operations.o: /usr/include/bits/uintn-identity.h
queue-operations.o: /usr/include/sys/select.h /usr/include/bits/select.h
queue-operations.o: /usr/include/bits/types/sigset_t.h
queue-operations.o: /usr/include/bits/types/__sigset_t.h
queue-operations.o: /usr/include/bits/types/struct_timeval.h
queue-operations.o: /usr/include/bits/types/struct_timespec.h
queue-operations.o: /usr/include/bits/pthreadtypes.h
queue-operations.o: /usr/include/bits/thread-shared-types.h
queue-operations.o: /usr/include/bits/pthreadtypes-arch.h
queue-operations.o: /usr/include/bits/atomic_wide_counter.h
queue-operations.o: /usr/include/bits/struct_mutex.h
queue-operations.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
queue-operations.o: /usr/include/bits/stdlib-float.h /usr/include/stdarg.h
queue-operations.o: /usr/include/string.h /usr/include/bits/types/locale_t.h
queue-operations.o: /usr/include/bits/types/__locale_t.h
queue-operations.o: /usr/include/strings.h ../lash-core/diag.h
queue-operations.o: ../lash-core/lash-diag.h ../lash-core/lash-types.h
queue-operations.o: ../lash-core/lash.h ../lash-core/resource.h
queue-operations.o: ../lash-core/arithmetic.h queue-operations.h qdd-symbol.h
qdd.o: /usr/include/stdlib.h /usr/include/bits/libc-header-start.h
qdd.o: /usr/include/features.h /usr/include/features-time64.h
qdd.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
qdd.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
qdd.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
qdd.o: /usr/include/stddef.h /usr/include/bits/waitflags.h
qdd.o: /usr/include/bits/waitstatus.h /usr/include/bits/floatn.h
qdd.o: /usr/include/bits/floatn-common.h /usr/include/sys/types.h
qdd.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
qdd.o: /usr/include/bits/time64.h /usr/include/bits/types/clock_t.h
qdd.o: /usr/include/bits/types/clockid_t.h /usr/include/bits/types/time_t.h
qdd.o: /usr/include/bits/types/timer_t.h /usr/include/bits/stdint-intn.h
qdd.o: /usr/include/endian.h /usr/include/bits/endian.h
qdd.o: /usr/include/bits/endianness.h /usr/include/bits/byteswap.h
qdd.o: /usr/include/bits/uintn-identity.h /usr/include/sys/select.h
qdd.o: /usr/include/bits/select.h /usr/include/bits/types/sigset_t.h
qdd.o: /usr/include/bits/types/__sigset_t.h
qdd.o: /usr/include/bits/types/struct_timeval.h
qdd.o: /usr/include/bits/types/struct_timespec.h
qdd.o: /usr/include/bits/pthreadtypes.h
qdd.o: /usr/include/bits/thread-shared-types.h
qdd.o: /usr/include/bits/pthreadtypes-arch.h
qdd.o: /usr/include/bits/atomic_wide_counter.h
qdd.o: /usr/include/bits/struct_mutex.h /usr/include/bits/struct_rwlock.h
qdd.o: /usr/include/alloca.h /usr/include/bits/stdlib-float.h
qdd.o: ../lash-core/lash-types.h ../lash-core/lash.h ../lash-core/resource.h
qdd.o: ../lash-core/datastruct.h /usr/include/string.h
qdd.o: /usr/include/bits/types/locale_t.h
qdd.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
qdd.o: ../lash-core/diag.h ../lash-core/lash-diag.h qdd.h
qdd.o: ../lash-core/lash-auto-operations.h ../lash-core/lash-auto.h
qdd.o: ../lash-core/auto-determinize.h ../lash-core/auto-minimize.h
qdd.o: ../lash-core/auto-product.h ../lash-core/auto-sequential.h
qdd.o: ../lash-core/auto-difference.h ../lash-core/auto-normalize.h
qdd.o: ../lash-core/auto-quotient.h ../lash-core/auto-count.h
qdd.o: ../lash-core/biguint.h lash-qdd.h qdd-symbol.h queue-operations.h
qdd.o: qdd-operations.h qdd-perform.h qdd-one-queue.h qdd-one-iterations.h
qdd.o: qdd-iterations.h qdd-machines.h qdd-automaton.h
qdd-automaton.o: ../lash-core/lash-auto.h /usr/include/stdlib.h
qdd-automaton.o: /usr/include/bits/libc-header-start.h
qdd-automaton.o: /usr/include/features.h /usr/include/features-time64.h
qdd-automaton.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
qdd-automaton.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
qdd-automaton.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
qdd-automaton.o: /usr/include/stddef.h /usr/include/bits/waitflags.h
qdd-automaton.o: /usr/include/bits/waitstatus.h /usr/include/bits/floatn.h
qdd-automaton.o: /usr/include/bits/floatn-common.h /usr/include/sys/types.h
qdd-automaton.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
qdd-automaton.o: /usr/include/bits/time64.h /usr/include/bits/types/clock_t.h
qdd-automaton.o: /usr/include/bits/types/clockid_t.h
qdd-automaton.o: /usr/include/bits/types/time_t.h
qdd-automaton.o: /usr/include/bits/types/timer_t.h
qdd-automaton.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
qdd-automaton.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
qdd-automaton.o: /usr/include/bits/byteswap.h
qdd-automaton.o: /usr/include/bits/uintn-identity.h /usr/include/sys/select.h
qdd-automaton.o: /usr/include/bits/select.h
qdd-automaton.o: /usr/include/bits/types/sigset_t.h
qdd-automaton.o: /usr/include/bits/types/__sigset_t.h
qdd-automaton.o: /usr/include/bits/types/struct_timeval.h
qdd-automaton.o: /usr/include/bits/types/struct_timespec.h
qdd-automaton.o: /usr/include/bits/pthreadtypes.h
qdd-automaton.o: /usr/include/bits/thread-shared-types.h
qdd-automaton.o: /usr/include/bits/pthreadtypes-arch.h
qdd-automaton.o: /usr/include/bits/atomic_wide_counter.h
qdd-automaton.o: /usr/include/bits/struct_mutex.h
qdd-automaton.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
qdd-automaton.o: /usr/include/bits/stdlib-float.h ../lash-core/lash-types.h
qdd-automaton.o: ../lash-core/lash.h ../lash-core/datastruct.h
qdd-automaton.o: /usr/include/string.h /usr/include/bits/types/locale_t.h
qdd-automaton.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
qdd-automaton.o: qdd.h ../lash-core/lash-auto-operations.h
qdd-automaton.o: ../lash-core/auto-determinize.h ../lash-core/auto-minimize.h
qdd-automaton.o: ../lash-core/auto-product.h ../lash-core/auto-sequential.h
qdd-automaton.o: ../lash-core/auto-difference.h ../lash-core/auto-normalize.h
qdd-automaton.o: ../lash-core/auto-quotient.h ../lash-core/auto-count.h
qdd-automaton.o: ../lash-core/biguint.h lash-qdd.h qdd-symbol.h
qdd-automaton.o: queue-operations.h qdd-operations.h qdd-perform.h
qdd-automaton.o: qdd-one-queue.h qdd-one-iterations.h qdd-iterations.h
qdd-automaton.o: qdd-machines.h qdd-automaton.h
qdd-symbol.o: qdd-symbol.h ../lash-core/lash-types.h ../lash-core/lash.h
qdd-one-queue.o: ../lash-core/diag.h ../lash-core/lash-diag.h
qdd-one-queue.o: ../lash-core/lash-types.h ../lash-core/lash.h
qdd-one-queue.o: ../lash-core/resource.h ../lash-core/datastruct.h
qdd-one-queue.o: /usr/include/string.h /usr/include/bits/libc-header-start.h
qdd-one-queue.o: /usr/include/features.h /usr/include/features-time64.h
qdd-one-queue.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
qdd-one-queue.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
qdd-one-queue.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
qdd-one-queue.o: /usr/include/stddef.h /usr/include/bits/types/locale_t.h
qdd-one-queue.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
qdd-one-queue.o: ../lash-core/auto.h ../lash-core/lash-auto.h
qdd-one-queue.o: /usr/include/stdlib.h /usr/include/bits/waitflags.h
qdd-one-queue.o: /usr/include/bits/waitstatus.h /usr/include/bits/floatn.h
qdd-one-queue.o: /usr/include/bits/floatn-common.h /usr/include/sys/types.h
qdd-one-queue.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
qdd-one-queue.o: /usr/include/bits/time64.h /usr/include/bits/types/clock_t.h
qdd-one-queue.o: /usr/include/bits/types/clockid_t.h
qdd-one-queue.o: /usr/include/bits/types/time_t.h
qdd-one-queue.o: /usr/include/bits/types/timer_t.h
qdd-one-queue.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
qdd-one-queue.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
qdd-one-queue.o: /usr/include/bits/byteswap.h
qdd-one-queue.o: /usr/include/bits/uintn-identity.h /usr/include/sys/select.h
qdd-one-queue.o: /usr/include/bits/select.h
qdd-one-queue.o: /usr/include/bits/types/sigset_t.h
qdd-one-queue.o: /usr/include/bits/types/__sigset_t.h
qdd-one-queue.o: /usr/include/bits/types/struct_timeval.h
qdd-one-queue.o: /usr/include/bits/types/struct_timespec.h
qdd-one-queue.o: /usr/include/bits/pthreadtypes.h
qdd-one-queue.o: /usr/include/bits/thread-shared-types.h
qdd-one-queue.o: /usr/include/bits/pthreadtypes-arch.h
qdd-one-queue.o: /usr/include/bits/atomic_wide_counter.h
qdd-one-queue.o: /usr/include/bits/struct_mutex.h
qdd-one-queue.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
qdd-one-queue.o: /usr/include/bits/stdlib-float.h
qdd-one-queue.o: ../lash-core/lash-auto-operations.h
qdd-one-queue.o: ../lash-core/auto-determinize.h ../lash-core/auto-minimize.h
qdd-one-queue.o: ../lash-core/auto-product.h ../lash-core/auto-sequential.h
qdd-one-queue.o: ../lash-core/auto-difference.h ../lash-core/auto-normalize.h
qdd-one-queue.o: ../lash-core/auto-quotient.h ../lash-core/auto-count.h
qdd-one-queue.o: ../lash-core/biguint.h queue-operations.h qdd-symbol.h qdd.h
qdd-one-queue.o: lash-qdd.h qdd-operations.h qdd-perform.h qdd-one-queue.h
qdd-one-queue.o: qdd-one-iterations.h qdd-iterations.h qdd-machines.h
qdd-one-queue.o: qdd-automaton.h
qdd-perform.o: ../lash-core/diag.h ../lash-core/lash-diag.h
qdd-perform.o: ../lash-core/lash-types.h ../lash-core/lash.h
qdd-perform.o: ../lash-core/resource.h ../lash-core/datastruct.h
qdd-perform.o: /usr/include/string.h /usr/include/bits/libc-header-start.h
qdd-perform.o: /usr/include/features.h /usr/include/features-time64.h
qdd-perform.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
qdd-perform.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
qdd-perform.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
qdd-perform.o: /usr/include/stddef.h /usr/include/bits/types/locale_t.h
qdd-perform.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
qdd-perform.o: ../lash-core/auto.h ../lash-core/lash-auto.h
qdd-perform.o: /usr/include/stdlib.h /usr/include/bits/waitflags.h
qdd-perform.o: /usr/include/bits/waitstatus.h /usr/include/bits/floatn.h
qdd-perform.o: /usr/include/bits/floatn-common.h /usr/include/sys/types.h
qdd-perform.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
qdd-perform.o: /usr/include/bits/time64.h /usr/include/bits/types/clock_t.h
qdd-perform.o: /usr/include/bits/types/clockid_t.h
qdd-perform.o: /usr/include/bits/types/time_t.h
qdd-perform.o: /usr/include/bits/types/timer_t.h
qdd-perform.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
qdd-perform.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
qdd-perform.o: /usr/include/bits/byteswap.h
qdd-perform.o: /usr/include/bits/uintn-identity.h /usr/include/sys/select.h
qdd-perform.o: /usr/include/bits/select.h /usr/include/bits/types/sigset_t.h
qdd-perform.o: /usr/include/bits/types/__sigset_t.h
qdd-perform.o: /usr/include/bits/types/struct_timeval.h
qdd-perform.o: /usr/include/bits/types/struct_timespec.h
qdd-perform.o: /usr/include/bits/pthreadtypes.h
qdd-perform.o: /usr/include/bits/thread-shared-types.h
qdd-perform.o: /usr/include/bits/pthreadtypes-arch.h
qdd-perform.o: /usr/include/bits/atomic_wide_counter.h
qdd-perform.o: /usr/include/bits/struct_mutex.h
qdd-perform.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
qdd-perform.o: /usr/include/bits/stdlib-float.h
qdd-perform.o: ../lash-core/lash-auto-operations.h
qdd-perform.o: ../lash-core/auto-determinize.h ../lash-core/auto-minimize.h
qdd-perform.o: ../lash-core/auto-product.h ../lash-core/auto-sequential.h
qdd-perform.o: ../lash-core/auto-difference.h ../lash-core/auto-normalize.h
qdd-perform.o: ../lash-core/auto-quotient.h ../lash-core/auto-count.h
qdd-perform.o: ../lash-core/biguint.h qdd-iterations-utils.h
qdd-perform.o: queue-operations.h qdd-symbol.h qdd.h lash-qdd.h
qdd-perform.o: qdd-operations.h qdd-perform.h qdd-one-queue.h
qdd-perform.o: qdd-one-iterations.h qdd-iterations.h qdd-machines.h
qdd-perform.o: qdd-automaton.h
qdd-operations.o: ../lash-core/diag.h ../lash-core/lash-diag.h
qdd-operations.o: ../lash-core/lash-types.h ../lash-core/lash.h
qdd-operations.o: ../lash-core/resource.h qdd.h
qdd-operations.o: ../lash-core/lash-auto-operations.h
qdd-operations.o: ../lash-core/lash-auto.h /usr/include/stdlib.h
qdd-operations.o: /usr/include/bits/libc-header-start.h
qdd-operations.o: /usr/include/features.h /usr/include/features-time64.h
qdd-operations.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
qdd-operations.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
qdd-operations.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
qdd-operations.o: /usr/include/stddef.h /usr/include/bits/waitflags.h
qdd-operations.o: /usr/include/bits/waitstatus.h /usr/include/bits/floatn.h
qdd-operations.o: /usr/include/bits/floatn-common.h /usr/include/sys/types.h
qdd-operations.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
qdd-operations.o: /usr/include/bits/time64.h
qdd-operations.o: /usr/include/bits/types/clock_t.h
qdd-operations.o: /usr/include/bits/types/clockid_t.h
qdd-operations.o: /usr/include/bits/types/time_t.h
qdd-operations.o: /usr/include/bits/types/timer_t.h
qdd-operations.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
qdd-operations.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
qdd-operations.o: /usr/include/bits/byteswap.h
qdd-operations.o: /usr/include/bits/uintn-identity.h
qdd-operations.o: /usr/include/sys/select.h /usr/include/bits/select.h
qdd-operations.o: /usr/include/bits/types/sigset_t.h
qdd-operations.o: /usr/include/bits/types/__sigset_t.h
qdd-operations.o: /usr/include/bits/types/struct_timeval.h
qdd-operations.o: /usr/include/bits/types/struct_timespec.h
qdd-operations.o: /usr/include/bits/pthreadtypes.h
qdd-operations.o: /usr/include/bits/thread-shared-types.h
qdd-operations.o: /usr/include/bits/pthreadtypes-arch.h
qdd-operations.o: /usr/include/bits/atomic_wide_counter.h
qdd-operations.o: /usr/include/bits/struct_mutex.h
qdd-operations.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
qdd-operations.o: /usr/include/bits/stdlib-float.h ../lash-core/datastruct.h
qdd-operations.o: /usr/include/string.h /usr/include/bits/types/locale_t.h
qdd-operations.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
qdd-operations.o: ../lash-core/auto-determinize.h
qdd-operations.o: ../lash-core/auto-minimize.h ../lash-core/auto-product.h
qdd-operations.o: ../lash-core/auto-sequential.h
qdd-operations.o: ../lash-core/auto-difference.h
qdd-operations.o: ../lash-core/auto-normalize.h ../lash-core/auto-quotient.h
qdd-operations.o: ../lash-core/auto-count.h ../lash-core/biguint.h lash-qdd.h
qdd-operations.o: qdd-symbol.h queue-operations.h qdd-operations.h
qdd-operations.o: qdd-perform.h qdd-one-queue.h qdd-one-iterations.h
qdd-operations.o: qdd-iterations.h qdd-machines.h qdd-automaton.h
qdd-iterations-utils.o: ../lash-core/lash-types.h ../lash-core/lash.h
qdd-iterations-utils.o: ../lash-core/datastruct.h /usr/include/string.h
qdd-iterations-utils.o: /usr/include/bits/libc-header-start.h
qdd-iterations-utils.o: /usr/include/features.h
qdd-iterations-utils.o: /usr/include/features-time64.h
qdd-iterations-utils.o: /usr/include/bits/wordsize.h
qdd-iterations-utils.o: /usr/include/bits/timesize.h
qdd-iterations-utils.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
qdd-iterations-utils.o: /usr/include/bits/long-double.h
qdd-iterations-utils.o: /usr/include/gnu/stubs.h /usr/include/stddef.h
qdd-iterations-utils.o: /usr/include/bits/types/locale_t.h
qdd-iterations-utils.o: /usr/include/bits/types/__locale_t.h
qdd-iterations-utils.o: /usr/include/strings.h ../lash-core/diag.h
qdd-iterations-utils.o: ../lash-core/lash-diag.h ../lash-core/resource.h
qdd-iterations-utils.o: queue-operations.h qdd-symbol.h qdd.h
qdd-iterations-utils.o: ../lash-core/lash-auto-operations.h
qdd-iterations-utils.o: ../lash-core/lash-auto.h /usr/include/stdlib.h
qdd-iterations-utils.o: /usr/include/bits/waitflags.h
qdd-iterations-utils.o: /usr/include/bits/waitstatus.h
qdd-iterations-utils.o: /usr/include/bits/floatn.h
qdd-iterations-utils.o: /usr/include/bits/floatn-common.h
qdd-iterations-utils.o: /usr/include/sys/types.h /usr/include/bits/types.h
qdd-iterations-utils.o: /usr/include/bits/typesizes.h
qdd-iterations-utils.o: /usr/include/bits/time64.h
qdd-iterations-utils.o: /usr/include/bits/types/clock_t.h
qdd-iterations-utils.o: /usr/include/bits/types/clockid_t.h
qdd-iterations-utils.o: /usr/include/bits/types/time_t.h
qdd-iterations-utils.o: /usr/include/bits/types/timer_t.h
qdd-iterations-utils.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
qdd-iterations-utils.o: /usr/include/bits/endian.h
qdd-iterations-utils.o: /usr/include/bits/endianness.h
qdd-iterations-utils.o: /usr/include/bits/byteswap.h
qdd-iterations-utils.o: /usr/include/bits/uintn-identity.h
qdd-iterations-utils.o: /usr/include/sys/select.h /usr/include/bits/select.h
qdd-iterations-utils.o: /usr/include/bits/types/sigset_t.h
qdd-iterations-utils.o: /usr/include/bits/types/__sigset_t.h
qdd-iterations-utils.o: /usr/include/bits/types/struct_timeval.h
qdd-iterations-utils.o: /usr/include/bits/types/struct_timespec.h
qdd-iterations-utils.o: /usr/include/bits/pthreadtypes.h
qdd-iterations-utils.o: /usr/include/bits/thread-shared-types.h
qdd-iterations-utils.o: /usr/include/bits/pthreadtypes-arch.h
qdd-iterations-utils.o: /usr/include/bits/atomic_wide_counter.h
qdd-iterations-utils.o: /usr/include/bits/struct_mutex.h
qdd-iterations-utils.o: /usr/include/bits/struct_rwlock.h
qdd-iterations-utils.o: /usr/include/alloca.h
qdd-iterations-utils.o: /usr/include/bits/stdlib-float.h
qdd-iterations-utils.o: ../lash-core/auto-determinize.h
qdd-iterations-utils.o: ../lash-core/auto-minimize.h
qdd-iterations-utils.o: ../lash-core/auto-product.h
qdd-iterations-utils.o: ../lash-core/auto-sequential.h
qdd-iterations-utils.o: ../lash-core/auto-difference.h
qdd-iterations-utils.o: ../lash-core/auto-normalize.h
qdd-iterations-utils.o: ../lash-core/auto-quotient.h
qdd-iterations-utils.o: ../lash-core/auto-count.h ../lash-core/biguint.h
qdd-iterations-utils.o: lash-qdd.h qdd-operations.h qdd-perform.h
qdd-iterations-utils.o: qdd-one-queue.h qdd-one-iterations.h qdd-iterations.h
qdd-iterations-utils.o: qdd-machines.h qdd-automaton.h qdd-iterations-utils.h
qdd-machines.o: /usr/include/stdlib.h /usr/include/bits/libc-header-start.h
qdd-machines.o: /usr/include/features.h /usr/include/features-time64.h
qdd-machines.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
qdd-machines.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
qdd-machines.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
qdd-machines.o: /usr/include/stddef.h /usr/include/bits/waitflags.h
qdd-machines.o: /usr/include/bits/waitstatus.h /usr/include/bits/floatn.h
qdd-machines.o: /usr/include/bits/floatn-common.h /usr/include/sys/types.h
qdd-machines.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
qdd-machines.o: /usr/include/bits/time64.h /usr/include/bits/types/clock_t.h
qdd-machines.o: /usr/include/bits/types/clockid_t.h
qdd-machines.o: /usr/include/bits/types/time_t.h
qdd-machines.o: /usr/include/bits/types/timer_t.h
qdd-machines.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
qdd-machines.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
qdd-machines.o: /usr/include/bits/byteswap.h
qdd-machines.o: /usr/include/bits/uintn-identity.h /usr/include/sys/select.h
qdd-machines.o: /usr/include/bits/select.h /usr/include/bits/types/sigset_t.h
qdd-machines.o: /usr/include/bits/types/__sigset_t.h
qdd-machines.o: /usr/include/bits/types/struct_timeval.h
qdd-machines.o: /usr/include/bits/types/struct_timespec.h
qdd-machines.o: /usr/include/bits/pthreadtypes.h
qdd-machines.o: /usr/include/bits/thread-shared-types.h
qdd-machines.o: /usr/include/bits/pthreadtypes-arch.h
qdd-machines.o: /usr/include/bits/atomic_wide_counter.h
qdd-machines.o: /usr/include/bits/struct_mutex.h
qdd-machines.o: /usr/include/bits/struct_rwlock.h /usr/include/alloca.h
qdd-machines.o: /usr/include/bits/stdlib-float.h /usr/include/string.h
qdd-machines.o: /usr/include/bits/types/locale_t.h
qdd-machines.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
qdd-machines.o: ../lash-core/diag.h ../lash-core/lash-diag.h
qdd-machines.o: ../lash-core/lash-types.h ../lash-core/lash.h
qdd-machines.o: ../lash-core/resource.h ../lash-core/datastruct.h
qdd-machines.o: ../lash-core/lash-auto.h ../lash-core/auto.h
qdd-machines.o: ../lash-core/lash-auto-operations.h
qdd-machines.o: ../lash-core/auto-determinize.h ../lash-core/auto-minimize.h
qdd-machines.o: ../lash-core/auto-product.h ../lash-core/auto-sequential.h
qdd-machines.o: ../lash-core/auto-difference.h ../lash-core/auto-normalize.h
qdd-machines.o: ../lash-core/auto-quotient.h ../lash-core/auto-count.h
qdd-machines.o: ../lash-core/biguint.h lash-qdd.h qdd-symbol.h
qdd-machines.o: queue-operations.h qdd-operations.h qdd-perform.h
qdd-machines.o: qdd-one-queue.h qdd-one-iterations.h qdd-iterations.h
qdd-machines.o: qdd-machines.h qdd.h qdd-automaton.h
qdd-io.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
qdd-io.o: /usr/include/features.h /usr/include/features-time64.h
qdd-io.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
qdd-io.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
qdd-io.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
qdd-io.o: /usr/include/stddef.h /usr/include/stdarg.h
qdd-io.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
qdd-io.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
qdd-io.o: /usr/include/bits/types/__mbstate_t.h
qdd-io.o: /usr/include/bits/types/__fpos64_t.h
qdd-io.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
qdd-io.o: /usr/include/bits/types/struct_FILE.h
qdd-io.o: /usr/include/bits/types/cookie_io_functions_t.h
qdd-io.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
qdd-io.o: /usr/include/bits/floatn-common.h ../lash-core/diag.h
qdd-io.o: ../lash-core/lash-diag.h ../lash-core/lash-types.h
qdd-io.o: ../lash-core/lash.h lash-qdd.h ../lash-core/lash-auto.h
qdd-io.o: /usr/include/stdlib.h /usr/include/bits/waitflags.h
qdd-io.o: /usr/include/bits/waitstatus.h /usr/include/sys/types.h
qdd-io.o: /usr/include/bits/types/clock_t.h
qdd-io.o: /usr/include/bits/types/clockid_t.h
qdd-io.o: /usr/include/bits/types/time_t.h /usr/include/bits/types/timer_t.h
qdd-io.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
qdd-io.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
qdd-io.o: /usr/include/bits/byteswap.h /usr/include/bits/uintn-identity.h
qdd-io.o: /usr/include/sys/select.h /usr/include/bits/select.h
qdd-io.o: /usr/include/bits/types/sigset_t.h
qdd-io.o: /usr/include/bits/types/__sigset_t.h
qdd-io.o: /usr/include/bits/types/struct_timeval.h
qdd-io.o: /usr/include/bits/types/struct_timespec.h
qdd-io.o: /usr/include/bits/pthreadtypes.h
qdd-io.o: /usr/include/bits/thread-shared-types.h
qdd-io.o: /usr/include/bits/pthreadtypes-arch.h
qdd-io.o: /usr/include/bits/atomic_wide_counter.h
qdd-io.o: /usr/include/bits/struct_mutex.h /usr/include/bits/struct_rwlock.h
qdd-io.o: /usr/include/alloca.h /usr/include/bits/stdlib-float.h
qdd-io.o: ../lash-core/datastruct.h /usr/include/string.h
qdd-io.o: /usr/include/bits/types/locale_t.h
qdd-io.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
qdd-io.o: qdd-symbol.h queue-operations.h qdd-operations.h qdd-perform.h
qdd-io.o: qdd-one-queue.h qdd-one-iterations.h qdd-iterations.h
qdd-io.o: qdd-machines.h ../lash-core/lash-auto-io.h ../lash-core/lash-io.h
qdd-io.o: ../lash-core/datastruct-io.h ../lash-core/auto-io-print.h
qdd-io.o: ../lash-core/auto-io-dots.h qdd.h
qdd-io.o: ../lash-core/lash-auto-operations.h ../lash-core/auto-determinize.h
qdd-io.o: ../lash-core/auto-minimize.h ../lash-core/auto-product.h
qdd-io.o: ../lash-core/auto-sequential.h ../lash-core/auto-difference.h
qdd-io.o: ../lash-core/auto-normalize.h ../lash-core/auto-quotient.h
qdd-io.o: ../lash-core/auto-count.h ../lash-core/biguint.h qdd-automaton.h
qdd-io.o: lash-qdd-io.h
