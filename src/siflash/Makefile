######################################################################
##                                                                  ##
##  Simple IF LASH (Siflash) compiler -- v0.9                       ##
##  =================================                               ##
##                                                                  ##
##     Makefile.                                                    ##
##                                                                  ##
##     06/30/00  :  Creation. (BB)                                  ##
##     02/12/01  :  Reorganization. (BB)                            ##
##     08/13/01  :  New source tree. (BB)                           ##
##     09/06/02  :  Reorganization. (BB)                            ##
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

SRCS-SIFLASH =	lexical.c \
		semantic.c \
		program.c \
		output.c \
		explore.c \
		siflash.c

OBJS-SIFLASH =	${SRCS-SIFLASH:.c=.o}

CFLAGS =  -g -c -O3 -ansi -Wall -Wtraditional -Wpointer-arith \
         -Waggregate-return -Wstrict-prototypes \
         -Wmissing-prototypes -Wmissing-declarations \
         -I$(LASH-CORE-DIR) -I$(LASH-NDD-DIR)

all: siflash

siflash: $(OBJS-SIFLASH)
	$(CC) -g -o siflash $(OBJS-SIFLASH) $(LASH-CORE) \
        $(LASH-ARITH) $(NDD-PACKAGE)
	mv siflash $(LASH-BIN-DIR)

clean:
	-rm -f $(OBJS-SIFLASH) siflash $(LASH-BIN-DIR)/siflash

depend: 
	makedepend -I$(LASH-CORE-DIR) -I$(LASH-NDD-DIR) $(SRCS-SIFLASH)

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
lexical.o: ../lash-core/lash-diag.h ../lash-core/resource.h siflash.h
lexical.o: lexical.h program.h ../lash-core/datastruct.h ../ndd/lash-ndd-io.h
lexical.o: ../lash-core/datastruct-io.h ../lash-core/lash-auto.h
lexical.o: ../lash-core/lash-io.h ../lash-core/lash-auto-io.h
lexical.o: ../lash-core/auto-io-print.h ../lash-core/auto-io-dots.h
lexical.o: ../ndd/lash-ndd.h ../ndd/ndd-sets.h ../ndd/ndd.h
lexical.o: ../lash-core/auto.h ../ndd/ndd-operations.h ../ndd/ndd-tests.h
lexical.o: ../ndd/ndd-equations.h ../ndd/ndd-projection.h
lexical.o: ../ndd/ndd-transformations.h ../ndd/ndd-iterations.h
lexical.o: ../ndd/ndd-machines.h
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
semantic.o: siflash.h lexical.h semantic.h program.h ../ndd/lash-ndd-io.h
semantic.o: ../lash-core/datastruct-io.h ../lash-core/lash-auto.h
semantic.o: /usr/include/stdlib.h /usr/include/bits/waitflags.h
semantic.o: /usr/include/bits/waitstatus.h /usr/include/sys/types.h
semantic.o: /usr/include/bits/types/clock_t.h
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
semantic.o: /usr/include/bits/stdlib-float.h ../lash-core/lash-io.h
semantic.o: ../lash-core/lash-auto-io.h ../lash-core/auto-io-print.h
semantic.o: ../lash-core/auto-io-dots.h ../ndd/lash-ndd.h ../ndd/ndd-sets.h
semantic.o: ../ndd/ndd.h ../lash-core/auto.h ../ndd/ndd-operations.h
semantic.o: ../ndd/ndd-tests.h ../ndd/ndd-equations.h ../ndd/ndd-projection.h
semantic.o: ../ndd/ndd-transformations.h ../ndd/ndd-iterations.h
semantic.o: ../ndd/ndd-machines.h
program.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
program.o: /usr/include/features.h /usr/include/features-time64.h
program.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
program.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
program.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
program.o: /usr/include/stddef.h /usr/include/stdarg.h
program.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
program.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
program.o: /usr/include/bits/types/__mbstate_t.h
program.o: /usr/include/bits/types/__fpos64_t.h
program.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
program.o: /usr/include/bits/types/struct_FILE.h
program.o: /usr/include/bits/types/cookie_io_functions_t.h
program.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
program.o: /usr/include/bits/floatn-common.h ../lash-core/lash.h
program.o: ../lash-core/lash-types.h ../lash-core/resource.h
program.o: ../lash-core/datastruct.h /usr/include/string.h
program.o: /usr/include/bits/types/locale_t.h
program.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
program.o: siflash.h program.h ../ndd/lash-ndd-io.h
program.o: ../lash-core/datastruct-io.h ../lash-core/lash-auto.h
program.o: /usr/include/stdlib.h /usr/include/bits/waitflags.h
program.o: /usr/include/bits/waitstatus.h /usr/include/sys/types.h
program.o: /usr/include/bits/types/clock_t.h
program.o: /usr/include/bits/types/clockid_t.h
program.o: /usr/include/bits/types/time_t.h /usr/include/bits/types/timer_t.h
program.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
program.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
program.o: /usr/include/bits/byteswap.h /usr/include/bits/uintn-identity.h
program.o: /usr/include/sys/select.h /usr/include/bits/select.h
program.o: /usr/include/bits/types/sigset_t.h
program.o: /usr/include/bits/types/__sigset_t.h
program.o: /usr/include/bits/types/struct_timeval.h
program.o: /usr/include/bits/types/struct_timespec.h
program.o: /usr/include/bits/pthreadtypes.h
program.o: /usr/include/bits/thread-shared-types.h
program.o: /usr/include/bits/pthreadtypes-arch.h
program.o: /usr/include/bits/atomic_wide_counter.h
program.o: /usr/include/bits/struct_mutex.h /usr/include/bits/struct_rwlock.h
program.o: /usr/include/alloca.h /usr/include/bits/stdlib-float.h
program.o: ../lash-core/lash-io.h ../lash-core/lash-auto-io.h
program.o: ../lash-core/auto-io-print.h ../lash-core/auto-io-dots.h
program.o: ../ndd/lash-ndd.h ../ndd/ndd-sets.h ../ndd/ndd.h
program.o: ../lash-core/auto.h ../ndd/ndd-operations.h ../ndd/ndd-tests.h
program.o: ../ndd/ndd-equations.h ../ndd/ndd-projection.h
program.o: ../ndd/ndd-transformations.h ../ndd/ndd-iterations.h
program.o: ../ndd/ndd-machines.h output.h ../lash-core/arithmetic.h explore.h
program.o: ../lash-core/biguint.h ../ndd/ndd-count.h
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
output.o: siflash.h program.h ../ndd/lash-ndd-io.h
output.o: ../lash-core/datastruct-io.h ../lash-core/lash-auto.h
output.o: /usr/include/stdlib.h /usr/include/bits/waitflags.h
output.o: /usr/include/bits/waitstatus.h /usr/include/sys/types.h
output.o: /usr/include/bits/types/clock_t.h
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
output.o: ../lash-core/lash-io.h ../lash-core/lash-auto-io.h
output.o: ../lash-core/auto-io-print.h ../lash-core/auto-io-dots.h
output.o: ../ndd/lash-ndd.h ../ndd/ndd-sets.h ../ndd/ndd.h
output.o: ../lash-core/auto.h ../ndd/ndd-operations.h ../ndd/ndd-tests.h
output.o: ../ndd/ndd-equations.h ../ndd/ndd-projection.h
output.o: ../ndd/ndd-transformations.h ../ndd/ndd-iterations.h
output.o: ../ndd/ndd-machines.h output.h
explore.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
explore.o: /usr/include/features.h /usr/include/features-time64.h
explore.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
explore.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
explore.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
explore.o: /usr/include/stddef.h /usr/include/stdarg.h
explore.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
explore.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
explore.o: /usr/include/bits/types/__mbstate_t.h
explore.o: /usr/include/bits/types/__fpos64_t.h
explore.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
explore.o: /usr/include/bits/types/struct_FILE.h
explore.o: /usr/include/bits/types/cookie_io_functions_t.h
explore.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
explore.o: /usr/include/bits/floatn-common.h ../lash-core/lash.h
explore.o: ../lash-core/lash-types.h ../lash-core/lash-diag.h
explore.o: ../lash-core/lash-auto.h /usr/include/stdlib.h
explore.o: /usr/include/bits/waitflags.h /usr/include/bits/waitstatus.h
explore.o: /usr/include/sys/types.h /usr/include/bits/types/clock_t.h
explore.o: /usr/include/bits/types/clockid_t.h
explore.o: /usr/include/bits/types/time_t.h /usr/include/bits/types/timer_t.h
explore.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
explore.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
explore.o: /usr/include/bits/byteswap.h /usr/include/bits/uintn-identity.h
explore.o: /usr/include/sys/select.h /usr/include/bits/select.h
explore.o: /usr/include/bits/types/sigset_t.h
explore.o: /usr/include/bits/types/__sigset_t.h
explore.o: /usr/include/bits/types/struct_timeval.h
explore.o: /usr/include/bits/types/struct_timespec.h
explore.o: /usr/include/bits/pthreadtypes.h
explore.o: /usr/include/bits/thread-shared-types.h
explore.o: /usr/include/bits/pthreadtypes-arch.h
explore.o: /usr/include/bits/atomic_wide_counter.h
explore.o: /usr/include/bits/struct_mutex.h /usr/include/bits/struct_rwlock.h
explore.o: /usr/include/alloca.h /usr/include/bits/stdlib-float.h
explore.o: ../lash-core/datastruct.h /usr/include/string.h
explore.o: /usr/include/bits/types/locale_t.h
explore.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
explore.o: ../ndd/lash-ndd.h ../ndd/ndd-sets.h ../ndd/ndd.h
explore.o: ../lash-core/auto.h ../ndd/ndd-operations.h ../ndd/ndd-tests.h
explore.o: ../ndd/ndd-equations.h ../ndd/ndd-projection.h
explore.o: ../ndd/ndd-transformations.h ../ndd/ndd-iterations.h
explore.o: ../ndd/ndd-machines.h ../lash-core/resource.h siflash.h program.h
explore.o: ../ndd/lash-ndd-io.h ../lash-core/datastruct-io.h
explore.o: ../lash-core/lash-io.h ../lash-core/lash-auto-io.h
explore.o: ../lash-core/auto-io-print.h ../lash-core/auto-io-dots.h
explore.o: ../lash-core/arithmetic.h explore.h ../lash-core/biguint.h
explore.o: ../ndd/ndd-count.h ../lash-core/diag.h
siflash.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
siflash.o: /usr/include/features.h /usr/include/features-time64.h
siflash.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
siflash.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
siflash.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
siflash.o: /usr/include/stddef.h /usr/include/stdarg.h
siflash.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
siflash.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
siflash.o: /usr/include/bits/types/__mbstate_t.h
siflash.o: /usr/include/bits/types/__fpos64_t.h
siflash.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
siflash.o: /usr/include/bits/types/struct_FILE.h
siflash.o: /usr/include/bits/types/cookie_io_functions_t.h
siflash.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
siflash.o: /usr/include/bits/floatn-common.h /usr/include/stdlib.h
siflash.o: /usr/include/bits/waitflags.h /usr/include/bits/waitstatus.h
siflash.o: /usr/include/sys/types.h /usr/include/bits/types/clock_t.h
siflash.o: /usr/include/bits/types/clockid_t.h
siflash.o: /usr/include/bits/types/time_t.h /usr/include/bits/types/timer_t.h
siflash.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
siflash.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
siflash.o: /usr/include/bits/byteswap.h /usr/include/bits/uintn-identity.h
siflash.o: /usr/include/sys/select.h /usr/include/bits/select.h
siflash.o: /usr/include/bits/types/sigset_t.h
siflash.o: /usr/include/bits/types/__sigset_t.h
siflash.o: /usr/include/bits/types/struct_timeval.h
siflash.o: /usr/include/bits/types/struct_timespec.h
siflash.o: /usr/include/bits/pthreadtypes.h
siflash.o: /usr/include/bits/thread-shared-types.h
siflash.o: /usr/include/bits/pthreadtypes-arch.h
siflash.o: /usr/include/bits/atomic_wide_counter.h
siflash.o: /usr/include/bits/struct_mutex.h /usr/include/bits/struct_rwlock.h
siflash.o: /usr/include/alloca.h /usr/include/bits/stdlib-float.h
siflash.o: /usr/include/string.h /usr/include/bits/types/locale_t.h
siflash.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
siflash.o: ../lash-core/lash-types.h ../lash-core/lash.h
siflash.o: ../lash-core/lash-diag.h ../lash-core/datastruct.h siflash.h
siflash.o: lexical.h grammar.h semantic.h
