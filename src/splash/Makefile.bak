######################################################################
##                                                                  ##
##  LASH Simple-Promela compiler -- v0.9                            ##
##  ============================                                    ##
##                                                                  ##
##     Makefile.                                                    ##
##                                                                  ##
##     06/30/00  :  Creation. (BB)                                  ##
##     02/12/01  :  Reorganization. (BB)                            ##
##     08/13/01  :  New source tree. (BB)                           ##
##     07/17/02  :  Reorganization. (BB)                            ##
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

SRCS-SPLASH =	lexical.c \
		semantic.c \
		program.c \
		output.c \
		explore.c \
		splash.c

OBJS-SPLASH =	${SRCS-SPLASH:.c=.o}

CFLAGS =  -g -c -O3 -ansi -Wall -Wtraditional -Wpointer-arith \
         -Waggregate-return -Wstrict-prototypes \
         -Wmissing-prototypes -Wmissing-declarations \
         -I$(LASH-CORE-DIR) -I$(LASH-NDD-DIR)

all: splash

splash: $(OBJS-SPLASH)
	$(CC) -g -o splash $(OBJS-SPLASH) $(LASH-CORE) $(LASH-ARITH) \
        $(NDD-PACKAGE)
	mv splash $(LASH-BIN-DIR)

clean:
	-rm -f $(OBJS-SPLASH) splash $(LASH-BIN-DIR)/splash

depend: 
	makedepend -I$(LASH-CORE-DIR) -I$(LASH-NDD-DIR) $(SRCS-SPLASH)

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
lexical.o: ../lash-core/lash-diag.h ../lash-core/resource.h splash.h
lexical.o: lexical.h
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
semantic.o: splash.h lexical.h semantic.h program.h
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
program.o: splash.h program.h output.h explore.h
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
output.o: splash.h program.h output.h
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
explore.o: ../ndd/ndd-machines.h ../lash-core/resource.h splash.h program.h
explore.o: explore.h
splash.o: /usr/include/stdio.h /usr/include/bits/libc-header-start.h
splash.o: /usr/include/features.h /usr/include/features-time64.h
splash.o: /usr/include/bits/wordsize.h /usr/include/bits/timesize.h
splash.o: /usr/include/stdc-predef.h /usr/include/sys/cdefs.h
splash.o: /usr/include/bits/long-double.h /usr/include/gnu/stubs.h
splash.o: /usr/include/stddef.h /usr/include/stdarg.h
splash.o: /usr/include/bits/types.h /usr/include/bits/typesizes.h
splash.o: /usr/include/bits/time64.h /usr/include/bits/types/__fpos_t.h
splash.o: /usr/include/bits/types/__mbstate_t.h
splash.o: /usr/include/bits/types/__fpos64_t.h
splash.o: /usr/include/bits/types/__FILE.h /usr/include/bits/types/FILE.h
splash.o: /usr/include/bits/types/struct_FILE.h
splash.o: /usr/include/bits/types/cookie_io_functions_t.h
splash.o: /usr/include/bits/stdio_lim.h /usr/include/bits/floatn.h
splash.o: /usr/include/bits/floatn-common.h /usr/include/stdlib.h
splash.o: /usr/include/bits/waitflags.h /usr/include/bits/waitstatus.h
splash.o: /usr/include/sys/types.h /usr/include/bits/types/clock_t.h
splash.o: /usr/include/bits/types/clockid_t.h
splash.o: /usr/include/bits/types/time_t.h /usr/include/bits/types/timer_t.h
splash.o: /usr/include/bits/stdint-intn.h /usr/include/endian.h
splash.o: /usr/include/bits/endian.h /usr/include/bits/endianness.h
splash.o: /usr/include/bits/byteswap.h /usr/include/bits/uintn-identity.h
splash.o: /usr/include/sys/select.h /usr/include/bits/select.h
splash.o: /usr/include/bits/types/sigset_t.h
splash.o: /usr/include/bits/types/__sigset_t.h
splash.o: /usr/include/bits/types/struct_timeval.h
splash.o: /usr/include/bits/types/struct_timespec.h
splash.o: /usr/include/bits/pthreadtypes.h
splash.o: /usr/include/bits/thread-shared-types.h
splash.o: /usr/include/bits/pthreadtypes-arch.h
splash.o: /usr/include/bits/atomic_wide_counter.h
splash.o: /usr/include/bits/struct_mutex.h /usr/include/bits/struct_rwlock.h
splash.o: /usr/include/alloca.h /usr/include/bits/stdlib-float.h
splash.o: /usr/include/string.h /usr/include/bits/types/locale_t.h
splash.o: /usr/include/bits/types/__locale_t.h /usr/include/strings.h
splash.o: ../lash-core/lash-types.h ../lash-core/lash.h
splash.o: ../lash-core/lash-diag.h ../lash-core/datastruct.h splash.h
splash.o: lexical.h grammar.h semantic.h
