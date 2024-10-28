######################################################################
##                                                                  ##
##  LASH package -- v0.92                                           ##
##  ============                                                    ##
##                                                                  ##
##     Makefile.                                                    ##
##                                                                  ##
##     02/02/01  :  Creation. (LL)                                  ##
##     02/24/01  :  Reorganization. (BB)                            ##
##     08/14/01  :  Added RVA package. (BB)                         ##
##     08/29/02  :  Reorganization. (BB)                            ##
##     09/06/02  :  Added Siflash compiler. (BB)                    ##
##                                                                  ##
##  Copyright (c) 1998-2002, Universite de Liege (ULg). All rights  ##
##  reserved. This package is provided for evaluation purposes and  ##
##  educational use only. No guarantee is expressed or implied by   ## 
##  the distribution of this software. The commercial use and the   ##
##  redistribution of this product or of any part of it are not     ##
##  permitted without our prior, express, and written consent.      ##
##                                                                  ##
######################################################################

all:	
	cd src/lash-core && $(MAKE)
	cd src/ndd && $(MAKE)
	cd src/qdd  && $(MAKE)
	cd src/rva && $(MAKE)
	cd src/presburger && $(MAKE)
	cd src/splash && $(MAKE)
	cd src/siflash && $(MAKE)

clean:
	cd src/lash-core && $(MAKE) clean
	cd src/ndd && $(MAKE) clean
	cd src/qdd  && $(MAKE) clean
	cd src/rva && $(MAKE) clean
	cd src/presburger && $(MAKE) clean
	cd src/splash && $(MAKE) clean
	cd src/siflash && $(MAKE) clean

depend:
	cd src/lash-core && $(MAKE) depend
	cd src/ndd && $(MAKE) depend
	cd src/qdd  && $(MAKE) depend
	cd src/rva && $(MAKE) depend
	cd src/presburger && $(MAKE) depend
	cd src/splash && $(MAKE) depend
	cd src/siflash && $(MAKE) depend
