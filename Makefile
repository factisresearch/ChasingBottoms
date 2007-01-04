# This Makefile is currently only used to build the documentation and
# to run a test suite automatically using darcs.

########################################################################
# You may need to edit the following variables:

# Path to Haddock.
HADDOCK ?= haddock

# URL prefix leading to Haddock documentation for the hierarchical
# libraries.
GHC_DOC_URL ?= http://www.haskell.org/ghc/docs/latest/html/libraries

# Path prefix leading to Haddock interface files for the hierarchical
# libraries. These files should be compiled using a Haddock program
# which is interface compatible with the one listed above (same
# version).
GHC_DOC_PATH ?= /chalmers/sw/unsup/ghc-6.6/share/ghc-6.6/html/libraries

# Documentation is stored in this directory. Note that the directory
# is emptied first.
DOCDIR = docs

# Path to GHC 6.6, used to run the tests.
GHC_66 ?= _ghc_6.6

########################################################################

EXPOSED_SOURCES = ChasingBottoms.hs ChasingBottoms/Approx.hs		\
ChasingBottoms/ApproxShow.hs ChasingBottoms/ContinuousFunctions.hs	\
ChasingBottoms/IsBottom.hs ChasingBottoms/Nat.hs			\
ChasingBottoms/SemanticOrd.hs ChasingBottoms/TimeOut.hs

HIDDEN_SOURCES = ChasingBottoms/IsType.hs ChasingBottoms/Tests.hs	\
ChasingBottoms/Approx/Tests.hs ChasingBottoms/ApproxShow/Tests.hs	\
ChasingBottoms/ContinuousFunctions/Tests.hs				\
ChasingBottoms/IsBottom/Tests.hs ChasingBottoms/IsType/Tests.hs		\
ChasingBottoms/Nat/Tests.hs ChasingBottoms/SemanticOrd/Tests.hs		\
ChasingBottoms/TimeOut/Tests.hs ChasingBottoms/TestUtilities.hs		\
ChasingBottoms/TestUtilities/Generators.hs				\
ChasingBottoms/TestLibraryWhenCompiling.hs

# GHC packages whose documentation we want to hyperlink to.
PACKAGES = base mtl QuickCheck

FILES_TO_BE_EXCLUDED = .boring

$(DOCDIR) : $(DOCDIR)/index.html
$(DOCDIR)/index.html : $(addprefix Test/,$(EXPOSED_SOURCES)) Header
	-rm -rf $(DOCDIR)
	mkdir -p $(DOCDIR)
	$(HADDOCK) -h --title="Chasing Bottoms" --prologue=Header -o$(DOCDIR) \
	  $(foreach pkg,$(PACKAGES),\
             -i$(GHC_DOC_URL)/$(pkg),$(GHC_DOC_PATH)/$(pkg)/$(pkg).haddock) \
	  $(filter Test/%,$^)

# Target used by darcs dist.
# After building the documentation we remove the generated files. We
# do not want them included in the tar ball generated by darcs.
# We also remove some other files.
dist: $(DOCDIR)
	-rm $(FILES_TO_BE_EXCLUDED)

# Runs all tests using GHC 6.6.
compile = $(1) -ignore-dot-ghci -no-recomp --make $(2)
testWithCompiler = $(call compile,$(1),Test.ChasingBottoms) && $(call	\
compile,$(1),Test.ChasingBottoms.Tests -main-is				\
Test.ChasingBottoms.Tests.main -o tests) && ./tests
.PHONY : test
test:
	$(call testWithCompiler,$(GHC_66))
