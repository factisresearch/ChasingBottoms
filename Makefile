# This Makefile is currently only used to build the documentation.

########################################################################
# You may need to edit the following variables:

# Path to Haddock.
HADDOCK ?= haddock-0.6

# URL prefix leading to Haddock documentation for the hierarchical
# libraries.
GHC_DOC_URL ?= http://www.haskell.org/ghc/docs/latest/html/libraries

# Path prefix leading to Haddock interface files for the hierarchical
# libraries.
GHC_DOC_PATH ?= /usr/ed-pkg/wild/ghc-6.4/share/ghc-6.4/html/libraries

# Store documentation in this directory, after emptying it.
DOCDIR = docs

########################################################################

EXPOSED_SOURCES = ChasingBottoms.hs ChasingBottoms/Approx.hs	\
ChasingBottoms/ApproxShow.hs ChasingBottoms/IsBottom.hs		\
ChasingBottoms/Nat.hs ChasingBottoms/SemanticOrd.hs		\
ChasingBottoms/TimeOut.hs

CPPHS_PROCESSED_SUFFIX = .processed

# CPP is used for conditional compilation. Different code is selected
# dependent on the version of GHC used. That does not affect the
# comments, though, so we can just run cpphs without bothering about
# GHC versions.
%.hs$(CPPHS_PROCESSED_SUFFIX) : %.hs
	cpphs --noline $^ -O$@

$(DOCDIR) : $(addprefix Test/,$(EXPOSED_SOURCES:=$(CPPHS_PROCESSED_SUFFIX))) \
            Header
	-rm -rf $(DOCDIR)
	mkdir -p $(DOCDIR)
	$(HADDOCK) -h --title="Chasing Bottoms" --prologue=Header -odocs \
	  -i$(GHC_DOC_URL)/base,$(GHC_DOC_PATH)/base/base.haddock \
	  -i$(GHC_DOC_URL)/QuickCheck,$(GHC_DOC_PATH)/QuickCheck/QuickCheck.haddock \
	  $(filter Test/%,$^)
	-rm -f $(filter Test/%,$^)
