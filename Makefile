# This Makefile is currently only used to build the documentation.

########################################################################
# You may need to edit the following variables:

# URL prefix leading to Haddock documentation for the hierarchical
# libraries.
GHC_DOC_URL ?= http://www.haskell.org/ghc/docs/latest/html/libraries
# Path prefix leading to Haddock interface files for the hierarchical
# libraries.
GHC_DOC_PATH ?= /usr/local/share/ghc-6.4/html/libraries
# Store documentation in this directory, after removing it.
DOCDIR = docs

########################################################################

EXPOSED_SOURCES = ChasingBottoms.hs ChasingBottoms/Approx.hs	\
ChasingBottoms/ApproxShow.hs ChasingBottoms/IsBottom.hs		\
ChasingBottoms/Nat.hs ChasingBottoms/SemanticOrd.hs		\
ChasingBottoms/TimeOut.hs

# CPP is used for conditional compilation. Different code is selected
# dependent on the version of GHC used. That does not affect the
# comments, though, so we can just run cpphs without bothering about
# GHC versions.
%.hs.processed : %.hs
	cpphs --noline $^ -O$@

$(DOCDIR) : $(addprefix Test/,$(EXPOSED_SOURCES:=.processed)) Header
	-rm -rf $(DOCDIR)
	mkdir -p $(DOCDIR)
	haddock -h --title="Chasing Bottoms" --prologue=Header -odocs \
	  -i$(GHC_DOC_URL)/base,$(GHC_DOC_PATH)/base/base.haddock \
	  -i$(GHC_DOC_URL)/QuickCheck,$(GHC_DOC_PATH)/QuickCheck/QuickCheck.haddock \
	  $(filter Test/%,$^)
