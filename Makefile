# This Makefile is currently only used to run a test suite
# automatically using darcs.

########################################################################
# You may need to edit the following variables:

# Path to GHC 6.8, used to run the tests.
GHC_68 ?= ghc

# GHC packages necessary for building and testing the library.
PACKAGES = base-3.0.1.0 containers-0.1.0.1 random-1.0.0.0 mtl-1.1.0.0	\
QuickCheck-1.1.0.0 array-0.1.0.0

########################################################################

# Runs all tests using GHC 6.8.
compile = $(1) -ignore-dot-ghci -no-recomp -hide-all-packages \
	  $(foreach pkg,$(PACKAGES),-package $(pkg)) --make $(2)
testWithCompiler = $(call compile,$(1),Test.ChasingBottoms) && $(call	\
compile,$(1),Test.ChasingBottoms.Tests -main-is				\
Test.ChasingBottoms.Tests.main -o tests) && ./tests
.PHONY : test
test:
	$(call testWithCompiler,$(GHC_68))
