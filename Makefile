# This Makefile is currently only used to build the documentation.

EXPOSED_SOURCES = ChasingBottoms.hs ChasingBottoms/Approx.hs	\
ChasingBottoms/ApproxShow.hs ChasingBottoms/IsBottom.hs		\
ChasingBottoms/Nat.hs ChasingBottoms/SemanticOrd.hs		\
ChasingBottoms/TimeOut.hs

docs : $(addprefix Test/,$(EXPOSED_SOURCES))
	-rm -rf docs
	mkdir -p docs
	haddock -h --title="Chasing Bottoms" --prologue=Header -odocs \
	  $^
