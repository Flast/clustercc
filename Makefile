ERLC=erlc -W
ERL=erl -boot start_clean

RM=rm -rf
DIALYZER=dialyzer

MODS=ccc_manage \
	 node_manager \
	 worker \
	 manage \
	 clusterccd
ERLS=$(MODS:%=%.erl)
BEAMS=$(MODS:%=%.beam)

.PHONY: all compile static-check clean
all: compile

compile: $(BEAMS)

static-check: $(ERLS)
	@$(DIALYZER) $(ERLS)

clean:
	@echo "  RM    $(BEAMS)"
	@$(RM) $(BEAMS)

.SUFFIXES: .erl .beam

.erl.beam:
	@echo "  ERLC  $<"
	@$(ERLC) $<
