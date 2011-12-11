ERLC=erlc -W
ERL=erl -boot start_clean

RM=rm -rf

MODS=ccc_manage \
	 node_manager \
	 manage \
	 distccd \
	 clusterccd
BEAMS=$(MODS:%=%.beam)

.PHONY: all compile clean
all: compile

compile: $(BEAMS)

clean:
	@echo "  RM    $(BEAMS)"
	@$(RM) $(BEAMS)

.SUFFIXES: .erl .beam

.erl.beam:
	@echo "  ERLC  $<"
	@$(ERLC) $<
