ERLC=erlc -W
ERL=erl -boot start_clean

RM=rm -rf

MODS=common_io \
	 ccc_manage \
	 node_manager
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
