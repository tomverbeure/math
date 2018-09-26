

all: math
	./math

display_string := -Ddisplay_string

source_dir := src
header_dir := src
build_dir  := build
bin_dir    := ./

sources := $(wildcard $(source_dir)/*.c)
objects := $(sources:$(source_dir)/%.c=$(build_dir)/%.o)

CPPFLAGS := $(display_string) -I$(header_dir) -MMD -MP -O0 -g
LDLIBS   := -lm

$(bin_dir)/math: $(objects) | $(bin_dir)
	$(LINK.o) $^ $(LDLIBS) -o $@

$(build_dir)/%.o: $(source_dir)/%.c | $(build_dir)
	$(COMPILE.c) $(OUTPUT_OPTION) $<

$(build_dir) $(bin_dir): ; mkdir $@

clean: ; $(RM) -fr  math $(objects) $(deps) $(build_dir)

-include $(deps)
