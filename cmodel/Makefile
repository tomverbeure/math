

all: math
	./math

display_string := -Ddisplay_string

source_dir := src
header_dir := src
build_dir  := build
bin_dir    := ./

sources := $(wildcard $(source_dir)/*.cpp)
objects := $(sources:$(source_dir)/%.cpp=$(build_dir)/%.o)

CC       := g++
CPPFLAGS := $(display_string) -I$(header_dir) -MMD -MP -O0 -g -std=c++11
LDLIBS   := -lm

$(bin_dir)/math: $(objects) | $(bin_dir)
	$(LINK.o) $^ $(LDLIBS) -o $@

$(build_dir)/%.o: $(source_dir)/%.cpp | $(build_dir)
	$(COMPILE.c) $(OUTPUT_OPTION) $<

$(build_dir) $(bin_dir): ; mkdir $@

clean: ; $(RM) -r math $(objects) $(deps) $(build_dir)

-include $(deps)

build/main.o: Makefile src/fpxx.h src/misc.h
