# Specify the gcc executable you will use the dragonegg plugin with here.
GCC?=gcc-4.5

# Specify the copy of LLVM you will build the plugin against by giving its
# llvm-config here.  To use an installed copy of LLVM, specify the installed
# llvm-config (just 'llvm-config' is enough if llvm-config is in your path).
# It is not necessary to install LLVM to build dragonegg against it.  Instead
# you can do an LLVM build and point LLVM_CONFIG to the copy of llvm-config
# that was created during the build.
LLVM_CONFIG?=llvm-config

# Location of the dragonegg source, useful if you want separate source and
# object directories.
SRC_DIR?=$(PWD)

ifndef VERBOSE
	QUIET:=@
endif

CFLAGS+=-Wall $(shell $(LLVM_CONFIG) --cflags) -fvisibility=hidden
CXXFLAGS+=-Wall $(shell $(LLVM_CONFIG) --cxxflags) -fvisibility=hidden

ifeq ($(shell uname),Darwin)
LOADABLE_MODULE_OPTIONS=-bundle -undefined dynamic_lookup
else
LOADABLE_MODULE_OPTIONS=-shared -Wl,--version-script=exports.map
endif

GCC_PLUGIN_DIR:=$(shell $(GCC) -print-file-name=plugin)
GCC_VERSION:=$(shell $(GCC) -dumpversion)
GCC_MAJOR=$(word 1, $(subst ., ,$(GCC_VERSION)))
GCC_MINOR=$(word 2, $(subst ., ,$(GCC_VERSION)))
TARGET_TRIPLE:=$(shell $(GCC) -dumpmachine)

# NOTE: replace with an informative string when doing a release.
REVISION:=$(shell svnversion -n $(SRC_DIR))

PLUGIN=dragonegg.so
PLUGIN_OBJECTS=llvm-cache.o llvm-convert.o llvm-backend.o llvm-debug.o \
	       llvm-types.o bits_and_bobs.o llvm-abi-default.o

TARGET_OBJECT=llvm-target.o
TARGET_SOURCE=$(SRC_DIR)/$(shell $(TARGET_UTIL) -p)/llvm-target.cpp

TARGET_UTIL_OBJECTS=target.o
TARGET_UTIL=./target

ALL_OBJECTS=$(PLUGIN_OBJECTS) $(TARGET_OBJECT) $(TARGET_UTIL_OBJECTS)

CPP_OPTIONS+=$(CPPFLAGS) $(shell $(LLVM_CONFIG) --cppflags) \
	     -MD -MP \
	     -DIN_GCC -DREVISION=\"$(REVISION)\" \
	     -DGCC_MAJOR=$(GCC_MAJOR) -DGCC_MINOR=$(GCC_MINOR) \
	     -DTARGET_NAME=\"$(TARGET_TRIPLE)\" \
	     -I$(SRC_DIR) -I$(GCC_PLUGIN_DIR)/include

LD_OPTIONS+=$(LDFLAGS) $(shell $(LLVM_CONFIG) --ldflags)

# NOTE: The following flags can only be used after TARGET_UTIL has been built.
TARGET_HEADERS+=-I$(SRC_DIR)/$(shell $(TARGET_UTIL) -p) \
		-I$(SRC_DIR)/$(shell $(TARGET_UTIL) -o)


default: $(PLUGIN)

$(TARGET_UTIL_OBJECTS): %.o : $(SRC_DIR)/utils/%.cpp
	@echo Compiling utils/$*.cpp
	$(QUIET)$(CXX) -c $(CPP_OPTIONS) $(CXXFLAGS) $<

$(TARGET_UTIL): $(TARGET_UTIL_OBJECTS)
	@echo Linking $@
	$(QUIET)$(CXX) -o $@ $^ $(LD_OPTIONS) \
	$(shell $(LLVM_CONFIG) --libs support)

%.o : $(SRC_DIR)/%.c $(TARGET_UTIL)
	@echo Compiling $*.c
	$(QUIET)$(CC) -c $(CPP_OPTIONS) $(TARGET_HEADERS) $(CFLAGS) $<

%.o : $(SRC_DIR)/%.cpp $(TARGET_UTIL)
	@echo Compiling $*.cpp
	$(QUIET)$(CXX) -c $(CPP_OPTIONS) $(TARGET_HEADERS) $(CXXFLAGS) $<

$(TARGET_OBJECT): $(TARGET_UTIL)
	@echo Compiling $(shell $(TARGET_UTIL) -p)/llvm-target.cpp
	$(QUIET)$(CXX) -o $@ -c $(CPP_OPTIONS) $(TARGET_HEADERS) $(CXXFLAGS) \
		$(TARGET_SOURCE)

$(PLUGIN): $(PLUGIN_OBJECTS) $(TARGET_OBJECT) $(TARGET_UTIL)
	@echo Linking $@
	$(QUIET)$(CXX) -o $@ $(LOADABLE_MODULE_OPTIONS) $(CXXFLAGS) \
	$(LD_OPTIONS) $(PLUGIN_OBJECTS) $(TARGET_OBJECT) \
	$(shell $(LLVM_CONFIG) --libs analysis core ipo scalaropts target \
	$(shell $(TARGET_UTIL) -p))

clean::
	$(QUIET)rm -f *.o *.d $(PLUGIN) $(TARGET_UTIL)


-include $(ALL_OBJECTS:.o=.d)

# The following target exists for the benefit of the dragonegg maintainers, and
# is not used in a normal build.
GENGTYPE_INPUT=$(SRC_DIR)/llvm-cache.c
GENGTYPE_OUTPUT=$(SRC_DIR)/gt-llvm-cache.h
$(GENGTYPE_OUTPUT):
	cd $(HOME)/GCC/objects/gcc && ./build/gengtype \
	  -P $(GENGTYPE_OUTPUT) $(GCC_PLUGIN_DIR) gtyp-input.list \
	    $(GENGTYPE_INPUT)
	sed -i "s/ggc_cache_tab .*\[\]/ggc_cache_tab gt_ggc_rc__gt_llvm_cache_h[]/" $(GENGTYPE_OUTPUT)
	sed -i "s/ggc_root_tab .*\[\]/ggc_root_tab gt_pch_rc__gt_llvm_cache_h[]/" $(GENGTYPE_OUTPUT)
