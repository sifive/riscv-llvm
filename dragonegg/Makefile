# Specify the gcc executable you will use the dragonegg plugin with here.
GCC?=gcc

# Specify the copy of LLVM you will build the plugin against by giving its
# llvm-config here.  To use an installed copy of LLVM, specify the installed
# llvm-config (just 'llvm-config' is enough if llvm-config is in your path).
# It is not necessary to install LLVM to build dragonegg against it.  Instead
# you can do an LLVM build and point LLVM_CONFIG to the copy of llvm-config
# that was created during the build.
LLVM_CONFIG?=llvm-config

# Location of this Makefile, useful if you want separate source and object
# directories.
TOP_DIR?=$(PWD)

# Normally the plugin will refuse to load into a different gcc to the one it was
# built against.  Uncomment this (or pass DISABLE_VERSION_CHECK=1 on the 'make'
# command line) to disable the check.
#DISABLE_VERSION_CHECK=1

# Where to find the lit.py script and modules, used for running tests.
LIT_DIR?=$(shell $(LLVM_CONFIG) --src-root)/utils/lit

INCLUDE_DIR=$(TOP_DIR)/include
SRC_DIR=$(TOP_DIR)/src

ifndef VERBOSE
QUIET:=@
endif

COMMON_FLAGS=-Wall -Wextra -fvisibility=hidden
CFLAGS+=$(COMMON_FLAGS) $(shell $(LLVM_CONFIG) --cflags)
CXXFLAGS+=$(COMMON_FLAGS) $(shell $(LLVM_CONFIG) --cxxflags)

ifeq ($(shell uname),Darwin)
LOADABLE_MODULE_OPTIONS=-bundle -undefined dynamic_lookup
else
LOADABLE_MODULE_OPTIONS=-shared -Wl,-O1 -Wl,--version-script=$(TOP_DIR)/exports.map
endif

GCC_PLUGIN_DIR:=$(shell $(GCC) -print-file-name=plugin)
GCC_VERSION:=$(shell $(GCC) -dumpversion).0
GCC_MAJOR=$(word 1, $(subst ., ,$(GCC_VERSION)))
GCC_MINOR=$(word 2, $(subst ., ,$(GCC_VERSION)))
GCC_MICRO=$(word 3, $(subst ., ,$(GCC_VERSION)))
TARGET_TRIPLE:=$(shell $(GCC) -dumpmachine)

LLVM_VERSION:=$(shell $(LLVM_CONFIG) --version)

PLUGIN=dragonegg.so
PLUGIN_OBJECTS=Backend.o Cache.o Constants.o Convert.o Debug.o DefaultABI.o \
	       Trees.o Types.o bits_and_bobs.o

TARGET_OBJECT=Target.o
TARGET_SOURCE=$(SRC_DIR)/$(shell $(TARGET_UTIL) -p)/Target.cpp

TARGET_UTIL_OBJECTS=TargetInfo.o
TARGET_UTIL=./TargetInfo

ALL_OBJECTS=$(PLUGIN_OBJECTS) $(TARGET_OBJECT) $(TARGET_UTIL_OBJECTS)

CPP_OPTIONS+=$(CPPFLAGS) $(shell $(LLVM_CONFIG) --cppflags) \
	     -MD -MP \
	     -DIN_GCC -DLLVM_VERSION=\"$(LLVM_VERSION)\" \
	     -DGCC_MAJOR=$(GCC_MAJOR) -DGCC_MINOR=$(GCC_MINOR) \
	     -DGCC_MICRO=$(GCC_MICRO) \
	     -I$(INCLUDE_DIR) -isystem$(GCC_PLUGIN_DIR)/include
ifdef DISABLE_VERSION_CHECK
CPP_OPTIONS+=-DDISABLE_VERSION_CHECK
endif

LD_OPTIONS+=$(shell $(LLVM_CONFIG) --ldflags) $(LDFLAGS)

# NOTE: The following flags can only be used after TARGET_UTIL has been built.
TARGET_HEADERS+=-DTARGET_NAME=\"$(shell $(TARGET_UTIL) -t)\" \
		-I$(INCLUDE_DIR)/$(shell $(TARGET_UTIL) -p) \
		-I$(INCLUDE_DIR)/$(shell $(TARGET_UTIL) -o)

ifdef VERBOSE
LIT_ARGS+=-v
else
LIT_ARGS+=-s -v
endif

LIT_SITE_CONFIG=test/dragonegg-lit.site.cfg
TEST_SRC_DIR=$(TOP_DIR)/test
export PYTHONPATH:=$(TEST_SRC_DIR):$(LIT_DIR)/lit:$(PYTHONPATH)

default: $(PLUGIN)

$(TARGET_UTIL_OBJECTS): %.o : $(TOP_DIR)/utils/%.cpp
	@echo Compiling utils/$*.cpp
	$(QUIET)$(CXX) -c -DTARGET_TRIPLE=\"$(TARGET_TRIPLE)\" \
	$(CPP_OPTIONS) $(CXXFLAGS) $<

$(TARGET_UTIL): $(TARGET_UTIL_OBJECTS)
	@echo Linking $@
	$(QUIET)$(CXX) -o $@ $^ $(shell $(LLVM_CONFIG) --libs support) \
	$(LD_OPTIONS)

%.o : $(SRC_DIR)/%.cpp $(TARGET_UTIL)
	@echo Compiling $*.cpp
	$(QUIET)$(CXX) -c $(TARGET_HEADERS) $(CPP_OPTIONS) $(CXXFLAGS) $<

$(TARGET_OBJECT): $(TARGET_UTIL)
	@echo Compiling $(shell $(TARGET_UTIL) -p)/Target.cpp
	$(QUIET)$(CXX) -o $@ -c $(TARGET_HEADERS) $(CPP_OPTIONS) $(CXXFLAGS) \
	$(TARGET_SOURCE)

$(PLUGIN): $(PLUGIN_OBJECTS) $(TARGET_OBJECT) $(TARGET_UTIL)
	@echo Linking $@
	$(QUIET)$(CXX) -o $@ $(LOADABLE_MODULE_OPTIONS) $(CXXFLAGS) \
	$(PLUGIN_OBJECTS) $(TARGET_OBJECT) $(shell $(LLVM_CONFIG) --libs \
	analysis core ipo scalaropts target $(shell $(TARGET_UTIL) -p)) \
	$(LD_OPTIONS)

$(LIT_SITE_CONFIG): $(TEST_SRC_DIR)/dragonegg-lit.site.cfg.in
	@echo "Making DragonEgg '$@' file..."
	$(QUIET)mkdir -p test
	$(QUIET)echo s=@DRAGONEGG_PLUGIN@=$(PWD)/$(PLUGIN)=g > lit.tmp
	$(QUIET)echo s=@GCC@=$(GCC)=g >> lit.tmp
	$(QUIET)echo s=@TEST_OUTPUT_DIR@=$(PWD)/test/Output=g >> lit.tmp
	$(QUIET)sed -f lit.tmp $< > $@
	$(QUIET)-rm -f lit.tmp

check:: $(PLUGIN) $(LIT_SITE_CONFIG)
	@echo "Running test suite"
	$(QUIET)$(LIT_DIR)/lit.py $(LIT_ARGS) --param site="$(LIT_SITE_CONFIG)" \
	--config-prefix=dragonegg-lit $(TEST_SRC_DIR)/compilator

clean::
	$(QUIET)rm -f *.o *.d $(PLUGIN) $(TARGET_UTIL) $(LIT_SITE_CONFIG)


-include $(ALL_OBJECTS:.o=.d)

# The following target exists for the benefit of the dragonegg maintainers, and
# is not used in a normal build.  You need to specify the path to the GCC build
# directory in GCC_BUILD_DIR.
GENGTYPE_INPUT=$(SRC_DIR)/Cache.cpp
GENGTYPE_OUTPUT=$(INCLUDE_DIR)/dragonegg/gt-cache-$(GCC_MAJOR).$(GCC_MINOR).h
gt-cache.h::
	$(QUIET)$(GCC_BUILD_DIR)/gcc/build/gengtype \
	-r $(GCC_BUILD_DIR)/gcc/gtype.state \
	-P $(GENGTYPE_OUTPUT) $(GENGTYPE_INPUT)
	$(QUIET)sed -i "s/__.*gt/__gt/" $(GENGTYPE_OUTPUT)
	$(QUIET)sed -i "s/_$(GCC_MAJOR)_$(GCC_MINOR)_/_/" $(GENGTYPE_OUTPUT)
