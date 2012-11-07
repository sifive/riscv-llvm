# Specify the gcc executable you will use the dragonegg plugin with either here
# or on the command line, for example like this (replace gcc-4.7 with the gcc
# you want to load the plugin into):
#   GCC=gcc-4.7 make ...
# If you don't specify anything, then by default the plugin targets the compiler
# used to build it.
GCC?=$(CXX)

# Specify the copy of LLVM you will build the plugin against by giving its
# llvm-config here or on the command line.  To use an installed copy of LLVM,
# specify the installed llvm-config (just 'llvm-config' is enough if it is in
# your path).  It is not necessary to install LLVM to build dragonegg against
# it.  Instead you can do an LLVM build and point LLVM_CONFIG to the copy of
# llvm-config that was created during the build.
LLVM_CONFIG?=llvm-config

# Location of this Makefile, useful if you want separate source and object
# directories.
TOP_DIR?=$(CURDIR)

# Normally the plugin will refuse to load into a different gcc to the one it was
# built against.  Uncomment this (or pass DISABLE_VERSION_CHECK=1 on the 'make'
# command line) to disable the check.
#DISABLE_VERSION_CHECK=1

# Where to find the lit.py script and modules, used for running tests.
LIT_DIR?=$(shell $(LLVM_CONFIG) --src-root)/utils/lit
# Where to find LLVM utils, used for running tests.
LLVM_TOOLS_DIR?=$(shell $(LLVM_CONFIG) --obj-root)/bin:$(shell $(LLVM_CONFIG) \
	--obj-root)/$(shell $(LLVM_CONFIG) --build-mode)/bin/

INCLUDE_DIR=$(TOP_DIR)/include
SRC_DIR=$(TOP_DIR)/src

ifndef VERBOSE
QUIET:=@
endif

COMMON_FLAGS=-Wall -Wextra
ifdef ENABLE_LLVM_PLUGINS
COMMON_FLAGS+=-DENABLE_LLVM_PLUGINS
else
COMMON_FLAGS+=-fvisibility=hidden
endif
CFLAGS+=$(COMMON_FLAGS) $(shell $(LLVM_CONFIG) --cflags)
CXXFLAGS+=$(COMMON_FLAGS) $(shell $(LLVM_CONFIG) --cxxflags)

ifeq ($(shell uname),Darwin)
LOADABLE_MODULE_OPTIONS=-bundle -undefined dynamic_lookup
else
LOADABLE_MODULE_OPTIONS=-shared -Wl,-O1
ifndef ENABLE_LLVM_PLUGINS
LOADABLE_MODULE_OPTIONS+=-Wl,--version-script=$(TOP_DIR)/exports.map
endif
endif

GCC_PLUGIN_DIR=$(shell $(GCC) -print-file-name=plugin)
GCC_VERSION=$(shell $(GCC) -dumpversion).0
GCC_MAJOR=$(word 1, $(subst ., ,$(GCC_VERSION)))
GCC_MINOR=$(word 2, $(subst ., ,$(GCC_VERSION)))
GCC_MICRO=$(word 3, $(subst ., ,$(GCC_VERSION)))
GCC_LANGUAGES=$(shell $(GCC) -v 2>&1 | grep '^Configured with:' | sed 's/^.*--enable-languages=\([^ ]*\).*/\1/')
TARGET_TRIPLE=$(shell $(GCC) -dumpmachine)

LLVM_VERSION=$(shell $(LLVM_CONFIG) --version)

PLUGIN=dragonegg.so
PLUGIN_OBJECTS=Aliasing.o Backend.o Cache.o ConstantConversion.o Convert.o \
	       Debug.o DefaultABI.o Trees.o TypeConversion.o bits_and_bobs.o

TARGET_OBJECT=Target.o
TARGET_SOURCE=$(SRC_DIR)/$(shell $(TARGET_UTIL) -p)/Target.cpp

TARGET_UTIL_OBJECTS=TargetInfo.o
TARGET_UTIL=./TargetInfo

ALL_OBJECTS=$(PLUGIN_OBJECTS) $(TARGET_OBJECT) $(TARGET_UTIL_OBJECTS)

CPP_OPTIONS+=$(CPPFLAGS) $(shell $(LLVM_CONFIG) --cppflags) \
	     -fno-rtti \
	     -MD -MP \
	     -DIN_GCC -DLLVM_VERSION=\"$(LLVM_VERSION)\" \
	     -DGCC_MAJOR=$(GCC_MAJOR) -DGCC_MINOR=$(GCC_MINOR) \
	     -DGCC_MICRO=$(GCC_MICRO) \
	     -I$(INCLUDE_DIR) -isystem$(GCC_PLUGIN_DIR)/include
ifdef DISABLE_VERSION_CHECK
CPP_OPTIONS+=-DDISABLE_VERSION_CHECK
endif

LD_OPTIONS+=$(shell $(LLVM_CONFIG) --ldflags) $(LDFLAGS)

LLVM_COMPONENTS=ipo scalaropts target
ifdef ENABLE_LLVM_PLUGINS
# The same components as the "opt" tool.
LLVM_COMPONENTS+=bitreader bitwriter asmparser instrumentation vectorize
endif

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

default: gcc-sane llvm-config-sane $(PLUGIN)

.PHONY: gcc-sane
gcc-sane:
	$(QUIET)$(GCC) --version > /dev/null

.PHONY: llvm-config-sane
llvm-config-sane:
	$(QUIET)$(LLVM_CONFIG) --version > /dev/null

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
	$(PLUGIN_OBJECTS) $(TARGET_OBJECT) \
	$(shell $(LLVM_CONFIG) --libs $(LLVM_COMPONENTS) \
	$(shell $(TARGET_UTIL) -p)) \
	$(LD_OPTIONS)

$(LIT_SITE_CONFIG): $(TEST_SRC_DIR)/dragonegg-lit.site.cfg.in
	@echo "Making DragonEgg '$@' file..."
	$(QUIET)mkdir -p test
	$(QUIET)echo s=@DRAGONEGG_PLUGIN@=$(CURDIR)/$(PLUGIN)=g > lit.tmp
	$(QUIET)echo s=@GCC@=$(GCC)=g >> lit.tmp
	$(QUIET)echo s=@GCC_LANGUAGES@=$(GCC_LANGUAGES)=g >> lit.tmp
	$(QUIET)echo s=@LLVM_TOOLS_DIR@=$(LLVM_TOOLS_DIR)=g >> lit.tmp
	$(QUIET)echo s=@TARGET_TRIPLE@=$(TARGET_TRIPLE)-gcc-$(GCC_MAJOR).$(GCC_MINOR)=g >> lit.tmp
	$(QUIET)echo s=@TEST_OUTPUT_DIR@=$(CURDIR)/test/Output=g >> lit.tmp
	$(QUIET)sed -f lit.tmp $< > $@
	$(QUIET)-rm -f lit.tmp

.PHONY: check-compilator
check-compilator: $(PLUGIN) $(LIT_SITE_CONFIG)
	@echo "Running test suite 'compilator'"
	$(QUIET)$(LIT_DIR)/lit.py $(LIT_ARGS) --param site="$(LIT_SITE_CONFIG)" \
	--config-prefix=compilator-lit $(TEST_SRC_DIR)/compilator

.PHONY: check-validator
check-validator: $(PLUGIN) $(LIT_SITE_CONFIG)
	@echo "Running test suite 'validator'"
	$(QUIET)$(LIT_DIR)/lit.py $(LIT_ARGS) --param site="$(LIT_SITE_CONFIG)" \
	--config-prefix=validator-lit $(TEST_SRC_DIR)/validator

.PHONY: check
check: check-validator check-compilator

.PHONY: clean
clean:
	$(QUIET)rm -f *.o *.d $(PLUGIN) $(TARGET_UTIL) $(LIT_SITE_CONFIG)


-include $(ALL_OBJECTS:.o=.d)

# The following target exists for the benefit of the dragonegg maintainers, and
# is not used in a normal build.  You need to specify the path to the GCC build
# directory in GCC_BUILD_DIR.
GENGTYPE_INPUT=$(SRC_DIR)/Cache.cpp
GENGTYPE_OUTPUT=$(INCLUDE_DIR)/dragonegg/gt-cache-$(GCC_MAJOR).$(GCC_MINOR).h
.PHONY: gt-cache.h
gt-cache.h:
	$(QUIET)$(GCC_BUILD_DIR)/gcc/build/gengtype \
	-r $(GCC_BUILD_DIR)/gcc/gtype.state \
	-P $(GENGTYPE_OUTPUT) $(GENGTYPE_INPUT)
	$(QUIET)sed -i "s/__.*gt/__gt/" $(GENGTYPE_OUTPUT)
	$(QUIET)sed -i "s/_$(GCC_MAJOR)_$(GCC_MINOR)_/_/" $(GENGTYPE_OUTPUT)
