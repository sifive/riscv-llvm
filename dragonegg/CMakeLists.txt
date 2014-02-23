message(STATUS "Configuring DragonEgg...")

add_definitions(-DIN_GCC)

set(DRAGONEGG_TARGET_GCC ${CMAKE_C_COMPILER} CACHE PATH "plugin --print-file-name=plugin")
message(STATUS "Target GCC is ${DRAGONEGG_TARGET_GCC}")

function(get_gcc_prop prop OUTVAR)
  execute_process(
    COMMAND ${DRAGONEGG_TARGET_GCC} "-${prop}"
    OUTPUT_STRIP_TRAILING_WHITESPACE
    OUTPUT_VARIABLE gcc_prop
    )
  set(${OUTVAR} ${gcc_prop} PARENT_SCOPE)
endfunction()

function(get_gcc_version)
  get_gcc_prop(dumpversion VER)
  set(VER__ "${VER}.0.0")

  string(REGEX MATCHALL "[0-9]+" VERS ${VER__})

  list(GET VERS 0 V0)
  set(GCC_MAJOR ${V0} PARENT_SCOPE)
  list(GET VERS 1 V1)
  set(GCC_MINOR ${V1} PARENT_SCOPE)
  list(GET VERS 2 V2)
  set(GCC_MICRO ${V2} PARENT_SCOPE)

  message(STATUS "The version is ${VER} (major=${V0}, minor=${V1}, micro=${V2})")
endfunction()

function(add_backend_header FLAG FILE)
  get_target_property(TARGET_UTIL TargetInfo LOCATION)
  set(DST ${CMAKE_BINARY_DIR}/include/dragonegg/${FILE})
  file(GLOB inc include/*/dragonegg/${FILE})
  add_custom_command(
    OUTPUT ${DST}
    COMMAND mkdir -p ${CMAKE_BINARY_DIR}/include/dragonegg
    COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/include/`${TARGET_UTIL} ${FLAG}`/dragonegg/${FILE} ${DST}
    DEPENDS
    ${inc}
    ${TARGET_UTIL}
    COMMENT "Updating include/dragonegg/${FILE}"
    )
  set_source_files_properties(${DST} PROPERTIES GENERATED 1)
  add_file_dependencies(src/Backend.cpp ${DST})
endfunction()

function(add_target_source FILE)
  get_target_property(TARGET_UTIL TargetInfo LOCATION)
  set(DST ${CMAKE_CURRENT_BINARY_DIR}/${FILE})
  file(GLOB src src/*/${FILE})
  add_custom_command(
    OUTPUT ${DST}
    COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/src/`${TARGET_UTIL} -p`/${FILE} ${DST}
    DEPENDS
    ${src}
    ${TARGET_UTIL}
    COMMENT "Updating ${FILE}"
    )
  set_source_files_properties(${DST} PROPERTIES GENERATED 1)
endfunction()

get_gcc_prop("-print-file-name=plugin" GCC_PLUGIN_DIR)
message(STATUS "GCC's plugin directory is ${GCC_PLUGIN_DIR}")
include_directories(SYSTEM "${GCC_PLUGIN_DIR}/include")

get_gcc_prop(dumpmachine TARGET_TRIPLE)
message(STATUS "Target triple is ${TARGET_TRIPLE}")
add_definitions(-DTARGET_TRIPLE="${TARGET_TRIPLE}")

get_gcc_version()
add_definitions(
  -DGCC_MAJOR=${GCC_MAJOR}
  -DGCC_MINOR=${GCC_MINOR}
  -DGCC_MICRO=${GCC_MICRO}
  )

# TODO: Put appropriate version string.
add_definitions(-DLLVM_VERSION="")

include_directories(include)

add_subdirectory(utils)

add_backend_header(-o OS.h)

# add_backend_header(-p Target.h)
string(REGEX MATCH "^(i[3-6]86|x86_64)-" TARGET_X86 ${TARGET_TRIPLE})
string(REGEX MATCH "^(arm|thumb)" TARGET_ARM ${TARGET_TRIPLE})
if (TARGET_ARM)
  set(TARGET_ARCH "ARM")
elseif (TARGET_X86)
  set(TARGET_ARCH "X86")
else ()
  message(FATAL_ERROR "Target architecture is unknown")
endif ()

message(STATUS "Target architecture is ${TARGET_ARCH}")
string(TOLOWER ${TARGET_ARCH} TARGET_arch_dir)
include_directories("include/${TARGET_arch_dir}")

file(GLOB SRC src/*.cpp)
set(LLVM_LINK_COMPONENTS ipo scalaropts X86)

add_llvm_loadable_module(
  dragonegg
  src/Aliasing.cpp
  src/Backend.cpp
  src/bits_and_bobs.cpp
  src/Cache.cpp
  src/ConstantConversion.cpp
  src/Convert.cpp
  src/Debug.cpp
  src/DefaultABI.cpp
  src/Trees.cpp
  src/TypeConversion.cpp
  src/${TARGET_arch_dir}/Target.cpp

  DEPENDS
  intrinsics_gen
  )

add_subdirectory(test)
