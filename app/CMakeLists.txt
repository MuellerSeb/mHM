# all fortran files
file(GLOB exe_sources *.*90)

# one excutable per file
set(exe_targets "")
foreach(src IN LISTS exe_sources)
  get_filename_component(stem "${src}" NAME_WE)
  set(tgt "${PROJECT_NAME}-${stem}-exe")
  add_executable("${tgt}" "${src}")
  set_target_properties("${tgt}" PROPERTIES OUTPUT_NAME "${PROJECT_NAME}-${stem}")
  target_link_libraries("${tgt}" PRIVATE mhm_lib)
  list(APPEND exe_targets "${tgt}")
endforeach()

# make all executables installable
install(
  TARGETS ${exe_targets}
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
)
