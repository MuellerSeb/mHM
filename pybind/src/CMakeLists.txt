cmake_minimum_required(VERSION 3.17.2...3.24)
enable_language(C)
find_package(Python REQUIRED COMPONENTS Interpreter Development.Module NumPy)

if(${SKBUILD_STATE} MATCHES "editable")
  set(mhm_install_dir "${CMAKE_CURRENT_SOURCE_DIR}/mhm")
else()
  set(mhm_install_dir "mhm")
endif()

# Grab the variables from a local Python installation F2PY headers
execute_process(
  COMMAND "${PYTHON_EXECUTABLE}"
  -c "import numpy.f2py as f, pathlib as p; print(p.Path(f.get_include()).as_posix())"
  OUTPUT_VARIABLE F2PY_INCLUDE_DIR
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

set(f2py_module_name "wrapper")
set(f2py_module_name_c "${f2py_module_name}module.c")
set(f2py_module_name_f "${f2py_module_name}-f2pywrappers2.f90")
set(f2py_fortranobject "${F2PY_INCLUDE_DIR}/fortranobject.c")
set(fortran_src_file "${CMAKE_CURRENT_SOURCE_DIR}/${f2py_module_name}.f90")
add_custom_target(genpyf ALL DEPENDS ${fortran_src_file})

# only use f2py to create c-wrapper for fortran module
add_custom_command(
  OUTPUT ${f2py_module_name_c} ${f2py_module_name_f}
  COMMAND ${Python_EXECUTABLE} -m "numpy.f2py"
    -m ${f2py_module_name}
    ${fortran_src_file}
    --lower
    --f2cmap ${CMAKE_CURRENT_SOURCE_DIR}/.f2py_f2cmap
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS ${fortran_src_file}
)

# add python module for wrapper
python_add_library(${f2py_module_name}
  MODULE WITH_SOABI
  ${f2py_module_name_c}
  ${f2py_fortranobject}
  ${f2py_module_name_f}
  ${fortran_src_file}
)

# link mhm to wrapper
target_include_directories(${f2py_module_name} PUBLIC ${F2PY_INCLUDE_DIR})
target_link_libraries(${f2py_module_name} PUBLIC mhm_lib Python::NumPy)
add_dependencies(${f2py_module_name} genpyf)

# install
install(TARGETS ${f2py_module_name} DESTINATION "${mhm_install_dir}")
