cmake_minimum_required(VERSION 3.13.4)
project(standalone-dialect LANGUAGES CXX C)
 
set(CMAKE_BUILD_WITH_INSTALL_NAME_DIR ON)
set(CMAKE_BUILD_TYPE Debug)

set(CMAKE_CXX_STANDARD 20 CACHE STRING "C++ standard to conform to")
SET(CMAKE_CXX_FLAGS "/EHsc")



#set(LLVM_DIR "D:/repositories/llvm-project/build/lib/cmake/llvm") 
set(LLVM_DIR "D:/bin/lib/cmake/llvm") 
find_package(LLVM REQUIRED CONFIG) 
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")  
#set(MLIR_DIR "D:/repositories/llvm-project/build/lib/cmake/mlir") 
set(MLIR_DIR "D:/bin/lib/cmake/mlir") 
find_package(MLIR REQUIRED CONFIG) 
message(STATUS "Using MLIRConfig.cmake in: ${MLIR_DIR}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
set(LLVM_RUNTIME_OUTPUT_INTDIR ${CMAKE_BINARY_DIR}/bin)
set(LLVM_LIBRARY_OUTPUT_INTDIR ${CMAKE_BINARY_DIR}/lib)
set(MLIR_BINARY_DIR ${CMAKE_BINARY_DIR})

# For a better template to copy, see examples/standalone
add_subdirectory(include/toy)

set(LLVM_LINK_COMPONENTS
  Support
  )

add_toy_chapter(toyc-ch2
  toyc.cpp
  parser/AST.cpp
  mlir/MLIRGen.cpp
  mlir/Dialect.cpp 

  DEPENDS
  ToyCh2OpsIncGen

  )
include_directories(include/)
include_directories(${CMAKE_CURRENT_BINARY_DIR}/include/)
target_link_libraries(toyc-ch2
  PRIVATE
    MLIRAnalysis
    MLIRIR
    MLIRParser
    MLIRSideEffectInterfaces 
    MLIRTransforms)
