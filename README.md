# while-mlir

cmake ..\llvm -G "Visual Studio 17 2022" -DLLVM_ENABLE_PROJECTS=mlir -DLLVM_BUILD_EXAMPLES=ON -DLLVM_TARGETS_TO_BUILD="host" -DCMAKE_BUILD_TYPE=Debug  -DCMAKE_OSX_ARCHITECTURES=x86_64 -DCMAKE_INSTALL_PREFIX=d:/bin  -Thost=x64 
cmake --build . --clean-first
 cmake --build . --target install