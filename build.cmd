call "D:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvars64.bat"
cmake -DLLVM_ENABLE_PROJECTS="clang" -DCMAKE_C_FLAGS="/utf-8" -DCMAKE_CXX_FLAGS="/utf-8" -DCMAKE_BUILD_TYPE=Release -B build -G Ninja llvm
cmake --build build -- -j 7
pause