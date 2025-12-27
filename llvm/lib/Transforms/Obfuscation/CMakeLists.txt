add_llvm_component_library(LLVMObfuscation
  BogusControlFlow.cpp
  ConstantEncryption.cpp
  CryptoUtils.cpp
  Flattening.cpp
  FunctionWrapper.cpp
  IndirectBranch.cpp
  Obfuscation.cpp
  SplitBasicBlocks.cpp
  StringEncryption.cpp
  SubstituteImpl.cpp
  Substitution.cpp
  Utils.cpp

  ADDITIONAL_HEADER_DIRS
  ${LLVM_MAIN_INCLUDE_DIR}/llvm/Transforms/Obfuscation

  DEPENDS
  intrinsics_gen

  LINK_COMPONENTS
  Analysis
  Core
  Support
  TransformUtils
  Scalar
  Linker
  Passes
  )
