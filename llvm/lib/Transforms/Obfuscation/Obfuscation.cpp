// For open-source license, please refer to
// [License](https://github.com/HikariObfuscator/Hikari/wiki/License).
// Adapted for LLVM 19 - Android NDK
//===----------------------------------------------------------------------===//
#include "llvm/Transforms/Obfuscation/Obfuscation.h"
#include "llvm/Transforms/Obfuscation/BogusControlFlow.h"
#include "llvm/Transforms/Obfuscation/ConstantEncryption.h"
#include "llvm/Transforms/Obfuscation/CryptoUtils.h"
#include "llvm/Transforms/Obfuscation/Flattening.h"
#include "llvm/Transforms/Obfuscation/FunctionWrapper.h"
#include "llvm/Transforms/Obfuscation/IndirectBranch.h"
#include "llvm/Transforms/Obfuscation/Split.h"
#include "llvm/Transforms/Obfuscation/StringEncryption.h"
#include "llvm/Transforms/Obfuscation/Substitution.h"
#include "llvm/Transforms/Obfuscation/Utils.h"
#include "llvm/IR/Module.h"
#include "llvm/InitializePasses.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Timer.h"
#include <cstdlib>

using namespace llvm;

// Obfuscator Options
static cl::opt<uint64_t> AesSeed("aesSeed", cl::init(0x1337),
                                 cl::desc("seed for the PRNG"));
static cl::opt<bool>
    EnableBogusControlFlow("enable-bcfobf", cl::init(false), cl::NotHidden,
                           cl::desc("Enable BogusControlFlow."));
static cl::opt<bool> EnableFlattening("enable-cffobf", cl::init(false),
                                      cl::NotHidden,
                                      cl::desc("Enable Flattening."));
static cl::opt<bool>
    EnableBasicBlockSplit("enable-splitobf", cl::init(false), cl::NotHidden,
                          cl::desc("Enable BasicBlockSpliting."));
static cl::opt<bool>
    EnableSubstitution("enable-subobf", cl::init(false), cl::NotHidden,
                       cl::desc("Enable Instruction Substitution."));
static cl::opt<bool> EnableAllObfuscation("enable-allobf", cl::init(false),
                                          cl::NotHidden,
                                          cl::desc("Enable All Obfuscation."));
static cl::opt<bool>
    EnableStringEncryption("enable-strcry", cl::init(false), cl::NotHidden,
                           cl::desc("Enable String Encryption."));
static cl::opt<bool>
    EnableConstantEncryption("enable-constenc", cl::init(false), cl::NotHidden,
                             cl::desc("Enable Constant Encryption."));
static cl::opt<bool>
    EnableIndirectBranching("enable-indibran", cl::init(false), cl::NotHidden,
                            cl::desc("Enable Indirect Branching."));
static cl::opt<bool>
    EnableFunctionWrapper("enable-funcwra", cl::init(false), cl::NotHidden,
                          cl::desc("Enable Function Wrapper."));

static void LoadEnv(void) {
  if (getenv("SPLITOBF")) {
    EnableBasicBlockSplit = true;
  }
  if (getenv("SUBOBF")) {
    EnableSubstitution = true;
  }
  if (getenv("ALLOBF")) {
    EnableAllObfuscation = true;
  }
  if (getenv("STRCRY")) {
    EnableStringEncryption = true;
  }
  if (getenv("INDIBRAN")) {
    EnableIndirectBranching = true;
  }
  if (getenv("FUNCWRA")) {
    EnableFunctionWrapper = true;
  }
  if (getenv("BCFOBF")) {
    EnableBogusControlFlow = true;
  }
  if (getenv("CFFOBF")) {
    EnableFlattening = true;
  }
  if (getenv("CONSTENC")) {
    EnableConstantEncryption = true;
  }
}

namespace llvm {
struct Obfuscation : public ModulePass {
  static char ID;
  Obfuscation() : ModulePass(ID) {
    initializeObfuscationPass(*PassRegistry::getPassRegistry());
  }
  StringRef getPassName() const override {
    return "HikariObfuscationScheduler";
  }
  bool runOnModule(Module &M) override {
    // Check if any obfuscation is enabled
    bool AnyEnabled = EnableBogusControlFlow || EnableFlattening ||
                      EnableBasicBlockSplit || EnableSubstitution ||
                      EnableAllObfuscation || EnableStringEncryption ||
                      EnableConstantEncryption || EnableIndirectBranching ||
                      EnableFunctionWrapper;
    if (!AnyEnabled)
      return false;

    TimerGroup *tg =
        new TimerGroup("Obfuscation Timer Group", "Obfuscation Timer Group");
    Timer *timer = new Timer("Obfuscation Timer", "Obfuscation Timer", *tg);
    timer->startTimer();

    errs() << "Running Hikari On " << M.getSourceFileName() << "\n";

    annotation2Metadata(M);

    // Now Encrypt Strings
    ModulePass *MP = createStringEncryptionPass(EnableAllObfuscation ||
                                                EnableStringEncryption);
    MP->runOnModule(M);
    delete MP;

    // Now perform Function-Level Obfuscation
    for (Function &F : M) {
      if (F.isDeclaration() || F.empty())
        continue;

      FunctionPass *P = nullptr;
      P = createSplitBasicBlockPass(EnableAllObfuscation ||
                                    EnableBasicBlockSplit);
      P->runOnFunction(F);
      delete P;

      P = createBogusControlFlowPass(EnableAllObfuscation ||
                                     EnableBogusControlFlow);
      P->runOnFunction(F);
      delete P;

      P = createFlatteningPass(EnableAllObfuscation || EnableFlattening);
      P->runOnFunction(F);
      delete P;

      P = createSubstitutionPass(EnableAllObfuscation || EnableSubstitution);
      P->runOnFunction(F);
      delete P;
    }

    MP = createConstantEncryptionPass(EnableConstantEncryption);
    MP->runOnModule(M);
    delete MP;

    errs() << "Doing Post-Run Cleanup\n";
    FunctionPass *P = createIndirectBranchPass(EnableAllObfuscation ||
                                               EnableIndirectBranching);
    for (Function &F : M) {
      if (!F.isDeclaration() && !F.empty())
        P->runOnFunction(F);
    }
    delete P;

    MP = createFunctionWrapperPass(EnableAllObfuscation ||
                                   EnableFunctionWrapper);
    MP->runOnModule(M);
    delete MP;

    // Cleanup Flags
    SmallVector<Function *, 8> toDelete;
    for (Function &F : M) {
      if (F.isDeclaration() && F.hasName() &&
          F.getName().starts_with("hikari_")) {
        for (User *U : F.users())
          if (Instruction *Inst = dyn_cast<Instruction>(U))
            Inst->eraseFromParent();
        toDelete.emplace_back(&F);
      }
    }
    for (Function *F : toDelete)
      F->eraseFromParent();

    timer->stopTimer();
    errs() << "Hikari Out\n";
    errs() << "Spend Time: "
           << format("%.7f", timer->getTotalTime().getWallTime()) << "s"
           << "\n";
    tg->clearAll();

    return true;
  }
};

ModulePass *createObfuscationLegacyPass() {
  LoadEnv();
  if (AesSeed != 0x1337) {
    cryptoutils->prng_seed(AesSeed);
  } else {
    cryptoutils->prng_seed();
  }
  errs() << "Initializing Hikari Core\n";
  return new Obfuscation();
}

PreservedAnalyses ObfuscationPass::run(Module &M, ModuleAnalysisManager &MAM) {
  if (createObfuscationLegacyPass()->runOnModule(M)) {
    return PreservedAnalyses::none();
  }
  return PreservedAnalyses::all();
}

} // namespace llvm

char Obfuscation::ID = 0;
INITIALIZE_PASS_BEGIN(Obfuscation, "obfus", "Enable Obfuscation", false, false)
INITIALIZE_PASS_DEPENDENCY(BogusControlFlow);
INITIALIZE_PASS_DEPENDENCY(Flattening);
INITIALIZE_PASS_DEPENDENCY(IndirectBranch);
INITIALIZE_PASS_DEPENDENCY(SplitBasicBlock);
INITIALIZE_PASS_DEPENDENCY(StringEncryption);
INITIALIZE_PASS_DEPENDENCY(Substitution);
INITIALIZE_PASS_END(Obfuscation, "obfus", "Enable Obfuscation", false, false)
