// For open-source license, please refer to
// [License](https://github.com/HikariObfuscator/Hikari/wiki/License).
// Modified to support linkonce_odr/weak functions
//===----------------------------------------------------------------------===//
#include "llvm/Transforms/Obfuscation/IndirectBranch.h"
#include "llvm/Transforms/Obfuscation/CryptoUtils.h"
#include "llvm/Transforms/Obfuscation/Utils.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/NoFolder.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/LowerSwitch.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include <unordered_map>
#include <unordered_set>

using namespace llvm;

static cl::opt<bool>
    UseStack("indibran-use-stack", cl::init(true), cl::NotHidden,
             cl::desc("[IndirectBranch]Stack-based indirect jumps"));
static bool UseStackTemp = true;

static cl::opt<bool>
    EncryptJumpTarget("indibran-enc-jump-target", cl::init(false),
                      cl::NotHidden,
                      cl::desc("[IndirectBranch]Encrypt jump target"));
static bool EncryptJumpTargetTemp = false;

namespace llvm {

// Check if function has COMDAT linkage that may be discarded
static bool mayBeDiscarded(Function *F) {
  GlobalValue::LinkageTypes L = F->getLinkage();
  return L == GlobalValue::LinkOnceAnyLinkage ||
         L == GlobalValue::LinkOnceODRLinkage ||
         L == GlobalValue::WeakAnyLinkage ||
         L == GlobalValue::WeakODRLinkage ||
         L == GlobalValue::AvailableExternallyLinkage;
}

struct IndirectBranch : public FunctionPass {
  static char ID;
  bool flag;
  bool initialized;
  // Per-function data structures to handle COMDAT properly
  std::unordered_map<Function *, std::unordered_map<BasicBlock *, unsigned long long>> funcIndexMap;
  std::unordered_map<Function *, GlobalVariable *> funcTableMap;
  std::unordered_map<Function *, ConstantInt *> encmap;
  std::unordered_set<Function *> to_obf_funcs;

  IndirectBranch() : FunctionPass(ID) {
    this->flag = true;
    this->initialized = false;
  }
  IndirectBranch(bool flag) : FunctionPass(ID) {
    this->flag = flag;
    this->initialized = false;
  }
  StringRef getPassName() const override { return "IndirectBranch"; }

  bool initialize(Module &M) {
    PassBuilder PB;
    FunctionAnalysisManager FAM;
    FunctionPassManager FPM;
    PB.registerFunctionAnalyses(FAM);
    FPM.addPass(LowerSwitchPass());

    for (Function &F : M) {
      if (!toObfuscate(flag, &F, "indibr"))
        continue;
      to_obf_funcs.insert(&F);

      if (!toObfuscateBoolOption(&F, "indibran_use_stack", &UseStackTemp))
        UseStackTemp = UseStack;

      FPM.run(F, FAM);

      if (!toObfuscateBoolOption(&F, "indibran_enc_jump_target",
                                 &EncryptJumpTargetTemp))
        EncryptJumpTargetTemp = EncryptJumpTarget;

      if (EncryptJumpTargetTemp)
        encmap[&F] = ConstantInt::get(
            Type::getInt32Ty(M.getContext()),
            cryptoutils->get_range(UINT8_MAX, UINT16_MAX * 2) * 4);

      // Create per-function jump table
      SmallVector<Constant *, 32> BBs;
      std::unordered_map<BasicBlock *, unsigned long long> &indexmap = funcIndexMap[&F];
      unsigned long long i = 0;

      for (BasicBlock &BB : F) {
        if (!BB.isEntryBlock()) {
          indexmap[&BB] = i++;
          BBs.emplace_back(
              EncryptJumpTargetTemp
                  ? ConstantExpr::getGetElementPtr(
                        Type::getInt8Ty(M.getContext()),
                        ConstantExpr::getBitCast(
                            BlockAddress::get(&BB),
                            Type::getInt8Ty(M.getContext())->getPointerTo()),
                        encmap[&F])
                  : BlockAddress::get(&BB));
        }
      }

      if (BBs.empty())
        continue;

      ArrayType *AT = ArrayType::get(
          Type::getInt8Ty(M.getContext())->getPointerTo(), BBs.size());
      Constant *BlockAddressArray =
          ConstantArray::get(AT, ArrayRef<Constant *>(BBs));

      // Create table name based on function
      std::string TableName = "IndirectBranchingTable_" + F.getName().str();
      
      GlobalVariable *Table = new GlobalVariable(
          M, AT, false, GlobalValue::LinkageTypes::PrivateLinkage,
          BlockAddressArray, TableName);

      // If function may be discarded (linkonce_odr, weak, etc.),
      // put the table in the same COMDAT group
      if (mayBeDiscarded(&F)) {
        if (F.hasComdat()) {
          Table->setComdat(F.getComdat());
        } else {
          // Create a new COMDAT group for this function
          Comdat *C = M.getOrInsertComdat(F.getName());
          C->setSelectionKind(Comdat::Any);
          // Note: We can't set COMDAT on the function if it doesn't have one
          // So we use the same linkage as the function for the table
          Table->setLinkage(F.getLinkage());
          if (Table->getLinkage() == GlobalValue::LinkOnceODRLinkage ||
              Table->getLinkage() == GlobalValue::WeakODRLinkage) {
            Table->setComdat(C);
          }
        }
        // Also set the section to match function's section behavior
        Table->setUnnamedAddr(GlobalValue::UnnamedAddr::Global);
      }

      appendToCompilerUsed(M, {Table});
      funcTableMap[&F] = Table;
    }

    this->initialized = true;
    return true;
  }

  bool runOnFunction(Function &Func) override {
    Module *M = Func.getParent();
    if (!this->initialized)
      initialize(*M);
    if (to_obf_funcs.find(&Func) == to_obf_funcs.end())
      return false;

    errs() << "Running IndirectBranch On " << Func.getName() << "\n";

    // Get per-function data
    auto tableIt = funcTableMap.find(&Func);
    if (tableIt == funcTableMap.end())
      return false;
    GlobalVariable *FuncTable = tableIt->second;
    std::unordered_map<BasicBlock *, unsigned long long> &indexmap = funcIndexMap[&Func];

    SmallVector<BranchInst *, 32> BIs;
    for (Instruction &Inst : instructions(Func))
      if (BranchInst *BI = dyn_cast<BranchInst>(&Inst))
        BIs.emplace_back(BI);

    Type *Int8Ty = Type::getInt8Ty(M->getContext());
    Type *Int32Ty = Type::getInt32Ty(M->getContext());
    Type *Int8PtrTy = Type::getInt8Ty(M->getContext())->getPointerTo();

    Value *zero = ConstantInt::get(Int32Ty, 0);

    IRBuilder<NoFolder> *IRBEntry =
        new IRBuilder<NoFolder>(&Func.getEntryBlock().front());

    for (BranchInst *BI : BIs) {
      if (UseStackTemp &&
          IRBEntry->GetInsertPoint() !=
              (BasicBlock::iterator)Func.getEntryBlock().front())
        IRBEntry->SetInsertPoint(Func.getEntryBlock().getTerminator());

      IRBuilder<NoFolder> *IRBBI = new IRBuilder<NoFolder>(BI);
      SmallVector<BasicBlock *, 2> BBs;

      if (BI->isConditional() && !BI->getSuccessor(1)->isEntryBlock())
        BBs.emplace_back(BI->getSuccessor(1));
      if (!BI->getSuccessor(0)->isEntryBlock())
        BBs.emplace_back(BI->getSuccessor(0));

      if (BBs.empty())
        continue;

      GlobalVariable *LoadFrom = nullptr;

      if (BI->isConditional() ||
          indexmap.find(BI->getSuccessor(0)) == indexmap.end()) {
        // Create local table for conditional branches
        ArrayType *AT = ArrayType::get(Int8PtrTy, BBs.size());
        SmallVector<Constant *, 2> BlockAddresses;
        for (BasicBlock *BB : BBs)
          BlockAddresses.emplace_back(
              EncryptJumpTargetTemp ? ConstantExpr::getGetElementPtr(
                                          Int8Ty,
                                          ConstantExpr::getBitCast(
                                              BlockAddress::get(BB), Int8PtrTy),
                                          encmap[&Func])
                                    : BlockAddress::get(BB));

        Constant *BlockAddressArray =
            ConstantArray::get(AT, ArrayRef<Constant *>(BlockAddresses));

        std::string LocalTableName = "IndirectBranchingLocalTable_" + Func.getName().str();
        LoadFrom = new GlobalVariable(
            *M, AT, false, GlobalValue::LinkageTypes::PrivateLinkage,
            BlockAddressArray, LocalTableName);

        // Apply same COMDAT handling for local tables
        if (mayBeDiscarded(&Func)) {
          if (Func.hasComdat()) {
            LoadFrom->setComdat(Func.getComdat());
          } else if (Func.getLinkage() == GlobalValue::LinkOnceODRLinkage ||
                     Func.getLinkage() == GlobalValue::WeakODRLinkage) {
            Comdat *C = M->getOrInsertComdat(Func.getName());
            LoadFrom->setLinkage(Func.getLinkage());
            LoadFrom->setComdat(C);
          }
          LoadFrom->setUnnamedAddr(GlobalValue::UnnamedAddr::Global);
        }

        appendToCompilerUsed(*Func.getParent(), {LoadFrom});
      } else {
        LoadFrom = FuncTable;
      }

      AllocaInst *LoadFromAI = nullptr;
      if (UseStackTemp) {
        LoadFromAI = IRBEntry->CreateAlloca(LoadFrom->getType());
        IRBEntry->CreateStore(LoadFrom, LoadFromAI);
      }

      Value *index, *RealIndex = nullptr;
      if (BI->isConditional()) {
        Value *condition = BI->getCondition();
        Value *zext = IRBBI->CreateZExt(condition, Int32Ty);
        if (UseStackTemp) {
          AllocaInst *condAI = IRBEntry->CreateAlloca(Int32Ty);
          IRBBI->CreateStore(zext, condAI);
          index = condAI;
        } else {
          index = zext;
        }
        RealIndex = index;
      } else {
        Value *indexval = nullptr;
        ConstantInt *IndexEncKey =
            EncryptJumpTargetTemp ? cast<ConstantInt>(ConstantInt::get(
                                        Int32Ty, cryptoutils->get_uint32_t()))
                                  : nullptr;
        if (EncryptJumpTargetTemp) {
          std::string IndexGVName = "IndirectBranchingIndex_" + Func.getName().str();
          GlobalVariable *indexgv = new GlobalVariable(
              *M, Int32Ty, false, GlobalValue::LinkageTypes::PrivateLinkage,
              ConstantInt::get(IndexEncKey->getType(),
                               IndexEncKey->getValue() ^
                                   indexmap[BI->getSuccessor(0)]),
              IndexGVName);

          if (mayBeDiscarded(&Func) && Func.hasComdat()) {
            indexgv->setComdat(Func.getComdat());
          }

          appendToCompilerUsed(*M, {indexgv});
          indexval = (UseStackTemp ? IRBEntry : IRBBI)
                         ->CreateLoad(indexgv->getValueType(), indexgv);
        } else {
          indexval = ConstantInt::get(Int32Ty, indexmap[BI->getSuccessor(0)]);
          if (UseStackTemp) {
            AllocaInst *indexAI = IRBEntry->CreateAlloca(Int32Ty);
            IRBEntry->CreateStore(indexval, indexAI);
            indexval = IRBBI->CreateLoad(indexAI->getAllocatedType(), indexAI);
          }
        }
        index = indexval;
        RealIndex = EncryptJumpTargetTemp ? IRBBI->CreateXor(index, IndexEncKey)
                                          : index;
      }

      Value *LI, *enckeyLoad, *gepptr = nullptr;
      if (UseStackTemp) {
        LoadInst *LILoadFrom =
            IRBBI->CreateLoad(LoadFrom->getType(), LoadFromAI);
        Value *GEP = IRBBI->CreateGEP(
            LoadFrom->getValueType(), LILoadFrom,
            {zero, BI->isConditional() ? IRBBI->CreateLoad(Int32Ty, RealIndex)
                                       : RealIndex});
        if (!EncryptJumpTargetTemp)
          LI = IRBBI->CreateLoad(Int8PtrTy, GEP,
                                 "IndirectBranchingTargetAddress");
        else
          gepptr = IRBBI->CreateLoad(Int8PtrTy, GEP);
      } else {
        Value *GEP = IRBBI->CreateGEP(LoadFrom->getValueType(), LoadFrom,
                                      {zero, RealIndex});
        if (!EncryptJumpTargetTemp)
          LI = IRBBI->CreateLoad(Int8PtrTy, GEP,
                                 "IndirectBranchingTargetAddress");
        else
          gepptr = IRBBI->CreateLoad(Int8PtrTy, GEP);
      }

      if (EncryptJumpTargetTemp) {
        ConstantInt *encenckey = cast<ConstantInt>(
            ConstantInt::get(Int32Ty, cryptoutils->get_uint32_t()));
        std::string EncKeyGVName = "IndirectBranchingAddressEncryptKey_" + Func.getName().str();
        GlobalVariable *enckeyGV = new GlobalVariable(
            *M, Int32Ty, false, GlobalValue::LinkageTypes::PrivateLinkage,
            ConstantInt::get(Int32Ty,
                             encenckey->getValue() ^ encmap[&Func]->getValue()),
            EncKeyGVName);

        if (mayBeDiscarded(&Func) && Func.hasComdat()) {
          enckeyGV->setComdat(Func.getComdat());
        }

        appendToCompilerUsed(*M, enckeyGV);
        enckeyLoad = IRBBI->CreateXor(
            IRBBI->CreateLoad(enckeyGV->getValueType(), enckeyGV), encenckey);
        LI =
            IRBBI->CreateGEP(Int8Ty, gepptr, IRBBI->CreateSub(zero, enckeyLoad),
                             "IndirectBranchingTargetAddress");
      }

      IndirectBrInst *indirBr = IndirectBrInst::Create(LI, BBs.size());
      for (BasicBlock *BB : BBs)
        indirBr->addDestination(BB);
      ReplaceInstWithInst(BI, indirBr);
    }

    shuffleBasicBlocks(Func);
    return true;
  }

  void shuffleBasicBlocks(Function &F) {
    SmallVector<BasicBlock *, 32> blocks;
    for (BasicBlock &block : F)
      if (!block.isEntryBlock())
        blocks.emplace_back(&block);

    if (blocks.size() < 2)
      return;

    for (size_t i = blocks.size() - 1; i > 0; i--)
      std::swap(blocks[i], blocks[cryptoutils->get_range(i + 1)]);

    Function::iterator fi = F.begin();
    for (BasicBlock *block : blocks) {
      fi++;
      block->moveAfter(&*(fi));
    }
  }
};

} // namespace llvm

FunctionPass *llvm::createIndirectBranchPass(bool flag) {
  return new IndirectBranch(flag);
}

char IndirectBranch::ID = 0;
INITIALIZE_PASS(IndirectBranch, "indibran", "IndirectBranching", false, false)
