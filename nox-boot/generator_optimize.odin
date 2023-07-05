package nox

import "tracy"

/**
* Executes optimization algorithms.
*
* @param generator The reference to the generator.
*/
generator_optimize :: proc(generator: ^Generator) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  for symbol in generator.resolver_output.reachable_sorted_symbols {
    if symbol.kind != .Routine || .Uninstantiated_Generic in symbol.flags || .Routine_Disabled in symbol.flags {
      continue;
    }

    if .Is_Intrinsic in (cast(^Type_Routine) symbol.type).routine_flags {
      continue;
    }

    llvm_routine := generator_symbol_to_llvm(generator, symbol);
    LLVMRunFunctionPassManager(generator.llvm_function_pass_manager, llvm_routine);
  }

  LLVMRunPassManager(generator.llvm_module_pass_manager, generator.module);
}

/**
* Initialize the optimization algorithms for the module manager.
*
* @param generator          The reference to the generator.
* @param optimization_level The optimization level.
*/
generator_init_module_pass_manager :: proc(generator: ^Generator, optimization_level: int) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  llvm_module_pass_manager := LLVMCreatePassManager();
  generator.llvm_module_pass_manager = llvm_module_pass_manager;

  if optimization_level < 0 {
    return;
  }

  LLVMAddAlwaysInlinerPass(llvm_module_pass_manager);
  LLVMAddStripDeadPrototypesPass(llvm_module_pass_manager);
  LLVMAddAnalysisPasses(generator.llvm_target_machine, llvm_module_pass_manager);

  if optimization_level == 0 {
    return;
  }
  
  LLVMAddGlobalDCEPass(llvm_module_pass_manager);
  LLVMAddIPSCCPPass(llvm_module_pass_manager);
  LLVMAddCalledValuePropagationPass(llvm_module_pass_manager);
  LLVMAddGlobalOptimizerPass(llvm_module_pass_manager);
  LLVMAddDeadArgEliminationPass(llvm_module_pass_manager);
  LLVMAddCFGSimplificationPass(llvm_module_pass_manager);
  if optimization_level < 2 {
    return;
  }

  LLVMAddFunctionInliningPass(llvm_module_pass_manager);
  LLVMAddCFGSimplificationPass(llvm_module_pass_manager);
  LLVMAddJumpThreadingPass(llvm_module_pass_manager);
  LLVMAddSimplifyLibCallsPass(llvm_module_pass_manager);
  LLVMAddTailCallEliminationPass(llvm_module_pass_manager);
  LLVMAddCFGSimplificationPass(llvm_module_pass_manager);
  LLVMAddReassociatePass(llvm_module_pass_manager);
  LLVMAddLoopRotatePass(llvm_module_pass_manager);
  LLVMAddLICMPass(llvm_module_pass_manager);
  LLVMAddCFGSimplificationPass(llvm_module_pass_manager);
  LLVMAddLoopIdiomPass(llvm_module_pass_manager);
  LLVMAddLoopDeletionPass(llvm_module_pass_manager);
  LLVMAddMergedLoadStoreMotionPass(llvm_module_pass_manager);
  LLVMAddMemCpyOptPass(llvm_module_pass_manager);
  LLVMAddSCCPPass(llvm_module_pass_manager);
  LLVMAddBitTrackingDCEPass(llvm_module_pass_manager);
  LLVMAddJumpThreadingPass(llvm_module_pass_manager);
  LLVMAddLICMPass(llvm_module_pass_manager);
  LLVMAddLoopRerollPass(llvm_module_pass_manager);
  LLVMAddAggressiveDCEPass(llvm_module_pass_manager);
  LLVMAddCFGSimplificationPass(llvm_module_pass_manager);

  LLVMAddGlobalDCEPass(llvm_module_pass_manager);
  LLVMAddGlobalOptimizerPass(llvm_module_pass_manager);
  LLVMAddLoopRotatePass(llvm_module_pass_manager);
  LLVMAddLoopVectorizePass(llvm_module_pass_manager);
  LLVMAddEarlyCSEPass(llvm_module_pass_manager);
  LLVMAddLICMPass(llvm_module_pass_manager);
  LLVMAddCFGSimplificationPass(llvm_module_pass_manager);
  LLVMAddSLPVectorizePass(llvm_module_pass_manager);
  LLVMAddLICMPass(llvm_module_pass_manager);
  LLVMAddAlignmentFromAssumptionsPass(llvm_module_pass_manager);
  LLVMAddStripDeadPrototypesPass(llvm_module_pass_manager);
  LLVMAddGlobalDCEPass(llvm_module_pass_manager);
  LLVMAddConstantMergePass(llvm_module_pass_manager);
  LLVMAddCFGSimplificationPass(llvm_module_pass_manager);
}

/**
* Initialize the optimization algorithms for the function manager.
*
* @param generator          The reference to the generator.
* @param optimization_level The optimization level.
*/
generator_init_function_pass_manager :: proc(generator: ^Generator, optimization_level: int) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  llvm_function_pass_manager := LLVMCreateFunctionPassManagerForModule(generator.module);
  generator.llvm_function_pass_manager = llvm_function_pass_manager;

  LLVMInitializeFunctionPassManager(llvm_function_pass_manager);

  if optimization_level >= 0 {
    LLVMAddMemCpyOptPass(llvm_function_pass_manager);
    LLVMAddPromoteMemoryToRegisterPass(llvm_function_pass_manager);
    LLVMAddMergedLoadStoreMotionPass(llvm_function_pass_manager);

    if optimization_level > 0 {
      LLVMAddSCCPPass(llvm_function_pass_manager);
      LLVMAddPromoteMemoryToRegisterPass(llvm_function_pass_manager);
      LLVMAddUnifyFunctionExitNodesPass(llvm_function_pass_manager);
      LLVMAddCFGSimplificationPass(llvm_function_pass_manager);
      LLVMAddEarlyCSEPass(llvm_function_pass_manager);
      LLVMAddLowerExpectIntrinsicPass(llvm_function_pass_manager);
    }
  }

  LLVMFinalizeFunctionPassManager(llvm_function_pass_manager);
}
