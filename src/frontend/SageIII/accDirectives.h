/* accDirectives.h */
/* Copyright (C) 2014 RIKEN AICS (of modifications) */

// This is derived from "OmpAttribute.h" and "ompAstConstruction.h",
// from "OMP" to "ACC".  See comments in them.

//#include "AstSimpleProcessing.h"

//class SgNode;

extern void acc_c_lexer_init(const char* s);
extern int acc_c_parse();
extern void acc_c_parser_init(SgPragmaDeclaration* d, SgStatement* n,
                              const char* s);
extern SgAccDirective* acc_c_parsed_directive();

extern void acc_f_lexer_init(const char* s);
extern int acc_f_parse();
extern void acc_f_parser_init(SgPragmaDeclaration* d, SgStatement* n,
                              const char* s);
extern SgAccDirective* acc_f_parsed_directive();

namespace AccSupport
{
#if 0
  class SgVarRefExpVisitor : public AstSimpleProcessing {
  private:
    std::vector<SgVarRefExp*> expressions;

  public:
    SgVarRefExpVisitor();
    std::vector<SgVarRefExp*> get_expressions();
    void visit(SgNode* node);
  };
#endif

  /*void accProcess(SgSourceFile* file);*/
  void accTakeoutDirectives(SgSourceFile *file);
  void accParseDirectives(SgSourceFile *file);

  enum acc_effected {acc_self, acc_next, acc_f_block};

  extern enum acc_effected accEffected(enum SgAccDirective::acc_directive e);

  enum SgAccDirective::acc_directive accBlockClosing(
    enum SgAccDirective::acc_directive e);

  // Import short visible names for enumerations.

#define DIRECTIVETYPE(X) \
  static const enum SgAccDirective::acc_directive X = SgAccDirective::X
#define CLAUSETYPE(X) \
  static const enum SgAccClause::acc_clause X = SgAccClause::X

  DIRECTIVETYPE(e_acc_parallel);
  DIRECTIVETYPE(e_acc_kernels);
  DIRECTIVETYPE(e_acc_data);
  DIRECTIVETYPE(e_acc_enter_data);
  DIRECTIVETYPE(e_acc_exit_data);
  DIRECTIVETYPE(e_acc_host_data);
  DIRECTIVETYPE(e_acc_loop);
  DIRECTIVETYPE(e_acc_parallel_loop);
  DIRECTIVETYPE(e_acc_kernels_loop);
  DIRECTIVETYPE(e_acc_cache);
  //DIRECTIVETYPE(e_acc_atomic);
  DIRECTIVETYPE(e_acc_atomic_read);
  DIRECTIVETYPE(e_acc_atomic_write);
  DIRECTIVETYPE(e_acc_atomic_update);
  DIRECTIVETYPE(e_acc_atomic_capture);
  DIRECTIVETYPE(e_acc_declare);
  DIRECTIVETYPE(e_acc_update);
  DIRECTIVETYPE(e_acc_routine);
  DIRECTIVETYPE(e_acc_wait);
  DIRECTIVETYPE(e_acc_end_parallel);
  DIRECTIVETYPE(e_acc_end_kernels);
  DIRECTIVETYPE(e_acc_end_data);
  DIRECTIVETYPE(e_acc_end_host_data);
  DIRECTIVETYPE(e_acc_end_atomic);

  CLAUSETYPE(e_acc_c_dtype);
  CLAUSETYPE(e_acc_c_if);
  CLAUSETYPE(e_acc_c_async);
  CLAUSETYPE(e_acc_c_wait);
  CLAUSETYPE(e_acc_c_num_gangs);
  CLAUSETYPE(e_acc_c_num_workers);
  CLAUSETYPE(e_acc_c_vector_length);
  CLAUSETYPE(e_acc_c_private);
  CLAUSETYPE(e_acc_c_firstprivate);
  CLAUSETYPE(e_acc_c_reduction);
  CLAUSETYPE(e_acc_c_default_none);
  CLAUSETYPE(e_acc_c_deviceptr);
  CLAUSETYPE(e_acc_c_copy);
  CLAUSETYPE(e_acc_c_copyin);
  CLAUSETYPE(e_acc_c_copyout);
  CLAUSETYPE(e_acc_c_create);
  CLAUSETYPE(e_acc_c_delete);
  CLAUSETYPE(e_acc_c_present);
  CLAUSETYPE(e_acc_c_pcopy);
  CLAUSETYPE(e_acc_c_pcopyin);
  CLAUSETYPE(e_acc_c_pcopyout);
  CLAUSETYPE(e_acc_c_pcreate);
  CLAUSETYPE(e_acc_c_use_device);
  CLAUSETYPE(e_acc_c_collapse);
  CLAUSETYPE(e_acc_c_gang);
  CLAUSETYPE(e_acc_c_worker);
  CLAUSETYPE(e_acc_c_vector);
  CLAUSETYPE(e_acc_c_seq);
  CLAUSETYPE(e_acc_c_auto);
  CLAUSETYPE(e_acc_c_tile);
  CLAUSETYPE(e_acc_c_independent);
  CLAUSETYPE(e_acc_c_device_resident);
  CLAUSETYPE(e_acc_c_link);
  CLAUSETYPE(e_acc_c_self);
  CLAUSETYPE(e_acc_c_host);
  CLAUSETYPE(e_acc_c_device);
  CLAUSETYPE(e_acc_c_bind);
  CLAUSETYPE(e_acc_c_nohost);
  CLAUSETYPE(e_acc_c_cache_arguments);
  CLAUSETYPE(e_acc_c_routine_name);
  CLAUSETYPE(e_acc_c_wait_arguments);
}
