/* accUnparse.h */
/* Copyright (C) 2014 RIKEN AICS (of modifications) */

namespace AccSupport
{
//#if (USE_ACC_IR_NODES == 1)

  void accUnparseDirective(UnparseLanguageIndependentConstructs* up,
                           SgAccDirective* v, SgUnparse_Info& info);
  void accUnparseDirectiveName(UnparseLanguageIndependentConstructs* up,
                               SgAccDirective* v, SgUnparse_Info& info);
  void accUnparseClause(UnparseLanguageIndependentConstructs* up,
                        SgAccClause* c, SgUnparse_Info& info);
  void accUnparseClauseName(UnparseLanguageIndependentConstructs* up,
                            SgAccClause* c, SgUnparse_Info& info);

//#endif //(USE_ACC_IR_NODES == 1)
}
