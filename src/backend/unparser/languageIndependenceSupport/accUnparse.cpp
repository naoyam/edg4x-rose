/* accUnparse.cpp */
/* Copyright (C) 2014 RIKEN AICS (of modifications) */

// This code is taken from the OMP support code in
// "unparseLanguageIndependentConstructs.C"

// Most parts of directives are not language-dependent.

#include <iostream>
#include <limits>

#include "sage3basic.h"
#include "unparser.h"
#include "tokenStreamMapping.h"
#include "previousAndNextNode.h"
#include "accDirectives.h"

//using namespace std;

namespace AccSupport
{
//#if (USE_ACC_IR_NODES == 1)

  void accUnparseDirective(UnparseLanguageIndependentConstructs* up,
                           Unparser* unp,
                           SgAccDirective* v, SgUnparse_Info& info);
  void accUnparseDirectiveName(UnparseLanguageIndependentConstructs* up,
                               SgAccDirective* v, SgUnparse_Info& info);
  void accUnparseClause(UnparseLanguageIndependentConstructs* up,
                        SgAccClause* c, SgUnparse_Info& info);
  bool accUnparseClauseName(UnparseLanguageIndependentConstructs* up,
                            SgAccClause* c, SgUnparse_Info& info);
  void accUnparseExpression(UnparseLanguageIndependentConstructs* up,
                            SgExpression* e, SgUnparse_Info& info);

  void accUnparseDirective(UnparseLanguageIndependentConstructs* up,
                           Unparser* unp,
                           SgAccDirective* v, SgUnparse_Info& info) {
    bool foldline = unp->currentFile->get_acc_foldline();
    ROSE_ASSERT(v != NULL);
    up->unparseAccPrefix(v, info);
    up->curprint(" ");
    accUnparseDirectiveName(up, v, info);
    std::vector<SgAccClause*> cc = v->get_clauses();
    if (cc.size() > 0) {
      up->curprint(" ");
      for (std::vector<SgAccClause*>::iterator
             i = cc.begin(); i != cc.end(); i++) {
        if (i != cc.begin()) {
          if (!foldline) {
            up->curprint(", ");
          } else {
            up->curprint(", &\n");
            up->unparseAccPrefix(v, info);
            up->curprint(" & ");
          }
        }
        accUnparseClause(up, (*i), info);
      }
    }
    up->curprint("\n");
    /*unp->u_sage->curprint_newline();*/
  }

  void accUnparseDirectiveName(UnparseLanguageIndependentConstructs* up,
                               SgAccDirective* v, SgUnparse_Info& info) {
    ROSE_ASSERT(v != NULL);
    enum SgAccDirective::acc_directive e = v->get_name();
    switch (e) {
      // Directives (2.5)

    case SgAccDirective::e_acc_parallel:
      up->curprint("parallel");
      break;
    case SgAccDirective::e_acc_kernels:
      up->curprint("kernels");
      break;
    case SgAccDirective::e_acc_data:
      up->curprint("data");
      break;
    case SgAccDirective::e_acc_enter_data:
      up->curprint("enter data");
      break;
    case SgAccDirective::e_acc_exit_data:
      up->curprint("exit data");
      break;
    case SgAccDirective::e_acc_host_data:
      up->curprint("host data");
      break;

      // Constructs (2.6.6, 2.7)

    case SgAccDirective::e_acc_loop:
      up->curprint("loop");
      break;
    case SgAccDirective::e_acc_parallel_loop:
      up->curprint("parallel loop");
      break;
    case SgAccDirective::e_acc_kernels_loop:
      up->curprint("kernels loop");
      break;
    case SgAccDirective::e_acc_cache:
      up->curprint("cache");
      break;

    case SgAccDirective::e_acc_atomic_update:
      up->curprint("atomic update");
      break;
    case SgAccDirective::e_acc_atomic_read:
      up->curprint("atomic read");
      break;
    case SgAccDirective::e_acc_atomic_write:
      up->curprint("atomic write");
      break;
    case SgAccDirective::e_acc_atomic_capture:
      up->curprint("atomic capture");
      break;

    case SgAccDirective::e_acc_declare:
      up->curprint("declare");
      break;

      // Executable Directives (2.12)

    case SgAccDirective::e_acc_update:
      up->curprint("update");
      break;
    case SgAccDirective::e_acc_routine:
      up->curprint("routine");
      break;
    case SgAccDirective::e_acc_wait:
      up->curprint("wait");
      break;

      // Fortran Block End (omitting the optional ones)

    case SgAccDirective::e_acc_end_parallel:
      up->curprint("end parallel");
      break;

    case SgAccDirective::e_acc_end_kernels:
      up->curprint("end kernels");
      break;

    case SgAccDirective::e_acc_end_data:
      up->curprint("end data");
      break;

    case SgAccDirective::e_acc_end_host_data:
      up->curprint("end host_data");
      break;

    case SgAccDirective::e_acc_end_atomic:
      up->curprint("end atomic");
      break;
    };
  }

  // Prints ACC clauses.  Reduction clauses need specific printing.
  // Arguments to device_type clauses are unquoted.  It treats array
  // sections specially in C; Note they only appear in var-list, and
  // thus, array section references are at the top-level.

  void accUnparseClause(UnparseLanguageIndependentConstructs* up,
                        SgAccClause* c, SgUnparse_Info& info) {
    ROSE_ASSERT(c != NULL);
    bool printsome = accUnparseClauseName(up, c, info);
    enum SgAccClause::acc_clause e = c->get_name();
    if (e == SgAccClause::e_acc_c_reduction) {
      up->curprint(printsome ? " (" : "(");
      std::vector<SgExpression*> ee = c->get_exprs();
      for (std::vector<SgExpression*>::iterator
             i = ee.begin(); i != ee.end(); i++) {
        if (i == ee.begin()) {
          assert(isSgStringVal(*i) != NULL);
          SgStringVal *n = isSgStringVal(*i);
          up->unparseAccReduceOp(n, info);
          up->curprint(" : ");
        } else if (i == (ee.begin() + 1)) {
          up->unparseExpression((*i), info);
        } else {
          up->curprint(", ");
          up->unparseExpression((*i), info);
        }
      }
      up->curprint(")");
    } else if (e == SgAccClause::e_acc_c_dtype) {
      up->curprint(printsome ? " (" : "(");
      std::vector<SgExpression*> ee = c->get_exprs();
      for (std::vector<SgExpression*>::iterator
             i = ee.begin(); i != ee.end(); i++) {
        if (i != ee.begin()) {
          up->curprint(", ");
        }
        SgStringVal* s = isSgStringVal(*i);
        assert(s != NULL);
        std::string v = s->get_value();
        up->curprint(v);
      }
      up->curprint(")");
    } else if (e == SgAccClause::e_acc_c_gang) {
      std::vector<SgExpression*> ee = c->get_exprs();
      if (ee.size() > 0) {
        assert(ee.size() == 2);
        SgExpression* e0 = ee[0];
        SgExpression* e1 = ee[1];
        if (e0 != NULL || e1 != NULL) {
          up->curprint(printsome ? " (" : "(");
          if (e0 != NULL) {
            up->curprint("num: ");
            up->unparseExpression(e0, info);
          }
          if (e0 != NULL && e1 != NULL) {
            up->curprint(", ");
          }
          if (e1 != NULL) {
            if (isSgStringVal(e1) != NULL) {
              up->curprint("static: *");
            } else {
              up->curprint("static: ");
              up->unparseExpression(e1, info);
            }
          }
          up->curprint(")");
        }
      }
    } else {
      std::vector<SgExpression*> ee = c->get_exprs();
      if (ee.size() > 0) {
        up->curprint(printsome ? " (" : "(");
        for (std::vector<SgExpression*>::iterator
               i = ee.begin(); i != ee.end(); i++) {
          if (i != ee.begin()) {
            up->curprint(", ");
          }
          /*up->unparseExpression((*i), info);*/
          accUnparseExpression(up, (*i), info);
        }
        up->curprint(")");
      }
    }
  }

  bool accUnparseClauseName(UnparseLanguageIndependentConstructs* up,
                            SgAccClause* c, SgUnparse_Info& info) {
    bool printed = false;
    enum SgAccClause::acc_clause e = c->get_name();
    switch (e) {
      // Clauses

      /* (dtype for device_type) */

    case SgAccClause::e_acc_c_dtype:
      up->curprint("dtype");
      printed = true;
      break;
    case SgAccClause::e_acc_c_if:
      up->curprint("if");
      printed = true;
      break;
    case SgAccClause::e_acc_c_async:
      up->curprint("async");
      printed = true;
      break;
    case SgAccClause::e_acc_c_wait:
      up->curprint("wait");
      printed = true;
      break;
    case SgAccClause::e_acc_c_num_gangs:
      up->curprint("num_gangs");
      printed = true;
      break;
    case SgAccClause::e_acc_c_num_workers:
      up->curprint("num_workers");
      printed = true;
      break;
    case SgAccClause::e_acc_c_vector_length:
      up->curprint("vector_length");
      printed = true;
      break;
    case SgAccClause::e_acc_c_private:
      up->curprint("private");
      printed = true;
      break;
    case SgAccClause::e_acc_c_firstprivate:
      up->curprint("firstprivate");
      printed = true;
      break;
    case SgAccClause::e_acc_c_reduction:
      up->curprint("reduction");
      printed = true;
      break;
    case SgAccClause::e_acc_c_default_none:
      up->curprint("default(none)");
      printed = true;
      break;

      // Data Clauses (2.6.5)

    case SgAccClause::e_acc_c_deviceptr:
      up->curprint("deviceptr");
      printed = true;
      break;
    case SgAccClause::e_acc_c_copy:
      up->curprint("copy");
      printed = true;
      break;
    case SgAccClause::e_acc_c_copyin:
      up->curprint("copyin");
      printed = true;
      break;
    case SgAccClause::e_acc_c_copyout:
      up->curprint("copyout");
      printed = true;
      break;
    case SgAccClause::e_acc_c_create:
      up->curprint("create");
      printed = true;
      break;
    case SgAccClause::e_acc_c_delete:
      up->curprint("delete");
      printed = true;
      break;
    case SgAccClause::e_acc_c_present:
      up->curprint("present");
      printed = true;
      break;
    case SgAccClause::e_acc_c_pcopy:
      up->curprint("pcopy");
      printed = true;
      break;
    case SgAccClause::e_acc_c_pcopyin:
      up->curprint("pcopyin");
      printed = true;
      break;
    case SgAccClause::e_acc_c_pcopyout:
      up->curprint("pcopyout");
      printed = true;
      break;
    case SgAccClause::e_acc_c_pcreate:
      up->curprint("pcreate");
      printed = true;
      break;

      // Host_Data Clauses

    case SgAccClause::e_acc_c_use_device:
      up->curprint("use_device");
      printed = true;
      break;

      // Loop Clauses

    case SgAccClause::e_acc_c_collapse:
      up->curprint("collapse");
      printed = true;
      break;
    case SgAccClause::e_acc_c_gang:
      up->curprint("gang");
      printed = true;
      break;
    case SgAccClause::e_acc_c_worker:
      up->curprint("worker");
      printed = true;
      break;
    case SgAccClause::e_acc_c_vector:
      up->curprint("vector");
      printed = true;
      break;
    case SgAccClause::e_acc_c_seq:
      up->curprint("seq");
      printed = true;
      break;
    case SgAccClause::e_acc_c_auto:
      up->curprint("auto");
      printed = true;
      break;
    case SgAccClause::e_acc_c_tile:
      up->curprint("tile");
      printed = true;
      break;
    case SgAccClause::e_acc_c_independent:
      up->curprint("independent");
      printed = true;
      break;

    case SgAccClause::e_acc_c_device_resident:
      up->curprint("device_resident");
      printed = true;
      break;
    case SgAccClause::e_acc_c_link:
      up->curprint("link");
      printed = true;
      break;

    case SgAccClause::e_acc_c_self:
      up->curprint("self");
      printed = true;
      break;
    case SgAccClause::e_acc_c_host:
      up->curprint("host");
      printed = true;
      break;
    case SgAccClause::e_acc_c_device:
      up->curprint("device");
      printed = true;
      break;
    case SgAccClause::e_acc_c_bind:
      up->curprint("bind");
      printed = true;
      break;
    case SgAccClause::e_acc_c_nohost:
      up->curprint("nohost");
      printed = true;
      break;

      // Additions for internal use

    case SgAccClause::e_acc_c_cache_arguments:
      //nothing
      break;
    case SgAccClause::e_acc_c_routine_name:
      //nothing
      break;
    case SgAccClause::e_acc_c_wait_arguments:
      //nothing
      break;
    }
    return printed;
  }

  // Prints an expression, but treats array sections specially in C.

  void accUnparseExpression(UnparseLanguageIndependentConstructs* up,
                            SgExpression* e, SgUnparse_Info& info) {
    Unparse_ExprStmt* langc = dynamic_cast<Unparse_ExprStmt*>(up);
    if (langc == NULL) {
      up->unparseExpression(e, info);
    } else if (isSgPntrArrRefExp(e) != NULL) {
      SgPntrArrRefExp* x = isSgPntrArrRefExp(e);
      SgExpression* lhs = x->get_lhs_operand();
      accUnparseExpression(up, lhs, info);
      up->curprint("[");
      SgExpression* rhs = x->get_rhs_operand();
      accUnparseExpression(up, rhs, info);
      up->curprint("]");
    } else if (isSgSubscriptExpression(e) != NULL) {
      SgSubscriptExpression* x = isSgSubscriptExpression(e);
      SgExpression* lb = x->get_lowerBound();
      if (isSgNullExpression(lb) == NULL) {
        up->unparseExpression(lb, info);
      }
      up->curprint(":");
      SgExpression* ub = x->get_upperBound();
      if (isSgNullExpression(ub) == NULL) {
        up->unparseExpression(ub, info);
      }
      SgExpression* st = x->get_stride();
      SgIntVal* v = isSgIntVal(st);
      assert(v != NULL && v->get_value() == 1);
    } else {
      up->unparseExpression(e, info);
    }
  }

//#endif //(USE_ACC_IR_NODES == 1)
}

//#if (USE_ACC_IR_NODES == 1)

// Unparses an ACC block.  This is dispatched directly from
// "unparseStatement()"; not via "unparseLanguageSpecificStatement()".
// It takes an Unparser "unp" and passes it to ACC unparser here,
// because "unp" is protected.

void UnparseLanguageIndependentConstructs
::unparseAccBlock(SgStatement* stmt, SgUnparse_Info& info) {
  Unparser* unparser = unp;
  SgAccBlock* b = isSgAccBlock(stmt);
  ROSE_ASSERT(b != NULL);
  SgAccDirective* v = b->get_directive();
  AccSupport::accUnparseDirective(this, unparser, v, info);
  SgStatement* body = b->get_body();
  if (body != NULL) {
    unparseStatement(body, info);
  }
  SgAccBlock* closing = b->get_closing();
  if (closing != NULL) {
    unparseStatement(closing, info);
  }
}

// Aborts when not overrided.

void UnparseLanguageIndependentConstructs::unparseAccPrefix(
  SgAccDirective* v, SgUnparse_Info& info) {
  std::cerr << "error: unparseAccPrefix() is abstract" << std::endl;
  ROSE_ASSERT(0);
}

// Aborts when not overrided.

void UnparseLanguageIndependentConstructs::unparseAccReduceOp(
  SgExpression* expr, SgUnparse_Info& info) {
  std::cerr << "error: unparseAccReduceOp() is abstract" << std::endl;
  ROSE_ASSERT(0);
}

// (C unparser) THIS SHOULD BE PLACED in "unparseCxx_statements.C" in
// "src/backend/unparser/CxxCodeGeneration/".

void Unparse_ExprStmt::unparseAccPrefix(
  SgAccDirective* v, SgUnparse_Info& info) {
  curprint("\n#pragma acc");
}

void Unparse_ExprStmt::unparseAccReduceOp(
  SgExpression* expr, SgUnparse_Info& info) {
  SgStringVal* s = isSgStringVal(expr);
  ROSE_ASSERT(s != NULL);
  std::string op = s->get_value();
  if (op == "plus") {
    curprint("+");
  } else if (op == "times") {
    curprint("*");
  } else if (op == "max") {
    curprint("max");
  } else if (op == "min") {
    curprint("min");
  } else if (op == "bitand") {
    curprint("&");
  } else if (op == "bitor") {
    curprint("|");
  } else if (op == "bitxor") {
    curprint("^");
  } else if (op == "logand") {
    curprint("&&");
  } else if (op == "logor") {
    curprint("||");
  } else if (op == "eqv") {
    ROSE_ASSERT(op != "eqv");
  } else if (op == "neqv") {
    ROSE_ASSERT(op != "neqv");
  } else {
    ROSE_ASSERT(0);
  }
}

// (Fortran unparser) THIS SHOULD BE PLACED in
// "unparseFortran_statements.C" in
// "src/backend/unparser/FortranCodeGeneration/".

void FortranCodeGeneration_locatedNode::unparseAccPrefix(
  SgAccDirective* v, SgUnparse_Info& info) {
  curprint("!$acc");
}

void FortranCodeGeneration_locatedNode::unparseAccReduceOp(
  SgExpression* expr, SgUnparse_Info& info) {
  SgStringVal* s = isSgStringVal(expr);
  ROSE_ASSERT(s != NULL);
  std::string op = s->get_value();
  if (op == "plus") {
    curprint("+");
  } else if (op == "times") {
    curprint("*");
  } else if (op == "max") {
    curprint("max");
  } else if (op == "min") {
    curprint("min");
  } else if (op == "bitand") {
    curprint("iand");
  } else if (op == "bitor") {
    curprint("ior");
  } else if (op == "bitxor") {
    curprint("ieor");
  } else if (op == "logand") {
    curprint(".and.");
  } else if (op == "logor") {
    curprint(".or.");
  } else if (op == "eqv") {
    curprint(".eqv.");
  } else if (op == "neqv") {
    curprint(".neqv.");
  } else {
    ROSE_ASSERT(0);
  }
}

//#endif //(USE_ACC_IR_NODES == 1)
