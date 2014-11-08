/* accFParser.yy -*-Mode: C++;-*- */
/* Copyright (C) 2014 RIKEN AICS (of modifications) */

// OpenACC (2.0) Parser.  This is derived from "ompparser.yy".  See
// comments of it.

/* Hitory of "ompparser.yy": */
/* Author: Markus Schordan, 2003 */
/* Modified by Christian Biesinger 2006 for OpenMP 2.0 */
/* Modified by Chunhua Liao for OpenMP 3.0 and connect to OmpAttribute, 2008 */

// RESTRICTIONS: (0) No (user defined) operators yet. (1) No
// common-block specifications.  (2) It assumes no string literals
// (they are downcased when appears).  (3) Optional block end markers
// are simply dropped, and no syntactic checks are performed.  (4)
// Location information is imprecise; all locations are at the
// beginning of directives.  (5) Complex literals need folding unary
// -/+ expressions to values for being constants.  It is not
// implemented.

// MEMO: (0) Directive syntax is the same for C and Fortran.  See
// "accCParser.yy" first.  The rules of Fortran expressions are
// adopted from the F2008 specification.  Large rule changes on
// "primary_R701" are needed to unify array references (part_ref_R612)
// and function calls (function_reference_R1219).  (part_ref_R612 is
// expanded from primary_R701 via a chain of designator_R601 ->
// array_element_R617 -> data_ref_R611).  (1) List of device-type is
// comma-separated.

%name-prefix="acc_f_"
%defines
%error-verbose

%{
#include <stdio.h>
#include <iostream>
#include <vector>
#include <assert.h>
#include "sage3basic.h"
#include "sageBuilder.h"
#include "fortran_support.h"
#include "accDirectives.h"

using namespace AccSupport;

#define acc_x_lexer_init acc_f_lexer_init
#define acc_x_directive acc_f_directive
#define acc_x_clause acc_f_clause
#define acc_x_directive_node acc_f_directive_node
#define acc_x_directive_next acc_f_directive_next
#define acc_x_location acc_f_location
#define acc_x_current acc_f_current
#define acc_x_scope acc_f_scope
#define acc_x_set_location acc_f_set_location

extern int acc_f_lex();
static int acc_f_error(const char*);

static SgPragmaDeclaration* acc_x_directive_node;
static SgStatement* acc_x_directive_next;
static Sg_File_Info* acc_x_location;
static SgAccDirective* acc_x_current;
static SgScopeStatement* acc_x_scope;

// Sets the both start/end locations at the beginning of directives;
// {e->set_file_info()} only sets the start.

static void acc_x_set_location(SgExpression *e, Sg_File_Info* location) {
  e->set_startOfConstruct(location);
  e->set_endOfConstruct(location);
  e->set_operatorPosition(location);
  /*loc->set_parent(e);*/
}

static SgAccDirective* acc_x_directive(
  enum SgAccDirective::acc_directive e,
  std::vector<SgAccClause*> *cc, Sg_File_Info* location) {
  SgAccDirective* v;
  if (cc == NULL) {
    v = new SgAccDirective(location, e);
  } else {
    v = new SgAccDirective(location, e, *cc);
    delete cc;
  }
  return v;
}

static SgAccClause* acc_x_clause(
  enum SgAccClause::acc_clause e,
  std::vector<SgExpression*> *ee, Sg_File_Info* location) {
  SgAccClause* c;
  if (ee == NULL) {
    c = new SgAccClause(location, e);
  } else {
    c = new SgAccClause(location, e, *ee);
    delete ee;
  }
  return c;
}

static SgExpression* acc_f_make_triplet(
  SgExpression* a0, SgExpression* a1, SgExpression* a2,
  Sg_File_Info* location) {
  SgExpression* lb;
  if (a0 != NULL) {
    lb = a0;
  } else {
    lb = SageBuilder::buildNullExpression();
    acc_x_set_location(lb, location);
  }
  SgExpression* ub;
  if (a1 != NULL) {
    ub = a1;
  } else {
    ub = SageBuilder::buildNullExpression();
    acc_x_set_location(ub, location);
  }
  SgExpression* st;
  if (a2 != NULL) {
    st = a2;
  } else {
    st = new SgIntVal(1, "1");
    acc_x_set_location(st, location);
  }
  SgExpression* e = new SgSubscriptExpression(lb, ub, st);
  lb->set_parent(e);
  ub->set_parent(e);
  st->set_parent(e);
  /*e->set_file_info(location);*/
  acc_x_set_location(e, location);
  return e;
}

// Makes either an array reference or a function call for "r()".  See
// c_action_data_ref() in "FortranParserActionROSE.C".  It handles
// SgPointerType, SgTypeCrayPointer, SgTypeString, SgArrayType, and
// SgFunctionType.  IT ASSUMES VERY AD-HOC RETURN TYPES FOR INTRINSIC
// FUNCTIONS.

static SgExpression* acc_f_make_app(SgExpression* r,
                                    std::vector<SgExpression*>* a,
                                    SgScopeStatement* scope,
                                    Sg_File_Info* location) {
  SgVarRefExp* vref = isSgVarRefExp(r);
  bool intrinsicp;
  if (vref != NULL) {
    SgVariableSymbol* n0 = vref->get_symbol();
    const char* name = n0->get_name().str();
    intrinsicp = matchAgainstIntrinsicFunctionList(name);
  } else {
    intrinsicp = false;
  }

  SgType* t = r->get_type();
  assert(intrinsicp || isSgTypeUnknown(t) == NULL);

  if (intrinsicp) {
    SgVariableSymbol* n0 = vref->get_symbol();
    SgName name = n0->get_name();
    /* (generateImplicitFunctionType() cannot be used. */
    SgType* rt = generateImplicitType(name);
    SgFunctionType* ty = new SgFunctionType(rt, false);
    for (std::vector<SgExpression*>::iterator
           i = a->begin(); i < a->end(); i++) {
      ty->append_argument((*i)->get_type());
    }

    SgProcedureHeaderStatement*
      d = new SgProcedureHeaderStatement(name, ty, NULL);
    d->set_startOfConstruct(location);
    d->set_endOfConstruct(location);
    d->set_scope(scope);
    d->set_firstNondefiningDeclaration(d);
    d->set_definingDeclaration(NULL);
    d->set_subprogram_kind(SgProcedureHeaderStatement::
                           e_function_subprogram_kind);
    SgFunctionParameterList *pp = d->get_parameterList();
    assert(pp != NULL);
    /*pp->set_scope(scope);*/

    SgFunctionSymbol* s = new SgFunctionSymbol(d);
    d->set_parent(s);
    SgFunctionRefExp* f = new SgFunctionRefExp(s, NULL);
    s->set_parent(f);
    acc_x_set_location(f, location);

    SgExprListExp* ax = SageBuilder::buildExprListExp(*a);
    SgFunctionCallExp* e = new SgFunctionCallExp(f, ax, NULL);
    f->set_parent(e);
    ax->set_parent(e);
    acc_x_set_location(e, location);
    delete a;
    return e;
  } else if (isSgTypeString(t) != NULL
             || isSgPointerType(t) != NULL
             || isSgTypeCrayPointer(t) != NULL) {
    // (No buildPntrArrRefExp()).
    /*{e->set_file_info(vref->get_file_info())} does not work.*/
    if (a->size() != 1) {
      std::cerr << "ACC: error: Multiple dimensions to reference a type"
      << " (" << t->class_name() <<")" << std::endl;
      ROSE_ASSERT(0);
    }
    SgExpression* ax = (*a)[0];
    SgExpression* e = new SgPntrArrRefExp(r, ax, NULL);
    vref->set_parent(e);
    ax->set_parent(e);
    acc_x_set_location(e, location);
    delete a;
    return e;
  } else if (isSgArrayType(t) != NULL) {
    // (No buildPntrArrRefExp()).
    /*{e->set_file_info(vref->get_file_info())} does not work.*/
    SgExprListExp* ax = SageBuilder::buildExprListExp(*a);
    SgExpression* e = new SgPntrArrRefExp(r, ax, NULL);
    vref->set_parent(e);
    ax->set_parent(e);
    ax->set_parent(e);
    acc_x_set_location(e, location);
    delete a;
    return e;
  } else if (isSgFunctionType(t) != NULL) {
    SgExprListExp* ax = SageBuilder::buildExprListExp(*a);
    SgExpression* e = SageBuilder::buildFunctionCallExp(r, ax);
    delete a;
    return e;
  } else {
    std::cerr << "ACC: error: Bad application form to a type"
    << " (" << t->class_name() <<")" << std::endl;
    ROSE_ASSERT(0);
  }
}

// Scans a real value.  It ignores KIND and PRECISION.

static SgExpression* acc_f_parse_real(
  const char* s0, Sg_File_Info* location) {
  std::string s(s0);
  size_t kpos = s.find('_');
  if (kpos != std::string::npos) {
    s.resize(kpos);
  }
  SgExpression* e;
  if (s.find('d')) {
    double v;
    int cc = sscanf(s.c_str(), "%le", &v);
    if (cc != 1) {
      std::cerr << "ACC: error: Bad literal (" << s << ")" << std::endl;
      ROSE_ASSERT(0);
    }
    SgDoubleVal* e0 = SageBuilder::buildDoubleVal(v);
    e0->set_valueString(std::string(s0));
    e = e0;
  } else {
    float v;
    int cc = sscanf(s.c_str(), "%e", &v);
    if (cc != 1) {
      std::cerr << "ACC: error: Bad literal (" << s << ")" << std::endl;
      ROSE_ASSERT(0);
    }
    SgFloatVal* e0 = SageBuilder::buildFloatVal(v);
    e0->set_valueString(std::string(s0));
    e = e0;
  }
  acc_x_set_location(e, location);
  return e;
}

// Scans an integer value.  It ignores KIND and PRECISION.

static SgExpression* acc_f_parse_integer(
  const char* s0, Sg_File_Info* location) {
  std::string s(s0);
  long v;
  int cc = sscanf(s.c_str(), "%ld", &v);
  if (cc != 1) {
    std::cerr << "ACC: error: Bad literal (" << s << ")" << std::endl;
    ROSE_ASSERT(0);
  }
  SgIntVal* e0 = SageBuilder::buildIntVal(v);
  e0->set_valueString(std::string(s0));
  acc_x_set_location(e0, location);
  return e0;
}

// Scans string literals.  RESTRICTIONS: It leaves consecutive quotes
// and escaped characters intact.

static SgExpression* acc_f_scan_string(
  std::string& s0, Sg_File_Info* location) {
  assert(s0.size() >= 2);
  std::string s1 = s0.substr(1, s0.size() - 2);
  assert((s0[0] != '\'' || s1.find("''") == std::string::npos)
         && (s0[0] != '"' || s1.find("\"\"") == std::string::npos)
         && s1.find('\\') == std::string::npos);
  SgExpression* e = new SgStringVal(location, s1);
  return e;
}

// Folds -/+ unary to make constants.

static SgValueExp* acc_f_fold_unary_plusminus(SgExpression* e) {
  SgValueExp* x = isSgValueExp(e);
  if (x != NULL) {
    return x;
  } else {
    /*TODO*/
    std::cerr << "ACC: Complex literals not implemented" << std::endl;
    ROSE_ASSERT(0);
  }
}

%}

%union {
  const char* string;
  int ivalue;
  double dvalue;
  SgAccDirective* directive;
  SgAccClause* clause;
  SgExpression* expr;
  std::vector<SgAccClause*>* vec_clause;
  std::vector<SgExpression*>* vec_expr;
}

%token <string> ACC END
%token <string> PARALLEL KERNELS DATA ENTER EXIT HOST_DATA LOOP
%token <string> CACHE ATOMIC READ WRITE UPDATE CAPTURE
%token <string> DECLARE ROUTINE WAIT
%token <string> DTYPE IF ASYNC NUM_GANGS NUM_WORKERS VECTOR_LENGTH
%token <string> PRIVATE FIRSTPRIVATE REDUCTION DEFAULT NONE
%token <string> DEVICEPTR COPY COPYIN COPYOUT CREATE DELETE PRESENT
%token <string> PCOPY PCOPYIN PCOPYOUT PCREATE
%token <string> USE_DEVICE
%token <string> COLLAPSE GANG WORKER VECTOR SEQ AUTO TILE INDEPENDENT
%token <string> DEVICE_RESIDENT LINK SELF HOST DEVICE BIND NOHOST
%token <string> NUM STATIC LENGTH
%token <string> MAX MIN IAND IOR IEOR

%token <string> POWER '*' '/' '+' '-' CONCAT
%token <string> _EQ_ _NE_ _LT_ _LE_ _GT_ _GE_
%token <string> EQV NEQV '<' LE '>' GE
%token <string> _NOT_ _AND_ _OR_ _EQV_ _NEQV_
%token <string> '=' '(' ')' ':' ','
%token <string> _TRUE_ _FALSE_
//%token <string> '[' ']' ',' '?' '&' '|' '^'
//%token <string> '.' DOT_OP_DOT

%token <string> DIGITS
%token <string> CONSTANTBOZ
%token <string> IDENTIFIER
%token <string> CONSTANTI CONSTANTR
%token <string> STRING

%token LEXICALERROR

%type <directive> acc_directive acc_directive1
%type <directive> parallel_directive kernels_directive
%type <directive> data_directive enter_data_directive
%type <directive> exit_data_directive host_data_directive loop_directive
%type <directive> cache_directive parallel_loop_directive
%type <directive> kernels_loop_directive atomic_directive
%type <directive> declare_directive update_directive routine_directive
%type <directive> wait_directive

%type <vec_clause> wait_directive_prefix
%type <vec_clause> parallel_clause_seq kernels_clause_seq
%type <vec_clause> data_clause_seq enter_data_clause_seq
%type <vec_clause> exit_data_clause_seq host_data_clause_seq
%type <vec_clause> loop_clause_seq parallel_loop_clause_seq
%type <vec_clause> kernels_loop_clause_seq declare_clause_seq
%type <vec_clause> update_clause_seq routine_clause_seq
%type <vec_clause> wait_clause_seq

%type <clause> parallel_clauses kernels_clauses data_clauses
%type <clause> enter_data_clauses exit_data_clauses
%type <clause> host_data_clauses loop_clauses
%type <clause> parallel_loop_clauses kernels_loop_clauses
%type <clause> declare_clauses update_clauses routine_clauses
%type <clause> wait_clauses dtype_clause if_clause
%type <clause> async_clause wait_clause num_gangs_clause
%type <clause> num_workers_clause vector_length_clause
%type <clause> private_clause firstprivate_clause
%type <clause> reduction_clause default_none_clause
%type <clause> deviceptr_clause copy_clause copyin_clause
%type <clause> copyout_clause create_clause delete_clause
%type <clause> present_clause pcopy_clause pcopyin_clause
%type <clause> pcopyout_clause pcreate_clause
%type <clause> use_device_clause collapse_clause gang_clause
%type <clause> worker_clause vector_clause seq_clause
%type <clause> auto_clause tile_clause independent_clause
%type <clause> device_resident_clause link_clause self_clause
%type <clause> host_clause device_clause bind_clause
%type <clause> nohost_clause

%type <string> reduction_operator

%type <vec_expr> size_expr_list
%type <expr> size_expr

%type <vec_expr> dtype_arg dtype_list
%type <vec_expr> expr_list var_list
%type <vec_expr> gang_arg_list

%type <expr> expr expr_R722 level_5_expr_R717 equiv_operand_R716
%type <expr> or_operand_R715 and_operand_R714 level_4_expr_R712
%type <expr> level_3_expr_R710
%type <expr> level_2_expr_R706
//%type <expr> add_operand_with_unary_
%type <expr> add_operand_R705
%type <expr> mult_operand_R704 level_1_expr_R702

%type <expr> primary_R701
%type <expr> literal_constant_R305
%type <expr> subscript_or_argument_R620_R1222
%type <vec_expr> subscript_or_argument_list
%type <expr> triplet_R621

//%type <string> defined_binary_op_R723 defined_unary_op_R703
//%type <string> defined_unary_op_R703

%type <expr> subsections
%type <vec_expr> section_subscript_list
%type <expr> varref
%type <expr> dtype

%type <string> varid

/* start point for the parsing */

%start acc_directive

%%

acc_directive
        : acc_directive1 {
           acc_x_current = $1;
        }
        ;

acc_directive1
        : parallel_directive
        | kernels_directive
        | data_directive
        | enter_data_directive
        | exit_data_directive
        | host_data_directive
        | loop_directive
        | cache_directive
        | parallel_loop_directive
        | kernels_loop_directive
        | atomic_directive
        | declare_directive
        | update_directive
        | routine_directive
        | wait_directive
        | ACC END PARALLEL {
          $$ = acc_x_directive(e_acc_end_parallel, NULL, acc_x_location);
        }
        | ACC END KERNELS {
          $$ = acc_x_directive(e_acc_end_kernels, NULL, acc_x_location);
        }
        | ACC END DATA {
          $$ = acc_x_directive(e_acc_end_data, NULL, acc_x_location);
        }
        | ACC END HOST_DATA {
          $$ = acc_x_directive(e_acc_end_host_data, NULL, acc_x_location);
        }
        | ACC END PARALLEL LOOP {
          $$ = NULL;
        }
        | ACC END KERNELS LOOP {
          $$ = NULL;
        }
        | ACC END ATOMIC {
          $$ = acc_x_directive(e_acc_end_atomic, NULL, acc_x_location);
        }
        | ACC {yyerror("Unsupported ACC directive found\n");}
        ;

/* Directives */

parallel_directive
        : ACC PARALLEL {
          $$ = acc_x_directive(e_acc_parallel, NULL, acc_x_location);
        }
        | ACC PARALLEL parallel_clause_seq {
          $$ = acc_x_directive(e_acc_parallel, $3, acc_x_location);
        }
        ;

parallel_clause_seq
        : parallel_clauses {
          $$ = new std::vector<SgAccClause*>(1, $1);
        }
        | parallel_clause_seq parallel_clauses {
          $1->push_back($2);
          $$ = $1;
        }
        | parallel_clause_seq ',' parallel_clauses {
          $1->push_back($3);
          $$ = $1;
        }
        ;

parallel_clauses
        : async_clause
        | wait_clause
        | num_gangs_clause
        | num_workers_clause
        | vector_length_clause
        | dtype_clause
        | if_clause
        | reduction_clause
        | copy_clause
        | copyin_clause
        | copyout_clause
        | create_clause
        | present_clause
        | pcopy_clause
        | pcopyin_clause
        | pcopyout_clause
        | pcreate_clause
        | deviceptr_clause
        | private_clause
        | firstprivate_clause
        | default_none_clause
        ;

kernels_directive
        : ACC KERNELS {
          $$ = acc_x_directive(e_acc_kernels, NULL, acc_x_location);
        }
        | ACC KERNELS kernels_clause_seq {
          $$ = acc_x_directive(e_acc_kernels, $3, acc_x_location);
        }
        ;

kernels_clause_seq
        : kernels_clauses {
          $$ = new std::vector<SgAccClause*>(1, $1);
        }
        | kernels_clause_seq kernels_clauses {
          $1->push_back($2);
          $$ = $1;
        }
        | kernels_clause_seq ',' kernels_clauses {
          $1->push_back($3);
          $$ = $1;
        }
        ;

kernels_clauses
        : async_clause
        | wait_clause
        | dtype_clause
        | if_clause
        | copy_clause
        | copyin_clause
        | copyout_clause
        | create_clause
        | present_clause
        | pcopy_clause
        | pcopyin_clause
        | pcopyout_clause
        | pcreate_clause
        | deviceptr_clause
        | default_none_clause
        ;

data_directive
        : ACC DATA {
          $$ = acc_x_directive(e_acc_data, NULL, acc_x_location);
        }
        | ACC DATA data_clause_seq {
          $$ = acc_x_directive(e_acc_data, $3, acc_x_location);
        }
        ;

data_clause_seq
        : data_clauses {
          $$ = new std::vector<SgAccClause*>(1, $1);
        }
        | data_clause_seq data_clauses {
          $1->push_back($2);
          $$ = $1;
        }
        | data_clause_seq ',' data_clauses {
          $1->push_back($3);
          $$ = $1;
        }
        ;

data_clauses
        : if_clause
        | copy_clause
        | copyin_clause
        | copyout_clause
        | create_clause
        | present_clause
        | pcopy_clause
        | pcopyin_clause
        | pcopyout_clause
        | pcreate_clause
        | deviceptr_clause
        ;

enter_data_directive
        : ACC ENTER DATA enter_data_clause_seq {
          $$ = acc_x_directive(e_acc_enter_data, $4, acc_x_location);
        }
        ;

enter_data_clause_seq
        : enter_data_clauses {
          $$ = new std::vector<SgAccClause*>(1, $1);
        }
        | enter_data_clause_seq enter_data_clauses {
          $1->push_back($2);
          $$ = $1;
        }
        | enter_data_clause_seq ',' enter_data_clauses {
          $1->push_back($3);
          $$ = $1;
        }
        ;

enter_data_clauses
        : if_clause
        | async_clause
        | wait_clause
        | copyin_clause
        | create_clause
        | pcopyin_clause
        | pcreate_clause
        ;

exit_data_directive
        : ACC EXIT DATA exit_data_clause_seq {
          $$ = acc_x_directive(e_acc_exit_data, $4, acc_x_location);
        }
        ;

exit_data_clause_seq
        : exit_data_clauses {
          $$ = new std::vector<SgAccClause*>(1, $1);
        }
        | exit_data_clause_seq exit_data_clauses {
          $1->push_back($2);
          $$ = $1;
        }
        | exit_data_clause_seq ',' exit_data_clauses {
          $1->push_back($3);
          $$ = $1;
        }
        ;

exit_data_clauses
        : if_clause
        | async_clause
        | wait_clause
        | copyout_clause
        | delete_clause
        ;

host_data_directive
        : ACC HOST_DATA host_data_clause_seq {
          $$ = acc_x_directive(e_acc_host_data, $3, acc_x_location);
        }
        ;

host_data_clause_seq
        : host_data_clauses {
          $$ = new std::vector<SgAccClause*>(1, $1);
        }
        | host_data_clause_seq host_data_clauses {
          $1->push_back($2);
          $$ = $1;
        }
        | host_data_clause_seq ',' host_data_clauses {
          $1->push_back($3);
          $$ = $1;
        }
        ;

host_data_clauses
        : use_device_clause
        ;

loop_directive
        : ACC LOOP {
          $$ = acc_x_directive(e_acc_loop, NULL, acc_x_location);
        }
        | ACC LOOP loop_clause_seq {
          $$ = acc_x_directive(e_acc_loop, $3, acc_x_location);
        }
        ;

loop_clause_seq
        : loop_clauses {
          $$ = new std::vector<SgAccClause*>(1, $1);
        }
        | loop_clause_seq loop_clauses {
          $1->push_back($2);
          $$ = $1;
        }
        | loop_clause_seq ',' loop_clauses {
          $1->push_back($3);
          $$ = $1;
        }
        ;

loop_clauses
        : collapse_clause
        | gang_clause
        | worker_clause
        | vector_clause
        | seq_clause
        | auto_clause
        | tile_clause
        | dtype_clause
        | independent_clause
        | private_clause
        | reduction_clause
        ;

cache_directive
        : ACC CACHE '(' var_list ')' {
          SgAccClause*
            c = acc_x_clause(e_acc_c_cache_arguments, $4, acc_x_location);
          std::vector<SgAccClause*>*
            cc = new std::vector<SgAccClause*>(1, c);
          $$ = acc_x_directive(e_acc_cache, cc, acc_x_location);
        }
        ;

parallel_loop_directive
        : ACC PARALLEL LOOP {
          $$ = acc_x_directive(e_acc_parallel_loop, NULL, acc_x_location);
        }
        | ACC PARALLEL LOOP parallel_loop_clause_seq {
          $$ = acc_x_directive(e_acc_parallel_loop, $4, acc_x_location);
        }
        ;

parallel_loop_clause_seq
        : parallel_loop_clauses {
          $$ = new std::vector<SgAccClause*>(1, $1);
        }
        | parallel_loop_clause_seq parallel_loop_clauses {
          $1->push_back($2);
          $$ = $1;
        }
        | parallel_loop_clause_seq ',' parallel_loop_clauses {
          $1->push_back($3);
          $$ = $1;
        }
        ;

parallel_loop_clauses
        : collapse_clause
        | gang_clause
        | worker_clause
        | vector_clause
        | seq_clause
        | auto_clause
        | tile_clause
        | dtype_clause
        | independent_clause
        | private_clause
        | reduction_clause
        | async_clause
        | wait_clause
        | num_gangs_clause
        | num_workers_clause
        | vector_length_clause
        | if_clause
        | copy_clause
        | copyin_clause
        | copyout_clause
        | create_clause
        | present_clause
        | pcopy_clause
        | pcopyin_clause
        | pcopyout_clause
        | pcreate_clause
        | deviceptr_clause
        | firstprivate_clause
        | default_none_clause
        ;

kernels_loop_directive
        : ACC KERNELS LOOP {
          $$ = acc_x_directive(e_acc_kernels_loop, NULL, acc_x_location);
        }
        | ACC KERNELS LOOP kernels_loop_clause_seq {
          $$ = acc_x_directive(e_acc_kernels_loop, $4, acc_x_location);
        }
        ;

kernels_loop_clause_seq
        : kernels_loop_clauses {
          $$ = new std::vector<SgAccClause*>(1, $1);
        }
        | kernels_loop_clause_seq kernels_loop_clauses {
          $1->push_back($2);
          $$ = $1;
        }
        | kernels_loop_clause_seq ',' kernels_loop_clauses {
          $1->push_back($3);
          $$ = $1;
        }
        ;

kernels_loop_clauses
        : collapse_clause
        | gang_clause
        | worker_clause
        | vector_clause
        | seq_clause
        | auto_clause
        | tile_clause
        | dtype_clause
        | independent_clause
        | private_clause
        | reduction_clause
        | async_clause
        | wait_clause
        | if_clause
        | copy_clause
        | copyin_clause
        | copyout_clause
        | create_clause
        | present_clause
        | pcopy_clause
        | pcopyin_clause
        | pcopyout_clause
        | pcreate_clause
        | deviceptr_clause
        | default_none_clause
        ;

atomic_directive
        : ACC ATOMIC {
          $$ = acc_x_directive(e_acc_atomic_update, NULL, acc_x_location);
        }
        | ACC ATOMIC UPDATE {
          $$ = acc_x_directive(e_acc_atomic_update, NULL, acc_x_location);
        }
        | ACC ATOMIC READ {
          $$ = acc_x_directive(e_acc_atomic_read, NULL, acc_x_location);
        }
        | ACC ATOMIC WRITE {
          $$ = acc_x_directive(e_acc_atomic_write, NULL, acc_x_location);
        }
        | ACC ATOMIC CAPTURE {
          $$ = acc_x_directive(e_acc_atomic_capture, NULL, acc_x_location);
        }
        ;

declare_directive
        : ACC DECLARE declare_clause_seq {
          $$ = acc_x_directive(e_acc_declare, $3, acc_x_location);
        }
        ;

declare_clause_seq
        : declare_clauses {
          $$ = new std::vector<SgAccClause*>(1, $1);
        }
        | declare_clause_seq declare_clauses {
          $1->push_back($2);
          $$ = $1;
        }
        | declare_clause_seq ',' declare_clauses {
          $1->push_back($3);
          $$ = $1;
        }
        ;

declare_clauses
        : copy_clause
        | copyin_clause
        | copyout_clause
        | create_clause
        | present_clause
        | pcopy_clause
        | pcopyin_clause
        | pcopyout_clause
        | pcreate_clause
        | deviceptr_clause
        | device_resident_clause
        | link_clause
        ;

/* Executable Directives */

update_directive
        : ACC UPDATE update_clause_seq {
          $$ = acc_x_directive(e_acc_update, $3, acc_x_location);
        }
        ;

update_clause_seq
        : update_clauses {
          $$ = new std::vector<SgAccClause*>(1, $1);
        }
        | update_clause_seq update_clauses {
          $1->push_back($2);
          $$ = $1;
        }
        | update_clause_seq ',' update_clauses {
          $1->push_back($3);
          $$ = $1;
        }
        ;

update_clauses
        : async_clause
        | wait_clause
        | dtype_clause
        | if_clause
        | self_clause
        | host_clause
        | device_clause
        ;

routine_directive
        : ACC ROUTINE routine_clause_seq {
          $$ = acc_x_directive(e_acc_routine, $3, acc_x_location);
        }
        | ACC ROUTINE '(' varref ')' routine_clause_seq {
          std::vector<SgExpression*>*
            ee = new std::vector<SgExpression*>(1, $4);
          SgAccClause*
            c0 = acc_x_clause(e_acc_c_routine_name, ee, acc_x_location);
          std::vector<SgAccClause*>* cc = $6;
          cc->insert(cc->begin(), c0);
          $$ = acc_x_directive(e_acc_routine, cc, acc_x_location);
        }
        ;

routine_clause_seq
        : routine_clauses {
          $$ = new std::vector<SgAccClause*>(1, $1);
        }
        | routine_clause_seq routine_clauses {
          $1->push_back($2);
          $$ = $1;
        }
        | routine_clause_seq ',' routine_clauses {
          $1->push_back($3);
          $$ = $1;
        }
        ;

routine_clauses
        : gang_clause
        | worker_clause
        | vector_clause
        | seq_clause
        | bind_clause
        | dtype_clause
        | nohost_clause
        ;

wait_directive
        : wait_directive_prefix {
          $$ = acc_x_directive(e_acc_wait, $1, acc_x_location);
        }
        | wait_directive_prefix wait_clause_seq {
          std::vector<SgAccClause*>* cc = $1;
          cc->insert(cc->begin(), ($2)->begin(), ($2)->end());
          $$ = acc_x_directive(e_acc_wait, cc, acc_x_location);
        }
        ;

wait_directive_prefix
        : ACC WAIT {
          $$ = new std::vector<SgAccClause*>();
        }
        | ACC WAIT '(' expr_list ')' {
          SgAccClause*
            c = acc_x_clause(e_acc_c_wait_arguments, $4, acc_x_location);
          $$ = new std::vector<SgAccClause*>(1, c);
        }
        ;

wait_clause_seq
        : wait_clauses {
          $$ = new std::vector<SgAccClause*>(1, $1);
        }
        | wait_clause_seq wait_clauses {
          $1->push_back($2);
          $$ = $1;
        }
        | wait_clause_seq ',' wait_clauses {
          $1->push_back($3);
          $$ = $1;
        }
        ;

wait_clauses
        : async_clause
        ;

/* Clauses */

dtype_clause
        : DTYPE '(' dtype_arg ')' {
          $$ = acc_x_clause(e_acc_c_dtype, $3, acc_x_location);
        }
        ;

if_clause
        : IF '(' expr ')' {
          std::vector<SgExpression*>*
            ee = new std::vector<SgExpression*>(1, $3);
          $$ = acc_x_clause(e_acc_c_if, ee, acc_x_location);
        }
        ;

async_clause
        : ASYNC {
          $$ = acc_x_clause(e_acc_c_async, NULL, acc_x_location);
        }
        | ASYNC '(' expr ')' {
          std::vector<SgExpression*>*
            ee = new std::vector<SgExpression*>(1, $3);
          $$ = acc_x_clause(e_acc_c_async, ee, acc_x_location);
        }
        ;

wait_clause
        : WAIT {
          $$ = acc_x_clause(e_acc_c_wait, NULL, acc_x_location);
        }
        | WAIT '(' expr_list ')' {
          $$ = acc_x_clause(e_acc_c_wait, $3, acc_x_location);
        }
        ;

num_gangs_clause
        : NUM_GANGS '(' expr ')' {
          std::vector<SgExpression*>*
            ee = new std::vector<SgExpression*>(1, $3);
          $$ = acc_x_clause(e_acc_c_num_gangs, ee, acc_x_location);
        }
        ;

num_workers_clause
        : NUM_WORKERS '(' expr ')' {
          std::vector<SgExpression*>*
            ee = new std::vector<SgExpression*>(1, $3);
          $$ = acc_x_clause(e_acc_c_num_workers, ee, acc_x_location);
        }
        ;

vector_length_clause
        : VECTOR_LENGTH '(' expr ')' {
          std::vector<SgExpression*>*
            ee = new std::vector<SgExpression*>(1, $3);
          $$ = acc_x_clause(e_acc_c_vector_length, ee, acc_x_location);
        }
        ;

private_clause
        : PRIVATE '(' var_list ')' {
          $$ = acc_x_clause(e_acc_c_private, $3, acc_x_location);
        }
        ;

firstprivate_clause
        : FIRSTPRIVATE '(' var_list ')' {
          $$= acc_x_clause(e_acc_c_firstprivate, $3, acc_x_location);
        }
        ;

reduction_clause
        : REDUCTION '(' reduction_operator ':' var_list ')' {
          SgExpression*
            op = new SgStringVal(acc_x_location, std::string($3));
          std::vector<SgExpression*> *ee = $5;
          ee->insert(ee->begin(), op);
          $$ = acc_x_clause(e_acc_c_reduction, ee, acc_x_location);
        }
        ;

default_none_clause
        : DEFAULT '(' NONE ')' {
          $$ = acc_x_clause(e_acc_c_default_none, NULL, acc_x_location);
        }
        ;

/* Data Clauses */

deviceptr_clause
        : DEVICEPTR '(' var_list ')' {
          $$ = acc_x_clause(e_acc_c_deviceptr, $3, acc_x_location);
        }
        ;

copy_clause
        : COPY '(' var_list ')' {
          $$ = acc_x_clause(e_acc_c_copy, $3, acc_x_location);
        }
        ;

copyin_clause
        : COPYIN '(' var_list ')' {
          $$ = acc_x_clause(e_acc_c_copyin, $3, acc_x_location);
        }
        ;

copyout_clause
        : COPYOUT '(' var_list ')' {
          $$ = acc_x_clause(e_acc_c_copyout, $3, acc_x_location);
        }
        ;

create_clause
        : CREATE '(' var_list ')' {
          $$ = acc_x_clause(e_acc_c_create, $3, acc_x_location);
        }
        ;

delete_clause
        : DELETE '(' var_list ')' {
          $$ = acc_x_clause(e_acc_c_delete, $3, acc_x_location);
        }
        ;

present_clause
        : PRESENT '(' var_list ')' {
          $$ = acc_x_clause(e_acc_c_present, $3, acc_x_location);
        }
        ;

pcopy_clause
        : PCOPY '(' var_list ')' {
          $$ = acc_x_clause(e_acc_c_pcopy, $3, acc_x_location);
        }
        ;

pcopyin_clause
        : PCOPYIN '(' var_list ')' {
          $$ = acc_x_clause(e_acc_c_pcopyin, $3, acc_x_location);
        }
        ;

pcopyout_clause
        : PCOPYOUT '(' var_list ')' {
          $$ = acc_x_clause(e_acc_c_pcopyout, $3, acc_x_location);
        }
        ;

pcreate_clause
        : PCREATE '(' var_list ')' {
          $$ = acc_x_clause(e_acc_c_pcreate, $3, acc_x_location);
        }
        ;

use_device_clause
        : USE_DEVICE '(' var_list ')' {
          $$ = acc_x_clause(e_acc_c_use_device, $3, acc_x_location);
        }
        ;

/* Loop Clauses */

collapse_clause
        : COLLAPSE '(' expr ')' {
          std::vector<SgExpression*>*
            ee = new std::vector<SgExpression*>(1, $3);
          $$ = acc_x_clause(e_acc_c_collapse, ee, acc_x_location);
        }
        ;

gang_clause
        : GANG {
          $$ = acc_x_clause(e_acc_c_gang, NULL, acc_x_location);
        }
        | GANG '(' gang_arg_list ')' {
          $$ = acc_x_clause(e_acc_c_gang, $3, acc_x_location);
        }
        ;

worker_clause
        : WORKER {
          $$ = acc_x_clause(e_acc_c_worker, NULL, acc_x_location);
        }
        | WORKER '(' expr ')' {
          std::vector<SgExpression*>*
            ee = new std::vector<SgExpression*>(1, $3);
          $$ = acc_x_clause(e_acc_c_worker, ee, acc_x_location);
        }
        | WORKER '(' NUM ':' expr ')' {
          std::vector<SgExpression*>*
            ee = new std::vector<SgExpression*>(1, $5);
          $$ = acc_x_clause(e_acc_c_worker, ee, acc_x_location);
        }
        ;

vector_clause
        : VECTOR {
          $$ = acc_x_clause(e_acc_c_vector, NULL, acc_x_location);
        }
        | VECTOR '(' expr ')' {
          std::vector<SgExpression*>*
            ee = new std::vector<SgExpression*>(1, $3);
          $$ = acc_x_clause(e_acc_c_vector, ee, acc_x_location);
        }
        | VECTOR '(' LENGTH ':' expr ')' {
          std::vector<SgExpression*>*
            ee = new std::vector<SgExpression*>(1, $5);
          $$ = acc_x_clause(e_acc_c_vector, ee, acc_x_location);
        }
        ;

seq_clause
        : SEQ {
          $$ = acc_x_clause(e_acc_c_seq, NULL, acc_x_location);
        }
        ;

auto_clause
        : AUTO {
          $$ = acc_x_clause(e_acc_c_auto, NULL, acc_x_location);
        }
        ;

tile_clause
        : TILE '(' size_expr_list ')' {
          $$ = acc_x_clause(e_acc_c_tile, $3, acc_x_location);
        }
        ;

independent_clause
        : INDEPENDENT {
          $$ = acc_x_clause(e_acc_c_independent, NULL, acc_x_location);
        }
        ;

device_resident_clause
        : DEVICE_RESIDENT '(' var_list ')' {
          $$ = acc_x_clause(e_acc_c_device_resident, $3, acc_x_location);
        }
        ;

link_clause
        : LINK '(' var_list ')' {
          $$ = acc_x_clause(e_acc_c_link, $3, acc_x_location);
        }
        ;

self_clause
        : SELF '(' var_list ')' {
          $$ = acc_x_clause(e_acc_c_self, $3, acc_x_location);
        }
        ;

host_clause
        : HOST '(' var_list ')' {
          $$ = acc_x_clause(e_acc_c_host, $3, acc_x_location);
        }
        ;

device_clause
        : DEVICE '(' var_list ')' {
          $$ = acc_x_clause(e_acc_c_device, $3, acc_x_location);
        }
        ;

bind_clause
        : BIND '(' varref ')' {
          std::vector<SgExpression*>*
            ee = new std::vector<SgExpression*>(1, $3);
          $$ = acc_x_clause(e_acc_c_device, ee, acc_x_location);
        }
        | BIND '(' STRING ')' {
          std::string s0 = $3;
          SgExpression* s = acc_f_scan_string(s0, acc_x_location);
          std::vector<SgExpression*>*
            ee = new std::vector<SgExpression*>(1, s);
          delete $3;
          $$ = acc_x_clause(e_acc_c_device, ee, acc_x_location);
        }
        ;

nohost_clause
        : NOHOST {
          $$ = acc_x_clause(e_acc_c_nohost, NULL, acc_x_location);
        }
        ;

reduction_operator
        : '+' {
          $$ = "plus";
        }
        | '*' {
          $$ = "times";
        }
        | MAX {
          $$ = "max";
        }
        | MIN {
          $$ = "min";
        }
        | IAND {
          $$ = "bitand";
        }
        | IOR {
          $$ = "bitior";
        }
        | IEOR {
          $$ = "bitxor";
        }
        | _AND_ {
          $$ = "logand";
        }
        | _OR_ {
          $$ = "logior";
        }
        | _EQV_ {
          $$ = "eqv";
        }
        | _NEQV_ {
          $$ = "neqv";
        }
        ;

size_expr_list
        : size_expr {
          $$ = new std::vector<SgExpression*>(1, $1);
        }
        | size_expr_list ',' size_expr {
          $1->push_back($3);
          $$ = $1;
        }
        ;

size_expr
        : '*' {
          $$ = new SgStringVal(acc_x_location, std::string("*"));
        }
        | expr
        ;

gang_arg_list
        : expr {
          std::vector<SgExpression*>* ee = new std::vector<SgExpression*>(2);
          ee->at(0) = $1;
          ee->at(1) = NULL;
          $$ = ee;
        }
        | NUM ':' expr {
          std::vector<SgExpression*>* ee = new std::vector<SgExpression*>(2);
          ee->at(0) = $3;
          ee->at(1) = NULL;
          $$ = ee;
        }
        | expr ',' STATIC ':' size_expr {
          std::vector<SgExpression*>* ee = new std::vector<SgExpression*>(2);
          ee->at(0) = $1;
          ee->at(1) = $5;
          $$ = ee;
        }
        | NUM ':' expr ',' STATIC ':' size_expr {
          std::vector<SgExpression*>* ee = new std::vector<SgExpression*>(2);
          ee->at(0) = $3;
          ee->at(1) = $7;
          $$ = ee;
        }
        | STATIC ':' size_expr {
          std::vector<SgExpression*>* ee = new std::vector<SgExpression*>(2);
          ee->at(0) = NULL;
          ee->at(1) = $3;
          $$ = ee;
        }
        | STATIC ':' size_expr ',' expr {
          std::vector<SgExpression*>* ee = new std::vector<SgExpression*>(2);
          ee->at(0) = $5;
          ee->at(1) = $3;
          $$ = ee;
        }
        | STATIC ':' size_expr ',' NUM ':' expr {
          std::vector<SgExpression*>* ee = new std::vector<SgExpression*>(2);
          ee->at(0) = $7;
          ee->at(1) = $3;
          $$ = ee;
        }
        ;

dtype_arg
        : '*' {
          SgExpression*
            e = new SgStringVal(acc_x_location, std::string("*"));
          $$ = new std::vector<SgExpression*>(1, e);
        }
        | dtype_list
        ;

dtype_list
        : dtype {
          $$ = new std::vector<SgExpression*>(1, $1);
        }
        | dtype_list ',' dtype {
          $1->push_back($3);
          $$ = $1;
        }
        ;

dtype
        : varid {
          $$ = new SgStringVal(acc_x_location, std::string($1));
        }
        ;

expr_list
        : expr {
          $$ = new std::vector<SgExpression*>(1, $1);
        }
        | expr_list ',' expr {
          $1->push_back($3);
          $$ = $1;
        }
        ;

/* Expressions etc. */

expr : expr_R722;

// (F2008 RULES)

expr_R722
        : level_5_expr_R717
        //| expr_R722 defined_binary_op_R723 level_5_expr_R717
        ;

//defined_binary_op_R723 : DOT_OP_DOT;

level_5_expr_R717
        : equiv_operand_R716
        | level_5_expr_R717 _EQV_ equiv_operand_R716 {
          SgExpression* e = SageBuilder::buildEqualityOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | level_5_expr_R717 _NEQV_ equiv_operand_R716 {
          SgExpression* e = SageBuilder::buildNotEqualOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        ;

equiv_operand_R716
        : or_operand_R715
        | equiv_operand_R716 _OR_ or_operand_R715 {
          SgExpression* e = SageBuilder::buildOrOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        ;

or_operand_R715
        : and_operand_R714
        | or_operand_R715 _AND_ and_operand_R714 {
          SgExpression* e = SageBuilder::buildAndOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        ;

and_operand_R714
        : level_4_expr_R712
        | _NOT_ level_4_expr_R712 {
          SgExpression* e = SageBuilder::buildNotOp($2);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        ;

level_4_expr_R712
        : level_3_expr_R710
        | level_4_expr_R712 _EQ_ level_3_expr_R710 {
          SgExpression* e = SageBuilder::buildEqualityOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | level_4_expr_R712 _NE_ level_3_expr_R710 {
          SgExpression* e = SageBuilder::buildNotEqualOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | level_4_expr_R712 _LT_ level_3_expr_R710 {
          SgExpression* e = SageBuilder::buildLessThanOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | level_4_expr_R712 _LE_ level_3_expr_R710 {
          SgExpression* e = SageBuilder::buildLessOrEqualOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | level_4_expr_R712 _GT_ level_3_expr_R710 {
          SgExpression* e = SageBuilder::buildGreaterThanOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | level_4_expr_R712 _GE_ level_3_expr_R710 {
          SgExpression* e = SageBuilder::buildGreaterOrEqualOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | level_4_expr_R712 EQV level_3_expr_R710 {
          SgExpression* e = SageBuilder::buildEqualityOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | level_4_expr_R712 NEQV level_3_expr_R710 {
          SgExpression* e = SageBuilder::buildNotEqualOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | level_4_expr_R712 '<' level_3_expr_R710 {
          SgExpression* e = SageBuilder::buildLessThanOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | level_4_expr_R712 LE level_3_expr_R710 {
          SgExpression* e = SageBuilder::buildLessOrEqualOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | level_4_expr_R712 '>' level_3_expr_R710 {
          SgExpression* e = SageBuilder::buildGreaterThanOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | level_4_expr_R712 GE level_3_expr_R710 {
          SgExpression* e = SageBuilder::buildGreaterOrEqualOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        ;

level_3_expr_R710
        : level_2_expr_R706
        | level_3_expr_R710 CONCAT level_2_expr_R706 {
          SgExpression* e = SageBuilder::buildConcatenationOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        ;

level_2_expr_R706
        : add_operand_R705
        | '+' add_operand_R705 {
          SgExpression* e = SageBuilder::buildUnaryAddOp($2);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | '-' add_operand_R705 {
          SgExpression* e = SageBuilder::buildMinusOp($2);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | level_2_expr_R706 '+' add_operand_R705 {
          SgExpression* e = SageBuilder::buildAddOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | level_2_expr_R706 '-' add_operand_R705 {
          SgExpression* e = SageBuilder::buildSubtractOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        ;

add_operand_R705
        : mult_operand_R704
        | add_operand_R705 '*' mult_operand_R704 {
          SgExpression* e = SageBuilder::buildMultiplyOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | add_operand_R705 '/' mult_operand_R704 {
          SgExpression* e = SageBuilder::buildDivideOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        ;

mult_operand_R704
        : level_1_expr_R702
        | level_1_expr_R702 POWER mult_operand_R704 {
          SgExpression* e = SageBuilder::buildExponentiationOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        ;

level_1_expr_R702
        : primary_R701
        //| defined_unary_op_R703 primary_R701
        ;

//defined_unary_op_R703 : DOT_OP_DOT;

primary_R701
        : varref
        | varref '(' ')' {
          std::vector<SgExpression*>* v0 = new std::vector<SgExpression*>();
          $$ = acc_f_make_app($1, v0, acc_x_scope, acc_x_location);
        }
        | varref '(' subscript_or_argument_list ')' {
          $$ = acc_f_make_app($1, $3, acc_x_scope, acc_x_location);
        }
        | primary_R701 '%' varref {
          SgExpression* r = new SgDotExp($1, $3, NULL);
          acc_x_set_location(r, acc_x_location);
          acc_x_set_location(r, acc_x_location);
          $$ = r;
        }
        | primary_R701 '%' varref '(' ')' {
          SgExpression* r = new SgDotExp($1, $3, NULL);
          acc_x_set_location(r, acc_x_location);
          std::vector<SgExpression*>* v0 = new std::vector<SgExpression*>();
          $$ = acc_f_make_app(r, v0, acc_x_scope, acc_x_location);
        }
        | primary_R701 '%' varref '(' subscript_or_argument_list ')' {
          SgExpression* r = new SgDotExp($1, $3, NULL);
          acc_x_set_location(r, acc_x_location);
          $$ = acc_f_make_app(r, $5, acc_x_scope, acc_x_location);
        }
        | literal_constant_R305
        | '(' expr_R722 ')' {
          $$ = $2;
        }
        //| coindexed_named_object
        //| complex_part_designator
        //| substring_R608
        ;

literal_constant_R305
        : CONSTANTI {
          const char* s = $1;
          SgExpression* e = acc_f_parse_integer(s, acc_x_location);
          $$ = e;
        }
        | CONSTANTR {
          const char* s = $1;
          SgExpression* e = acc_f_parse_real(s, acc_x_location);
          $$ = e;
        }
        | '(' level_2_expr_R706 ',' level_2_expr_R706 ')' {
          SgValueExp* x0 = acc_f_fold_unary_plusminus($2);
          SgValueExp* x1 = acc_f_fold_unary_plusminus($4);
          SgExpression* e = SageBuilder::buildComplexVal(x0, x1);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | _TRUE_ {
          SgExpression* e = SageBuilder::buildBoolValExp(1);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | _FALSE_ {
          SgExpression* e = SageBuilder::buildBoolValExp(0);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | STRING {
          std::string s0 = $1;
          SgExpression* e = acc_f_scan_string(s0, acc_x_location);
          delete $1;
          $$ = e;
        }
        | CONSTANTBOZ {
          /*TODO*/
          ROSE_ASSERT(0);
          $$ = NULL;
        }
        ;

subscript_or_argument_list
        : subscript_or_argument_R620_R1222 {
          $$ = new std::vector<SgExpression*>(1, $1);
        }
        | subscript_or_argument_list ',' subscript_or_argument_R620_R1222 {
          $1->push_back($3);
          $$ = $1;
        }
        ;

/* MEMO: Merge of section_subscript_R620 and actual_arg_spec_R1222. */

subscript_or_argument_R620_R1222
        : expr_R722
        | varref '=' expr_R722 {
          /*TODO*/ /*What?*/
          $$ = $3;
        }
        | '*' label_R312 {
          /*TODO*/ /*What?*/
          $$ = NULL;
        }
        | triplet_R621
        ;

triplet_R621
        : ':' {
          $$ = acc_f_make_triplet(NULL, NULL, NULL, acc_x_location);
        }
        | ':' expr_R722 {
          $$ = acc_f_make_triplet(NULL, $2, NULL, acc_x_location);
        }
        | ':' ':' expr_R722 {
          $$ = acc_f_make_triplet(NULL, NULL, $3, acc_x_location);
        }
        | ':' expr_R722 ':' expr_R722 {
          $$ = acc_f_make_triplet(NULL, $2, $4, acc_x_location);
          }
        | expr_R722 ':' {
          $$ = acc_f_make_triplet($1, NULL, NULL, acc_x_location);
        }
        | expr_R722 ':' expr_R722 {
          $$ = acc_f_make_triplet($1, $3, NULL, acc_x_location);
        }
        | expr_R722 ':' ':' expr_R722 {
          $$ = acc_f_make_triplet($1, NULL, $4, acc_x_location);
        }
        | expr_R722 ':' expr_R722 ':' expr_R722 {
          $$ = acc_f_make_triplet($1, $3, $5, acc_x_location);
        }
        ;

label_R312 : DIGITS;

varref
        : varid {
          SgExpression*
            e = SageBuilder::buildVarRefExp(std::string($1), acc_x_scope);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        ;

var_list
        : subsections {
          $$ = new std::vector<SgExpression*>(1, $1);
        }
        | var_list ',' subsections {
          $1->push_back($3);
          $$ = $1;
        }
        ;

subsections
        : varref
        | varref '(' section_subscript_list ')' {
          /*SgExpression* e = SageBuilder::buildPntrArrRefExp(r, ax);*/
          SgExpression* r = $1;
          SgExprListExp* ax = SageBuilder::buildExprListExp(*$3);
          SgPntrArrRefExp* e = new SgPntrArrRefExp(r, ax, NULL);
          r->set_parent(e);
          ax->set_parent(e);
          /*e->set_file_info(acc_x_location);*/
          acc_x_set_location(e, acc_x_location);
          delete ($3);
          $$ = e;
        }
        ;

section_subscript_list
        : triplet_R621 {
          $$ = new std::vector<SgExpression*>(1, $1);
        }
        | section_subscript_list ',' triplet_R621 {
          $1->push_back($3);
          $$ = $1;
        }
        ;

        /* AUTO/DEFAULT/DELETE/IF/STATIC are keywords. */

varid
        : IDENTIFIER
        | ACC | END
        | PARALLEL | KERNELS | DATA | ENTER | EXIT | HOST_DATA | LOOP
        | DTYPE | ASYNC | WAIT | NUM_GANGS | NUM_WORKERS | VECTOR_LENGTH
        | PRIVATE | FIRSTPRIVATE | REDUCTION | NONE
        | DEVICEPTR | COPY | COPYIN | COPYOUT | CREATE | PRESENT
        | PCOPY | PCOPYIN | PCOPYOUT | PCREATE
        | USE_DEVICE
        | COLLAPSE | GANG | WORKER | VECTOR | SEQ | TILE | INDEPENDENT
        | DEVICE_RESIDENT | LINK | SELF | HOST | DEVICE | BIND | NOHOST
        | NUM | LENGTH
        | MAX | MIN | IAND | IOR | IEOR
        ;

%%

static int yyerror(const char *s) {
  printf("ACC: %s\n", s);
  ROSE_ASSERT(0);
  return 0;
}

SgAccDirective* acc_f_parsed_directive() {
  SgAccDirective* v = acc_x_current;
  acc_x_current = NULL;
  return v;
}

void acc_f_parser_init(SgPragmaDeclaration* d, SgStatement* n,
                       const char* line) {
  acc_x_lexer_init(line);
  acc_x_directive_node = d;
  acc_x_directive_next = n;
  acc_x_location = d->get_file_info();
  acc_x_current = NULL;
  acc_x_scope = d->get_scope();
}
