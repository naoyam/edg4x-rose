/* accCParser.yy -*-Mode: C++;-*- */
/* Copyright (C) 2014 RIKEN AICS (of modifications) */

// OpenACC (2.0) Parser.  This is derived from "ompparser.yy".  See
// comments of it.

/* Hitory of "ompparser.yy": */
/* Author: Markus Schordan, 2003 */
/* Modified by Christian Biesinger 2006 for OpenMP 2.0 */
/* Modified by Chunhua Liao for OpenMP 3.0 and connect to OmpAttribute, 2008 */

// NOTES: (0) Array sections are represented by SgSubscriptExpression
// but a pair actually is start-size, not start-end.  Unparsing works
// but semantics processing will fail.  (1) A GANG clause is
// represented as a length-of-two list of expressions, the first is
// for NUM, and the second is for STATIC, and either can be null.  (2)
// Existence of a NUM keyword in a WORKER clause or a LENGTH keyword
// in a VECTOR clause is not recorded.  (3) Reduction operators are
// represented by string expressions of their names.

// RESTRICTIONS: (0) Syntax of expressions are for C99, not C++.  (1)
// Rules for sizeof and cast expressions are dropped.  It is partly
// because it makes necessary LEX take a list of typedefed
// identifiers.  (2) Rules for compound literals are dropped.  (3)
// Some others are simply dropped.  (4) Location information is
// imprecise; all locations are at the beginning of directives.

// MEMO: (0) See "Acc.code" in "src/ROSETTA/Grammar" for the
// enumeration definitions of the types of directives and clauses.
// (1) Fix the source line after calling builders such as
// "SageBuilder::buildAssignOp", because source lines are not likely
// set properly.

%name-prefix="acc_c_"
%defines
%error-verbose

%{
#include <stdio.h>
#include <iostream>
#include <vector>
#include <assert.h>
#include "sage3basic.h"
#include "sageBuilder.h"
#include "accDirectives.h"

using namespace AccSupport;

#define acc_x_lexer_init acc_c_lexer_init
#define acc_x_directive acc_c_directive
#define acc_x_clause acc_c_clause
#define acc_x_directive_node acc_c_directive_node
#define acc_x_directive_next acc_c_directive_next
#define acc_x_location acc_c_location
#define acc_x_current acc_c_current
#define acc_x_scope acc_c_scope
#define acc_x_set_location acc_c_set_location

extern int acc_c_lex();
static int acc_c_error(const char*);

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

// Scans a real value.

static SgExpression* acc_c_parse_real(
  const char* s0, Sg_File_Info* location) {
  std::string s(s0);
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

// Scans an integer value.  It ignores size.

static SgExpression* acc_c_parse_integer(
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

// Scans string literals.  RESTRICTIONS: It leaves escaped characters
// intact.

static SgExpression* acc_c_scan_string(
  std::string& s0, Sg_File_Info* location) {
  assert(s0.size() >= 2);
  std::string s1 = s0.substr(1, s0.size() - 2);
  assert(s1.find('\\') == std::string::npos);
  SgExpression* e = new SgStringVal(location, s1);
  return e;
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

%token <string> ACC
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
%token <string> MAX MIN

%token <string> SIZEOF
%token <string> '=' '(' ')' '[' ']' ',' ':' '?'
%token <string> '+' '-' '*' '/' '%' '&' '|' '^'
%token <string> AND IOR LSH RSH INCR DECR
%token <string> ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN
%token <string> AND_ASSIGN IOR_ASSIGN XOR_ASSIGN LSH_ASSIGN RSH_ASSIGN
%token <string> '<' '>' LE GE EQ NE
%token <string> '.' SREF

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
%type <vec_expr> expr_list var_list subsec_list
%type <vec_expr> gang_arg_list

%type <expr> expr expression assignment_expression conditional_expression
%type <expr> logical_or_expression logical_and_expression
%type <expr> ior_expression xor_expression and_expression
%type <expr> equality_expression relational_expression shift_expression
%type <expr> additive_expression multiplicative_expression primary_expression
%type <expr> cast_expression unary_expression postfix_expression
%type <vec_expr> argument_expression_list
%type <expr> subsec subsections
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
          SgExpression* s = acc_c_scan_string(s0, acc_x_location);
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
        | '&' {
          $$ = "bitand";
        }
        | '|' {
          $$ = "bitior";
        }
        | '^' {
          $$ = "bitxor";
        }
        | AND {
          $$ = "logand";
        }
        | IOR {
          $$ = "logior";
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
        : argument_expression_list
        ;

        /* Do not allow commas in "expr". */

expr
        : assignment_expression
        ;

/* Expressions, etc. */

expression
        : assignment_expression
        | expression ',' assignment_expression {
          SgExpression* e = SageBuilder::buildCommaOpExp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        ;

assignment_expression
        : conditional_expression
        | unary_expression '=' assignment_expression {
          SgExpression* e = SageBuilder::buildAssignOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | unary_expression ADD_ASSIGN assignment_expression {
          SgExpression* e = SageBuilder::buildPlusAssignOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | unary_expression SUB_ASSIGN assignment_expression {
          SgExpression* e = SageBuilder::buildMinusAssignOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | unary_expression MUL_ASSIGN assignment_expression {
          SgExpression* e = SageBuilder::buildMultAssignOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | unary_expression DIV_ASSIGN assignment_expression {
          SgExpression* e = SageBuilder::buildDivAssignOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | unary_expression MOD_ASSIGN assignment_expression {
          SgExpression* e = SageBuilder::buildModAssignOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | unary_expression AND_ASSIGN assignment_expression {
          SgExpression* e = SageBuilder::buildAndAssignOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | unary_expression IOR_ASSIGN assignment_expression {
          SgExpression* e = SageBuilder::buildIorAssignOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | unary_expression XOR_ASSIGN assignment_expression {
          SgExpression* e = SageBuilder::buildXorAssignOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | unary_expression LSH_ASSIGN assignment_expression {
          SgExpression* e = SageBuilder::buildLshiftAssignOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | unary_expression RSH_ASSIGN assignment_expression {
          SgExpression* e = SageBuilder::buildRshiftAssignOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        ;

conditional_expression
        : logical_or_expression
        | logical_or_expression '?' expression ':' assignment_expression {
          SgExpression* e = SageBuilder::buildConditionalExp($1, $3, $5);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        ;

logical_or_expression
        : logical_and_expression
        | logical_or_expression IOR logical_and_expression {
          SgExpression* e = SageBuilder::buildOrOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        ;

logical_and_expression
        : ior_expression
        | logical_and_expression AND ior_expression {
          SgExpression* e = SageBuilder::buildAndOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        ;

ior_expression
        : xor_expression
        | ior_expression '|' xor_expression {
          SgExpression* e = SageBuilder::buildBitOrOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        ;

xor_expression
        : and_expression
        | xor_expression '^' and_expression {
          SgExpression* e = SageBuilder::buildBitXorOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        ;

and_expression
        : equality_expression
        | and_expression '&' equality_expression {
          SgExpression* e = SageBuilder::buildBitAndOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        ;

equality_expression
        : relational_expression
        | equality_expression EQ relational_expression {
          SgExpression* e = SageBuilder::buildEqualityOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | equality_expression NE relational_expression {
          SgExpression* e = SageBuilder::buildNotEqualOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        ;

relational_expression
        : shift_expression
        | relational_expression '<' shift_expression {
          SgExpression* e = SageBuilder::buildLessThanOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | relational_expression '>' shift_expression {
          SgExpression* e = SageBuilder::buildGreaterThanOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | relational_expression LE shift_expression {
          SgExpression* e = SageBuilder::buildLessOrEqualOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | relational_expression GE shift_expression {
          SgExpression* e = SageBuilder::buildGreaterOrEqualOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        ;

shift_expression
        : additive_expression
        | shift_expression LSH additive_expression {
          SgExpression* e = SageBuilder::buildLshiftOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | shift_expression RSH additive_expression {
          SgExpression* e = SageBuilder::buildRshiftOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        ;

additive_expression
        : multiplicative_expression
        | additive_expression '+' multiplicative_expression {
          SgExpression* e = SageBuilder::buildAddOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | additive_expression '-' multiplicative_expression {
          SgExpression* e = SageBuilder::buildSubtractOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        ;

multiplicative_expression
        : cast_expression
        | multiplicative_expression '*' cast_expression {
          SgExpression* e = SageBuilder::buildMultiplyOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | multiplicative_expression '/' cast_expression {
          SgExpression* e = SageBuilder::buildDivideOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | multiplicative_expression '%' cast_expression {
          SgExpression* e = SageBuilder::buildModOp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        ;

cast_expression
        : unary_expression
          /*| ( type_name ) cast_expression*/
        ;

unary_expression
        : postfix_expression
        | '+' unary_expression {
          SgExpression* e = SageBuilder::buildUnaryAddOp($2);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | '-' unary_expression {
          SgExpression* e = SageBuilder::buildMinusOp($2);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | '!' unary_expression {
          SgExpression* e = SageBuilder::buildNotOp($2);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | '&' unary_expression {
          SgExpression* e = SageBuilder::buildAddressOfOp($2);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | '*' unary_expression {
          SgExpression* e = SageBuilder::buildPointerDerefExp($2);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | '~' unary_expression {
          SgExpression* e = SageBuilder::buildBitComplementOp($2);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | INCR unary_expression {
          SgExpression*
            e = SageBuilder::buildPlusPlusOp($2, SgUnaryOp::prefix);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | DECR unary_expression {
          SgExpression*
            e = SageBuilder::buildMinusMinusOp($2, SgUnaryOp::prefix);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | SIZEOF unary_expression {
          SgExpression*
            e = SageBuilder::buildSizeOfOp($2);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        /*| sizeof '(' type_name ')'*/
        ;

postfix_expression
        : primary_expression
        | postfix_expression '[' expression ']' {
          SgExpression*
            e = SageBuilder::buildPntrArrRefExp($1, $3);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | postfix_expression '(' ')' {
          SgExpression*
            e = SageBuilder::buildFunctionCallExp($1, NULL);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | postfix_expression '(' argument_expression_list ')' {
          SgExprListExp* a = SageBuilder::buildExprListExp(*$3);
          SgExpression*
           e = SageBuilder::buildFunctionCallExp($1, a);
          acc_x_set_location(e, acc_x_location);
          delete $3;
          $$ = e;
        }
        | postfix_expression '.' varid {
          SgExpression*
            slot = SageBuilder::buildVarRefExp(std::string($3), acc_x_scope);
          acc_x_set_location(slot, acc_x_location);
          SgExpression*
            e = SageBuilder::buildDotExp($1, slot);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | postfix_expression SREF varid {
          SgExpression*
            slot = SageBuilder::buildVarRefExp(std::string($3), acc_x_scope);
          acc_x_set_location(slot, acc_x_location);
          SgExpression*
            e = SageBuilder::buildArrowExp($1, slot);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | postfix_expression INCR {
          SgExpression*
            e = SageBuilder::buildPlusPlusOp($1, SgUnaryOp::postfix);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | postfix_expression DECR {
          SgExpression*
            e = SageBuilder::buildMinusMinusOp($1, SgUnaryOp::postfix);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        /*|( type_name ) { initializer_list }*/
        /*|( type_name ) { initializer_list , }*/
        ;

argument_expression_list
        : assignment_expression {
          $$ = new std::vector<SgExpression*>(1, $1);
        }
        | argument_expression_list ',' assignment_expression {
          $1->push_back($3);
          $$ = $1;
        }
        ;

primary_expression
        : varref
        | CONSTANTI {
          const char* s = $1;
          SgExpression* e = acc_c_parse_integer(s, acc_x_location);
          $$ = e;
        }
	| CONSTANTR {
          const char* s = $1;
          SgExpression* e = acc_c_parse_real(s, acc_x_location);
          $$ = e;
        }
        | STRING {
          std::string s0 = $1;
          SgExpression* s = acc_c_scan_string(s0, acc_x_location);
          delete $1;
          $$ = s;
        }
        | '(' expression ')' {
          $$ = $2;
        }
        ;

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
        | varref subsec_list {
          std::vector<SgExpression*>* ax = $2;
          SgExpression* e = $1;
          for (std::vector<SgExpression*>::iterator
                 i = ax->begin(); i != ax->end(); i++) {
            SgPntrArrRefExp* e1 = new SgPntrArrRefExp(e, (*i), NULL);
            e->set_parent(e1);
            (*i)->set_parent(e1);
            acc_x_set_location(e1, acc_x_location);
            e = e1;
          }
          delete (ax);
          $$ = e;
        }
        ;

subsec_list
        : subsec {
          $$ = new std::vector<SgExpression*>(1, $1);
        }
        | subsec_list subsec {
          $1->push_back($2);
          $$ = $1;
        }
        ;

subsec
        : '[' ':' ']' {
          SgExpression* lb = SageBuilder::buildNullExpression();
          acc_x_set_location(lb, acc_x_location);
          SgExpression* ub = SageBuilder::buildNullExpression();
          acc_x_set_location(ub, acc_x_location);
          SgExpression* one = new SgIntVal(1, "1");
          acc_x_set_location(one, acc_x_location);
          SgExpression* e = new SgSubscriptExpression(lb, ub, one);
          one->set_parent(e);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | '[' expr ':' ']' {
          SgExpression* lb = $2;
          SgExpression* ub = SageBuilder::buildNullExpression();
          acc_x_set_location(ub, acc_x_location);
          SgExpression* one = new SgIntVal(1, "1");
          acc_x_set_location(one, acc_x_location);
          SgExpression* e = new SgSubscriptExpression(lb, ub, one);
          lb->set_parent(e);
          ub->set_parent(e);
          one->set_parent(e);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | '[' ':' expr ']' {
          SgExpression* lb = SageBuilder::buildNullExpression();
          acc_x_set_location(lb, acc_x_location);
          SgExpression* ub = $3;
          SgExpression* one = new SgIntVal(1, "1");
          acc_x_set_location(one, acc_x_location);
          SgExpression* e = new SgSubscriptExpression(lb, ub, one);
          lb->set_parent(e);
          ub->set_parent(e);
          one->set_parent(e);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        | '[' expr ':' expr ']' {
          SgExpression* lb = $2;
          SgExpression* ub = $4;
          SgExpression* one = new SgIntVal(1, "1");
          acc_x_set_location(one, acc_x_location);
          SgExpression* e = new SgSubscriptExpression(lb, ub, one);
          lb->set_parent(e);
          ub->set_parent(e);
          one->set_parent(e);
          acc_x_set_location(e, acc_x_location);
          $$ = e;
        }
        ;

        /* AUTO/DEFAULT/DELETE/IF/STATIC are keywords. */

varid
        : IDENTIFIER
        | ACC
        | PARALLEL | KERNELS | DATA | ENTER | EXIT | HOST_DATA | LOOP
        | DTYPE | ASYNC | WAIT | NUM_GANGS | NUM_WORKERS | VECTOR_LENGTH
        | PRIVATE | FIRSTPRIVATE | REDUCTION | NONE
        | DEVICEPTR | COPY | COPYIN | COPYOUT | CREATE | PRESENT
        | PCOPY | PCOPYIN | PCOPYOUT | PCREATE
        | USE_DEVICE
        | COLLAPSE | GANG | WORKER | VECTOR | SEQ | TILE | INDEPENDENT
        | DEVICE_RESIDENT | LINK | SELF | HOST | DEVICE | BIND | NOHOST
        | NUM | LENGTH
        | MAX | MIN
        ;

%%

static int yyerror(const char *s) {
  printf("ACC: %s\n", s);
  ROSE_ASSERT(0);
  return 0;
}

SgAccDirective* acc_c_parsed_directive() {
  SgAccDirective* v = acc_x_current;
  acc_x_current = NULL;
  return v;
}

void acc_c_parser_init(SgPragmaDeclaration* d, SgStatement* n,
                       const char* line) {
  acc_x_lexer_init(line);
  acc_x_directive_node = d;
  acc_x_directive_next = n;
  acc_x_location = d->get_file_info();
  acc_x_current = NULL;
  acc_x_scope = d->get_scope();
}
