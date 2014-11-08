/* accFLexer.ll -*-Mode: C++;-*- */
/* Copyright (C) 2014 RIKEN AICS (of modifications) */

/* OpenACC (2.0) Parser. */

/* This is derived from "omplexer.ll".  See comments of it.  Makefile
   needs a fixed name as an output file.  */

/* RESTRICTIONS: (0) No quotes in strings are embedded by two
   consecutive quotes.  It does not scan escaped characters.  Strings
   are downcased. */

%option prefix="acc_f_"
%option outfile="lex.yy.c"
%option stack

%{

extern "C" int acc_f_wrap() {return 1;}

extern int acc_f_lex();

#include <stdio.h>
#include <string>
#include <iostream>
#include <assert.h>
#include "sage3basic.h"
#include "sageBuilder.h"
#include "accDirectives.h"
#include "accFParser.h"

using namespace AccSupport;

#define YY_NO_TOP_STATE
#define YY_NO_POP_STATE

static const char* acc_f_input_string = NULL;

#define YY_INPUT(BUF, COUNT, MAXSIZE) { \
    if (*acc_f_input_string == '\0') \
      COUNT = 0; \
    else { \
      strncpy(BUF, acc_f_input_string, MAXSIZE); \
      BUF[MAXSIZE] = 0; \
      COUNT = strlen(BUF); \
      acc_f_input_string += COUNT; \
    } \
}

/* Copies a lexer text for buffering.  It keeps up to four texts of
   identifiers/literals, assuming they will be copied in the parser
   quickly. */

static void acc_f_copy_text() {
  static std::string acc_f_buf[4];
  static int acc_f_idx = 0;
  acc_f_buf[acc_f_idx] = std::string(yytext);
  acc_f_lval.string = acc_f_buf[acc_f_idx].c_str();
  acc_f_idx = ((acc_f_idx + 1) % 4);
}

#define COPYTEXT() acc_f_copy_text()
#define COPYSTRING() (acc_f_lval.string = strdup(yytext))

%}

blank		[ \t]
newline		[\n\r]
digit		[0-9]
hexdigit	[0-9a-f]

id		[a-z_][a-z0-9_$@]*

%%

	/* Directives */

acc		{COPYTEXT(); return ACC;}
end		{COPYTEXT(); return END;}

parallel	{COPYTEXT(); return PARALLEL;}
kernels		{COPYTEXT(); return KERNELS;}
data		{COPYTEXT(); return DATA;}
enter		{COPYTEXT(); return ENTER;}
exit		{COPYTEXT(); return EXIT;}
host_data	{COPYTEXT(); return HOST_DATA;}
loop		{COPYTEXT(); return LOOP;}
cache		{COPYTEXT(); return CACHE;}
atomic		{COPYTEXT(); return ATOMIC;}

read		{COPYTEXT(); return READ;}
write		{COPYTEXT(); return WRITE;}
update		{COPYTEXT(); return UPDATE;}
capture		{COPYTEXT(); return CAPTURE;}

declare		{COPYTEXT(); return DECLARE;}

routine		{COPYTEXT(); return ROUTINE;}
wait		{COPYTEXT(); return WAIT;}

	/* Clauses */

device_type	{COPYTEXT(); return DTYPE;}
dtype		{COPYTEXT(); return DTYPE;}

if		{COPYTEXT(); return IF;}
async		{COPYTEXT(); return ASYNC;}
num_gangs	{COPYTEXT(); return NUM_GANGS;}
num_workers	{COPYTEXT(); return NUM_WORKERS;}
vector_length	{COPYTEXT(); return VECTOR_LENGTH;}
private		{COPYTEXT(); return PRIVATE;}
firstprivate	{COPYTEXT(); return FIRSTPRIVATE;}
reduction	{COPYTEXT(); return REDUCTION;}
default		{COPYTEXT(); return DEFAULT;}
none		{COPYTEXT(); return NONE;}

	/* Data Clauses */

deviceptr	{COPYTEXT(); return DEVICEPTR;}
copy		{COPYTEXT(); return COPY;}
copyin		{COPYTEXT(); return COPYIN;}
copyout		{COPYTEXT(); return COPYOUT;}
create		{COPYTEXT(); return CREATE;}
delete		{COPYTEXT(); return DELETE;}
present		{COPYTEXT(); return PRESENT;}
present_or_copy	 	{COPYTEXT(); return PCOPY;}
pcopy			{COPYTEXT(); return PCOPY;}
present_or_copyin	{COPYTEXT(); return PCOPYIN;}
pcopyin			{COPYTEXT(); return PCOPYIN;}
present_or_copyout	{COPYTEXT(); return PCOPYOUT;}
pcopyout		{COPYTEXT(); return PCOPYOUT;}
present_or_create	{COPYTEXT(); return PCREATE;}
pcreate			{COPYTEXT(); return PCREATE;}

	/* Host_Data Clauses */

use_device	{COPYTEXT(); return USE_DEVICE;}

		/* Loop Clauses */

collapse	{COPYTEXT(); return COLLAPSE;}
gang		{COPYTEXT(); return GANG;}
worker		{COPYTEXT(); return WORKER;}
vector		{COPYTEXT(); return VECTOR;}
seq		{COPYTEXT(); return SEQ;}
auto		{COPYTEXT(); return AUTO;}
tile		{COPYTEXT(); return TILE;}
independent	{COPYTEXT(); return INDEPENDENT;}

device_resident {COPYTEXT(); return DEVICE_RESIDENT;}
link		{COPYTEXT(); return LINK;}
self		{COPYTEXT(); return SELF;}
host		{COPYTEXT(); return HOST;}
device		{COPYTEXT(); return DEVICE;}

bind		{COPYTEXT(); return BIND;}
nohost		{COPYTEXT(); return NOHOST;}

	/* Others */

num		{COPYTEXT(); return NUM;}
static		{COPYTEXT(); return STATIC;}
length		{COPYTEXT(); return LENGTH;}

max		{COPYTEXT(); return MAX;}
min		{COPYTEXT(); return MIN;}

"**"		{COPYTEXT(); return POWER;}
"*"		{COPYTEXT(); return ('*');}
"/"		{COPYTEXT(); return ('/');}
"+"		{COPYTEXT(); return ('+');}
"-"		{COPYTEXT(); return ('-');}
"//"		{COPYTEXT(); return CONCAT;}

".eq."		{COPYTEXT(); return _EQ_;}
".ne."		{COPYTEXT(); return _NE_;}
".lt."		{COPYTEXT(); return _LT_;}
".le."		{COPYTEXT(); return _LE_;}
".gt."		{COPYTEXT(); return _GT_;}
".ge."		{COPYTEXT(); return _GE_;}

"=="		{COPYTEXT(); return EQV;}
"/="		{COPYTEXT(); return NEQV;}
"<"		{COPYTEXT(); return ('<');}
"<="		{COPYTEXT(); return LE;}
">"		{COPYTEXT(); return ('>');}
">="		{COPYTEXT(); return GE;}

".not."		{COPYTEXT(); return _NOT_;}
".and."		{COPYTEXT(); return _AND_;}
".or."		{COPYTEXT(); return _OR_;}
".eqv."		{COPYTEXT(); return _EQV_;}
".neqv."	{COPYTEXT(); return _NEQV_;}

"="		{COPYTEXT(); return ('=');}
"("		{COPYTEXT(); return ('(');}
")"		{COPYTEXT(); return (')');}
":"		{COPYTEXT(); return (':');}
","		{COPYTEXT(); return (',');}
	/*"%"		{COPYTEXT(); return ('%');}*/

".true."	{COPYTEXT(); return _TRUE_;}
".false."	{COPYTEXT(); return _FALSE_;}

"."		{COPYTEXT(); return ('.');}

{digit}+		{COPYTEXT(); return CONSTANTI;}
{digit}+_{digit}+	{COPYTEXT(); return CONSTANTI;}
{digit}+_{id}		{COPYTEXT(); return CONSTANTI;}

b\'{digit}+\'		{COPYTEXT(); return CONSTANTBOZ;}
b\"{digit}+\"		{COPYTEXT(); return CONSTANTBOZ;}
o\'{digit}+\'		{COPYTEXT(); return CONSTANTBOZ;}
o\"{digit}+\"		{COPYTEXT(); return CONSTANTBOZ;}
z\'{hexdigit}+\'	{COPYTEXT(); return CONSTANTBOZ;}
z\"{hexdigit}+\"	{COPYTEXT(); return CONSTANTBOZ;}

{digit}+\.{digit}*			{COPYTEXT(); return CONSTANTR;}
\.{digit}+				{COPYTEXT(); return CONSTANTR;}
{digit}+\.{digit}*[ed][-+]?{digit}+	{COPYTEXT(); return CONSTANTR;}
\.{digit}+[ed][-+]?{digit}+		{COPYTEXT(); return CONSTANTR;}

{digit}+\.{digit}*_{digit}+		{COPYTEXT(); return CONSTANTR;}
\.{digit}+_{digit}+			{COPYTEXT(); return CONSTANTR;}
{digit}+\.{digit}*[ed][-+]?{digit}+_{digit}+ {COPYTEXT(); return CONSTANTR;}
\.{digit}+[ed][-+]?{digit}+_{digit}+	{COPYTEXT(); return CONSTANTR;}

{digit}+\.{digit}*_{id}			{COPYTEXT(); return CONSTANTR;}
\.{digit}+_{id}				{COPYTEXT(); return CONSTANTR;}
{digit}+\.{digit}*[ed][-+]?{digit}+_{id} {COPYTEXT(); return CONSTANTR;}
\.{digit}+[ed][-+]?{digit}+_{id}	{COPYTEXT(); return CONSTANTR;}

\'(''|\\.|[^\\'])*\'		{COPYSTRING(); return STRING;}
\"(""|\\.|[^\\"])*\"		{COPYSTRING(); return STRING;}
{digit}+_\'(''|\\.|[^\\'])*\'	{COPYSTRING(); return STRING;}
{digit}+_\"(""|\\.|[^\\"])*\"	{COPYSTRING(); return STRING;}
{id}_\'(''|\\.|[^\\'])*\'	{COPYSTRING(); return STRING;}
{id}_\"(""|\\.|[^\\"])*\"	{COPYSTRING(); return STRING;}

{id}		{COPYTEXT(); return IDENTIFIER;}

{blank}*	{}

"\\"		{std::cerr << "Bad backslash in ACC directives" << std::endl;
		 assert(0);}
{newline}	{std::cerr << "Bad newline in ACC directives" << std::endl;
		 assert(0);}

.		{std::cerr << "Bad character in ACC directives" << std::endl;
		 return LEXICALERROR;}

%%

void acc_f_lexer_init(const char* s) {
  acc_f_input_string = s;
  acc_f_restart(acc_f_in);
}
