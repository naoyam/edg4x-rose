/* accCLexer.ll -*-Mode: C++;-*- */
/* Copyright (C) 2014 RIKEN AICS (of modifications) */

/* OpenACC (2.0) Parser. */

/* This is derived from "omplexer.ll".  See comments of it.  Makefile
   needs a fixed name as an output file. */

/* RESTRICTIONS: (0) Only a small part of literal constants is
   incompletely supported: * No concatenation of adjacent strings. *
   No wide strings. */

%option prefix="acc_c_"
%option outfile="lex.yy.c"
%option stack

%{

#include <stdio.h>
#include <string>
#include <iostream>
#include <assert.h>
#include "sage3basic.h"
#include "sageBuilder.h"
#include "accDirectives.h"
#include "accCParser.h"

using namespace AccSupport;

#define YY_NO_TOP_STATE
#define YY_NO_POP_STATE

extern "C" int acc_c_wrap() {return 1;}

extern int acc_c_lex();

static const char* acc_c_input_string = NULL;

#define YY_INPUT(BUF, COUNT, MAXSIZE) { \
    if (*acc_c_input_string == '\0') \
      COUNT = 0; \
    else { \
      strncpy(BUF, acc_c_input_string, MAXSIZE); \
      BUF[MAXSIZE] = 0; \
      COUNT = strlen(BUF); \
      acc_c_input_string += COUNT; \
    } \
}

/* Copies a lexer text to a buffer.  It keeps up to four texts of
   identifiers/literals, assuming they will be copied in the parser
   quickly. */

static void acc_c_copy_text() {
  static std::string acc_c_buf[4];
  static int acc_c_idx = 0;
  acc_c_buf[acc_c_idx] = std::string(yytext);
  acc_c_lval.string = acc_c_buf[acc_c_idx].c_str();
  acc_c_idx = ((acc_c_idx + 1) % 4);
}

#define COPYTEXT() acc_c_copy_text()
#define COPYSTRING() (acc_c_lval.string = strdup(yytext))

%}

blank		[ \t]
newline		[\n\r]
digit		[0-9]

id		[a-zA-Z_][a-zA-Z0-9_]*

%%

	/* Directives */

acc		{COPYTEXT(); return ACC;}

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
present_or_copy		{COPYTEXT(); return PCOPY;}
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

	/* C-keywords */

sizeof		{COPYTEXT(); return SIZEOF;}

"="		{COPYTEXT(); return ('=');}
"("		{COPYTEXT(); return ('(');}
")"		{COPYTEXT(); return (')');}
"["		{COPYTEXT(); return ('[');}
"]"		{COPYTEXT(); return (']');}
","		{COPYTEXT(); return (',');}
":"		{COPYTEXT(); return (':');}
"?"		{COPYTEXT(); return ('?');}
"+"		{COPYTEXT(); return ('+');}
"-"		{COPYTEXT(); return ('-');}
"*"		{COPYTEXT(); return ('*');}
"/"		{COPYTEXT(); return ('/');}
"%"		{COPYTEXT(); return ('%');}
"&"		{COPYTEXT(); return ('&');}
"|"		{COPYTEXT(); return ('|');}
"^"		{COPYTEXT(); return ('^');}
"&&"		{COPYTEXT(); return AND;}
"||"		{COPYTEXT(); return IOR;}
"<<"		{COPYTEXT(); return LSH;}
">>"		{COPYTEXT(); return RSH;}
"++"		{COPYTEXT(); return INCR;}
"--"		{COPYTEXT(); return DECR;}

"+="		{COPYTEXT(); return ADD_ASSIGN;}
"-="		{COPYTEXT(); return SUB_ASSIGN;}
"*="		{COPYTEXT(); return MUL_ASSIGN;}
"/="		{COPYTEXT(); return DIV_ASSIGN;}
"%="		{COPYTEXT(); return MOD_ASSIGN;}
"&="		{COPYTEXT(); return AND_ASSIGN;}
"|="		{COPYTEXT(); return IOR_ASSIGN;}
"^="		{COPYTEXT(); return XOR_ASSIGN;}
"<<="		{COPYTEXT(); return LSH_ASSIGN;}
">>="		{COPYTEXT(); return RSH_ASSIGN;}

"<"		{COPYTEXT(); return ('<');}
">"		{COPYTEXT(); return ('>');}
"<="		{COPYTEXT(); return LE;}
">="		{COPYTEXT(); return GE;}
"=="		{COPYTEXT(); return EQ;}
"!="		{COPYTEXT(); return NE;}

"."		{COPYTEXT(); return ('.');}
"->"		{COPYTEXT(); return SREF;}

{digit}+	{COPYTEXT(); return CONSTANTI;}

{digit}+\.{digit}*			{COPYTEXT(); return CONSTANTR;}
\.{digit}+				{COPYTEXT(); return CONSTANTR;}
{digit}+\.{digit}*[ed][-+]?{digit}+	{COPYTEXT(); return CONSTANTR;}
\.{digit}+[ed][-+]?{digit}+		{COPYTEXT(); return CONSTANTR;}

\"(\\.|[^\\"])*\"	{COPYSTRING(); return STRING;}

{id}		{COPYTEXT(); return IDENTIFIER;}

{blank}*	{}

"\\"		{std::cerr << "Bad backslash in ACC directives" << std::endl;
		 assert(0);}
{newline}	{std::cerr << "Bad newline in ACC directives" << std::endl;
		 assert(0);}

.		{std::cerr << "Bad character in ACC directives" << std::endl;
		 return LEXICALERROR;}

%%

void acc_c_lexer_init(const char* s) {
  acc_c_input_string = s;
  acc_c_restart(acc_c_in);
}
