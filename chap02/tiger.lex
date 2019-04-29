%{
#include <string.h>
#include "util.h"
#include "tokens.h"
#include "errormsg.h"

int charPos=1;

int yywrap(void)
{
 charPos=1;
 return 1;
}


void adjust(void)
{
 EM_tokPos=charPos;
 charPos+=yyleng;
}

// comment nesting
int commentNesting = 0;

// string buffer
static char *strbuf;
static int strbuf_size;
static int strbuf_leng;
static void strbuf_init() {
  if(strbuf) free(strbuf);
  strbuf = checked_malloc(32);
  strbuf[0] = '\0';
  strbuf_size = 32;
  strbuf_leng = 0;
}
static void strbuf_pushback(char ch) {
  if (strbuf_leng == strbuf_size) {
    char *tmp = checked_malloc(strbuf_size*=2);
    memcpy(tmp, strbuf, strbuf_leng); free(strbuf);
    strbuf = tmp;
  }
  strbuf[strbuf_leng] = ch;
  strbuf[strbuf_leng+=1] = '\0';
}

%}

space     [ \t\n\r]
id        [a-zA-Z][a-zA-Z0-9_]*
ctrl_char \^[@A-Z\[\\\]\^_\?]

%x ID_STATE COMMENT_STATE STRING_STATE STRING_ESCAPE_STATE

%%
[ \t\r]	{adjust(); continue;}
\n	    {adjust(); EM_newline(); continue;}


","	 {adjust(); return COMMA;}
":"  {adjust(); return COLON;}
";"  {adjust(); return SEMICOLON;}
"."  {adjust(); return DOT;}
"("  {adjust(); return LPAREN;}
")"  {adjust(); return RPAREN;}
"["  {adjust(); return LBRACK;}
"]"  {adjust(); return RBRACK;}
"{"  {adjust(); return LBRACE;}
"}"  {adjust(); return RBRACE;}
"+"  {adjust(); return PLUS;}
"-"  {adjust(); return MINUS;}
"*"  {adjust(); return TIMES;}
"/"  {adjust(); return DIVIDE;}
"&"  {adjust(); return AND;}
"|"  {adjust(); return OR;}
"="  {adjust(); return EQ;}
"<>" {adjust(); return NEQ;}
"<"  {adjust(); return LT;}
"<=" {adjust(); return LE;}
">"  {adjust(); return GT;}
">=" {adjust(); return GE;}
":=" {adjust(); return ASSIGN;}


for  	 {adjust(); return FOR;}
while  {adjust(); return WHILE;}
if     {adjust(); return IF;}
then   {adjust(); return THEN;}
else   {adjust(); return ELSE;}
break  {adjust(); return BREAK;}
to     {adjust(); return TO;}
do     {adjust(); return DO;}
let    {adjust(); return LET;}
in     {adjust(); return IN;}
end    {adjust(); return END;}
of     {adjust(); return OF;}
var    {adjust(); return VAR;}
type   {adjust(); return TYPE;}
function  {adjust(); return FUNCTION;}
array  {adjust(); return ARRAY;}
nil    {adjust(); return NIL;}


[0-9]+	 {adjust(); yylval.ival=atoi(yytext); return INT;}

{id} {adjust();yylval.sval=strdup(yytext); return ID;}


"/*"    {adjust(); ++commentNesting; BEGIN COMMENT_STATE;}
<COMMENT_STATE>"/*"    {adjust(); ++commentNesting; continue;}
<COMMENT_STATE>"*/"    {adjust(); --commentNesting; if(!commentNesting) BEGIN INITIAL;}
<COMMENT_STATE>\n      {adjust(); EM_newline(); continue;}
<COMMENT_STATE><<EOF>> {EM_error(EM_tokPos, "Unterminated comment."); yyterminate();}
<COMMENT_STATE>.       {adjust();}


\"    {adjust(); strbuf_init(); BEGIN STRING_STATE;}
<STRING_STATE>\"      {adjust(); BEGIN INITIAL; yylval.sval = strdup(strbuf); return STRING;}
<STRING_STATE>\n      {adjust(); EM_error(EM_tokPos, "Unterminated string."); yyterminate();}
<STRING_STATE>\\      {adjust(); BEGIN STRING_ESCAPE_STATE;}
<STRING_STATE><<EOF>> {EM_error(EM_tokPos, "Unterminated string."); yyterminate();}
<STRING_STATE>.       {adjust(); strbuf_pushback(*yytext); continue;}

<STRING_ESCAPE_STATE>n           {adjust(); strbuf_pushback('\n'); BEGIN STRING_STATE;}
<STRING_ESCAPE_STATE>t           {adjust(); strbuf_pushback('\t'); BEGIN STRING_STATE;}
<STRING_ESCAPE_STATE>{ctrl_char} {adjust(); strbuf_pushback(yytext[1]-'@'); BEGIN STRING_STATE;}
<STRING_ESCAPE_STATE>[0-9]{3}    {adjust(); strbuf_pushback(atoi(yytext)); BEGIN STRING_STATE;}
<STRING_ESCAPE_STATE>\"          {adjust(); strbuf_pushback('\"'); BEGIN STRING_STATE;}
<STRING_ESCAPE_STATE>\\          {adjust(); strbuf_pushback('\\'); BEGIN STRING_STATE;}
<STRING_ESCAPE_STATE>{space}+\\  {adjust();
                                  for(int i = 0; yytext[i]; ++i) 
                                    if (yytext[i] == '\n') EM_newline();
                                   BEGIN STRING_STATE;}


.	 {adjust(); EM_error(EM_tokPos,"illegal token");}


