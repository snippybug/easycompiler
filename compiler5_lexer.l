%{
#include <string.h>
#include <stdlib.h>

#define YYSTYPE struct SyntaxNode *
#include "y.tab.h"

extern YYSTYPE yylval;
extern int lineno;
extern int curnum;
extern int curid;

extern int insert_str(char *str);
%}


letter	[a-zA-Z]
number	[0-9]

%%

main			{ return MAIN; }
read			{ return READ; }
print			{ return PRINT; }
int				{ return INT; }
char			{ return CHAR; }
if				{ return IF; }
else			{ return ELSE; }
while			{ return WHILE; }
for				{ return FOR; }
return			{ return RETURN; }
[_a-zA-Z][_a-zA-Z0-9]*		{
				curid = insert_str(yytext);
				return ID; }
{number}+			{ 
				curnum = atoi(yytext);
				return NUM; }
"="				{ return ASSIGN; }
"("				{ return LP; }
")"				{ return RP; }
"{"				{ return LCB; }
"}"				{ return RCB; }
"["				{ return LSB; }
"]"				{ return RSB; }
"=="			{ return EQ; }
">"				{ return GT; }
"<"				{ return LT; }
">="			{ return GE; }
"<="			{ return LE; }
"!="			{ return NE; }
"+"				{ return ADD; }
"-"				{ return SUB; }
"*"				{ return MUL; }
"/"				{ return DIV; }
"%"				{ return MOD; }
"++"			{ return INC; }
"--"			{ return DEC; }
"&&"			{ return AND; }
"||"			{ return OR; }
"!"				{ return NOT; }
"<<"			{ return LS; }
">>"			{ return RS; }
"&"				{ return BAND; }
"|"				{ return BOR; }
"~"				{ return BNOT; }
"^"				{ return BXOR; }
";"				{ return SEMI; }
","				{ return COM; }
"//".*\n			{lineno++;}
"/*"				{ char ch;
				do{
					ch=input();
					if(ch == '*'){
						ch = input(); 
						if(ch == '/'){
							break;
						}
					}
					if(ch == '\n')
						lineno++;
				}while(1);
				}	
[ \t]			;
\n				{lineno++;}

%%

