/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
  if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
    YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;

extern YYSTYPE cool_yylval;


/*  Add Your own definitions here
****************************************/
int comment_stack;
int string_len;
void error(char* msg);
void clearBuf();
int strError();
int tooLong();
/****************************************/
%}

%option noyywrap

 /* Define conditions for concise
 *****************************************/
%x COM_ENCLOSE
%x COM_DASH
%x STRING
%x STRING_RES
%x ESCAPED
/*****************************************/

DIGIT       [0-9]
LETTER      [0-9a-zA-Z_]
/* Type identifiers begin with a capital letter */
TYPEID      [A-Z]{LETTER}*
/* Object identifiers begin with a lower case letter */
OBJECTID    [a-z]{LETTER}*
/* Special notations in one character
    using escape character "\" to specify */
SYMBOLS     [\+\-\*\/\=\<\.\~\,\;\:\(\)\@\{\}]
WS          [ \f\r\t\v]


%% /*-----------------------------------------------------------------------*/

    /* Integers are non-empty strings of digits 0-9 */
{DIGIT}+    {
    cool_yylval.symbol = inttable.add_string(yytext);
    return INT_CONST;
}


    /* All special symbols specified from Figure 1 */
"<-"        {return ASSIGN      ;}
"=>"        {return DARROW      ;}
"<="        {return LE          ;}
{SYMBOLS}   {return yytext[0]   ;}


    /* the first letter of true and false must be lowercase;
        the trailing letters may be upper or lower case
        "?i:" here indicate case insensitive */
t(?i:rue)       {
    cool_yylval.boolean = true;
    return BOOL_CONST;
}
f(?i:alse)      {
    cool_yylval.boolean = false;
    return BOOL_CONST;
}


    /* keywords:
        "?i:" here indicate case insensitive
    */
(?i:class)      {return CLASS       ;}
(?i:else)       {return ELSE        ;}
(?i:fi)         {return FI          ;}
(?i:if)         {return IF          ;}
(?i:in)         {return IN          ;}
(?i:inherits)   {return INHERITS    ;}
(?i:let)        {return LET         ;}
(?i:loop)       {return LOOP        ;}
(?i:pool)       {return POOL        ;}
(?i:then)       {return THEN        ;}
(?i:while)      {return WHILE       ;}
(?i:case)       {return CASE        ;}
(?i:esac)       {return ESAC        ;}
(?i:of)         {return OF          ;}
(?i:new)        {return NEW         ;}
(?i:isvoid)     {return ISVOID      ;}
(?i:not)        {return NOT         ;}


    /* Identifiers are strings (other than keywords) consisting of
        letters, digits, and the underscore character
        **************************************************
        **The identifiers must be matched after keywords**
        **************************************************
    */
{TYPEID}    {
    cool_yylval.symbol = stringtable.add_string(yytext);
    return TYPEID;
}

{OBJECTID}  {
    cool_yylval.symbol = stringtable.add_string(yytext);
    return OBJECTID;
}


    /* Comments include two kinds of styles
--------------------------------------------------------------------------------
    Single line comments */
"--"                    {BEGIN(COM_DASH);}
<COM_DASH>\n            {
    curr_lineno++;
    BEGIN(0);
}
<COM_DASH><<EOF>>       {
    /* This condition may never occur */
    curr_lineno++;
    BEGIN(0);
}
<COM_DASH>.             {}

    /* Nested comments */
"(*"                    {
    comment_stack = 1;
    BEGIN(COM_ENCLOSE);
}
<COM_ENCLOSE>"(*"       {comment_stack++;}
<COM_ENCLOSE><<EOF>>    {
    error("EOF in comment");
    BEGIN(0);   /*Actually it may be not necessary for single file*/
    return ERROR;
}
<COM_ENCLOSE>\n         {curr_lineno++;}
<COM_ENCLOSE>"*)"       {
    comment_stack--;
    if (comment_stack==0){
        BEGIN(0);
    }
}
<COM_ENCLOSE>.          {}
"*)"                    {
    error("Unmatched *)");
    BEGIN(0);
    return ERROR;
}


    /*
--------------------------------------------------------------------------------
    String*/
\"                      {
    BEGIN(STRING);
    string_len = 0;
}
<STRING>\"              {
    cool_yylval.symbol = stringtable.add_string(string_buf);
    clearBuf();
    BEGIN(0);
    return STR_CONST;
}
<STRING,ESCAPED><<EOF>> {
    error("EOF in string constant");
    BEGIN(0);
    return strError();
}
<STRING,ESCAPED>\0      {
    error("String contains null character");
    BEGIN(STRING_RES);
    return strError();
}
<STRING>\n              {
    error("Unterminated string constant");
    curr_lineno++;
    /* Here assume the programmer just forget to close the string */
    BEGIN(0);
    return strError();
}
<STRING>\\              {
    if(string_len+1 >= MAX_STR_CONST){
        return tooLong();
    }else{
        string_len++;
        BEGIN(ESCAPED);
        /* create a new escaped state inside STRING
            note to resume back everytime*/
    }
}

    /* Note that only \b \t \n \f and \ itself are special
        to be one special character*/
<ESCAPED>b          {strcat(string_buf, "\b");BEGIN(STRING);}
<ESCAPED>t          {strcat(string_buf, "\t");BEGIN(STRING);}
<ESCAPED>n          {strcat(string_buf, "\n");BEGIN(STRING);}
<ESCAPED>f          {strcat(string_buf, "\f");BEGIN(STRING);}
<ESCAPED>\\         {strcat(string_buf, "\\");BEGIN(STRING);}

<ESCAPED>\n         {
    /* escaped newline in string is valid */
    if(string_len+1 >= MAX_STR_CONST){
        return tooLong();
    }else{
        string_len++;
        curr_lineno++;
        strcat(string_buf, yytext);
        BEGIN(STRING);
    }
}

<ESCAPED>.          {
    /* only one char after \ will be effected */
    strcat(string_buf, yytext);
    BEGIN(STRING);
}

<STRING>.           {
    if(string_len+1 >= MAX_STR_CONST){
        return tooLong();
    }else{
        string_len++;
        strcat(string_buf, yytext);
    }
}

    /* Waiting for chance to resume the lexing */
<STRING_RES>\"      {BEGIN(0);}
<STRING_RES>\n      {
    curr_lineno++;
    BEGIN(0);
}
    /* count line and skip all useless strings */
<STRING_RES>\\\n    {curr_lineno++;}
<STRING_RES>.       {}


    /* Here all cases should be taken into account */
\n                  {curr_lineno++;}
    /* eat up spaces and report all invalid tokens */
{WS}                {}
.                   {
    error(yytext);
    return ERROR;
}

%% /*------------------------------------------------------------------------*/


    /* Auxiliary functions here */
void error(char* msg){
    cool_yylval.error_msg = msg;
}

void clearBuf(){
    /* clear the string buffer */
    string_buf[0] = '\0';
}

int strError(){
    /* for general use of error in string */
    clearBuf();
    return ERROR;
}

int tooLong(){
    error("String constant too long");
    BEGIN(STRING_RES);
    return strError();
}
