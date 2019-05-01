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
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

 /* record nested comment layer */
static int comment_layer = 0;
%}


/*
 * Define names for regular expressions here.
 */

DIGIT           [0-9]
%Start          COMMENT
%Start          INLINE_COMMENT
%Start          STRING

%%

 /* =================
  *  Nested comments
  * ================= 
  */

 /* begin a comment or nested comment */
<INITIAL,COMMENT,INLINE_COMMENT>"(*" {
  comment_layer++;
  BEGIN COMMENT;
}

 /* if we met *) in (the outermost) comment */
<COMMENT>"*)" {
  comment_layer--;
  if(comment_layer == 0) {
    BEGIN 0; }
}

 /* any character other than '\n','(','*' is ok */ 
<COMMENT>[^\n(*]* {  }

 /* ( or ) or * that appears only once */
<COMMENT>[()*] {  }

<COMMENT>\n { curr_lineno++; }

 /*
  * Error handling in comment 
  */

"*)" {
    cool_yylval.error_msg = "Unmatched *)";
    return ERROR;
}

<COMMENT><<EOF>> {
    cool_yylval.error_msg = "EOF in comment";
    BEGIN 0;
    return ERROR;
}


 /* ===============
  * inline comments
  * ===============
  */

 /* if seen "--", start inline comment */
<INITIAL>"--" { BEGIN INLINE_COMMENT; }

 /* any character other than '\n' is a nop in inline comments */ 
<INLINE_COMMENT>[^\n]* { }

 /* if seen '\n' in inline comment, the comment ends */
<INLINE_COMMENT>\n {
    curr_lineno++;
    BEGIN 0;
}

 /* =========
  * STR_CONST
  * =========
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

 /* if seen '\"', start string */
<INITIAL>\" {
    BEGIN STRING;
    yymore();
}

 /* Cannot read '\\' '\"' '\n' */
<STRING>[^\\\"\n]* { yymore(); }

 /* normal escape characters, not \n */
<STRING>\\[^\n] { yymore(); }

 /* seen a '\\' at the end of a line, the string continues */
<STRING>\\\n {
    curr_lineno++;
    yymore();
}

 /* meet EOF in the middle of a string, error */
<STRING><<EOF>> {
    yylval.error_msg = "EOF in string constant";
    BEGIN 0;
    yyrestart(yyin);
    return ERROR;
}

 /* meet a '\n' in the middle of a string without a '\\', error */
<STRING>\n {
    yylval.error_msg = "Unterminated string constant";
    BEGIN 0;
    curr_lineno++;
    return ERROR;
}

 /* string ends, we need to deal with some escape characters */
<STRING>\" {
   char* ptr = yytext; 
   ptr++;

   string_buf_ptr = string_buf;
   int string_len = 0;
   
   while( *ptr != '"' && string_len < MAX_STR_CONST ) {
     if( *ptr == '\0' ) {
         cool_yylval.error_msg = "String contains null character";
         BEGIN 0;
         return ERROR;
     }
     else if( *ptr == '\\' ){
         ptr++;
         switch(*ptr){
           case 'b':
             *string_buf_ptr++ = '\b';
             break;
           case 't':
             *string_buf_ptr++ = '\t';
             break;
           case 'f':
             *string_buf_ptr++ = '\f';
             break;
           case 'n':
             *string_buf_ptr++ = '\n';
             break;
           case '\0':
             cool_yylval.error_msg = "String contains null character";
             BEGIN 0;
             return ERROR;
           default:
             *string_buf_ptr++ = *ptr;
             break;
         }
         ptr++; string_len++;
     }else{
        *string_buf_ptr++ = *ptr++;
        string_len++;
     }
   }
   if( string_len >= MAX_STR_CONST ) {
     cool_yylval.error_msg = "String constant too long"; 
     BEGIN 0;
     return ERROR;
   }

   *string_buf_ptr++ = '\0';
   cool_yylval.symbol = stringtable.add_string(string_buf);
   BEGIN 0;
   return STR_CONST;
}

 /*
  *  The multiple-character operators.
  */

 /* =========
  * operators
  * =========
  */
"=>"            { return (DARROW);      }
"<-"            { return (ASSIGN);      }
"<="            { return (LE);          }

 /* ========
  * keywords
  * ========
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */

 /* apply i option for case-insensitive */
(?i:class)      { return CLASS;     }
(?i:else)       { return ELSE;      }
(?i:fi)         { return FI;        }
(?i:if)         { return IF;        }
(?i:in)         { return IN;        }
(?i:inherits)   { return INHERITS;  }
(?i:let)        { return LET;       }
(?i:loop)       { return LOOP;      }
(?i:pool)       { return POOL;      }
(?i:then)       { return THEN;      }
(?i:while)      { return WHILE;     }
(?i:case)       { return CASE;      } 
(?i:esac)       { return ESAC;      }
(?i:of)         { return OF;        }
(?i:new)        { return NEW;       }
(?i:isvoid)     { return ISVOID;    }
(?i:not)        { return NOT;       } 

 /* INT_CONST */
{DIGIT}+ {
    cool_yylval.symbol = inttable.add_string(yytext);
    return INT_CONST;
}

 /* BOOL_CONST */
t(?i:rue) {
  cool_yylval.boolean = 1;
  return BOOL_CONST;
}

f(?i:alse) {
  cool_yylval.boolean = 0;
  return BOOL_CONST;
}

 /* only differences between type and object id is the leading char case */

 /* TYPEID */
 /* Class names begin with an uppercase letter. */
[A-Z][A-Za-z0-9_]* {
    cool_yylval.symbol = idtable.add_string(yytext);
    return TYPEID;
}

 /* OBJECTID */
[a-z][A-Za-z0-9_]* {
  cool_yylval.symbol = idtable.add_string(yytext);
  return OBJECTID;
}


 /* ========
  * others 
  * ========
  */

 /* White Space */
[ \f\r\t\v]+ { }

 /* New Line */
"\n" { curr_lineno++; }

 /* all allowed single character symbols */
[:;{}().+\-*/<,~@=] { return *yytext; }

.               {
    cool_yylval.error_msg = yytext;
    return ERROR;
}

%%
