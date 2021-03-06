/* A Bison parser, made by GNU Bison 3.3.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2019 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

#ifndef YY_YY_PARSER_H_INCLUDED
# define YY_YY_PARSER_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    TOK_PACKAGE = 258,
    TOK_VERSION = 259,
    TOK_OPTION = 260,
    TOK_DEFGROUP = 261,
    TOK_GROUPOPTION = 262,
    TOK_DEFMODE = 263,
    TOK_MODEOPTION = 264,
    TOK_YES = 265,
    TOK_NO = 266,
    TOK_ON = 267,
    TOK_OFF = 268,
    TOK_FLAG = 269,
    TOK_PURPOSE = 270,
    TOK_DESCRIPTION = 271,
    TOK_USAGE = 272,
    TOK_DEFAULT = 273,
    TOK_GROUP = 274,
    TOK_GROUPDESC = 275,
    TOK_MODE = 276,
    TOK_MODEDESC = 277,
    TOK_MULTIPLE = 278,
    TOK_ARGOPTIONAL = 279,
    TOK_TYPESTR = 280,
    TOK_SECTION = 281,
    TOK_DETAILS = 282,
    TOK_SECTIONDESC = 283,
    TOK_TEXT = 284,
    TOK_VERSIONTEXT = 285,
    TOK_ARGS = 286,
    TOK_VALUES = 287,
    TOK_HIDDEN = 288,
    TOK_DEPENDON = 289,
    TOK_STRING = 290,
    TOK_CHAR = 291,
    TOK_ARGTYPE = 292,
    TOK_SIZE = 293
  };
#endif
/* Tokens.  */
#define TOK_PACKAGE 258
#define TOK_VERSION 259
#define TOK_OPTION 260
#define TOK_DEFGROUP 261
#define TOK_GROUPOPTION 262
#define TOK_DEFMODE 263
#define TOK_MODEOPTION 264
#define TOK_YES 265
#define TOK_NO 266
#define TOK_ON 267
#define TOK_OFF 268
#define TOK_FLAG 269
#define TOK_PURPOSE 270
#define TOK_DESCRIPTION 271
#define TOK_USAGE 272
#define TOK_DEFAULT 273
#define TOK_GROUP 274
#define TOK_GROUPDESC 275
#define TOK_MODE 276
#define TOK_MODEDESC 277
#define TOK_MULTIPLE 278
#define TOK_ARGOPTIONAL 279
#define TOK_TYPESTR 280
#define TOK_SECTION 281
#define TOK_DETAILS 282
#define TOK_SECTIONDESC 283
#define TOK_TEXT 284
#define TOK_VERSIONTEXT 285
#define TOK_ARGS 286
#define TOK_VALUES 287
#define TOK_HIDDEN 288
#define TOK_DEPENDON 289
#define TOK_STRING 290
#define TOK_CHAR 291
#define TOK_ARGTYPE 292
#define TOK_SIZE 293

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 150 "../../src/parser.yy" /* yacc.c:1921  */

    char   *str;
    char    chr;
    int	    argtype;
    int	    boolean;
    class AcceptedValues *ValueList;
    struct gengetopt_option *gengetopt_option;
    struct multiple_size *multiple_size;

#line 144 "parser.h" /* yacc.c:1921  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif


extern YYSTYPE yylval;
extern YYLTYPE yylloc;
int yyparse (void);

#endif /* !YY_YY_PARSER_H_INCLUDED  */
