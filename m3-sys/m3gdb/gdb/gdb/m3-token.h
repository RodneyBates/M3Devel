/* M3 language support routines for GDB, the GNU debugger.
   Copyright 2006 Free Software Foundation, Inc.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#if !defined (M3_TOKEN_H)
#define M3_TOKEN_H 1

#include "defs.h"

enum m3_token_kind {

  TK_EOF,              /* end-of-file */

  /* literals */

  TK_IDENT,            /* identifier       => string, length */
  TK_INTEGER_LIT,      /* INTEGER literal => intval         */
  TK_LONGINT_LIT,      /* LONGINT literal => intval         */
  TK_REAL_LIT,         /* REAL literal     => floatval       */
  TK_LREAL_LIT,        /* LONGREAL literal => floatval       */
  TK_XREAL_LIT,        /* EXTENDED literal => floatval       */
  TK_CHAR_LIT,         /* CHAR literal     => intval         */
  TK_WIDECHAR_LIT,     /* WIDECHAR literal => intval         */
  TK_TEXT_LIT,         /* TEXT literal     => string, length */
  TK_WIDETEXT_LIT,     /* W"" TEXT literal => string, length */

  /* operators */

  TK_PLUS, TK_MINUS, TK_ASTERISK, TK_SLASH, TK_ASSIGN, TK_AMPERSAND,
  TK_DOT, TK_COMMA, TK_SEMI, TK_LPAREN, TK_LBRACKET, TK_LBRACE,
  TK_ARROW, TK_EQUAL, TK_SHARP, TK_LESS, TK_GREATER, TK_LSEQUAL,
  TK_GREQUAL, TK_DOTDOT, TK_COLON, TK_RPAREN, TK_RBRACKET, TK_RBRACE,
  TK_BAR, TK_SUBTYPE, TK_IMPLIES,

  /* reserved words */

  TK_AND, TK_ANY, TK_ARRAY, TK_AS, TK_BEGIN, TK_BITS, TK_BRANDED, TK_BY,
  TK_CASE, TK_CONST, TK_DIV, TK_DO, TK_ELSE, TK_ELSIF, TK_END, TK_EVAL,
  TK_EXCEPT, TK_EXCEPTION, TK_EXIT, TK_EXPORTS, TK_FINALLY, TK_FOR,
  TK_FROM, TK_GENERIC, TK_IF, TK_IMPORT, TK_IN, TK_INTERFACE, TK_LOCK,
  TK_LOOP, TK_METHODS, TK_MOD, TK_MODULE, TK_NOT, TK_OBJECT, TK_OF,
  TK_OR, TK_OVERRIDES, TK_PROCEDURE, TK_RAISE, TK_RAISES, TK_READONLY,
  TK_RECORD, TK_REF, TK_REPEAT, TK_RETURN, TK_REVEAL, TK_ROOT, TK_SET, TK_THEN,
  TK_TO, TK_TRY, TK_TYPE, TK_TYPECASE, TK_UNSAFE, TK_UNTIL, TK_UNTRACED,
  TK_VALUE, TK_VAR, TK_WHILE, TK_WITH,

  /* reserved identifiers */

  TK_ABS, TK_ADDRESS, TK_ADR, TK_ADRSIZE, TK_BITSIZE, TK_BOOLEAN,
  TK_BYTESIZE, TK_CARDINAL, TK_CEILING, TK_CHAR, TK_DEC, TK_DISPOSE,
  TK_EXTENDED, TK_FALSE, TK_FIRST, TK_FLOAT, TK_FLOOR, TK_INC,
  TK_INTEGER, TK_ISTYPE, TK_LAST, TK_LONGCARD, TK_LONGINT, TK_LONGREAL,
  TK_LOOPHOLE, TK_MAX, TK_MIN, TK_MUTEX, TK_NARROW, TK_NEW, TK_NIL, TK_NULL,
  TK_NUMBER, TK_ORD, TK_REAL, TK_REFANY, TK_ROUND, TK_SUBARRAY, TK_TEXT,
  TK_TRUE, TK_TRUNC, TK_TYPECODE, TK_VAL, TK_WIDECHAR,

  /* misc. debugger tokens */

  TK_GDB_HISTORY,       /*  $n   - history reference  => intval      */
  TK_REGISTER,          /*  $rn  - register reference => intval      */
  TK_GDB_VAR,           /*  $id  - GDB variable       => string, len */
  TK_ERROR              /*  lexical error... */

};

struct m3_token {
  enum m3_token_kind  kind;
  char * string;
  int length;
  LONGEST intval;
  double floatval;
};

extern char *
scan_m3_token PARAMS ((char *, struct m3_token *));

extern char *
m3_token_name PARAMS ((struct m3_token *));

#endif /* !defined (M3_TOKEN_H) */

/* End of file "m3-token.h" */
