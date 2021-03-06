(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

(* A "TextLiteral.T" is a text generated by the compiler.  They contain
   either 8-bit "CHAR" characters or 16-bit or 32-bit "WIDECHAR" characters, 
   but not both CHARs and WIDECHARs.  These values do not reside in the heap.
   In spite of the declaration, the "contents" array is only large enough
   to hold the literal and its null termination. *)

UNSAFE INTERFACE TextLiteral;

IMPORT RTHooks, TextClass;

CONST
(* We desire no limit here:
    MaxBytes = LAST (INTEGER);

   But cm3 measures sizes in bits, and the size must fit in an INTEGER:
    MaxBytes = LAST(INTEGER) DIV BITSIZE(Byte).

   And the overall type size, in bits, must fit in an INTEGER:
     MaxBytes = LAST(INTEGER) DIV BITSIZE(Byte) - something

   And we want the 32bit host to be able to target 64bit, so
    sizes must fit in 32bits for now:
     MaxBytes = LAST(32bit integer) DIV BITSIZE(Byte) - 7 - 8 * ORD(BITSIZE(INTEGER) = 64)
     or 2 * BYTESIZE(INTEGER) possibly - 1

   And we want the type to be more similar for 32bit/64bit pickle interchange:
   Really?
     MaxBytes = LAST(32bit integer) DIV BITSIZE(Byte) - 15
     
    Or we desire a syntax to not state a maximum at all.
    The code is unsafe either way.

 NOTE: This is the maximum size of a literal, not of a TEXT.
 TODO: frontend should use LONGINT or TInt, and possibly usually measure in
 bytes. Pickles should better tolerate 32bit/64bit differences.
    MaxBytes = LAST (INTEGER) - BYTESIZE (INTEGER) * 2;

    If the value is too larger, we get:
    ../src/text/TextLiteral.i3", line 46: CM3 restriction: record or object type is too large
*)
    MaxBytes = 16_7FFFFFFF DIV BITSIZE (Byte) - 15; (* 268,435,440 almost 256MB *)

TYPE
  T = RTHooks.TextLiteral;
REVEAL
  T = TEXT BRANDED "TextLiteral.T" OBJECT
    cnt : INTEGER;
    buf : ARRAY [0..MaxBytes - 1] OF Byte;
  OVERRIDES
    get_info       := RTHooks.TextLitInfo;
    get_char       := RTHooks.TextLitGetChar;
    get_wide_char  := RTHooks.TextLitGetWideChar;
    get_chars      := RTHooks.TextLitGetChars;
    get_wide_chars := RTHooks.TextLitGetWideChars;
  END;
  (* The array contains the characters of the text followed by a null
     character.  If "cnt" is negative each BYTESIZE(WIDECHAR) bytes in "buf"
     is a "WIDECHAR".  Otherwise, each byte of "buf" is a "CHAR". *)

TYPE
  Byte = [0..255];

END TextLiteral.
