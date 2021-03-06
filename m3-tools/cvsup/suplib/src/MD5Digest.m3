(* Copyright 1996-2003 John D. Polstra.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgment:
 *      This product includes software developed by John D. Polstra.
 * 4. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

MODULE MD5Digest;

IMPORT Text, Word;

CONST
  HexTab = ARRAY [0..15] OF CHAR{
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
  };

PROCEDURE Compare(READONLY a, b: T): [-1..1] =
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      IF a[i] < b[i] THEN
	RETURN -1;
      ELSIF a[i] > b[i] THEN
	RETURN 1;
      END;
    END;
    RETURN 0;
  END Compare;

PROCEDURE Equal(READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN a = b;
  END Equal;

PROCEDURE FromText(t: TEXT): T
  RAISES {Malformed} =
  VAR
    digest: T;
    val: Word.T := 0;
  BEGIN
    IF Text.Length(t) # 2*NUMBER(digest) THEN RAISE Malformed END;
    FOR i := FIRST(digest) TO LAST(digest) DO
      WITH ch = Text.GetChar(t, 2*i) DO
	CASE ch OF
	| '0'..'9' => val := ORD(ch) - ORD('0');
	| 'a'..'f' => val := ORD(ch) - ORD('a') + 10;
	| 'A'..'F' => val := ORD(ch) - ORD('A') + 10;
	ELSE RAISE Malformed END;
      END;
      val := Word.Shift(val, 4);
      WITH ch = Text.GetChar(t, 2*i+1) DO
	CASE ch OF
	| '0'..'9' => val := Word.Plus(val, ORD(ch) - ORD('0'));
	| 'a'..'f' => val := Word.Plus(val, ORD(ch) - ORD('a') + 10);
	| 'A'..'F' => val := Word.Plus(val, ORD(ch) - ORD('A') + 10);
	ELSE RAISE Malformed END;
      END;
      digest[i] := val;
    END;
    RETURN digest;
  END FromText;

PROCEDURE Hash(READONLY a: T): Word.T =
  VAR
    r: Word.T;
  BEGIN
    r :=
      Word.Or(
	Word.Shift(
	  Word.Or(
	    Word.Shift(
	      Word.Or(
		Word.Shift(a[0], 8),
		a[1]),
	      8),
	    a[2]),
	  8),
	a[3]);
    r :=
      Word.Xor(
	r,
	Word.Or(
	  Word.Shift(
	    Word.Or(
	      Word.Shift(
		Word.Or(
		  Word.Shift(a[4], 8),
		  a[5]),
		8),
	      a[6]),
	    8),
	  a[7])
	);
    r :=
      Word.Xor(
	r,
	Word.Or(
	  Word.Shift(
	    Word.Or(
	      Word.Shift(
		Word.Or(
		  Word.Shift(a[8], 8),
		  a[9]),
		8),
	      a[10]),
	    8),
	  a[11])
	);
    r :=
      Word.Xor(
	r,
	Word.Or(
	  Word.Shift(
	    Word.Or(
	      Word.Shift(
		Word.Or(
		  Word.Shift(a[12], 8),
		  a[13]),
		8),
	      a[14]),
	    8),
	  a[15])
	);
    RETURN r;
  END Hash;

PROCEDURE ToText(READONLY a: T): TEXT =
  VAR
    str: ARRAY [0..31] OF CHAR;
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      WITH val = a[i], j = 2*i DO
	str[j] := HexTab[ Word.RightShift(val, 4) ];
	str[j+1] := HexTab[ Word.And(val, 16_f) ];
      END;
    END;
    RETURN Text.FromChars(str);
  END ToText;

BEGIN
END MD5Digest.
