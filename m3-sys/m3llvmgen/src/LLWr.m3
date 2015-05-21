(* Copyright (C) 1993, Digital Equipment Corporation           *) 
(* All rights reserved.                                        *) 
(* See the file COPYRIGHT for a full description.              *) 
(*                                                             *) 
(* Last modified on Mon Sep 26 14:27:10 PDT 1994 by isard      *) 
(*      modified on Mon Jun 27 17:05:36 PDT 1994 by kalsow     *) 
(*      modified on Tue May 25 14:22:35 PDT 1993 by muller     *) 
 
MODULE LLWr 
 
; IMPORT Text 
; IMPORT Thread 
; IMPORT Wr 

; FROM LLTypes IMPORT LLVarTyp , LLProcTyp , VLoc 
; IMPORT M3Buf 
; IMPORT M3CG 
; FROM M3CG IMPORT Name , TypeUID 
; FROM M3CG IMPORT Var , Proc , Label , No_label 
; FROM M3CG IMPORT Type 
; IMPORT M3ID 
; IMPORT Target 
; IMPORT TFloat 
; IMPORT TIntN 
 
; REVEAL T 
    = Public BRANDED "LLWr.T" OBJECT 
        wr : Wr . T := NIL 
      ; buf : M3Buf . T := NIL 
      ; buf_len : INTEGER := 0 
      ; OVERRIDES Flush := Flush 
      ; NL := NL 
      ; Cmd := Cmd 
      ; ZName := ZName 
      ; VName := VName 
      ; PName := PName 
      ; TName := TName 
      ; Flt := Flt 
      ; Bool := Bool 
      ; Lab := Lab 
      ; Tipe := Tipe 
      ; Int := Int 
      ; TInt := TInt 
      ; BInt := BInt 
      ; Txt := Txt 
      ; OutC := OutC 
      ; OutT := OutT 
      ; OutN := OutN 
      ; OutS := OutS 
      ; OutI := OutI 
      END 
 
; PROCEDURE NL ( t : T ) 
  = BEGIN 
      OutT ( t , Wr . EOL ) 
    ; Flush ( t ) 
    END NL 
 
; PROCEDURE Cmd ( t : T ; cmd : TEXT ) 
  = VAR len := Text . Length ( cmd ) 
  ; BEGIN 
      OutC ( t , '\t' ) 
    ; OutT ( t , cmd ) 
    ; FOR i := 0 TO 14 - len DO OutC ( t , ' ' ) END 
    ; OutC ( t , ' ' ) 
    ; OutC ( t , ' ' ) 
    END Cmd 
 
; PROCEDURE ZName ( t : T ; n : Name ) 
  = BEGIN 
      OutC ( t , ' ' ) 
    ; IF ( n = M3ID . NoID ) 
      THEN 
        OutC ( t , '*' ) 
      ELSE 
        OutN ( t , n ) 
      END 
    END ZName 
 
; PROCEDURE XName ( t : T ; n : Name ) 
  = BEGIN 
      IF ( n # M3ID . NoID ) 
      THEN 
        OutC ( t , '[' ) 
      ; OutN ( t , n ) 
      ; OutC ( t , ']' ) 
      END 
    END XName 
 
; PROCEDURE VName ( t : T ; v : Var ) 
  = CONST VTag = ARRAY VLoc OF TEXT { " gv." , " tv." } 
  ; BEGIN 
      TYPECASE v 
      OF 
      | NULL 
        =>  OutT ( t , " *" ) 
      | LLVarTyp ( x ) 
        =>  OutT ( t , VTag [ x . loc ] ) 
          ; OutI ( t , x . tag ) 
          ; XName ( t , x . name ) 
      ELSE 
        OutT ( t , " v.???" ) 
      END 
    END VName 
 
; PROCEDURE PName ( t : T ; p : Proc ) 
  = BEGIN 
      TYPECASE p 
      OF 
      | NULL 
        =>  OutT ( t , " *" ) 
      | LLProcTyp ( x ) 
        =>  OutT ( t , " p." ) 
          ; OutI ( t , x . tag ) 
          ; XName ( t , x . name ) 
      ELSE 
        OutT ( t , " p.???" ) 
      END 
    END PName 
 
; PROCEDURE TName ( t : T ; type : Type ) 
  = CONST type_names 
      = ARRAY Type OF TEXT 
          { " Word.8" 
          , " Int.8" 
          , " Word.16" 
          , " Int.16" 
          , " Word.32" 
          , " Int.32" 
          , " Word.64" 
          , " Int.64" 
          , " Reel" 
          , " LReel" 
          , " XReel" 
          , " Addr" 
          , " Struct" 
          , " Void" 
          } 
  ; BEGIN 
      OutT ( t , type_names [ type ] ) 
    END TName 
 
; PROCEDURE Flt ( t : T ; READONLY f : Target . Float ) 
  = CONST FType = ARRAY Target . Precision OF TEXT { " R " , " L " , " X " } 
  ; VAR buf : ARRAY [ 0 .. BITSIZE ( Target . Extended ) ] OF CHAR 
  ; BEGIN 
      OutT ( t , FType [ TFloat . Prec ( f ) ] ) 
    ; OutS ( t , SUBARRAY ( buf , 0 , TFloat . ToChars ( f , buf ) ) ) 
    END Flt 
 
; PROCEDURE Bool ( t : T ; b : BOOLEAN ) 
  = CONST Tags = ARRAY BOOLEAN OF CHAR { 'F' , 'T' } 
  ; BEGIN 
      OutC ( t , ' ' ) 
    ; OutC ( t , Tags [ b ] ) 
    END Bool 
 
; PROCEDURE Lab ( t : T ; i : Label ) 
  = BEGIN 
      OutC ( t , ' ' ) 
    ; IF ( i = No_label ) 
      THEN 
        OutC ( t , '*' ) 
      ELSE 
        OutT ( t , "L." ) 
      ; OutI ( t , i ) 
      END 
    END Lab 
 
; PROCEDURE Tipe ( t : T ; type : TypeUID ) 
  = BEGIN 
      OutT ( t , " " ) 
    ; OutI ( t , type ) 
    END Tipe 
 
; PROCEDURE Int ( t : T ; i : INTEGER ) 
  = BEGIN 
      OutC ( t , ' ' ) 
    ; OutI ( t , i ) 
    END Int 
 
; PROCEDURE TInt ( t : T ; READONLY i : TIntN . T ) 
  = VAR buf : ARRAY [ 0 .. BITSIZE ( TIntN . T ) ] OF CHAR 
  ; BEGIN 
      OutC ( t , ' ' ) 
    ; OutS ( t , SUBARRAY ( buf , 0 , TIntN . ToChars ( i , buf ) ) ) 
    END TInt 
 
; PROCEDURE BInt ( t : T ; i : INTEGER ) 
  = BEGIN 
      Int ( t , i ) 
     (* since the reader doesn't know how to read 'bytes+bits' *) 
    END BInt 
 
; 
(********* 
PROCEDURE BInt (t: T;  i: INTEGER) = 
  VAR x := i MOD TIntN.Byte; 
      y := i DIV TIntN.Byte; 
  BEGIN 
    IF (x = 0) THEN 
      Int (t, y); 
    ELSE 
      Int (t, y);  OutC (t, '+');  OutI (t, x); 
    END; 
  END BInt; 
***************) 
 
  CONST VanillaChars = SET OF CHAR { ' ' , '!' , '#' .. '[' , ']' .. '~' } 
; Digits 
    = ARRAY [ 0 .. 7 ] OF CHAR { '0' , '1' , '2' , '3' , '4' , '5' , '6' , '7' } 
 
; PROCEDURE Txt ( t : T ; txt : TEXT ) 
  = VAR c : CHAR 
  ; BEGIN 
      OutC ( t , ' ' ) 
    ; IF ( txt = NIL ) 
      THEN 
        OutC ( t , '*' ) 
      ; RETURN 
      END 
    ; OutC ( t , '"' ) 
    ; FOR i := 0 TO Text . Length ( txt ) - 1 
      DO 
        c := Text . GetChar ( txt , i ) 
      ; IF ( c IN VanillaChars ) 
        THEN 
          OutC ( t , c ) 
        ELSE 
          OutC ( t , '\\' ) 
        ; OutC ( t , Digits [ ORD ( c ) DIV 64 ] ) 
        ; OutC ( t , Digits [ ORD ( c ) MOD 64 DIV 8 ] ) 
        ; OutC ( t , Digits [ ORD ( c ) MOD 8 ] ) 
        END 
      END 
    ; OutC ( t , '"' ) 
    END Txt 
 
; 
(*--------------------------------------------------------- low level I/O ---*) 
 
  PROCEDURE Flush ( t : T ) 
  = 
        <*FATAL Wr.Failure, Thread.Alerted*> 
    BEGIN 
      M3Buf . Flush ( t . buf , t . wr ) 
    ; t . buf_len := 0 
    ; Wr . Flush ( t . wr ) 
    END Flush 
 
; PROCEDURE IncLength ( t : T ; approximateIncrease : CARDINAL ) 
  = BEGIN 
      INC ( t . buf_len , approximateIncrease ) 
    ; IF ( t . buf_len >= 1024 ) OR ( t . buf_len < 0 ) 
      THEN 
        Flush ( t ) 
      END 
    END IncLength 
 
; PROCEDURE OutC ( t : T ; c : CHAR ) 
  = BEGIN 
      M3Buf . PutChar ( t . buf , c ) 
    ; IncLength ( t , 1 ) 
    END OutC 
 
; PROCEDURE OutT ( t : T ; txt : TEXT ) 
  = BEGIN 
      M3Buf . PutText ( t . buf , txt ) 
    ; IncLength ( t , Text . Length ( txt ) ) 
    END OutT 
 
; PROCEDURE OutN ( t : T ; n : Name ) 
  = BEGIN 
      M3ID . Put ( t . buf , n ) 
    ; IncLength ( t , 10 ) 
    ; (* This does not have to be accurate. *) 
    END OutN 
 
; PROCEDURE OutS ( t : T ; READONLY buf : ARRAY OF CHAR ) 
  = BEGIN 
      M3Buf . PutSub ( t . buf , buf ) 
    ; IncLength ( t , NUMBER ( buf ) ) 
    END OutS 
 
; PROCEDURE OutI ( t : T ; i : INTEGER ) 
  = BEGIN 
      M3Buf . PutInt ( t . buf , i ) 
    ; IncLength ( t , 4 ) 
    ; (* This does not have to be accurate. *) 
    END OutI 
 
; 
 
(*---------------------------------------------------------------------------*) 
 
  PROCEDURE New ( output : Wr . T ) : T 
  = BEGIN 
      RETURN NEW ( T , wr := output , buf := M3Buf . New ( ) , buf_len := 0 ) 
    END New 
 
; BEGIN 
  END LLWr 
. 
