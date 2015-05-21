
(* Copyright (C) 2015, Rodney M. Bates                         *) 
(* All rights reserved.                                        *) 
(* See the file COPYRIGHT.RMB a full description.              *) 
(*                                                             *) 
 
(* Modula-3 types matching types found in various places in llvm, 
   along with some minimal support. *) 

UNSAFE MODULE LLVMTypes

; IMPORT Ctypes 
; IMPORT M3toC 
; IMPORT Text 

; PROCEDURE CopyM3ToSR ( T : TEXT ) : StringRef
  (* Contains a string copy, in an UNTRACED object, which must be freed. *)

  = VAR LLen : CARDINAL 
  ; VAR LResult : StringRef 

  ; BEGIN
      IF T = NIL 
      THEN LResult := StringRefEmpty 
      ELSE 
        LLen := Text . Length ( T )
      ; IF LLen = 0      
        THEN LResult := StringRefEmpty
        ELSE 
          LResult . Data := M3toC . CopyTtoS ( T ) 
        ; LResult . Length := LLen 
        END (* IF *) 
      END (* IF *) 
    ; RETURN LResult 
    END CopyM3ToSR 

; PROCEDURE FreeCopiedSR ( VAR SR : StringRef )  

  = BEGIN
      M3toC . FreeCopiedS ( SR . Data )
    ; SR . Data := NIL 
    ; SR . Length := 0 
    END FreeCopiedSR 

; VAR (* CONST *) NullChar : CHAR := '\n' 
; VAR (* CONST *) EmptyCStringPtr 
      := LOOPHOLE ( ADR ( NullChar ) , Ctypes . const_char_star ) 

; BEGIN (* LLVMTypes *) 
    StringRefEmpty := StringRef { EmptyCStringPtr , 0 } 
  END LLVMTypes
.

