 
(* Copyright (C) 2014, Rodney M. Bates                         *) 
(* All rights reserved.                                        *) 
(* See the file COPYRIGHT for a full description.              *) 
(*                                                             *) 
 
INTERFACE LLGen 
 
; IMPORT Wr 

; IMPORT LLTypes 

; FROM Target IMPORT M3BackendMode_t 
 
; PROCEDURE New 
    ( logfile : Wr . T ; BackendMode : M3BackendMode_t ) 
  : LLTypes . CodeGenTyp  
  (* A new, initialized code generator that uses llvm. 
     If logfile # NIL and # "", it writes a log on 'logfile'. 
  *) 
 
; PROCEDURE CleanupCGAltogether ( CodeGen : LLTypes . CodeGenTyp ) 

; END LLGen 
. 
 
