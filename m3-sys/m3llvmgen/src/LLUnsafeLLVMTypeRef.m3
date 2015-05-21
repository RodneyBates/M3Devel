
(* Copyright (C) 2015, Rodney M. Bates                         *) 
(* All rights reserved.                                        *) 
(* See the file COPYRIGHT for a full description.              *) 
(*                                                             *) 
 
(* UNSAFE utilities for cm3-to-llvm code generation. *) 

UNSAFE MODULE LLUnsafeLLVMTypeRef 

; IMPORT LLVM 

; PROCEDURE Adr ( READONLY Referent : LLVM . TypeRef ) 
  : UNTRACED REF LLVM . TypeRef

  = BEGIN 
      RETURN ADR ( Referent ) 
    END Adr 

; BEGIN 
  END LLUnsafeLLVMTypeRef 
. 

