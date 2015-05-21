
(* Copyright (C) 2015, Rodney M. Bates                         *) 
(* All rights reserved.                                        *) 
(* See the file COPYRIGHT for a full description.              *) 
(*                                                             *) 
 
MODULE LLInit 

; IMPORT LLVM 
; IMPORT IntLLVMTypeRefTbl 

; PROCEDURE InitBuiltinTypes 
    ( T : IntLLVMTypeRefTbl . Default ; C : LLVM . ContextRef ) 

  = VAR LInt8TypeRef : LLVM . TypeRef 

  ; BEGIN 
      LInt8TypeRef := LLVM . LLVMInt8TypeInContext ( C ) 
    ; EVAL T . put ( UID_INTEGER , LLVM . LLVMInt64TypeInContext ( C ) )
    ; EVAL T . put ( UID_LONGINT , LLVM . LLVMInt64TypeInContext ( C ) )
    ; EVAL T . put ( UID_WORD , LLVM . LLVMInt64TypeInContext ( C ) )
    ; EVAL T . put ( UID_LONGWORD , LLVM . LLVMInt64TypeInContext ( C ) )
    ; EVAL T . put ( UID_REEL , LLVM . LLVMFloatTypeInContext ( C ) )
    ; EVAL T . put ( UID_LREEL , LLVM . LLVMDoubleTypeInContext ( C ) )
    ; EVAL T . put ( UID_XREEL , LLVM . LLVMDoubleTypeInContext ( C ) )
    ; EVAL T . put ( UID_BOOLEAN , LLVM . LLVMInt1TypeInContext ( C ) )
    ; EVAL T . put ( UID_CHAR , LLVM . LLVMInt8TypeInContext ( C )  )
    ; EVAL T . put ( UID_WIDECHAR , LLVM . LLVMInt32TypeInContext ( C ) )
(* The "Element" type of a pointer type cannot be void. *) 
    ; EVAL T . put ( UID_MUTEX , LLVM . LLVMPointerType ( LInt8TypeRef ) )
    ; EVAL T . put ( UID_TEXT , LLVM . LLVMPointerType ( LInt8TypeRef ) )
    ; EVAL T . put ( UID_UNTRACED_ROOT , LLVM . LLVMPointerType ( LInt8TypeRef ) )
    ; EVAL T . put ( UID_ROOT , LLVM . LLVMPointerType ( LInt8TypeRef ) )
    ; EVAL T . put ( UID_REFANY , LLVM . LLVMPointerType ( LInt8TypeRef ) )
    ; EVAL T . put ( UID_ADDR , LLVM . LLVMPointerType ( LInt8TypeRef ) )
    ; EVAL T . put ( UID_NULL , LLVM . LLVMPointerType ( LInt8TypeRef ) )
(*
    ; EVAL T . put ( UID_RANGE_0_31 , LLVM . LLVM )
    ; EVAL T . put ( UID_RANGE_0_63 , LLVM . LLVM )
    ; EVAL T . put ( UID_PROC1 , LLVM . LLVM )
    ; EVAL T . put ( UID_PROC2 , LLVM . LLVM )
    ; EVAL T . put ( UID_PROC3 , LLVM . LLVM )
    ; EVAL T . put ( UID_PROC4 , LLVM . LLVM )
    ; EVAL T . put ( UID_PROC5 , LLVM . LLVM )
    ; EVAL T . put ( UID_PROC6 , LLVM . LLVM )
    ; EVAL T . put ( UID_PROC7 , LLVM . LLVM )
    ; EVAL T . put ( UID_PROC8 , LLVM . LLVM )
*) 
    END InitBuiltinTypes 

; BEGIN 
  END LLInit
. 



