

(* Copyright (C) 2015, Rodney M. Bates                         *) 
(* All rights reserved.                                        *) 
(* See the file COPYRIGHT.RMB a full description.              *) 
(*                                                             *) 
 
(* Modula-3 types matching types found in various places in llvm, 
   along with some minimal support. *) 

INTERFACE LLVMTypes 

; IMPORT Ctypes 
; FROM LLVM IMPORT ValueRef 

(* These could come from almost anywhere. *) 
(* TODO: Figure out where/how to get the right types for these: *) 
; TYPE size_t = INTEGER 
; TYPE int64_t = INTEGER 
; TYPE uint64_t = INTEGER 
; TYPE bool = [ 0 .. 1 ] 
; CONST false = 0 
; CONST true = 1 
; TYPE unsigned = [ 0 .. 16_7FFFFFFF ] 
      
; TYPE Opaque = RECORD END 

; TYPE MDNodeRef = UNTRACED BRANDED "M3DIBOpaqueMDNode" REF Opaque

(* From llvm/IR/Function.h: *) 

(* class Function* *) 
; TYPE FunctionRef = UNTRACED BRANDED "LLVMTypesOpaqueFunction" REF Opaque

(* From llvm/IR/BasicBlock.h: *) 
(* This is found in LLVM.i3. *) 

(* From llvm/IR/Instruction.h: *) 

(* class Instruction* *) 
; TYPE InstructionRef = UNTRACED BRANDED "LLVMTypesOpaqueInstruction" REF Opaque

(* From llvm/ADT/StringRef.h: *) 

(* class StringRef (NOTE: this is not a pointer). *) 
; TYPE StringRef 
    = RECORD 
        Data : Ctypes . const_char_star 
      ; Length : size_t
      END 

; CONST StringRefNull = StringRef { Data := NIL , Length := 0 }
  (* NOTE: The C binding LLVMDIBuilder will replace this by a StringRef
           with Data pointing to an empty C string, as expected by the
           functions in LLVM, e.g. DIBuilder.  But Core, provided by llvm,
           won't do this, so use StringRefEmpty, unfortunately not constant.
  *) 

; VAR StringRefEmpty : StringRef (* Will point to an empty string, not NIL. *) 

; PROCEDURE CopyM3ToSR ( T : TEXT ) : StringRef
  (* Contains a string copy, in an UNTRACED object, which must be freed. 
     Will not contain a NIL Data field, but rather a pointer to an empty
     C string, with lifetime forever.
  *) 

; PROCEDURE FreeCopiedSR ( VAR SR : StringRef )  

(* From llvm/ADT/ArrayRef.h: *) 

(* class ArrayRef<Value *> (NOTE: this is not a pointer). *) 
(* FIXME:  This needs another level of indirection.  Data is a pointer
   into an array of *pointers to* ValueRefs.  *) 
; TYPE ArrayRefOfValueRef 
    = RECORD 
        Data : ValueRef
      ; Length : size_t
      END 

; CONST ArrayRefOfValueRefEmpty 
    = ArrayRefOfValueRef { Data := NIL , Length := 0 }

; END LLVMTypes 
. 

