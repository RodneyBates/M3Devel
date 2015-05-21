(* Copyright (C) 1993, Digital Equipment Corporation           *) 
(* All rights reserved.                                        *) 
(* See the file COPYRIGHT for a full description.              *) 
(*                                                             *) 
(* Last modified on Tue Dec 20 16:26:29 PST 1994 by kalsow     *) 
(*      modified on Mon Sep 26 14:27:12 PDT 1994 by isard      *) 
(*      modified on Mon Apr 13 09:55:12 PDT 1992 by muller     *) 
 
INTERFACE LLTypes 
 
; IMPORT LLVM 
; IMPORT M3CG 
; FROM M3CG IMPORT BitSize , BitOffset 
; FROM M3CG IMPORT ByteOffset , ByteSize , (*Byte*)Alignment 
; FROM M3CG IMPORT Var , Proc , Name , Label 
; FROM M3CG IMPORT Type , MType , IType , TypeUID 
; FROM M3ObjFile IMPORT Seg 
; IMPORT M3ID 
; IMPORT Target 
; IMPORT TIntN 
 
; EXCEPTION OutOfMemory ( TEXT ) 

; PROCEDURE ReportOutOfMemory ( Msg : TEXT ) 

; TYPE CodeGenTyp <: Public 
; TYPE Public 
    = M3CG . T OBJECT 
        proc_reguse := ARRAY Regno OF BOOLEAN { FALSE , .. } ; 
      METHODS 
        NewLLVar 
          ( t : Type 
          ; m3t : TypeUID 
          ; s : ByteSize 
          ; a : Alignment 
          ; name : Name := M3ID . NoID 
          ; Prefix : TEXT := "" 
          ) 
        : LLVarTyp 
      ; debug_set_label ( label : Label ) 
      END 
 
; TYPE VLoc = { global , temp } 
 
; TYPE OLoc = { mem , register , fstack , imm } 
 
; TYPE LLVarTyp (* We store this in M3CG.Var variables. *) 
    = Var OBJECT 
        VarNo : INTEGER 
      ; VarLLType : LLTypeTyp := NIL  
      ; VarllvmValueRef : LLVM . ValueRef := NIL 
      ; VarTypeUID : TypeUID 
      ; VarName : Name := M3ID . NoID 
      ; VarllvmName : Name := M3ID . NoID 

      ; varCGType : Type 
      ; var_size : ByteSize 
      ; var_align : Alignment 
      ; exported := FALSE 
      ; seg : Seg 
      ; symbol : INTEGER 
      ; offset : ByteOffset 
      ; loc : VLoc 
      ; stack_temp := FALSE 
      ; scope := 0 
      ; parent : LLProcTyp := NIL 
      END 
 
; TYPE ProcList = REF RECORD loc : ByteOffset ; link : ProcList ; END 
 
; TYPE Temp = RECORD var : LLVarTyp ; free : BOOLEAN ; END 
 
; TYPE LLProcTyp (* We store this in M3CG.Proc variables. *) 
    = Proc OBJECT 
        ProcNo : INTEGER 
      ; ProcLLFuncType : LLFuncTypeTyp 
      ; ProcName : Name := M3ID . NoID 
      ; ProcllvmName : Name := M3ID . NoID 
      ; ProcllvmFunctionRef : LLVM . ValueRef := NIL
      ; ProcFormalsList :  REF ARRAY OF LLVarTyp  

      ; ProcResultCGType : Type 
      ; lev := 0 
      ; parent : LLProcTyp := NIL 
      ; framesize := 0 
      ; paramsize := 8 
      ; tempsize := 0 
      ; temparr : REF ARRAY OF Temp 
      ; templimit := 16 
      ; stdcall : BOOLEAN 
      ; (* => callee cleans *) 
        symbol : INTEGER 
      ; exported := FALSE 
      ; import := FALSE 
      ; offset := 0 
      ; bound := FALSE 
      ; usage : ProcList := NIL 
      ; fenceVar : LLVarTyp := NIL 
      END 

(*------ Types --------------------------------------------------------------*) 
(* These are roughly isomorphic to llvm types.  A graph of them get built 
   from the front-end types provided.  In case of recursive types, a 
   graph traversal is needed to find cycles that cannot map directly 
   to llvm's more restricted system of recursive types.
*)   

; PROCEDURE NewTypeRefArray ( Ct : CARDINAL ) : REF ARRAY OF LLVM . TypeRef  

; CONST DepthInfinity = LAST ( CARDINAL ) (* Must be > all real depths. *) 

; TYPE LLTypeTyp 
    = OBJECT 
        LLTCurrentChild : LLTypeTyp := NIL (* Temporary, during traversal. *) 
      ; LLTDepth : CARDINAL := DepthInfinity (* Temporary, during traversal. *)
      ; LLTllvmTypeRef : LLVM . TypeRef := NIL 
      ; LLTypeUID : TypeUID (* := NoUID ?? *) 
      ; LLTIsComplete : BOOLEAN := FALSE 
      END (* LLTypeTyp *) 

; TYPE LLTypeFwdRefTyp  
    = LLTypeTyp OBJECT
        LLTFwdResolvedType : LLTypeTyp 
      END 
  (* A forward reference to a type is represented by this node, with
     LLTTypeUID set and LLTFwdResolvedType = NIL, and the UID mapping
     to the node.  When the UID is declared, LLTFwdResolvedType is changed
     to point to the real type node, as is the UID mapping.  These nodes 
     must be looked-through.
  *) 
 
; PROCEDURE NewLLTypeFwdRef ( ) : LLTypeFwdRefTyp  

; TYPE LLFuncTypeTyp 
    = LLTypeTyp OBJECT 
        FuncTypeResultTypeUID : TypeUID 
      ; FuncTypeResultLLType : LLTypeTyp 
      ; FuncTypeResultCGType : CGType 
      ; FuncTypeCallingConvention := CallingConvention   
      ; FuncTypeFormals : REF ARRAY OF REF LLFormalTyp := NIL 
        (* ^Exact size.  Could be NIL, if formal count = 0. *)
      ; FuncTypeHasResultFormal : BOOLEAN := FALSE 
      END (* LLFuncTypeTyp *)

; PROCEDURE NewLLFuncType ( ) : LLFuncTypeTyp  

; TYPE LLFormalTyp 
    = RECORD 
        FormalTypeUID : TypeUID := 0 (* What is an undefined UID? *)  
      ; FormalCGType : CGType 
      ; FormalName : Name := M3ID . NoID 
      ; FormalType : LLTypeTyp := NIL 
      ; FormalBitOffset : BitOffset := 0 
      ; FormalBitSize : BitSize := 0 
      ; FormalByteAlign : Alignment := 0 
      END (* LLFormalTyp *)  
 
; PROCEDURE NewLLFormal ( ) : LLFormalTyp 

; PROCEDURE NewLLFormalArray ( Ct : CARDINAL ) : REF ARRAY OF REF LLFormalTyp 
  (* POST: Possibly NIL if Ct = 0 *) 

; TYPE LLStructTypeTyp 
    = LLTypeTyp OBJECT 
        StructTypeFields : REF ARRAY OF REF LLFieldTyp := NIL 
      ; StructTypeBitSize : BitSize := 0 
(* Do we need an alignment?  It looks hard to get. *) 
      END (* LLStructTypeTyp *)
 
; PROCEDURE NewLLStructType ( ) : LLStructTypeTyp  

; TYPE LLFieldTyp 
    = RECORD 
        FieldTypeUID : TypeUID := 0 (* What is an undefined UID? *)  
      ; FieldName : Name := M3ID . NoID 
      ; FieldBitOffset : BitOffset := 0 
      ; FieldBitSize : BitSize := 0 
      ; FieldByteAlign : Alignment := 0 
      ; FieldType : LLTypeTyp := NIL 
      END (* LLFieldTyp *) 

; PROCEDURE NewLLField ( ) : REF LLFieldTyp 

; PROCEDURE NewLLFieldArray ( Ct : CARDINAL ) : REF ARRAY OF REF LLFieldTyp 

; TYPE LLArrayTypeTyp 
    = LLTypeTyp OBJECT 
        LLTArrayElemCt : CARDINAL := 0 
      ; LLTArrayElementType : LLTypeTyp
      END 

; PROCEDURE NewLLArrayType ( ) : LLArrayTypeTyp  

; TYPE LLPtrTypeTyp 
    = LLTypeTyp OBJECT 
        LLTPtrReferentType : LLTypeTyp 
      ; LLTPtrIsPseudoStruct : BOOLEAN := FALSE 
      END 

; PROCEDURE NewLLPtrType ( ) : LLPtrTypeTyp  

; TYPE MVar 
    = RECORD 
        var : LLVarTyp 
      ; mvar_offset : ByteOffset := 0 
      ; mvar_type : MType 
      END 
 
; CONST NoStore = MVar { var := NIL , mvar_type := FIRST ( MType ) } 
 
(* If an operand requires two registers, it has parts 0 and 1. 
 * etc. This is used to implement multi precision integers, 
 * and possibly enregistering structs. Likewise, if an 
 * operand fits in one register, it has size 1, else 2. 
 *) 
 
; TYPE OperandPart = [ 0 .. 1 ] 
; TYPE OperandSize = [ 1 .. 2 ] 
 
; TYPE Operand1 
    = RECORD 
        loc : OLoc 
      ; mvar : MVar := NoStore 
      ; reg : Regno := 0 
      ;                     (* seems like it should be -1 *) 
        imm : INTEGER := 0 
      ;                     (* This might change to TIntN.T. *) 
        stackp : CARDINAL := 0 
      ;                      (* this field might go away; seems like it should be -1 *) 
        opcode := FALSE 
      ; 
      END 
 
; Operand 
    = RECORD 
        loc : OLoc 
      ; mvar : MVar := NoStore 
      ; reg := ARRAY OperandPart OF Regno { 0 , .. } 
      ; (* seems like it should be -1 *) 
        imm : TIntN . T := TIntN . Zero 
      ; optype : Type := Type . Void 
      ; stackp : CARDINAL := 0 
      ; (* seems like it should be -1 *) 
        opcode := FALSE 
      ; 
      END 
 
; TYPE FlToInt = { Round , Floor , Ceiling , Truncate } 
 
; TYPE Force = { any , mem , anydword , anytemp , anyregimm , anyreg , regset } 
 
; CONST NRegs : INTEGER = 7 
 
; TYPE Regno = [ - 1 .. NRegs ] 
 
; 
(* These constants relate to how x86 instructions are encoded. Do not change them. *) 
 
  CONST EAX = 0 
; ECX = 1 
; EDX = 2 
; EBX = 3 
; ESP = 4 
; EBP = 5 
; ESI = 6 
; EDI = 7 
; CONST RegName 
    = ARRAY Regno OF TEXT 
        { "*NOREG*" 
        , "EAX" 
        , "ECX" 
        , "EDX" 
        , "EBX" 
        , "ESP" 
        , "EBP" 
        , "ESI" 
        , "EDI" 
        } 
 
; TYPE RegSet = SET OF Regno 
 
(* ESP and EBP are "special" and excluded from these sets. *) 
 
; CONST NonVolatileRegisters = RegSet { EDI , ESI , (*EBP,*) EBX } 
; CONST VolatileRegisters = RegSet { EAX , ECX , EDX (*,ESP*) } 
; CONST AllRegisters 
    = RegSet { EAX , ECX , EDX , EBX ,        (*ESP,*)(*EBP,*) ESI , EDI } 
; CONST RegistersForByteOperations = RegSet { EAX , EBX , ECX , EDX } 
 
; PROCEDURE TypeIsUnsigned ( t : Type ) : BOOLEAN 
; PROCEDURE TypeIsSigned ( t : Type ) : BOOLEAN 
; PROCEDURE TypeIs64 ( t : Type ) : BOOLEAN 
; PROCEDURE SplitMVar 
    ( READONLY mvar : MVar ; VAR mvarA : ARRAY OperandPart OF MVar ) 
  : OperandSize 
; PROCEDURE SplitImm 
    ( type : Type 
    ; READONLY imm : TIntN . T 
    ; VAR immA : ARRAY OperandPart OF TIntN . T 
    ) 
  : OperandSize 
; PROCEDURE SplitOperand 
    ( READONLY op : Operand ; VAR opA : ARRAY OperandPart OF Operand ) 
  : OperandSize 
; PROCEDURE GetOperandSize ( READONLY op : Operand ) : OperandSize 
; PROCEDURE GetTypeSize ( type : Type ) : OperandSize 
 
; CONST TZero = TIntN . Zero 
; CONST TOne = TIntN . One 
 
; CONST UnsignedType 
    = ARRAY IType OF IType 
        { Type . Word32 , Type . Word32 , Type . Word64 , Type . Word64 } 
 
; CONST MaximumShift 
    = ARRAY IType OF TIntN . T 
        { TIntN . ThirtyOne 
        , TIntN . ThirtyOne 
        , TIntN . SixtyThree 
        , TIntN . SixtyThree 
        } 
 
; CONST MinimumShift 
    = ARRAY IType OF TIntN . T 
        { TIntN . MThirtyOne 
        , TIntN . MThirtyOne 
        , TIntN . MSixtyThree 
        , TIntN . MSixtyThree 
        } 
 
; CONST BitCountMask = MaximumShift 
 
; VAR(*CONST*) IntType : ARRAY M3CG . MType OF Target . Int_type 
 
; END LLTypes 
. 
