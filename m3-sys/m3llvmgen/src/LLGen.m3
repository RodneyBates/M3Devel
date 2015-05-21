
(* Copyright (C) 2015, Rodney M. Bates                         *) 
(* All rights reserved.                                        *) 
(* See the file COPYRIGHT for a full description.              *) 
(*                                                             *) 
 
UNSAFE MODULE LLGen EXPORTS LLGen , LLTypes 
(* TODO: Eliminate the EXPORT of LLTypes, when done stripping out x86 stuff. *) 
(* UNSAFE, because we have to allocate untraced strings to pass to llvm. *) 
 
; IMPORT Ctypes 
; IMPORT Fmt 
; IMPORT IntRefTbl  
; IMPORT Stdio 
; IMPORT Text 
; IMPORT TextUtils 
; IMPORT Thread 
; IMPORT Word 
; IMPORT Wr 

; IMPORT Error 
; IMPORT IntLLTypeTbl 
; IMPORT IntLLVMTypeRefTbl 
; IMPORT LLCode 
; IMPORT LLInit 
; IMPORT LLStack 
; IMPORT LLTypes 
(* ; FROM LLTypes IMPORT LLProcTyp , LLTypeTyp , LLVarTyp , CodeGenTyp 
TODO: ^Have to get the EXPORT of LLTypes out of here, before doing this. 
*) 
; IMPORT LLVM (* M3 binding to llvm Core, a C binding to some llvm things. *)  
; IMPORT LLVMTypes 
; IMPORT LLWr 

; IMPORT M3DIB (* M3 binding to LLVMDIBuilder, a C binding to llvm::DIBuilder. *) 
; IMPORT M3CG 
; IMPORT M3CG_Ops (* Contains REVEAL of M3CG.T. *) 
; IMPORT M3ID 
; IMPORT M3ObjFile 
; IMPORT NTObjFile (* Temporary. *) 
; IMPORT Target 
; FROM Target IMPORT M3BackendMode_t , BackendModeStrings 
; IMPORT TargetMap 
; IMPORT TFloat 
; IMPORT TIntN 
; IMPORT TWordN 
(* ; IMPORT Version This would no doubt create cyclic imports. *)  
 
; FROM TargetMap IMPORT CG_Bytes 
; FROM M3CG IMPORT Name , ByteOffset , TypeUID , CallingConvention 
; FROM M3CG IMPORT BitSize , ByteSize , Alignment , Frequency 
; FROM M3CG IMPORT Var , Proc , Label , No_label , Sign , BitOffset 
; FROM M3CG IMPORT ZType , AType , RType , IType , MType 
; FROM M3CG IMPORT CompareOp , ConvertOp , RuntimeError , MemoryOrder , AtomicOp 
; FROM M3CG_Ops IMPORT ErrorHandler 
; FROM M3ObjFile IMPORT Seg 
; FROM LLStack IMPORT MaxMin , ShiftType 
; FROM LLCode IMPORT Cond , Op , FOp , unscond , revcond , FloatBytes 

; TYPE CGType = M3CG . Type 
; TYPE CStringTyp = UNTRACED REF CHAR 
  (* Use this in place of Ctypes.[const_]char_star.  The latter points to a
     signed integer subrange that would require lots of tedious LOOPHOLEing. 
     The loophole happens anyway when our M3 binding links to C code.
  *) 

; TYPE RuntimeHook = REF RECORD name : Name ; proc : Proc ; END 

; CONST DW_LANG_Modula3 = 16_8000
(* FIXME: should be 16_17, but will have to alter code in llvm, 
   lib/IR/DIBuilder.cpp:101 to not assert against this. 
*)  
; CONST ProducerNameT = "CM3 Modula3 compiler, version " (* & Version . Text *) 

; REVEAL CodeGenTyp 
    = LLTypes . Public BRANDED "LLGen.U" OBJECT 
(* llvm: *) 
        llvmContextRef : LLVM . ContextRef := NIL 
      ; llvmModuleRef : LLVM . ModuleRef := NIL 
      ; LLTypeTbl : IntLLTypeTbl . T := NIL 
        (* UID -> LLTypes . LLTypeTyp *)  
      ; FormalTypesList : REF ARRAY OF LLTypeTyp := NIL 
      ; llvmTypeTbl : IntLLVMTypeRefTbl . T := NIL (* UID -> llvmTypeRef *)  
      ; diBuilderRef : M3DIB . DIBuilderRef := NIL
      ; EmptyStringSR : LLVMTypes . StringRef := LLVMTypes . StringRefNull 
      ; ModuleNameT : TEXT := NIL 
      ; ModuleNameSR : LLVMTypes . StringRef := LLVMTypes . StringRefNull 
      ; DataLayoutSR : LLVMTypes . StringRef := LLVMTypes . StringRefNull 
      ; TargetTripleSR : LLVMTypes . StringRef := LLVMTypes . StringRefNull 
      ; WidecharBitsize : INTEGER 
      ; FullSourceFileNameT : TEXT := NIL 
      ; BackendMode : M3BackendMode_t 

      ; llvmVoidTypeRef : LLVM . TypeRef := NIL 
      ; llvmFormalTypesList : REF ARRAY OF LLVM . TypeRef := NIL 
      ; diCompileUnit : M3DIB . LLVMDICompileUnit
      ; diFile : M3DIB . LLVMDIFile 

      (* Used during processing of a procedure signature: *) 
      ; CurrentSigProc : LLProcTyp := NIL 
      ; CurrentProc : LLProcTyp := NIL 
      ; CurrentFuncType : LLTypes . LLFuncTypeTyp := NIL 
      ; CurrentFormalsCt : INTEGER := 0  
      ; NextFormalSs := 0  

      (* Used during processing of a record or object type: *) 
      ; CurrentStructType : LLTypes . LLStructTypeTyp := NIL 
      ; NextFieldSs := 0 

      ; in_proc : BOOLEAN := FALSE 
(* end llvm *) 
 
      ; rawwr : Wr . T := NIL 
      ; wr : LLWr . T := NIL 
      ; cg : LLCode . T := NIL 
      ; vstack : LLStack . T := NIL 
      ; obj : M3ObjFile . T := NIL 
      ; debug := FALSE 
      ; Err : ErrorHandler := NIL 
      ; runtime : IntRefTbl . T := NIL 
      ; textsym : INTEGER 
      ; init_varstore : LLVarTyp := NIL 
      ; init_count : INTEGER 
 
      (* calls are never nested; results are stored in temporaries and repushed 
        F1(F2(F3()) compiles to 
          temp = F3() 
          temp = F2(temp) 
          F1(temp) 
          (whether it is the same temp, is a different matter; I don't know) 
        *) 
      ; call_param_size := ARRAY [ 0 .. 1 ] OF INTEGER { 0 , 0 } 
      ; in_proc_call : [ 0 .. 1 ] := 0 
      ; static_link := ARRAY [ 0 .. 1 ] OF LLVarTyp { NIL , NIL } 
      ; procframe_ptr : ByteOffset 
      ; exit_proclabel : Label := - 1 
      ; last_exitbranch := - 1 
      ; NextVarNo := 1 (* Also next formal, when processing a signature. *) 
      ; NextProcNo := 1 
      ; next_scope := 1 
      ; builtins : ARRAY Builtin OF LLProcTyp 
      ; global_var : LLVarTyp := NIL 
      ; lineno : INTEGER 
      ; reportlabel : Label 
      ; usedfault := FALSE 
(* NOT llvm: *) 
(* end NOT llvm *) 
 
      OVERRIDES 
        NewLLVar := NewLLVar 
      ; next_label := next_label 
      ; set_error_handler := set_error_handler 
      ; begin_unit := begin_unit 
      ; end_unit := end_unit 
      ; import_unit := import_unit 
      ; export_unit := export_unit 
      ; set_source_file := set_source_file 
      ; set_source_line := set_source_line 
      ; declare_typename := declare_typename 
      ; declare_array := declare_array 
      ; declare_open_array := declare_open_array 
      ; declare_enum := declare_enum 
      ; declare_enum_elt := declare_enum_elt 
      ; declare_packed := declare_packed 
      ; declare_record := declare_record 
      ; declare_field := declare_field 
      ; declare_set := declare_set 
      ; declare_subrange := declare_subrange 
      ; declare_pointer := declare_pointer 
      ; declare_indirect := declare_indirect 
      ; declare_proctype := declare_proctype 
      ; declare_formal := declare_formal 
      ; declare_raises := declare_raises 
      ; declare_object := declare_object 
      ; declare_method := declare_method 
      ; declare_opaque := declare_opaque 
      ; reveal_opaque := reveal_opaque 
      ; set_runtime_proc := set_runtime_proc 
      ; import_global := import_global 
      ; declare_segment := declare_segment 
      ; bind_segment := bind_segment 
      ; declare_global := declare_global 
      ; declare_constant := declare_constant 
      ; declare_local := declare_local 
      ; declare_param := declare_param 
      ; declare_temp := declare_temp 
      ; free_temp := free_temp 
      ; declare_exception := declare_exception 
      ; begin_init := begin_init 
      ; end_init := end_init 
      ; init_int := init_int 
      ; init_proc := init_proc 
      ; init_label := init_label 
      ; init_var := init_var 
      ; init_offset := init_offset 
      ; init_chars := init_chars 
      ; init_float := init_float 
      ; import_procedure := import_procedure 
      ; declare_procedure := declare_procedure 
      ; begin_procedure := begin_procedure 
      ; end_procedure := end_procedure 
      ; begin_block := begin_block 
      ; end_block := end_block 
      ; note_procedure_origin := note_procedure_origin 
      ; set_label := set_label 
      ; debug_set_label := debug_set_label 
      ; jump := jump 
      ; if_true := if_true 
      ; if_false := if_false 
      ; if_compare := if_compare 
      ; case_jump := case_jump 
      ; exit_proc := exit_proc 
      ; load := load 
      ; store := store 
      ; load_address := load_address 
      ; load_indirect := load_indirect 
      ; store_indirect := store_indirect 
      ; load_nil := load_nil 
      ; load_integer := load_integer 
      ; load_float := load_float 
      ; compare := compare 
      ; add := add 
      ; subtract := subtract 
      ; multiply := multiply 
      ; divide := divide 
      ; div := div 
      ; mod := mod 
      ; negate := negate 
      ; abs := abs 
      ; max := max 
      ; min := min 
      ; cvt_int := cvt_int 
      ; cvt_float := cvt_float 
      ; set_union := set_union 
      ; set_difference := set_difference 
      ; set_intersection := set_intersection 
      ; set_sym_difference := set_sym_difference 
      ; set_member := set_member 
      ; set_compare := set_compare 
      ; set_range := set_range 
      ; set_singleton := set_singleton 
      ; not := not 
      ; and := and 
      ; or := or 
      ; xor := xor 
      ; shift := shift 
      ; shift_left := shift_left 
      ; shift_right := shift_right 
      ; rotate := rotate 
      ; rotate_left := rotate_left 
      ; rotate_right := rotate_right 
      ; widen := widen 
      ; chop := chop 
      ; extract := extract 
      ; extract_n := extract_n 
      ; extract_mn := extract_mn 
      ; insert := insert 
      ; insert_n := insert_n 
      ; insert_mn := insert_mn 
      ; swap := swap 
      ; pop := pop 
      ; copy := copy 
      ; copy_n := copy_n 
      ; zero := zero 
      ; zero_n := zero_n 
      ; loophole := loophole 
      ; abort := abort 
      ; check_nil := check_nil 
      ; check_lo := check_lo 
      ; check_hi := check_hi 
      ; check_range := check_range 
      ; check_index := check_index 
      ; check_eq := check_eq 
      ; add_offset := add_offset 
      ; index_address := index_address 
      ; start_call_direct := start_call_direct 
      ; call_direct := call_direct 
      ; start_call_indirect := start_call_indirect 
      ; call_indirect := call_indirect 
      ; pop_param := pop_param 
      ; pop_struct := pop_struct 
      ; pop_static_link := pop_static_link 
      ; load_procedure := load_procedure 
      ; load_static_link := load_static_link 
      ; comment := comment 
      ; store_ordered := store_ordered 
      ; load_ordered := load_ordered 
      ; exchange := exchange 
      ; compare_exchange := compare_exchange 
      ; fence := fence 
      ; fetch_and_op := fetch_and_op 
      ; widechar_size := widechar_size 
      END 
(*---------------------------------------------------------------------------*) 
 
; CONST CompareOpName 
    = ARRAY CompareOp OF TEXT { " EQ" , " NE" , " GT" , " GE" , " LT" , " LE" } 
; CompareOpCond 
    = ARRAY CompareOp OF Cond 
        { Cond . E , Cond . NE , Cond . G , Cond . GE , Cond . L , Cond . LE } 
; CompareOpProc 
    = ARRAY [ CompareOp . GT .. CompareOp . LE ] OF Builtin 
        { Builtin . set_gt 
        , Builtin . set_ge 
        , Builtin . set_lt 
        , Builtin . set_le 
        } 
; CONST ConvertOpName 
    = ARRAY ConvertOp OF TEXT { " round" , " trunc" , " floor" , " ceiling" } 
; ConvertOpKind 
    = ARRAY ConvertOp OF FlToInt 
        { FlToInt . Round 
        , FlToInt . Truncate 
        , FlToInt . Floor 
        , FlToInt . Ceiling 
        } 
; CONST Alignmask 
    = ARRAY [ 1 .. 8 ] OF INTEGER 
              (* 1 => -1     2 => -2      3  4 => -4      5  6  7  8 => -8 *) 
        { 16_FFFFFFFF , 16_FFFFFFFE , 0 , 16_FFFFFFFC , 0 , 0 , 0 , 16_FFFFFFF8 } 

; CONST ResultLocalName = "_result" 
; VAR G_resultID : M3ID . T 

; PROCEDURE initGlobals ( ) 
  = BEGIN  
      G_resultID := M3ID . Add ( ResultLocalName ) 
    END initGlobals 

; PROCEDURE PrefixedName ( M3Name : M3ID . T ; Prefix : TEXT := "" ) 
  : M3ID . T 

  = VAR LText : TEXT 
  ; VAR LResultID : M3ID . T 

  ; BEGIN 
      LText := M3ID . ToText ( M3Name ) 
    ; LResultID := M3ID . Add ( Prefix & LText )
    ; RETURN LResultID  
    END PrefixedName 
 
(*---------------------------------------------------------------------------*) 

; PROCEDURE New 
    ( logfile : Wr . T ; BackendMode : M3BackendMode_t ) : CodeGenTyp 

  = VAR CodeGen : CodeGenTyp 
 
  ; BEGIN 
      IntType [ CGType . Int8 ] := Target . Int8 
    ; IntType [ CGType . Int16 ] := Target . Int16 
    ; IntType [ CGType . Int32 ] := Target . Int32 
    ; IntType [ CGType . Int64 ] := Target . Int64 
    ; IntType [ CGType . Word8 ] := Target . Word8 
    ; IntType [ CGType . Word16 ] := Target . Word16 
    ; IntType [ CGType . Word32 ] := Target . Word32 
    ; IntType [ CGType . Word64 ] := Target . Word64 
    ; TIntN . Init ( ) 

    ; CodeGen := NEW ( CodeGenTyp ) 
    ; CodeGen . BackendMode := BackendMode 
    ; CodeGen . runtime := NEW ( IntRefTbl . Default ) . init ( 20 ) 
    ; CodeGen . llvmContextRef := LLVM . LLVMContextCreate ( ) 
    ; CodeGen . LLTypeTbl 
        := NEW ( IntLLTypeTbl . Default ) . init ( 80 ) 
    ; CodeGen . llvmTypeTbl 
        := NEW ( IntLLVMTypeRefTbl . Default ) . init ( 80 ) 
    ; LLInit . InitBuiltinTypes 
        ( CodeGen . llvmTypeTbl , CodeGen . llvmContextRef ) 
    ; CodeGen . llvmVoidTypeRef 
        := LLVM . LLVMVoidTypeInContext ( CodeGen . llvmContextRef )  
    ; CodeGen . EmptyStringSR := LLVMTypes . CopyM3ToSR ( "" )
    ; CodeGen . DataLayoutSR := LLVMTypes . CopyM3ToSR ( DataLayout ( ) )
    ; CodeGen . TargetTripleSR := LLVMTypes . CopyM3ToSR ( TargetTriple ( ) )



    ; CodeGen . obj :=  NTObjFile . New ( )
      (* This is temporary, just to keep from getting constant NIL deref
         faults on CodeGen . obj.  It will all be removed someday. *) 
    ; IF logfile # NIL 
      THEN 
        CodeGen . debug := TRUE 
      ; CodeGen . wr := LLWr . New ( logfile ) 
      ELSE 
        CodeGen . wr := NIL 
      END 
    ; CodeGen . cg := LLCode . New ( CodeGen , CodeGen . wr ) 
    ; CodeGen . vstack 
        := LLStack . New ( CodeGen , CodeGen . cg , CodeGen . debug ) 
    ; FOR b := FIRST ( CodeGen . builtins ) TO LAST ( CodeGen . builtins ) 
      DO 
        CodeGen . builtins [ b ] := NIL 
      END 
    ; RETURN CodeGen 
    END New 

(* EXPORTED: *) 
; PROCEDURE CleanupCGAltogether 
    ( CodeGen : CodeGenTyp ) 

  = BEGIN
      LLVMTypes . FreeCopiedSR ( CodeGen . EmptyStringSR ) 
    ; LLVMTypes . FreeCopiedSR ( CodeGen . DataLayoutSR ) 
    ; LLVMTypes . FreeCopiedSR ( CodeGen . TargetTripleSR ) 
    ; LLVM . LLVMContextDispose ( CodeGen . llvmContextRef ) 
    END CleanupCGAltogether 
 
; PROCEDURE CleanupCGForCompUnit ( CodeGen : CodeGenTyp ) 

  = BEGIN
      LLVMTypes . FreeCopiedSR ( CodeGen . ModuleNameSR ) 
    ; LLVM . LLVMDisposeModule ( CodeGen . llvmModuleRef ) 
      (* Which, as I understand, will dispose fully deeply. *) 
    END CleanupCGForCompUnit 

(*----------- Building function types for procedures and proctypes ----------*) 

; PROCEDURE StartFuncType 
      ( FuncTypeUID : TypeUID 
      ; ResultTypeUID : TypeUID 
      ; ResultCGType : CGType 
      ; FormalsCt : CARDINAL 
      ; CC : CallingConvention 
      )
  : LLTypes . LLFuncTypeTyp 
 
  = VAR LocLLFuncType : LLTypes . LLFuncTypeTyp 

  ; BEGIN 
      LocLLFuncType := LLTypes . NewLLFuncType ( ) 
    ; LocLLFuncType . LLTCurrentChild := NIL 
    ; LocLLFuncType . LLTDepth := DepthInfinity 
    ; LocLLFuncType . LLTllvmTypeRef := NIL 
    ; LocLLFuncType . LLTypeUID := FuncTypeUID  
    ; LocLLFuncType . LLTIsComplete := FALSE 

    ; LocLLFuncType . FuncTypeResultTypeUID := ResultTypeUID 
    ; LocLLFuncType . FuncTypeResultCGType := CGType 
    ; LocLLFuncType . FuncTypeCallingConvention := CC  
    ; LocLLFuncType . FuncTypeResultLLType := NIL  
    ; LocLLFuncType . FuncTypeFormals 
        := LLTypes . NewLLFormalArray ( FormalsCt ) 
    ; LocLLFuncType . FuncTypeHasResultFormal : BOOLEAN := FALSE 
    ; CodeGen . CurrentFuncType := LocLLFuncType 
    ; CodeGen . CurrentFormalsCt := FormalsCt 
    ; CodeGen . NextFormalSs := 0   
    END StartFuncType 

; PROCEDURE AddFuncTypeFormal 
    ( CodeGen : CodeGenTyp 
    ; FormalName : Name 
    ; FormalTypeUID : TypeUID 
    ; FormalCGType : CGType 
    ) 
  : LLTypes . LLFormalTyp 

  = VAR LocLLFormal : LLTypes . LLFormalTyp 

  ; BEGIN
      LocLLFormal := LLTypes . NewLLFormal ( )  
    ; LocLLFormal . FormalName := FormalName   
    ; LocLLFormal . FormalTypeUID := FormalTypeUID  
    ; LocLLFormal . FormalCGType := FormalCGType  
    ; LocLLFormal . FormalType := LLTypeOfUID ( CodeGen , FormalTypeUID ) 
    ; WITH WFormal = CodeGen . CurrentFuncType . FuncTypeFormals ^ 
                     [ CodeGen . NextFormalSs ]
      DO
        <* ASSERT WFormal = NIL *> 
        WFormal := LocLLFormal 
      END (* WITH *) 
    ; INC ( CodeGen . NextFormalSs ) 
    ; RETURN LocLLFormal 
    END AddFuncTypeFormal 

; PROCEDURE FinishFuncType ( CodeGen : CodeGenTyp ) 

  = BEGIN 
      IF CodeGen . CurrentFuncType # NIL 
      THEN (* We have been building the formals/locals/result of a previously 
              declared function type9.  Close it out. *) 
        LProc := CodeGen . CurrentSigProc 
      ; IF LProc . ProcLLFuncType . FuncTypeResultLLType = NIL 
        THEN (* There was no formal or local named ResultLocalName, 
                => this is a proper procedure. *) 
        ELSIF LProc . ProcLLFuncType . FuncTypeHasResultFormal 
        THEN (* It's an M3 function lowered to return its result in an added
                parameter, passed by reference. *) 
        ELSE (* It's a function that returns its result using the obvious 
                value return mechanism for scalar result type. *) 
        END (* IF *) 

      END (* IF *) 
    END FinishFuncType

; PROCEDURE StartProcSignature ( CodeGen : CodeGenTyp ; ) 

(* Complete this, to be done sufficiently late. *)  
; PROCEDURE FinishProcSignature ( CodeGen : CodeGenTyp ) 

  = VAR LProc : LLProcTyp
  ; VAR LFormalVar : LLVarTyp 
  ; VAR LllvmFuncTypeRef : LLVM . TypeRef  
  ; VAR LllvmFormalValueRef : LLVM . ValueRef 

  ; BEGIN
    (* We have seen all of the procedure signature. *)  
      LProc := CodeGen . CurrentSigProc 
    ; IF LProc . ProcLLFuncType . FuncTypeResultLLType = NIL 
      THEN (* There was no formal or local named ResultLocalName, 
              => this is a proper procedure. *) 
      ELSIF LProc . ProcLLFuncType . FuncTypeHasResultFormal 
      THEN (* It's an M3 function lowered to return its result in an added
              parameter, passed by reference. *) 
      ELSE (* It's a function that returns its result using the obvious 
              value return mechanism for scalar result type. *) 
      END (* IF *) 
(* Complete this, to be done sufficiently late. 
    ; IF CodeGen . CurrentFormalsCt = 0 
      THEN 
        LllvmFuncTypeRef 
          := LLVM . LLVMFunctionTypeSafe
               ( LProc . ProcLLFuncType . FuncTypeResultLLType  
               , NIL 
               , CodeGen . CurrentFormalsCt 
               )
      ELSE
        <* ASSERT NUMBER ( CodeGen . FormalTypesList ^ ) 
                  = CodeGen . CurrentFormalsCt *>  
        LllvmFuncTypeRef 
          := LLVM . LLVMFunctionTypeSafe
               ( LProc . ProcLLFuncType . FuncTypeResultLLType  
               , CodeGen . llvmFormalTypesList ^ [ 0 ]  
               , CodeGen . CurrentFormalsCt 
               )
      END 
    ; <*ASSERT LllvmFuncTypeRef # NIL*> 
      LProc . llvmFunctionRef 
        := LLVM . LLVMAddFunctionCHAR 
             ( CodeGen . llvmModuleRef 
             , M3ID . ToStr ( LProc . ProcName ) 
             , LllvmFuncTypeRef 
             ) 
    ; <*ASSERT LProc . llvmFunctionRef # NIL*> 
      FOR RI := 0 TO CodeGen . CurrentFormalsCt - 1 
      DO 
        LFormalVar := LProc . ProcFormalsList ^ [ RI ] 
      ; LllvmFormalValueRef 
          := LLVM . LLVMGetParam ( LProc . llvmFunctionRef , RI ) 
      ; LLVM . LLVMSetValueNameCHAR 
          ( LllvmFormalValueRef , M3ID . ToStr ( LFormalVar . VarllvmName ) ) 
      END (* FOR *) 
*) 
    END FinishProcSignature 

; PROCEDURE AddFuncResultTypeUID 
    ( CodeGen : CodeGenTyp ; ResultTypeUID : TypeUID )

  = BEGIN 
      CodeGen . CurrentFuncType .  FuncTypeResultTypeUID := ResultTypeUID
    END AddFuncResultTypeUID
 
(*-------------------------------------------------------- Type utilities. --*) 
 
; PROCEDURE LLTypeOfUID ( CodeGen : CodeGenTyp ; RefdUID : TypeUID ) 
  : LLTypes . LLTypeTyp 
  (* The real type corresponding to RefdUID, if it is declared.  Otherwise,
    a forward type reference node. 
  *) 

  = VAR LocExistingLLType : LLTypes . LLTypeTyp  
  ; VAR LocLLTypeFwd : LLTypes . LLTypeFwdRefTyp  

  ; BEGIN 
      IF CodeGen . LLTypeTbl . get ( RefdUID , LocExistingLLType ) 
      THEN (* UID has been seen. *)
        TYPECASE LocExistingLLType 
        OF NULL => <* ASSERT FALSE *>  
        | LLTypes . LLTypeFwdRefTyp ( TFwdRef ) 
        => (* A subsequent reference to a once forward referenced UID. *) 
          <* ASSERT TFwdRef . LLTFwdResolvedType = NIL *>  
          RETURN TFwdRef 
        ELSE (* It is declared. *) 
          RETURN LocExistingLLType 
        END (* TYPECASE *)  
      ELSE (* RefdUID is undeclared, this is its first reference. *) 
        LocLLTypeFwd := LLTypes . NewLLTypeFwdRef ( ) 
      ; LocLLTypeFwd . LLTypeUID := RefdUID
      ; LocLLTypeFwd . LLTFwdResolvedType := NIL  
      ; EVAL CodeGen . LLTypeTbl . put ( RefdUID , LocLLTypeFwd )
      ; RETURN LocLLTypeFwd 
      END (* IF *) 
    END LLTypeOfUID 

; PROCEDURE DeclareUID ( CodeGen : CodeGenTyp ; LLType : LLTypes . LLTypeTyp ) 
  (* PRE: NOT ISTYPE ( LLType , LLTypes . LLTypeFwdRefTyp ) *) 

  = VAR LocExistingLLType : LLTypes . LLTypeTyp  

  ; BEGIN 
      IF CodeGen . LLTypeTbl . get ( LLType . LLTypeUID , LocExistingLLType ) 
      THEN (* UID has been seen. *)
        TYPECASE LocExistingLLType 
        OF NULL => <* ASSERT FALSE *>  
        | LLTypes . LLTypeFwdRefTyp ( TFwdRef ) 
        => (* This declaration resolves forward references. *) 
          TFwdRef . LLTFwdResolvedType := LLType 
        ; EVAL CodeGen . LLTypeTbl . put ( LLType . LLTypeUID , LLType )
        ELSE (* Redeclaration *) 
          <* ASSERT FALSE *> 
        END (* TYPECASE *)         
      ELSE (* UID has never been seen. *) 
        EVAL CodeGen . LLTypeTbl . put ( LLType . LLTypeUID , LLType )
      END (* IF *) 
    END DeclareUID 

; PROCEDURE LookThruLLType ( LLType : LLTypes . LLTypeTyp ) 
  : LLTypes . LLTypeTyp

  = BEGIN 
      TYPECASE LLType 
      OF NULL => <* ASSERT FALSE *>  
      | LLTypes . LLTypeFwdRefTyp ( TFwdRef ) 
      => RETURN TFwdRef . LLTFwdResolvedType 
                (* Could be NIL, if not yet declared. *) 
      ELSE RETURN LLType 
      END (* TYPECASE *) 
    END LookThruLLType 

; EXCEPTION CTBackout 
  (* Use this for the back out arc from VisitChild to Traverse, where 
     the path is static. *) 

; PROCEDURE CompleteLlvmType ( CodeGen : CodeGenTyp ; RootLLNode : LLTypeTyp ) 
  (* POST: The llvm TypeRef is built for RootLLNode. *) 

  = VAR CTBackoutNode : LLTypeTyp 
        (* ^If non-NIL, we are backing out to this node... *) 

  ; PROCEDURE Traverse ( TravLLNode : LLTypeTyp ; Depth : CARDINAL )
    (* PRE: TravLLNode # NIL. 
       PRE: Depth <= LLTypes . DepthInfinity.
       POST: CTBackoutNode # NIL OR TravLLNode . TTLllvmTypeRef # NIL  
    *) 

    = PROCEDURE VisitChild ( ChildLLNode : LLTypeTyp ) 
      RAISES { CTBackout } 
      (* PRE: ChildLLNode # NIL *)  
      (* POST: Either raises CTBackout, with CTBackoutNode # NIL 
               OR returns with ChildLLNode . LLTllvmTypeRef # NIL 
      *) 

      = BEGIN 
          <* ASSERT ChildLLNode # NIL *> 
          TravLLNode . LLTCurrentChild := ChildLLNode 
        ; TravLLNode . LLTDepth := Depth 
        ; <* ASSERT Depth + 1 < LLTypes . DepthInfinity *>
          (* ^An unlikely overflow case. *)   
          Traverse ( ChildLLNode , Depth + 1 ) 
        ; IF CTBackoutNode # NIL  
          THEN (* We are backing out, after allocating a pseudo struct. *) 
            IF TravLLNode = CTBackoutNode 
            THEN (* The backout stops here.  Resume normal traversal. *) 
              <* ASSERT TravLLNode . LLTllvmTypeRef # NIL *> 
              <* ASSERT ISTYPE ( TravLLNode , LLTypes . LLPtrTypeTyp ) *> 
              CTBackoutNode := NIL 
            ; TravLLNode . LLTCurrentChild := NIL 
            ; TravLLNode . LLTDepth := LLTypes . DepthInfinity 
            ELSE (* Keep backing out. *) 
              RAISE CTBackout    
            END (* IF *) 
          ELSE (* Ordinary return. *)   
            <* ASSERT TravLLNode . LLTllvmTypeRef # NIL *> 
            TravLLNode . LLTCurrentChild := NIL 
          ; TravLLNode . LLTDepth := LLTypes . DepthInfinity  
          END (* IF *) 
        END VisitChild 

    ; VAR LocCt , LocI : CARDINAL 
    ; VAR LocLLNode : LLTypeTyp 
    ; VAR LocChildType : LLTypes . LLTypeTyp 
    ; VAR LocLlvmTypes : REF ARRAY OF LLVM . TypeRef := NIL 
    ; VAR LocChildllvmTypeRef : LLVM . TypeRef 

    ; BEGIN (* Traverse *) 
        TRY 
          IF TravLLNode . LLTllvmTypeRef # NIL   
          THEN (* This node is already allocated. *) (* Allow to RETURN *) 
          ELSIF TravLLNode . LLTDepth < Depth 
          THEN (* We visited this node earlier.  A cycle closes here. *) 
            (* Starting here, at the beginning node of the cycle, start a 
               second lap through the nodes, in the same direction, looking 
               for one that can be allocated.  This is simple enough for a
               loop, rather than recursion. *) 
            LocLLNode := TravLLNode 
          ; LOOP 
              TYPECASE LocLLNode OF
                NULL => <* ASSERT FALSE *> 

              | LLTypes . LLPtrTypeTyp ( TPtr )
              => <* ASSERT TPtr . LLTllvmTypeRef = NIL *>  
                 (* Else would have stopped traversal the first lap around 
                    the cycle. *)
                TPtr . LLTllvmTypeRef 
                  := LLVM . LLVMStructCreateNamed 
                       ( CodeGen . llvmContextRef , LocLLNode . LLTypeName ) 
(* Complete: convert the type name, use it, free it. *) 
              ; TPtr . LLTPtrIsPseudoStruct := TRUE 
              ; CTBackoutNode := LocLLNode (* Back out around to this node. *) 
              ; RETURN 

              | LLTypes . LLStructTypeTyp 
              => <* ASSERT FALSE *>  
                 (* Would have stopped traversal the first lap around the 
                    cycle. *)

              ELSE (* Not a type we are looking for. *) 
                <* ASSERT LocLLNode . LLTllvmTypeRef = NIL *> 
                (* Else would have stopped traversal the first lap around the 
                   cycle. *)
                LocLLNode := LocLLNode . LLTCurrentChild 
              ; <* ASSERT LocLLNode # TravLLNode  *> 
                (* Front end should prevent cycles with no pointer. *) 
              (* And loop to continue the second lap around the cycle. *) 
              END (* TYPECASE *)    
            END (* LOOP *) 

          ELSE (* Unallocated node, not previously visited. *) 

            TYPECASE TravLLNode 
            OF LLTypes . LLStructTypeTyp ( TStruct ) 
            => <* ASSERT TStruct . LLTllvmTypeRef # NIL *> 
               (* We always preallocate structs when first built. *) 
              <* ASSERT TravLLNode . LLTDepth = LLTypes . DepthInfinity *> 
              (* A struct never participates in a cycle. *) 
              IF TStruct . StructTypeFields = NIL 
              THEN LocCt := 0 
              ELSE LocCt := NUMBER ( TStruct . StructTypeFields ^ ) 
              END (* IF *) 
            ; IF LocCt > 0 
              THEN LocLlvmTypes := LLTypes . NewTypeRefArray ( LocCt )  
              END (* IF *) 
            ; LocI := 0 
            ; LOOP 
                IF LocI >= LocCt 
                THEN (* Done, all children were already allocated.  Sieze the
                        opportunity to complete the struct now.  *) 
                  LLVM . LLVMStructSetBodySafe 
                    ( TStruct . LLTllvmTypeRef 
                    , LocLlvmTypes ^ [ 0 ]   
                    , LocCt 
                    , LLVM . True (* Packed. *) 
                    ) 
                ; TravLLNode . LLTIsComplete := TRUE 
                ; EXIT 
                ELSE
                  LocChildType 
                    := TStruct . StructTypeFields ^ [ LocI ] . FieldType 
                ; IF LocChildType . LLTllvmTypeRef = NIL
                  THEN EXIT (* An unallocated child.  Give up. *) 
                  ELSE 
                    LocLlvmTypes ^ [ LocI ] := LocChildType . LLTllvmTypeRef  
                  ; INC ( LocI ) 
                  (* Allow to LOOP. *) 
                  END (* IF *) 
                END (* IF *) 
              END (* LOOP *)   

            | LLTypes . LLFuncTypeTyp ( TFunc ) 
            => VisitChild ( TFunc . FuncTypeResultLLType ) 
               (* ^Could raise CTBackout. *) 
            ; IF TFunc . FuncTypeFormals = NIL 
              THEN LocCt := 0 
              ELSE LocCt := NUMBER ( TFunc . FuncTypeFormals ^ ) 
              END (* IF *) 
            ; IF LocCt > 0 
              THEN LocLlvmTypes := LLTypes . NewTypeRefArray ( LocCt )  
              END (* IF *) 
            ; FOR RI := 0 TO LocCt - 1 
              DO LocChildType := TFunc . FuncTypeFormals ^ [ RI ] . FormalType  
              ; VisitChild ( LocChildType ) (* Could raise CTBackout. *)
              ; <* ASSERT LocChildType . LLTllvmTypeRef # NIL *>
                LocLlvmTypes ^ [ RI ] := LocChildType . LLTllvmTypeRef 
              END (* FOR *) 
            ; TFunc . LLTllvmTypeRef 
                := LLVM . LLVMFunctionTypeSafe 
                     ( TFunc . FuncTypeResultLLType . LLTllvmTypeRef 
                     , (* READONLY *) LocLlvmTypes ^ [ 0 ] 
                     , LocCt 
                     ) 
            ; TFunc . LLTIsComplete := TRUE 

            | LLTypes . LLArrayTypeTyp ( TArray ) 
            => VisitChild ( TArray . LLTArrayElementType ) 
               (* ^Could raise CTBackout. *)
            ; TArray . LLTllvmTypeRef 
                := LLVM . LLVMArrayType 
                     ( TArray . LLTArrayElementType . LLTllvmTypeRef 
                     , TArray . LLTArrayElemCt 
                     ) 
            ; TArray . LLTIsComplete := TRUE 

            | LLTypes . LLPtrTypeTyp ( TPtr ) 
            => LocChildllvmTypeRef 
                 := TPtr . LLTPtrReferentType . LLTllvmTypeRef 
            ; IF TPtr . LLTPtrIsPseudoStruct 
              THEN 
                IF LocChildllvmTypeRef # NIL 
                THEN (* Seize the opportunity to complete this pseudo struct. *)
                  LLVM .  LLVMStructSetBodySafe
                    ( TPtr . LLTllvmTypeRef 
                    , (* READONLY *) LocChildllvmTypeRef  
                    , 1 (* One struct member. *) 
                    , LLVM . True (* Packed. *) 
                    ) 
                ; TPtr . LLTIsComplete := TRUE 
                END (* IF *) 
              ELSE (* It's a true pointer, so far. *) 
                IF LocChildllvmTypeRef # NIL 
                THEN (* Can complete it as a true llvm pointer. *) 
                  TPtr . LLTllvmTypeRef 
                    := LLVM . LLVMPointerType ( LocChildllvmTypeRef ) 
                ; TPtr . LLTIsComplete := TRUE 
                ELSE 
                  VisitChild ( TPtr . LLTPtrReferentType ) 
                  (* ^Could raise CTBackout. *)
                ; IF TPtr . LLTPtrIsPseudoStruct
                  THEN (* It turned into a pseudo struct, to break a cycle. *) 
                    (* Leave it to be completed later. *) 
                    <* ASSERT TPtr . LLTllvmTypeRef # NIL *> 
                  ELSE (* Now its referent will be allocated.  Complete it. *) 
                    LocChildllvmTypeRef 
                      := TPtr . LLTPtrReferentType . LLTllvmTypeRef 
                  ; <* ASSERT LocChildllvmTypeRef = NIL *> 
                    TPtr . LLTllvmTypeRef 
                      := LLVM . LLVMPointerType ( LocChildllvmTypeRef ) 
                  ; TPtr . LLTIsComplete := TRUE 
                  END (* IF *) 
                END (* IF *)  
              END (* IF *) 
            ELSE 
              <* ASSERT FALSE *> 
              (* Other types are primitive.  They are always built complete; 
                 we won't get here. *)
            END (* TYPECASE *) 
          END (* IF *) 
        EXCEPT CTBackout => (* Just return. *) 
          <* ASSERT CTBackoutNode # NIL *>
        END (* EXCEPT *) 
      END Traverse 

  ; PROCEDURE TopLevelComplete ( ) 

    = VAR LocCt : CARDINAL 
    ; VAR LocChildType : LLTypes . LLTypeTyp 
    ; VAR LocLlvmTypes : REF ARRAY OF LLVM . TypeRef 
    ; VAR LocChildllvmTypeRef : LLVM . TypeRef 

    ; BEGIN (* TopLevelComplete *) 
      TYPECASE RootLLNode 
      OF LLTypes . LLStructTypeTyp ( TStruct ) 
      => IF NOT TStruct . LLTIsComplete 
        THEN 
          IF TStruct . StructTypeFields = NIL 
          THEN LocCt := 0 
          ELSE LocCt := NUMBER ( TStruct . StructTypeFields ^ ) 
          END (* IF *) 
        ; IF LocCt > 0 
          THEN LocLlvmTypes := LLTypes . NewTypeRefArray ( LocCt )  
          END (* IF *) 
        ; FOR RI := 0 TO LocCt - 1 
          DO LocChildType := TStruct . StructTypeFields ^ [ RI ] . FieldType 
          ; Traverse ( LocChildType , Depth := 0 ) 
          ; <* ASSERT CTBackoutNode = NIL *>
            <* ASSERT LocChildType . LLTllvmTypeRef # NIL *>   
            LocLlvmTypes ^ [ RI ] := LocChildType . LLTllvmTypeRef  
          END (* FOR *) 
        ; LLVM . LLVMStructSetBodySafe 
            ( TStruct . LLTllvmTypeRef 
            , LocLlvmTypes ^ [ 0 ]   
            , LocCt 
            , LLVM . True (* Packed. *) 
            ) 
        ; RootLLNode . LLTIsComplete := TRUE 
        END (* IF*) 

      | LLTypes . LLPtrTypeTyp ( TPtr ) 
      => IF NOT TPtr . LLTIsComplete 
        THEN 
          <* ASSERT TPtr . LLTPtrIsPseudoStruct *> 
          Traverse ( TPtr . LLTPtrReferentType , Depth := 0 )
        ; <* ASSERT CTBackoutNode = NIL *>
          LocChildllvmTypeRef := TPtr . LLTPtrReferentType . LLTllvmTypeRef 
        ; <* ASSERT LocChildllvmTypeRef # NIL *> 
          LLVM . LLVMStructSetBodySafe 
            ( TPtr . LLTllvmTypeRef 
            , (* READONLY *) LocChildllvmTypeRef  
            , 1 (* One struct member. *) 
            , LLVM . True (* Packed. *) 
            ) 
        ; RootLLNode . LLTIsComplete := TRUE 
        END (* IF *) 
      ELSE (* Of TYPECASE *)
        <* ASSERT RootLLNode . LLTIsComplete *> 
      END (* TYPECASE *) 
    END TopLevelComplete 

  ; BEGIN (* CompleteLlvmType *) 
      CTBackoutNode := NIL  
    ; Traverse ( RootLLNode , Depth := 0 )
    ; <* ASSERT CTBackoutNode = NIL *>
      <* ASSERT RootLLNode . LLTllvmTypeRef # NIL *> 
      TopLevelComplete ( ) 
    END CompleteLlvmType 

(*----------------------------------------------------------- ID counters ---*) 
 
; PROCEDURE next_label ( CodeGen : CodeGenTyp ; n : INTEGER := 1 ) : Label 
  = BEGIN 
      RETURN CodeGen . cg . reserve_labels ( n ) 
    END next_label 
 
(*------------------------------------------------ READONLY configuration ---*) 
 
; PROCEDURE SuppressErr ( msg : TEXT ) 

  = BEGIN 
      Error . Warn ( 2 , msg ) 
    END SuppressErr 

; PROCEDURE set_error_handler ( CodeGen : CodeGenTyp ; p : ErrorHandler ) 
  = BEGIN 
      IF p = Error . Msg 
      THEN p := SuppressErr 
      END (* IF *) 
(* REMOVEME ^This is temporary, until we get unnecessary stuff stripped out. *) 
    ; CodeGen . Err := p 
    ; CodeGen . cg . set_error_handler ( p ) 
    ; CodeGen . vstack . set_error_handler ( p ) 
    END set_error_handler 

(*------------------------------------------------ Disposing C/C++ memory ---*)

(*----------------------------------------------------- Utility. ------------*) 

; PROCEDURE SplitDirFile 
     ( FullFileNameT : TEXT ; VAR (*OUT*) DirNameT , FileNameT : TEXT ) 
  (* Split FullFileNameT into a directory name and a simple file name. *) 

(* TODO: Surely, this has to duplicate something, somewhere that we could use. *)

  = VAR LLen , LSlashPos : INTEGER 

  ; BEGIN
      LLen := Text . Length ( FullFileNameT ) 
    ; LSlashPos := Text . FindCharR ( FullFileNameT , '/' ) 
    ; IF LSlashPos = - 1 (* No slash found. *) 
      THEN (* Try a backslash. *) 
        LSlashPos := Text . FindCharR ( FullFileNameT , '\\' ) 
      END  
    ; IF LSlashPos = - 1 
      THEN (* No path delimiter. *) 
        DirNameT := "" 
      ; FileNameT := FullFileNameT 
      ELSE
        DirNameT := Text . Sub ( FullFileNameT , 0 , LSlashPos ) 
      ; FileNameT := Text . Sub ( FullFileNameT , LSlashPos , LLen ) 
      END (* IF *) 
    END SplitDirFile 

(*----------------------------------------------------- compilation units ---*) 

; VAR DataLayoutMangling : BOOLEAN := FALSE 

; PROCEDURE DataLayout ( ) : TEXT 

  = CONST EndianMap = ARRAY BOOLEAN (* Little_endian *) OF TEXT { "E" , "e" } 

  ; VAR LResult : TEXT := "" 
  ; VAR LStackSize : CARDINAL 

  ; BEGIN 
    (* Endianness: *) 
      LResult := LResult & EndianMap [ Target . Little_endian ] 

    (* Max stack alignment. *)  
    ; LStackSize := MAX ( Target . Integer . size , Target . Address . size )
    ; LStackSize := ( ( LStackSize + 31 ) DIV 32 ) * 32 
    ; LResult := LResult & "-S" 
    ; LResult := LResult & Fmt . Int ( LStackSize ) 

    (* Pointer properties: *) 
    ; LResult := LResult & "-p:"  
    ; LResult := LResult & Fmt . Int ( Target . Address . size ) 
    ; LResult := LResult & ":"  
    ; LResult := LResult & Fmt . Int ( Target . Address . align ) 

    (* 64-bit int: *) 
    ; LResult := LResult & "-i64:64:64"  

    (* Aggregates: *) 
    ; LResult := LResult & "-a:8" 

    (* Mangling: *) 
    ; (* Not accepted by 3.4.2: *) 
      IF DataLayoutMangling 
      THEN 
        IF TextUtils . StartsWith ( Target . System_name , "MIPS" )
        THEN LResult := LResult & "-m:m" 
        ELSIF TextUtils . EndsWith ( Target . System_name , "NT" )
        THEN LResult := LResult & "-m:w" 
        ELSIF TextUtils . StartsWith ( Target . System_name , "NT" )
        THEN LResult := LResult & "-m:w" 
        ELSIF TextUtils . StartsWith ( Target . System_name , "MACH" )
  (* TODO: ^If we ever get a Mach-O target, be sure this is the right spelling. *) 
        THEN LResult := LResult & "-m:o" 
        ELSE LResult := LResult & "-m:e" 
        END 
      END 

    (* Native integer widths: *) 
    ; IF TextUtils . StartsWith ( Target . System_name , "PPC64" )
      THEN LResult := LResult & "-n32:64" 
      ELSIF TextUtils . StartsWith ( Target . System_name , "PPC" )
      THEN LResult := LResult & "-n64" 
      ELSE LResult := LResult & "-n8:16:32:64" 
      END (* IF *) 
    ; RETURN LResult 
    END DataLayout 
 
; PROCEDURE TargetTriple ( ) : TEXT 
  = BEGIN 
      RETURN "x86_64--Linux-ELF" 
   (* RETURN Target . System_name *) 
(* TODO: ^Make this match what llvm expects. *) 
    END TargetTriple  
 
; PROCEDURE begin_unit ( CodeGen : CodeGenTyp ; optimize : INTEGER ) 
  (* called before any other method to initialize the compilation unit *) 

  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "begin_unit" ) 
      ; CodeGen . wr . Int ( optimize ) 
      ; CodeGen . wr . NL ( ) 
      END 
    ; CodeGen . llvmModuleRef 
        := LLVM . LLVMModuleCreateWithName ( CodeGen . EmptyStringSR . Data ) 
    ; CodeGen . diBuilderRef 
        := M3DIB . DIBBuilderCreate ( CodeGen . llvmModuleRef ) 
    ; LLVM . LLVMSetDataLayout 
        ( CodeGen . llvmModuleRef , CodeGen . DataLayoutSR . Data ) 
    ; LLVM . LLVMSetTarget 
        ( CodeGen . llvmModuleRef , CodeGen . TargetTripleSR . Data ) 
    ; CodeGen . FullSourceFileNameT := NIL 
    ; CodeGen . CurrentSigProc := NIL (* Denotes not inside a signature. *)  
    ; CodeGen . CurrentProc := NIL (* Denotes not inside a procedure. *)  

    ; CodeGen . cg . set_obj ( CodeGen . obj ) 
    ; CodeGen . cg . init ( ) 
    ; CodeGen . vstack . init ( ) 
    ; CodeGen . NextVarNo := 1 
    ; CodeGen . NextVarNo := 1 
    ; CodeGen . next_scope := 1 
    ; CodeGen . global_var := NIL 
    ; CodeGen . in_proc_call := 0 
    ; CodeGen . reportlabel := CodeGen . cg . reserve_labels ( 1 ) 
    ; CodeGen . usedfault := FALSE 
 
    ; FOR b := FIRST ( CodeGen . builtins ) TO LAST ( CodeGen . builtins ) 
      DO 
        CodeGen . builtins [ b ] := NIL 
      END 
 (* llvm remove: 
    ; CodeGen . textsym 
        := CodeGen . obj . define_symbol 
             ( M3ID . Add ( "TextSegment" ) , Seg . Text , 0 ) 
    ; CodeGen . cg . set_textsym ( CodeGen . textsym ) 
 end llvm remove *) 
 
    END begin_unit 
 
; PROCEDURE end_unit ( CodeGen : CodeGenTyp ) 
  (* called after all other methods to finalize the unit and write the 
     resulting object *) 
  = VAR LBcFileNameT : TEXT 
  ; VAR LBcFileNameSR : LLVMTypes . StringRef 
  ; VAR LCode : INTEGER 

  ; <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      IF CodeGen . usedfault 
      THEN 
        makereportproc ( CodeGen ) 
      END 
 
    ; IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "end_unit" ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . end ( ) 
    ; CodeGen . cg . end ( )
 
    ; CASE CodeGen . BackendMode 
      OF M3BackendMode_t . ExtLlvmObj , M3BackendMode_t . ExtLlvmAsm 
      => M3DIB . DIBfinalize ( CodeGen . diBuilderRef ) 
      (* Write bitcode. *) 
      ; LBcFileNameT := BitcodeFileNameTOfUnitText ( CodeGen . ModuleNameT ) 
      ; LBcFileNameSR := LLVMTypes . CopyM3ToSR ( LBcFileNameT ) 
      ; LCode 
          := LLVM . LLVMWriteBitcodeToFile 
               ( CodeGen . llvmModuleRef , LBcFileNameSR . Data ) 
(* TODO: There is some finalize llvm function to call, maybe here. *) 
      ; IF LCode # 0 
        THEN
          Wr . PutText ( Stdio . stderr , "Failure writing bitcode file \"" ) 
        ; Wr . PutText ( Stdio . stderr , LBcFileNameT ) 
        ; Wr . PutText ( Stdio . stderr , "\", error code: " ) 
        ; Wr . PutText ( Stdio . stderr , Fmt . Int ( LCode ) ) 
        ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
        END (* IF *) 
      | M3BackendMode_t . IntLlvmObj , M3BackendMode_t . IntLlvmAsm 
      => Wr . PutText ( Stdio . stderr , "Unimplemented llvm backend mode: " ) 
      ; Wr . PutText 
          ( Stdio . stderr , BackendModeStrings [ CodeGen . BackendMode ] ) 
      ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
      ELSE 
        Wr . PutText ( Stdio . stderr , "LLGen got a non-llvm backend mode: " ) 
      ; Wr . PutText 
          ( Stdio . stderr , BackendModeStrings [ CodeGen . BackendMode ] ) 
      ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
      END (* CASE *)

    ; LLVMTypes . FreeCopiedSR ( LBcFileNameSR ) 
    ; CleanupCGForCompUnit ( CodeGen )  
    END end_unit 

; PROCEDURE import_unit ( CodeGen : CodeGenTyp ; n : Name ) 
        (* note that the current compilation unit imports the interface 'n' *) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "import_unit" ) 
      ; CodeGen . wr . ZName ( n ) 
      ; CodeGen . wr . NL ( ) 
      END 
    END import_unit 
 
 
; PROCEDURE export_unit ( CodeGen : CodeGenTyp ; n : Name ) 
    (* note that the current compilation unit exports the interface 'n' *) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "export_unit" ) 
      ; CodeGen . wr . ZName ( n ) 
      ; CodeGen . wr . NL ( ) 
      END 
    END export_unit 
 
(*------------------------------------------------ debugging line numbers ---*) 
 
; PROCEDURE set_source_file ( CodeGen : CodeGenTyp ; FullFileNameT : TEXT ) 
  (* PRE: FullFileName # NIL. *) 
  (* Set the current source file name.  Subsequent statements 
     and expressions are associated with this source location. *) 
  = VAR LDirNameT , LFileNameT : TEXT 
  ; VAR LDirNameSR , LFileNameSR , LProducerSR : LLVMTypes . StringRef 

  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . OutT ( "set_source_file                 -----FILE " ) 
      ; CodeGen . wr . OutT ( FullFileNameT ) 
      ; CodeGen . wr . OutT ( "  -----" ) 
      ; CodeGen . wr . NL ( ) 
      END 
    ; <* ASSERT FullFileNameT # NIL *> 
      IF CodeGen . FullSourceFileNameT = NIL 
      THEN (* This is the first we've seen of the file name. *)  
        CodeGen . FullSourceFileNameT := FullFileNameT 
      ; SplitDirFile ( FullFileNameT , (*OUT*) LDirNameT , (*OUT*) LFileNameT ) 
      ; LDirNameSR := LLVMTypes . CopyM3ToSR ( LDirNameT )
      ; LFileNameSR := LLVMTypes . CopyM3ToSR ( LFileNameT ) 
      ; LProducerSR := LLVMTypes . CopyM3ToSR ( ProducerNameT ) 
      ; CodeGen . diFile 
          := M3DIB . DIBcreateFile 
               ( CodeGen . diBuilderRef , LDirNameSR , LFileNameSR )
      ; CodeGen . diCompileUnit 
          := M3DIB . DIBcreateCompileUnit 
               ( CodeGen . diBuilderRef  
               , DW_LANG_Modula3 
               , LFileNameSR 
               , LDirNameSR 
               , LProducerSR 
               , isOptimized := LLVMTypes . false 
               , Flags := CodeGen . EmptyStringSR 
(* TODO: ^Put something in here. *) 
               , RV := 2 (* Runtime version. (cm3 runtime?) *) 
               , SplitName := CodeGen . EmptyStringSR 
               ) 
      ; LLVMTypes . FreeCopiedSR ( LDirNameSR ) 
      ; LLVMTypes . FreeCopiedSR ( LFileNameSR ) 
      ; LLVMTypes . FreeCopiedSR ( LProducerSR ) 
      END (* IF *) 
 (* llvm remove: 
    ; CodeGen . obj . set_source_file ( FullFileNameT ) 
 end llvm remove *) 
    END set_source_file 
 
; PROCEDURE set_source_line ( CodeGen : CodeGenTyp ; line : INTEGER ) 
  (* Sets the current source line number.  Subsequent statements 
       and expressions are associated with this source location. *) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . OutT ( "set_source_line                 -----LINE" ) 
      ; CodeGen . wr . Int ( line ) 
      ; CodeGen . wr . OutT ( "  -----" ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . lineno := line 
 
 (* llvm remove: 
    ; CodeGen . obj . set_source_line ( line ) 
 end llvm remove *) 
    END set_source_line 
 
(*------------------------------------------- debugging type declarations ---*) 
 
; PROCEDURE declare_typename ( CodeGen : CodeGenTyp ; type : TypeUID ; n : Name ) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "declare_typename" ) 
      ; CodeGen . wr . Tipe ( type ) 
      ; CodeGen . wr . ZName ( n ) 
      ; CodeGen . wr . NL ( ) 
      END 
    END declare_typename 
 
; PROCEDURE declare_array ( CodeGen : CodeGenTyp ; type , index , elt : TypeUID ; s : BitSize ) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "declare_array" ) 
      ; CodeGen . wr . Tipe ( type ) 
      ; CodeGen . wr . Tipe ( index ) 
      ; CodeGen . wr . Tipe ( elt ) 
      ; CodeGen . wr . BInt ( s ) 
      ; CodeGen . wr . NL ( ) 
      END 
    END declare_array 
 
; PROCEDURE declare_open_array ( CodeGen : CodeGenTyp ; type , elt : TypeUID ; s : BitSize ) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "declare_open_array" ) 
      ; CodeGen . wr . Tipe ( type ) 
      ; CodeGen . wr . Tipe ( elt ) 
      ; CodeGen . wr . BInt ( s ) 
      ; CodeGen . wr . NL ( ) 
      END 
    END declare_open_array 
 
; PROCEDURE declare_enum ( CodeGen : CodeGenTyp ; type : TypeUID ; n_elts : INTEGER ; s : BitSize ) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "declare_enum" ) 
      ; CodeGen . wr . Tipe ( type ) 
      ; CodeGen . wr . Int ( n_elts ) 
      ; CodeGen . wr . BInt ( s ) 
      ; CodeGen . wr . NL ( ) 
      END 
    END declare_enum 
 
; PROCEDURE declare_enum_elt ( CodeGen : CodeGenTyp ; n : Name ) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "declare_enum_elt" ) 
      ; CodeGen . wr . ZName ( n ) 
      ; CodeGen . wr . NL ( ) 
      END 
    END declare_enum_elt 
 
; PROCEDURE declare_packed ( CodeGen : CodeGenTyp ; type : TypeUID ; s : BitSize ; base : TypeUID ) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "declare_packed" ) 
      ; CodeGen . wr . Tipe ( type ) 
      ; CodeGen . wr . BInt ( s ) 
      ; CodeGen . wr . Tipe ( base ) 
      ; CodeGen . wr . NL ( ) 
      END 
    END declare_packed 
 
; PROCEDURE declare_record 
    ( CodeGen : CodeGenTyp ; type : TypeUID ; s : BitSize ; FieldCt : INTEGER ) 

  = VAR LRT : LLTypes . LLStructTypeTyp 

  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "declare_record" ) 
      ; CodeGen . wr . Tipe ( type ) 
      ; CodeGen . wr . BInt ( s ) 
      ; CodeGen . wr . Int ( FieldCt ) 
      ; CodeGen . wr . NL ( ) 
      END 

    ; LRT := LLTypes . NewLLStructType ( )  
    ; LRT . LLTypeUID := type 
    ; LRT . StructTypeBitSize := s 
    ; LRT . StructTypeFields := LLTypes . NewLLFieldArray ( FieldCt )  
    ; CodeGen . CurrentStructType := LRT 
    ; CodeGen . NextFieldSs := 0 
    END declare_record 
 
; PROCEDURE declare_field 
    ( CodeGen : CodeGenTyp ; n : Name ; o : BitOffset ; s : BitSize 
    ; type : TypeUID 
    ) 

  = VAR LF : REF LLTypes . LLFieldTyp 

  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "declare_field" ) 
      ; CodeGen . wr . ZName ( n ) 
      ; CodeGen . wr . BInt ( o ) 
      ; CodeGen . wr . BInt ( s ) 
      ; CodeGen . wr . Tipe ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 

    ; LF := LLTypes . NewLLField ( )   
    ; LF . FieldTypeUID := type  
    ; LF . FieldName := n
    ; LF . FieldBitOffset := o 
    ; LF . FieldBitSize := s 
    ; LF . FieldByteAlign := 0 (* Where can we get this from? *)  
    ; LF . FieldType := LLTypeOfUID ( CodeGen , type )  
    ; CodeGen . CurrentStructType . StructTypeFields ^ [ CodeGen . NextFieldSs ]
        := LF 
    ; INC ( CodeGen . NextFieldSs ) 
    END declare_field 

; PROCEDURE FinishStructType ( CodeGen : CodeGenTyp )  

  = VAR LRT : LLTypes . LLStructTypeTyp  

  ; BEGIN 
      LRT := CodeGen . CurrentStructType 
(* CHECK ^Whose LRT . LLTypeUID must be set. *) 
    ; <* ASSERT CodeGen . NextFieldSs = NUMBER ( LRT . StructTypeFields ^ ) *> 
(* LllvmFields := NIL 
   ^Complete This. *)  
      AddType ( CodeGen , LRT )   
    END FinishStructType 
 
; PROCEDURE declare_set 
    ( CodeGen : CodeGenTyp 
    ; type : TypeUID (* Declared type. *)
    ; domain : TypeUID (* Base type. *) 
    ; s : BitSize 
    ) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "declare_set" ) 
      ; CodeGen . wr . Tipe ( type ) 
      ; CodeGen . wr . Tipe ( domain ) 
      ; CodeGen . wr . BInt ( s ) 
      ; CodeGen . wr . NL ( ) 
      END 
    END declare_set 
 
; PROCEDURE declare_subrange 
    ( CodeGen : CodeGenTyp 
    ; type : TypeUID (* Declared type. *)
    ; domain : TypeUID (* Base type. *) 
    ; READONLY min , max : Target . Int 
    ; s : BitSize 
    ) 
  = VAR LLLType : LLTypes . LLTypeTyp 
  ; VAR LTypeRef : LLVM . TypeRef 

  ; BEGIN 
      LTypeRef := LLVM . LLVMIntTypeInContext ( CodeGen . llvmContextRef , s ) 
    ; LLLType := NIL 
(* COMPLETE^, including LLLType . LLTypeUID := type *) 
    ; AddType ( CodeGen , LLLType ) 
    ; IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "declare_subrange" ) 
      ; CodeGen . wr . Tipe ( type ) 
      ; CodeGen . wr . Tipe ( domain ) 
      ; CodeGen . wr . TInt ( TIntN . FromTargetInt ( min , NUMBER ( min ) ) ) 
      ; (* What about s for size? *) 
        CodeGen . wr . TInt ( TIntN . FromTargetInt ( max , NUMBER ( max ) ) ) 
      ; (* What about s for size? *) 
        CodeGen . wr . BInt ( s ) 
      ; CodeGen . wr . NL ( ) 
      END 
    END declare_subrange 
 
; PROCEDURE declare_pointer 
    ( CodeGen : CodeGenTyp ; type , target : TypeUID ; brand : TEXT ; traced : BOOLEAN ) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "declare_pointer" ) 
      ; CodeGen . wr . Tipe ( type ) 
      ; CodeGen . wr . Tipe ( target ) 
      ; CodeGen . wr . Txt ( brand ) 
      ; CodeGen . wr . Bool ( traced ) 
      ; CodeGen . wr . NL ( ) 
      END 
    END declare_pointer 
 
 
; PROCEDURE declare_indirect ( CodeGen : CodeGenTyp ; type , target : TypeUID ) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "declare_indirect" ) 
      ; CodeGen . wr . Tipe ( type ) 
      ; CodeGen . wr . Tipe ( target ) 
      ; CodeGen . wr . NL ( ) 
      END 
    END declare_indirect 
 
 
; PROCEDURE declare_proctype 
    ( CodeGen : CodeGenTyp 
    ; type : TypeUID 
    ; n_formals : INTEGER 
    ; result : TypeUID 
    ; n_raises : INTEGER 
    ; cc : CallingConvention 
    ) 
  (* Only produces debug info. *) 

  = VAR LocLLFuncType : LLTypes . LLFuncTypeTyp 

  ; BEGIN 
      IF CodeGen . CurrentSigProc # NIL 
      THEN (* We were inside the formals/locals/result of a previous declared
              or imported procedure.  Close it out. *) 
        FinishProcSignature ( CodeGen ) 
      END (* IF *) 
    ; LocLLFuncType := StartFuncType ( NoUID , NoUID , FormalsCt ) 
    ; CodeGen . CurrentFuncType := LocLLFuncType 
    ; IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "declare_proctype" ) 
      ; CodeGen . wr . Tipe ( type ) 
      ; CodeGen . wr . Int ( n_formals ) 
      ; CodeGen . wr . Tipe ( result ) 
      ; CodeGen . wr . Int ( n_raises ) 
      ; CodeGen . wr . Txt ( cc . name ) 
      ; CodeGen . wr . NL ( ) 
      END 
    END declare_proctype 
 
; PROCEDURE declare_formal 
    ( CodeGen : CodeGenTyp ; FormalName : Name ; FormalTypeUID : TypeUID ) 
  (* This is a formal of a procedure type, created by declare_proctype. *) 

  = VAR LocFormal : LLTypes . LLFormalTyp 

  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "declare_formal" ) 
      ; CodeGen . wr . ZName ( FormalName ) 
      ; CodeGen . wr . Tipe ( FormalTypeUID  ) 
      ; CodeGen . wr . NL ( ) 
      END 
    ; LocFormal 
        := AddFuncTypeFormal 
             ( CodeGen , FormalName , FormalTypeUID , CGType . void ) 
    ; LocFormal . Formal := 
    ; LocFormal . Formal := 
    ; LocFormal . Formal := 
    END declare_formal 
 
; PROCEDURE declare_raises ( CodeGen : CodeGenTyp ; n : Name ) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "declare_raises" ) 
      ; CodeGen . wr . ZName ( n ) 
      ; CodeGen . wr . NL ( ) 
      END 
    END declare_raises 
 
 
; PROCEDURE declare_object 
    ( CodeGen : CodeGenTyp 
    ; type , super : TypeUID 
    ; brand : TEXT 
    ; traced : BOOLEAN 
    ; FieldCt , n_methods : INTEGER 
    ; field_size : BitSize 
    ) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "declare_object" ) 
      ; CodeGen . wr . Tipe ( type ) 
      ; CodeGen . wr . Tipe ( super ) 
      ; CodeGen . wr . Txt ( brand ) 
      ; CodeGen . wr . Bool ( traced ) 
      ; CodeGen . wr . Int ( FieldCt ) 
      ; CodeGen . wr . Int ( n_methods ) 
      ; CodeGen . wr . BInt ( field_size ) 
      ; CodeGen . wr . NL ( ) 
      END 
    END declare_object 
 
; PROCEDURE declare_method ( CodeGen : CodeGenTyp ; n : Name ; signature : TypeUID ) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "declare_method" ) 
      ; CodeGen . wr . ZName ( n ) 
      ; CodeGen . wr . Tipe ( signature ) 
      ; CodeGen . wr . NL ( ) 
      END 
    END declare_method 
 
; PROCEDURE declare_opaque ( CodeGen : CodeGenTyp ; type , super : TypeUID ) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "declare_opaque" ) 
      ; CodeGen . wr . Tipe ( type ) 
      ; CodeGen . wr . Tipe ( super ) 
      ; CodeGen . wr . NL ( ) 
      END 
    END declare_opaque 
 
; PROCEDURE reveal_opaque ( CodeGen : CodeGenTyp ; lhs , rhs : TypeUID ) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "reveal_opaque" ) 
      ; CodeGen . wr . Tipe ( lhs ) 
      ; CodeGen . wr . Tipe ( rhs ) 
      ; CodeGen . wr . NL ( ) 
      END 
    END reveal_opaque 
 
; PROCEDURE declare_exception 
    ( CodeGen : CodeGenTyp 
    ; n : Name 
    ; arg_type : TypeUID 
    ; raise_proc : BOOLEAN 
    ; base : Var 
    ; offset : INTEGER 
    ) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "declare_exception" ) 
      ; CodeGen . wr . ZName ( n ) 
      ; CodeGen . wr . Tipe ( arg_type ) 
      ; CodeGen . wr . Bool ( raise_proc ) 
      ; CodeGen . wr . VName ( base ) 
      ; CodeGen . wr . Int ( offset ) 
      ; CodeGen . wr . NL ( ) 
      END 
    END declare_exception 
 
(*--------------------------------------------------------- runtime hooks ---*) 
 
; PROCEDURE GetRuntimeHook ( CodeGen : CodeGenTyp ; n : Name ) : RuntimeHook 
  = VAR ref : REFANY 
  ;   e : RuntimeHook 
  ; BEGIN 
      IF CodeGen . runtime . get ( n , ref ) 
      THEN 
        e := ref 
      ELSE 
        e := NEW ( RuntimeHook , name := n , proc := NIL ) 
      ; EVAL CodeGen . runtime . put ( n , e ) 
      END 
    ; RETURN e 
    END GetRuntimeHook 
 
; PROCEDURE set_runtime_proc ( CodeGen : CodeGenTyp ; n : Name ; p : Proc ) 
  = VAR e := GetRuntimeHook ( CodeGen , n ) 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "set_runtime_proc" ) 
      ; CodeGen . wr . ZName ( n ) 
      ; CodeGen . wr . PName ( p ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; e . proc := p 
    END set_runtime_proc 
 
; PROCEDURE get_runtime_hook ( CodeGen : CodeGenTyp ; n : Name ; VAR p : Proc ) 
  = VAR e := GetRuntimeHook ( CodeGen , n ) 
  ; BEGIN 
      p := e . proc 
    END get_runtime_hook 
 
; 
(*------------------------------------------------- variable declarations ---*) 
 
  PROCEDURE NewLLVar 
    ( CodeGen : CodeGenTyp 
    ; type : CGType 
    ; uid : TypeUID 
    ; s : ByteSize 
    ; a : Alignment 
    ; name : Name := M3ID . NoID 
    ; Prefix : TEXT 
    ) 
  : LLVarTyp 

  = VAR LLVar : LLTypes . LLVarTyp 

  ; BEGIN 
      LLVar 
        := NEW 
             ( LLTypes . LLVarTyp 
             , VarNo := CodeGen . NextVarNo 
             , varCGType := type
             , VarTypeUID := uid 
             , var_size := s 
             , var_align := a 
             , seg := Seg . Data 
             ) 
    ; IF name = M3ID . NoID 
      THEN 
        LLVar . VarName := M3ID . Add ( "T$" & Fmt . Int ( LLVar . VarNo ) ) 
      ELSIF uid = - 1 
      THEN LLVar . VarName := M3ID . Add ( "_M" & M3ID . ToText ( name ) ) 
      ELSE LLVar . VarName := M3ID . Add ( "_" & M3ID . ToText ( name ) ) 
      END 
    ; LLVar . VarllvmName := PrefixedName ( LLVar . VarName , Prefix ) 
 
    ; INC ( CodeGen . NextVarNo ) 
    ; RETURN LLVar 
    END NewLLVar 
 
; PROCEDURE import_global 
    ( CodeGen : CodeGenTyp 
    ; n : Name 
    ; s : ByteSize 
    ; a : Alignment 
    ; type : CGType 
    ; m3t : TypeUID 
    ) 
  : Var 

  = VAR LLVar : LLVarTyp := NewLLVar ( CodeGen , type , m3t , s , a , n , "@" ) 

  ; BEGIN 
      LLVar . symbol := CodeGen . obj . import_symbol ( LLVar . VarName ) 
    ; LLVar . offset := 0 
    ; LLVar . loc := VLoc . global 
 
    ; IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "import_global" ) 
      ; CodeGen . wr . ZName ( n ) 
      ; CodeGen . wr . Int ( s ) 
      ; CodeGen . wr . Int ( a ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . Tipe ( m3t ) 
      ; CodeGen . wr . VName ( LLVar ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; RETURN LLVar 
    END import_global 
(* 
; PROCEDURE MaybeUnitProc ( CodeGen : CodeGenTyp ; LProc : LLProcTyp ) 
  (* Lazily create an LL ModuleRef, possibly using the procedure name as 
     module name, if it's a module body procedure. *) 
 
  = VAR LName : TEXT := NIL 
  ; VAR LNameLen : CARDINAL 
  ; VAR LUnitChar : CHAR 
  ; VAR LIsModuleName , LIsInterfaceName : BOOLEAN := FALSE 
 
  ; BEGIN 
      LName := M3ID . ToText ( LProc . ProcName ) 
    ; IF LName = NIL THEN RETURN END (* IF *) 
    ; LNameLen := Text . Length ( LName ) 
    ; IF LNameLen < 3 THEN RETURN END (* IF *) 
    ; IF Text . GetChar ( LName , LNameLen - 3 ) # '_' 
         OR Text . GetChar ( LName , LNameLen - 1 ) # '3' 
      THEN RETURN 
      END (* IF *) 
    ; LUnitChar := Text . GetChar ( LName , LNameLen - 2 ) 
    ; CASE LUnitChar 
      OF 'I' , 'M' (* Its an interface or module procedure name. *) 
      => <* ASSERT CodeGen . llvmModuleRef = NIL *>
        CodeGen . ModuleNameSR := LLVMTypes . CopyM3ToSR ( LName )  
      ; CodeGen . llvmModuleRef 
          := LLVM . LLVMModuleCreateWithName ( CodeGen . ModuleNameSR . Data ) 
      ELSE 
      END (* CASE *) 
    END MaybeUnitProc 
*) 
 
; PROCEDURE UnitTextOfGlobalVarSegName ( SegName : Name ) : TEXT 
 
  = VAR LSegText : TEXT := NIL 
  ; VAR LSegTextLen : CARDINAL 
  ; VAR LUnitChar : CHAR 
 
  ; BEGIN 
      LSegText := M3ID . ToText ( SegName ) 
    ; IF LSegText = NIL THEN RETURN "" END (* IF *) 
    ; LSegTextLen := Text . Length ( LSegText ) 
    ; IF LSegTextLen < 3 THEN RETURN LSegText END (* IF *) 
    ; IF Text . GetChar ( LSegText , 1 ) # '_' 
      THEN RETURN LSegText 
      END (* IF *) 
    ; LUnitChar := Text . GetChar ( LSegText , 0 ) 
    ; CASE LUnitChar 
      OF 'I' 
      => RETURN Text . Sub ( LSegText , 2 , LSegTextLen - 2 ) & ".i3" 
      | 'M' 
      => RETURN Text . Sub ( LSegText , 2 , LSegTextLen - 2 ) & ".m3" 
      ELSE RETURN LSegText 
      END (* CASE *) 
    END UnitTextOfGlobalVarSegName 

; PROCEDURE BitcodeFileNameTOfUnitText ( UnitText : TEXT ) : TEXT  

  = VAR LLen : INTEGER 
  ; VAR LPrefix : TEXT 
  ; VAR LCh : CHAR 

  ; BEGIN 
(* TODO:  This duplicates already executed computation of this file name, done
          in Builder.TempLlvmName, with which this must agree.  Getting that
          value to here was just too big a tangent at the time.  
*) 
      IF UnitText = NIL THEN UnitText := "" END
    ; LLen := Text . Length ( UnitText ) 
    ; IF LLen >= 3
      THEN  
        LCh := Text . GetChar ( UnitText , LLen - 2 ) 
      ; IF Text . GetChar ( UnitText , LLen - 1 ) = '3'  
           AND Text . GetChar ( UnitText , LLen - 3 ) = '.'
           AND LCh IN SET OF CHAR { 'i' , 'm' } 
        THEN
          LPrefix := Text . Sub ( UnitText , 0 , LLen - 2 ) 
        ; RETURN LPrefix & Text . FromChar ( LCh ) & "b" 
        END (* IF *) 
      END (* IF *) 
      (* As a last resort, that will probably be hard to find: *) 
    ; RETURN UnitText & ".bc" 
    END BitcodeFileNameTOfUnitText 
 
; PROCEDURE declare_segment 
    ( CodeGen : CodeGenTyp ; n : Name ; m3t : TypeUID ; is_const : BOOLEAN ) 
  : Var 
 
  = CONST SegMap = ARRAY BOOLEAN(*is_const*) OF Seg { Seg . Data , Seg . Text } 
 
  ; VAR LLVar : LLVarTyp 
        := NewLLVar ( CodeGen , CGType . Void , m3t , 0 , 4 , n , "@" )
(* CHECK                                                          ^ *)  

; VAR LGMI : Ctypes. const_char_star
 
  ; BEGIN 
      IF CodeGen . global_var = NIL AND NOT is_const  
      THEN (* The unique segment for module/interface global variables. *) 
        CodeGen . global_var := LLVar 
      (* This is the unique place where we get the module name. *) 
      ; LGMI := LLVM . LLVMGetModuleIdentifier ( CodeGen . llvmModuleRef ) 
      ; <* ASSERT LGMI^ = ORD('\n') *> 
        CodeGen . ModuleNameT := UnitTextOfGlobalVarSegName ( n ) 
      ; CodeGen . ModuleNameSR 
          := LLVMTypes . CopyM3ToSR ( CodeGen . ModuleNameT )  
      ; LLVM . LLVMSetModuleIdentifier 
          ( CodeGen . llvmModuleRef , CodeGen . ModuleNameSR . Data ) 
      END 
 
    ; LLVar . seg := SegMap [ is_const ] 
    ; LLVar . symbol := CodeGen . obj . define_symbol ( LLVar . VarName , LLVar . seg , 0 ) 
    ; LLVar . offset := 0 
    ; LLVar . loc := VLoc . global 
 
    ; IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "declare_segment" ) 
      ; CodeGen . wr . ZName ( n ) 
      ; CodeGen . wr . Tipe ( m3t ) 
      ; CodeGen . wr . Bool ( is_const ) 
      ; CodeGen . wr . VName ( LLVar ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; RETURN LLVar 
    END declare_segment 
 
; PROCEDURE bind_segment 
    ( CodeGen : CodeGenTyp 
    ; v : Var 
    ; s : ByteSize 
    ; a : Alignment 
    ; type : CGType 
    ; exported , inited : BOOLEAN 
    ) 
  = VAR LLVar := NARROW ( v , LLVarTyp ) 
  ; BEGIN 
            <* ASSERT inited *> 
 
      LLVar . varCGType := type 
    ; LLVar . var_size := s 
    ; LLVar . var_align := a 
 
    ; IF exported 
      THEN 
        CodeGen . obj . export_symbol ( LLVar . symbol ) 
      END 
 
    ; IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "bind_segment" ) 
      ; CodeGen . wr . VName ( v ) 
      ; CodeGen . wr . Int ( s ) 
      ; CodeGen . wr . Int ( a ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . Bool ( exported ) 
      ; CodeGen . wr . Bool ( inited ) 
      ; CodeGen . wr . NL ( ) 
      END 
    END bind_segment 
 
; PROCEDURE declare_global 
    ( CodeGen : CodeGenTyp 
    ; n : Name 
    ; s : ByteSize 
    ; a : Alignment 
    ; type : CGType 
    ; m3t : TypeUID 
    ; exported , inited : BOOLEAN 
    ) 
  : Var 
  = BEGIN 
      RETURN 
        DeclareGlobal ( CodeGen , n , s , a , type , m3t , exported , inited , FALSE ) 
    END declare_global 
 
; PROCEDURE declare_constant 
    ( CodeGen : CodeGenTyp 
    ; n : Name 
    ; s : ByteSize 
    ; a : Alignment 
    ; type : CGType 
    ; m3t : TypeUID 
    ; exported , inited : BOOLEAN 
    ) 
  : Var 
  = BEGIN 
      RETURN 
        DeclareGlobal ( CodeGen , n , s , a , type , m3t , exported , inited , TRUE ) 
    END declare_constant 
 
; PROCEDURE DeclareGlobal 
    ( CodeGen : CodeGenTyp 
    ; n : Name 
    ; s : ByteSize 
    ; a : Alignment 
    ; type : CGType 
    ; m3t : TypeUID 
    ; exported , inited , is_const : BOOLEAN 
    ) 
  : Var 

  = CONST SegMap = ARRAY BOOLEAN OF Seg { Seg . Data , Seg . Text } 
  ; CONST DeclTag 
      = ARRAY BOOLEAN OF TEXT { "declare_global" , "declare_constant" } 

  ; VAR LLVar : LLVarTyp := NewLLVar ( CodeGen , type , m3t , s , a , n , "@" ) 

  ; BEGIN 
      LLVar . loc := VLoc . global 
    ; LLVar . seg := SegMap [ is_const ] 
    ; IF inited 
      THEN 
        LLVar . symbol := CodeGen . obj . define_symbol ( LLVar . VarName , LLVar . seg , 0 ) 
      ELSE 
        LLVar . symbol := CodeGen . obj . define_bss_symbol ( LLVar . VarName , s , a ) 
      END 
    ; IF exported 
      THEN 
        CodeGen . obj . export_symbol ( LLVar . symbol ) 
      END 
    ; IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( DeclTag [ is_const ] ) 
      ; CodeGen . wr . ZName ( n ) 
      ; CodeGen . wr . Int ( s ) 
      ; CodeGen . wr . Int ( a ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . Tipe ( m3t ) 
      ; CodeGen . wr . Bool ( exported ) 
      ; CodeGen . wr . Bool ( inited ) 
      ; CodeGen . wr . VName ( LLVar ) 
      ; CodeGen . wr . NL ( ) 
      END 
    ; RETURN LLVar 
    END DeclareGlobal 

; PROCEDURE declare_local 
    ( CodeGen : CodeGenTyp 
    ; n : Name 
    ; s : ByteSize 
    ; a : Alignment 
    ; type : CGType 
    ; m3t : TypeUID 
    ; in_memory , up_level : BOOLEAN 
    ; f : Frequency 
    ) 
  : Var 

  = VAR LocFormal : LLTypes . LLFormalTyp 
  ; VAR LLVar : LLVarTyp 
  ; VAR LParamProc : LLProcTyp 
  ; VAR LLLType : LLTypes . LLTypeTyp  

  ; BEGIN 
      IF CodeGen . in_proc 
      THEN 
        LLVar := get_temp_var ( CodeGen , type , s , a , n ) 
      ELSE 
        LLVar := create_temp_var ( CodeGen , type , s , a , n ) 
      END 
    ; LLVar . VarTypeUID := m3t 
    ; LLLType := LLTypeOfUID ( CodeGen , m3t ) 
    ; LLVar . VarLLType  := LLLType 
    ; LLVar . VarllvmValueRef := NIL (*COMPLETE ME*) 
    ; LParamProc := CodeGen . CurrentSigProc 
    ; IF LParamProc # NIL AND n = G_resultID 
      THEN (* This is the frontend's internally-generated local variable
              named "_result", for a function result.  It is the only place
              we are given the UID of the result type.  It would be just 
              ever so much cleaner if declare_procedure and import_procedure 
              gave us this, but that would require a simple but very 
              pervasive change in backend-independent code.
           *) 
        <* ASSERT LParamProc . ProcLLFuncType . FuncTypeResultLLType = NIL *> 
           (* ^Only one function result. *) 
        LParamProc . ProcLLFuncType . FuncTypeResultLLType := LLLType 
      ; LParamProc . ProcLLFuncType . FuncTypeHasResultFormal := FALSE   
      END (* IF *) 
 
    ; IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "declare_local" ) 
      ; CodeGen . wr . ZName ( n ) 
      ; CodeGen . wr . Int ( s ) 
      ; CodeGen . wr . Int ( a ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . Tipe ( m3t ) 
      ; CodeGen . wr . Bool ( in_memory ) 
      ; CodeGen . wr . Bool ( up_level ) 
      ; CodeGen . wr . Int ( f ) 
      ; CodeGen . wr . VName ( LLVar ) 
      ; CodeGen . wr . Int ( LLVar . offset ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; RETURN LLVar 
    END declare_local 
 
; PROCEDURE mangle_procname 
    ( base : M3ID . T ; arg_size : INTEGER ; std_call : BOOLEAN ) 
  : M3ID . T 
  = VAR buf : ARRAY [ 0 .. 3 ] OF CHAR 
  ;   txt := M3ID . ToText ( base ) 
  ;   len := Text . Length ( txt ) 
  ; BEGIN 
            (* return the 64bit functions unchanged *) 
 
      IF len > NUMBER ( buf ) 
      THEN 
        Text . SetChars ( SUBARRAY ( buf , 0 , NUMBER ( buf ) ) , txt ) 
      ; IF buf = ARRAY OF CHAR { '_' , 'm' , '3' , '_' } 
        THEN 
          RETURN base 
        END 
      END 
 
    ; IF std_call 
      THEN 
        RETURN 
          M3ID . Add ( Fmt . F ( "_%s@%s" , txt , Fmt . Int ( arg_size ) ) ) 
      ELSE 
        RETURN M3ID . Add ( Fmt . F ( "_%s" , txt ) ) 
      END 
    END mangle_procname 

; PROCEDURE declare_param 
    ( CodeGen : CodeGenTyp 
    ; ParamName : Name 
    ; ParamByteSize : ByteSize 
    ; ParamAlign : Alignment 
    ; ParamCGType : CGType 
    ; ParamUID : TypeUID 
    ; InMemory : BOOLEAN 
    ; UpLevel : BOOLEAN 
    ; ParamFrequency : Frequency 
    ) 
  : Var 
  (* A formal parameter of a procedure (constant), created by the most recent
     declare_procedure or import_procedure.  The frontend will supply these in 
     their lexical order, relative to each other, but many other things can be
     interspersed. 
  *)

  = VAR LocFormal : LLTypes . LLFormalTyp 
  ; VAR LLVar : LLVarTyp 
  ; VAR LProc : LLProcTyp 

  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "declare_param" ) 
      ; CodeGen . wr . ZName ( ParamName ) 
      ; CodeGen . wr . Int ( ParamByteSize ) 
      ; CodeGen . wr . Int ( ParamAlign ) 
      ; CodeGen . wr . TName ( ParamCGType ) 
      ; CodeGen . wr . Tipe ( ParamUID ) 
      ; CodeGen . wr . Bool ( InMemory ) 
      ; CodeGen . wr . Bool ( UpLevel ) 
      ; CodeGen . wr . Int ( ParamFrequency ) 
      ; CodeGen . wr . VName ( LLVar ) 
      ; CodeGen . wr . Int ( LLVar . offset ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; LocFormal 
        := AddFuncTypeFormal 
             ( CodeGen , ParamName , FormalTypeUID , ParamCGType ) 
    ; LocFormal . FormalBitSize := ParamByteSize * 8  
    ; LocFormal . FormalByteAlign := ParamAlign 
    ; LocFormal . FormalInMemory := InMemory
    ; LocFormal . FormalUpLevel := UpLevel
    ; LocFormal . FormalFrequency := ParamFrequency  



    ; LProc := CodeGen .CurrentSigProc 
    ; LLVar := NewLLVar ( CodeGen , ParamCGType , ParamUID , ParamByteSize , 4 , ParamName , "%" ) 
    ; LLVar . VarLLType  := LLTypeOfUID ( CodeGen , ParamUID ) 
    ; CodeGen . FormalTypesList ^ [ CodeGen . NextFormalSs ] 
        := LLVar . VarLLType  
    (* LLVar . llvmValueRef will have to wait until llvm has created the 
       FunctionRef, which has to wait until after llvm creates the TypeRef 
       for the function type, which has to wait until after we have seen all 
       the formals and whether there is a local named "_result", to get 
       their types.
    *) 
      (* Assume ParamAlign = 4 and ESP is dword aligned... *) 
    ; ParamByteSize := ( ParamByteSize + 3 ) DIV 4 * 4 
    ; LLVar . offset := LProc . paramsize 
    ; LLVar . loc := VLoc . temp 
    ; LLVar . parent := LProc 

    ; INC ( LProc . paramsize , ParamByteSize ) 
    ; LProc . ProcFormalsList ^ [ CodeGen . NextFormalSs ] := LLVar 
    ; IF CodeGen . CurrentFormalsCt = 0 AND LProc . stdcall 
      THEN (* callee cleans & mangled name *) 
        LProc . ProcName 
          := mangle_procname 
               ( LProc . ProcName , LProc . paramsize - 8 , std_call := TRUE ) 
      ; IF LProc . import 
        THEN 
          LProc . symbol := CodeGen . obj . import_symbol ( LProc . ProcName ) 
        ELSE 
          LProc . symbol 
            := CodeGen . obj . define_symbol ( LProc . ProcName , Seg . Text , 0 ) 
        END 
      ; IF LProc . exported 
        THEN CodeGen . obj . export_symbol ( LProc . symbol ) 
        END (* IF *) 
      END (* IF *) 

    ; IF LProc # NIL AND ParamName = G_resultID 
      THEN (* This is the frontend's internally-generated formal 
              named "_result", for a function result.  It is the only place
              we are given the UID of the result type.  
           *) 
        <* ASSERT LProc . ProcLLFuncType . FuncTypeResultLLType = NIL *> 
           (* ^Only one function result. *) 
        LProc . ProcLLFuncType . FuncTypeResultLLType := LLVar . VarLLType  
      ; LProc . ProcLLFuncType . FuncTypeHasResultFormal := TRUE  
      END (* IF *) 

    ; INC ( CodeGen . NextFormalSs ) 
 
    ; RETURN LLVar 
    END declare_param 
 
; PROCEDURE declare_temp 
    ( CodeGen : CodeGenTyp ; TempByteSize : ByteSize ; a : Alignment ; type : CGType ; InMemory : BOOLEAN ) 
  : Var 
  = VAR LLVar : LLVarTyp 
  ; BEGIN 
      <* ASSERT CodeGen . in_proc *> 
 
      LLVar := get_temp_var ( CodeGen , type , TempByteSize , a ) 
 
    ; IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "declare_temp" ) 
      ; CodeGen . wr . Int ( TempByteSize ) 
      ; CodeGen . wr . Int ( a ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . Bool ( InMemory ) 
      ; CodeGen . wr . VName ( LLVar ) 
      ; CodeGen . wr . Int ( LLVar . offset ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; RETURN LLVar 
    END declare_temp 
 
; PROCEDURE get_temp_var 
    ( CodeGen : CodeGenTyp ; type : CGType ; TempByteSize : ByteSize ; a : Alignment ; n : Name := M3ID . NoID ) 
  : LLVarTyp 

  = BEGIN 
      (* round size and alignment up to 4 *) 
      IF TempByteSize < 4 THEN TempByteSize := 4 END 
    ; IF a < 4 THEN a := 4 END 
    (* reuse an existing temporary variable if possible *) 
 
    ; FOR i := 0 TO CodeGen . CurrentProc . tempsize - 1 
      DO 
        WITH temp = CodeGen . CurrentProc . temparr [ i ] 
        DO 
          IF temp . free AND temp . var . var_size = TempByteSize 
             AND temp . var . var_align >= a 
          THEN 
         (* reinitialize existing temporary variable *) 
            temp . free := FALSE 
          ; temp . var . varCGType := type 
          ; temp . var . stack_temp := FALSE 
          ; temp . var . scope := CodeGen . next_scope - 1 
          ; RETURN temp . var 
          END 
        END 
      END 

    (* grow temporary variable array if necessary *) 
    ; IF CodeGen . CurrentProc . tempsize = CodeGen . CurrentProc . templimit 
      THEN expand_temp ( CodeGen ) 
      END 

    (* initialize new temporary variable *) 
    ; WITH temp = CodeGen . CurrentProc . temparr 
             [ CodeGen . CurrentProc . tempsize ] 
      DO 
        temp . var := create_temp_var ( CodeGen , type , TempByteSize , a , n ) 
      ; <* ASSERT temp . var . varCGType = type *> 
        temp . free := FALSE 
      ; temp . var . scope := CodeGen . next_scope - 1 
      END 
    ; INC ( CodeGen . CurrentProc . tempsize ) 
    ; RETURN 
        CodeGen . CurrentProc . temparr 
          [ CodeGen . CurrentProc . tempsize - 1 ] . var 
    END get_temp_var 
 
; PROCEDURE expand_temp ( CodeGen : CodeGenTyp ) 
  = VAR newarr := NEW ( REF ARRAY OF Temp , CodeGen . CurrentProc . templimit * 2 ) 
  ; BEGIN 
      FOR i := 0 TO ( CodeGen . CurrentProc . templimit - 1 ) 
      DO 
        newarr [ i ] := CodeGen . CurrentProc . temparr [ i ] 
      END 
 
    ; CodeGen . CurrentProc . templimit := CodeGen . CurrentProc . templimit * 2 
    ; CodeGen . CurrentProc . temparr := newarr 
    END expand_temp 
 
; PROCEDURE create_temp_var 
    ( CodeGen : CodeGenTyp ; type : CGType ; TempByteSize : ByteSize ; a : Alignment ; n : Name ) 
  : LLVarTyp 
  = VAR LLVar : LLVarTyp := NewLLVar ( CodeGen , type , 0 , TempByteSize , a , n , "%" ) 
  ; BEGIN 
      LLVar . loc := VLoc . temp 
    ; LLVar . parent := CodeGen . CurrentProc 
 
    ; CodeGen . CurrentProc . framesize 
        := Word . And ( CodeGen . CurrentProc . framesize + a - 1 , Alignmask [ a ] ) 
 
    ; INC ( CodeGen . CurrentProc . framesize , TempByteSize ) 
 
    ; LLVar . offset := - CodeGen . CurrentProc . framesize 
 
    ; RETURN LLVar 
    END create_temp_var 
 
; PROCEDURE free_temp ( CodeGen : CodeGenTyp ; v : Var ) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "free_temp" ) 
      ; CodeGen . wr . VName ( v ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; FOR i := 0 TO CodeGen . CurrentProc . tempsize - 1 
      DO 
        IF ( NOT CodeGen . CurrentProc . temparr [ i ] . free ) 
           AND CodeGen . CurrentProc . temparr [ i ] . var = v 
        THEN 
          CodeGen . CurrentProc . temparr [ i ] . free := TRUE 
        ; RETURN 
        END 
      END 
 
    ; Err ( CodeGen , "Couldn't find var to free in 'free_temp'" ) 
    ; <* ASSERT FALSE *> 
    END free_temp 
 
; 
(*---------------------------------------- static variable initialization ---*) 
 
  PROCEDURE begin_init ( CodeGen : CodeGenTyp ; v : Var ) 
  = VAR LLVar := NARROW ( v , LLVarTyp ) 
  ;   offs , pad : INTEGER 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "begin_init" ) 
      ; CodeGen . wr . VName ( v ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; <* ASSERT CodeGen . init_varstore = NIL *> 
 
      CodeGen . init_varstore := LLVar 
 
    ; offs := CodeGen . obj . cursor ( LLVar . seg ) 
 
    ; IF Word . And ( offs , LLVar . var_align - 1 ) # 0 
      THEN 
        pad 
          := LLVar . var_align - Word . And ( offs , LLVar . var_align - 1 ) 
      ; INC ( offs , pad ) 
      ; IF Word . And ( pad , 3 ) # 0 
        THEN 
          CodeGen . obj . append ( LLVar . seg , 0 , Word . And ( pad , 3 ) ) 
        ; pad := Word . And ( pad , 16_FFFFFFFC ) 
        END 
 
      ; pad := pad DIV 4 
      ; FOR i := 1 TO pad 
        DO 
          CodeGen . obj . append ( LLVar . seg , 0 , 4 ) 
        END 
      END 
 
    ; CodeGen . obj . move_symbol ( LLVar . symbol , offs ) 
 
    ; CodeGen . init_count := 0 
    END begin_init 
 
; PROCEDURE end_init ( CodeGen : CodeGenTyp ; v : Var ) 
  = VAR LLVar := NARROW ( v , LLVarTyp ) 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "end_init" ) 
      ; CodeGen . wr . VName ( v ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; <* ASSERT v = CodeGen . init_varstore *> 
      pad_init ( CodeGen , LLVar . var_size ) 
    ; CodeGen . init_varstore := NIL 
    END end_init 
 
; PROCEDURE init_int 
    ( CodeGen : CodeGenTyp ; o : ByteOffset ; READONLY value : Target . Int ; type : CGType ) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "init_int" ) 
      ; CodeGen . wr . Int ( o ) 
      ; CodeGen . wr . TInt ( TIntN . FromTargetInt ( value , CG_Bytes [ type ] ) ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; pad_init ( CodeGen , o ) 
    ; CodeGen . obj . appendBytes 
        ( CodeGen . init_varstore . seg , SUBARRAY ( value , 0 , CG_Bytes [ type ] ) ) 
    ; INC ( CodeGen . init_count , CG_Bytes [ type ] ) 
 
    ; 
    END init_int 
 
; PROCEDURE init_proc ( CodeGen : CodeGenTyp ; o : ByteOffset ; value : Proc ) 
  = VAR LProc := NARROW ( value , LLProcTyp ) 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "init_proc" ) 
      ; CodeGen . wr . Int ( o ) 
      ; CodeGen . wr . PName ( value ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; pad_init ( CodeGen , o ) 
 
    ; CodeGen . obj . append ( CodeGen . init_varstore . seg , 0 , 4 ) 
    ; INC ( CodeGen . init_count , 4 ) 
 
    ; CodeGen . obj . relocate ( CodeGen . init_varstore . symbol , o , LProc . symbol ) 
    END init_proc 
 
; PROCEDURE init_label ( CodeGen : CodeGenTyp ; o : ByteOffset ; value : Label ) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "init_label" ) 
      ; CodeGen . wr . Int ( o ) 
      ; CodeGen . wr . Lab ( value ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; pad_init ( CodeGen , o ) 
 
    ; CodeGen . cg . log_label_init ( CodeGen . init_varstore , o , value ) 
 
    ; INC ( CodeGen . init_count , 4 ) 
    END init_label 
 
; PROCEDURE init_var ( CodeGen : CodeGenTyp ; o : ByteOffset ; value : Var ; bias : ByteOffset ) 
  = VAR LLVar := NARROW ( value , LLVarTyp ) 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "init_var" ) 
      ; CodeGen . wr . Int ( o ) 
      ; CodeGen . wr . VName ( value ) 
      ; CodeGen . wr . Int ( bias ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; <* ASSERT LLVar . loc = VLoc . global *> 
 
      pad_init ( CodeGen , o ) 
 
    ; CodeGen . obj . append ( CodeGen . init_varstore . seg , bias , 4 ) 
    ; INC ( CodeGen . init_count , 4 ) 
 
    ; CodeGen . obj . relocate ( CodeGen . init_varstore . symbol , o , LLVar . symbol ) 
    END init_var 
 
; PROCEDURE init_offset ( CodeGen : CodeGenTyp ; o : ByteOffset ; value : Var ) 
  = VAR LLVar := NARROW ( value , LLVarTyp ) 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "init_offset" ) 
      ; CodeGen . wr . Int ( o ) 
      ; CodeGen . wr . VName ( value ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; <* ASSERT LLVar . loc = VLoc . temp *> 
 
      pad_init ( CodeGen , o ) 
 
    ; CodeGen . obj . append ( CodeGen . init_varstore . seg , LLVar . offset , 4 ) 
    ; INC ( CodeGen . init_count , 4 ) 
    END init_offset 
 
; PROCEDURE init_chars ( CodeGen : CodeGenTyp ; o : ByteOffset ; value : TEXT ) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "init_chars" ) 
      ; CodeGen . wr . Int ( o ) 
      ; CodeGen . wr . Txt ( value ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; pad_init ( CodeGen , o ) 
 
    ; WITH len = Text . Length ( value ) 
      DO 
        FOR i := 0 TO len - 1 
        DO 
          CodeGen . obj . append 
            ( CodeGen . init_varstore . seg 
            , ORD ( Text . GetChar ( value , i ) ) , 1 
            ) 
        END 
 
      ; INC ( CodeGen . init_count , len ) 
      END 
    END init_chars 
 
; PROCEDURE init_float 
    ( CodeGen : CodeGenTyp ; o : ByteOffset ; READONLY f : Target . Float ) 
  = VAR flarr : FloatBytes 
  ;   size : INTEGER 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "init_float" ) 
      ; CodeGen . wr . Int ( o ) 
      ; CodeGen . wr . Flt ( f ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; size := TFloat . ToBytes ( f , flarr ) 
 
    ; <* ASSERT size = 4 OR size = 8 *> 
      pad_init ( CodeGen , o ) 
 
    ; FOR i := 0 TO size - 1 
      DO 
        CodeGen . obj . append 
          ( CodeGen . init_varstore . seg , flarr [ i ] , 1 ) 
      ; INC ( CodeGen . init_count ) 
      END 
    END init_float 
 
; PROCEDURE pad_init ( CodeGen : CodeGenTyp ; o : ByteOffset ) 
  = BEGIN 
      <* ASSERT CodeGen . init_count <= o *> 
      <* ASSERT o <= CodeGen . init_varstore . var_size *> 
 
      FOR i := CodeGen . init_count TO o - 1 
      DO 
        CodeGen . obj . append ( CodeGen . init_varstore . seg , 0 , 1 ) 
      END 
 
    ; CodeGen . init_count := o 
    END pad_init 
 
; 
(*------------------------------------------------------------ procedures ---*) 
 
  PROCEDURE NewProc 
    ( CodeGen : CodeGenTyp 
    ; n : Name 
    ; FormalsCt : INTEGER 
    ; ResultCGType : CGType 
    ; cc : CallingConvention 
    ) 
  : LLTypes . LLProcTyp 

  = VAR LProc : LLTypes . LLProcTyp 

  ; BEGIN 
      LProc 
        := NEW ( LLTypes . LLProcTyp 
             , ProcNo := CodeGen . NextVarNo 
             , ProcResultCGType := ResultCGType 
             , stdcall := ( cc . m3cg_id = 1 ) 
             ) 
    ; LProc . ProcFormalsList 
        := NEW ( REF ARRAY OF LLTypes . LLVarTyp , FormalsCt ) 
    ; IF n = M3ID . NoID 
      THEN 
        LProc . ProcName := M3ID . Add ( "P$" & Fmt . Int ( LProc . ProcNo ) ) 
      ELSE LProc . ProcName := n 
      END 
    ; LProc . ProcllvmName := PrefixedName ( LProc . ProcName , "@" )

    ; LProc . templimit := 16 
    ; LProc . temparr := NEW ( REF ARRAY OF Temp , LProc . templimit ) 

    ; INC ( CodeGen . NextVarNo ) 
    ; RETURN LProc 
    END NewProc 
 
; PROCEDURE import_procedure 
    ( CodeGen : CodeGenTyp 
    ; n : Name 
    ; FormalsCt : INTEGER 
    ; ret_type : CGType 
    ; cc : CallingConvention 
    ) 
  : Proc 

  = VAR LocLLFuncType : LLTypes . LLFuncTypeTyp 
  ; VAR LProc : LLProcTyp 

  ; BEGIN 
      IF CodeGen . CurrentSigProc # NIL 
      THEN (* We were inside the formals/locals/result of a previous declared
              or imported procedure.  Close it out. *) 
        FinishProcSignature ( CodeGen ) 
      END (* IF *) 
    ; LocLLFuncType := StartFuncType ( NoUID , NoUID , FormalsCt ) 
    ; CodeGen . CurrentFuncType := LocLLFuncType 


    ; LProc := NewProc ( CodeGen , n , FormalsCt , ret_type , cc ) 
    ; LProc . import := TRUE 
    ; IF ( FormalsCt = 0 OR NOT LProc . stdcall ) 
         AND Text . Length ( M3ID . ToText ( n ) ) > 0 
      THEN 
        LProc . ProcName := mangle_procname ( LProc . ProcName , 0 , LProc . stdcall ) 
      ; LProc . symbol := CodeGen . obj . import_symbol ( LProc . ProcName ) 
      END 
 
    (* Things in CodeGen that apply to one procedure. *) 
    ; CodeGen . CurrentFormalsCt := FormalsCt 
    ; CodeGen . CurrentSigProc := LProc 
    ; IF NOT CodeGen . in_proc 
      THEN CodeGen . CurrentProc := LProc 
(* CHECK:  ^Is this necessary? *) 
      END (* IF *) 
    ; IF FormalsCt > 0 
      THEN 
        CodeGen . FormalTypesList 
          := NEW ( REF ARRAY OF LLTypes . LLTypeTyp , FormalsCt ) 
      ; LProc . ProcFormalsList := NEW ( REF ARRAY OF LLVarTyp , FormalsCt ) 
      ELSE 
        CodeGen . FormalTypesList := NIL 
      ; LProc . ProcFormalsList := NIL 
      END (* IF *) 
    ; CodeGen . NextFormalSs := 0 

    ; IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "import_procedure" ) 
      ; CodeGen . wr . ZName ( n ) 
      ; CodeGen . wr . Int ( FormalsCt ) 
      ; CodeGen . wr . TName ( ret_type ) 
      ; CodeGen . wr . Txt ( cc . name ) 
      ; CodeGen . wr . PName ( LProc ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; RETURN LProc 
    END import_procedure 

; PROCEDURE declare_procedure 
    ( CodeGen : CodeGenTyp 
    ; n : Name 
    ; FormalsCt : INTEGER 
    ; return_type : CGType 
    ; lev : INTEGER 
    ; cc : CallingConvention 
    ; exported : BOOLEAN 
    ; parent : Proc 
    ) 
  : Proc 
 
  = VAR LocLLFuncType : LLTypes . LLFuncTypeTyp 
  ; VAR LProc : LLProcTyp 
(* TODO: ^We need an llvm type for the return type.  That probably requires
         a type UID.  A CGType has too much info lost.  This is a difficult
         design decision about cm3 having lowered farther than llvm is
         designed to know about.
*) 
 
  ; BEGIN 
      IF CodeGen . CurrentSigProc # NIL 
      THEN (* We were inside the formals/locals/result of a previous declared
              or imported procedure.  Close it out. *) 
        FinishProcSignature ( CodeGen ) 
      END (* IF *) 
    ; LocLLFuncType := StartFuncType ( NoUID , NoUID , FormalsCt ) 
    ; CodeGen . CurrentFuncType := LocLLFuncType 


    ; LProc := NewProc ( CodeGen , n , FormalsCt , return_type , cc ) 
    ; LProc . exported := exported 
    ; LProc . lev := lev 
    ; LProc . parent := parent 
    ; LProc . ProcLLFuncType . FuncTypeHasResultFormal := FALSE 
    ; IF LProc . lev # 0 THEN INC ( LProc . framesize , 4 ) END 
    ; IF FormalsCt = 0 OR NOT LProc . stdcall 
      THEN 
        LProc . ProcName 
          := mangle_procname ( LProc . ProcName , 0 , LProc . stdcall ) 
      ; LProc . symbol 
          := CodeGen . obj . define_symbol ( LProc . ProcName , Seg . Text , 0 ) 
      ; IF exported 
        THEN CodeGen . obj . export_symbol ( LProc . symbol ) 
        END (* IF *) 
      END (* IF *) 
    ; LProc . ProcLLFuncType . FuncTypeResultLLType := NIL (* Will change. *)   
(* TODO ^Either NIL or a single shared empty list, when FormalsCt = 0 *) 

    (* Things in CodeGen that apply to one procedure. *) 
    ; CodeGen . CurrentFormalsCt := FormalsCt 
    ; CodeGen . CurrentSigProc := LProc 
    ; IF NOT CodeGen . in_proc 
      THEN CodeGen . CurrentProc := LProc 
(* CHECK:  ^Is this necessary? *) 
      END (* IF *) 
    ; IF FormalsCt > 0 
      THEN 
        CodeGen . FormalTypesList 
          := NEW ( REF ARRAY OF LLTypes . LLTypeTyp , FormalsCt ) 
      ; LProc . ProcFormalsList := NEW ( REF ARRAY OF LLVarTyp , FormalsCt ) 
      ELSE 
        CodeGen . FormalTypesList := NIL 
      ; LProc . ProcFormalsList := NIL 
      END (* IF *) 
    ; CodeGen . NextFormalSs := 0 

    ; IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "declare_procedure" ) 
      ; CodeGen . wr . ZName ( n ) 
      ; CodeGen . wr . Int ( FormalsCt ) 
      ; CodeGen . wr . TName ( return_type ) 
      ; CodeGen . wr . Int ( lev ) 
      ; CodeGen . wr . Txt ( cc . name ) 
      ; CodeGen . wr . Bool ( exported ) 
      ; CodeGen . wr . PName ( parent ) 
      ; CodeGen . wr . PName ( LProc ) 
      ; CodeGen . wr . NL ( ) 
      END 
    ; RETURN LProc 
    END declare_procedure 
 
; PROCEDURE begin_procedure ( CodeGen : CodeGenTyp ; p : Proc ) 
  = VAR LProc : LLProcTyp 
  ; BEGIN 
      LProc := NARROW ( p , LLProcTyp ) 
    ; IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "begin_procedure" ) 
      ; CodeGen . wr . PName ( p ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . clearall ( ) 
 
    ; <* ASSERT NOT CodeGen . in_proc *> 
      CodeGen . in_proc := TRUE 
 
    ; CodeGen . CurrentProc := p 
    ; CodeGen . cg . set_current_proc ( p ) 
    ; CodeGen . vstack . set_current_proc ( p ) 
    ; CodeGen . last_exitbranch := - 1 
    ; CodeGen . exit_proclabel := - 1 
 
    ; 
        (* Mark non-volatiles as not used, until known otherwise. *) 
 
      CodeGen . proc_reguse [ EBX ] := FALSE 
    ; CodeGen . proc_reguse [ ESI ] := FALSE 
    ; CodeGen . proc_reguse [ EDI ] := FALSE 
 
    ; LProc . offset := CodeGen . obj . cursor ( Seg . Text ) 
    ; LProc . bound := TRUE 
 
    ; WHILE LProc . usage # NIL 
      DO 
        CodeGen . obj . patch 
          ( Seg . Text 
          , LProc . usage . loc 
          , LProc . offset - ( LProc . usage . loc + 4 ) 
          , 4 
          ) 
      ; LProc . usage := LProc . usage . link 
      END 
 
    ; CodeGen . obj . move_symbol ( LProc . symbol , LProc . offset ) 
 
    ; CodeGen . obj . begin_procedure ( LProc . symbol ) 
 
    ; CodeGen . cg . pushOp ( CodeGen . cg . reg [ EBP ] ) 
    ; CodeGen . cg . movOp ( CodeGen . cg . reg [ EBP ] , CodeGen . cg . reg [ ESP ] ) 
 
    ; CodeGen . cg . immOp ( Op . oSUB , CodeGen . cg . reg [ ESP ] , TWordN . Max16 ) 
    ; CodeGen . procframe_ptr := CodeGen . obj . cursor ( Seg . Text ) - 4 
 
    ; CodeGen . cg . pushOp ( CodeGen . cg . reg [ EBX ] ) 
    ; CodeGen . cg . pushOp ( CodeGen . cg . reg [ ESI ] ) 
    ; CodeGen . cg . pushOp ( CodeGen . cg . reg [ EDI ] ) 
 
    ; IF CodeGen . CurrentProc . lev # 0 
      THEN 
        CodeGen . cg . store_ind 
          ( CodeGen . cg . reg [ ECX ] , CodeGen . cg . reg [ EBP ] , - 4 , CGType . Addr ) 
      END 
 
    ; CodeGen . CurrentProc . tempsize := 0 
 
    ; 
        <* ASSERT CodeGen . next_scope = 1 *> 
 
      begin_block ( CodeGen ) 
    END begin_procedure 
 
; PROCEDURE end_procedure ( CodeGen : CodeGenTyp ; p : Proc ) 
  = VAR LProc : LLProcTyp 
  ; BEGIN 
      LProc := NARROW ( p , LLProcTyp ) 
    ; IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "end_procedure" ) 
      ; CodeGen . wr . PName ( p ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; procedure_epilogue ( CodeGen ) 
 
    ; <* ASSERT CodeGen . in_proc *> 
      <* ASSERT CodeGen . CurrentProc = p *> 
 
      CodeGen . CurrentProc . framesize 
        := Word . And ( CodeGen . CurrentProc . framesize + 3 , 16_FFFFFFFC ) 
 
    ; CodeGen . obj . patch 
        ( Seg . Text , CodeGen . procframe_ptr , CodeGen . CurrentProc . framesize , 4 ) 
 
    ; CodeGen . in_proc := FALSE 
 
    ; CodeGen . obj . end_procedure ( LProc . symbol ) 
 
    ; end_block ( CodeGen ) 
    END end_procedure 
 
; PROCEDURE begin_block ( CodeGen : CodeGenTyp ) 
  = 
        (* marks the beginning of a nested anonymous block *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "begin_block" ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; INC ( CodeGen . next_scope ) 
    END begin_block 
 
; PROCEDURE end_block ( CodeGen : CodeGenTyp ) 
  = 
        (* marks the ending of a nested anonymous block *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "end_block" ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; 
        <* ASSERT CodeGen . next_scope > 1 *> 
      DEC ( CodeGen . next_scope ) 
 
    ; free_locals ( CodeGen , CodeGen . next_scope ) 
    END end_block 
 
; PROCEDURE free_locals ( CodeGen : CodeGenTyp ; scope : INTEGER ) 
  = BEGIN 
      FOR i := 0 TO CodeGen . CurrentProc . tempsize - 1 
      DO 
        IF ( NOT CodeGen . CurrentProc . temparr [ i ] . free ) 
           AND CodeGen . CurrentProc . temparr [ i ] . var . scope = scope 
        THEN 
          CodeGen . CurrentProc . temparr [ i ] . free := TRUE 
        END 
      END 
    END free_locals 
 
; PROCEDURE note_procedure_origin ( CodeGen : CodeGenTyp ; p : Proc ) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "note_procedure_origin" ) 
      ; CodeGen . wr . PName ( p ) 
      ; CodeGen . wr . NL ( ) 
      END 
    END note_procedure_origin 
 
; 
(*------------------------------------------------------------ statements ---*) 
 
  PROCEDURE debug_set_label ( CodeGen : CodeGenTyp ; label : Label ) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . OutT ( "set_label" ) 
      ; CodeGen . wr . Lab ( label ) 
      ; CodeGen . wr . NL ( ) 
      END 
    END debug_set_label 
 
; PROCEDURE set_label ( CodeGen : CodeGenTyp ; label : Label ; 
                                           <*UNUSED*> barrier : BOOLEAN ) 
  = 
        (* define 'label' to be at the current pc *) 
    BEGIN 
      CodeGen . cg . set_label ( label ) 
 
    ; CodeGen . vstack . clearall ( ) 
    END set_label 
 
; PROCEDURE jump ( CodeGen : CodeGenTyp ; label : Label ) 
  = 
        (* GOTO label *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "jump" ) 
      ; CodeGen . wr . Lab ( label ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . cg . brOp ( Cond . Always , label ) 
    END jump 
 
; PROCEDURE if_true ( CodeGen : CodeGenTyp ; type : IType ; label : Label ; <*UNUSED*> f : Frequency ) 
  = 
        (* IF (s0 . type # 0) GOTO label ; pop *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "if_true" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . Lab ( label ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . doimm ( Op . oCMP , TZero , FALSE ) 
    ; CodeGen . cg . brOp ( Cond . NZ , label ) 
    END if_true 
 
; PROCEDURE if_false ( CodeGen : CodeGenTyp ; type : IType ; label : Label ; <*UNUSED*> f : Frequency ) 
  = 
        (* IF (s0 . type = 0) GOTO label ; pop *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "if_false" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . Lab ( label ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . doimm ( Op . oCMP , TZero , FALSE ) 
    ; CodeGen . cg . brOp ( Cond . Z , label ) 
    END if_false 
 
; PROCEDURE if_compare 
    ( CodeGen : CodeGenTyp 
    ; type : ZType 
    ; op : CompareOp 
    ; label : Label 
    ; 
                              <*UNUSED*> f : Frequency 
    ) 
  = 
(* IF (s1 . type  op  s0 . type) GOTO label ; pop(2) *) 
    VAR cond := CompareOpCond [ op ] 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "if_compare" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . OutT ( CompareOpName [ op ] ) 
      ; CodeGen . wr . Lab ( label ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CASE type 
      OF 
      | CGType . Word32 
      , CGType . Int32 
      , CGType . Word64 
      , CGType . Int64 
      , CGType . Addr 
        =>  CodeGen . vstack . unlock ( ) 
          ; IF CodeGen . vstack . dobin ( Op . oCMP , TRUE , FALSE , type ) 
            THEN 
              cond := revcond [ cond ] 
            END 
      | CGType . Reel , CGType . LReel , CGType . XReel 
        =>  IF CodeGen . cg . ftop_inmem 
            THEN 
              CodeGen . cg . binFOp ( FOp . fCOMP , 1 ) 
            ELSE 
              CodeGen . cg . binFOp ( FOp . fCOMPP , 1 ) 
            ; cond := revcond [ cond ] 
            END 
          ; CodeGen . vstack . discard ( 2 ) 
          ; CodeGen . vstack . unlock ( ) 
          ; CodeGen . vstack . corrupt ( EAX , operandPart := 0 ) 
          ; CodeGen . cg . noargFOp ( FOp . fNSTSWAX ) 
          ; CodeGen . cg . noargOp ( Op . oSAHF ) 
      END 
 
    ; CASE type 
      OF 
      | CGType . Word32 
      , CGType . Word64 
      , CGType . Addr 
      , CGType . Reel 
      , CGType . LReel 
      , CGType . XReel 
        => (* FCOM sets the unsigned compare flags *) 
            cond := unscond [ cond ] 
      ELSE 
      END 
 
    ; CodeGen . cg . brOp ( cond , label ) 
    END if_compare 
 
; PROCEDURE case_jump ( CodeGen : CodeGenTyp ; type : IType ; READONLY labels : ARRAY OF Label ) 
  = 
        (* "GOTO labels[s0 . type] ; pop" with no range checking on s0 . type *) 
    VAR stack0 : INTEGER 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "case_jump" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . Int ( NUMBER ( labels ) ) 
      ; FOR i := FIRST ( labels ) TO LAST ( labels ) 
        DO 
          CodeGen . wr . Lab ( labels [ i ] ) 
        END 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; stack0 := CodeGen . vstack . pos ( 0 , "case_jump" ) 
    ; CodeGen . vstack . unlock ( ) 
    ; CodeGen . vstack . find ( stack0 , Force . anyreg ) 
    ; CodeGen . cg . case_jump ( CodeGen . vstack . op ( stack0 ) , labels ) 
    ; CodeGen . vstack . discard ( 1 ) 
    END case_jump 
 
; PROCEDURE exit_proc ( CodeGen : CodeGenTyp ; type : CGType ) 
  = 
        (* Returns s0 . type if type is not Void, otherwise returns no value. *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "exit_proc" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; IF type # CGType . Void 
      THEN 
        CodeGen . vstack . unlock ( ) 
 
      ; WITH stack0 = CodeGen . vstack . pos ( 0 , "exit_proc" ) 
        DO 
          IF Target . FloatType [ type ] 
          THEN 
            CodeGen . cg . f_exitproc ( ) 
          ELSIF TypeIs64 ( type ) 
          THEN 
            CodeGen . vstack . find ( stack0 , Force . regset , RegSet { EAX , EDX } ) 
          ELSE 
            CodeGen . vstack . find ( stack0 , Force . regset , RegSet { EAX } ) 
          END 
        END 
 
      ; CodeGen . vstack . discard ( 1 ) 
      END 
 
    ; IF CodeGen . exit_proclabel = - 1 
      THEN 
        CodeGen . exit_proclabel := CodeGen . cg . reserve_labels ( 1 , FALSE ) 
      END 
 
    ; CodeGen . last_exitbranch := CodeGen . obj . cursor ( Seg . Text ) 
 
    ; CodeGen . cg . brOp ( Cond . Always , CodeGen . exit_proclabel ) 
    END exit_proc 
 
; PROCEDURE procedure_epilogue ( CodeGen : CodeGenTyp ) 
  = CONST NOP = 16_90 
  ; BEGIN 
      IF CodeGen . exit_proclabel = - 1 
      THEN 
        RETURN 
      (* Strange as it may seem, some procedures have no exit points. *) 
      END 
 
    ; IF CodeGen . last_exitbranch = CodeGen . obj . cursor ( Seg . Text ) - 5 
      THEN 
        (* Don't generate a branch to the epilogue at the last exit 
         point of the procedure *) 
        CodeGen . cg . set_label ( CodeGen . exit_proclabel , offset := - 5 ) 
      ; CodeGen . obj . backup ( Seg . Text , 5 ) 
      ELSE 
        CodeGen . cg . set_label ( CodeGen . exit_proclabel ) 
      END 
 
    ; IF CodeGen . proc_reguse [ EDI ] 
      THEN 
        CodeGen . cg . popOp ( CodeGen . cg . reg [ EDI ] ) 
      ELSE 
        CodeGen . obj . patch ( Seg . Text , CodeGen . procframe_ptr + 6 , NOP , 1 ) 
      END 
 
    ; IF CodeGen . proc_reguse [ ESI ] 
      THEN 
        CodeGen . cg . popOp ( CodeGen . cg . reg [ ESI ] ) 
      ELSE 
        CodeGen . obj . patch ( Seg . Text , CodeGen . procframe_ptr + 5 , NOP , 1 ) 
      END 
 
    ; IF CodeGen . proc_reguse [ EBX ] 
      THEN 
        CodeGen . cg . popOp ( CodeGen . cg . reg [ EBX ] ) 
      ELSE 
        CodeGen . obj . patch ( Seg . Text , CodeGen . procframe_ptr + 4 , NOP , 1 ) 
      END 
 
    ; CodeGen . cg . noargOp ( Op . oLEAVE ) 
    ; IF CodeGen . CurrentProc . stdcall 
      THEN 
        CodeGen . cg . cleanretOp ( CodeGen . CurrentProc . paramsize - 8 ) 
      ELSE 
        CodeGen . cg . noargOp ( Op . oRET ) 
      END 
    END procedure_epilogue 
 

(*------------------------------------------------------------ load/store ---*) 
 
; PROCEDURE load 
    ( CodeGen : CodeGenTyp 
    ; v : Var 
    ; o : ByteOffset 
    ; type : MType 
    ; type_multiple_of_32 : ZType 
    ) 
  = 
(* push; s0 . u := Mem [ ADR(v) + o ].type ;  The only allowed (type->u) conversions 
   are {Int,Word}{8,16} -> {Int,Word}{32,64} and {Int,Word}32 -> {Int,Word}64. 
   The source type, type, determines whether the value is sign-extended or 
   zero-extended. *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "load" ) 
      ; CodeGen . wr . VName ( v ) 
      ; CodeGen . wr . Int ( o ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . TName ( type_multiple_of_32 ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; 
        <* ASSERT CG_Bytes[type_multiple_of_32] >= CG_Bytes[type] *> 
 
      CodeGen . vstack . push 
        ( MVar { var := v , mvar_offset := o , mvar_type := type } ) 
    END load 
 
; PROCEDURE store 
    ( CodeGen : CodeGenTyp 
    ; v : Var 
    ; o : ByteOffset 
    ; type_multiple_of_32 : ZType 
    ; type : MType 
    ; 
    ) 
  = 
(* Mem [ ADR(v) + o ].u := s0 . type; pop *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "store" ) 
      ; CodeGen . wr . VName ( v ) 
      ; CodeGen . wr . Int ( o ) 
      ; CodeGen . wr . TName ( type_multiple_of_32 ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; 
        <* ASSERT CG_Bytes[type_multiple_of_32] >= CG_Bytes[type] *> 
 
      CodeGen . vstack . pop 
        ( MVar { var := v , mvar_offset := o , mvar_type := type } ) 
    END store 
 
; PROCEDURE load_address ( CodeGen : CodeGenTyp ; v : Var ; o : ByteOffset ) 
  = 
(* push; s0 . A := ADR(v) + o *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "load_address" ) 
      ; CodeGen . wr . VName ( v ) 
      ; CodeGen . wr . Int ( o ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . doloadaddress ( v , o ) 
    END load_address 
 
; PROCEDURE load_indirect 
    ( CodeGen : CodeGenTyp ; o : ByteOffset ; type : MType ; type_multiple_of_32 : ZType ) 
  = 
(* s0 . type_multiple_of_32 := Mem [s0 . A + o] . type  *) 
    VAR newreg : ARRAY OperandPart OF Regno 
  ;   size : OperandSize 
  ;   regset : RegSet 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "load_indirect" ) 
      ; CodeGen . wr . Int ( o ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . TName ( type_multiple_of_32 ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; 
        <* ASSERT CG_Bytes[type_multiple_of_32] >= CG_Bytes[type] *> 
 
      CodeGen . vstack . unlock ( ) 
 
    ; WITH stack0 = CodeGen . vstack . pos ( 0 , "load_indirect" ) 
      DO 
        CodeGen . vstack . find ( stack0 , Force . anyreg , AllRegisters , TRUE ) 
      ; IF Target . FloatType [ type ] 
        THEN 
          CodeGen . cg . f_loadind ( CodeGen . vstack . op ( stack0 ) , o , type ) 
        ; CodeGen . vstack . dealloc_reg ( stack0 , operandPart := 0 ) 
        ; CodeGen . vstack . set_fstack ( stack0 ) 
        ELSE 
          size := GetTypeSize ( type ) 
        ; 
            <* ASSERT size = GetTypeSize(type_multiple_of_32) *> 
 
          (* allocate the registers *) 
 
          IF CG_Bytes [ type ] = 1 
          THEN 
                <* ASSERT size = 1 *> 
            regset := RegistersForByteOperations 
          ELSE 
            regset := AllRegisters 
          END 
 
        ; FOR i := 0 TO size - 1 
          DO 
            newreg [ i ] := CodeGen . vstack . freereg ( regset , operandPart := i ) 
          ; regset := ( regset - RegSet { newreg [ i ] } ) 
          ; <* ASSERT newreg[i] # -1 *> 
          END 
 
        ; 
            (* do the loads *) 
 
          FOR i := 0 TO size - 1 
          DO 
            CodeGen . cg . load_ind 
              ( newreg [ i ] , CodeGen . vstack . op ( stack0 ) , o + 4 * i , type ) 
          END 
 
        ; 
            (* do the bookkeeping about the loads *) 
          (* previous contents of stack0 was just an address, no loop over size *) 
 
          CodeGen . vstack . dealloc_reg ( stack0 , operandPart := 0 ) 
        ; FOR i := 0 TO size - 1 
          DO 
            CodeGen . vstack . set_reg ( stack0 , newreg [ i ] , operandPart := i ) 
          END 
        END 
      ; CodeGen . vstack . set_type ( stack0 , type_multiple_of_32 ) 
      END 
    END load_indirect 
 
; PROCEDURE store_indirect 
    ( CodeGen : CodeGenTyp ; o : ByteOffset ; type_multiple_of_32 : ZType ; type : MType ) 
  = 
(* Mem [s1 . A + o] . type := s0 . type_multiple_of_32; pop (2) *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "store_indirect" ) 
      ; CodeGen . wr . Int ( o ) 
      ; CodeGen . wr . TName ( type_multiple_of_32 ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; 
        <* ASSERT CG_Bytes[type_multiple_of_32] >= CG_Bytes[type] *> 
 
      CodeGen . vstack . unlock ( ) 
    ; WITH (* stack0 = CodeGen . vstack . pos(0, "store_indirect"), *) 
        stack1 = CodeGen . vstack . pos ( 1 , "store_indirect" ) 
      DO 
        IF Target . FloatType [ type ] 
        THEN 
          CodeGen . vstack . find ( stack1 , Force . anyreg , AllRegisters , TRUE ) 
        ; CodeGen . cg . f_storeind ( CodeGen . vstack . op ( stack1 ) , o , type ) 
        ; CodeGen . vstack . discard ( 2 ) 
        ELSE 
          CodeGen . vstack . dostoreind ( o , type ) 
        END 
      END 
    END store_indirect 
 
; 
(*-------------------------------------------------------------- literals ---*) 
 
  PROCEDURE load_nil ( CodeGen : CodeGenTyp ) 
  = 
        (* push ; s0 . A := a *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "load_nil" ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . unlock ( ) 
    ; CodeGen . vstack . pushimmT ( TZero , CGType . Addr ) 
    END load_nil 
 
; PROCEDURE load_integer ( CodeGen : CodeGenTyp ; type : IType ; READONLY j : Target . Int ) 
  = 
        (* push ; s0 . type := i *) 
    VAR i := TIntN . FromTargetInt ( j , CG_Bytes [ type ] ) 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "load_integer" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . TInt ( i ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . unlock ( ) 
    ; CodeGen . vstack . pushimmT ( i , type ) 
    END load_integer 
 
; PROCEDURE load_float ( CodeGen : CodeGenTyp ; type : RType ; READONLY f : Target . Float ) 
  = 
        (* push ; s0 . type := f *) 
    VAR flarr : FloatBytes 
  ;   size : INTEGER 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "load_float" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . Flt ( f ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . pushnew ( type , Force . any ) 
    ; size := TFloat . ToBytes ( f , flarr ) 
    ; IF size # CG_Bytes [ type ] 
      THEN 
        Err ( CodeGen , "Floating size mismatch in load_float" ) 
      END 
    ; CodeGen . cg . f_loadlit ( flarr , type ) 
    END load_float 
 
; 
(*------------------------------------------------------------ arithmetic ---*) 
 
  PROCEDURE compare 
    ( CodeGen : CodeGenTyp ; type : ZType ; result_type : IType ; op : CompareOp ) 
  = 
        (* s1 . result_type := (s1 . type  op  s0 . type)  ; pop *) 
 
    (* 
    Comparison often needs to convert part of EFLAGS to a register-sized boolean. 
      Or if one is lucky, just do a conditional branch based on EFLAGS 
      and never materialize a register-sized boolean. 
 
    Historically Modula-3 m3back did comparisons like this. 
      cmp 
      setCcOp to memory temporary on stack; setcc only sets one byte 
      xor result_reg, result_reg ; result_reg := 0 
      mov result_reg_low, memory ; now have a register-sized boolean 
 
    We can do much better. 
 
    setCcOp is a family of instructions that materialize 
    a computation of EFLAGS as an 8 bit boolean. 
    There is sete, setne, setg, setl, seta, setb, etc. 
    Anything you might conditionally branch on jcc, also has setcc. 
    A "catch" however is it only gives you an 8 bit boolean, and 
    code often wants a register sized boolean. 
 
    Let's take the following C code as representative of our tasks, 
    and observe how the C compiler optimizes it, and match it. 
    That is a general technique I often follow, look 
    at the optimized C output and match it. 
 
        signed 
 
        int signed_LT(int a, int b) { return a < b; } 
            xor eax, eax 
            cmp 
            setl al 
 
        int signed_LE(int a, int b) { return a <= b; } 
            xor eax, eax 
            cmp 
            setle al 
 
        EQ and NE are the same for signed vs. unsigned 
 
        int EQ(int a, int b) { return a == b; } 
            xor eax, eax 
            cmp 
            sete al 
 
        int NE(int a, int b) { return a != b; } 
            xor eax, eax 
            cmp 
            setne al 
 
        GE and GT are the same as LT and LE but with either operands reversed 
            or the setcc condition altered. 
 
        int signed_GE(int a, int b) { return a >= b; } 
            xor eax, eax 
            cmp 
            setge al 
 
        int signed_GT(int a, int b) { return a > b; } 
            xor eax, eax 
            cmp 
            setg al 
 
        unsigned 
 
        int unsigned_LT(unsigned a, unsigned b) { return a < b; } 
            cmp 
            sbb eax, eax 
            neg eax 
 
        Let's understand this. 
            sbb is subtract with carry/borrow. 
            subtract from self is zero, and then carry/borrow 
            is one more -- either 0 or -1. 
            And then neg to turn -1 to 1. 
            So sbb, neg materialize carry as a register-sized boolean. 
 
        int unsigned_LE(unsigned a, unsigned b) { return a <= b; } 
            cmp 
            sbb eax, eax 
            inc eax 
 
        Let's understand this. 
            sbb is subtract with carry/borrow. 
            subtract from self is zero, and then carry/borrow 
            is one more -- either 0 or -1. 
            And then inc turns -1 to 0, 0 to 1. 
            So sbb, inc materialize carry as a register-sized boolean, inverted. 
 
        int unsigned_GE(unsigned a, unsigned b) { return a >= b; } 
            cmp parameters reversed vs. LT 
            sbb eax, eax ; see unsigned_LE for explanation 
            inc eax      ; see unsigned_LE for explanation 
 
        int unsigned_GT(unsigned a, unsigned b) { return a > b; } 
            cmp 
            sbb eax, eax ; see unsigned_LT for explanation 
            neg eax      ; see unsigned_LE for explanation 
 
        int unsigned_EQ(unsigned a, unsigned b) { return a == b; } 
            same as signed: 
            xor eax, eax 
            cmp 
            sete al 
 
        int unsigned_NE(unsigned a, unsigned b) { return a != b; } 
            same as signed: 
            xor eax, eax 
            cmp 
            setne al 
 
        Fill these in if they prove interesting. 
        Actually they are. 
        Signed comparison to zero of a value in a register is 
        sometimes done with test reg, reg. 
        Sometimes the zero for the return value in progress 
        doubles as the zero for the comparison. 
        Also unsigned compare to zero is special. For example, 
            unsigned values are always GE zero, never LT zero. 
 
        int signed_GE0(int a) { return a >= 0; } 
            xor eax, eax 
            if a is in memory 
                cmp a to eax 
            else if a is in register (__fastcall to simulate) 
                test a, a 
            setge al 
 
        int signed_GT0(int a) { return a > 0; } 
            xor eax, eax 
            if a is in memory 
                cmp a to eax 
            else if a is in register (__fastcall to simulate) 
                test a, a 
            setg al 
 
        int signed_LT0(int a) { return a < 0; } 
            xor eax, eax 
            if a is in memory 
                cmp a to eax 
            else if a is in register (__fastcall to simulate) 
                test a, a 
            setl al 
 
        int signed_LE0(int a) { return a <= 0; } 
            xor eax, eax 
            cmp to eax 
            setle al 
 
        int signed_EQ0(int a) { return a == 0; } 
            xor eax, eax 
            cmp to eax 
            sete al 
 
        int signed_NE0(int a, int b) { return a != 0; } 
            xor eax, eax 
            cmp to eax 
            setne al 
 
        int unsigned_GE0(unsigned a) { return a >= 0; } 
            This is always true. 
            xor eax, eax 
            inc eax 
 
        int unsigned_GT0(unsigned a) { return a > 0; } 
            xor eax, eax 
            cmp to eax (reversed?) 
            sbb eax, eax ; see unsigned_LT for explanation 
            neg eax      ; see unsigned_LT for explanation 
 
        int unsigned_LT0(unsigned a) { return a < 0; } 
            This is always false. 
            xor eax, eax 
 
        int unsigned_LE0(unsigned a) { return a <= 0; } 
            Same as EQ0. 
 
        int unsigned_EQ0(unsigned a) { return a == 0; } 
            input is in memory 
            xor eax, eax 
            cmp to eax 
            sete al 
 
        int __fastcall unsigned_EQ0(unsigned a) { return a == 0; } 
            input is in a register 
            xor eax, eax 
            test reg, reg 
            sete al 
 
        int unsigned_NE0(unsigned a) { return a != 0; } 
            same as EQ0 but setne instead of set 
 
    From the code for these functions, we observe that 
    there are multiple approaches, depending on what in EFLAGS is needed. 
 
    There is: 
        xor reg,reg 
        cmp 
        setcc reg_low_byte 
 
    and 
        cmp 
        some math involving EFLAGS, such as sbb. 
 
    The xor presumably has to precede the cmp in order to not lose the EFLAGS. 
 
  *) 
    VAR r : Regno := - 1 
  ;   reversed := FALSE 
  ;   cond := CompareOpCond [ op ] 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "compare" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . TName ( result_type ) 
      ; CodeGen . wr . OutT ( CompareOpName [ op ] ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; 
        <* ASSERT cond # Cond . Z AND cond # Cond . NZ *> 
 
      IF Target . FloatType [ type ] 
      THEN 
        condset ( CodeGen , cond , type ) 
      ; RETURN 
      END 
 
    ; IF TypeIsSigned ( type ) 
         OR op IN SET OF CompareOp { CompareOp . EQ , CompareOp . NE } 
      THEN 
        CodeGen . vstack . unlock ( ) 
      ; r 
          := CodeGen . vstack . freereg 
               ( RegistersForByteOperations , operandPart := 0 ) 
      ; CodeGen . cg . binOp ( Op . oXOR , CodeGen . cg . reg [ r ] , CodeGen . cg . reg [ r ] ) 
      ; reversed := CodeGen . vstack . dobin ( Op . oCMP , TRUE , FALSE , type ) 
      ; IF reversed 
        THEN 
          cond := revcond [ cond ] 
        END 
      ; CodeGen . cg . setccOp ( CodeGen . cg . reg [ r ] , cond ) 
      ELSE 
        CodeGen . vstack . unlock ( ) 
      ; IF op IN SET OF CompareOp { CompareOp . LE , CompareOp . GT } 
        THEN 
          CodeGen . vstack . swap ( ) 
        ; IF op = CompareOp . LE 
          THEN 
            op := CompareOp . GE 
          ELSE 
            op := CompareOp . LT 
          END 
        END 
      ; reversed := CodeGen . vstack . dobin ( Op . oCMP , FALSE , FALSE , type ) 
      ; 
          <* ASSERT NOT reversed *> 
        CodeGen . vstack . unlock ( ) 
      ; r := CodeGen . vstack . freereg ( operandPart := 0 ) 
      ; CodeGen . cg . binOp ( Op . oSBB , CodeGen . cg . reg [ r ] , CodeGen . cg . reg [ r ] ) 
      ; IF op = CompareOp . LT 
        THEN 
          CodeGen . cg . unOp ( Op . oNEG , CodeGen . cg . reg [ r ] ) 
        ELSE 
          CodeGen . cg . incOp ( CodeGen . cg . reg [ r ] ) 
        END 
      END 
    ; CodeGen . vstack . unlock ( ) 
    ; CodeGen . vstack . pushnew ( CGType . Word32 , Force . regset , RegSet { r } ) 
    END compare 
 
; PROCEDURE add ( CodeGen : CodeGenTyp ; type : AType ) 
  = 
        (* s1 . type := s1 . type + s0 . type ; pop *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "add" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; IF Target . FloatType [ type ] 
      THEN 
        CodeGen . cg . binFOp ( FOp . fADDP , 1 ) 
      ; CodeGen . vstack . discard ( 1 ) 
      ELSE 
        CodeGen . vstack . unlock ( ) 
      ; EVAL CodeGen . vstack . dobin ( Op . oADD , TRUE , TRUE , type ) 
      END 
    END add 
 
; PROCEDURE subtract ( CodeGen : CodeGenTyp ; type : AType ) 
  = 
        (* s1 . type := s1 . type - s0 . type ; pop *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "subtract" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; IF Target . FloatType [ type ] 
      THEN 
        CodeGen . cg . binFOp ( FOp . fSUBP , 1 ) 
      ; CodeGen . vstack . discard ( 1 ) 
      ELSE 
        CodeGen . vstack . unlock ( ) 
      ; EVAL CodeGen . vstack . dobin ( Op . oSUB , FALSE , TRUE , type ) 
      END 
    END subtract 
 
; PROCEDURE multiply ( CodeGen : CodeGenTyp ; type : AType ) 
  = 
        (* s1 . type := s1 . type * s0 . type ; pop *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "multiply" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; IF TypeIs64 ( type ) 
      THEN 
        start_int_proc ( CodeGen , Builtin . mul64 ) 
      ; pop_param ( CodeGen , CGType . Word64 ) 
      ; pop_param ( CodeGen , CGType . Word64 ) 
      ; call_64 ( CodeGen , Builtin . mul64 ) 
      ELSIF Target . FloatType [ type ] 
      THEN 
        CodeGen . cg . binFOp ( FOp . fMUL , 1 ) 
      ; CodeGen . vstack . discard ( 1 ) 
      ELSIF type = CGType . Int32 
      THEN 
        CodeGen . vstack . doimul ( ) 
      ELSE 
            <* ASSERT type = CGType . Word32 *> 
        CodeGen . vstack . doumul ( ) 
      END 
    END multiply 
 
; PROCEDURE divide ( CodeGen : CodeGenTyp ; type : RType ) 
  = 
        (* s1 . type := s1 . type / s0 . type ; pop *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "divide" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . cg . binFOp ( FOp . fDIV , 1 ) 
    ; CodeGen . vstack . discard ( 1 ) 
    END divide 
 
; CONST SignName = ARRAY Sign OF TEXT { " P" , " N" , " X" } 
 
; PROCEDURE div ( CodeGen : CodeGenTyp ; type : IType ; a , b : Sign ) 
  = 
        (* s1 . type := s1 . type DIV s0 . type ; pop *) 
    VAR builtin : Builtin 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "div" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . OutT ( SignName [ a ] ) 
      ; CodeGen . wr . OutT ( SignName [ b ] ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; IF TypeIs64 ( type ) 
      THEN 
        CASE type 
        OF CGType . Int64 
          =>  builtin := Builtin . div64 
        | CGType . Word64 
          =>  builtin := Builtin . udiv64 
        ELSE <* ASSERT FALSE *> 
        END 
      ; CodeGen . vstack . swap ( ) 
      ; start_int_proc ( CodeGen , builtin ) 
      ; pop_param ( CodeGen , CGType . Word64 ) 
      ; pop_param ( CodeGen , CGType . Word64 ) 
      ; call_64 ( CodeGen , builtin ) 
      ; RETURN 
      END 
 
    ; IF TypeIsUnsigned ( type ) 
      THEN 
        a := Sign . Positive 
      ; b := Sign . Positive 
      END 
 
    ; CodeGen . vstack . dodiv ( a , b ) 
    END div 
 
; PROCEDURE mod ( CodeGen : CodeGenTyp ; type : IType ; a , b : Sign ) 
  = 
        (* s1 . type := s1 . type MOD s0 . type ; pop *) 
    VAR builtin : Builtin 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "mod" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . OutT ( SignName [ a ] ) 
      ; CodeGen . wr . OutT ( SignName [ b ] ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; IF TypeIs64 ( type ) 
      THEN 
        CASE type 
        OF CGType . Int64 
          =>  builtin := Builtin . mod64 
        | CGType . Word64 
          =>  builtin := Builtin . umod64 
        ELSE <* ASSERT FALSE *> 
        END 
      ; CodeGen . vstack . swap ( ) 
      ; start_int_proc ( CodeGen , builtin ) 
      ; pop_param ( CodeGen , CGType . Word64 ) 
      ; pop_param ( CodeGen , CGType . Word64 ) 
      ; call_64 ( CodeGen , builtin ) 
      ; RETURN 
      END 
 
    ; IF TypeIsUnsigned ( type ) 
      THEN 
        a := Sign . Positive 
      ; b := Sign . Positive 
      END 
 
    ; CodeGen . vstack . domod ( a , b ) 
    END mod 
 
; PROCEDURE negate ( CodeGen : CodeGenTyp ; type : AType ) 
  = 
        (* s0 . type := - s0 . type *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "negate" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; IF Target . FloatType [ type ] 
      THEN 
        CodeGen . cg . noargFOp ( FOp . fCHS ) 
      ELSE 
        CodeGen . vstack . doneg ( ) 
      END 
    END negate 
 
; PROCEDURE abs ( CodeGen : CodeGenTyp ; type : AType ) 
  = 
        (* s0 . type := ABS (s0 . type) (noop on Words) *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "abs" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; IF TypeIsUnsigned ( type ) 
      THEN 
        RETURN 
      ELSIF TypeIsSigned ( type ) 
      THEN 
        CodeGen . vstack . doabs ( ) 
      ELSE 
        CodeGen . cg . noargFOp ( FOp . fABS ) 
      END 
    END abs 
 
; PROCEDURE max ( CodeGen : CodeGenTyp ; type : ZType ) 
  = 
        (* s1 . type := MAX (s1 . type, s0 . type) ; pop *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "max" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . domaxmin ( type , MaxMin . Max ) 
    END max 
 
; PROCEDURE min ( CodeGen : CodeGenTyp ; type : ZType ) 
  = 
        (* s1 . type := MIN (s1 . type, s0 . type) ; pop *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "min" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . domaxmin ( type , MaxMin . Min ) 
    END min 
 
; PROCEDURE cvt_int ( CodeGen : CodeGenTyp ; type : RType ; itype : IType ; op : ConvertOp ) 
  = 
        (* s0 . x := ROUND (s0 . type) *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "cvt_int" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . TName ( itype ) 
      ; CodeGen . wr . OutT ( ConvertOpName [ op ] ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . fltoint ( ConvertOpKind [ op ] , itype ) 
    END cvt_int 
 
; PROCEDURE cvt_float ( CodeGen : CodeGenTyp ; type : AType ; rtype : RType ) 
  = 
        (* s0 . x := FLOAT (s0 . type, rtype) *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "cvt_float" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . TName ( rtype ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; IF Target . FloatType [ type ] 
      THEN 
        RETURN 
      END 
 
    ; CodeGen . vstack . inttoflt ( ) 
    END cvt_float 
 
; 
(*------------------------------------------------------------------ sets ---*) 
 
  PROCEDURE set_op3 ( CodeGen : CodeGenTyp ; s : ByteSize ; builtin : Builtin ) 
  = 
        (* s2 . B := s1 . B op s0 . B ; pop(3) *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( BuiltinDesc [ builtin ] . name ) 
      ; CodeGen . wr . Int ( s ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; start_int_proc ( CodeGen , builtin ) 
    ; load_stack_param ( CodeGen , CGType . Addr , 2 ) 
    ; load_stack_param ( CodeGen , CGType . Addr , 1 ) 
    ; load_stack_param ( CodeGen , CGType . Addr , 0 ) 
    ; CodeGen . vstack . discard ( 3 ) 
    ; CodeGen . vstack . pushimmI ( s * 8 , CGType . Word32 ) 
    ; pop_param ( CodeGen , CGType . Word32 ) 
    ; call_int_proc ( CodeGen , builtin ) 
    END set_op3 
 
; PROCEDURE set_union ( CodeGen : CodeGenTyp ; s : ByteSize ) 
  = 
        (* s2 . B := s1 . B + s0 . B ; pop(3) *) 
    BEGIN 
      set_op3 ( CodeGen , s , Builtin . set_union ) 
    END set_union 
 
; PROCEDURE set_difference ( CodeGen : CodeGenTyp ; s : ByteSize ) 
  = 
        (* s2 . B := s1 . B - s0 . B ; pop(3) *) 
    BEGIN 
      set_op3 ( CodeGen , s , Builtin . set_difference ) 
    END set_difference 
 
; PROCEDURE set_intersection ( CodeGen : CodeGenTyp ; s : ByteSize ) 
  = 
        (* s2 . B := s1 . B * s0 . B ; pop(3) *) 
    BEGIN 
      set_op3 ( CodeGen , s , Builtin . set_intersection ) 
    END set_intersection 
 
; PROCEDURE set_sym_difference ( CodeGen : CodeGenTyp ; s : ByteSize ) 
  = 
        (* s2 . B := s1 . B / s0 . B ; pop(3) *) 
    BEGIN 
      set_op3 ( CodeGen , s , Builtin . set_sym_difference ) 
    END set_sym_difference 
 
; PROCEDURE set_member ( CodeGen : CodeGenTyp ; s : ByteSize ; type : IType ) 
  = 
        (* s1 . type := (s0 . type IN s1 . B) ; pop *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "set_member" ) 
      ; CodeGen . wr . Int ( s ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . unlock ( ) 
 
    ; WITH stack0 = CodeGen . vstack . pos ( 0 , "set_member" ) 
      , stack1 = CodeGen . vstack . pos ( 1 , "set_member" ) 
      DO 
 
        (* Better would be: 
      IF CodeGen . vstack . loc(stack0) # OLoc . imm OR TWordN . GT(u . vstack . op(stack0).imm, TWordN . Max8) THEN 
        CodeGen . vstack . find(stack0, Force . anyreg); 
      ELSE 
        CodeGen . vstack . find(stack0, Force . any); 
      END; 
      CodeGen . vstack . find(stack1, Force . any); 
 
      but we don't have things quite right, so settle. 
      *) 
        CodeGen . vstack . find ( stack0 , Force . anyreg ) 
      ; CodeGen . vstack . find ( stack1 , Force . anyreg ) 
 
      ; CodeGen . cg . bitTestOp 
          ( CodeGen . vstack . op ( stack1 ) , CodeGen . vstack . op ( stack0 ) ) 
      ; CodeGen . vstack . discard ( 2 ) 
      END 
 
    ; CodeGen . vstack . unlock ( ) 
 
    ; 
      (* see the end of condset 
 
    CodeGen . vstack . pushnew(Type . Word8, Force . mem); 
    WITH stop0 = CodeGen . vstack . op(u . vstack . pos(0, "set_singleton")) DO 
      stop0 . mvar . var . stack_temp := FALSE; 
      CodeGen . cg . setccOp(stop0, Cond . B);          B = unsigned below = C = carry 
    END; 
 
    4 instructions: 
 
    0000003F: 0F 92 45 F0        setb        byte ptr [ebp-10h] 
    00000043: 33 D2              xor         edx,edx 
    00000045: 8A 55 F0           mov         dl,byte ptr [ebp-10h] 
    00000048: 89 55 F4           mov         dword ptr [ebp-0Ch],edx 
 
    Let's try something else. 
    Goal is to capture the carry flag as a boolean in a Word. 
    *) 
 
        (* Convert carry to register-sized boolean. *) 
 
      CodeGen . vstack . pushnew ( CGType . Word32 , Force . anyreg ) 
    ; WITH stop0 = CodeGen . vstack . op ( CodeGen . vstack . pos ( 0 , "set_member" ) ) 
      DO 
        CodeGen . cg . binOp ( Op . oSBB , stop0 , stop0 ) 
      ; (* 0 if carry was clear, -1 if carry was set *) 
        CodeGen . cg . unOp ( Op . oNEG , stop0 ) 
      ;                                  (* 0 if carry was clear,  1 if carry was set *) 
      END 
 
    ; 
    END set_member 
 
; PROCEDURE set_compare ( CodeGen : CodeGenTyp ; s : ByteSize ; op : CompareOp ; type : IType ) 
  = 
        (* s1 . type := (s1 . B  op  s0 . B)  ; pop *) 
    VAR proc : Builtin 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "set_compare" ) 
      ; CodeGen . wr . Int ( s ) 
      ; CodeGen . wr . OutT ( CompareOpName [ op ] ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; IF op = CompareOp . EQ OR op = CompareOp . NE 
      THEN 
        proc := Builtin . memcmp 
      ; start_int_proc ( CodeGen , proc ) 
      ; CodeGen . vstack . pushimmI ( s , CGType . Word32 ) 
      ; pop_param ( CodeGen , CGType . Word32 ) 
      ; pop_param ( CodeGen , CGType . Addr ) 
      ; pop_param ( CodeGen , CGType . Addr ) 
      ; call_int_proc ( CodeGen , proc ) 
 
      ; 
        (* If EAX = 0, we want 1, if EAX # 0, we want 0. 
       * Compile this and match it: 
       * int F1(); 
       * int F2() { return F1() == 0; } 
       * int F3() { return F1() != 0; } 
       *) 
        CodeGen . cg . unOp ( Op . oNEG , CodeGen . cg . reg [ EAX ] ) 
      ; CodeGen . cg . binOp 
          ( Op . oSBB , CodeGen . cg . reg [ EAX ] , CodeGen . cg . reg [ EAX ] ) 
      ; IF op = CompareOp . EQ 
        THEN 
          CodeGen . cg . incOp ( CodeGen . cg . reg [ EAX ] ) 
        ELSE 
          CodeGen . cg . unOp ( Op . oNEG , CodeGen . cg . reg [ EAX ] ) 
        END 
      ELSE 
        proc := CompareOpProc [ op ] 
      ; start_int_proc ( CodeGen , proc ) 
      ; CodeGen . vstack . swap ( ) 
      ; pop_param ( CodeGen , CGType . Addr ) 
      ; pop_param ( CodeGen , CGType . Addr ) 
      ; CodeGen . vstack . pushimmI ( s * 8 , CGType . Word32 ) 
      ; pop_param ( CodeGen , CGType . Word32 ) 
      ; call_int_proc ( CodeGen , proc ) 
      END 
 
    ; 
    END set_compare 
 
; PROCEDURE set_range ( CodeGen : CodeGenTyp ; s : ByteSize ; type : IType ) 
  = 
        (* s2 . A [s1 . type .. s0 . type] := 1's; pop(3) *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "set_range" ) 
      ; CodeGen . wr . Int ( s ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; start_int_proc ( CodeGen , Builtin . set_range ) 
    ; load_stack_param ( CodeGen , CGType . Addr , 2 ) 
    ; load_stack_param ( CodeGen , type , 1 ) 
    ; pop_param ( CodeGen , type ) 
    ; CodeGen . vstack . discard ( 2 ) 
    ; call_int_proc ( CodeGen , Builtin . set_range ) 
    END set_range 
 
; PROCEDURE set_singleton ( CodeGen : CodeGenTyp ; s : ByteSize ; type : IType ) 
  = 
        (* s1 . A [s0 . type] := 1; pop(2) *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "set_singleton" ) 
      ; CodeGen . wr . Int ( s ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; 
        (* bit test and set -- we don't care about the test *) 
 
      WITH stack0 = CodeGen . vstack . pos ( 0 , "set_singleton" ) 
      , stack1 = CodeGen . vstack . pos ( 1 , "set_singleton" ) 
      DO 
 
        CodeGen . vstack . unlock ( ) 
 
      ; 
        (* single byte constants can be immediate 
       * but the front end never generates that 
       *) 
 
        <* ASSERT CodeGen . vstack . loc(stack0) # OLoc . imm *> 
 
         (*IF CodeGen . vstack . loc(stack0) # OLoc . imm OR TWordN . GT(u . vstack . op(stack0).imm, TWordN . Max8) THEN*) 
        CodeGen . vstack . find ( stack0 , Force . anyreg ) 
      ; 
        (*ELSE*) 
            (*u . vstack . find(stack0, Force . any);*) 
    (*END;*) 
        CodeGen . vstack . find ( stack1 , Force . any ) 
      ; CodeGen . cg . bitTestAndSetOp 
          ( CodeGen . vstack . op ( stack1 ) , CodeGen . vstack . op ( stack0 ) ) 
      ; CodeGen . vstack . discard ( 2 ) 
      END 
 
    END set_singleton 
 
; 
(*------------------------------------------------- Word.T bit operations ---*) 
 
  PROCEDURE not ( CodeGen : CodeGenTyp ; type : IType ) 
  = 
        (* s0 . type := Word . Not (s0 . type) *) 
    VAR not : TIntN . T 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "not" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; WITH stack0 = CodeGen . vstack . pos ( 0 , "not" ) 
      DO 
        IF CodeGen . vstack . loc ( stack0 ) = OLoc . imm 
        THEN 
          TWordN . Not ( CodeGen . vstack . op ( stack0 ) . imm , not ) 
        ; CodeGen . vstack . set_imm ( stack0 , not ) 
        ELSE 
          CodeGen . vstack . unlock ( ) 
        ; CodeGen . vstack . find ( stack0 , Force . anytemp ) 
        ; CodeGen . cg . unOp ( Op . oNOT , CodeGen . vstack . op ( stack0 ) ) 
        ; CodeGen . vstack . newdest ( CodeGen . vstack . op ( stack0 ) ) 
        END 
      END 
    END not 
 
; PROCEDURE and ( CodeGen : CodeGenTyp ; type : IType ) 
  = 
        (* s1 . type := Word . And (s1 . type, s0 . type) ; pop *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "and" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . unlock ( ) 
    ; EVAL CodeGen . vstack . dobin ( Op . oAND , TRUE , TRUE , type ) 
    END and 
 
; PROCEDURE or ( CodeGen : CodeGenTyp ; type : IType ) 
  = 
        (* s1 . type := Word . Or  (s1 . type, s0 . type) ; pop *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "or" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . unlock ( ) 
    ; EVAL CodeGen . vstack . dobin ( Op . oOR , TRUE , TRUE , type ) 
    END or 
 
; PROCEDURE xor ( CodeGen : CodeGenTyp ; type : IType ) 
  = 
        (* s1 . type := Word . Xor (s1 . type, s0 . type) ; pop *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "xor" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . unlock ( ) 
    ; EVAL CodeGen . vstack . dobin ( Op . oXOR , TRUE , TRUE , type ) 
    END xor 
 
; PROCEDURE shift_left ( CodeGen : CodeGenTyp ; type : IType ) 
  = 
        (* s1 . type := Word . Shift  (s1 . type, s0 . type) ; pop *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "shift_left" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; EVAL CodeGen . vstack . doshift ( type , ShiftType . LeftAlreadyBounded ) 
    END shift_left 
 
; PROCEDURE shift_right ( CodeGen : CodeGenTyp ; type : IType ) 
  = 
        (* s1 . type := Word . Shift  (s1 . type, -s0 . type) ; pop *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "shift_right" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; EVAL CodeGen . vstack . doshift ( type , ShiftType . RightAlreadyBounded ) 
    END shift_right 
 
; PROCEDURE shift ( CodeGen : CodeGenTyp ; type : IType ) 
  = 
        (* s1 . type := Word . Shift  (s1 . type, s0 . type) ; pop *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "shift" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; EVAL CodeGen . vstack . doshift ( type , ShiftType . UnboundedPositiveIsLeft ) 
    END shift 
 
; PROCEDURE rotate ( CodeGen : CodeGenTyp ; type : IType ) 
  = 
        (* s1 . type := Word . Rotate (s1 . type, s0 . type) ; pop *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "rotate" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; IF CodeGen . vstack . dorotate ( type ) 
      THEN 
        RETURN 
      END 
 
    ; do_rotate_or_shift_64 ( CodeGen , Builtin . rotate64 ) 
    END rotate 
 
; PROCEDURE rotate_left ( CodeGen : CodeGenTyp ; type : IType ) 
  = 
        (* s1 . type := Word . Rotate (s1 . type, s0 . type) ; pop *) 
    VAR rotateCount : INTEGER 
  ;   rotate : TIntN . T 
  ;   and : TIntN . T 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "rotate_left" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . unlock ( ) 
    ; WITH stack0 = CodeGen . vstack . pos ( 0 , "rotate_left" ) 
      , stack1 = CodeGen . vstack . pos ( 1 , "rotate_left" ) 
      DO 
        IF CodeGen . vstack . loc ( stack0 ) = OLoc . imm 
        THEN 
          IF CodeGen . vstack . loc ( stack1 ) = OLoc . imm 
          THEN 
            IF NOT TIntN . ToHostInteger 
                 ( CodeGen . vstack . op ( stack0 ) . imm , rotateCount ) 
            THEN 
              Err ( CodeGen , "unable to convert rotate count to host integer" ) 
            END 
          ; TWordN . Rotate 
              ( CodeGen . vstack . op ( stack1 ) . imm , rotateCount , rotate ) 
          ; CodeGen . vstack . set_imm ( stack1 , rotate ) 
          ELSE 
            TWordN . And 
              ( CodeGen . vstack . op ( stack0 ) . imm , MaximumShift [ type ] , and ) 
          ; CodeGen . vstack . set_imm ( stack0 , and ) 
          ; IF TypeIs64 ( type ) 
            THEN 
              do_rotate_or_shift_64 ( CodeGen , Builtin . rotate_left64 ) 
            ; RETURN 
            END 
          ; CodeGen . vstack . find ( stack1 , Force . anytemp ) 
          ; CodeGen . cg . immOp 
              ( Op . oROL 
              , CodeGen . vstack . op ( stack1 ) 
              , CodeGen . vstack . op ( stack0 ) . imm 
              ) 
          ; CodeGen . vstack . newdest ( CodeGen . vstack . op ( stack1 ) ) 
          END 
        ELSE 
          IF TypeIs64 ( type ) 
          THEN 
            do_rotate_or_shift_64 ( CodeGen , Builtin . rotate_left64 ) 
          ; RETURN 
          END 
        ; CodeGen . vstack . find ( stack0 , Force . regset , RegSet { ECX } ) 
        ; CodeGen . vstack . find ( stack1 , Force . anytemp ) 
 
        ; IF CodeGen . vstack . loc ( stack1 ) = OLoc . imm 
          THEN 
            CodeGen . vstack . find ( stack1 , Force . anyreg ) 
          END 
 
        ; CodeGen . cg . unOp ( Op . oROL , CodeGen . vstack . op ( stack1 ) ) 
        ; CodeGen . vstack . newdest ( CodeGen . vstack . op ( stack1 ) ) 
        END 
 
      ; CodeGen . vstack . discard ( 1 ) 
      END 
    END rotate_left 
 
; PROCEDURE rotate_right ( CodeGen : CodeGenTyp ; type : IType ) 
  = 
        (* s1 . type := Word . Rotate (s1 . type, -s0 . type) ; pop *) 
    VAR rotateCount : INTEGER 
  ;   rotate : TIntN . T 
  ;   and : TIntN . T 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "rotate_right" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . unlock ( ) 
    ; WITH stack0 = CodeGen . vstack . pos ( 0 , "rotate_right" ) 
      , stack1 = CodeGen . vstack . pos ( 1 , "rotate_right" ) 
      DO 
        IF CodeGen . vstack . loc ( stack0 ) = OLoc . imm 
        THEN 
          IF CodeGen . vstack . loc ( stack1 ) = OLoc . imm 
          THEN 
            IF NOT TIntN . ToHostInteger 
                 ( CodeGen . vstack . op ( stack0 ) . imm , rotateCount ) 
            THEN 
              Err ( CodeGen , "unable to convert rotate count to host integer" ) 
            END 
          ; TWordN . Rotate 
              ( CodeGen . vstack . op ( stack1 ) . imm , - rotateCount , rotate ) 
          ; CodeGen . vstack . set_imm ( stack1 , rotate ) 
          ELSE 
            TWordN . And 
              ( CodeGen . vstack . op ( stack0 ) . imm , MaximumShift [ type ] , and ) 
          ; CodeGen . vstack . set_imm ( stack0 , and ) 
          ; IF TypeIs64 ( type ) 
            THEN 
              do_rotate_or_shift_64 ( CodeGen , Builtin . rotate_right64 ) 
            ; RETURN 
            END 
          ; CodeGen . vstack . find ( stack1 , Force . anytemp ) 
          ; CodeGen . cg . immOp 
              ( Op . oROR 
              , CodeGen . vstack . op ( stack1 ) 
              , CodeGen . vstack . op ( stack0 ) . imm 
              ) 
          ; CodeGen . vstack . newdest ( CodeGen . vstack . op ( stack1 ) ) 
          END 
        ELSE 
          IF TypeIs64 ( type ) 
          THEN 
            do_rotate_or_shift_64 ( CodeGen , Builtin . rotate_right64 ) 
          ; RETURN 
          END 
        ; CodeGen . vstack . find ( stack0 , Force . regset , RegSet { ECX } ) 
        ; CodeGen . vstack . find ( stack1 , Force . anytemp ) 
 
        ; IF CodeGen . vstack . loc ( stack1 ) = OLoc . imm 
          THEN 
            CodeGen . vstack . find ( stack1 , Force . anyreg ) 
          END 
 
        ; CodeGen . cg . unOp ( Op . oROR , CodeGen . vstack . op ( stack1 ) ) 
        ; CodeGen . vstack . newdest ( CodeGen . vstack . op ( stack1 ) ) 
        END 
 
      ; CodeGen . vstack . discard ( 1 ) 
      END 
    END rotate_right 
 
; PROCEDURE widen ( CodeGen : CodeGenTyp ; sign_extend : BOOLEAN ) 
  = 
        (* s0 . I64 := s0 . I32;  IF sign_extend THEN SignExtend s0;  *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "widen" ) 
      ; CodeGen . wr . Bool ( sign_extend ) 
      ; CodeGen . wr . NL ( ) 
      END 
    ; <*ASSERT FALSE*> 
    END widen 
 
; PROCEDURE chop ( CodeGen : CodeGenTyp ) 
  = 
        (* s0 . I32 := Word . And (s0 . I64, 16_ffffffff);  *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "chop" ) 
      ; CodeGen . wr . NL ( ) 
      END 
    ; <*ASSERT FALSE*> 
    END chop 
 
; PROCEDURE extract ( CodeGen : CodeGenTyp ; type : IType ; sign_extend : BOOLEAN ) 
  = 
    (* s2 . type := Word . Extract(s2 . type, s1 . type, s0 . type); 
     IF sign_extend THEN SignExtend s2 END; pop(2) *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "extract" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . Bool ( sign_extend ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . doextract ( type , sign_extend ) 
    END extract 
 
; PROCEDURE extract_n ( CodeGen : CodeGenTyp ; type : IType ; sign_extend : BOOLEAN ; n : CARDINAL ) 
  = 
    (* s1 . type := Word . Extract(s1 . type, s0 . type, n); 
     IF sign_extend THEN SignExtend s1 END; pop(1) *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "extract_n" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . Bool ( sign_extend ) 
      ; CodeGen . wr . Int ( n ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . doextract_n ( type , sign_extend , n ) 
    END extract_n 
 
; PROCEDURE extract_mn ( CodeGen : CodeGenTyp ; type : IType ; sign_extend : BOOLEAN ; m , n : CARDINAL ) 
  = 
    (* s0 . type := Word . Extract(s0 . type, m, n); 
     IF sign_extend THEN SignExtend s0 END; *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "extract_mn" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . Bool ( sign_extend ) 
      ; CodeGen . wr . Int ( m ) 
      ; CodeGen . wr . Int ( n ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . doextract_mn ( type , sign_extend , m , n ) 
    END extract_mn 
 
; PROCEDURE insert ( CodeGen : CodeGenTyp ; type : IType ) 
  = 
        (* s3 . type := Word . Insert (s3 . type, s2 . type, s1 . type, s0 . type) ; pop(3) *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "insert" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . doinsert ( type ) 
    END insert 
 
; PROCEDURE insert_n ( CodeGen : CodeGenTyp ; type : IType ; n : CARDINAL ) 
  = 
        (* s2 . type := Word . Insert (s2 . type, s1 . type, s0 . type, n) ; pop(2) *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "insert_n" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . Int ( n ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . doinsert_n ( type , n ) 
    END insert_n 
 
; PROCEDURE insert_mn ( CodeGen : CodeGenTyp ; type : IType ; m , n : CARDINAL ) 
  = 
        (* s1 . type := Word . Insert (s1 . type, s0 . type, m, n) ; pop(2) *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "insert_mn" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . Int ( m ) 
      ; CodeGen . wr . Int ( n ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . doinsert_mn ( type , m , n ) 
    END insert_mn 
 
; 
(*------------------------------------------------ misc. stack/memory ops ---*) 
 
  PROCEDURE swap ( CodeGen : CodeGenTyp ; a , b : CGType ) 
  = 
        (* tmp := s1 ; s1 := s0 ; s0 := tmp *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "swap" ) 
      ; CodeGen . wr . TName ( a ) 
      ; CodeGen . wr . TName ( b ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . swap ( ) 
    END swap 
 
; PROCEDURE pop ( CodeGen : CodeGenTyp ; type : CGType ) 
  = 
        (* pop(1) (i . e. discard s0) *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "pop" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . unlock ( ) 
 
    ; IF Target . FloatType [ type ] 
      THEN 
        WITH stack0 = CodeGen . vstack . pos ( 0 , "pop" ) 
        DO 
              <* ASSERT CodeGen . vstack . loc(stack0) = OLoc . fstack *> 
          CodeGen . cg . fstack_discard ( ) 
        END 
      END 
 
    ; CodeGen . vstack . discard ( 1 ) 
    END pop 
 
; PROCEDURE copy_n 
    ( CodeGen : CodeGenTyp ; type_multiple_of_32 : IType ; type : MType ; overlap : BOOLEAN ) 
  = 
        (* Mem[s2 . A:s0 . type_multiple_of_32] := Mem[s1 . A:s0 . type_multiple_of_32]; pop(3)*) 
    CONST Mover 
      = ARRAY BOOLEAN OF Builtin { Builtin . memcpy , Builtin . memmove } 
  ; VAR n : INTEGER 
  ;   mover := Mover [ overlap ] 
  ;   shift : TIntN . T 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "copy_n" ) 
      ; CodeGen . wr . TName ( type_multiple_of_32 ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . Bool ( overlap ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; WITH stack0 = CodeGen . vstack . pos ( 0 , "copy_n" ) 
      DO 
        IF CodeGen . vstack . loc ( stack0 ) = OLoc . imm 
        THEN 
          IF NOT TIntN . ToHostInteger ( CodeGen . vstack . op ( stack0 ) . imm , n ) 
          THEN 
            Err ( CodeGen , "copy_n: unable to convert to host integer" ) 
          END 
        ; CodeGen . vstack . discard ( 1 ) 
        ; copy ( CodeGen , n , type , overlap ) 
        ; RETURN 
        END 
      END 
 
    ; IF CG_Bytes [ type ] # 1 
      THEN 
        WITH stack0 = CodeGen . vstack . pos ( 0 , "copy_n" ) 
        DO 
          CodeGen . vstack . unlock ( ) 
 
        ; CASE CG_Bytes [ type ] 
          OF 2 
            =>  shift := TIntN . One 
          | 4 
            =>  shift := TIntN . Two 
          | 8 
            =>  shift := TIntN . Three 
          ELSE 
            Err ( CodeGen , "Unknown MType size in copy_n" ) 
          END 
 
        ; CodeGen . vstack . find ( stack0 , Force . anyreg ) 
 
        ; CodeGen . cg . immOp ( Op . oSHL , CodeGen . vstack . op ( stack0 ) , shift ) 
        END 
      END 
 
    ; start_int_proc ( CodeGen , mover ) 
    ; pop_param ( CodeGen , type_multiple_of_32 ) 
    ; pop_param ( CodeGen , CGType . Addr ) 
    ; pop_param ( CodeGen , CGType . Addr ) 
    ; call_int_proc ( CodeGen , mover ) 
 
    ; CodeGen . vstack . discard ( 1 ) 
    END copy_n 
 
; CONST MAXINLINECOPY = 8 
 
; CONST faketype 
    = ARRAY [ 1 .. 4 ] OF MType 
        { CGType . Word8 , CGType . Word16 , CGType . Word32 , CGType . Word32 } 
 
; PROCEDURE inline_copy ( CodeGen : CodeGenTyp ; n , size : INTEGER ; forward : BOOLEAN ) 
  = VAR start , end , step : INTEGER 
  ;   movereg : Regno 
  ; BEGIN 
      IF forward 
      THEN 
        start := 0 
      ; end := n - 1 
      ; step := 1 
      ELSE 
        start := n - 1 
      ; end := 0 
      ; step := - 1 
      END 
 
    ; movereg := CodeGen . vstack . freereg ( operandPart := 0 ) 
 
    ; WITH stop0 = CodeGen . vstack . op ( CodeGen . vstack . pos ( 0 , "inline_copy" ) ) 
      , stop1 = CodeGen . vstack . op ( CodeGen . vstack . pos ( 1 , "inline_copy" ) ) 
      DO 
        FOR i := start TO end BY step 
        DO 
          CodeGen . cg . fast_load_ind ( movereg , stop0 , i * size , size ) 
        ; CodeGen . cg . store_ind 
            ( CodeGen . cg . reg [ movereg ] , stop1 , i * size , faketype [ size ] ) 
        END 
      END 
    END inline_copy 
 
; PROCEDURE string_copy ( CodeGen : CodeGenTyp ; n , size : INTEGER ; forward : BOOLEAN ) 
  = VAR tn , tNMinus1 , tsize , tint : TIntN . T 
  ; BEGIN 
      CodeGen . vstack . corrupt ( ECX , operandPart := 0 ) 
    ; CodeGen . cg . movImmI ( CodeGen . cg . reg [ ECX ] , n ) 
 
    ; IF forward 
      THEN 
        CodeGen . cg . noargOp ( Op . oCLD ) 
      ELSE 
        IF NOT TIntN . FromHostInteger ( n , Target . Integer . bytes , tn ) 
        THEN 
          Err ( CodeGen , "string_copy: unable to convert n to target int" ) 
        END 
      ; IF NOT TIntN . FromHostInteger 
             ( size , Target . Integer . bytes , tsize ) 
        THEN 
          Err ( CodeGen , "string_copy: unable to convert size to target int" ) 
        END 
      ; IF NOT TIntN . Subtract ( tn , TIntN . One , tNMinus1 ) 
        THEN 
          Err ( CodeGen , "string_copy: Subtract overflowed" ) 
        END 
      ; IF NOT TIntN . Multiply ( tNMinus1 , tsize , tint ) 
        THEN 
          Err ( CodeGen , "string_copy: Multiply overflowed" ) 
        END 
      ; CodeGen . cg . immOp ( Op . oADD , CodeGen . cg . reg [ ESI ] , tint ) 
      ; CodeGen . cg . immOp ( Op . oADD , CodeGen . cg . reg [ EDI ] , tint ) 
      ; CodeGen . cg . noargOp ( Op . oSTD ) 
      END 
 
    ; CodeGen . cg . noargOp ( Op . oREP ) 
    ; CASE size 
      OF 1 
        =>  CodeGen . cg . noargOp ( Op . oMOVSB ) 
      | 2 
        =>  CodeGen . cg . MOVSWOp ( ) 
      | 4 
        =>  CodeGen . cg . noargOp ( Op . oMOVSD ) 
      ELSE 
        Err ( CodeGen , "Illegal size in copy" ) 
      END 
 
    ; IF NOT forward 
      THEN 
        CodeGen . cg . noargOp ( Op . oCLD ) 
      END 
    END string_copy 
 
; PROCEDURE copy ( CodeGen : CodeGenTyp ; n : INTEGER ; type : MType ; overlap : BOOLEAN ) 
  = 
        (* Mem[s1 . A:sz] := Mem[s0 . A:sz]; pop(2)*) 
    VAR size := CG_Bytes [ type ] 
  ;   forward , end : Label := No_label 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "copy" ) 
      ; CodeGen . wr . Int ( n ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . Bool ( overlap ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; IF size = 1 AND Word . And ( n , 3 ) = 0 
      THEN 
        n := Word . Shift ( n , - 2 ) 
      ; size := 4 
      END 
 
    ; IF size = 2 AND Word . And ( n , 1 ) = 0 
      THEN 
        n := Word . Shift ( n , - 1 ) 
      ; size := 4 
      END 
 
    ; IF size = 8 
      THEN 
        n := Word . Shift ( n , 1 ) 
      ; size := 4 
      END 
 
    ; CodeGen . vstack . unlock ( ) 
 
    ; WITH stack0 = CodeGen . vstack . pos ( 0 , "copy" ) 
      , stack1 = CodeGen . vstack . pos ( 1 , "copy" ) 
      DO 
        IF n > MAXINLINECOPY 
        THEN 
          CodeGen . vstack . find ( stack0 , Force . regset , RegSet { ESI } ) 
        ; CodeGen . vstack . find ( stack1 , Force . regset , RegSet { EDI } ) 
        ; CodeGen . proc_reguse [ ESI ] := TRUE 
        ; CodeGen . proc_reguse [ EDI ] := TRUE 
        ELSE 
          CodeGen . vstack . find ( stack0 , Force . anyreg , AllRegisters , TRUE ) 
        ; CodeGen . vstack . find ( stack1 , Force . anyreg , AllRegisters , TRUE ) 
        END 
      END 
 
    ; IF overlap AND n > 1 
      THEN 
        forward := CodeGen . cg . reserve_labels ( 1 , TRUE ) 
      ; end := CodeGen . cg . reserve_labels ( 1 , TRUE ) 
      ; CodeGen . cg . binOp 
          ( Op . oCMP , CodeGen . cg . reg [ ESI ] , CodeGen . cg . reg [ EDI ] ) 
      ; CodeGen . cg . brOp ( Cond . GE , forward ) 
 
      ; IF n <= MAXINLINECOPY 
        THEN 
          inline_copy ( CodeGen , n , size , FALSE ) 
        ELSE 
          string_copy ( CodeGen , n , size , FALSE ) 
        END 
 
      ; CodeGen . cg . brOp ( Cond . Always , end ) 
      ; CodeGen . cg . set_label ( forward ) 
      END 
 
    ; IF n <= MAXINLINECOPY 
      THEN 
        inline_copy ( CodeGen , n , size , TRUE ) 
      ELSE 
        string_copy ( CodeGen , n , size , TRUE ) 
      END 
 
    ; IF overlap AND n > 1 
      THEN 
        CodeGen . cg . set_label ( end ) 
      END 
 
    ; IF n > MAXINLINECOPY 
      THEN 
        CodeGen . vstack . newdest ( CodeGen . cg . reg [ ESI ] ) 
      ; CodeGen . vstack . newdest ( CodeGen . cg . reg [ EDI ] ) 
      END 
 
    ; CodeGen . vstack . discard ( 2 ) 
    END copy 
 
; PROCEDURE zero_n ( CodeGen : CodeGenTyp ; type_multiple_of_32 : IType ; type : MType ) 
  = 
        (* Mem[s1 . A:s0 . type_multiple_of_32] := 0; pop(2) *) 
(*VAR n: INTEGER; 
      shift: TIntN . T;*) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "zero_n" ) 
      ; CodeGen . wr . TName ( type_multiple_of_32 ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; 
    <* ASSERT FALSE *> 
 
    (* zero_n is implemented incorrectly in the gcc backend, 
     * therefore it must not be used. 
 
    WITH stack0 = CodeGen . vstack . pos(0, "zero_n") DO 
      IF CodeGen . vstack . loc(stack0) = OLoc . imm THEN 
        IF NOT TIntN . ToHostInteger(u . vstack . op(stack0).imm, n) THEN 
          Err(u, "zero_n: unable to convert to host integer"); 
        END; 
        CodeGen . vstack . discard(1); 
        zero(u, n, type); 
        RETURN; 
      END 
    END; 
 
    IF CG_Bytes[type] # 1 THEN 
      WITH stack0 = CodeGen . vstack . pos(0, "zero_n") DO 
        CodeGen . vstack . unlock(); 
        CodeGen . vstack . find(stack0, Force . anyreg); 
 
        CASE CG_Bytes[type] OF 
          2 => shift := TIntN . One; 
        | 4 => shift := TIntN . Two; 
        | 8 => shift := TIntN . Three; 
        ELSE 
          Err(u, "Unknown MType size in zero_n"); 
        END; 
 
        CodeGen . cg . immOp(Op . oSHL, CodeGen . vstack . op(stack0), shift); 
      END 
    END; 
 
    start_int_proc (u, Builtin . memset); 
    pop_param (u, type_multiple_of_32); 
    CodeGen . vstack . pushimmT (TZero, CGType . Word32); 
    pop_param (u, CGType . Word32); 
    pop_param (u, CGType . Addr); 
    call_int_proc (u, Builtin . memset); 
 
    CodeGen . vstack . discard(1); 
    *) 
    END zero_n 
 
; PROCEDURE zero ( CodeGen : CodeGenTyp ; n : INTEGER ; type : MType ) 
  = 
        (* Mem[s0 . A:sz] := 0; pop(1) *) 
    VAR size := CG_Bytes [ type ] 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "zero" ) 
      ; CodeGen . wr . Int ( n ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; IF size = 1 AND Word . And ( n , 3 ) = 0 
      THEN 
        n := Word . Shift ( n , - 2 ) 
      ; size := 4 
      END 
 
    ; IF size = 2 AND Word . And ( n , 1 ) = 0 
      THEN 
        n := Word . Shift ( n , - 1 ) 
      ; size := 4 
      END 
 
    ; IF size = 8 
      THEN 
        n := Word . Shift ( n , 1 ) 
      ; size := 4 
      END 
 
    ; CodeGen . vstack . unlock ( ) 
 
    ; IF n > MAXINLINECOPY 
      THEN 
        CodeGen . vstack . find 
          ( CodeGen . vstack . pos ( 0 , "zero" ) , Force . regset , RegSet { EDI } ) 
      ; CodeGen . vstack . corrupt ( EAX , operandPart := 0 ) 
      ; CodeGen . vstack . corrupt ( ECX , operandPart := 0 ) 
 
      ; CodeGen . cg . binOp 
          ( Op . oXOR , CodeGen . cg . reg [ EAX ] , CodeGen . cg . reg [ EAX ] ) 
      ; CodeGen . cg . movImmI ( CodeGen . cg . reg [ ECX ] , n ) 
 
      ; CodeGen . cg . noargOp ( Op . oCLD ) 
      ; CodeGen . cg . noargOp ( Op . oREP ) 
      ; CASE size 
        OF 1 
          =>  CodeGen . cg . noargOp ( Op . oSTOSB ) 
        | 2 
          =>  CodeGen . cg . STOSWOp ( ) 
        | 4 
          =>  CodeGen . cg . noargOp ( Op . oSTOSD ) 
        ELSE 
          Err ( CodeGen , "Illegal size in zero" ) 
        END 
      ; CodeGen . vstack . newdest ( CodeGen . cg . reg [ EDI ] ) 
 
      ; 
      ELSE 
        WITH stack0 = CodeGen . vstack . pos ( 0 , "zero" ) 
        , stop0 = CodeGen . vstack . op ( stack0 ) 
        DO 
          CodeGen . vstack . find ( stack0 , Force . anyreg , AllRegisters , TRUE ) 
        ; FOR i := 0 TO n - 1 
          DO 
            CodeGen . cg . store_ind 
              ( Operand { loc := OLoc . imm , imm := TZero , optype := type } 
              , stop0 
              , i * size 
              , faketype [ size ] 
              ) 
          END 
        END 
      END 
 
    ; CodeGen . vstack . discard ( 1 ) 
    END zero 
 
; 
(*----------------------------------------------------- internal procedures ---*) 
 
  TYPE Builtin 
    = { set_union , set_difference , set_intersection , set_sym_difference 
      , set_range , set_lt , set_le , set_gt , set_ge , memmove , memcpy 
      , memset , memcmp , mul64 , udiv64 , umod64 , div64 , mod64 
      , rotate_left64 , rotate_right64 , rotate64 
      } 
 
; 
(* union .. sym_difference -> (n_bits, *c, *b, *a): Void 
   range                   -> (b, a, *s): Void 
   eq .. ge                -> (n_bits, *b, *a): Int 
   member                  -> (elt, *set): Int 
   singleton               -> (a, *s): Void *) 
 
  TYPE BP 
    = RECORD 
        name : TEXT 
      ; FormalsCt : INTEGER 
      ; (* counted in 32bit words *) 
        ret_type : CGType 
      ; lang : TEXT 
      ; 
      END 
 
; CONST BuiltinDesc 
    = ARRAY Builtin OF BP 
        { BP { "set_union" , 4 , CGType . Void , "__stdcall" } 
        , BP { "set_difference" , 4 , CGType . Void , "__stdcall" } 
        , BP { "set_intersection" , 4 , CGType . Void , "__stdcall" } 
        , BP { "set_sym_difference" , 4 , CGType . Void , "__stdcall" } 
        , BP { "set_range" , 3 , CGType . Void , "__stdcall" } 
        , BP { "set_lt" , 3 , CGType . Int32 , "__stdcall" } 
        , BP { "set_le" , 3 , CGType . Int32 , "__stdcall" } 
        , BP { "set_gt" , 3 , CGType . Int32 , "__stdcall" } 
        , BP { "set_ge" , 3 , CGType . Int32 , "__stdcall" } 
        , BP { "memmove" , 3 , CGType . Addr , "C" } 
        , BP { "memcpy" , 3 , CGType . Addr , "C" } 
        , BP { "memset" , 3 , CGType . Addr , "C" } 
        , BP { "memcmp" , 3 , CGType . Int32 , "C" } 
        , 
 
          (* custom calling convention: parameters pushed, removed 
     * by callee, but name is not __stdcall, call_64 pokes 
     * the parameter size to 0 
     *) 
          BP { "_allmul" , 0 , CGType . Word64 , "C" } 
        , (* 64bit multiply; signed or unsigned *) 
          BP { "_aulldiv" , 0 , CGType . Word64 , "C" } 
        , (* 64bit unsigned divide *) 
          BP { "_aullrem" , 0 , CGType . Word64 , "C" } 
        , (* 64bit unsigned mod/remainder *) 
 
          BP { "m3_div64" , 4 , CGType . Int64 , "__stdcall" } 
        , BP { "m3_mod64" , 4 , CGType . Int64 , "__stdcall" } 
        , BP { "m3_rotate_left64" , 3 , CGType . Word64 , "__stdcall" } 
        , BP { "m3_rotate_right64" , 3 , CGType . Word64 , "__stdcall" } 
        , BP { "m3_rotate64" , 3 , CGType . Word64 , "__stdcall" } 
        } 
 
 
; PROCEDURE start_int_proc ( CodeGen : CodeGenTyp ; b : Builtin ) 
  = BEGIN 
      WITH proc = CodeGen . builtins [ b ] , desc = BuiltinDesc [ b ] 
      DO 
        IF proc = NIL 
        THEN 
          proc 
            := import_procedure 
                 ( CodeGen 
                 , M3ID . Add ( desc . name ) 
                 , desc . FormalsCt 
                 , desc . ret_type 
                 , Target . FindConvention ( desc . lang ) 
                 ) 
        ; FOR i := 1 TO desc . FormalsCt 
          DO 
            EVAL 
              declare_param 
                ( CodeGen 
                , M3ID . NoID 
                , 4 
                , 4 
                , CGType . Word32 
                , 0 
                , FALSE 
                , FALSE 
                , 100 
                ) 
          END 
        END 
      ; start_call_direct ( CodeGen , proc , 0 , desc . ret_type ) 
      END 
    END start_int_proc 
 
; PROCEDURE call_int_proc ( CodeGen : CodeGenTyp ; b : Builtin ) 
  = BEGIN 
      call_direct ( CodeGen , CodeGen . builtins [ b ] , BuiltinDesc [ b ] . ret_type ) 
    END call_int_proc 
 
; 
(*----------------------------------------------------------- conversions ---*) 
 
  PROCEDURE loophole ( CodeGen : CodeGenTyp ; from , to : ZType ) 
  = 
        (* s0 . to := LOOPHOLE(s0 . from, to) *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "loophole" ) 
      ; CodeGen . wr . TName ( from ) 
      ; CodeGen . wr . TName ( to ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . doloophole ( from , to ) 
 
    ; 
    END loophole 
 
; 
(*------------------------------------------------ traps & runtime checks ---*) 
 
  PROCEDURE abort ( CodeGen : CodeGenTyp ; code : RuntimeError ) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "abort" ) 
      ; CodeGen . wr . Int ( ORD ( code ) ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; reportfault ( CodeGen , code ) 
    END abort 
 
; PROCEDURE check_nil ( CodeGen : CodeGenTyp ; code : RuntimeError ) 
  = 
        (* IF (s0 . A = NIL) THEN abort(code) *) 
    VAR safelab : Label 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "check_nil" ) 
      ; CodeGen . wr . Int ( ORD ( code ) ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . unlock ( ) 
    ; WITH stack0 = CodeGen . vstack . pos ( 0 , "check_nil" ) 
      DO 
        IF CodeGen . vstack . loc ( stack0 ) = OLoc . imm 
        THEN 
          IF TIntN . EQ ( CodeGen . vstack . op ( stack0 ) . imm , TZero ) 
          THEN 
            reportfault ( CodeGen , code ) 
          END 
        ELSE 
          CodeGen . vstack . find ( stack0 , Force . anyreg , AllRegisters , TRUE ) 
 
        ; IF NOT CodeGen . vstack . non_nil ( CodeGen . vstack . reg ( stack0 ) ) 
          THEN 
            CodeGen . cg . immOp ( Op . oCMP , CodeGen . vstack . op ( stack0 ) , TZero ) 
          ; safelab := CodeGen . cg . reserve_labels ( 1 , TRUE ) 
          ; CodeGen . cg . brOp ( Cond . NE , safelab ) 
          ; reportfault ( CodeGen , code ) 
          ; CodeGen . cg . set_label ( safelab ) 
          END 
 
        ; CodeGen . vstack . set_non_nil ( CodeGen . vstack . reg ( stack0 ) ) 
        END 
      END 
    END check_nil 
 
; PROCEDURE check_lo 
    ( CodeGen : CodeGenTyp ; type : IType ; READONLY j : Target . Int ; code : RuntimeError ) 
  = 
        (* IF (s0 . type < i) THEN abort(code) *) 
    VAR safelab : Label 
  ;   i := TIntN . FromTargetInt ( j , CG_Bytes [ type ] ) 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "check_lo" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . TInt ( i ) 
      ; CodeGen . wr . Int ( ORD ( code ) ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . unlock ( ) 
    ; WITH stack0 = CodeGen . vstack . pos ( 0 , "check_lo" ) 
      DO 
        IF CodeGen . vstack . loc ( stack0 ) = OLoc . imm 
        THEN 
          IF TIntN . LT ( CodeGen . vstack . op ( stack0 ) . imm , i ) 
          THEN 
            reportfault ( CodeGen , code ) 
          END 
        ELSE 
          CodeGen . vstack . find ( stack0 , Force . anyreg ) 
        ; IF TIntN . GE 
               ( CodeGen . vstack . lower ( CodeGen . vstack . reg ( stack0 ) ) , i ) 
          THEN 
            (* ok *) 
          ELSIF TIntN . LT 
               ( CodeGen . vstack . upper ( CodeGen . vstack . reg ( stack0 ) ) , i ) 
          THEN 
            reportfault ( CodeGen , code ) 
          ELSE 
            CodeGen . cg . immOp ( Op . oCMP , CodeGen . vstack . op ( stack0 ) , i ) 
          ; safelab := CodeGen . cg . reserve_labels ( 1 , TRUE ) 
          ; CodeGen . cg . brOp ( Cond . GE , safelab ) 
          ; reportfault ( CodeGen , code ) 
          ; CodeGen . cg . set_label ( safelab ) 
          ; CodeGen . vstack . set_lower ( CodeGen . vstack . reg ( stack0 ) , i ) 
          END 
        END 
      END 
    END check_lo 
 
; PROCEDURE check_hi 
    ( CodeGen : CodeGenTyp ; type : IType ; READONLY j : Target . Int ; code : RuntimeError ) 
  = 
        (* IF (i < s0 . type) THEN abort(code) *) 
    VAR safelab : Label 
  ;   i := TIntN . FromTargetInt ( j , CG_Bytes [ type ] ) 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "check_hi" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . TInt ( i ) 
      ; CodeGen . wr . Int ( ORD ( code ) ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . unlock ( ) 
    ; WITH stack0 = CodeGen . vstack . pos ( 0 , "check_hi" ) 
      DO 
        IF CodeGen . vstack . loc ( stack0 ) = OLoc . imm 
        THEN 
          IF TIntN . LT ( i , CodeGen . vstack . op ( stack0 ) . imm ) 
          THEN 
            reportfault ( CodeGen , code ) 
          END 
        ELSE 
          CodeGen . vstack . find ( stack0 , Force . anyreg ) 
        ; IF TIntN . LE 
               ( CodeGen . vstack . upper ( CodeGen . vstack . reg ( stack0 ) ) , i ) 
          THEN 
            (* ok *) 
          ELSIF TIntN . GT 
               ( CodeGen . vstack . lower ( CodeGen . vstack . reg ( stack0 ) ) , i ) 
          THEN 
            reportfault ( CodeGen , code ) 
          ELSE 
            CodeGen . cg . immOp ( Op . oCMP , CodeGen . vstack . op ( stack0 ) , i ) 
          ; safelab := CodeGen . cg . reserve_labels ( 1 , TRUE ) 
          ; CodeGen . cg . brOp ( Cond . LE , safelab ) 
          ; reportfault ( CodeGen , code ) 
          ; CodeGen . cg . set_label ( safelab ) 
          ; CodeGen . vstack . set_upper ( CodeGen . vstack . reg ( stack0 ) , i ) 
          END 
        END 
      END 
    END check_hi 
 
; PROCEDURE check_range 
    ( CodeGen : CodeGenTyp ; type : IType ; READONLY xa , xb : Target . Int ; code : RuntimeError ) 
  = 
        (* IF (s0 . type < a) OR (b < s0 . type) THEN abort(code) *) 
    VAR lo , hi : TIntN . T 
  ;   safelab , outrange : Label 
  ;   a := TIntN . FromTargetInt ( xa , CG_Bytes [ type ] ) 
  ;   b := TIntN . FromTargetInt ( xb , CG_Bytes [ type ] ) 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "check_range" ) 
      ; CodeGen . wr . TInt ( a ) 
      ; CodeGen . wr . TInt ( b ) 
      ; CodeGen . wr . Int ( ORD ( code ) ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . unlock ( ) 
    ; WITH stack0 = CodeGen . vstack . pos ( 0 , "check_range" ) 
      DO 
        IF CodeGen . vstack . loc ( stack0 ) = OLoc . imm 
        THEN 
          lo := CodeGen . vstack . op ( stack0 ) . imm 
        ; IF TIntN . LT ( lo , a ) OR TIntN . LT ( b , lo ) 
          THEN 
            reportfault ( CodeGen , code ) 
          END 
        ; RETURN 
        END 
 
      ; CodeGen . vstack . find ( stack0 , Force . anyreg ) 
      ; WITH reg = CodeGen . vstack . reg ( stack0 ) 
        DO 
          lo := CodeGen . vstack . lower ( reg ) 
        ; hi := CodeGen . vstack . upper ( reg ) 
        ; IF TIntN . LE ( a , lo ) AND TIntN . LE ( hi , b ) 
          THEN 
            (* ok *) 
          ELSIF TIntN . LT ( hi , a ) OR TIntN . LT ( b , lo ) 
          THEN 
            reportfault ( CodeGen , code ) 
          ELSIF TIntN . LE ( hi , b ) 
          THEN 
            check_lo ( CodeGen , type , xa , code ) 
          ELSIF TIntN . GE ( lo , a ) 
          THEN 
            check_hi ( CodeGen , type , xb , code ) 
          ELSIF TIntN . EQ ( a , TZero ) 
          THEN 
                (* 0 <= x <= b  ==>   UNSIGNED(x) <= b *) 
            safelab := CodeGen . cg . reserve_labels ( 1 , TRUE ) 
          ; CodeGen . cg . immOp ( Op . oCMP , CodeGen . vstack . op ( stack0 ) , b ) 
          ; CodeGen . cg . brOp ( unscond [ Cond . LE ] , safelab ) 
          ; reportfault ( CodeGen , code ) 
          ; CodeGen . cg . set_label ( safelab ) 
          ; CodeGen . vstack . set_upper ( reg , b ) 
          ; CodeGen . vstack . set_lower ( reg , a ) 
          ELSE 
            safelab := CodeGen . cg . reserve_labels ( 1 , TRUE ) 
          ; outrange := CodeGen . cg . reserve_labels ( 1 , TRUE ) 
          ; CodeGen . cg . immOp ( Op . oCMP , CodeGen . vstack . op ( stack0 ) , a ) 
          ; CodeGen . cg . brOp ( Cond . L , outrange ) 
          ; CodeGen . cg . immOp ( Op . oCMP , CodeGen . vstack . op ( stack0 ) , b ) 
          ; CodeGen . cg . brOp ( Cond . LE , safelab ) 
          ; CodeGen . cg . set_label ( outrange ) 
          ; reportfault ( CodeGen , code ) 
          ; CodeGen . cg . set_label ( safelab ) 
          ; CodeGen . vstack . set_upper ( reg , b ) 
          ; CodeGen . vstack . set_lower ( reg , a ) 
          END 
        END 
      END 
    END check_range 
 
; PROCEDURE check_index ( CodeGen : CodeGenTyp ; type : IType ; code : RuntimeError ) 
  = 
    (* IF NOT (0 <= s1 . type < s0 . type) THEN 
       abort(code) 
     END; 
     pop *) 
    (* s0 . type is guaranteed to be positive so the unsigned 
     check (s0 . W <= s1 . W) is sufficient. *) 
    VAR safelab : Label 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "check_index" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . Int ( ORD ( code ) ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . unlock ( ) 
    ; WITH stack0 = CodeGen . vstack . pos ( 0 , "check_index" ) 
      , stack1 = CodeGen . vstack . pos ( 1 , "check_index" ) 
      DO 
        IF CodeGen . vstack . loc ( stack0 ) = OLoc . imm 
           AND CodeGen . vstack . loc ( stack1 ) = OLoc . imm 
        THEN 
          IF TWordN . LE 
               ( CodeGen . vstack . op ( stack0 ) . imm 
               , CodeGen . vstack . op ( stack1 ) . imm 
               ) 
          THEN 
            reportfault ( CodeGen , code ) 
          END 
        ELSE 
 
          CodeGen . vstack . find ( stack0 , Force . any ) 
        ; CodeGen . vstack . find ( stack1 , Force . anyregimm ) 
        ; IF CodeGen . vstack . loc ( stack0 ) = OLoc . mem 
          THEN 
            CodeGen . vstack . find ( stack0 , Force . anyregimm ) 
          END 
 
        ; safelab := CodeGen . cg . reserve_labels ( 1 , TRUE ) 
 
        ; IF CodeGen . vstack . loc ( stack0 ) = OLoc . imm 
          THEN 
            CodeGen . cg . binOp 
              ( Op . oCMP 
              , CodeGen . vstack . op ( stack1 ) 
              , CodeGen . vstack . op ( stack0 ) 
              ) 
          ; CodeGen . cg . brOp ( Cond . B , safelab ) 
          ELSE 
            CodeGen . cg . binOp 
              ( Op . oCMP 
              , CodeGen . vstack . op ( stack0 ) 
              , CodeGen . vstack . op ( stack1 ) 
              ) 
          ; CodeGen . cg . brOp ( Cond . A , safelab ) 
          END 
 
        ; reportfault ( CodeGen , code ) 
        ; CodeGen . cg . set_label ( safelab ) 
        END 
      END 
 
    ; CodeGen . vstack . discard ( 1 ) 
    END check_index 
 
; PROCEDURE check_eq ( CodeGen : CodeGenTyp ; type : IType ; code : RuntimeError ) 
  = 
    (* IF (s0 . type # s1 . type) THEN 
       abort(code); 
       Pop (2) *) 
    VAR safelab : Label 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "check_eq" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . Int ( ORD ( code ) ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . unlock ( ) 
    ; WITH stack0 = CodeGen . vstack . pos ( 0 , "check_index" ) 
      , stack1 = CodeGen . vstack . pos ( 1 , "check_index" ) 
      DO 
        CodeGen . vstack . find ( stack0 , Force . any ) 
      ; CodeGen . vstack . find ( stack1 , Force . anyregimm ) 
      ; IF CodeGen . vstack . loc ( stack0 ) = OLoc . mem 
        THEN 
          CodeGen . vstack . find ( stack0 , Force . anyregimm ) 
        END 
 
      ; IF CodeGen . vstack . loc ( stack0 ) = OLoc . imm 
        THEN 
          CodeGen . cg . binOp 
            ( Op . oCMP , CodeGen . vstack . op ( stack1 ) , CodeGen . vstack . op ( stack0 ) ) 
        ELSE 
          CodeGen . cg . binOp 
            ( Op . oCMP , CodeGen . vstack . op ( stack0 ) , CodeGen . vstack . op ( stack1 ) ) 
        END 
 
      ; safelab := CodeGen . cg . reserve_labels ( 1 , TRUE ) 
      ; CodeGen . cg . brOp ( Cond . E , safelab ) 
      ; reportfault ( CodeGen , code ) 
      ; CodeGen . cg . set_label ( safelab ) 
      END 
 
    ; CodeGen . vstack . discard ( 2 ) 
    END check_eq 
 
; PROCEDURE reportfault ( CodeGen : CodeGenTyp ; code : RuntimeError ) 
  = 
        (* 32: see M3CG . RuntimeError, RuntimeError . T *) 
    VAR info := ORD ( code ) + CodeGen . lineno * 32 
  ; BEGIN 
            <* ASSERT ORD (code) < 32 *>(* lose fault code not ok *) 
    (* ASSERT CodeGen . lineno <= (LAST(INTEGER) DIV 32) *)   (* losing line number ok *) 
      CodeGen . cg. movImmI ( CodeGen . cg . reg [ EAX ] , info ) 
    ; CodeGen . cg . intCall ( CodeGen . reportlabel ) 
    ; CodeGen . usedfault := TRUE 
    END reportfault 
 
; PROCEDURE makereportproc ( CodeGen : CodeGenTyp ) 
  = VAR repproc : Proc 
  ;   labelname : TEXT 
  ;   reportsymbol : INTEGER 
  ; BEGIN 
      get_runtime_hook ( CodeGen , M3ID . Add ( "ReportFault" ) , repproc ) 
 
    ; CodeGen . cg . set_label ( CodeGen . reportlabel ) 
 
    ; labelname := M3ID . ToText ( CodeGen . global_var . VarName ) & "_CRASH" 
 
    ; reportsymbol 
        := CodeGen . obj . define_symbol 
             ( M3ID . Add ( labelname ) 
             , Seg . Text 
             , CodeGen . obj . cursor ( Seg . Text ) 
             ) 
    ; CodeGen . obj . begin_procedure ( reportsymbol ) 
 
    ; CodeGen . cg . pushOp ( CodeGen . cg . reg [ EBP ] ) 
    ; CodeGen . cg . movOp ( CodeGen . cg . reg [ EBP ] , CodeGen . cg . reg [ ESP ] ) 
 
    ; CodeGen . cg . pushOp ( CodeGen . cg . reg [ EAX ] ) 
    ;                            (* runtime error code + line number *) 
 
      IF ( repproc # NIL ) 
      THEN 
        start_call_direct ( CodeGen , repproc , 0 , CGType . Void ) 
      ; INC ( CodeGen . call_param_size [ CodeGen . in_proc_call - 1 ] , 4 ) 
      ; (* remember error code *) 
        load_address ( CodeGen , CodeGen . global_var , 0 ) 
      ; pop_param ( CodeGen , CGType . Addr ) 
      ; call_direct ( CodeGen , repproc , CGType . Void ) 
      ELSE 
        CodeGen . Err ( "cannot locate the runtime procedure: ReportFault !" ) 
      END 
 
    ; CodeGen . obj . end_procedure ( reportsymbol ) 
    END makereportproc 
 
; 
(*---------------------------------------------------- address arithmetic ---*) 
 
  PROCEDURE add_offset ( CodeGen : CodeGenTyp ; i : INTEGER ) 
  = 
        (* s0 . A := s0 . A + i *) 
    VAR ti , imm_plus_i : TIntN . T 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "add_offset" ) 
      ; CodeGen . wr . Int ( i ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; IF NOT TIntN . FromHostInteger ( i , Target . Integer . bytes , ti ) 
      THEN 
        Err ( CodeGen , "add_offset: failed to convert i to target integer" ) 
      END 
 
    ; CodeGen . vstack . unlock ( ) 
    ; WITH stack0 = CodeGen . vstack . pos ( 0 , "add_offset" ) 
      DO 
        IF CodeGen . vstack . loc ( stack0 ) = OLoc . imm 
        THEN 
          IF NOT TIntN . Add 
               ( CodeGen . vstack . op ( stack0 ) . imm , ti , imm_plus_i ) 
          THEN 
            Err ( CodeGen , "add_offset: Add overflowed" ) 
          END 
        ; CodeGen . vstack . set_imm ( stack0 , imm_plus_i ) 
        ELSE 
          CodeGen . vstack . find ( stack0 , Force . anytemp , AllRegisters , TRUE ) 
 
        ; CodeGen . cg . immOp ( Op . oADD , CodeGen . vstack . op ( stack0 ) , ti ) 
 
        ; CodeGen . vstack . newdest ( CodeGen . vstack . op ( stack0 ) ) 
        END 
      END 
    END add_offset 
 
; PROCEDURE log2 ( int : INTEGER ) : INTEGER 
  = 
(* Return log2(int) if int is a power of 2, -1 if it is 0, otherwise -2 *) 
    BEGIN 
      IF Word . And ( int , int - 1 ) # 0 
      THEN 
        RETURN - 2 
      END 
 
    ; IF int = 0 
      THEN 
        RETURN - 1 
      END 
 
    ; FOR i := 0 TO 31 
      DO 
        int := Word . Shift ( int , - 1 ) 
      ; IF int = 0 
        THEN 
          RETURN i 
        END 
      END 
 
    ; RETURN - 1 
    END log2 
 
; PROCEDURE index_address ( CodeGen : CodeGenTyp ; type : IType ; size : INTEGER ) 
  = 
        (* s1 . A := s1 . A + s0 . type * size ; pop *) 
    VAR shift : INTEGER 
  ;   neg := FALSE 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "index_address" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . Int ( size ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; IF size = 0 
      THEN 
        Err ( CodeGen , "size = 0 in index_address" ) 
      END 
 
    ; IF size < 0 
      THEN 
        size := - size 
      ; neg := TRUE 
      END 
 
    ; shift := log2 ( size ) 
 
    ; CodeGen . vstack . doindex_address ( shift , size , neg ) 
    END index_address 
 
; 
(*------------------------------------------------------- procedure calls ---*) 
 
  PROCEDURE call_64 ( CodeGen : CodeGenTyp ; builtin : Builtin ) 
  = BEGIN 
 
            (* all 64bit helpers pop their parameters, even if they are __cdecl named. *) 
 
      CodeGen . call_param_size [ CodeGen . in_proc_call - 1 ] := 0 
 
    ; 
      (* There is a problem with our register bookkeeping, such 
     * that we can't preserve non volatiles across function calls, 
     * and we even get confused about volatiles (they 
     * should be computed after the function call, not before). 
     *) 
 
      CodeGen . vstack . all_to_mem ( ) 
    ; (* hack *) 
 
      call_int_proc ( CodeGen , builtin ) 
 
    ; 
    END call_64 
 
; PROCEDURE do_rotate_or_shift_64 ( CodeGen : CodeGenTyp ; builtin : Builtin ) 
  = BEGIN 
      start_int_proc ( CodeGen , builtin ) 
    ; pop_param ( CodeGen , CGType . Word32 ) 
    ; (* shift count *) 
      pop_param ( CodeGen , CGType . Word64 ) 
    ; (* value to shift *) 
      call_64 ( CodeGen , builtin ) 
    END do_rotate_or_shift_64 
 
; PROCEDURE start_call_direct ( CodeGen : CodeGenTyp ; p : Proc ; lev : INTEGER ; type : CGType ) 
  = 
        (* begin a procedure call to a procedure at static level 'lev'. *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "start_call_direct" ) 
      ; CodeGen . wr . PName ( p ) 
      ; CodeGen . wr . Int ( lev ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; 
        (* ASSERT CodeGen . in_proc_call < 2 *)(* ? *) 
 
      CodeGen . static_link [ CodeGen . in_proc_call ] := NIL 
    ; CodeGen . call_param_size [ CodeGen . in_proc_call ] := 0 
    ; INC ( CodeGen . in_proc_call ) 
    END start_call_direct 
 
; PROCEDURE start_call_indirect ( CodeGen : CodeGenTyp ; type : CGType ; cc : CallingConvention ) 
  = 
        (* begin a procedure call to a procedure at static level 'lev'. *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "start_call_indirect" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . Txt ( cc . name ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; 
        (* ASSERT CodeGen . in_proc_call < 2 *)(* ? *) 
 
      CodeGen . static_link [ CodeGen . in_proc_call ] := NIL 
    ; CodeGen . call_param_size [ CodeGen . in_proc_call ] := 0 
    ; INC ( CodeGen . in_proc_call ) 
    END start_call_indirect 
 
; PROCEDURE pop_param ( CodeGen : CodeGenTyp ; type : MType ) 
  = 
        (* pop s0 and make it the "next" parameter in the current call *) 
    BEGIN 
      (*IF CodeGen . debug THEN 
      CodeGen . wr . Cmd   ("pop_param"); 
      CodeGen . wr . TName (type); 
      CodeGen . wr . NL    (); 
    END;*) 
 
      load_stack_param ( CodeGen , type , 0 ) 
    ; CodeGen . vstack . discard ( 1 ) 
 
    ; 
    END pop_param 
 
; PROCEDURE load_stack_param ( CodeGen : CodeGenTyp ; type : MType ; depth : INTEGER ) 
  = 
        (* make value at vstack[depth] the next parameter in the current call *) 
    VAR opA : ARRAY OperandPart OF Operand 
  ;   size : OperandSize 
  ; BEGIN 
 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "load_stack_param" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . Int ( depth ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . unlock ( ) 
 
    ; 
        <* ASSERT CodeGen . in_proc_call > 0 *> 
 
      WITH stack = CodeGen . vstack . pos ( depth , "load_stack_param" ) 
      DO 
        IF Target . FloatType [ type ] 
        THEN 
              <* ASSERT depth = 0 *> 
          IF type = CGType . Reel 
          THEN 
            CodeGen . cg . immOp ( Op . oSUB , CodeGen . cg . reg [ ESP ] , TIntN . Four ) 
          ELSE 
            CodeGen . cg . immOp ( Op . oSUB , CodeGen . cg . reg [ ESP ] , TIntN . Eight ) 
          END 
        ; CodeGen . cg . f_storeind ( CodeGen . cg . reg [ ESP ] , 0 , type ) 
        ELSE 
          CodeGen . vstack . find ( stack , Force . anyregimm ) 
        ; size := SplitOperand ( CodeGen . vstack . op ( stack ) , opA ) 
        ; FOR i := size - 1 TO 0 BY - 1 
          DO 
            CodeGen . cg . pushOp ( opA [ i ] ) 
          END 
        END 
      END 
 
    ; 
        <* ASSERT CG_Bytes[type] <= 4 OR CG_Bytes[type] = 8 *> 
      IF CG_Bytes [ type ] <= 4 
      THEN 
        INC ( CodeGen . call_param_size [ CodeGen . in_proc_call - 1 ] , 4 ) 
      ELSE 
        INC ( CodeGen . call_param_size [ CodeGen . in_proc_call - 1 ] , 8 ) 
      END 
 
    END load_stack_param 
 
; PROCEDURE pop_struct ( CodeGen : CodeGenTyp ; type : TypeUID ; s : ByteSize ; a : Alignment ) 
  = 
    (* pop s0 and make it the "next" parameter in the current call 
   * NOTE that we implement call by value, the struct is 
   * copied to temporary space on the machine stack 
   *) 
    VAR ts : TIntN . T 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "pop_struct" ) 
      ; CodeGen . wr . Tipe ( type ) 
      ; CodeGen . wr . Int ( s ) 
      ; CodeGen . wr . Int ( a ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; 
        <* ASSERT CodeGen . in_proc_call > 0 *> 
 
      (* round struct size up to multiple of 4 or 8 *) 
 
      <* ASSERT a <= 4 OR a = 8 *> 
      IF a <= 4 
      THEN 
        s := Word . And ( s + 3 , 16_FFFFFFFC ) 
      ELSE 
        s := Word . And ( s + 7 , Alignmask [ 8 ] ) 
      END 
 
    ; CodeGen . vstack . unlock ( ) 
 
    ; WITH stack0 = CodeGen . vstack . pos ( 0 , "pop_struct" ) 
      DO 
 
        IF NOT TIntN . FromHostInteger ( s , Target . Integer . bytes , ts ) 
        THEN 
          Err ( CodeGen , "pop_struct: unable to convert s to target int" ) 
        END 
 
      ; 
          (* if the struct is "large", use rep mov to copy it to the machine stack *) 
 
        IF TIntN . GT ( ts , TIntN . ThirtyTwo ) 
        THEN 
          CodeGen . cg . immOp ( Op . oSUB , CodeGen . cg . reg [ ESP ] , ts ) 
 
        ; CodeGen . vstack . find ( stack0 , Force . regset , RegSet { ESI } ) 
        ; CodeGen . vstack . corrupt ( EDI , operandPart := 0 ) 
        ; CodeGen . vstack . corrupt ( ECX , operandPart := 0 ) 
 
        ; CodeGen . cg . movOp ( CodeGen . cg . reg [ EDI ] , CodeGen . cg . reg [ ESP ] ) 
        ; CodeGen . cg . movImmI ( CodeGen . cg . reg [ ECX ] , s DIV 4 ) 
 
        ; CodeGen . cg . noargOp ( Op . oCLD ) 
        ; CodeGen . cg . noargOp ( Op . oREP ) 
        ; CodeGen . cg . noargOp ( Op . oMOVSD ) 
 
        ; CodeGen . vstack . newdest ( CodeGen . cg . reg [ ESI ] ) 
        ELSE 
 
              (* if the struct is "small", use a few load/push to copy it to the machine stack *) 
 
          CodeGen . vstack . find ( stack0 , Force . anyreg , AllRegisters , TRUE ) 
 
        ; WITH temp = CodeGen . vstack . freereg ( operandPart := 0 ) 
          DO 
            FOR i := 1 
            TO ( s DIV 4 ) 
               DO CodeGen . cg . load_ind 
                 ( temp 
                 , CodeGen . vstack . op ( stack0 ) 
                 , s - ( i * 4 ) 
                 , CGType . Word32 
                 ) 
            ;  CodeGen . cg . pushOp ( CodeGen . cg . reg [ temp ] ) 
            ; 
            END 
          END 
        END 
      END 
 
    ; CodeGen . vstack . discard ( 1 ) 
 
    ; INC ( CodeGen . call_param_size [ CodeGen . in_proc_call - 1 ] , s ) 
    END pop_struct 
 
; PROCEDURE pop_static_link ( CodeGen : CodeGenTyp ) 
  = BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "pop_static_link" ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; 
        <* ASSERT CodeGen . in_proc_call > 0 *> 
 
      CodeGen . static_link [ CodeGen . in_proc_call - 1 ] 
        := declare_temp ( CodeGen , 4 , 4 , CGType . Addr , FALSE ) 
 
    ; CodeGen . vstack . pop 
        ( MVar 
            { var := CodeGen . static_link [ CodeGen . in_proc_call - 1 ] 
            , mvar_offset := 0 
            , mvar_type := CGType . Addr 
            } 
        ) 
    END pop_static_link 

; PROCEDURE call_direct ( CodeGen : CodeGenTyp ; p : Proc ; type : CGType ) 
    (* call the procedure identified by block b.  The procedure 
     returns a value of type type. *) 
  = VAR LProc : LLProcTyp  
  ; VAR call_param_size : TIntN . T 
  ; BEGIN 
      LProc := NARROW ( p , LLProcTyp ) 
    ; IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "call_direct" ) 
      ; CodeGen . wr . PName ( p ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; 
        <* ASSERT CodeGen . in_proc_call > 0 *> 
 
      IF LProc . lev # 0 
      THEN 
        load_static_link_toC ( CodeGen , p ) 
      END 
 
    ; CodeGen . vstack . releaseall ( ) 
 
    ; IF LProc . import 
      THEN 
        CodeGen . cg . absCall ( p ) 
      ELSE 
        IF LProc . bound 
        THEN 
          CodeGen . cg . relCall 
            ( LProc . offset - ( CodeGen . obj . cursor ( Seg . Text ) + 5 ) ) 
        ELSE 
          CodeGen . cg . relCall ( 0 ) 
        ; LProc . usage 
            := NEW 
                 ( ProcList 
                 , loc := CodeGen . obj . cursor ( Seg . Text ) - 4 
                 , link := LProc . usage 
                 ) 
        END 
      END 
 
    ; IF ( NOT LProc . stdcall ) (* => caller cleans *) 
         AND CodeGen . call_param_size [ CodeGen . in_proc_call - 1 ] > 0 
      THEN 
 
        IF NOT TIntN . FromHostInteger 
             ( CodeGen . call_param_size [ CodeGen . in_proc_call - 1 ] 
             , Target . Integer . bytes 
             , call_param_size 
             ) 
        THEN 
          Err 
            ( CodeGen , "call_direct: unable to convert param_size to target integer" ) 
        END 
      ; CodeGen . cg . immOp ( Op . oADD , CodeGen . cg . reg [ ESP ] , call_param_size ) 
      END 
 
    ; IF type = CGType . Struct 
      THEN 
        type := CGType . Addr 
      END 
 
    ; IF type # CGType . Void 
      THEN 
        IF Target . FloatType [ type ] 
        THEN 
          CodeGen . vstack . pushnew ( type , Force . any ) 
        ; CodeGen . cg . f_pushnew ( ) 
        ELSIF TypeIs64 ( type ) 
        THEN 
          CodeGen . vstack . pushnew ( type , Force . regset , RegSet { EAX , EDX } ) 
        ELSE 
          CodeGen . vstack . pushnew 
            ( FixReturnValue ( CodeGen , type ) , Force . regset , RegSet { EAX } ) 
        END 
      END 
 
    ; DEC ( CodeGen . in_proc_call ) 
    END call_direct 
 
; PROCEDURE call_indirect ( CodeGen : CodeGenTyp ; type : CGType ; cc : CallingConvention ) 
  = 
    (* call the procedure whose address is in s0 . A and pop s0.  The 
     procedure returns a value of type type. *) 
    VAR call_param_size : TIntN . T 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "call_indirect" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . Txt ( cc . name ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; 
        <* ASSERT CodeGen . in_proc_call > 0 *> 
 
      CodeGen . vstack . releaseall ( ) 
 
    ; IF CodeGen . static_link [ CodeGen . in_proc_call - 1 ] # NIL 
      THEN 
            (*u . vstack . corrupt(ECX, operandPart := 0);*) 
        CodeGen . cg . movOp 
          ( CodeGen . cg . reg [ ECX ] 
          , Operand 
              { loc := OLoc . mem 
              , optype := CGType . Addr 
              , mvar 
                  := MVar 
                       { var := CodeGen . static_link [ CodeGen . in_proc_call - 1 ] 
                       , mvar_offset := 0 
                       , mvar_type := CGType . Addr 
                       } 
              } 
          ) 
      ; free_temp ( CodeGen , CodeGen . static_link [ CodeGen . in_proc_call - 1 ] ) 
      ; CodeGen . static_link [ CodeGen . in_proc_call - 1 ] := NIL 
      END 
 
    ; CodeGen . cg . rmCall 
        ( CodeGen . vstack . op ( CodeGen . vstack . pos ( 0 , "call_indirect" ) ) ) 
    ; CodeGen . vstack . discard ( 1 ) 
 
    ; IF ( cc . m3cg_id = 0 ) 
         AND CodeGen . call_param_size [ CodeGen . in_proc_call - 1 ] > 0 
      THEN 
 
            (* caller-cleans calling convention *) 
 
        IF NOT TIntN . FromHostInteger 
             ( CodeGen . call_param_size [ CodeGen . in_proc_call - 1 ] 
             , Target . Integer . bytes 
             , call_param_size 
             ) 
        THEN 
          Err 
            ( CodeGen , "call_indirect: unable to convert param_size to target integer" ) 
        END 
 
      ; CodeGen . cg . immOp ( Op . oADD , CodeGen . cg . reg [ ESP ] , call_param_size ) 
      END 
 
    ; IF type = CGType . Struct 
      THEN 
        type := CGType . Addr 
      END 
 
    ; IF type # CGType . Void 
      THEN 
        IF Target . FloatType [ type ] 
        THEN 
          CodeGen . vstack . pushnew ( type , Force . any ) 
        ; CodeGen . cg . f_pushnew ( ) 
        ELSIF TypeIs64 ( type ) 
        THEN 
          CodeGen . vstack . pushnew ( type , Force . regset , RegSet { EAX , EDX } ) 
        ELSE 
          CodeGen . vstack . pushnew 
            ( FixReturnValue ( CodeGen , type ) , Force . regset , RegSet { EAX } ) 
        END 
      END 
 
    ; DEC ( CodeGen . in_proc_call ) 
    END call_indirect 
 
; PROCEDURE FixReturnValue ( CodeGen : CodeGenTyp ; type : CGType ) : CGType 
  = 
    (* The Microsoft C compiler does not return full 32-bit values in EAX 
     for functions that return 8-bit return types. 
     Likewise for 16-bit return types prior to Visual C++ 5.0. 
     This code generator assumes that registers always contain 32-bit values. 
     We compensate here. *) 
    BEGIN 
      CASE type 
      OF 
      | CGType . Int8 
        => (* 8-bit signed integer *) 
            CodeGen . cg . CBWOp ( ) 
          ;                       (* AX  := SIGN-EXTEND (AL) *) 
            CodeGen . cg . noargOp ( Op . oCWDE ) 
          ;                       (* EAX := SIGN-EXTEND (AX) *) 
            type := CGType . Int32 
 
          ; 
      | CGType . Int16 
        => (* 16-bit signed integer *) 
                    (* EAX := SIGN-EXTEND (AX) *) 
            CodeGen . cg . noargOp ( Op . oCWDE ) 
          ; type := CGType . Int32 
 
          ; 
      | CGType . Word8 
        => (* 8-bit unsigned integer *) 
            CodeGen . cg . immOp ( Op . oAND , CodeGen . cg . reg [ EAX ] , TWordN . Max8 ) 
          ;                                                (* EAX &= 16_FF *) 
            type := CGType . Word32 
 
          ; 
      | CGType . Word16 
        => (* 16-bit unsigned integer *) 
            CodeGen . cg . immOp ( Op . oAND , CodeGen . cg . reg [ EAX ] , TWordN . Max16 ) 
          ;                                                 (* EAX &= 16_FFFF *) 
            type := CGType . Word32 
 
          ; 
      ELSE (* value is ok *) 
      END 
    ; RETURN type 
    END FixReturnValue 
 
; 
(*------------------------------------------- procedure and closure types ---*) 
 
  PROCEDURE load_procedure ( CodeGen : CodeGenTyp ; p : Proc ) 
  = VAR LProc := NARROW ( p , LLProcTyp ) 
  ; 
      (* push; s0 . A := ADDR (p's body) *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "load_procedure" ) 
      ; CodeGen . wr . PName ( p ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . unlock ( ) 
    ; CodeGen . vstack . pushnew ( CGType . Addr , Force . anyreg ) 
    ; WITH stack0 = CodeGen . vstack . pos ( 0 , "load_procedure" ) 
      DO 
        CodeGen . cg . movDummyReloc 
          ( CodeGen . vstack . op ( stack0 ) , LProc . symbol ) 
      END 
    END load_procedure 
 
; PROCEDURE load_static_link ( CodeGen : CodeGenTyp ; p : Proc ) 
  = VAR LProc := NARROW ( p , LLProcTyp ) 
  ; 
      (* push; s0 . A := (static link needed to call p, NIL for top-level procs) *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "load_static_link" ) 
      ; CodeGen . wr . PName ( p ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; CodeGen . vstack . unlock ( ) 
 
    ; IF LProc . lev = 0 
      THEN 
        CodeGen . vstack . pushimmT ( TZero , CGType . Word32 ) 
      ELSE 
        CodeGen . vstack . pushnew ( CGType . Addr , Force . anyreg ) 
      ; CodeGen . cg . get_frame 
          ( CodeGen . vstack . op ( CodeGen . vstack . pos ( 0 , "load_static_link" ) ) 
            . reg 
              [ 0 ] 
          , LProc . parent 
          , CodeGen . CurrentProc 
          ) 
      END 
    END load_static_link 
 
; PROCEDURE load_static_link_toC ( CodeGen : CodeGenTyp ; p : Proc ) 
  = VAR LProc := NARROW ( p , LLProcTyp ) 
  ; 
      (* push; s0 . A := (static link needed to call p, NIL for top-level procs) *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "load_static_link_toC" ) 
      ; CodeGen . wr . PName ( p ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; IF LProc . lev = 0 
      THEN 
        CodeGen . vstack . corrupt ( ECX , operandPart := 0 ) 
      ; CodeGen . cg . movImmT ( CodeGen . cg . reg [ ECX ] , TZero ) 
      ELSE 
        CodeGen . vstack . unlock ( ) 
      ; CodeGen . vstack . corrupt ( ECX , operandPart := 0 ) 
      ; CodeGen . cg . get_frame ( ECX , LProc . parent , CodeGen . CurrentProc ) 
      END 
    END load_static_link_toC 
 
; 
(*---------------------------------------------------------- produce code ---*) 
 
  PROCEDURE fltregcmp ( CodeGen : CodeGenTyp ) : BOOLEAN 
  = VAR reversed := FALSE 
  ; BEGIN 
      IF CodeGen . cg . ftop_inmem 
      THEN 
        CodeGen . cg . binFOp ( FOp . fCOMP , 1 ) 
      ELSE 
        CodeGen . cg . binFOp ( FOp . fCOMPP , 1 ) 
      ; reversed := TRUE 
      END 
    ; CodeGen . vstack . discard ( 2 ) 
 
    ; CodeGen . vstack . unlock ( ) 
    ; CodeGen . vstack . corrupt ( EAX , operandPart := 0 ) 
    ; CodeGen . cg . noargFOp ( FOp . fNSTSWAX ) 
    ; CodeGen . cg . noargOp ( Op . oSAHF ) 
 
    ; RETURN reversed 
    END fltregcmp 
 
; PROCEDURE condset ( CodeGen : CodeGenTyp ; cond : Cond ; type : ZType ) 
  = VAR reversed := FALSE 
  ; BEGIN 
 
      (* This function used to deal with any integer type 
     * as well, but that isn't needed presently. 
     *) 
      <* ASSERT Target . FloatType[type] *> 
 
      reversed := fltregcmp ( CodeGen ) 
    ; IF reversed 
      THEN 
        cond := revcond [ cond ] 
      END 
 
    ; 
        (* FCOM sets the unsigned compare flags *) 
      cond := unscond [ cond ] 
 
    ; CodeGen . vstack . pushnew ( CGType . Word8 , Force . mem ) 
    ; WITH stop0 = CodeGen . vstack . op ( CodeGen . vstack . pos ( 0 , "condset" ) ) 
      DO 
        stop0 . mvar . var . stack_temp := FALSE 
      ; CodeGen . cg . setccOp ( stop0 , cond ) 
      END 
    END condset 
 
; 
(*----------------------------------------------------------------- misc. ---*) 
 
  PROCEDURE comment ( CodeGen : CodeGenTyp ; a , b , c , d : TEXT := NIL ) 
  = VAR i : INTEGER := - 1 
  ; BEGIN 
      Cmt ( CodeGen , a , i ) 
    ; Cmt ( CodeGen , b , i ) 
    ; Cmt ( CodeGen , c , i ) 
    ; Cmt ( CodeGen , d , i ) 
    ; Cmt ( CodeGen , " \n" , i ) 
    END comment 
 
; PROCEDURE Cmt ( CodeGen : CodeGenTyp ; text : TEXT ; VAR width : INTEGER ) 
  = VAR ch : CHAR 
  ; BEGIN 
      IF ( NOT CodeGen . debug ) OR ( text = NIL ) 
      THEN 
        RETURN 
      END 
    ; FOR i := 0 TO Text . Length ( text ) - 1 
      DO 
        ch := Text . GetChar ( text , i ) 
      ; IF ch = '\n' OR ch = '\r'  
        THEN 
          CodeGen . wr . OutC ( ch ) 
        ; width := - 1 
        ELSE 
          IF ( width = - 1 ) 
          THEN 
            CodeGen . wr . OutT ( "\t# " ) 
          ; width := 0 
          END 
        ; CodeGen . wr . OutC ( ch ) 
        END 
      END 
    END Cmt 
 
 
; 
(*--------------------------------------------------------------- atomics ---*) 
 
  PROCEDURE store_ordered 
    ( CodeGen : CodeGenTyp 
    ; type_multiple_of_32 : ZType 
    ; type : MType 
    ; <*UNUSED*> order : MemoryOrder 
    ) 
  = 
(* Mem [s1 . A] . u := s0 . type; 
   pop (2) *) 
    VAR retry : Label 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "store_ordered" ) 
      ; CodeGen . wr . TName ( type_multiple_of_32 ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; 
        <* ASSERT CG_Bytes[type_multiple_of_32] >= CG_Bytes[type] *> 
 
      IF TypeIs64 ( type ) 
      THEN 
        (* see: http://niallryan.com/node/137 
       * see fetch_and_op 
       *) 
 
        CodeGen . vstack . unlock ( ) 
      ; CodeGen . vstack . pushnew ( type , Force . regset , RegSet { EDX , EAX } ) 
 
      ; WITH oldValue = CodeGen . vstack . pos ( 0 , "fetch_and_op" ) 
        , newValue = CodeGen . vstack . pos ( 1 , "fetch_and_op" ) 
        , atomicVariable = CodeGen . vstack . pos ( 2 , "fetch_and_op" ) 
        DO 
 
          CodeGen . vstack . find ( newValue , Force . regset , RegSet { ECX , EBX } ) 
        ; CodeGen . proc_reguse [ EBX ] := TRUE 
 
        ; 
            (* x . vstack . find(atomicVariable, Force . any); bug *) 
          CodeGen . vstack . find ( atomicVariable , Force . anyreg ) 
        ; CodeGen . cg . load_ind 
            ( EAX , CodeGen . vstack . op ( atomicVariable ) , 0 , type ) 
        ; CodeGen . cg . load_ind 
            ( EDX , CodeGen . vstack . op ( atomicVariable ) , 4 , type ) 
        ; retry := CodeGen . next_label ( ) 
        ; CodeGen . cg . set_label ( retry ) 
        ; CodeGen . cg . lock_compare_exchange 
            ( CodeGen . vstack . op ( atomicVariable ) 
            , CodeGen . vstack . op ( newValue ) 
            , type 
            ) 
        ; CodeGen . cg . brOp ( Cond . NE , retry ) 
        ; CodeGen . vstack . newdest ( CodeGen . vstack . op ( atomicVariable ) ) 
        ; (* Is this needed? *) 
          CodeGen . vstack . newdest ( CodeGen . vstack . op ( newValue ) ) 
        ;                                              (* Is this needed? *) 
          CodeGen . vstack . newdest ( CodeGen . vstack . op ( oldValue ) ) 
        ;                                              (* Is this needed? *) 
          CodeGen . vstack . discard ( 3 ) 
        END 
      ; RETURN 
      END 
 
    ; CodeGen . fence ( MemoryOrder . Sequential ) 
    ; CodeGen . vstack . unlock ( ) 
    ; WITH stack0 = CodeGen . vstack . pos ( 0 , "store_ordered" ) 
      , stack1 = CodeGen . vstack . pos ( 1 , "store_ordered" ) 
      DO 
        CodeGen . vstack . find ( stack0 , Force . any ) 
      ; CodeGen . vstack . find ( stack1 , Force . mem ) 
      ; CodeGen . vstack . dostoreind ( 0 , type ) 
      END 
    ; CodeGen . fence ( MemoryOrder . Sequential ) 
    END store_ordered 
 
; PROCEDURE load_ordered 
    ( CodeGen : CodeGenTyp 
    ; type : MType 
    ; type_multiple_of_32 : ZType 
    ; <*UNUSED*> order : MemoryOrder 
    ) 
  = 
(* s0 . type_multiple_of_32 := Mem [s0 . A] . type  *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "load_ordered" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . TName ( type_multiple_of_32 ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; 
        <* ASSERT CG_Bytes[type_multiple_of_32] >= CG_Bytes[type] *> 
 
      IF TypeIs64 ( type ) 
      THEN 
 
            (* see: http://niallryan.com/node/137 *) 
 
        CodeGen . vstack . pushimmT ( TZero , CGType . Word64 ) 
      ; CodeGen . vstack . pushimmT ( TZero , CGType . Word64 ) 
      ; compare_exchange_helper ( CodeGen , type ) 
      ; CodeGen . vstack . unlock ( ) 
      ; CodeGen . vstack . pushnew ( type , Force . regset , RegSet { EAX , EDX } ) 
      ; RETURN 
      END 
 
    ; CodeGen . vstack . unlock ( ) 
    ; CodeGen . fence ( MemoryOrder . Sequential ) 
    ; CodeGen . load_indirect ( 0 , type , type_multiple_of_32 ) 
    ; CodeGen . fence ( MemoryOrder . Sequential ) 
    END load_ordered 
 
; PROCEDURE exchange 
    ( CodeGen : CodeGenTyp 
    ; type : MType 
    ; type_multiple_of_32 : ZType 
    ; <*UNUSED*> order : MemoryOrder 
    ) 
  = 
(* tmp := Mem [s1 . A + o] . type; 
   Mem [s1 . A + o] . type := s0 . type_multiple_of_32; 
   s0 . type_multiple_of_32 := tmp; 
   pop *) 
    VAR reg : Regno 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "exchange" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . TName ( type_multiple_of_32 ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; 
        <* ASSERT CG_Bytes[type_multiple_of_32] >= CG_Bytes[type] *> 
 
      IF TypeIs64 ( type ) 
      THEN 
            (* Push arbitrary value for the first compare. *) 
        CodeGen . vstack . pushimmT ( TZero , CGType . Word64 ) 
      ; CodeGen . vstack . swap ( ) 
      ; compare_exchange_helper ( CodeGen , type ) 
      ; CodeGen . vstack . unlock ( ) 
      ; CodeGen . vstack . pushnew ( type , Force . regset , RegSet { EAX , EDX } ) 
      ; RETURN 
      END 
 
    ; WITH newValue = CodeGen . vstack . pos ( 0 , "exchange" ) 
      , atomicVariable = CodeGen . vstack . pos ( 1 , "exchange" ) 
      DO 
        CodeGen . vstack . unlock ( ) 
      ; CodeGen . vstack . find ( newValue , Force . anyreg ) 
      ; CodeGen . vstack . find ( atomicVariable , Force . anyreg ) 
      ; reg := CodeGen . vstack . op ( newValue ) . reg [ 0 ] 
      ; CodeGen . cg . lock_exchange 
          ( CodeGen . vstack . op ( atomicVariable ) 
          , CodeGen . vstack . op ( newValue ) 
          , type 
          ) 
      ; CodeGen . vstack . discard ( 2 ) 
      ; CodeGen . vstack . unlock ( ) 
      ; CodeGen . vstack . pushnew ( type , Force . regset , RegSet { reg } ) 
      END 
    END exchange 
 
; PROCEDURE compare_exchange_helper ( CodeGen : CodeGenTyp ; type : CGType ) 
  = BEGIN 
 
      CodeGen . vstack . unlock ( ) 
 
    ; WITH newValue = CodeGen . vstack . pos ( 0 , "compare_exchange" ) 
      , compareValueAndOldValueIfFailed 
            = CodeGen . vstack . pos ( 1 , "compare_exchange" ) 
      , atomicVariable = CodeGen . vstack . pos ( 2 , "compare_exchange" ) 
      DO 
 
        IF TypeIs64 ( type ) 
        THEN 
          (* 
         * 64 bit form has very particular register allocation requirements. 
         *) 
          CodeGen . vstack . find 
            ( compareValueAndOldValueIfFailed 
            , Force . regset 
            , RegSet { EAX , EDX } 
            ) 
        ; CodeGen . vstack . find ( newValue , Force . regset , RegSet { ECX , EBX } ) 
        ; CodeGen . proc_reguse [ EBX ] := TRUE 
        ELSE 
          CodeGen . vstack . find 
            ( compareValueAndOldValueIfFailed , Force . regset , RegSet { EAX } ) 
        ; CodeGen . vstack . find ( newValue , Force . anyreg ) 
        END 
      ; CodeGen . vstack . find ( atomicVariable , Force . anyreg ) 
      ; CodeGen . cg . lock_compare_exchange 
          ( CodeGen . vstack . op ( atomicVariable ) 
          , CodeGen . vstack . op ( newValue ) 
          , type 
          ) 
      ; CodeGen . vstack . discard ( 3 ) 
      END 
 
    ; 
    END compare_exchange_helper 
 
; PROCEDURE compare_exchange 
    ( CodeGen : CodeGenTyp 
    ; type : MType 
    ; type_multiple_of_32 : ZType 
    ; result_type : IType 
    ; 
                                    <*UNUSED*> success , failure : MemoryOrder 
    ) 
  = 
(* original := Mem[s2 . A] . type; 
   spurious_failure := whatever; 
   IF original = Mem[s1 . A] . type AND NOT spurious_failure THEN 
     Mem [s2 . A] . type := s0 . type_multiple_of_32; 
     s2 . result_type := 1; 
   ELSE 
     Mem [s2 . A] . type := original; x86 really does rewrite the original value, atomically 
     s2 . result_type := 0; 
   END; 
   pop(2); 
   This is permitted to fail spuriously. 
   That is, even if Mem[s2 . a] = Mem[s1 . a], we might 
     still go down the then branch. 
*) 
    BEGIN 
 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "compare_exchange" ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . TName ( type_multiple_of_32 ) 
      ; CodeGen . wr . TName ( result_type ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; 
        <* ASSERT CG_Bytes[type_multiple_of_32] >= CG_Bytes[type] *> 
      <* ASSERT CG_Bytes[result_type] = 4 *> 
 
      compare_exchange_helper ( CodeGen , type ) 
 
    ; 
        (* Get the zero flag into a register. Is there a better way? *) 
 
      CodeGen . vstack . unlock ( ) 
    ; CodeGen . vstack . pushnew ( CGType . Word8 , Force . mem ) 
    ; WITH stop0 = CodeGen . vstack . op ( CodeGen . vstack . pos ( 0 , "condset" ) ) 
      DO 
        stop0 . mvar . var . stack_temp := FALSE 
      ; CodeGen . cg . setccOp ( stop0 , Cond . E ) 
      END 
 
    ; 
    END compare_exchange 
 
; PROCEDURE fence ( CodeGen : CodeGenTyp ; <*UNUSED*> order : MemoryOrder ) 
  = 
(* 
 * Exchanging any memory with any register is a serializing instruction. 
 *) 
    BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "fence" ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; <* ASSERT CodeGen . in_proc *> 
      <* ASSERT CodeGen . CurrentProc # NIL *> 
 
      CodeGen . vstack . unlock ( ) 
 
    ; IF CodeGen . CurrentProc . fenceVar = NIL 
      THEN 
        CodeGen . CurrentProc . fenceVar 
          := get_temp_var ( CodeGen , CGType . Word32 , 4 , 4 ) 
      END 
    ; CodeGen . vstack . push 
        ( MVar { CodeGen . CurrentProc . fenceVar , mvar_type := CGType . Word32 } ) 
    ; CodeGen . vstack . pushnew ( CGType . Word32 , Force . anyreg ) 
    ; EVAL CodeGen . vstack . dobin ( Op . oXCHG , TRUE , TRUE , CGType . Word32 ) 
    ; CodeGen . vstack . discard ( 1 ) 
    END fence 
 
; CONST AtomicOpToOp 
    = ARRAY AtomicOp OF Op 
        { Op . oXADD , Op . oXADD , Op . oOR , Op . oAND , Op . oXOR } 
; CONST AtomicOpName 
    = ARRAY AtomicOp OF TEXT { "add" , "sub" , "or" , "and" , "xor" } 
; CONST AtomicAddSub = SET OF AtomicOp { AtomicOp . Add , AtomicOp . Sub } 
 
; PROCEDURE fetch_and_op 
    ( CodeGen : CodeGenTyp 
    ; atomic_op : AtomicOp 
    ; type : MType 
    ; type_multiple_of_32 : ZType 
    ; 
                                <*UNUSED*> order : MemoryOrder 
    ) 
  = 
(* original := Mem [s1 . A] . type; 
   Mem [s1 . A] . type := original op s0 . type_multiple_of_32; 
   s1 . type_multiple_of_32 := original; 
   pop 
 
=> store the new value, return the old value 
 
Generally we use interlocked compare exchange loop. 
Some operations can be done better though. 
*) 
    VAR retry : Label 
  ;   is64 := TypeIs64 ( type ) 
  ;   addSub := ( NOT is64 ) AND atomic_op IN AtomicAddSub 
  ; BEGIN 
      IF CodeGen . debug 
      THEN 
        CodeGen . wr . Cmd ( "fetch_and_op" ) 
      ; CodeGen . wr . OutT ( AtomicOpName [ atomic_op ] ) 
      ; CodeGen . wr . TName ( type ) 
      ; CodeGen . wr . TName ( type_multiple_of_32 ) 
      ; CodeGen . wr . NL ( ) 
      END 
 
    ; 
        <* ASSERT CG_Bytes[type_multiple_of_32] >= CG_Bytes[type] *> 
 
      CodeGen . vstack . unlock ( ) 
 
    ; IF addSub 
      THEN 
        CodeGen . vstack . pushnew ( type , Force . anyreg ) 
      ; (* any? *) 
        CodeGen . vstack . pushnew ( type , Force . anyreg ) 
      ; (* any? *) 
      ELSIF is64 
      THEN 
        CodeGen . vstack . pushnew ( type , Force . regset , RegSet { EDX , EAX } ) 
      ; CodeGen . vstack . pushnew ( type , Force . regset , RegSet { ECX , EBX } ) 
      ; CodeGen . proc_reguse [ EBX ] := TRUE 
      ELSE 
        CodeGen . vstack . pushnew ( type , Force . regset , RegSet { EAX } ) 
      ; CodeGen . vstack . pushnew ( type , Force . anyreg ) 
      END 
 
    ; 
(* 
    mov oldValue, mem-or-reg; oldValue is EAX or EDX:EAX 
retry: 
    mov newValue, oldValue; oldValue is EAX or EDX:EAX 
    op  newValue, secondOperand; newValue is whatever register allocator decides, or ECX:EBX 
    lock cmpxchg[8b] BYTE OR WORD or DWORD or QWORD PTR [atomicVariable], newValue 
    ; original value is in EAX or EDX:EAX, eq or ne. 
    jne retry 
    ; EAX or EDX:EAX contains old value 
*) 
      WITH newValue = CodeGen . vstack . pos ( 0 , "fetch_and_op" ) 
      , oldValue = CodeGen . vstack . pos ( 1 , "fetch_and_op" ) 
      , operand = CodeGen . vstack . pos ( 2 , "fetch_and_op" ) 
      , atomicVariable = CodeGen . vstack . pos ( 3 , "fetch_and_op" ) 
      DO 
        IF CG_Bytes [ type ] < 4 
        THEN 
          CodeGen . vstack . find ( operand , Force . anyreg ) 
        ELSE 
          CodeGen . vstack . find ( operand , Force . any ) 
        END 
      ; 
          (* x . vstack . find(atomicVariable, Force . any); bug *) 
        CodeGen . vstack . find ( atomicVariable , Force . anyreg ) 
 
      ; IF addSub 
        THEN 
          IF atomic_op = AtomicOp . Sub 
          THEN 
            CodeGen . vstack . doneg ( operand ) 
          END 
        ; CodeGen . cg . write_lock_prefix ( ) 
        ; CodeGen . cg . binOp 
            ( AtomicOpToOp [ atomic_op ] 
            , CodeGen . vstack . op ( newValue ) 
            , CodeGen . vstack . op ( operand ) 
            ) 
        ; CodeGen . vstack . doneg ( operand ) 
        ; CodeGen . cg . binOp 
            ( AtomicOpToOp [ atomic_op ] 
            , CodeGen . vstack . op ( oldValue ) 
            , CodeGen . vstack . op ( operand ) 
            ) 
        ELSE 
          CodeGen . cg . load_ind 
            ( EAX , CodeGen . vstack . op ( atomicVariable ) , 0 , type ) 
        ; IF is64 
          THEN 
            CodeGen . cg . load_ind 
              ( EDX , CodeGen . vstack . op ( atomicVariable ) , 4 , type ) 
          END 
        ; retry := CodeGen . next_label ( ) 
        ; CodeGen . cg . set_label ( retry ) 
        ; CodeGen . cg . movOp 
            ( CodeGen . vstack . op ( newValue ) , CodeGen . vstack . op ( oldValue ) ) 
        ; CodeGen . cg . binOp 
            ( AtomicOpToOp [ atomic_op ] 
            , CodeGen . vstack . op ( newValue ) 
            , CodeGen . vstack . op ( operand ) 
            ) 
        ; CodeGen . cg . lock_compare_exchange 
            ( CodeGen . vstack . op ( atomicVariable ) 
            , CodeGen . vstack . op ( newValue ) 
            , type 
            ) 
        ; CodeGen . cg . brOp ( Cond . NE , retry ) 
        END 
      ; CodeGen . vstack . newdest ( CodeGen . vstack . op ( atomicVariable ) ) 
      ; (* Is this needed? Probably. *) 
        CodeGen . vstack . newdest ( CodeGen . vstack . op ( operand ) ) 
      ;                                              (* Is this needed? *) 
        CodeGen . vstack . newdest ( CodeGen . vstack . op ( newValue ) ) 
      ;                                              (* Is this needed? *) 
        CodeGen . vstack . newdest ( CodeGen . vstack . op ( oldValue ) ) 
                                                     (* Is this needed? *) 
 
          (* Store the new value (already done), return the old value (these discard/swaps). *) 
      ; CodeGen . vstack . discard ( 1 ) 
      ; CodeGen . vstack . swap ( ) 
      ; CodeGen . vstack . discard ( 1 ) 
      ; CodeGen . vstack . swap ( ) 
      ; CodeGen . vstack . discard ( 1 ) 
      END 
 
    ; 
    END fetch_and_op 
 
; PROCEDURE widechar_size ( CodeGen : CodeGenTyp ; bitsize : INTEGER ) 
  = BEGIN 
      CodeGen . WidecharBitsize := bitsize 
    END widechar_size 

; PROCEDURE Err ( CodeGen : CodeGenTyp ; err : TEXT ) 
  = BEGIN 
      CodeGen . Err ( err ) 
    ; <* ASSERT FALSE *> 
    END Err 

; BEGIN 
    initGlobals ( ) 
  END LLGen 
. 
