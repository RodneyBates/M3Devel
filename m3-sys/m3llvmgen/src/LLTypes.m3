MODULE LLTypes 

; FROM M3CG IMPORT Type 
; IMPORT TIntN
; IMPORT TWordN 

; PROCEDURE ReportOutOfMemory ( Msg : TEXT ) 

  = BEGIN 
(* TODO: Complete me. *) 
    END ReportOutOfMemory 

; PROCEDURE NewLLStructType ( ) : LLStructTypeTyp 

  = VAR LRT : LLStructTypeTyp 

  ; BEGIN 
      LRT := NEW ( LLStructTypeTyp ) 
    ; IF LRT = NIL 
      THEN ReportOutOfMemory ( "LLTypes.LLStructTypeTyp" ) 
      ELSE RETURN LRT 
      END (* IF *) 
    END NewLLStructType

; PROCEDURE NewLLField ( ) : REF LLFieldTyp 

  = VAR LF : REF LLFieldTyp 

  ; BEGIN 
      LF := NEW ( REF LLFieldTyp ) 
    ; IF LF = NIL 
      THEN ReportOutOfMemory ( "LLTypes.LLFieldTyp" ) 
      ELSE RETURN LF 
      END (* IF *) 
    END NewLLField 

; PROCEDURE NewLLFieldArray ( Ct : CARDINAL ) : REF ARRAY OF REF LLFieldTyp 

  = VAR LRAF : REF ARRAY OF REF LLFieldTyp 

  ; BEGIN 
      LRAF := NEW ( REF ARRAY OF REF LLFieldTyp , Ct ) 
    ; IF LRAF = NIL 
      THEN ReportOutOfMemory ( "REF ARRAY OF LLTypes.LLFieldTyp" ) 
      ELSE RETURN LRAF 
      END (* IF *) 
    END NewLLFieldArray  

; PROCEDURE TypeIs64 ( type : Type ) : BOOLEAN 
  = BEGIN 
      RETURN 
        type IN ( SET OF Type { Type . Int64 , Type . Word64 } ) 
    END TypeIs64 
 
; PROCEDURE TypeIsUnsigned ( type : Type ) : BOOLEAN 
  = BEGIN 
      RETURN 
        type IN ( SET OF Type { Type . Word32 , Type . Word64 } ) 
    END TypeIsUnsigned 
 
; PROCEDURE TypeIsSigned ( type : Type ) : BOOLEAN 
  = BEGIN 
      RETURN type IN ( SET OF Type { Type . Int32 , Type . Int64 } ) 
    END TypeIsSigned 
 
; PROCEDURE SplitMVar 
    ( READONLY mvar : MVar ; VAR mvarA : ARRAY OperandPart OF MVar ) 
  : OperandSize 
  = VAR type := mvar . mvar_type 
  ; BEGIN 
      mvarA [ 0 ] := mvar 
    ; IF NOT TypeIs64 ( type ) 
      THEN 
        RETURN 1 
      END 
    ; mvarA [ 1 ] := mvar 
    ; IF mvar . var # NIL 
      THEN 
        (* <* ASSERT mvar . var . var_size = CG_Bytes[type] *> *) 
      END 
    ; INC ( mvarA [ 1 ] . mvar_offset , 4 ) 
    ; mvarA [ 0 ] . mvar_type := Type . Word32 
    ;                                       (* low part of 64bit integer is always unsigned *) 
      IF type = Type . Int64 
      THEN 
        mvarA [ 1 ] . mvar_type := Type . Int32 
      ;                                     (* high part signedness is same as unsplit type *) 
      ELSIF type = Type . Word64 
      THEN 
        mvarA [ 1 ] . mvar_type := Type . Word32 
      ;                                     (* high part signedness is same as unsplit type *) 
      ELSE 
        <* ASSERT FALSE *> 
      END 
    ; RETURN 2 
    END SplitMVar 
 
; PROCEDURE SplitImm 
    ( type : Type 
    ; READONLY imm : TIntN . T 
    ; VAR immA : ARRAY OperandPart OF TIntN . T 
    ) 
  : OperandSize 
  = BEGIN 
      TWordN . And ( imm , TWordN . Max32 , immA [ 0 ] ) 
    ; TWordN . RightShift ( imm , 32 , immA [ 1 ] ) 
    ; RETURN GetTypeSize ( type ) 
    END SplitImm 
 
; PROCEDURE GetTypeSize ( type : Type ) : OperandSize 
  = 
(* In "words" or "registers": 1 or 2 *) 
    BEGIN 
      RETURN 1 + ORD ( TypeIs64 ( type ) ) 
    END GetTypeSize 
 
; PROCEDURE GetOperandSize ( READONLY op : Operand ) : OperandSize 
  = BEGIN 
      RETURN GetTypeSize ( op . optype ) 
    END GetOperandSize 
 
; PROCEDURE SplitOperand 
    ( READONLY op : Operand ; VAR opA : ARRAY OperandPart OF Operand ) 
  : OperandSize 
  = VAR type := op . optype 
  ;   mvarA : ARRAY OperandPart OF MVar 
  ;   immA : ARRAY OperandPart OF TIntN . T 
  ; BEGIN 
      opA [ 0 ] := op 
 
    ; IF GetTypeSize ( type ) = 1 
      THEN 
        RETURN 1 
      END 
 
    ; opA [ 1 ] := op 
    ; opA [ 0 ] . optype := Type . Word32 
    ;                                 (* low part of 64bit integer is always unsigned *) 
      IF type = Type . Int64 
      THEN 
        opA [ 1 ] . optype := Type . Int32 
      ;                               (* high part signedness is same as unsplit type *) 
      ELSIF type = Type . Word64 
      THEN 
        opA [ 1 ] . optype := Type . Word32 
      ;                               (* high part signedness is same as unsplit type *) 
      ELSE 
        <* ASSERT FALSE *> 
      END 
 
    ; CASE op . loc 
      OF 
      | OLoc . fstack => <* ASSERT FALSE *> 
      | OLoc . imm 
        =>  EVAL SplitImm ( type , op . imm , immA ) 
          ; opA [ 0 ] . imm := immA [ 0 ] 
          ; opA [ 1 ] . imm := immA [ 1 ] 
      | OLoc . register 
        =>  opA [ 0 ] . reg [ 0 ] := op . reg [ 0 ] 
          ; opA [ 1 ] . reg [ 0 ] := op . reg [ 1 ] 
      | OLoc . mem 
        =>  EVAL SplitMVar ( op . mvar , mvarA ) 
          ; opA [ 0 ] . mvar := mvarA [ 0 ] 
          ; opA [ 1 ] . mvar := mvarA [ 1 ] 
      END 
    ; RETURN 2 
    END SplitOperand 

; BEGIN 
  END LLTypes 
 . 
