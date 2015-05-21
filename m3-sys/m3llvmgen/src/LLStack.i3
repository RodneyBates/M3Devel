
(* Copyright (C) 1993, Digital Equipment Corporation           *) 
(* All rights reserved.                                        *) 
(* See the file COPYRIGHT for a full description.              *) 
(*                                                             *) 
(* Last modified on Wed Mar 22 08:05:39 PST 1995 by kalsow     *) 
(*      modified on Fri Nov 25 11:34:48 PST 1994 by isard      *) 
(*      modified on Mon Apr 13 09:55:12 PDT 1992 by muller     *) 
 
INTERFACE LLStack 
 
; IMPORT LLCode 
; FROM LLCode IMPORT Op 
; IMPORT LLTypes 
; FROM LLTypes IMPORT Operand , OLoc , MVar , Regno , Force , FlToInt 
; FROM LLTypes IMPORT LLProcTyp , LLVarTyp , OperandPart , AllRegisters 
; IMPORT LLWr 
; IMPORT TIntN 
; FROM M3CG IMPORT Type , MType , ZType , IType , Sign , ByteOffset 
; FROM M3CG_Ops IMPORT ErrorHandler 
 
(* "bounded" means the shift count has already been checked to be less than 32 or such, 
 * unbounded means it might be arbitrarily high. 
 *) 
; TYPE ShiftType 
    = { LeftAlreadyBounded , RightAlreadyBounded , UnboundedPositiveIsLeft } 
 
; TYPE T <: Public 
; TYPE Public 
    = OBJECT 
      METHODS 
        unlock ( ) 
      ; clearall ( ) 
      ; releaseall ( ) 
      ; all_to_mem ( ) 
      ; lock ( r : Regno ) 
      ; find 
          ( stackp : CARDINAL 
          ; force : Force 
          ; set := AllRegisters 
          ; hintaddr := FALSE 
          ) 
      ; freereg ( set := AllRegisters ; operandPart : OperandPart ) : Regno 
      ; set_reg ( stackp : CARDINAL ; r : Regno ; operandPart : OperandPart ) 
      ; set_type ( stackp : CARDINAL ; type : Type ) 
      ; dealloc_reg ( stackp : CARDINAL ; operandPart : OperandPart ) 
      ; corrupt ( reg : Regno ; operandPart : OperandPart ) 
      ; set_fstack ( stackp : CARDINAL ) 
      ; set_mvar ( stackp : CARDINAL ; READONLY mvar : MVar ) 
      ; set_imm ( stackp : CARDINAL ; READONLY imm : TIntN . T ) 
      ; loc ( stackp : CARDINAL ) : OLoc 
      ; op ( stackp : CARDINAL ) : Operand 
      ; pos ( depth : CARDINAL ; place : TEXT ) : CARDINAL 
      ; discard ( depth : CARDINAL ) 
      ; set_error_handler ( err : ErrorHandler ) 
      ; push ( READONLY mvar : MVar ) 
      ; pushnew ( type : MType ; force : Force ; set := AllRegisters ) 
      ; pushimmI ( imm : INTEGER ; type : Type ) 
      ; pushimmT ( imm : TIntN . T ; type : Type ) 
      ; pop ( READONLY mvar : MVar ) 
      ; doloadaddress ( v : LLVarTyp ; o : ByteOffset ) 
      ; dobin 
          ( op : Op ; symmetric , overwritesdest : BOOLEAN ; type : Type ) : BOOLEAN 
      ; dostoreind ( o : ByteOffset ; type : MType ) 
      ; doumul ( ) 
      ; doimul ( ) 
      ; dodiv ( a , b : Sign ) 
      ; domod ( a , b : Sign ) 
      ; doneg ( stack_position := 0 ) 
      ; doabs ( ) 
      ; domaxmin ( type : ZType ; maxmin : MaxMin ) 
      ; fltoint ( mode : FlToInt ; type : Type ) 
      ; inttoflt ( ) 
      ; doshift ( type : IType ; shiftType : ShiftType ) : BOOLEAN 
      ; dorotate ( type : IType ) : BOOLEAN 
      ; doextract ( type : IType ; sign_extend : BOOLEAN ) 
      ; doextract_n ( type : IType ; sign_extend : BOOLEAN ; count : CARDINAL ) 
      ; doextract_mn 
          ( type : IType ; sign_extend : BOOLEAN ; offset , count : CARDINAL ) 
      ; doinsert ( type : IType ) 
      ; doinsert_n ( type : IType ; count : CARDINAL ) 
      ; doinsert_mn ( type : IType ; offset , count : CARDINAL ) 
      ; swap ( ) 
      ; doloophole ( from , to : ZType ) 
      ; doindex_address ( shift , size : INTEGER ; neg : BOOLEAN ) 
      ; docopy ( type : MType ; overlap : BOOLEAN ) 
      ; docopy_n ( n : INTEGER ; type : MType ; overlap : BOOLEAN ) 
      ; doimm ( op : Op ; READONLY imm : TIntN . T ; overwritesdest : BOOLEAN ) 
      ; newdest ( READONLY op : Operand ) 
      ; init ( ) 
      ; end ( ) 
      ; set_current_proc ( LLProc : LLProcTyp ) 
      ; reg ( stackp : CARDINAL ) : Regno 
      ; lower ( reg : Regno ) : TIntN . T 
      ; set_lower ( reg : Regno ; low : TIntN . T ) 
      ; upper ( reg : Regno ) : TIntN . T 
      ; set_upper ( reg : Regno ; up : TIntN . T ) 
      ; non_nil ( reg : Regno ) : BOOLEAN 
      ; set_non_nil ( reg : Regno ) 
      ; 
      END 
 
; TYPE MaxMin = { Max , Min } 
 
; PROCEDURE New ( parent : LLTypes . CodeGenTyp ; cg : LLCode . T ; debug : BOOLEAN ) 
  : T 
 
; PROCEDURE Debug ( t : T ; tag : TEXT ; wr : LLWr . T ) 
 
; END LLStack 
. 
