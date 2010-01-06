(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: GenLE.mg                                              *)
(* Last Modified On Mon Dec  5 15:30:50 PST 1994 By kalsow     *)
(*      Modified On Tue Apr 10 11:13:09 1990 By muller         *)

GENERIC MODULE GenLE (Rep);

IMPORT CG, CallExpr, Expr, ExprRep, Procedure, Target, TWord;
IMPORT Bool, IntegerExpr, Value, Formal, Type, ProcType;
FROM Rep IMPORT T;

VAR Z: CallExpr.MethodList;
VAR formals: Value.T;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formals, ce.proc);
    ce.type := Bool.T;
  END Check;

PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    Expr.Compile (ce.args[0]);
    Expr.Compile (ce.args[1]);
    CG.Compare (Rep.Unsigned, CG.Cmp.LE);
  END Compile;

PROCEDURE PrepBR (ce: CallExpr.T;  true, false: CG.Label;  freq: CG.Frequency)=
  BEGIN
    Expr.Prep (ce.args[0]);
    Expr.Prep (ce.args[1]);
    Expr.Compile (ce.args[0]);
    Expr.Compile (ce.args[1]);
    CG.If_then (Rep.Unsigned, CG.Cmp.LE, true, false, freq);
  END PrepBR;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR w0, w1: Target.Int;
  BEGIN
    IF GetArgs (ce.args, w0, w1)
      THEN RETURN Bool.Map [TWord.LE (w0, w1)];
      ELSE RETURN NIL;
    END;
  END Fold;

PROCEDURE GetArgs (args: Expr.List;  VAR w0, w1: Target.Int): BOOLEAN =
  VAR e0, e1: Expr.T;  t: Type.T;
  BEGIN
    e0 := Expr.ConstValue (args[0]);
    e1 := Expr.ConstValue (args[1]);
    RETURN (e0 # NIL) AND IntegerExpr.Split (e0, w0, t) AND 
           (e1 # NIL) AND IntegerExpr.Split (e1, w1, t);
  END GetArgs;

PROCEDURE Initialize () =
  VAR
    x1 := Formal.NewBuiltin ("x", 0, T);
    y1 := Formal.NewBuiltin ("y", 1, T);
    t1 := ProcType.New (Bool.T, x1, y1);
  BEGIN
    Z := CallExpr.NewMethodList (2, 2, TRUE, TRUE, TRUE, Bool.T,
                                 NIL,
                                 CallExpr.NotAddressable,
                                 Check,
                                 CallExpr.PrepArgs,
                                 Compile,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 PrepBR,
                                 CallExpr.NoBranch,
                                 Fold,
                                 CallExpr.NoBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.Define ("LE", Z, FALSE, t1);
    formals := ProcType.Formals (t1);
  END Initialize;

BEGIN
END GenLE.
