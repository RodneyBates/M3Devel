GENERIC INTERFACE FloatTrans(R, RB, RX);
(* Arithmetic for Modula-3, see doc for details

   Abstract: Generic wrapper routines for (mainly) transcendent functions

   If R.T is

   REAL, then the instantiated module is like Mth

   LONGREAL, then the instantiated module is a wrapper for Math with
   procedure names conforming to the conventions

   EXTENDED, then the instantiated module may be used for transcendental
   computations but the precision is only that of LONGREAL because this is
   the precision of the standard C math library


   SqRt should throw an exception if applied to negative numbers *)



TYPE T = R.T;


TYPE Ftn = PROCEDURE (x: T; ): T;

CONST
  (*---distinguished elements---*)
  Zero     = RB.Zero;
  Half     = RB.Half;
  One      = RB.One;
  MinusOne = RB.MinusOne;
  Two      = RB.Two;

  E = FLOAT(2.71828182845904523536028747135266249776X0, T); (* e *)
  LnFour = FLOAT(1.38629436111989061883446424291635313615X0, T); (* ln(4) *)
  SqRt2ByE = FLOAT(0.85776388496070679648018964127877247812X0, T); (* sqrt(2/e) *)
  SqRtTwo = FLOAT(1.41421356237309504880168872420969807857X0, T); (* sqrt(2) *)

  LnTwo = LnFour * Half;         (*FLOAT(0.693147180559945D0,T);*)(* ln(2) *)

  Pi   = FLOAT(3.14159265358979323846264338327950288420X0, T);
  LnPi = FLOAT(1.14472988584940017414342735135305871165X0, T); (* ln(pi) *)
  SqRtPi = FLOAT(1.77245385090551602729816748334114518280X0, T);

  TwoPi      = Two * Pi;
  OneOverPi  = One / Pi;
  TwoOverPi  = Two / Pi;
  FourOverPi = Two * TwoOverPi;

  EulerGamma = FLOAT(0.57721566490153286060651209008240243106X0, T); (* Euler's
                                                                        constant
                                                                        "gamma"*)
  GoldenRatio = FLOAT(1.61803398874989484820458683436563811772X0, T); (* golden
                                                                         ratio *)
  DegPerRad = FLOAT(180.0D0, R.T) / Pi; (* degrees per radian *)
  RadPerDeg = Pi / FLOAT(180.0D0, R.T); (* radians per degree *)

CONST
  Base            = R.Base;
  Precision       = R.Precision;
  MaxFinite       = R.MaxFinite;
  MinPos          = R.MinPos;
  MinPosNormal    = R.MinPosNormal;
  MaxExpDigits    = R.MaxExpDigits;
  MaxSignifDigits = R.MaxSignifDigits;

  (*---boundaries for precision testing---*)
  Tiny = R.MinPos * FLOAT(1000.0, T); (* nearly 0.0 *)
  Huge = R.MaxFinite / FLOAT(1000.0, T); (* nearly infinite *)
  (*Eps = Pow(FLOAT(R.Base,T),-FLOAT(R.Precision,T)); (* approx relative
     machine precision *) *)
  (*Eps = LongFloat.Scalb(One,-R.Precision);*)
  Eps = RX.Eps;

<* INLINE *>
PROCEDURE Abs (c: T; ): T;       (* magnitude *)
<* INLINE *>
PROCEDURE AbsSqr (c: T; ): T;    (* square of the magnitude *)

(*---- Exponential and Logarithm functions ----*)
<* INLINE *>
PROCEDURE Exp (x: T; ): T;       (* e^x *)
<* INLINE *>
PROCEDURE Expm1 (x: T; ): T;     (* e^(x-1) *)
<* INLINE *>
PROCEDURE Ln (x: T; ): T;        (* ln(x) *)
<* INLINE *>
PROCEDURE Ln1p (x: T; ): T;      (* ln(1+x) *)
<* INLINE *>
PROCEDURE Lb (x: T; ): T;        (* log2(x) *)
<* INLINE *>
PROCEDURE Lg (x: T; ): T;        (* log10(x) *)
<* INLINE *>
PROCEDURE Log (x, y: T; ): T;    (* log_y(x) *)
<* INLINE *>
PROCEDURE Pow (x, y: T; ): T;    (* x^y *)
<* INLINE *>
PROCEDURE SqRt (x: T; ): T;      (* square root of x *)

(*---- Trigonometric functions ----*)
<* INLINE *>
PROCEDURE Cos (x: T; ): T;       (* cosine of x radians. *)
<* INLINE *>
PROCEDURE Sin (x: T; ): T;       (* sine of x radians. *)
<* INLINE *>
PROCEDURE Tan (x: T; ): T;       (* tangent of x radians. *)
<* INLINE *>
PROCEDURE ArcCos (x: T; ): T;    (* arc cosine of x in radians. *)
<* INLINE *>
PROCEDURE ArcSin (x: T; ): T;    (* arc sine of x in radians. *)
<* INLINE *>
PROCEDURE ArcTan (x: T; ): T;    (* arc tangent of x in radians. *)
<* INLINE *>
PROCEDURE ArcTan2 (y, x: T; ): T; (* arc tangent of y/x in radians. *)

(*---- Hyperbolic trigonometric functions ----*)

<* INLINE *>
PROCEDURE CosH (x: T; ): T;      (* hyperbolic cosine of x. *)
<* INLINE *>
PROCEDURE SinH (x: T; ): T;      (* hyperbolic sine of x. *)
<* INLINE *>
PROCEDURE TanH (x: T; ): T;      (* hyperbolic tangent of x. *)
<* INLINE *>
PROCEDURE ArCosH (x: T; ): T;    (* inverse hyperbolic cosine of x *)
<* INLINE *>
PROCEDURE ArSinH (x: T; ): T;    (* inverse hyperbolic sine of x *)
<* INLINE *>
PROCEDURE ArTanH (x: T; ): T;    (* inverse hyperbolic tangent of x *)


(*---- Other Functions ----*)

<* INLINE *>
PROCEDURE Sgn (x: T; ): T;       (* One if x is positive, MinusOne if x is
                                    negative, Zero if x is zero *)

END FloatTrans.
