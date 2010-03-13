UNSAFE MODULE Main;

(* This program endeavors to exercise
 * every sort of integer, float, address comparison,
 * and some set compares, in order to support
 * cleanup and improvement of comparison in m3back.
 * If this isn't fleshed out further, it really
 * belongs in the "c for code" test section.
 *)

<*UNUSED*>PROCEDURE IntegerLT(a,b:INTEGER):BOOLEAN=BEGIN RETURN a<b; END IntegerLT;
<*UNUSED*>PROCEDURE IntegerLE(a,b:INTEGER):BOOLEAN=BEGIN RETURN a<=b; END IntegerLE;
<*UNUSED*>PROCEDURE IntegerGT(a,b:INTEGER):BOOLEAN=BEGIN RETURN a>b; END IntegerGT;
<*UNUSED*>PROCEDURE IntegerGE(a,b:INTEGER):BOOLEAN=BEGIN RETURN a>=b; END IntegerGE;
<*UNUSED*>PROCEDURE IntegerEQ(a,b:INTEGER):BOOLEAN=BEGIN RETURN a=b; END IntegerEQ;
<*UNUSED*>PROCEDURE IntegerNE(a,b:INTEGER):BOOLEAN=BEGIN RETURN a#b; END IntegerNE;
<*UNUSED*>PROCEDURE IntegerLT0(a:INTEGER):BOOLEAN=BEGIN RETURN a<0; END IntegerLT0;
<*UNUSED*>PROCEDURE IntegerLE0(a:INTEGER):BOOLEAN=BEGIN RETURN a<=0; END IntegerLE0;
<*UNUSED*>PROCEDURE IntegerGT0(a:INTEGER):BOOLEAN=BEGIN RETURN a>0; END IntegerGT0;
<*UNUSED*>PROCEDURE IntegerGE0(a:INTEGER):BOOLEAN=BEGIN RETURN a>=0; END IntegerGE0;
<*UNUSED*>PROCEDURE IntegerEQ0(a:INTEGER):BOOLEAN=BEGIN RETURN a=0; END IntegerEQ0;
<*UNUSED*>PROCEDURE IntegerNE0(a:INTEGER):BOOLEAN=BEGIN RETURN a#0; END IntegerNE0;

<*UNUSED*>PROCEDURE RealLT(a,b:REAL):BOOLEAN=BEGIN RETURN a<b; END RealLT;
<*UNUSED*>PROCEDURE RealLE(a,b:REAL):BOOLEAN=BEGIN RETURN a<=b; END RealLE;

<*UNUSED*>PROCEDURE RealGT(a,b:REAL):BOOLEAN=BEGIN RETURN a>b; END RealGT;
<*UNUSED*>PROCEDURE RealGE(a,b:REAL):BOOLEAN=BEGIN RETURN a>=b; END RealGE;

<*UNUSED*>PROCEDURE RealEQ(a,b:REAL):BOOLEAN=BEGIN RETURN a=b; END RealEQ;
<*UNUSED*>PROCEDURE RealNE(a,b:REAL):BOOLEAN=BEGIN RETURN a#b; END RealNE;

<*UNUSED*>PROCEDURE LongrealLT(a,b:LONGREAL):BOOLEAN=BEGIN RETURN a<b; END LongrealLT;
<*UNUSED*>PROCEDURE LongrealLE(a,b:LONGREAL):BOOLEAN=BEGIN RETURN a<=b; END LongrealLE;
<*UNUSED*>PROCEDURE LongrealGT(a,b:LONGREAL):BOOLEAN=BEGIN RETURN a>b; END LongrealGT;
<*UNUSED*>PROCEDURE LongrealGE(a,b:LONGREAL):BOOLEAN=BEGIN RETURN a>=b; END LongrealGE;
<*UNUSED*>PROCEDURE LongrealEQ(a,b:LONGREAL):BOOLEAN=BEGIN RETURN a=b; END LongrealEQ;
<*UNUSED*>PROCEDURE LongrealNE(a,b:LONGREAL):BOOLEAN=BEGIN RETURN a#b; END LongrealNE;

<*UNUSED*>PROCEDURE ExtendedLT(a,b:EXTENDED):BOOLEAN=BEGIN RETURN a<b; END ExtendedLT;
<*UNUSED*>PROCEDURE ExtendedLE(a,b:EXTENDED):BOOLEAN=BEGIN RETURN a<=b; END ExtendedLE;
<*UNUSED*>PROCEDURE ExtendedGT(a,b:EXTENDED):BOOLEAN=BEGIN RETURN a>b; END ExtendedGT;
<*UNUSED*>PROCEDURE ExtendedGE(a,b:EXTENDED):BOOLEAN=BEGIN RETURN a>=b; END ExtendedGE;
<*UNUSED*>PROCEDURE ExtendedEQ(a,b:EXTENDED):BOOLEAN=BEGIN RETURN a=b; END ExtendedEQ;
<*UNUSED*>PROCEDURE ExtendedNE(a,b:EXTENDED):BOOLEAN=BEGIN RETURN a#b; END ExtendedNE;

<*UNUSED*>PROCEDURE AddressLT(a,b:ADDRESS):BOOLEAN=BEGIN RETURN a<b; END AddressLT;
<*UNUSED*>PROCEDURE AddressLE(a,b:ADDRESS):BOOLEAN=BEGIN RETURN a<=b; END AddressLE;
<*UNUSED*>PROCEDURE AddressGT(a,b:ADDRESS):BOOLEAN=BEGIN RETURN a>b; END AddressGT;
<*UNUSED*>PROCEDURE AddressGE(a,b:ADDRESS):BOOLEAN=BEGIN RETURN a>=b; END AddressGE;
<*UNUSED*>PROCEDURE AddressEQ(a,b:ADDRESS):BOOLEAN=BEGIN RETURN a=b; END AddressEQ;
<*UNUSED*>PROCEDURE AddressNE(a,b:ADDRESS):BOOLEAN=BEGIN RETURN a#b; END AddressNE;
<*UNUSED*>PROCEDURE AddressLT0(a:ADDRESS):BOOLEAN=BEGIN RETURN a<NIL; END AddressLT0;
<*UNUSED*>PROCEDURE AddressLE0(a:ADDRESS):BOOLEAN=BEGIN RETURN a<=NIL; END AddressLE0;
<*UNUSED*>PROCEDURE AddressGT0(a:ADDRESS):BOOLEAN=BEGIN RETURN a>NIL; END AddressGT0;
<*UNUSED*>PROCEDURE AddressGE0(a:ADDRESS):BOOLEAN=BEGIN RETURN a>=NIL; END AddressGE0;
<*UNUSED*>PROCEDURE AddressEQ0(a:ADDRESS):BOOLEAN=BEGIN RETURN a=NIL; END AddressEQ0;
<*UNUSED*>PROCEDURE AddressNE0(a:ADDRESS):BOOLEAN=BEGIN RETURN a#NIL; END AddressNE0;

(* CARDINAL is really the same as INTEGER.
 * To get unsigned, use ADDRESS.
 *)

<*UNUSED*>PROCEDURE CardinalLT(a,b:CARDINAL):BOOLEAN=BEGIN RETURN a<b; END CardinalLT;
<*UNUSED*>PROCEDURE CardinalLE(a,b:CARDINAL):BOOLEAN=BEGIN RETURN a<=b; END CardinalLE;
<*UNUSED*>PROCEDURE CardinalGT(a,b:CARDINAL):BOOLEAN=BEGIN RETURN a>b; END CardinalGT;
<*UNUSED*>PROCEDURE CardinalGE(a,b:CARDINAL):BOOLEAN=BEGIN RETURN a>=b; END CardinalGE;
<*UNUSED*>PROCEDURE CardinalEQ(a,b:CARDINAL):BOOLEAN=BEGIN RETURN a=b; END CardinalEQ;
<*UNUSED*>PROCEDURE CardinalNE(a,b:CARDINAL):BOOLEAN=BEGIN RETURN a#b; END CardinalNE;


<*UNUSED*>PROCEDURE LongintLT(a,b:LONGINT):BOOLEAN=BEGIN RETURN a<b; END LongintLT;
<*UNUSED*>PROCEDURE LongintLE(a,b:LONGINT):BOOLEAN=BEGIN RETURN a<=b; END LongintLE;
<*UNUSED*>PROCEDURE LongintGT(a,b:LONGINT):BOOLEAN=BEGIN RETURN a>b; END LongintGT;
<*UNUSED*>PROCEDURE LongintGE(a,b:LONGINT):BOOLEAN=BEGIN RETURN a>=b; END LongintGE;
<*UNUSED*>PROCEDURE LongintEQ(a,b:LONGINT):BOOLEAN=BEGIN RETURN a=b; END LongintEQ;
<*UNUSED*>PROCEDURE LongintNE(a,b:LONGINT):BOOLEAN=BEGIN RETURN a#b; END LongintNE;
<*UNUSED*>PROCEDURE LongintLT0(a:LONGINT):BOOLEAN=BEGIN RETURN a<0L; END LongintLT0;
<*UNUSED*>PROCEDURE LongintLE0(a:LONGINT):BOOLEAN=BEGIN RETURN a<=0L; END LongintLE0;
<*UNUSED*>PROCEDURE LongintGT0(a:LONGINT):BOOLEAN=BEGIN RETURN a>0L; END LongintGT0;
<*UNUSED*>PROCEDURE LongintGE0(a:LONGINT):BOOLEAN=BEGIN RETURN a>=0L; END LongintGE0;
<*UNUSED*>PROCEDURE LongintEQ0(a:LONGINT):BOOLEAN=BEGIN RETURN a=0L; END LongintEQ0;
<*UNUSED*>PROCEDURE LongintNE0(a:LONGINT):BOOLEAN=BEGIN RETURN a#0L; END LongintNE0;

<*UNUSED*>PROCEDURE LongcardLT(a,b:LONGCARD):BOOLEAN=BEGIN RETURN a<b; END LongcardLT;
<*UNUSED*>PROCEDURE LongcardLE(a,b:LONGCARD):BOOLEAN=BEGIN RETURN a<=b; END LongcardLE;
<*UNUSED*>PROCEDURE LongcardGT(a,b:LONGCARD):BOOLEAN=BEGIN RETURN a>b; END LongcardGT;
<*UNUSED*>PROCEDURE LongcardGE(a,b:LONGCARD):BOOLEAN=BEGIN RETURN a>=b; END LongcardGE;
<*UNUSED*>PROCEDURE LongcardEQ(a,b:LONGCARD):BOOLEAN=BEGIN RETURN a=b; END LongcardEQ;
<*UNUSED*>PROCEDURE LongcardNE(a,b:LONGCARD):BOOLEAN=BEGIN RETURN a#b; END LongcardNE;


TYPE Set = SET OF [0..1023];
<*UNUSED*>PROCEDURE SetLT(READONLY a,b:Set):BOOLEAN=BEGIN RETURN a<b; END SetLT;
<*UNUSED*>PROCEDURE SetLE(READONLY a,b:Set):BOOLEAN=BEGIN RETURN a<=b; END SetLE;
<*UNUSED*>PROCEDURE SetGT(READONLY a,b:Set):BOOLEAN=BEGIN RETURN a>b; END SetGT;
<*UNUSED*>PROCEDURE SetGE(READONLY a,b:Set):BOOLEAN=BEGIN RETURN a>=b; END SetGE;
<*UNUSED*>PROCEDURE SetEQ(READONLY a,b:Set):BOOLEAN=BEGIN RETURN a=b; END SetEQ;
<*UNUSED*>PROCEDURE SetNE(READONLY a,b:Set):BOOLEAN=BEGIN RETURN a#b; END SetNE;


<*UNUSED*>PROCEDURE CardinalLT0(a:CARDINAL):BOOLEAN=BEGIN RETURN a<0; END CardinalLT0;
<*UNUSED*>PROCEDURE CardinalLE0(a:CARDINAL):BOOLEAN=BEGIN RETURN a<=0; END CardinalLE0;
<*UNUSED*>PROCEDURE CardinalGT0(a:CARDINAL):BOOLEAN=BEGIN RETURN a>0; END CardinalGT0;
<*UNUSED*>PROCEDURE CardinalGE0(a:CARDINAL):BOOLEAN=BEGIN RETURN a>=0; END CardinalGE0;
<*UNUSED*>PROCEDURE CardinalEQ0(a:CARDINAL):BOOLEAN=BEGIN RETURN a=0; END CardinalEQ0;
<*UNUSED*>PROCEDURE CardinalNE0(a:CARDINAL):BOOLEAN=BEGIN RETURN a#0; END CardinalNE0;

<*UNUSED*>PROCEDURE LongcardLT0(a:LONGCARD):BOOLEAN=BEGIN RETURN a<0L; END LongcardLT0;
<*UNUSED*>PROCEDURE LongcardLE0(a:LONGCARD):BOOLEAN=BEGIN RETURN a<=0L; END LongcardLE0;
<*UNUSED*>PROCEDURE LongcardGT0(a:LONGCARD):BOOLEAN=BEGIN RETURN a>0L; END LongcardGT0;
<*UNUSED*>PROCEDURE LongcardGE0(a:LONGCARD):BOOLEAN=BEGIN RETURN a>=0L; END LongcardGE0;
<*UNUSED*>PROCEDURE LongcardEQ0(a:LONGCARD):BOOLEAN=BEGIN RETURN a=0L; END LongcardEQ0;
<*UNUSED*>PROCEDURE LongcardNE0(a:LONGCARD):BOOLEAN=BEGIN RETURN a#0L; END LongcardNE0;


<*UNUSED*>PROCEDURE no_overlap_less_LT(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a<b; END no_overlap_less_LT;
<*UNUSED*>PROCEDURE no_overlap_less_LE(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a<=b; END no_overlap_less_LE;
<*UNUSED*>PROCEDURE no_overlap_less_GT(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a>b; END no_overlap_less_GT;
<*UNUSED*>PROCEDURE no_overlap_less_GE(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a>=b; END no_overlap_less_GE;
<*UNUSED*>PROCEDURE no_overlap_less_EQ(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a=b; END no_overlap_less_EQ;
<*UNUSED*>PROCEDURE no_overlap_less_NE(a:[0..1]; b:[2..3]):BOOLEAN=BEGIN RETURN a#b; END no_overlap_less_NE;

<*UNUSED*>PROCEDURE minimum_overlap_less_LT(a:[0..1]; b:[1..2]):BOOLEAN=BEGIN RETURN a<b; END minimum_overlap_less_LT;
<*UNUSED*>PROCEDURE minimum_overlap_less_LE(a:[0..1]; b:[1..2]):BOOLEAN=BEGIN RETURN a<=b; END minimum_overlap_less_LE;
<*UNUSED*>PROCEDURE minimum_overlap_less_GT(a:[0..1]; b:[1..2]):BOOLEAN=BEGIN RETURN a>b; END minimum_overlap_less_GT;
<*UNUSED*>PROCEDURE minimum_overlap_less_GE(a:[0..1]; b:[1..2]):BOOLEAN=BEGIN RETURN a>=b; END minimum_overlap_less_GE;
<*UNUSED*>PROCEDURE minimum_overlap_less_EQ(a:[0..1]; b:[1..2]):BOOLEAN=BEGIN RETURN a=b; END minimum_overlap_less_EQ;
<*UNUSED*>PROCEDURE minimum_overlap_less_NE(a:[0..1]; b:[1..2]):BOOLEAN=BEGIN RETURN a#b; END minimum_overlap_less_NE;


<*UNUSED*>PROCEDURE no_overlap_greater_LT(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a<b; END no_overlap_greater_LT;
<*UNUSED*>PROCEDURE no_overlap_greater_LE(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a<=b; END no_overlap_greater_LE;
<*UNUSED*>PROCEDURE no_overlap_greater_GT(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a>b; END no_overlap_greater_GT;
<*UNUSED*>PROCEDURE no_overlap_greater_GE(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a>=b; END no_overlap_greater_GE;
<*UNUSED*>PROCEDURE no_overlap_greater_EQ(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a=b; END no_overlap_greater_EQ;
<*UNUSED*>PROCEDURE no_overlap_greater_NE(a:[2..3]; b:[0..1]):BOOLEAN=BEGIN RETURN a#b; END no_overlap_greater_NE;


<*UNUSED*>PROCEDURE minimum_overlap_greater_LT(a:[1..2]; b:[0..1]):BOOLEAN=BEGIN RETURN a<b; END minimum_overlap_greater_LT;
<*UNUSED*>PROCEDURE minimum_overlap_greater_LE(a:[1..2]; b:[0..1]):BOOLEAN=BEGIN RETURN a<=b; END minimum_overlap_greater_LE;
<*UNUSED*>PROCEDURE minimum_overlap_greater_GT(a:[1..2]; b:[0..1]):BOOLEAN=BEGIN RETURN a>b; END minimum_overlap_greater_GT;
<*UNUSED*>PROCEDURE minimum_overlap_greater_GE(a:[1..2]; b:[0..1]):BOOLEAN=BEGIN RETURN a>=b; END minimum_overlap_greater_GE;
<*UNUSED*>PROCEDURE minimum_overlap_greater_EQ(a:[1..2]; b:[0..1]):BOOLEAN=BEGIN RETURN a=b; END minimum_overlap_greater_EQ;
<*UNUSED*>PROCEDURE minimum_overlap_greater_NE(a:[1..2]; b:[0..1]):BOOLEAN=BEGIN RETURN a#b; END minimum_overlap_greater_NE;


TYPE Numbers = {Zero, One, Two, Three, Four};
TYPE LowNumbers = [Numbers.Zero..Numbers.One];
TYPE HighNumbers = [Numbers.Three..Numbers.Four];
TYPE MiddleNumbers = [Numbers.One..Numbers.Three];

<*UNUSED*>PROCEDURE no_overlap_less_enum_LT(a:LowNumbers; b:HighNumbers):BOOLEAN=BEGIN RETURN a<b; END no_overlap_less_enum_LT;
<*UNUSED*>PROCEDURE no_overlap_less_enum_LE(a:LowNumbers; b:HighNumbers):BOOLEAN=BEGIN RETURN a<=b; END no_overlap_less_enum_LE;
<*UNUSED*>PROCEDURE no_overlap_less_enum_GT(a:LowNumbers; b:HighNumbers):BOOLEAN=BEGIN RETURN a>b; END no_overlap_less_enum_GT;
<*UNUSED*>PROCEDURE no_overlap_less_enum_GE(a:LowNumbers; b:HighNumbers):BOOLEAN=BEGIN RETURN a>=b; END no_overlap_less_enum_GE;
<*UNUSED*>PROCEDURE no_overlap_less_enum_EQ(a:LowNumbers; b:HighNumbers):BOOLEAN=BEGIN RETURN a=b; END no_overlap_less_enum_EQ;
<*UNUSED*>PROCEDURE no_overlap_less_enum_NE(a:LowNumbers; b:HighNumbers):BOOLEAN=BEGIN RETURN a#b; END no_overlap_less_enum_NE;

<*UNUSED*>PROCEDURE minimum_overlap_less_enum_LT(a:LowNumbers; b:MiddleNumbers):BOOLEAN=BEGIN RETURN a<b; END minimum_overlap_less_enum_LT;
<*UNUSED*>PROCEDURE minimum_overlap_less_enum_LE(a:LowNumbers; b:MiddleNumbers):BOOLEAN=BEGIN RETURN a<=b; END minimum_overlap_less_enum_LE;
<*UNUSED*>PROCEDURE minimum_overlap_less_enum_GT(a:LowNumbers; b:MiddleNumbers):BOOLEAN=BEGIN RETURN a>b; END minimum_overlap_less_enum_GT;
<*UNUSED*>PROCEDURE minimum_overlap_less_enum_GE(a:LowNumbers; b:MiddleNumbers):BOOLEAN=BEGIN RETURN a>=b; END minimum_overlap_less_enum_GE;
<*UNUSED*>PROCEDURE minimum_overlap_less_enum_EQ(a:LowNumbers; b:MiddleNumbers):BOOLEAN=BEGIN RETURN a=b; END minimum_overlap_less_enum_EQ;
<*UNUSED*>PROCEDURE minimum_overlap_less_enum_NE(a:LowNumbers; b:MiddleNumbers):BOOLEAN=BEGIN RETURN a#b; END minimum_overlap_less_enum_NE;


<*UNUSED*>PROCEDURE no_overlap_greater_enum_LT(a:HighNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a<b; END no_overlap_greater_enum_LT;
<*UNUSED*>PROCEDURE no_overlap_greater_enum_LE(a:HighNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a<=b; END no_overlap_greater_enum_LE;
<*UNUSED*>PROCEDURE no_overlap_greater_enum_GT(a:HighNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a>b; END no_overlap_greater_enum_GT;
<*UNUSED*>PROCEDURE no_overlap_greater_enum_GE(a:HighNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a>=b; END no_overlap_greater_enum_GE;
<*UNUSED*>PROCEDURE no_overlap_greater_enum_EQ(a:HighNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a=b; END no_overlap_greater_enum_EQ;
<*UNUSED*>PROCEDURE no_overlap_greater_enum_NE(a:HighNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a#b; END no_overlap_greater_enum_NE;


<*UNUSED*>PROCEDURE minimum_overlap_greater_enum_LT(a:MiddleNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a<b; END minimum_overlap_greater_enum_LT;
<*UNUSED*>PROCEDURE minimum_overlap_greater_enum_LE(a:MiddleNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a<=b; END minimum_overlap_greater_enum_LE;
<*UNUSED*>PROCEDURE minimum_overlap_greater_enum_GT(a:MiddleNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a>b; END minimum_overlap_greater_enum_GT;
<*UNUSED*>PROCEDURE minimum_overlap_greater_enum_GE(a:MiddleNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a>=b; END minimum_overlap_greater_enum_GE;
<*UNUSED*>PROCEDURE minimum_overlap_greater_enum_EQ(a:MiddleNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a=b; END minimum_overlap_greater_enum_EQ;
<*UNUSED*>PROCEDURE minimum_overlap_greater_enum_NE(a:MiddleNumbers; b:LowNumbers):BOOLEAN=BEGIN RETURN a#b; END minimum_overlap_greater_enum_NE;

BEGIN
END Main.
