INTERFACE RandomCombinedSlow;
(* Gnu CopyLefted. *)
(**
Abstract: Psuedo-random number generator by Warren D. Smith.
Runtimes (in 100 MHz pentium clock ticks per call, roughly):
    RandWord           470
    Uni01             1000
    FasterUni01         80
    FasterRandWord      80

Usage:  You can call these directly, or use the T OBJECTS:
    VAR
      myrand:=NEW(RandomCombinedSlow.T).init();
    BEGIN
      value:=myrand.uniform();
    END;


3/23/96  Warren Smith    Initial version
*)

IMPORT RandomBasic;

(** The random words output by these generators ought to be extremely
random since the engine is a combination of 5 generators, each pretty good by
itself, and all 5 work according to different principles. I think
they will be good enough for any application except cryptography, and
quite likely good enough even for that. Its only
disadvantage is it is too slow for applications which do
a very small amount of computing per random number.
**********************************************)


(*** Initializes all random number generators here. Quite slow.
If fixed=FALSE (the default) will incorporate the
   time into the seed.
If TRUE will use a particular fixed seed.
*************************************************************)
TYPE
  T <: TPublic;

  TPublic =
    RandomBasic.T OBJECT METHODS init (fixed: BOOLEAN := FALSE; ): T; END;

END RandomCombinedSlow.
