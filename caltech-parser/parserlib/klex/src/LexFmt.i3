(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: LexFmt.i3,v 1.2 2001-09-19 15:05:08 wagner Exp $ *)

INTERFACE LexFmt;
IMPORT Wr;
IMPORT Rd, TokSpec;
TYPE
  T <: Public;
  Public = OBJECT METHODS
    writeInterface(to: Wr.T);
    writeModule(to: Wr.T);
    test();
  END;
PROCEDURE New(from: Rd.T; tok: TokSpec.T;
              outMN, tokMN: TEXT): T;
END LexFmt.
