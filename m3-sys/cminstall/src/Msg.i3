(* Copyright 1996, Critical Mass, Inc.  All rights reserved. *)

INTERFACE Msg;

IMPORT AtomList;

VAR Debugging := FALSE;

PROCEDURE AskBool (question, default: TEXT): BOOLEAN;
PROCEDURE Ask     (question, default: TEXT): TEXT;

PROCEDURE Debug (a, b, c, d: TEXT := NIL);
PROCEDURE Out   (a, b, c, d, e, f: TEXT := NIL);
PROCEDURE Warn  (a, b, c, d: TEXT := NIL);
PROCEDURE Error (ec: AtomList.T;  a, b, c, d: TEXT := NIL);

PROCEDURE AttachDrain (filename: TEXT);
PROCEDURE FinishLog (filename: TEXT);

END Msg.
