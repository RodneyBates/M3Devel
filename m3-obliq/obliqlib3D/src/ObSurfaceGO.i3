(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sat Jun  4 20:11:05 PDT 1994 by najork                   *)
(*       Created on Tue Jan 19 14:00:00 PST 1994 by najork                   *)


INTERFACE ObSurfaceGO;

IMPORT ObGO, ObProtoLoader;

PROCEDURE SetupPackage ();
PROCEDURE SetupModule (loader : ObProtoLoader.T);

TYPE T <: ObGO.T;

END ObSurfaceGO.
