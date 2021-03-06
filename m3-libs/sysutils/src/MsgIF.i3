(* Copyright 1999-2002 elego Software Solutions GmbH, Berlin, Germany.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

(*--------------------------------------------------------------------------*)
INTERFACE MsgIF;

IMPORT Wr, Pathname, OSError;

(*--------------------------------------------------------------------------*)
TYPE
  T <: Public;


  (*------------------------------------------------------------------------*)
  (* A MsgIF.T is an objectified version of the Msg interface. It contains
     several writer references to use for different output methods. The
     boolean variables tFlag, dFlag, and vFlag have been replaced by a
     concept of levels. This module should better be called MsgWr, but this
     name is already used in libm3.
  *)
  Public = OBJECT
    errorWr      : Wr.T;
    fatalWr      : Wr.T;
    warningWr    : Wr.T;
    debugWr      : Wr.T;
    vWr          : Wr.T;
    tWr          : Wr.T;
    dWr          : Wr.T;
    beepFlag     : BOOLEAN;
    debugLevel   : INTEGER;
    traceLevel   : INTEGER;
    verboseLevel : INTEGER;
  METHODS
    (*----------------------------------------------------------------------*)
    init(wr : Wr.T; dLevel := 0; tLevel := 0; vLevel := 0; beep := FALSE) : T;
    (* initialize all writers with wr *)

    (*----------------------------------------------------------------------*)
    detailedInit(wr : Wr.T; dLevel := 0; tLevel := 0; vLevel := 0;
                 beep := FALSE;
                 errorWr   : Wr.T := NIL; (* use wr if not specified *)
                 fatalWr   : Wr.T := NIL; (* use wr if not specified *)
                 warningWr : Wr.T := NIL; (* use wr if not specified *)
                 debugWr   : Wr.T := NIL; (* use wr if not specified *)
                 dWr       : Wr.T := NIL; (* use wr if not specified *)
                 tWr       : Wr.T := NIL; (* use wr if not specified *)
                 vWr       : Wr.T := NIL; (* use wr if not specified *)
                 ) : T;
    (* initialize a special setup *)

    (*----------------------------------------------------------------------*)
    close();
    (* close all writers *)

    (*----------------------------------------------------------------------*)
    error(msg : TEXT);
      (* write `msg' tagged as error to `errorWr' *)

    (*----------------------------------------------------------------------*)
    fatal(msg : TEXT);
      (* write `msg' tagged as fatal error to `fatalWr' *)

    (*----------------------------------------------------------------------*)
    warning(msg : TEXT);
      (* write `msg' tagged as warning to `warningWr' *)

    (*----------------------------------------------------------------------*)
    debug(msg : TEXT; level := 1);
      (* write `msg' tagged as debug to `debugWr' *)

    (*----------------------------------------------------------------------*)
    error2(proc, msg : TEXT);
      (* write `proc: msg' tagged as error to `errorWr' *)

    (*----------------------------------------------------------------------*)
    fatal2(proc, msg : TEXT);
      (* write `proc: msg' tagged as fatal error to `fatalWr' *)

    (*----------------------------------------------------------------------*)
    warning2(proc, msg : TEXT);
      (* write `proc: msg' tagged as warning to `warningWr' *)

    (*----------------------------------------------------------------------*)
    debug2(proc, msg : TEXT; level := 1);
      (* write `proc: msg' tagged as debug to `debugWr' *)

    (*----------------------------------------------------------------------*)
    v(msg : TEXT; unconditionalNewLine := TRUE; level := 1);
      (* write `msg' to `vWr' if vFlag set *)

    (*----------------------------------------------------------------------*)
    v2(proc, msg : TEXT; unconditionalNewLine := TRUE; level := 1);
      (* write `proc: msg' to `vWr' if vFlag set *)

    (*----------------------------------------------------------------------*)
    t(msg : TEXT; unconditionalNewLine := TRUE; level := 1);
      (* write `msg' to `tWr' if tFlag set *)

    (*----------------------------------------------------------------------*)
    t2(proc, msg : TEXT; unconditionalNewLine := TRUE; level := 1);
      (* write `proc: msg' to `tWr' if tFlag set *)

    (*----------------------------------------------------------------------*)
    d(msg : TEXT; unconditionalNewLine := TRUE; level := 1);
      (* write `msg' to `dWr' if dFlag set *)

    (*----------------------------------------------------------------------*)
    d2(proc, msg : TEXT; unconditionalNewLine := TRUE; level := 1);
      (* write `proc: msg' to `dWr' if dFlag set *)

  END;

(*--------------------------------------------------------------------------*)
PROCEDURE New(wr : Wr.T; dLevel := 0; tLevel := 0; vLevel := 0;
              beep := FALSE) : T;

(*--------------------------------------------------------------------------*)
PROCEDURE NewTextWr(dLevel := 0; tLevel := 0; vLevel := 0; beep := FALSE) : T;

(*--------------------------------------------------------------------------*)
PROCEDURE NewFileWr(fn : Pathname.T; dLevel := 0; tLevel := 0; vLevel := 0;
                    beep := FALSE) : T
  RAISES {OSError.E};

(*--------------------------------------------------------------------------*)
PROCEDURE NewStdWr(dLevel := 0; tLevel := 0; vLevel := 0; beep := FALSE) : T;

END MsgIF.
