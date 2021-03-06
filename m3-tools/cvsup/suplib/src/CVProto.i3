(* Copyright 1996-2003 John D. Polstra.
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
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgment:
 *      This product includes software developed by John D. Polstra.
 * 4. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
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

INTERFACE CVProto;

IMPORT FileAttr, Rd, Thread, TokScan, Wr;

EXCEPTION NotSupported;

TYPE
  VersionNumber = [0..65535];

  T = OBJECT
    major, minor: VersionNumber;
    v: Values;
  METHODS
    getCmd(rd: Rd.T): TokScan.T
      RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};
    putCmd(wr: Wr.T;
           cmd: TEXT;
           f0, f1, f2, f3, f4, f5, f6, f7, f8, f9: TEXT := NIL;
	   more := FALSE)
      RAISES {Thread.Alerted, Wr.Failure};
  END;

  Values = RECORD
    (* The following fields depend solely on the protocol version.
       The defaults here generally specify the behavior of older
       versions of the protocol.  Newer versions have the opposite
       values. *)
    hasFileAttrs          := FALSE;
    hasHardLinks          := FALSE;
    dirsAreExplicit       := FALSE;
    serverSendsFilter     := TRUE;
    hasClientAccepts      := FALSE;
    hasKeywordControl     := FALSE;
    hasMuxMode            := FALSE;
    exchangesVersions     := FALSE;
    hidesAtticInCVSHeader := FALSE;
    sendsRevDates         := FALSE;  (* In checkout mode. *)
    clientSendsUmask      := FALSE;
    handlesWhiteSpace     := FALSE;  (* In file names, for example. *)
    hasLooseRCSCheck      := FALSE;
    sendsDeltaPhrases     := FALSE;
    hasMD5Auth            := FALSE;
    hasRsyncFilter        := FALSE;
    hasS1GBug             := TRUE;

    (* The following fields are carried along for convenience, but
       they are not directly dependent on the protocol version.  They
       are separately negotiated at start-up time. *)
    attrSupport: FileAttr.SupportInfo;
  END;

VAR
  Current: T;  (* CONST *)

PROCEDURE Lookup(major, minor: VersionNumber): T
  RAISES {NotSupported};
(* Returns the protocol descriptor for the given version number. *)

PROCEDURE Resolve(major, minor: VersionNumber): T
  RAISES {NotSupported};
(* Returns the protocol descriptor for the lesser of the given version
   and the current version. *)

PROCEDURE HasS1GBug(proto: T; versionString: TEXT): BOOLEAN;
(* Returns "TRUE" if the peer is an old version which has the S1G bug.
   "proto" is the negotiated protocol, and "versionString" is the
   textual version name received from the peer. *)

END CVProto.
