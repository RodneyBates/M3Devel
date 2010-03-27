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

INTERFACE StatusFile;

IMPORT FileStatus, Pathname, SupFileRec, Thread, Time;

TYPE
  T = OBJECT METHODS
    get(name: Pathname.T; isDirUp := FALSE; deleteTo := FALSE): FileStatus.T
      RAISES {FileStatus.Error, Thread.Alerted};
    delete(name: Pathname.T; isDirUp := FALSE)
      RAISES {FileStatus.Error, Thread.Alerted};
    put(fs: FileStatus.T)
      RAISES {FileStatus.Error, Thread.Alerted};
    close()
      RAISES {FileStatus.Error, Thread.Alerted};
  END;

PROCEDURE Open(sfr: SupFileRec.T;
               scanTime: Time.T := -1.0d0;
	       destDir: Pathname.T := NIL;
	       readFromDestDir := FALSE): T
  RAISES {FileStatus.Error, Thread.Alerted};
(* Opens the status file for the collection specified by "sfr". *)

(* The "scanTime" parameter controls whether the file is opened read-only,
   or for updating.  The default value causes it to be opened read-only.
   If a non-default scan time is specified, the status file is opened in
   update mode, and the given scan time is written into the file header.
   In that case, a separate destination directory can also be specified,
   and it will be prepended to the standard filename for writing the status
   file.  If "readFromDestDir" is "TRUE", then the status file will be
   read from the destination directory as well. *)

END StatusFile.
