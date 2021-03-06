(*                            -*- Mode: Modula-3 -*- 
 * 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * See file COPYRIGHT-COLUMBIA for details.
 * 
 * Author          : Blair MacIntyre
 * Created On      : Mon Feb 20 17:43:14 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Sat Aug  9 13:47:31 1997
 * Update Count    : 8
 * 
 * $Source: /opt/cvs/cm3/m3-comm/rdwr/src/RdWrPipe.i3,v $
 * $Date: 2001-12-02 00:35:21 $
 * $Author: wagner $
 * $Revision: 1.2 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.1.1.1  2001/12/02 00:29:10  wagner
 * Blair MacIntyre's rdwr library
 *
 * Revision 1.2  1997/08/11 20:36:20  bm
 * Various fixes
 *
 * 
 * HISTORY
 *)

(* The FileRdWr module sets up and returns a Rd/Wr pair which are
   linked together. Anything written to the writer is immediately
   available to the reader.  *)

INTERFACE RdWrPipe;

IMPORT Rd, Wr;

CONST
  (* the default size of the shared buffer *)
  BufferSize = 1024;

PROCEDURE New(VAR rd: Rd.T; VAR wr: Wr.T; buff_size: CARDINAL :=
  BufferSize; nm : TEXT := NIL);
(* Returns a read and writer which are connected together. *)

PROCEDURE ResetRdCounter(rd: Rd.T);
(* Reset the cur, lo and hi pointers, to allow this to read more
   characters than LAST(CARDINAL).  Should be called periodically. *)

PROCEDURE ResetWrCounter(wr: Wr.T);
(* Reset the cur, lo and hi pointers, to allow this to write more
   characters than LAST(CARDINAL).  Should be called periodically. *)

END RdWrPipe.
