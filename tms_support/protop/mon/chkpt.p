/*******************************************************************************
 *******************************************************************************
 **                                                                           **
 **                                                                           **
 **  Copyright 2003-2006 Tom Bascom, Greenfield Technologies                  **
 **  http://www.greenfieldtech.com                                            **
 **                                                                           **
 **  ProTop is free software; you can redistribute it and/or modify it        **
 **  under the terms of the GNU General Public License (GPL) as published     **
 **  by the Free Software Foundation; either version 2 of the License, or     **
 **  at your option) any later version.                                       **
 **                                                                           **
 **  ProTop is distributed in the hope that it will be useful, but WITHOUT    **
 **  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or    **
 **  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License     **
 **  for more details.                                                        **
 **                                                                           **
 **  See TERMS.TXT for more information regarding the Terms and Conditions    **
 **  of use and alternative licensing options for this software.              **
 **                                                                           **
 **  A copy of the GPL is in GPL.TXT which was provided with this package.    **
 **                                                                           **
 **  See http://www.fsf.org for more information about the GPL.               **
 **                                                                           **
 **                                                                           **
 *******************************************************************************
 *******************************************************************************
 *
 * chkpt.p
 *
 *
 * Checkpoint History.
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	October 27, 2004
 *
 */

{tms_support/protop/lib/protop.i}

define variable support as character no-undo initial "Checkpoint History".

define variable xseq#        as integer no-undo.

define variable old_log-rd as integer no-undo.
define variable old_os-rd  as integer no-undo.

define temp-table tt_checkpt no-undo
  field xid   as integer
  field id    as integer
  field st    as character
  field fini  as character
  field dirty as integer
  field cptq  as integer
  field scan  as integer
  field apwq  as integer
  field flush as integer
  index xid-idx is unique primary xid /* descending */.

/* restart
 *
 */

procedure mon-restart:

  empty temp-table tt_checkpt.

  delete procedure this-procedure.

  return.

end.

/* initialize
 *
 */

procedure mon-init:

  empty temp-table tt_checkpt.

  /* define labels
   *
   */

  /* no labels for section 1 */

  /* section 2 */

  ui-define-label( support, 2, 1, "Cpt#",       " ChkPt" ).
  ui-define-label( support, 2, 2, "CptStart",   "   Start" ).
  ui-define-label( support, 2, 3, "CptEnd",     "     End" ).
  ui-define-label( support, 2, 4, "CptDirty",   "   Dirty" ).
  ui-define-label( support, 2, 5, "CptQ",       "  ChkPtQ" ).
  ui-define-label( support, 2, 6, "CptScan",    "    Scan" ).
  ui-define-label( support, 2, 7, "CptApwQ",    "   APW Q" ).
  ui-define-label( support, 2, 8, "CptFlushed", " Flushed" ).

  xseq# = 0.

  return.

end.

/* update
 *
 */

procedure mon-update:

  if do-update( support ) = no then return.

  for each dictdb._Checkpoint no-lock:

    find tt_checkpt exclusive-lock where tt_checkpt.id = _checkpoint._checkpoint-id no-error.

    if not available tt_checkpt then
      do:
        create tt_checkpt.
        assign
          xseq# = xseq# + 1
          tt_checkpt.xid = xseq#
        .
      end.

    assign
      tt_checkpt.id    = _Checkpoint._Checkpoint-Id
      tt_checkpt.st    = substring( _Checkpoint._Checkpoint-Time, 12, 8 )
      tt_checkpt.fini  = substring( _Checkpoint._Checkpoint-Len, 12, 8 )
      tt_checkpt.dirty = _Checkpoint._Checkpoint-Dirty
      tt_checkpt.cptq  = _Checkpoint._Checkpoint-CptQ
      tt_checkpt.scan  = _Checkpoint._Checkpoint-Scan
      tt_checkpt.apwq  = _Checkpoint._Checkpoint-ApwQ
      tt_checkpt.flush = _Checkpoint._Checkpoint-Flush
    .

    if tt_checkpt.fini = ? then tt_checkpt.fini = "".

  end.

  define variable i as integer no-undo.

  do-display( support, 1, 100, 11, 1, "Checkpoint Data" ).
  do-display( support, 2, 100, 11, 1, "-" ).

  find dictdb._ActSummary no-lock.

  ui-det( support, 1, 1, 1, "Label-1", "    Checkpoints:" ).
  ui-det( support, 1, 1, 2, "ChkPts",  string( _ActSummary._Summary-Chkpts, ">>>>>9" )).

  ui-det( support, 1, 2, 1, "Label-2", "Buffers Flushed:" ).
  ui-det( support, 1, 2, 2, "Flushed", string( _ActSummary._Summary-Flushed, ">>>>>9" )).

  release dictdb._ActSummary.

  for each tt_checkpt no-lock:

    i = i + 1.

    ui-det( support, 2, i,  1, "Cpt#",       string( tt_checkpt.id,    ">>>>>9" )).
    ui-det( support, 2, i,  2, "CptStart",   string( tt_checkpt.st,    "x(8)" )).
    ui-det( support, 2, i,  3, "CptEnd",     string( tt_checkpt.fini,  "x(8)" )).
    ui-det( support, 2, i,  4, "CptDirty",   string( tt_checkpt.dirty, ">>>>>>>9" )).
    ui-det( support, 2, i,  5, "CptQ",       string( tt_checkpt.cptq,  ">>>>>>>9" )).
    ui-det( support, 2, i,  6, "CptScan",    string( tt_checkpt.scan,  ">>>>>>>9" )).
    ui-det( support, 2, i,  7, "CptApwQ",    string( tt_checkpt.apwq,  ">>>>>>>9" )).
    ui-det( support, 2, i,  8, "CptFlushed", string( tt_checkpt.flush, ">>>>>>>9" )).

  end.

  return.

end.

/** Initialize PP
 **
 **/

subscribe to "mon-restart" anywhere run-procedure "mon-restart".
subscribe to "mon-init"    anywhere run-procedure "mon-init".
subscribe to "mon-update"  anywhere run-procedure "mon-update".

publish "register-disp-type" ( input support ).

return.
