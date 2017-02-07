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
 * helpers.p
 *
 *
 * Show helper process (BIW, AIW & APW) activity
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	May 29, 2006
 *
 */

{tms_support/protop/lib/protop.i}

define variable support as character no-undo initial "Helper Processes".

{tms_support/protop/lib/tt_xstat.i}

ratio-calc = 1.

/* restart
 *
 */

procedure mon-restart:

  empty temp-table tt_xstat.

  delete procedure this-procedure.

  return.

end.

/* initialize
 *
 */

procedure mon-init:

  empty temp-table tt_xstat.

  /* define labels
   *
   */

  ui-define-label( support, 1,  1, "xname",   "Helper Metric       " ).
  ui-define-label( support, 1,  2, "helper1", "        BIW" ).
  ui-define-label( support, 1,  3, "helper2", "    BIW%" ).
  ui-define-label( support, 1,  4, "helper3", "        AIW" ).
  ui-define-label( support, 1,  5, "helper4", "    AIW%" ).

  ui-define-label( support, 2,  1, "xname",   "                    " ).
  ui-define-label( support, 2,  2, "apw1", "        APW" ).
  ui-define-label( support, 2,  3, "apw2", "    APW%" ).
  ui-define-label( support, 2,  4, "apw3", "           " ).
  ui-define-label( support, 2,  5, "apw4", "        " ).

  return.

end.

/* update
 *
 */

procedure mon-update:

  define variable tlist as character no-undo.
  define variable xlist as character no-undo.

  if do-update( support ) = no then return.

  find dictdb._ActBiLog no-lock no-error.
  find dictdb._ActAiLog no-lock no-error.
  run update_xstat (  1, "      Notes Written:", "", "", 0, _BiLog-RecWriten,   0, _AiLog-RecWriten ).
  run update_xstat (  2, "       BI/AI Writes:", "", "", _BiLog-BIWWrites, _BiLog-TotalWrts, _AiLog-AIWWrites, _AiLog-TotWrites ).
  run update_xstat (  3, "     BIW/AIW Writes:", "", "", 0, _BiLog-BIWWrites,   0, _AiLog-AIWWrites ).
  run update_xstat (  4, "Partial Buff Writes:", "", "", 0, _BiLog-PartialWrts, 0, _AiLog-PartialWrt ).
  run update_xstat (  5, "  Busy Buffer Waits:", "", "", 0, _BiLog-BbuffWaits,  0, _AiLog-BBuffWaits ).
  run update_xstat (  6, " Empty Buffer Waits:", "", "", 0, _BiLog-EbuffWaits,  0, _AiLog-NobufAvail ).

  find dictdb._ActPWs no-lock no-error.
  find dictdb._Startup no-lock no-error.
  run update_xstat ( 20, "          DB Writes:", "", "", 0, _PW-TotDBWrites, 0, 0 ).
  run update_xstat ( 21, "         APW Writes:", "", "", 0, _PW-DBWrites, _PW-DBWrites, _PW-TotDBWrites ).
  run update_xstat ( 22, "        Scan Cycles:", "", "", 0, _PW-ScanCycles,   0, 0 ).
  run update_xstat ( 23, "    Buffers Scanned:", "-pwsdelay", "", 0, _PW-BuffsScaned,  0, _Startup-APWSTime ).
  run update_xstat ( 24, "    APW Scan Writes:", "-pwqdelay", "", 0, _PW-ScanWrites,   0, _Startup-APWQTime ).
  run update_xstat ( 25, "   APW Queue Writes:", "-pwscan",   "", 0, _PW-ApwQWrites,   0, _Startup-APWBuffs ).
  run update_xstat ( 26, " Chkpt Queue Writes:", "-pwwmax",   "", 0, _PW-CkpQWrites,   0, _Startup-APWMaxWrites ).
  run update_xstat ( 27, "Flush at Checkpoint:", "", "", 0, _PW-Flushed,      0, 0 ).

  run age_xstat.

  define variable i as integer no-undo.
  define variable x as integer no-undo.
  define variable z as integer no-undo.

  define variable sort-criteria as character no-undo case-sensitive.

  do-display( support, 1, 100, 11, 1, "Helper Processes" ).
  do-display( support, 2, 110, 11, 1, " " ).

  do-SumSample( output x, output z ).

  publish "get-sort-criteria" ( output sort-criteria ).

  define query q for tt_xstat.
  open query q for each tt_xstat no-lock by tt_xstat.xid.

  do while true:

    get next q.

    if not available tt_xstat then leave.

    if tt_xstat.xname = "" then next.

    i = i + 1.

    if tt_xstat.xid < 20 then
      do:
        ui-det( support, 1, i, 1, "xname", string( tt_xstat.xname, "x(20)" )).
        ui-det( support, 1, i, 2, "helper1", string( ( tt_xstat.stat2[x] / z ), ">>>>>>>9.99" )).
        if tt_xstat.xid = 2 then
          ui-det( support, 1, i, 3, "helper2", string( ( tt_xstat.stat-ratio ), " >>9.99%" )).
         else
          ui-det( support, 1, i, 3, "helper2", "        " ).
        ui-det( support, 1, i, 4, "helper3", string( ( tt_xstat.stat4[x] / z ), ">>>>>>>9.99" )).
        if tt_xstat.xid = 2 then
          ui-det( support, 1, i, 5, "helper4", string( ( tt_xstat.stat-ratio2 ), " >>9.99%" )).
         else
          ui-det( support, 1, i, 5, "helper4", "        " ).
      end.
     else if tt_xstat.xid >= 20 and tt_xstat.xid < 27 then
      do:
        ui-det( support, 2, i, 1, "xname", string( tt_xstat.xname, "x(20)" )).
        ui-det( support, 2, i, 2, "apw1", string( ( tt_xstat.stat2[x] / z ), ">>>>>>>9.99" )).
        if tt_xstat.xid = 21 then
          ui-det( support, 2, i, 3, "apw2", string( ( tt_xstat.stat-ratio2 ), " >>9.99%" )).
         else
          ui-det( support, 2, i, 3, "apw2", "        " ).
        ui-det( support, 2, i, 4, "apw3", string( tt_xstat.misc1, "x(12)" )).
        if tt_xstat.misc1 > "" then ui-det( support, 2, i, 5, "apw4", string( tt_xstat.stat4[3], ">>>>>>9" )).
      end.
     else if tt_xstat.xid = 27 then
      do:
        ui-det( support, 2, i, 1, "xname", string( tt_xstat.xname, "x(20)" )).
        ui-det( support, 2, i, 2, "apw1", string( ( tt_xstat.stat2[3] ), ">>>>>>>>>>9" )).
      end.

  end.

  close query q.

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
