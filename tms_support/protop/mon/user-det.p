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
 * user-det.p
 *
 *
 * User detail drill down.
 *
 *
 * To Do:
 *
 *	Dig information out of -y output
 *		- startup parameters
 *		- r-code swapping
 *		- s high water mark
 *	Process open files
 *	Monitor -T files (srt, lbi, dbi...)
 *		- last access
 *		- growth
 *	Unix idle time
 *	Show locks that are blocking other people?
 *	CRUD statistics (in hopes that they'll be per user some day...)
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	September 21, 2003
 *
 */

{tms_support/protop/lib/protop.i}
{tms_support/protop/lib/tt_xstat.i}

define variable support as character no-undo initial "User Details".

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

  /* variant 2 -- there are no labels for variant 1	*/

  ui-define-label( support, 2, 1, "UsrNum",   "  Usr" ).
  ui-define-label( support, 2, 2, "UsrName",  "Name           " ).
  ui-define-label( support, 2, 3, "UsrFlag",  "Flags" ).
  ui-define-label( support, 2, 4, "UsrPid",   "PID     " ).
  ui-define-label( support, 2, 5, "UsrDBA",   " DB Access" ).
  ui-define-label( support, 2, 6, "UsrDBR",   "  OS Reads" ).
  ui-define-label( support, 2, 7, "UsrDBW",   " OS Writes" ).
  ui-define-label( support, 2, 8, "HR",       "   Hit%" ).

  return.

end.

/* update
 *
 */

procedure mon-update:

  define variable xuser        as character no-undo.
  define variable examine-item as integer   no-undo.

  if do-update( support ) = no then return.

  publish "get-examine-item" ( output examine-item ).

  define variable u_name  as character no-undo.
  define variable u_flags as character no-undo.
  define variable u_pid   as character no-undo.

  find first dictdb._Connect no-lock where _Connect-usr = examine-item no-error.
  if available _Connect then
    xuser = _Connect-name.
   else
    xuser = "".

  for each dictdb._UserIO no-lock where _UserIO-name = xuser:

    get-userinfo( input _UserIO-id, output u_name, output u_flags, output u_pid ).

    run update_xstat (
      input _UserIO-usr,			/* Unique Id	*/
      input u_name,
      input u_flags,
      input u_pid,
      input _UserIO-dbaccess,			/* Stat1	*/
      input _UserIO-dbread,			/* Stat2	*/
      input _UserIO-dbwrite,			/* Stat3	*/
      input 0
    ).

  end.

  run age_xstat.

  define variable i as integer no-undo.
  define variable x as integer no-undo.
  define variable z as integer no-undo.

  define variable sort-criteria as character no-undo case-sensitive.

  define variable blkstr as character no-undo.
  define variable trxstr as character no-undo.

  publish "get-sort-criteria" ( output sort-criteria ).

  do-display( support, 1, 100, 11, 1, "User Details" ).
  do-display( support, 2, 100, 11, 1, "User " + string( examine-item) + "'s Sessions" ).

  do-SumSample( output x, output z ).

  find dictdb._Connect no-lock where _Connect-usr = examine-item no-error.

  if not available _Connect then return.

  blkstr = get-blocked( input _Connect-usr ).
  if blkstr = "" then blkstr = "--Not Blocked--".

  /** This is NOT portable...
   **
   ** This stuff is really experimental -- feel free to play but I'm leaving
   ** it out of the "production" release.
   **
   **/

  if opsys = "unix" then
    do:

      define variable j as integer no-undo initial 1.
      define variable k as integer no-undo initial 1.

      define variable lg-line as character no-undo extent 5 format "x(80)".
      define variable of-line as character no-undo extent 5 format "x(80)".

/***
      input through value( "grep " + '" ' + string( _Connect-usr ) + '[:,]" ' + pdbname( "dictdb" ) + ".lg" + " | tail -5" ).
      repeat on error undo, leave:
        import unformatted lg-line[j].
        j = j + 1.
      end.
      input close.
      if j > 5 then j = 5.

      input through value( "/usr/sbin/lsof -p " + string( _Connect-PID ) + " | grep -f ./lsof.str" ).
      repeat on error undo, leave:
        import unformatted of-line[k].
        k = k + 1.
      end.
      input close.
      if k > 5 then k = 5.
 ***/

    end.

  /** End of non-portable code...
   **
   **/

  find first dictdb._Trans no-lock where _Trans-usrnum = _Connect-usr no-error.

  if available _Trans then
    trxstr =
      ( if _Trans-txtime = ? then "" else _Trans-txtime ) + " " +
      _Trans-state + " " +
      ( if _Trans-duration = ? then "" else string( _Trans-duration, "hh:mm:ss" )) + " " +
      ( if available _Connect then ( string( _Connect-wait ) + " " + string( _Connect-wait1 )) else "" ).
   else
    trxstr = "--None--".

  i = 1.

  ui-det( support, 1, i,  1, "UsrNum-lbl",  "Usr#: " ).
  ui-det( support, 1, i,  2, "UsrNum",      string( _Connect-usr,    ">>>9" )).
  ui-det( support, 1, i,  3, "UsrName-lbl", " Name: " ).
  ui-det( support, 1, i,  4, "UsrName",     string( _Connect-name,   "x(12)" )).
  ui-det( support, 1, i,  5, "UsrPID-lbl",  " PID: " ).
  ui-det( support, 1, i,  6, "UsrPID",      string( _Connect-PID,    ">>>>>>>9" )).
  ui-det( support, 1, i,  7, "UsrDev-lbl",  " Device: " ).
  ui-det( support, 1, i,  8, "UsrDev",      string( _Connect-device, "x(25)" )).

  i = 2.

  ui-det( support, 1, i,  1, "Trx-lbl",  "Transaction: " ).
  ui-det( support, 1, i,  2, "Trx-str", trxstr ).

  i = 3.

  ui-det( support, 1, i,  1, "Blk-lbl", " Blocked On: " ).
  ui-det( support, 1, i,  2, "Blk-str", blkstr ).

  do i = 1 to j: 
    ui-det( support, 1, ( i + 3 ), 1, "LgLine",  lg-line[i] ).
  end.

  do i = 1 to k: 
    ui-det( support, 1, ( i + j + 3 ),  1, "OFLine",  of-line[i] ).
  end.

  i = 3 + j + k.

  define query q for tt_xstat.

  case sort-criteria:
    when "a" then open query q for each tt_xstat no-lock by tt_xstat.stat1[5]   descending.
    when "r" then open query q for each tt_xstat no-lock by tt_xstat.stat2[5]   descending.
    when "w" then open query q for each tt_xstat no-lock by tt_xstat.stat3[5]   descending.
    when "f" then open query q for each tt_xstat no-lock by tt_xstat.misc1.
    when "p" then open query q for each tt_xstat no-lock by tt_xstat.misc2.
    when "h" then open query q for each tt_xstat no-lock by tt_xstat.stat-ratio descending.
    when "A" then open query q for each tt_xstat no-lock by tt_xstat.stat1[5].
    when "R" then open query q for each tt_xstat no-lock by tt_xstat.stat2[5].
    when "W" then open query q for each tt_xstat no-lock by tt_xstat.stat3[5].
    when "F" then open query q for each tt_xstat no-lock by tt_xstat.misc1      descending.
    when "P" then open query q for each tt_xstat no-lock by tt_xstat.misc2      descending.
    when "H" then open query q for each tt_xstat no-lock by tt_xstat.stat-ratio.
    when "n" then open query q for each tt_xstat no-lock by tt_xstat.xname.
    when "N" then open query q for each tt_xstat no-lock by tt_xstat.xname      descending.
    when "#" then open query q for each tt_xstat no-lock by tt_xstat.xid.
    when "-" then open query q for each tt_xstat no-lock by tt_xstat.xid        descending.
    otherwise     open query q for each tt_xstat no-lock by tt_xstat.stat1[5]   descending.
  end.

  do while true:

    get next q.

    if not available tt_xstat then leave.

    i = i + 1.

    ui-det( support, 2, i, 1, "UsrNum",  string( tt_xstat.xid,       ">>>>9" )).
    ui-det( support, 2, i, 2, "UsrName", string( tt_xstat.xname,     "x(15)" )).
    ui-det( support, 2, i, 3, "UsrFlag", string( tt_xstat.misc1,     "x(5)" )).
    ui-det( support, 2, i, 4, "UsrPid",  string( tt_xstat.misc2,     "x(8)" )).
    ui-det( support, 2, i, 5, "UsrDBA",  string( ( tt_xstat.stat1[x] / z ), ">>>>>>>>>9" )).
    ui-det( support, 2, i, 6, "UsrDBR",  string( ( tt_xstat.stat2[x] / z ), ">>>>>>>>>9" )).
    ui-det( support, 2, i, 7, "UsrDBW",  string( ( tt_xstat.stat3[x] / z ), ">>>>>>>>>9" )).
    ui-det( support, 2, i, 8, "HR",      string( tt_xstat.stat-ratio, ">>9.99%" )).

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
