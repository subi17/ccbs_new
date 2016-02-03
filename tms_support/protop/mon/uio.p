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
 * uio.p
 *
 *
 * UserIO monitoring.
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	September 5, 2003
 *
 */

{tms_support/protop/lib/protop.i}

define variable support  as character no-undo initial "User IO".

{tms_support/protop/lib/tt_xstat.i}

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

  ui-define-label( support, 1, 1, "xid",        "  Usr" ).
  ui-define-label( support, 1, 2, "xname",      "Name    " ).
  ui-define-label( support, 1, 3, "misc1",      "Flags" ).
  ui-define-label( support, 1, 4, "misc2",      "PID     " ).
  ui-define-label( support, 1, 5, "stat1",      " DB Access" ).
  ui-define-label( support, 1, 6, "stat2",      "  OS Reads" ).
  ui-define-label( support, 1, 7, "stat3",      " OS Writes" ).
  ui-define-label( support, 1, 8, "stat-ratio", "   Hit%" ).
  ui-define-label( support, 1, 9, "CPUTime",    "     CPU" ).

  return.

end.

/* update
 *
 */

procedure mon-update:

  define variable tlist  as character no-undo.
  define variable xlist  as character no-undo.

  define variable u_name  as character no-undo.
  define variable u_flags as character no-undo.
  define variable u_pid   as character no-undo.

  if do-update( support ) = no then return.

  publish "get-tlist" ( output tlist ).
  publish "get-xlist" ( output xlist ).

  for each  dictdb._UserIO no-lock where _UserIO-usr <> ?:

    if tlist <> "" then if lookup( _UserIO-name, tlist )  = 0 then next.
    if xlist <> "" then if lookup( _UserIO-name, xlist ) <> 0 then next.

    get-userinfo( input _UserIO-Id, output u_name, output u_flags, output u_pid ).

    run update_xstat (
      input _UserIO-usr,			/* unique key		*/
      input _UserIO-name,
      input u_flags,
      input u_pid,
      input _UserIO-dbaccess,			/* stat1		*/
      input _UserIO-dbread,			/* stat2		*/
      input _UserIO-dbwrite,			/* stat3		*/
      input 0
    ).

  end.

  run age_xstat.

  define variable i as integer no-undo.
  define variable x as integer no-undo.
  define variable z as integer no-undo.

  define variable sort-criteria as character no-undo case-sensitive.

  do-display( support, 1, 100, 11, 1, "UIO" ).

  do-SumSample( output x, output z ).

  publish "get-sort-criteria" ( output sort-criteria ).

  define query q for tt_xstat.

  case sort-criteria:
    when "a" then open query q for each tt_xstat /* no-lock */ by tt_xstat.stat1[x]   descending.
    when "r" then open query q for each tt_xstat /* no-lock */ by tt_xstat.stat2[x]   descending.
    when "w" then open query q for each tt_xstat /* no-lock */ by tt_xstat.stat3[x]   descending.
    when "h" then open query q for each tt_xstat /* no-lock */ by tt_xstat.stat-ratio descending.
    when "f" then open query q for each tt_xstat /* no-lock */ by tt_xstat.misc1.
    when "p" then open query q for each tt_xstat /* no-lock */ by tt_xstat.misc2.
    when "A" then open query q for each tt_xstat /* no-lock */ by tt_xstat.stat1[x].
    when "R" then open query q for each tt_xstat /* no-lock */ by tt_xstat.stat2[x].
    when "W" then open query q for each tt_xstat /* no-lock */ by tt_xstat.stat3[x].
    when "H" then open query q for each tt_xstat /* no-lock */ by tt_xstat.stat-ratio.
    when "F" then open query q for each tt_xstat /* no-lock */ by tt_xstat.misc1      descending.
    when "P" then open query q for each tt_xstat /* no-lock */ by tt_xstat.misc2      descending.
    when "n" then open query q for each tt_xstat /* no-lock */ by tt_xstat.xname.
    when "N" then open query q for each tt_xstat /* no-lock */ by tt_xstat.xname      descending.
    when "#" then open query q for each tt_xstat /* no-lock */ by tt_xstat.xid.
    when "-" then open query q for each tt_xstat /* no-lock */ by tt_xstat.xid        descending.
    otherwise     open query q for each tt_xstat /* no-lock */ by tt_xstat.stat1[x]   descending.
  end.

/***
  define variable qh as handle no-undo.
  define variable wc as character no-undo initial "for each tt_xstat ".

  qh = query q:handle.
  qh:query-prepare( wc + "by tt_xstat.stat1[5] descending" ).
  qh:query-open().
 ***/

  do while true:

    get next q.

    if not available tt_xstat then leave.

    if tt_xstat.xname = "" then next.

    i = i + 1.

    if i > 25 then leave.

    find xconnect no-lock where xconnect.usrnum = tt_xstat.xid no-error.

    ui-det( support, 1, i, 1, "xid",   string( tt_xstat.xid,                   ">>>>9" )).
    ui-det( support, 1, i, 2, "xname", string( tt_xstat.xname,                  "x(8)" )).
    ui-det( support, 1, i, 3, "misc1", string( tt_xstat.misc1,                  "x(5)" )).
    ui-det( support, 1, i, 4, "misc2", string( tt_xstat.misc2,                  "x(8)" )).
    ui-det( support, 1, i, 5, "stat1", string( ( tt_xstat.stat1[x] / z ), ">>>>>>>>>9" )).
    ui-det( support, 1, i, 6, "stat2", string( ( tt_xstat.stat2[x] / z ), ">>>>>>>>>9" )).
    ui-det( support, 1, i, 7, "stat3", string( ( tt_xstat.stat3[x] / z ), ">>>>>>>>>9" )).
    ui-det( support, 1, i, 8, "stat-ratio", string( tt_xstat.stat-ratio,     ">>9.99%" )).
    if available xconnect then
      ui-det( support, 1, i, 9, "CPUTime", string( xconnect.CPUTime, "hh:mm:ss" )).

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
