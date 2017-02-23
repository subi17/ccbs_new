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
 * server-det.p
 *
 *
 * Remote client server detail monitoring.
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	September 21, 2003
 *
 */

{../tms_support/protop/lib/protop.i}
{../tms_support/protop/lib/tt_xstat.i}
{../tms_support/protop/lib/tt_servers.i}

define variable support as character no-undo initial "Server Details".

define variable tlist as character no-undo.
define variable xlist as character no-undo.

/* restart
 *
 */

procedure mon-restart:

  empty temp-table tt_servers.
  empty temp-table tt_xstat.

  delete procedure this-procedure.

  return.

end.

/* initialize
 *
 */

procedure mon-init:

  empty temp-table tt_servers.
  empty temp-table tt_xstat.

  /* define labels
   *
   */

  /* variant 1 */

  ui-define-label( support, 1,  1, "SrvNum",  " Srv" ).
  ui-define-label( support, 1,  2, "SrvType", "Type " ).
  ui-define-label( support, 1,  3, "SrvPort", " Port" ).
  ui-define-label( support, 1,  4, "CurrUsr", "Con" ).
  ui-define-label( support, 1,  5, "MaxUsr",  "Max" ).
  ui-define-label( support, 1,  6, "MsgRecv", "Msg Recv" ).
  ui-define-label( support, 1,  7, "MsgSent", "Msg Sent" ).
  ui-define-label( support, 1,  8, "RecRecv", "Rec Recv" ).
  ui-define-label( support, 1,  9, "RecSent", "Rec Sent" ).
  ui-define-label( support, 1, 10, "QryRecv", "Qry Sent" ).
  ui-define-label( support, 1, 11, "TmIntr",  "TimeSlice" ).

  /* variant 2 */

  ui-define-label( support, 2, 1, "UsrNum",   "  Usr" ).
  ui-define-label( support, 2, 2, "UsrName",  "Name           " ).
  ui-define-label( support, 2, 3, "UsrFlag",  "Flags" ).
  ui-define-label( support, 2, 4, "UsrPid",   "PID     " ).
  ui-define-label( support, 2, 5, "UsrDBA",   " DB Access" ).
  ui-define-label( support, 2, 6, "UsrDBR",   " OS Reads " ).
  ui-define-label( support, 2, 7, "UsrDBW",   " OS Writes" ).
  ui-define-label( support, 2, 8, "HR",       "   Hit%" ).

  return.

end.

/* update the sample
 *
 */

procedure mon-update:

  define variable examine-item as integer no-undo.

  if do-update( support ) = no then return.

  publish "get-examine-item" ( output examine-item ).

  run upd-servers( input examine-item ).

  define variable u_name  as character no-undo.
  define variable u_flags as character no-undo.
  define variable u_pid   as character no-undo.

  for each  dictdb._Connect no-lock where _Connect-Server = examine-item,
      first dictdb._UserIO  no-lock where _UserIO-usr = _Connect-usr:

    get-userinfo( input _UserIO-id, output u_name, output u_flags, output u_pid ).

    run update_xstat (
      input _UserIO-usr,
      input u_name,
      input u_flags,
      input u_pid,
      input _UserIO-dbaccess,
      input _UserIO-dbread,
      input _UserIO-dbwrite,
      input 0
    ).

  end.

  run age_xstat.

  define variable i as integer no-undo.
  define variable x as integer no-undo.
  define variable z as integer no-undo.

  define variable sort-criteria as character no-undo case-sensitive.

  publish "get-sort-criteria" ( output sort-criteria ).

  do-display( support, 1, 100, 11, 1, "Server Details" ).
  do-display( support, 2, 100, 11, 1, "Users for Server " +  string( examine-item ) + "").

  do-SumSample( output x, output z ).

  /* no such server?
   *
   */

  find first tt_servers no-lock where tt_servers.srvnum = examine-item no-error.
  if not available tt_servers then return.

  i = 1.	/* general server data	*/

  ui-det( support, 1, i,  1, "SrvNum",  string( tt_servers.srvnum,    ">>>9" )).
  ui-det( support, 1, i,  2, "SrvTyp",  string( tt_servers.srvtyp,    "x(5)" )).
  ui-det( support, 1, i,  3, "SrvPort", string( tt_servers.srvport,  ">>>>9" )).
  ui-det( support, 1, i,  4, "CurrUsr", string( tt_servers.curr-usr,   ">>9" )).
  ui-det( support, 1, i,  5, "MaxUsr",  string( tt_servers.max-usr,    ">>9" )).

  i = 2.	/* cumulative server metrics	*/

  ui-detx( support, 1, i,  1, "SrvNum",  "             Cumulative:", "colspan=5" ).

  ui-det( support, 1, i,  6, "MsgRecv", string( ( tt_servers.msg-recv[4] / xtime ),  ">>>>>>>9" )).
  ui-det( support, 1, i,  7, "MsgSent", string( ( tt_servers.msg-sent[4] / xtime ),  ">>>>>>>9" )).
  ui-det( support, 1, i,  8, "RecRecv", string( ( tt_servers.rec-recv[4] / xtime ),  ">>>>>>>9" )).
  ui-det( support, 1, i,  9, "RecSent", string( ( tt_servers.rec-sent[4] / xtime ),  ">>>>>>>9" )).
  ui-det( support, 1, i, 10, "QryRecv", string( ( tt_servers.qry-recv[4] / xtime ),  ">>>>>>>9" )).
  ui-det( support, 1, i, 11, "TmIntr",  string( ( tt_servers.tm-intr[4]  / xtime ), ">>>>>>>>9" )).

  i = 3.	/* server metrics for the interval	*/

  ui-detx( support, 1, i,  1, "SrvNum",  "               Interval:", "colspan=5" ).

  ui-det( support, 1, i,  6, "MsgRecv", string( ( tt_servers.msg-recv[5] / itime ),  ">>>>>>>9" )).
  ui-det( support, 1, i,  7, "MsgSent", string( ( tt_servers.msg-sent[5] / itime ),  ">>>>>>>9" )).
  ui-det( support, 1, i,  8, "RecRecv", string( ( tt_servers.rec-recv[5] / itime ),  ">>>>>>>9" )).
  ui-det( support, 1, i,  9, "RecSent", string( ( tt_servers.rec-sent[5] / itime ),  ">>>>>>>9" )).
  ui-det( support, 1, i, 10, "QryRecv", string( ( tt_servers.qry-recv[5] / itime ),  ">>>>>>>9" )).
  ui-det( support, 1, i, 11, "TmIntr",  string( ( tt_servers.tm-intr[5]  / itime ), ">>>>>>>>9" )).

  /* display the sessions connected to this server
   *
   */

  define query q for tt_xstat.

  case sort-criteria:
    when "i" then open query q for each tt_xstat no-lock by tt_xstat.stat1[5] descending.
    when "c" then open query q for each tt_xstat no-lock by tt_xstat.stat2[5] descending.
    when "R" then open query q for each tt_xstat no-lock by tt_xstat.stat-ratio descending.
    when "n" then open query q for each tt_xstat no-lock by tt_xstat.xname.
    when "I" then open query q for each tt_xstat no-lock by tt_xstat.stat1[4] descending.
    when "C" then open query q for each tt_xstat no-lock by tt_xstat.stat2[4] descending.
    when "r" then open query q for each tt_xstat no-lock by tt_xstat.stat-ratio descending.
    when "N" then open query q for each tt_xstat no-lock by tt_xstat.xid.
  end.

  do while true:

    get next q.

    if not available tt_xstat then leave.

    i = i + 1.

    ui-det( support, 2, i, 1, "UsrNum",  string( tt_xstat.xid,              ">>>>9" )).
    ui-det( support, 2, i, 2, "UsrName", string( tt_xstat.xname,            "x(15)" )).
    ui-det( support, 2, i, 3, "UsrFlag", string( tt_xstat.misc1,            "x(5)" )).
    ui-det( support, 2, i, 4, "UsrPid",  string( tt_xstat.misc2,            "x(8)" )).
    ui-det( support, 2, i, 5, "UsrDBA",  string( ( tt_xstat.stat1[x] / z ), ">>>>>>>>>9" )).
    ui-det( support, 2, i, 6, "UsrDBR",  string( ( tt_xstat.stat2[x] / z ), ">>>>>>>>>9" )).
    ui-det( support, 2, i, 7, "UsrDBW",  string( ( tt_xstat.stat3[x] / z ), ">>>>>>>>>9" )).
    ui-det( support, 2, i, 8, "HR",      string( tt_xstat.stat-ratio,       ">>9.99%" )).

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
