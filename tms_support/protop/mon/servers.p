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
 * servers.p
 *
 *
 * Remote client servers monitoring.
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	September 21, 2003
 *
 */

{lib/protop.i}
{lib/tt_servers.i}

define variable support as character no-undo initial "Servers".

/* restart
 *
 */

procedure mon-restart:

  empty temp-table tt_servers.

  delete procedure this-procedure.

  return.

end.

/* initialize
 *
 */

procedure mon-init:

  empty temp-table tt_servers.

  /* define labels
   *
   */

  ui-define-label( support, 1,  1, "SrvNum",  " Srv" ).
  ui-define-label( support, 1,  2, "SrvType", "Type " ).
  ui-define-label( support, 1,  3, "SrvPort", " Port" ).
  ui-define-label( support, 1,  4, "CurrUsr", "Con" ).
  ui-define-label( support, 1,  5, "MaxUsr",  "Max" ).
  ui-define-label( support, 1,  6, "MsgRecv", " MRecv" ).
  ui-define-label( support, 1,  7, "MsgSent", " MSent" ).
  ui-define-label( support, 1,  8, "RecRecv", " RRecv" ).
  ui-define-label( support, 1,  9, "RecSent", " RSent" ).
  ui-define-label( support, 1, 10, "QryRecv", " QSent" ).
  ui-define-label( support, 1, 11, "TmIntr",  " Slice" ).

  return.

end.

/* update
 *
 */

procedure mon-update:

  define variable sort-criteria as character no-undo case-sensitive.

  if do-update( support ) = no then return.

  for each dictdb._Servers no-lock,
      first dictdb._ActServer where dictdb._ActServer._Server-Id = dictdb._Servers._Server-Id:

    run upd-servers( input dictdb._Servers._Server-Id ).

  end.

  define variable i as integer no-undo.
  define variable x as integer no-undo.
  define variable z as integer no-undo.

  do-display( support, 1, 100, 11, 1, "Servers" ).

  do-SumSample( output x, output z ).

  publish "get-sort-criteria" ( output sort-criteria ).

  define query q for tt_servers.

  open query q for each tt_servers no-lock.

  case sort-criteria:

    when "t"  then open query q for each tt_servers no-lock by tt_servers.srvtyp.
    when "p"  then open query q for each tt_servers no-lock by tt_servers.srvport     descending.
    when "c"  then open query q for each tt_servers no-lock by tt_servers.curr-usr    descending.
    when "x"  then open query q for each tt_servers no-lock by tt_servers.max-usr     descending.
    when "mr" then open query q for each tt_servers no-lock by tt_servers.msg-recv[x] descending.
    when "ms" then open query q for each tt_servers no-lock by tt_servers.msg-sent[x] descending.
    when "rr" then open query q for each tt_servers no-lock by tt_servers.rec-recv[x] descending.
    when "rs" then open query q for each tt_servers no-lock by tt_servers.rec-sent[x] descending.
    when "qr" then open query q for each tt_servers no-lock by tt_servers.qry-recv[x] descending.
    when "s"  then open query q for each tt_servers no-lock by tt_servers.tm-intr[x]  descending.

    when "T"  then open query q for each tt_servers no-lock by tt_servers.srvtyp      descending.
    when "P"  then open query q for each tt_servers no-lock by tt_servers.srvport.
    when "C"  then open query q for each tt_servers no-lock by tt_servers.curr-usr.
    when "X"  then open query q for each tt_servers no-lock by tt_servers.max-usr.
    when "MR" then open query q for each tt_servers no-lock by tt_servers.msg-recv[x].
    when "MS" then open query q for each tt_servers no-lock by tt_servers.msg-sent[x].
    when "RR" then open query q for each tt_servers no-lock by tt_servers.rec-recv[x].
    when "RS" then open query q for each tt_servers no-lock by tt_servers.rec-sent[x].
    when "QR" then open query q for each tt_servers no-lock by tt_servers.qry-recv[x].
    when "S"  then open query q for each tt_servers no-lock by tt_servers.tm-intr[x].

    when "#"  then open query q for each tt_servers no-lock by tt_servers.srvnum.
    when "-"  then open query q for each tt_servers no-lock by tt_servers.srvnum      descending.
    otherwise      open query q for each tt_servers no-lock by tt_servers.msg-recv[x] descending.

  end.

  do while true:

    get next q.

    if not available tt_servers then leave.

    i = i + 1.

    ui-det( support, 1, i,  1, "SrvNum",  string( tt_servers.srvnum,        ">>>9" )).
    ui-det( support, 1, i,  2, "SrvTyp",  string( tt_servers.srvtyp,        "x(5)" )).
    ui-det( support, 1, i,  3, "SrvPort", string( tt_servers.srvport,      ">>>>9" )).
    ui-det( support, 1, i,  4, "CurrUsr", string( tt_servers.curr-usr,       ">>9" )).
    ui-det( support, 1, i,  5, "MaxUsr",  string( tt_servers.max-usr,        ">>9" )).
    ui-det( support, 1, i,  6, "MsgRecv", string( ( tt_servers.msg-recv[x] / z ), ">>>>>9" )).
    ui-det( support, 1, i,  7, "MsgSent", string( ( tt_servers.msg-sent[x] / z ), ">>>>>9" )).
    ui-det( support, 1, i,  8, "RecRecv", string( ( tt_servers.rec-recv[x] / z ), ">>>>>9" )).
    ui-det( support, 1, i,  9, "RecSent", string( ( tt_servers.rec-sent[x] / z ), ">>>>>9" )).
    ui-det( support, 1, i, 10, "QryRecv", string( ( tt_servers.qry-recv[x] / z ), ">>>>>9" )).
    ui-det( support, 1, i, 11, "TmIntr",  string( ( tt_servers.tm-intr[x]  / z ), ">>>>>9" )).

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
