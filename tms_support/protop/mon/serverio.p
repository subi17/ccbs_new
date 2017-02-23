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
 * serverio.p
 *
 *
 * Remote client server IO impact.
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

define variable support as character no-undo initial "Server IO".

define temp-table tt_serverio no-undo
  field xid      as integer
  field xvalid   as logical
  field srvnum   as integer
  field srvtyp   as character
  field srvport  as integer
  field curr-usr as integer
  field max-usr  as integer
  field srv-dba  as integer extent 5
  field srv-dbr  as integer extent 5
  field srv-dbw  as integer extent 5
  field hr       as decimal
  index xid-idx is unique primary xid.

procedure update_serverio:

  define input parameter p_xid      as integer   no-undo.
  define input parameter p_srvnum   as integer   no-undo.
  define input parameter p_srvtyp   as character no-undo.
  define input parameter p_srvport  as integer   no-undo.
  define input parameter p_curr-usr as integer   no-undo.
  define input parameter p_max-usr  as integer   no-undo.
  define input parameter p_this1    as integer   no-undo.
  define input parameter p_this2    as integer   no-undo.
  define input parameter p_this3    as integer   no-undo.

  find tt_serverio exclusive-lock where tt_serverio.xid = p_xid no-error.

  if available tt_serverio then
    do:

      assign
        tt_serverio.xvalid   = yes				/* is this xid active?		*/
        tt_serverio.srvnum   = p_srvnum
        tt_serverio.srvtyp   = p_srvtyp
        tt_serverio.srvport  = p_srvport
        tt_serverio.curr-usr = p_curr-usr
        tt_serverio.max-usr  = p_max-usr
        tt_serverio.srv-dba[3] = p_this1
        tt_serverio.srv-dbr[3] = p_this2
        tt_serverio.srv-dbw[3] = p_this3
      .

      if tt_serverio.srv-dba[3] < tt_serverio.srv-dba[3] then	/* detect reuse of an id (stat rolling backwards...) 	*/
        assign
          {../tms_support/protop/lib/init-xrec.i tt_serverio.srv-dba p_this1}
          {../tms_support/protop/lib/init-xrec.i tt_serverio.srv-dbr p_this2}
          {../tms_support/protop/lib/init-xrec.i tt_serverio.srv-dbw p_this3}
        .

    end.
   else
    do:

      create tt_serverio.
      assign
        tt_serverio.xvalid     = yes
        tt_serverio.xid        = p_xid
        tt_serverio.srvnum     = p_srvnum
        tt_serverio.srvtyp     = p_srvtyp
        tt_serverio.srvport    = p_srvport
        tt_serverio.curr-usr   = p_curr-usr
        tt_serverio.max-usr    = p_max-usr
        {../tms_support/protop/lib/init-xrec.i tt_serverio.srv-dba p_this1}
        {../tms_support/protop/lib/init-xrec.i tt_serverio.srv-dbr p_this2}
        {../tms_support/protop/lib/init-xrec.i tt_serverio.srv-dbw p_this3}
      .

    end.

  return.

end.

procedure age_serverio:

  define variable x as integer no-undo.
  define variable z as integer no-undo.

  do-SumSample( output x, output z ).

  for each tt_serverio exclusive-lock:

    if tt_serverio.xvalid = no then
      delete tt_serverio.
     else
      assign
        tt_serverio.xvalid = no
        {../tms_support/protop/lib/upd-xrec.i tt_serverio.srv-dba tt_serverio.srv-dba[3]}
        {../tms_support/protop/lib/upd-xrec.i tt_serverio.srv-dbr tt_serverio.srv-dbr[3]}
        {../tms_support/protop/lib/upd-xrec.i tt_serverio.srv-dbw tt_serverio.srv-dbw[3]}
        tt_serverio.hr  = 100 * (( tt_serverio.srv-dba[x] - tt_serverio.srv-dbr[x] ) / tt_serverio.srv-dba[x] )
        tt_serverio.hr  = ( if tt_serverio.hr = ? then 0 else tt_serverio.hr )
      .

  end.

  return.

end.

/* restart
 *
 */

procedure mon-restart:

  empty temp-table tt_serverio.

  delete procedure this-procedure.

  return.

end.

/* initialize
 *
 */

procedure mon-init:

  empty temp-table tt_serverio.

  /* define labels
   *
   */

  ui-define-label( support, 1, 1, "SrvNum",  " Srv" ).
  ui-define-label( support, 1, 2, "SrvType", "Type " ).
  ui-define-label( support, 1, 3, "SrvPort", " Port" ).
  ui-define-label( support, 1, 4, "CurrUsr", "Con" ).
  ui-define-label( support, 1, 5, "MaxUsr",  "Max" ).
  ui-define-label( support, 1, 6, "SrvDBA",  " DB Access" ).
  ui-define-label( support, 1, 7, "SrvDBR",  "  OS Reads" ).
  ui-define-label( support, 1, 8, "SrvDBW",  " OS Writes" ).
  ui-define-label( support, 1, 9, "SrvHR",   "   Hit%" ).

  return.

end.

/* update
 *
 */

procedure mon-update:

  define variable sort-criteria as character no-undo case-sensitive.

  define variable x-dba as integer no-undo.
  define variable x-dbr as integer no-undo.
  define variable x-dbw as integer no-undo.

  if do-update( support ) = no then return.

  for each dictdb._Servers no-lock,
      first dictdb._ActServer where dictdb._ActServer._Server-Id = dictdb._Servers._Server-Id:

    assign
      x-dba = 0
      x-dbr = 0
      x-dbw = 0
    .

    for each  dictdb._Connect no-lock where _Connect-Server = _Servers._Server-Num,
        first dictdb._UserIO  no-lock where _UserIO-usr = _Connect-usr:
      assign
        x-dba = x-dba + _UserIO-DBAccess
        x-dbr = x-dbr + _UserIO-DBRead
        x-dbw = x-dbw + _UserIO-DBWrite
      .
    end.

    run update_serverio(
      input _Servers._Server-Id,
      input _Servers._Server-Num,
      input _Servers._Server-Type,
      input _Servers._Server-Port,
      input _Servers._Server-CurrUsers,
      input _Servers._Server-MaxUsers,
      input x-dba,
      input x-dbr,
      input x-dbw
    ).

  end.

  run age_serverio.

  define variable i as integer no-undo.
  define variable x as integer no-undo.
  define variable z as integer no-undo.

  do-display( support, 1, 100, 11, 1, "Server IO" ).

  do-SumSample( output x, output z ).

  publish "get-sort-criteria" ( output sort-criteria ).

  define query q for tt_serverio.

  case sort-criteria:

    when "t" then open query q for each tt_serverio no-lock by tt_serverio.srvtyp.
    when "p" then open query q for each tt_serverio no-lock by tt_serverio.srvport.
    when "c" then open query q for each tt_serverio no-lock by tt_serverio.curr-usr   descending.
    when "x" then open query q for each tt_serverio no-lock by tt_serverio.max-usr    descending.
    when "a" then open query q for each tt_serverio no-lock by tt_serverio.srv-dba[x] descending.
    when "r" then open query q for each tt_serverio no-lock by tt_serverio.srv-dbr[x] descending.
    when "w" then open query q for each tt_serverio no-lock by tt_serverio.srv-dbw[x] descending.
    when "h" then open query q for each tt_serverio no-lock by tt_serverio.hr         descending.

    when "T" then open query q for each tt_serverio no-lock by tt_serverio.srvtyp  descending.
    when "P" then open query q for each tt_serverio no-lock by tt_serverio.srvport descending.
    when "C" then open query q for each tt_serverio no-lock by tt_serverio.curr-usr.
    when "X" then open query q for each tt_serverio no-lock by tt_serverio.max-usr.
    when "A" then open query q for each tt_serverio no-lock by tt_serverio.srv-dba[x].
    when "R" then open query q for each tt_serverio no-lock by tt_serverio.srv-dbr[x].
    when "W" then open query q for each tt_serverio no-lock by tt_serverio.srv-dbw[x].
    when "H" then open query q for each tt_serverio no-lock by tt_serverio.hr.

    when "#" then open query q for each tt_serverio no-lock by tt_serverio.srvnum.
    when "-" then open query q for each tt_serverio no-lock by tt_serverio.srvnum     descending.
    otherwise     open query q for each tt_serverio no-lock by tt_serverio.srv-dba[x] descending.

  end.

  do while true:

    get next q.

    if not available tt_serverio then leave.

    i = i + 1.

    ui-det( support, 1, i, 1, "SrvNum",  string( tt_serverio.srvnum,   ">>>9" )).
    ui-det( support, 1, i, 2, "SrvType", string( tt_serverio.srvtyp,   "x(5)" )).
    ui-det( support, 1, i, 3, "SrvPort", string( tt_serverio.srvport, ">>>>9" )).
    ui-det( support, 1, i, 4, "CurrUsr", string( tt_serverio.curr-usr,  ">>9" )).
    ui-det( support, 1, i, 5, "MaxUsr",  string( tt_serverio.max-usr,   ">>9" )).
    ui-det( support, 1, i, 6, "SrvDBA",  string( ( tt_serverio.srv-dba[x] / z ), ">>>>>>>>>9" )).
    ui-det( support, 1, i, 7, "SrvDBR",  string( ( tt_serverio.srv-dbr[x] / z ), ">>>>>>>>>9" )).
    ui-det( support, 1, i, 8, "SrvDBW",  string( ( tt_serverio.srv-dbw[x] / z ), ">>>>>>>>>9" )).
    ui-det( support, 1, i, 9, "SrvHR",   string( tt_serverio.hr,    ">>9.99%" )).

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
