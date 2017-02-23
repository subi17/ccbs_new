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
 * activetrx.p
 *
 *
 * Active trx monitoring (all TRX *except* "Allocated").
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	August 28, 2003
 *
 */

{../tms_support/protop/lib/protop.i}

define variable support as character no-undo initial "Active TRX".

define temp-table tt_trx no-undo
  field xid      as integer
  field xtime    as character
  field usrnum   as integer
  field xname    as character
  field trx-num  as integer
  field trx-rl   as integer
  field trx-st   as character
  field duration as integer
  field trx-wait as character
  index xid-idx is unique primary xid.

/* restart
 *
 */

procedure mon-restart:

  empty temp-table tt_trx.

  delete procedure this-procedure.

  return.

end.

/* initialize
 *
 */

procedure mon-init:

  empty temp-table tt_trx.

  /* define labels
   *
   */

  ui-define-label( support, 1, 1, "UsrNum",      "  Usr" ).
  ui-define-label( support, 1, 2, "UserName",    "Name        " ).
  ui-define-label( support, 1, 3, "TrxNum",      "   TRX Num" ).
  ui-define-label( support, 1, 4, "TrxRL",       "BI Clstr" ).
  ui-define-label( support, 1, 5, "TrxTime",     "Start   " ).
  ui-define-label( support, 1, 6, "TrxState",    "Trx Stat" ).
  ui-define-label( support, 1, 7, "TrxDuration", "Duration" ).
  ui-define-label( support, 1, 8, "TrWait",      "Wait           " ).

  return.

end.

/* update
 *
 */

procedure mon-update:

  define variable sort-criteria as character no-undo case-sensitive.

  if do-update( support ) = no then return.

  empty temp-table tt_trx.	/* Yes, empty it every time -- this is a snapshot	*/

  /* Get open transaction data from _Trans VST
   *
   */

  for each dictdb._Trans no-lock where _Trans-usrnum <> ? and _Trans-state <> "allocated":

    /* get some info about the session that started the trx
     *
     */

    find dictdb._Connect no-lock where _Connect-usr = _Trans-usrnum no-error.

    create tt_trx.
    assign
      tt_trx.xid      = _Trans-id
      tt_trx.xtime    = ( if _Trans-txtime <> ? then substring( _Trans-txtime, 12 ) else "" )
      tt_trx.usrnum   = _Trans-usrnum
      tt_trx.xname    = ( if available _Connect then _Connect-name else "" )
      tt_trx.trx-num  = _Trans-num
      tt_trx.trx-rl   = _Trans-counter
      tt_trx.trx-st   = _Trans-state
      tt_trx.duration = ( if _Trans-duration <> ? then _Trans-duration else 0 )
      tt_trx.trx-wait = ( if available _Connect then ( string( _Connect-wait ) + " " + string( _Connect-wait1 )) else "" )
    .

  end.

  define variable i as integer no-undo.

  do-display( support, 1, 100, 11, 1, 'Active Transactions  --  (Does NOT include "Allocated TRX")' ).

  publish "get-sort-criteria" ( output sort-criteria ).

  define query q for tt_trx.

  case sort-criteria:
    when "x" then open query q for each tt_trx no-lock by tt_trx.trx-num   descending.
    when "b" then open query q for each tt_trx no-lock by tt_trx.trx-rl    descending.
    when "t" then open query q for each tt_trx no-lock by tt_trx.xtime     descending.
    when "s" then open query q for each tt_trx no-lock by tt_trx.trx-st.
    when "d" then open query q for each tt_trx no-lock by tt_trx.duration  descending.
    when "w" then open query q for each tt_trx no-lock by tt_trx.trx-wait  descending.
    when "X" then open query q for each tt_trx no-lock by tt_trx.trx-num.
    when "B" then open query q for each tt_trx no-lock by tt_trx.trx-rl.
    when "T" then open query q for each tt_trx no-lock by tt_trx.xtime.
    when "S" then open query q for each tt_trx no-lock by tt_trx.trx-st    descending.
    when "D" then open query q for each tt_trx no-lock by tt_trx.duration.
    when "W" then open query q for each tt_trx no-lock by tt_trx.trx-wait.
    when "n" then open query q for each tt_trx no-lock by tt_trx.xname.
    when "N" then open query q for each tt_trx no-lock by tt_trx.xname     descending.
    when "#" then open query q for each tt_trx no-lock by tt_trx.xid.
    when "-" then open query q for each tt_trx no-lock by tt_trx.xid       descending.
    otherwise     open query q for each tt_trx no-lock by tt_trx.duration  descending.
  end.

  do while true:

    get next q.

    if not available tt_trx then leave.

    i = i + 1.

    ui-det( support, 1, i, 1, "UsrNum",      string( tt_trx.usrnum,   ">>>>9" )).
    ui-det( support, 1, i, 2, "UserName",    string( tt_trx.xname,    "x(12)" )).
    ui-det( support, 1, i, 3, "TrxNum",      string( tt_trx.trx-num,  ">>>>>>>>>9" )).
    ui-det( support, 1, i, 4, "TrxRL",       string( tt_trx.trx-rl,   ">>>>>>>9" )).
    ui-det( support, 1, i, 5, "TrxTime",     string( tt_trx.xtime,    "x(8)" )).
    ui-det( support, 1, i, 6, "TrxState",    string( tt_trx.trx-st,   "x(8)" )).
    ui-det( support, 1, i, 7, "TrxDuration", string( tt_trx.duration, "hh:mm:ss" )).
    ui-det( support, 1, i, 8, "TrWait",      string( tt_trx.trx-wait, "x(15)" )).

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
