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
 * blocked.p
 *
 *
 * Blocked clients.
 *
 *
 * To Do:
 *
 *	Handle other sorts of blocked clients
 *	Ought to break up "blkstr" into it's components
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	September 16, 2003
 *
 */

{protop/lib/protop.i}

define variable support as character no-undo initial "Blocked".

define temp-table blocked no-undo
  field xid      as integer
  field xvalid   as logical
  field usrnum   as integer
  field xname    as character
  field xtime    as integer
  field ztime    as integer
  field bstr     as character
  index usrnum-idx is unique primary usrnum.

/* restart
 *
 */

procedure mon-restart:

  empty temp-table blocked.

  delete procedure this-procedure.

  return.

end.

/* initialize
 *
 */

procedure mon-init:

  empty temp-table blocked.

  /* define labels
   *
   */

  ui-define-label( support, 1, 1, "UsrNum",    "  Usr" ).
  ui-define-label( support, 1, 2, "UserName",  "Name        " ).
  ui-define-label( support, 1, 3, "BlockTime", " Waiting" ).
  ui-define-label( support, 1, 4, "Note",      "Note                                                " ).

  return.

end.

/* update
 *
 */

procedure mon-update:

  define variable i as integer   no-undo.
  define variable c as character no-undo.
  define variable t as logical   no-undo.

  define variable sort-criteria as character no-undo case-sensitive.

  if do-update( support ) = no then return.

/***
  empty temp-table blocked.	/* Yes, empty it every time -- this one doesn't make sense to persist...	*/
 ***/

  for each blocked:
    blocked.xvalid = no.
  end.

  /* get a list of blocked sessions
   *
   */

  for each dictdb._Connect no-lock where
     _Connect-usr <> ? and
     _Connect-wait <> " -- ":	/* not perfect -- but works for record locks  */


/***
    create blocked.
    assign
      blocked.xid      = _Connect-id
      blocked.usrnum   = _Connect-usr
      blocked.xname    = _Connect-name
   .
 ***/

    find blocked where blocked.usrnum = _Connect-usr no-error.

    if not available blocked then
      do:
        create blocked.
        assign
          blocked.xid      = _Connect-id
          blocked.usrnum   = _Connect-usr
          blocked.xname    = _Connect-name
          blocked.xtime    = time
       .
      end.

    assign
      blocked.xvalid = yes
      blocked.ztime  = time - blocked.xtime
      blocked.bstr   = get-blocked( _Connect-usr ).		/* build a description of the reason	*/
    .

  end.

  for each blocked where blocked.xvalid = no:
    delete blocked.
  end.

  do-display( support, 1, 100, 11, 1, "Blocked Sessions" ).

  define query q for blocked.
  open query q for each blocked no-lock.

  publish "get-sort-criteria" ( output sort-criteria ).

  case sort-criteria:
    when "w" then open query q for each blocked no-lock by blocked.ztime      descending.
    when "W" then open query q for each blocked no-lock by blocked.ztime.
    when "n" then open query q for each blocked no-lock by blocked.xname.
    when "N" then open query q for each blocked no-lock by blocked.xname       descending.
    when "#" then open query q for each blocked no-lock by blocked.usrnum.
    when "-" then open query q for each blocked no-lock by blocked.usrnum     descending.
    otherwise     open query q for each blocked no-lock by blocked.ztime      descending.
  end.

  do while true:

    get next q.

    if not available blocked then leave.

    i = i + 1.

    ui-det( support, 1, i, 1, "UsrNum",    string( blocked.usrnum, ">>>>9" )).
    ui-det( support, 1, i, 2, "UserName",  string( blocked.xname,  "x(12)" )).
    ui-det( support, 1, i, 3, "BlockTime", string( blocked.ztime,  "hh:mm:ss" )).
    ui-det( support, 1, i, 4, "Note",      string( blocked.bstr,   "x(52)" )).

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
