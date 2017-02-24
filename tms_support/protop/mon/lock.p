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
 * lock.p
 *
 *
 * Record locks
 *
 *
 * To Do:
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	July 14, 2005
 *
 */

{protop/lib/protop.i}

define variable support as character no-undo initial "Record Locks".

define temp-table tt_lock no-undo
  field usrnum   as integer
  field xrecid   as recid
  field xvalid   as logical
  field xtime    as integer
  field ztime    as integer
  field name     as character
  field tbl      as character
  field locktype as character
  field chain    as integer
  field flags    as character
  index xrecid-idx is unique primary usrnum xrecid.

/* restart
 *
 */

procedure mon-restart:

  empty temp-table tt_lock.

  delete procedure this-procedure.

  return.

end.

/* initialize
 *
 */

procedure mon-init:

  empty temp-table tt_lock.

  /* define labels
   *
   */

  ui-define-label( support, 1, 1, "UsrNum",   "  Usr" ).
  ui-define-label( support, 1, 2, "UserName", "Name        " ).
  ui-define-label( support, 1, 3, "Recid",    "     Recid" ).
  ui-define-label( support, 1, 4, "LockWait", "Duration" ).
  ui-define-label( support, 1, 5, "Table",    "Table               " ).
  ui-define-label( support, 1, 6, "Type",     "Type" ).
  ui-define-label( support, 1, 7, "Chain",    "   Chain" ).
  ui-define-label( support, 1, 8, "Flags",    "Flags" ).

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

  for each tt_lock:
    tt_lock.xvalid = no.
  end.

  /* get a list of record locks
   *
   */

  for each dictdb._Lock no-lock while _Lock-usr <> ?:

    find tt_lock where tt_lock.usrnum = _Lock-usr and tt_lock.xrecid = _Lock-recid no-error.

    if not available tt_lock then
      do:
        find _file no-lock where _file._file-num = _Lock-table no-error.
        create tt_lock.
        assign
          tt_lock.usrnum   = _Lock-usr
          tt_lock.xrecid    = integer( _Lock-recid )
          tt_lock.xtime    = time
          tt_lock.name     = _Lock-name 
          tt_lock.locktype = _Lock-type
          tt_lock.chain    = _Lock-chain
          tt_lock.tbl      = _File._File-name
       .
       release _File.
      end.

    assign
      tt_lock.xvalid = yes
      tt_lock.flags  = _Lock-Flags
      tt_lock.ztime  = time - tt_lock.xtime
    .

  end.

  for each tt_lock where tt_lock.xvalid = no:
    delete tt_lock.
  end.

  define query q for tt_lock.

  do-display( support, 1, 100, 11, 1, "Record Locks" ).

  publish "get-sort-criteria" ( output sort-criteria ).

  case sort-criteria:
    when "r" then open query q for each tt_lock no-lock by tt_lock.xrecid     descending.
    when "w" then open query q for each tt_lock no-lock by tt_lock.ztime      descending.
    when "t" then open query q for each tt_lock no-lock by tt_lock.tbl.
    when "y" then open query q for each tt_lock no-lock by tt_lock.locktype.
    when "c" then open query q for each tt_lock no-lock by tt_lock.chain      descending.
    when "f" then open query q for each tt_lock no-lock by tt_lock.flags      descending.
    when "R" then open query q for each tt_lock no-lock by tt_lock.xrecid.
    when "W" then open query q for each tt_lock no-lock by tt_lock.ztime.
    when "T" then open query q for each tt_lock no-lock by tt_lock.tbl        descending.
    when "Y" then open query q for each tt_lock no-lock by tt_lock.locktype   descending.
    when "C" then open query q for each tt_lock no-lock by tt_lock.chain.
    when "F" then open query q for each tt_lock no-lock by tt_lock.flags.
    when "n" then open query q for each tt_lock no-lock by tt_lock.name.
    when "N" then open query q for each tt_lock no-lock by tt_lock.name       descending.
    when "#" then open query q for each tt_lock no-lock by tt_lock.usrnum.
    when "-" then open query q for each tt_lock no-lock by tt_lock.usrnum     descending.
    otherwise     open query q for each tt_lock no-lock by tt_lock.ztime      descending.
  end.

  do while true:

    get next q.

    if not available tt_lock then leave.

    i = i + 1.

    ui-det( support, 1, i, 1, "UsrNum",   string( tt_lock.usrnum,   ">>>>9" )).
    ui-det( support, 1, i, 2, "UserName", string( tt_lock.name,     "x(12)" )).
    ui-det( support, 1, i, 3, "Recid",    string( tt_lock.xrecid,   ">>>>>>>>>9" )).
    ui-det( support, 1, i, 4, "LockWait", string( tt_lock.ztime,    "hh:mm:ss" )).
    ui-det( support, 1, i, 5, "Table",    string( tt_lock.tbl,      "x(20)" )).
    ui-det( support, 1, i, 6, "Type",     string( tt_lock.locktype, "x(4)" )).
    ui-det( support, 1, i, 7, "Chain",    string( tt_lock.chain,    ">>>>>>>9" )).
    ui-det( support, 1, i, 8, "Flags",    string( tt_lock.flags,    "x(5)" )).

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
