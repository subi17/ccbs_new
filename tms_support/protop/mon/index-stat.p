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
 * index-stat.p
 *
 *
 * Index IO Statistics.
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	October 28, 2003
 *
 */

{tms_support/protop/lib/protop.i}

define variable support as character no-undo initial "Index Statistics".

{tms_support/protop/lib/tt_index.i}

/* restart
 *
 */

procedure mon-restart:

  empty temp-table tt_index.

  delete procedure this-procedure.

  return.

end.

/* initialize
 *
 */

procedure mon-init:

  empty temp-table tt_index.

  /* define labels
   *
   */

  ui-define-label( support, 1, 1, "IdxNum",    "Idx#" ).
  ui-define-label( support, 1, 2, "IdxName",   "Index Name                              " ).
  ui-define-label( support, 1, 3, "IdxNote",   "  " ).
  ui-define-label( support, 1, 4, "IdxCre",    "Create" ).
  ui-define-label( support, 1, 5, "IdxRd",     "   Read" ).
  ui-define-label( support, 1, 6, "IdxSplit",  "Split" ).
  ui-define-label( support, 1, 7, "IdxDel",    "  Del" ).
  ui-define-label( support, 1, 8, "IdxBlkDel", "BlkD" ).

  return.

end.

/* update
 *
 */

procedure mon-update:

  define variable examine-item as integer no-undo.
  
  if do-update( support ) = no then return.

  for each dictdb._IndexStat no-lock:

    find dictdb._Index no-lock where _Index._Idx-num = _IndexStat-id no-error.
    /* if not available then leave? */
    if available dictdb._Index then run upd-tt_index ( _IndexStat._IndexStat-id ).

  end.

/***
  for each dictdb._File no-lock where _File._File-num > 0 and _File._File-num < 16000,
      each dictdb._Index of _File no-lock:
    run upd-tt_index ( _Index._Idx-num ).
  end.
 ***/

  run age_index.

  define variable i as integer no-undo.
  define variable x as integer no-undo.
  define variable z as integer no-undo.

  define variable sort-criteria as character no-undo case-sensitive.

  do-display( support, 1, 100, 11, 1, "Index Statistics" ).

  do-SumSample( output x, output z ).

  publish "get-sort-criteria" ( output sort-criteria ).

  define query q for tt_index.

  case sort-criteria:
    when "c" then open query q for each tt_index no-lock by tt_index.idx-cre[x]    descending.
    when "r" then open query q for each tt_index no-lock by tt_index.idx-rd[x]     descending.
    when "s" then open query q for each tt_index no-lock by tt_index.idx-split[x]  descending.
    when "d" then open query q for each tt_index no-lock by tt_index.idx-del[x]    descending.
    when "b" then open query q for each tt_index no-lock by tt_index.idx-blkdel[x] descending.
    when "C" then open query q for each tt_index no-lock by tt_index.idx-cre[x].
    when "R" then open query q for each tt_index no-lock by tt_index.idx-rd[x].
    when "S" then open query q for each tt_index no-lock by tt_index.idx-split[x].
    when "D" then open query q for each tt_index no-lock by tt_index.idx-del[x].
    when "B" then open query q for each tt_index no-lock by tt_index.idx-blkdel[x].
    when "n" then open query q for each tt_index no-lock by tt_index.idxname.
    when "N" then open query q for each tt_index no-lock by tt_index.idxname       descending.
    when "#" then open query q for each tt_index no-lock by tt_index.xid.
    when "-" then open query q for each tt_index no-lock by tt_index.xid           descending.
    otherwise     open query q for each tt_index no-lock by tt_index.idx-rd[x]     descending.
  end.

  do while true:

    get next q.

    if not available tt_index then leave.

    i = i + 1.

    ui-det( support, 1, i, 1, "IdxNum",    string( tt_index.xid,                       ">>>9" )).
/*
    ui-det( support, 1, i, 2, "IdxName",   string( tt_index.idxname,                  "x(25)" )).
 */
    ui-det( support, 1, i, 2, "IdxName",   string( tt_index.tblname + "." + tt_index.idxname, "x(40)" )).
    ui-det( support, 1, i, 3, "IdxNote",   string( tt_index.idxnote,                   "x(2)" )).
    ui-det( support, 1, i, 4, "IdxCre",    string( ( tt_index.idx-cre[x]    / z ), ">>>>>9" )).
    ui-det( support, 1, i, 5, "IdxRd",     string( ( tt_index.idx-rd[x]     / z ), ">>>>>>9" )).
    ui-det( support, 1, i, 6, "IdxSplit",  string( ( tt_index.idx-split[x]  / z ), ">>>>9" )).
    ui-det( support, 1, i, 7, "IdxDel",    string( ( tt_index.idx-del[x]    / z ), ">>>>9" )).
    ui-det( support, 1, i, 8, "IdxBlkDel", string( ( tt_index.idx-blkdel[x] / z ), ">>>9" )).

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
