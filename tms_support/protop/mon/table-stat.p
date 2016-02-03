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
 * table-stat.p
 *
 *
 * Table IO Statistics.
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	October 27, 2003
 *
 */

{tms_support/protop/lib/protop.i}

define variable support as character no-undo initial "Table Statistics".

{tms_support/protop/lib/tt_table.i}

/* restart
 *
 */

procedure mon-restart:

  empty temp-table tt_table.

  delete procedure this-procedure.

  return.

end.

/* initialize
 *
 */

procedure mon-init:

  empty temp-table tt_table.

  /* define labels
   *
   */

  ui-define-label( support, 1, 1, "TblNum",   "Tbl#" ).
  ui-define-label( support, 1, 2, "TblName",  "Table Name          " ).
  ui-define-label( support, 1, 3, "Create",  "   Create" ).
  ui-define-label( support, 1, 4, "Read",    "     Read" ).
  ui-define-label( support, 1, 5, "Update",  "   Update" ).
  ui-define-label( support, 1, 6, "Delete",  "   Delete" ).
  ui-define-label( support, 1, 7, "TempTable", "TempTable" ).

  return.

end.

/* update
 *
 */

procedure mon-update:

  define variable examine-item as integer no-undo.
  
  if do-update( support ) = no then return.

  for each dictdb._TableStat no-lock:

    find dictdb._File no-lock where _File._File-num = _TableStat-id no-error.
    /* if not available then leave? */
    if available dictdb._File then run upd-tt_table ( _TableStat._TableStat-id ).

  end.

  run age_table.

  define variable i as integer no-undo.
  define variable x as integer no-undo.
  define variable z as integer no-undo.

  define variable sort-criteria as character no-undo case-sensitive.

  do-display( support, 1, 100, 11, 1, "Table Statistics" ).

  do-SumSample( output x, output z ).

  publish "get-sort-criteria" ( output sort-criteria ).

  define query q for tt_table.

  case sort-criteria:
    when "c" then open query q for each tt_table no-lock by tt_table.tbl-cre[x] descending.
    when "r" then open query q for each tt_table no-lock by tt_table.tbl-rd[x]  descending.
    when "u" then open query q for each tt_table no-lock by tt_table.tbl-upd[x] descending.
    when "d" then open query q for each tt_table no-lock by tt_table.tbl-del[x] descending.
    when "C" then open query q for each tt_table no-lock by tt_table.tbl-cre[x].
    when "R" then open query q for each tt_table no-lock by tt_table.tbl-rd[x].
    when "U" then open query q for each tt_table no-lock by tt_table.tbl-upd[x].
    when "D" then open query q for each tt_table no-lock by tt_table.tbl-del[x].
    when "n" then open query q for each tt_table no-lock by tt_table.tblname.
    when "N" then open query q for each tt_table no-lock by tt_table.tblname    descending.
    when "#" then open query q for each tt_table no-lock by tt_table.xid.
    when "-" then open query q for each tt_table no-lock by tt_table.xid        descending.
    otherwise     open query q for each tt_table no-lock by tt_table.tbl-rd[x]  descending.
  end.

  do while true:

    get next q.

    if not available tt_table then leave.

    i = i + 1.

    ui-det( support, 1, i, 1, "TblNum",  string( tt_table.xid,                     ">>>9" )).
    ui-det( support, 1, i, 2, "TblName", string( tt_table.tblname,                "x(20)" )).
    ui-det( support, 1, i, 3, "Create",  string( ( tt_table.tbl-cre[x] / z ), ">>>>>>>>9" )).
    ui-det( support, 1, i, 4, "Read",    string( ( tt_table.tbl-rd[x]  / z ), ">>>>>>>>9" )).
    ui-det( support, 1, i, 5, "Update",  string( ( tt_table.tbl-upd[x] / z ), ">>>>>>>>9" )).
    ui-det( support, 1, i, 6, "Delete",  string( ( tt_table.tbl-del[x] / z ), ">>>>>>>>9" )).
    ui-det( support, 1, i, 7, "TempTable", tt_table.ttbl ).

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
