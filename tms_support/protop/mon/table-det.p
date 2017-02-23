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
 * table-det.p
 *
 *
 * Drill down into table access details.
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	October 13, 2003
 *
 */

{../tms_support/protop/lib/protop.i}

define variable support as character no-undo initial "Table Details".

{../tms_support/protop/lib/tt_table.i}
{../tms_support/protop/lib/tt_index.i}

/* restart
 *
 */

procedure mon-restart:

  empty temp-table tt_table.
  empty temp-table tt_index.

  delete procedure this-procedure.

  return.

end.

/* initialize
 *
 */

procedure mon-init:

  empty temp-table tt_table.
  empty temp-table tt_index.

  /* define labels
   *
   */

  /* variant 1 */

  ui-define-label( support, 1, 1, "TblNum",  "Tbl#" ).
  ui-define-label( support, 1, 2, "TblName", "Table Name          " ).
  ui-define-label( support, 1, 3, "Oper",    "Operation" ).
  ui-define-label( support, 1, 4, "Accum",   "Cumulative" ).
  ui-define-label( support, 1, 5, "Inter",   "  Interval" ).

  /* variant 2 */

  ui-define-label( support, 2, 1, "IdxNum",    "Idx#" ).
  ui-define-label( support, 2, 2, "IdxName",   "Index Name               " ).
  ui-define-label( support, 2, 3, "IdxNote",   "  " ).
  ui-define-label( support, 2, 4, "IdxCre",    "  Create" ).
  ui-define-label( support, 2, 5, "IdxRd",     "    Read" ).
  ui-define-label( support, 2, 6, "IdxSplit",  "   Split" ).
  ui-define-label( support, 2, 7, "IdxDel",    "  Delete" ).
  ui-define-label( support, 2, 8, "IdxBlkDel", "  BlkDel" ).

  return.

end.

/* update the sample
 *
 */

procedure mon-update:

  define variable examine-item as integer no-undo.

  if do-update( support ) = no then return.

  publish "get-examine-item" ( output examine-item ).

  find first tt_table no-lock no-error.
  if available tt_table and tt_table.xid <> examine-item then
    do:
      empty temp-table tt_table.
      empty temp-table tt_index.
    end.

  run upd-tt_table ( examine-item ).

  find dictdb._File no-lock where _File._File-num = examine-item no-error.

  for each _Index no-lock of _File:
    run upd-tt_index ( _Index._Idx-num ).
  end.

  run age_table.
  run age_index.

  define variable i as integer no-undo.
  define variable x as integer no-undo.
  define variable z as integer no-undo.

  define variable sort-criteria as character no-undo case-sensitive.

  publish "get-sort-criteria" ( output sort-criteria ).

  /* no such table?
   *
   */

  find first tt_table no-lock where tt_table.xid = examine-item no-error.
  if not available tt_table then
    do:
      do-display( support, 1, 100, 11, 1, "Table Details for UNKNOWN table" ).
      do-display( support, 2, 100, 11, 1, "Indexes for UNKNOWN table" ).
      return.
    end.
   else
    do:
      do-display( support, 1, 100, 11, 1, "Table Details for " + tt_table.tblname ).
      do-display( support, 2, 100, 11, 1, "Indexes for " + tt_table.tblname ).
    end.

  do-SumSample( output x, output z ).

  i = 1.	/* general table data & create details	*/

  ui-det( support, 1, i,  1, "TblNum",  string( tt_table.xid,      ">>>9" )).
  ui-det( support, 1, i,  2, "TblName", string( tt_table.tblname, "x(20)" )).
  ui-det( support, 1, i,  3, "Oper",    "  Create:" ).
  ui-det( support, 1, i,  4, "Accum",   string( ( tt_table.tbl-cre[4] / xtime ), ">>>>>>>>>9" )).
  ui-det( support, 1, i,  5, "Inter",   string( ( tt_table.tbl-cre[5] / itime ), ">>>>>>>>>9" )).

  i = 2.	/* read metrics		*/

  ui-det( support, 1, i,  1, "TblNum",  "    " ).
  ui-det( support, 1, i,  2, "TblName", fill( " ", 20 )).
  ui-det( support, 1, i,  3, "Oper",    "    Read:" ).
  ui-det( support, 1, i,  4, "Accum",  string( ( tt_table.tbl-rd[4] / xtime ), ">>>>>>>>>9" )).
  ui-det( support, 1, i,  5, "Inter",  string( ( tt_table.tbl-rd[5] / itime ), ">>>>>>>>>9" )).

  i = 3.	/* update metrics	*/

  ui-det( support, 1, i,  1, "TblNum",  "    " ).
  ui-det( support, 1, i,  2, "TblName", fill( " ", 20 )).
  ui-det( support, 1, i,  3, "Oper",  "  Update:" ).
  ui-det( support, 1, i,  4, "Accum",  string( ( tt_table.tbl-upd[4] / xtime ), ">>>>>>>>>9" )).
  ui-det( support, 1, i,  5, "Inter",  string( ( tt_table.tbl-upd[5] / itime ), ">>>>>>>>>9" )).

  i = 4.	/* delete metrics	*/

  ui-det( support, 1, i,  1, "TblNum",  "    " ).
  ui-det( support, 1, i,  2, "TblName", fill( " ", 20 )).
  ui-det( support, 1, i,  3, "Oper",  "  Delete:" ).
  ui-det( support, 1, i,  4, "Accum",  string( ( tt_table.tbl-del[4] / xtime ), ">>>>>>>>>9" )).
  ui-det( support, 1, i,  5, "Inter",  string( ( tt_table.tbl-del[5] / itime ), ">>>>>>>>>9" )).

  /* display the sessions connected to this server
   *
   */

  define query q for tt_index.

  open query q for each tt_index no-lock.

  do while true:

    get next q.

    if not available tt_index then leave.

    i = i + 1.

    ui-det( support, 2, i,  1, "IdxNum",    string( tt_index.xid,                       ">>>9" )).
    ui-det( support, 2, i,  2, "IdxName",   string( tt_index.idxname,                  "x(25)" )).
    ui-det( support, 2, i,  3, "IdxNote",   string( tt_index.idxnote,                   "x(2)" )).
    ui-det( support, 2, i,  4, "IdxCre",    string( ( tt_index.idx-cre[x]    / z ), ">>>>>>>9" )).
    ui-det( support, 2, i,  5, "IdxRd",     string( ( tt_index.idx-rd[x]     / z ), ">>>>>>>9" )).
    ui-det( support, 2, i,  6, "IdxSplit",  string( ( tt_index.idx-split[x]  / z ), ">>>>>>>9" )).
    ui-det( support, 2, i,  7, "IdxDel",    string( ( tt_index.idx-del[x]    / z ), ">>>>>>>9" )).
    ui-det( support, 2, i,  8, "IdxBlkDel", string( ( tt_index.idx-blkdel[x] / z ), ">>>>>>>9" )).

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
