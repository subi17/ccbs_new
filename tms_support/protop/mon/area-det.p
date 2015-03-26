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
 * area-det.p
 *
 *
 * Storage Area Details.
 *
 *
 * To Do:
 *
 *	Area specific notes:
 *	  Fixed/Variable extent indicator
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	October 27, 2003
 *
 */

{lib/protop.i}

define variable support as character no-undo initial "Area Details".

define temp-table tt_area no-undo
  field xid     as integer
  field xname   as character
  field btot    as integer
  field bhiw    as integer
  field blkf    as integer
  field buse    as decimal
  field rpb     as integer
  field xnote   as character
  index xid-idx is unique primary xid.

{lib/tt_table.i}
{lib/tt_index.i}

/* restart
 *
 */

procedure mon-restart:

  empty temp-table tt_area.
  empty temp-table tt_table.
  empty temp-table tt_index.

  delete procedure this-procedure.

  return.

end.

/* initialize
 *
 */

procedure mon-init:

  empty temp-table tt_area.
  empty temp-table tt_table.
  empty temp-table tt_index.

  /* define labels
   *
   */

  /* variant 1 */

  ui-define-label( support, 1, 1, "AreaId",      "Area" ).
  ui-define-label( support, 1, 2, "AreaName",    "Area Name                " ).
  ui-define-label( support, 1, 3, "AreaBlocks",  "  Blocks" ).
  ui-define-label( support, 1, 4, "AreaHiWater", "Hi Water" ).
  ui-define-label( support, 1, 5, "AreaFree",    "    Free" ).
  ui-define-label( support, 1, 6, "AreaPctUsed", "  %Used" ).
  ui-define-label( support, 1, 7, "AreaRPB",     "RPB" ).
  ui-define-label( support, 1, 8, "AreaNote",    "Note      " ).

  /* variant 2 */

  ui-define-label( support, 2, 1, "TblNum",  "Tbl#" ).
  ui-define-label( support, 2, 2, "TblName", "Table Name                " ).

  ui-define-label( support, 2, 3, "Create",  "   Create" ).
  ui-define-label( support, 2, 4, "Read",    "     Read" ).
  ui-define-label( support, 2, 5, "Update",  "   Update" ).
  ui-define-label( support, 2, 6, "Delete",  "   Delete" ).

  /* variant 3 */

  ui-define-label( support, 3, 1, "IdxNum",  "Idx#" ).
  ui-define-label( support, 3, 2, "IdxName", "Index Name             " ).
  ui-define-label( support, 3, 3, "IdxNote", "  " ).

  ui-define-label( support, 3, 4, "IdxCre",    "   Create" ).
  ui-define-label( support, 3, 5, "IdxRd",     "     Read" ).
  ui-define-label( support, 3, 6, "IdxSplit",  "    Split" ).
  ui-define-label( support, 3, 7, "IdxDel",    "   Delete" ).
  ui-define-label( support, 3, 8, "IdxBlkDel", "  BlkDel" ).

  /* define other display attributes
   *
   */

  ui-define-attr( support, 1, 2, "AreaName", "align", "left" ).

  return.

end.

/* update
 *
 */

procedure mon-update:

  define variable bfree  as decimal no-undo.
  define variable used   as decimal no-undo.
  define variable seqnum as integer no-undo.
  define variable so_idx as integer no-undo.
  define variable so_tbl as integer no-undo.
  if do-update( support ) = no then return.

  define variable examine-item as integer no-undo.

  publish "get-examine-item" ( output examine-item ).

  empty temp-table tt_area.

  /* Join the _AreaStatus VST with the _Area meta-schema table to get a more complete picture
   *
   */

  for each dictdb._AreaStatus no-lock where _AreaStatus-AreaNum = examine-item,
      dictdb._Area no-lock where dictdb._Area._Area-num = dictdb._AreaStatus._AreaStatus-Areanum:

    /* calculate free blocks & % used
     *
     */

    if ( _AreaStatus-Freenum = ? ) then
      bfree = _AreaStatus-Totblocks - _AreaStatus-Hiwater.
     else
      bfree = _AreaStatus-Totblocks - _AreaStatus-Hiwater + _AreaStatus-Freenum.

    if bfree = ? then bfree = _AreaStatus-totblocks.

    used = (( _AreaStatus-totblocks - bfree) / _AreaStatus-totblocks ) * 100.

    /* if there's already a matching record use that, otherwise create one
     *
     */

    find tt_area exclusive-lock where tt_area.xid = _AreaStatus-Areanum no-error.
    if not available tt_area then create tt_area.
    assign
      tt_area.xid   = _AreaStatus-Areanum
      tt_area.xname = _AreaStatus-Areaname
      tt_area.btot  = _AreaStatus-Totblocks
      tt_area.bhiw  = _AreaStatus-Totblocks - bfree
      tt_area.blkf  = bfree
      tt_area.buse  = used
      tt_area.rpb   = exp( 2, _Area._Area-recbits )	/* thanks to Dmitri Levin!	*/
    .

    /* count the number of storage objects (tables & indexes) in this storage area
     *
     */

    assign
      so_tbl = 0
      so_idx = 0
      tt_area.xnote = ""
    .

    for each _storageobject no-lock where _storageobject._area-number = tt_area.xid and _storageobject._object-num > 0 and _storageobject._object-associate > 0:
      if       _storageobject._object-type = 1 then
        do:
          so_tbl = so_tbl + 1.
          run upd-tt_table ( _storageobject._object-number ).
        end.
       else if _storageobject._object-type = 2 then
        do:
          so_idx = so_idx + 1.
          run upd-tt_index ( _storageobject._object-number ).
        end.
    end.

    if so_tbl > 0 then tt_area.xnote = tt_area.xnote + "t(" + string( so_tbl ) + ") ".
    if so_idx > 0 then tt_area.xnote = tt_area.xnote + "i(" + string( so_idx ) + ") ".

    /* if there are data tables or indexes in the schema area (area 6) note it with a "*"
     *
     */

    if tt_area.xid = 6 and ( so_tbl + so_idx ) > 0 then tt_area.xnote = tt_area.xnote + "***".

    /* figure out if after image areas are empty, full or busy and indicate status
     *
     */

    if _areaStatus-Areaname matches "*After Image Area*" then
      tt_area.xnote = ai-info( input _AreaStatus-LastExtent, output seqnum ).

  end.

  run age_table.
  run age_index.

  define variable i as integer no-undo.
  define variable x as integer no-undo.
  define variable z as integer no-undo.

  define variable sort-criteria as character no-undo case-sensitive.

  find tt_area no-lock where tt_area.xid = examine-item no-error.
  if not available tt_area then
    do:
      do-display( support, 1, 100, 11, 1, "Area Details for UNKNOWN area" ).
      do-display( support, 2, 100, 11, 1, "Table Details for UNKNOWN area" ).
      do-display( support, 3, 100, 11, 1, "Index Details for UNKNOWN area" ).
      return.
    end.

  do-display( support, 1, 100, 11, 1, "Area Details for " + tt_area.xname ).
  do-display( support, 2, 100, 11, 1, "Table Details for " + tt_area.xname ).
  do-display( support, 3, 100, 11, 1, "Index Details for " + tt_area.xname ).

  do-SumSample( output x, output z ).

  i = 0.

  for each tt_area no-lock where tt_area.xid = examine-item:

    i = i + 1.

    ui-det( support, 1, i, 1, "AreaId",      string( tt_area.xid,   ">>>9" )).
    ui-det( support, 1, i, 2, "AreaName",    string( tt_area.xname, "x(25)" )).
    ui-det( support, 1, i, 3, "AreaBlocks",  string( tt_area.btot,  ">>>>>>>9" )).
    ui-det( support, 1, i, 4, "AreaHiWater", string( tt_area.bhiw,  ">>>>>>>9" )).
    ui-det( support, 1, i, 5, "AreaFree",    string( tt_area.blkf,  ">>>>>>>9" )).
    ui-det( support, 1, i, 6, "AreaPctUsed", string( tt_area.buse,  ">>9.99%" )).
    ui-det( support, 1, i, 7, "AreaRPB",     string( tt_area.rpb,   ">>9" )).
    ui-det( support, 1, i, 8, "AreaNote",    string( tt_area.xnote, "x(10)" )).

  end.

  i = 0.

  for each tt_table no-lock:

    i = i + 1.

    ui-det( support, 2, i, 1, "TblNum",  string( tt_table.xid,      ">>>9" )).
    ui-det( support, 2, i, 2, "TblName", string( tt_table.tblname,  "x(26)" )).
    ui-det( support, 2, i, 3, "Create",  string( ( tt_table.tbl-cre[x] / z ), ">>>>>>>>9" )).
    ui-det( support, 2, i, 4, "Read",    string( ( tt_table.tbl-rd[x]  / z ), ">>>>>>>>9" )).
    ui-det( support, 2, i, 5, "Update",  string( ( tt_table.tbl-upd[x] / z ), ">>>>>>>>9" )).
    ui-det( support, 2, i, 6, "Delete",  string( ( tt_table.tbl-del[x] / z ), ">>>>>>>>9" )).

  end.

  i = 0.

  for each tt_index no-lock:

    i = i + 1.

    ui-det( support, 3, i, 1, "IdxNum",    string( tt_index.xid,        ">>>9" )).
    ui-det( support, 3, i, 2, "IdxName",   string( tt_index.idxname,   "x(23)" )).
    ui-det( support, 3, i, 3, "IdxNote",   string( tt_index.idxnote,    "x(2)" )).
    ui-det( support, 3, i, 4, "IdxCre",    string( ( tt_index.idx-cre[x]    / z ), ">>>>>>>>9" )).
    ui-det( support, 3, i, 5, "IdxRd",     string( ( tt_index.idx-rd[x]     / z ), ">>>>>>>>9" )).
    ui-det( support, 3, i, 6, "IdxSplit",  string( ( tt_index.idx-split[x]  / z ), ">>>>>>>>9" )).
    ui-det( support, 3, i, 7, "IdxDel",    string( ( tt_index.idx-del[x]    / z ), ">>>>>>>>9" )).
    ui-det( support, 3, i, 8, "IdxBlkDel", string( ( tt_index.idx-blkdel[x] / z ),  ">>>>>>>9" )).

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
