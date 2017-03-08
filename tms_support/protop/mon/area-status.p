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
 * area-status.p
 *
 *
 * Storage Area Statistics monitoring.
 *
 *
 * To Do:
 *
 *	Area specific notes:
 *	  - bi??? clusters in use?
 *	  Fixed/Variable extent indicator
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	August 28, 2003
 *
 *
 * History:
 *
 *	Implemented Dmitri Levin's RPB solution
 *	September 22, 2003
 *
 *	Added George Potemkin's ai-info routine to get empty/full/busy status
 *	September 26, 2003
 *
 */

{protop/lib/protop.i}

define variable support as character no-undo initial "Area Status".

define temp-table tt_area no-undo
  field xid     as integer
  field xname   as character
  field balloc  as integer
  field bvar    as integer
  field btot    as integer
  field bhiw    as integer
  field blkf    as integer
  field buse    as decimal
  field rpb     as integer
  field xnote   as character
  index xid-idx is unique primary xid.

/* restart
 *
 */

procedure mon-restart:

  empty temp-table tt_area.

  delete procedure this-procedure.

  return.

end.

/* initialize
 *
 */

procedure mon-init:

  empty temp-table tt_area.

  /* define labels
   *
   */

  ui-define-label( support, 1,  1, "AreaId",      " A#" ).
  ui-define-label( support, 1,  2, "AreaName",    "Area Name           " ).
  ui-define-label( support, 1,  3, "AreaAlloc",   "    Alloc" ).
  ui-define-label( support, 1,  4, "AreaVar",     "      Var" ).
/** ui-define-label( support, 1,  5, "AreaBlocks",  "   Blocks" ). **/
  ui-define-label( support, 1,  6, "AreaHiWater", " Hi Water" ).
  ui-define-label( support, 1,  7, "AreaFree",    "     Free" ).
  ui-define-label( support, 1,  8, "AreaPctUsed",  "  %Used" ).
/** ui-define-label( support, 1, 9, "AreaRPB",     "RPB" ). **/
  ui-define-label( support, 1, 10, "AreaNote",    "Note    " ).

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

  define variable blks-alloc as decimal   no-undo format ">,>>>,>>>,>>9".
  define variable pct-alloc  as decimal   no-undo format ">>>,>>9%".
  define variable vsize      as decimal   no-undo format ">,>>>,>>>,>>9".

  define variable sort-criteria as character no-undo case-sensitive.

  if do-update( support ) = no then return.

  /*** This should be a stable set of records -- there's no benefit to cleaning it out...
  empty temp-table tt_area.
   ***/

  /* Join the _AreaStatus VST with the _Area meta-schema table to get a more complete picture
   *
   */

  for each dictdb._AreaStatus no-lock,
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

    assign
      blks-alloc = 0
      vsize = 0
    .

    for each _AreaExtent no-lock where _AreaExtent._Area-number = _AreaStatus._AreaStatus-AreaNum:
      if _Extent-type >= 4 and _Extent-type <= 7 then
        vsize = max( _Extent-size, _AreaStatus-totblocks - blks-alloc ).
       else
        blks-alloc = blks-alloc + integer( _Extent-size / ( _Area-blocksize / 1024 )).
    end.

    if blks-alloc = 0 then
      pct-alloc = used.
     else
      pct-alloc = (( _AreaStatus-totblocks - bfree ) / blks-alloc ) * 100.

    /* if there's already a matching record use that, otherwise create one
     *
     */

    find tt_area exclusive-lock where tt_area.xid = _AreaStatus-Areanum no-error.
    if not available tt_area then create tt_area.
    assign
      tt_area.xid    = _AreaStatus-Areanum
      tt_area.xname  = _AreaStatus-Areaname
      tt_area.balloc = blks-alloc
      tt_area.bvar   = vsize
      tt_area.btot   = _AreaStatus-Totblocks
      tt_area.bhiw   = _AreaStatus-Totblocks - bfree
      tt_area.blkf   = bfree
      tt_area.buse   = /* used */ pct-alloc
      tt_area.rpb    = exp( 2, _Area._Area-recbits )	/* thanks to Dmitri Levin!	*/
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
      if       _storageobject._object-type = 1 then so_tbl = so_tbl + 1.
       else if _storageobject._object-type = 2 then so_idx = so_idx + 1.
    end.

    if so_tbl > 0 then tt_area.xnote = tt_area.xnote + "t(" + string( so_tbl ) + ") ".
    if so_idx > 0 then tt_area.xnote = tt_area.xnote + "i(" + string( so_idx ) + ") ".

    /* if there are data tables or indexes in the schema area (area 6) note it with a "*"
     *
     */

    if tt_area.xid = 6 and ( so_tbl + so_idx ) > 0 then tt_area.xnote = tt_area.xnote + "*".

    /* figure out if after image areas are empty, full or busy and indicate status
     *
     */

    if _areaStatus-Areaname matches "*After Image Area*" then
      tt_area.xnote = ai-info( input _AreaStatus-LastExtent, output seqnum ).

  end.

  define variable i as integer no-undo.

  do-display( support, 1, 100, 11, 1, "Area Statistics" ).

  publish "get-sort-criteria" ( output sort-criteria ).

  define query q for tt_area.

  case sort-criteria:
    when "b" then open query q for each tt_area no-lock by tt_area.btot  descending.
    when "h" then open query q for each tt_area no-lock by tt_area.bhiw  descending.
    when "f" then open query q for each tt_area no-lock by tt_area.blkf  descending.
    when "u" then open query q for each tt_area no-lock by tt_area.buse  descending.
    when "r" then open query q for each tt_area no-lock by tt_area.rpb   descending.
    when "B" then open query q for each tt_area no-lock by tt_area.btot.
    when "H" then open query q for each tt_area no-lock by tt_area.bhiw.
    when "F" then open query q for each tt_area no-lock by tt_area.blkf.
    when "U" then open query q for each tt_area no-lock by tt_area.buse.
    when "R" then open query q for each tt_area no-lock by tt_area.rpb.
    when "n" then open query q for each tt_area no-lock by tt_area.xname.
    when "N" then open query q for each tt_area no-lock by tt_area.xname descending.
    when "#" then open query q for each tt_area no-lock by tt_area.xid.
    when "-" then open query q for each tt_area no-lock by tt_area.xid   descending.
    otherwise     open query q for each tt_area no-lock by tt_area.buse  descending.
  end.

  do while true:

    get next q.

    if not available tt_area then leave.

    i = i + 1.

    ui-det( support, 1, i,  1, "AreaId",      string( tt_area.xid,   ">>9" )).
    ui-det( support, 1, i,  2, "AreaName",    string( tt_area.xname, "x(20)" )).
    ui-det( support, 1, i,  3, "AreaBlocks",  string( tt_area.balloc, ">>>>>>>>9" )).
    ui-det( support, 1, i,  4, "AreaBlocks",  string( tt_area.bvar,   ">>>>>>>>9" )).
/** ui-det( support, 1, i,  5, "AreaBlocks",  string( tt_area.btot,  ">>>>>>>9" )). **/
    ui-det( support, 1, i,  6, "AreaHiWater", string( tt_area.bhiw,  ">>>>>>>>9" )).
    ui-det( support, 1, i,  7, "AreaFree",    string( tt_area.blkf,  ">>>>>>>>9" )).
    ui-det( support, 1, i,  8, "AreaPctUsed", string( tt_area.buse,  ">>>>>9%" )).
/** ui-det( support, 1, i,  9, "AreaRPB",     string( tt_area.rpb,   ">>9" )). **/
    ui-det( support, 1, i, 10, "AreaNote",    string( tt_area.xnote, "x(8)" )).

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
