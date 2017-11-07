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
 * ai-status.p
 *
 *
 * After Image extents.
 *
 *
 * To Do:
 *
 *	AI Summary screen
 *	Fixed/Variable extent indicator
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	September 26, 2003
 *
 */

{protop/lib/protop.i}

define variable support as character no-undo initial "AI Status".

define temp-table tt_ai no-undo
  field xid     as integer   format ">>>9"      label "Area"
  field xname   as character format "x(10)"     label "Extent"
  field btot    as integer   format ">>>>>>>9"  label "Blocks"
  field bhiw    as integer   format ">>>>>>>9"  label "Hi Water"
  field blkf    as integer   format ">>>>>>>9"  label "Free"
  field buse    as decimal   format ">,>>9.99%" label "%Used"
  field seqnum  as integer   format ">>>>>>>9"  label "Seq#"
  field xnote   as character format "x(12)"     label "Note"
  index xid-idx is unique primary xid.

/* restart
 *
 */

procedure mon-restart:

  empty temp-table tt_ai.

  delete procedure this-procedure.

  return.

end.

/* initialize
 *
 */

procedure mon-init:

  empty temp-table tt_ai.

  /* define labels
   *
   */

  ui-define-label( support, 1, 1, "AreaId",  "Area" ).
  ui-define-label( support, 1, 2, "Extent",  "Extent            " ).
  ui-define-label( support, 1, 3, "Blocks",  "  Blocks" ).
  ui-define-label( support, 1, 4, "HiWater", "Hi Water" ).
  ui-define-label( support, 1, 5, "Free",    "    Free" ).
  ui-define-label( support, 1, 6, "PctUsed", "    %Used" ).
  ui-define-label( support, 1, 7, "SeqNum",  "    Seq#" ).
  ui-define-label( support, 1, 8, "Note",    "Note      " ).

  return.

end.

/* update
 *
 */

procedure mon-update:

  define variable bfree  as decimal   no-undo.
  define variable used   as decimal   no-undo.
  define variable so_idx as integer   no-undo.
  define variable so_tbl as integer   no-undo.
  define variable fname  as character no-undo.

  if do-update( support ) = no then return.

  /*** This should be a stable set of records -- there's no benefit to cleaning it out...
  empty temp-table tt_ai.
   ***/

  /* Join the _AreaStatus VST with the _Area meta-schema table to get a more complete picture
   *
   * Only select after image extents
   *
   */

  for each dictdb._AreaStatus no-lock where _areaStatus-Areaname matches "*After Image Area*",
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

    /* get the filename of the after image extent (it's always the last one & there's only one)
     *
     */

    assign
      fname = _AreaStatus-LastExtent
      fname = substring( fname, r-index( fname,  "/" ) + 1 )
      fname = substring( fname, r-index( fname, "~\" ) + 1 )	/* Windows fix thanks to Patrick Tingen	*/
    .

    /* if there's already a matching record use that, otherwise create one
     *
     */

    find tt_ai exclusive-lock where tt_ai.xid = _AreaStatus-Areanum no-error.
    if not available tt_ai then create tt_ai.
    assign
      tt_ai.xid   = _AreaStatus-Areanum
      tt_ai.xname = fname
      tt_ai.btot  = _AreaStatus-Totblocks
      tt_ai.bhiw  = _AreaStatus-Totblocks - bfree
      tt_ai.blkf  = bfree
      tt_ai.buse  = used
    .

    /* figure out if after image areas are empty, full or busy and indicate status
     *
     */

    tt_ai.xnote = ai-info( input _AreaStatus-LastExtent, output tt_ai.seqnum ).

  end.

  define variable i as integer no-undo.

  do-display( support, 1, 100, 11, 1, "After Imaging Status" ).

  define query q for tt_ai.
  open query q for each tt_ai no-lock by tt_ai.xid. /* descending. */

  do while true:

    get next q.

    if not available tt_ai then leave.

    if tt_ai.xname = "" then next.

    i = i + 1.

    ui-det( support, 1, i, 1, "AreaId",  string( tt_ai.xid,    ">>>9" )).
    ui-det( support, 1, i, 2, "Extent",  string( tt_ai.xname,  "x(18)" )).
    ui-det( support, 1, i, 3, "Blocks",  string( tt_ai.btot,   ">>>>>>>9" )).
    ui-det( support, 1, i, 4, "HiWater", string( tt_ai.bhiw,   ">>>>>>>9" )).
    ui-det( support, 1, i, 5, "Free",    string( tt_ai.blkf,   ">>>>>>>9" )).
    ui-det( support, 1, i, 6, "PctUsed", string( tt_ai.buse,   ">,>>9.99%" )).
    ui-det( support, 1, i, 7, "SeqNum",  string( tt_ai.seqnum, ">>>>>>>9" )).
    ui-det( support, 1, i, 8, "Note",    string( tt_ai.xnote,  "x(10)" )).

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
