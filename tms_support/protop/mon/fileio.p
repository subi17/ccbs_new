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
 * fileio.p
 *
 *
 * Database Extent IO activity.
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	September 5, 2003
 *
 *
 * History:
 *
 *	Accepted changes from Patrick Tingen handling Windows filenames ("\" vs "/")
 *	September 27, 2003
 *
 */

{lib/protop.i}

define variable support  as character no-undo initial "File IO".

{lib/tt_xstat.i}

/* restart
 *
 */

procedure mon-restart:

  empty temp-table tt_xstat.

  delete procedure this-procedure.

  return.

end.

/* initialize
 *
 */

procedure mon-init:

  empty temp-table tt_xstat.

  /* define labels
   *
   */

  ui-define-label( support, 1, 1, "xid",    "  Id" ).
  ui-define-label( support, 1, 2, "xname",  "Extent Name              " ).
  ui-define-label( support, 1, 3, "misc1",  "Mode    " ).
  ui-define-label( support, 1, 4, "misc2",  "Blksz" ).
  ui-define-label( support, 1, 5, "stat4",  "      Size" ).
  ui-define-label( support, 1, 6, "stat1",  "  Read" ).
  ui-define-label( support, 1, 7, "stat2",  " Write" ).
  ui-define-label( support, 1, 8, "stat3",  "Extend" ).

  return.

end.

/* update
 *
 */

procedure mon-update:

  define variable tlist as character no-undo.
  define variable xlist as character no-undo.

  if do-update( support ) = no then return.

  publish "get-tlist" ( output tlist ).
  publish "get-xlist" ( output xlist ).

  define variable fname as character no-undo.

  for each dictdb._FileList no-lock:

    find dictdb._ActIOFile no-lock where _IOFile-filename = _FileList-Name no-error.
    find dictdb._AreaExtent no-lock where _Extent-Path = _FileList-Name no-error.

    assign
      fname = _FileList-Name
      fname = substring( fname, r-index( fname,  "/" ) + 1 )
      fname = substring( fname, r-index( fname, "~\" ) + 1 )	/* Windows fix thanks to Patrick Tingen	*/
    .

    if tlist <> "" then if lookup( fname, tlist )  = 0 then next.
    if xlist <> "" then if lookup( fname, xlist ) <> 0 then next.

    run update_xstat (
      input _FileList-Id,
      input fname,
      input ( if available _AreaExtent then ( if _Extent-type >= 4 and _Extent-type <= 7 then "V" else "F" ) else " " ) + " " + replace( _FileList-OpenMode, "IO", "" ),
      input string( _FileList-BlkSize, ">>>>9" ),
      input ( if available _ActIOFile then _IOFile-reads else ? ),
      input ( if available _ActIOFile then _IOFile-writes else ? ),
      input ( if available _ActIOFile then _IOFile-extends else ? ),
      input ( if available _AreaExtent then string( _Extent-size ) else ? )
    ).

  end.

  run age_xstat.

  define variable i as integer no-undo.
  define variable x as integer no-undo.
  define variable z as integer no-undo.

  define variable sort-criteria as character no-undo case-sensitive.

  do-display( support, 1, 100, 11, 1, "Database File IO" ).

  do-SumSample( output x, output z ).

  publish "get-sort-criteria" ( output sort-criteria ).

  define query q for tt_xstat.

  case sort-criteria:
    when "m" then open query q for each tt_xstat no-lock by tt_xstat.misc1.
    when "b" then open query q for each tt_xstat no-lock by tt_xstat.misc2      descending.
    when "r" then open query q for each tt_xstat no-lock by tt_xstat.stat1[x]   descending.
    when "w" then open query q for each tt_xstat no-lock by tt_xstat.stat2[x]   descending.
    when "x" then open query q for each tt_xstat no-lock by tt_xstat.stat3[x]   descending.
    when "s" then open query q for each tt_xstat no-lock by tt_xstat.stat4[1]   descending.

    when "M" then open query q for each tt_xstat no-lock by tt_xstat.misc1     descending.
    when "B" then open query q for each tt_xstat no-lock by tt_xstat.misc2.
    when "R" then open query q for each tt_xstat no-lock by tt_xstat.stat1[x].
    when "W" then open query q for each tt_xstat no-lock by tt_xstat.stat2[x].
    when "X" then open query q for each tt_xstat no-lock by tt_xstat.stat3[x].
    when "S" then open query q for each tt_xstat no-lock by tt_xstat.stat4[1].

    when "n" then open query q for each tt_xstat no-lock by tt_xstat.xname.
    when "N" then open query q for each tt_xstat no-lock by tt_xstat.xname      descending.
    when "#" then open query q for each tt_xstat no-lock by tt_xstat.xid.
    when "-" then open query q for each tt_xstat no-lock by tt_xstat.xid        descending.
    otherwise     open query q for each tt_xstat no-lock by tt_xstat.stat1[x]   descending.
  end.

  do while true:

    get next q.

    if not available tt_xstat then leave.

    if tt_xstat.xname = "" then next.

    i = i + 1.

    ui-det( support, 1, i,  1, "xid",   string( tt_xstat.xid,                ">>>9" )).
    ui-det( support, 1, i,  2, "xname", string( tt_xstat.xname,             "x(25)" )).
    ui-det( support, 1, i,  3, "misc1", string( tt_xstat.misc1,              "x(8)" )).
    ui-det( support, 1, i,  4, "misc2", string( tt_xstat.misc2,              "x(5)" )).
    ui-det( support, 1, i,  5, "stat4", string( tt_xstat.stat4[1],     ">>>>>>>>>9" )).
    ui-det( support, 1, i,  6, "stat1", string( ( tt_xstat.stat1[x] / z ), ">>>>>9" )).
    ui-det( support, 1, i,  7, "stat2", string( ( tt_xstat.stat2[x] / z ), ">>>>>9" )).
    ui-det( support, 1, i,  8, "stat3", string( ( tt_xstat.stat3[x] / z ), ">>>>>9" )).

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
