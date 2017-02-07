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
 * latch.p
 *
 *
 * Latch Waits.
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	September 5, 2003
 *
 */

{tms_support/protop/lib/protop.i}

define variable support  as character no-undo initial "Latch Waits".

{tms_support/protop/lib/tt_xstat.i}

/* a prototype for a more generic approach to sort_by
 *
 * issues:
 *    - handling of "x" is crude
 *    - criteria should handle abbreviated field names as well as #s
 *    - fields ought to be numbered without holes
 *    - won't work very well for multiple simultaneous displays
 *    - ascending/descending is crude
 *    - datatype & array determination ought to be automatic somehow
 */

function sort_by returns character (
  input p_dtype    as character,
  input p_variant  as integer,
  input p_criteria as character,
  input p_ttable   as character,
  input p_def-fld  as character,
  input p_x        as integer
  ).

  define variable wc as character no-undo.

  find first tt_ui-sch no-lock where
       tt_ui-sch.display_type    = p_dtype and
       tt_ui-sch.display_variant = p_variant and
       tt_ui-sch.data_order      = integer( p_criteria ) no-error.

  if not available tt_ui-sch then
    wc =
     "for each " + p_ttable +
     " by " + p_ttable + "." +
     p_def-fld
    .
   else
    wc =
     "for each " + p_ttable +
     " by " + p_ttable + "." +
     trim( tt_ui-sch.data_key, "label." ) +
     "[" + string( p_x ) + "] descending"
    .

/***  message wc. ***/

  return wc.

end.

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

  ui-define-label( support, 1,  1, "xid",        " Id" ).
  ui-define-label( support, 1,  2, "xname",      "Latch               " ).
  ui-define-label( support, 1,  5, "stat1",      "  Requests" ).
  ui-define-label( support, 1,  6, "stat2",      "     Waits" ).
  ui-define-label( support, 1,  8, "stat-ratio",    "  Lock%" ).

  return.

end.

/* update
 *
 */

procedure mon-update:

  define variable tlist as character no-undo.
  define variable xlist as character no-undo.

  if do-update( support ) = no then return.

  publish "get-tlist" ( output tlist ).			/* yes, I know that these are global but this	*/
  publish "get-xlist" ( output xlist ).			/* makes life lots easier for now		*/

  for each dictdb._Latch where _Latch-Id <> ? no-lock:

    if tlist <> "" then if lookup( _Latch-name, tlist )  = 0 then next.
    if xlist <> "" then if lookup( _Latch-name, xlist ) <> 0 then next.

    run update_xstat (
      input _Latch-Id,
      input _Latch-name,
      input "",
      input "",
      input _Latch-lock,
      input _Latch-wait,
      input 0,
      input 0
    ).

  end.

  run age_xstat.

  define variable i as integer no-undo.
  define variable x as integer no-undo.
  define variable z as integer no-undo.

  define variable sort-criteria as character no-undo case-sensitive.

  do-display( support, 1, 100, 11, 1, "Latch Waits" ).

  do-SumSample( output x, output z ).

  publish "get-sort-criteria" ( output sort-criteria ).

  define query q for tt_xstat.

/***
  define variable qh as handle no-undo.

  qh = query q:handle.
  qh:query-prepare( sort_by( support, 1, sort-criteria, "tt_xstat", "stat1[5] descending", x )). 
  qh:query-open().
 ***/

  case sort-criteria:
    when "l" then open query q for each tt_xstat no-lock by tt_xstat.stat1[x]   descending.
    when "w" then open query q for each tt_xstat no-lock by tt_xstat.stat2[x]   descending.
    when "h" then open query q for each tt_xstat no-lock by tt_xstat.stat-ratio descending.
    when "L" then open query q for each tt_xstat no-lock by tt_xstat.stat1[x].
    when "W" then open query q for each tt_xstat no-lock by tt_xstat.stat2[x].
    when "H" then open query q for each tt_xstat no-lock by tt_xstat.stat-ratio.
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

    ui-det( support, 1, i,  1, "xid",   string( tt_xstat.xid,                       ">>9" )).
    ui-det( support, 1, i,  2, "xname", string( tt_xstat.xname,                   "x(20)" )).
    ui-det( support, 1, i,  5, "stat1", string( ( tt_xstat.stat1[x] / z ),   ">>>>>>>>>9" )).
    ui-det( support, 1, i,  6, "stat2", string( ( tt_xstat.stat2[x] / z ),   ">>>>>>>>>9" )).
    ui-det( support, 1, i,  8, "stat-ratio", string( tt_xstat.stat-ratio,       ">>9.99%" )).

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
