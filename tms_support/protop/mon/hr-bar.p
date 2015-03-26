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
 * hr-bar.p
 *
 *
 * Hit Ratio Bar Chart.
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	November 2, 2003
 *
 */

{lib/protop.i}

define variable support as character no-undo initial "Hit Ratio Bar Chart".

define variable bar-marker   as character no-undo initial "@".	/* # is too busy for my taste	*/
define variable chart-lines  as integer   no-undo initial  10.	/* 20 also works well		*/
define variable keep-history as integer   no-undo initial 100.

define variable xseq# as integer no-undo.

define variable old_log-rd as integer no-undo.
define variable old_os-rd  as integer no-undo.

define variable lo as decimal no-undo.
define variable hi as decimal no-undo.

define temp-table tt_hit-ratio no-undo
  field xid        as integer
  field log-rd     as integer
  field os-rd      as integer
  field hit-ratio  as decimal
  index xidx-idx is unique primary xid descending.

/* restart
 *
 */

procedure mon-restart:

  empty temp-table tt_hit-ratio.

  delete procedure this-procedure.

  return.

end.

/* initialize
 *
 */

procedure mon-init:

  empty temp-table tt_hit-ratio.

  find dictdb._ActBuffer  no-lock.
  assign
    xseq#      = 0
    lo         = 100
    old_log-rd = _ActBuffer._Buffer-LogicRds
    old_os-rd  = _ActBuffer._Buffer-OSRds
  .

  release dictdb._ActBuffer.

  return.

end.

/* update
 *
 */

procedure mon-update:

  define variable x as decimal   no-undo.		/* throw away output parameter	*/
  define variable d as character no-undo.		/* throw away output parameter	*/

  if do-update( support ) = no then return.

  find dictdb._ActBuffer no-lock.

  create tt_hit-ratio.
  assign
    xseq# = xseq# + 1
    tt_hit-ratio.xid    = xseq#
  .
  assign
    tt_hit-ratio.log-rd = _ActBuffer._Buffer-LogicRds - old_log-rd
    tt_hit-ratio.os-rd  = _ActBuffer._Buffer-OSRds    - old_os-rd
    tt_hit-ratio.hit-ratio = hr( input tt_hit-ratio.log-rd, input tt_hit-ratio.os-rd, output d, output x, output x )
    old_log-rd = _ActBuffer._Buffer-LogicRds
    old_os-rd  = _ActBuffer._Buffer-OSRds
  .

  release dictdb._ActBuffer.

  for each tt_hit-ratio exclusive-lock where tt_hit-ratio.xid <= ( xseq# - keep-history ):
    delete tt_hit-ratio.
  end.

  define variable i  as integer   no-undo.
  define variable s  as character no-undo.
  define variable xx as decimal   no-undo.

  do-display( support, 1, 100, 11, 1, "Hit Ratio Bar Chart" ).

  for each tt_hit-ratio no-lock:
    assign
      lo = min( tt_hit-ratio.hit-ratio, lo )
      hi = max( tt_hit-ratio.hit-ratio, hi )
    .
  end.

  hi = 100.	/* yeah, yeah... 	*/

  /* Pick brackets that make "nice" values for the y-axis
   *
   */

  if lo       >= 99 then lo = 99.
   else if lo >= 98 then lo = 98.
   else if lo >= 95 then lo = 95.
   else if lo >= 90 then lo = 90.
   else                  lo =  0.

  xx = ( hi - lo ) / chart-lines.

  do i = 0 to chart-lines:
    x = 100 - ( i * xx ).
    s = string( x, ">>9.99" ) + " ".
    for each tt_hit-ratio no-lock:
      s = s + ( if tt_hit-ratio.hit-ratio >= x then bar-marker else " " ).
    end.
    ui-det( support, 1, ( i + 1 ), 1, "ChLine", s ).
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
