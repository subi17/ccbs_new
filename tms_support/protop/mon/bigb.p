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
 * bigb.p
 *
 *
 * Make a stab a guesstimating the effects of changing -B.
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	October 9, 2003
 *
 *
 * History:
 *
 *	Gus suggested that this (based on an old PEG post) would make an interesting module.
 *	September 30, 2003
 *
 */

{lib/protop.i}

define variable support as character no-undo initial "BigB".

define temp-table tt_bigb no-undo
  field xpct    as decimal
  field bigb    as integer
  field db-pct  as decimal
  field hr      as decimal
  field mpct    as decimal
  field hpct    as decimal
  field osr     as integer
  field lrx     as integer extent 5
  field osrx    as integer extent 5
  index xpct-idx is unique primary xpct.

/* Predict Big B
 *
 * Based on the calculation in this thread:
 *
 *	http://www.peg.com/lists/dba/history/200301/msg00509.html
 *
 * which originates in a 1998 posting (by me) referring to some interesting
 * research published by IBM.  The formula used here is the one derived by
 * Tim Casey:
 *
 * 	m2 = m1 * sqrt( b1 / b2 )
 *
 * This results in a simple calculation of the expected impact on OS reads
 * which is generally the ultimate goal of tuning -B.
 *
 * This is not an exact calculation -- there should be a locally calibrated
 * constant applied to  ( b1 / b2 ).  The value is probably between 0.5 & 1.5
 * but these results are, IMHO, close enough to give an idea of what to expect
 * plus or minus a bit (maybe 25%).
 *
 * It should also be noted that as increases in -B reduce OS reads you can
 * expect to see a possibly significant increase in logical reads since
 * less time will be spent waiting for OS reads -- this will impact the
 * OS reads in a recursive manner...
 *
 * You'll get better results in you sample for a longer period.  10 second
 * samples (for instance) can have quite a lot of variation from sample to
 * sample.  I'd try at least 60 seconds for starters.
 *
 */

procedure predict-b:

  define input parameter p_pct  as decimal no-undo.
  define input parameter p_buf  as integer no-undo.
  define input parameter p_lrx  as integer no-undo.
  define input parameter p_osrx as integer no-undo.
  define input parameter p_used as integer no-undo.

  define variable x as integer no-undo.
  define variable z as integer no-undo.

  do-SumSample( output x, output z ).

  find tt_bigb exclusive-lock where tt_bigb.xpct = p_pct no-error.

  if available tt_bigb then
    assign
      tt_bigb.lrx[3]  = p_lrx
      tt_bigb.osrx[3] = p_osrx
    .
   else
    do:
      create tt_bigb.
      assign
        tt_bigb.xpct = p_pct					/* % of current Big B	*/
        tt_bigb.bigb = p_buf * ( tt_bigb.xpct / 100 )		/* Modified Big B	*/
        {lib/init-xrec.i tt_bigb.lrx  p_lrx}
        {lib/init-xrec.i tt_bigb.osrx p_osrx}
      .
    end.

  assign

    {lib/upd-xrec.i tt_bigb.lrx  tt_bigb.lrx[3]}		/* Logical Reads	*/
    {lib/upd-xrec.i tt_bigb.osrx tt_bigb.osrx[3]} 		/* OS Reads		*/

    tt_bigb.mpct =						/* Predicted Miss %	*/
      ( if tt_bigb.bigb > p_used then 0 else				/* it all fits in -B	*/
      100 * exp(( p_buf / tt_bigb.bigb ), 0.5 ) *			/* sqrt( OldB / NewB )	*/
      ( 1 - (( tt_bigb.lrx[x] - tt_bigb.osrx[x] ) / tt_bigb.lrx[x] )))	/* Current Miss %	*/

    tt_bigb.hpct = 100 - tt_bigb.mpct				/* Predicted Hit %	*/
    tt_bigb.osr  = tt_bigb.lrx[x] * ( tt_bigb.mpct / 100 )	/* Predicted OS Reads	*/
    tt_bigb.hr   = tt_bigb.lrx[x] / tt_bigb.osr			/* Predicted Hit Ratio	*/
    tt_bigb.db-pct = ( tt_bigb.bigb / p_used ) * 100.
    tt_bigb.hr = ( if tt_bigb.hr = ? then 99999999 else tt_bigb.hr )
  .

  return.

end.

/* restart
 *
 */

procedure mon-restart:

  empty temp-table tt_bigb.

  delete procedure this-procedure.

  return.

end.

/* initialize
 *
 */

procedure mon-init:

  empty temp-table tt_bigb.

  /* define labels
   *
   */

  ui-define-label( support, 1, 1, "xpct",  "    Pct" ).
  ui-define-label( support, 1, 2, "bigb",  "      Big B" ).
  ui-define-label( support, 1, 3, "dbpct", "  % db Size" ).
  ui-define-label( support, 1, 4, "hr",    "   Hit:1" ).
  ui-define-label( support, 1, 5, "mpct",  "   Miss%" ).
  ui-define-label( support, 1, 6, "hpct",  "    Hit%" ).
  ui-define-label( support, 1, 7, "osr",   "OS Reads" ).

  /***
  ui-define-label( support, 1, 8, "lrx",   "LRX     " ).
  ui-define-label( support, 1, 9, "osrx",  "OSRX    " ).
   ***/

  return.

end.

/* update
 *
 */

procedure mon-update:

  define variable examine-item as integer no-undo.

  define variable u-pct as decimal no-undo.
  define variable bfree as integer no-undo.
  define variable used  as integer no-undo.

  define variable i     as integer no-undo.
  define variable b_pct as integer no-undo extent 12
    initial [ 10, 25, 50, 100, 150, 200, 400, 800, 1000, 2000, 5000, 10000 ].

  if do-update( support ) = no then return.

  publish "get-examine-item" ( output examine-item ).

  find first dictdb._BuffStatus no-lock.
  find first dictdb._ActBuffer  no-lock.

  /* How many blocks is this database using?
   *
   * This is a quick & dirty calculation -- maybe it should be better?
   *
   */

  for each dictdb._AreaStatus no-lock:
    if ( _AreaStatus-Freenum = ? ) then
      bfree = _AreaStatus-Totblocks - _AreaStatus-Hiwater.
     else
      bfree = _AreaStatus-Totblocks - _AreaStatus-Hiwater + _AreaStatus-Freenum.
    if bfree = ? then bfree = _AreaStatus-totblocks.
    used = used + ( _AreaStatus-totblocks - bfree ).
  end.

  do i = 1 to 12:
    run predict-b( b_pct[i], _BuffStatus._BfStatus-TotBufs, _ActBuffer._Buffer-LogicRds, _ActBuffer._Buffer-OSRds, used ).
  end.

  if examine-item > 0 then
    do:
      u-pct = 100 * ( examine-item / _BuffStatus._BfStatus-TotBufs ).
      run predict-b( u-pct, _BuffStatus._BfStatus-TotBufs, _ActBuffer._Buffer-LogicRds, _ActBuffer._Buffer-OSRds, used ).
    end.

  /*** if we don't RELEASE this the summary screen fails to find it...
   ***
   ***/

  release dictdb._ActBuffer.

  define variable x as integer no-undo.
  define variable z as integer no-undo.

  do-display( support, 1, 100, 11, 1, "Big B GuessTimator" ).

  do-SumSample( output x, output z ).

  /*** Sort criteria aren't useful to this display type
   ***
   ***/

  i = 0.

  for each tt_bigb no-lock by tt_bigb.xpct:

    i = i + 1.

    ui-det( support, 1, i, 1, "xpct",  string( tt_bigb.xpct,         ">>>>>9%" )).
    ui-det( support, 1, i, 2, "bigb",  string( tt_bigb.bigb,     ">>>>>>>>>>9" )).
    ui-det( support, 1, i, 3, "dbpct", string( tt_bigb.db-pct,   ">>>>>9.999%" )).
    ui-det( support, 1, i, 4, "hr",    string( tt_bigb.hr,          ">>>>>>>9" )).
    ui-det( support, 1, i, 5, "mpct",  string( tt_bigb.mpct,        ">>9.999%" )).
    ui-det( support, 1, i, 6, "hpct",  string( tt_bigb.hpct,        ">>9.999%" )).
    ui-det( support, 1, i, 7, "osr",   string( ( tt_bigb.osr / z ), ">>>>>>>9" )).

    /***
    ui-det( support, 1, i, 8, "lrx",  string( tt_bigb.lrx[x],  ">>>>>>>9" )).
    ui-det( support, 1, i, 9, "osrx", string( tt_bigb.osrx[x], ">>>>>>>9" )).
     ***/

    if tt_bigb.xpct = 100 then ui-det( support, 1, i, 10, "flag", "<== Current -B" ).

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
