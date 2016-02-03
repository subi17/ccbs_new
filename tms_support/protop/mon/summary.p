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
 * summary.p
 *
 *
 * Show a summary screen of interesting database metrics.
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	August 28, 2003
 *
 */

{tms_support/protop/lib/protop.i}
{local/mon/summary.v}

define variable support as character no-undo initial "Summary".

define temp-table tt_sstat no-undo
  field xid          as integer
  field sum-stat     as integer extent 5
  index xid-idx is unique primary xid.

define variable init-sum  as logical no-undo initial yes.
define variable init-tt   as logical no-undo initial yes.

/* restart
 *
 */

procedure mon-restart:

  empty temp-table tt_sstat.

  delete procedure this-procedure.

  return.

end.

/* initialize
 *
 */

procedure mon-init:

  empty temp-table tt_sstat.

  init-tt = yes.	/* say it ain't so!	*/
  run mon-update.
  init-tt = no.

  return.

end.

/* update
 *
 */

procedure mon-update:

  define variable latch-tmo  as integer no-undo.

  define variable warn-areas as integer no-undo.
  define variable worst-area as decimal no-undo.

  define variable ai_busy    as integer no-undo.
  define variable ai_exts    as integer no-undo.
  define variable ai_full    as integer no-undo.
  define variable ai_empty   as integer no-undo.

  if do-update( support ) = no then return.

  /* latch timeouts
   *
   */

  latch-tmo = 0.
  for each dictdb._latch no-lock:
    latch-tmo = latch-tmo + _latch._latch-wait.
  end.

  find dictdb._ActSummary no-lock.
  find dictdb._ActBuffer  no-lock.

  /* build the temp-table
   *
   */

  if init-tt = yes then
    do:

      create tt_sstat.
      assign
        tt_sstat.xid   = 4
        {tms_support/protop/lib/init-xrec.i tt_sstat.sum-stat _ActBuffer._Buffer-LogicRds}
      .

      create tt_sstat.
      assign
        tt_sstat.xid   = 5
        {tms_support/protop/lib/init-xrec.i tt_sstat.sum-stat _ActBuffer._Buffer-OSRds}
      .

      create tt_sstat.
      assign
        tt_sstat.xid   = 6
        {tms_support/protop/lib/init-xrec.i tt_sstat.sum-stat _ActSummary._Summary-RecReads}
/***
        {tms_support/protop/lib/init-xrec.i tt_sstat.sum-stat _ActSummary._Summary-Chkpts}
 ***/
      .

      create tt_sstat.
      assign
        tt_sstat.xid   = 7
        {tms_support/protop/lib/init-xrec.i tt_sstat.sum-stat "_ActBuffer._Buffer-LogicRds / _ActSummary._Summary-RecReads"}
/***
        {tms_support/protop/lib/init-xrec.i tt_sstat.sum-stat _ActSummary._Summary-Flushed}
 ***/
      .

      create tt_sstat.
      assign
        tt_sstat.xid   = 21
        {tms_support/protop/lib/init-xrec.i tt_sstat.sum-stat _ActSummary._Summary-Commits}
      .

      create tt_sstat.
      assign
        tt_sstat.xid   = 22
        {tms_support/protop/lib/init-xrec.i tt_sstat.sum-stat latch-tmo}
      .

      create tt_sstat.
      assign
        tt_sstat.xid   = 24
        {tms_support/protop/lib/init-xrec.i tt_sstat.sum-stat _ActBuffer._Buffer-LRUwrts}
      .

    end.
   else
    do:
      for each tt_sstat exclusive-lock:
        case tt_sstat.xid:
          when  4 then assign {tms_support/protop/lib/upd-xrec.i tt_sstat.sum-stat _ActBuffer._Buffer-LogicRds}.
          when  5 then assign {tms_support/protop/lib/upd-xrec.i tt_sstat.sum-stat _ActBuffer._Buffer-OSRds}.
/***
          when  6 then assign {tms_support/protop/lib/upd-xrec.i tt_sstat.sum-stat _ActSummary._Summary-Chkpts}.
          when  7 then assign {tms_support/protop/lib/upd-xrec.i tt_sstat.sum-stat _ActSummary._Summary-Flushed}.
 ***/
          when  6 then assign {tms_support/protop/lib/upd-xrec.i tt_sstat.sum-stat _ActSummary._Summary-RecReads}.
          when  7 then assign {tms_support/protop/lib/upd-xrec.i tt_sstat.sum-stat "_ActBuffer._Buffer-LogicRds / _ActSummary._Summary-RecReads"}.
          when 21 then assign {tms_support/protop/lib/upd-xrec.i tt_sstat.sum-stat _ActSummary._Summary-Commits}.
          when 22 then assign {tms_support/protop/lib/upd-xrec.i tt_sstat.sum-stat latch-tmo}.
          when 24 then assign {tms_support/protop/lib/upd-xrec.i tt_sstat.sum-stat _ActBuffer._Buffer-LRUwrts}.
        end.
      end.
    end.

  release dictdb._ActBuffer.

  define variable dname     as character no-undo.

  define variable lr1       as integer   no-undo.    define variable lr2       as integer   no-undo.
  define variable osr1      as integer   no-undo.    define variable osr2      as integer   no-undo.
  define variable hr1-str   as character no-undo.    define variable hr2-str   as character no-undo.
  define variable mr1       as decimal   no-undo.    define variable mr2       as decimal   no-undo.
  define variable hr1       as decimal   no-undo.    define variable hr2       as decimal   no-undo.

/***
  define variable chkp1     as integer   no-undo.    define variable chkp2     as integer   no-undo.
  define variable flsh1     as integer   no-undo.    define variable flsh2     as integer   no-undo.
 ***/
  define variable RecRd1    as integer   no-undo.    define variable RecRd2    as integer   no-undo.
  define variable LogRec1   as decimal   no-undo.    define variable LogRec2   as decimal   no-undo.
  define variable trx1      as integer   no-undo.    define variable trx2      as integer   no-undo.
  define variable ltmo1     as integer   no-undo.    define variable ltmo2     as integer   no-undo.
  define variable evict1    as integer   no-undo.    define variable evict2    as integer   no-undo.

  define variable oldbi     as integer   no-undo.
  define variable currbi    as integer   no-undo.

  define variable con-self  as integer   no-undo.
  define variable con-remc  as integer   no-undo.
  define variable con-batch as integer   no-undo.
  define variable con-serv  as integer   no-undo.
  define variable con-other as integer   no-undo.
  define variable con-trx   as integer   no-undo.
  define variable con-block as integer   no-undo.
  define variable con-total as integer   no-undo.

  define variable old-trx   as integer   no-undo.

  do-display( support, 1, 10, 1, 1, "Summary" ).

  /* session/connection types -- these don't make sense as accumulated metrics
   *
   */

  for each dictdb._Connect no-lock where dictdb._Connect._Connect-type = "self":
    con-self = con-self + 1.
  end.

  for each dictdb._Connect no-lock where dictdb._Connect._Connect-type = "remc":
    con-remc = con-remc + 1.
  end.

  for each dictdb._Connect no-lock where dictdb._Connect._Connect-Device = "batch":
    con-batch = con-batch + 1.
  end.

  for each dictdb._Connect no-lock where dictdb._Connect._Connect-type = "serv":
    con-serv = con-serv + 1.
  end.

  for each dictdb._Trans no-lock where _Trans-usrnum <> ?:
    con-trx = con-trx + 1.
/***
    if _Trans-counter <> ? and _Trans-counter > 0 then
      do:
        if oldbi = 0 or _Trans-counter < oldbi then oldbi = _Trans-counter.
      end.
    if _Trans-counter <> ? and _Trans-counter > 0 then currbi = max( currbi, _Trans-counter ).
 ***/
  end.

  /* Adjust to align with checkpoint numbers -- if we can.  _Trans-counter is the
   * checkpoint# since the last "truncate bi".  Unfortunately the only way to map
   * from that number to _BfStatus-LastCkpNum involves starting a transaction in
   * mon-init().  And that is optional since ProTop shouldn't require the user to
   * write to a monitored database.  So if that option isn't enabled (chkp-base = ?)
   * we have to live with the fact that we can't map them.  Which means that we're
   * showing 0 at times when the real active checkpoint is probably something else.
   * That's not as bad as it sounds though because it should only happen when there
   * are no active transactions which means that, while we don't know what the
   * actual current checkpoint # is, we also only have one active checkpoint so
   * there's nothing to be excited about in these two numbers.  (The whole point
   * of this metric is to show growth in the number of active bi clusters.)
   *
   */

/***
  find dictdb._BuffStatus no-lock.

  if chkp-base <> ? then
    do:
      currbi = _BfStatus-LastCkpNum.
      if oldbi = 0 then
        oldbi = currbi.
       else
        oldbi = oldbi - chkp-base.
    end.
   else
    do:
      if oldbi = 0 then oldbi = currbi.
    end.
***/

  currbi = chkptnum( input-output oldbi ).

  for each dictdb._Connect no-lock where _Connect-usr <> ? and _Connect-wait <> " -- ":	/* not perfect -- but works for record locks  */
    con-block = con-block + 1.
  end.

  for each dictdb._Connect where dictdb._Connect._Connect-usr <> ? no-lock:
    con-total = con-total + 1.
  end.
  con-other = con-total - con-self - con-remc - con-serv.	/* "batch" is redundant... */

  find dictdb._Startup no-lock.
  find dictdb._DbStatus no-lock.
  find dictdb._BuffStatus no-lock.

  dname = ldbname( "dictdb" ) + " " + "[" + pdbname( "dictdb" ) + "]".

  find tt_sstat where tt_sstat.xid = 4 no-lock.
  assign
    lr1 = tt_sstat.sum-stat[4] / xtime
    lr2 = tt_sstat.sum-stat[5] / itime
  .

  find tt_sstat where tt_sstat.xid = 5 no-lock.
  assign
    osr1 = tt_sstat.sum-stat[4] / xtime
    osr2 = tt_sstat.sum-stat[5] / itime
  .

  find tt_sstat where tt_sstat.xid = 6 no-lock.
  assign
/***
    chkp1 = tt_sstat.sum-stat[4]
    chkp2 = tt_sstat.sum-stat[5]
 ***/
    RecRd1 = tt_sstat.sum-stat[4] / xtime
    RecRd2 = tt_sstat.sum-stat[5] / itime
  .

  find tt_sstat where tt_sstat.xid = 7 no-lock.
  assign
/***
    flsh1 = tt_sstat.sum-stat[4] / xtime
    flsh2 = tt_sstat.sum-stat[5] / itime
 ***/
/***
    LogRec1 = tt_sstat.sum-stat[4]
    LogRec2 = tt_sstat.sum-stat[5]
 ***/
    LogRec1 = lr1 / RecRd1
    LogRec2 = lr2 / RecRd2
  .

  find tt_sstat where tt_sstat.xid = 21 no-lock.
  assign
    trx1 = tt_sstat.sum-stat[4] / xtime
    trx2 = tt_sstat.sum-stat[5] / itime
  .

  find tt_sstat where tt_sstat.xid = 22 no-lock.
  assign
    ltmo1 = tt_sstat.sum-stat[4] / xtime
    ltmo2 = tt_sstat.sum-stat[5] / itime
  .

  find tt_sstat where tt_sstat.xid = 24 no-lock.
  assign
    evict1 = tt_sstat.sum-stat[4]
    evict2 = tt_sstat.sum-stat[5]
  .

  hr( input lr1, input osr1, output hr1-str, output hr1, output mr1 ).
  hr( input lr2, input osr2, output hr2-str, output hr2, output mr2 ).

  hr2-str = substring( hr2-str, 3 ).

  warn-areas = chkarea( 90.0, output worst-area ).

  ai_busy = chkai( output ai_exts, output ai_full, output ai_empty ).

  old-trx = 0.
  for each dictdb._Trans no-lock where _Trans-usrnum <> ? and _Trans-state <> "allocated":
    if _Trans-duration > old-trx then old-trx = _Trans-duration.
  end.

  ui-det( support, 1, 1,  1, "d01", string( hr1-str, "x(10)" )).
  ui-det( support, 1, 1,  2, "d02", string( mr1,     ">>>>9.999%" )).
  ui-det( support, 1, 1,  3, "d03", string( hr1,     ">>>>9.999%" )).
  ui-det( support, 1, 1,  4, "d04", string( lr1,     ">>>>>>>>>9" )).
  ui-det( support, 1, 1,  5, "d05", string( osr1,    ">>>>>>>>>9" )).
/***
  ui-det( support, 1, 1,  6, "d06", string( chkp1,   ">>>>>>>>>9" )).
  ui-det( support, 1, 1,  7, "d07", string( flsh1,   ">>>>>>>>>9" )).
 ***/
  ui-det( support, 1, 1,  6, "d06", string( RecRd1,   ">>>>>>>>>9" )).
  ui-det( support, 1, 1,  7, "d07", string( LogRec1,  ">>>>>>9.99" )).
  ui-det( support, 1, 1,  8, "d08", string( warn-areas, ">>>>>>>>>9" )).

  ui-det( support, 1, 1, 11, "d11", string( hr2-str, "x(8)" )).
  ui-det( support, 1, 1, 12, "d12", string( mr2,     ">>9.999%" )).
  ui-det( support, 1, 1, 13, "d13", string( hr2,     ">>9.999%" )).
  ui-det( support, 1, 1, 14, "d14", string( lr2,     ">>>>>>>9" )).
  ui-det( support, 1, 1, 15, "d15", string( osr2,    ">>>>>>>9" )).
/***
  ui-det( support, 1, 1, 16, "d16", string( chkp2,   ">>>>>>>9" )).
  ui-det( support, 1, 1, 17, "d17", string( flsh2,   ">>>>>>>9" )).
 ***/
  ui-det( support, 1, 1, 16, "d16", string( RecRd2,   ">>>>>>>9" )).
  ui-det( support, 1, 1, 17, "d17", string( LogRec2,  ">>>>9.99" )).
  ui-det( support, 1, 1, 18, "d18", string( worst-area, ">>>9.99%" )).

  ui-det( support, 1, 1, 21, "d21", string( trx1,    ">>>>>>>9" )).
  ui-det( support, 1, 1, 22, "d22", string( ltmo1,   ">>>>>>>9" )).
  ui-det( support, 1, 1, 23, "d23", string( _BuffStatus._BfStatus-TotBufs, ">>>>>>>9" )).
  ui-det( support, 1, 1, 24, "d24", string( evict1,  ">>>>>>>9" )).
  ui-det( support, 1, 1, 25, "d25", string( _Startup._Startup-LockTable, ">>>>>>>9" )).
  ui-det( support, 1, 1, 26, "d26", string( _DbStatus._DbStatus-MostLocks, ">>>>>>>9" )).
  ui-det( support, 1, 1, 27, "d27", string( oldbi, ">>>>>>>9" )).


  ui-det( support, 1, 1, 31, "d31", string( trx2,     ">>>>>>>9" )).
  ui-det( support, 1, 1, 32, "d32", string( ltmo2,    ">>>>>>>9" )).
  ui-det( support, 1, 1, 33, "d33", string( _BuffStatus._BfStatus-ModBuffs, ">>>>>>>9" )).
  ui-det( support, 1, 1, 34, "d34", string( evict2,   ">>>>>>>9" )).
  ui-det( support, 1, 1, 35, "d35", string( _DbStatus._DbStatus-NumLocks, ">>>>>>>9" )).
  ui-det( support, 1, 1, 36, "d36", string( old-trx,  "hh:mm:ss" )).
  ui-det( support, 1, 1, 37, "d37", string( currbi,   ">>>>>>>9" )).


  ui-det( support, 1, 1, 41, "d41", string( con-total, ">>>>9" )).
  ui-det( support, 1, 1, 42, "d42", string( con-self,  ">>>>9" )).
  ui-det( support, 1, 1, 43, "d43", string( con-remc,  ">>>>9" )).
  ui-det( support, 1, 1, 44, "d44", string( con-batch, ">>>>9" )).
  ui-det( support, 1, 1, 45, "d45", string( con-serv,  ">>>>9" )).
  ui-det( support, 1, 1, 46, "d46", string( con-other, ">>>>9" )).
  ui-det( support, 1, 1, 47, "d47", string( con-trx,   ">>>>9" )).
  ui-det( support, 1, 1, 48, "d48", string( con-block, ">>>>9" )).

  if ai_busy > 0 then
    do:
      ui-det( support, 1, 1, 28, "d28", string( ai_full, ">9" ) + " of " + string( ai_exts, ">9" )).
      ui-det( support, 1, 1, 38, "d38", fill( " ", 6 - integer( truncate( ai_busy / 10, 0 ))) + "a" + string( ai_busy )).
    end.
   else
    do:
      find first _Logging no-lock.
      if _Logging-AiJournal = "yes" then
        ui-det( support, 1, 1, 28, "d28", "Enabled" ).
       else
        ui-det( support, 1, 1, 28, "d28", "Disabled" ).
    end.

  ui-det( support, 1, 1, 10, "d10", string( time,  "hh:mm:ss" )).
  ui-det( support, 1, 1, 20, "d20", string( today, "99/99/99" )).
  ui-det( support, 1, 1, 40, "d40", string( dname, "x(80)" )).

  define variable x as integer   no-undo.
  define variable z as integer   no-undo.
  define variable r as character no-undo case-sensitive.
  define variable s as character no-undo case-sensitive.

  publish "get-RateRaw" ( output r ).
  publish "get-SumSample" ( output s ).

  if s = "S" then
    s = "Summary".
   else
    s = "Sample".

  if r = "R" then
    r = " Raw".
   else
    r = "Rate".

  ui-det( support, 1, 1, 30, "d30", string( s,  "x(8)" )).
  ui-det( support, 1, 1, 50, "d50", string( r,  "x(4)" )).

  release _Startup.
  release _DbStatus.
  release _BuffStatus.

  {tms_support/protop/local/mon/summary.i}

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
