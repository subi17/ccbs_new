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
 * netstat.p
 *
 *
 * Network Statistics (Specific to Linux).
 *
 * $ netstat -i
 * Kernel Interface table
 * Iface   MTU Met   RX-OK RX-ERR RX-DRP RX-OVR   TX-OK TX-ERR TX-DRP TX-OVR Flg
 * eth0   1500   0 1210278      0      0      0  302055      0      0      0 BMRU
 * lo    16436   0   10585      0      0      0   10585      0      0      0 LRU
 * 
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	November 6, 2003
 *
 */

{protop/lib/protop.i}

define variable support as character no-undo initial "Network Statistics".

define temp-table tt_netstat no-undo
  field dev         as character
  field xvalid      as logical
  field pckt-rx     as integer extent 5
  field pckt-rx-err as integer extent 5
  field pckt-tx     as integer extent 5
  field pckt-tx-err as integer extent 5
  index dev-idx is unique primary dev.

procedure update_netstat:

  define input parameter p_dev         as character no-undo.
  define input parameter p_pckt-rx     as integer   no-undo.
  define input parameter p_pckt-rx-err as integer   no-undo.
  define input parameter p_pckt-tx     as integer   no-undo.
  define input parameter p_pckt-tx-err as integer   no-undo.

  find tt_netstat exclusive-lock where tt_netstat.dev = p_dev no-error.

  if not available tt_netstat then
    do:
      create tt_netstat.
      assign
        tt_netstat.dev = p_dev
        {protop/lib/init-xrec.i tt_netstat.pckt-rx     p_pckt-rx}
        {protop/lib/init-xrec.i tt_netstat.pckt-rx-err p_pckt-rx-err}
        {protop/lib/init-xrec.i tt_netstat.pckt-tx     p_pckt-tx}
        {protop/lib/init-xrec.i tt_netstat.pckt-tx-err p_pckt-tx-err}
      .
    end.

  assign
    tt_netstat.xvalid = yes
/***
    {protop/lib/upd-xrec.i tt_netstat.pckt-rx     p_pckt-rx}
    {protop/lib/upd-xrec.i tt_netstat.pckt-rx-err p_pckt-rx-err}
    {protop/lib/upd-xrec.i tt_netstat.pckt-tx     p_pckt-tx}
    {protop/lib/upd-xrec.i tt_netstat.pckt-tx-err p_pckt-tx-err}
 ***/
    tt_netstat.pckt-rx[3] =     p_pckt-rx
    tt_netstat.pckt-rx-err[3] = p_pckt-rx-err
    tt_netstat.pckt-tx[3] =     p_pckt-tx
    tt_netstat.pckt-tx-err[3] = p_pckt-tx-err
  .

  return.

end.

procedure age_netstat:

  for each tt_netstat exclusive-lock:

    if tt_netstat.xvalid = no then
      delete tt_netstat.
     else
      assign
        tt_netstat.xvalid = no
        {protop/lib/upd-xrec.i tt_netstat.pckt-rx     tt_netstat.pckt-rx[3]}
        {protop/lib/upd-xrec.i tt_netstat.pckt-rx-err tt_netstat.pckt-rx-err[3]}
        {protop/lib/upd-xrec.i tt_netstat.pckt-tx     tt_netstat.pckt-tx[3]}
        {protop/lib/upd-xrec.i tt_netstat.pckt-tx-err tt_netstat.pckt-tx-err[3]}
      .

  end.

  return.

end.

/* restart
 *
 */

procedure mon-restart:

  empty temp-table tt_netstat.

  delete procedure this-procedure.

  return.

end.

/* initialize
 *
 */

procedure mon-init:

  empty temp-table tt_netstat.

  /* define labels
   *
   */

  ui-define-label( support, 1, 1, "Dev",   "Interface           " ).
  ui-define-label( support, 1, 2, "Rx",    "   Received" ).
  ui-define-label( support, 1, 3, "RxErr", " Rcvd - Err" ).
  ui-define-label( support, 1, 4, "Tx",    "Transmitted" ).
  ui-define-label( support, 1, 5, "TxErr", "Trans - Err" ).

  return.

end.

/* update
 *
 */

procedure mon-update:

  define variable xline as character no-undo.
  define variable s     as character no-undo.

  define variable xdev         as character no-undo.
  define variable xpckt-rx     as integer   no-undo.
  define variable xpckt-rx-err as integer   no-undo.
  define variable xpckt-tx     as integer   no-undo.
  define variable xpckt-tx-err as integer   no-undo.

  if do-update( support ) = no then return.

  input through value( "netstat -i" ).

  import ^.	/* eat the header line	*/
  import ^.	/* eat the header line	*/

  /* There are formatting problems with large numbers :(
   *
   * Overflowing counters eliminate spaces between fields and shift the fields so
   * that you cannot do a simple "import" -- you have to parse the line.  This
   * works for me (so far) but it's hard to be certain because it takes a while
   * to generate enough traffic to test.
   *
   */

  repeat:

    xline  = "".

    import delimiter "~n" xline.

    xdev = substring( xline, 1, index( xline, " " ) - 1 ).

    if substring( xline, 20, 1 ) <> " " then
      assign
        s = substring( xline, 20, 9 )
        xline = substring( xline, 29 )
      .
     else
      assign
        s = substring( xline, 21, 7 )
        xline = substring( xline, 28 )
      .

    assign
      xpckt-rx = integer( s )
      xpckt-rx-err = integer( substring( xline, 1, 7 ))
      xline = substring( xline, 22 )
    .

    if substring( xline, 1, 1 ) <> " " then
      assign
        s = substring( xline, 1, 9 )
        xline = substring( xline, 10 )
      .
     else
      assign
        s = substring( xline, 2, 7 )
        xline = substring( xline, 9 )
      .

    assign
      xpckt-tx = integer( s )
      xpckt-tx-err = integer( substring( xline, 1, 7 ))
    .

    run update_netstat(
      input xdev,
      input xpckt-rx,
      input xpckt-rx-err,
      input xpckt-tx,
      input xpckt-tx-err
    ).

  end.

  input close.

  run age_netstat.

  define variable i as integer no-undo.
  define variable x as integer no-undo.
  define variable z as integer no-undo.

  do-display( support, 1, 100, 11, 1, "Network Interface Statistics" ).

  do-SumSample( output x, output z ).

  define query q for tt_netstat.
  open query q for each tt_netstat no-lock.

  do while true:

    get next q.

    if not available tt_netstat then leave.

    i = i + 1.

    ui-det( support, 1, i,  1, "Dev",   string( tt_netstat.dev, "x(20)" )).
    ui-det( support, 1, i,  2, "Rx",    string( ( tt_netstat.pckt-rx[x] / z ),     ">>>>>>>>>>9" )).
    ui-det( support, 1, i,  3, "RxErr", string( ( tt_netstat.pckt-rx-err[x] / z ), ">>>>>>>>>>9" )).
    ui-det( support, 1, i,  4, "Tx",    string( ( tt_netstat.pckt-tx[x] / z ),     ">>>>>>>>>>9" )).
    ui-det( support, 1, i,  5, "TxErr", string( ( tt_netstat.pckt-tx-err[x] / z ), ">>>>>>>>>>9" )).

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
