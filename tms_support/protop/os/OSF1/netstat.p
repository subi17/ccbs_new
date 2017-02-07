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
 * Network Statistics (Specific to Tru64).
 *
 *
 * $ netstat -i
 * Name  Mtu   Network     Address               Ipkts Ierrs    Opkts Oerrs  Coll
 * bcm0  1500  <Link>      00:10:18:0c:4d:0a  6312796924     0 6349375456     0     0
 * bcm0  1500  DLI         none               6312796924     0 6349375456     0     0
 * bcm0  1500  10.15       goober             6312796924     0 6349375456     0     0
 * bcm0  1500  10.15       barney             6312796924     0 6349375456     0     0
 * bcm1* 1500  <Link>      00:10:18:0c:4d:d8         0     0        0     0     0
 * ee0*  1500  <Link>      00:13:21:08:03:7a         0     0        0     0     0
 * ee1*  1500  <Link>      00:13:21:08:03:7b         0     0        0     0     0
 * ee2   1500  <Link>      00:13:21:08:06:4c  10795480241     1 10815403022     0     0
 * ee2   1500  10.1.0      member1-icstcp0    10795480241     1 10815403022     0     0
 * ee3*  1500  <Link>      00:13:21:08:06:4d         0     0        0     0     0 
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	November 6, 2003
 *
 */

{tms_support/protop/lib/protop.i}

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
        {tms_support/protop/lib/init-xrec.i tt_netstat.pckt-rx     p_pckt-rx}
        {tms_support/protop/lib/init-xrec.i tt_netstat.pckt-rx-err p_pckt-rx-err}
        {tms_support/protop/lib/init-xrec.i tt_netstat.pckt-tx     p_pckt-tx}
        {tms_support/protop/lib/init-xrec.i tt_netstat.pckt-tx-err p_pckt-tx-err}
      .
    end.

  assign
    tt_netstat.xvalid = yes
/***
    {tms_support/protop/lib/upd-xrec.i tt_netstat.pckt-rx     p_pckt-rx}
    {tms_support/protop/lib/upd-xrec.i tt_netstat.pckt-rx-err p_pckt-rx-err}
    {tms_support/protop/lib/upd-xrec.i tt_netstat.pckt-tx     p_pckt-tx}
    {tms_support/protop/lib/upd-xrec.i tt_netstat.pckt-tx-err p_pckt-tx-err}
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
        {tms_support/protop/lib/upd-xrec.i tt_netstat.pckt-rx     tt_netstat.pckt-rx[3]}
        {tms_support/protop/lib/upd-xrec.i tt_netstat.pckt-rx-err tt_netstat.pckt-rx-err[3]}
        {tms_support/protop/lib/upd-xrec.i tt_netstat.pckt-tx     tt_netstat.pckt-tx[3]}
        {tms_support/protop/lib/upd-xrec.i tt_netstat.pckt-tx-err tt_netstat.pckt-tx-err[3]}
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
  define variable xaddr        as character no-undo.
  define variable xpckt-rx     as integer   no-undo.
  define variable xpckt-rx-err as integer   no-undo.
  define variable xpckt-tx     as integer   no-undo.
  define variable xpckt-tx-err as integer   no-undo.

  if do-update( support ) = no then return.

  input through value( "netstat -i" ).

  import ^.	/* eat the header line	*/

  repeat:

    xline  = "".

    import xdev ^ ^ xaddr xpckt-rx xpckt-rx-err xpckt-tx xpckt-tx-err.

    /*** if num-entries( xaddr, "." ) < 2 then next. ***/

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
