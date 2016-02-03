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
 * cnx.p
 *
 *
 * Session Breakdown by Connection Type (sort of)
 *
 * Sessions are categorized using _Connect-Type, _Connect-Device and _Connect-name
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	May 22, 2006
 *
 */

{tms_support/protop/lib/protop.i}

&global-define	EXT	101

define variable support as character no-undo initial "Connection Summary".

define variable i as integer no-undo.

define variable log-rd       as integer no-undo extent {&EXT}.
define variable os-rd        as integer no-undo extent {&EXT}.
define variable os-wr        as integer no-undo extent {&EXT}.
define variable old_log-rd   as integer no-undo extent {&EXT}.
define variable old_os-rd    as integer no-undo extent {&EXT}.
define variable old_os-wr    as integer no-undo extent {&EXT}.
define variable u            as integer no-undo extent {&EXT}.
define variable t            as integer no-undo extent {&EXT}.
define variable b            as integer no-undo extent {&EXT}.
define variable all_label    as character no-undo initial "All Sessions".

define temp-table tt_hit-ratio no-undo
  field xid        as integer
  field xlabel     as character
  field users      as integer
  field xlog-rd    as integer
  field xos-rd     as integer
  field xos-wr     as integer
  field hit-ratio  as decimal
  field trx        as integer
  field blk        as integer
  index xidx-rdx is unique primary xid descending.

define temp-table tt_cnx_cfg no-undo
  field e_order     as integer
  field d_order     as integer
  field xlabel      as character
  field c_type      as character
  field c_device    as character
  field c_name      as character
.


function mk-tt_hr returns logical ( input id as integer, input xname as character ):

  create tt_hit-ratio.
  assign
    tt_hit-ratio.xid    = id
    tt_hit-ratio.xlabel = xname
    log-rd[id] = 0
    os-rd[id]  = 0
    os-wr[id]  = 0
    u[id]      = 0
    t[id]      = 0
    b[id]      = 0
  .

  return yes.

end.


function upd-tt_hr returns logical ( input id as integer, input logr as integer, input osr as integer, input osw as integer, input trxid as integer, input w as character ):

  assign
    log-rd[id] = log-rd[id] + logr
    os-rd[id]  = os-rd[id]  + osr
    os-wr[id]  = os-wr[id]  + osw
    u[id]      = u[id] + 1
    t[id]      = t[id] + ( if trxid > 0 then 1 else 0 )
    b[id]      = b[id] + ( if w <> " -- " then 1 else 0 )
  .

  return yes.

end.


function sum-tt_hr returns logical ( input id as integer ):

  define variable x as decimal   no-undo.		/* throw away output parameter	*/
  define variable d as character no-undo.		/* throw away output parameter	*/

  find tt_hit-ratio exclusive-lock where tt_hit-ratio.xid = id no-error.

  if not available tt_hit-ratio then return no.

  assign
    tt_hit-ratio.xlog-rd    = max( 0, ( log-rd[id] - old_log-rd[id] ))
    tt_hit-ratio.xos-rd     = max( 0, ( os-rd[id]  - old_os-rd[id]  ))
    tt_hit-ratio.xos-wr     = max( 0, ( os-wr[id]  - old_os-wr[id]  ))
    tt_hit-ratio.hit-ratio  = hr( input tt_hit-ratio.xlog-rd, input tt_hit-ratio.xos-rd, output d, output x, output x )
    tt_hit-ratio.users      = u[id]
    tt_hit-ratio.trx        = t[id]
    tt_hit-ratio.blk        = b[id]
    old_log-rd[id]          = log-rd[id]
    old_os-rd[id]           = os-rd[id]
    old_os-wr[id]           = os-wr[id]
  .

  if tt_hit-ratio.hit-ratio = ? then tt_hit-ratio.hit-ratio = 0.

  return yes.

end.


function cnx_category returns integer ( input p_type as character, input p_device as character, input p_name as character ):

  for each tt_cnx_cfg no-lock where tt_cnx_cfg.d_order > 1 and tt_cnx_cfg.d_order < {&EXT}:
    if can-do( tt_cnx_cfg.c_type,   p_type )   and
       can-do( tt_cnx_cfg.c_device, p_device ) and
       can-do( tt_cnx_cfg.c_name,   p_name )   then return tt_cnx_cfg.d_order.
  end.

  return {&EXT}.

end.


/* restart
 *
 */

procedure mon-restart:

  empty temp-table tt_hit-ratio.
  empty temp-table tt_cnx_cfg.

  delete procedure this-procedure.

  return.

end.

/* initialize
 *
 */

procedure mon-init:

  empty temp-table tt_hit-ratio.
  empty temp-table tt_cnx_cfg.

  define variable d as character no-undo extent 8.
  define variable cfg_file as character no-undo.

  cfg_file = search( "etc/cnx.cfg" ).

  if cfg_file <> ? then
    do:

      input from value( cfg_file ).

      repeat on endkey undo, leave:

        d = "".

        import d.

        if d[1] begins "#" or d[1] = "" then
          next.
         else
          do:
            create tt_cnx_cfg.
            assign
              tt_cnx_cfg.e_order  = integer( d[1] )	/* should be > 1 and < {&EXT} ...				*/
              tt_cnx_cfg.d_order  = integer( d[2] )	/* should be > 1 and < {&EXT} ...				*/
              tt_cnx_cfg.xlabel   = d[3]
              tt_cnx_cfg.c_type   = d[4]		/* these values could (should) be scrubbed to be CAN-DO safe	*/
              tt_cnx_cfg.c_device = d[5]
              tt_cnx_cfg.c_name   = d[6]
            no-error.
            if error-status:num-messages > 0 then
              delete tt_cnx_cfg.
             else if (( tt_cnx_cfg.e_order <= 1 ) or ( tt_cnx_cfg.e_order >= {&EXT} )) or
                     (( tt_cnx_cfg.d_order <= 1 ) or ( tt_cnx_cfg.d_order >= {&EXT} )) then
              do:
                if tt_cnx_cfg.d_order = 1 then all_label = d[3].
                delete tt_cnx_cfg.
              end.
          end.

      end.

      input close.

    end.

  /* define labels
   *
   */

  ui-define-label( support, 1, 1, "xType",  " Session Type" ).
  ui-define-label( support, 1, 2, "xUsers",         "Users" ).
  ui-define-label( support, 1, 3, "xLogRd",     "   Log Rd" ).
  ui-define-label( support, 1, 4, "xOsRd",      "    OS Rd" ).
/*ui-define-label( support, 1, 5, "xLogWr",     "   Log Wr" ). */
  ui-define-label( support, 1, 6, "xOsWr",        "  OS Wr" ).
  ui-define-label( support, 1, 7, "xHR",         "    Hit%" ).
  ui-define-label( support, 1, 8, "xTRX",          "   Trx" ).
  ui-define-label( support, 1, 9, "xBLK",           " Blkd" ).

  return.

end.

/* update
 *
 */

procedure mon-update:

  define variable x as decimal no-undo.
  define variable y as integer no-undo.
  define variable z as integer no-undo.

  if do-update( support ) = no then return.

  empty temp-table tt_hit-ratio.

  find tt_cnx_cfg no-lock where tt_cnx_cfg.d_order = 1 no-error.
  mk-tt_hr( 1, ( if available tt_cnx_cfg then tt_cnx_cfg.xlabel else all_label )).

  for each tt_cnx_cfg no-lock by tt_cnx_cfg.d_order:
    mk-tt_hr( tt_cnx_cfg.d_order, tt_cnx_cfg.xlabel ).
  end.

  find tt_cnx_cfg no-lock where tt_cnx_cfg.d_order = {&EXT} no-error.
  mk-tt_hr( {&EXT}, ( if available tt_cnx_cfg then tt_cnx_cfg.xlabel else "Other" )).

  for each dictdb._Connect no-lock where _Connect-usr >= 0,
      dictdb._UserIO no-lock where _UserIO-id = _Connect-id:

    assign
      x = x + 1
      y = y + ( if _Connect-TransId > 0 then 1 else 0 )
      z = z + ( if _Connect-wait <> " -- " then 1 else 0 )
    .

    upd-tt_hr( cnx_category( _Connect-Type, _Connect-Device, _Connect-Name ), _UserIO-dbaccess, _UserIO-dbread, _UserIO-dbwrite, _Connect-TransId, _Connect-wait ).

  end.

  find dictdb._ActBuffer no-lock.
  upd-tt_hr( 1, _ActBuffer._Buffer-LogicRds, _ActBuffer._Buffer-OSRds, _ActBuffer._Buffer-OSWrts, 0, " -- " ).
  release dictdb._ActBuffer.

  assign
    u[1] = x	/* since we don't actually accumulate u[1] above we need to cheat...	*/
    t[1] = y
    b[1] = z
  .

  do i = 1 to {&EXT}:
    sum-tt_hr( i ).
  end.

  /* "Helpers" is special -- the hit ratio is the ratio of writes performed by the helpers vs the total # of writes
   *
   */

  find tt_cnx_cfg no-lock where tt_cnx_cfg.xlabel = "Helpers" no-error.
  if available( tt_cnx_cfg ) then
    do:

      find tt_hit-ratio no-lock where tt_hit-ratio.xid = 1 no-error.
      x = tt_hit-ratio.xos-wr.

      find tt_hit-ratio exclusive-lock where tt_hit-ratio.xid = tt_cnx_cfg.d_order no-error.
      if available tt_hit-ratio then
        tt_hit-ratio.hit-ratio = ( if x > 0 then (( tt_hit-ratio.xos-wr / x ) * 100 ) else 0 ).

    end.

  /* finally ;-)
   */

  do-display( support, 1, 100, 11, 1, "Connection Summary" ).

  i = 0.
  for each tt_hit-ratio no-lock by xid:

    i = i + 1.

    ui-det( support, 1, i,  1, "xType",  string( tt_hit-ratio.xlabel,                 "x(13)" )).
    ui-det( support, 1, i,  2, "xusers", string( tt_hit-ratio.users ,                 ">>>>9" )).
    ui-det( support, 1, i,  3, "xLogRd", string(( tt_hit-ratio.xlog-rd / itime ), ">>>>>>>>9" )).
    ui-det( support, 1, i,  4, "xOsRd",  string(( tt_hit-ratio.xos-rd  / itime ), ">>>>>>>>9" )).
    ui-det( support, 1, i,  5, "xOsWr",  string(( tt_hit-ratio.xos-wr  / itime ),   ">>>>>>9" )).
    ui-det( support, 1, i,  6, "xHR",    string( tt_hit-ratio.hit-ratio,           "->>9.99%" )).
    ui-det( support, 1, i,  7, "xTRX",   string( tt_hit-ratio.trx,                   ">>>>>9" )).
    ui-det( support, 1, i,  8, "xBLK",   string( tt_hit-ratio.blk,                    ">>>>9" )).

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
