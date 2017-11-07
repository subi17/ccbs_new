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
 * config.p
 *
 *
 * Show configuration options and startup parameters
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	July 23, 2006
 *
 */

{protop/lib/protop.i}

define variable support as character no-undo initial "Config and Startup".

{protop/lib/tt_xstat.i}

ratio-calc = 1.

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

  ui-define-label( support, 1, 1, "xname",   "                              " ).
  ui-define-label( support, 1, 2, "config1", "" ).

  return.

end.

/* update
 *
 */

procedure mon-update:

  define variable i     as integer no-undo.
  define variable xspin as integer no-undo.

  if do-update( support ) = no then return.

  find dictdb._Startup no-lock no-error.
  find first dictdb._dbStatus no-lock.

  if _Startup-spin < 0 then
    xspin = _Startup-spin + 65536.
   else
    xspin = _Startup-spin.

  run update_xstat (  1, "Progress Version:", "", "", 0, xversion( _dbStatus-ShmVers ), 0, 0 ).
  run update_xstat (  2, "   DB Block Size:", "", "", 0, _dbStatus-dbBlkSize,   0, 0 ).
  run update_xstat (  3, "   BI Block Size:", "", "", 0, _dbStatus-biBlkSize,   0, 0 ).
  run update_xstat (  4, " BI Cluster Size:", "", "", 0, _dbStatus-biClSize * _dbStatus-biBlkSize / 1024,    0, 0 ).
  run update_xstat (  5, "   AI Block Size:", "", "", 0, _dbStatus-aiBlkSize,   0, 0 ).
  run update_xstat (  6, "           -spin:", "", "", 0, xspin,                 0, 0 ).
  run update_xstat (  7, "       -directio:", "", "", 0, _Startup-directio,     0, 0 ).
  run update_xstat (  8, "              -i:", "", "", 0, _Startup-CrashProt,    0, 0 ).
  run update_xstat (  9, "              -r:", "", "", 0, _Startup-BiIO,         0, 0 ).
  run update_xstat ( 10, "              -n:", "", "", 0, _Startup-MaxUsers - 1, 0, 0 ).

  do-display( support, 1, 100, 11, 1, "Config and Startup" ).

  define query q for tt_xstat.
  open query q for each tt_xstat no-lock by tt_xstat.xid.

  do while true:

    get next q.

    if not available tt_xstat then leave.

    if tt_xstat.xname = "" then next.

    i = i + 1.

    ui-det( support, 1, i, 1, "xname",   string( tt_xstat.xname, "x(20)" )).
    case i:
      when  1 then ui-det( support, 1, i, 2, "config1", string( tt_xstat.stat2[1], ">>>>>9.9" )).
      when  2 then ui-det( support, 1, i, 2, "config1", string( tt_xstat.stat2[1], ">>>>>>>9" )).
      when  3 then ui-det( support, 1, i, 2, "config1", string( tt_xstat.stat2[1], ">>>>>>>9" )).
      when  4 then ui-det( support, 1, i, 2, "config1", string( tt_xstat.stat2[1], ">>>>>>>9" )).
      when  5 then ui-det( support, 1, i, 2, "config1", string( tt_xstat.stat2[1], ">>>>>>>9" )).
      when  6 then ui-det( support, 1, i, 2, "config1", string( tt_xstat.stat2[1], ">>>>>>>9" )).
      when  7 then ui-det( support, 1, i, 2, "config1", ( if tt_xstat.stat2[1] = 0 then "      No" else "     Yes" )).
      when  8 then ui-det( support, 1, i, 2, "config1", ( if tt_xstat.stat2[1] = 0 then "      -i" else "Reliable" )).
      when  9 then ui-det( support, 1, i, 2, "config1", ( if tt_xstat.stat2[1] = 0 then "     Raw" else "Buffered" )).
      when 10 then ui-det( support, 1, i, 2, "config1", string( tt_xstat.stat2[1], ">>>>>>>9" )).
    end.

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
