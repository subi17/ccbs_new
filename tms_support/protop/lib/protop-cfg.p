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
 * protop-cfg.p
 *
 *
 * Initialize global shared variables configured in etc/protop.cfg
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	June 15, 2006
 *
 * 	Moved responsibility for configuration of log files and alert
 *	e-mail addresses from lib/protop.i to lib/protop-cfg.p
 *	June 15, 2006
 * 
 */

{protop/lib/protop.i}

define variable i as integer no-undo.

define variable k as character no-undo.
define variable v as character no-undo.

/* load protop.cfg 
 *
 */

if search( "etc/protop.cfg" ) = ? then
  do:

    assign
      pt_logdir   = "/tmp"
      pt_logname  = "&5.&2.&3"
      pt_mailcmd  = ""				/* "mailx -s ""&1"" "	*/
      pt_alert    = ""
      pt_alarm    = ""
      pt_pager    = ""
      pt_xfertime = "23:30"
    .

  end.
 else
  do:

    input from value( search( "etc/protop.cfg" )).

    repeat on endkey undo, leave:

      assign
        k = ""
        v = ""
      .

      import k v.

      case k:
        when "logdir"   then pt_logdir   = v.
        when "logname"  then pt_logname  = v.
        when "mailcmd"  then pt_mailcmd  = v.
        when "alert"    then pt_alert    = v.
        when "alarm"    then pt_alarm    = v.
        when "pager"    then pt_pager    = v.
        when "xfertime" then pt_xfertime = v.
      end.

    end.

    input close. 

  end.

do i = 1 to num-entries( pt_xfertime ):
  pt_xfertm[i] =
   ( 3600 * integer( entry( 1, entry( i, pt_xfertime ), ":" ))) +
   (   60 * integer( entry( 2, entry( i, pt_xfertime ), ":" )))
  .
end.

return.
