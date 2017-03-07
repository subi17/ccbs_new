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
 * alert.p
 *
 *
 * Process Alerts
 *
 *	Events are fired when an alert threshold test succeeds (or fails if
 *	you're a "half empty" sort of person...) inside the ui-det() function.
 *	The criteria for the alert and the name of the event(s) to fire are
 *	specified in protop/etc/alert.cfg.
 *
 *	These events are provided as examples -- you are not restricted to
 *	this set of events.  Feel free to create additional events.  Event
 *	names are user-defined.  All you need is a loadable module that will
 *	respond to your event.
 *
 *	If you create an operating system specific event it should be in
 *	the os-specific sub-directory of protop/os.  There is, for instance,
 *	a simple e-mail alert in /protop/os/rh8/alert.p
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

define stream alert-stream.

/* restart
 *
 */

procedure mon-restart:

  delete procedure this-procedure.

  return.

end.

/* just write the text to the message area
 *
 */

procedure alert-message:

  define input parameter alert-text   as character no-undo.
  define input parameter alert-metric as character no-undo.

  if session:batch = yes then return.

  hide message no-pause.
  message alert-text.

  return.

end.


/* append the text plus the date & time to a log file
 *
 */

procedure alert-log:

  define input parameter alert-text   as character no-undo.
  define input parameter alert-metric as character no-undo.

  define variable lfname as character no-undo.

  lfname = subtmpl( pt_logdir, today, "" ) + "/" + subtmpl( pt_logname, today, "protop" ) + ".log".

  output to value( lfname ) append.
  put unformatted alert-text skip.
  output close.

  return.

end.


/* send out an alert e-mail
 *
 */

procedure alert-mail:

  define input parameter alert-text   as character no-undo.
  define input parameter alert-metric as character no-undo.

  define variable i       as integer   no-undo.
  define variable tmpname as character no-undo.

  if pt_alert = "" then return.

  RUN adecomm/_tmpfile.p ( "", "", output tmpname ).

  output stream alert-stream to value( tmpname ).

  put stream alert-stream unformatted alert-text skip.
  output stream alert-stream close.

  do i = 1 to num-entries( pt_alert ):
    os-command silent value(
      substitute( pt_mailcmd, "Alert: " + alert-metric ) +
      entry( i, pt_alert ) + " < " + tmpname
    ).
  end.

  os-delete value( tmpname ).

  return.

end.


/* send out an alarm e-mail
 *
 */

procedure alarm-mail:

  define input parameter alert-text   as character no-undo.
  define input parameter alert-metric as character no-undo.

  define variable i       as integer   no-undo.
  define variable tmpname as character no-undo.

  if pt_alarm = "" then return.

  RUN adecomm/_tmpfile.p ( "", "", output tmpname ).

  output stream alert-stream to value( tmpname ).

  put stream alert-stream unformatted alert-text skip.
  output stream alert-stream close.

  do i = 1 to num-entries( pt_alarm ):
    os-command silent value(
      substitute( pt_mailcmd, "Alarm: " + alert-metric ) +
      entry( i, pt_alarm ) + " < " + tmpname
    ).
  end.

  os-delete value( tmpname ).

  return.

end.


/* send out a page
 *
 */

procedure alert-page:

  define input parameter alert-text   as character no-undo.
  define input parameter alert-metric as character no-undo.

  define variable i       as integer   no-undo.
  define variable tmpname as character no-undo.

  if pt_pager = "" then return.

  RUN adecomm/_tmpfile.p ( "", "", output tmpname ).

  output stream alert-stream to value( tmpname ).

  put stream alert-stream unformatted alert-text skip.
  output stream alert-stream close.

  do i = 1 to num-entries( pt_pager ):
    os-command silent value(
      substitute( pt_mailcmd, "Page: " + alert-metric ) +
      entry( i, pt_pager ) + " < " + tmpname
    ).
  end.

  os-delete value( tmpname ).

  return.

end.

/** Initialize PP
 **
 **/

subscribe to "mon-restart"   anywhere run-procedure "mon-restart".

subscribe to "alert-message" anywhere run-procedure "alert-message".
subscribe to "alert-log"     anywhere run-procedure "alert-log".

subscribe to "alert-mail"    anywhere run-procedure "alert-mail".
subscribe to "alarm-mail"    anywhere run-procedure "alarm-mail".
subscribe to "alert-page"    anywhere run-procedure "alert-page".

return.
