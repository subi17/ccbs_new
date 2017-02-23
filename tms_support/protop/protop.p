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
 * protop.p
 *
 *
 * Program to interactively monitor various Progress database statistics
 * a la "top".
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	August 28, 2003
 *
 *
 * History:
 *
 *	Accepted changes from Sam Paakki regarding "quit" function and PROPATH
 *	September 26, 2003
 *
 *	Accepted changes from Patrick Tingen modifying html output, adding <esc> as
 *	a "quit" key and adding a "splash" message during initialization.
 *	October 30, 2003
 *
 *	Modified to support Windows GUI functionality.
 *	December 10, 2004
 *
 */

{../tms_support/protop/lib/protop.i}

define variable c as character no-undo case-sensitive.
define variable n as integer   no-undo.

define variable curr-disp   as character no-undo.
define variable doWeb       as logical   no-undo.
define variable keepRunning as logical   no-undo initial TRUE.

/* ProTop might not be running as a stand-alone program.
 *
 */

procedure quitme:	/* If there is no parent procedure, we QUIT, otherwise drop out of the main loop & RETURN */

  empty temp-table tt_ui-hdr.
  empty temp-table tt_ui-sch.
  empty temp-table tt_ui-det.

  keepRunning = FALSE.		/* drop out of the event loop...	*/

  if "{&window-system}" <> "tty" then
    apply "window-close" to this-procedure.
   else
    if program-name(4) = ? then QUIT.

  return.

end.

/** ProTop Main Body
 **
 **/

define variable hWin as handle no-undo.

if "{&window-system}" = "tty" then message 'Initializing ProTop release {&protop-version}.'.

RUN tms_support/protop/lib/protoplib.p persistent.
RUN tms_support/protop/lib/vstlib.p persistent.
RUN tms_support/protop/lib/ui-lib.p persistent.
RUN tms_support/protop/lib/monitor.p persistent.
RUN tms_support/protop/lib/displist.p persistent.
RUN tms_support/protop/lib/command.p persistent.

RUN tms_support/protop/out/summary.p persistent.

if {&html-on} then
  do:

    file-info:filename = "{&html-dir}".
    if file-info:full-pathname = ? then
      publish "set-html-dir" ( input session:temp-dir + "/" ).
     else
      publish "set-html-dir" ( input file-info:full-pathname + "/" ).

    RUN out/html.p persistent.

  end.

subscribe to "command-quit" anywhere run-procedure "quitme".		/* SMP */

publish "command-line".
publish "mon-load".
publish "mon-define".

/** Psuedo VST field
 **
 ** find out what the base for the checkpoint counter is
 **
 ** we need an active transaction to do this!
 **
 **/

&if {&map-chkp} = "yes" &then

do transaction:
  create dictdb._user.
  find first dictdb._Trans no-lock where _Trans-num = dbtaskid( "dictdb" ).
  find _BuffStatus no-lock.
  chkp-base = _Trans-counter - _BfStatus-LastCkpNum.
  undo.
end.

&endif

publish "get-curr-disp" ( output curr-disp ).
if curr-disp = "" then publish "set-curr-disp" ( input "Summary" ).

on any-printable of this-procedure do:
  c = chr( lastkey ).
  publish "process-command" ( input c, "" ).
  return no-apply.
end.

publish "mon-init".
init-timer().

/* the monitoring loop
 *
 */

hide message no-pause. 
etime( yes ).

do while keepRunning:

  empty temp-table tt_ui-det.

  pause 0 before-hide.		/* some things just aren't worth chasing... */
  hide message no-pause.

  protop-time = etime.

  upd-timer().
  publish "mon-update".
  protop-upd = etime - protop-time.
  protop-time = etime.

  publish "vdisplay".
  protop-chui = etime - protop-time.
  protop-time = etime.

  publish "get-doWeb" ( output doWeb ).
  if doWeb then publish "html-display".
  protop-html = etime - protop-time.
  protop-time = etime.

  /* Wait interval for next sample or process a user command if something is typed
   *
   */		  /*** message stime ltime xtime itime. ***/

  publish "get-interval" ( output n ).		/* it might change...	*/

  /* This is almost too much fun...
   *
   */

  readkey pause n.
  apply lastkey to this-procedure.
  process events.

  /* The above works much better than a WAIT-FOR PAUSE N for the most
   * part.  It does not, however, seem to work for WINDOW-CLOSE events
   * (my guess is that it doesn't catch any "GUI" events).  Which is
   * unfortunate.
   *
   */

end.

quit.

return.
