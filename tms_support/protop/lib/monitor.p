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
 * monitor.p
 *
 *
 * Procedure to manage global attributes of ProTop.
 *
 *
 * Known Bugs & Issues:
 *
 *
 * To Do:
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	September 23, 2003
 *
 * History:
 *
 *	Accepted changes from Patrick Tingen to set html output dir and
 *	eliminating the nasty curr-page shared variable.
 *	October 30, 2003
 *
 *	Removed opsys-dir and replaced reference in lib/monitor.p with a
 *	check of "uname -a".  Renamed the os/ directories to match result
 *	of uname -a.
 *	May 2, 2006
 * 
 * 	Moved responsibility for configuration of log files and alert
 *	e-mail addresses from protop.i to monitor.p
 *	June 13, 2006
 * 
 */

{tms_support/protop/lib/protop.i}

/* Attributes that we're taking responsibility for.
 *
 * Be careful with quoted quotes.
 *
 *
 */

{tms_support/protop/lib/attribute.i curr-disp      character "format 'x(20)'"  "Display:"}
{tms_support/protop/lib/attribute.i disp-type-list character "format 'x(70)'"  "Available Display Types:"}

{tms_support/protop/lib/attribute.i examine-item   integer   "format '>>>>>9'" "Examine:"       "integer"}
{tms_support/protop/lib/attribute.i interval       integer   "initial 5"       "New interval:"  "integer" "interval = max( interval, 1 )."}
{tms_support/protop/lib/attribute.i sort-criteria  character "initial 'i' case-sensitive" "Sort criteria"}
{tms_support/protop/lib/attribute.i tlist          character "format 'x(70)'"  "Track:"}
{tms_support/protop/lib/attribute.i xlist          character "format 'x(70)'"  "eXclude:"}
{tms_support/protop/lib/attribute.i RateRaw        character "format 'x'" "(r)ate or (R)aw:"}
{tms_support/protop/lib/attribute.i SumSample      character "format 'x'" "(s)ummary or (S)ample:"}
{tms_support/protop/lib/attribute.i DoWeb          logical   "format 'Yes/No' initial {&html-on}" "Enable Web pages?" "logical"}

{tms_support/protop/lib/attribute.i curr-page      integer   "format '>>9' initial 1"   "Set current page:" "integer"
  "if curr-page < 1 then curr-page = 1."
}

{tms_support/protop/lib/attribute.i html-dir       character "format 'x(60)'"    "HTML dir:" "string" 
  "file-info:file-name = NewValue. 
   if file-info:full-pathname = ? then
     do: 
       message 'Invalid directory.' skip 'Html dir not changed.' view-as alert-box info buttons ok.
       return. 
     end. "
}

/* register a display type
 *
 */

procedure reg-disp:

  define input parameter p_disp-type as character no-undo.

  define variable disp-type-list as character no-undo.

  publish "get-disp-type-list" ( output disp-type-list ).

  if p_disp-type <> "" and lookup( p_disp-type, disp-type-list ) = 0 then
    if disp-type-list = "" then
      disp-type-list = p_disp-type.
     else
      disp-type-list = disp-type-list + "," + p_disp-type.

  publish "set-disp-type-list" ( input disp-type-list ).

  return.

end.

procedure mon-reload:

  hide all no-pause.

  empty temp-table tt_ui-hdr.
  empty temp-table tt_ui-sch.
  empty temp-table tt_ui-det.

  publish "mon-restart".
  publish "mon-load".
  publish "mon-define".
  publish "mon-init".
  init-timer().

  return.

end.

procedure mon-load:

  define variable fname as character no-undo.
  define variable dname as character no-undo.

  define variable schedule_wday  as logical no-undo extent  7 initial yes.
  define variable schedule_mday  as logical no-undo extent 31 initial yes.
  define variable schedule_start as integer no-undo initial 0.
  define variable schedule_end   as integer no-undo initial 86400.

  define variable i as integer no-undo.

  /* load monitoring modules from the standard directory
   *
   */

  if searchDir( "mon" ) <> ? then
    do:

      input from os-dir( searchDir( "mon" )).

      import.	/* eat .	*/
      import.	/* eat ..	*/

      repeat:
        import ^ fname.
        if substring( fname, length( fname) - 1, 2 ) = ".p" then run value( fname ) persistent.
      end.

      input close.

    end.

  /* load monitoring modules from an OS specific directory (if any)
   *
   */

  dname = "".

  if opsys = "unix" then
    do:
      input through value( "uname -a" ).
      import dname.
      input close.
    end.

  if dname <> "" and searchDir( "os/" + dname ) <> ? then
    do:

      input from os-dir( searchDir( "os/" + dname )).

      import.	/* eat .	*/
      import.	/* eat ..	*/

      repeat:
        import ^ fname.
        if substring( fname, length( fname) - 1, 2 ) = ".p" then run value( fname ) persistent.
      end.

      input close.

    end.


  /* load protop.cfg 
   *
   */

  run lib/protop-cfg.p.


  /* load alert configuration
   *
   */

  empty temp-table tt_alert.

  if search( "etc/alert.cfg" ) = ? then return.			/* yes, returning in such a fashion is frowned on...	*/

  input from value( search( "etc/alert.cfg" )).

  repeat on endkey undo, leave:

    create tt_alert.
    import
      tt_alert.alert_key
      tt_alert.alert_dtype
      tt_alert.alert_compare
      tt_alert.alert_target
      tt_alert.alert_sensitivity
      tt_alert.alert_notify
      tt_alert.alert_message
      tt_alert.alert_action
    .

    if tt_alert.alert_key begins "#" or tt_alert.alert_key = "" then
      do:
        delete tt_alert.
        next.
      end.

    if tt_alert.alert_key = "schedule" then				/* overloaded field names :-(, fix it later		*/
      do:

        /* day of week -- sunday = 1
         */

        if tt_alert.alert_dtype = "*" or tt_alert.alert_dtype = "" then
          schedule_wday = yes.
         else
          do:
            schedule_wday = no.
            schedule_mday = no.						/* if weekdays are being specified default mdays to no	*/
            do i = 1 to num-entries( tt_alert.alert_dtype ):
              schedule_wday[ integer( entry( i, tt_alert.alert_dtype )) ] = yes.
            end.
          end.

        /* day of month
         */

        if tt_alert.alert_compare = "*" or tt_alert.alert_compare = "" then
          schedule_mday = yes.
         else
          do:
            schedule_mday = no.
            do i = 1 to num-entries( tt_alert.alert_compare ):
              schedule_mday[ integer( entry( i, tt_alert.alert_compare )) ] = yes.
            end.
          end.

        if tt_alert.alert_target = "*" or tt_alert.alert_target = "" then
          schedule_start = 0.
         else
          do:
            schedule_start = integer( entry( 1, tt_alert.alert_target, ":" )) * 3600.
            if num-entries( tt_alert.alert_target, ":" ) >= 2 then
              schedule_start = schedule_start + integer( entry( 2, tt_alert.alert_target, ":" )) * 60.
            if num-entries( tt_alert.alert_target, ":" ) = 3 then
              schedule_start = schedule_start + integer( entry( 3, tt_alert.alert_target, ":" )).
          end.

        if tt_alert.alert_sensitivity = "*" or tt_alert.alert_sensitivity = "" then
          schedule_end = 86400.
         else
          do:
            schedule_end = integer( entry( 1, tt_alert.alert_sensitivity, ":" )) * 3600.
            if num-entries( tt_alert.alert_sensitivity, ":" ) >= 2 then
              schedule_end = schedule_end + integer( entry( 2, tt_alert.alert_sensitivity, ":" )) * 60.
            if num-entries( tt_alert.alert_sensitivity, ":" ) = 3 then
              schedule_end = schedule_end + integer( entry( 3, tt_alert.alert_sensitivity, ":" )).
          end.

        delete tt_alert.	/* don't keep this record -- it isn't an alert!	*/
        next.

      end.

    if tt_alert.alert_dtype = "num" then tt_alert.alert_target-num = decimal( tt_alert.alert_target ).

    if tt_alert.alert_sensitivity = "" then tt_alert.alert_sensitivity = "1".
    if tt_alert.alert_notify = "" then tt_alert.alert_notify = "always".

    if num-entries( tt_alert.alert_sensitivity, ":" ) = 1 then
      assign
        tt_alert.sense_numerator   = integer( tt_alert.alert_sensitivity )
        tt_alert.sense_denominator = integer( tt_alert.alert_sensitivity )
      .
     else if num-entries( tt_alert.alert_sensitivity, ":" ) = 2 then
      assign
        tt_alert.sense_numerator   = integer( entry( 1, tt_alert.alert_sensitivity, ":"  ))
        tt_alert.sense_denominator = integer( entry( 2, tt_alert.alert_sensitivity, ":"  ))
      .

    if       tt_alert.alert_notify = "always" then tt_alert.alert_interval =     0.
     else if tt_alert.alert_notify = "daily"  then tt_alert.alert_interval = 86400.
     else if tt_alert.alert_notify = "hourly" then tt_alert.alert_interval =  3600.
     else assign tt_alert.alert_interval = integer( tt_alert.alert_notify ) no-error.

    /* set the schedule
     */

    do i = 1 to 7:
      tt_alert.alert_wday[i]  = schedule_wday[i].
    end.

    do i = 1 to 31:
      tt_alert.alert_mday[i]  = schedule_mday[i].
    end.

    assign
      tt_alert.alert_start = schedule_start
      tt_alert.alert_end   = schedule_end
    .

  end.

  delete tt_alert.	/* delete the last line read -- it is always bogus.	*/
 
  input close.

  return.

end.

/** Initialize the PP
 **
 **/

subscribe to "mon-load"           anywhere run-procedure "mon-load".
subscribe to "mon-reload"         anywhere run-procedure "mon-reload".
subscribe to "register-disp-type" anywhere run-procedure "reg-disp".

return.
