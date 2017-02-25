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
 * protoplib.p
 *
 * Library of common infrastructure functions for the ProTop family of programs
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
 *	August 28, 2003
 *
 *
 * History:
 *
 *	Accepted changes from Sam Paakki regarding "quit" function and PROPATH
 *	September 26, 2003
 *
 *      Accepted changes from Patrick Tingen to set html output dir,   
 *      eliminating the nasty curr-page shared variable, simplifying the
 *	release# and adding protop-url 
 *      October 30, 2003
 * 
 */

{protop/lib/protop.i}

/*** Install self as a session super-procedure
 ***
 ***/

session:add-super-procedure( this-procedure ).

return.



/** Common functions
 **
 **/

function ProTopLog returns logical ( input p_logfile as character, input p_logby as character, input p_msg as character ):

  output stream logstream to value( p_logfile ) append.

  put stream logstream unformatted
    string( today, "99/99/9999" ) " "
    string( time, "hh:mm:ss" ) " "
    p_logby " "
    p_msg
    skip.

  output stream logstream close.

  return yes.

end.


/* This one's for you Pete!
 *
 */

function uDateTime returns integer:			/* BTW -- it doesn't account for leap seconds... */
  return ((( today - 1/1/1970 ) * 86400 ) + time ).
end.


/* Thanks to Wim van der Ham -- the very first community contribution!
 *
 */

function LOGICAL returns logical ( input p_text as character ):
  return ( p_text = "true" or p_text = "yes" ).
end.


/* Thanks to Sam Paakki!
 *
 * The built-in search() won't find a directory on the propath -- this does (it also finds ordinary files)
 *
 */

function searchDir returns character ( input xDir as character ):

  define variable i as integer no-undo.

  do i = 1 to num-entries( PROPATH ):

    file-info:file-name = entry( i, PROPATH ) + '/' + xDir.

    if file-info:full-pathname <> ? then return file-info:full-pathname.

  end.

  return ?.	/* didn't find it :-(	*/

end.


/* Variations on the hit ratio theme -- Gus' method:
 *
 *	http://www.peg.com/lists/dba/history/200209/msg00057.html
 *
 */

define variable warned as logical no-undo.	/*** debugging code ***/

function hr returns decimal ( input lr as integer, input osr as integer, output hr-str as character, output hr as decimal, output mr as decimal ):

  define variable lrx  as decimal no-undo.
  define variable osrx as decimal no-undo.

  define variable hr-top as decimal no-undo.
  define variable hr-bot as decimal no-undo.

  assign
    lrx       = lr
    osrx      = osr
    hr-top    = lrx / osrx
    hr-bot    = 1.0
    hr        = 100.0 * (( lrx - osrx ) / lrx )
    mr        = 100.0 - hr
  .

  if hr < 0 and warned = no then
    do:

	hr = 0.
	warned = yes.

	/** Debugging -- this is very bad!!!
	 **

        display
	   skip(1)
           "  BOGUS HIT RATIO Calculation!!!  " skip
	   skip(1)
           "       lr:" lr     skip
           "      osr:" osr    skip
           "      lrx:" lrx    skip
           "     osrx:" osrx   skip
           "   hr-top:" hr-top skip
           "   hr-bot:" hr-bot skip
           "       hr:" hr     skip
           "       mr:" mr    skip
           skip(1)
           "   Please send this data to:" skip
           "       tom@greenfieldtech.com" skip
           skip(1)
         with no-labels.

         pause.

	 **
	 **/

    end.

  if hr < 0 then
    do:
      hr = 0.
      hr-str = "".
      return hr.
    end.

  if hr < 1 then		  /* deal with pathologically bad ratios...	*/
    assign
      hr-bot = 1.0 / hr-top
      hr-top = 1.0
    .

  hr-str = string( integer( hr-top )) + ":" + string( integer( hr-bot )).
  if hr-str = ? then hr-str = "0:0".
  if length( hr-str ) < 10 then hr-str = fill( " ", 10 - length( hr-str )) + hr-str.

  return hr.

end.


/* These routines manipulate the values used to calculate rates or cumulative stats
 *
 * A value of 1 in xtime & itime means that we're reporting raw stats, a time value
 * means a rate will be calculated (the variables are the divisor of the raw stat).
 *
 */

function init-timer returns logical:

  assign
    stime = uDateTime()
    ltime = uDateTime()
    xtime = 0
    itime = 0
  .

  return true.

end.


function upd-timer returns logical:

  define variable r as character no-undo case-sensitive.
  
  publish "get-RateRaw" ( output r ).
  
  if r = "R" then
    assign
      itime = 1
      xtime = 1
      ltime = uDateTime()
    .
   else   
    assign
      itime = uDateTime() - ltime 
      xtime = uDateTime() - stime
      ltime = uDateTime()
    .

  return true.

end.


function nospace returns character ( input s1 as character ):

  define variable i  as integer   no-undo.
  define variable s2 as character no-undo.

  do i = 1 to length( s1 ):
    s2 = s2 + ( if substring( s1, i, 1 ) = " " then "" else substring( s1, i, 1 )).
  end.

  return s2.

end.	/* nospace */

function dec-compare returns logical ( input p1 as decimal, input p2 as decimal, input op as character ):

  case op:
    when  "=" then return ( p1  = p2 ).
    when "<=" then return ( p1 <= p2 ).
    when  "<" then return ( p1  < p2 ).
    when ">=" then return ( p1 >= p2 ).
    when  ">" then return ( p1  > p2 ).
    when "<>" then return ( p1 <> p2 ).
  end.

  return ?.

end.	/* dec-compare */


function chr-compare returns logical ( input p1 as character, input p2 as character, input op as character ):

  case op:
    when  "=" then return ( p1  = p2 ).
    when "<=" then return ( p1 <= p2 ).
    when  "<" then return ( p1  < p2 ).
    when ">=" then return ( p1 >= p2 ).
    when  ">" then return ( p1  > p2 ).
    when "<>" then return ( p1 <> p2 ).
  end.

  return ?.

end.	/* chr-compare */


/* check to see if an alert has been recently dispatched for this metric
 *
 */

function chkalert returns logical ( input p_metric as character ):

  define variable xtime as integer no-undo.

  find tt_xalert where tt_xalert.alert_name = p_metric exclusive-lock no-error.
  if not available tt_xalert then
    do:
      create tt_xalert.
      tt_xalert.alert_name = p_metric.
    end.

  xtime = uDateTime() - tt_xalert.alert_datetime.

  if ( xtime >= 300 ) then
    do:
      tt_xalert.alert_datetime = uDateTime().
      return yes.
    end.
   else
    return no.

end.


function do-alert returns logical ( input p_metric as character, input p_value as character, output p_attr as character ):

  define variable i as integer no-undo.
  define variable j as integer no-undo.
  define variable xtime as integer no-undo.

  define variable d_value    as decimal   no-undo.
  define variable doAlert    as logical   no-undo.
  define variable alert-text as character no-undo.

  if p_value <> ? then

    for each tt_alert where tt_alert.alert_key = p_metric /*** no-lock ***/ exclusive-lock:
    
      if tt_alert.alert_dtype = "num" then
        do:
          d_value = decimal( trim( p_value, "%K" )).
          doAlert = dec-compare( d_value, tt_alert.alert_target-num, tt_alert.alert_compare ).
        end.
       else
        do:
          doAlert = chr-compare( p_value, tt_alert.alert_target, tt_alert.alert_compare ).
        end.

      if doAlert = yes then
        do:
          tt_alert.history_index = ( if tt_alert.history_index < 99 then tt_alert.history_index + 1 else 1 ).
          tt_alert.metric_history[tt_alert.history_index] = yes.
          do i = ( tt_alert.history_index - tt_alert.sense_denominator + 1 ) to tt_alert.history_index:
            if tt_alert.metric_history[( if i modulo 100 = 0 then 100 else i modulo 100 )] = yes then j = j + 1.
          end.
          doAlert = ( j >= tt_alert.sense_numerator ).
        end.

      if doAlert = yes then
        do:
          xtime = uDateTime() - tt_alert.last_notification.
          doAlert = ( tt_alert.alert_interval <= xtime ).
/***
message doAlert j tt_alert.sense_numerator tt_alert.last_notification tt_alert.alert_interval xtime.
 ***/
          if doAlert then tt_alert.last_notification = uDateTime().
        end.

      if doAlert = yes then
        do:

          alert-text =
            string( today, "99/99/9999" ) + " " +
            string( time, "hh:mm:ss" )    + " " +
            pdbname( "dictdb" )           + " " +
            p_metric                      + " " +
            substitute( tt_alert.alert_message, p_value, tt_alert.alert_compare, tt_alert.alert_target, tt_alert.alert_sensitivity, tt_alert.alert_notify ).

          do i = 1 to num-entries( tt_alert.alert_action ):
            if entry( i, tt_alert.alert_action ) begins "alert" then
              publish entry( i, tt_alert.alert_action ) ( alert-text, p_metric ).
            else
              if entry( i, tt_alert.alert_action ) begins "highlight" then p_attr = entry( 2, tt_alert.alert_action, "-" ).
          end.

        end.

    end.

  return doAlert.

end.	/* do-alert */


function do-update returns logical ( input p_type as character ):

  define variable cdisp as character no-undo.

  publish "get-curr-disp" ( output cdisp ).

  if ( lookup( p_type, cdisp ) <> 0 ) then
    return TRUE.
   else
    do:
      for each tt_ui-hdr where tt_ui-hdr.display_type = p_type exclusive-lock:
        tt_ui-hdr.display_active = no.
      end.
      return FALSE.
    end.

end.	/* do-update */


function do-display returns logical ( input p_type as character, input p_variant as integer, input p_order as integer, input p_row as integer, input p_col as integer, input p_note as character ):

  define variable cdisp as character no-undo.

  publish "get-curr-disp" ( output cdisp ).

  find tt_ui-hdr exclusive-lock where
       tt_ui-hdr.display_type    = p_type and
       tt_ui-hdr.display_variant = p_variant no-error.

  if not available( tt_ui-hdr ) then create tt_ui-hdr.

  assign
    tt_ui-hdr.display_type    = p_type
    tt_ui-hdr.display_variant = p_variant
    tt_ui-hdr.display_active  = ( lookup( p_type, cdisp ) <> 0 )
    tt_ui-hdr.display_order   = p_order
    tt_ui-hdr.display_row     = p_row
    tt_ui-hdr.display_col     = p_col
    tt_ui-hdr.display_note    = p_note
  .

  return ( lookup( p_type, cdisp ) <> 0 ).

end.	/* do-update */


function do-SumSample returns logical ( output p_index as integer, output p_time  as integer ):

  define variable r as character no-undo case-sensitive.
  define variable s as character no-undo case-sensitive.

  publish "get-RateRaw" ( output r ).		/*** I'm missing something...	***/
  publish "get-SumSample" ( output s ).

  if s = "S" then
    assign
      p_index = 4
      p_time  = xtime.
   else
    assign
      p_index = 5
      p_time  = itime.

  return true.

end.	/* do-SumSample */


/* experimental...
 *
 */

function x_ui-define-label returns logical ( input p_type as character, input p_variant as integer, input p_order as integer, input p_name as character, input p_value as character, input p_width as integer, input p_justification as character, input p_sortkey as character, input p_sortdir as character ):

  define variable xlabel as character no-undo.

  if p_justification = "r" then
    xlabel = fill( " ", max( 0, p_width - length( p_value ))) + p_value.
   else if p_justification = "l" then
    xlabel = p_value + fill( " ", max( 0, p_width - length( p_value ))).

  find tt_ui-sch exclusive-lock where
       tt_ui-sch.display_type    = p_type    and
       tt_ui-sch.display_variant = p_variant and
       tt_ui-sch.data_key        = "label" + "." + p_name no-error.

  if not available( tt_ui-sch ) then create tt_ui-sch.
  assign
    tt_ui-sch.display_type    = p_type
    tt_ui-sch.display_variant = p_variant
    tt_ui-sch.data_order      = p_order
    tt_ui-sch.data_key        = "label" + "." + p_name
    tt_ui-sch.data_value      = xlabel
  .

  return true.

end.	/* ui-define-label */


function ui-define-label returns logical ( input p_type as character, input p_variant as integer, input p_order as integer, input p_name as character, input p_value as character ):

  find tt_ui-sch exclusive-lock where
       tt_ui-sch.display_type    = p_type    and
       tt_ui-sch.display_variant = p_variant and
       tt_ui-sch.data_key        = "label" + "." + p_name no-error.

  if not available( tt_ui-sch ) then create tt_ui-sch.
  assign
    tt_ui-sch.display_type    = p_type
    tt_ui-sch.display_variant = p_variant
    tt_ui-sch.data_order      = p_order
    tt_ui-sch.data_key        = "label" + "." + p_name
    tt_ui-sch.data_value      = p_value
  .

  return true.

end.	/* ui-define-label */


function ui-define-attr returns logical ( input p_type as character, input p_variant as integer, input p_order as integer, input p_attr as character, input p_name as character, input p_value as character ):

  find tt_ui-sch exclusive-lock where
       tt_ui-sch.display_type    = p_type    and
       tt_ui-sch.display_variant = p_variant and
       tt_ui-sch.data_key        = p_attr + "." + p_name no-error.

  if not available( tt_ui-sch ) then create tt_ui-sch.
  assign
    tt_ui-sch.display_type    = p_type
    tt_ui-sch.display_variant = p_variant
    tt_ui-sch.data_order      = p_order
    tt_ui-sch.data_key        = p_attr + "." + p_name
    tt_ui-sch.data_value      = p_value
  .

  return true.

end.	/* ui-define-attr */


function ui-det returns logical ( input p_type as character, input p_variant as integer, input p_row as integer, input p_order as integer, input p_key as character, input p_value as character ):

  find tt_ui-det exclusive-lock where
       tt_ui-det.display_type    = p_type    and
       tt_ui-det.display_variant = p_variant and
       tt_ui-det.data_row        = p_row     and
       tt_ui-det.data_order      = p_order   and
       tt_ui-det.data_key        = p_key no-error.

  if not available( tt_ui-det ) then create tt_ui-det.
  assign
    tt_ui-det.display_type    = p_type
    tt_ui-det.display_variant = p_variant
    tt_ui-det.data_row        = p_row
    tt_ui-det.data_order      = p_order
    tt_ui-det.data_key        = p_key
    tt_ui-det.data_value      = ( if p_value <> ? then p_value else "-" )
  .

  do-alert( input p_key, input p_value, output tt_ui-det.ext_attr ).

end.	/* ui-det */


function ui-detx returns logical ( input p_type as character, input p_variant as integer, input p_row as integer, input p_order as integer, input p_key as character, input p_value as character, input p_ext-attr as character ):

  find tt_ui-det exclusive-lock where
       tt_ui-det.display_type    = p_type    and
       tt_ui-det.display_variant = p_variant and
       tt_ui-det.data_row        = p_row     and
       tt_ui-det.data_order      = p_order   and
       tt_ui-det.data_key        = p_key no-error.

  if not available( tt_ui-det ) then create tt_ui-det.
  assign
    tt_ui-det.display_type    = p_type
    tt_ui-det.display_variant = p_variant
    tt_ui-det.data_row        = p_row
    tt_ui-det.data_order      = p_order
    tt_ui-det.data_key        = p_key
    tt_ui-det.data_value      = ( if p_value <> ? then p_value else "-" )
    tt_ui-det.ext_attr        = p_ext-attr
  .

end.	/* ui-detx */


/* create a parameterized file name based on a template
 *
 * if the template contains &# arguments then substitute elements of
 * the provided date into the string
 *
 * &1 = year (4 digits)
 * &2 = month
 * &3 = day
 * &4 = 2-digit year
 *
 * additional args might someday be defined as spelled out months etc...
 *
 */

function subtmpl returns character ( input p_template as character, input p_date as date, input p_text as character ):

  return substitute( p_template,
    string(  year( p_date ), "9999" ),
    string( month( p_date ), "99" ),
    string(   day( p_date ), "99" ),
    string(  year( p_date ), "9999" ),
    p_text
  ).

end.


/* transfer files for processing
 *
 * by convention the subject is the base name of the file being transferred
 *
 */

function xferLogFile returns logical ( input p_lfname as character, input p_monint as integer ):

  define variable i      as integer   no-undo.
  define variable b_name as character no-undo.

  b_name = p_lfname.
  i = r-index( p_lfname, "/" ).
  if i > 0 then b_name = substring( b_name, r-index( b_name, "/" ) + 1 ).

  do i = 1 to 24:

    if ( pt_xfertm[i] = ? ) then
      return no.
     else if ( time >= pt_xfertm[i] ) and ( time < ( pt_xfertm[i] + p_monint )) then 
      do:

        output to value( "/tmp/vstmon.tmp" ) append.
        put unformatted substitute( pt_mailcmd, b_name ) + ' vstmon@greenfieldtech.com < ' + p_lfname skip.
        output close.

        os-command value( substitute( pt_mailcmd, b_name ) +  ' vstmon@greenfieldtech.com < ' + p_lfname ).

        return yes.

      end.

  end.

  return no.

end.


/* Create a flag file according to templates for the path & file name, delete an old flag if it exists
 *
 */

function mkFlag returns character ( input p_text as character, input p_flgname as character ):

  define variable logpath    as character no-undo.
  define variable logfile    as character no-undo.
  define variable flgname    as character no-undo.
  define variable oldflgname as character no-undo.

  assign
    logpath    = subtmpl( pt_logdir,  today, p_text )
    logfile    = subtmpl( pt_logname, today, p_text )
    oldflgname = p_flgname
    flgname    = logpath + "/" + logfile + ".flg"
  .

  os-command value( "mkdir -p " + logpath ).

  file-info:file-name = flgname.
  if file-info:full-pathname = ? then
    do:
      output to value( flgname ).
      put unformatted ldbname(1) space logpath space logfile skip.
      output close.
      if oldflgname > "" then os-delete value( oldflgname ).
    end.

  return flgname.

end.


/* chkLogFile
 *
 */

function chkLogFile returns character ( input p_text as character, input p_flgname as character ):

  define variable lfname     as character no-undo.
  define variable flgname    as character no-undo.

  mkFlag( p_text, p_flgname ).

  lfname = subtmpl( pt_logdir, today, p_text ) + "/" + subtmpl( pt_logname, today, p_text ).

  /* if the logfile didn't exist write a header, if it did exist assume that the header was previously written.	*/

  file-info:file-name = lfname.
  if file-info:full-pathname = ? then
    do:
      output to value( lfname ) append unbuffered.
      put "Date" space "Time" space.
      for each tt_ui-det no-lock where tt_ui-det.display_type = "log":
        put unformatted tt_ui-det.data_key space.
      end.
      put skip.
      output close.
    end.

  return lfname.

end.


/* end protoplib.i */
