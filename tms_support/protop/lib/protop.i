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
 * protop.i
 *
 * Header file for protop family of programs
 *
 *
 * Known Bugs & Issues:
 *
 *	Need to obfuscate variable names to prevent name collisions?
 *
 *
 * To Do:
 *
 *	Replace functions with function headers and move actual functions to PP
 *	(probably monitor.p)
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

&global-define	protop-version	xix		/* Roman numerals for now...						*/

&global-define  protop-url      http://www.greenfieldtech.com/protop.shtml

&global-define	html-on		no		/* create HTML output? 							*/
&global-define	html-dir	/tmp/		/* where to put HTML output -- if blank it will go to session:temp-dir	*/
&global-define	map-chkp        "no"		/* Create a _User record in order to map checkpoint numbers		*/
&global-define	show-top        13		/* starting screen row# to display detail lines at			*/

/* Progress made me do it!
 *
 */

define new global shared stream ftpsh.

/* it's unfortunate but sometimes I'm just too lazy...
 *
 */

define new global shared variable interactive as logical no-undo.
define new global shared variable chkp-base   as integer no-undo initial ?.	/* cover for the lack of a VST field	*/
										/* corresponding to base checkpoint#	*/

/* The values for these are defined in etc/protop.cfg and set by lib/monitor.p
 *
 */

define new global shared variable pt_logdir      as character no-undo initial "/tmp".
define new global shared variable pt_logname     as character no-undo initial "&5.&2.&3".
define new global shared variable pt_mailcmd     as character no-undo initial 'mailx "-s &1" '.
define new global shared variable pt_alert       as character no-undo.
define new global shared variable pt_alarm       as character no-undo.
define new global shared variable pt_pager       as character no-undo.
define new global shared variable pt_xfertime    as character no-undo initial "23:45".

/* These are indirectly set
 *
 */

define new global shared variable pt_xfertm      as integer   no-undo extent 24 initial ?.

/* These are, IMHO, appropriately global
 * 
 */

define new global shared variable h_flib as handle no-undo.			/* where is the function library?	*/

define new global shared variable stime as integer no-undo.			/* start time				*/
define new global shared variable ltime as integer no-undo.			/* last time				*/
define new global shared variable xtime as integer no-undo.			/* total time				*/
define new global shared variable itime as integer no-undo.			/* iteration time			*/

/* internal diagnostics -- what routines are taking too much time?
 *
 */

define new global shared variable protop-time as integer no-undo.
define new global shared variable protop-upd  as integer no-undo.
define new global shared variable protop-disp as integer no-undo.
define new global shared variable protop-chui as integer no-undo.
define new global shared variable protop-html as integer no-undo.

define stream logstream.

/** Temp Table Definitions
 **
 ** Yup, they're shared.  But this stuff makes no sense across session boundaries anyway.
 ** And a shared temp-table is logically the same as a db table so who really cares?
 **
 **/

define new global shared temp-table tt_ui-hdr no-undo
  field display_type    as character
  field display_variant as integer
  field display_active  as logical
  field display_order   as integer		/* hint	*/
  field display_row     as integer		/* hint, relative to other variants	*/
  field display_col     as integer		/* hint, relative to other variants	*/
  field display_note    as character
  index ui-hdr-idx is unique primary display_type display_variant.

define new global shared temp-table tt_ui-sch no-undo
  field display_type    as character
  field display_variant as integer
  field data_key        as character
  field data_row        as integer	/* always 0 (enables a break-by though...)	*/
  field data_order      as integer
  field data_value      as character
  index ui-det-idx is unique primary display_type display_variant data_key.

define new global shared temp-table tt_ui-det no-undo
  field display_type    as character
  field data_row        as integer
  field data_order      as integer
  field display_variant as integer
  field data_key        as character
  field data_value      as character
  field ext_attr        as character
  index ui-det-idx is unique primary display_type display_variant data_row data_order data_key.

define new global shared temp-table tt_alert no-undo
  field alert_key         as character
  field alert_dtype       as character
  field alert_compare     as character
  field alert_target      as character
  field alert_target-num  as decimal
  field alert_sensitivity as character
  field alert_notify      as character
  field alert_action      as character
  field alert_message     as character
  field alert_interval    as integer
  field sense_numerator   as integer
  field sense_denominator as integer
  field last_notification as integer
  field metric_history    as logical extent 100
  field history_index     as integer
  field alert_start       as integer
  field alert_end         as integer
  field alert_wday        as logical extent 7
  field alert_mday        as logical extent 31
  index alert-idx is primary alert_key.

define temp-table tt_xalert no-undo
  field alert_name     as character
  field alert_datetime as integer.

define new global shared temp-table xconnect no-undo
  field xgen      as integer
  field ID        as integer   format ">>>>9"    label  "Id"		/* _Connect Id -- if known	*/
  field PID       as integer   format ">>>>>>>9" label "PID"
  field uxName    as character format "x(8)"     label "UnixName"
  field TTY       as character format "x(8)"     label "TTY"
  field CPUTime   as integer   format ">>>>9"    label "CPU Time"
  field Command   as character format "x(60)"    label "Command"
  field UsrNum    as integer   format ">>>>9"    label "UsrNum"
  field prName    as character format "x(8)"     label "ProgressName"
  index pid-idx   is unique primary pid
  index usrnum-idx usrnum
  index xgen-idx xgen
.

{tms_support/protop/lib/protoplib.i}
{tms_support/protop/lib/vstlib.i}

/* end protop.i */
