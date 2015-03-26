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
 * protoplib.i
 *
 * ProTop infrastructure library definitions
 *
 */

function ProTopLog returns logical ( input p_logfile as character, input p_logby as character, input p_msg as character ) in super.
function uDateTime returns integer () in super.
function LOGICAL returns logical ( input p_text as character ) in super.
function searchDir returns character ( input xDir as character ) in super.
function hr returns decimal ( input lr as integer, input osr as integer, output hr-str as character, output hr as decimal, output mr as decimal ) in super.
function init-timer returns logical () in super.
function upd-timer returns logical () in super.
function nospace returns character ( input s1 as character ) in super.
function dec-compare returns logical ( input p1 as decimal, input p2 as decimal, input op as character ) in super.
function chr-compare returns logical ( input p1 as character, input p2 as character, input op as character ) in super.
function chkalert returns logical ( input p_metric as character ) in super.
function do-alert returns logical ( input p_metric as character, input p_value as character, output p_attr as character ) in super.
function do-update returns logical ( input p_type as character ) in super.
function do-display returns logical ( input p_type as character, input p_variant as integer, input p_order as integer, input p_row as integer, input p_col as integer, input p_note as character ) in super.
function do-SumSample returns logical ( output p_index as integer, output p_time  as integer ) in super.
function ui-define-label returns logical ( input p_type as character, input p_variant as integer, input p_order as integer, input p_name as character, input p_value as character ) in super.
function ui-define-attr returns logical ( input p_type as character, input p_variant as integer, input p_order as integer, input p_attr as character, input p_name as character, input p_value as character ) in super.
function ui-det returns logical ( input p_type as character, input p_variant as integer, input p_row as integer, input p_order as integer, input p_key as character, input p_value as character ) in super.
function ui-detx returns logical ( input p_type as character, input p_variant as integer, input p_row as integer, input p_order as integer, input p_key as character, input p_value as character, input p_ext-attr as character ) in super.
function subtmpl returns character ( input p_template as character, input p_date as date, input p_text as character ) in super.
function xferLogFile returns logical ( input p_lfname as character, input p_monint as integer ) in super.
function mkFlag returns character ( input p_text as character, input p_flgname as character ) in super.
function chkLogFile returns character ( input p_text as character, input p_flgname as character ) in super.

function x_ui-define-label returns logical ( input p_type as character, input p_variant as integer, input p_order as integer, input p_name as character, input p_value as character, input p_width as integer, input p_justification as character, input p_sortkey as character, input p_sortdir as character ) in super.

/* end protoplib.i */
