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
 * vstlib.i
 *
 * VST library definitions
 *
 */

function get-userinfo returns character ( input p_id as integer, output p_name as character, output p_flags as character, output p_pid as character ) in super.
function get-blocked returns character ( input p_usr as integer ) in super.
function ai-info returns character ( input vAiFile as character, output vAiGenNum as integer ) in super.
function chkarea returns integer ( input threshold as decimal, output worst as decimal ) in super.
function chkai returns integer ( output ai_exts as integer, output ai_full as integer, output ai_empty as integer ) in super.
function chkptnum returns integer ( input-output oldbi as integer ) in super.
function xversion returns decimal ( input p_shmver as integer ) in super.

/* end vstlib.i */
