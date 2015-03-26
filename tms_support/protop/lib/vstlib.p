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
 * vstlib.p
 *
 * Functions that deal with VSTs
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

{lib/protop.i}

/*** Install self as a session super-procedure
 ***
 ***/

session:add-super-procedure( this-procedure ).

return.


/** VST functions
 **
 **/


function get-userinfo returns character ( input p_id as integer, output p_name as character, output p_flags as character, output p_pid as character ):

  define variable xflag as character no-undo.

  find first dictdb._Connect no-lock where _Connect-id = p_id no-error.

  assign
    p_name  = ""
    p_flags = ""
    p_pid   = ""
  .

  if not available _connect then return "".

  if _Connect-type = "self" then
    p_flags = "S".
   else if _Connect-type = "remc" then
    p_flags = "R".
   else
    p_flags = "O".	/* "Other" -- I'm too lazy to dream up codes right now...	*/

  if _Connect-device = "batch" then
    p_flags = p_flags + "B".
   else
    p_flags = p_flags + " ".

  xflag = " ".
  if _Connect-TransId    > 0 then xflag = "*".
  if _Connect-Disconnect = 1 then xflag = "d".
  if _Connect-Resync     = 1 then xflag = "r".
  if _Connect-Interrupt  = 1 then xflag = "i".

  p_flags = p_flags + xflag.

  p_name = _Connect-name.

  /** If it's a remote client the server number might be more interesting than the PID
   **
   **/

  p_pid = string( _Connect-PID ).

  return _Connect-name.

end.	/* get-userinfo */


function get-blocked returns character ( input p_usr as integer ):

  define variable i as integer   no-undo.
  define variable c as character no-undo.
  define variable t as logical   no-undo.

  define variable bxwait as character no-undo.
  define variable bresrc as character no-undo.
  define variable bxnote as character no-undo.

  define variable b_str  as character no-undo initial "Not Blocked".

  find first dictdb._Connect no-lock where _Connect-usr = p_usr no-error.

  if available _Connect and _Connect-wait <> " -- " then
    do:

      /** Only handles record locks at the moment...
       **
       **/

      bxwait = _Connect-wait.

      case _Connect-wait:

        when "REC" then
        do:

          assign
            bresrc = string( _Connect-wait1 )
            t = no
          .

          for each dictdb._Lock no-lock while _Lock-usr <> ? /** and _Lock-recid <> ? **/:

            if _Lock-recid = _Connect-wait1 then
              do:

                if t = no then          /* figure out the table that the recid belongs to... */
                  do:
                    t = yes.
                    find _file no-lock where _file._file-num = _Lock-table no-error.
                    if available _file then bxnote = "[" + _file._file-name + "]".
                  end.

                if _Lock-usr = _Connect-usr then	/* the blocked usr that we're looking at	*/
                  do:
                    bxwait = bxwait + " ".
                    do i = 1 to 8:
                      c = substring( _Lock-flags, i, 1 ).
                      if c <> " " then bxwait = bxwait + c.
                    end.
                  end.
                 else
                  do:
                    bxnote = bxnote + " " + _Lock-name.		/* a user who is queued -- 1st one holds the lock...	*/
                  end.

              end.

          end.

        end.	/* record lock */

        otherwise	/* no meaningful interpretations at the moment... */
        do:

          assign
            bresrc = string( _Connect-wait1 )
          .

/*** special purpose logging...
          ProTopLog( "blocked.log", "protop-lib:get-blocked()", bxwait + " " + bresrc ).
 ***/

        end.	/* otherwise */

      end.	/* case */

    end.

  b_str = bxwait + ":" + bresrc + ":" + bxnote.

  return b_str.

end.	/* get-blocked */


/* Thanks to George Potemkin!
 *
 */

function ai-info returns character ( input vAiFile as character, output vAiGenNum as integer ):

  DEFINE VARIABLE vAiBlkSize AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vAiStatus  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vStatus    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vRaw       AS RAW       NO-UNDO.

  find first _logging no-lock.
  vAiBlkSize = _logging._logging-aiBlkSize.

  file-info:file-name = vAiFile.

  if file-info:full-pathname <> ? and index( file-info:file-type, "r" ) <> 0  and ( vAiBlkSize > 0 ) then
    do:

      /* Offset Len  Description
       * ------ ---  -----------
       * 24-27    4  AiGenNum      AI extent sequence number (mb_aigennbr, Seqno)
       * 40-43    4  AiStatus      1=Busy, 2=Empty, 4=Full
       *
       * v10 moved everything 65 bytes...
       * ( if integer( dbversion(1)) > 9 then 65 else 0 ).
       */

      INPUT FROM VALUE( vAiFile ).

      ASSIGN LENGTH( vRaw ) = 4.
      SEEK   INPUT TO 24 + vAiBlkSize + ( if integer( dbversion(1)) > 9 then 65 else 0 ).
      IMPORT UNFORMATTED vRaw.
      ASSIGN vAiGenNum = GET-LONG( vRaw, 1 ).
            
      SEEK   INPUT TO 40 + vAiBlkSize + ( if integer( dbversion(1)) > 9 then 65 else 0 ).
      IMPORT UNFORMATTED vRaw.
      ASSIGN vAiStatus = GET-LONG(vRaw, 1).

      ASSIGN LENGTH(vRaw) = 0.
      INPUT CLOSE.

      vStatus =
        IF       vAiStatus EQ 0 THEN "None"		/* Msg 694	*/
         ELSE IF vAiStatus EQ 1 THEN "Busy"		/* Msg 3792	*/
         ELSE IF vAiStatus EQ 2 THEN "Empty"		/* Msg 3793	*/
         ELSE IF vAiStatus EQ 4 THEN "Full"		/* Msg 3794	*/
         ELSE IF vAiStatus EQ 8 THEN "Locked"		/* ????		*/
         ELSE "Unknown".				/* Msg 737	*/

    end.
   else
    do:
      vStatus = "Can not open".
    end.

  return vstatus.

end.	/* ai-info */


/* check storage areas for spare capacity
 *
 * integrated changes from Phil Freed 7/8/05
 *
 *
 */

function chkarea returns integer ( input threshold as decimal, output worst as decimal ):

  define variable total-free as decimal no-undo.
  define variable used       as decimal no-undo.
  define variable i          as integer no-undo.

  worst = 0.

  for each _AreaStatus no-lock where
      _areaStatus-AreaNum >= 6 and
      ( not _areaStatus-AreaName matches "*After Image Area*" ):

    total-free = _AreaStatus-Totblocks - _AreaStatus-Hiwater.
    if ( _AreaStatus-Freenum <> ? ) then
      total-free = total-free + _AreaStatus-Freenum.

    used = 100 * ( _AreaStatus-totblocks - total-free ) / _AreaStatus-totblocks.

    worst = max( worst, used ).
    if used >= threshold then i = i + 1.

  end.

  return i.

end.


/* check ai extent status
 *
 */

function chkai returns integer ( output ai_exts as integer, output ai_full as integer, output ai_empty as integer ):

  define variable ai_seq  as integer.
  define variable ai_busy as integer.

  assign
    ai_exts  = 0
    ai_busy  = 0
    ai_full  = 0
    ai_empty = 0
    ai_seq   = 0
  .

  for each _AreaStatus no-lock where ( _areaStatus-Areaname matches "*After Image Area*" ):

    ai_exts = ai_exts + 1.

    case ai-info( input _AreaStatus-LastExtent, output ai_seq ):
      when "full"  then ai_full  = ai_full  + 1.
      when "empty" then ai_empty = ai_empty + 1.
      when "busy"  then ai_busy  = ai_exts.
    end.

  end.

  return ai_busy.

end.


/* chkptnum
 *
 */

function chkptnum returns integer ( input-output oldbi as integer ):

  /* Adjust to align with checkpoint numbers -- if we can.  _Trans-counter is the
   * checkpoint# since the last "truncate bi".  Unfortunately the only way to map
   * from that number to _BfStatus-LastCkpNum involves starting a transaction in
   * mon-init().  And that is optional since ProTop shouldn't require the user to
   * write to a monitored database.  So if that option isn't enabled (chkp-base = ?)
   * we have to live with the fact that we can't map them.  Which means that we're
   * showing 0 at times when the real active checkpoint is probably something else.
   * That's not as bad as it sounds though because it should only happen when there
   * are no active transactions which means that, while we don't know what the
   * actual current checkpoint # is, we also only have one active checkpoint so
   * there's nothing to be excited about in these two numbers.  (The whole point
   * of this metric is to show growth in the number of active bi clusters.)
   *
   */

  define variable currbi as integer no-undo.

  for each dictdb._Trans no-lock where _Trans-usrnum <> ?:
    if _Trans-counter <> ? and _Trans-counter > 0 then
      do:
        if oldbi = 0 or _Trans-counter < oldbi then oldbi = _Trans-counter.
        currbi = max( currbi, _Trans-counter ).
      end.
  end.

  find dictdb._BuffStatus no-lock.

  if chkp-base <> ? then
    do:
      currbi = _BfStatus-LastCkpNum.
      if oldbi = 0 then
        oldbi = currbi.
       else
        oldbi = oldbi - chkp-base.
    end.
   else
    do:
      if oldbi = 0 then oldbi = currbi.
    end.

  if oldbi > currbi then oldbi = currbi.

  return currbi.

end.


function xversion returns decimal ( input p_shmver as integer ):

  define variable x as decimal no-undo.

  x = p_shmver.

  if p_shmver > 64000000 then
    x = p_shmver - 64000000.

  if       x >=  9000 and  x <= 9048 then x =  9.0.
   else if x >=  9049 and  x <= 9199 then x =  9.1.
   else if x >= 10000 and x <= 10099 then x = 10.0.
   else if x >= 10100 and x <= 10199 then x = 10.1.
   else x = 0.

  return x.

end.


/* end vstlib.i */
