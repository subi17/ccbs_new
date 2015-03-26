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
 * summary.p
 *
 * Generic summary display procedure
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	September 29, 2003
 *
 *
 * History:
 *
 *	Accepted changes from Patrick Tingen eliminating the nasty curr-page
 *	shared variable and modifying the release numbering mechanism.
 *	October 30, 2003
 *
 *	Modified to support Windows GUI functionality.
 *	December 10, 2004
 *
 */

{lib/protop.i}

define variable hWin as handle no-undo.

define variable dname as character no-undo.
define variable d     as character no-undo extent 55 format "x(10)".	/* dummy variable for display placeholder */

&IF "{&WINDOW-SYSTEM}" <> "tty" &THEN
form skip(15) with frame z stream-io no-box row 1 column 1 width 7.
&ENDIF

form
&IF "{&WINDOW-SYSTEM}" = "tty" &THEN
    d[10] "   ProTop {&protop-version} -- Progress Database Monitor " to 66 d[20] format "x(8)" to 80 skip
    d[30]    d[40] format "x(64)" d[50] format "x(4)" to 80 skip
&ELSE
skip(2)
&ENDIF
/***	potentially available array header elements
    d[60] d[70] d[80] d[90]
 ***/
    "Hit Ratio:"  d[1] d[11]      "Commits:" to 45 d[21] format "x(8)" d[31] format "x(8)" "Sessions:" to 74 d[41] format "x(5)" to 80 skip
    "   Miss% :"  d[2] d[12]  "Latch Waits:" to 45 d[22] format "x(8)" d[32] format "x(8)"    "Local:" to 74 d[42] format "x(5)" to 80 skip
    "    Hit% :"  d[3] d[13] "Tot/Mod Bufs:" to 45 d[23] format "x(8)" d[33] format "x(8)"   "Remote:" to 74 d[43] format "x(5)" to 80 skip
    "Log Reads:"  d[4] d[14]   "Evict Bufs:" to 45 d[24] format "x(8)" d[34] format "x(8)"    "Batch:" to 74 d[44] format "x(5)" to 80 skip
    " OS Reads:"  d[5] d[15]   "Lock Table:" to 45 d[25] format "x(8)" d[35] format "x(8)"   "Server:" to 74 d[45] format "x(5)" to 80 skip
/***	older display elements
    "   Chkpts:"  d[6] d[16] "Lock Tbl HWM:" to 45 d[26] format "x(8)" d[36] format "x(8)"    "Other:" to 74 d[46] format "x(5)" to 80 skip
    "  Flushed:"  d[7] d[17]  "Old/Curr BI:" to 45 d[27] format "x(8)" d[37] format "x(8)"      "TRX:" to 74 d[47] format "x(5)" to 80 skip
 ***/
    "Rec Reads:"  d[6] d[16] "LkHWM|OldTrx:" to 45 d[26] format "x(8)" d[36] format "x(8)"    "Other:" to 74 d[46] format "x(5)" to 80 skip
    "  Log/Rec:"  d[7] d[17]  "Old/Curr BI:" to 45 d[27] format "x(8)" d[37] format "x(8)"      "TRX:" to 74 d[47] format "x(5)" to 80 skip
    "Area Full:"  d[8] d[18]  "After Image:" to 45 d[28] format "x(8)" d[38] format "x(8)"  "Blocked:" to 74 d[48] format "x(5)" to 80 skip
    {local/out/summary.i}
/***	potentially available array detail elements
    "      xxx:"  d[9] d[19]          "xxx:" to 45 d[29] format "x(8)" d[39] format "x(8)"     "xxx:" to 74 d[49] format "x(5)" to 80 skip
    d[81] d[82] d[83] d[84] d[85] d[86] d[87] d[88] d[89] skip
    d[91] d[92] d[93] d[94] d[95] d[96] d[97] d[98] d[99] skip
 ***/
    skip(1)
&IF "{&WINDOW-SYSTEM}" <> "tty" &THEN
    skip(3)
&ENDIF
 with
  stream-io
  frame summary-stats
&IF "{&WINDOW-SYSTEM}" <> "tty" &THEN
  width 87
  column 6
&ENDIF
  overlay
  row 1
  no-box
  no-labels.

procedure display_sum:

  define variable support as character no-undo initial "Summary".     
  if do-update( support ) = no then return. 

  current-window = hWin.

  for each tt_ui-hdr no-lock where tt_ui-hdr.display_type = "summary" and tt_ui-hdr.display_active = yes:

    for each tt_ui-det no-lock where
          tt_ui-det.display_type = tt_ui-hdr.display_type and
          tt_ui-det.data_row = 1
        by tt_ui-det.data_order:

      if tt_ui-det.data_order = 40 then		 /* center the db name */
        do:
          assign
            dname = tt_ui-det.data_value
            dname = fill( " ", ( integer(( 64 - r-index( dname, "]" )) / 2 ))) + dname
          .
          if "{&window-system}" = "tty" then
            display dname @ d[40] with frame summary-stats.
           else
do:
            hWin:title = "ProTop release {&protop-version} -- Summary " + tt_ui-det.data_value.
view frame z.
end.
        end.
       else
        if "{&window-system}" = "tty" or tt_ui-det.data_order modulo 10 <> 0 then
          do:
            if tt_ui-det.ext_attr <> "" then
              color display value( tt_ui-det.ext_attr ) d[tt_ui-det.data_order] with frame summary-stats.
            display tt_ui-det.data_value @ d[tt_ui-det.data_order] with frame summary-stats.
          end.
     end.

  end.

  return.

end.

&IF "{&WINDOW-SYSTEM}" = "tty" &THEN

hWin = current-window.		/* there is only one window in tty mode	*/

&ELSE

create window hWin.
assign
  hWin:width-chars  = 126 /** 112 **/
  hWin:height-chars = 10
  hWin:scroll-bars  = no
  hWin:message-area = no
  hWin:status-area  = no
  hwin:sensitive    = yes
  hWin:visible      = yes
.

frame summary-stats:parent = hWin.

on "close" of this-procedure do:
  delete object hWin.
  delete object this-procedure.
end.

on "window-close" of hWin do:
  apply "close" to this-procedure.
end.

&ENDIF

subscribe to "vdisplay"     anywhere run-procedure "display_sum".

return.
