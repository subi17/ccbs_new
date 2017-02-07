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
 * detail.p
 *
 * Generic detail display procedure
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

define input parameter det_type as character no-undo.

{tms_support/protop/lib/protop.i}

define variable frame-offset as integer no-undo.
define variable frame-height as integer no-undo.

define temp-table v_display no-undo
  field v_row  as integer
  field v_data as character format "x(80)"
  index v_disp is unique primary v_row.

define variable hWin as handle no-undo.

define variable virtual-row  as integer no-undo.
define variable first-detail as integer no-undo initial 1.  /*** set this down below... ***/

procedure paint:

  for each v_display no-lock:
    display v_display.v_data with stream-io no-box no-labels row frame-offset frame-height down.
  end.

end.

procedure add_vline:

  define input parameter vline as character no-undo.

  create v_display.
  assign
    virtual-row      = virtual-row + 1
    v_display.v_row  = virtual-row
    v_display.v_data = vline
  .

  return.

end.

procedure display_det:

  define variable r   as integer no-undo.
  define variable x   as integer no-undo.
  define variable y   as integer no-undo.

  define variable top          as integer no-undo.	/* number of detail rows per window		*/
  define variable num-disps    as integer no-undo.	/* how many types of displays are we handling?	*/
  define variable my-pos       as integer no-undo.	/* which position in the list is mine? 		*/
  define variable num-lines    as integer no-undo.	/* how many lines do we have to work with?	*/
  define variable first-row    as integer no-undo.	/* first row of detail data			*/
  define variable last-row     as integer no-undo.	/* last row of detail data			*/
  define variable iCurrPage    as integer no-undo. 	/* Current page					*/
  define variable disps-done   as integer no-undo.	/* how many displays have we shown?		*/
  define variable line-bank    as integer no-undo.	/* how many lines were unused?			*/

  publish "get-curr-page" ( output iCurrPage ).

  for each tt_ui-hdr no-lock where tt_ui-hdr.display_active = yes and tt_ui-hdr.display_order >= 100 break by tt_ui-hdr.display_type:

    if first-of( tt_ui-hdr.display_type ) then
      do:
        num-disps = num-disps + 1.
        if tt_ui-hdr.display_type = this-procedure:private-data then my-pos = num-disps.
      end.

  end.

/***
my-pos = 1.
num-disps = 1.
 ***/

  find tt_ui-hdr no-lock where tt_ui-hdr.display_type = "summary" and tt_ui-hdr.display_active = yes no-error.
  if available( tt_ui-hdr )  then
    first-detail = {&show-top}.			/* it would be better to get this dynamically...	*/
   else
    first-detail = 1.

  /* Set the number of rows of detail data and the "window" size based on how many
   * display types are active.
   *
   */

  if "{&window-system}" = "tty" then
    assign
      num-lines    = hWin:height-chars - first-detail + 1				/* is the screen big enough?		*/
      top          = integer( truncate(( num-lines / num-disps ), 0 ) - 3 )		/* how big can a window be?		*/
      frame-height = top + 3								/* header + detail lines		*/
      frame-offset = ( first-detail + ((( my-pos - 1 )) * ( frame-height )))		/* where is this frame positioned?	*/
      first-row    = ((( iCurrPage - 1 ) * top ) + 1 )					/* first row# on the page		*/
      last-row     = ( iCurrPage * top )						/* last row# on the page		*/
      line-bank    = num-lines - ( num-disps * ( frame-height ))			/* how many lines are left over?	*/
    .
   else
    assign
      num-lines    = 56 /* hWin:height-chars - first-detail + 1 */
      top          = num-lines - 3
      frame-height = num-lines
      frame-offset = 1
      first-row    = ((( iCurrPage - 1 ) * top ) + 1 )
      last-row     = ( iCurrPage * top )
      line-bank    = num-lines - ( num-disps * ( frame-height ))
    .

/***
message num-disps my-pos num-lines top frame-height frame-offset.
pause.
 ***/

  if top = ? then						/* assume we're in batch mode...		*/
    assign
      top       = 999999
      first-row =      1
      last-row  = 999999
    .

  define variable dummy as character no-undo format "x(78)".
  define variable dumm2 as character no-undo format "x(78)".

  for each tt_ui-hdr no-lock where tt_ui-hdr.display_type = this-procedure:private-data and tt_ui-hdr.display_active = yes
      break by tt_ui-hdr.display_type
            by tt_ui-hdr.display_variant:

    assign
      dummy = ""
      dumm2 = ""
    .

    /* If there are schema entries then do a header
     *
     */

    for each tt_ui-sch no-lock where
          tt_ui-sch.display_type    = tt_ui-hdr.display_type    and
          tt_ui-sch.display_variant = tt_ui-hdr.display_variant and
          lookup( "label", tt_ui-sch.data_key, "." ) > 0
        by tt_ui-sch.data_order:

      assign
        dummy = dummy + tt_ui-sch.data_value + " "
        dumm2 = dumm2 + fill( "-", length( tt_ui-sch.data_value )) + " "
      .

    end.

    assign
      dummy = substring( dummy, 1, length( dummy ) - 1 )
      dumm2 = substring( dumm2, 1, length( dumm2 ) - 1 )
    .

    if tt_ui-hdr.display_note <> "" then run add_vline( tt_ui-hdr.display_note ).
    if dummy <> "" then run add_vline( dummy ).
    if dumm2 <> "" then run add_vline( dumm2 ).

    dummy = "".

    /* Detail lines
     *
     */

    r = 0.

    for each tt_ui-det no-lock where
          tt_ui-det.display_type    = tt_ui-hdr.display_type    and
          tt_ui-det.display_variant = tt_ui-hdr.display_variant and
          tt_ui-det.data_row >= first-row and
          tt_ui-det.data_row <= last-row
        break by tt_ui-det.data_row
        by tt_ui-det.data_order:

      dummy = dummy + tt_ui-det.data_value + " ".

      if last-of( tt_ui-det.data_row ) then
        do:
          run add_vline( dummy ).
          dummy = "".
          r = r + 1.
        end.

    end.

  end.	/* for each ui_hdr... */

  /* Display the detail lines
   *
   */

  current-window = hWin.

  run paint.

  empty temp-table v_display.

  return.

end.

/** Initialize the PP
 **
 **/

this-procedure:private-data = det_type.

&IF "{&WINDOW-SYSTEM}" = "tty" &THEN

hWin = current-window.		/* there is only one window in tty mode	*/

on "close" of this-procedure do:
  run add_vline( "" ).
  run paint.
  delete procedure this-procedure.
end.

&ELSE

create window hWin.
assign
  hWin:title        = "ProTop release {&protop-version}  --  " + det_type
  hWin:width-chars  = 112 /** 112 **/
  hWin:height-chars = 24
  hWin:scroll-bars  = no
  hWin:message-area = no
  hWin:status-area  = no
  hwin:sensitive    = yes
  hWin:visible      = yes
.

on "close" of this-procedure do:
  delete object hWin.
  delete object this-procedure.
end.

on "window-close" of hWin do:
  apply "close" to this-procedure.
end.

&ENDIF

subscribe to "vdisplay" anywhere run-procedure "display_det".

return.
