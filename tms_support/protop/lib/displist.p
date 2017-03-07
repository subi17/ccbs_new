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
 * displist.p
 *
 * manage selection list for display types
 *
 * October 7, 2003 */

{protop/lib/protop.i}

define variable s as character no-undo
  view-as selection-list multiple inner-chars 30 inner-lines 15 sort scrollbar-vertical.

define button b_ok     label "Ok"     auto-go default.
define button b_cancel label "Cancel" auto-end-key.

form space(4) s space(4) skip(1)
     space(6) b_ok space(6) b_cancel skip
  with frame a
    color value( "blue/white" ) 
    title color value( "cyan" ) " Display Types "
    overlay row 2 column 10 no-labels.

procedure upd-display:

  define variable sx  as character no-undo.
  define variable cp  as handle    no-undo.
  define variable cpx as handle    no-undo.
  define variable i   as integer   no-undo.
  define variable z   as logical   no-undo.

  publish "get-curr-disp" ( output sx ).

  /* make sure that every display that has been selected is actually running
   */

  do i = 1 to num-entries( sx ):
    z = no.
    cp = session:first-procedure.
    do while valid-handle( cp ).					/* shouldn't we only look at "detail.p" procedures?	*/
      if cp:private-data = entry( i, sx ) then do: z = yes. leave. end.
      cp = cp:next-sibling.
    end.
    if z = no then
      do:
        if entry( i, sx ) = "Summary" then	/* "Summary" is special... for now	*/
          .
         else
          RUN out/detail.p persistent ( entry( i, sx )).
      end.

  end.

  /* make sure that *only* displays that have been selected are running
   */

  cp = session:first-procedure.
  do while valid-handle( cp ).
    if ( r-index( cp:file-name, "detail.p" ) > 1 ) and
       ( lookup( cp:private-data, sx ) = 0 ) and
       ( cp:private-data <> "Summary" ) then	/* "Summary" is special... for now	*/
      do:
        cpx = cp:prev-sibling.
        apply "close" to cp.
        cp = cpx.
      end.
    cp = cp:next-sibling.
  end.

  return.

end.

procedure pick-active:

  define variable p_available as character no-undo.
  define variable p_active    as character no-undo.

  define variable sx  as character no-undo.

  do with frame a view-as dialog-box:

    publish "get-disp-type-list" ( output p_available ).
    publish "get-curr-disp" ( output p_active ).

    assign
      s:list-items   = p_available
      s:screen-value = p_active
      /* s:inner-lines  = screen-lines - 8 */	/* argh!  must be a constant :-(	*/
    .

    s:scroll-to-item(1).

    color display value( "green/white" ) s with frame a.
    color display value( "blue/white" ) b_ok with frame a.
    color display value( "red" ) b_cancel with frame a.

    view frame a.

    if "{&window-system}" = "tty" then
      status input '<Space>/<Enter> to select, <F1>/^X to save, <F4>/^E to quit.'.

    enable s b_ok b_cancel with frame a.

    wait-for "go", "endkey" of frame a.

    sx = input s:input-value.

    hide frame a no-pause.

  end.

  if last-event:function = "endkey" then return.

  publish "set-curr-disp" ( input sx ).
  publish "upd-display".

  return.

end.

subscribe to "pick-curr-disp" anywhere run-procedure "pick-active".
subscribe to "upd-display" anywhere run-procedure "upd-display".

return.
