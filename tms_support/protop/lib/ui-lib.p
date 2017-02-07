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
 * ui-lib.p
 *
 *
 * Library of routines specific to user interface actions
 *
 *
 * Subscribes to:
 *
 *	command-help
 *	command-options
 *	connect-database
 *	change-database
 *	mon-reload
 *	mon-restart
 *
 *
 * Known Bugs & Issues:
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	October 31, 2003
 *
 *
 * History:
 *
 *	Integrated code from Marian Edu permitting database selection
 *	September 18, 2003
 *
 *	Accepted changes from Sam Paakki regarding "quit" function and PROPATH
 *	September 26, 2003
 *
 *	Accepted changes from Patrick Tingen to set html output dir,
 *	eliminating the nasty curr-page shared variable and automatically
 *	adjusting the height of the help screen.
 *	October 30, 2003
 *
 *      Migrated out of command.p
 *      October 31, 2003
 *
 */

{tms_support/protop/lib/protop.i}

define variable hWin as handle.

/* Thanks to Marian Edu for the sample code!
 *
 */

procedure connect-database:

  DEFINE VARIABLE p_dbname AS CHARACTER NO-UNDO.
  DEFINE VARIABLE p_lname  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE p_type   AS CHARACTER NO-UNDO INIT "PROGRESS".

  RUN adecomm/_dbconn.p(INPUT-OUTPUT p_dbname, INPUT-OUTPUT p_lname, INPUT-OUTPUT p_type).

  RUN lib/setwkdb.p ( input ldbname( p_dbname )).

end.

procedure change-database:

  define variable i as integer no-undo.

  define variable s as character no-undo
    view-as selection-list inner-chars 40 inner-lines 18 sort scrollbar-vertical.

  form s with frame a no-box no-labels overlay row 2 column 10.

  do i = 1 to num-dbs:
    if i = 1 then
      s:list-items = ldbname( i ).
     else
      s:list-items = s:list-items + "," + ldbname( i ).
  end.

  s = ldbname( "dictdb" ).

  update s go-on( "enter" ) with frame a.

  RUN lib/setwkdb.p ( input s:screen-value ).

  /* That's nice -- but now that we've changed the alias we have to unload & reload
   * all running PPs before they will notice the change...
   *
   */

  publish "mon-reload".

end.

/* display command help
 *
 */

procedure show-help:

  define input parameter p_context as character no-undo.

  define variable i           as integer   no-undo.
  define variable iInnerLines as integer   no-undo.
  define variable cCurrDisp   as character no-undo.

  define variable cHelpText   as character no-undo
    view-as editor inner-chars 78 inner-lines 20 scrollbar-vertical.

  form
     cHelpText
   with
    frame show-help
    overlay
    no-box
    no-labels
    column 1
    stream-io
    width 80 /*** 84 ***/
    row 1.

  if "{&window-system}" = "tty" then
    hWin = current-window.
   else
    do:

      create window hWin.
      assign
        hWin:title        = "ProTop release {&protop-version}  Help"
        hWin:width-chars  = 83
        hWin:height-chars = 30
        hWin:scroll-bars  = no
        hWin:message-area = no
        hWin:status-area  = no
        hwin:sensitive    = yes
        hWin:visible      = yes
        current-window    = hWin
      .

    end.

  do with frame show-help:

    /* adjust frame height to take advantage of large screen */

    iInnerLines = minimum( 30, screen-lines ).
    frame show-help:height = iInnerLines + 2.
    cHelpText:inner-lines  = iInnerLines.

    if p_context <> "" then
      do:
        cHelpText:read-file( "hlp/" + p_context ).
        assign cHelpText.
      end.
     else
      do:
        publish "get-curr-disp" ( output cCurrDisp ).
        do i = 1 to num-entries( cCurrDisp ):
          cHelpText:insert-file( "hlp/" + nospace( entry( i, cCurrDisp )) + ".hlp" ).
          assign cHelpText.
        end.
      end.
  
    cHelpText:read-only = yes.
  
    display cHelpText.
  
    /* Tee hee :-)  I used an editing block!
     *
     */

    prompt-for cHelpText go-on( "enter" " " "q" )
      editing:
        readkey.
        apply lastkey.
      end.

  end. /* do with frame */

  hide frame show-help no-pause.

  if "{&window-system}" <> "tty" then
    delete object hWin.

  return.

end.

/* show the current options settings
 *
 */

procedure show-options:

  define variable curr-disp     as character no-undo format "x(20)".
  define variable examine-item  as integer   no-undo.
  define variable interval      as integer   no-undo.
  define variable show_top      as integer   no-undo.
  define variable iCurrPage     as integer    no-undo.
  define variable sort-criteria as character no-undo case-sensitive.
  define variable tlist         as character no-undo format "x(70)".
  define variable xlist         as character no-undo format "x(70)".

  define variable sc as character no-undo format "x(30)".

  publish "get-curr-disp" ( output curr-disp ).
  publish "get-examine-item" ( output examine-item ).
  publish "get-interval" ( output interval ).
  publish "get-show_top" ( output show_top ).
  publish "get-sort-criteria" ( output sort-criteria ).
  publish "get-tlist" ( output tlist ).
  publish "get-xlist" ( output xlist ).
  publish "get-curr-page" ( output iCurrPage ).

  sc = entry( lookup( sort-criteria, "i,I,c,C,n,N" ), "[i]nterval Descending,[I]nterval Ascending,[c]umulative Descending,[C]umulative Ascending,[n]ame Descending,[N]ame Ascending" ).

  display
    skip(1)
    "        Display (d):" curr-disp skip
    "   Examine Item (e):" string( examine-item ) skip
    "       Interval (i):" string( interval ) + " seconds" format "x(15)" skip
    "   Current Page (p):" string( iCurrPage ) skip
    "  Sort Criteria (b):" sc skip
    skip(1)
    "  (T)rack  (t):" skip
      tlist to 74     skip
    " e(X)clude (x):" skip
      xlist to 74
    skip(1)
   with
    frame show-options
    title " ProTop -- Current Option Settings "
    overlay
    no-labels
    row 2.

  pause.

  hide frame show-options no-pause.

  return.

end.

/**  Initialize PP
 **
 **/

subscribe to "command-help"       anywhere run-procedure "show-help".
subscribe to "command-options"    anywhere run-procedure "show-options".
subscribe to "connect-database"   anywhere run-procedure "connect-database".
subscribe to "change-database"    anywhere run-procedure "change-database".
subscribe to "mon-reload"         anywhere run-procedure "mon-reload".
subscribe to "mon-restart"        anywhere run-procedure "mon-restart".

return.
