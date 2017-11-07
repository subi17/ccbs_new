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
 * command.p
 *
 *
 * A publish & subscribe based command processor
 *
 *
 * Subscribes to:
 *
 *	command-line
 *	process-command
 *
 *
 * Known Bugs & Issues:
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
 *	Accepted changes from Patrick Tingen to set html output dir and
 *	eliminating the nasty curr-page shared variable.
 *	October 30, 2003
 *
 */

{protop/lib/protop.i}

/************************************************************
 ************************************************************

 The Command Processor

 ************************************************************
 ************************************************************/

/** figure out what to do...
 **
 **/

procedure process-command:

  define input parameter c  as character case-sensitive no-undo.
  define input parameter p  as character no-undo.

  define variable iCurrPage as integer   no-undo.

  case c:

    when "?" then publish "command-help" ( "Commands.hlp" ).
    when "h" then publish "command-help" ( "ProTop.hlp" ).
    when "H" then publish "command-help" ( "" ).

    when "b" then publish "adj-sort-criteria" ( input p ).
    when "c" then publish "connect-database".
    when "d" then publish "adj-curr-disp" ( input p ).
    when "D" then publish "adj-html-dir" ( input p ).
    when "e" then publish "adj-examine-item" ( input p ).

    when "i" then publish "adj-interval" ( input p ).
    when "m" then publish "change-database".
    when "o" then publish "command-options".
    when "O" then publish "adj-opsys-dir" ( input p ).
    when "p" then publish "adj-curr-page" ( input p ).
    when "q" then publish "command-quit".

    when "r" then publish "set-RateRaw" ( input "r" ).
    when "R" then publish "set-RateRaw" ( input "R" ).
    when "s" then publish "set-SumSample" ( input "s" ).
    when "S" then publish "set-SumSample" ( input "S" ).

    when "t" then publish "adj-tlist" ( input p ).
    when "u" then publish "adj-xuser" ( input p ).
    when "w" then publish "mon-reload".
    when "W" then publish "adj-doweb" ( input p ).
    when "x" then publish "adj-xlist" ( input p ).
    when "z" then
      do:
        publish "mon-init".
        init-timer().
      end.

    when "+" or 
    when "-" then
      do: 
        publish "get-curr-page" ( output iCurrPage ).
        if c = '+' then
          iCurrPage = iCurrPage + 1.
         else
          iCurrPage = iCurrPage - 1.
        if iCurrPage <= 0 then iCurrPage = 1.
        publish "set-curr-page" ( input iCurrPage ).
      end.

  end case.

  return.

end.

/** Initial commands passed in via -param
 **
 ** Format is pipe delimited commands with the command separated from
 ** any data with a semi-colon.  Commands are a single character.  Command
 ** data (if any) is a comma delimited list.  For instance:
 **
 **	-param "i;5|d;Summary,User IO"
 **
 **/

procedure command-line:

  define variable i as integer no-undo.		/* no program is complete without "i" :-)	*/
  define variable j as integer no-undo.		/* and j is icing on the cake ;-)		*/
  define variable rc as character no-undo.
  define variable c  as character no-undo.
  define variable p  as character no-undo.

  interactive = no.

  do i = 1 to num-entries( session:parameter, "|" ):

    assign
      rc = entry( i, session:parameter, "|" )
      c  = entry( 1, rc, ";" )
    .

  if num-entries( rc, ";" ) = 1 then
    publish "process-command" ( input c, input "" ).
   else
    publish "process-command" ( input c, entry( 2, rc, ";" )).

  end.

  interactive = yes.

  publish "upd-display".

  return.

end.

/**  Initialize PP
 **
 **/

subscribe to "command-line"       anywhere run-procedure "command-line".
subscribe to "process-command"    anywhere run-procedure "process-command".
subscribe to "mon-reload"         anywhere run-procedure "mon-reload".
subscribe to "mon-restart"        anywhere run-procedure "mon-restart".

return.
