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
 * attribute.i
 *
 * Manage public attributes
 *
 * There are 3 events defined by this include file:
 *
 *	get-	This event returns the value of the attribute.  The result is
 *		passed as a parameter.
 *
 *	set-	This event sets the value of the attribute.  The new value is
 *		passed as a parameter.
 *
 *	adj-	This event prompts for the value of the attribute unless we're
 *		in batch mode.  If we're in batch mode then it acts just like
 *		"set-" (by calling "set-").
 *
 * Arguments:
 *
 *  {1} The attribute being manipulated.
 *  {2} The attributes datatype.
 *  {3} Formatting & initialization.
 *  {4} Prompt text for interactive update.
 *  {5} Conversion from string to {2} (blank for string)
 *  {6} Special code (enforce limits)
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
 *      Accepted changes from Patrick Tingen to permit rollback of a variable
 *      if it fails validity checking via {6}
 *      October 30, 2003
 * 
 */

define variable {1} as {2} no-undo {3}.

procedure get-{1}:

  define output parameter p_{1} as {2} no-undo.

  p_{1} = {1}.    

  return.         

end.

procedure set-{1}:

  define input parameter p_{1} as {2} no-undo.

  if "{1}" = "curr-disp" and p_{1} = ? then	/* don't let curr-disp get set to ?	*/
    assign {1} = {5}( "" ) no-error.
   else
    {1} = p_{1}.

  if "{1}" = "curr-disp" then		/* if we're setting curr-disp publish a "set-" event	*/
    publish "set-" + string( p_{1} ).	/* for the *value* of curr-disp!			*/

  return.         

end.

procedure adj-{1}:

  define input parameter p as character no-undo.

  define variable NewValue like {1} no-undo.

  if interactive = no then
    assign NewValue = {5}( p ) no-error.
   else 
    do:
      if "{1}" = "curr-disp" then	/* a truly breathtaking kludge...	*/
        do:
          publish "pick-curr-disp".	/* but it allows a selection list ;)	*/
          return.
        end.
       else 
        do:
          publish "get-{1}" ( output NewValue ).
          message "{4}" update NewValue.
        end.
    end.

  {6}	/* special circumstances. */

  publish "set-{1}" ( input NewValue ).

  return.

end.

subscribe to "set-{1}" anywhere run-procedure "set-{1}".
subscribe to "get-{1}" anywhere run-procedure "get-{1}".
subscribe to "adj-{1}" anywhere run-procedure "adj-{1}".

/* end attribute.i */
