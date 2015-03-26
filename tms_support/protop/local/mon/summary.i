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
 * summary.i
 *
 * local additions to the summary display
 *
 * local/out/summary.i		definition of frame elements
 * local/mon/summary.i		logic to populate frame elements
 * local/mon/summary.v		variable definitions
 *
 * by defualt the frame definitions and logic are commented out
 * in the sample code
 *
 */

/*** an example of interesting local summary data -- uses the sports2000 db
 ***
 ***

/* Don't check with every monitoring interval -- that would put an
 * inappropriate load on the system for this example.  Other situations
 * may be different.
 * 
 */

if time >= next-chk then
  do:

    assign
      order-tot = 0
      order-vol = 0
      next-chk  = time + 300.
    .

    for each order no-lock where order.orderdate = today:

      order-tot = order-tot + 1.

      for each orderline no-lock where orderline.ordernum = order.ordernum:
        order-vol = order-vol + orderline.extendedprice.
      end.

    end.

    order-vol = order-vol / 1000.	/* report in 1,000s of dollars */


  end.

    ui-det( support, 1, 1,  9, "d09", string( order-vol, "$>>>>>>>9K" )).
    ui-det( support, 1, 1, 19, "d19", string( order-tot,   ">>>>>>>9" )).

 *** end of example
 ***
 ***/

/*** end summary.i ***/
