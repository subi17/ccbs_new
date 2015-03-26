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

/*** available summary array elements
 ***
 ***
    "      xxx:"  d[9] d[19]          "xxx:" to 45 d[29] format "x(8)" d[39] format "x(8)"     "xxx:" to 74 d[49] format "x(5)" to 80 skip
    d[81] d[82] d[83] d[84] d[85] d[86] d[87] d[88] d[89] skip
    d[91] d[92] d[93] d[94] d[95] d[96] d[97] d[98] d[99] skip
 ***
 ***
 ***/

/*** An example of local summary information
 ***
 ***

    skip(1)
    "Order Vol:"  d[9] d[19] format "x(8)" "Orders" skip

 ***
 ***
 ***/
