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
 * tt_table.i
 *
 */

define temp-table tt_table no-undo
  field xid      as integer
  field xvalid   as logical
  field tblname  as character
  field ttbl     as character
  field tbl-cre  as integer extent 5
  field tbl-rd   as integer extent 5
  field tbl-upd  as integer extent 5
  field tbl-del  as integer extent 5
  index xid-idx is unique primary xid.

procedure upd-tt_table:

  define input parameter p_tbl as integer no-undo.

  define variable x as integer no-undo.
  define variable z as integer no-undo.

  do-SumSample( output x, output z ).

/**
  find dictdb._File no-lock where _File._File-num = p_tbl no-error.
  find dictdb._TableStat no-lock where _TableStat-id = p_tbl no-error.
 **/

  find dictdb._TableStat no-lock where _TableStat-id = p_tbl no-error.
  find dictdb._File no-lock where _File._File-num = _TableStat-id no-error.

  if not available dictdb._File then return.

  find tt_table exclusive-lock where tt_table.xid = p_tbl no-error.

  if not available tt_table then
    do:
      create tt_table.
      assign
        tt_table.xid      = p_tbl
        tt_table.xvalid   = yes
        tt_table.TblName  = ( if available( _TableStat ) then "" else "*" ) + _File._File-name
      .
      if available _TableStat then		/* _TableStat range may not be active...	*/
        assign
          {lib/init-xrec.i tt_table.tbl-cre _TableStat-create}
          {lib/init-xrec.i tt_table.tbl-rd  _TableStat-read}
          {lib/init-xrec.i tt_table.tbl-upd _TableStat-update}
          {lib/init-xrec.i tt_table.tbl-del _TableStat-delete}
        .
    end.
   else
    tt_table.xvalid = yes.

  if available _TableStat then		/* _TableStat range may not be active...	*/
    do:

      assign
        {lib/upd-xrec.i tt_table.tbl-cre _TableStat-create}
        {lib/upd-xrec.i tt_table.tbl-rd  _TableStat-read}
        {lib/upd-xrec.i tt_table.tbl-upd _TableStat-update}
        {lib/upd-xrec.i tt_table.tbl-del _TableStat-delete}
      .

      /* This uses the *base* statistics rather than the sampled data -- so it reveals
       * the historical pattern rather than current usage.
       */

      /***
      if ( _TableStat-create > 100 and _TableStat-delete > 100 ) and
         ( abs( _TableStat-create - _TableStat-delete ) < ( _TableStat-create * 0.2 )) then
        tt_table.ttbl = "   ***".
       else
        tt_table.ttbl = "".
       ***/

      if ( tt_table.tbl-cre[x] > 10 and tt_table.tbl-del[x] > 10 ) and
         ( abs( tt_table.tbl-cre[x] - tt_table.tbl-del[x] ) < ( tt_table.tbl-cre[x] * 0.2 )) then
        tt_table.ttbl = "   ***".
       else
        tt_table.ttbl = "".

    end.

  return.

end.

/* this will be called by the including procedure's mon-update procedure
 *
 */

procedure age_table:

/***
 ***
  define variable x as integer no-undo.
  define variable z as integer no-undo.

  do-SumSample( output x, output z ).
 ***
 ***/

  for each tt_table exclusive-lock:

    if tt_table.xvalid = no then
      delete tt_table.
     else
      assign
        tt_table.xvalid = no
/***
 ***
        {lib/upd-xrec.i tt_table.stat1 tt_table.stat1[3]}
        {lib/upd-xrec.i tt_table.stat2 tt_table.stat2[3]}
        {lib/upd-xrec.i tt_table.stat3 tt_table.stat3[3]}
        tt_xstat.stat-ratio  = 100 * (( tt_xstat.stat1[x] - tt_xstat.stat2[x] ) / tt_xstat.stat1[x] )
        tt_xstat.stat-ratio  = ( if tt_xstat.stat-ratio = ? then 0 else tt_xstat.stat-ratio )
 ***
 ***/
      .

  end.

  return.

end.
