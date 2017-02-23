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
 * tt_index.i
 *
 */

define temp-table tt_index no-undo
  field xid        as integer
  field xvalid     as logical
  field idxname    as character
  field tblname    as character
  field idxnote    as character
  field idx-blkdel as integer extent 5
  field idx-cre    as integer extent 5
  field idx-del    as integer extent 5
  field idx-rd     as integer extent 5
  field idx-split  as integer extent 5
  index xid-idx is unique primary xid.

procedure upd-tt_index:

  define input parameter p_idx as integer no-undo.

  find dictdb._IndexStat no-lock where _IndexStat-id = p_idx no-error.
  find dictdb._Index no-lock where _Index._Idx-num = _IndexStat-id no-error.

  if not available _Index then return.

  find dictdb._File no-lock where recid( _File ) = _Index._File-recid no-error.

/***
  find dictdb._Index no-lock where _Index._Idx-num = p_idx no-error.
  find dictdb._IndexStat no-lock where _IndexStat-id = _Index._Idx-num no-error.
 ***/

  find tt_index exclusive-lock where tt_index.xid = p_idx no-error.

  if not available tt_index then
    do:
      create tt_index.
      assign
        tt_index.xid      = p_idx
        tt_index.xvalid   = yes
        tt_index.idxname  = ( if available( _IndexStat ) then "" else "*" ) + _Index._Index-name
        tt_index.tblname  = ( if available( _File ) then _File._File-name else "" )
        tt_index.idxnote  = 
          ( if dictdb._file._prime-index = recid( _index ) then "P" else "" ) +
          ( if _index._unique then "U" else "" )
      .
      if available _IndexStat then		/* _IndexStat range may not be active...	*/
        assign
          {../tms_support/protop/lib/init-xrec.i tt_index.idx-blkdel _IndexStat-blockdelete}
          {../tms_support/protop/lib/init-xrec.i tt_index.idx-cre    _IndexStat-create}
          {../tms_support/protop/lib/init-xrec.i tt_index.idx-del    _IndexStat-delete}
          {../tms_support/protop/lib/init-xrec.i tt_index.idx-rd     _IndexStat-read}
          {../tms_support/protop/lib/init-xrec.i tt_index.idx-split  _IndexStat-split}
        .
    end.
   else
    tt_index.xvalid = yes.

  if available _IndexStat then		/* _IndexStat range may not be active...	*/
    assign
      {../tms_support/protop/lib/upd-xrec.i tt_index.idx-blkdel _IndexStat-blockdelete}
      {../tms_support/protop/lib/upd-xrec.i tt_index.idx-cre    _IndexStat-create}
      {../tms_support/protop/lib/upd-xrec.i tt_index.idx-del    _IndexStat-delete}
      {../tms_support/protop/lib/upd-xrec.i tt_index.idx-rd     _IndexStat-read}
      {../tms_support/protop/lib/upd-xrec.i tt_index.idx-split  _IndexStat-split}
    .

  return.

end.

/* this will be called by the including procedure's mon-update procedure
 *
 */

procedure age_index:

/***
 ***
  define variable x as integer no-undo.
  define variable z as integer no-undo.

  do-SumSample( output x, output z ).
 ***
 ***/

  for each tt_index exclusive-lock:

    if tt_index.xvalid = no then
      delete tt_index.
     else
      assign
        tt_index.xvalid = no
/***
 ***
        {../tms_support/protop/lib/upd-xrec.i tt_index.stat1 tt_index.stat1[3]}
        {../tms_support/protop/lib/upd-xrec.i tt_index.stat2 tt_index.stat2[3]}
        {../tms_support/protop/lib/upd-xrec.i tt_index.stat3 tt_index.stat3[3]}
        tt_xstat.stat-ratio  = 100 * (( tt_xstat.stat1[x] - tt_xstat.stat2[x] ) / tt_xstat.stat1[x] )
        tt_xstat.stat-ratio  = ( if tt_xstat.stat-ratio = ? then 0 else tt_xstat.stat-ratio )
 ***
 ***/
      .

  end.

  return.

end.
