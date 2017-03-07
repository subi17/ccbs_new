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
 * tt_servers.i
 *
 */

define temp-table tt_servers no-undo
  field xid      as integer
  field srvnum   as integer
  field srvtyp   as character
  field srvport  as integer
  field curr-usr as integer
  field max-usr  as integer
  field msg-recv as integer extent 5
  field msg-sent as integer extent 5
  field rec-recv as integer extent 5
  field rec-sent as integer extent 5
  field qry-recv as integer extent 5
  field tm-intr  as integer extent 5
  index xid-idx is unique primary xid.

procedure upd-servers:

  define input parameter p_svr as integer no-undo.

  for each dictdb._Servers no-lock where _Servers._Server-num = p_svr,
      first dictdb._ActServer where dictdb._ActServer._Server-Id = dictdb._Servers._Server-Id:

    find tt_servers exclusive-lock where tt_servers.xid = _Servers._Server-Id no-error.
    if not available tt_servers then
      do:

        create tt_servers.
        assign
          tt_servers.xid      = _Servers._Server-Id
          tt_servers.srvnum   = _Servers._Server-num
          tt_servers.srvtyp   = _Servers._Server-type
          tt_servers.srvport  = _Servers._Server-port
          tt_servers.curr-usr = _Servers._Server-CurrUsers
          tt_servers.max-usr  = _Servers._Server-MaxUsers
          {protop/lib/init-xrec.i tt_servers.msg-recv _ActServer._Server-MsgRec}
          {protop/lib/init-xrec.i tt_servers.msg-sent _ActServer._Server-MsgSent}
          {protop/lib/init-xrec.i tt_servers.rec-recv _ActServer._Server-RecRec}
          {protop/lib/init-xrec.i tt_servers.rec-sent _ActServer._Server-RecSent}
          {protop/lib/init-xrec.i tt_servers.qry-recv _ActServer._Server-QryRec}
          {protop/lib/init-xrec.i tt_servers.tm-intr  _ActServer._Server-TimeSlice}
        .

      end.

    assign
      {protop/lib/upd-xrec.i tt_servers.msg-recv _ActServer._Server-MsgRec}
      {protop/lib/upd-xrec.i tt_servers.msg-sent _ActServer._Server-MsgSent}
      {protop/lib/upd-xrec.i tt_servers.rec-recv _ActServer._Server-RecRec}
      {protop/lib/upd-xrec.i tt_servers.rec-sent _ActServer._Server-RecSent}
      {protop/lib/upd-xrec.i tt_servers.qry-recv _ActServer._Server-QryRec}
      {protop/lib/upd-xrec.i tt_servers.tm-intr  _ActServer._Server-TimeSlice}
    .

  end.

  return.

end.
