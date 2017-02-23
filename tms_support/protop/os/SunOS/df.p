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
 * df.p
 *
 *
 * Free disk space (Specific to Sun).
 *
 *	$ df -k
 *	Filesystem            kbytes    used   avail capacity  Mounted on
 *	/dev/vx/dsk/rootvol  4131384 1036087 3053984    26%    /
 *	/proc                      0       0       0     0%    /proc
 *	fd                         0       0       0     0%    /dev/fd
 *	mnttab                     0       0       0     0%    /etc/mnttab
 *	swap                 6795056      32 6795024     1%    /var/run
 *	swap                 6797904    2880 6795024     1%    /tmp
 *	/dev/vx/dsk/array/db5
 *	                     8388608 5290184 2904831    65%    /db5
 *	/dev/vx/dsk/array/db6
 *	                     8388608 5290232 2904786    65%    /db6
 *	/dev/vx/dsk/array/db7
 *	                     8388608 5290224 2904794    65%    /db7
 *	/dev/vx/dsk/array/db2
 *	                     8388608 5290240 2904779    65%    /db2
 *	/dev/vx/dsk/array/db1
 *	                     8388608 5290240 2904779    65%    /db1
 *	/dev/vx/dsk/array/hsto
 *	                     8388608 1498501 6489451    19%    /hsto
 *	/dev/vx/dsk/array/db3
 *	                     8388608 5290200 2904816    65%    /db3
 *	/dev/vx/dsk/array/admin
 *	                     8388608 7756342  593207    93%    /admin
 *	/dev/vx/dsk/array/db4
 *	                     8388608 5290232 2904786    65%    /db4
 *	/dev/vx/dsk/array/db8
 *	                     8388608 7704088  641796    93%    /db8
 *	/dev/vx/dsk/array/progress
 *	                     1048576  633763  388902    62%    /progress
 *	/dev/vx/dsk/datadg/ai1
 *	                     2097152  279119 1704411    15%    /ai1
 *	/dev/vx/dsk/datadg/bi2
 *	                     2097152  476808 1519075    24%    /bi2
 *	/dev/vx/dsk/datadg/bi1
 *	                     2097152  543144 1456885    28%    /bi1
 *	/dev/vx/dsk/datadg/ai2
 *	                     2097152  148175 1827171     8%    /ai2
 *	/dev/vx/dsk/datadg/dupcalls
 *	                     4194304 2436960 1647549    60%    /dup_calls
 *	/dev/vx/dsk/datadg/misc
 *	                     16777216  193343 15547963     2%    /misc
 *	/dev/vx/dsk/datadg/backup
 *	                     83886080 72892339 10306665    88%    /backup
 *	/dev/vx/dsk/crash    4608000    2233 4317914     1%    /var/crash
 *	/dev/vx/dsk/local    2097152  253963 1728043    13%    /usr/local
 *	/dev/vx/dsk/users    2097152 1732470  341943    84%    /usr/users
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	May 3, 2005
 *
 */

{../tms_support/protop/lib/protop.i}

define variable support as character no-undo initial "Disk Free".

define temp-table tt_disk-free no-undo
  field mnt       as character
  field xvalid    as logical
  field dev       as character
  field threshold as integer
  field fs-size   as integer
  field fs-free   as integer
  field fs-grow   as integer extent 5
  field pct-used  as decimal
  field alert     as character format "x(3)"
  index mnt-idx is unique primary mnt.

define temp-table tt_df_cfg no-undo
  field order       as integer
  field xtype       as character
  field pattern     as character
  field threshold   as decimal
.

procedure update_disk-free:

  define input parameter p_mnt      as character no-undo.
  define input parameter p_dev      as character no-undo.
  define input parameter p_fs-size  as integer   no-undo.
  define input parameter p_fs-free  as integer   no-undo.
  define input parameter p_thold    as decimal   no-undo.

  find tt_disk-free exclusive-lock where tt_disk-free.mnt = p_mnt no-error.

  if available tt_disk-free then
    do:

      assign
        tt_disk-free.xvalid     = yes				/* is this xid active?		*/
        tt_disk-free.mnt        = p_mnt
        tt_disk-free.dev        = p_dev
        tt_disk-free.threshold  = p_thold
        tt_disk-free.fs-size    = p_fs-size
        tt_disk-free.fs-free    = p_fs-free
        tt_disk-free.fs-grow[3] = p_fs-free
      .

    end.
   else
    do:

      create tt_disk-free.
      assign
        tt_disk-free.xvalid     = yes				/* is this xid active?		*/
        tt_disk-free.mnt        = p_mnt
        tt_disk-free.dev        = p_dev
        tt_disk-free.threshold  = p_thold
        tt_disk-free.fs-size    = p_fs-size
        tt_disk-free.fs-free    = p_fs-free
        {../tms_support/protop/lib/init-xrec.i tt_disk-free.fs-grow p_fs-free}
      .

    end.

  return.

end.

procedure age_disk-free:

  for each tt_disk-free exclusive-lock:

    if tt_disk-free.xvalid = no then
      delete tt_disk-free.
     else
      assign
        tt_disk-free.xvalid = no
        {../tms_support/protop/lib/upd-xrec.i tt_disk-free.fs-grow tt_disk-free.fs-grow[3]}
        tt_disk-free.pct-used  = 100 * (( tt_disk-free.fs-size - tt_disk-free.fs-free ) / tt_disk-free.fs-size )
        tt_disk-free.pct-used  = ( if tt_disk-free.pct-used = ? then 0 else tt_disk-free.pct-used )
        tt_disk-free.alert =  ( if tt_disk-free.pct-used >= tt_disk-free.threshold then "*****" else "" )
      .

  end.

  return.

end.

/* restart
 *
 */

procedure mon-restart:

  empty temp-table tt_disk-free.

  delete procedure this-procedure.

  return.

end.

/* initialize
 *
 */

procedure mon-init:

  define variable t as decimal no-undo.
  define variable xorder as character no-undo.
  define variable xthold as character no-undo.

  empty temp-table tt_disk-free.
  empty temp-table tt_df_cfg.

  input from value( searchDir( "etc/df.cfg" )).      /* Thanks Sam! */ 

  repeat on endkey undo, leave:

    assign
      xorder = ""
      xthold = ""
    .

    create tt_df_cfg.
    import
      xorder
      tt_df_cfg.xtype
      tt_df_cfg.pattern
      xthold
    .

    if xorder begins "#" or xorder = "" then
      do:
        delete tt_df_cfg.
        next.
      end.

    assign
      tt_df_cfg.order = integer( xorder )
      tt_df_cfg.threshold = decimal( xthold )
    .

  end.

  delete tt_df_cfg.	/* delete the last line read -- it is always bogus.	*/
 
  input close.

  find tt_df_cfg no-lock where tt_df_cfg.xtype = "default" no-error.
  if available tt_df_cfg then
    do:
      t = tt_df_cfg.threshold.
      for each tt_df_cfg exclusive-lock:
        if tt_df_cfg.threshold = 0 or tt_df_cfg.threshold = ? then tt_df_cfg.threshold = t.
      end.
    end.

  /* define labels
   *
   */

  ui-define-label( support, 1, 1, "Mnt",     "Mount Point                   " ).
/***
  ui-define-label( support, 1, 2, "Dev",     "Device              " ).
 ***/
  ui-define-label( support, 1, 3, "FSSZ",    "   FS Size" ).
  ui-define-label( support, 1, 4, "FSFree",  "   FS Free" ).
  ui-define-label( support, 1, 5, "PctUsed", "  Used%" ).
  ui-define-label( support, 1, 6, "Growth",  "Growth" ).
  ui-define-label( support, 1, 7, "Alert",   "Alert" ).

  return.

end.

/* update
 *
 */

procedure mon-update:

  define variable inline   as character no-undo extent 20.

  define variable xmnt     as character no-undo format "x(30)".
  define variable xdev     as character no-undo format "x(80)".
  define variable xfs-size as integer   no-undo format ">>>>>>>>9".
  define variable xfs-free as integer   no-undo format ">>>>>>>>9".

  define variable ok as logical no-undo.
  define variable thold as decimal no-undo.

  if do-update( support ) = no then return.

  input through value( "df -k" ).

  import ^.	/* eat the header line	*/

  repeat:

    assign
      inline   = ""
      xmnt     = ""
      xdev     = ""
      xfs-size =  0
      xfs-free =  0
      thold    = 90
    .

    import inline no-error.
    xdev = inline[1].
    if inline[2] > "" then
      assign
        xmnt     = inline[6]
        xfs-size =  integer( inline[2] )
        xfs-free =  integer( inline[4] )
      no-error.
     else
      do:
        import inline no-error.
        assign
          xmnt     = inline[5]
          xfs-size =  integer( inline[1] )
          xfs-free =  integer( inline[3] )
        no-error.
      end.

/***
    import xdev xfs-size ^ xfs-free ^ xmnt no-error.
    if xfs-size = 0 then import xfs-size ^ xfs-free ^ xmnt no-error.		/* xdev was too long and the line was split	*/
 ***/

    if xmnt = "" or error-status:num-messages > 0 then next.

    check_pattern: do:

      find tt_df_cfg no-lock where tt_df_cfg.xtype = "default" no-error.
      if available tt_df_cfg then thold = tt_df_cfg.threshold.

      ok = yes.

      for each tt_df_cfg no-lock where tt_df_cfg.xtype = "include" by tt_df_cfg.order:
        if xmnt matches tt_df_cfg.pattern then
          do:
            if tt_df_cfg.threshold > 0 then thold = tt_df_cfg.threshold.
            leave check_pattern.
          end.
      end.

      for each tt_df_cfg no-lock where tt_df_cfg.xtype = "exclude" by tt_df_cfg.order:
        if xmnt matches tt_df_cfg.pattern then
          do:
            ok = no.
            leave check_pattern.
          end.
      end.

    end.

    if ok then run update_disk-free( xmnt, xdev, xfs-size, xfs-free, thold ).

  end.

  input close.

  run age_disk-free.

  define variable i as integer no-undo.
  define variable x as integer no-undo.
  define variable z as integer no-undo.

  define variable sort-criteria as character no-undo case-sensitive.

  publish "get-sort-criteria" ( output sort-criteria ).

  do-display( support, 1, 100, 11, 1, "Free Disk Space" ).

  do-SumSample( output x, output z ).

  define query q for tt_disk-free.

  /*** open query q for each tt_disk-free no-lock. ***/

  case sort-criteria:
    when "s" then open query q for each tt_disk-free no-lock by tt_disk-free.fs-size    descending.
    when "f" then open query q for each tt_disk-free no-lock by tt_disk-free.fs-free    descending.
    when "m" then open query q for each tt_disk-free no-lock by tt_disk-free.mnt.
    when "d" then open query q for each tt_disk-free no-lock by tt_disk-free.dev.
    when "u" then open query q for each tt_disk-free no-lock by tt_disk-free.pct-used   descending.
    when "a" then open query q for each tt_disk-free no-lock by tt_disk-free.alert.
    when "S" then open query q for each tt_disk-free no-lock by tt_disk-free.fs-size.
    when "F" then open query q for each tt_disk-free no-lock by tt_disk-free.fs-free.
    when "M" then open query q for each tt_disk-free no-lock by tt_disk-free.mnt        descending.
    when "D" then open query q for each tt_disk-free no-lock by tt_disk-free.dev        descending.
    when "U" then open query q for each tt_disk-free no-lock by tt_disk-free.pct-used.
    when "A" then open query q for each tt_disk-free no-lock by tt_disk-free.alert      descending.
    otherwise     open query q for each tt_disk-free no-lock by tt_disk-free.pct-used   descending.
  end.

  do while true:

    get next q.

    if not available tt_disk-free then leave.

    i = i + 1.

    ui-det( support, 1, i,  1, "Mnt",     string( tt_disk-free.mnt,      "x(30)" )).
/***
    ui-det( support, 1, i,  2, "Dev",     string( tt_disk-free.dev,      "x(20)" )).
 ***/
    ui-det( support, 1, i,  3, "FSSZ",    string( tt_disk-free.fs-size,  ">>>>>>>>>9" )).
    ui-det( support, 1, i,  4, "FSFree",  string( tt_disk-free.fs-free,  ">>>>>>>>>9" )).
    ui-det( support, 1, i,  5, "PctUsed", string( tt_disk-free.pct-used, ">>9.99%" )).
    ui-det( support, 1, i,  6, "Growth",  string( ( -1 * tt_disk-free.fs-grow[x] / z ), "->>>>9" )).
    ui-det( support, 1, i,  7, "Alert",   tt_disk-free.alert ).

  end.

  close query q.

  return.

end.

/** Initialize PP
 **
 **/

subscribe to "mon-restart" anywhere run-procedure "mon-restart".
subscribe to "mon-init"    anywhere run-procedure "mon-init".
subscribe to "mon-update"  anywhere run-procedure "mon-update".

publish "register-disp-type" ( input support ).

return.
