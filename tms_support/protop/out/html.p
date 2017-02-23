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
 * html.p
 *
 * Generic HTML display procedure
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	October 2, 2003
 *
 *
 * History:
 *
 *	Improved HTML handling by Patrick Tingen
 *	October 15, 2003
 */
 
/* defs for frames and framesets to make it a bit easier to code */
&scoped-define frame_title    <frame src="title.html"     marginheight="1" scrolling="no">
&scoped-define frame_summary  <frame src="summary_1.html" marginheight="1" marginwidth="1">
&scoped-define frame_blank    <frame src="blank.html"     marginheight="1" marginwidth="1">
&scoped-define frameset_1row  <frameset rows="10%,90%"         frameborder="0">
&scoped-define frameset_2rows <frameset rows="10%,45%,45%"     frameborder="0">
&scoped-define frameset_3rows <frameset rows="10%,30%,30%,30%" frameborder="0">
&scoped-define frameset_2cols <frameset cols="50%,50%"         frameborder="0">

{../tms_support/protop/lib/protop.i}

define variable gcPrevActive  as character  no-undo. /* detect changes in active modules */
define variable gcPrevHtmlDir as character  no-undo. /* detect changes in html directory */

/* Main HTML output generator
 * 
 */
procedure html_display:
  define buffer xtt_ui-sch for tt_ui-sch.

  define variable d          as character  no-undo extent 55 format "x(10)".	/* dummy variable for display placeholder */
  define variable cCellAlign as character  no-undo. /* cell alignment */
  define variable cActive    as character  no-undo. /* active displays */
  define variable cDataValue as character  no-undo. /* value of the label */
  define variable cHtmlDir   as character  no-undo. /* output dir */


  /* Find out if the html dir has changed. In that case, we might need to 
   * recreate the static html / css files. 
   */
  publish "get-html-dir" (output cHtmlDir).
  if cHtmlDir <> gcPrevHtmlDir then RUN html_init.

  /* Find out if modules have been de-activated. If so, replace the current
   * html file with a blank page to avoid users looking at old data. If the 
   * module gets re-activated later, the actual data will reappear in the browser.
   */
  if cActive ne gcPrevActive and gcPrevActive ne "" then
  for each tt_ui-hdr no-lock 
      where tt_ui-hdr.display_active eq no
        and tt_ui-hdr.display_type   ne 'summary'
        and lookup(tt_ui-hdr.display_type,gcPrevActive) > 0:

      /* change contents of the page with that of the blank page */
      RUN html_blank ( cHtmlDir + substitute('&1_&2.html'
                                                , tt_ui-hdr.display_type
                                                , tt_ui-hdr.display_variant)).
  end.

  /* now, refresh the list of active displays for the next iteration. */
  publish "get-curr-disp" ( output gcPrevActive ).

  /* Generate blank screen. Note that this screen needs to be generated again 
   * in case there has been a change in active modules. 
   */
  RUN html_blank ( cHtmlDir + 'blank.html').

  /* Write module.
   * From release xi on the summary screen is optional. 
   */
  for each tt_ui-hdr no-lock 
    where tt_ui-hdr.display_active eq yes
    break by tt_ui-hdr.display_order
          by tt_ui-hdr.display_type:

    /* Open output to a new file and write title to it.
     * 
     */
    if first-of(tt_ui-hdr.display_type) then 
    do:
      output to value( cHtmlDir + 'temp.html').

      RUN html_header.

      /* different handling of summary and non-summary screen */
      if tt_ui-hdr.display_type = 'summary' then
      do:
        /* fill summary array */
        assign d = ''.
        for each tt_ui-det no-lock 
            where tt_ui-det.display_type = 'summary' 
              and tt_ui-det.data_row     = 1:
           d[tt_ui-det.data_order] = tt_ui-det.data_value.
        end.
      end.
      else 
      do:
        put unformatted 
                 '<body>' 
                   /*-- create table with 2 rows and 2 cols for data and links --*/
         skip ' <!-- table for separating data and links -->'
         skip '  <table cellspacing="0">' 
                /*-- put the header in the top row --*/
         skip '  <tr align="left" class="header">'
         skip '    <td align="left">' tt_ui-hdr.display_note '</td>'
         skip '    <td class="time">' string(time,'hh:mm:ss') '</tr>' 
                /*-- put table with data in left cell --*/
         skip '  <tr> <td width="90%">'
         skip ' ' 
         skip ' <!-- table with actual data -->'
         skip '  <table cellspacing="0">' 
         skip.
      end.
    end.    

    if tt_ui-hdr.display_type = 'summary' then
    do:
      /* Write summary table 
       *
       */
      put unformatted 
             '<body>' 
        skip '<table cellspacing="0">' 
        skip '  <tr align="left" class="header">'
        skip '    <td>' d[52] '&nbsp; - &nbsp; ' d[53] ' </td> <td class="time">' d[51] '</td></tr>'
        skip '  <tr>'
        skip '  <td width="90%">' 
        skip ' '
        skip '  <!-- summary info -->'
        skip '  <table cellspacing="0">'
        /* insert one row of empty cells to 'fix' the width of all columns */
        skip '    <tr><td width="16%">&nbsp;</td>'
        skip '        <td width="12%"></td>'
        skip '        <td width="12%"></td>'
        skip '        <td width="22%"></td>'
        skip '        <td width="9%"></td>'
        skip '        <td width="9%"></td>'
        skip '        <td width="10%"></td>'
        skip '        <td width="9%"></td> </tr>'
        skip '    <tr align="right"><td class="label">   Hit Ratio</td><td>' d[1]  '</td><td>' d[11] '</td><td class="label">     Commits</td><td>' d[21] '</td><td>' d[31] '</td><td class="label">  Local</td><td>' d[41] '</td></tr>'
        skip '    <tr align="right"><td class="label">      Miss% </td><td>' d[2]  '</td><td>' d[12] '</td><td class="label"> Latch Waits</td><td>' d[22] '</td><td>' d[32] '</td><td class="label"> Remote</td><td>' d[42] '</td></tr>'
        skip '    <tr align="right"><td class="label">       Hit% </td><td>' d[3]  '</td><td>' d[13] '</td><td class="label">Tot/Mod Bufs</td><td>' d[23] '</td><td>' d[33] '</td><td class="label">  Batch</td><td>' d[43] '</td></tr>'
        skip '    <tr align="right"><td class="label">   Log Reads</td><td>' d[4]  '</td><td>' d[14] '</td><td class="label">Evicted Bufs</td><td>' d[24] '</td><td>' d[34] '</td><td class="label"> Server</td><td>' d[44] '</td></tr>'
        skip '    <tr align="right"><td class="label">    OS Reads</td><td>' d[5]  '</td><td>' d[15] '</td><td class="label">  Lock Table</td><td>' d[25] '</td><td>' d[35] '</td><td class="label">  Other</td><td>' d[45] '</td></tr>'
        skip '    <tr align="right"><td class="label"> Checkpoints</td><td>' d[6]  '</td><td>' d[16] '</td><td class="label">Lock Tbl HWM</td><td>' d[26] '</td><td>' d[36] '</td><td class="label">    TRX</td><td>' d[46] '</td></tr>'
        skip '    <tr align="right"><td class="label">     Flushed</td><td>' d[7]  '</td><td>' d[17] '</td><td class="label"> Old/Curr BI</td><td>' d[27] '</td><td>' d[37] '</td><td class="label">Blocked</td><td>' d[47] '</td></tr>' 
        skip '    <tr align="right"><td class="label">ProTop eTime</td><td>' d[29] '</td><td>' d[39] '</td><td>'                d[49]   '</td><td>' d[50] '</td><td>'       '</td><td class="label">  Total</td><td>' d[48] '</td></tr>'
        .
    end. /* summary */
    else
    do:
      /* Write labels 
       * 
       */
      for each tt_ui-sch no-lock 
        where tt_ui-sch.display_type    = tt_ui-hdr.display_type 
          and tt_ui-sch.display_variant = tt_ui-hdr.display_variant 
          and lookup( "label", tt_ui-sch.data_key, "." ) > 0
        break by tt_ui-sch.data_row
              by tt_ui-sch.data_order:
  
        /* The "align" attribute -- mostly these default to "right"
         * 
         */
        find xtt_ui-sch no-lock 
          where xtt_ui-sch.display_type    = tt_ui-hdr.display_type  
            and xtt_ui-sch.display_variant = tt_ui-hdr.display_variant
            and xtt_ui-sch.data_order      = tt_ui-sch.data_order
            and lookup( "align", xtt_ui-sch.data_key, "." ) > 0
                no-error.
  
        if available( xtt_ui-sch ) then
          cCellAlign = substitute(' align="&1"',xtt_ui-sch.data_value).
         else
          cCellAlign = ''. /* inherit default alignment from <TR> */
  
        /* If the data-value is "Cumulative" or "Interval", make the description
         * shorter so the frame does not get too wide.
         */
        cDataValue = tt_ui-sch.data_value.
        cDataValue = replace( cDataValue, 'Cumulative','Cum').
        cDataValue = replace( cDataValue, 'Interval','Int').
  
        /* Output the header line
         * 
         */       
        if first-of( tt_ui-sch.data_row ) then 
            put unformatted skip '    <tr class="label" align="right">'.
  
        put unformatted 
          skip substitute('      <td&1>&2&3</td>'
                          , cCellAlign
                          , cDataValue
                          , (if not last-of( tt_ui-sch.data_row ) then '&nbsp;' else '')
                          ).
        
        if last-of( tt_ui-sch.data_row ) then 
            put unformatted skip '    </tr>'.
      end. /* labels */
  
      /* Detail lines
       *
       */
      for each tt_ui-det no-lock 
        where tt_ui-det.display_type    = tt_ui-hdr.display_type    
          and tt_ui-det.display_variant = tt_ui-hdr.display_variant 
        break by tt_ui-det.data_row
              by tt_ui-det.data_order:
  
        /* The "align" attribute -- mostly these default to "right"
         *
         */
        find xtt_ui-sch no-lock 
          where xtt_ui-sch.display_type    = tt_ui-hdr.display_type  
            and xtt_ui-sch.display_variant = tt_ui-hdr.display_variant 
            and xtt_ui-sch.data_order      = tt_ui-det.data_order 
            and lookup( "align", xtt_ui-sch.data_key, "." ) > 0
                no-error.
  
        if available( xtt_ui-sch ) then
          cCellAlign = substitute(' align="&1"',xtt_ui-sch.data_value).
         else
          cCellAlign = ''. /* inherit default alignment from <TR> */
  
        /* Output the detail line
         *
         */
        if first-of( tt_ui-det.data_row ) then put unformatted '    <tr align="right">'.
  
        put unformatted 
          skip substitute('      <td&1>&2&3</td>'
                           , cCellAlign
                           , tt_ui-det.data_value
                           , (if not last-of( tt_ui-det.data_row ) then '&nbsp;' else '')
                           ).
  
        if last-of( tt_ui-det.data_row ) then put unformatted skip '    </tr>'.
  
      end. /* Detail lines */
    end. /* non-summary */


    /* Close output
     *
     */
    if last-of(tt_ui-hdr.display_type) then 
    do:
      put unformatted 
        skip '  </table> </td> <!-- actual data ends here -->' 
        .

      RUN html_links.

      put unformatted 
        skip '  </tr> </table>'
        skip '</body>' 
        skip '</html>' 
        skip.
      output close.

      RUN html_rename ( input cHtmlDir + 'temp.html',
                        input cHtmlDir + substitute('&1_&2.html'
                                                , tt_ui-hdr.display_type
                                                , tt_ui-hdr.display_variant)).
    end.
  end.

  return.
end procedure. /* html_display */


/* Put general html header
 * 
 */
procedure html_header:
  define variable iInterval as integer    no-undo.

  /* get the interval from protop
   */
  publish "get-Interval" ( output iInterval ).
  if not (iInterval > 0) then iInterval = 10.
  
  put unformatted '<html>' 
    skip '<head>' 
    skip ' <title>ProTop</title>' 
    skip ' <link href="protop.css" REL="stylesheet" TYPE="text/css"/>' 
    skip substitute('   <meta http-equiv="refresh" content="&1"></meta>',iInterval) 
    skip '</head>' 
    skip ' ' 
    .
end procedure. /* html_header */


/* Provide links to other active screens
 * 
 */
procedure html_links:
  define buffer btt_ui-hdr for tt_ui-hdr.

  put unformatted
      skip ' '
      skip '    <!-- Links -->'
      skip '    <td align="right">'
      skip '    <small><br>'
      .

  for each btt_ui-hdr no-lock 
      where btt_ui-hdr.display_active = yes
      break by btt_ui-hdr.display_order
            by btt_ui-hdr.display_type:

      put unformatted
          skip substitute('      <A href="&1_&2.html">&1</a><br>', display_type, display_variant) 
          .
  end. 

  /* add link to a blank page */
  put unformatted
      skip '      <A href="blank.html">blank</a><br>' 
      skip '    </small>'
      skip '    __________<br>'
      skip '    split <A href="splitcols.html">col</a>'
      skip '          <A href="splitrows.html">row</a><br>'
      skip '    <A href="blank.html" target=_parent>unsplit</a><br>'
      skip '    </td>'
      .
end procedure. /* html_links */


/* Generate a blank page with only links to active displays
 * 
 */
procedure html_blank:
  define input  parameter cFileName as character  no-undo.
  define variable cHtmlDir as character  no-undo.

  publish "get-html-dir" (output cHtmlDir).

  output to value( cHtmlDir + 'temp.html').
  RUN html_header.
  put unformatted 
         '<body>' 
    skip '  <script>'
    skip '    if (top.location == self.location) ~{'
    skip '      top.location.href="protop.html"; ~}'
    skip '  </script>'
    skip ' '
    skip '  <!-- table for separating data and links -->'
    skip '  <table cellspacing="0">' 
    skip '  <tr> <td width=80%> &nbsp; </td>'
    .
  
  RUN html_links.
  
  put unformatted 
    skip '  </tr> </table>'
    skip '</body>' 
    skip '</html>' 
    skip.
  output close.

  RUN html_rename(input cHtmlDir + 'temp.html',
                  input cFileName ).

end procedure. /* html_blank */


/* (Re) initialize html:
 * Generate the main pages in the html dir.
 */
procedure html_init:
  /* write static pages */
  RUN html_css.
  RUN html_framesets.
  RUN html_title.

  /* remember html dir for detection of changes */
  publish "get-html-dir" (output gcPrevHtmlDir).

end procedure. /* html_init */


/* Write the cascaded style sheet protop.css
 * 
 */
procedure html_css:
  define variable cHtmlDir as character  no-undo.
  publish "get-html-dir" (output cHtmlDir).

  /* If the stylesheet already exists, skip the process of creating one
  ** since the user may have altered it. */
  if search(cHtmlDir + 'protop.css') = ? then 
  do:
    /* Style sheet */
    output to value( cHtmlDir + 'protop.css').
    put unformatted
           '.toprow     ~{ background-color:#006666; color:#FFFF00; vertical-align: middle; ~}                                 '
      skip '.title      ~{ font-size:300%; font-weight:bold;   vertical-align: middle; text-align:left ~}                      '
      skip '.title2     ~{ font-size:120%; font-weight:normal; vertical-align: middle; text-align:left ~}                      '
      skip '.subtitle   ~{ font-size:120%; font-weight:normal; vertical-align: middle; text-align:right ~}                     '
      skip '.header     ~{ background-color:#FFFF00; color:#006666; font-size:120%; font-weight:bold ~}                        '
      skip '.label      ~{ background-color:#FFFFCC; color:#006666; font-weight:bold~}                                         '
      skip '.time       ~{ text-align:right; font-size:smaller ~}                                                              '
      skip 'body        ~{ background-color:#FFFFCC; color:#000000; font-size:8pt; font-family: Arial, Helvetica; ~}           '       
      skip 'table       ~{ background-color:#FFFFCC; color:#000000; margin: 0px; width:100%; border-width:1px; padding:0px;  ~}'
      skip 'tr          ~{ vertical-align:top ~}                                                                               '
      skip '.a2         ~{ color:#FFFF00; text-decoration: none~}'
      skip '.a2:hover   ~{ color:#FFFF00; text-decoration: underline~}'
      skip 'a           ~{ color:#006666; text-decoration: none~}'
      skip 'a:hover     ~{ color:#006666; text-decoration: underline~}'
      .
    output close. 
  end.
end procedure. /* html_css */


/* Write the different framesets for the various layouts.
 *
 */
procedure html_framesets.
  define variable cHtmlDir as character  no-undo.
  publish "get-html-dir" (output cHtmlDir).

  /* If a frameset already exists, skip the process of creating it */
  /* Split rows */
  if search(cHtmlDir + 'splitrows.html') = ? then 
  do:
    output to value( cHtmlDir + 'splitrows.html').
    put unformatted '<frameset rows="50%,50%"    frameborder="0">'
      skip '{&frame_blank}' 
      skip '{&frame_blank}'
      skip '</frameset>'
      .
    output close.
  end.

  /* Split cols */
  if search(cHtmlDir + 'splitcols.html') = ? then 
  do:
    output to value( cHtmlDir + 'splitcols.html').
    put unformatted '<frameset cols="50%,50%"    frameborder="0">'
      skip '{&frame_blank}' 
      skip '{&frame_blank}'
      skip '</frameset>'
      .
    output close.
  end.

  /* 1 x 1 layout */
  if search(cHtmlDir + 'protop.html') = ? then 
  do:
    output to value( cHtmlDir + 'protop.html').
    put unformatted '{&frameset_1row} {&frame_title}'
      skip '  {&frame_blank} </frameset>'
      skip '</frameset>'
      .
    output close.
  end.

  /* 1 x 2 layout */
  if search(cHtmlDir + 'protop1x2.html') = ? then 
  do:
    output to value( cHtmlDir + 'protop1x2.html').
    put unformatted '{&frameset_2rows} {&frame_title}'
      skip '  {&frame_blank} {&frame_blank} </frameset>'
      skip '</frameset>'
      .
    output close.
  end.

  /* 1 x 3 layout */
  if search(cHtmlDir + 'protop1x3.html') = ? then 
  do:
    output to value( cHtmlDir + 'protop1x3.html').
    put unformatted '{&frameset_3rows} {&frame_title}'
      skip '  {&frame_blank} {&frame_blank} {&frame_blank} </frameset>'
      skip '</frameset>'
      .
    output close.
  end.

  /* 2 x 1 layout */
  if search(cHtmlDir + 'protop2x1.html') = ? then 
  do:
    output to value( cHtmlDir + 'protop2x1.html').
    put unformatted '{&frameset_1row} {&frame_title}'
      skip '  {&frameset_2cols} {&frame_blank} {&frame_blank} </frameset>'
      skip '</frameset>'
      .
    output close.
  end.

  /* 2 x 2 layout */
  if search(cHtmlDir + 'protop2x2.html') = ? then 
  do:
    output to value( cHtmlDir + 'protop2x2.html').
    put unformatted '{&frameset_2rows} {&frame_title}'
      skip '  {&frameset_2cols} {&frame_blank} {&frame_blank} </frameset>'
      skip '  {&frameset_2cols} {&frame_blank} {&frame_blank} </frameset>'
      skip '</frameset>'
      .
    output close.
  end.

  /* 2 x 3 layout */
  if search(cHtmlDir + 'protop2x3.html') = ? then 
  do:
    output to value( cHtmlDir + 'protop2x3.html').
    put unformatted '{&frameset_3rows} {&frame_title}'
      skip '  {&frameset_2cols} {&frame_blank} {&frame_blank} </frameset>'
      skip '  {&frameset_2cols} {&frame_blank} {&frame_blank} </frameset>'
      skip '  {&frameset_2cols} {&frame_blank} {&frame_blank} </frameset>'
      skip '</frameset>'
      .
    output close.
  end.
end procedure. /* html_framesets */


/* Write the title page with the links to the various layouts.
 * 
 */
procedure html_title:
  define variable cHtmlDir as character  no-undo.
  publish "get-html-dir" (output cHtmlDir).

  /* title page, do not create if already exists. */
  if search(cHtmlDir + 'title.html') = ? then 
  do:
    output to value( cHtmlDir + 'title.html').

    RUN html_header.

    put unformatted 
           '<body vlink="#FFFF00" alink="#FFFF00" link="#FFFF00">'
      skip '  <table cellspacing="0" width="100%" height="100%">'
      skip '    <tr class="toprow">'
      skip '       <td><span class="title"> &nbsp <a class="a2" href="{&protop-url}" target="_new">ProTop</a> </span>'
      skip '           <span class="title2"> release {&protop-version} </span></td>'
      skip '       <td class="subtitle">'
      skip '              <A class="a2" href="protop.html"    target=_top>[1x1] </a>'
      skip '              <A class="a2" href="protop1x2.html" target=_top>[1x2] </a>'
      skip '              <A class="a2" href="protop1x3.html" target=_top>[1x3] </a> <br>'
      skip '              <A class="a2" href="protop2x1.html" target=_top>[2x1] </a>'
      skip '              <A class="a2" href="protop2x2.html" target=_top>[2x2] </a>'
      skip '              <A class="a2" href="protop2x3.html" target=_top>[2x3] </a> </td>'
      skip '       <td class="subtitle"> Progress Database Monitor &nbsp </td>'
      skip '    </tr>'
      skip '  </table>'
      skip '</body>'
      skip '</html>'
      .
    output close. 
  end.
end procedure. /* html_title */



/* Rename the html file to avoid strange results if progress writes 
 * to the file while the browser is reloading.
 * 
 */
procedure html_rename:
  define input parameter cOldName as character no-undo.
  define input parameter cNewName as character no-undo.

  os-delete value( cNewName ).
  os-rename value( cOldName ) value( cNewName ).
end procedure. /* html_rename */


/******* main execution block *******/

subscribe to "vdisplay"     anywhere run-procedure "html_display".
subscribe to "html-display" anywhere run-procedure "html_display".

RUN html_init.
return.
