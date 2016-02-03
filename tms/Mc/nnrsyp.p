/* --------------------------------------------------------------------------
  MODULE .......: NNRSYP.P
  TASK .........: Updating of reseller file
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 30-04-98
  CHANGED ......: 05.06.98 kl - update asiakas.sm-code with reseller.sm-code
                  09.02.99 pt - Reseller.address
                  25.02.99 pt - search by f3 sets now order 3, not 1
                  18.05.99 jp - uright1 & uright2 added      
                  19.12.02 jr - Evetnlog
                  31.12.02 aam  sm-code removed 
                  29.09.03 aam  brand
                  16.07.04 tk print salesmen passwords with f3
                  
  VERSION ......: M15
  -------------------------------------------------------------------------- */
&GLOBAL-DEFINE BrTable Reseller

{Syst/commali.i}
{Syst/eventval.i} 

def /* new */ shared var siirto as char.

def var Reseller  like Reseller.Reseller  no-undo.
def var sm-code  like Reseller.Reseller  no-undo.
def var RsName like Reseller.RsName   no-undo.
def var old-sm-code like sm-code      no-undo.


def var xrecid       as recid                           init ?.
def var firstline    as int                    no-undo  init 0.
def var order        as int                    no-undo  init 1.
def var ordercount   as int                    no-undo  init 2.
def var ufkey        as log                    no-undo  init true.
def var delline      as int                    no-undo  init 0.
def var ex-order     as int                    no-undo.
def var memory       as recid                  no-undo.
def var line         as int format "99"        no-undo.
def var must-print   as log                    no-undo.
def var must-add     as log                    no-undo.
def var fr-header    as char                   no-undo.
def var rtab         as recid extent 24        no-undo.
def var i            as int                    no-undo.
def var ok           as log format "Yes/No"    no-undo.
def var sm-name      as c                      no-undo.
def var old-comm     as de                     no-undo.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhresell AS HANDLE NO-UNDO.
   lhresell = BUFFER Reseller:HANDLE.
   RUN StarEventInitialize(lhresell).

   ON F12 ANYWHERE 
   DO:
      RUN eventview2.p(lhresell).
   END.
END.

form
    Reseller.brand
    Reseller.Reseller      /* column-label format */
    Reseller.RsName     /* column-label format */
with width 80 overlay scroll 1 15 down
    color value(cfc)
    title color value(ctc) " " + ynimi +
    " Resellers "
    + string(pvm,"99-99-99") + " "
    frame sel.

{Func/brand.i}

form
    Reseller.Reseller     /* label format */
    Reseller.RsName    /* label format */
    Reseller.address[1]     label "Address"  format "x(30)"
    Reseller.address[2]     label "Address"  format "x(30)"
    Reseller.address[3]     label "Address"  format "x(30)"
    Reseller.email          label "Email"
    Reseller.Fuc1
    Reseller.Fuc2
    with  overlay row 5 centered color value(cfc) title color value(ctc)
    fr-header with side-labels 1 columns
    frame lis.

form    
 skip(1)
 " You have changed the SALESMAN code and/or the commission %% " skip
"  of this reseller.  " skip(1)
 " These new values are now being updated onto each " skip
 " customer record where this RESELLER code exists.      " skip(1)
 " Do You REALLY accept this change (Y/N) ? " ok no-label skip(1)
with 
 centered overlay title " SALESMAN CODE WAS CHANGED " frame smc.

form /* serch */
    "Brand:" lcBrand skip
    "Code :" Reseller
    help "Give reseller's code"
    with row 4 col 2 title color value(ctc) " FIND RESELLER'S CODE "
    color value(cfc) no-labels overlay frame f1.

form /* reseller search with field RsName */
    "Brand:" lcBrand skip
    "Name :" RsName
    help "Give reseller's name or its first characters"
    with row 4 col 2 title color value(ctc) " FIND RESELLER'S NAME "
    color value(cfc) no-labels overlay frame f2.


cfc = "sel". run ufcolor. assign ccc = cfc.
view frame sel.

find first Reseller
WHERE Reseller.Brand = lcBrand no-lock no-error.
if available Reseller then assign
   memory       = recid(Reseller)
   must-print = true
   must-add    = false.
else assign
   memory       = ?
   must-print = false
   must-add    = true.

LOOP:
repeat with frame sel:

    if order <> ex-order then do:
       ex-order = order.
       if order = 1 then put screen row 19 col 30 " By reseller's code ".
       if order = 2 then put screen row 19 col 30 " By reseller's name ".
    end.

   if must-add then do:  /* Reseller -ADD  */
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = false.
      run ufcolor.
add-new:
      repeat with frame lis on endkey undo add-new, leave add-new.
        pause 0 no-message.
        clear frame lis no-pause.
        ehto = 9. run ufkey.
        do transaction:
           prompt-for Reseller.Reseller
           validate
              (Reseller.Reseller = "" or
              not can-find(Reseller using  Reseller.Reseller WHERE
                           Reseller.Brand = lcBrand),
              "reseller " + string(input Reseller.Reseller) +
              " already existis !").
           if input Reseller.Reseller = "" then leave add-new.

           create Reseller.
           assign
           Reseller.Brand    = lcBrand
           Reseller.Reseller = input frame lis Reseller.Reseller.
           update Reseller.RsName
                  Reseller.address[1 for 3]
                  Reseller.email
                  Reseller.Fuc1
                  Reseller.Fuc2
           editing:
              readkey.
              if lookup(keylabel(lastkey),poisnap) > 0 then do:
                 hide message no-pause.
              end.
              apply lastkey.
           end.
           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhresell).
           assign
           memory = recid(Reseller)
           xrecid = memory.
        end.
      end.  /* add-new */
      hide frame lis no-pause.
      assign must-print = true.

      /* any records available ? */
      find first Reseller
      WHERE Reseller.Brand = lcBrand no-lock no-error.
      if not available Reseller then leave LOOP.
      next LOOP.
   end.

print-line:
   do :
      if must-print then do:
        up frame-line - 1.
        find Reseller where recid(Reseller) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose keyvalue = memory
        beginning from line 'delline' */

        /* if a line has just been deleted, then ... */
        if delline > 0 then down delline - 1.

        repeat with frame sel:
           if available Reseller then do:

              display 
              Reseller.Brand
              Reseller.Reseller Reseller.RsName .

              rtab[frame-line] = recid(Reseller).
              if order = 1 then find next Reseller
              WHERE Reseller.Brand = lcBrand no-lock no-error.
              else if order = 2 then find next Reseller use-index RsName
              WHERE Reseller.Brand = lcBrand no-lock no-error.
           end.
           else do:
              clear no-pause.
              rtab[frame-line] = ?.
           end.
           if frame-line = frame-down then leave.
           down.
        end.
        up frame-line - 1.
        down firstline.
        assign firstline = 0
               must-print = false.
        pause 0 no-message.

        /* one page of data has been printed and
        the cursor is in the upmost line for 'choose' */
      end. /* must-print = true */
   end. /* print-line */

   /* if lastly a line has been deleted */
   if delline > 0 then down delline - 1.
   assign delline = 0.

BROWSE:
   repeat with frame sel on endkey undo, return:

      if ufkey then do:
        assign
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 2431 ufk[4]= 756
        ufk[5]= 5  ufk[6]= 4   ufk[7]= 0 ufk[8]= 8   ufk[9]= 1
        ehto = 3 ufkey = false.

        {Syst/uright1.i '"5,6"'}

        run ufkey.p.
      end.

      hide message no-pause.
      if order = 1 then do:
        choose row Reseller.Reseller ;(uchoose.i;) no-error with frame sel.
        color display value(ccc) Reseller.Reseller with frame sel.
      end.
      else if order = 2 then do:
        choose row Reseller.RsName ;(uchoose.i;) no-error with frame sel.
        color display value(ccc) Reseller.RsName with frame sel.
      end.
      if rtab[frame-line] = ? then next.

      nap = keylabel(lastkey).

      if lookup(nap,"cursor-right") > 0 then do:
        order = order + 1. if order > ordercount then order = 1.
      end.
      if lookup(nap,"cursor-left") > 0 then do:
        order = order - 1. if order = 0 then order = ordercount.
      end.

      if order <> ex-order then do:
        assign firstline = 0 memory = rtab[frame-line].
        find Reseller where recid(Reseller) = memory.
        do i = 1 to frame-line - 1:
           if order = 1 then find prev Reseller
           WHERE Reseller.Brand = lcBrand no-lock no-error.
           else if order = 2 then find prev Reseller use-index RsName
           WHERE Reseller.Brand = lcBrand no-lock no-error.
           if available Reseller then
              assign firstline = i memory = recid(Reseller).
           else leave.
        end.
        must-print = true.
        next LOOP.
      end.

      if rtab[frame-line] = ? and not must-add then do:
        bell.
        message "You are on an empty row, move upwards !".
        pause 1 no-message.
        next.
      end.

      assign nap = keylabel(lastkey).

      /* previous line */
      if lookup(nap,"cursor-up") > 0 then do with frame sel:
        if frame-line = 1 then do:
           find Reseller where recid(Reseller) = rtab[1] no-lock.
           if order = 1 then find prev Reseller
           WHERE Reseller.Brand = lcBrand no-lock no-error.
           else if order = 2 then find prev Reseller use-index RsName
           WHERE Reseller.Brand = lcBrand no-lock no-error.
           if not available Reseller then do:
              message "YOU ARE ON THE FIRST ROW !".
              bell. pause 1 no-message.
              next BROWSE.
           end.
           else do:
              /* a previous one was found */
              scroll down.
              display 
              Reseller.Brand
              Reseller.Reseller Reseller.RsName .
              do i = frame-down to 2 by -1:
                 rtab[i] = rtab[i - 1].
              end.
              assign
              rtab[1] = recid(Reseller)
              memory = rtab[1].
           end.
        end.
        else up 1.
      end. /* previous line */

      /* next line */
      else if lookup(nap,"cursor-down") > 0 then do
      with frame sel:
        if frame-line = frame-down then do:
           find Reseller where recid(Reseller) = rtab[frame-down] no-lock .
           if order = 1 then find next Reseller
           WHERE Reseller.Brand = lcBrand no-lock no-error.
           else if order = 2 then find next Reseller use-index RsName
           WHERE Reseller.Brand = lcBrand no-lock no-error.
           if not available Reseller then do:
              message "YOU ARE ON THE LAST ROW !".
              bell. pause 1 no-message.
              next BROWSE.
           end.
           else do:
              /* yet another record was found */
              scroll up.
              display 
              Reseller.Brand
              Reseller.Reseller Reseller.RsName 
              /* Reseller.cp */.
              do i = 1 to frame-down - 1:
                 rtab[i] = rtab[i + 1].
              end.
              rtab[frame-down] = recid(Reseller).
              /* finally last line's keyvalue is saved */
              memory = rtab[1].
           end.
        end.
        else down 1 .
      end. /* next line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up") > 0 then do:
        memory = rtab[1].
        find Reseller where recid(Reseller) = memory no-lock no-error.
        if order = 1 then find prev Reseller
        WHERE Reseller.Brand = lcBrand no-lock no-error.
        else if order = 2 then find prev Reseller use-index RsName
        WHERE Reseller.Brand = lcBrand no-lock no-error.
        if available Reseller then do:
           memory = recid(Reseller).

           /* go back one page */
           do line = 1 to (frame-down - 1):
              if order = 1 then find prev Reseller
              WHERE Reseller.Brand = lcBrand no-lock no-error.
              else if order = 2 then find prev Reseller use-index RsName
              WHERE Reseller.Brand = lcBrand no-lock no-error.
              if available Reseller then memory = recid(Reseller).
              else line = frame-down.
           end.
           must-print = true.
           next LOOP.
        end.
        else do:
           /* this is the first data page */
           message "YOU ARE ON THE FIRST PAGE !".
           bell. pause 1 no-message.
        end.
     end. /* previous page */

     /* next page */
     else if lookup(nap,"next-page,page-down") > 0 then do with frame sel:
       /* cursor to the downmost line */
       if rtab[frame-down] = ? then do:
           message "YOU ARE ON THE LAST PAGE !".
           bell. pause 1 no-message.
       end.
       else do: /* the downmost line wasn't empty */
           memory = rtab[frame-down].
           find Reseller where recid(Reseller) = memory no-lock.
           must-print = true.
           next LOOP.
       end.
     end. /* next page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 then do on endkey undo, next LOOP:
       cfc = "puyr". run ufcolor.
       Reseller = "".
       ehto = 9. run ufkey. ufkey = true.
       DISP lcBrand with frame f1.
       UPDATE 
          lcBrand WHEN gcAllBrand
           Reseller with frame f1.
       hide frame f1 no-pause.

       if Reseller <> "" then do:
          find first Reseller where  
             Reseller.Reseller >= Reseller AND
             Reseller.Brand = lcBrand no-lock no-error.

          if not fRecFound(1) THEN NEXT BROWSE.

          next LOOP.
       end.
     end. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 then do on endkey undo, next LOOP:

       cfc = "puyr". run ufcolor.
       RsName = "".
       ehto = 9. run ufkey. ufkey = true.

       DISP lcBrand with frame f2.
       UPDATE 
          lcBrand WHEN gcAllBrand
          RsName with frame f2.
       hide frame f2 no-pause.

       if RsName <> "" then do:
          find first Reseller where 
             Reseller.RsName >= RsName AND
             Reseller.Brand = lcBrand
             use-index RsName no-lock no-error.

          if not fRecFound(2) THEN NEXT BROWSE.

          next LOOP.
       end.
     end. /* Haku sar. 2 */

     else if lookup(nap,"3,f3") > 0 then do:  /* salesmen */

       delline = frame-line.
       find Reseller where recid(Reseller) = rtab[frame-line] no-lock.
       if available Reseller 
       then run smpwpr(Reseller.Reseller).
       ufkey = true. 
     end.
     
     else if lookup(nap,"4,f4") > 0 then do:  /* salesmen */

       delline = frame-line.
       find Reseller where recid(Reseller) = rtab[frame-line] no-lock.
       if available Reseller 
       then run nnsmyp(Reseller.Reseller).
       ufkey = true. 
     end.

     else if lookup(nap,"5,f5") > 0 then do:  /* lisays */

        {Syst/uright2.i}

        must-add = true.
        next LOOP.
     end.

     else if lookup(nap,"6,f6") > 0 then do transaction:  /* removal */

       {Syst/uright2.i}

       delline = frame-line.
       find Reseller where recid(Reseller) = rtab[frame-line] no-lock.

       /* line to be deleted is lightened */
       color display value(ctc)
       Reseller.Reseller Reseller.RsName.

       if order = 1 then find next Reseller
       WHERE Reseller.Brand = lcBrand no-lock no-error.
       else if order = 2 then find next Reseller use-index RsName
       WHERE Reseller.Brand = lcBrand no-lock no-error.
       if available Reseller then memory = recid(Reseller).
       else do:
          /* the one to be deleted is rereaden */
          find Reseller where recid(Reseller) = rtab[frame-line] no-lock.
          /* and then the previous one */
          if order = 1 then find prev Reseller
          WHERE Reseller.Brand = lcBrand no-lock no-error.
          else if order = 2 then find prev Reseller use-index RsName
          WHERE Reseller.Brand = lcBrand no-lock no-error.
          if available Reseller then do:
             assign
             delline = delline - 1  /* cause the last one is to be deleted */
             memory = recid(Reseller).
          end.
       end.

       /* 'find' back to the row to be deleted */
       find Reseller where recid(Reseller) = rtab[frame-line]
       exclusive-lock.

       assign ok = false.
       message "DO YOU REALLY WANT TO DELETE (y/n) ? " update ok.
       color display value(ccc)
       Reseller.Reseller Reseller.RsName.
       if ok then do:
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhresell).
           delete Reseller.

           /* in the last record was deleted ? */
           if not can-find(first Reseller
           WHERE Reseller.Brand = lcBrand) then do:
              clear frame sel no-pause.
              pause 0 no-message.
              leave LOOP.
           end.
           must-print = true.
           next LOOP.
       end.
       else delline = 0. /* wasn't the last one */
     end. /* removal */

     else if lookup(nap,"enter,return") > 0 then CHANGE:
     do with frame lis transaction:
       /* change */
       {Syst/uright2.i}

       find Reseller where recid(Reseller) = rtab[frame-line(sel)]
       exclusive-lock.

       assign fr-header = " CHANGE " ufkey = true ehto = 9.
       run ufkey.
       cfc = "lis". run ufcolor.
       display Reseller.Reseller .

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhresell).
       update Reseller.RsName
              Reseller.address[1 for 3]
              Reseller.email
              Reseller.Fuc1
              Reseller.Fuc2
       editing:
              readkey.
              if lookup(keylabel(lastkey),poisnap) > 0 then do:
                 hide message no-pause.
              end.
              apply lastkey.
       end.
       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhresell).

       hide frame lis no-pause.
       display Reseller.RsName
       with frame sel.
       xrecid = recid(Reseller).
     end.

     else if lookup(nap,"home") > 0 then do:
       if order = 1 then find first Reseller
       WHERE Reseller.Brand = lcBrand no-lock no-error.
       else if order = 2 then find first Reseller use-index RsName
       WHERE Reseller.Brand = lcBrand no-lock no-error.
       assign memory = recid(Reseller) must-print = true.
       next LOOP.
     end.

     else if lookup(nap,"end") > 0 then do : /* last record */
       if order = 1 then find last Reseller
       WHERE Reseller.Brand = lcBrand no-lock no-error.
       else if order = 2 then find last Reseller use-index RsName
       WHERE Reseller.Brand = lcBrand no-lock no-error.
       assign memory = recid(Reseller) must-print = true.
       next LOOP.
     end.

     else if lookup(nap,"8,f8") > 0 then leave LOOP.

  end.  /* BROWSE */
end.  /* LOOP */

hide frame sel no-pause.
si-recid = xrecid.
