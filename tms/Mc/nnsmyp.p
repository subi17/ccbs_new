/* --------------------------------------------------------------------
  MODULE .......: NNSMYP.P
  FUNCTION .....: Maintain salesmen
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 21-01-98
  MODIFIED .....: 27.12.98 pt f4: memberships in sm-groups
                  25.02.99 pt f7: move customers
                  18.05.99 jp uright1 & uright2 added   
                  26.04.02/tk eventlogging added
                  21.05.02/tk f1: find
                              f3: invoice texts
                  03.06.02/tk delete InvTexts            
                  30.12.02/aam reseller & rslevel added,
                               so-code & p-perc removed 
                  25.02.03/tk tokens            
                  17.09.03/aam brand
                  03.10.03/aam contract (F4)
                  06.09.04/aam CustNum
                  
  VERSION ......: M15
  ------------------------------------------------------------------ */
&GLOBAL-DEFINE BrTable Salesman

{Syst/commali.i}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhSalesman AS HANDLE NO-UNDO.
   lhSalesman = BUFFER Salesman:HANDLE.
   RUN StarEventInitialize(lhSalesman).

   DEFINE VARIABLE lhInvText AS HANDLE NO-UNDO.
   lhInvText = BUFFER InvText:HANDLE.
   RUN StarEventInitialize(lhInvText).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhSalesman).
   END.
END.


DEF INPUT PARAMETER icResell AS CHAR NO-UNDO.

def /* new */ shared var siirto as char.


def var xSalesman   like Salesman.Salesman     no-undo.
def var RsName    like Reseller.RsName       no-undo.
def var p-SmName  like Salesman.SmName     no-undo.
def var SmName    like Salesman.SmName     no-undo.
def var xReseller   like Salesman.Reseller     no-undo.

def var xrecid     as recid                           init ?.
def var firstline  as int                    no-undo  init 0.
def var order      as int                    no-undo  init 1.
def var ordercount as int                    no-undo  init 3.
def var ufkey      as log                    no-undo  init true.
def var delline    as int                    no-undo  init 0.
def var ex-order   as int                    no-undo.
def var memory     as recid                  no-undo.
def var line       as int format "99"        no-undo.
def var must-print as log                    no-undo.
def var must-add   as log                    no-undo.
def var fr-header  as char                   no-undo.
def var rtab       as recid extent 24        no-undo.
def var i          as int                    no-undo.
def var ok         as log format "Yes/No"    no-undo.
def var amt        as i                      no-undo.

def buffer my-Salesman for Salesman.

form
    Salesman.Brand       column-label "Brand" format "x(5)"
    Salesman.Salesman    column-label "Salesman" format "x(8)"
    Salesman.SmName      column-label "SmName"   format "x(19)"
    Salesman.Reseller                            format "x(8)"
    RsName               column-label "RsName"   format "x(16)"
    Salesman.RsLevel      
    Salesman.Parent      column-label "PS-code"  format "x(8)"
with
    width 80 overlay scroll 1 15 down row 1
    color value(cfc)
    title color value(ctc) " " + ynimi + " Maintain salesmen "
    + string(pvm,"99-99-99") + " " frame sel.

form
    "Salesman's code ...:" Salesman.Salesman                        skip
    "Salesman's name ...:" Salesman.SmName                          skip
    "Reseller's code ...:" Salesman.Reseller
       Reseller.RsName no-label skip
    "Reseller level ....:" Salesman.RsLevel skip
    "Customer ..........:" Salesman.Custnum 
       Customer.CustName skip
    "E-mail address ....:" Salesman.email                           skip
    "Parent's code .....:" Salesman.Parent  p-SmName  no-label      skip
    with  overlay row 4 centered
    color value(cfc)
    title color value(ctc)
    fr-header with  no-labels /* 1 columns */
    frame lis.

{Func/brand.i}

form
   skip(1)
" All active contracts of " skip
" Salesman" Salesman.Salesman Salesman.SmName no-label skip(1) 
" shall be moved onto " skip(1)
" Salesman" xSalesman 
help "Code of Salesman who 'inherits' contracts from this Salesman"
validate(xSalesman = "" or 
         can-find(my-Salesman where my-Salesman.Salesman = xSalesman),
         "UNKNOWN Salesman !")
my-Salesman.SmName no-label                               skip(1)

with
   overlay centered no-labels row 6 title 
   " MOVE CONTRACTS ONTO OTHER Salesman " frame move.

form /*  search with field Salesman */
    "Brand ..:" lcBrand skip
    "Salesman:" xSalesman
    help "Give Salesman's code"
    with row 4 col 2 title color value(ctc) " FIND Salesman'S CODE "
    color value(cfc) no-labels overlay frame f3.

form /*  search with field SmName */
    "Brand:" lcBrand skip
    "Name :" SmName
    help "Give Salesman's name"
    with row 4 col 2 title color value(ctc) " FIND Salesman'S NAME "
    color value(cfc) no-labels overlay frame f2.

form /*  search with field Reseller */
    "Brand ..:" lcBrand skip
    "Reseller:" xReseller
    help "Give reseller code"
    with row 4 col 2 title color value(ctc) " FIND RESELLER "
    color value(cfc) no-labels overlay frame f1.

FUNCTION fRsName RETURNS CHARACTER.

   find first Reseller where
              Reseller.Brand    = Salesman.Brand AND
              Reseller.Reseller = Salesman.Reseller
   no-lock no-error.
   if available Reseller 
   then RETURN Reseller.RsName.
   else RETURN "UNKNOWN".

END FUNCTION.


cfc = "sel". RUN Syst/ufcolor. assign ccc = cfc.

view frame sel.

IF icResell NE "" THEN
   ordercount = 2.

RUN local-find-first.

if available Salesman then assign
   memory      = recid(Salesman)
   must-print  = true
   must-add    = false.
else assign
   memory      = ?
   must-print  = false
   must-add    = true.

LOOP:
repeat with frame sel:

   if order <> ex-order then do:
      ex-order = order.
      if order = 1 then put screen row 19 col 30 " By reseller/level ".
      if order = 2 then put screen row 19 col 30 " By name           ".
      if order = 3 then put screen row 19 col 30 " By code           ".
   end.

   if must-add then do:  /* Salesman -ADD  */
      hide frame lis.
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = false.
      RUN Syst/ufcolor.

      add-new:
      repeat with frame lis on endkey undo add-new, leave add-new.
        pause 0 no-message.
        clear frame lis no-pause.
        ehto = 9. RUN Syst/ufkey.

        PROMPT-FOR Salesman.Salesman.
        IF INPUT FRAME lis Salesman.Salesman = "" THEN UNDO, LEAVE add-new.

        IF CAN-FIND(FIRST Salesman USING FRAME lis Salesman.Salesman WHERE
                          Salesman.Brand = lcBrand)
        THEN DO:
           MESSAGE "Salesman already exists with code" 
                   INPUT FRAME lis Salesman.Salesman
           VIEW-AS ALERT-BOX
           ERROR.
           NEXT.
        END. 

        CREATE Salesman.
        ASSIGN FRAME lis Salesman.Salesman
               Salesman.Brand = lcBrand.

        IF icResell NE "" THEN 
           Salesman.Reseller = icResell.

        RUN local-update-record. 

        IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
        UNDO add-new, LEAVE add-new.

        IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhSalesman).

        memory = recid(Salesman).
      end.  /* add-new */

      hide frame lis no-pause.
      assign must-print = true.

      /* any records available ? */
      find first Salesman
      /* search condition */ no-lock no-error.
      if not available Salesman then leave LOOP.
      next LOOP.
   end.

print-line:
   do :
      if must-print then do:
        up frame-line - 1.
        find Salesman where recid(Salesman) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose keyvalue = memory
        beginning from line 'delline' */

        /* if a line has just been deleted, then ... */
        if delline > 0 then down delline - 1.

        repeat with frame sel:
           if available Salesman then do:

              RUN local-disp-row.

              rtab[frame-line] = recid(Salesman).

              RUN local-find-next.
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
        ufk[1]= 816  ufk[2]= 0 
        ufk[3]= 2228    ufk[4]= 1036
        ufk[5]= 5    ufk[6]= 4  ufk[7]= 0 
        ufk[8]= 8    ufk[9]= 1
        ehto = 3 ufkey = false.

        {Syst/uright1.i '"4,5,6,7"'} 

        RUN Syst/ufkey.p.
      end.

      hide message no-pause.
      if order = 1 then do:
        choose row Salesman.Reseller ;(uchoose.i;) no-error with frame sel.
        color display value(ccc) Salesman.Reseller with frame sel.
      end.
      else if order = 2 then do:
        choose row Salesman.SmName ;(uchoose.i;) no-error with frame sel.
        color display value(ccc) Salesman.SmName with frame sel.
      end.
      else if order = 3 then do:
        choose row Salesman.Salesman ;(uchoose.i;) no-error with frame sel.
        color display value(ccc) Salesman.Salesman with frame sel.
      end.

      nap = keylabel(lastkey).

      if rtab[frame-line] = ? AND lookup(nap,"5,f5,8,f8") = 0
      then do:
        bell.
        message "You are on a empty row, move upwards !".
        pause 1 no-message.
        next.
      end.

      if lookup(nap,"cursor-right") > 0 then do:
        order = order - 1. if order = 0 then order = ordercount.
      end.
      if lookup(nap,"cursor-left") > 0 then do:
        order = order + 1. if order > ordercount then order = 1.
      end.

      if order <> ex-order then do:
        assign firstline = 0 memory = rtab[frame-line].
        find Salesman where recid(Salesman) = memory.
        do i = 1 to frame-line - 1:
          RUN local-find-prev.
          if available Salesman then
              assign firstline = i memory = recid(Salesman).
           else leave.
        end.
        must-print = true.
        next LOOP.
      end.

      /* previous line */
      if lookup(nap,"cursor-up") > 0 then do with frame sel:
        if frame-line = 1 then do:
           find Salesman where recid(Salesman) = rtab[1] no-lock.
           RUN local-find-prev.
           if not available Salesman then do:
              message "YOU ARE ON THE FIRST ROW !".
              bell. pause 1 no-message.
              next BROWSE.
           end.
           else do:
              /* a previous one was found */
              scroll down.

              RsName = fRsName().

              display Salesman.Salesman Salesman.SmName 
                      Salesman.Parent 
                      Salesman.Reseller RsName
                      Salesman.RsLevel.
              do i = frame-down to 2 by -1:
                 rtab[i] = rtab[i - 1].
              end.
              assign
              rtab[1] = recid(Salesman)
              memory = rtab[1].
           end.
        end.
        else up 1.
      end. /* previous line */

      /* next line */
      else if lookup(nap,"cursor-down") > 0 then do
      with frame sel:
        if frame-line = frame-down then do:
           find Salesman where recid(Salesman) = rtab[frame-down] no-lock .
           RUN local-find-next.
           if not available Salesman then do:
              message "YOU ARE ON THE LAST ROW !".
              bell. pause 1 no-message.
              next BROWSE.
           end.
           else do:
              /* yet another record was found */
              scroll up.

              RsName = fRsName().

              display Salesman.Salesman Salesman.SmName
                      Salesman.Parent 
                      Salesman.Reseller RsName
                      Salesman.RsLevel.
              do i = 1 to frame-down - 1:
                 rtab[i] = rtab[i + 1].
              end.
              rtab[frame-down] = recid(Salesman).
              /* finally last line's keyvalue is saved */
              memory = rtab[1].
           end.
        end.
        else down 1 .
      end. /* next line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 then do:
        memory = rtab[1].
        find Salesman where recid(Salesman) = memory no-lock no-error.
        RUN local-find-prev.
        if available Salesman then do:
           memory = recid(Salesman).

           /* go back one page */
           do line = 1 to (frame-down - 1):
              RUN local-find-prev.
              if available Salesman then memory = recid(Salesman).
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
     else if lookup(nap,"next-page,page-down,+") > 0 then do with frame sel:
       /* cursor to the downmost line */
       if rtab[frame-down] = ? then do:
           message "YOU ARE ON THE LAST PAGE !".
           bell. pause 1 no-message.
       end.
       else do: /* the downmost line wasn't empty */
           memory = rtab[frame-down].
           find Salesman where recid(Salesman) = memory no-lock.
           must-print = true.
           next LOOP.
       end.
     end. /* next page */

    else if lookup(nap,"1,f1") > 0 then do:

        assign
        ufk[1]= 761 ufk[2]= 30 ufk[3]= 35 ufk[4]= 0
        ufk[5]= 0    ufk[6]= 0  ufk[7]= 0  ufk[8]= 8 ufk[9]= 1.

        IF icResell NE "" THEN ASSIGN 
           ufk[1] = 0 ufk[3] = 0. 

        ehto = 3. ufkey = false.
        RUN Syst/ufkey.p.

        readkey. nap = keylabel(lastkey).

         /* Haku sarakk. 1 */
         if lookup(nap,"1,f1") > 0 then do on endkey undo, next LOOP:

           cfc = "puyr". RUN Syst/ufcolor.
           xReseller = "".
           ehto = 9. RUN Syst/ufkey. ufkey = true.
           DISPLAY lcBrand WITH FRAME F1.
           UPDATE lcBrand WHEN gcAllBrand
                  xReseller with frame f1.
           hide frame f1 no-pause.

           if xReseller <> "" then do:
              find first Salesman where 
                         Salesman.Brand = lcBrand AND
                         Salesman.Reseller >= xReseller
              use-index Reseller /* search condition */ no-lock no-error.

              if not fRecFound(1) THEN NEXT BROWSE.

              next LOOP.
           end.
         end. /* Haku sar. 1 */

         /* Haku sarakk. 2 */
         else if lookup(nap,"2,f2") > 0 then do on endkey undo, next LOOP:

           cfc = "puyr". RUN Syst/ufcolor.
           SmName = "".
           ehto = 9. RUN Syst/ufkey. ufkey = true.
           DISPLAY lcBrand WITH FRAME F2.
           UPDATE lcBrand WHEN gcAllBrand
                  SmName with frame f2.
           hide frame f2 no-pause.

           if SmName <> "" then do:
              IF icResell = "" THEN 
              find first Salesman where 
                 Salesman.Brand = lcBrand AND
                 Salesman.SmName >= SmName
              use-index SmName /* search condition */ no-lock no-error.

              ELSE FIND FIRST Salesman USE-INDEX SmName WHERE 
                              Salesman.Brand = lcBrand AND
                              Salesman.Reseller = icResell AND
                              Salesman.SmName >= SmName NO-LOCK NO-ERROR.

              if not fRecFound(2) THEN NEXT BROWSE.

              next LOOP.
           end.
         end. /* Haku sar. 2 */

         /* Haku 3 */
         else if lookup(nap,"3,f3") > 0 then do on endkey undo, next LOOP:
           cfc = "puyr". RUN Syst/ufcolor.
           xSalesman = "".
           ehto = 9. RUN Syst/ufkey. ufkey = true.
           DISPLAY lcBrand WITH FRAME F3.
           UPDATE lcBrand WHEN gcAllBrand
                  xSalesman with frame f3.
           hide frame f3 no-pause.

           if xSalesman <> "" then do:
              find first Salesman where 
                         Salesman.Brand = lcBrand AND
                         Salesman.Salesman >= xSalesman

              /* search condition */ no-lock no-error.
              if not fRecFound(3) THEN NEXT BROWSE.

              next LOOP.
           end.
         end. /* Haku sar. 3 */

         else if lookup(nap,"8,f8") > 0 THEN DO:
            ufkey=true.
            NEXT BROWSE.
         end.
     end.    /* FIND */

     ELSE IF lookup(nap,"3,f3") > 0 THEN DO:
        FIND Salesman WHERE recid(Salesman) = rtab[FRAME-line(sel)] NO-LOCK.
        RUN Mc/smname(Salesman.Salesman). 
        ASSIGN memory = recid(Salesman) must-print = true ufkey=true.
        NEXT LOOP.
     END.   

     ELSE IF lookup(nap,"4,f4") > 0 THEN DO:
        FIND Salesman WHERE recid(Salesman) = rtab[FRAME-line(sel)] NO-LOCK.
        RUN Mc/contract(0,
                     Salesman.Salesman). 
        ASSIGN memory = recid(Salesman) must-print = true ufkey=true.
        NEXT LOOP.
     END.   

     if lookup(nap,"5,f5") > 0 then do:  /* lisays */

        {Syst/uright2.i}

        must-add = true.
        next LOOP.
     end.

     else if lookup(nap,"6,f6") > 0 then do transaction:  /* removal */

       {Syst/uright2.i}

       delline = frame-line.
       find Salesman where recid(Salesman) = rtab[frame-line] no-lock.

       /* line to be deleted is lightened */
       color display value(ctc)
       Salesman.Salesman Salesman.SmName 
       Salesman.Parent 
       /* Salesman.p-perc */
       Salesman.Reseller RsName
       Salesman.RsLevel.

       RUN local-find-next.
       if available Salesman then memory = recid(Salesman).
       else do:
          /* the one to be deleted is rereaden */
          find Salesman where recid(Salesman) = rtab[frame-line] no-lock.
          /* and then the previous one */
          RUN local-find-prev.
          if available Salesman then do:
             assign
             delline = delline - 1  /* cause the last one is to be deleted */
             memory = recid(Salesman).
          end.
       end.

       /* 'find' back to the row to be deleted */
       find Salesman where recid(Salesman) = rtab[frame-line]
       exclusive-lock.

       assign ok = false.
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " update ok.
       color display value(ccc)
       Salesman.Salesman Salesman.SmName 
       Salesman.Parent 
       Salesman.Reseller RsName
       Salesman.RsLevel.

       if ok then do:

           FOR EACH InvText WHERE 
                    InvText.Brand    = Salesman.Brand AND 
                    InvText.target   = "Salesman" AND  
                    InvText.keyvalue = STRING(Salesman.Salesman).

               IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhInvText).
               delete InvText.
           END.

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhSalesman).

           delete Salesman.

           /* if the last record was deleted ? */
           if not can-find(first Salesman
           /* search condition */) then do:
              clear frame sel no-pause.
              pause 0 no-message.
              leave LOOP.
           end.
           must-print = true.
           next LOOP.
       end.
       else delline = 0. /* wasn't the last one */
     end. /* removal */

     else if lookup(nap,"7,f7") > 0 then do with frame move TRANS: 
        /* TRANSFER */

        {Syst/uright2.i}

        find Salesman where recid(Salesman) = rtab[frame-line(sel)] no-lock.
        pause 0.
        disp Salesman.Salesman Salesman.SmName.
        ehto = 9. ufkey = true. RUN Syst/ufkey.
        xSalesman = "". update xSalesman.

        IF xSalesman = Salesman.Salesman THEN DO:
           MESSAGE "Source and target of transfer are the same."
           VIEW-AS ALERT-BOX.
           NEXT.
        END.

        if xSalesman ne "" then do for my-Salesman:

           find my-Salesman where my-Salesman.Salesman = xSalesman no-lock.
           disp my-Salesman.SmName.

           ok = false.
           message
           "Do You REALLY Want to move active contracts FROM sm" 
           Salesman.Salesman
           "onto sm" my-Salesman.Salesman "?"
           VIEW-AS ALERT-BOX
           QUESTION
           BUTTONS YES-NO
           SET ok. 
           if not ok then next.

           message "Moving contracts, wait ....".
           amt = 0.

           FOR EACH contract EXCLUSIVE-LOCK WHERE 
                    contract.Salesman = Salesman.Salesman AND
                    contract.ToDate >= TODAY:

              contract.Salesman = my-Salesman.Salesman.
              amt = amt + 1.
              pause 0.
              disp 
                 amt label "# of moved contracts" 
              with  
                 centered frame amt row 6 1 down side-labels overlay 
                 title " MOVING ... ".
           end.

           hide frame amt no-pause.

           message 
           "Totally" amt "contracts updated - press ENTER !"
           VIEW-AS ALERT-BOX
           INFORMATION.

        end.      
        hide frame move.
     end.

     else if lookup(nap,"enter,return") > 0 then
     do with frame lis transaction:
       /* change */
       {Syst/uright2.i}
       find Salesman where recid(Salesman) = rtab[frame-line(sel)]
       exclusive-lock.

       assign fr-header = " CHANGE " ufkey = true ehto = 9.
       RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSalesman).

       RUN local-update-record. 

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSalesman).

       xrecid = recid(Salesman).

       hide frame lis no-pause.

       RsName = fRsName().
       display Salesman.SmName
               Salesman.Parent
               Salesman.Reseller RsName
               Salesman.RsLevel
               with frame sel.
     end.

     else if lookup(nap,"home,h") > 0 then do:
       RUN local-find-first.
       assign memory = recid(Salesman) must-print = true.
       next LOOP.
     end.

     else if lookup(nap,"end,e") > 0 then do : /* last record */
       RUN local-find-last.
       assign memory = recid(Salesman) must-print = true.
       next LOOP.
     end.


     else if lookup(nap,"8,f8") > 0 then leave LOOP.

  end.  /* BROWSE */
end.  /* LOOP */

hide frame sel no-pause.
si-recid = xrecid.

PROCEDURE local-find-prev:

   IF order = 1 then do:
      IF icResell = ""
      THEN find prev Salesman WHERE Salesman.Brand = lcBrand 
      use-index Reseller
         /* search condition */ no-lock no-error.
      ELSE FIND PREV Salesman USE-INDEX Reseller WHERE 
         Salesman.Brand = lcBrand AND
         Salesman.Reseller = icResell NO-LOCK NO-ERROR.
   END. 
   ELSE IF order = 2 then do:
      IF icResell = "" 
      THEN find prev Salesman WHERE Salesman.Brand = lcBrand use-index SmName
      /* search condition */ no-lock no-error.
      ELSE FIND PREV Salesman WHERE 
         Salesman.Brand = lcBrand AND
         Salesman.Reseller = icResell
         USE-INDEX SmName NO-LOCK NO-ERROR.
   END. 

   else if order = 3 then find prev Salesman WHERE
      Salesman.Brand = lcBrand use-index Salesman
      /* search condition */ no-lock no-error.

END PROCEDURE. 

PROCEDURE local-find-next:

   IF order = 1 then do:
      IF icResell = ""
      THEN FIND NEXT Salesman WHERE Salesman.Brand = lcBrand use-index Reseller
         /* search condition */ no-lock no-error.
      ELSE FIND NEXT Salesman USE-INDEX Reseller WHERE 
         Salesman.Brand = lcBrand AND
         Salesman.Reseller = icResell NO-LOCK NO-ERROR.
   END. 
   ELSE IF order = 2 then do:
      IF icResell = "" 
      THEN find NEXT Salesman use-index SmName WHERE
         Salesman.Brand = lcBrand
      /* search condition */ no-lock no-error.
      ELSE FIND NEXT Salesman WHERE 
         Salesman.Brand = lcBrand AND
         Salesman.Reseller = icResell
         USE-INDEX SmName NO-LOCK NO-ERROR.
   END. 

   else if order = 3 then find NEXT Salesman WHERE
       Salesman.Brand = lcBrand use-index Salesman
      /* search condition */ no-lock no-error.

END PROCEDURE. 

PROCEDURE local-find-first:

   IF order = 1 then do:
      IF icResell = ""
      THEN FIND FIRST Salesman use-index Reseller WHERE
         Salesman.Brand = lcBrand
         /* search condition */ no-lock no-error.
      ELSE FIND FIRST Salesman USE-INDEX Reseller WHERE 
         Salesman.Brand = lcBrand AND
         Salesman.Reseller = icResell NO-LOCK NO-ERROR.
   END. 
   ELSE IF order = 2 then do:
      IF icResell = "" 
      THEN find FIRST Salesman use-index SmName WHERE
         Salesman.Brand = lcBrand
      /* search condition */ no-lock no-error.
      ELSE FIND FIRST Salesman WHERE 
         Salesman.Brand = lcBrand AND
         Salesman.Reseller = icResell
         USE-INDEX SmName NO-LOCK NO-ERROR.
   END. 

   else if order = 3 then find FIRST Salesman use-index Salesman WHERE
      Salesman.Brand = lcBrand
      /* search condition */ no-lock no-error.

END PROCEDURE. 

PROCEDURE local-find-last:

   IF order = 1 then do:
      IF icResell = ""
      THEN FIND LAST Salesman use-index Reseller WHERE
         Salesman.Brand = lcBrand
         /* search condition */ no-lock no-error.
      ELSE FIND LAST Salesman USE-INDEX Reseller WHERE 
         Salesman.Brand = lcBrand AND
         Salesman.Reseller = icResell NO-LOCK NO-ERROR.
   END. 
   ELSE IF order = 2 then do:
      IF icResell = "" 
      THEN find LAST Salesman use-index SmName WHERE
         Salesman.Brand = lcBrand
      /* search condition */ no-lock no-error.
      ELSE FIND LAST Salesman WHERE 
         Salesman.Brand = lcBrand AND
         Salesman.Reseller = icResell
         USE-INDEX SmName NO-LOCK NO-ERROR.
   END. 

   else if order = 3 then find LAST Salesman use-index Salesman WHERE
      Salesman.Brand = lcBrand
      /* search condition */ no-lock no-error.

END PROCEDURE. 



PROCEDURE local-UPDATE-record:

   DEF BUFFER bParent FOR Salesman. 

   PAUSE 0. 

   REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:

      find bParent where 
           bParent.Brand    = Salesman.Brand AND
           bParent.Salesman = Salesman.Parent
         no-lock no-error.
      if avail bParent then assign p-SmName = bParent.SmName.
      else assign p-SmName = "".

      display Salesman.Salesman p-SmName
              Salesman.Reseller.

      find Reseller no-lock where
           Reseller.Reseller = Salesman.Reseller 
      no-error.
      if available Reseller then display Reseller.RsName.

      display "" @ Customer.CustName.
      if Salesman.CustNum > 0 then do:
         find Customer where
              Customer.CustNum = Salesman.CustNum no-lock no-error.
         if available Customer then display Customer.CustName.
      end.

      update Salesman.SmName 
             Salesman.Reseller WHEN icResell = "" 
             Salesman.RsLevel
             Salesman.CustNum
             Salesman.email
             Salesman.Parent 
      WITH FRAME lis EDITING :

         readkey. nap = keylabel(lastkey).

         if lookup(nap,poisnap) > 0 then do:

            if frame-field = "Reseller" then do:
               find Reseller no-lock where
                    Reseller.Brand    = Salesman.Brand AND
                    Reseller.Reseller = input Salesman.Reseller 
               no-error.
               if not available Reseller then do:
                  bell.
                  message "Reseller" +
                          input frame lis Salesman.Reseller +
                          " does not exist !".
                  pause 2.
                  next-prompt Salesman.Reseller.
                  next.
               end.
               display Reseller.RsName.
            end.

            else if frame-field = "Parent" then do:

               IF INPUT Salesman.Parent = "" 
               THEN DISPLAY "" ;& p-SmName.

               ELSE DO:

                  find first bParent where
                             bParent.Brand    = Salesman.Brand AND 
                             bParent.Salesman = input Salesman.Parent
                  no-lock no-error.

                  if not avail bParent then do:
                     bell.
                     message "Salesman's code """ +
                             input frame lis Salesman.Parent +
                             """ does not exist !".
                     pause 2.
                     next-prompt Salesman.Parent.
                     next.
                  end.

                  IF bParent.Reseller NE INPUT Salesman.Reseller
                  THEN DO:
                     MESSAGE "Parent Salesman belongs to another reseller."
                     VIEW-AS ALERT-BOX.
                     NEXT.
                  END. 

                  IF bParent.RsLevel >= INPUT Salesman.RsLevel
                  THEN DO:
                     MESSAGE "Parent Salesman must be on upper level in"
                             "reseller's organization."
                     VIEW-AS ALERT-BOX.
                     NEXT.
                  END. 

                  else disp bParent.SmName @ p-SmName.
               END.

            end. /* Parent */

            ELSE IF FRAME-FIELD = "CustNum" THEN DO:
               IF INPUT Salesman.CustNum > 0 THEN DO:
                   FIND Customer WHERE
                        Customer.CustNum = INPUT Salesman.CustNum 
                   NO-LOCK NO-ERROR.
                   IF AVAILABLE Customer THEN DISPLAY Customer.CustName.
                   ELSE DO:
                      MESSAGE "Unknown customer"
                      VIEW-AS ALERT-BOX.
                      NEXT.
                   END.
               END.
               ELSE DISPLAY "" @ Customer.CustName.
            END.

         end.

         apply lastkey.

      end.

      LEAVE.

   END.

END PROCEDURE. 

PROCEDURE local-disp-row:

   RsName = fRsName().

   display Salesman.Brand
           Salesman.Salesman Salesman.SmName 
           Salesman.Parent 
           Salesman.Reseller RsName
           Salesman.RsLevel
           WITH FRAME sel.

END PROCEDURE.
