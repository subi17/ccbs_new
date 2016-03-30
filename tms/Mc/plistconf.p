/* -----------------------------------------------
  MODULE .......: PLISTCONF.P
  FUNCTION .....: Maintain price list configuration for pricing plan
  APPLICATION ..: TMS
  AUTHOR .......: KL
  CREATED ......: 24.05.2002
  MODIFIED .....: 13.09.02/aam link to rateplan instead of customer,
                               check currency
                  03.09.02/jr  Fixed plist and dates validates
                  04.09.02/jr  Fixed period & priority validate              
                  17.12.02/aam StartCharge added 
                  26.02.03/aam Prefix and DedicList added 
                  03.03.03/aam priority shift corrected
                  20.03.03/aam one parameter added for tariff.p
                  04.04.03 kl RUN Mc/tariff, new parameter
                  26.06.03 kl new paramter for tariff
                  15.09.03/aam brand

  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

{Syst/eventval.i}

DEF INPUT PARAMETER  icRatePlan  AS CHAR NO-UNDO.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhPListConf AS HANDLE NO-UNDO.
   lhPListConf = BUFFER PListConf:HANDLE.   
   RUN StarEventInitialize(lhPListConf).

   DEFINE VARIABLE lhTariff AS HANDLE NO-UNDO.
   lhTariff = BUFFER tariff:HANDLE.
   RUN StarEventInitialize(lhTariff).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhPListConf).
   END.
END.


def buffer fPListConf for PListConf.

def /* new */ shared var siirto as char.

def var PList     like PListConf.PriceList no-undo.
def var PLName    like PriceList.PLName    no-undo.
def var dFrom     like Tariff.ValidFrom    no-undo.
def var prod-code like BillItem.BillCOde   no-undo.

def var xrecid       as recid                           init ?.
def var firstline    as int                    no-undo  init 0.
def var order        as int                    no-undo  init 1.
def var ordercount   as int                    no-undo  init 1.
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
def var vdate        as da  format "99-99-99"  no-undo.
def var llActive     as log                    no-undo format "*/".

DEF VAR llInclVat    AS LOG                    NO-UNDO. 
DEF VAR lcCurrency   AS CHAR                   NO-UNDO. 

form
   PListConf.Brand       format "x(4)" column-label "Bran"
   PListConf.RatePlan    
   PListConf.PriceList   FORMAT "X(18)"
   PriceList.PLName      format "x(15)"
   PListConf.prior       column-label "Pri"
   PListConf.StartCharge column-label "SC" 
   PListConf.dFrom   
   PListConf.dTo
   llActive          column-label "Act."
with width 80 overlay scroll 1 15 down ROW 1
   color value(cfc) title color value(ctc) " " + ynimi +
   " Pricelist history " + string(pvm,"99-99-99") + " "
   frame sel.

form
   "   Rating plan ....:" PListConf.RatePlan   FORMAT "X(12)"  skip
   "   Price list code :" PListConf.PriceList  FORMAT "X(18)" skip
   "   Priority key ...:" PListConf.prior       skip
   "   Starting charge :" PListConf.StartCharge format "Allow/Prohibit" skip
   "   Valid from date :" PListConf.dFrom       skip
   "   Valid to date ..:" PListConf.dTo
with  overlay row 6 centered width 35
   color value(cfc)
   title color value(ctc)
   fr-header with no-labels frame lis.

form /* Price List search with field PList */
   PList help "Give pricelist's code"
with row 4 col 2 title color value(ctc) " FIND CODE "
   color value(cfc) no-labels overlay frame f1.

form /* Price List search with field PLName */
   PLName help "Give pricelist's name"
with row 4 col 2 title color value(ctc) " FIND NAME "
   color value(cfc) no-labels overlay frame f2.

cfc = "sel". RUN Syst/ufcolor. assign ccc = cfc.
view frame sel.

FIND FIRST RatePlan WHERE
           RatePlan.Brand    = gcBrand AND 
           RatePlan.RatePlan = icRatePlan NO-LOCK NO-ERROR.
IF NOT AVAILABLE RatePlan THEN RETURN.

find first PListConf OF RatePlan use-index browse
no-lock no-error.
if available PListConf then assign
   memory     = recid(PListConf)
   must-print = true
   must-add   = false.
else assign
   memory     = ?
   must-print = false
   must-add   = true.

LOOP:
repeat with frame sel:

    if order <> ex-order then do:
       ex-order = order.
    end.

   if must-add then do:  /* PListConf -ADD  */
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = false.
      RUN Syst/ufcolor.
      add-new:
      repeat with frame lis on endkey undo add-new, leave add-new.
         pause 0 no-message.
         clear frame lis no-pause.
         ehto = 9. RUN Syst/ufkey.
         do transaction:
            create PListConf.
            assign PListConf.Brand    = RatePlan.Brand 
                   PListConf.RatePlan = icRatePlan.
            run pLocalUpdateOthers.
            if RETURN-VALUE NE "0" then undo,leave add-new.
         end.

         IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhPListConf).

      end.  /* add-new */

      hide frame lis no-pause.
      assign must-print = true.

      /* any records available ? */
      find first PListConf OF RatePlan use-index browse
      no-lock no-error.
      if not available PListConf then leave LOOP.
      next LOOP.

   end.

print-line:
   do :
      if must-print then do with frame sel:
         up frame-line - 1.
         find PListConf where recid(PListConf) = memory no-lock no-error.

         /* print 1 page data on the screen
         beginning from the record whose keyvalue = memory
         beginning from line 'delline' */

         /* if a line has just been deleted, then ... */
         if delline > 0 then down delline - 1.

         repeat with frame sel:
            if available PListConf then do:

               run pLocalDisplayOthers.

               rtab[frame-line] = recid(PListConf).
               if order = 1 then find next PListConf of RatePlan 
               use-index browse                         
               no-lock no-error.
            end.
            else do:
               clear frame sel no-pause.
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
         ufk[1] = 0 ufk[2] = 0 ufk[3] = 0 ufk[4] = 878
         ufk[5] = 5 ufk[6] = 4 ufk[7] = 0 ufk[8] = 8 ufk[9]= 1
         ehto = 3 ufkey = false.

         {Syst/uright1.i '"5,6"'}

         RUN Syst/ufkey.p.
      end.

      hide message no-pause.
      if order = 1 then do:
         choose row PListConf.RatePlan {Syst/uchoose.i} no-error with frame sel.
         color display value(ccc) PListConf.RatePlan with frame sel.
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
         find PListConf where recid(PListConf) = memory.
         do i = 1 to frame-line - 1:
            if order = 1 then find prev PListConf of RatePlan 
               use-index browse no-lock no-error.
            if available PListConf then
               assign firstline = i memory = recid(PListConf).
            else leave.
         end.
         must-print = true.
         next LOOP.
      end.

      if rtab[frame-line] = ? and not must-add then do:
         bell.
         message "You are on a empty row, move upwards !".
         pause 1 no-message.
         next.
      end.

      assign nap = keylabel(lastkey).

      /* previous line */
      if lookup(nap,"cursor-up") > 0 then do with frame sel:
         if frame-line = 1 then do:
            find PListConf where recid(PListConf) = rtab[1] no-lock.
            if order = 1 then find prev PListConf of RatePlan
            use-index browse no-lock no-error.

            if not available PListConf then do:
               message "YOU ARE ON THE FIRST ROW !".
               bell. pause 1 no-message.
               next BROWSE.
            end.
            else do:
               /* a previous one was found */
               scroll down.

               run pLocalDisplayOthers.

               do i = frame-down to 2 by -1:
                  rtab[i] = rtab[i - 1].
               end.

               assign
                  rtab[1] = recid(PListConf)
                  memory  = rtab[1].
            end.
         end.
         else up 1.
      end. /* previous line */

      /* next line */
      else if lookup(nap,"cursor-down") > 0 then do with frame sel:
         if frame-line = frame-down then do:
            find PListConf where recid(PListConf) = rtab[frame-down] no-lock .

            if order = 1 then find next PListConf of RatePlan
            use-index browse no-lock no-error.
            if not available PListConf then do:
               message "YOU ARE ON THE LAST ROW !".
               bell. pause 1 no-message.
               next BROWSE.
            end.
            else do:
               /* yet another record was found */
               scroll up.

               run pLocalDisplayOthers.

               do i = 1 to frame-down - 1:
                  rtab[i] = rtab[i + 1].
               end.
               assign
                  rtab[frame-down] = recid(PListConf).
                  /* finally last line's keyvalue is saved */
                  memory           = rtab[1].
            end.
         end.
         else down 1 .
      end. /* next line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 then do:
         memory = rtab[1].
         find PListConf where recid(PListConf) = memory no-lock no-error.
         if order = 1 then find prev PListConf of RatePlan
         use-index browse no-lock no-error.
         if available PListConf then do:
            memory = recid(PListConf).

            /* go back one page */
            do line = 1 to (frame-down - 1):
               if order = 1 then find prev PListConf of RatePlan
               use-index browse no-lock no-error.
               if available PListConf then memory = recid(PListConf).
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
            find PListConf where recid(PListConf) = memory no-lock.
            must-print = true.
            next LOOP.
        end.
     end. /* next page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 then do on endkey undo, next LOOP:
        cfc = "puyr". RUN Syst/ufcolor.
        PList = "".
        ehto = 9. RUN Syst/ufkey. ufkey = true.
        update PList with frame f1.
        hide frame f1 no-pause.
        if PList <> "" then do:
           find first PListConf of RatePlan use-index browse where 
                      PListConf.PriceList >= PList 
           no-lock no-error.
           if not available PListConf then do:
              bell.
              message "NONE FOUND !".
              pause 1 no-message.
              next BROWSE.
           end.
           /*  PListConf/PList was found */
           assign order = 1 memory = recid(PListConf) must-print = true.
           next LOOP.
        end.
     end. /* Haku sar. 1 */

     else if lookup(nap,"4,f4") > 0 THEN DO:  /* tariffs */
        FIND PListConf where recid(PListConf) = rtab;<frame-line(sel);> 
            no-lock no-error.

        IF AVAILABLE PListConf THEN DO:
           RUN Mc/tariff(0,0,PListConf.PriceList,0,"",0). 
           UFKEY = TRUE.
           ex-order = 0. 
           NEXT LOOP. 
        END.
     end. 

     else if lookup(nap,"5,f5") > 0 and ufk[5] > 0 then do:  /* lisays */

         {Syst/uright2.i}

         must-add = true.
         next LOOP.
     end.

     else if lookup(nap,"6,f6") > 0 and ufk[6] > 0
     then do transaction:  /* removal */

        {Syst/uright2.i}

        delline = frame-line (sel).
        find PListConf where recid(PListConf) = rtab[frame-line] no-lock.

        /* line to be deleted is lightened */
        color display value(ctc)
           PListConf.RatePlan
           PListConf.PriceList 
           PriceList.PLName 
           PListConf.prior 
           PListConf.StartCharge
           PListConf.dFrom
           PListConf.dTo.

        if order = 1 then find next PListConf of RatePlan
        use-index browse no-lock no-error.

        if available PListConf then memory = recid(PListConf).
        else do:
           /* the one to be deleted is rereaden */
           find PListConf where recid(PListConf) = rtab[frame-line] no-lock.
           /* and then the previous one */
           if order = 1 then find prev PListConf of RatePlan
           use-index browse  no-lock no-error.

           if available PListConf then do:
              assign
              delline = delline - 1  /* cause the last one is to be deleted */
              memory = recid(PListConf).
           end.
        end.

        /* 'find' back to the row to be deleted */
        find PListConf where recid(PListConf) = rtab[frame-line]
        exclusive-lock.

        assign ok = false.

        message 
           "NOTICE:" skip
           "If any calls are rated using this price list it"
           "SHOULD NOT BE DELETED !" skip
           "Are you SURE You want to delete ?"
        view-as alert-box question buttons YES-NO update ok.

        color display value(ccc)
           PListConf.RatePlan
           PListConf.PriceList 
           PriceList.PLName 
           PListConf.prior 
           PListConf.StartCharge
           PListConf.dFrom
           PListConf.dTo.

        if ok then do:
            i = 0.
            IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhPListConf).
            delete PListConf.
            /* in the last record was deleted ? */
            if not can-find(first PListConf of RatePlan)
            then do:
               clear frame sel no-pause.
               pause 0 no-message.
               leave LOOP.
            end.
            must-print = true.
            next LOOP.
        end.

        else delline = 0. /* wasn't the last one */

     end. /* removal */

     else if lookup(nap,"enter,return") > 0 AND qupd
     then do with frame lis transaction:
        /* change */
        {Syst/uright2.i}

        find first PListConf where 
             recid(PListConf) = rtab[frame-line(sel)]
        exclusive-lock no-error.
        assign fr-header = " CHANGE " ufkey = true ehto = 9.
        RUN Syst/ufkey.
        cfc = "lis". RUN Syst/ufcolor.
        display PListConf.RatePlan .

        IF llDoEvent THEN RUN StarEventSetOldBuffer(lhPListConf).

        run pLocalUpdateOthers.

        xrecid = recid(PListConf).

        IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhPListConf).

        RUN pLocalDisplayOthers. 

     end.

     else if lookup(nap,"home,h") > 0 then do:
        if order = 1 then find first PListConf of RatePlan
        use-index browse no-lock no-error.
        assign memory = recid(PListConf) must-print = true.
        next LOOP.
     end.

     else if lookup(nap,"end,e") > 0 then do : /* last record */
        if order = 1 then find last PListConf of RatePlan
        use-index browse no-lock no-error.
        assign memory = recid(PListConf) must-print = true.
        next LOOP.
     end.

     else if lookup(nap,"8,f8") > 0 then leave LOOP.

  end.  /* BROWSE */

end.  /* LOOP */

hide frame sel no-pause.
si-recid = xrecid.

procedure pLocalUpdateOthers:

   def buffer bufHist  for PListConf.
   def buffer bufHist2 for PListConf.
   def buffer bufList  for PriceList.

   disp PListConf.RatePlan  with frame lis.

   update  
      PListConf.PriceList 
      PListConf.prior 
      PListConf.StartCharge
      PListConf.dFrom 
      PListConf.dTo
   with frame lis editing:

      readkey.
      nap = keylabel(lastkey).

      if lookup(nap,poisnap) > 0 then do:
         if frame-field = "PriceList" then do:
            if input frame lis PListConf.PriceList = "" then return "9".
            assign PListConf.PriceList.
            find first PriceList where
                       PriceList.Brand     = PListConf.Brand AND
                       PriceList.PriceList = PListConf.PriceList
            no-lock no-error.
            if not avail PriceList then do:
               message 
                  "Price list" input PlistConf.PriceList "does not exist !"
               view-as alert-box.
               next.
            end.

            ASSIGN llInclVat  = PriceList.InclVat
                   lcCurrency = PriceList.Currency.

            /* same vat type and currency */
            FOR FIRST bufHist NO-LOCK WHERE
               bufHist.Brand    = RatePlan.Brand AND
               bufHist.RatePlan = RatePlan.RatePlan AND
               RECID(bufhist) NE RECID(PListConf),
            FIRST bufList OF bufHist NO-LOCK:
               ASSIGN llInclVat  = bufList.InclVat
                      lcCurrency = bufList.Currency. 
            END.

            IF PriceList.InclVat NE llInclVat THEN DO:
               message
                  "Price list \"" PriceList.PriceList 
                  "\" has different VAT handling"
                  "than other price lists on rating plan"
                  icRatePlan ", e.g. \"" bufList.PriceList "\""
               view-as alert-box.        
               next.
            end.

            IF PriceList.Currency NE lcCurrency THEN DO:
               message
                  "Price list \"" PriceList.PriceList 
                  "\" has different currency"
                  "than other price lists on rating plan"
                  icRatePlan ", e.g. \"" bufList.PriceList "\""
               view-as alert-box.        
               next.
            end.

         end.

         else if frame-field = "prior" then do:
            assign PListConf.prior.
            if prior = 0 then do:
               message 
                  "Value has to be bigger than 0 !"
               view-as alert-box error.
               next.
            end.
         end.

         else if frame-field = "dFrom" then do:
            assign PListConf.dFrom no-error.
            if error-status:error then 
            DO:
               message "Invalid date !"
               VIEW-AS ALERT-BOX.
               NEXT.
            END.
            if PListConf.dFrom = ? then do:
               message
                  "Empty value not allowed"
               view-as alert-box error.
               next.
            end.
         end.
         else if frame-field = "dTo" then do:

            assign PListConf.dTo no-error.
            if error-status:error then
            DO:
               message "Invalid date !"
               VIEW-AS ALERT-BOX.
               NEXT.
            END.

            if PListConf.dTo = ? then do:
               message
                  "Empty value not allowed"
               view-as alert-box error.
               next.
            end.

            if (PListConf.PriceList entered  OR
                PListConf.prior   entered  OR
                PListConf.dFRom   entered  OR
                PListConf.dTo     entered) THEN DO:

               find first bufHist use-index browse where
                          bufHist.Brand      = PListConf.Brand    AND
                          bufHist.RatePlan   = PListConf.RatePlan AND
                          ((bufHist.dFrom   >= PListConf.dFrom    AND
                           bufHist.dFrom    <= PlistConf.dTo)     OR
                          (bufHist.dTo      <= PListConf.dTo      AND
                           bufHist.dTo      >= PlistConf.dFrom)   OR
                           (bufhist.dfrom < plistconf.dfrom       AND
                            bufhist.dto   > plistconf.dto))       AND
                          bufHist.prior      = PListConf.prior    AND
                          recid(bufHist)    NE recid(PListConf)
               no-lock no-error.

               if avail bufHist then do:

                  ok = TRUE. 
                  message 
                     "At least one pricelist for the same period with equal"
                     "priority was found." skip
                     "Do You want to change priority lower"
                     "for the other lists at the same period ?"
                     view-as alert-box question buttons YES-NO update ok.

                  if ok then do:

                     must-print = true.

                     for each bufHist no-lock where
                              bufHist.Brand     = PListConf.Brand      AND
                              bufHist.RatePlan  = PListConf.RatePlan   AND
                              bufHist.dFrom    <= PListConf.dTo        AND
                              bufHist.dTo      >= PlistConf.dFrom      AND
                              bufHist.prior    >= PListConf.prior      AND
                              recid(bufHist)    NE recid(PListConf)
                     BY BufHist.Prior DESC:

                        find first bufHist2 where
                             recid(bufHist2) = recid(bufHist)
                        exclusive-lock.
                        assign bufHist2.prior = bufHist2.prior + 1.

                     end.

                  end.

               end.

            end.

         end.
      end.

      apply lastkey.

   end.

   assign      
      memory     = recid(PListConf) 
      xrecid     = memory.

   hide frame lis no-pause. 

   return "0".

end.

procedure pLocalDisplayOthers:

   find first PriceList OF PListConf
   no-lock no-error.

   llActive = (PListConf.dFrom <= today AND PListConf.dTo >= today).

   display   
      PListConf.Brand
      PListConf.RatePlan  
      PListConf.PriceList
      PriceList.PLName    WHEN AVAILABLE PriceList
      PListConf.prior  
      PListConf.StartCharge
      PListConf.dFrom  
      PListConf.dTo 
      llActive
   with frame sel.

end.
