/***********************************************************
  Module .......: H-MSISDN.P
  Parent .......: APPLHELP.P
  FUNCTION .....: Help browser of MSISDN 
  APPLICATION ..: NN
  AUTHOR .......: jp
  CREATED ......: 28-06-99
  MODIFIED .....: 24.08.99 pt msisdn.i
  Version ......: M15
                 03.03.03  jp billLevel removed
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/msisdn.i}

def shared var siirto as char.

def var CLI     like MSISDN.CLI   no-undo.
def var StatusCode   like MSStat.StatusCode no-undo.
def var rtab        as recid extent 11      no-undo.
def var ufkey       as log init true        no-undo.
def var i           as int                  no-undo.
def var memory      as recid                no-undo.
def var must-print  as logic                no-undo.
def var must-add    as logic                no-undo.

form
      MSISDN.CLI
      MSStat.StatusName  format "x(18)" 
      MSISDN.ActionDate
      MSISDN.ValidFrom
      MSISDN.CustNum
      MSClass.MCName format "x(8)" Column-label "MSISDN Class" 
      with scroll 1 11 down  row 4 centered color value(cfc)
    title color value(ctc) " MSISDNs " overlay frame sel.

form /* SEEK Code */
    m_pref space(0) 
    CLI format "x(12)"
    help "Enter MSISDN No. "
    with row 4 col 2 title color value(ctc) " FIND MSISDN "
    color value(cfc) no-labels overlay frame hayr.


form
    MSISDN.CustNum    label "Customer ..." Customer.CustName no-label at 25 skip
  WITH
  overlay row 4 centered
    COLOR VALUE(cfc)
    title COLOR VALUE(ctc) " Customer Data of MSISDN " + MSISDN.CLI + " "
    side-labels 
    FRAME cust.

cfc = "sel". RUN Syst/ufcolor. assign ccc = cfc.

MAIN:
repeat:

   find first MSISDN WHERE 
              MSISDN.Brand = gcBrand   AND
              MSISDN.ValidTo > fMakeTS()  AND 
              MSISDN.StatusCode = 1 
   NO-LOCK NO-ERROR.
   FIND MSClass WHERE 
        MSClass.Brand  = gcBrand AND 
        MSClass.MCCode = MSISDN.MCCode NO-LOCK NO-ERROR.

   if not available MSISDN then do:
      must-print = false.
      must-add = true.
   end.
   else do:
      memory = recid(MSISDN).
      must-print = true.
   end.

pause 0.
view frame  sel.

LOOP:
   Repeat with frame sel:

print-line:
   do :
      if must-print then do:
         clear frame sel all no-pause.
         find MSISDN where recid(MSISDN) = memory no-lock no-error.
         /* Print to screen */
         rtab = ?.
         do while frame-line<= frame-down and available MSISDN:
            RUN local-disp-row.
            rtab[frame-line] = recid(MSISDN).
            down with frame sel.
            find next MSISDN WHERE
                      MSISDN.BRand      = gcBrand  AND 
                       MSISDN.ValidTo > fMakeTS()  AND
                      MSISDN.StatusCode = 1 no-lock no-error.
         end.
         must-print = false.
         up frame-line(sel) - 1 with frame sel.
      end. /* must-print */

      if ufkey then do:
         assign
         ufk = 0 ufk[1] = 36 ufk[3] = 238 ufk[4] = 788 ufk[5] = 11
         ufk[6] = 0 ufk[8] = 8  ufk[9] = 1
         siirto = ? ehto = 3 ufkey = false.
         RUN Syst/ufkey.p.
      end.
  end. /* print-line */

BROWSE:
      repeat with frame sel on endkey undo, retuRN:

         hide message no-pause.
         choose row MSISDN.CLI {Syst/uchoose.i} no-error with frame sel.
         color display value(ccc) MSISDN.CLI with frame sel.

         if frame-value = "" and rtab[frame-line] = ? then next.
         nap = keylabel(lastkey).

         /* previous line */
         if lookup(nap,"cursor-up") > 0 then do
         with frame sel:
            if frame-line = 1 then do:
               find MSISDN where recid(MSISDN) = rtab[frame-line] no-lock.
               find prev MSISDN WHERE
                         MSISDN.BRand      = gcBrand  AND 
                          MSISDN.ValidTo > fMakeTS()  AND 
                         MSISDN.StatusCode = 1 
               no-lock no-error.
               if not available MSISDN then do:
                  bell.
                  message "You are on 1st row !".              
                  pause 1 no-message.
                  next BROWSE.
               end.
               else do:
                  /* a previous one was found */
                  scroll down.
                  do i = 11 to 2 by -1:
                     rtab[i] = rtab[i - 1].
                  end.
                  RUN local-disp-row.
                  rtab[frame-line] = recid(MSISDN).
                  memory = recid(MSISDN).
               end.
            end.
            else up 1.
         end. /* previous line */

         /* next line */
         if lookup(nap,"cursor-down") > 0 then do with frame sel:
            if frame-line = frame-down then do:
               find MSISDN where recid(MSISDN) = rtab[frame-line] no-lock .
               find next MSISDN WHERE
                         MSISDN.StatusCode = 1  AND 
                          MSISDN.ValidTo > fMakeTS()  AND 
                         MSISDN.Brand      = gcBrand 
               no-lock no-error.
               if not available MSISDN then do:
                  bell.
                  message "You are on last row !".
                  pause 1 no-message.
                  next BROWSE.
               end.
               else do:
                  /* yet another record was found */
                  scroll up.
                  do i = 1 to 10:
                     rtab[i] = rtab[i + 1].
                  end.
                  RUN local-disp-row.
                  rtab[frame-line] = recid(MSISDN).
                  /* finally last line's KeyValue is saved */
                  memory = rtab[1].
               end.
            end.
            else down 1 .
         end. /* next line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 then do with frame sel:
            find MSISDN where recid(MSISDN) = memory no-lock no-error.
            find prev MSISDN WHERE
                      MSISDN.Brand      = gcBrand AND 
                       MSISDN.ValidTo > fMakeTS() AND
                      MSISDN.StatusCode = 1 
            no-lock no-error.
            if available MSISDN then do:

               do i = 1 to (frame-down - 1):
                  find prev MSISDN WHERE
                            MSISDN.Brand      = gcBrand AND
                             MSISDN.ValidTo > fMakeTS() AND 
                            MSISDN.StatusCode = 1 no-lock no-error.
                  if available MSISDN then memory = recid(MSISDN).
                  else i = frame-down.
               end.
               must-print = true.
               next LOOP.
            end.
            else do:
               /* this is the first data page */
               bell.
               message "This is the 1st page !".          
               pause 1 no-message.
            end.
        end. /* previous page */

        /* next page */
        else if lookup(nap,"page-down,next-page") > 0 then do with frame sel:
           if rtab[frame-down] = ? then do:
               bell.
               message "This is the last page !".
               pause 1 no-message.
           end.
           else do: /* the downmost line wasn't empty */
               memory = rtab[frame-down].
               must-print = true.
               next LOOP.
           end.
        end. /* next page */

        /* Seek */
        if lookup(nap,"1,f1") > 0 then do on ENDkey undo, NEXT LOOP:
           /*CLI*/
           cfc = "puyr". RUN Syst/ufcolor.
           ehto = 9. RUN Syst/ufkey. ufkey = true.
           clear frame hayr.
           disp m_pref with frame  hayr.
           set CLI with frame hayr.
           hide frame hayr no-pause.
           if CLI ENTERED then do:
              find first MSISDN where 
                         Msisdn.Brand = gcBrand AND 
                         MSISDN.CLI >= CLI AND
                          MSISDN.ValidTo > fMakeTS() AND
                         MSISDN.StatusCode = 1
              no-lock no-error.
             if not available MSISDN then do:
                       bell.
                       message "None found !".    
                       pause 1 no-message.
                       next BROWSE.
              end.
              /*  MSISDN was found */
              assign
                memory = recid(MSISDN)
                must-print = true.
           end.
           next LOOP.
        end. /* Seek */




     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN 
CUST:     
     REPEAT: /* show Customer data */
        RUN local-find-this (false).
        IF MSISDN.CustNum = 0 then do:
           message 
           "This MSISDN " MSISDN.CLI skip
           "is not attached to any Customer !"
           view-as alert-box error.
           leave.
        end.
        pause 0.
        clear frame cust no-pause.
CU-DATA:
        repeat with frame cust:

           find Customer of MSISDN WHERE
                            Msisdn.Brand      = gcBrand AND 
                            MSISDN.StatusCode = 1 no-lock no-error.

           disp
           MSISDN.CustNum    Customer.CustName when avail Customer
           with frame cust.
CU-Action:
           repeat with frame cust:
              assign ufk = 0 ufk[8] = 8 ehto =  0.
              RUN Syst/ufkey.
              case toimi:
                 WHEN 8 THEN do:
                    ufkey = true. 
                    hide frame cust no-pause.
                    LEAVE cu-data.
                 END.   
              end.
           end.     
        end.   
        ufkey = true. 
        NEXT LOOP.
     end.      






        /* Choose */
        else if lookup(nap,"return,enter,5,f5") > 0 then do:
           find MSISDN where recid(MSISDN) = rtab[frame-line] no-lock.
           siirto = string(MSISDN.CLI).
           leave MAIN.
        end. /* Choose */
        /* First record */
        else if lookup(nap,"home,h") > 0 then do:
           find first MSISDN WHERE
                      MSISDN.Brand      = gcBrand  AND 
                       MSISDN.ValidTo > fMakeTS()  AND 
                      MSISDN.StatusCode = 1 no-lock.
           memory = recid(MSISDN).
           must-print = true.
           next LOOP.
        end. /* First record */

        /* last record */
        else if lookup(nap,"end,e") > 0 then do :
           find last MSISDN WHERE
                     MSISDN.Brand      = gcBrand AND 
                      MSISDN.ValidTo > fMakeTS()  AND 
                     MSISDN.StatusCode = 1 no-lock.
           memory = recid(MSISDN).
           must-print = true.
           next LOOP.
        end. /* last record */

        else if nap = "8" or nap = "f8" then leave MAIN. /* Return */

     end.  /* BROWSE */
   end.  /* LOOP */
end. /* MAIN */
hide frame sel no-pause.

PROCEDURE local-find-this:

    def INPUT parameter exlock as lo no-undo.

    if exlock then
      find MSISDN WHERE recid(MSISDN) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       find MSISDN WHERE recid(MSISDN) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE LOCAL-DISP-ROW:
    find MSStat where 
         MSStat.StatusCode = MSISDN.StatusCode 
    no-lock no-error.
    display
         MSISDN.CLI
         MSStat.StatusName
         MSISDN.ActionDate
         MSISDN.ValidFrom
         MSISDN.CustNum
         MSClass.MCName
    with frame sel.

END.
