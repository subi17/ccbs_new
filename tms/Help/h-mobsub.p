/***********************************************************
  Module .......: H-mobsub.P
  Parent .......: APPLHELP.P
  FUNCTION .....: Help browser of mobsub 
  APPLICATION ..: NN
  AUTHOR .......: jp
  CREATED ......: 28-06-99
  MODIFIED .....: 24.08.99 pt mobsub.i
  Version ......: M15
                 03.03.03  jp billLevel removed
  ------------------------------------------------------ */

{Syst/commali.i}

def shared var siirto as char.

def var CLI     like mobsub.CLI   no-undo.
def var StatusCode   like MSStat.StatusCode no-undo.
def var rtab        as recid extent 11      no-undo.
def var ufkey       as log init true        no-undo.
def var i           as int                  no-undo.
def var memory      as recid                no-undo.
def var must-print  as logic                no-undo.
def var must-add    as logic                no-undo.

form
      mobsub.CLI
      
      mobsub.Custnum
      mobsub.ActivationDate
      mobsub.CustNum

      with scroll 1 11 down  row 4 centered color value(cfc)
    title color value(ctc) " mobsubs " overlay frame sel.

form /* SEEK Code */
    CLI format "x(12)"
    help "Enter mobsub No. "
    with row 4 col 2 title color value(ctc) " FIND mobsub "
    color value(cfc) no-labels overlay frame hayr.



cfc = "sel". RUN Syst/ufcolor.p. assign ccc = cfc.

MAIN:
repeat:

   find first mobsub WHERE 
              mobsub.BRand      = gcBrand 
   NO-LOCK NO-ERROR.

   if not available mobsub then do:
      must-print = false.
      must-add = true.
   end.
   else do:
      memory = recid(mobsub).
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
         find mobsub where recid(mobsub) = memory no-lock no-error.
         /* Print to screen */
         rtab = ?.
         do while frame-line<= frame-down and available mobsub:
            RUN local-disp-row.
            rtab[frame-line] = recid(mobsub).
            down with frame sel.
            find next mobsub WHERE
                      mobsub.BRand      = gcBrand  no-lock no-error.
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
         choose row mobsub.CLI {Syst/uchoose.i} no-error with frame sel.
         color display value(ccc) mobsub.CLI with frame sel.

         if frame-value = "" and rtab[frame-line] = ? then next.
         nap = keylabel(lastkey).

         /* previous line */
         if lookup(nap,"cursor-up") > 0 then do
         with frame sel:
            if frame-line = 1 then do:
               find mobsub where recid(mobsub) = rtab[frame-line] no-lock.
               find prev mobsub WHERE
                         mobsub.BRand      = gcBrand  
               no-lock no-error.
               if not available mobsub then do:
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
                  rtab[frame-line] = recid(mobsub).
                  memory = recid(mobsub).
               end.
            end.
            else up 1.
         end. /* previous line */

         /* next line */
         if lookup(nap,"cursor-down") > 0 then do with frame sel:
            if frame-line = frame-down then do:
               find mobsub where recid(mobsub) = rtab[frame-line] no-lock .
               find next mobsub WHERE
                         mobsub.Brand      = gcBrand 
               no-lock no-error.
               if not available mobsub then do:
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
                  rtab[frame-line] = recid(mobsub).
                  /* finally last line's KeyValue is saved */
                  memory = rtab[1].
               end.
            end.
            else down 1 .
         end. /* next line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 then do with frame sel:
            find mobsub where recid(mobsub) = memory no-lock no-error.
            find prev mobsub WHERE
                      mobsub.Brand      = gcBrand 
            no-lock no-error.
            if available mobsub then do:

               do i = 1 to (frame-down - 1):
                  find prev mobsub WHERE
                            mobsub.Brand      = gcBrand no-lock no-error.
                  if available mobsub then memory = recid(mobsub).
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
           cfc = "puyr". RUN Syst/ufcolor.p.
           ehto = 9. RUN Syst/ufkey.p. ufkey = true.
           clear frame hayr.
           set CLI with frame hayr.
           hide frame hayr no-pause.
           if CLI ENTERED then do:
              find first mobsub where 
                         mobsub.Brand = gcBrand AND 
                         mobsub.CLI >= CLI 
              no-lock no-error.
             if not available mobsub then do:
                       bell.
                       message "None found !".    
                       pause 1 no-message.
                       next BROWSE.
              end.
              /*  mobsub was found */
              assign
                memory = recid(mobsub)
                must-print = true.
           end.
           next LOOP.
        end. /* Seek */


     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN  DO:
        RUN local-find-this (false).
        IF mobsub.IMSI = "" THEN DO:
           MESSAGE
           "This mobsub No. (" + mobsub.CLI + ")"  skip
           "is NOT attached to any IMSI No !"
           VIEW-AS ALERT-BOX Error.
           NEXT loop.
        END.

        FIND IMSI WHERE imsi.IMSI = mobsub.IMSI NO-LOCK NO-ERROR.

        IF IMSI.UserSeq = 0 THEN DO:
           MESSAGE
           "This mobsub No." mobsub.CLI      skip
           "Belongs to IMSI No." mobsub.IMSI  skip(1)
           "BUT"                               skip(1)

           "There is no Subscriber Associated" skip
           "With IMSI No." mobsub.IMSI
           view-as alert-box error.
        END.   
        ELSE RUN Mm/shmobu.p(IMSI.UserSeq).

        ufkey = true.
        NEXT loop.
     END.



     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN 
CUST:     
     REPEAT: /* show Customer data */
        RUN local-find-this (false).
        IF mobsub.CustNum = 0 then do:
           message 
           "This mobsub " mobsub.CLI skip
           "is not attached to any Customer !"
           view-as alert-box error.
           leave.
        end.
        pause 0.
        clear frame cust no-pause.
CU-DATA:
        repeat with frame cust:

           find Customer of mobsub WHERE
                            mobsub.Brand      = gcBrand no-lock no-error.

           disp
           mobsub.CustNum    Customer.CustName when avail Customer
           mobsub.IMSI
           with frame cust.
CU-Action:
           repeat with frame cust:
              assign ufk = 0 ufk[8] = 8 ehto =  0.
              RUN Syst/ufkey.p.
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
           find mobsub where recid(mobsub) = rtab[frame-line] no-lock.
           siirto = string(mobsub.msseq).
           leave MAIN.
        end. /* Choose */
        /* First record */
        else if lookup(nap,"home,h") > 0 then do:
           find first mobsub WHERE
                      mobsub.Brand      = gcBrand  no-lock no-error.
           memory = recid(mobsub).
           must-print = true.
           next LOOP.
        end. /* First record */

        /* last record */
        else if lookup(nap,"end,e") > 0 then do :
           find last mobsub WHERE
                     mobsub.Brand      = gcBrand no-lock no-error.
           memory = recid(mobsub).
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
      find mobsub WHERE recid(mobsub) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       find mobsub WHERE recid(mobsub) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE LOCAL-DISP-ROW:

    display
         mobsub.CLI
         mobsub.Custnum
         mobsub.ActivationDate
    with frame sel.

END.
