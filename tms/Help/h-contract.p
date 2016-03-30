/*------------------------------------------------------
  Module .......: h-Contract.p
  Parent .......: APPLHELP.P
  FUNCTION .....: Help browser of contract 
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 02.01.03
  MODIFIED .....: 16.01.04/aam try fo find current CLI for contract
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

DEF INPUT PARAMETER iiCustNum AS INT NO-UNDO. 

def shared var siirto as char.

DEF VAR ldtDate     as date                 no-undo. 
def var rtab        as recid extent 11      no-undo.
def var ufkey       as log init true        no-undo.
def var i           as int                  no-undo.
def var memory      as recid                no-undo.
def var must-print  as logic                no-undo.
def var must-add    as logic                no-undo.
DEF VAR lcCLI       AS CHAR                 NO-UNDO. 

form
    Contract.FromDate
    Contract.ToDate
    Contract.Contract
    Contract.CustNum
    lcCLI  FORMAT "X(16)" LABEL "CLI" 
    Contract.Salesman
    with scroll 1 11 down  row 4 centered color value(cfc)
    title color value(ctc) " Contracts " overlay frame sel.

form /* SEEK Code */
    ldtDate
    help "Enter Contract Date"
    with row 4 col 2 title color value(ctc) " FIND CONTRACT "
    color value(cfc) no-labels overlay frame hayr.

cfc = "sel". RUN Syst/ufcolor. assign ccc = cfc.

MAIN:
repeat:

   find first Contract where Contract.CustNum = iiCustNum
   no-lock no-error.
   if not available Contract then do:
      must-print = false.
      must-add = false.
   end.
   else do:
      memory = recid(Contract).
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
         find Contract where recid(Contract) = memory no-lock no-error.

         /* Print to screen */
         rtab = ?.
         do while frame-line<= frame-down and available Contract:
         
            run local-disp-row.

            rtab[frame-line] = recid(Contract).
            down with frame sel.
            find next Contract where Contract.CustNum = iiCustNum 
            no-lock no-error.
         end.
         must-print = false.
         up frame-line(sel) - 1 with frame sel.
      end. /* must-print */

      if ufkey then do:
         assign
         ufk = 0 
         ufk[1] = 28 ufk[5] = 11
         ufk[6] = 0  ufk[8] = 8  ufk[9] = 1
         siirto = ? ehto = 3 ufkey = false.
         RUN Syst/ufkey.p.
      end.
  end. /* print-line */

BROWSE:
      repeat with frame sel on endkey undo, retuRN:

         hide message no-pause.
         choose row Contract.FromDate {Syst/uchoose.i} no-error with frame sel.
         color display value(ccc) Contract.FromDate with frame sel.

         nap = keylabel(lastkey).

         if frame-value = "" and rtab[frame-line] = ? and
            lookup(nap,"8,f8") = 0
         then next.

         /* previous line */
         if lookup(nap,"cursor-up") > 0 then do
         with frame sel:
            if frame-line = 1 then do:
               find Contract where recid(Contract) = rtab[frame-line] no-lock.
               find prev Contract where Contract.CustNum = iiCustNum
               no-lock no-error.
               if not available Contract then do:
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
      
                  run local-disp-row.

                  rtab[frame-line] = recid(Contract).
                  memory = recid(Contract).
               end.
            end.
            else up 1.
         end. /* previous line */

         /* next line */
         if lookup(nap,"cursor-down") > 0 then do with frame sel:
            if frame-line = frame-down then do:
               find Contract where recid(Contract) = rtab[frame-line] no-lock .
               find next Contract where Contract.CustNum = iiCustNum
               no-lock no-error.
               if not available Contract then do:
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
      
                  run local-disp-row.

                  rtab[frame-line] = recid(Contract).
                  /* finally last line's KeyValue is saved */
                  memory = rtab[1].
               end.
            end.
            else down 1 .
         end. /* next line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 then do with frame sel:
            find Contract where recid(Contract) = memory no-lock no-error.
            find prev Contract where Contract.CustNum = iiCustNum
            no-lock no-error.
            if available Contract then do:

               do i = 1 to (frame-down - 1):
                  find prev Contract where Contract.CustNum = iiCustNum
                  no-lock no-error.
                  if available Contract then memory = recid(Contract).
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
           /*ldtDate*/
           cfc = "puyr". RUN Syst/ufcolor.
           ehto = 9. RUN Syst/ufkey. ufkey = true.
           set ldtDate with frame hayr.
           hide frame hayr no-pause.
           if ldtDate ENTERED then do:
              find first Contract where 
                         Contract.CustNum = iiCustNum AND
                         Contract.FromDate >= ldtDate
              no-lock no-error.
             if not available Contract then do:
                       bell.
                       message "None found !".    
                       pause 1 no-message.
                       next BROWSE.
              end.
              /*  Contract was found */
              assign
                memory = recid(Contract)
                must-print = true.
           end.
           next LOOP.
        end. /* Seek */

        /* Choose */
        else if lookup(nap,"return,enter,5,f5") > 0 then do:
           find Contract where recid(Contract) = rtab[frame-line] no-lock.
           siirto = string(Contract.Contract).
           leave MAIN.
        end. /* Choose */
        /* First record */
        else if lookup(nap,"home,h") > 0 then do:
           find first Contract no-lock where Contract.CustNum = iiCustNum
              no-error.
           memory = recid(Contract).
           must-print = true.
           next LOOP.
        end. /* First record */

        /* last record */
        else if lookup(nap,"end,e") > 0 then do :
           find last Contract no-lock where Contract.CustNum = iiCustNum
              no-error.
           memory = recid(Contract).
           must-print = true.
           next LOOP.
        end. /* last record */

        else if nap = "8" or nap = "f8" then leave MAIN. /* Return */

     end.  /* BROWSE */
   end.  /* LOOP */
end. /* MAIN */
hide frame sel no-pause.

PROCEDURE local-disp-row:

    lcCLI = "".

    FOR EACH MSOwner NO-LOCK WHERE
             MSOwner.Brand    = Contract.Brand AND
             MSOwner.Contract = Contract.Contract
    BY MsOwner.TSEnd DESC:
    
        lcCLI = MsOwner.CLI.
        LEAVE.
    END.
    
    display Contract.FromDate
            Contract.ToDate
            Contract.Contract
            Contract.CustNum
            Contract.Salesman
            lcCLI
            with frame sel.
            
END PROCEDURE.
