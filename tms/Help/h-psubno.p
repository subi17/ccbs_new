/*------------------------------------------------------
  MODULE .......: h-pCLI.p
  PARENT .......: APPLHELP.P
  FUNCTION .....: Help browser of customer's CLIs
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 16-06-99
  MODIFIED .....: 11.09.02 jp only active CLIs
  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

def INPUT Parameter CustNum like presel.CustNum no-undo.

def  shared var siirto as char.

def var CLI      like CLI.CLI    no-undo.
def var rtab        as recid extent 11      no-undo.
def var ufkey       as log init true        no-undo.
def var i           as int                  no-undo.
def var memory      as recid                no-undo.
def var must-print  as logic                no-undo.
def var must-add    as logic                no-undo.
def var lname       as c                    no-undo.


form
      CLI.CLI format "x(20)"
    with scroll 1 11 down  row 4 WIDTH 40 centered color value(Syst.Var:cfc)
    title color value(Syst.Var:ctc) " SubNumbers of Cust(" + string(CustNum) + ") " 
    overlay frame sel.

form /* SEEK CODE */
    CLI
    help "Enter Customer's CLI"
    with row 4 col 2 title color value(Syst.Var:ctc) " FIND NUMBER "
    color value(Syst.Var:cfc) no-labels overlay frame hayr.

find  Customer where Customer.CustNum =  CustNum no-lock.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. assign Syst.Var:ccc = Syst.Var:cfc.
view frame sel.
MAIN:
repeat:

   find first CLI where CLI.CustNum = CustNum AND
   cli.clstamp >= 20491231 no-lock no-error.
   if not available CLI then do:
      MESSAGE
      "There are NOT any Subscriper's numbers"   SKIP
      "for customer n:o" CustNum
      VIEW-AS ALERT-BOX
      TITLE " NO NUMBERS ".
      RETURN.
        /* must-print = false. must-add = true.  */
   end.
   else do:
      memory = recid(CLI).
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
         find CLI where recid(CLI) = memory no-lock no-error.

         /* Print to screen */
         rtab = ?.
         do while frame-line<= frame-down and available CLI:

            display
            CLI.CLI
            with frame sel.
            rtab[frame-line] = recid(CLI).
            down with frame sel.
            find next CLI where CLI.CustNum = CustNum AND
            CLI.clstamp >= 20491231 no-lock no-error.
         end.
         must-print = false.
         up frame-line(sel) - 1 with frame sel.
      end. /* must-print */

      if ufkey then do:
         assign
         Syst.Var:ufk = 0 Syst.Var:ufk[1] = 36 Syst.Var:ufk[5] = 11
         Syst.Var:ufk[6] = 0 Syst.Var:ufk[8] = 8  Syst.Var:ufk[9] = 1
         siirto = ? Syst.Var:ehto = 3 ufkey = false.
         RUN Syst/ufkey.p.
      end.
  end. /* print-line */

BROWSE:
      repeat with frame sel on endkey undo, return:

         hide message no-pause.
         choose row CLI.CLI {Syst/uchoose.i} no-error with frame sel.
         color display value(Syst.Var:ccc) CLI.CLI with frame sel.

         if frame-value = "" and rtab[frame-line] = ? then next.
         Syst.Var:nap = keylabel(lastkey).

         /* previous line */
         if lookup(Syst.Var:nap,"cursor-up") > 0 then do
         with frame sel:
            if frame-line = 1 then do:
               find CLI where recid(CLI) = rtab[frame-line] no-lock.
               find prev CLI where CLI.CustNum = CustNum AND
               CLI.clstamp >= 20491231 no-lock no-error.
               if not available CLI then do:
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

                  display
                  CLI.CLI
                  with frame sel.

                  rtab[frame-line] = recid(CLI).
                  memory = recid(CLI).
               end.
            end.
            else up 1.
         end. /* previous line */

         /* next line */
         if lookup(Syst.Var:nap,"cursor-down") > 0 then do with frame sel:
            if frame-line = frame-down then do:
               find CLI where recid(CLI) = rtab[frame-line] no-lock.
               find NEXT CLI where CLI.CustNum = CustNum AND
               CLI.clstamp >= 20491231 no-lock no-error.
               if not available CLI then do:
                  bell.
                  message "You are on last row !".              
                  pause 1 no-message.
                  next BROWSE.
               end.
               else do:
                  /* a previous one was found */
                  scroll up.
                  do i = 1 to 10 :
                     rtab[i] = rtab[i + 1].
                  end.

                  display
                  CLI.CLI
                  with frame sel.

                  rtab[frame-line] = recid(CLI).
                  memory = recid(CLI).
               end.
            end.
            else down 1.


         end. /* next line */

         /* previous page */
         else if lookup(Syst.Var:nap,"page-up,prev-page") > 0 then do with frame sel:
            find CLI where recid(CLI) = memory no-lock no-error.
            find prev CLI where CLI.CustNum = CustNum AND
            CLI.clstamp >= 20491231 no-lock no-error.
            if available CLI then do:

               do i = 1 to (frame-down - 1):
                  find prev CLI where CLI.CustNum = CustNum AND
                  CLI.clstamp >= 20491231   no-lock no-error.
                  if available CLI then memory = recid(CLI).
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
        else if lookup(Syst.Var:nap,"page-down,next-page") > 0 then do with frame sel:
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
        if lookup(Syst.Var:nap,"1,f1") > 0 then do:  /* bs-code */
           Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
           Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = true.
           set CLI with frame hayr.
           hide frame hayr no-pause.
           if CLI ENTERED then do:
              find first CLI where CLI.CLI >= CLI         AND
                                   CLI.CustNum = CustNum  AND
                                   CLI.clstamp >= 20491231
              no-lock no-error.
              if not available CLI then do:
                       bell.
                       message "None found !".    
                       pause 1 no-message.
                       next BROWSE.
              end.
              /*  CLI was found */
              assign
                memory = recid(CLI)
                must-print = true.
           end.
           next LOOP.
        end. /* Seek */

        /* Choose */
        else if lookup(Syst.Var:nap,"return,enter,5,f5") > 0 then do:
           find CLI where recid(CLI) = rtab[frame-line] no-lock.
           IF CAN-FIND(FIRST presel WHERE presel.CLI = CLI.CLI) THEN
           DO:
              MESSAGE "Number has been allready preselected !". 
              PAUSE.
              NEXT.
           END.
           ELSE DO:
              siirto = string(CLI.CLI).
              leave MAIN.
           END.
        end. /* Choose */
        /* First record */
        else if lookup(Syst.Var:nap,"home,h") > 0 then do:
           find first CLI where CLI.CustNum = CustNum AND
           CLI.clstamp >= 20491231 no-lock.
           memory = recid(CLI).
           must-print = true.
           next LOOP.
        end. /* First record */

        /* last record */
        else if lookup(Syst.Var:nap,"end,e") > 0 then do :
           find last CLI where CLI.CustNum = CustNum AND 
           CLI.clstamp >= 20491231 no-lock.
           memory = recid(CLI).
           must-print = true.
           next LOOP.
        end. /* last record */

        else if Syst.Var:nap = "8" or Syst.Var:nap = "f8" then leave MAIN. /* Return */

     end.  /* BROWSE */
   end.  /* LOOP */
end. /* MAIN */
hide frame sel no-pause.


