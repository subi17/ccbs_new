/*------------------------------------------------------
  Module .......: H-ACYP.P
  Parent .......: APPLHELP.P
  FUNCTION .....: Help browser of Account Types
  APPLICATION ..: NN
  AUTHOR .......: jp
  CREATED ......: 04-02-00 jp
  MODIFIED .....: 
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

def /*new*/ shared var siirto as char.

def var AccType      like Account.AccType    no-undo.
def var rtab         as recid extent 11      no-undo.
def var ufkey        as log init true        no-undo.
def var i            as int                  no-undo.
def var memory       as recid                no-undo.
def var must-print   as logic                no-undo.
def var must-add     as logic                no-undo.
def var AccType-name as c                    no-undo.


DEF TEMP-TABLE actype NO-UNDO
   FIELD AccType AS I  FORMAT ">9"   LABEL "AccountType"
   FIELD ac-name AS C  FORMAT "x(30)" LABEL "AccountName"
   INDEX AccType AccType.



form
      actype.AccType
      actype.ac-name  format "x(30)"
    with scroll 1 11 down  row 4 centered color value(cfc)
    title color value(ctc) " Account TYPES " overlay frame sel.

form /* SEEK Code */
    AccType
    help "Enter Code of AccType"
    with row 4 col 2 title color value(ctc) " FIND CODE "
    color value(cfc) no-labels overlay frame hayr.


FOR EACH TMSCodes NO-LOCK WHERE
         TMSCodes.TableName = "Account" AND
         TMSCodes.FieldName = "AccType" AND
         TMSCodes.InUse > 0:
         
   CREATE actype.
   ASSIGN
      actype.AccType = INTEGER(TMSCodes.CodeValue)
      actype.ac-name = TMSCodes.CodeName.
END.


cfc = "sel". RUN Syst/ufcolor.p. assign ccc = cfc.

MAIN:
repeat:

   find first actype no-lock no-error.
   if not available actype then do:
      must-print = false.
      must-add = true.
   end.
   else do:
      memory = recid(actype).
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
         find actype where recid(actype) = memory no-lock no-error.

         /* Print to screen */
         rtab = ?.
         do while frame-line<= frame-down and available acType:
            display
            actype.AccType
            actype.ac-name
            with frame sel.
            rtab[frame-line] = recid(actype).
            down with frame sel.
            find next actype no-lock no-error.
         end.
         must-print = false.
         up frame-line(sel) - 1 with frame sel.
      end. /* must-print */

      if ufkey then do:
         assign
         ufk = 0 ufk[1] = 35 ufk[5] = 11
         ufk[6] = 0 ufk[8] = 8  ufk[9] = 1
         siirto = ? ehto = 3 ufkey = false.
         RUN Syst/ufkey.p.
      end.
  end. /* print-line */

BROWSE:
      repeat with frame sel on endkey undo, retuRN:

         hide message no-pause.
         choose row actype.AccType {Syst/uchoose.i} no-error with frame sel.
         color display value(ccc) actype.AccType with frame sel.

         if frame-value = "" and rtab[frame-line] = ? then next.
         nap = keylabel(lastkey).

         /* previous line */
         if lookup(nap,"cursor-up") > 0 then do
         with frame sel:
            if frame-line = 1 then do:
               find actype where recid(actype) = rtab[frame-line] no-lock.
               find prev actype no-lock no-error.
               if not available actype then do:
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
                  display actype.AccType actype.ac-name.
                  rtab[frame-line] = recid(actype).
                  memory = recid(actype).
               end.
            end.
            else up 1.
         end. /* previous line */

         /* next line */
         if lookup(nap,"cursor-down") > 0 then do with frame sel:
            if frame-line = frame-down then do:
               find actype where recid(actype) = rtab[frame-line] no-lock .
               find next actype no-lock no-error.
               if not available actype then do:
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
                  display actype.AccType actype.ac-name.
                  rtab[frame-line] = recid(actype).
                  /* finally last line's KeyValue is saved */
                  memory = rtab[1].
               end.
            end.
            else down 1 .
         end. /* next line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 then do with frame sel:
            find actype where recid(actype) = memory no-lock no-error.
            find prev actype no-lock no-error.
            if available actype then do:

               do i = 1 to (frame-down - 1):
                  find prev actype no-lock no-error.
                  if available actype then memory = recid(actype).
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
           /*se-code*/
           cfc = "puyr". RUN Syst/ufcolor.p.
           ehto = 9. RUN Syst/ufkey.p. ufkey = true.
           set AccType with frame hayr.
           hide frame hayr no-pause.
           if AccType ENTERED then do:
              find first actype where actype.AccType >= AccType
              no-lock no-error.
             if not available actype then do:
                       bell.
                       message "None found !".    
                       pause 1 no-message.
                       next BROWSE.
              end.
              /*  Account was found */
              assign
                memory = recid(actype)
                must-print = true.
           end.
           next LOOP.
        end. /* Seek */

        /* Choose */
        else if lookup(nap,"return,enter,5,f5") > 0 then do:
           find actype where recid(actype) = rtab[frame-line] no-lock.
           siirto = string(actype.AccType).
           leave MAIN.
        end. /* Choose */
        /* First record */
        else if lookup(nap,"home,h") > 0 then do:
           find first actype no-lock.
           memory = recid(actype).
           must-print = true.
           next LOOP.
        end. /* First record */

        /* last record */
        else if lookup(nap,"end,e") > 0 then do :
           find last actype no-lock.
           memory = recid(actype).
           must-print = true.
           next LOOP.
        end. /* last record */

        else if nap = "8" or nap = "f8" then leave MAIN. /* Return */

     end.  /* BROWSE */
   end.  /* LOOP */
end. /* MAIN */
hide frame sel no-pause.
