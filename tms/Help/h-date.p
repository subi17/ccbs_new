/*------------------------------------------------------
  Module .......: h-date.p
  Parent .......: h-date.p
  FUNCTION .....: Help browser of date
  APPLICATION ..: NN
  AUTHOR .......: pt
  CREATED ......: 14-03-07 kl

  MODIFIED .....: 
  Version ......: yoigo
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/fctchange.i}

def shared var siirto as char.

DEF INPUT PARAMETER icvalue AS CHAR NO-UNDO.
DEF INPUT PARAMETER iiMSSeq AS INT  NO-UNDO.
DEF INPUT PARAMETER icNewCLIType AS CHAR NO-UNDO.

def var rtab        as recid extent 11          no-undo.
def var ufkey       as log init true            no-undo.
def var i           as int                      no-undo.
def var memory      as recid                    no-undo.
def var must-print  as logic                    no-undo.
def var must-add    as logic                    no-undo.
DEF VAR new-ts-date AS DATE                     NO-UNDO.
DEF VAR ldaSTCDates AS DATE NO-UNDO EXTENT 2.

DEF buffer bMobsub FOR Mobsub.

DEFINE TEMP-TABLE Paiva NO-UNDO 
   FIELD Paiva AS DATE FORMAT "99-99-9999"
INDEX paiva AS PRIMARY paiva.

FIND FIRST bMobsub WHERE 
           bMobsub.MSSeq = iiMSSeq NO-LOCK NO-ERROR.

FIND FIRST CLIType WHERE
           CLIType.Brand = "1" AND
           CLIType.CLIType = icNewCLIType NO-LOCK NO-ERROR.

ASSIGN ldaSTCDates[1] = TODAY + 1
       ldaSTCDates[2] = Func.Common:mLastDayOfMonth(TODAY) + 1.

/* Only postpaid to postpaid */
IF NOT (CLIType.PayType = 2 OR bMobsub.PayType = TRUE) AND
   ldaSTCDates[1] <> ldaSTCDates[2] AND
   fIsiSTCAllowed(INPUT bMobsub.MsSeq) THEN DO:
   CREATE paiva.
          paiva = ldaSTCDates[1].
END.

/* Only prepaid to prepaid */
IF NOT (CLIType.PayType = 1 OR bMobsub.PayType = FALSE) AND
   ldaSTCDates[1] <> ldaSTCDates[2] AND
   fIsiSTCAllowed(INPUT bMobsub.MsSeq) THEN DO:
   CREATE paiva.
          paiva = ldaSTCDates[1].
END.

CREATE paiva.
       paiva = ldaSTCDates[2].

form
      paiva.paiva LABEL "" FORMAT "99-99-9999"

    with scroll 1 4 down  row 4 centered color value(Syst.Var:cfc)
    title color value(Syst.Var:ctc) " Dates " overlay frame sel.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. assign Syst.Var:ccc = Syst.Var:cfc.
MAIN:
repeat:

   find first Paiva no-lock no-error.
   if not available Paiva then do:
      must-print = false.
      MESSAGE "No invoice sections available"
      VIEW-AS ALERT-BOX INFORMATION.
      RETURN.
   end.
   else do:
      memory = recid(Paiva).
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
         find Paiva where recid(Paiva) = memory no-lock no-error.

         /* Print to screen */
         rtab = ?.
         do while frame-line<= frame-down and available Paiva:
            display
            Paiva.Paiva
            with frame sel.
            rtab[frame-line] = recid(Paiva).
            down with frame sel.
            find next Paiva
            no-lock no-error.
         end.
         must-print = false.
         up frame-line(sel) - 1 with frame sel.
      end. /* must-print */

      if ufkey then do:
         assign
         Syst.Var:ufk = 0 Syst.Var:ufk[1] = 0 Syst.Var:ufk[5] = 11
         Syst.Var:ufk[6] = 0 Syst.Var:ufk[8] = 8  Syst.Var:ufk[9] = 1
         siirto = ? Syst.Var:ehto = 3 ufkey = false.
         RUN Syst/ufkey.p.
      end.
  end. /* print-line */

BROWSE:
      repeat with frame sel on endkey undo, retuRN:

         hide message no-pause.
         choose row Paiva.Paiva {Syst/uchoose.i} no-error with frame sel.
         color display value(Syst.Var:ccc) Paiva.Paiva with frame sel.

         if frame-value = "" and rtab[frame-line] = ? then next.
         Syst.Var:nap = keylabel(lastkey).

         /* previous line */
         if lookup(Syst.Var:nap,"cursor-up") > 0 then do
         with frame sel:
            if frame-line = 1 then do:
               find Paiva where recid(Paiva) = rtab[frame-line] no-lock.
               find prev Paiva
               no-lock no-error.
               if not available Paiva then do:
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
                  display Paiva.Paiva.
                  rtab[frame-line] = recid(Paiva).
                  memory = recid(Paiva).
               end.
            end.
            else up 1.
         end. /* previous line */

         /* next line */
         if lookup(Syst.Var:nap,"cursor-down") > 0 then do with frame sel:
            if frame-line = frame-down then do:
               find Paiva where recid(Paiva) = rtab[frame-line] no-lock .
               find next Paiva
               no-lock no-error.
               if not available Paiva then do:
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
                  display Paiva.Paiva .
                  rtab[frame-line] = recid(Paiva).
                  /* finally last line's KeyValue is saved */
                  memory = rtab[1].
               end.
            end.
            else down 1 .
         end. /* next line */

         /* previous page */
         else if lookup(Syst.Var:nap,"page-up,prev-page") > 0 then do with frame sel:
            find Paiva where recid(Paiva) = memory no-lock no-error.
            find prev Paiva
            no-lock no-error.
            if available Paiva then do:

               do i = 1 to (frame-down - 1):
                  find prev Paiva
                  no-lock no-error.
                  if available Paiva then memory = recid(Paiva).
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

        /* Choose */
        else if lookup(Syst.Var:nap,"return,enter,5,f5") > 0 then do:
           find Paiva where recid(Paiva) = rtab[frame-line] no-lock.
           siirto = string(Paiva.Paiva,"99-99-99").
           leave MAIN.
        end. /* Choose */
        /* First record */
        else if lookup(Syst.Var:nap,"home,h") > 0 then do:
           find first Paiva no-lock.
           memory = recid(Paiva).
           must-print = true.
           next LOOP.
        end. /* First record */

        /* last record */
        else if lookup(Syst.Var:nap,"end,e") > 0 then do :
           find last Paiva no-lock.
           memory = recid(Paiva).
           must-print = true.
           next LOOP.
        end. /* last record */

        else if Syst.Var:nap = "8" or Syst.Var:nap = "f8" then leave MAIN. /* Return */

     end.  /* BROWSE */
   end.  /* LOOP */
end. /* MAIN */
hide frame sel no-pause.
