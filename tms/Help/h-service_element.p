/*------------------------------------------------------
  Module .......: h-service_element.p
  Parent .......: APPLHELP.P
  FUNCTION .....: Help browser of service elements (components of a package)
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 09.11.10
  Version ......: yoigo
  ------------------------------------------------------ */

{Syst/commali.i}

def shared var siirto as char.

DEF VAR lcEvent     as char                 no-undo. 
def var rtab        as recid extent 11      no-undo.
def var ufkey       as log init true        no-undo.
def var i           as int                  no-undo.
def var memory      as recid                no-undo.
def var must-print  as logic                no-undo.
def var must-add    as logic                no-undo.
DEF VAR lcServPac   AS CHAR                 NO-UNDO.

form
    ServEl.ServPac   
    ServEl.ServCom    
    ServCom.SCName FORMAT "X(30)" COLUMN-LABEL "Name"
    with scroll 1 11 down  row 4 centered color value(Syst.Var:cfc)
    title color value(Syst.Var:ctc) " SERVICE COMPONENTS " overlay frame sel.

form /* SEEK Code */
    lcEvent
    help "Enter component"
    with row 4 col 2 title color value(Syst.Var:ctc) " FIND Component "
    color value(Syst.Var:cfc) no-labels overlay frame hayr.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. assign Syst.Var:ccc = Syst.Var:cfc.

ASSIGN
   lcServPac = Syst.Var:gcHelpParam
   Syst.Var:gcHelpParam = "".

MAIN:
repeat:

   RUN local-find-first.
   if not available ServEl then do:
      must-print = false.
      must-add = false.
   end.
   else do:
      memory = recid(ServEl).
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
         find ServEl where recid(ServEl) = memory no-lock no-error.

         /* Print to screen */
         rtab = ?.
         do while frame-line<= frame-down and available ServEl:
         
            run local-disp-row.

            rtab[frame-line] = recid(ServEl).
            down with frame sel.
            RUN local-find-next.
         end.
         must-print = false.
         up frame-line(sel) - 1 with frame sel.
      end. /* must-print */

      if ufkey then do:
         assign
         Syst.Var:ufk = 0 
         Syst.Var:ufk[1] = 816 WHEN lcServPac > "" 
         Syst.Var:ufk[5] = 11
         Syst.Var:ufk[6] = 0  
         Syst.Var:ufk[8] = 8  
         siirto = ? 
         Syst.Var:ehto  = 3 
         ufkey = false.
         RUN Syst/ufkey.p.
      end.
  end. /* print-line */

      BROWSE:
      repeat with frame sel on endkey undo, retuRN:

         hide message no-pause.
         choose row ServEl.ServPac {Syst/uchoose.i} no-error with frame sel.
         color display value(Syst.Var:ccc) ServEl.ServPac with frame sel.

         Syst.Var:nap = keylabel(lastkey).

         if frame-value = "" and rtab[frame-line] = ? and
            lookup(Syst.Var:nap,"8,f8") = 0
         then next.

         /* previous line */
         if lookup(Syst.Var:nap,"cursor-up") > 0 then do
         with frame sel:
            if frame-line = 1 then do:
               find ServEl where recid(ServEl) = rtab[frame-line] 
                    no-lock.
               RUN local-find-prev.
               if not available ServEl then do:
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

                  rtab[frame-line] = recid(ServEl).
                  memory = recid(ServEl).
               end.
            end.
            else up 1.
         end. /* previous line */

         /* next line */
         if lookup(Syst.Var:nap,"cursor-down") > 0 then do with frame sel:
            if frame-line = frame-down then do:
               find ServEl where recid(ServEl) = rtab[frame-line] 
                    no-lock .
               RUN local-find-next.
               if not available ServEl then do:
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

                  rtab[frame-line] = recid(ServEl).
                  /* finally last line's KeyValue is saved */
                  memory = rtab[1].
               end.
            end.
            else down 1 .
         end. /* next line */

         /* previous page */
         else if lookup(Syst.Var:nap,"page-up,prev-page") > 0 then do with frame sel:
            find ServEl where recid(ServEl) = memory no-lock no-error.
            RUN local-find-prev.

            if available ServEl then do:
               do i = 1 to (frame-down - 1):
                  RUN local-find-prev.
                  if available ServEl then memory = recid(ServEl).
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
        if lookup(Syst.Var:nap,"1,f1") > 0 then do on ENDkey undo, NEXT LOOP:
           /*lcEvent*/
           Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
           Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = true.
           set lcEvent with frame hayr.
           hide frame hayr no-pause.
           if lcEvent ENTERED then do:
              find first ServEl where 
                         ServEl.Brand = Syst.Var:gcBrand AND
                         ServEl.ServPac = lcServPac AND
                         ServEl.ServCom >= lcEvent
              no-lock no-error.
             if not available ServEl then do:
                       bell.
                       message "None found !".    
                       pause 1 no-message.
                       next BROWSE.
              end.
              /*  ServEl was found */
              assign
                memory = recid(ServEl)
                must-print = true.
           end.
           next LOOP.
        end. /* Seek */

        /* Choose */
        else if lookup(Syst.Var:nap,"return,enter,5,f5") > 0 then do:
           find ServEl where recid(ServEl) = rtab[frame-line] no-lock.
           IF lcServPac > "" THEN siirto = ServEl.ServCom.
           ELSE siirto = ServEl.ServPac.
           leave MAIN.
        end. /* Choose */

        /* First record */
        else if lookup(Syst.Var:nap,"home,h") > 0 then do:
           RUN local-find-first.
           memory = recid(ServEl).
           must-print = true.
           next LOOP.
        end. /* First record */

        /* last record */
        else if lookup(Syst.Var:nap,"end,e") > 0 then do :
           RUN local-find-last.
           memory = recid(ServEl).
           must-print = true.
           next LOOP.
        end. /* last record */

        else if Syst.Var:nap = "8" or Syst.Var:nap = "f8" then leave MAIN. /* Return */

     end.  /* BROWSE */
   end.  /* LOOP */
end. /* MAIN */

hide frame sel no-pause.

PROCEDURE local-disp-row:

    FIND FIRST ServCom WHERE
               ServCom.Brand  = Syst.Var:gcBrand AND
               ServCom.ServCom = ServEl.ServCom NO-LOCK NO-ERROR.
    display ServEl.ServPac
            ServEl.ServCom
            ServCom.SCName WHEN AVAILABLE ServCom
    with frame sel.
            
END PROCEDURE.

PROCEDURE local-find-first:

   IF lcServPac > "" THEN 
      FIND FIRST ServEl WHERE 
                 ServEl.Brand = Syst.Var:gcBrand AND
                 ServEl.ServPac = lcServPac NO-LOCK NO-ERROR.
   ELSE FIND FIRST ServEl USE-INDEX ServPac WHERE
                   ServEl.Brand = Syst.Var:gcBrand NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-last:

   IF lcServPac > "" THEN 
      FIND LAST ServEl WHERE 
                ServEl.Brand = Syst.Var:gcBrand AND
                ServEl.ServPac = lcServPac NO-LOCK NO-ERROR.
   ELSE FIND LAST ServEl USE-INDEX ServPac WHERE
                  ServEl.Brand = Syst.Var:gcBrand NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-next:

   IF lcServPac > "" THEN 
      FIND NEXT ServEl WHERE 
                ServEl.Brand = Syst.Var:gcBrand AND
                ServEl.ServPac = lcServPac NO-LOCK NO-ERROR.
   ELSE FIND NEXT ServEl USE-INDEX ServPac WHERE
                  ServEl.Brand = Syst.Var:gcBrand NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-prev:

   IF lcServPac > "" THEN 
      FIND PREV ServEl WHERE 
                ServEl.Brand = Syst.Var:gcBrand AND
                ServEl.ServPac = lcServPac NO-LOCK NO-ERROR.
   ELSE FIND PREV ServEl USE-INDEX ServPac WHERE
                  ServEl.Brand = Syst.Var:gcBrand NO-LOCK NO-ERROR.

END PROCEDURE.


