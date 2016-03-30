/*------------------------------------------------------
  Module .......: h-fatgroup.p
  Parent .......: APPLHELP.P
  FUNCTION .....: Help browser of Fatime Group
  APPLICATION ..: NN
  AUTHOR .......: jp
  CREATED ......: 21.08.02
                  15.09.03 jp Brand                        
  MODIFIED .....: 08.09.06/aam type selection (gcHelpParam),
                               local-procedures
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

def  shared var siirto as char.

def var ob-code     like FATGroup.FTGrp     no-undo. 
def var rtab        as recid extent 11      no-undo.
def var ufkey       as log init true        no-undo.
def var i           as int                  no-undo.
def var memory      as recid                no-undo.
def var must-print  as logic                no-undo.
def var must-add    as logic                no-undo.
DEF VAR liType      AS INT                  NO-UNDO.
DEF VAR lcValue     AS CHAR                 NO-UNDO.

form
    FATGroup.FTGrp
    FATGroup.FTGName  format "x(30)"
    FATGroup.FatType  
    FATGroup.Priority
    FATGroup.Amount   FORMAT "->>>>9.99" COLUMN-LABEL "Disc.Amt"
    FATGroup.FatPerc  COLUMN-LABEL "Disc.%"
    FATGroup.PeriodQty COLUMN-LABEL "Per."
    with scroll 1 11 down  row 4 centered color value(cfc)
    title color value(ctc) " FAT Group " overlay frame sel.

form /* SEEK Code */
    ob-code
    help "Enter code of an Fatime Group"
    with row 4  col 2 title color value(ctc) " FIND GROUP "
    color value(cfc) no-labels overlay frame hayr.

cfc = "sel". RUN Syst/ufcolor. assign ccc = cfc.

liType  = ?.
       
/* filters selected */
IF gcHelpParam > "" THEN DO i = 1 TO NUM-ENTRIES(gcHelpParam,";"):

   lcValue = ENTRY(i,gcHelpParam,";").
   
   IF lcValue BEGINS "type" THEN 
      liType = INTEGER(SUBSTRING(lcValue,5)) NO-ERROR.
END.

gcHelpParam = "".

MAIN:
repeat:

   RUN local-find-first.
    
   if not available FATGroup then do:
      MESSAGE "No FATIME groups are available"
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   end.
   else do:
      memory = recid(FATGroup).
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
            find FATGroup where recid(FATGroup) = memory no-lock no-error.

            /* Print to screen */
            rtab = ?.
            do while frame-line<= frame-down and available FATGroup:
            
               RUN local-disp-row.
           
               rtab[frame-line] = recid(FATGroup).
               down with frame sel.

               RUN local-find-next.
            end.
            must-print = false.
            up frame-line(sel) - 1 with frame sel.
         end. /* must-print */

         if ufkey then do:
            assign
            ufk = 0 ufk[1] = 35 ufk[5] = 11
            ufk[6] = 0 ufk[8] = 8  ufk[9] = 1
            siirto = ? ehto = 3 ufkey = false.
            RUN Syst/ufkey.
         end.
      end. /* print-line */

      BROWSE:
      repeat with frame sel on endkey undo, retuRN:

         hide message no-pause.
         choose row FATGroup.FTGrp {Syst/uchoose.i} no-error with frame sel.
         color display value(ccc) FATGroup.FTGrp with frame sel.

         if frame-value = "" and rtab[frame-line] = ? then next.
         nap = keylabel(lastkey).

         /* previous line */
         if lookup(nap,"cursor-up") > 0 then do
         with frame sel:
            if frame-line = 1 then do:
               find FATGroup where recid(FATGroup) = rtab[frame-line] no-lock.
               
               RUN local-find-prev.
               if not available FATGroup then do:
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
                  rtab[frame-line] = recid(FATGroup).
                  memory = recid(FATGroup).
               end.
            end.
            else up 1.
         end. /* previous line */

         /* next line */
         if lookup(nap,"cursor-down") > 0 then do with frame sel:
            if frame-line = frame-down then do:
               find FATGroup where recid(FATGroup) = rtab[frame-line] no-lock .
               
               RUN local-find-next.
               if not available FATGroup then do:
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
                  rtab[frame-line] = recid(FATGroup).
                  /* finally last line's KeyValue is saved */
                  memory = rtab[1].
               end.
            end.
            else down 1 .
         end. /* next line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 then do with frame sel:
            find FATGroup where recid(FATGroup) = memory no-lock no-error.

            RUN local-find-prev.
            
            if available FATGroup then do:

               do i = 1 to (frame-down - 1):
                  
                  RUN local-find-prev.
                  if available FATGroup then memory = recid(FATGroup).
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
        if lookup(nap,"1,f1") > 0 then do:  /* ob-code */
           cfc = "puyr". RUN Syst/ufcolor.
           ehto = 9. RUN Syst/ufkey. ufkey = true.
           update ob-code with frame hayr.
           hide frame hayr no-pause.

           if ob-code ENTERED then do:
              IF liType NE ? THEN
                 FIND FIRST FATGroup USE-INDEX FATType WHERE
                            FATGroup.Brand   = gcBrand AND 
                            FATGroup.FATType = liType  AND
                            FATGroup.FTGrp  >= ob-code NO-LOCK NO-ERROR.

              ELSE 
                 FIND FIRST FATGroup USE-INDEX FtGrp WHERE
                            FATGroup.Brand   = gcBrand AND 
                            FATGroup.FTGrp  >= ob-code NO-LOCK NO-ERROR.

              if not available FATGroup then do:
                 bell.
                 message "None found !".    
                 pause 1 no-message.
                 next BROWSE.
              end.
             
              assign
                memory = recid(FATGroup)
                must-print = true.
           end.
           
           next LOOP.

        end. /* Seek */

        /* Choose */
        else if lookup(nap,"return,enter,5,f5") > 0 then do:
           find FATGroup where recid(FATGroup) = rtab[frame-line] no-lock.
           siirto = string(FATGroup.FTGrp).
           leave MAIN.
        end. /* Choose */

        /* First record */
        else if lookup(nap,"home,h") > 0 then do:
           RUN local-find-first.
           memory = recid(FATGroup).
           must-print = true.
           next LOOP.
        end. /* First record */

        /* last record */
        else if lookup(nap,"end,e") > 0 then do :
           RUN local-find-last.
           memory = recid(FATGroup).
           must-print = true.
           next LOOP.
        end. /* last record */

        else if nap = "8" or nap = "f8" then leave MAIN. /* Return */

     end.  /* BROWSE */
   end.  /* LOOP */
end. /* MAIN */

hide frame sel no-pause.

PROCEDURE local-disp-row:

   DISPLAY FATGroup.FTGrp
           FATGroup.FTGName
           FATGroup.FATType
           FATGroup.Priority
           FATGroup.Amount
           FATGroup.FatPerc
           FATGroup.PeriodQty
   WITH FRAME sel.
 
END PROCEDURE.

PROCEDURE local-find-first:

   IF liType = ? THEN DO:
      FIND FIRST FATGroup USE-INDEX FTGrp WHERE 
                 FATGroup.Brand = gcBrand NO-LOCK NO-ERROR.
   END.
                 
   ELSE DO:
      FIND FIRST FATGroup USE-INDEX FATType WHERE 
                 FATGroup.Brand   = gcBrand AND
                 FATGroup.FatType = liType NO-LOCK NO-ERROR.
   END.
   
END PROCEDURE.

PROCEDURE local-find-last:

   IF liType = ? THEN DO:
      FIND LAST FATGroup USE-INDEX FTGrp WHERE 
                FATGroup.Brand = gcBrand NO-LOCK NO-ERROR.
   END.
                 
   ELSE DO:
      FIND LAST FATGroup USE-INDEX FATType WHERE 
                FATGroup.Brand   = gcBrand AND
                FATGroup.FatType = liType NO-LOCK NO-ERROR.
   END.
   
END PROCEDURE.

PROCEDURE local-find-prev:

   IF liType = ? THEN DO:
      FIND PREV FATGroup USE-INDEX FTGrp WHERE 
                FATGroup.Brand = gcBrand NO-LOCK NO-ERROR.
   END.
                 
   ELSE DO:
      FIND PREV FATGroup USE-INDEX FATType WHERE 
                FATGroup.Brand   = gcBrand AND
                FATGroup.FatType = liType NO-LOCK NO-ERROR.
   END.
   
END PROCEDURE.

PROCEDURE local-find-next:

   IF liType = ? THEN DO:
      FIND NEXT FATGroup USE-INDEX FTGrp WHERE 
                FATGroup.Brand = gcBrand NO-LOCK NO-ERROR.
   END.
                 
   ELSE DO:
      FIND NEXT FATGroup USE-INDEX FATType WHERE 
                FATGroup.Brand   = gcBrand AND
                FATGroup.FatType = liType NO-LOCK NO-ERROR.
   END.
   
END PROCEDURE.


