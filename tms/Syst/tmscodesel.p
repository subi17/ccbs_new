/*------------------------------------------------------
  Module .......: tmscodesel.p
  Parent .......: 
  FUNCTION .....: Advanced select list for TMSCodes
  APPLICATION ..: TMS 
  AUTHOR .......: as 
  CREATED ......: 04/2008 
  MODIFIED .....: 
  Version ......: all 
  ------------------------------------------------------ */

{Syst/commali.i}

DEF INPUT PARAM icTableName  AS CHAR  NO-UNDO.
DEF INPUT PARAM icFieldName  AS CHAR  NO-UNDO.
DEF INPUT PARAM icCodeGroup  AS CHAR  NO-UNDO.
DEF INPUT PARAM icHeader     AS CHAR  NO-UNDO.
DEF INPUT PARAM ilInclude    AS LOG   NO-UNDO.
DEF INPUT PARAM icValues     AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocCodeValue AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocCodeName  AS CHAR  NO-UNDO.

IF icHeader = "" THEN icHeader = icFieldName.

def var ob-code     like TMSCodes.CodeValue     no-undo. 
def var rtab        as recid extent 11      no-undo.
def var ufkey       as log init true        no-undo.
def var i           as int                  no-undo.
def var memory      as recid                no-undo.
def var must-print  as logic                no-undo.

form
      TMSCodes.CodeValue
      TMSCodes.CodeName  format "x(40)"
    with scroll 1 11 down  row 4 centered color value(cfc)
    title color value(ctc) icHeader overlay with frame sel.

form /* SEEK Code */
    ob-code
    help "Enter Type "
    with row 4  col 2 title color value(ctc) " FIND CODE "
    color value(cfc) no-labels overlay frame hayr.

cfc = "sel". run ufcolor. assign ccc = cfc.
ocCodeValue = "". 
ocCodeName = "". 

MAIN:
repeat:

   find first TMSCodes WHERE
              TMSCodes.TableName = icTableName AND
              TMSCodes.FieldName = icFieldName AND
              TMSCodes.CodeGroup = icCodeGroup AND
             (IF icValues = "" THEN TRUE ELSE        
              IF ilInclude THEN LOOKUP(TMSCodes.CodeValue,icValues) > 0
              ELSE LOOKUP(TMSCodes.CodeValue,icValues) = 0 )
              NO-LOCK NO-ERROR.
   if not available TMSCodes then do:
      must-print = false.
   end.
   else do:
      memory = recid(TMSCodes).
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
         find TMSCodes where recid(TMSCodes) = memory no-lock no-error.

         /* Print to screen */
         rtab = ?.
         do while frame-line<= frame-down and available TMSCodes:
            display
            TMSCodes.CodeValue
            TMSCodes.CodeName
            with frame sel.
            rtab[frame-line] = recid(TMSCodes).
            down with frame sel.
            find next TMSCodes    WHERE
                      TMSCodes.TableName = icTableName AND
                      TMSCodes.FieldName = icFieldName AND
                      TMSCodes.CodeGroup = icCodeGroup AND
                     (IF icValues = "" THEN TRUE ELSE        
                      IF ilInclude THEN LOOKUP(TMSCodes.CodeValue,icValues) > 0
                      ELSE LOOKUP(TMSCodes.CodeValue,icValues) = 0 )
            no-lock no-error.
         end.
         must-print = false.
         up frame-line(sel) - 1 with frame sel.
      end. /* must-print */

      if ufkey then do:
         assign
         ufk = 0 ufk[1] = 35 ufk[5] = 11
         ufk[6] = 0 ufk[8] = 8  ufk[9] = 1
         ehto = 3 ufkey = false.
         run ufkey.
      end.
  end. /* print-line */

BROWSE:
      repeat with frame sel on endkey undo, retuRN:

         hide message no-pause.
         choose row TMSCodes.CodeValue ;(uchoose.i;) no-error with frame sel.
         color display value(ccc) TMSCodes.CodeValue with frame sel.

         nap = keylabel(lastkey).
         if frame-value = "" and rtab[frame-line] = ? and
            lookup(nap,"8,f8") = 0
         then next.

         /* previous line */
         if lookup(nap,"cursor-up") > 0 then do
         with frame sel:
            if frame-line = 1 then do:
               find TMSCodes where recid(TMSCodes) = rtab[frame-line] no-lock.
               find prev TMSCodes WHERE
                         TMSCodes.TableName = icTableName AND
                         TMSCodes.FieldName = icFieldName AND
                         TMSCodes.CodeGroup = icCodeGroup AND 
                     (IF icValues = "" THEN TRUE ELSE        
                      IF ilInclude THEN LOOKUP(TMSCodes.CodeValue,icValues) > 0
                      ELSE LOOKUP(TMSCodes.CodeValue,icValues) = 0 )
               no-lock no-error.
               if not available TMSCodes then do:
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
                  display TMSCodes.CodeValue TMSCodes.CodeName.
                  rtab[frame-line] = recid(TMSCodes).
                  memory = recid(TMSCodes).
               end.
            end.
            else up 1.
         end. /* previous line */

         /* next line */
         if lookup(nap,"cursor-down") > 0 then do with frame sel:
            if frame-line = frame-down then do:
               find TMSCodes where recid(TMSCodes) = rtab[frame-line] no-lock .
               find next TMSCodes WHERE
                         TMSCodes.TableName = icTableName AND
                         TMSCodes.FieldName = icFieldName AND
                         TMSCodes.CodeGroup = icCodeGroup AND 
                     (IF icValues = "" THEN TRUE ELSE        
                      IF ilInclude THEN LOOKUP(TMSCodes.CodeValue,icValues) > 0
                      ELSE LOOKUP(TMSCodes.CodeValue,icValues) = 0 )
                no-lock no-error.
               if not available TMSCodes then do:
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
                  display TMSCodes.CodeValue TMSCodes.CodeName.
                  rtab[frame-line] = recid(TMSCodes).
                  /* finally last line's KeyValue is saved */
                  memory = rtab[1].
               end.
            end.
            else down 1 .
         end. /* next line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 then do with frame sel:
            find TMSCodes where recid(TMSCodes) = memory no-lock no-error.
            find prev TMSCodes WHERE
                      TMSCodes.TableName = icTableName AND
                      TMSCodes.FieldName = icFieldName AND
                      TMSCodes.CodeGroup = icCodeGroup AND
                     (IF icValues = "" THEN TRUE ELSE        
                      IF ilInclude THEN LOOKUP(TMSCodes.CodeValue,icValues) > 0
                      ELSE LOOKUP(TMSCodes.CodeValue,icValues) = 0 )
            no-lock no-error.
            if available TMSCodes then do:

               do i = 1 to (frame-down - 1):
                  find prev TMSCodes WHERE
                            TMSCodes.TableName = icTableName AND
                            TMSCodes.FieldName = icFieldName AND
                            TMSCodes.CodeGroup = icCodeGroup AND
                           (IF icValues = "" THEN TRUE
                            ELSE IF ilInclude THEN 
                              LOOKUP(TMSCodes.CodeValue,icValues) > 0 
                            ELSE LOOKUP(TMSCodes.CodeValue,icValues) = 0 )
                  no-lock no-error.
                  if available TMSCodes then memory = recid(TMSCodes).
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
           cfc = "puyr". run ufcolor.
           ehto = 9. run ufkey. ufkey = true.
           update ob-code with frame hayr.
           hide frame hayr no-pause.
           if ob-code ENTERED then do:
              find first TMSCodes where 
                         TMSCodes.CodeValue >= ob-code AND
                         TMSCodes.TableName = icTableName AND
                         TMSCodes.FieldName = icFieldName AND
                         TMSCodes.CodeGroup = icCodeGroup AND 
                     (IF icValues = "" THEN TRUE ELSE        
                      IF ilInclude THEN LOOKUP(TMSCodes.CodeValue,icValues) > 0
                      ELSE LOOKUP(TMSCodes.CodeValue,icValues) = 0 )
              NO-LOCK NO-ERROR.           
              if not available TMSCodes then do:
                       bell.
                       message "None found !".    
                       pause 1 no-message.
                       next BROWSE.
              end.
              /*  ob-code was found */
              assign
                memory = recid(TMSCodes)
                must-print = true.
           end.
           next LOOP.
        end. /* Seek */

        /* Choose */
        else if lookup(nap,"return,enter,5,f5") > 0 then do:
           find TMSCodes where recid(TMSCodes) = rtab[frame-line] no-lock.
           ocCodeValue = TMSCodes.CodeValue.
           ocCodeName  = TMSCodes.CodeName.
           leave MAIN.
        end. /* Choose */
        
        /* First record */
        else if lookup(nap,"home,h") > 0 then do:
           find first TMSCodes no-lock WHERE
                     TMSCodes.TableName = icTableName AND
                     TMSCodes.FieldName = icFieldName AND
                     TMSCodes.CodeGroup = icCodeGroup AND 
                     (IF icValues = "" THEN TRUE ELSE        
                      IF ilInclude THEN LOOKUP(TMSCodes.CodeValue,icValues) > 0
                      ELSE LOOKUP(TMSCodes.CodeValue,icValues) = 0 ) NO-ERROR.
           memory = recid(TMSCodes).
           must-print = true.
           next LOOP.
        end. /* First record */

        /* last record */
        else if lookup(nap,"end,e") > 0 then do :
           find last TMSCodes no-lock WHERE
                     TMSCodes.TableName = icTableName AND
                     TMSCodes.FieldName = icFieldName AND
                     TMSCodes.CodeGroup = icCodeGroup AND 
                     (IF icValues = "" THEN TRUE ELSE 
                      IF ilInclude THEN LOOKUP(TMSCodes.CodeValue,icValues) > 0
                      ELSE LOOKUP(TMSCodes.CodeValue,icValues) = 0 ) NO-ERROR.
           memory = recid(TMSCodes).
           must-print = true.
           next LOOP.
        end. /* last record */

        else if nap = "8" or nap = "f8" then leave MAIN. /* Return */

     end.  /* BROWSE */
   end.  /* LOOP */
end. /* MAIN */
hide frame sel no-pause.
