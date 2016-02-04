/*------------------------------------------------------
  Module .......: h-invotxt
  Parent .......: APPLHELP.P
  FUNCTION .....: Help browser of billing Type
  APPLICATION ..: NN
  AUTHOR .......: jp
  CREATED ......: 28-12-99
  MODIFIED .....: 
  Version ......: M15
  ------------------------------------------------------------------- */

{Syst/commali.i}
{Func/func.i}

def shared var siirto as char. 
DEF INPUT PARAMETER ictarget   AS CHAR NO-UNDO.
DEF INPUT PARAMETER icKeyValue AS CHAR NO-UNDO.
DEF INPUT PARAMETER iimsseq  AS INT  NO-UNDO.

def var ob-code     like InvText.TxtTitle     no-undo. 
def var rtab        as recid extent 11      no-undo.
def var ufkey       as log init true        no-undo.
def var i           as int                  no-undo.
def var memory      as recid                no-undo.
def var must-print  as logic                no-undo.
def var must-add    as logic                no-undo.
DEF VAR lcText      AS CHAR                 NO-UNDO.
DEF VAR ok          AS LOG                  NO-UNDO.
DEF VAR lddate      AS DATE                 NO-UNDO.

form
    InvText.TxtTitle     FORMAT "X(25)"
    lctext format "x(48)" COLUMN-LABEL "TEXT" 
    with scroll 1 11 down  row 4  centered color value(cfc)
    title color value(ctc) " Invoice Text " overlay frame sel.

form /* SEEK Code */
    ob-code
    help "Enter Type of an Object Billing Type"
    with row 4  col 2 title color value(ctc) " FIND CODE "
    color value(cfc) no-labels overlay frame hayr.

cfc = "sel". RUN Syst/ufcolor. assign ccc = cfc.
MAIN:
repeat:

   find first InvText no-lock WHERE 
              invText.Brand = gcBrand   AND 
              InvText.Target = icTarget AND 
       (if ickeyvalue = "" THEN TRUE ELSE InvText.Keyvalue = icKeyvalue)  
    no-error.
   if not available InvText then do:
      must-print = false.
      must-add = true.
   end.
   else do:
      memory = recid(InvText).
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
         find InvText where recid(InvText) = memory no-lock no-error.

         /* Print to screen */
         rtab = ?.
         do while frame-line<= frame-down and available InvText:
            display
            InvText.TxtTitle
            invtext.invtext @ lctext
            with frame sel.
            rtab[frame-line] = recid(InvText).
            down with frame sel.
            find next InvText no-lock WHERE 
                      invText.Brand = gcBrand AND 
                      InvText.Target = icTarget AND 
            (if ickeyvalue = "" THEN TRUE ELSE InvText.Keyvalue = icKeyvalue)
            no-error.
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
         choose row InvText.TxtTitle ;(uchoose.i;) no-error with frame sel.
         color display value(ccc) 
         InvText.TxtTitle with frame sel.

         if frame-value = "" and rtab[frame-line] = ? then next.
         nap = keylabel(lastkey).

         /* previous line */
         if lookup(nap,"cursor-up") > 0 then do
         with frame sel:
            if frame-line = 1 then do:
               find InvText where recid(InvText) = rtab[frame-line] no-lock.
               find prev InvText no-lock WHERE 
                         invText.Brand = gcBrand AND 
                         InvText.Target = icTarget AND 
                (if ickeyvalue = "" THEN TRUE 
                 ELSE InvText.Keyvalue = icKeyvalue) no-error.
               if not available InvText then do:
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
                     InvText.TxtTitle 
                     invtext.invtext @ lctext.
                  rtab[frame-line] = recid(InvText).
                  memory = recid(InvText).
               end.
            end.
            else up 1.
         end. /* previous line */

         /* next line */
         if lookup(nap,"cursor-down") > 0 then do with frame sel:
            if frame-line = frame-down then do:
               find InvText where recid(InvText) = rtab[frame-line] no-lock .
               find next InvText no-lock WHERE 
                         invText.Brand = gcBrand AND 
                         InvText.Target = icTarget AND 
                         (if ickeyvalue = "" THEN TRUE 
                         ELSE InvText.Keyvalue = icKeyvalue)  no-error.
               if not available InvText then do:
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
                  display InvText.TxtTitle 
                  invtext.invtext @ lctext 
                  rtab[frame-line] = recid(InvText).
                  /* finally last line's KeyValue is saved */
                  memory = rtab[1].
               end.
            end.
            else down 1 .
         end. /* next line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 then do with frame sel:
            find InvText where recid(InvText) = memory no-lock no-error.
            find prev InvText no-lock WHERE 
                      invText.Brand = gcBrand AND 
                      InvText.Target = icTarget AND 
              (if ickeyvalue = "" THEN TRUE ELSE InvText.Keyvalue = icKeyvalue)             no-error.
            if available InvText then do:

               do i = 1 to (frame-down - 1):
                  find prev InvText no-lock WHERE 
                            invText.Brand = gcBrand AND 
                            InvText.Target = icTarget AND 
             (if ickeyvalue = "" THEN TRUE ELSE InvText.Keyvalue = icKeyvalue) 
             no-error.
                  if available InvText then memory = recid(InvText).
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
              find first InvText where InvText.TxtTitle >= ob-code
             AND  invText.Brand = gcBrand AND 
                  InvText.Target = icTarget AND 
              (if ickeyvalue = "" THEN TRUE ELSE InvText.Keyvalue = icKeyvalue) 
                  no-lock no-error.
               if not available InvText then do:
                       bell.
                       message "None found !".    
                       pause 1 no-message.
                       next BROWSE.
               end.
              /*  ob-code was found */
              assign
                memory = recid(InvText)
                must-print = true.
           end.
           next LOOP.
        end. /* Seek */

        /* Choose */
        else if lookup(nap,"return,enter,5,f5") > 0 then do:
           find InvText where recid(InvText) = rtab[frame-line] no-lock.
            
           FIND FIRST mobsub WHERE
                      mobsub.msseq = iimsseq NO-LOCK NO-ERROR.

           FIND customer WHERE 
                customer.CustNum = mobsub.custnum no-lock no-error.
                
           IF NOT AVAIL customer THEN DO:
               MESSAGE 
               "Unknown/missing Customer number " mobsub.custnum " !"
               VIEW-AS ALERT-BOX .
               LEAVE.
           
           ENd.

            lddate = today.

           fReplaceSMS 
             (INPUT invtext.invtext, iimsseq , lddate, OUTPUT siirto).   

           MESSAGE
           "Approve pre-defined text?" SKIP(1 )
              siirto 
           VIEW-AS ALERt-BOX buttons yes-no update ok.

           if not ok then next.

           leave MAIN.
        end. /* Choose */
        /* First record */
        else if lookup(nap,"home,h") > 0 then do:
           find first InvText no-lock
           WHERE invText.Brand = gcBrand AND InvText.Target = icTarget AND (if ickeyvalue = "" THEN TRUE ELSE InvText.Keyvalue = icKeyvalue) 
               no-error.
           memory = recid(InvText).
           must-print = true.
           next LOOP.
        end. /* First record */

        /* last record */
        else if lookup(nap,"end,e") > 0 then do :
           find last InvText no-lock
           WHERE invText.Brand = gcBrand AND 
                 InvText.Target = icTarget AND 
              (if ickeyvalue = "" THEN TRUE ELSE InvText.Keyvalue = icKeyvalue) 
               no-error.
           memory = recid(InvText).
           must-print = true.
           next LOOP.
        end. /* last record */

        else if nap = "8" or nap = "f8" then leave MAIN. /* Return */

     end.  /* BROWSE */
   end.  /* LOOP */
end. /* MAIN */
hide frame sel no-pause.
