/*---------------------------------------------------------
  Module .......: h-FeeModel.p 
  Parent .......: APPLHELP.P
  FUNCTION .....: Help browser of Billing Events       
  APPLICATION ..: NN
  AUTHOR .......: pt
  CREATED ......: 07.10.99 pt
  MODIFIED .....: jp 09.09.03  brand
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

def  shared var siirto as char.

def var FeeModel        like FeeModel.FeeModel     no-undo. 
def var rtab        as recid extent 11      no-undo.
def var ufkey       as log init true        no-undo.
def var i           as int                  no-undo.
def var memory      as recid                no-undo.
def var must-print  as logic                no-undo.
def var must-add    as logic                no-undo.

form
      FeeModel.FeeModel FORMAT "X(16)"
      FeeModel.FeeName  format "x(50)"
    with scroll 1 11 down  row 4 centered color value(Syst.Var:cfc) overlay
    title color value(Syst.Var:ctc) " Billing Events " FRAME sel.                

form /* SEEK Code */
    FeeModel
    help "Enter Code of a Billing Event "        
    with row 4 col 2 title color value(Syst.Var:ctc) " FIND CODE "
    color value(Syst.Var:cfc) no-labels overlay frame hayr.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. assign Syst.Var:ccc = Syst.Var:cfc.
MAIN:
repeat:

   find first FeeModel WHERE FeeModel.Brand = Syst.Var:gcBrand 
   no-lock no-error.
   if not available FeeModel then do:
      must-print = false.
      must-add = true.
   end.
   else do:
      memory = recid(FeeModel).
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
         find FeeModel where recid(FeeModel) = memory no-lock no-error.

         /* Print to screen */
         rtab = ?.
         do while frame-line<= frame-down and available FeeModel:
            display
            FeeModel.FeeModel
            FeeModel.FeeName
            with frame sel.
            rtab[frame-line] = recid(FeeModel).
            down with frame sel.
            find next FeeModel WHERE FeeModel.Brand = Syst.Var:gcBrand
            no-lock no-error.
         end.
         must-print = false.
         up frame-line(sel) - 1 with frame sel.
      end. /* must-print */

      if ufkey then do:
         assign
         Syst.Var:ufk = 0 Syst.Var:ufk[1] = 35 Syst.Var:ufk[5] = 11
         Syst.Var:ufk[6] = 0 Syst.Var:ufk[8] = 8  Syst.Var:ufk[9] = 1
         siirto = ? Syst.Var:ehto = 3 ufkey = false.
         RUN Syst/ufkey.p.
      end.
  end. /* print-line */

BROWSE:
      repeat with frame sel on endkey undo, retuRN:

         hide message no-pause.
         choose row FeeModel.FeeModel {Syst/uchoose.i} no-error with frame sel.
         color display value(Syst.Var:ccc) FeeModel.FeeModel with frame sel.

         if frame-value = "" and rtab[frame-line] = ? then next.
         Syst.Var:nap = keylabel(lastkey).

         /* previous line */
         if lookup(Syst.Var:nap,"cursor-up") > 0 then do
         with frame sel:
            if frame-line = 1 then do:
               find FeeModel where recid(FeeModel) = rtab[frame-line] no-lock.
               find prev FeeModel WHERE FeeModel.Brand = Syst.Var:gcBrand
               no-lock no-error.
               if not available FeeModel then do:
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
                  display FeeModel.FeeModel FeeModel.FeeName.
                  rtab[frame-line] = recid(FeeModel).
                  memory = recid(FeeModel).
               end.
            end.
            else up 1.
         end. /* previous line */

         /* next line */
         if lookup(Syst.Var:nap,"cursor-down") > 0 then do with frame sel:
            if frame-line = frame-down then do:
               find FeeModel where recid(FeeModel) = rtab[frame-line] no-lock .
               find next FeeModel WHERE FeeModel.Brand = Syst.Var:gcBrand
               no-lock no-error.
               if not available FeeModel then do:
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
                  display FeeModel.FeeModel FeeModel.FeeName.
                  rtab[frame-line] = recid(FeeModel).
                  /* finally last line's KeyValue is saved */
                  memory = rtab[1].
               end.
            end.
            else down 1 .
         end. /* next line */

         /* previous page */
         else if lookup(Syst.Var:nap,"page-up,prev-page") > 0 then do with frame sel:
            find FeeModel where recid(FeeModel) = memory no-lock no-error.
            find prev FeeModel WHERE FeeModel.Brand = Syst.Var:gcBrand
            no-lock no-error.
            if available FeeModel then do:

               do i = 1 to (frame-down - 1):
                  find prev FeeModel WHERE FeeModel.Brand = Syst.Var:gcBrand
                  no-lock no-error.
                  if available FeeModel then memory = recid(FeeModel).
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
        if lookup(Syst.Var:nap,"1,f1") > 0 then do:  /* FeeModel */
           Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
           Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = true.
           update FeeModel with frame hayr.
           hide frame hayr no-pause.
           if FeeModel ENTERED then do:
              find first FeeModel where 
                         FeeModel.Brand     = Syst.Var:gcBrand  AND 
                         FeeModel.FeeModel >= FeeModel
              no-lock no-error.
               if not available FeeModel then do:
                       bell.
                       message "None found !".    
                       pause 1 no-message.
                       next BROWSE.
               end.
              /*  FeeModel was found */
              assign
                memory = recid(FeeModel)
                must-print = true.
           end.
           next LOOP.
        end. /* Seek */

        /* Choose */
        else if lookup(Syst.Var:nap,"return,enter,5,f5") > 0 then do:
           find FeeModel where recid(FeeModel) = rtab[frame-line] no-lock.
           siirto = string(FeeModel.FeeModel).
           leave MAIN.
        end. /* Choose */
        /* First record */
        else if lookup(Syst.Var:nap,"home,h") > 0 then do:
           find first FeeModel no-lock WHERE FeeModel.Brand = Syst.Var:gcBrand.
           memory = recid(FeeModel).
           must-print = true.
           next LOOP.
        end. /* First record */

        /* last record */
        else if lookup(Syst.Var:nap,"end,e") > 0 then do :
           find last FeeModel no-lock WHERE FeeModel.Brand = Syst.Var:gcBrand.
           memory = recid(FeeModel).
           must-print = true.
           next LOOP.
        end. /* last record */

        else if Syst.Var:nap = "8" or Syst.Var:nap = "f8" then leave MAIN. /* Return */

     end.  /* BROWSE */
   end.  /* LOOP */
end. /* MAIN */
hide frame sel no-pause.
