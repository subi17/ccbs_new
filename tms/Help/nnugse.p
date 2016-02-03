/*------------------------------------------------------
  MODULE .......: NNUGSE.P
  KUTSUVAMODULI : APPLHELP.P
  FUNCTION .....: Search of User Groups
  SOVELLUTUS ...: NN
  CREATED ......: 13.12.98 pt
  changePVM ....:
  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

def shared var siirto as char.

def var UserGroup        like  UserGrp.UserGroup     no-undo.
def var rtab           as re extent 11          no-undo.
def var ufkey          as lo init true          no-undo.
def var i              as i                     no-undo.
def var muisti         as re                    no-undo.
def var must-print     as lo                    no-undo.
def var must-add       as lo                    no-undo.
def var nro            as c  format "x(5)"      no-undo.
def var tyhja          as c  format "x(80)"     no-undo.
def var tlli-ots       as c.

form
    UserGrp.UserGroup
    UserGrp.UGName  format "x(30)"
    UserGrp.memo[1]  format "x(20)"
with scroll 1 11 down  row 4 centered color value(cfc)
    title color value(ctc) " User Groups " overlay frame tlse.

form
    UserGrp.UserGroup skip
    UserGrp.UGName skip
    UserGrp.memo[1]    skip

with overlay row 8 centered
    title color value(ctc) tlli-ots
    color value(cfc) side-labels 1 col
    frame tlli.

form /* Invoicing Group :n hakua varten */
    UserGroup
    help "Enter Code of an User Group"
    with row 4 col 2 title color value(ctc) " FIND CODE "
    color value(cfc) no-labels overlay frame hayr.

cfc = "tlse". run ufcolor. assign ccc = cfc.
Runko:
repeat:

   find first UserGrp no-lock no-error.
   if not available UserGrp then do:
      must-print = false.
      must-add = true.
   end.
   else do:
      muisti = recid(UserGrp).
      must-print = true.
   end.

LOOP:
   Repeat with frame tlse:
   if must-add then do:  /* Invoicing Group  lisays  */
      assign
      cfc = "tlli"
      tlli-ots = " ADD ".
      run ufcolor.
add-new:
      repeat with frame tlli:
         pause 0 no-message.
         clear frame tlli no-pause.
         repeat with frame tlli:
            prompt-for UserGrp.UserGroup.
            if input UserGrp.UserGroup = "" then do:
               hide frame tlli no-pause.
               leave add-new.
            end.
            if can-find (UserGrp where UserGrp.UserGroup =
            input UserGrp.UserGroup) then do:
               bell.
               message "User Group  " + string(input UserGrp.UserGroup)
               + " already exists !".
               next.
            end.
            leave.
         end.
         create UserGrp.
         assign
           muisti = recid(UserGrp)
           UserGrp.UserGroup = input UserGrp.UserGroup.
         update UserGrp.UGName UserGrp.memo[1] .
         clear frame tlli no-pause.
      end.  /* add-new */
      assign
      must-add = false
      must-print = true.

      /* onko yhtaan tietuetta ? */
      find first UserGrp no-lock no-error.
      if not available UserGrp then leave LOOP.
      next LOOP.
   end.

print-line:
   do :
      if must-print then do:
         clear frame tlse all no-pause.
         find UserGrp where recid(UserGrp) = muisti no-lock no-error.

         /* Tulostetaan ruudullinen */
         rtab = ?.
         do while frame-line<= frame-down and available UserGrp:
            display
            UserGrp.UserGroup
            UserGrp.UGName
            UserGrp.memo[1] with frame tlse.
            rtab[frame-line] = recid(UserGrp).
            down with frame tlse.
            find next UserGrp no-lock no-error.
         end.
         must-print = false.
         up frame-line(tlse) - 1 with frame tlse.
      end. /* must-print */

      if ufkey then do:
         assign
         ufk = 0 ufk[1] = 35 ufk[5] = 11
         /* ufk[6] = 5  no new records here ... */
         ufk[8] = 8  ufk[9] = 1 siirto = ? ehto = 3 ufkey = false.
         run ufkey.p.
      end.
  end. /* print-line */

BROWSE:
      repeat with frame tlse on endkey undo, return:

         hide message no-pause.
         choose row UserGrp.UserGroup ;(uchoose.i;) no-error with frame tlse.
         color display value(ccc) UserGrp.UserGroup with frame tlse.

         if frame-value = "" and rtab[frame-line] = ? then next.
         nap = keylabel(lastkey).

         /* previous line */
         if lookup(nap,"cursor-up") > 0 then do
         with frame tlse:
            if frame-line = 1 then do:
               find UserGrp where recid(UserGrp) = rtab[frame-line] no-lock.
               find prev UserGrp no-lock no-error.
               if not available UserGrp then do:
                  bell.
                  message "You are on the 1st row !".
                  pause 1 no-message.
                  next BROWSE.
               end.
               else do:
                  /* a previous one was found */
                  scroll down.
                  do i = 11 to 2 by -1:
                     rtab[i] = rtab[i - 1].
                  end.
                  display UserGrp.UserGroup UserGrp.UGName UserGrp.memo[1].
                  rtab[frame-line] = recid(UserGrp).
                  muisti = recid(UserGrp).
               end.
            end.
            else up 1.
         end. /* previous line */

         /* next line */
         if lookup(nap,"cursor-down") > 0 then do with frame tlse:
            if frame-line = frame-down then do:
               find UserGrp where recid(UserGrp) = rtab[frame-line] no-lock .
               find next UserGrp no-lock no-error.
               if not available UserGrp then do:
                  bell.
                  message "You are on the last row !".
                  pause 1 no-message.
                  next BROWSE.
               end.
               else do:
                  /* yet another record was found */
                  scroll up.
                  do i = 1 to 10:
                     rtab[i] = rtab[i + 1].
                  end.
                  display UserGrp.UserGroup UserGrp.UGName UserGrp.memo[1].
                  rtab[frame-line] = recid(UserGrp).
                  /* finally last line's keyvalue is saved */
                  muisti = rtab[1].
               end.
            end.
            else down 1 .
         end. /* next line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 then do with frame tlse:
            find UserGrp where recid(UserGrp) = muisti no-lock no-error.
            find prev UserGrp no-lock no-error.
            if available UserGrp then do:
               /* mennaan tiedostoa taaksepAin 1 sivun verran */
               do i = 1 to (frame-down - 1):
                  find prev UserGrp no-lock no-error.
                  if available UserGrp then muisti = recid(UserGrp).
                  else i = frame-down.
               end.
               must-print = true.
               next LOOP.
            end.
            else do:
               /* this is the first data page */
               bell.
               message "This is the first page !".
               pause 1 no-message.
            end.
        end. /* previous page */

        /* next page */
        else if lookup(nap,"page-down,next-page") > 0 then do with frame tlse:
           if rtab[frame-down] = ? then do:
               bell.
               message "This is the last page !".
               pause 1 no-message.
           end.
           else do: /* the downmost line wasn't empty */
               muisti = rtab[frame-down].
               must-print = true.
               next LOOP.
           end.
        end. /* next page */

        /* Haku */
        if lookup(nap,"1,f1") > 0 then do:  /* haku */
           cfc = "puyr". run ufcolor.
           UserGroup = "".
           ehto = 9. run ufkey. ufkey = true.
           update UserGroup with frame hayr.
           hide frame hayr no-pause.
           if UserGroup <> "" then do:
              find first UserGrp where UserGrp.UserGroup >= input UserGroup
              no-lock no-error.
              if not available UserGrp then do:
                 message "Not found !".
                 pause 1 no-message.
                 next BROWSE.
              end.
              /*  User Group  was found */
              assign
                muisti = recid(UserGrp)
                must-print = true.
              next LOOP.
           end.
        end. /* Haku */

        /* Valinta */
        else if lookup(nap,"return,enter,5,f5") > 0 then do:
           find UserGrp where recid(UserGrp) = rtab[frame-line] no-lock.
           siirto = string(UserGrp.UserGroup).
           leave runko.
        end. /* Valinta */
        /* Ensimmainen tietue */
        else if lookup(nap,"home,h") > 0 then do:
           find first UserGrp no-lock.
           muisti = recid(UserGrp).
           must-print = true.
           next LOOP.
        end. /* Ensimmainen tietue */

        /* last record */
        else if lookup(nap,"end,e") > 0 then do :
           find last UserGrp no-lock.
           muisti = recid(UserGrp).
           must-print = true.
           next LOOP.
        end. /* last record */

        else if nap = "8" or nap = "f8" then leave runko. /* Paluu */

     end.  /* BROWSE */
   end.  /* LOOP */
end. /* Runko */
hide frame tlse no-pause.
