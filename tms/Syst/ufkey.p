/*------------------------------------------------------
  MODULE .......: UFKEY.P
  KUTSUVAMODULI : PN*.P
  FUNCTION .....: TOIMINTOJEN KYSELYVALINTA
  SOVELLUTUS ...: PERUSNIEKKA
  AUTHOR .......:
  CREATED ......: 26.10.1987
  changePVM ....: 16.11.1987
                  22.04.2002/aam clear functionality from f1,f2,f4 when
                                 buttons are cleared ("ehto = 3")
                  14.06.2002/aam new type 5 for clearing buttons
                  13.08.2002/aam new type 10 
  VERSION ......: M15
  ------------------------------------------------------ */

{commali.i}

def var tyhja as char format "x(80)".

def var ufk_nap as char.
def var ufk_mitka as char.
def var ufk_toimi as char.
def var ufk_nro as int.

assign  ufk_mitka = "1,f1,2,f2,3,f3,4,f4,5,f5,6,f6,7,f7,8,f8,RETURN,ENTER"
        ufk_toimi = "112233445566778899".

form
   sel_t[1 for 4] space(9) sel_t[5 for 4]  skip
   sel_t[9 for 4] space(9) sel_t[13 for 4]
   with no-box no-labels row 20 column 1  frame f_selh.

   color display message sel_t with frame f_selh.


if ehto = 0 or ehto = 1 or ehto = 3 or 
   ehto = 5 or ehto = 9 or ehto = 10
then do:
   sel_t = " ".
   if ehto = 9 or ehto = 10 then do:
      assign  sel_t[01] = "READY/GO"
              sel_t[11] = ""
              sel_t[04] = "CANCEL"
              sel_t[12] = "".
      if ehto = 9 then assign 
              sel_t[03] = "INSERT" 
              sel_t[07] = "PREVIOUS"
              sel_t[15] = "VALUE"
              sel_t[08] = "CLEAR"
              sel_t[16] = "FIELD".
      on F4 endkey.
   end.

   else if ehto ne 5 then do ufk_nro = 1 to 8:
      if ufk[ufk_nro] <> 0 then do:
             find MenuText where 
                  MenuText.MenuNum = ufk[ufk_nro] 
             no-lock no-error.
             if available MenuText then
                assign sel_t [ufk_nro]     = substring(MenuText,1,8)
                       sel_t [ufk_nro + 8] = substring(MenuText,9).
             else
                assign sel_t [ufk_nro]     = string(ufk[ufk_nro]) + " ?"
                       sel_t [ufk_nro + 8] = "".
      end.
   end.

   /* clear all functionality from function keys */
   if ehto = 5 then do:
       on f1 bell.
       on f2 bell.
       on f4 bell. 
   end.

   /* tulostetaan valintatekstit ruudulle */
   pause 0.
   display sel_t with frame f_selh.
end.

if ehto < 3 then do:
   /* kysytAAn toimintovalinnat */
   repeat with frame f_selh:
      readkey.
      apply lastkey.
      assign
         toimi = 0
         ufk_nap = keylabel(lastkey)
         ufk_nro = lookup(ufk_nap,ufk_mitka).

      if ufk_nro <> 0 then
         assign toimi = integer(substring(ufk_toimi,ufk_nro,1)).
         if toimi > 0 and ufk[toimi] = 0 then toimi = 0.

      if toimi = 0 then do:
         bell.
         next.
      end.
      leave.
   end.
end.

/* lopuksi tyhjennetaan alaruudut */
if ehto = 0 or ehto = 4 then do:
   tyhja = fill(" ",80).
   hide message.
   put screen row 20 tyhja.
   put screen row 21 tyhja.
end.
