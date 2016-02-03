/* -----------------------------------------------------------------
  MODULE .......: UKALE.P
  FUNCTION .....: CALENDAR
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 02.06.89 pt
  MODIFIED .....: 03.08.98 kl weekdays in english
                  18.05.99 kl thousand YEAR problem fixed
  Version ......: M15
  ----------------------------------------------------------------- */



{Syst/commali.i}

def new shared var uviikko as int format "999999".
DEF NEW shared VAR uvpvm AS Date.

DEF VAR chalku   AS CHAR NO-UNDO.
DEF VAR ekapv AS INT     NO-UNDO.
DEF VAR pp AS INT        NO-UNDO.
DEF VAR kk AS INT        NO-UNDO.
DEF VAR vv AS INT        NO-UNDO.

DEF VAR i AS INT         NO-UNDO.
def var p as char format "xx" EXTENT 37 NO-UNDO.
DEF VAR kuut AS CHAR     NO-UNDO.
DEF VAR kuu AS CHAR      NO-UNDO.
def var vk as char format "XXXXX" EXTENT 6 NO-UNDO.

DEF VAR uf AS logic      NO-UNDO.
DEF VAR chpv AS INT      NO-UNDO.
DEF VAR ipvm AS Date     NO-UNDO.

ASSIGN
kuut="Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec"
uf = TRUE.

form
  "mo tu we th fr sa su   Week" skip(1)
  p[1]  p[2]  p[3]  p[4]  p[5]  p[6]  p[7]  " "  vk[1] SKIP
  p[8]  p[9]  p[10] p[11] p[12] p[13] p[14] " "  vk[2] SKIP
  p[15] p[16] p[17] p[18] p[19] p[20] p[21] " "  vk[3] SKIP
  p[22] p[23] p[24] p[25] p[26] p[27] p[28] " "  vk[4] SKIP
  p[29] p[30] p[31] p[32] p[33] p[34] p[35] " "  vk[5] SKIP
  p[36] p[37] "                "                 vk[6]

  WITH TITLE COLOR value(ctc) kuu
  COLOR value(cfc) ROW 9 col 50
  NO-LABEL OVERLAY FRAME kal.

  ASSIGN
  kk=month(si-pvm) vv=year(si-pvm) pp=day(si-pvm)
  chalku=string(pp)
  cfc = "kal". RUN ufcolor.


LOOP:
repeat:

   /* mikA on kuluvan kuukauden 1. pAivA ? */
   ASSIGN
      pp=weekday(date(kk,1,vv))
      /* jotta sunnuntaista saataisiin viikon 7. pAivA : */
      pp = pp - 1.
      IF pp = 0 THEN pp = 7.
   ASSIGN
      chpv = pp  p = ""
      kuu = " " + entry(kk,kuut) + " " + string(vv) + " "
      ipvm = date(kk,1,vv)
      uvpvm = ipvm.

   /* tAytetAAn p-tauluun ao. mAArA pAiviA */
   ASSIGN i = 1.
   repeat:
     IF month(ipvm) <> kk THEN DO:
        /* choosea varten: ed. kk:n viimeinen pvA */
        if lookup(nap,"1,f1") > 0 THEN chalku = string(day(ipvm - 1)).
        LEAVE.
     END.

     assign p[pp] = string(i,"ZZ")
            pp    = pp + 1
            ipvm  = ipvm + 1
            i     = i + 1.
   END.
   /* lasketaan viikkonumerot */
   DO i = 1 TO 6:
      RUN uviik.
      ASSIGN
      vk[i] = substring(string(uviikko,"999999"),3,2) + "-"
            + substring(string(uviikko,"999999"),5,2)
      uvpvm = uvpvm + 7.
   END.

   DISPLAY p vk WITH FRAME kal.

toimi:
   repeat WITH FRAME kal:
      IF uf THEN DO:
         ASSIGN
            ufk[1]=24 ufk[2]=25 ufk[3]=0 ufk[4]=0
            ufk[5]=11 ufk[6]=0  ufk[7]=0 ufk[8]=8 ehto=3.
         if helpkey <> "f9" THEN ufk[5] = 0.
         RUN ufkey.p.
         uf=false.
      END.
      nap = "".
      CHOOSE FIELD p
      keys chalku
      COLOR value(ctc) no-error.
      nap = keylabel(LASTKEY).

      if nap = "1" or nap = "f1" THEN DO:
         /* edellinen kuukausi */
         kk = kk - 1.
         IF kk = 0 THEN DO:
            kk = 12.
            vv = vv - 1.
         END.
         HIDE FRAME kal no-pause.
         NEXT LOOP.
      END.

      else if nap = "2" or nap = "f2" THEN DO:
         /* seuraava kuukausi */
         kk = kk + 1.
         IF kk = 13 THEN DO:
            kk = 1.
            vv = vv + 1.
         END.
         HIDE FRAME kal no-pause.
         chalku = "1".
         NEXT LOOP.
      END.
      else if lookup(nap,"5,f5,enter,return") > 0  THEN DO:
         if frame-value = "" THEN NEXT.
         pp = integer(frame-value).
         IF vv < 100 THEN DO:
            IF vv > 80 AND vv < 1980 THEN vv = vv + 1900.
            ELSE IF vv > 80 THEN vv = vv + 2000.
         END.
         si-pvm = date(kk,pp,vv).
         HIDE FRAME kal no-pause.
         LEAVE LOOP.
      END.

      else if nap = "8" or nap = "f8" THEN DO:
         si-pvm = ?.
         HIDE FRAME kal no-pause.
         LEAVE LOOP.
      END.

   END. /* toimi */
END. /* LOOP */

RETURN.

