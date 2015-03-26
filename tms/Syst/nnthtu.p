/* ------------------------------------------------------
  MODULE .......: TSTHTU.P
  KUTSUVAMODULI : UTUTEH.P
  FUNCTION .....: Tehosteiden koeprint-line
  SOVELLUTUS ...: TS
  AUTHOR .......: TT
  CREATED ......: 17.06.1991
  changePVM ....: 03.04.92/tt
        25.09.96 /tt --> Ruotsiksi, sovellettu nn:lle
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}

{utumaa.i "new"}

ASSIGN
tila  = TRUE
tuni1 = "nnthtu"
tuni2 = "".

DEF shared VAR si-kirj AS CHAR NO-UNDO.
def var uline as char format "x(69)" NO-UNDO.
DEF VAR yrnimi LIKE Company.CompName.
DEF VAR sl AS INT.
DEF VAR rl AS INT.
DEF VAR pp AS INT.
DEF VAR kk AS INT.
DEF VAR vv AS INT.
def var paivays as char format "x(10)".
DEF VAR i AS INT.

/* Haetaan Company */
FIND FIRST Company no-lock no-error.
IF NOT AVAILABLE Company THEN DO:
   message "Ej grunuppgifter !".
   BELL.
   RETURN.
END.
ELSE yrnimi = caps(CompName).

/* Haetaan 1. PrintCodes */
FIND FIRST PrintCodes where PrintCodes.PrinterId = si-kirj
USE-INDEX EffName no-lock no-error.
IF NOT AVAILABLE PrintCodes THEN DO:
   message "Denna printer har ej effekt !".
   BELL.
   RETURN.
END.
ELSE uline = PrintCodes.EffOn[2] + EffName + PrintCodes.EffOff[2].

/* Muotoillaan pAivAys 'viralliseksi' */
ASSIGN
pp      = day(TODAY)
kk      = month(TODAY)
vv      = year(TODAY)
paivays = string(pp) + "." + string(kk) + "." + string(vv).

/* YlAformi sivulle */
form header
     skip(2)
     yrnimi    at 9 "EFFEKT" AT 45 SKIP
     Address    AT 9 SKIP
     PostOffice    AT 9 SKIP
     paivays   AT 45 skip(3)
     WITH FRAME yla width 78 NO-LABELS no-box.

{tmsreport.i "return"}

message "Utskriv p~åg~år !".
ASSIGN
sl = 0.

/* NAytetAAn otsikkotiedot */
view STREAM tul FRAME yla.
ASSIGN rl = 10.

print-line:
repeat WHILE AVAILABLE PrintCodes ON ENDKEY UNDO print-line, LEAVE print-line:

   put stream tul unformatted uline format "x(69)" AT 9 skip(1).

   /* linelaskurit */

   ASSIGN rl = rl + 2.
   FIND NEXT PrintCodes where PrintCodes.PrinterId = si-kirj
   USE-INDEX EffName no-lock no-error.
   IF AVAILABLE PrintCodes THEN ASSIGN 
      uline = PrintCodes.EffOn[2] + EffName + PrintCodes.EffOff[2].
END. /* print-line */

/* vielA viimeinen sivu kohdalleen */
DO i = rl TO spit1:
    PUT STREAM tul skip(1).
END.

tila = FALSE. {tmsreport.i}

HIDE MESSAGE no-pause.

