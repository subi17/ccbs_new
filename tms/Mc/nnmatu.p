/* -----------------------------------------------
  MODULE .......: nnmatu.p
  FUNCTION .....: Country information report.
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 17-03-97
  changePVM ....: 15.09.03/aam brand
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/utumaa.i "new"}

assign tuni1 = "nnmatu"
       tuni2 = "".

DEF STREAM tul.
DEF NEW shared STREAM excel.

DEF VAR i            AS i                 NO-UNDO.
DEF VAR rl           AS i                 NO-UNDO.
DEF VAR sl           AS i                 NO-UNDO.
DEF VAR ok           AS lo                NO-UNDO.

def var excel        as lo                no-undo format "Yes/No".
def var exPaymFile       as c  format "x(30)" NO-UNDO.
DEF VAR exheader     AS c                 NO-UNDO.
DEF VAR tab          AS c                 NO-UNDO.

DEF VAR ma-nro1      LIKE CCN.CCN  NO-UNDO.
DEF VAR ma-nro2      LIKE CCN.CCN  NO-UNDO.

DEF VAR lev          AS i                 NO-UNDO init 114.

form header /* tulosteen pAAotsikko */
   fill ("=",lev) format "x(80)"       SKIP
   ynimi at 1 "COUNTRY NUMBERS" at 31 pvm format "99-99-99" TO 78
   "Page"       at 66 sl format "ZZZ9" SKIP
   fill ("=",lev) format "x(80)"       SKIP

   "Nr"           AT 1
   "Country name" AT 5
   "Zone"         AT 37

   fill ("-",lev) format "x(80)" SKIP

WITH
   width 80 NO-LABEL no-box
   FRAME sivuots.

PAUSE 0.

form
    skip(1)
"  Instruction:  This program prints out a of all countries         " skip
"                for consecutive Country numbers determined below.  " skip(1)
"                Report will be sorted by Country number."
skip(4)

"                Country ..........:" ma-nro1  to 39 "-" ma-nro2      skip(1)
"                Excel -file ......:"              excel
                    help "If Yes, a File with excel format is created"
                    AT 37                                             SKIP
"                File Name ........:"              exPaymFile
                    help "Excel File name"
                    AT 37                                             skip(4)
WITH
    COLOR value(cfc) TITLE COLOR value(cfc)
    " " + ynimi + " Country number report " + string(pvm,"99-99-99") + " "
    ROW 1 width 80 NO-LABEL
    FRAME rajat.

ASSIGN
   ma-nro1  =  1    ma-nro2  = 999
   tab      = chr(9)
   exheader = "Cons. nr,Country Name,Zone".

rajat:
repeat WITH FRAME rajat:

   ehto = 9. RUN Syst/ufkey.
   UPDATE
      ma-nro1    ma-nro2
      excel
   WITH FRAME rajat.

   IF excel THEN DO:
      FIND FIRST TMSUser where TMSUser.UserCode = katun no-lock no-error.
      assign exPaymFile = TMSUser.RepDir + "/" + "country.txt".
      UPDATE exPaymFile WITH FRAME rajat.
      OUTPUT STREAM excel TO value(exPaymFile).
   END.

toimi:
   repeat WITH FRAME rajat:
      ASSIGN
      ufk = 0 ehto = 0
      ufk[1] = 7 ufk[5] = 63 ufk[8] = 8.
      RUN Syst/ufkey.
      IF toimi = 1 THEN NEXT  rajat.
      IF toimi = 8 THEN LEAVE rajat.
      IF toimi = 5 THEN  LEAVE toimi.
   END.  /* toimi */

   IF NOT excel THEN DO:
      tila = TRUE.
      {Syst/tmsreport.i "leave rajat"}
   END.

   message "Printing ...".

   FOR EACH  CCN  no-lock  where
             CCN.Brand = gcBrand AND
             CCN.CCN  >= ma-nro1 AND
             CCN.CCN  <= ma-nro2,

       FIRST BDest no-lock  where
             BDest.Brand = gcBrand AND
             BDest.CCN   = CCN.CCN     

      BREAK
         BY CCN.CCN:

      IF NOT excel THEN DO:
         IF sl = 0 THEN DO:
            sl = sl + 1.
            view STREAM tul FRAME sivuots.
            rl = 7. .
         END.

         IF rl >= skayt1 - 1 THEN DO:
            PUT STREAM tul UNFORMATTED skip(spit1 - rl).
            sl = sl + 1.
            view STREAM tul FRAME sivuots.
            rl = 7. .
         END.

         PUT STREAM tul
            CCN.CCN    AT 1
            CCN.CCNName   AT 5
            BDest.BDest   AT 37 SKIP.

         rl = rl + 1.

      END.
      ELSE DO:

         IF sl = 0 THEN DO:
            sl = sl + 1.
            DO i = 1 TO 3:
               PUT STREAM excel UNFORMATTED
                  entry(i,exheader) tab.
            END.
            RUN Syst/uexskip(2).
         END.

         PUT STREAM excel UNFORMATTED
            CCN.CCN    tab
            CCN.CCNName   tab
            BDest.BDest.

         RUN Syst/uexskip(1).

      END.
   END. /* FOR EACH */

   IF NOT excel THEN DO:
      PUT STREAM tul UNFORMATTED skip(spit1 - rl).

      ASSIGN tila = FALSE.
      {Syst/tmsreport.i}
   END.

   LEAVE.
END. /* rajat */
HIDE FRAME rajat.

