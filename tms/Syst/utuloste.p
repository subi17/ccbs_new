/*------------------------------------------------------
  MODULE .......: UTULOSTE.P
  TEHTÄVÄ ......: Tulostuksen ohjaus - avaus & suljenta
  SOVELLUTUS ...: TS
  TEKIJÄ .......: TT
  CREATED ......: 17.06.1991
  changePVM ....: 20.03.92 /tt 03.04.92/tt
                  17.12.93 /tt --> Nollataan tiedostoprint-linejno kun syOtetty
                                   999999
                  22.12.93 /tt --> Oheiskirjoitin sekA DOs:ille ja UNIX:ille
                  12.1.93  /tt --> MyOs varsinaiset tehosteet vaihdetaan F1:llA
                  12.09.94 /tt --> Jos tehostetta ei lOydy, ohitetaan TMSPrinter
                  14.09.94 /tt --> MikAli ohjelmaa kutsutaan tarratulostuksesta
                                   kAtetAAn tarralla maariteltya kirjoitilta
                  18.08.95 /tt --> Otettu HUBER-versio pohjaksi, jossa kORjattu
                                   aloitustehosteen paikka ja oheistulostuksen
                                   kirjoitinnimi, jotta oikea scripti löytyisi.
                                   KysytAAn kuitenkin tiedostonimi.
                  04.05.97 /pt --> ruotsalainen versio
                  19.07.2001/aam   DOn't use jno
                  03.12.2001/pt    select-printer streamlined
                  19.03.2002/aam   "oso" can include the file name,
                                   longer format for "oso"
                  26.04.02 jp/lp  added EMail sending
                  12.06.02 jr     Suggest kaytt.txtdir FOR file direc.
                  16.10.02/aam    suggest user's email if report hasn't got 
                                  any and is changeable,
                                  don't try to send mail if email not selected 
                  23.10.02/jp     possibility change e-mail subject
                  28.03.03/tk     fStr2Unix for email subject
                  09.08.04/aam    add ".txt" to filename in GetFileName
                  23.09.04/aam    suggest user's email if "tmsuser" is defined
                                  on report
                  03.05.05/aam    try to maintain the default effect when 
                                  printer is changed
                  18.08.06/aam    don't prompt user when tuni2 = "direct"
  Version ......: M15
  ------------------------------------------------------ */
{commali.i}
{utumaa.i}
{email.i}
{chkmail.i}

DEF VAR ret          AS i                      NO-UNDO.
DEF NEW SHARED VAR umaara  AS I  NO-UNDO.
def NEW SHARED VAR si-tied AS C FORMAT "x(25)" NO-UNDO.

DEF VAR otsi1    LIKE TMSReport.Memo.
DEF VAR otsi2    LIKE TMSReport.Memo.
DEF VAR kirfyy1  LIKE TMSPrinter.Device.
DEF VAR kirfyy2  LIKE TMSPrinter.Device.
DEF VAR rlev1    LIKE TMSReport.PageWidth.
DEF VAR rlev2    LIKE TMSReport.PageWidth.

DEF VAR kirloo1  AS C  FORMAT "x(20)".
DEF VAR kirloo2  AS C  FORMAT "x(20)".
DEF VAR tehnim1  AS C  FORMAT "x(20)".
DEF VAR tehnim2  AS C  FORMAT "x(20)".
DEF VAR ufkey    AS LO                 NO-UNDO.
DEF VAR i        AS I.
DEF VAR vufk     AS I  EXTENT 9.
DEF VAR vtoimi   AS I.
DEF VAR ok       AS LO FORMAT "Yes/No" NO-UNDO.
DEF VAR osohak   AS C  INIT   "/tmp/"  NO-UNDO.
DEF VAR strnimi1 AS C.
DEF VAR strnimi2 AS C.
DEF VAR cfile AS c NO-UNDO.

DEF VAR lcUserEMail AS CHAR NO-UNDO. 
DEF VAR lcDefEffect AS CHAR NO-UNDO. 

DEF BUFFER xPrintCodes FOR PrintCodes.
DEF BUFFER xTMSPrinter FOR TMSPrinter.

ASSIGN umaara = 0.

FORm
    skip(1)
    "Name of output file:" oso FORMAT "x(50)"
    help "Name FOR output file OR device"           skip

    WITH OVERLAY ROW 6 centered COLOR value(cfc)
    title colOR value(ctc) " DESTINATION " NO-LABELS
    FRAME osoite.

FORM
    SKIP(1)
    "Send E-mail to:" updemail FORMAT "x(75)"
    HELP "Receivers of this report"   SKIP
    "Enter Name for the report:" mailsubj format "x(25)" 

    WITH OVERLAY ROW 6 CENTERED COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " EMail " NO-LABELS
    FRAME EMail.


FORm skip(1)
     "Printer .......:" kirloo1 SKIP
     "Function ......:" tehnim1 SKIP
     "# of lines/page:" spit1  "lines" SKIP
     "- use .........:" skayt1 "lines" SKIP
     WITH TITLE COLOR value(ctc) otsi1 NO-LABELS
     ROW 13 col 2 COLOR value(cfc) OVERLAY
     FRAME kirj1.

FORm skip(1)
     "Kirjoitin ...:" kirloo2 SKIP
     "Tehoste .....:" tehnim2 SKIP
     "Sivun pituus :" spit2 "lineA" SKIP
     "KAytettAvissA:" skayt2 "lineA" SKIP
     WITH TITLE COLOR value(ctc) otsi2 NO-LABELS
     ROW 13 col 43 COLOR value(cfc) OVERLAY
     FRAME kirj2.

FUNCTION GetFileName RETURNS CHAR.

    DEF VAR xfile AS C  NO-UNDO.

    DO i = 1 TO 99:
        IF search(osohak + "nn" + string(i,"99") + ".txt") = ?
        THEN DO:
            ASSIGN xfile = osohak + "nn" + string(i,"99") + ".txt".
            LEAVE.
        END.
    END.
    IF i = 100 THEN ASSIGN xfile = osohak + "nn" + string(time) + ".txt".

    RETURN xfile.

END FUNCTION.

FIND TMSUser NO-LOCK WHERE TMSUser.UserCode = katun NO-ERROR.
IF AVAIL TMSUser THEN DO:
   IF TMSUser.RepDir NE "" THEN osohak = TMSUser.RepDir.
   lcUserEMail = TMSUser.EMail.
END. 

IF tila THEN DO: /* Tila = TRUE; OPEN STREAM AND INITIALISE printer */

   FIND FIRST TMSReport WHERE TMSReport.RepName = tuni1 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE TMSReport THEN
   FIND FIRST TMSReport WHERE TMSReport.RepName = "********" NO-LOCK NO-ERROR.
   FIND FIRST TMSRepCfg WHERE TMSRepCfg.UserCode = katun AND TMSRepCfg.RepName = tuni1
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE TMSRepCfg THEN
      FIND FIRST TMSRepCfg WHERE TMSRepCfg.RepName = tuni1 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE TMSRepCfg THEN
      FIND FIRST TMSRepCfg WHERE TMSRepCfg.RepName = "********" NO-LOCK NO-ERROR.

   ASSIGN umaara = TMSReport.PrintQty.

   FIND TMSPrinter of TMSRepCfg NO-LOCK NO-ERROR.
   IF TMSRepCfg.Effect NE "" THEN DO:
   
      lcDefEffect = TMSRepCfg.Effect.
      
      FIND FIRST PrintCodes WHERE 
                 PrintCodes.PrinterId = TMSPrinter.PrinterId AND
                 PrintCodes.Effect = TMSRepCfg.Effect 
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE PrintCodes THEN DO:
         ASSIGN 
            tehon1  = "" 
            tehoff1 = "" 
            tehnim1 = "No Effect"
            spit1   = 64 
            skayt1  = 61.
      END.
      ELSE ASSIGN 
            tehon1 = PrintCodes.EffOn[2] 
            tehoff1 = PrintCodes.EffOff[2]
            tehnim1 = PrintCodes.EffName
            spit1   = PrintCodes.PageLength
            skayt1  = PrintCodes.AvailLines.
   END.

   ASSIGN 
     TMSPrinter = TMSPrinter.PrinterId.

   IF otsi1 = "" THEN ASSIGN
      otsi1   = " " + TMSReport.Memo + " "
      kirloo1 = TMSPrinter.PrinterId
      kirfyy1 = TMSPrinter.Device.

   rlev1   = TMSReport.PageWidth.


   /* IF there is a secondary printout also, i.e. 2 OUTPUT streams from
      a single program */


   IF tuni2 NE "" AND tuni2 NE "direct" THEN DO:
      FIND FIRST TMSReport WHERE TMSReport.RepName = tuni2 NO-LOCK NO-ERROR.
      IF NOT AVAILABLE TMSReport THEN
      FIND FIRST TMSReport WHERE TMSReport.RepName = "********" NO-LOCK NO-ERROR.
      FIND FIRST TMSRepCfg WHERE TMSRepCfg.UserCode = katun AND TMSRepCfg.RepName = tuni2
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE TMSRepCfg THEN
         FIND FIRST TMSRepCfg WHERE TMSRepCfg.RepName = tuni2 NO-LOCK NO-ERROR.
      IF NOT AVAILABLE TMSRepCfg THEN
         FIND FIRST TMSRepCfg WHERE TMSRepCfg.RepName = "********" NO-LOCK NO-ERROR.

      FIND TMSPrinter of TMSRepCfg NO-LOCK NO-ERROR.
      IF TMSRepCfg.Effect NE "" THEN DO:
         FIND FIRST PrintCodes WHERE PrintCodes.PrinterId = TMSPrinter.PrinterId AND
         PrintCodes.Effect = TMSRepCfg.Effect NO-LOCK NO-ERROR.
         IF NOT AVAILABLE PrintCodes THEN DO:
            ASSIGN tehon2 = "" tehoff2 = "" tehnim2 = "Nej effekt"
            spit1 = 48 skayt1 = 42.
         END.
         ELSE ASSIGN tehon2 = PrintCodes.EffOn[2] tehoff2 = PrintCodes.EffOff[2]
                     tehnim2 = PrintCodes.EffName
                     tehnim2 = PrintCodes.EffName
                     spit2   = PrintCodes.PageLength
                     skayt2  = PrintCodes.AvailLines.
      END.

      ASSIGN
        otsi2   = " " + TMSReport.Memo + " "
        kirloo2 = TMSPrinter.PrinterId
        kirfyy2 = TMSPrinter.Device
        rlev2   = PrintCodes.PageWidth.
   END.

   cfc = "uprinter". RUN ufcolor.
   ufkey = TRUE.

   /* Talletetaan ufk-arvot */
   DO i = 1 TO 9:
       ASSIGN vufk[i] = ufk[i].
   END.
   ASSIGN vtoimi = toimi.

   LOOP:
   REPEAT ON ENDKEY UNDO LOOP, LEAVE LOOP:
   
    IF TMSReport.UpdPerm AND tuni2 NE "direct" THEN DO:
    
      view FRAME kirj1.
      DISPLAY kirloo1 tehnim1 spit1 skayt1 WITH FRAME kirj1.
      IF tuni2 NE "" THEN DO:
         view FRAME kirj2.
         DISPLAY kirloo2 tehnim2 spit2 skayt2 WITH FRAME kirj2.
      END.

      IF ufkey THEN DO:
         ASSIGN
         ufk = 0
         ufk[1] = 151 ufk[2] = 152 ufk[3] = 153 ufk[5] = 63
         ufk[8] = 157 ufk[9] = 0 ehto = 0.

         IF tuni2 NE "" THEN ASSIGN ufk[4] = 154 ufk[6] = 155 ufk[7] = 156.
         ufkey = FALSE.
      END.

      toimi:
      REPEAT:

         ehto = 0. RUN ufkey.
         IF toimi = 1 THEN DO:     /* muutetaan kirjoitinta */

            RUN select-printer(INPUT-OUTPUT kirloo1).
            IF kirloo1 NE TMSPrinter.PrinterId THEN DO:

               FIND TMSPrinter WHERE
               TMSPrinter.PrinterId = kirloo1 NO-LOCK NO-ERROR.

               /* first try to use same effect as the previous printer */
               IF lcDefEffect > "" THEN 
               FIND FIRST PrintCodes of TMSPrinter WHERE 
                          PrintCodes.Effect = lcDefEffect
               NO-LOCK NO-ERROR.
               
               /* look FOR default Effect */
               IF lcDefEffect = "" OR NOT AVAILABLE PrintCodes THEN 
               FIND FIRST PrintCodes of TMSPrinter WHERE 
                          PrintCodes.Effect = "E"
               NO-LOCK NO-ERROR.

               /* IF NOT found, ANY Effect will DO */
               IF NOT AVAILABLE PrintCodes THEN
               FIND FIRST PrintCodes OF TMSPrinter.


               /* store values TO SHARED VARIABLES */
               ASSIGN
               tehon1    = PrintCodes.EffOn[2]
               tehoff1   = PrintCodes.EffOff[2]
               tehnim1   = PrintCodes.EffName
               spit1     = PrintCodes.PageLength
               skayt1    = PrintCodes.AvailLines
               kirloo1   = TMSPrinter.PrinterId
               kirfyy1   = TMSPrinter.Device.

               /* IF a slave printer was used, store the UNIX script name
               into shared 'kirjoitin' VARIABLE */

               TMSPrinter = TMSPrinter.PrinterId.

               DISPLAY 
                  kirloo1 
                  tehnim1 
                  spit1 
                  skayt1 
               WITH FRAME kirj1.
            END. /* IF printer was changed */
            NEXT toimi.
         END.

         ELSE IF toimi = 2 THEN DO:
            FIND FIRST PrintCodes WHERE 
                       PrintCodes.EffName = tehnim1 AND
                       PrintCodes.PrinterId = kirloo1 
            USE-INDEX EffName NO-LOCK NO-ERROR.

            FIND NEXT PrintCodes WHERE 
                      PrintCodes.PrinterId = kirloo1
            USE-INDEX EffName NO-LOCK NO-ERROR.

            IF NOT AVAILABLE PrintCodes THEN
                 FIND FIRST PrintCodes WHERE 
                            PrintCodes.PrinterId = kirloo1
                 USE-INDEX EffName NO-LOCK NO-ERROR.

            ASSIGN
              tehon1      = PrintCodes.EffOn[2]
              tehoff1     = PrintCodes.EffOff[2]
              tehnim1     = PrintCodes.EffName
              spit1       = PrintCodes.PageLength
              skayt1      = PrintCodes.AvailLines
              /* make this the new default for this session */
              lcDefEffect = PrintCodes.Effect.

              DISPLAY 
                 tehnim1 
                 spit1 
                 skayt1 
              WITH FRAME kirj1.
              NEXT toimi.
         END.

         ELSE IF toimi = 3 THEN DO:
            ehto = 9. RUN ufkey.
            UPDATE spit1 skayt1
                      validate(skayt1 <= input spit1, "Value is TOo large !")
            WITH FRAME kirj1.
            ufkey = TRUE.
            NEXT LOOP.
         END.

         ELSE IF toimi = 4 AND tuni2 NE "" THEN DO:
            FIND xTMSPrinter WHERE xTMSPrinter.PrinterId = kirloo2 NO-LOCK NO-ERROR.
            FIND NEXT xTMSPrinter NO-LOCK NO-ERROR.
            IF NOT AVAILABLE xTMSPrinter THEN
               FIND FIRST xTMSPrinter NO-LOCK NO-ERROR.
            ASSIGN
              kirloo2 = TMSPrinter.PrinterId
              kirfyy2 = TMSPrinter.Device.
            FIND FIRST TMSRepCfg of xTMSPrinter NO-LOCK NO-ERROR.
            IF AVAILABLE TMSRepCfg THEN DO:
               FIND FIRST TMSReport WHERE TMSReport.RepName = TMSRepCfg.RepName
               NO-LOCK NO-ERROR.
               ASSIGN
                 otsi2   = TMSReport.Memo.
                 view FRAME kirj2.
            END.
            DISPLAY kirloo2 WITH FRAME kirj2.
            NEXT toimi.
         END.

         ELSE IF toimi = 5 THEN DO:
            str1 = kirfyy1.
            LEAVE toimi.
         END.

         ELSE IF toimi = 6 AND tuni2 NE "" THEN DO:
            FIND FIRST xPrintCodes WHERE 
                       xPrintCodes.EffName = tehnim2 AND
                       xPrintCodes.PrinterId = kirloo2 
            USE-INDEX EffName NO-LOCK NO-ERROR.

            FIND NEXT xPrintCodes WHERE 
                      xPrintCodes.PrinterId = kirloo2
            USE-INDEX EffName NO-LOCK NO-ERROR.

            IF NOT AVAILABLE xPrintCodes THEN
                 FIND FIRST xPrintCodes WHERE PrintCodes.PrinterId = kirloo2
                 USE-INDEX EffName NO-LOCK NO-ERROR.

            ASSIGN
              tehon2  = xPrintCodes.EffOn[2]
              tehoff2 = xPrintCodes.EffOff[2]
              tehnim2 = xPrintCodes.EffName.

              DISPLAY tehnim2 WITH FRAME kirj2.
              NEXT toimi.
         END.

         ELSE IF toimi = 7 AND tuni2 NE "" THEN DO:
            ehto = 9. RUN ufkey.
            UPDATE spit2 skayt2
                      validate(skayt2 <= input spit2, "Value is TOo large!")
            WITH FRAME kirj2.
            ufkey = TRUE.
            NEXT LOOP.
         END.

         ELSE IF toimi = 8 THEN DO:
            str1 = "".
            str2 = "".
            LEAVE LOOP.
         END.

      END. /* toimi */
    END. /* IF UpdPerm */

    ELSE str1 = kirfyy1.

    updemail = "". 

    HIDE FRAME kirj1 no-PAUSE.

      IF kirfyy1 BEGINS "-" THEN DO:
         IF kirfyy1 = "-" THEN DO:
            /* IF file has already been given THEN use it */
            IF oso BEGINS "-" THEN ASSIGN 
                oso2 = SUBSTRING(oso,2).
            ELSE ASSIGN oso2 = GetFileName().

            ASSIGN oso  = oso2.
         END.

         ELSE DO:
            /* there is a pre-defined file name */
            ASSIGN oso2 = substr(kirfyy1,2) oso = oso2. 
         END.   

         ASSIGN oheis = TRUE.
         
         IF tuni2 NE "direct" THEN DO:
            view FRAME osoite.
            UPDATE oso WITH FRAME osoite.
         END. 
         
         IF oso = "" THEN oso = oso2.
         oso2 = oso.

         strnimi1  = substring(kirfyy1,2).
                 
         HIDE FRAME osoite no-PAUSE.
      END.

      ELSE IF kirfyy1 BEGINS "email" THEN DO:

         ASSIGN
         updemail = TMSReport.EMail
         oheis    = TRUE
         oso      = GetFileName()
         mailsubj = TMSReport.memo.

         /* if address has not been defined and is changeable OR
               address is "tmsuser" 
            then suggest user's address */
         IF updemail = "" AND TMSReport.ChEMail = TRUE
         THEN updemail = lcUserEMail.
         
         updemail = replace(updemail,"tmsuser",lcUserEMail).

         IF TMSReport.ChEMail = TRUE AND tuni2 NE "direct"
         THEN DO:
      ABC: REPEAT WITH FRAME EMail:    
            DISP TMSReport.EMail @ updemail mailsubj.
            UPDATE updemail mailsubj WITH FRAME EMail.
            IF updemail = "" THEN DO:
                ASSIGN str1 = "" str2 = "".
                LEAVE.
            END.    
            ELSE DO:
               fIsEmail(INPUT updemail, OUTPUT ret).
               IF ret NE 1 THEN DO:                      
                  BELL.
                  MESSAGE "Invalid E-mail address!".
                  NEXT ABC.
               END. 
               ELSE LEAVE ABC.
            END.
           END.
         END.

         ELSE IF tuni2 NE "direct" THEN DO:
            DISPLAY updemail WITH FRAME EMail.

            UPDATE mailsubj with frame email.

            IF mailsubj = "" then mailsubj = tmsreport.memo.

            IF updemail = "" THEN DO:
               MESSAGE 
               "E-Mail address is missing!" SKIP
               "Please define E-mail configurations " SKIP
               "to printout parameter." 
               VIEW-AS ALERT-BOX TITLE "PRINTING CANCELLED".

               ASSIGN str1 = "" str2 = "".
               LEAVE.
            END.
         END.

         strnimi1  = substring(kirfyy1,2).
         HIDE FRAME EMail no-PAUSE.
      END.

      ELSE oheis = FALSE.


      IF tuni2 NE "direct" THEN DO:
         ufk = 0. ehto = 3. RUN ufkey.
      END.
      
      /* Avataan streamit ja tulostetaan aloitustehosteet */

      IF opsys = "UNIX" THEN DO:

         IF NOT oheis THEN
         OUTPUT STREAM tul THROUGH value(kirfyy1) page-size 0.
         /* Jos tulostettaisiin tiedostoon UNIXissa */   
         ELSE OUTPUT STREAM tul TO value(oso) page-size 0.
      END.
      ELSE DO:  /* DOS */
         IF NOT oheis THEN
         OUTPUT STREAM tul TO value(kirfyy1) page-size 0.
         /* Jos tulostettaisiin tiedostoon DOsissa */
         ELSE OUTPUT STREAM tul TO value(oso) page-size 0.
      END.

      PUT STREAM tul CONTROL tehon1. /* Aloitustehoste */
      
      IF tuni2 <> "" AND tuni2 NE "direct" THEN DO:
         ASSIGN strnimi2  = "".
         IF kirfyy2 BEGINS "-" OR kirfyy2 BEGINS "email" THEN DO:

            ASSIGN oso2 = GetFileName()
                   oso  = oso2. 
            view FRAME osoite.
            UPDATE oso WITH FRAME osoite.
            ASSIGN oheis2 = TRUE.
            IF oso = "" THEN oso = oso2.
            oso2 = oso.
            strnimi2  = substring(kirfyy2,2).
            HIDE FRAME osoite no-PAUSE.
         END.
         ELSE oheis2 = FALSE.

         /* Avataan streamit ja tulostetaan aloitustehosteet */
         IF opsys = "UNIX" THEN DO:
            IF NOT oheis2 THEN
            OUTPUT STREAM tul THROUGH value(kirfyy2) page-size 0.
            /* Jos tulostettaisiin tiedostoon UNIXissa */
            ELSE OUTPUT STREAM tul TO value(oso) page-size 0.
         END.
         ELSE DO:
            IF NOT oheis2 THEN
            OUTPUT STREAM tul TO value(kirfyy2) page-size 0.
            /* Jos tulostettaisiin tiedostoon DOsissa */
            ELSE OUTPUT STREAM tul TO value(oso) page-size 0.
         END.
         PUT STREAM tul2 CONTROL tehon2. /* Aloitustehoste2 */
      END.

      LEAVE LOOP.
   END. /* LOOP */

   /* Palautetaan ufk-arvot */
   DO i = 1 TO 9:
       ASSIGN ufk[i] = vufk[i].
   END.
   ASSIGN toimi = vtoimi.

   IF tuni2 NE "" AND tuni2 NE "direct" THEN HIDE FRAME kirj2 no-PAUSE.

END. /* Tila = avaa */

ELSE DO: /* Tila = sulje */

   PUT STREAM tul CONTROL tehoff1. /* Lopetustehoste */
   OUTPUT STREAM tul CLOSE.

   /* SET ALL E-mail VALUES FOR VARIABLES */
   IF updemail NE "" THEN DO:
      ASSIGN
      xMailAddr   = updemail   
      xMailSubj   = fStr2Unix(mailsubj)  /* replace(mailsubj," ","_") */
      xMailAttach = TRIM(oso).

      DEF STREAM report.
      cfile  = GetFileName().
      OUTPUT STREAM report TO VALUE(cfile).
      PUT STREAM report UNFORMATTED 
         "Report : " mailsubj SKIP
         "Sender : " katun SKIP
         "Day ...: " STRING(pvm,"99.99.9999") SKIP.
       OUTPUT STREAM report CLOSE.

       SendMail(TRIM(cfile),xmailattach).
   END.

   PAUSE 0.

   IF tuni2 NE "" AND tuni2 NE "direct" THEN DO:
      PUT STREAM tul2 CONTROL tehoff2. /* Lopetustehoste */
      OUTPUT STREAM tul2 CLOSE.
   END.

   /* pitikO tulostaa oheiskirjoittimelle ? */
   IF oheis THEN DO:
      IF opsys = "UNIX" AND 
         strnimi1 NE "" AND 
         strnimi1 NE ? 
      THEN DO:

         /* utuloste talletti oheiskirjoittimen kirloon:n kirjoitin-
         muuttujaan avauksen yhteydessa.  haetaan nyt sama TMSPrinter
         takaisin ja otetaan talteen scriptin nimi, jolla tulostetaan
         tilapaistiedosto, jossa teksti sijaitsee */

         FIND TMSPrinter WHERE TMSPrinter.PrinterId = TMSPrinter NO-LOCK.
         strnimi1 = substring(TMSPrinter.Device,2).

         UNIX SILENT value(strnimi1) value(oso).
         oso = "".
         PAUSE 0 BEFORE-hide.
      END.
      ELSE IF opsys NE "unix" THEN DO:
         PAUSE 0 no-MESSAGE.

         DOS list value(oso).
         PAUSE 0 no-MESSAGE.
         ok = FALSE.
         MESSAGE "Shall this file (" + oso + ") be deleted Y/N ?"
         UPDATE ok.

         IF ok THEN DO:
            DOS SILENT del value(oso).
            oso = "".
         END.
         PAUSE 0 beFORe-hide.
      END.
   END.

END. /* Tila = sulje */

RETURN.

PROCEDURE select-printer:

   DEF INPUT-OUTPUT PARAMETER xname AS C  NO-UNDO.

   DEF VAR i AS I  NO-UNDO.
   DEF VAR lpname  LIKE TMSPrinter.PrinterId EXTENT 100 NO-UNDO.

   DEF BUFFER xprinter FOR TMSPrinter.

   FORM

      lpname[ 1 FOR 100] SKIP

   WITH
      OVERLAY TITLE " SELECT PRINTER "  ROW 2 COL 50 1 col 
      NO-LABEL FRAME psel.

   PAUSE 0.

   i = 0.
   FOR
   EACH xprinter WHERE CAN-FIND(FIRST PrintCodes OF xprinter) NO-LOCK:   

      IF i = 100 THEN DO:
         MESSAGE "Only 100 printers can be displeyed for selection"
         VIEW-AS ALERT-BOX WARNING.
         LEAVE.
      END.

      i = i + 1.
      lpname[i] = xprinter.PrinterId.
   END.

   ehto = 4. RUN ufkey.
   CLEAR FRAME psel.
   VIEW  FRAME psel.
   DISP  lpname WITH FRAME psel.

   MESSAGE "SELECT PRINTER AND PRESS ENTER !".

PSEL:  
   REPEAT WITH FRAME psel:
       CHOOSE FIELD lpname KEYS xname.
       xname = FRAME-VALUE.
       IF xname = "" THEN DO:
          BELL.
          NEXT.
       END.
       ELSE LEAVE.
   END.
   HIDE FRAME psel NO-PAUSE.
   PAUSE 0.

END PROCEDURE.


