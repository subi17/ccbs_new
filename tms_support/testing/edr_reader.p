/* ----------------------------------------------------------------------
MODULE .......: edr_reader.p
TASK .........: Creates test EDRs 
APPLICATION ..: TMS
AUTHOR .......: anttis 
CREATED ......: 18.2.2013
prepedr
CHANGED ......:
Version ......: Yoigo
----------------------------------------------------------------------- */

{Syst/commpaa.i}
ASSIGN
   katun = "Qvantel"
   gcBrand = "1".

DEFINE VARIABLE lcCLI AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCLIType AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcIMSI AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldadate AS DATE FORMAT "99.99.9999" NO-UNDO.
DEFINE VARIABLE lcTime AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldtDateTime AS DATETIME NO-UNDO.
DEFINE VARIABLE strDate AS CHAR NO-UNDO.
DEFINE VARIABLE strTime AS CHAR NO-UNDO.

DEF VAR lcRootDir AS CHAR NO-UNDO. 
DEF VAR lcCDRDir AS CHAR NO-UNDO. 
DEF VAR lcCDRFile AS CHAR NO-UNDO. 

/*Search correct directory*/
lcRootDir = SEARCH("donotremove_testdir.txt").
lcRootDir = REPLACE(lcrootDir, "donotremove_testdir.txt", "").



FORM
    "MSISDN .....:" lcCli FORMAT "x(10)" lcCLIType SKIP
    "EDR File....:" lcCDRFile FORMAT "x(30)" HELP "F9 - File browser" SKIP
    "Event Day ..:" ldaDate HELP "(empty = default)" SKIP
    "Event Time..:" lcTime HELP "(empty = default)" SKIP
WITH OVERLAY ROW 4 centered
    TITLE " Test EDR Generator "  
    NO-LABELS 
    FRAME lis.

RUN pUserInput.
/* IF  User Wanted TO Cancel this Change TRANSACTION */
IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
KEYLABEL(lastkey) = "F4" THEN UNDO, RETURN.

DEF VAR liHours AS INT NO-UNDO. 
DEF VAR liMins AS INT NO-UNDO. 
DEF VAR ufkey AS LOG NO-UNDO INIT TRUE.

PROCEDURE pUserInput:

   REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:
   
      DISP lcCLI
           lcCDRFile
           ldaDate
           lcTime WITH FRAME lis.

      UPDATE
         lcCli
         lcCDRFile
         ldaDate
         lcTime
      WITH FRAME lis EDITING:
      
         IF ufkey THEN DO:
            ASSIGN ehto = 9. RUN Syst/ufkey.p.
            ufkey = false.
         END.

         READKEY.
         
         nap = keylabel(lastkey).
            
         IF KEYLABEL(LASTKEY) = "F9" AND 
            FRAME-FIELD = "lcCDRFile" THEN DO:
            RUN Syst/filebrowser.p(
               lcRootDir + "edrfiles/*.asc").
            lcCDRFile = RETURN-VALUE.
            DISP lcCDRFile WITH FRAME lis.
            ufkey = true.
            NEXT.
         END.


         IF LOOKUP(nap,poisnap) > 0 THEN DO:

            IF FRAME-FIELD = "lcCli" THEN DO:
               FIND FIRST mobsub WHERE
                          mobsub.cli = INPUT lcCli
               NO-LOCK NO-ERROR.
               IF NOT AVAIL mobsub then do:
                  MESSAGE "Subcription" lcCli "not found"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               end.
               ASSIGN
                  lcCLIType = mobsub.clitype
                  lcImsi = mobsub.imsi.

               DISP lcCLIType WITH frame lis.
            END.

            ELSE IF FRAME-FIELD = "lcCDRFile" THEN DO:
               IF NOT INPUT lcCDRFile > "" THEN NEXT.
               IF SEARCH(lcRootDir + "edrfiles/" +
                         INPUT lcCDRFile) = ? THEN DO:
                  MESSAGE "File does not exists" VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
            END.

            ELSE IF FRAME-FIELD = "lcTime" THEN DO:
               ASSIGN lcTime.
               IF lcTime > "" THEN DO:
                  IF NUM-ENTRIES(lcTime,":") NE 2 THEN DO:
                     MESSAGE "Incorrect time format" VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
                  ASSIGN
                     liHours = INT(ENTRY(1,lcTime,":"))
                     liMins = INT(ENTRY(2,lcTime,":")) NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN DO:
                     MESSAGE "Incorrect time format" VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
                  IF (liHours < 0 OR liHours > 23) OR
                     (liMins < 0 OR liMins > 59) THEN DO:
                     MESSAGE "Incorrect time format" VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.
            END.
         END.

         APPLY LASTKEY.

      END.
      LEAVE.
   END.
END.

lcCDRDir = lcRootDir + "edrfiles/".




ldtDateTime = DATETIME(MONTH(ldaDate), DAY(ldadate), YEAR(ldadate),
    liHours, liMins).



RUN VALUE(lcRootDir + "create_edrfile.p") (
      lcCli,
      ldtDateTime,
      (lcCDRDir + lcCDRFile),
      (lcRootDir + "edrfiles/temp/" + lcCDRFile + ".test")).


RUN /apps/yoigo/tms/Rate/edr_reader.p(
     lcRootDir + "edrfiles/temp/" + lcCDRFile + ".test",
     0).
     
