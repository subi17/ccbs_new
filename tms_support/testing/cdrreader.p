/* ----------------------------------------------------------------------
MODULE .......: cdrreader.p
TASK .........: Creates test CDRs 
APPLICATION ..: TMS
AUTHOR .......: anttis 
CREATED ......: 2.8.2012
CHANGED ......: 29.1.2015 kariaika & ilkkasav
Version ......: Yoigo
----------------------------------------------------------------------- */

{Syst/commpaa.i}
{tms_support/testing/cdrreader.i}
ASSIGN
   katun = "Qvantel"
   gcBrand = "1".

DEFINE VARIABLE lcCLI AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSecCLI AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCLIType AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcIMSI AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldadate AS DATE FORMAT "99.99.9999" INIT TODAY NO-UNDO.
DEFINE VARIABLE tmpdate AS DATE FORMAT "99/99/9999" INIT TODAY NO-UNDO.
DEFINE VARIABLE lcTime AS CHARACTER INIT "00:15" NO-UNDO.
DEFINE VARIABLE ldtDateTime AS DATETIME NO-UNDO.
DEFINE VARIABLE lcMeas AS CHAR NO-UNDO INIT "300".
DEFINE VARIABLE liMeas AS INT NO-UNDO.
DEFINE VARIABLE strDate AS CHAR NO-UNDO.
DEFINE VARIABLE strTime AS CHAR NO-UNDO.
DEFINE VARIABLE lcCount AS CHAR NO-UNDO INIT "1".
DEFINE VARIABLE liCount AS INT NO-UNDO.

DEF VAR lcRootDir AS CHAR NO-UNDO. 
DEF VAR lcCDRDir AS CHAR NO-UNDO. 
DEF VAR lcCDRFile AS CHAR NO-UNDO. 

/*FILE-INFO:FILE-NAME = ".".
lcRootDir = FILE-INFO:FULL-PATHNAME + "/". */
/* lcRootDir = "/apps/xfera/kariaika/yoigo/tms_support/testing/". */

/*Search correct directory*/
lcRootDir = SEARCH("donotremove_testdir.txt").
lcRootDir = REPLACE(lcrootDir, "donotremove_testdir.txt", "").




FORM
    "MSISDN ................:" lcCli FORMAT "x(10)" lcCLIType SKIP
    "2nd PARTY..............:" lcSecCli FORMAT "x(10)" 
                                HELP "(empty = no change)" lcCLIType  SKIP 
    "CDR File...............:" lcCDRFile FORMAT "x(30)" 
                                HELP "F9 - File browser" SKIP
    "Event Day .............:" ldaDate HELP "(empty = default)" SKIP
    "Event Time.............:" lcTime HELP "(empty = default)" SKIP
    "CDR Count..............:" lcCount HELP "(empty = default)" SKIP
    "Duration(s)/Amount(kb).:" lcMeas HELP "(empty = default)" SKIP
WITH OVERLAY ROW 6 centered
    TITLE " Test CDR Generator "  
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
           lcSecCLI
           lcCDRFile
           ldaDate
           lcTime 
           lcCount
           lcMeas WITH FRAME lis.

      UPDATE
         lcCli
         lcSecCLI
         lcCDRFile
         ldaDate
         lcTime
         lcCount
         lcMeas WITH FRAME lis EDITING:
      
         IF ufkey THEN DO:
            ASSIGN ehto = 9. RUN Syst/ufkey.p.
            ufkey = false.
         END.

         READKEY.
         
         nap = keylabel(lastkey).
            
         IF KEYLABEL(LASTKEY) = "F9" AND 
            FRAME-FIELD = "lcCDRFile" THEN DO:
            RUN Syst/filebrowser.p(
               lcRootDir + "cdrfiles/*.asc").
            LCcdrfILE = RETURN-VALUE.
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
                  MESSAGE "Subcription" lcCli "not found" VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               end.
               ASSIGN
                  lcCLIType = mobsub.clitype
                  lcImsi = mobsub.imsi.

               DISP lcCLIType WITH frame lis.
            END.

            IF FRAME-FIELD = "lcSecCli" THEN DO:
               ASSIGN lcSecCli.
            END.
            ELSE IF FRAME-FIELD = "lcCDRFile" THEN DO:
               IF NOT INPUT lcCDRFile > "" THEN NEXT.
               IF SEARCH(lcRootDir + "cdrfiles/" +
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
                     next.
                  END.
               END.
            END.

            ELSE IF FRAME-FIELD = "lcCount" THEN DO:
                ASSIGN lcCount.
                       liCount = INTEGER(lcCount).
                IF (INTEGER(lcCount) < 0 OR INTEGER(lcCount) > 1000) THEN DO:
                   MESSAGE "Count have to be between 0 and 1000." 
                       VIEW-AS ALERT-BOX ERROR.
                   NEXT.
                END.
             END.
             
             ELSE IF FRAME-FIELD = "lcMeas" THEN DO:
               ASSIGN lcMeas.
                      liMeas = INTEGER(lcMeas).
             END.
         END.

         APPLY LASTKEY.

      END.
      LEAVE.
   END.
END.


/*liTimeMs = MTIME(DATETIME*/
ldtDateTime = DATETIME(MONTH(ldaDate), DAY(ldadate), YEAR(ldadate), 
    liHours, liMins).


/*liTimeMs = countMs(liHours, liMins);
ldtDateTime = DATETIME(ldadate, liTimeMs).
*/
lcCDRDir = lcRootDir + "cdrfiles/".
RUN VALUE(lcRootDir + "create_test_cdrfile.p") (
          lcCli,
          lcSecCLI,
          LCiMSI,
          ldtDateTime,
          liMeas,
          (lcCDRDir + lcCDRFile),
          (lcRootDir + "cdrfiles/temp/" + lcCDRFile + ".test"),
          liCount ).
    
RUN /apps/yoigo/tms/Rate/onlinereader.p(
         true,lcRootDir + "cdrfiles/temp/" + lcCDRFile + ".test",
         true,0).
    
