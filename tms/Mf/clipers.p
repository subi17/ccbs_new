/* ----------------------------------------------------------------------------
  MODULE .......: clipers.p
  TASK .........: 
  APPLICATION ..: 
  AUTHOR .......: kl
  CREATED ......: 21.10.02
  CHANGED ......: 20.12.02 kl add end of series automatically
                  07.01.03 kl nndirlog commented
                  21.01.03 kl updating improved
                  06.03.03 kl MASTER 1.0 changes
                  12.03.03 tk SerNum and BillTarg added
                  16.04.03 kl INT(xCLIFrom) => DEC(xCLIFrom)
                  13.01.06/aam BTName removed
                  24.01.06/jt DYNAMIC-FUNCTION("fDispCustName"
  Version ......: M15
 --------------------------------------------------------------------------- */

{Func/timestamp.i}
{Syst/commali.i}
{Func/cparam2.i}

DEF TEMP-TABLE ttCLI NO-UNDO LIKE CLI.
DEF BUFFER bufatno FOR CLI.

DEF VAR lDate1    AS DA  format "99-99-99"  NO-UNDO. 
DEF VAR lDate2    AS DA  format "99-99-99"  NO-UNDO.  
DEF VAR lTime1    AS I                      NO-UNDO. 
DEF VAR lTime2    AS I   format "99999999"  NO-UNDO.
DEF VAR lCTime1   AS C                      NO-UNDO. 
DEF VAR lCTime2   AS C                      NO-UNDO. 
DEF VAR lOName    LIKE CLI.Owner            NO-UNDO. 
DEF VAR xCustNum  LIKE Customer.CustNum     NO-UNDO. 
DEF VAR xBillTarg LIKE CLISer.BillTarget    NO-UNDO.
DEF VAR xCLIFrom  LIKE CLISer.CLIFrom       NO-UNDO. 
DEF VAR xCLITo    LIKE CLISer.CLITo         NO-UNDO. 
DEF VAR xmemo     LIKE CLISer.memo          NO-UNDO. 
DEF VAR xSecr     AS LOG                    NO-UNDO. 
DEF VAR nlkm2     AS INT                    NO-UNDO. 
DEF VAR nlkm      AS INT                    NO-UNDO.
DEF VAR lcCustName AS CHAR                  NO-UNDO.

DEF NEW shared VAR siirto AS CHAR.

FORM
   SKIP(1)
   "   Note: Now You can define a new OWNER for this"       SKIP
   "         A-Number (CLI) and update ACTIVE PERIOD"       SKIP(1)
   " A-sub number .. :" CLI.CLI                             SKIP
   " Owner name .... :" lOName
      HELP "Name of new owner (for call specifications etc)" SKIP
   " Active FROM ....:" lDate1 lCTime1 FORMAT "99:99:99"     SKIP
   " Active TO ......:" lDate2 lCTime2 FORMAT "99:99:99"
WITH
   CENTERED NO-LABELS OVERLAY ROW 6 TITLE " MAINTAIN OWNER INFORMATION " 
FRAME frmUpdCLI.

FORM
                                                             SKIP(1)
   " Customer number ..:" CLISer.CustNum  lcCustName         SKIP
   " A-Sub No from ....:" CLISer.CLIFrom                     SKIP
   " A-Sub No to ......:" CLISer.CLITo                       SKIP
   " Active TO ........:" lDate2 lCTime2 FORMAT "99:99:99"   SKIP(1)
WITH OVERLAY ROW 6 CENTERED 
   TITLE " Set time stamp for closing CLI numbers " NO-LABELS
FRAME frmDel.

FORM 
                                                             SKIP(1)
   " Customer number ..:" xCustNum   lcCustName              SKIP
   " Billing target ...:" xBillTarg                          SKIP
   " No. of A-sub nos .:" nlkm  format "zz,zz9" 
      help "How many separate A-sub numbers in this serie ?" SKIP
   " A-Sub No from ....:" xCLIFrom                           SKIP
   " A-Sub No till ....:" xCLITo                             SKIP
   " Active FROM ......:" lDate1 lCTime1 FORMAT "99:99:99"   SKIP
   " Active TO ........:" lDate2 lCTime2 FORMAT "99:99:99"   SKIP
   " Secret ...........:" xSecr       FORMAT "Secr/"           SKIP
   " Memo .............:" xmemo
      help "A Text for Customer Care"                        SKIP(1)
WITH 
   OVERLAY ROW 6 CENTERED COLOR VALUE(cfc)
   TITLE COLOR VALUE(ctc) " ADD CLI SERIES " WITH NO-LABELS
FRAME frmAddSeries.

FORM
   " Secret ...........:" CLISer.Secr
      HELP "Are this serie Secret(Y) or No(N) ?"                       SKIP
   " Memo .............:" CLISer.Memo
       HELP "A Text for customer care only"                             SKIP
WITH
   CENTERED ROW 8 TITLE " UPDATE A-SUB SERIES INFO " 
   OVERLAY WITH NO-LABELS FRAME frmMemo.

PROCEDURE pUpdateCLI:

   DEF INPUT-OUTPUT PARAMETER pRecId AS RECID NO-UNDO.

   DEF VAR lPrevTime AS I  NO-UNDO.
   DEF VAR lPrevDate AS DA NO-UNDO.
   DEF VAR lStamp1   AS C                      NO-UNDO.
   DEF VAR lStamp2   AS C                      NO-UNDO.
   DEF VAR ok        AS LO                     NO-UNDO.
   DEF VAR lClStamp  AS DE                     NO-UNDO.

   FIND FIRST CLI WHERE
        RECID(CLI) = pRecId
   EXCLUSIVE-LOCK NO-ERROR.

   fSplitTS(INPUT CLI.crStamp, OUTPUT lDate1, OUTPUT lTime1).
   fSplitTS(INPUT CLI.clStamp, OUTPUT lDate2, OUTPUT lTime2).

   ASSIGN
      lOName  = CLI.Owner
      lCTime1 = string(lTime1,"hh:mm:ss")
      lCTime2 = string(lTime2,"hh:mm:ss")
      substr(lCTime1,3,1) = ""
      substr(lCTime1,5,1) = ""
      substr(lCTime2,3,1) = ""
      substr(lCTime2,5,1) = "".

   PAUSE 0.
   DISP CLI.CLI WITH FRAME frmUpdCli.

   LOOP:
   REPEAT ON ENDKEY UNDO LOOP,LEAVE LOOP:
      UPDATE  
         lOName
         lDate1 
         lCTime1 
         lDate2 
         lCTime2 
      WITH FRAME frmUpdCli EDITING:
         READKEY.
         IF KEYLABEL(LASTKEY) = "F4" THEN UNDO LOOP,LEAVE LOOP.
         IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:
            IF FRAME-FIELD = "lCTime1" THEN DO:
               IF NOT fCheckTime(INPUT FRAME frmUpdCli lCTime1) THEN DO:
                  MESSAGE 
                     "Invalid time: " + INPUT FRAME frmUpdCli lCTime1 
                  VIEW-AS ALERT-BOX error.
                  NEXT-PROMPT lCTime1.
                  NEXT.
               END.
            END.
            ELSE IF FRAME-FIELD = "lCTime2" THEN DO:
               IF NOT fCheckTime(INPUT FRAME frmUpdCli lCTime2) THEN DO:
                  MESSAGE 
                     "Invalid time: " + INPUT FRAME frmUpdCli lCTime2 
                  VIEW-AS ALERT-BOX error.
                  NEXT-PROMPT lCTime2.
                  NEXT.
               END.
            END.
         END.
         APPLY LASTKEY.
      END.

      IF lOName ENTERED OR lDate1 ENTERED OR lCTime1 ENTERED OR 
                           lDate2 ENTERED OR lCTime2 ENTERED THEN DO:

         ok = FALSE.
         MESSAGE
            "You have updated information for CLI " + CLI.CLI + "!" SKIP
            "Do You want to keep old information as a history record" SKIP
            "and create a new record with updated information ?"      SKIP(1)
            "YES    = Create new record"                              SKIP
            "NO     = Update old record"                              SKIP
            "CANCEL = UNDO             "                              SKIP
         VIEW-AS ALERT-BOX BUTTONS YES-NO-CANCEL UPDATE ok.

         /* add new record */
         IF ok = TRUE THEN DO:

            IF CLI.crStamp = fHMS2TS(lDate1,lCTime1) THEN DO:
               MESSAGE
                  "You can't use same validation period for new record !"
               VIEW-AS ALERT-BOX.
               NEXT LOOP.
            END.

            FIND FIRST bufatno WHERE
                       bufatno.CLI   = CLI.CLI  AND
                       bufatno.crStamp >= CLI.crStamp AND
                       bufatno.crStamp <= CLI.clStamp AND
                 RECID(bufatno) NE RECID(CLI)
            NO-LOCK NO-ERROR.
            IF NOT AVAIL bufatno THEN DO:
               FIND FIRST bufatno WHERE
                          bufatno.CLI   = CLI.CLI  AND
                          bufatno.clStamp >= CLI.crStamp AND
                          bufatno.clStamp <= CLI.clStamp AND
                    RECID(bufatno) NE RECID(CLI)
               NO-LOCK NO-ERROR.
            END.
            IF AVAIL bufatno THEN DO:
               MESSAGE
                  "Overlapping period was found for customer " +
                  STRING(bufatno.CustNum)
               VIEW-AS ALERT-BOX.
               NEXT LOOP.
            END.

            CREATE ttCLI.
            BUFFER-COPY CLI EXCEPT 
               CLI.crStamp
               CLI.clStamp 
               CLI.Owner
            TO ttCLI. 

            ttCLI.Owner = lOName.
            ttCLI.crStamp  = fHMS2TS(lDate1,lCTime1).
            ttCLI.clStamp  = fHMS2TS(lDate2,lCTime2).

            fSplitTS(ttCLI.crStamp, OUTPUT lPrevDate, OUTPUT lPrevTime).

            IF lPrevTime = 0 THEN ASSIGN
               lPrevDate = lPrevDate - 1
               lPrevTime = 86399.
            ELSE ASSIGN 
               lPrevTime = lPrevTime - 1.

            CLI.clStamp = fHMS2TS(lPrevDate,string(lPrevTime,"hh:mm:ss")).

            CREATE bufatno.
            BUFFER-COPY ttCLI TO bufatno.
            EMPTY TEMP-TABLE ttCLI.

            pRecid = RECID(bufatno).

            MESSAGE
               "NOTICE" skip(1)
               "Passivated numbers can be seen in F7 - CLI HISTORY," SKIP
               "Active CLIs are shown in this window."
            VIEW-AS ALERT-BOX.

         END.
         /* update current record */
         ELSE IF ok = FALSE THEN DO:
            lClStamp = fHMS2TS(lDate2,lCTime2).

            FIND FIRST bufatno WHERE
                       bufatno.CLI   = CLI.CLI  AND
                       bufatno.crStamp >= CLI.crStamp AND
                       bufatno.crStamp <= lclStamp AND
                 RECID(bufatno) NE RECID(CLI)
            NO-LOCK NO-ERROR.
            IF NOT AVAIL bufatno THEN DO:
               FIND FIRST bufatno WHERE
                          bufatno.CLI   = CLI.CLI  AND
                          bufatno.clStamp >= CLI.crStamp AND
                          bufatno.clStamp <= lClStamp AND
                    RECID(bufatno) NE RECID(CLI)
               NO-LOCK NO-ERROR.
            END.
            IF AVAIL bufatno THEN DO:
               MESSAGE
                  "Overlapping period was found for customer " +
                  STRING(bufatno.CustNum)
               VIEW-AS ALERT-BOX.
               NEXT LOOP.
            END.


            CLI.Owner = lOName.
            CLI.clStamp  = fHMS2TS(lDate2,lCTime2).
         END.
         /* cancel updating */
         ELSE LEAVE LOOP.
      END.

      LEAVE LOOP.

   END.

   HIDE FRAME frmUpdCli no-pause.

END PROCEDURE.

PROCEDURE pUpdateInfo:

   DEF INPUT PARAMETER pRecId AS RECID.

   FIND FIRST CLISer WHERE
        RECID(CLISer) = pRecId
   NO-LOCK NO-ERROR.

   PAUSE 0.
   UPDATE
      CLISer.Secr
      CLISer.Memo
   WITH FRAME frmMemo.

   CLEAR FRAME frmMemo ALL.
   HIDE  FRAME frmMemo NO-PAUSE.

END PROCEDURE.

PROCEDURE pDelSeries:  

   DEF INPUT PARAMETER pRecId  AS REC NO-UNDO.

   DEF INPUT-OUTPUT PARAMETER delline  AS INT NO-UNDO.

   DEF VAR lOK      AS LO   NO-UNDO.
   DEF VAR rc       AS INT  NO-UNDO.
   DEF VAR seli     AS CHAR NO-UNDO.
   DEF VAR lDel     AS INT  NO-UNDO.
   DEF VAR lCLStamp AS DEC  NO-UNDO.

   MESSAGE  
      "Do You want close CLI numbers OR delete them completely ?" SKIP(1)
      "YES    = CLOSE " SKIP
      "NO     = DELETE" SKIP
      "CANCEL = UNDO   " 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE lOK.  

   CASE lOK: 
      WHEN TRUE  THEN lDel = 3. 
      WHEN FALSE THEN lDel = 4. 
      WHEN ?     THEN lDel = 0.
   END.  

   FIND FIRST CLISer WHERE
        RECID(CLISer) = pRecId
   EXCLUSIVE-LOCK NO-ERROR.

   FIND FIRST Customer WHERE
              Customer.CustNum = CLISer.CustNum
   NO-LOCK NO-ERROR.
   
   lcCustName =  DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                  BUFFER Customer).
   IF lDel = 3 THEN DO: 

      fSplitTS(INPUT fMakeTS(), OUTPUT lDate2, OUTPUT lTime2).   

      ASSIGN   
         lCTime2 = string(lTime2,"hh:mm:ss")   
         substr(lCTime2,3,1) = ""   
         substr(lCTime2,5,1) = "".

      DISP 
         CLISer.CustNum
         lcCustName
         CLISer.CLIFrom 
         CLISer.CLITo 
      WITH FRAME frmDel. 

      UPDATE 
         lDate2 
         lCTime2 
      WITH FRAME frmDel EDITING. 
         READKEY. 
         nap = KEYLABEL(LASTKEY).
         IF nap = "F4" THEN DO:
            lDel = 0.
            LEAVE.
         END.
         IF FRAME-FIELD = "lCTime2" THEN DO:  
            IF NOT fCheckTime(INPUT FRAME frmUpdCLI lCTime2) THEN DO:  
               MESSAGE   
                  "Invalid time: " + INPUT FRAME frmUpdCLI lCTime2   
               VIEW-AS ALERT-BOX error. 
               NEXT-PROMPT lCTime2. 
               NEXT. 
            END. 
         END. 
         APPLY LASTKEY. 
      END.

      lCLStamp = fHMS2TS(lDate2,lCTime2).  

   END. 

   IF lDel = 0 THEN delline = 0. 
   ELSE DO:  

      MESSAGE "Deleting a-sub numbers, wait ...". 

      RUN nnatmu 
        (INPUT  CLISer.SerNum,
         INPUT  CLISer.CLIFrom,  
         INPUT  CLISer.CLITo,  
         INPUT  0,
         INPUT  lCLStamp,  
         INPUT  CLISer.CustNum,
         INPUT  CLISer.BillTarg,
         INPUT  lDel, 
         OUTPUT rc,  
         OUTPUT seli).

      /*
      IF Customer.conn    = TRUE AND 
         Customer.rs-code = ""   AND 
         CLISer.Secr  = FALSE THEN 
         RUN nndirlog(CLISer.CustNum,
                      CLISer.CLIFrom, 
                      CLISer.CLITo,2).
      */

      /* lDel 4 means that also single numbers are deleted */
      IF lDel = 4 THEN DO:

         DELETE CLISer.

      END. 
      ELSE delline = 0.

      MESSAGE seli VIEW-AS ALERT-BOX.

   END.

   CLEAR FRAME frmDel ALL.
   HIDE  FRAME frmDel NO-PAUSE.

END.

PROCEDURE pAddSeries: 

   DEF INPUT        PARAMETER CustNum  AS INT NO-UNDO.
   DEF INPUT        PARAMETER BillTarg AS INT NO-UNDO.
   DEF INPUT-OUTPUT PARAMETER memory   AS REC NO-UNDO.

   DEF VAR nlkm2    AS INT  NO-UNDO.
   DEF VAR rc       AS INT  NO-UNDO.
   DEF VAR seli     AS CHAR NO-UNDO.
   DEF VAR ok       AS LOG  NO-UNDO.
   DEF VAR lCRStamp AS DEC  NO-UNDO.
   DEF VAR lCLStamp AS DEC  NO-UNDO.
   DEF VAR lOK      AS LOG  NO-UNDO INIT TRUE.

   ASSIGN 
      xCLIFrom = "" 
      xCLITo = "" 
      xmemo    = "" 
      nlkm     = 0
      lCRStamp = DEC(fCParamC("DefCRStamp"))
      lCLStamp = DEC(fCParamC("DefCLStamp")).

   fSplitTS(INPUT lCRStamp, OUTPUT lDate1, OUTPUT lTime1). 
   fSplitTS(INPUT lCLStamp, OUTPUT lDate2, OUTPUT lTime2).  

   ASSIGN  
      lCTime1 = string(lTime1,"hh:mm:ss")  
      lCTime2 = string(lTime2,"hh:mm:ss")  
      substr(lCTime1,3,1) = ""  
      substr(lCTime1,5,1) = ""  
      substr(lCTime2,3,1) = ""  
      substr(lCTime2,5,1) = "".   

   ASSIGN
      xCustNum  = CustNum
      xBillTarg = BillTarg.

   IF xCustNum NE 0 THEN DO:
      FIND FIRST Customer WHERE
                 Customer.CustNum = xCustNum
      NO-LOCK NO-ERROR.
      lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                    BUFFER Customer).
      DISP
         xCustNum
         lcCustName
      WITH FRAME frmAddSeries.

      IF xBillTarg NE 0 THEN DO:
         FIND FIRST BillTarget WHERE
                    BillTarget.CustNum    = xCustNum AND
                    BillTarget.BillTarget = xBillTarg
         NO-LOCK NO-ERROR.
         DISP
            xBillTarg
         WITH FRAME frmAddSeries.
      END.

   END.

   LOOP:
   REPEAT:

      nlkm = 1.

      UPDATE 
         xCustNum  WHEN xCustNum  = 0
         xBillTarg WHEN xBillTarg = 0
         nlkm  
         xCLIFrom  
         xCLITo  
         lDate1 
         lCTime1 
         lDate2 
         lCTime2 
         xSecr  
         xmemo  
      WITH FRAME frmAddSeries EDITING:  

         READKEY. NAP = KEYLABEL(LASTKEY). 

         IF NAP = "F4" THEN DO:
            lOK = FALSE.
            UNDO LOOP, LEAVE LOOP.
         END.

         IF nap = "F9" AND FRAME-FIELD = "xBillTarg" THEN DO:

            RUN h-billtarg (INPUT xCustNum).

            IF siirto NE "" AND siirto NE ? THEN
               DISPLAY INTEGER(siirto) @ xBillTarg WITH FRAME frmAddSeries.

            RUN ufkey.

         END.    

         IF LOOKUP(nap,poisnap) > 0 THEN DO:

            IF FRAME-FIELD = "xCustNum" THEN DO:
               IF INPUT FRAME frmAddSeries xCustNum = 0 THEN LEAVE LOOP.
               ELSE DO:
                  ASSIGN xCustNum.
                  FIND FIRST Customer WHERE
                             Customer.CustNum = xCustNum
                  NO-LOCK NO-ERROR.
                                    
                  IF NOT AVAIL Customer THEN DO:
                     MESSAGE
                        "Customer number " STRING(xCustNum) " does not exist !"
                     VIEW-AS ALERT-BOX.
                     NEXT.
                  END.
                  lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                                 BUFFER Customer).

                  
                  DISPLAY lcCustName WITH FRAME frmAddSeries.
               END.

            END.

            IF FRAME-FIELD = "xBillTarg" THEN DO:
               IF INPUT FRAME frmAddSeries xBillTarg = "" THEN DO:
                  MESSAGE
                     "Billing target does not exist !"
                  VIEW-AS ALERT-BOX.
                  NEXT.
               END.
               ELSE DO:
                  ASSIGN xBillTarg.
                  FIND FIRST BillTarget WHERE
                             BillTarget.CustNum    = xCustNum AND
                             BillTarget.BillTarget = xBillTarg
                  NO-LOCK NO-ERROR.
                  IF NOT AVAIL BillTarget THEN DO:
                     MESSAGE
                        "Billing target does not exist !"
                     VIEW-AS ALERT-BOX.
                     NEXT.
                  END.
               END.
            END.

            IF FRAME-FIELD = "nlkm" THEN DO: 
               ASSIGN INPUT FRAME frmAddSeries nlkm. 
               IF nlkm = 0 THEN LEAVE LOOP.
            END.

            IF FRAME-FIELD = "xCLIFrom" THEN DO: 

               ASSIGN INPUT FRAME frmAddSeries xCLIFrom.

               IF xCLIFrom = "" THEN DO: 
                  NEXT-PROMPT nlkm WITH FRAME frmAddSeries. 
                  NEXT LOOP.
               END.  

               IF xCLITo = "" THEN DO: 
                  xCLITo = STRING(DEC(xCLIFrom) + nlkm - 1).
                  nlkm2 = 1.
                  DO WHILE SUBSTR(xCLIFrom,nlkm2,1) = "0":
                     ASSIGN
                        xCLITo = "0" + xCLITo
                        nlkm2    = nlkm2 + 1.
                  END.
                  DISPLAY xCLITo WITH FRAME frmAddSeries.
               END.

            END.

            ELSE IF FRAME-FIELD = "xCLITo" THEN DO: 

               ASSIGN INPUT FRAME frmAddSeries xCLITo. 

               IF xCLITo = "" THEN DO: 
                  NEXT-PROMPT xCLIFrom WITH FRAME frmAddSeries.
                  NEXT LOOP.
               END. 

               IF xCLITo < xCLIFrom THEN DO: 
                  MESSAGE "Invalid order !" VIEW-AS ALERT-BOX.  
                  NEXT-PROMPT xCLIFrom WITH FRAME frmAddSeries.
                  NEXT LOOP.
               END.  

               IF LENGTH(xCLIFrom) NE LENGTH(xCLITo) THEN DO:  
                  MESSAGE 
                     "Both numbers must be of same length !"
                  VIEW-AS ALERT-BOX.
                  NEXT-PROMPT xCLIFrom WITH FRAME frmAddSeries.
                  NEXT LOOP.
               END.
               FIND FIRST CLISer WHERE
                          CLISer.CLIFrom = xCLIFrom AND
                          CLISer.CLITo   = xCLITo   AND
                          CLISer.CustNum = CustNum
               NO-LOCK NO-ERROR.
               IF AVAIL CLISer THEN DO:
                  MESSAGE
                     "Same series exists already for this customer !" SKIP
                     "You have delete it first to create a new one."
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT-PROMPT nlkm WITH FRAME frmAddSeries.
                  NEXT LOOP.
               END.

               nlkm2 = (decimal(xCLITo) - decimal(xCLIFrom) + 1). 

               IF nlkm2 NE nlkm THEN DO: 
                  MESSAGE 
                     "Number" nlkm2 "differs from given number"
                     INPUT nlkm "!"
                  VIEW-AS ALERT-BOX. 
                  NEXT LOOP.
               END. 

            END. /* xCLITo */ 

            IF FRAME-FIELD = "lCTime1" THEN DO: 
               IF NOT fCheckTime(INPUT FRAME frmAddSeries lCTime1) THEN DO: 
                  MESSAGE  
                     "Invalid time: " + INPUT FRAME frmAddSeries lCTime1  
                  VIEW-AS ALERT-BOX error. 
                  NEXT-PROMPT lCTime1 WITH FRAME frmAddSeries.
                  NEXT LOOP.
               END.
            END.    
            ELSE IF FRAME-FIELD = "lCTime2" THEN DO: 
               IF NOT fCheckTime(INPUT FRAME frmAddSeries lCTime2) THEN DO: 
                  MESSAGE  
                     "Invalid time: " + INPUT FRAME frmAddSeries lCTime2  
                  VIEW-AS ALERT-BOX error. 
                  NEXT-PROMPT lCTime2 WITH FRAME frmAddSeries.
                  NEXT LOOP.
               END.

               ASSIGN
                  lCRStamp = fHMS2TS(lDate1,lCTime1)
                  lCLStamp = fHMS2TS(lDate2,lCTime2). 

               RUN nnatmu 
                 (INPUT  0,
                  INPUT  xCLIFrom,  
                  INPUT  xCLITo,  
                  INPUT  lCRStamp, 
                  INPUT  lCLStamp, 
                  INPUT  CustNum, 
                  INPUT  xBillTarg, 
                  INPUT  1,
                  OUTPUT rc,  
                  OUTPUT seli).   

               IF rc NE 0 THEN DO: 
                  ok = FALSE. 
                  MESSAGE  
                     seli SKIP 
                     "Do You want to continue adding CLI numbers ?" 
                  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ok. 
                  IF NOT ok THEN NEXT LOOP.
               END.

            END.  

         END.  

         APPLY LASTKEY.

      END. /* editing */

      IF lOK AND xCLIFrom NE "" AND xCLITo NE "" AND xCustNum NE 0 THEN DO:  

         CREATE CLISer. 
         ASSIGN 
            CLISer.SerNum     = NEXT-VALUE(CLISeries)
            CLISer.BillTarget = xBillTarg
            CLISer.CustNum    = xCustNum 
            CLISer.CLIFrom    = xCLIFrom 
            CLISer.CLITo      = xCLITo 
            CLISer.Secr       = xSecr 
            CLISer.memo       = xmemo 
            memory            = recid(CLISer).

         /*
         IF Customer.conn    = TRUE AND 
            Customer.rs-code = ""   AND 
            CLISer.Secr  = FALSE THEN 
            RUN nndirlog(CLISer.CustNum, 
                         CLISer.CLIFrom, 
                         CLISer.CLITo,1).
         */

         PAUSE 0. 
         MESSAGE "Creating unique number records into database, wait ...". 

         RUN nnatmu 
           (INPUT  CLISer.SerNum,
            INPUT  CLISer.CLIFrom,  
            INPUT  CLISer.CLITo,  
            INPUT  lCRStamp, 
            INPUT  lCLStamp, 
            INPUT  CLISer.CustNum,
            INPUT  xBillTarg,  
            INPUT  2,
            OUTPUT rc,  
            OUTPUT seli).  

         HIDE MESSAGE NO-PAUSE. 
         MESSAGE seli VIEW-AS ALERT-BOX.

      END.

      LEAVE LOOP.

   END.

   CLEAR FRAME frmAddSeries ALL.
   HIDE  FRAME frmAddSeries NO-PAUSE.

END PROCEDURE.
