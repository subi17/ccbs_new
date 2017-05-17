/* ----------------------------------------------------------------------
MODULE .......: create_test_cdrfile.p
TASK .........: Creates a test CDR file for onlinereader
APPLICATION ..: TMS
AUTHOR .......: ilkkasav & kariaika 
CREATED ......: 2.2.2015
CHANGED ......:
Version ......: Yoigo
----------------------------------------------------------------------- */
{Func/detailvalue.i}
{testing/cdrreader.i}
DEF INPUT PARAM icCLI AS CHAR NO-UNDO.
DEF INPUT PARAM icSecCLI AS CHAR NO-UNDO.
DEF INPUT PARAM icIMSI AS CHARACTER NO-UNDO. 
DEF INPUT PARAM idtCallDateTime AS DATETIME NO-UNDO.
DEF INPUT PARAM iiMeas AS INT NO-UNDO. 
DEF INPUT PARAM icFile AS CHAR NO-UNDO.
DEF INPUT PARAM icOUTPUTFile AS CHAR NO-UNDO.
DEF INPUT PARAM iiIterations AS INTEGER NO-UNDO.

DEF VAR lcLine AS CHAR NO-UNDO. /*cdr entry*/
DEF VAR lcCallDate AS CHAR NO-UNDO. /*for CDR filling*/
DEF VAR lcCallTime AS CHAR NO-UNDO. /*for CDR filling*/
DEF VAR ldtCallDateTime AS DATETIME NO-UNDO. /*for generating new timestamps for CDR*/

DEFINE VARIABLE lcVersion AS CHARACTER NO-UNDO.
DEFINE VARIABLE liCallDatePos AS INTEGER NO-UNDO. 
DEFINE VARIABLE liCallTimePos AS INTEGER NO-UNDO. 
DEFINE VARIABLE liCallDurPos AS INTEGER NO-UNDO.
DEFINE VARIABLE liCallDestPos AS INTEGER NO-UNDO.
DEFINE VARIABLE liCallCasePos AS INTEGER NO-UNDO.
DEFINE VARIABLE liCallOrigPos AS INTEGER NO-UNDO.
DEFINE VARIABLE liCallRecFormatPos  AS INTEGER NO-UNDO.
DEFINE VARIABLE liCallRecTypePos AS INTEGER NO-UNDO.
DEFINE VARIABLE liCount AS INTEGER NO-UNDO.
DEFINE VARIABLE liDataInPos AS INTEGER NO-UNDO.
DEFINE VARIABLE liDataOutPos AS INTEGER NO-UNDO.
DEFINE VARIABLE liTmpSecCLI AS INTEGER NO-UNDO.
DEFINE VARIABLE lcSecCLIPrefix AS CHARACTER NO-UNDO.
DEFINE VARIABLE liCDRTypePos AS INTEGER NO-UNDO.
DEFINE VARIABLE lcCDRType AS CHARACTER NO-UNDO.
DEFINE VARIABLE liOrigImsi AS INTEGER NO-UNDO. 

/*CDR creation is available only in internal environments.
Check that we are not in production*/
IF ( fISInInternalEnv() <> TRUE ) THEN DO:

 MESSAGE "NOT IN TEST" VIEW-AS ALERT-BOX ERROR.
 RETURN.

END.

FILE-INFO:FILE-NAME = icFile.
IF FILE-INFO:FILE-TYPE = ? THEN DO:
   MESSAGE SUBST("ERROR:File &1 not found", icFile) VIEW-AS ALERT-BOX.
   RETURN.
END.

IF idtCallDateTime NE ? THEN DO:
   lcCallDate = STRING(YEAR(idtCallDateTime)) +
                STRING(MONTH(idtCallDateTime),"99") +
                STRING(DAY(idtCallDateTime),"99").

    lcCallTime = fMTimetoString(MTIME(idtCallDateTime)).
END.


FUNCTION fReplaceValue RETURNS LOGICAL
   (INPUT-OUTPUT icLine AS CHARACTER,
    iiPos AS INTEGER,
    icValue AS CHARACTER):

    if ENTRY(iiPos,icLine,"|") ne "" then
    icLine = replace(icLine,"|" + ENTRY(iiPos,icLine,"|") + "|","|" + 
      icValue + "|") NO-ERROR. 

    RETURN (NOT ERROR-STATUS:ERROR).

END FUNCTION. 

DEF STREAM sinfile.
INPUT STREAM sinfile FROM VALUE(icFile).

DEF STREAM soutfile.
OUTPUT STREAM soutfile to VALUE(icOUTPUTFile).

repeat:
   IMPORT STREAM sinfile UNFORMATTED lcLine.
   
   /*Look for field indexes for different types of CDRs*/
   lcVersion = TRIM(ENTRY(5,lcLine,"|")) + TRIM(ENTRY(4,lcLine,"|")).
   IF lcVersion = "0102YC" THEN DO: /* YPR-3890 Premium/Google CDR's format */
      liCallRecFormatPos = fGetPosition((lcVersion),"Format version").
      liCallRecTypePos = fGetPosition((lcVersion),"Event type").
      liCallDatePos = fGetPosition((lcVersion),"Event date").
      liCallTimePos = fGetPosition((lcVersion),"Event time").
      liCallDestPos = fGetPosition((lcVersion),"Destination address").
      liCallDurPos = fGetPosition((lcVersion),"Duration").
      liCallOrigPos = fGetPosition((lcVersion),"Charged subscriber").
      liCallCasePos = fGetPosition((lcVersion),"Call case number").
      liDataInPos = fGetPosition((lcVersion),"Data amount incoming").
      liDataOutPos = fGetPosition((lcVersion),"Data amount outgoing").
      liCDRTypePos = fGetPosition((lcVersion),"Event subtype").
      liOrigImsi = fGetPosition((lcVersion),"Originating IMSI").
   END.
   ELSE DO:
      liCallRecFormatPos = fGetPosition((lcVersion),"Format version").
      liCallRecTypePos = fGetPosition((lcVersion),"Record type").
      liCallDatePos = fGetPosition((lcVersion),"Start date").
      liCallTimePos = fGetPosition((lcVersion),"Start time").
      liCallDestPos = fGetPosition((lcVersion),"Destination address").
      liCallDurPos = fGetPosition((lcVersion),"Duration").
      liCallOrigPos = fGetPosition((lcVersion),"Originating address").
      liCallCasePos = fGetPosition((lcVersion),"Call case number").
      liDataInPos = fGetPosition((lcVersion),"Data amount incoming").
      liDataOutPos = fGetPosition((lcVersion),"Data amount outgoing").
      liCDRTypePos = fGetPosition((lcVersion),"Event subtype").
      liOrigImsi = fGetPosition((lcVersion),"Originating IMSI").
   END.
   /*Read Call/CDR type for correct data handling*/
   lcCDRType = ENTRY(liCDRTypePos,lcLine,"|").

   IF TRIM(ENTRY(liCallRecTypePos,lcLine,"|")) EQ "GE" THEN DO:
      
      IF NOT fReplaceValue(INPUT-OUTPUT lcLine,liOrigIMSI,icImsi) THEN NEXT.
      IF (icSecCli <> "") THEN DO:
         IF NOT fReplaceValue(INPUT-OUTPUT lcLine,liCallDestPos,icSecCli) 
         THEN NEXT.
      END.
   END.
   ELSE DO:
      /*roaming calls: l*/
      IF LOOKUP(TRIM(ENTRY(liCallCasePos,lcLine,"|")),"7,33,96,106") > 0 
         THEN DO:
      /* Roaming support removed in this version.
         If it  is needed this must be moved to loop*
         
         IF NOT fReplaceValue(INPUT-OUTPUT lcLine,liCallDestPos,icCli) 
            THEN NEXT.
         IF (icSecCli <> "") THEN DO:
            IF NOT fReplaceValue(INPUT-OUTPUT lcLine,liCallOrigPos,icSecCli) 
               THEN NEXT.
         END.
         ROAMING ENDS*/
      END.
      ELSE DO:
         IF NOT fReplaceValue(INPUT-OUTPUT lcLine,liCallOrigPos,icCli) 
            THEN NEXT.
         IF (icSecCli <> "") THEN DO:   
            IF NOT fReplaceValue(INPUT-OUTPUT lcLine,liCallDestPos,icSecCli) 
               THEN NEXT.
         END.
      END.
 
   END.

   /*CONN - data calls: divide data to in/out */
   IF (lcCDRType = "CONN") AND iiMeas NE 0 THEN DO:
      IF NOT fReplaceValue(INPUT-OUTPUT lcLine,liDataInPos,
          STRING(1024 * iiMeas / 2)) THEN NEXT.
      if ERROR-STATUS:ERROR THEN NEXT.

      IF NOT fReplaceValue(INPUT-OUTPUT lcLine,liDataOutPos,
          STRING(1024 * iiMeas / 2)) THEN NEXT.

   END.
   if ERROR-STATUS:ERROR THEN NEXT.

   /**/
   IF (lcCDRType <> "VOICE" AND lcVersion <> "0101YF") THEN DO:
       IF iiMeas NE 0 THEN DO:
          lcLine = REPLACE(lcLine, "|" + ENTRY(liCallDurPos,lcLine,"|") + 
             "|", "|" + STRING(60) + "|") NO-ERROR.
          if ERROR-STATUS:ERROR THEN NEXT.
       END.
   END.
   ELSE DO:
       IF iiMeas NE 0 THEN DO:
          lcLine = REPLACE(lcLine, "|" + ENTRY(liCallDurPos,lcLine,"|") +
             "|", "|" + STRING(iiMeas) + "|") NO-ERROR.
          if ERROR-STATUS:ERROR THEN NEXT.
       END.
   END.

   ldtCallDateTime = idtCallDateTime.
   /* lcSecCLIPrefix = SUBSTRING(icSecCli,1,4). */
   liTmpSecCLI = INTEGER(icSecCli). 

   /*Write all entries in a loop:*/
   DO liCount = 1 TO iiIterations:
       IF lcCallDate > "" THEN DO:
          IF NOT fReplaceValue(INPUT-OUTPUT lcLine,liCallDatePos,lcCallDate)
             THEN NEXT.
       END.

       IF lcCallTime > "" THEN DO:
          IF NOT fReplaceValue(INPUT-OUTPUT lcLine,liCallTimePos,lcCallTime)
             THEN NEXT.
       END.
    
    /*New voice call starts when the previous ended. Other type records are done
      in 5 minutes interval. NOTE: now we support time in minutes.*/

       IF lcCDRType <> "VOICE" THEN DO:
          ldtCallDateTime = ADD-INTERVAL(ldtCallDateTime, 60, 'seconds' ).
          lcCallTime = fMTimeToString(MTIME(ldtCallDateTime)).
       END.
       ELSE DO:
          ldtCallDateTime = ADD-INTERVAL(ldtCallDateTime, iiMeas, 'seconds' ).
          lcCallTime = fMTimeToString(MTIME(ldtCallDateTime)).
       END.
       IF ldtCallDateTime NE ? THEN DO:
          lcCallDate = STRING(YEAR(ldtCallDateTime)) +
                       STRING(MONTH(ldtCallDateTime),"99") +
                       STRING(DAY(ldtCallDateTime),"99").
       END.
       lcCallTime = fMTimetoString(MTIME(ldtCallDateTime)).
       /* add other party number if it is given.*/
       /* first three number indicates international number prefix
          it should not change. */
       IF (icSecCli <> "") THEN DO:
            IF NOT fReplaceValue(INPUT-OUTPUT lcLine,liCallDestPos,
               STRING(liTmpSecCli))
               THEN NEXT.
            liTmpSecCLI = INTEGER(liTmpSecCLI) + 1.
       END.


      /*end of other party change*/
   PUT STREAM soutfile UNFORMATTED lcLine skip.
   END. /*loop*/   
END.

OUTPUT STREAM soutfile CLOSE.
INPUT STREAM sinfile CLOSE.

RETURN "".

