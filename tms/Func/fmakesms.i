/* fmakesms.i       28.05.04/aam 
   create an entry to CallAlarm to be sent as SMS 

   changes:         27.01.05/aam fMakeSchedSMS
                    19.07.05/tk  fSetTSLimit
*/

{commali.i}
{timestamp.i}
{tmsconst.i}

&IF "{&fmakesms}" NE "YES"
&THEN

&GLOBAL-DEFINE fmakesms YES

FUNCTION _fCreateCallAlarm RETURNS INTEGER
   (iiCustNum  AS INT,
    icCLI      AS CHAR,
    iiType     AS INT,
    icMessage  AS CHAR,
    idtActTime AS DEC,
    icOrig     AS CHAR,
    icActInt   AS CHAR).

   IF icCLI = "" OR icMessage = "" THEN RETURN 0.
   IF icCLI EQ ? OR icMessage EQ ? THEN RETURN 0.
   
   IF idtActTime = 0 OR
      idtActTime = ? THEN idtActTime = fMakeTS().
   
   /* already done */
   FOR FIRST CallAlarm NO-LOCK WHERE
             CallAlarm.Brand      = gcBrand AND
             CallAlarm.CLI        = icCLI   AND
             CallAlarm.DeliStat   = 1       AND
             CallAlarm.CreditType = iiType  AND
             CAllAlarm.ActStamp   = idtActTime AND
             CallAlarm.DeliMsg    = icMessage:
             
      RETURN 0.
   END.
             
   CREATE CallAlarm.
   ASSIGN CallAlarm.CLSeq      = 0
          CallAlarm.CASeq      = NEXT-VALUE(CallAlarm)
          CallAlarm.CustNo     = iiCustNum 
          CallAlarm.CLI        = icCLI
          CallAlarm.DeliStat   = 1            
          CallAlarm.Delitype   = (IF iiType EQ {&SMSTYPE_HPD} THEN 5 ELSE 1)
          CallAlarm.DeliPara   = "1"
          CallAlarm.DeliMsg    = icMessage
          CallAlarm.Limit      = 0
          CallAlarm.CreditType = iiType
          CallAlarm.Brand      = gcBrand
          CallAlarm.ActStamp   = idtActTime
          CallAlarm.Orig       = icOrig
          CallAlarm.ActInterval = icActInt.

   RETURN CallAlarm.CASeq. 
   
END FUNCTION.

/* Sender number and time interval parameter added */
FUNCTION fMakeSchedSMS2 RETURNS INTEGER
   (iiCustNum  AS INT,
    icCLI      AS CHAR,
    iiType     AS INT,
    icMessage  AS CHAR,
    idtActTime AS DEC,
    icOrig     AS CHAR,
    icActInt   AS CHAR).
   
   RETURN _fCreateCallAlarm(iiCustNum,
                            icCLI,
                            iiType,
                            icMessage,
                            idtActTime,
                            icOrig,
                            icActInt).
END FUNCTION.

FUNCTION fMakeSchedSMS RETURNS INTEGER
   (iiCustNum  AS INT,
    icCLI      AS CHAR,
    iiType     AS INT,
    icMessage  AS CHAR,
    idtActTime AS DEC):
   
   RETURN _fCreateCallAlarm(iiCustNum,
                            icCLI,
                            iiType,
                            icMessage,
                            idtActTime,
                            "",
                            "").
         
END.

FUNCTION fSetTSLimit RETURNS DECIMAL
   (INPUT pdeTStamp AS DECIMAL,
    INPUT piLimit1  AS INTEGER,
    INPUT piLimit2  AS INTEGER):
   
   DEFINE VARIABLE liSeconds AS INTEGER NO-UNDO.
   DEFINE VARIABLE ldaDate   AS DATE    NO-UNDO.
   DEFINE VARIABLE liTime    AS INTEGER NO-UNDO.
   
   liSeconds = (pdeTStamp - TRUNC(pdeTStamp,0)) * 100000.

   IF liSeconds < piLimit1 THEN ASSIGN
      pdeTStamp = TRUNC(pdeTStamp,0) + (piLimit1 / 100000).
   ELSE IF liSeconds > piLimit2 THEN DO:
      fSplitTS(INPUT pdeTStamp, OUTPUT ldaDate, OUTPUT liTime).
      ldaDate = ldaDate + 1.
      pdeTStamp = fMake2Dt(INPUT ldaDate, INPUT piLimit1).
   END.
   
   RETURN pdeTStamp.
END.
   
&ENDIF
