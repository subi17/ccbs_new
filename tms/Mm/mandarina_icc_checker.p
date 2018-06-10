/* ----------------------------------------------------------------------
  MODULE .......: mandarina_icc_checker.p
  TASK .........: Program removes Mandarina redirection if it is active
                  when ICC is done.
  APPLICATION ..: tms
  AUTHOR .......: ilsavola
  CREATED ......: 08/2017
  Version ......: yoigo
----------------------------------------------------------------------
10/01/2018 ashok YDR-2754 Inactivate Barring after ICC Change
                          Code optimization
---------------------------------------------------------------------- */

/*----------------------------------------------------------------------
https://kethor.qvantel.com/browse/MANDLP-10
---------------------------------------------------------------------- */

/* includes */
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/cparam2.i}
{Func/lpfunctions.i}
{Func/barrfunc.i}

/* Lock and seek time hadling parameters */
DEF VAR lcTableName            AS CHAR    NO-UNDO.
DEF VAR lcActionID             AS CHAR    NO-UNDO.
DEF VAR ldCurrentTimeTS        AS DEC     NO-UNDO.
DEF VAR ldCollPeriodStartTS    AS DEC     NO-UNDO.
DEF VAR ldCollPeriodEndTS      AS DEC     NO-UNDO.
DEF VAR lcError                AS CHAR    NO-UNDO.
DEF VAR llgReqDone             AS LOGICAL NO-UNDO.
DEF VAR lcLogDirectory         AS CHAR    NO-UNDO INITIAL "/tmp/". /* /tmp/mnt/store/riftp/mandarina/logs/ */
DEF VAR lcICCLog               AS CHAR    NO-UNDO.
DEF VAR lcBarrings             AS CHAR    NO-UNDO.
DEFINE VARIABLE rmInternetBarring AS LOGICAL NO-UNDO.
DEFINE VARIABLE rmCustLostBarring AS LOGICAL NO-UNDO.

DEF STREAM sICCLog.    /* Log file for ICC_checker executions */

DEF BUFFER bLP_MsRequest FOR MsRequest.

/* Log file for ICC_checker executions */
lcLogDirectory = fCParamC("MandarinaLogsDir") NO-ERROR.
lcICCLog = lcLogDirectory + 
           STRING(YEAR(TODAY), "9999") + 
           STRING(MONTH(TODAY), "99" ) +
           STRING(DAY(TODAY), "99") + 
           "_ICC_checker.log".          
           
OUTPUT STREAM sICCLog TO VALUE(lcICCLog) APPEND.
PUT STREAM sICCLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";mandarina_icc_checker_starts" SKIP.

/* Check if process ir running */
ASSIGN
   lcTableName = "MANDARINA"
   lcActionID  = "Mandarina_ICC_Checker"
   ldCurrentTimeTS = Func.Common:mMakeTS().

DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  Syst.Var:gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName NO-ERROR.

   IF AVAIL ActionLog AND
      ActionLog.ActionStatus EQ {&ACTIONLOG_STATUS_PROCESSING} THEN DO:
      PUT STREAM sICCLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";another_mandarina_icc_checker_running" SKIP.
      PUT STREAM sICCLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";mandarina_icc_checker_finishing" SKIP.
      OUTPUT STREAM sICCLog CLOSE.
      QUIT.
   END.

   IF NOT AVAIL ActionLog THEN DO:
      /*First execution stamp*/
      CREATE ActionLog.
      ASSIGN
         ActionLog.Brand        = Syst.Var:gcBrand
         ActionLog.TableName    = lcTableName
         ActionLog.ActionID     = lcActionID
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}
         ActionLog.UserCode     = Syst.Var:katun
         ActionLog.ActionTS     = ldCurrentTimeTS.
      RELEASE ActionLog.
      PUT STREAM sICCLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";mandarina_icc_checker_first_run" SKIP.
      PUT STREAM sICCLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";mandarina_icc_checker_finishes" SKIP.
      OUTPUT STREAM sICCLog CLOSE.
      QUIT. /*No reporting in first time.*/
   END.
   ELSE DO:
      /*Set collection period start time to match the previous period end*/
      ASSIGN
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_PROCESSING}
         ActionLog.UserCode     = Syst.Var:katun
         ldCollPeriodStartTS    = ActionLog.ActionTS.
      RELEASE Actionlog.
   END.
END.

ldCollPeriodEndTS = Func.Common:mSecOffSet(ldCurrentTimeTS, -60). /*Now - 1 minute */
PUT STREAM sICCLog UNFORMATTED "Collection period: " + Func.Common:mTS2HMS(ldCollPeriodStartTS) + " TO " Func.Common:mTS2HMS(ldCollPeriodEndTS) SKIP.
/* ICC lookup part */
/* Find ICC requests */
/* Find information if LP is active. If yes, switch it off */
FOR EACH MsRequest NO-LOCK WHERE
         MsRequest.Brand EQ Syst.Var:gcBrand AND
         MsRequest.ReqStatus EQ {&REQUEST_STATUS_DONE} AND
         MsRequest.UpdateStamp > ldCollPeriodStartTS AND
         MsRequest.UpdateStamp <= ldCollPeriodEndTS AND
         MsRequest.ReqType EQ {&REQTYPE_ICC_CHANGE} /*15*/ AND
         MsRequest.UpdateStamp <= MsRequest.DoneStamp
         USE-Index UpdateStamp:
   FIND FIRST bLP_MsRequest NO-LOCK WHERE 
              bLP_MsRequest.MsSeq EQ MsRequest.MsSeq AND
              bLP_MsRequest.ActStamp < MsRequest.DoneStamp AND
              bLP_MsRequest.ReqCparam1 EQ "LP" 
              USE-INDEX MsActStamp NO-ERROR.
   IF AVAIL bLP_MsRequest THEN DO:
      ASSIGN 
        rmInternetBarring = FALSE 
        rmCustLostBarring = FALSE .
      /* Check barring status */
      lcBarrings = Func.BarrMethod:mGetActiveBarrings(MsRequest.MsSeq).
      
      IF lcBarrings <> "" THEN DO:
         IF LOOKUP("DEBT_LP", lcBarrings) <> 0 OR LOOKUP("DEBT_HOTLP", lcBarrings) <> 0 THEN DO:
            PUT STREAM sICCLog UNFORMATTED
               STRING(TIME,"hh:mm:ss") + ";" +  
               STRING(MsRequest.MsSeq) + ";" +
               STRING(MsRequest.CustNum) + ";WARNING:DEBT_barring_active" SKIP. 
            NEXT.
         END.
      END. 
      
      /* begin YDR-2668. IF Internet barring active because of Mandarina, then remove. */
      rmInternetBarring = LOOKUP("Internet", lcBarrings) <> 0 AND
                             CAN-FIND(FIRST Memo WHERE
                                            Memo.Brand EQ Syst.Var:gcBrand AND
                                            Memo.CustNum EQ MsRequest.CustNum AND
                                            Memo.HostTable EQ "MobSub" AND
                                            Memo.MemoTitle EQ "OTA Barring activado"
                                            USE-INDEX CustNum) .
      /* YDR-2754 */
      rmCustLostBarring = LOOKUP("Cust_LOST", lcBarrings) <> 0 AND
                            fGetBarringRequestSource(MsRequest.MsSeq , "Cust_LOST" , "ACTIVE") EQ {&REQUEST_SOURCE_YOIGO_TOOL} .

      IF rmInternetBarring OR rmCustLostBarring THEN DO:
          RUN pRemoveBarring.
          IF RETURN-VALUE = "OK" THEN DO:
             IF rmInternetBarring THEN 
                PUT STREAM sICCLog UNFORMATTED 
                    STRING(TIME,"hh:mm:ss") + ";" +  
                    STRING(MsRequest.MsSeq) + ";" +
                    STRING(MsRequest.CustNum) + ";" + 
                    "Internet_barring_deactivate"
                    SKIP.
            IF rmCustLostBarring THEN 
                PUT STREAM sICCLog UNFORMATTED 
                   STRING(TIME,"hh:mm:ss") + ";" +  
                   STRING(MsRequest.MsSeq) + ";" +
                   STRING(MsRequest.CustNum) + ";" + 
                   "Cust_Lost_barring_deactivate"
                   SKIP.
          END.
          ELSE DO:
             PUT STREAM sICCLog UNFORMATTED 
                STRING(TIME,"hh:mm:ss") + ";" +  
                STRING(MsRequest.MsSeq) + ";" +
                STRING(MsRequest.CustNum) + ";" +
                RETURN-VALUE
                SKIP.        
          END.
      END.

      IF (bLP_MsRequest.ReqCparam2 EQ "REDIRECTION_OTFAILED1" OR
          bLP_MsRequest.ReqCparam2 EQ "REDIRECTION_OTFAILED2") THEN DO:
         /*The last LP command was Mandarina LP redirection setting
           -> this must be cancelled*/
         llgReqDone = fMakeLPCommandRequest(MsRequest.MsSeq,
                                            "remove",
                                            MsRequest.CustNum,
                                            "Redirection removed",
                                            "Redirection removed, reason: ICC",
                                            "ICC_Checker",
                                            "5", /* Automatic */ 
                                            INPUT-OUTPUT lcError).
         PUT STREAM sICCLog UNFORMATTED 
            STRING(TIME,"hh:mm:ss") + ";" +  
            STRING(MsRequest.MsSeq) + ";" +
            STRING(MsRequest.CustNum) + ";" + 
            (IF llgReqDone THEN "remove"
             ELSE "ERROR:" + lcError)
            SKIP.
         /* TODO: Source setting for DUMP purposes. */              
      END.
      ELSE NEXT. /*do not remove other commands*/
   END.
   ELSE NEXT.
END.

/*End of ICC lookup */

/*Update cunrent collection period end time to actionlog*/
DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  Syst.Var:gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName    AND
              ActionLog.ActionStatus NE {&ACTIONLOG_STATUS_SUCCESS}
   EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL ActionLog THEN DO:
      ActionLog.ActionTS = ldCollPeriodEndTS.
      ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}.
   END.
   RELEASE ActionLog.
END.

PUT STREAM sICCLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";mandarina_icc_checker_finishes" SKIP.
OUTPUT STREAM sICCLog CLOSE.

/*-------------------------------------------------
 Internal Procedures
-------------------------------------------------*/
/* YDR-2754 ( Also combined YDR-2668 ) */
PROCEDURE pRemoveBarring:

    DEFINE VARIABLE liRequest AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lcResult  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcCommand AS CHARACTER NO-UNDO.
    
    FIND FIRST MobSub WHERE
               MobSub.Brand EQ Syst.Var:gcBrand AND
               Mobsub.CLI   EQ MsRequest.CLI
               USE-INDEX CLI NO-LOCK NO-ERROR.

    IF NOT AVAILABLE MobSub THEN
        RETURN "ERROR:MSISDN_not_found".

    IF rmInternetBarring AND rmCustLostBarring THEN 
        lcCommand = "Internet=0,Cust_LOST=0".
    ELSE IF rmInternetBarring THEN 
        lcCommand = "Internet=0".
    ELSE 
        lcCommand = "Cust_LOST=0".

    RUN Mm/barrengine.p(mobsub.MsSeq,
        lcCommand,                    /* Barring */
        {&REQUEST_SOURCE_YOIGO_TOOL}, /* source    */
        "Sistema",                    /* creator  */
        Func.Common:mMakeTS(),        /* activate */
        "",                           /* SMS      */
        OUTPUT lcResult).

    liRequest = INTEGER(lcResult) NO-ERROR.                             
    IF liRequest > 0 THEN 
    DO:
        Func.Common:mWriteMemoWithType("Mobsub",
            STRING(mobsub.MsSeq),
            mobsub.CustNum,
            "OTA Barring desactivado",          /* memo title */
            "Redirection removed, reason: ICC", /* memo text  */
            "Service",                          /* memo type  */   
            "Sistema").                         /* memo creator    */
        RETURN "OK". 
    END.
    ELSE 
        RETURN "ERROR:" + lcResult.
END PROCEDURE.

