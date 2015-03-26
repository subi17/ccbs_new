/* ----------------------------------------------------------------------
  MODULE .......: ifs_subscription.p
  TASK .........: Create a dump file for IFS from subscriptions
  APPLICATION ..: tms
  AUTHOR .......: aam 
  CREATED ......: 28.05.09 (using sap_contract_account)
  Version ......: yoigo
---------------------------------------------------------------------- */

{commali.i}
{cparam2.i}
{dumpfile_run.i}
{barrfunc.i}
{tmsconst.i}

DEF INPUT  PARAMETER iiDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEFINE VARIABLE AKTYP    AS CHARACTER NO-UNDO. /* Activity category  */
DEFINE VARIABLE RLTP1    AS CHARACTER NO-UNDO. /* BDT: Object part   */
DEFINE VARIABLE VKONT    AS CHARACTER NO-UNDO. /* Contract Account Number  */
DEFINE VARIABLE GPART    AS CHARACTER NO-UNDO. /* BP Number  */
DEFINE VARIABLE VKTYP    AS CHARACTER NO-UNDO. /* Contract Account Category */
DEFINE VARIABLE VKONA    AS CHARACTER NO-UNDO. /* Contract account number */ 
DEFINE VARIABLE GPART_HDR_EXT AS CHARACTER NO-UNDO. /* BP number
                                                       in external system  */
DEFINE VARIABLE APPLK         AS CHARACTER NO-UNDO. /* Application area   */
DEFINE VARIABLE CREATION_DATE AS CHARACTER NO-UNDO.  
DEFINE VARIABLE END_DATE      AS CHARACTER NO-UNDO.
DEFINE VARIABLE SUBS_STATUS   AS CHARACTER NO-UNDO.
DEF VAR TERM_REASON   AS CHARACTER NO-UNDO.

DEF VAR ldaModified    AS DATE   NO-UNDO.
DEF VAR liCnt          AS INT    NO-UNDO.
DEF VAR lhMobSub       AS HANDLE NO-UNDO.
DEF VAR lhTermMobSub   AS HANDLE NO-UNDO.
DEF VAR lhOrder        AS HANDLE NO-UNDO.
DEF VAR lhCustomer     AS HANDLE NO-UNDO.
DEF VAR lhMsOwner      AS HANDLE NO-UNDO.
DEF VAR llPick         AS LOG    NO-UNDO.
DEF VAR ldtLastDump    AS DATETIME NO-UNDO.
DEF VAR lcCustKey      AS CHAR   NO-UNDO.
DEF VAR lcCustDenied   AS CHAR   NO-UNDO.
DEF VAR lcModFields    AS CHAR   NO-UNDO.
DEF VAR liModTime      AS INT    NO-UNDO.
DEF VAR liStatus       AS INT    NO-UNDO.
DEF VAR lcOwnerKey     AS CHAR   NO-UNDO.

DEF TEMP-TABLE ttPicked NO-UNDO
   FIELD MsSeq AS INT 
   INDEX MsSeq MsSeq.
   
DEFINE STREAM sLog.


FUNCTION fDate2String RETURNS CHAR
   (idaDate AS DATE):
   
   IF idaDate = ? THEN RETURN "".
   
   RETURN STRING(YEAR(idaDate),"9999") +
          STRING(MONTH(idaDate),"99")  +
          STRING(DAY(idaDate),"99").
   
END FUNCTION.

/* separate fCollect* functions for each table so that they can be called 
   dynamically from pFindFromEventLog */
FUNCTION fCollectMobSub RETURNS LOGIC:

   IF CAN-FIND(FIRST ttPicked WHERE ttPicked.MsSeq = MobSub.MsSeq) THEN 
      RETURN FALSE.

   CREATE ttPicked.
   ttPicked.MsSeq = MobSub.MsSeq.

   RETURN TRUE. 
END FUNCTION.

FUNCTION fCollectTermMobSub RETURNS LOGIC:

   IF CAN-FIND(FIRST ttPicked WHERE ttPicked.MsSeq = TermMobSub.MsSeq) THEN 
      RETURN FALSE.

   CREATE ttPicked.
   ttPicked.MsSeq = TermMobSub.MsSeq.

   RETURN TRUE. 
END FUNCTION.

FUNCTION fCollectMsOwner RETURNS LOGIC:

   IF CAN-FIND(FIRST ttPicked WHERE ttPicked.MsSeq = MsOwner.MsSeq) THEN 
      RETURN FALSE.

   CREATE ttPicked.
   ttPicked.MsSeq = MsOwner.MsSeq.

   RETURN TRUE. 
END FUNCTION.
 
FUNCTION fCollectCustomer RETURNS LOGIC:

   FOR EACH MobSub NO-LOCK WHERE
            MobSub.Brand   = gcBrand AND
            MobSub.AgrCust = Customer.CustNum:
      IF CAN-FIND(FIRST ttPicked WHERE ttPicked.MsSeq = MobSub.MsSeq) THEN
         NEXT.

      CREATE ttPicked.
      ttPicked.MsSeq = MobSub.MsSeq.
   END.   
   
   FOR EACH TermMobSub NO-LOCK WHERE
            TermMobSub.Brand   = gcBrand AND
            TermMobSub.AgrCust = Customer.CustNum:
      IF CAN-FIND(FIRST ttPicked WHERE ttPicked.MsSeq = TermMobSub.MsSeq) THEN
         NEXT.

      CREATE ttPicked.
      ttPicked.MsSeq = TermMobSub.MsSeq.
   END.   
   
   RETURN TRUE. 
END FUNCTION.
 
 
/***** Main start **********/

OUTPUT STREAM sLog TO VALUE(icFile).

FIND FIRST DumpFile WHERE DumpFile.DumpID = iiDumpID NO-LOCK NO-ERROR.

fSplitTS(idLastDump,
         OUTPUT ldaModified,
         OUTPUT liModTime).

ldtLastDump = fTimeStamp2DateTime(idLastDump).

ASSIGN
   lhMobSub     = BUFFER MobSub:HANDLE
   lhTermMobSub = BUFFER TermMobSub:HANDLE
   lhOrder      = BUFFER Order:HANDLE
   lhCustomer   = BUFFER Customer:HANDLE
   lhMsOwner    = BUFFER MsOwner:HANDLE
   /* customers that are not transferred */
   lcCustDenied = fCParamC("AgrCustNoTransfer")

   /* constants */
   AKTYP    = "04"   
   RLTP1    = "MKK"  
   APPLK    = "T".  

IF icDumpMode = "modified" THEN DO:
   ASSIGN 
      lcCustKey  = fEventKeyFields(lhCustomer)
      lcOwnerKey = fEventKeyFields(lhMsOwner).
   IF AVAILABLE DumpFile THEN lcModFields = DumpFile.EventLogFields.

   RUN pModifiedSubscriptions. 
END.   

/* full dumps */
ELSE DO:

   RUN pActiveSubscriptions.

   IF NOT olInterrupted THEN 
      RUN pTerminatedSubscriptions.
END.

IF NOT olInterrupted THEN 
   RUN pVirtualSubscriptions.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
   
OUTPUT STREAM sLog CLOSE.


/******** Main end *******/


PROCEDURE pModifiedSubscriptions:

   DEF VAR ldaEventDate AS DATE NO-UNDO.
   DEF VAR lcModTime    AS CHAR NO-UNDO.
   DEF VAR ldChkStamp   AS DEC  NO-UNDO.
   DEF VAR liPayType    AS INT  NO-UNDO.
   

   DO liCnt = 1 TO NUM-ENTRIES(icEventSource,"|"):
   
      /* check modification from eventlog */
      IF ENTRY(liCnt,icEventSource,"|") = "EventLog" THEN DO:
      
         RUN pFindFromEventLog(lhMobSub,
                               ENTRY(liCnt,icEventFields,"|"),
                               lcModFields,
                               idLastDump,
                               "fCollectMobSub").
 
         RUN pFindFromEventLog(lhTermMobSub,
                               ENTRY(liCnt,icEventFields,"|"),
                               lcModFields,
                               idLastDump,
                               "fCollectTermMobSub").
      END.
      
      /* check modification from a timestamp field */
      ELSE IF ENTRY(liCnt,icEventSource,"|") = "field" AND
              ENTRY(liCnt,icEventFields,"|") = "ActivationTS" 
      THEN DO:
      
         DO liStatus = 1 TO 99:

            FOR EACH MobSub NO-LOCK WHERE
                     MobSub.Brand    = gcBrand AND
                     MobSub.MsStatus = liStatus AND
                     MobSub.ActivationDate >= ldaModified AND
                     MobSub.ActivationTS >= idLastDump:
               fCollectMobSub().      
            END.
 
            FOR EACH TermMobSub NO-LOCK WHERE
                     TermMobSub.Brand    = gcBrand AND
                     TermMobSub.MsStatus = liStatus AND
                     TermMobSub.ActivationDate >= ldaModified AND
                     TermMobSub.ActivationTS >= idLastDump:
               fCollectTermMobSub().      
            END.
         END.   
         
      END.
   END.

   /* subscription is dumped also when customer is changed */
   RUN pFindFromEventLog(lhCustomer,
                         lcCustKey,
                         "",
                         idLastDump,
                         "fCollectCustomer").
   
   /* check also from msowner */
   RUN pFindFromEventLog(lhMsOwner,
                         lcOwnerKey,
                         "",
                         idLastDump,
                         "fCollectMsOwner").
 
   ldChkStamp = fOffSet(idLastDump,-3).
      
   /* and from barring requests (if barring is changed from one package 
      to another then msstatus is not updated) */
   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.Brand = gcBrand AND 
            MsRequest.ReqType = 35 AND
            MsRequest.ReqStat = 2  AND
            MsRequest.ActStamp >= ldChkStamp AND
            MsRequest.DoneStamp >= idLastDump:

      FIND FIRST MobSub WHERE MobSub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.
      IF AVAILABLE MobSub THEN DO:
         fCollectMobSub().
         NEXT.
      END.
 
      FIND FIRST TermMobSub WHERE 
         TermMobSub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.
      IF AVAILABLE TermMobSub THEN DO:
         fCollectTermMobSub().
      END.
   END.   

   FOR EACH ttPicked:
   
      FIND FIRST MobSub WHERE MobSub.MsSeq = ttPicked.MsSeq NO-LOCK NO-ERROR.
      IF AVAILABLE MobSub THEN DO:
          RUN pWrite2File(lhMobSub,
                          1 + INTEGER(MobSub.PayType)).
      END.
 
      ELSE DO:
         FIND FIRST TermMobSub WHERE 
            TermMobSub.MsSeq = ttPicked.MsSeq NO-LOCK NO-ERROR.
         IF AVAILABLE TermMobSub THEN DO:
            liPayType = 1.
            FOR FIRST MsOwner NO-LOCK WHERE
                      MsOwner.MsSeq = TermMobSub.MsSeq:
               liPayType = 1 + INTEGER(MsOwner.PayType).
            END.
      
            RUN pWrite2File(lhTermMobSub,
                            liPayType).
         END.
      END.
    
   END.
    
END PROCEDURE.
    
PROCEDURE pActiveSubscriptions:

   /* active subscriptions */
   MobLoop:
   FOR EACH MobSub NO-LOCK WHERE 
            MobSub.Brand = gcBrand AND
            MobSub.CLI > ""        AND
            MobSub.AgrCust > 0
      ON QUIT UNDO, RETRY
      ON STOP UNDO, RETRY:

      /* make sure that ctrl-c doesn't quit */
      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END.

      IF LOOKUP(STRING(MobSub.AgrCust),lcCustDenied) > 0 THEN NEXT. 
        
      RUN pWrite2File(lhMobSub,
                      1 + INTEGER(MobSub.PayType)).

   END.  /* mobloop */

END PROCEDURE.  /* pActiveSubscriptions */

PROCEDURE pTerminatedSubscriptions:

   DEF VAR liPayType AS INT  NO-UNDO.
   
   /* terminated */
   TermMobLoop:
   FOR EACH TermMobSub NO-LOCK WHERE 
            TermMobSub.Brand = gcBrand
      ON QUIT UNDO, RETRY
      ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END.

      IF LOOKUP(STRING(TermMobSub.AgrCust),lcCustDenied) > 0 THEN NEXT. 

      liPayType = 1.
      FOR FIRST MsOwner NO-LOCK WHERE
                MsOwner.MsSeq = TermMobSub.MsSeq:
         liPayType = 1 + INTEGER(MsOwner.PayType).
      END.
      
      RUN pWrite2File(lhTermMobSub,
                      liPayType).

   END.  /* termmobloop */

END PROCEDURE. /* pTerminatedSubscriptions */

PROCEDURE pVirtualSubscriptions:

   /* undelivered orders, from which a cash invoice has been created */
   VirtualMobLoop:
   FOR EACH TMSCodes NO-LOCK WHERE
            TMSCodes.TableName = "Order" AND
            TMSCodes.FieldName = "StatusCode" AND
            TMSCodes.CodeValue NE "6",
       EACH Order NO-LOCK USE-INDEX StatusCode WHERE
            Order.Brand      = gcBrand AND
            Order.StatusCode = TMSCodes.CodeValue AND
            Order.InvNum > 0  AND
            Order.CustNum > 0
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END.

      IF CAN-FIND(FIRST MobSub WHERE MobSub.MsSeq = Order.MsSeq) THEN NEXT.
      IF CAN-FIND(FIRST TermMobSub WHERE TermMobSub.MsSeq = Order.MsSeq) THEN 
         NEXT. 
      IF LOOKUP(STRING(Order.CustNum),lcCustDenied) > 0 THEN NEXT. 

      /* dump only modified ones */
      IF icDumpMode = "modified" THEN DO:
         
         FIND FIRST Invoice WHERE Invoice.InvNum = Order.InvNum 
            NO-LOCK NO-ERROR.
      
         llPick = (AVAILABLE Invoice AND Invoice.ChgStamp >= idLastDump).
       
         /* check also if customer was changed */
         IF NOT llPick THEN DO:
            FIND FIRST Customer WHERE Customer.CustNum = Order.CustNum
               NO-LOCK NO-ERROR.
         
            IF AVAILABLE Customer THEN    
            llPick = fWasRecordModified(lhCustomer,
                                        "EventLog",
                                        lcCustKey,
                                        idLastDump,
                                        ldaModified,
                                        ldtLastDump,
                                        "").
         END.             
 
         IF NOT llPick THEN NEXT.
      END.
                  
      RUN pWrite2File(lhOrder,
                      1 + INTEGER(Order.PayType)).
   
   END.

END PROCEDURE.  /* pVirtualSubscriptions */

PROCEDURE pWrite2File:
   
   DEF INPUT PARAMETER ihTable AS HANDLE NO-UNDO.
   DEF INPUT PARAMETER iiPayType AS INT  NO-UNDO.
   
   DEF VAR lcCustField AS CHAR NO-UNDO.
   DEF VAR ldaSubsDate AS DATE NO-UNDO.
   DEF VAR liTime      AS INT  NO-UNDO.
   DEF VAR liMsSeq     AS INT  NO-UNDO.
   DEF VAR ldaActDate  AS DATE NO-UNDO.

   ASSIGN
      liMsSeq  = ihTable:BUFFER-FIELD("MsSeq"):BUFFER-VALUE
      VKONT    = STRING(liMsSeq,"999999999999") 
      VKONA    = ihTable:BUFFER-FIELD("CLI"):BUFFER-VALUE 
      GPART_HDR_EXT = ""  
      SUBS_STATUS   = ""
      END_DATE      = ""
      TERM_REASON   = "".   

   CASE ihTable:NAME:
   
   WHEN "MobSub" THEN DO:
      IF ihTable:BUFFER-FIELD("MsStatus"):BUFFER-VALUE = 8 THEN DO:
         SUBS_STATUS = fCheckStatus(liMsSeq).
         IF SUBS_STATUS = "" OR SUBS_STATUS = ? THEN 
            SUBS_STATUS = "BARRED".
      END.

      ASSIGN   
         ldaActDate  = ihTable:BUFFER-FIELD("ActivationDate"):BUFFER-VALUE
         lcCustField = "InvCust".
   END.
   
   WHEN "TermMobSub" THEN DO:
      FIND FIRST MsOwner WHERE MsOwner.MsSeq = liMsSeq NO-LOCK NO-ERROR.
      IF AVAILABLE MsOwner AND MsOwner.TsEnd < 99999999 THEN DO:
         fSplitTS(MsOwner.TSEnd,
                  OUTPUT ldaSubsDate,
                  OUTPUT liTime).
         END_DATE = fDate2String(ldaSubsDate).
      END.
      
      ASSIGN 
         ldaActDate  = ihTable:BUFFER-FIELD("ActivationDate"):BUFFER-VALUE
         lcCustField = "InvCust".
      
      FOR EACH MsRequest NO-LOCK WHERE 
               MSRequest.MsSeq     = liMsSeq AND 
               MsRequest.ReqType   = 18               AND 
               MsRequest.ReqStatus = 2                
               BY MsRequest.ActStamp DESCENDING :
          
          FIND FIRST TMSCodes WHERE 
                     TMSCodes.TableName = "MsRequest"          AND 
                     TMSCodes.FieldName = "TermReason"         AND 
                     TMSCodes.CodeValue = MsRequest.ReqCParam3 NO-LOCK NO-ERROR.
                     
          IF AVAILABLE TMSCodes THEN  
              TERM_REASON = (IF TMSCODES.CodeName > "" THEN TMSCODES.CodeName 
                             ELSE TMSCodes.CodeValue).               
          ELSE TERM_REASON = MsRequest.ReqCParam3.
                                         
          LEAVE.                       
      END.
          
   END.

   WHEN "Order" THEN DO:
      fSplitTS(Order.CrStamp,   
               OUTPUT ldaActDate,
               OUTPUT liTime).

      /* YOT-1125 */
      IF Order.MNPStatus > 0 AND
         LOOKUP(Order.StatusCode,{&ORDER_CLOSE_STATUSES}) > 0 THEN DO:

         FIND FIRST OrderTimeStamp WHERE
                    OrderTimeStamp.Brand   = gcBrand   AND
                    OrderTimeStamp.OrderID = Order.OrderID AND
                    OrderTimeStamp.RowType = 3 NO-LOCK NO-ERROR.
         
         IF AVAIL OrderTimeStamp THEN DO:
            
            fSplitTS(OrderTimeStamp.TimeStamp,   
                     OUTPUT ldaSubsDate,
                     OUTPUT liTime).
            END_DATE = fDate2String(ldaSubsDate).
         END.
      END.
               
      lcCustField = "CustNum".         
   END.
   
   END CASE.

   ASSIGN
      CREATION_DATE = fDate2String(ldaActDate)
      GPART         = STRING(ihTable:BUFFER-FIELD(lcCustField):BUFFER-VALUE,
                             "9999999999") 
      SUBS_STATUS   = FILL(" ",15 - LENGTH(SUBS_STATUS)) + SUBS_STATUS
      VKTYP         = "Z" + STRING(iiPayType)
      oiEvents      = oiEvents + 1.

   IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP oiEvents LABEL "Subscriptions" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END.
 
   /* Dump one record */ 
   PUT STREAM sLog UNFORMATTED 
      /*  1 */ STRING(AKTYP,"X(2)")
      /*  2 */ STRING(RLTP1,"X(6)")
      /*  3 */   /* RLTP2           - NOT USED */ FILL(" ",6)
      /*  4 */   /* RLTP3           - NOT USED */ FILL(" ",6)
      /*  5 */   /* RLTP4           - NOT USED */ FILL(" ",6)
      /*  6 */   /* RLTP5           - NOT USED */ FILL(" ",6)
      /*  7 */   /* RLTP6           - NOT USED */ FILL(" ",6)
      /*  8 */   /* RLTP7           - NOT USED */ FILL(" ",6)
      /*  9 */   /* RLTP8           - NOT USED */ FILL(" ",6)
      /* 10 */   /* RLTP9           - NOT USED */ FILL(" ",6)
      /* 11 */ STRING(VKONT,"999999999999")  
      /* 12 */ STRING(GPART,"9999999999")
      /* 13 */ STRING(VKTYP,"X(2)") 
      /* 14 */ STRING(VKONA,"X(20)")  
      /* 15 */ STRING(GPART_HDR_EXT,"X(20)") 
      /* 16 */ STRING(APPLK,"X(1)") 
      /* 17 */   /* VALDT           - NOT USED */ FILL(" ",8)
      /* 18 */   /* VKBEZ           - NOT USED */ FILL(" ",35)
      /* 19 */ STRING(CREATION_DATE,"X(8)")
      /* 20 */ STRING(END_DATE,"X(8)")
      /* 21 */ STRING(SUBS_STATUS,"X(15)")
      /* 22 */ FILL(" ",1)
      /* 23 */ STRING(TERM_REASON,"X(50)")  
      SKIP.

END PROCEDURE.  /* pWrite2File */


