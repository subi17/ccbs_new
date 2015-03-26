/* ------------------------------------------------------
  MODULE .......: COACREP
  FUNCTION .....: Commission event report for ac
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 30.06.05/aam (from copayrep)
  MODIFIED .....: 
  VERSION ......: M15
  ------------------------------------------------------ */

{commali.i}
{cparam2.i}
{utumaa.i}
{timestamp.i}
{fprevoper.i}

DEF TEMP-TABLE ttMark NO-UNDO
    FIELD EventID AS INT.

DEF TEMP-TABLE ttEvent NO-UNDO
    FIELD EventId    AS INT
    FIELD CoRuleID   AS INT 
    FIELD Reseller   AS CHAR
    FIELD Salesman   AS CHAR
    FIELD EventCust  AS CHAR
    FIELD Amount     AS DEC
    FIELD CLI        AS CHAR
    FIELD CLIType    AS CHAR
    FIELD PrevOper   AS CHAR
    FIELD OrdChannel AS CHAR
    FIELD OrdDate    AS DATE
    FIELD ActDate    AS DATE
    INDEX Reseller Reseller Salesman CLI. 

DEF TEMP-TABLE ttSum NO-UNDO
    FIELD PrevOper AS CHAR
    FIELD CoAmt    AS DEC 
    FIELD CoRuleID AS INT
    FIELD ActQty   AS INT
    FIELD Amount   AS DEC
    INDEX PrevOper PrevOper CoRuleID CoAmt.
    
DEF INPUT PARAMETER iiCORuleID   AS INT   NO-UNDO. 
DEF INPUT PARAMETER icReseller1  AS CHAR  NO-UNDO. 
DEF INPUT PARAMETER icReseller2  AS CHAR  NO-UNDO. 
DEF INPUT PARAMETER icSalesman1  AS CHAR  NO-UNDO. 
DEF INPUT PARAMETER icSalesman2  AS CHAR  NO-UNDO. 
DEF INPUT PARAMETER iiCustNum1   AS INT   NO-UNDO. 
DEF INPUT PARAMETER iiCustNum2   AS INT   NO-UNDO. 
DEF INPUT PARAMETER idtCalcDate1 AS DATE  NO-UNDO.
DEF INPUT PARAMETER idtCalcDate2 AS DATE  NO-UNDO.
DEF INPUT PARAMETER idtPaymDate1 AS DATE  NO-UNDO.
DEF INPUT PARAMETER idtPaymDate2 AS DATE  NO-UNDO.
DEF INPUT PARAMETER icFile       AS CHAR  NO-UNDO.

DEF OUTPUT PARAMETER TABLE FOR ttMark.
DEF OUTPUT PARAMETER oiCount     AS INT   NO-UNDO.


DEF VAR lcDateHeader AS CHAR NO-UNDO.
DEF VAR lcText       AS CHAR NO-UNDO. 
DEF VAR lcSesNum     AS CHAR NO-UNDO.
DEF VAR liEventCust  AS INT  NO-UNDO. 
DEF VAR ldActDate    AS DEC  NO-UNDO.
DEF VAR liMsSeq      AS INT  NO-UNDO. 
DEF VAR liTime       AS INT  NO-UNDO. 
DEF VAR lcOper       AS CHAR NO-UNDO. 

DEF BUFFER bEvent     FOR CoEvent.
DEF BUFFER bSman      FOR Salesman. 
DEF BUFFER bEventCust FOR Customer.

IF icFile > "" THEN ASSIGN
   lcSesNum               = SESSION:NUMERIC-FORMAT
   SESSION:NUMERIC-FORMAT = "EUROPEAN".
   
FORM "Qty:" AT 2 oiCount FORMAT ">>,>>>,>>9" 
WITH NO-LABELS OVERLAY ROW 12 CENTERED TITLE " Collecting "
   FRAME fQty.


FUNCTION fCollect RETURNS LOGICAL.

   CREATE ttEvent.
   ASSIGN ttEvent.EventId  = CoEvent.CoEventId        
          ttEvent.Salesman = CoEvent.Salesman
          ttEvent.Amount   = CoEvent.CommAmt
          ttEvent.CoRuleID = CoEvent.CoRuleID.

   IF CoEvent.Salesman > "" THEN DO:
      FIND Salesman WHERE 
           Salesman.Brand    = gcBrand AND
           Salesman.Salesman = CoEvent.Salesman 
         NO-LOCK NO-ERROR.
      IF AVAILABLE Salesman THEN 
         ttEvent.Reseller = Salesman.Reseller.
   END.         
   
   IF ttEvent.Reseller < icReseller1 OR
      ttEvent.Reseller > icReseller2 
   THEN DO:
      DELETE ttEvent.
      RETURN FALSE.
   END.
   
   /* check from rule definition the base of commission */
   FIND CoRule NO-LOCK WHERE
        CoRule.Brand    = gcBrand AND
        CoRule.CoRuleID = CoEvent.CoRuleID NO-ERROR.

   IF AVAILABLE CoRule THEN DO:

      ASSIGN lcText      = ""
             liEventCust = 0.

      IF LOOKUP(CoEvent.HostTable,"MCLI,FCLI") > 0 THEN DO:
         ttEvent.CLI = ENTRY(1,CoEvent.HostKey,"/").
         
         IF NUM-ENTRIES(CoEvent.HostKey,"/") > 1 
         THEN liEventCust = INTEGER(ENTRY(2,CoEvent.HostKey,"/")) NO-ERROR.
         
         IF NUM-ENTRIES(CoEvent.HostKey,"/") > 2 
         THEN liMsSeq = INTEGER(ENTRY(3,CoEvent.HostKey,"/")) NO-ERROR.
         
      END.
      
      IF liEventCust > 0 THEN DO:
         FIND bEventCust WHERE bEventCust.CustNum = liEventCust 
         NO-LOCK NO-ERROR.
         IF AVAILABLE bEventCust THEN ttEvent.EventCust = bEventCust.CustName.
      END.

      IF ttEvent.CLI > "" AND liMsSeq > 0 THEN DO:
      
         ldActDate = 0.
         FIND MobSub WHERE MobSub.CLI = ttEvent.CLI NO-LOCK NO-ERROR.
         IF AVAILABLE MobSub AND MobSub.MsSeq = liMsSeq THEN ASSIGN
            ttEvent.CLIType = MobSub.CLIType
            ldActDate       = MobSub.ActivationTS.
            
         ELSE FOR EACH MsOwner NO-LOCK WHERE
                       MsOwner.CLI     = ttEvent.CLI AND
                       MsOwner.CustNum = liEventCust AND
                       MsOwner.MsSeq   = liMsSeq
              BY MsOwner.TsBeg:
              
                 ASSIGN ldActDate       = MsOwner.TsBeg       
                        ttEvent.CLIType = MsOwner.CLIType.
                 LEAVE.
         END. 

         IF ldActDate > 0 THEN DO:
            fSplitTs(ldActDate,
                     OUTPUT ttEvent.ActDate,
                     OUTPUT liTime).
         END.

         ttEvent.PrevOper = fPrevOperator(liMsSeq).
         
         FOR FIRST Order NO-LOCK USE-INDEX MsSeq WHERE
                   Order.MsSeq      = liMsSeq AND
                   Order.StatusCode = "6" AND
                   Order.OrderType  < 2:
            ttEvent.OrdChannel = Order.OrderChannel.
                   
            fSplitTS(Order.CrStamp,
                     OUTPUT ttEvent.OrdDate,
                     OUTPUT liTime).
         END.
       
      END. 

   END. 

   /* to summary */
   IF ttEvent.PrevOper = "sonera" 
   THEN lcOper = ttEvent.PrevOper.
   ELSE lcOper = "Others".
   
   FIND FIRST ttSum WHERE
              ttSum.PrevOper = lcOper        AND
              ttSum.CoAmt    = CoEvent.CoAmt AND
              ttSum.CoRuleID = CoEvent.CoRuleID NO-ERROR.
   IF NOT AVAILABLE ttSum THEN DO:
      CREATE ttSum.
      ASSIGN ttSum.PrevOper = lcOper
             ttSum.CoAmt    = CoEvent.CoAmt
             ttSum.CoRuleID = CoEvent.CoRuleID.
   END.
   ASSIGN ttSum.ActQty = ttSum.ActQty + 1
          ttSum.Amount = ttSum.Amount + ttEvent.Amount.
            
   
   ASSIGN oiCount = oiCount + 1.

   IF oiCount < 100 OR oiCount MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISPLAY oiCount WITH FRAME fQty.
   END.

END FUNCTION.


PAUSE 0.
VIEW FRAME fQty. 

IF idtPaymDate1 = ? AND idtPaymDate2 NE ? THEN idtPaymDate1 = 01/01/1990.

/* collect events */
FOR EACH CoEvent NO-LOCK USE-INDEX CalcDate WHERE
         CoEvent.Brand     = gcBrand       AND
         CoEvent.CalcDate >= idtCalcDate1  AND
         CoEvent.CalcDate <= idtCalcDate2  AND
         CoEvent.Salesman >= icSalesman1   AND
         CoEvent.Salesman <= icSalesman2   AND
         CoEvent.CustNum  >= iiCustNum1    AND
         CoEvent.CustNum  <= iiCustNum2    AND
         LOOKUP(CoEvent.HostTable,"MCLI,FCLI") > 0:

      IF iiCORuleID > 0 AND CoEvent.CORuleID NE iiCORuleID THEN NEXT.
      
      IF idtPaymDate1 = ? THEN DO:
         IF CoEvent.PaymDate NE ? THEN NEXT.
      END.
      ELSE DO:
         IF CoEvent.PaymDate = ? OR 
            CoEvent.PaymDate < idtPaymDate1 OR
            CoEvent.PaymDate > idtPaymDate2
         THEN NEXT.
      END.
      
      fCollect().      

END.   


HIDE FRAME fQty NO-PAUSE. 

OUTPUT STREAM tul TO VALUE(icFile).

PUT STREAM tul UNFORMATTED
     "Previous Operator"    CHR(9)
     "Activated"            CHR(9)
     "Commission Per CLI"   CHR(9)
     "Total Commission EUR" CHR(9)
     "Rule"                 SKIP.

FOR EACH ttSum:

   PUT STREAM tul UNFORMATTED
      ttSum.PrevOper       CHR(9)
      ttSum.ActQty         CHR(9)
      ttSum.CoAmt          CHR(9)
      ttSum.Amount         CHR(9).
      
   FIND CoRule WHERE CoRule.CoRuleID = ttSum.CoRuleID NO-LOCK NO-ERROR.
   IF AVAILABLE CoRule THEN PUT STREAM tul UNFORMATTED 
      CoRule.RuleDesc.

   PUT STREAM tul UNFORMATTED SKIP.

   ACCUMULATE ttSum.ActQty (TOTAL)
              ttSum.Amount (TOTAL).
END.

PUT STREAM tul UNFORMATTED
   "Total"                    CHR(9)
   (ACCUM TOTAL ttSum.ActQty) CHR(9)
                              CHR(9)
   (ACCUM TOTAL ttSum.Amount) SKIP(1).
   
PUT STREAM tul UNFORMATTED
      "Salesman"           CHR(9)
      "Customer"           CHR(9)
      "CLI"                CHR(9)
      "CLI Type"           CHR(9)
      "Previous  Operator" CHR(9)
      "Commission EUR"     CHR(9)
      "Channel"            CHR(9)
      "Order Date"         CHR(9)
      "Activation Date"    CHR(9)
      "Rule"               SKIP.

FOR EACH ttEvent
BREAK BY ttEvent.Reseller
      BY ttEvent.Salesman
      BY ttEvent.CLI:

   CREATE ttMark.
   ttMark.EventId = ttEvent.EventId.
            
   PUT STREAM tul UNFORMATTED
      ttEvent.Salesman          CHR(9)
      ttEvent.EventCust         CHR(9)
      ttEvent.CLI               CHR(9)
      ttEvent.CLIType           CHR(9)
      ttEvent.PrevOper          CHR(9)
      ttEvent.Amount            CHR(9)
      ttEvent.OrdChannel        CHR(9)
      STRING(ttEvent.OrdDate,"99.99.9999") CHR(9)
      STRING(ttEvent.ActDate,"99.99.9999") CHR(9).
      
   FIND CoRule WHERE CoRule.CoRuleID = ttEvent.CoRuleID NO-LOCK NO-ERROR.
   IF AVAILABLE CoRule THEN PUT STREAM tul UNFORMATTED 
      CoRule.RuleDesc.
      
   PUT STREAM tul UNFORMATTED SKIP.
      
   
   IF LAST-OF(ttEvent.Salesman) THEN DO:              

   END.  /* last-of Salesman */

   IF LAST-OF(ttEvent.Reseller) AND 
      ttEvent.Reseller NE ""    
   THEN DO:              

   END.  /* last-of reseller */

   /* grand total  */
   IF LAST(ttEvent.Salesman) THEN DO:

   END.

END. 

OUTPUT STREAM tul CLOSE.
   
SESSION:NUMERIC-FORMAT = lcSesNum.


