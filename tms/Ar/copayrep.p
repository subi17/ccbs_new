/* ------------------------------------------------------
  MODULE .......: COPAYREP
  FUNCTION .....: Commission event report
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 13.02.03
  MODIFIED .....: 21.11.03/aam new structure
                  19.04.04/aam to excel
                  09.08.04/aam iiCORuleID, ttEvent.EventCust
                  30.08.04/aam brand
  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/cparam2.i}
{Syst/utumaa.i}

DEF TEMP-TABLE ttMark NO-UNDO
    FIELD EventID AS INT.

DEF TEMP-TABLE ttEvent NO-UNDO
    FIELD EventId   AS INT
    FIELD RuleID    AS INT
    FIELD Target    AS CHAR
    FIELD Reseller  AS CHAR
    FIELD Salesman  AS CHAR
    FIELD CustNum   AS INT
    FIELD CommPoint AS CHAR
    FIELD CommBase  AS CHAR
    FIELD EventCust AS CHAR
    FIELD BasisType AS INT 
    FIELD CalcDate  AS DATE
    FIELD FromDate  AS DATE
    FIELD ToDate    AS DATE
    FIELD PaymDate  AS DATE
    FIELD CoAmt     AS DEC
    FIELD CoPerc    AS DEC 
    FIELD Amount    AS DEC
    FIELD BaseAmt   AS DEC
    INDEX Target Target FromDate CommBase. 

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

DEF VAR viiva1 AS CHAR      FORMAT "x(112)" NO-UNDO.
DEF VAR viiva2 LIKE viiva1.
DEF VAR viiva3 LIKE viiva1.
DEF VAR viiva4 LIKE viiva1.
DEF VAR sl     AS INT                       NO-UNDO.
DEF VAR rl     AS INT                       NO-UNDO.
DEF VAR lev    AS INT                       NO-UNDO INIT 112.
DEF VAR otsi   AS CHAR EXTENT 39            NO-UNDO.

DEF VAR lcDateHeader AS CHAR NO-UNDO.
DEF VAR lcText       AS CHAR NO-UNDO. 
DEF VAR lcSesNum     AS CHAR NO-UNDO.
DEF VAR liEventCust  AS INT  NO-UNDO. 

DEF BUFFER bEvent     FOR CoEvent.
DEF BUFFER bSman      FOR Salesman. 
DEF BUFFER bEventCust FOR Customer.

ASSIGN 
    viiva1   = FILL("=",lev)
    viiva2   = FILL("=",lev)
    viiva3   = FILL("-",lev)
    viiva4   = FILL("-",lev).

IF icFile > "" THEN ASSIGN
   lcSesNum               = SESSION:NUMERIC-FORMAT
   SESSION:NUMERIC-FORMAT = "EUROPEAN".
   
form header
   viiva1 AT 1 SKIP
   ynimi at 1 FORMAT "x(30)" 
      "COMMISSION REPORT" at 40
      "Page" at 102  
      sl FORMAT "ZZZZ9" SKIP
   lcDateHeader AT 40 FORMAT "X(30)"
      pvm FORMAT "99.99.9999" at 103 SKIP
   viiva2 at 1 SKIP
   "CalcDate"     AT 5
   "Point"        AT 14
   "Event"        AT 20
   "Time period"  AT 46
   "Base Amt"     TO 74
   "Fixed"        TO 84
   "%"            TO 91
   "Commission"   TO 102
   "PaymDate"     AT 104
   SKIP
   viiva3
   WITH WIDTH 112 NO-LABELS NO-BOX FRAME sivuotsi.

FORM "Qty:" AT 2 oiCount FORMAT ">>,>>>,>>9" 
WITH NO-LABELS OVERLAY ROW 12 CENTERED TITLE " Collecting "
   FRAME fQty.


FUNCTION fCollect RETURNS LOGICAL.

   CREATE ttEvent.
   ASSIGN ttEvent.EventId  = CoEvent.CoEventId        
          ttEvent.RuleID   = CoEvent.CoRuleID
          ttEvent.Salesman = CoEvent.Salesman
          ttEvent.CustNum  = CoEvent.CustNum
          ttEvent.CalcDate = CoEvent.CalcDate
          ttEvent.FromDate = CoEvent.CommFrom
          ttEvent.ToDate   = CoEvent.CommTo 
          ttEvent.PaymDate = CoEvent.PaymDate
          ttEvent.CoAmt    = CoEvent.CoAmt
          ttEvent.CoPerc   = CoEvent.CoPerc
          ttEvent.Amount   = CoEvent.CommAmt
          ttEvent.BaseAmt  = CoEvent.BaseAmt.

   IF CoEvent.Salesman > "" THEN DO:
      ttEvent.Target = CoEvent.Salesman.
      FIND Salesman WHERE 
           Salesman.Brand    = gcBrand AND
           Salesman.Salesman = CoEvent.Salesman 
         NO-LOCK NO-ERROR.
      IF AVAILABLE Salesman THEN 
         ttEvent.Reseller = Salesman.Reseller.
   END.         
   ELSE ttEvent.Target = STRING(CoEvent.CustNum,"999999999").
   
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

      ttEvent.BasisType = CoRule.BasisType.
      
      CASE CoRule.CommPoint:
      WHEN 0 THEN ttEvent.CommPoint = "Order".
      WHEN 1 THEN ttEvent.CommPoint = "Bill.".
      WHEN 2 THEN ttEvent.CommPoint = "Paym.".
      END CASE. 

      ASSIGN lcText      = ""
             liEventCust = 0.

      CASE CoEvent.HostTable:
      WHEN "CoEvent" THEN DO:
            lcText = "Share from ".
            FIND FIRST bEvent NO-LOCK WHERE
                       bEvent.CoEventID = INTEGER(CoEvent.HostKey) NO-ERROR.
            IF AVAILABLE CoEvent THEN DO:
               FIND bSman NO-LOCK WHERE 
                    bSman.Brand    = gcBrand AND
                    bSman.Salesman = bEvent.Salesman NO-ERROR.
               lcText = lcText + IF AVAILABLE bSman
                                 THEN bSman.SmName
                                 ELSE bEvent.Salesman.
            END.
         END. 
      OTHERWISE DO:
            lcText = CoEvent.HostTable + " " + CoEvent.HostKey.
            IF CoEvent.HostTable = "Invoice" THEN DO:
               FIND Invoice WHERE Invoice.InvNum = INTEGER(CoEvent.HostKey)
               NO-LOCK NO-ERROR.
               IF AVAILABLE Invoice 
               THEN lcText = lcText + " Cust: " + STRING(Invoice.CustNum).
               /*
               IF CoEvent.CoTargID > 0
               THEN lcText = lcText + " CCN: " + STRING(CoEvent.CoTargID).
               */
               liEventCust = Invoice.CustNum.
            END.   
            ELSE IF LOOKUP(CoEvent.HostTable,"MCLI,FCLI") > 0 THEN DO:
               IF NUM-ENTRIES(CoEvent.HostKey,"/") > 1 
               THEN liEventCust = INTEGER(ENTRY(2,CoEvent.HostKey,"/"))
                                  NO-ERROR.
                
            END.
         END.
      END CASE.

      ttEvent.CommBase = lcText. 
      
      IF liEventCust > 0 THEN DO:
         FIND bEventCust WHERE bEventCust.CustNum = liEventCust 
         NO-LOCK NO-ERROR.
         IF AVAILABLE bEventCust THEN ttEvent.EventCust = bEventCust.CustName.
      END.

   END. 

   ASSIGN oiCount = oiCount + 1.

   IF oiCount < 100 OR oiCount MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISPLAY oiCount WITH FRAME fQty.
   END.

END FUNCTION.


FUNCTION fCheckPage RETURNS LOGIC 
    (iAddLine AS INT).

    IF icFile = "" AND rl + iAddLine >= skayt1 THEN DO:
                        
        IF sl > 0 THEN DO:
           {Syst/uprfeed.i rl}
        END.
        
        sl = sl + 1.

        VIEW STREAM tul FRAME sivuotsi.  
        rl = 6.

        RETURN TRUE.
    END.

    ELSE RETURN FALSE.
END.

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
         CoEvent.CustNum  <= iiCustNum2:

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

IF icFile > "" THEN DO:
   
   OUTPUT STREAM tul TO VALUE(icFile).
   
   PUT STREAM tul UNFORMATTED
      "Reseller"       CHR(9)
      "Rs Name"        CHR(9)
      "Salesman"       CHR(9)
      "Sm Name"        CHR(9)
      "Calc Date"      CHR(9)
      "Rule ID"        CHR(9)
      "Comm.Point"     CHR(9)
      "Event"          CHR(9)
      "Event Customer" CHR(9) 
      "Period From"    CHR(9)
      "Period To"      CHR(9)
      "Base Amt"       CHR(9)
      "Fixed"          CHR(9)
      "%"              CHR(9)
      "Commission"     CHR(9)
      "PaymDate"       SKIP.
END.

FOR EACH ttEvent
BREAK BY ttEvent.Reseller
      BY ttEvent.Target
      BY ttEvent.FromDate:

   CREATE ttMark.
   ttMark.EventId = ttEvent.EventId.

   IF FIRST-OF(ttEvent.Reseller) AND 
      ttEvent.Reseller > ""      AND
      icFile = ""
   THEN DO:

      /* each reseller to it's own page */
      fCheckPage(999).

      FIND Reseller NO-LOCK WHERE 
           Reseller.Brand    = gcBrand AND
           Reseller.Reseller = ttEvent.Reseller NO-ERROR.

      PUT STREAM tul UNFORMATTED
         "Reseller: " AT 1
         ttEvent.Reseller
         " "
         (IF AVAILABLE Reseller THEN Reseller.RsName ELSE "")
         SKIP(1).
      rl = rl + 2.
   END.      

   IF FIRST-OF(ttEvent.Target) AND icFile = "" THEN DO:

      /* each Customer to it's own page */
      IF ttEvent.Salesman = "" THEN DO:
         fCheckPage(999).

         FIND Customer WHERE Customer.CustNum = ttEvent.CustNum
            NO-LOCK NO-ERROR.
            
         PUT STREAM tul UNFORMATTED
            "Customer: " AT 1
            ttEvent.CustNum
            " "
            (IF AVAILABLE Customer THEN Customer.CustName ELSE "")
            SKIP(1).
            
         rl = rl + 2.
      END.
            
      ELSE DO:   

         fCheckPage(3).

         FIND Salesman NO-LOCK WHERE 
              Salesman.Brand    = gcBrand AND
              Salesman.Salesman = ttEvent.Salesman NO-ERROR.

         PUT STREAM tul UNFORMATTED
            "Salesman: " AT 3
            ttEvent.Salesman
            " "
            (IF AVAILABLE Salesman THEN Salesman.SmName ELSE "")
            SKIP.

         rl = rl + 1.
      END.
      
   END.

   IF icFile = "" THEN DO:
   
      fCheckPage(0).

      PUT STREAM tul 
      ttEvent.CalcDate       AT 5   FORMAT "99.99.99"
      ttEvent.CommPoint      AT 14  FORMAT "X(5)"
      ttEvent.CommBase       AT 20  FORMAT "X(25)"
      ttEvent.FromDate       AT 46  FORMAT "99.99.99"
      "-"
      ttEvent.ToDate                FORMAT "99.99.99"
      ttEvent.BaseAmt        TO 74  FORMAT "->>>>>>9.99"
      ttEvent.CoAmt          TO 84  FORMAT "->>>>9.99"
      ttEvent.CoPerc         TO 91  FORMAT ">>9.99"
      ttEvent.Amount         TO 102 FORMAT "->>>>>9.99"
      ttEvent.PaymDate       AT 104 FORMAT "99.99.99"
      SKIP.

      rl = rl + 1.

      ACCUMULATE ttEvent.Amount (TOTAL BY ttEvent.Target
                                       BY ttEvent.Reseller).

   END.

   ELSE DO:
      FIND Reseller WHERE 
           Reseller.Brand    = gcBrand AND
           Reseller.Reseller = ttEvent.Reseller NO-LOCK NO-ERROR.
      
      PUT STREAM tul UNFORMATTED
      ttEvent.Reseller          CHR(9)
      (IF AVAILABLE Reseller 
       THEN Reseller.RsName
       ELSE "")                 CHR(9).
       
      IF ttEvent.Salesman = "" THEN DO:
         FIND Customer WHERE Customer.CustNum = ttEvent.CustNum 
            NO-LOCK NO-ERROR.
         PUT STREAM tul UNFORMATTED
            ttEvent.CustNum  CHR(9)
            (IF AVAILABLE Customer
             THEN Customer.CustName
             ELSE "")        CHR(9).
      END.
      ELSE DO:
      
         FIND Salesman WHERE 
              Salesman.Brand    = gcBrand AND
              Salesman.Salesman = ttEvent.Salesman 
            NO-LOCK NO-ERROR.
            
         PUT STREAM tul UNFORMATTED
            ttEvent.Salesman   CHR(9)
            (IF AVAILABLE Salesman
             THEN Salesman.SmName
             ELSE "")          CHR(9).
      END.
      
      PUT STREAM tul UNFORMATTED 
      ttEvent.CalcDate          CHR(9)
      ttEvent.RuleID            CHR(9)
      ttEvent.CommPoint         CHR(9)
      ttEvent.CommBase          CHR(9)
      ttEvent.EventCust         CHR(9)
      ttEvent.FromDate          CHR(9)
      ttEvent.ToDate            CHR(9)
      ttEvent.BaseAmt           CHR(9)
      ttEvent.CoAmt             CHR(9)
      ttEvent.CoPerc            CHR(9)
      ttEvent.Amount            CHR(9)
      (IF ttEvent.PaymDate NE ?
       THEN STRING(ttEvent.PaymDate,"99.99.9999")
       ELSE "")
      SKIP.
      
   END.
   
   IF LAST-OF(ttEvent.Target) AND icFile = "" THEN DO:              

      fCheckPage(2).

      IF ttEvent.Salesman = "" 
      THEN PUT STREAM tul UNFORMATTED
         FILL("-",lev) AT 1 SKIP
         ttEvent.CustNum AT 1 " total".

      ELSE PUT STREAM tul UNFORMATTED
         FILL("-",lev - 2) AT 3 SKIP
         ttEvent.Salesman AT 3 " total".

      PUT STREAM tul 
         (ACCUM TOTAL BY ttEvent.Target ttEvent.Amount)
            TO 102 FORMAT "->>>>>>>>9.99"
         SKIP(1).

      rl = rl + 3.

   END.  /* last-of Salesman */

   IF LAST-OF(ttEvent.Reseller) AND 
      ttEvent.Reseller NE ""    AND
      icFile = ""
   THEN DO:              

      fCheckPage(2).

      PUT STREAM tul UNFORMATTED
         FILL("=",lev) AT 1 SKIP
         ttEvent.Reseller AT 1 " total".

      PUT STREAM tul 
         (ACCUM TOTAL BY ttEvent.Reseller ttEvent.Amount)
            TO 102 FORMAT "->>>>>>>>9.99"
         SKIP.

      rl = rl + 2.

   END.  /* last-of reseller */

END. 

IF icFile = "" THEN DO:
   {Syst/uprfeed.i rl}
END.

ELSE DO:
   OUTPUT STREAM tul CLOSE.
   
   SESSION:NUMERIC-FORMAT = lcSesNum.
END.

