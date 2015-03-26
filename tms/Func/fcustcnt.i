/* fcustcnt.i       24.09.02/aam
   Update customer's various counters

   changed:         09.10.02/jp Added new function: fCallCounter
                    28.01.08 kl update SaldoCounter.Qty

*/

DEF TEMP-TABLE ttSaldoCounter NO-UNDO
   LIKE SaldoCounter.

/* update counters */
FUNCTION fCustCount RETURNS LOGICAL
   (iCustNum AS INT,
    iType    AS CHAR,
    iAmt     AS DEC). 

   IF iType = "" OR iAmt = 0
   THEN RETURN FALSE. 

   FIND CustCount EXCLUSIVE-LOCK WHERE
        CustCount.CustNum = iCustNum NO-ERROR.

   IF NOT AVAILABLE CustCount THEN DO:
      CREATE CustCount.
      ASSIGN CustCount.CustNum = iCustNum.
   END.

   CASE iType:
   WHEN "UB"    THEN CustCount.Unbilled = CustCount.Unbilled + iAmt. 
   END CASE. 

   RETURN TRUE. 

END FUNCTION.

/* get counters for display */
FUNCTION fGetCustCount RETURNS DECIMAL
   (iCustNum AS INT,
    iType    AS CHAR). 

   DEF VAR ldCustCountAmt AS DEC NO-UNDO. 

   IF iType = ""  THEN RETURN 0.00. 

   IF NOT AVAILABLE CustCount OR
      CustCount.CustNum NE iCustNum 
   THEN FIND CustCount NO-LOCK WHERE
             CustCount.CustNum = iCustNum NO-ERROR.

   IF NOT AVAILABLE CustCount THEN RETURN 0.00. 

   CASE iType:
   WHEN "UB"    THEN ldCustCountAmt = CustCount.Unbilled. 
   OTHERWISE         ldCustCountAmt = 0.
   END CASE. 

   RETURN ldCustCountAmt. 

END FUNCTION.


FUNCTION fCallCount RETURNS LOGICAL
( INPUT iiMSSeq  as INT,
  INPUT iiPeriod AS INT,
  INPUT idePrice AS DEC).

   UPDSaldoCounter:
   REPEAT:
      FIND FIRST SaldoCounter WHERE
           SaldoCounter.MSSeq   = iiMSSeq   AND 
           SaldoCounter.Period  = iiPeriod 
      EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

      IF locked(SaldoCounter) THEN DO:
         PAUSE 1 NO-MESSAGE.
         NEXT UPDSaldoCounter.
      END.

      ELSE LEAVE UPDSaldoCounter.
   END.   

   IF NOT AVAILABLE SaldoCounter THEN DO:
      CREATE SaldoCounter.
      ASSIGN SaldoCounter.MSSeq   = iiMSSeq  
             SaldoCounter.Period  = iiPeriod.
   END.

   ASSIGN
      SaldoCounter.Amt = SaldoCounter.Amt + idePrice
      SaldoCounter.Qty = SaldoCounter.Qty + 1.

   RELEASE SaldoCounter.

   RETURN TRUE. 

END FUNCTION.

FUNCTION fTempTableCallCount RETURNS LOGICAL
( INPUT iiMSSeq  as INT,
  INPUT iiPeriod AS INT,
  INPUT idePrice AS DEC).

   FIND FIRST ttSaldoCounter WHERE
              ttSaldoCounter.MSSeq   = iiMSSeq   AND 
              ttSaldoCounter.Period  = iiPeriod NO-ERROR.

   IF NOT AVAILABLE ttSaldoCounter THEN DO:
      CREATE ttSaldoCounter.
      ASSIGN ttSaldoCounter.MSSeq   = iiMSSeq  
             ttSaldoCounter.Period  = iiPeriod.
   END.

   ASSIGN
      ttSaldoCounter.Amt = ttSaldoCounter.Amt + idePrice
      ttSaldoCounter.Qty = ttSaldoCounter.Qty + 1.

   RETURN TRUE. 

END FUNCTION.

FUNCTION fSaldoCounter2Temp RETURNS LOGICAL
   (INPUT iiMsSeq     AS INT,
    INPUT idaFromDate AS DATE,
    INPUT idaToDate   AS DATE):
   
   FOR EACH SaldoCounter NO-LOCK WHERE
            SaldoCounter.MsSeq  = iiMsSeq  AND 
            SaldoCounter.Period >= YEAR(idaFromDate) * 100 + 
                                   MONTH(idaFromDate) AND 
            Saldocounter.Period <= YEAR(idaToDate) * 100 + 
                                   MONTH(idaToDate):

      IF CAN-FIND(FIRST ttSaldoCounter WHERE
                        ttSaldoCounter.MsSeq = SaldoCounter.MsSeq AND
                        ttSaldoCounter.Period = SaldoCounter.Period)
      THEN NEXT. 
      
      CREATE ttSaldoCounter.
      BUFFER-COPY SaldoCounter TO ttSaldoCounter.
      ASSIGN 
         ttSaldoCounter.Amt = 0
         ttSaldoCounter.Qty = 0.
   END.
    
END FUNCTION.    

FUNCTION fTemp2SaldoCounter RETURNS LOGICAL:

   DEF BUFFER bDblCounter FOR SaldoCounter.

   FOR EACH ttSaldoCounter:
   
      GetSaldoCounter:
      REPEAT:
         FIND FIRST SaldoCounter WHERE
                    SaldoCounter.MSSeq  = ttSaldoCounter.MSSeq   AND 
                    SaldoCounter.Period = ttSaldoCounter.Period 
         EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

         IF LOCKED(SaldoCounter) THEN DO:
            PAUSE 2 NO-MESSAGE.
            NEXT GetSaldoCounter.
         END.
         ELSE LEAVE GetSaldoCounter.
      END.   

      IF NOT AVAILABLE SaldoCounter THEN CREATE SaldoCounter.
      ELSE DO:
         /* remove possible double counter */
         FIND FIRST bDblCounter WHERE
                    bDblCounter.MSSeq   = ttSaldoCounter.MSSeq AND
                    bDblCounter.Period  = ttSaldoCounter.Period AND
                    RECID(bDblCounter) NE RECID(SaldoCounter)
         NO-LOCK NO-ERROR.
         IF AVAILABLE bDblCounter THEN DO:
            FIND CURRENT bDblCounter EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF AVAILABLE bDblCounter THEN DELETE bDblCounter.
         END.
      END.    

      BUFFER-COPY ttSaldoCounter TO SaldoCounter.

      RELEASE SaldoCounter.
   END.
   
   EMPTY TEMP-TABLE ttSaldoCounter.
   
   RETURN TRUE. 

END FUNCTION.




