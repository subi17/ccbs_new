/* fbankdata.i      25.04.04/aam
   convert bankaccount into standard form &
   check last digit of bank account
   
   changes:         26.01.05/aam fCustBankAcc added 
                    16.11.06/aam fCheckBankAcc for Spain
   
*/
{timestamp.i}
{tmsconst.i}
{date.i}  

/* convert into data form */
FUNCTION fBankAcc2Data RETURNS CHARACTER
   (icBankAcc AS CHAR).

   DEF VAR lcBankData AS CHAR NO-UNDO.
   DEF VAR liPos      AS INT  NO-UNDO. 
      
   lcBankData = REPLACE(icBankAcc,"-","").
     
   IF LENGTH(lcBankData) = 14 THEN RETURN lcBankData.
   
   /* different rule for some banks */
   IF SUBSTRING(icBankAcc,1,1) = "4" OR
      SUBSTRING(icBankAcc,1,1) = "5"
   THEN liPos = 7.
   ELSE liPos = 6. 
   
   /* add zeros to obtain full length */
   lcBankData = SUBSTRING(lcBankData,1,liPos) +
                FILL("0",14 - LENGTH(lcBankData)) +
                SUBSTRING(lcBankData,liPos + 1).

   RETURN lcBankData.
   
END FUNCTION.

/* convert from data form into visible form */
FUNCTION fBankData2Acc RETURNS CHARACTER
   (icBankData AS CHAR).

   DEF VAR liPos    AS INT  NO-UNDO. 
   DEF VAR lcOffice AS CHAR NO-UNDO. 
      
   /* not in data form */
   IF INDEX(icBankData,"-") > 0 OR
      LENGTH(icBankData) NE 14
   THEN RETURN icBankData.

   lcOffice = SUBSTRING(icBankData,7).
   
   /* remove leading zeros, zeros inside of account will remain (banks 4/5) */
   REPEAT:
      IF SUBSTRING(lcOffice,1,1) = "0" 
      THEN lcOffice = SUBSTRING(lcOffice,2).
      ELSE LEAVE.
   END. 
   
   icBankData = SUBSTRING(icBankData,1,6) + "-" + lcOffice.
            
   RETURN icBankData.
   
END FUNCTION.

/* check if last digit matches */
FUNCTION fCheckFinBankAcc RETURNS LOGICAL
   (icBankAcc AS CHAR).

   DEF VAR liBCnt AS INT NO-UNDO.
   DEF VAR liBChk AS INT NO-UNDO.
   DEF VAR liBNbr AS INT NO-UNDO.
   
   /* must be in data format */ 
   IF INDEX(icBankAcc,"-") > 0 OR LENGTH(icBankAcc) NE 14
   THEN icBankAcc = fBankAcc2Data(icBankAcc).
   
   liBChk = 0.
   
   DO liBCnt = 13 TO 1 BY -1:
     
      /* only numbers */
      IF INDEX("0123456789",SUBSTRING(icBankAcc,liBCnt,1)) = 0 THEN 
      RETURN FALSE.
      
      liBNbr = INTEGER(SUBSTRING(icBankAcc,liBCnt,1)).
      
      /* factors are 2 and 1 in turns */      
      IF liBCnt MOD 2 NE 0 THEN liBNbr = liBNbr * 2. 
      
      /* numbers are counted individually */
      IF liBNbr > 9 
      THEN liBChk = liBChk + INTEGER(SUBSTRING(STRING(liBNbr),1,1)) +
                             INTEGER(SUBSTRING(STRING(liBNbr),2,1)).
      ELSE liBChk = liBChk + liBNbr.
   END.

   /* subtract from next multiple of 10 */
   liBChk = liBChk MOD 10.
   IF liBChk NE 0 THEN liBChk = 10 - liBChk. 

   RETURN (liBChk = INTEGER(SUBSTRING(icBankAcc,14,1))).   
   
END FUNCTION.


/* customer's bank account */
FUNCTION fCustBankAcc RETURNS CHARACTER.

   DEF VAR lcCustBank AS CHAR NO-UNDO.
   
   /* first check from dd-authorization */
   RUN nnsvte (Customer.CustNum,
               TODAY, 
               OUTPUT lcCustBank).
               
   /* if empty then from customer data */            
   IF lcCustBank = "" THEN lcCustBank = Customer.BankAcc.

   RETURN lcCustBank.

END FUNCTION.
 

FUNCTION fCheckIBANValue RETURN LOG
   (icBankAcc AS CHAR).

   DEF VAR ldeBankAcc AS DEC NO-UNDO.
   DEF VAR lcIbanVal AS CHAR NO-UNDO.
   DEF VAR ldeIbanCalRes1 AS DEC NO-UNDO.
   DEF VAR ldeIbanCalRes2 AS DEC NO-UNDO.

   IF NOT icBankAcc BEGINS "ES" THEN RETURN FALSE.

   ASSIGN
      lcIbanVal = SUBSTRING(icBankAcc,5) + "1428" + SUBSTRING(icBankAcc,3,2)
      ldeBankAcc = DEC(lcIbanVal)
      ldeIbanCalRes1 = ldeBankAcc / 97
      ldeIbanCalRes2 = (97 * TRUNCATE(ldeIbanCalRes1,0)) NO-ERROR.

   IF ERROR-STATUS:ERROR THEN RETURN FALSE.

   IF (ldeBankAcc - ldeIbanCalRes2) EQ 1 THEN RETURN TRUE.
   ELSE RETURN FALSE.

END FUNCTION.


FUNCTION fCheckBankAcc RETURNS LOGICAL
   (icBankAcc AS CHAR).

   DEF VAR liBCnt   AS INT NO-UNDO.
   DEF VAR liBChk   AS INT NO-UNDO.
   DEF VAR liBNbr   AS INT NO-UNDO.
   DEF VAR liWeight AS INT NO-UNDO EXTENT 10 
      INIT [1,2,4,8,5,10,9,7,3,6].


   IF LENGTH(icBankAcc) NE 24 OR NOT icBankAcc BEGINS "ES" THEN RETURN FALSE.    

   /* IBAN Check */
   IF NOT fCheckIBANValue(icBankAcc) THEN RETURN FALSE.
   
   liBChk = 0.
          
   /* first 8 numbers have their own control digit (9. digit) */
   DO liBCnt = 5 TO 12:
     
      /* only numbers */
      IF INDEX("0123456789",SUBSTRING(icBankAcc,liBCnt,1)) = 0 THEN 
      RETURN FALSE.
      
      ASSIGN liBNbr = INTEGER(SUBSTRING(icBankAcc,liBCnt,1))
             liBChk = liBChk + liBNbr * liWeight[liBCnt - 2].
   END.
      
   liBChk = 11 - (liBChk MOD 11).
   IF liBChk = 10 THEN liBChk = 1.
   IF liBChk = 11 THEN liBChk = 0.

   IF SUBSTRING(icBankAcc,13,1) NE STRING(liBChk) THEN RETURN FALSE.

   liBChk = 0.
   
   /* last 10 numbers have their own control digit (10. digit) */
   DO liBCnt = 15 TO 24:
     
      /* only numbers */
      IF INDEX("0123456789",SUBSTRING(icBankAcc,liBCnt,1)) = 0 THEN 
      RETURN FALSE.
      
      ASSIGN liBNbr = INTEGER(SUBSTRING(icBankAcc,liBCnt,1))
             liBChk = liBChk + liBNbr * liWeight[liBCnt - 14].
   END.
      
   liBChk = 11 - (liBChk MOD 11).
   IF liBChk = 10 THEN liBChk = 1.
   IF liBChk = 11 THEN liBChk = 0.

   IF SUBSTRING(icBankAcc,14,1) NE STRING(liBChk) THEN RETURN FALSE.
    
   RETURN TRUE.
   
END FUNCTION.

FUNCTION fChangeCharNum RETURN LOGICAL
   (INPUT icInput AS CHAR,
    OUTPUT lcOutput AS CHAR).

   DEF VAR liInd AS INT NO-UNDO.
   DEF VAR liVal AS INT NO-UNDO.
   DEF VAR lcLetters AS CHAR NO-UNDO INIT
    "A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z".

   DO liInd = 1 TO LENGTH(icInput):
      liVal = LOOKUP(SUBSTRING(icInput,liInd,1),lcLetters).
      IF liVal > 0 THEN
         lcOutput = lcOutput + STRING(9 + liVal).
      ELSE
         lcOutput = lcOutput + SUBSTRING(icInput,liInd,1).
   END.

   RETURN True.

END FUNCTION.

FUNCTION fCalculateMandate RETURNS LOGICAL
   (INPUT iiMsSeq AS INT,
    INPUT idaOrderDate AS DATE,
    INPUT iiCustNum AS INT, 
    OUTPUT lcMandate AS CHAR).

   DEF BUFFER Order FOR Order.
   DEF BUFFER Customer FOR Customer.

   DEF VAR lcCtry AS CHAR NO-UNDO INIT "142800".
   DEF VAR limod AS DEC NO-UNDO.
   DEF VAR lcCustId AS CHAR NO-UNDO. 
   DEF VAR lcContId AS CHAR NO-UNDO.
   DEF VAR lcBusCode AS CHAR NO-UNDO INIT "001".
   DEF VAR lcInput AS CHAR NO-UNDO.
   DEF VAR lcLastMItems AS CHAR NO-UNDO. 
   DEF VAR ldeMVal1 AS DEC NO-UNDO.
   DEF VAR ldeMRes1 AS DEC NO-UNDO.
   DEF VAR ldeMRes2 AS DEC NO-UNDO.
   DEF VAR ldeMRes3 AS DEC NO-UNDO.
   DEF VAR ldaDateOrder AS DATE NO-UNDO.
   DEF VAR liTimeOrder AS INT NO-UNDO.
   DEF VAR lcCustomerId AS CHAR NO-UNDO. 
   DEF VAR lcContractId AS CHAR NO-UNDO.
   DEF VAR ldOrDate AS DEC NO-UNDO. 
   DEF VAR lcOrderDate AS CHAR NO-UNDO. 

   /* Contract ID */
   FIND FIRST Order NO-LOCK WHERE
              Order.MsSeq = iiMsSeq AND
             (Order.OrderType EQ {&ORDER_TYPE_NEW} OR
              Order.OrderType EQ {&ORDER_TYPE_MNP}) NO-ERROR.
   IF NOT AVAIL Order THEN RETURN FALSE.

   /* Customer ID */
   FIND FIRST Customer NO-LOCK WHERE
              Customer.CustNum = iiCustNum NO-ERROR.
   IF NOT AVAIL Customer THEN RETURN FALSE.

   /* lcLastMItems */
   ASSIGN
      lcCustomerId = FILL("0",9 - LENGTH(Customer.OrgId)) + Customer.OrgId
      lcContractId = FILL("0",8 - LENGTH(Order.ContractID)) + Order.ContractID
      lcLastMItems = lcCustomerId + lcContractId
      lcOrderDate = STRING(YEAR(idaOrderDate) MOD 100,"99") + 
                    STRING(MONTH(idaOrderDate),"99") + 
                    STRING(DAY(idaOrderDate),"99").

   /* Replace characters with digits */
   fChangeCharNum(Customer.OrgId,
                  OUTPUT lcCustId).
   
   fChangeCharNum(Order.ContractID,
                  OUTPUT lcContId).

   /* Control digits calculation */
   ASSIGN   
      lcInput = lcCustId + lcContId + lcCtry
      ldeMVal1 = DEC(lcInput)
      ldeMRes1 = ldeMVal1 / 97
      ldeMRes2 = 97 * TRUNCATE(ldeMRes1,0)
      ldeMRes3 = ldeMVal1 - ldeMRes2.
   limod = 98 - INT(ldeMRes3).

   IF LENGTH(STRING(limod)) = 1 THEN
      lcMandate = UPPER("ES0" + STRING(limod) + lcBusCode + lcLastMItems + lcOrderDate).
   ELSE
      lcMandate = UPPER("ES" + STRING(limod) + lcBusCode + lcLastMItems + lcOrderDate).

   RETURN TRUE.
END FUNCTION.

FUNCTION fGetOrderMandateId RETURN LOGICAL
   (BUFFER Order FOR Order,
    OUTPUT ocMandateId AS CHAR,
    OUTPUT odaMandateDate AS DATE):

   DEF VAR ldaDate AS DATE NO-UNDO. 
   DEF VAR liTime AS INT NO-UNDO. 

   DEF BUFFER OrderAction FOR OrderAction.

   IF NOT AVAIL Order OR Order.CliType BEGINS "TARJ" THEN
      RETURN FALSE.

   FIND FIRST OrderAction NO-LOCK WHERE
              OrderAction.Brand     = Order.Brand AND
              OrderAction.OrderId   = Order.OrderId AND
              OrderAction.ItemType  = "Mandate" NO-ERROR.
 
   IF AVAIL OrderAction THEN DO:
      fSplitTS(Order.CrStamp,ldaDate,liTime).
      ASSIGN
         ocMandateId = OrderAction.ItemKey
         odaMandateDate = ldaDate.
   END.
   /* Added an X for MandateID YDR-1553 */
   ELSE ASSIGN
      ocMandateId = STRING(Order.CustNum) + "X" +
                      FILL("0",29 - LENGTH(STRING(Order.CustNum)))
      odaMandateDate = 10/31/2009.

   RETURN TRUE.

END FUNCTION.

FUNCTION fCalcSepaBankAcc RETURNS CHARACTER
   (icBankAcc AS CHAR).

   DEF VAR lcIban1 AS CHAR NO-UNDO.
   DEF VAR lcIban2 AS CHAR NO-UNDO.
   DEF VAR lcIbanFinal AS CHAR NO-UNDO.
   DEF VAR lcRes AS CHAR NO-UNDO.
   DEF VAR liPos AS INT NO-UNDO.
   DEF VAR liFinalValue AS INT NO-UNDO.
   DEF VAR ldeIbanVal AS DEC NO-UNDO.
   DEF VAR ldeIbanRes1 AS DEC NO-UNDO.
   DEF VAR ldeIbanRes2 AS DEC NO-UNDO.
   DEF VAR ldeIbanRes3 AS DEC NO-UNDO.

   ASSIGN lcIban1 = icBankAcc
          lcIban2 = lcIban1 + "142800"
          ldeIbanVal = DEC(lcIban2)
          ldeIbanRes1 = ldeIbanVal / 97
          lcRes = STRING(ldeIbanRes1)
          liPos = INDEX(lcRes,'.')
          ldeIbanRes2 = (97 * DEC(SUBSTRING(STRING(ldeIbanRes1),1,liPos - 1)))
          ldeIbanRes3 = ldeIbanVal - ldeIbanRes2.


   liFinalValue = 98 - INT(ldeIbanRes3).
   IF LENGTH(STRING(liFinalValue)) = 1 THEN
      lcIbanFinal = "ES0" + STRING(liFinalValue) + lcIban1.
   ELSE
      lcIbanFinal = "ES" + STRING(liFinalValue) + lcIban1.

   RETURN lcIbanFinal.

END FUNCTION.

/* Bank account is not allowed to change empty if postpaid subscriptions or
less than 40 days from latest postpaid termination / STC" */
FUNCTION fChkBankAccChange RETURNS LOGICAL
   (iiCustNum AS INT).
   DEF VAR ldeDate AS DEC NO-UNDO.

   /* any postpaid subscriptions */
   IF CAN-FIND (FIRST mobsub WHERE mobsub.Brand = gcBrand AND
                                   mobsub.custnum = iiCustNum AND
                                   mobsub.paytype = FALSE NO-LOCK) THEN
      RETURN FALSE.

   ldeDate = fDate2TS(TODAY - 40).

   /* is terminated postpaid subscription during last 40 days */
   FOR EACH MsRequest WHERE MsRequest.Brand = gcBrand AND
                            MsRequest.Reqtype = 18 AND
                            MsRequest.CustNum = iiCustNum AND
                            MsRequest.ActStamp > ldeDate AND
                            MsRequest.ReqStatus = 2 NO-LOCK:
      IF CAN-FIND (FIRST msOwner WHERE
                         msOwner.Brand = gcBrand AND
                         msOwner.CLI = MsRequest.CLI AND
                         msOwner.tsEnd > ldeDate AND
                         msOwner.MsSeq = MsRequest.MsSeq AND
                         msOwner.paytype = FALSE NO-LOCK) THEN
         RETURN FALSE.
   END.
   /* is STC from postpaid done during last 40 days */
   IF CAN-FIND (FIRST MsRequest WHERE MsRequest.Brand = gcBrand AND
                                      MsRequest.Reqtype = 0 AND
                                      MsRequest.CustNum = iiCustNum AND
                                      MsRequest.ActStamp > ldeDate AND
                                      MsRequest.ReqStatus = 2 AND
                                      MsRequest.ReqCParam1 BEGINS "CONT"
                                      NO-LOCK) THEN
      RETURN FALSE.
   /* otherwise account can be changed empty */
   RETURN TRUE.
END FUNCTION.

