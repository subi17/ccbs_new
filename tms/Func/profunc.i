/* ----------------------------------------------------------------------
  MODULE .......: profunc.i
  TASK .........: Functions for handling Yoigo PRO related functionality
                  Reference: YPRO-project
  APPLICATION ..: tms
  AUTHOR .......:
  VERSION.......:
  CREATED ......: 24.5.2017
  CHANGED ......:
  ------------------------------------------------------------------------*/
 
{Syst/tmsconst.i}
&IF "{&YOIGOPROFUNC_I}" NE "YES"
&THEN
&GLOBAL-DEFINE YOIGOPROFUNC_I YES
{Func/fmakemsreq.i}
{Func/orderfunc.i}
{Func/femailinvoice.i}
{Func/email.i}
{Func/fixedlinefunc.i}

DEF TEMP-TABLE ttOrderList
   FIELD OrderID AS INT
   INDEX OrderID OrderID DESC.
DEF STREAM soutfile.

FUNCTION fIsPro RETURNS LOGICAL
   (icCategory AS CHAR):

   FIND FIRST CustCat NO-LOCK where
              CustCat.Brand EQ Syst.Parameters:gcbrand AND
              CustCat.Category EQ icCategory NO-ERROR.
              
   IF AVAIL CustCat AND Custcat.pro THEN RETURN TRUE.
   RETURN FALSE.
END.

/*tested, ok*/
/*Function seeks COFF order for given Msrequest.
If the order is not found the function returns an error code.*/
FUNCTION fFindCOFFOrder RETURNS CHAR
   (iiMsSeq AS INT):
   DEF BUFFER bOrder FOR Order.

   EMPTY TEMP-TABLE ttOrderList.
   FOR EACH  bOrder NO-LOCK WHERE
             bOrder.MsSeq EQ iiMsSeq:
      IF fIsConvergenceTariff(bOrder.CLIType) THEN DO:
        CREATE ttOrderList.
        ASSIGN ttOrderList.OrderID = bOrder.OrderId.
      END.
   END.
   FIND FIRST ttOrderList NO-ERROR.
   IF AVAIL ttORderList THEN RETURN STRING(ttOrderList.OrderID).

   RETURN "ERROR: Order not found for mobsub " + STRING(iiMsSeq).
END.



FUNCTION fParseEmailByRequest RETURNS CHAR
   (iiMsRequest AS INT,
    icTemplate AS CHAR):
   DEF VAR lcOutput AS CHAR NO-UNDO.
   DEF VAR lcMailHeader AS CHAR NO-UNDO.
   DEF VAR lcReplace AS CHAR NO-UNDO.
   DEF BUFFER bMsRequest FOR MsRequest.
   DEF BUFFER bCustomer FOR Customer.
   FIND FIRST bMsRequest NO-LOCK WHERE
              bMsRequest.MsRequest EQ iiMsRequest NO-ERROR.
   IF NOT AVAIL bMsRequest THEN RETURN "ERROR: Request not found " +
                                   STRING(iiMsRequest).
   FIND FIRST bCustomer NO-LOCK WHERE
              bCustomer.CustNum EQ bMsRequest.CustNum.
    IF NOT AVAIL bCustomer THEN
       RETURN "ERROR: Customer of requst not found " + STRING(iiMsRequest).

   lcOutput = fGetEmailText("EMAIL",
                               icTemplate,
                               1,
                               OUTPUT lcMailHeader).
   IF lcOutput EQ ""/* OR lcMailHeader EQ ""*/ THEN
      RETURN "ERROR: Email content fetching error" +
             STRING(BCustomer.CustID) + " " +
             STRING(icTemplate).

   /*Seek tags:*/
   IF INDEX(lcOutput, "#CUSTNAME") > 0 THEN DO:
      lcOutput = REPLACE(lcOutput, "#CUSTNAME", 
         (bCustomer.CustName + " " + bCustomer.FirstName + " ")   ).
   END.
   IF INDEX(lcOutput, "#ORDERID") > 0 THEN DO:
      lcOutput = REPLACE(lcOutput, 
                         "#ORDERID", 
                         fFindCOFFOrder(bMsRequest.MsSeq)).
   END.
   IF INDEX(lcOutput, "#CUSTTYPE") > 0 THEN DO:
      lcOutput = REPLACE(lcOutput, "#CUSTTYPE", STRING(bCustomer.CustIdType)).
   END.
   IF INDEX(lcOutput, "#CUSTID") > 0 THEN DO:
      lcOutput = REPLACE(lcOutput, "#CUSTID", STRING(bCustomer.Custid)).
   END.
   IF INDEX(lcOutput, "#EMAIL") > 0 THEN DO:
      lcReplace = ENTRY(3,bMSRequest.ReqCparam6, "|").
      lcOutput = REPLACE(lcOutput, "#EMAIL", lcReplace).
   END.
   IF INDEX(lcOutput, "#NUMBER") > 0 THEN DO:
      lcReplace = ENTRY(2,bMsRequest.Reqcparam6, "|").
      lcOutput = REPLACE(lcOutput, "#NUMBER", lcReplace).
   END.

   RETURN lcOutput.

END.


FUNCTION fSendEmailByRequest RETURNS CHAR
   (iiMsRequest AS INT,
    icTemplate AS CHAR):
   DEF VAR lcOutput AS CHAR NO-UNDO.
   DEF VAR lcMailFile AS CHAR NO-UNDO.
   DEF VAR lcMailHeader AS CHAR NO-UNDO.
   DEF VAR lcReplace AS CHAR NO-UNDO.
   DEF VAR lcMailDir AS CHAR NO-UNDO.
   DEF VAR lcStatus AS CHAR NO-UNDO.
   DEF BUFFER bMsRequest FOR MsRequest.
   DEF BUFFER bCustomer FOR Customer.
   
   FIND FIRST bMsRequest NO-LOCK WHERE
              bMsRequest.MsRequest EQ iiMsRequest NO-ERROR.
   IF NOT AVAIL bMsRequest THEN RETURN "ERROR: Request not found " +
                                   STRING(iiMsRequest).
   FIND FIRST bCustomer NO-LOCK WHERE
              bCustomer.CustNum EQ bMsRequest.CustNum.
    IF NOT AVAIL bCustomer THEN
       RETURN "ERROR: Customer of requst not found " + STRING(iiMsRequest).

   lcOutput = fGetEmailText("EMAIL",
                             icTemplate,
                             1,
                             OUTPUT lcMailHeader).
   
   IF lcOutput EQ ""/* OR lcMailHeader EQ ""*/ THEN
      RETURN "ERROR: Email content fetching error" +
             STRING(BCustomer.CustID) + " " +
             STRING(icTemplate).

   /*Seek tags:*/
   IF INDEX(lcOutput, "#CUSTNAME") > 0 THEN DO:
      lcOutput = REPLACE(lcOutput, "#CUSTNAME", 
         (bCustomer.CustName + " " + bCustomer.FirstName + " ")   ).
   END.
   IF INDEX(lcOutput, "#ORDERID") > 0 THEN DO:
      lcOutput = REPLACE(lcOutput, 
                         "#ORDERID", 
                         fFindCOFFOrder(bMsRequest.MsSeq)).
   END.
   IF INDEX(lcOutput, "#CUSTTYPE") > 0 THEN DO:
      lcOutput = REPLACE(lcOutput, "#CUSTTYPE", STRING(bCustomer.CustIdType)).
   END.
   IF INDEX(lcOutput, "#CUSTID") > 0 THEN DO:
      lcOutput = REPLACE(lcOutput, "#CUSTID", STRING(bCustomer.Orgid)).
   END.
   IF INDEX(lcOutput, "#EMAIL") > 0 THEN DO:
      IF NUM-ENTRIES(bMSRequest.ReqCparam6) > 2 THEN
         lcReplace = ENTRY(3,bMSRequest.ReqCparam6, "|").
      ELSE lcReplace = ENTRY(2,bMSRequest.ReqCparam6, "|").
      lcOutput = REPLACE(lcOutput, "#EMAIL", lcReplace).
   END.
   IF INDEX(lcOutput, "#NUMBER") > 0 THEN DO:
      lcReplace = ENTRY(2,bMsRequest.Reqcparam6, "|").
      lcOutput = REPLACE(lcOutput, "#NUMBER", lcReplace).
   END.

   IF INDEX(lcMailHeader, "#STATUS") > 0 THEN DO:
      IF msrequest.reqtype EQ 9 THEN DO:
         IF msrequest.reqstatus EQ {&REQUEST_STATUS_CONFIRMATION_PENDING} THEN
            lcstatus = "3 - Pending deactivation".
         ELSE lcStatus = "0 - Inactive".
      END.
      IF msrequest.reqtype EQ 8 THEN DO:
         IF msrequest.reqstatus EQ {&REQUEST_STATUS_CONFIRMATION_PENDING} THEN
            lcstatus = "2 - Pending activation".
         ELSE lcStatus = "1 - Active".
      END.
      lcMailHeader = REPLACE(lcMailHeader, "#STATUS", lcstatus).
   END.

   /*Set email sending parameters*/
   /*lcMailDir = "/tmp/". /*To be sure that we have some place*/
   lcMailDir = fCParam("YPRO", "YPRO_SVA_email_dir").
   lcMailFile = lcMailDir + "SVA_email" + STRING(bMsRequest.Msrequest) + ".txt".
   
   OUTPUT STREAM soutfile to VALUE(lcMailFile).
   PUT STREAM soutfile UNFORMATTED lcOutput skip.
   */
   ASSIGN
      xMailFrom = fCParamC("DefEmailSender")
      xMailAddr = fCParam("YPRO", "SVA_BO_EMAIL_ADDRESS")
      xMailSubj = lcMailHeader.
      SendMaileInvoice(lcOutput, "", "").

   /*Used email file removal or saving to logs?*/

   RETURN "".

END.


   
/*'off', 'on', 'cancel activation', 'cancel deactivation'*/
FUNCTION fMakeProActRequest RETURNS INT(
   INPUT iiMsSeq AS INT,
   INPUT icContr AS CHAR,
   INPUT idActStamp AS DEC,
   INPUT icParam1 AS CHAR,
   INPUT icParam2 AS CHAR,
   INPUT icAction AS CHAR, 
   OUTPUT ocErr AS CHAR):
   DEF VAR liRequest AS INT NO-UNDO.
   DEF VAR liReqType AS INT NO-UNDO.
   DEF VAR lcError AS CHAR NO-UNDO.
   DEF VAR lcParams AS CHAR NO-UNDO.

   DEF BUFFER bOwner FOR MSOwner. 
   DEF BUFFER bMsRequest FOR MsRequest.
   FIND FIRST bOwner WHERE bOwner.MsSeq = iiMsSeq NO-LOCK NO-ERROR.

   IF NOT AVAIL bOwner THEN RETURN 0.
   lcParams = "SVA". /*To indicate that we are handling SVA request.*/
   IF icParam1 NE "" THEN DO:
      IF icParam1 EQ "no" THEN lcParams =  lcParams + "_NO_WAIT".
      ELSE DO:
         lcParams =  lcParams + "|" + icParam1.
         IF icParam2 NE "" THEN DO:
            lcParams = lcParams + "|" + icParam2. 
         END.
      END. 
   END.

   DO TRANS:
   IF icAction BEGINS "cancel" THEN DO:
      IF icAction EQ "cancel activation" THEN 
         liReqType = {&REQTYPE_CONTRACT_ACTIVATION}.
      ELSE IF icAction EQ "cancel deactivation" THEN
         liReqType = {&REQTYPE_CONTRACT_TERMINATION}.
      ELSE DO:
         ocErr = "Incorrect request".
         RETURN 0.
      END.
         
      FIND FIRST MsRequest WHERE
                 MsRequest.Brand EQ gcBrand AND
                 MsRequest.ReqType EQ liReqType AND
                 MsRequest.ReqStatus EQ {&REQUEST_STATUS_CONFIRMATION_PENDING} AND
                 MsRequest.ReqCParam6 EQ icContr NO-ERROR.
      IF NOT AVAIL MsRequest THEN DO:
         ocErr = "Cancellation not possible, request not found".
         RETURN 0.
      END.
      fReqStatus(4, "SVA Operation Cancellation").
      RETURN MsRequest.MsRequest.
        
   END.
   ELSE IF icAction EQ "on" THEN 
      icAction = "act".
   ELSE if icAction EQ "off" THEN 
      icAction = "term".

      liRequest = fPCActionRequest(iiMsSeq,
                                   icContr,
                                   icAction,
                                   idActStamp,
                                   TRUE, /* fees */
                                   {&REQUEST_SOURCE_CONTRACT_ACTIVATION},
                                   "",
                                   0,
                                   FALSE,
                                   "",
                                   0,
                                   0,
                                   lcParams,
                                   OUTPUT ocErr).
   END. /*Trans*/  
   RETURN liRequest. /*bCreaReq.MsRequest.*/
END.



/*Function returns TRUE if the order exsists and it is done from PRO channel.*/
FUNCTION fIsProOrder RETURNS LOGICAL
   (iiOrderID as INT):

   FIND FIRST Order NO-LOCK WHERE
              Order.Brand EQ  Syst.Parameters:gcBrand AND
              Order.OrderID EQ iiOrderID NO-ERROR.

   IF INDEX(Order.orderchannel,"PRO") > 0 THEN
      RETURN TRUE.
   ELSE RETURN FALSE.
   
END.
/*Function returns True if a tariff can be defined as 2P tariff.
NOTE: False is returned in real false cases and also in error cases. */
FUNCTION fIs2PTariff RETURNS LOGICAL
   (icCliType AS CHAR):

   DEF BUFFER CLIType FOR CLIType.

   FIND FIRST CLIType NO-LOCK WHERE
              CLIType.Brand EQ Syst.Parameters:gcBrand AND
              CLIType.CliType EQ icCLIType NO-ERROR.
   IF AVAIL CliType AND
            CliType.TariffType EQ {&CLITYPE_TARIFFTYPE_FIXEDONLY} THEN 
      RETURN TRUE.

   RETURN FALSE.
END.

/* Find correct customer segment */
FUNCTION fgetCustSegment RETURNS CHAR
   (INPUT icIdType AS CHAR,
    INPUT ilSelfemployed AS LOG,
    INPUT ilProCust AS LOG,
    INPUT-OUTPUT ocCategory AS CHAR):
   DEF VAR lcSegment AS CHAR NO-UNDO.
   IF ilProCust THEN DO:
      FIND FIRST CustCat NO-LOCK WHERE
                 Custcat.brand EQ Syst.Parameters:gcBrand AND
                 Custcat.custidtype EQ icIdType AND
                 CustCat.selfemployed EQ ilSelfEmployed AND
                 CustCat.pro EQ ilProCust NO-ERROR.
      IF AVAIL CustCat THEN DO:
         lcSegment = CustCat.Segment.
         ocCategory = CustCat.category.
      END.
   END.
   ELSE DO:
      IF icIDType EQ "CIF" THEN
         ocCategory = "23".
      ELSE IF icIDType EQ "NIF" AND ilSelfEmployed THEN 
         ocCategory = "44".         
      ELSE IF icIDType EQ "NIF" AND NOT ilSelfEmployed THEN
         ocCategory = "10". 
      ELSE IF icIDType EQ "NIE" AND ilSelfEmployed THEN
         ocCategory = "45".         
      ELSE IF icIDType EQ "NIE" AND NOT ilSelfEmployed THEN
         ocCategory = "11". 
      FIND FIRST CustCat NO-LOCK WHERE
                 Custcat.brand EQ Syst.Parameters:gcBrand AND
                 Custcat.category EQ ocCategory NO-ERROR.
      IF AVAIL CustCat THEN   
         lcSegment = CustCat.Segment.
   END.
   IF lcSegment = "" THEN
      lcSegment = "NEW". /* Unknown, should not come here */

   RETURN lcSegment.
END.

/*STC is restricted from Prepaid to postpaid and 2P*/
FUNCTION fValidateProSTC RETURNS CHAR
   (iiCustomer AS INT,
    icCurrCLIType AS CHAR,
    icNewCLIType AS CHAR):

   DEF BUFFER bCurr FOR CLIType.
   DEF BUFFER bNew FOR CLIType.
   
   FIND FIRST Customer NO-LOCK WHERE
              Customer.CustNum EQ iiCustomer NO-ERROR.
   IF NOT AVAIL Customer THEN RETURN "Customer not found".
   IF NOT fIsPro(Customer.Category) THEN RETURN "". /*No PRO logic needed*/

   FIND FIRST bCurr NO-LOCK WHERE
              bCurr.Brand EQ Syst.Parameters:gcbrand AND
              bCurr.Clitype EQ icCurrCLIType NO-ERROR.
   IF NOT AVAIL bCurr THEN RETURN "Incorrect CLIType".

   FIND FIRST bNew NO-LOCK WHERE
              bNew.Brand EQ Syst.Parameters:gcbrand AND
              bNew.Clitype EQ icNewCLIType NO-ERROR.
   IF NOT AVAIL bNew THEN RETURN "Incorrect CLIType".

   IF bCurr.PayType EQ {&CLITYPE_PAYTYPE_PREPAID} THEN RETURN "". /*No PRO logic for prepaid*/

   IF bNew.Paytype EQ {&CLITYPE_PAYTYPE_PREPAID} THEN 
      RETURN "STC to Prepaid is not allowed for Pro customer".
   IF fIs2PTariff(bNew.Clitype) THEN 
      RETURN "STC to 2P is not allowed for Pro customer".
      
   RETURN "".
END.

&ENDIF


