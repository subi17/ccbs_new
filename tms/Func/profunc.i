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
/*{Func/email.i}*/
{Func/fixedlinefunc.i}

FUNCTION fIsPro RETURNS LOGICAL
   (icCategory AS CHAR):

   FIND FIRST CustCat NO-LOCK where
              CustCat.Brand EQ Syst.Parameters:gcbrand AND
              CustCat.Category EQ icCategory NO-ERROR.
              
   IF AVAIL CustCat AND Custcat.pro THEN RETURN TRUE.
   RETURN FALSE.
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
                 MsRequest.ReqCParam3 EQ icContr NO-ERROR.
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


