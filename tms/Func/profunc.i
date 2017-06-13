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

{Func/orderfunc.i}

FUNCTION fIsPro RETURNS LOGICAL
   (icCategory AS CHAR):

   FIND FIRST CustCat NO-LOCK where
              CustCat.Brand EQ Syst.Parameters:gcbrand AND
              CustCat.Category EQ icCategory NO-ERROR.
              
   IF AVAIL CustCat AND Custcat.pro THEN RETURN TRUE.
   RETURN FALSE.
END.

FUNCTION fMakeProAckRequest RETURNS LOGICAL (
   INPUT iiMsSeq AS INT,
   INPUT icContr AS CHAR,
   INPUT idActStamp AS DEC,
   INPUT iiMsRequest AS INT):
   DEF VAR liRequest AS INT NO-UNDO.
   DEF VAR lcError         AS CHAR NO-UNDO.

   liRequest = fPCActionRequest(iiMsSeq,
                                icContr,
                                "act",
                                idActStamp,
                                TRUE, /* fees */
                                {&REQUEST_SOURCE_CONTRACT_ACTIVATION},
                                "",
                                iiMsRequest,
                                FALSE,
                                "",
                                0,
                                0,
                                OUTPUT lcError).
   IF liRequest = 0 THEN
      /* write possible error to a memo */
      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                       "MobSub",
                       STRING(MsRequest.MsSeq),
                       MsRequest.Custnum,
                       icContr + " activation failed",
                       lcError).
END.

FUNCTION fProMobileAct RETURN LOGICAL 
   (INPUT iimsseq AS INT,
    INPUT idactstamp AS DEC,
    INPUT iiMsrequest AS INT):
   fMakeProAckRequest(iimsseq,"FLEX_UPSELL_500MB",idactstamp,iiMsrequest).
   fMakeProAckRequest(iimsseq,"VOICE5000",idactstamp,iiMsrequest).
   fMakeProAckRequest(iimsseq,"INT_VOICE100",idactstamp,iiMsrequest).
   fMakeProAckRequest(iimsseq,"SMS5000",idactstamp,iiMsrequest).
END.    

FUNCTION fProFixedAct RETURN LOGICAL
   (INPUT iimsseq AS INT,
    INPUT idactstamp AS DEC,
    INPUT iiMsrequest AS INT):
   fMakeProAckRequest(iimsseq,"FIX_VOICE1000",idactstamp,iiMsrequest).
   fMakeProAckRequest(iimsseq,"INT_FIX_VOICE1000",idactstamp,iiMsrequest).
END.

/*Function returns TRUE if the order exsists and it is done for PRO customer.*/
FUNCTION fIsProOrder RETURNS LOGICAL
   (iiOrderID as INT):
   DEF BUFFER OrderCustomer FOR OrderCustomer.

   FIND FIRST OrderCustomer NO-LOCK WHERE
              OrderCustomer.Brand EQ  Syst.Parameters:gcBrand AND
              Ordercustomer.OrderID EQ iiOrderID AND
              OrderCustomer.Rowtype EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}.

   IF NOT AVAIL OrderCustomer THEN RETURN FALSE.
   RETURN fIsPRO(Ordercustomer.Category).
   
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


