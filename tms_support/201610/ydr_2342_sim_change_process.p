{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
Syst.Var:katun = "OTANOK".

{Syst/tmsconst.i}
{Func/msreqfunc.i}

DEF STREAM sIn.
DEF STREAM sOut.
DEF STREAM sICC.

DEF VAR lcCLI      AS CHAR   NO-UNDO.
DEF VAR liCount    AS INT    NO-UNDO.
DEF VAR liMsSeq    AS INT    NO-UNDO.
DEF VAR liCustNum  AS INT    NO-UNDO.
DEF VAR liRowNum   AS INT    NO-UNDO.
DEF VAR lcFileName AS CHAR   NO-UNDO.
DEF VAR lcLine     AS CHAR   NO-UNDO.
DEF VAR liLoop1    AS INT    NO-UNDO.
DEF VAR lhTable    AS HANDLE NO-UNDO.
DEF VAR lhField    AS HANDLE NO-UNDO.
DEF VAR iLargestID AS INT    NO-UNDO.
DEF VAR lcOldICC   AS CHAR   NO-UNDO.

DEFINE TEMP-TABLE ttOneDelivery NO-UNDO
   FIELD RowNum        AS INTEGER             
   /* Skip this OrderId field when generating dextra file: 
      important for internal reason: logging, etc.   */
   FIELD OrderId       AS INTEGER                   
   FIELD RequestID     AS CHARACTER FORMAT "X(8)"
   FIELD ActionID      AS CHARACTER FORMAT "X(2)"
   FIELD ProductID     AS CHARACTER FORMAT "X(9)"
   FIELD ContractId    AS CHARACTER FORMAT "X(10)"
   FIELD NIE           AS CHARACTER FORMAT "X(9)"
   FIELD NIF           AS CHARACTER FORMAT "X(9)"
   FIELD CIF           AS CHARACTER FORMAT "X(9)"
   FIELD PassPort      AS CHARACTER FORMAT "X(10)"
   FIELD SubsType      AS CHARACTER FORMAT "X(10)"
   /* 10 */
   FIELD ICCNum        AS CHARACTER FORMAT "X(13)"
   FIELD MSISDN        AS CHARACTER FORMAT "X(10)"
   FIELD TmpMSISDN     AS CHARACTER FORMAT "X(10)"
   FIELD MNPState      AS CHARACTER FORMAT "X(10)"
   FIELD VoiceMail     AS CHARACTER FORMAT "X(1)"
   FIELD XFUserID      AS CHARACTER FORMAT "X(10)"
   FIELD XFPWD         AS CHARACTER FORMAT "X(8)"
   FIELD Company       AS CHARACTER FORMAT "X(60)"
   FIELD Name          AS CHARACTER FORMAT "X(60)"
   FIELD SurName1      AS CHARACTER FORMAT "X(60)"
   /* 20 */
   FIELD SurName2      AS CHARACTER FORMAT "X(60)"
   FIELD DelivCO       AS CHARACTER FORMAT "X(30)"
   FIELD DelivAddr     AS CHARACTER FORMAT "X(60)"
   FIELD DelivCity     AS CHARACTER FORMAT "X(60)"
   FIELD DelivZip      AS CHARACTER FORMAT "X(5)"
   FIELD DelivRegi     AS CHARACTER FORMAT "X(60)"
   FIELD DelivCoun     AS CHARACTER FORMAT "X(20)"
   FIELD CustAddr      AS CHARACTER FORMAT "X(60)"
   FIELD CustCity      AS CHARACTER FORMAT "X(60)"
   FIELD CustZip       AS CHARACTER FORMAT "X(5)"
   /* 30 */
   FIELD CustRegi      AS CHARACTER FORMAT "X(60)"
   FIELD CustCoun      AS CHARACTER FORMAT "X(20)"
   FIELD MobConNum     AS CHARACTER FORMAT "X(15)"
   FIELD FixConNum     AS CHARACTER FORMAT "X(15)"
   FIELD EMail         AS CHARACTER FORMAT "X(60)"
   FIELD PaymInfo      AS CHARACTER FORMAT "X(1)"
   FIELD TermAmt       AS CHARACTER FORMAT "X(7)"
   FIELD TopUp         AS CHARACTER FORMAT "X(7)"
   FIELD MNPTransT     AS CHARACTER FORMAT "X(12)"
   FIELD InvRefNum     AS CHARACTER FORMAT "X(20)"
   /* 40 */
   FIELD InvNum        AS CHARACTER FORMAT "X(20)"
   FIELD TaxInfoServ   AS CHARACTER FORMAT "X(4)"
   FIELD TaxInfoTerm   AS CHARACTER FORMAT "X(4)"
   FIELD SalesChan     AS CHARACTER FORMAT "X(2)"
   FIELD DelivPaym     AS CHARACTER FORMAT "X(7)"
   FIELD DelivCost     AS CHARACTER FORMAT "X(7)"
   FIELD ServTotal     AS CHARACTER FORMAT "X(7)"
   FIELD TermTotal     AS CHARACTER FORMAT "X(7)"
   FIELD TaxService    AS CHARACTER FORMAT "X(7)"
   FIELD TaxTerminal   AS CHARACTER FORMAT "X(7)"
   /* 50 */
   FIELD InvoiceTotal  AS CHARACTER FORMAT "X(7)"
   FIELD DiscountTotal AS CHARACTER FORMAT "X(7)"
   .

DEFINE TEMP-TABLE ttInvRow NO-UNDO
   FIELD RowNum      AS INTEGER
   FIELD ProductId   AS CHARACTER FORMAT "X(9)"
   FIELD ProductDesc AS CHARACTER FORMAT "X(80)"
   FIELD UnitPrice   AS CHARACTER FORMAT "X(7)"
   FIELD Quantity    AS CHARACTER FORMAT "X(2)"
   FIELD Discount    AS CHARACTER FORMAT "X(7)"
   FIELD TotalPrice  AS CHARACTER FORMAT "X(7)"
   
   INDEX RowNum AS PRIMARY RowNum.

DEFINE TEMP-TABLE ttExtra NO-UNDO
   FIELD RowNum      AS INTEGER
   FIELD DataService AS CHARACTER FORMAT "X(9)"
   FIELD PayTerm     AS CHARACTER FORMAT "X(12)"
   FIELD OrderDate   AS CHARACTER FORMAT "X(8)"
   FIELD ResidualAmount AS CHARACTER FORMAT "X(7)"
   FIELD DeliveryType AS CHAR FORMAT "X(1)"
   FIELD KialaCode   AS CHAR FORMAT "X(4)"
   FIELD ContractFileName AS CHAR FORMAT "X(14)"
   INDEX RowNum AS PRIMARY RowNum.

DEFINE TEMP-TABLE ttOutputText 
   FIELD cText AS CHARACTER
   FIELD id AS INTEGER
   INDEX idx id.

FUNCTION fIsMNPOutOngoing RETURNS LOGICAL
(icCLI AS CHAR):
   
   DEF BUFFER bMNPSubChk     FOR MNPSub.
   DEF BUFFER bMNPProcessChk FOR MNPProcess.

   FOR EACH bMNPSubChk NO-LOCK WHERE
            bMNPSubChk.CLI = icCLI,
      EACH bMNPProcessChk NO-LOCK WHERE
           bMNPProcessChk.MNPSeq     = bMNPSubChk.MNPSeq AND
           bMNPProcessChk.MNPType    = {&MNP_TYPE_OUT}   AND
           bMNPProcessChk.StatusCode = {&MNP_ST_ACON}:
      RETURN TRUE.
   END.
   RETURN FALSE.

END FUNCTION.

FUNCTION fSearchStock RETURNS CHARACTER
   (icStock AS CHAR,
   icZipCode AS CHAR).

   DEF VAR liLoop AS INT NO-UNDO. 

   FOR EACH Stock WHERE
            Stock.Brand   = Syst.Var:gcBrand AND
            Stock.StoType = icStock NO-LOCK:
      DO liLoop = 1 TO NUM-ENTRIES(Stock.ZipCodeExp,","):
        IF icZipCode MATCHES
           ENTRY(liLoop, Stock.ZipCodeExp,",") THEN DO:
           RETURN  Stock.Stock.
        END.
      END.
   END.
   
   RETURN icStock.

END FUNCTION.

FUNCTION get_account_data RETURN LOGICAL
      ( INPUT piCustNum AS INT,
        OUTPUT pcLogin AS CHAR,
        OUTPUT pcPassword AS CHAR ):
    FIND UserAccount NO-LOCK NO-PREFETCH
    WHERE UserAccount.CustNum = piCustNum NO-ERROR.
    IF AVAILABLE UserAccount THEN ASSIGN
        pcLogin = UserAccount.login
        pcPassword = UserAccount.password
    .
    RETURN AVAILABLE UserAccount.
END FUNCTION.

FUNCTION pCheckTextSection RETURNS LOG (INPUT-OUTPUT pcText AS CHARACTER,
                                         INPUT plLast AS LOGICAL):
  IF LENGTH(pcText) > 30000 OR plLast THEN
  DO:
     CREATE ttOutputText.
     ASSIGN ttOutputText.cText = REPLACE(pcText, CHR(10), " ")
            ttOutputText.cText = REPLACE(pcText, "+", " ")
            ttOutputText.id = iLargestId.
     iLargestId = iLargestId + 1.
     pcText = "".
     RETURN TRUE.
  END.
  RETURN FALSE.
END.

FUNCTION fDelivSIM RETURNS LOG
   (INPUT pcICC AS CHARACTER):
   
   DEF VAR lcUID       AS CHAR NO-UNDO.
   DEF VAR lcPWD       AS CHAR NO-UNDO.
   DEF VAR lcDeliRegi  AS CHAR NO-UNDO.
   DEF VAR i           AS INT  NO-UNDO.
   DEF VAR lcFirstName AS CHAR NO-UNDO.
   DEF VAR lcSurName1  AS CHAR NO-UNDO.
   DEF VAR lcSurName2  AS CHAR NO-UNDO.
   DEF VAR lcMobile    AS CHAR NO-UNDO.
   DEF VAR lcFixed     AS CHAR NO-UNDO.
   DEF VAR lcEmail     AS CHAR NO-UNDO.

   DEF BUFFER AgreeCustomer FOR Customer.

   FIND FIRST AgreeCustomer NO-LOCK WHERE
              AgreeCustomer.CustNum = MobSub.Custnum NO-ERROR.
   
   IF AgreeCustomer.CustIDType = "CIF" THEN DO:
      FIND FIRST CustContact NO-LOCK WHERE
                 CustContact.Brand    = Syst.Var:gcBrand                              AND
                 CustContact.CustNum  = AgreeCustomer.CustNum                AND
                 CustContact.CustType = {&ORDERCUSTOMER_ROWTYPE_CIF_CONTACT} NO-ERROR.
      IF AVAIL CustContact THEN
         ASSIGN lcFirstName = CustContact.FirstName
                lcSurName1  = CustContact.CustName
                lcSurName2  = CustContact.SurName2
                lcMobile    = CustContact.SMSNumber
                lcFixed     = CustContact.Phone
                lcEmail     = CustContact.Email.
   END.
   ELSE
      ASSIGN lcFirstName = AgreeCustomer.FirstName
             lcSurName1  = AgreeCustomer.CustName
             lcSurName2  = AgreeCustomer.SurName2
             lcMobile    = AgreeCustomer.MobileNumber
             lcFixed     = AgreeCustomer.Phone
             lcEmail     = AgreeCustomer.Email.

   get_account_data(AgreeCustomer.CustNum, OUTPUT lcUID, OUTPUT lcPWD).
 
   FIND FIRST Region NO-LOCK WHERE
              Region.Region = AgreeCustomer.Region
              NO-ERROR.
      lcDeliRegi = Region.RgName.
   liRowNum = liRowNum + 1.

   CREATE ttOneDelivery.
   ASSIGN
      ttOneDelivery.RowNum        = liRowNum
      ttOneDelivery.OrderId       = 0
      ttOneDelivery.RequestID     = "NRM"
      ttOneDelivery.ActionID      = STRING(SIM.DealerStat)
      ttOneDelivery.ProductID     = "" 
      ttOneDelivery.ContractID    = ""
      ttOneDelivery.NIE           = AgreeCustomer.OrgId WHEN AgreeCustomer.CustIdType = "NIE"
      ttOneDelivery.NIF           = AgreeCustomer.OrgId WHEN AgreeCustomer.CustIdType = "NIF"
      ttOneDelivery.CIF           = AgreeCustomer.OrgId WHEN AgreeCustomer.CustIdType = "CIF"
      ttOneDelivery.PassPort      = AgreeCustomer.OrgId WHEN AgreeCustomer.CustIdType = "PassPort"
      ttOneDelivery.SubsType      = MobSub.CLIType
      ttOneDelivery.ICCNum        = SUBSTR(SIM.ICC,7)
      ttOneDelivery.MSISDN        = MobSub.CLI
      ttOneDelivery.MNPState      = "0"
      ttOneDelivery.VoiceMail     = "633633633"
      ttOneDelivery.XFUserID      = lcUID
      ttOneDelivery.XFPWD         = lcPWD
      ttOneDelivery.Company       = AgreeCustomer.Company
      ttOneDelivery.Name          = lcFirstName
      ttOneDelivery.SurName1      = lcSurName1
      ttOneDelivery.SurName2      = lcSurName2
      ttOneDelivery.DelivCO       = AgreeCustomer.COName
      ttOneDelivery.DelivAddr     = AgreeCustomer.Address
      ttOneDelivery.DelivCity     = AgreeCustomer.PostOffice
      ttOneDelivery.DelivZip      = AgreeCustomer.ZIP
      ttOneDelivery.DelivRegi     = lcDeliRegi
      ttOneDelivery.DelivCoun     = AgreeCustomer.Country
      ttOneDelivery.CustAddr      = AgreeCustomer.Address
      ttOneDelivery.CustCity      = AgreeCustomer.PostOffice
      ttOneDelivery.CustZip       = AgreeCustomer.ZIP
      ttOneDelivery.CustRegi      = lcDeliRegi
      ttOneDelivery.CustCoun      = AgreeCustomer.Country
      ttOneDelivery.MobConNum     = lcMobile
      ttOneDelivery.FixConNum     = lcFixed
      ttOneDelivery.EMail         = lcEmail
      ttOneDelivery.TopUp         = STRING(0.0,"zzz9.99")
      ttOneDelivery.ServTotal     = "0"
      ttOneDelivery.TermTotal     = "0"
      ttOneDelivery.TaxService    = "0"
      ttOneDelivery.TaxTerminal   = "0"
      ttOneDelivery.InvoiceTotal  = "0"
      ttOneDelivery.DiscountTotal = "0"
      ttOneDelivery.TermAmt       = STRING(0.0,"zzz9.99")
      .
      
   IF AgreeCustomer.CustIdType = "CIF" THEN
   ASSIGN
      ttOneDelivery.NIE           = AgreeCustomer.AuthCustID WHEN AgreeCustomer.AuthCustIDType = "NIE"
      ttOneDelivery.NIF           = AgreeCustomer.AuthCustID WHEN AgreeCustomer.AuthCustIDType = "NIF"
      ttOneDelivery.PassPort      = AgreeCustomer.AuthCustID WHEN AgreeCustomer.AuthCustIDType = "PassPort".
   
   CREATE ttInvRow.
  
   ASSIGN
      ttInvRow.RowNum      = ttOneDelivery.RowNum
      ttInvRow.ProductId   = "TS00000U3"
      ttInvRow.ProductDesc = ""
      ttInvRow.UnitPrice   = ""
      ttInvRow.Quantity    = ""
      ttInvRow.Discount    = ""
      ttInvRow.TotalPrice  = "".
  
  DO i = 1 TO 7:

      CREATE ttInvRow.
     
      ASSIGN
         ttInvRow.RowNum      = ttOneDelivery.RowNum
         ttInvRow.ProductId   = ""
         ttInvRow.ProductDesc = ""
         ttInvRow.UnitPrice   = ""
         ttInvRow.Quantity    = ""
         ttInvRow.Discount    = ""
         ttInvRow.TotalPrice  = "".

   END.
   
   CREATE ttExtra.
   ASSIGN ttExtra.RowNum           = ttOneDelivery.RowNum
          ttExtra.DataService      = ""
          ttExtra.PayTerm          = ""
          ttExtra.OrderDate        = ""
          ttExtra.ResidualAmount   = ""
          ttExtra.DeliveryType     = "1"
          ttExtra.ContractFileName = "".

END FUNCTION.

INPUT  STREAM sIn  FROM "ydr_2342_input_yoigo_test.txt".
OUTPUT STREAM sOut TO   "ydr_2342_output_yoigo_test.log".

PUT STREAM sOut UNFORMATTED
   "MSISDN;Reason"
   SKIP.

msisdn:
REPEAT TRANSACTION:
   ASSIGN lcCLI     = ""
          liMsSeq   = 0
          liCustNum = 0
          .
   IMPORT STREAM sIn UNFORMATTED lcCLI.

   IF lcCLI = "" THEN NEXT.
   
   PUT STREAM sOut UNFORMATTED lcCLI.
   
   IF NOT CAN-FIND (FIRST MSISDN WHERE 
                          MSISDN.Brand = Syst.Var:gcBrand AND
                          MSISDN.CLI   = lcCLI) THEN DO:
      PUT STREAM sOut UNFORMATTED
         ";MSISDN not found"
         SKIP.
      NEXT msisdn.
   END.

   FIND FIRST MobSub NO-LOCK WHERE
              MobSub.Brand = Syst.Var:gcBrand AND
              MobSub.CLI   = lcCLI   NO-ERROR.
   IF NOT AVAIL MobSub THEN DO:
      PUT STREAM sOut UNFORMATTED
         ";Subscription Already Terminated"
         SKIP.
      NEXT msisdn.
   END.
   ELSE 
      ASSIGN liMsSeq   = MobSub.MsSeq
             liCustNum = MobSub.CustNum
             lcOldICC  = MobSub.ICC.
 
   RELEASE MobSub.

   IF Mnp.MNPOutGoing:mIsMNPOutOngoing(lcCLI) THEN DO:
      PUT STREAM sOut UNFORMATTED
         ";Ongoing MNP OUT in ACON Status"
         SKIP.
      NEXT msisdn.
   END.
   
   FIND FIRST Order NO-LOCK WHERE
              Order.MsSeq     = liMsSeq              AND
              Order.OrderType = 2                    AND
              LOOKUP(Order.StatusCode,"6,7,8,9") = 0 AND
              NOT Order.OrderChannel BEGINS "Renewal_POS" NO-ERROR.

   IF AVAIL Order AND 
     (Order.ICC > "" OR 
      can-find(FIRST OrderAction WHERE
                 OrderAction.Brand    = Syst.Var:gcBrand AND
                 OrderAction.OrderId  = Order.OrderId AND
                 OrderAction.ItemType = "SIMType" AND
                 OrderAction.ItemKey > "")) THEN DO:
      PUT STREAM sOut UNFORMATTED
         ";Ongoing renuevo order WITH SIM type change"
         SKIP.
      NEXT msisdn.
   END.

   IF CAN-FIND(FIRST MsRequest WHERE
                     MsRequest.MsSeq     = liMsSeq                                AND
                     MsRequest.ReqType   = {&REQTYPE_ICC_CHANGE}                  AND
                     MsRequest.ReqStatus = {&REQUEST_STATUS_CONFIRMATION_PENDING} AND
                     MsRequest.ReqSource = {&REQUEST_SOURCE_RENEWAL}) THEN DO:
      PUT STREAM sOut UNFORMATTED
         ";Ongoing renuevo order WITH SIM type change"
         SKIP.
      NEXT msisdn.
   END.

   RUN pCreateReq. 

   IF RETURN-VALUE NE "" THEN DO:
      PUT STREAM sOut UNFORMATTED
         ";" RETURN-VALUE
      SKIP.
      NEXT msisdn.
   END.

   PUT STREAM sOut UNFORMATTED
      ";Success"
      SKIP.

   liCount = liCount + 1.
   IF liCount MOD 1 EQ 0 THEN DO:
      DISP liCount.
      PAUSE 0.
   END.

END.

INPUT  STREAM sIn  CLOSE.
OUTPUT STREAM sOut CLOSE.

/*
FOR EACH MSREquest NO-LOCK WHERE
         MSREquest.Brand = "1" and
         MSREquest.ReqType = 15 and
         MSREquest.ReqStatus = 20:

   FIND FIRST MobSub NO-LOCK WHERE
              MobSub.MsSeq = MSREquest.msseq NO-ERROR.
   IF AVAIL MobSub THEN DO:
      FIND FIRST SIM NO-LOCK WHERE
                 SIM.Brand = Syst.Var:gcBrand AND
                 SIM.ICC   = MSREquest.REqcparam2 aND
                 SIM.simstat = 13 NO-ERROR.
      IF AVAIL SIM THEN
         fDelivSIM(SIM.ICC).
   END.
END.


/*Logistics file*/
RUN pLO.

*/

PROCEDURE pCreateReq:

   DEF VAR lcStock AS CHAR NO-UNDO.
   DEF VAR lcICC   AS CHAR NO-UNDO.

   DEF BUFFER MsRequest FOR MsRequest.
   DEF BUFFER Customer  FOR Customer.
   DEF BUFFER new-SIM   FOR SIM.
   DEF BUFFER bOldOrder FOR Order.

   FIND FIRST Customer NO-LOCK WHERE
              Customer.CustNum = liCustNum NO-ERROR.
   IF AVAIL Customer THEN DO:
      
      IF Customer.ZipCode BEGINS "35" OR
         Customer.ZipCode BEGINS "38" THEN
         lcStock = "NEW_CAN_ISL".
      ELSE
         lcStock = "NEW".

      SearchSIM:
      FOR EACH SIM NO-LOCK USE-INDEX simstat WHERE
               SIM.Brand = Syst.Var:gcBrand AND
               SIM.Stock = lcStock AND
               SIM.SimStat = {&SIM_SIMSTAT_AVAILABLE} AND
               SIM.SimArt   = "universal":
         
            IF SIM.MsSeq > 0 THEN DO:
               FIND FIRST bOldOrder WHERE
                          bOldOrder.MsSeq = SIM.MsSeq
                    NO-LOCK USE-INDEX MsSeq NO-ERROR.
               IF AVAIL bOldOrder AND
                  Func.Common:mOffSet(bOldOrder.CrStamp, 24 * 7) > Func.Common:mMakeTS()
               THEN NEXT.
            END.

            FIND new-SIM EXCLUSIVE-LOCK WHERE
                 ROWID(new-SIM) = ROWID(SIM)
                 NO-ERROR NO-WAIT.
            IF AVAIL new-SIM THEN
               ASSIGN lcICC           = new-SIM.ICC
                      new-SIM.SimStat = 13
                      new-SIM.MsSeq   = liMsSeq
                      .
            LEAVE SearchSIM.
      END.
      RELEASE new-SIM.
   END.

   IF lcICC EQ "" THEN RETURN "SIM not available".

   CREATE MsRequest.
   ASSIGN MsRequest.MsRequest  = NEXT-VALUE(MsRequest)
          MsRequest.ReqType    = {&REQTYPE_ICC_CHANGE}
          MsRequest.Brand      = Syst.Var:gcBrand
          MsRequest.UserCode   = Syst.Var:katun
          MsRequest.ActStamp   = Func.Common:mMakeTS()
          MsRequest.ReqStatus  = 20
          MsRequest.CLI        = lcCLI
          MsRequest.MsSeq      = liMsSeq
          MsRequest.CustNum    = liCustNum
          MsRequest.CreStamp   = Func.Common:mMakeTS()
          MsRequest.ReqCParam1 = "CHANGEICC"
          MsRequest.ReqCParam2 = lcICC
          MsRequest.ReqSource  = {&REQUEST_SOURCE_SCRIPT}
          .
   
   Func.Common:mWriteMemo("MobSub",
                    STRING(liMsSeq),
                    liCustNum,
                    "Cambio de número ICC",
                    STRING("Solicitado por el cliente") +
                    STRING("~n Old ICC:" + lcOldICC +
                           "~n New ICC:" + lcICC)).

   RETURN "".

END PROCEDURE.
   

PROCEDURE pLO:

lcFileName = "/store/riftp/logistics/icc/spool/nrm_" + Func.Common:mDateFmt(TODAY,"ddmmyyyy") + 
             REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt".
OUTPUT STREAM sICC TO VALUE(lcFileName).
iLargestId = 1.
FOR EACH ttOneDelivery NO-LOCK BREAK BY ttOneDelivery.RowNum:

   lhTable = BUFFER ttOneDelivery:HANDLE.
   lcLine = "".

   DO liLoop1 = 1 TO lhTable:NUM-FIELDS:

      lhField = lhTable:BUFFER-FIELD(liLoop1).

      IF lhField:NAME NE "RowNum" AND lhField:NAME NE "OrderId" THEN
         lcLine  = lcLine + STRING(lhField:BUFFER-VALUE,lhField:FORMAT).

      pCheckTextSection(INPUT-OUTPUT lcLine, FALSE).

   END.

   FOR EACH ttInvRow NO-LOCK WHERE
            ttInvRow.RowNum = ttOneDelivery.RowNum:

      lhTable = BUFFER ttInvRow:HANDLE.

      DO liLoop1 = 1 TO lhTable:NUM-FIELDS:

         lhField = lhTable:BUFFER-FIELD(liLoop1).
     

         IF lhField:NAME NE "RowNum" THEN
            lcLine  = lcLine + STRING(lhField:BUFFER-VALUE,lhField:FORMAT).
         IF liLoop1 = lhTable:NUM-FIELDS THEN
            pCheckTextSection(INPUT-OUTPUT lcLine, TRUE).
         ELSE
            pCheckTextSection(INPUT-OUTPUT lcLine, FALSE).
      
      END.

   END.
   
   /* Write the DataService Field in the last of each line */
   FOR FIRST ttExtra WHERE
             ttExtra.RowNum = ttOneDelivery.RowNum NO-LOCK:

      lhTable = BUFFER ttExtra:HANDLE.

      DO liLoop1 = 1 TO lhTable:NUM-FIELDS:
         lhField = lhTable:BUFFER-FIELD(liLoop1).

         IF lhField:NAME = "RowNum" THEN NEXT.
      
         lcLine  = lcLine + STRING(lhField:BUFFER-VALUE,lhField:FORMAT).
         IF liLoop1 = lhTable:NUM-FIELDS THEN
            pCheckTextSection(INPUT-OUTPUT lcLine, TRUE).
         ELSE
            pCheckTextSection(INPUT-OUTPUT lcLine, FALSE).
      END. /* DO liLoop1 = 1 TO lhTable:NUM-FIELDS: */
   END. /* FOR FIRST ttExtra WHERE */

   FOR EACH ttOutputText:
      PUT STREAM sICC UNFORMATTED ttOutputText.cText.
   END. 
   EMPTY TEMP-TABLE ttOutputText.

   PUT STREAM sICC UNFORMATTED CHR(10).

END.

EMPTY TEMP-TABLE ttOneDelivery.
EMPTY TEMP-TABLE ttInvRow.
EMPTY TEMP-TABLE ttExtra.

IF VALID-HANDLE(lhField) THEN DELETE OBJECT lhField.
IF VALID-HANDLE(lhTable) THEN DELETE OBJECT lhTable.

OUTPUT STREAM sICC CLOSE.
/* UNIX SILENT VALUE("mv " + lcFileName + " " + "/store/riftp/logistics/icc/outgoing/"). */
END PROCEDURE.
