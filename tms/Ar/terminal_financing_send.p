/* ----------------------------------------------------------------------
  MODULE .......: terminal_financing_send.p
  TASK .........: Create terminal financing file for the bank. YDR-945
  APPLICATION ..: tms
  AUTHOR .......: anttis
  CREATED ......: 25.04.13
  Version ......: yoigo
---------------------------------------------------------------------- */
{Syst/commali.i}
{Func/cparam2.i}
{Syst/dumpfile_run.i}
{Syst/tmsconst.i}
{Func/forderstamp.i}
{Func/msreqfunc.i}
{Func/ftransdir.i}
{Func/financed_terminal.i}

DEF INPUT PARAMETER iiMSrequest AS INT  NO-UNDO.

FIND MSRequest WHERE 
     MSRequest.MSRequest = iiMSRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest OR 
   MsRequest.ReqType NE {&REQTYPE_TERMINAL_FINANCE_BANK_FILE} THEN
   RETURN "ERROR".

/* request is under work */
IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

DEF VAR lcCustIdType AS CHAR NO-UNDO. 
DEF VAR ldaOrderDate AS DATE NO-UNDO. 
DEF VAR ldeTSFrom AS DEC NO-UNDO. 
DEF VAR ldeTSTo AS DEC NO-UNDO. 
DEF VAR lcGender AS CHAR NO-UNDO. 
DEF VAR lcFuc AS CHAR NO-UNDO EXTENT 2. 
DEF VAR lcPayTermType AS CHAR NO-UNDO EXTENT 2.
DEF VAR lcErrorFile AS CHAR NO-UNDO.
DEF VAR ldeTotalAmount AS DEC NO-UNDO. 
DEF VAR liErrors AS INT NO-UNDO. 
DEF VAR liCurrentPeriod AS INT NO-UNDO. 
DEF VAR lcRootDir AS CHAR NO-UNDO. 
DEF VAR lcOutDir AS CHAR NO-UNDO. 
DEF VAR lcSpoolDir AS CHAR NO-UNDO.
DEF VAR lcLogDir AS CHAR NO-UNDO. 
DEF VAR lcFile AS CHAR NO-UNDO. 
DEF VAR liFFItemCount AS INT NO-UNDO. 
DEF VAR ldeFFItemAmount AS DEC NO-UNDO. 
DEF var liLineNum AS INT  NO-UNDO.
DEF VAR lcBankCode AS CHAR NO-UNDO. 
DEF VAR ldeRVPerc AS DEC NO-UNDO. 
DEF VAR ldeRVAmt AS DEC NO-UNDO. 
DEF VAR lcTFBank AS CHAR NO-UNDO. 
DEF VAR lcResellers AS CHAR NO-UNDO. 
DEF VAR lcRootDirCetelem AS CHAR NO-UNDO.
DEF VAR lcSpoolDirCetelem AS CHAR NO-UNDO.
DEF VAR lcOutDirCetelem AS CHAR NO-UNDO.
DEF VAR lcLogDirCetelem AS CHAR NO-UNDO.

FOR EACH Reseller NO-LOCK WHERE
         Reseller.Brand = Syst.Var:gcBrand:
   IF Reseller.Fuc1 > "" AND
      Reseller.Fuc2 > "" THEN
   lcResellers = lcResellers + "," + Reseller.Reseller.
END.
lcResellers = SUBSTRING(lcResellers,2).

IF lcResellers EQ "" THEN DO:
   fReqStatus(3,"SYSTEM ERROR: Reseller FUC configurations missing").
   RETURN.
END.

DEF STREAM sErr.
DEF STREAM sout.

DEF BUFFER bMsRequest FOR MsRequest.

/* previous week from sunday to saturday */
ASSIGN
   ldeTsFrom         = Func.Common:mMake2DT(MsRequest.ReqDtParam1,0)
   ldeTsTo           = Func.Common:mMake2DT(MsRequest.ReqDtParam2,86399)
   liCurrentPeriod   = YEAR(TODAY) * 100 + MONTH(TODAY)
   lcTFBank          = MsRequest.ReqCParam1
   lcRootDir         = fCParam("TermFinance","OutRootDir")
   lcRootDirCetelem  = fCParam("TermFinance","OutRootDirCetelem").

IF NOT lcRootDir > "" THEN DO:
   fReqStatus(3,"SYSTEM ERROR: Output directory not defined").
   RETURN.
END.

IF lcTFBank = {&TF_BANK_CETELEM} AND NOT lcRootDirCetelem > "" THEN DO:
   fReqStatus(3,"SYSTEM ERROR: Output directory for Cetelem files not defined").
   RETURN.
END.

ASSIGN
   lcSpoolDir        = lcRootDir          + "spool/"
   lcOutDir          = lcRootDir          + "outgoing/"
   lcLogDir          = lcRootDir          + "logs/"
   lcSpoolDirCetelem = lcRootDirCetelem   + "spool/"
   lcOutDirCetelem   = lcRootDirCetelem   + "outgoing/"
   lcLogDirCetelem   = lcRootDirCetelem   + "logs/"
   lcErrorFile = lcLogDir + "ALTASYOIGO_" + 
      STRING(MsRequest.MsRequest) + "_" +
      STRING(YEAR(TODAY) * 10000 + 
             MONTH(TODAY) * 100 + 
             DAY(TODAY)) + "_" +
      STRING(TIME) + ".log".

CASE lcTFBank:
   WHEN {&TF_BANK_UNOE} THEN
      lcFile     = lcSpoolDir + "ALTASYOIGO.txt".
   WHEN {&TF_BANK_SABADELL} THEN
      lcFile     = lcSpoolDir + "ALTASYOIGO_SABADELL_" +
                    STRING(YEAR(TODAY) * 10000 + 
                           MONTH(TODAY) * 100 + 
                           DAY(TODAY)) + ".txt" .
   WHEN {&TF_BANK_CETELEM} THEN
      lcFile     = lcSpoolDirCetelem + "ALTASYOIGO_CETELEM_" +
                    STRING(YEAR(TODAY) * 10000 + 
                           MONTH(TODAY) * 100 + 
                           DAY(TODAY)) + ".txt" .
   OTHERWISE DO:
      fReqStatus(3,"ERROR: Unsupported bank code").
      RETURN.
   END.
END.

FUNCTION fDate2String RETURNS CHAR
   (idaDate AS DATE):

   IF idaDate = ? THEN RETURN "".

   RETURN STRING(YEAR(idaDate),"9999") +
          STRING(MONTH(idaDate),"99")  +
          STRING(DAY(idaDate),"99").

END FUNCTION.

FUNCTION fErrorLog RETURN LOGICAL(
   iiOrderId AS INT,
   icErrorText AS CHAR):
    
   IF liErrors EQ 0 THEN 
      OUTPUT STREAM sErr TO VALUE(lcErrorFile) APPEND.
      
   liErrors = liErrors + 1.

   PUT STREAM sErr UNFORMATTED
      Func.Common:mTS2HMS(Func.Common:mMakeTS()) ":"
      iiOrderId ":" icErrorText skip.

END.

DEFINE TEMP-TABLE ttProfession NO-UNDO
   FIELD profession AS CHAR
   FIELD contractType AS CHAR
   FIELD activityCode AS CHAR
INDEX profession IS PRIMARY UNIQUE profession.
   
FOR EACH TMSCodes NO-LOCK WHERE
         TMSCodes.TableName = "OrderCustomer" AND
         TMSCodes.FieldName = "Profession":

   IF NUM-ENTRIES(TMSCodes.ConfigValue) NE 4 THEN DO:
      fReqStatus(3,"SYSTEM_ERROR: Incorrect profession configurations").
      RETURN.
   END.

   CREATE ttProfession.
   IF lcTFBank = {&TF_BANK_UNOE} THEN ASSIGN
      ttProfession.Profession   = TMSCodes.CodeValue
      ttProfession.contractType = ENTRY(3,TMSCodes.ConfigValue)
      ttProfession.activityCode = ENTRY(4,TMSCodes.ConfigValue).
   ELSE ASSIGN
      ttProfession.Profession   = TMSCodes.CodeValue
      ttProfession.contractType = ENTRY(1,TMSCodes.ConfigValue)
      ttProfession.activityCode = ENTRY(2,TMSCodes.ConfigValue).
END.

IF NOT CAN-FIND(FIRST ttProfession) THEN DO:
   fReqStatus(3,"SYSTEM_ERROR: Incorrect profession configurations").
   RETURN.
END.

OUTPUT STREAM sout TO VALUE(lcFile).

DEFINE TEMP-TABLE ttOrderCustomer NO-UNDO LIKE OrderCustomer.

ORDER_LOOP:
FOR EACH FixedFee EXCLUSIVE-LOCK WHERE
         FixedFee.FinancedResult = {&TF_STATUS_WAITING_SENDING},
   FIRST OrderTimeStamp NO-LOCK WHERE
         OrderTimeStamp.Brand = Syst.Var:gcBrand AND
         OrderTimeStamp.OrderId = FixedFee.OrderID AND
         OrderTimeStamp.RowType = {&ORDERTIMESTAMP_DELIVERY} AND
         OrderTimeStamp.TimeStamp <= ldeTSTo,
   FIRST Order NO-LOCK WHERE
         Order.Brand = Syst.Var:gcBrand AND
         Order.OrderId = FixedFee.OrderID,
   FIRST OrderCustomer NO-LOCK WHERE
         OrderCustomer.Brand = Syst.Var:gcBrand AND
         OrderCustomer.OrderId = Order.OrderId AND
         OrderCustomer.RowType = 1 BY OrderTimeStamp.TimeStamp:

   EMPTY TEMP-TABLE ttOrderCustomer NO-ERROR.

   Func.Common:mTS2Date(Order.CrStamp, OUTPUT ldaOrderDate).

   IF FixedFee.BillCode EQ "RVTERM" THEN DO:

      FIND SingleFee NO-LOCK WHERE
           SingleFee.Brand       = Syst.Var:gcBrand AND
           SingleFee.Custnum     = Order.CustNum AND
           SingleFee.HostTable   = "Mobsub" AND
           SingleFee.KeyValue    = STRING(Order.MsSeq) AND
           SingleFee.OrderID     = FixedFee.OrderID AND
           SingleFee.CalcObj     = "RVTERM" NO-ERROR.
      
      IF NOT AVAIL SingleFee THEN DO:
         fErrorLog(Order.OrderID,"ERROR:Q25 fee not found").
         FixedFee.FinancedResult = {&TF_STATUS_YOIGO_ANALYZE_FAILED}.
         NEXT ORDER_LOOP. 
      END.
      CASE SingleFee.BillCode:
         WHEN "RVTERM1EF" THEN IF lcTFBank NE "0049" THEN NEXT ORDER_LOOP.
         WHEN "RVTERMBSF" THEN IF lcTFBank NE "0081" THEN NEXT ORDER_LOOP.
         WHEN "RVTERMBCF" THEN IF lcTFBank NE "0225" THEN NEXT ORDER_LOOP.
         OTHERWISE DO:
            fErrorLog(Order.OrderID,"ERROR:Q25 fee financed by Yoigo").
            FixedFee.FinancedResult = {&TF_STATUS_YOIGO_ANALYZE_FAILED}.
            NEXT ORDER_LOOP. 
         END.
      END.
   END.

   /* direct channels */
   IF INDEX(Order.OrderChannel, "POS") = 0 THEN DO:

      IF CAN-FIND(FIRST OrderAction WHERE
                        OrderAction.Brand    = Syst.Var:gcBrand AND
                        OrderAction.OrderId  = Order.OrderId AND
                        OrderAction.ItemType = "TerminalFinancing" AND
                        OrderAction.ItemKey  = "0225") THEN DO:
         IF lcTFBank NE {&TF_BANK_CETELEM} AND
            FixedFee.BillCode NE "RVTERM" THEN NEXT ORDER_LOOP.
      END.
      ELSE DO:
         IF lcTFBank NE {&TF_BANK_UNOE} AND
            FixedFee.BillCode NE "RVTERM" THEN NEXT ORDER_LOOP.
      END.

      IF LOOKUP(Order.OrderChannel,"self,renewal,fusion_self") > 0 THEN ASSIGN
         lcFUC[1] = "332577543" 
         lcFUC[2] = "332577576".
      ELSE ASSIGN
         lcFUC[1] = "332577626" 
         lcFUC[2] = "332577683".
   END.
   /* YTS-8634 */
   ELSE IF Order.Reseller EQ "" AND 
           FixedFee.BillCode EQ "RVTERM" THEN DO:
      ASSIGN
         lcFUC[1] = "323841452" 
         lcFUC[2] = "332296599".
   END.
   /* indirect channels */
   ELSE IF LOOKUP(Order.Reseller,lcResellers) > 0 THEN DO:

      FIND Reseller NO-LOCK WHERE
           Reseller.Brand = Syst.Var:gcBrand AND
           Reseller.Reseller = Order.Reseller NO-ERROR.

      IF NOT AVAIL Reseller THEN DO:
         fErrorLog(Order.OrderID,
                   SUBST("ERROR:Unknown reseller: &1", Order.reseller)).
         FixedFee.FinancedResult = {&TF_STATUS_YOIGO_ANALYZE_FAILED}.
         NEXT ORDER_LOOP.
      END.
      
      ASSIGN
         lcFUC[1] = reseller.fuc1
         lcFUC[2] = reseller.fuc2.

      IF FixedFee.BillCode NE "RVTERM" THEN DO: 
         FIND FIRST ResellerTF NO-LOCK USE-INDEX ResellerTF WHERE
                    ResellerTF.Brand = Reseller.Brand AND
                    ResellerTF.Reseller = Reseller.Reseller AND
                    ResellerTF.ValidFrom <= ldaOrderDate NO-ERROR.
         IF NOT AVAIL ResellerTF THEN DO:
            fErrorLog(Order.OrderID,SUBST("ERROR:Missing ResellerTF: &1",
                                           Reseller.Reseller)).
            FixedFee.FinancedResult = {&TF_STATUS_YOIGO_ANALYZE_FAILED}.
            NEXT ORDER_LOOP. 
         END.

         IF ResellerTF.TFBank NE lcTFBank THEN DO:
            IF ResellerTF.TFBank EQ "0000"
               THEN FixedFee.FinancedResult = {&TF_STATUS_YOIGO}.
            NEXT ORDER_LOOP.
         END.
      END.

   END.
   ELSE DO:
      fErrorLog(Order.OrderID,SUBST("WARNING:Unsupported reseller: &1",
                                 Order.Reseller)).
      FixedFee.FinancedResult = {&TF_STATUS_YOIGO_ANALYZE_FAILED}.
      NEXT ORDER_LOOP. 
   END.
   
   FIND Mobsub NO-LOCK WHERE
        Mobsub.MsSeq = Order.MsSeq NO-ERROR.
   IF NOT AVAIL Mobsub THEN DO:
      FixedFee.FinancedResult = {&TF_STATUS_YOIGO_SUB_TERMINATED}.
      NEXT ORDER_LOOP.
   END.
   
   IF FixedFee.BillCode EQ "RVTERM" THEN DO:
      FIND FIRST Customer NO-LOCK WHERE
                 Customer.Custnum = Mobsub.Custnum NO-ERROR.
      IF NOT AVAIL Customer THEN NEXT.
      BUFFER-COPY Customer EXCEPT Language TO ttOrderCustomer.
      ASSIGN
         ttOrderCustomer.CustTitle  = Customer.HonTitle
         ttOrderCustomer.CustId     = Customer.OrgId
         ttOrderCustomer.CustIdType = Customer.CustIdType
         ttOrderCustomer.BankCode   = Customer.BankAcct
         ttOrderCustomer.SurName1   = Customer.Custname
         ttOrderCustomer.Company    = Customer.CompanyName
         ttOrderCustomer.OutBankMarketing = Customer.OutMarkBank.
   END.
   ELSE BUFFER-COPY OrderCustomer TO ttOrderCustomer.

   CASE ttOrderCustomer.CustIdType:
      WHEN "NIF" THEN lcCustIdType = "05".
      WHEN "NIE" THEN lcCustIdType = "01".
      OTHERWISE DO: 
         fErrorLog(Order.OrderID,
                   SUBST("ERROR:Unsupported customer ID type: &1",
                         ttOrderCustomer.CustIdType)).
         FixedFee.FinancedResult = {&TF_STATUS_YOIGO_ANALYZE_FAILED}.
         NEXT ORDER_LOOP.
      END.
   END.

   IF LOOKUP(ttOrderCustomer.CustTitle,
             "Sr.,Mr.,Sr,Mr") > 0 THEN lcGender = "01".
   ELSE IF LOOKUP(ttOrderCustomer.CustTitle,
           "Mrs.,Sra.,Mrs,Sra") > 0 THEN lcGender = "02".
   ELSE DO:
      fErrorLog(Order.OrderID,SUBST("ERROR:Unknown customer title: &1",
                              ttOrderCustomer.CustTitle)).
      FixedFee.FinancedResult = {&TF_STATUS_YOIGO_ANALYZE_FAILED}.
      NEXT ORDER_LOOP.
   END.

   FIND Nationality NO-LOCK WHERE
        Nationality.Nationality = ttOrderCustomer.Nationality NO-ERROR.
   IF NOT AVAIL Nationality THEN DO:
      fErrorLog(Order.OrderID,SUBST("ERROR:Unknown nationality: &1",
                              ttOrderCustomer.Nationality)).
      FixedFee.FinancedResult = {&TF_STATUS_YOIGO_ANALYZE_FAILED}.
      NEXT ORDER_LOOP.
   END.

   IF NOT Nationality.TFNationality > "" THEN DO:
      fErrorLog(Order.OrderID,SUBST("SYSTEM ERROR:Undefined TFNationality: &1",
                              ttOrderCustomer.Nationality)).
      FixedFee.FinancedResult = {&TF_STATUS_YOIGO_ANALYZE_FAILED}.
      NEXT ORDER_LOOP.
   END.
  
   FIND FIRST ttProfession NO-LOCK WHERE
              ttProfession.Profession = ttOrderCustomer.Profession NO-ERROR.

   IF NOT AVAIL ttProfession AND NOT fIsDirectChannelCetelemOrder(BUFFER Order) THEN DO:
      fErrorLog(Order.OrderID,SUBST("ERROR:Unknown profession: &1",
                ttOrderCustomer.profession)).
      FixedFee.FinancedResult = {&TF_STATUS_YOIGO_ANALYZE_FAILED}.
      NEXT ORDER_LOOP.
   END.

   IF FixedFee.BillCode EQ "RVTERM" THEN
      FIND FIRST FMItem NO-LOCK WHERE
                 FMItem.Brand     = Syst.Var:gcBrand AND
                 FMItem.FeeModel  = FixedFee.FeeModel AND
                 FMItem.ToDate   >= FixedFee.BegDate AND
                 FMItem.FromDate <= FixedFee.BegDate NO-ERROR.
   ELSE
      FIND FIRST FMItem NO-LOCK WHERE
                 FMItem.Brand     = Syst.Var:gcBrand AND
                 FMItem.FeeModel  = FixedFee.FeeModel AND
                 FMItem.ToDate   >= ldaOrderDate AND
                 FMItem.FromDate <= ldaOrderDate NO-ERROR.
   IF NOT AVAIL FMItem THEN DO:
      fErrorLog(Order.OrderID,
         SUBST("SYSTEM_ERROR:FeeModel not defined for &1",FixedFee.CalcObj)).
      FixedFee.FinancedResult = {&TF_STATUS_YOIGO_ANALYZE_FAILED}.
      NEXT ORDER_LOOP.
   END.
   
   CASE FMItem.FFItemQty:
      WHEN 12 THEN DO:
         /* YOT-4735 Temporarily fix, UNOE financing activated.
            6666 will mean that the bank will reject it.
            Temporary change from 6666 to 0212.
            Older change: terminal_financing_read.p TF_Q25_EXTENSION_CODES 
         IF lcTFBank EQ {&TF_BANK_UNOE} THEN 
            lcPayTermType[1] = "6666".  
         ELSE */
            lcPayTermType[1] = "0212".
      END.
      WHEN 18 THEN lcPayTermType[1] = "0018".
      WHEN 24 THEN DO:
         /* YTS-6873: Own code for non-residual fee cass */
         IF ldaOrderDate >= 5/1/2015 THEN 
            lcPayTermType[1] = "0034".
         ELSE
            lcPayTermType[1] = "0024".
      END.
      OTHERWISE DO:
         fErrorLog(Order.OrderID,
            SUBST("SYSTEM_ERROR:Unsupported PAYTERM contract length &1",fmitem.FFItemQty)).
         FixedFee.FinancedResult = {&TF_STATUS_YOIGO_ANALYZE_FAILED}.
         NEXT ORDER_LOOP.
      END.
   END.

   IF CAN-FIND(FIRST bMsRequest WHERE
                     bMsRequest.MsSeq = Order.MsSeq AND
                     bMsRequest.ReqType = 10 AND
                     LOOKUP(STRING(bMsRequest.ReqStat),
                        {&REQ_INACTIVE_STATUSES}) = 0) THEN DO:
      FixedFee.FinancedResult = {&TF_STATUS_YOIGO_ACC}.
      NEXT ORDER_LOOP.
   END.
   
   IF Mobsub.Custnum NE OrderCustomer.Custnum THEN DO:
      FixedFee.FinancedResult = {&TF_STATUS_YOIGO_ACC}.
      NEXT ORDER_LOOP.
   END.

   /* check if ACC is already done during order creation and 
      delivery time */  
   IF CAN-FIND(FIRST bMsRequest WHERE
                     bMsRequest.MsSeq = Order.MsSeq AND
                     bMsRequest.ReqType = 10 AND
                     bMsRequest.ActStamp >= Order.CrStamp AND
                     bMsRequest.ReqStatus NE 4) THEN DO:
      FixedFee.FinancedResult = {&TF_STATUS_YOIGO_ACC}.
      NEXT ORDER_LOOP.
   END.

   IF FixedFee.EndPeriod <= liCurrentPeriod THEN DO:
      FixedFee.FinancedResult = {&TF_STATUS_YOIGO_FF_TERMINATED}.
      NEXT ORDER_LOOP.
   END.

   ASSIGN
      liFFItemCount = 0
      ldeFFItemAmount = 0.
   FOR EACH FFItem OF FixedFee NO-LOCK:
      ASSIGN
         liFFItemCount = liFFItemCount + 1
         ldeFFItemAmount = ldeFFItemAmount + FFItem.Amt.
   END.
   
   IF liFFItemCount NE FMItem.FFItemQty OR 
      (FixedFee.BillCode BEGINS "PAYTERM" AND
       ROUND(ldeFFItemAmount,2) NE ROUND(fmitem.FFItemQty * fmitem.Amount,2)) THEN DO:
      FixedFee.FinancedResult = {&TF_STATUS_YOIGO_FF_CHANGED}.
      NEXT ORDER_LOOP.
   END.

   ldeTotalAmount = ROUND(ldeFFItemAmount,2).

   IF LENGTH(ttOrderCustomer.BankCode) EQ 24 THEN 
      lcBankCode = SUBSTRING(ttOrderCustomer.BankCode,5).
   ELSE lcBankCode = ttOrderCustomer.BankCode.
   
   IF NOT FixedFee.BillCode BEGINS "PAYTERM" THEN RELEASE SingleFee.
   ELSE
   FIND FIRST SingleFee NO-LOCK WHERE
              SingleFee.Brand = Syst.Var:gcBrand AND
              SingleFee.Custnum = FixedFee.Custnum AND
              SingleFee.HostTable = FixedFee.HostTable AND
              SingleFee.KeyValue = Fixedfee.KeyValue AND
              SingleFee.SourceKey = FixedFee.SourceKey AND
              SingleFee.SourceTable = FixedFee.SourceTable AND
              SingleFee.CalcObj = "RVTERM" AND
              SingleFee.Amt > 0 NO-ERROR.
      
   IF AVAIL SingleFee THEN DO:

      ASSIGN
         ldeRVPerc = TRUNC(SingleFee.Amt / 
                          (ldeTotalAmount + SingleFee.Amt) * 100 + 0.05,1)
         ldeRVAmt = SingleFee.Amt.

      FIND FIRST TFConf NO-LOCK WHERE
                 TFConf.RVPercentage = ldeRVPerc AND
                 TFConf.ValidTo >= ldaOrderDate AND
                 TFConf.ValidFrom <= ldaOrderDate NO-ERROR.

      IF NOT AVAIL TFConf THEN DO:
         fErrorLog(Order.OrderID,
            SUBST("SYSTEM_ERROR:Terminal financing configuration not found")).
         FixedFee.FinancedResult = {&TF_STATUS_YOIGO_ANALYZE_FAILED}.
         NEXT ORDER_LOOP.
      END.

      ASSIGN
         lcPayTermType[1] = TFConf.PaytermCode WHEN TFConf.RVPercentage NE 0
         lcPayTermType[2] = TFConf.ResidualCode.
   END.

   ELSE ASSIGN
      ldeRVPerc = 0
      ldeRVAmt  = 0.

   ASSIGN
      FixedFee.TFBank         = lcTFBank
      FixedFee.FinancedResult = {&TF_STATUS_SENT_TO_BANK}
      liLineNum = liLineNum + 1.

   FIND FIRST FixedFeeTF EXCLUSIVE-LOCK WHERE
              FixedFeeTF.FFNum = FixedFee.FFNum NO-ERROR.

   IF NOT AVAIL FixedFeeTF THEN DO:
      CREATE FixedFeeTF.
      ASSIGN
         FixedFeeTF.FFNum  = FixedFee.FFNum.
   END.

   ASSIGN
      FixedFeeTF.OrderId   = FixedFee.OrderId
      FixedFeeTF.BankDate  = TODAY
      FixedFeeTF.TFBank    = lcTFBank
      FixedFeeTF.OrgId     = ttOrderCustomer.CustId
      FixedFeeTF.Amount = ldeTotalAmount
      FixedFeeTF.ResidualAmount = SingleFee.Amt 
         WHEN AVAIL SingleFee.
   
   RUN pPrintLine(lcFUC[1],
                  ldeTotalAmount,
                  lcPayTermType[1]).
   
   IF AVAIL SingleFee THEN
   RUN pPrintLine(lcFUC[2],
                  SingleFee.Amt,
                  lcPayTermType[2]).

END.

OUTPUT STREAM sout CLOSE.

IF liErrors > 0 THEN
   OUTPUT STREAM sErr CLOSE.

IF lcTFBank = {&TF_BANK_CETELEM} 
THEN fMove2TransDir(lcFile, "", lcOutDirCetelem).
ELSE fMove2TransDir(lcFile, "", lcOutDir).

fReqStatus(2,"").

PROCEDURE pPrintLine:

   DEF INPUT PARAM icFUC AS CHAR NO-UNDO. 
   DEF INPUT PARAM ideTotalAmount AS DEC NO-UNDO. 
   DEF INPUT PARAM icPaytermType AS CHAR NO-UNDO. 
   
   DEF VAR lcTotalAmount AS CHAR NO-UNDO. 
   DEF VAR lcRVAmt  AS CHAR NO-UNDO. 
   DEF VAR lcRVPerc AS CHAR NO-UNDO. 
      
   ASSIGN
      lcTotalAmount = REPLACE(REPLACE(TRIM(STRING(ideTotalAmount,"->>>>>>9.99")),",",""),".","")
      lcRVAmt       = REPLACE(REPLACE(TRIM(STRING(ldeRVAmt,"->>>>>9.99")),",",""),".","")
      lcRVPerc      = REPLACE(REPLACE(TRIM(STRING(ldeRVPerc,"->>>9.99")),",",""),".","").
   
   PUT STREAM sout 
   /*NVENDCAPCTA*/   " " FORMAT "X(8)"
   /*NORDEN*/        STRING(liLineNum,"999999") FORMAT "X(6)"
   /*NCTR*/          "201080" FORMAT "x(6)"
   /*CTDOCUPRS*/     UPPER(lcCustIdType) FORMAT "X(2)"
   /*NDOCUPRS*/      UPPER(ttOrderCustomer.CustId) FORMAT "X(9)"
   /*FILLER VACIO */ " " FORMAT "X(1)"
   /*DREDUPRS*/      UPPER(ttOrderCustomer.Surname1) + " " + 
                     UPPER(ttOrderCustomer.Surname2) + "," + 
                     UPPER(ttOrderCustomer.FirstName) FORMAT "X(26)"
   /*NOMBRECLI*/     UPPER(ttOrderCustomer.FirstName) FORMAT "X(30)"
   /*APELLIDO1*/     UPPER(ttOrderCustomer.SurName1) FORMAT "X(30)"
   /*APELLIDO2*/     UPPER(ttOrderCustomer.SurName2) FORMAT "X(30)"
   /*TDIRECCI*/      UPPER(ttOrderCustomer.Address) FORMAT "X(40)"
   /*FILLER_VACIO*/  " " FORMAT "X(24)"
   /*DPOBLACI*/      UPPER(ttOrderCustomer.PostOffice) FORMAT "X(30)"
   /*CPOSTAL*/       UPPER(ttOrderCustomer.ZipCode) FORMAT "X(5)"
   /*TELEFONO*/      UPPER(MobSub.CLI) FORMAT "X(10)"
   /*FNACCLI*/       fDate2String(ttOrderCustomer.BirthDay) FORMAT "X(8)"
   /*COSEXO */       lcGender FORMAT "X(2)"
   /*COESTCIV*/      "01" FORMAT "X(2)"
   /*FALTACLI*/      FILL("0",8) FORMAT "X(8)"
   /*COACTPROF*/     (IF AVAIL ttProfession
                      THEN ttProfession.activityCode ELSE "") FORMAT "X(4)"
   /*INGBRU*/        "0000000060101" FORMAT "X(13)"
   /*DEMPTRAB*/      UPPER(ttOrderCustomer.Company) FORMAT "X(30)"
   /*TDIREMP*/       STRING(Order.MsSeq) FORMAT "X(10)"
                     STRING(Order.OrderID) FORMAT "X(10)"
                     FILL("0",6 - LENGTH(lcRVAmt)) + 
                        lcRVAmt FORMAT "X(6)"
                     FILL("0",4 - LENGTH(lcRVPerc)) + 
                        lcRVPerc FORMAT "X(4)"
   /*DPOBLEMP*/      "MADRID" FORMAT "X(30)"
   /*CPOEMP*/        "28000" FORMAT "X(5)"
   /*TELEFEMP*/      STRING(ttOrderCustomer.Custnum) FORMAT "X(10)"
   /*TELEFEMP2*/     " " FORMAT "X(10)"
   /*TIPOCONTR*/     (IF AVAIL ttProfession 
                      THEN ttProfession.contractType ELSE "") FORMAT "X(4)"
   /*ANTEMPRES*/     "01" FORMAT "X(2)"
   /*TIPVIVIE*/      "01" FORMAT "X(2)"
   /*CRGVIVIE*/      "00" FORMAT "X(2)"
   /*VALVIVIE*/      "0000000000601" FORMAT "X(13)"
   /*NESF*/          UPPER(SUBSTRING(lcBankCode,1,4)) FORMAT "X(4)"
   /*NOSF*/          UPPER(SUBSTRING(lcBankCode,5,4)) FORMAT "X(4)"
   /*CMODCBAN*/      UPPER(SUBSTRING(lcBankCode,9,2)) FORMAT "X(2)"
   /*NCTAESF*/       UPPER(SUBSTRING(lcBankCode,11,10))
                     FORMAT "X(10)"
   /*TIPOCLNTE*/     "T" FORMAT "X(1)"
   /*ILMTECTA*/      "0000000030000" FORMAT "X(13)"
   /*NFPADEFCTA*/    "00" FORMAT "X(2)"
   /*CFPARVL*/       "00" FORMAT "X(2)"
   /*PORMENSRVL*/    FILL("0",7) FORMAT "X(7)"
   /*IRCBRVL*/       FILL("0",13) FORMAT "X(13)"
   /*CMOTRCLI*/      "00" FORMAT "X(2)"
   /*NESCAPCTA*/     icFUC FORMAT "X(9)"
   /*CSEGU*/         "02" FORMAT "X(2)"
   /*TIPSEGU*/       "00" FORMAT "X(2)"
   /*CIDIOMA*/       "01" FORMAT "X(2)"
   /*IMPFINANCIADO*/ FILL("0",13 - LENGTH(lcTotalAmount)) +
                     lcTotalAmount FORMAT "X(13)"
   /*CMONEDAOPER*/   "02" FORMAT "X(2)"
   /*FORMA_DE_PAGO*/ icPayTermType FORMAT "X(4)"
   /*FILLER VACIO*/  " " FORMAT "X(4)"
   /*DES-MAIL*/      (IF lcTFBank EQ {&TF_BANK_CETELEM} THEN ttOrderCustomer.Email
                      ELSE " ") FORMAT "X(50)"
   /*COD-PAIS*/      "0011" FORMAT "X(4)"
   /*XTI-ROBINSON*/  (IF lcTFBank EQ {&TF_BANK_UNOE} THEN "S"
                      ELSE STRING(ttOrderCustomer.OutBankMarketing,"S/N"))
                      FORMAT "X(1)"
   /*COD-PAISNACIM*/ Nationality.TFNationality FORMAT "X(4)"
   /*COD-PAISNACION*/ Nationality.TFNationality FORMAT "X(4)"
   /*FILLER VACIO*/  " " FORMAT "X(55)"
   /* RESULTADO OPERACION */ " " FORMAT "X(2)".
   PUT STREAM sout CONTROL CHR(13) CHR(10).

END PROCEDURE. 

