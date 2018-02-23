/* ----------------------------------------------------------------------
  MODULE .......: Ar/terminal_financing_dump.p
  TASK .........: Create a dump file for Termianal Financing 
---------------------------------------------------------------------- */

DEFINE INPUT  PARAMETER icDumpID      AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER icFile        AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER icDumpMode    AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER idLastDump    AS DECIMAL    NO-UNDO.
DEFINE INPUT  PARAMETER icEventSource AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER icEventFields AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER oiEvents      AS INTEGER    NO-UNDO.
DEFINE OUTPUT PARAMETER olInterrupted AS LOGICAL    NO-UNDO.

{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Func/cparam2.i}
{Syst/dumpfile_run.i}
{Syst/tmsconst.i}
{Func/forderstamp.i}
{Func/msreqfunc.i}
{Func/ftransdir.i}
{Func/financed_terminal.i}

DEFINE VARIABLE lcNumeric         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDelimiter       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDumpFields      AS CHARACTER NO-UNDO.

DEFINE VARIABLE ldaModified       AS DATE      NO-UNDO.
DEFINE VARIABLE liTimeMod         AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldtLastDump       AS DATETIME  NO-UNDO.
DEFINE VARIABLE ldeTotal          AS DECIMAL   NO-UNDO. 
DEFINE VARIABLE liCount           AS INTEGER   NO-UNDO. 
DEFINE VARIABLE ldeExtensions     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE liQ25             AS INTEGER   NO-UNDO. 
DEFINE VARIABLE liPayterm         AS INTEGER   NO-UNDO. 
DEFINE VARIABLE liExtension       AS INTEGER   NO-UNDO.  
DEFINE VARIABLE ldePayterms       AS DECIMAL   NO-UNDO. 
DEFINE VARIABLE ldeQ25            AS DECIMAL   NO-UNDO. 
DEFINE VARIABLE liCounter         AS INTEGER   NO-UNDO.

DEFINE VARIABLE ldaOrderDate      AS DATE      NO-UNDO. 
DEFINE VARIABLE ldeTotalAmount    AS DECIMAL   NO-UNDO. 
DEFINE VARIABLE liErrors          AS INTEGER   NO-UNDO. 
DEFINE VARIABLE liCurrentPeriod   AS INTEGER   NO-UNDO. 
DEFINE VARIABLE lcFile            AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liFFItemCount     AS INTEGER   NO-UNDO. 
DEFINE VARIABLE ldeFFItemAmount   AS DECIMAL   NO-UNDO. 
DEFINE VARIABLE liLineNum         AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcBankCode        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldeRVPerc         AS DECIMAL   NO-UNDO. 
DEFINE VARIABLE ldeRVAmt          AS DECIMAL   NO-UNDO. 
DEFINE VARIABLE lcTFBank          AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcResellers       AS CHARACTER NO-UNDO. 

DEFINE STREAM sFile.
DEFINE BUFFER bMsRequest FOR MSRequest.
DEFINE TEMP-TABLE ttOrderCustomer NO-UNDO LIKE OrderCustomer.

FUNCTION fErrorLog RETURN LOGICAL( iiOrderId AS INTEGER, icErrorText AS CHARACTER):
END.

DO ON ERROR UNDO , LEAVE :
    
    lcNumeric = SESSION:NUMERIC-FORMAT.
    liCurrentPeriod  = YEAR(TODAY) * 100 + MONTH(TODAY).

    FIND FIRST DumpFile WHERE DumpFile.DumpID = icDumpID NO-LOCK NO-ERROR.
    IF AVAILABLE DumpFile THEN DO:
        lcDelimiter  = fInitDelimiter(DumpFile.DumpDelimiter).
        IF DumpFile.DecimalPoint = "." THEN 
            SESSION:NUMERIC-FORMAT = "AMERICAN".
        ELSE 
            SESSION:NUMERIC-FORMAT = "EUROPEAN".
    END.
    ELSE 
        ASSIGN 
            lcDelimiter            = CHR(9)
            SESSION:NUMERIC-FORMAT = "AMERICAN".
            
    FOR EACH DFField OF DumpFile NO-LOCK 
       WHERE DFField.ToDate   >= TODAY 
         AND DFField.FromDate <= TODAY BY DFField.OrderNbr:
             
        lcDumpFields = lcDumpFields 
                     + (IF lcDumpFields > "" THEN "," ELSE "") 
                     + DFField.DFField.
    END.
    
    Func.Common:mSplitTS(idLastDump, OUTPUT ldaModified, OUTPUT liTimeMod).
    
    OUTPUT STREAM sFile TO VALUE(icFile).
        file_blk:
        DO ON ERROR UNDO , LEAVE 
           ON QUIT UNDO, RETRY
           ON STOP UNDO, RETRY:
            IF RETRY THEN DO:
                olInterrupted = TRUE.
                LEAVE file_blk.
            END.
            RUN pFetchAndWriteData.
        END. 
    OUTPUT STREAM sFile CLOSE.
    
        
END.    

PROCEDURE pFetchAndWriteData:

    DO liCounter = 1 TO NUM-ENTRIES({&TF_BANK_CODES}):
        EMPTY TEMP-TABLE ttOrderCustomer.
        lcTFBank = ENTRY(liCounter,{&TF_BANK_CODES}).
        
        ORDER_LOOP:
        FOR EACH FixedFee NO-LOCK WHERE
                 FixedFee.FinancedResult  = {&TF_STATUS_WAITING_SENDING},
            FIRST OrderTimeStamp NO-LOCK WHERE
                  OrderTimeStamp.Brand    = Syst.Var:gcBrand AND
                  OrderTimeStamp.OrderId  = FixedFee.OrderID AND
                  OrderTimeStamp.RowType  = {&ORDERTIMESTAMP_DELIVERY},
            FIRST Order NO-LOCK WHERE
                  Order.Brand             = Syst.Var:gcBrand AND
                  Order.OrderId           = FixedFee.OrderId ,
            FIRST OrderCustomer NO-LOCK WHERE
                  OrderCustomer.Brand     = Syst.Var:gcBrand AND
                  OrderCustomer.OrderId   = Order.OrderId    AND
                  OrderCustomer.RowType   = 1 BY OrderTimeStamp.TimeStamp:
        
            Func.Common:mTS2Date(Order.CrStamp, OUTPUT ldaOrderDate).
            
            IF FixedFee.BillCode EQ "RVTERM" THEN 
            DO:
                FIND SingleFee NO-LOCK WHERE
                    SingleFee.Brand       = Syst.Var:gcBrand AND
                    SingleFee.Custnum     = Order.CustNum AND
                    SingleFee.HostTable   = "Mobsub" AND
                    SingleFee.KeyValue    = STRING(Order.MsSeq) AND
                    SingleFee.OrderID     = FixedFee.OrderID AND
                    SingleFee.CalcObj     = "RVTERM" NO-ERROR.
              
                IF NOT AVAILABLE SingleFee THEN 
                DO:
                    fErrorLog(Order.OrderID,"ERROR:Q25 fee not found").
                    NEXT ORDER_LOOP. 
                END.
                CASE SingleFee.BillCode:
                    WHEN "RVTERM1EF" THEN IF lcTFBank NE "0049" THEN NEXT ORDER_LOOP.
                    WHEN "RVTERMBSF" THEN IF lcTFBank NE "0081" THEN NEXT ORDER_LOOP.
                    WHEN "RVTERMBCF" THEN IF lcTFBank NE "0225" THEN NEXT ORDER_LOOP.
                    OTHERWISE 
                    DO:
                        fErrorLog(Order.OrderID,"ERROR:Q25 fee financed by Yoigo").
                        NEXT ORDER_LOOP. 
                    END.
                END.
            END.
        
            RELEASE resellertf.
    
            /* direct channels */
            IF INDEX(Order.OrderChannel, "POS") = 0 THEN 
            DO:
                IF CAN-FIND(FIRST OrderAction WHERE
                    OrderAction.Brand    = Syst.Var:gcBrand AND
                    OrderAction.OrderId  = Order.OrderId AND
                    OrderAction.ItemType = "TerminalFinancing" AND
                    OrderAction.ItemKey  = "0225") THEN 
                DO:
                    IF lcTFBank NE {&TF_BANK_CETELEM} AND
                        FixedFee.BillCode NE "RVTERM" THEN NEXT ORDER_LOOP.
                END.
                ELSE 
                DO:
                    IF lcTFBank NE {&TF_BANK_UNOE} AND
                        FixedFee.BillCode NE "RVTERM" THEN NEXT ORDER_LOOP.
                END.
            END.
            /* YTS-8634 */
            ELSE IF Order.Reseller EQ "" AND  
                 FixedFee.BillCode EQ "RVTERM" THEN 
            DO:
            END.
                /* indirect channels */
            ELSE IF LOOKUP(Order.Reseller,lcResellers) > 0 THEN 
            DO:
                FIND Reseller NO-LOCK WHERE
                     Reseller.Brand    = Syst.Var:gcBrand AND
                     Reseller.Reseller = Order.Reseller NO-ERROR.
        
                IF NOT AVAILABLE Reseller THEN 
                DO:
                    fErrorLog(Order.OrderID,
                        SUBST("ERROR:Unknown reseller: &1", Order.reseller)).
                    NEXT ORDER_LOOP.
                END.
              
                IF FixedFee.BillCode NE "RVTERM" THEN 
                DO: 
                    FIND FIRST ResellerTF NO-LOCK USE-INDEX ResellerTF WHERE
                               ResellerTF.Brand      = Reseller.Brand AND
                               ResellerTF.Reseller   = Reseller.Reseller AND
                               ResellerTF.ValidFrom <= ldaOrderDate NO-ERROR.
                    IF NOT AVAILABLE ResellerTF THEN 
                    DO:
                        fErrorLog(Order.OrderID,SUBST("ERROR:Missing ResellerTF: &1",
                            Reseller.Reseller)).
                        NEXT ORDER_LOOP. 
                    END.
                    IF ResellerTF.TFBank NE lcTFBank THEN 
                        NEXT ORDER_LOOP.
                END.
            END.
            ELSE 
            DO:
                fErrorLog(Order.OrderID,SUBST("WARNING:Unsupported reseller: &1",
                    Order.Reseller)).
                NEXT ORDER_LOOP. 
            END.
    
            FIND Mobsub NO-LOCK WHERE
                 Mobsub.MsSeq = Order.MsSeq NO-ERROR.
            IF NOT AVAILABLE Mobsub THEN 
            DO:
                NEXT ORDER_LOOP.
            END.
            IF FixedFee.BillCode EQ "RVTERM" THEN DO:
                FIND FIRST Customer NO-LOCK WHERE
                     Customer.Custnum = Mobsub.Custnum NO-ERROR.
                IF NOT AVAIL Customer THEN NEXT.
                BUFFER-COPY Customer EXCEPT Language TO ttOrderCustomer.
            END.
            ELSE 
                BUFFER-COPY OrderCustomer TO ttOrderCustomer.
        
            CASE ttOrderCustomer.CustIdType:
                WHEN "NIF" THEN .
                WHEN "NIE" THEN .
                OTHERWISE DO: 
                    fErrorLog(Order.OrderID,
                           SUBST("ERROR:Unsupported customer ID type: &1",
                                 ttOrderCustomer.CustIdType)).
                    NEXT ORDER_LOOP.
                END.
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
            IF NOT AVAILABLE FMItem THEN 
            DO:
                fErrorLog(Order.OrderID,
                    SUBST("SYSTEM_ERROR:FeeModel not defined for &1",FixedFee.CalcObj)).
                NEXT ORDER_LOOP.
            END.
           
            CASE FMItem.FFItemQty:
                WHEN 12 THEN .
                WHEN 18 THEN .
                WHEN 24 THEN . 
                OTHERWISE 
                DO:
                    fErrorLog(Order.OrderID,
                        SUBST("SYSTEM_ERROR:Unsupported PAYTERM contract length &1",fmitem.FFItemQty)).
                    NEXT ORDER_LOOP.
                END.
            END.
    
            IF CAN-FIND(FIRST bMsRequest WHERE
                              bMsRequest.MsSeq   = Order.MsSeq AND
                              bMsRequest.ReqType = 10 AND
                LOOKUP(STRING(bMsRequest.ReqStat), {&REQ_INACTIVE_STATUSES}) = 0) THEN NEXT ORDER_LOOP.
            
            IF Mobsub.Custnum NE OrderCustomer.Custnum THEN NEXT ORDER_LOOP.
    
            /* check if ACC is already done during order creation and 
               delivery time */  
            IF CAN-FIND(FIRST bMsRequest WHERE
                              bMsRequest.MsSeq     = Order.MsSeq   AND
                              bMsRequest.ReqType   = 10            AND
                              bMsRequest.ActStamp >= Order.CrStamp AND
                              bMsRequest.ReqStatus NE 4)           THEN NEXT ORDER_LOOP.
    
            IF FixedFee.EndPeriod <= liCurrentPeriod THEN NEXT ORDER_LOOP.
        
            ASSIGN
                liFFItemCount   = 0
                ldeFFItemAmount = 0.
            FOR EACH FFItem OF FixedFee NO-LOCK:
                ASSIGN
                    liFFItemCount   = liFFItemCount + 1
                    ldeFFItemAmount = ldeFFItemAmount + FFItem.Amt.
            END.
    
            IF liFFItemCount NE FMItem.FFItemQty OR 
                (FixedFee.BillCode BEGINS "PAYTERM" AND
                ROUND(ldeFFItemAmount,2) NE ROUND(fmitem.FFItemQty * fmitem.Amount,2)) THEN 
            DO:
                NEXT ORDER_LOOP.
            END.
    
            ldeTotalAmount = ROUND(ldeFFItemAmount,2).
        
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
              
            IF AVAILABLE SingleFee THEN 
            DO:
        
                ASSIGN
                    ldeRVPerc = TRUNC(SingleFee.Amt / 
                                  (ldeTotalAmount + SingleFee.Amt) * 100 + 0.05,1)
                    ldeRVAmt  = SingleFee.Amt.
        
                FIND FIRST TFConf NO-LOCK WHERE
                    TFConf.RVPercentage = ldeRVPerc AND
                    TFConf.ValidTo >= ldaOrderDate AND
                    TFConf.ValidFrom <= ldaOrderDate NO-ERROR.
        
                IF NOT AVAILABLE TFConf THEN 
                DO:
                    fErrorLog(Order.OrderID,
                        SUBST("SYSTEM_ERROR:Terminal financing configuration not found")).
                    NEXT ORDER_LOOP.
                END.
            END.
        
            ELSE ASSIGN
                    ldeRVPerc = 0
                    ldeRVAmt  = 0.
        
            ASSIGN
                liCount       = liCount + 1
                ldeTotal      = ldeTotal + ldeTotalAmount + (IF AVAILABLE SingleFee THEN SingleFee.amt ELSE 0)
                ldeQ25        = ldeQ25 + SingleFee.amt         WHEN AVAILABLE SingleFee
                ldePayterms   = ldePayterms + ldeTotalAmount   WHEN fixedfee.billcode BEGINS "payterm" 
                ldeExtensions = ldeExtensions + ldeTotalAmount WHEN fixedfee.billcode BEGINS "rvterm".
        
            IF AVAILABLE SingleFee THEN 
                ASSIGN
                    liCount = liCount + 1
                    liQ25   = liQ25   + 1.
            IF fixedfee.billcode BEGINS "payterm" THEN
                liPayterm = liPayterm + 1.
            ELSE 
                liExtension = liExtension + 1.
        END.
        oiEvents = oiEvents + 1 .
        RUN pWriteDump.
        
        ASSIGN 
            ldeTotal       = 0
            licount        = 0
            ldePayterms    = 0 
            ldeQ25         = 0 
            ldeExtensions  = 0
            liPayterm      = 0
            liQ25          = 0
            liExtension    = 0 .
    END.
END PROCEDURE.

PROCEDURE pWriteDump:
    
    DEFINE VARIABLE liCnt   AS INTEGER NO-UNDO.
    DEFINE VARIABLE lcField AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcValue AS CHARACTER NO-UNDO.
    
    DO liCnt = 1 TO NUM-ENTRIES(lcDumpFields):

        lcField = ENTRY(liCnt,lcDumpFields).
      
        IF lcField BEGINS "#" THEN 
        DO:
            CASE lcField:
                WHEN "#BANKCODE"   THEN lcValue =  lcTFBank.
                WHEN "#BANKNAME"   THEN DO:
                         IF lcTFBank =  "0049" THEN "Uno-E".
                    ELSE IF lcTFBank =  "0225" THEN "Cetelem".
                    ELSE IF lcTFBank =  "0081" THEN "Sabadell".
                END.
                WHEN "#RUNDATE"    THEN lcValue =  STRING(TODAY,"99/99/9999").
                WHEN "#PAYTERMQTY" THEN lcValue =  STRING(liPayterm).
                WHEN "#Q25QTY"     THEN lcValue =  STRING(liQ25).
                WHEN "#TOTALAMT"   THEN lcValue =  STRING(ldeTotal,">>>,>>>,>>>,>>9.99").
                WHEN "#PAYTERMAMT" THEN lcValue =  STRING(ldePayterms,">>>,>>>,>>>,>>9.99").
                WHEN "#Q25AMT"     THEN lcValue =  STRING(ldeQ25,">>>,>>>,>>>,>>9.99").
                WHEN "#Q25EXTNAMT" THEN lcValue =  STRING(ldeExtensions,">>>,>>>,>>>,>>9.99").
                OTHERWISE lcValue = "".
            END CASE.
        END.
        ELSE lcValue = "". 
  
        PUT STREAM sFile UNFORMATTED lcValue.

        IF liCnt < NUM-ENTRIES(lcDumpFields) THEN 
            PUT STREAM sFile UNFORMATTED lcDelimiter.
      
    END.
    
    PUT STREAM sFile UNFORMATTED SKIP.
   
END PROCEDURE.



/* 


PUT UNFORMATTED 
        lcTFBank LABEL "Bank"
        ldeTotal LABEL "TotalAmt" FORMAT ">>>,>>>,>>>,>>9.99" "E"
        licount LABEL "TotalQty"
        SKIP(1)
        ldePayterms LABEL "PaytermAmt" FORMAT ">>>,>>>,>>>,>>9.99"
        ldeQ25 LABEL "Q25Amt"
        ldeExtensions LABEL "Q25ExtensionAmt"
        liPayterm LABEL "PaytermQty"
        liQ25 LABEL "Q25Qty"
        liExtension LABEL "Q25ExtensionQty" WITH 1 COL.
        
         */
