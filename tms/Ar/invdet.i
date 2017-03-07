/* invdet.i     30.10.02/aam show invoice details 

                26.02.03/aam cType, dType, ddBankAcc 
                16.05.03/aam ddState 
                02.10.03/aam SpecDel
                17.12.03/aam VatUsage, VatIncl
                04.11.04/aam use fInvBal
                11.08.05/aam ClaimState 5 
                29.03.06/aam PaymState 5
                16.06.06/aam Invoice.ClaimState replaces ClaimQty,
                             EndInvoice
                29.11.06/aam ExtInvID             
                24.01.07/aam second column moved to 42
                25.04.07/aam pInvoiceUpdate

   callers: payments.p
            nnasla.p
            claimhist.p
            nnlayp.p
*/


{Syst/commali.i}
{Func/timestamp.i}
{Syst/tmsconst.i}
{Func/fuserright.i}            
{Func/finvbal.i}
{Func/fduedate.i}

DEF VAR lcInvType    AS CHAR NO-UNDO.
DEF VAR lcClaimDesc AS CHAR NO-UNDO. 
DEF VAR lcPaymState  AS CHAR NO-UNDO.
DEF VAR lcPrintState AS CHAR NO-UNDO. 
DEF VAR lcIFSState   AS CHAR NO-UNDO. 
DEF VAR lcDType      AS CHAR NO-UNDO.
DEF VAR lcCType      AS CHAR NO-UNDO.
DEF VAR lcDDState    AS CHAR NO-UNDO. 
DEF VAR lcCurrency   AS CHAR NO-UNDO.
DEF VAR liDetCount   AS INT  NO-UNDO. 
DEF VAR ldtStampDate AS DATE NO-UNDO.
DEF VAR lcDispStamp  AS CHAR NO-UNDO. 
DEF VAR ldUnPaid     AS DEC  NO-UNDO.
DEF VAR lcSep        AS CHAR NO-UNDO.
DEF VAR lcCrExtID    AS CHAR NO-UNDO.
DEF VAR ldDetCredit  AS DEC  NO-UNDO.
DEF VAR ldDetCLoss   AS DEC  NO-UNDO.
DEF VAR liODIRequest AS INT  NO-UNDO.
DEF VAR lcFullLine   AS CHAR NO-UNDO.
DEF VAR lcSubLine1   AS CHAR NO-UNDO.
DEF VAR lcSubLine2   AS CHAR NO-UNDO.
DEF VAR lcVatUsage   AS CHAR NO-UNDO.

DEF BUFFER bViewInv FOR Invoice.

ASSIGN
   lcSep = CHR(1)
   lcFullLine = FILL("-",78)
   lcSubLine1 = FILL("-",35)
   lcSubLine2 = FILL("=",35).

FORM
    Invoice.ExtInvID   
       LABEL "Invoice Number"             
    Invoice.InvType    
       LABEL "Invoice Type " AT 42
       FORMAT ">9"
    lcInvType 
       NO-LABEL FORMAT "X(19)" 
       SKIP

    Invoice.InvDate      
       LABEL "Invoice Date ." FORMAT "99-99-99"    
    lcDispStamp    
       LABEL "Created ....." FORMAT "X(18)" AT 42 
       SKIP

    Invoice.DueDate 
       LABEL "Due Date ....." FORMAT "99-99-99"   
    liODIRequest 
       LABEL "Request ID .." AT 42
       FORMAT ">>>>>>>>"
       SKIP

    Invoice.FromDate     
       LABEL "Billing Period" FORMAT "99-99-99" "-"   
    Invoice.ToDate    
       NO-LABEL FORMAT "99-99-99"   
    Invoice.FirstCall 
       LABEL "1st Call ...." 
       FORMAT "99-99-99"  AT 42  
       SKIP
       
    lcFullLine 
       NO-LABEL
       FORMAT "X(78)"
       SKIP

    Invoice.AmtExclVAT  
       LABEL "Total Excl.TAX" FORMAT "z,zzz,zz9.99+" 
    Invoice.VatUsage 
       LABEL "TAX Usage ..." AT 42
    lcVatUsage
       NO-LABEL
       FORMAT "X(20)"
       SKIP

    Invoice.VATAmt      
       LABEL "TAX Amount ..." FORMAT "z,zzz,zz9.99+" 
    Invoice.VatIncl AT 42
       LABEL "TAX Excl/Incl"
       FORMAT "Included/Excluded"
       SKIP
       
    lcSubLine1 
       NO-LABEL
       FORMAT "X(35)"
    Invoice.DeliveryState AT 42 
       LABEL "IFS status .."
       VALIDATE(Invoice.DeliveryState >= 0 AND Invoice.DeliveryState < 2,
                "Value not allowed")
       lcIFSState FORMAT "x(18)" NO-LABEL
       SKIP

    Invoice.InterestAmt 
       LABEL "Interest ....." FORMAT "z,zzz,zz9.99+" 
    Invoice.DelType  
       LABEL "Delivery Type" AT 42 FORMAT ">9" 
    lcDType 
       NO-LABEL FORMAT "X(19)"
       SKIP

    Invoice.AdvPaym     
       LABEL "Adv. Payment ." FORMAT "z,zzz,zz9.99+" 
    Invoice.PrintState     
       LABEL "Print. Status" AT 42
       lcPrintState NO-LABEL FORMAT "X(20)" 
       SKIP

    Invoice.OverPaym     
       LABEL "Overpayment .." FORMAT "z,zzz,zz9.99+" 
    Invoice.InvCfg[1] 
       LABEL "Print. Denied" AT 42
       HELP  "Deny printing of this invoice (Yes/No)" 
       SKIP

    Invoice.InvAmt   
       LABEL "TOTAL ........" FORMAT "z,zzz,zz9.99+" 
    lcCurrency   
       NO-LABEL  FORMAT "x(8)"      
    Invoice.ChargeType      
       LABEL "Charge Type ." AT 42
    lcCType 
       NO-LABEL FORMAT "X(16)"
       SKIP
       
    lcSubLine2 
       NO-LABEL
       FORMAT "X(35)"
    lcddState          
       LABEL "DD Status ..." AT 42
       FORMAT "x(18)"
       SKIP

    Invoice.PaidAmt  
       LABEL "Paid ........."  FORMAT "z,zzz,zz9.99+" 
    Invoice.PaymState 
       LABEL "Paym. Status " AT 42
    lcPaymState       
       NO-LABEL FORMAT "X(15)" 
       SKIP

    ldDetCredit 
       LABEL "Credited ....."  FORMAT "z,zzz,zz9.99+"
    lcCrExtID 
       LABEL "Credit Note ." AT 42 
       FORMAT "X(12)" 
       SKIP

    ldDetCLoss         
       LABEL "Doubtful A/R ." FORMAT "z,zzz,zz9.99+" 
    Invoice.ClaimStatus     
       LABEL "Claim Status " AT 42
       FORMAT "x(6)"
       HELP "Claiming status"
    lcClaimDesc
       NO-LABEL FORMAT "X(15)"
       SKIP

    ldUnPaid         
       LABEL "Unpaid Balance" FORMAT "z,zzz,zz9.99+" 
    Invoice.ClaimPerm      
       LABEL "Claim Allowed" AT 42
       FORMAT "Yes/No"
       HELP "Allow claiming (Yes/No)" 
       SKIP

    WITH  OVERLAY CENTERED ROW 1 SIDE-LABELS
    TITLE " VIEW INVOICE DATA (" + STRING(Invoice.InvNum) + ") "
    FRAME fInvDet.

FORM
   SKIP(1)
   Invoice.BillRun AT 2 FORMAT "X(30)" NO-LABEL
   SKIP(1)
   WITH OVERLAY CENTERED ROW 10 TITLE " BILLING RUN ID " FRAME fBillRunID.


PROCEDURE pInvoiceDetails:

   DEF INPUT PARAMETER iiInvno AS INT NO-UNDO.
   DEF INPUT PARAMETER ilHide  AS LOG NO-UNDO.

   FIND Invoice WHERE 
        Invoice.InvNum = iiInvno NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Invoice THEN DO:
      MESSAGE "Invoice information cannot be retrieved."
      VIEW-AS ALERT-BOX.
      RETURN.
   END. 

   FIND Customer WHERE 
        Customer.CustNum = Invoice.CustNum NO-LOCK NO-ERROR.

   ASSIGN lcInvType    = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                          "Invoice",
                                          "InvType",
                                          STRING(Invoice.InvType))
                                       
          lcPrintState = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                          "Invoice",
                                          "PrintState",
                                          STRING(Invoice.PrintState))
          
          lcIFSState = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                        "Invoice",
                                        "DeliveryState",
                                        STRING(Invoice.DeliveryState))
                 
          lcClaimDesc = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                          "Invoice",
                                          "ClaimStatus",
                                          Invoice.ClaimStatus)
                                          
          lcPaymState  = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                          "Invoice",
                                          "PaymState",
                                          STRING(Invoice.PaymState))
          
          lcCType      = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                          "Invoice",
                                          "ChargeType",
                                          STRING(Invoice.ChargeType))

          lcDType      = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                          "Invoice",
                                          "DelType",
                                          STRING(Invoice.DelType))

          lcVatUsage   = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                          "Invoice",
                                          "VatUsage",
                                          STRING(Invoice.VatUsage))

          lcCurrency   = Invoice.Currency
          ldUnPaid     = fInvBal(BUFFER Invoice,TODAY).

   fSplitTS(Invoice.ChgStamp,
            OUTPUT ldtStampDate,
            OUTPUT liDetCount).
            
   ASSIGN lcDispStamp = STRING(ldtStampDate,"99-99-99") + " " +
                        STRING(liDetCount,"hh:mm:ss")
          lcDDState   = IF Invoice.DDBankAcc = ""
                        THEN ""
                        ELSE (string(invoice.ddstate) + " " +
                             DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                              "Invoice",
                                              "DDState",
                                              STRING(Invoice.DDState)))
          lcCrExtID   = "".
                             
   IF Invoice.CrInvNum > 0 THEN
   FOR FIRST bViewInv NO-LOCK WHERE
             bViewInv.InvNum = Invoice.CrInvNum:
      lcCrExtID = bViewInv.ExtInvID.
   END.

   liODIRequest = 0.
   /* odi request */
   FOR FIRST MsRequest NO-LOCK WHERE
             MsRequest.Brand      = gcBrand AND
             MsRequest.ReqType    = 20      AND
             MsRequest.CustNum    = Invoice.CustNum AND
             MsRequest.ReqCParam1 = Invoice.ExtInvID:
      liODIRequest = MsRequest.MsRequest.       
   END.

   ldDetCredit = fCreditedAmt(BUFFER Invoice,TODAY).  
   ldDetCLoss  = fCredLossPaid(BUFFER Invoice,TODAY,OUTPUT liDetCount).
    
   IF LOOKUP(STRING(Invoice.InvType),"5,8,9,11,13") > 0  
   THEN lcCrExtID:LABEL IN FRAME fInvDet = "Credited Inv.".
   ELSE lcCrExtID:LABEL IN FRAME fInvDet = "Credit Note .".

   PAUSE 0.
   DISPLAY Invoice.ExtInvID Invoice.InvType lcInvType
           Invoice.InvDate lcDispStamp
           Invoice.DueDate liODIRequest
           Invoice.FromDate Invoice.ToDate Invoice.FirstCall 
           lcFullLine
           Invoice.AmtExclVAT Invoice.VatUsage lcVatUsage Invoice.VatIncl
           Invoice.VATAmt Invoice.DelType lcDType
           lcSubLine1 Invoice.PrintState lcPrintState 
           Invoice.DeliveryState lcIFSState
           Invoice.InterestAmt Invoice.InvCfg[1]  
           Invoice.AdvPaym 
           Invoice.OverPaym Invoice.ChargeType lcCType
           Invoice.InvAmt lcCurrency lcDDState
           lcSubLine2
           (Invoice.InvAmt - ldUnPaid - ldDetCredit - ldDetCLoss) 
              @ Invoice.PaidAmt  
           Invoice.PaymState lcPaymState
           ldDetCredit lcCrExtID 
           ldDetCLoss Invoice.ClaimStatus lcClaimDesc 
           ldUnpaid Invoice.ClaimPerm 
   WITH FRAME fInvDet.

   IF ilHide THEN DO:
      MESSAGE "Press ENTER to continue".
      PAUSE NO-MESSAGE.

      HIDE FRAME fInvDet NO-PAUSE.
   END. 

END PROCEDURE. 

/* update and run related procedures
   note: caller takes care of invoice eventlog 
*/
PROCEDURE pInvoiceUpdate:

   DEF INPUT PARAMETER iiInvNum AS INT  NO-UNDO.
 
   DEF VAR lcOldBank  AS CHAR NO-UNDO.
   DEF VAR ldtChkDate AS DATE NO-UNDO.
   DEF VAR ldtOldDue  AS DATE NO-UNDO.
   DEF VAR lcCode     AS CHAR NO-UNDO.
   DEF VAR llOk       AS LOG  NO-UNDO.
   DEF VAR liDueDate  AS INT  NO-UNDO. 
   DEF VAR lcUpdate   AS CHAR NO-UNDO.

   lcUpdate = fTokenRights(katun,"SYST,BILL").
 
   FIND Invoice WHERE Invoice.InvNum = iiInvNum NO-LOCK.

   REPEAT ON ENDKEY UNDO, LEAVE:
   
      ASSIGN
         ufk    = 0
         ufk[1] = IF lcUpdate = "RW" AND Invoice.PaymState NE 4 THEN 7 ELSE 0
         ufk[2] = 927
         ufk[3] = 829
         ufk[4] = 1170
         ufk[5] = 790
         ufk[6] = 1152
         ufk[7] = 1752
         ufk[8] = 8
         ehto   = 0.
      RUN Syst/ufkey.p.

      IF toimi = 1 THEN DO TRANS:

         FIND CURRENT Invoice EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      
         IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "Invoice record is in use !"
            VIEW-AS ALERT-BOX INFORMATION.
            HIDE FRAME fInvDet NO-PAUSE.
            RETURN "LOCKED".
         END.

         ehto = 9.
         RUN Syst/ufkey.p.

         ASSIGN 
            lcOldBank  = Invoice.DDBankAcc
            ldtOldDue  = Invoice.DueDate.

         UPDATE
            Invoice.DeliveryState WHEN Invoice.DeliveryState NE 2
            Invoice.DelType
            Invoice.PrintState
            Invoice.InvCfg[1]
            Invoice.ClaimPerm WHEN Invoice.PaymState NE 4
         WITH FRAME fInvDet EDITING:

            READKEY.
            nap = KEYLABEL(LASTKEY).

            IF nap = "F9" AND 
               LOOKUP(FRAME-FIELD,"DelType,PrintState,DeliveryState") > 0 
            THEN DO:

               IF FRAME-FIELD = "DelType" THEN DO:

                  RUN Help/h-tmscodes.p(INPUT "Invoice",     /* TableName*/
                                       "DelType",       /* FieldName */
                                       "Billing",     /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN DO:
                     IF LOOKUP(lcCode,SUBST("&1,&2,&3",
                        {&INV_DEL_TYPE_EMAIL_PENDING},
                        {&INV_DEL_TYPE_FUSION_EMAIL_PENDING},
                        {&INV_DEL_TYPE_FUSION_EMAIL})) > 0 THEN DO:
                        MESSAGE "Incorrect delivery type"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END. /* IF LOOKUP(lcCode,SUBST("&1,&2,&3", */
                     lcDType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                                "Invoice",
                                                "DelType",
                                                lcCode).

                     DISPLAY INTEGER(lcCode) ;& Invoice.DelType
                             lcDType WITH FRAME fInvDet.
                  END.
               END.

               ELSE IF FRAME-FIELD = "PrintState" THEN DO:

                  RUN Help/h-tmscodes.p(INPUT "Invoice",   
                                       "PrintState", 
                                       "Report",    
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN DO:
                     lcPrintState = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                                     "Invoice",
                                                     "PrintState",
                                                     lcCode).

                     DISPLAY INTEGER(lcCode) ;& Invoice.PrintState
                             lcPrintState WITH FRAME fInvDet.
                  END.
               END.
               ELSE IF FRAME-FIELD = "DeliveryState" THEN DO:

                  RUN Help/h-tmscodes.p(INPUT "Invoice",   
                                       "DeliveryState", 
                                       "Billing",    
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN DO:
                     lcIFSState = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                                     "Invoice",
                                                     "DeliveryState",
                                                     lcCode).

                     DISPLAY INTEGER(lcCode) ;& Invoice.DeliveryState
                             lcIFSState WITH FRAME fInvDet.
                  END.
               END.

               ehto = 9.
               RUN Syst/ufkey.p.
               NEXT. 
            END.

            ELSE IF LOOKUP(nap,poisnap) > 0 THEN DO WITH FRAME fInvDet:

               IF FRAME-FIELD = "DelType" THEN DO:
                
                  lcDType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                             "Invoice",
                                             "DelType",
                                             STRING(INPUT Invoice.DelType)).
                                          
                  IF lcDType = "" THEN DO:
                     MESSAGE "Unknown delivery type"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               
                  DISPLAY lcDType.
               END.
                                   
               ELSE IF FRAME-FIELD = "PrintState" THEN DO:

                  lcPrintState = 
                     DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                      "Invoice",
                                      "PrintState",
                                      STRING(INPUT Invoice.PrintState)).
                                          
                  IF lcPrintState = "" THEN DO:
                     MESSAGE "Unknown printing status"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               
                  DISPLAY lcPrintState.
               END.
               
               ELSE IF FRAME-FIELD = "DeliveryState" THEN DO:

                  lcIFSState = 
                     DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                      "Invoice",
                                      "DeliveryState",
                                      STRING(INPUT Invoice.DeliveryState)).
                                          
                  IF lcIFSState = "" THEN DO:
                     MESSAGE "Unknown IFS status"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               
                  DISPLAY lcIFSState.
               END.
            
            END.
          
            APPLY LASTKEY.
         END. 

         IF Invoice.DDBankAcc NE lcOldBank AND Invoice.DDState > 0 THEN DO:
            ASSIGN 
               ldtChkDate = fChkDueDate(TODAY + 1)
               llOk       = TRUE.
                 
            MESSAGE "Bank data has been changed." SKIP
                    "Invoice will be resent to bank with due date"
                   STRING(ldtChkDate,"99.99.9999")
            VIEW-AS ALERT-BOX
            BUTTONS OK-CANCEL
            TITLE " Direct Debit "
            SET llOk.

            IF NOT llOk THEN Invoice.DDBankAcc = lcOldBank.
            ELSE ASSIGN 
               Invoice.DDState = 0
               Invoice.DueDate = ldtChkDate.
         END.
           
         IF Invoice.DueDate = ? THEN DO:
            MESSAGE "Due date can not be empty"
            VIEW-AS ALERT-BOX ERROR.
            Invoice.DueDate = ldtOldDue.
         END.
           
         /* max transfer limit exceeded */ 
         IF liDueDate > 0 THEN DO:
            ldtChkDate =  fChkDueDate(ldtOldDue + liDueDate).

            IF Invoice.DueDate > ldtChkDate THEN DO:
               MESSAGE "Latest due date for this invoice is"
                       STRING(ldtChkDate,"99.99.9999")
               VIEW-AS ALERT-BOX ERROR.

               Invoice.DueDate = ldtChkDate.
            END. 
         END.

      END.

      /* memo */
      ELSE IF toimi = 2 THEN DO:
         RUN Mc/memo.p(INPUT Invoice.CustNum,
                  INPUT "Invoice",
                  INPUT STRING(Invoice.InvNum),
                  INPUT "Invoice number").
      END.

      /* payments */
      ELSE IF toimi = 3 THEN DO:
        RUN Ar/payments.p(0,
                     Invoice.InvNum,
                     "").
      END.
      
      /* subinvoices */
      ELSE IF toimi = 4 THEN DO:
         RUN Ar/subinvoice.p (Invoice.InvNum).
      END.
      
      /* invoice rows */
      ELSE IF toimi = 5 THEN DO:
         RUN Ar/nnlryp.p(Invoice.InvNum,0).
      END.
      
      /* other actions */
      ELSE IF toimi = 6 THEN DO:
      
         otheractions:
         REPEAT ON ENDKEY UNDO, NEXT:

            ASSIGN ufk    = 0
                   ufk[1] = 0 
                   ufk[2] = 1650 
                   ufk[3] = 908
                   ufk[4] = 1492
                   ufk[5] = 1561
                   ufk[6] = 862
                   ufk[7] = 1796
                   ufk[8] = 8
                   ehto   = 0.
            RUN Syst/ufkey.p.
               
            /* reference nbr  */
            IF toimi = 1 THEN DO: 
               RUN Mc/showpr.p(Invoice.CustNum,
                          Invoice.InvNum).
            END.
  
            ELSE IF toimi = 2 THEN DO:
               PAUSE 0.
               DISP Invoice.BillRun WITH FRAME fBillRunID.

               ASSIGN
                  ufk    = 0
                  ufk[8] = 8
                  ehto   = 0.
               RUN Syst/ufkey.p.
            
               HIDE FRAME fBillRunID NO-PAUSE. 
            END.

            /* credit invoice */
            ELSE IF toimi = 3 THEN DO:
               si-recid2 = RECID(Invoice).
              
               RUN Ar/nncimu.p.

               si-recid2  = ?.
               
               IF Invoice.CrInvNum > 0 THEN RETURN "".
            END. 

            /* claiming history */
            ELSE IF toimi = 4 THEN DO:
               RUN Ar/claimhis.p(0,Invoice.InvNum).
            END.
 
            /* invoice row counters */
            ELSE IF toimi = 5 THEN DO:
               RUN Inv/invrowcounter.p(0,0,Invoice.InvNum,""). 
            END. 
           
            /* interest events */ 
            ELSE IF toimi = 6 THEN DO:
               RUN Ar/nnkoyp.p (Invoice.CustNum).
            END.
             
            /* view send log */
            ELSE IF toimi = 7 THEN DO:
               RUN Mc/itsendlo.p(0,
                            Invoice.InvNum,
                            0,
                            0).
            END.
            
            ELSE IF toimi = 8 THEN LEAVE otheractions.
        
         END.  /* otheractions */
 
      END.
      
      /* show eventlog */
      ELSE IF toimi = 7 THEN DO:
         RUN Mc/eventsel.p ("Invoice",
                       STRING(iiInvNum)).
      END.
        
      ELSE IF LOOKUP(KEYLABEL(LASTKEY),"enter,return,8,f8") > 0 THEN DO:
         RETURN "QUIT".
      END.
   
   END.

   RETURN "".
   
END PROCEDURE.
 
 
