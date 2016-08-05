/*-----------------------------------------------------------------------------
  MODULE .......: nnlsyp.p
  KUTSUVA MODULI: nn.P
  FUNCTION .....: Avointen laskujen yllApito
  APPLICATION ..: nn
  AUTHOR .......: TT
  CREATED ......: 24.02.1997 pt
  changePVM ....: 01.04.1998 pt Invoice.InvAmt : one may NOT UPDATE
                  21.04.1998 kl InvGroup.UpdCustBal ( AccNum receivable )
                  09.10.1998 pt RUN Mc/memo (nninme.p)
                  13.04.1999 pt in English
                  24.08.1999 pt set TimeStamp when changed
                  30.04.2002 aam "kr" references removed 
                  02.05.2002 tk  eventlogging added
                  20.05.2002 tk  RUN Mc/memo
                  10.06.2002 aam use Invoice.OverPaym AND OPAccNum FOR
                                 overpayment,
                                 don't UPDATE InvDate, InvType, PaymState, 
                                 ClaimDate & ClaimQty,
                                 get CommPaid amounts according TO
                                 Payment.AcChargeType
                  24.10.2002 aam fTMSCodeName()
                  12.11.2002 jr "nnlaki" => "invoice" in memo
                  18.11.02 lp - F9 for ClaimCancel and ClaimState
                              - Cash Discount and Demanded at ->UPDATE
                  05.03.03 tk  tokens            
                  27.01.03 aam  ChargeType and DelType added 
                  26.02.03 aam  DDBankAcc
                  14.05.03 aam  update fkonto
                  11.06.03/aam due date cannot be changed if ddstate > 0
                  15.09.03/aam brand
                  02.10.03/aam SpecDel
                  06.02.04 jp  custnum for memo
                  04.05.04/aam account with 6 digits
                  18.05.04/aam max. addition to due date from DueDateTrans
                  11.08.05/aam create ClaimHist for ClaimState 5
                  21.12.05/aam DDFile
                  16.06.06/aam ClaimState instead of ClaimQty
  Version ......: M15
  ---------------------------------------------------------------------------*/

{Syst/commali.i}
{Func/timestamp.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'invoice'}
{Func/fctype.i}
{Func/fduedate.i}
{Func/cparam2.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhInvoice AS HANDLE NO-UNDO.
   lhInvoice = BUFFER Invoice:HANDLE.
   RUN StarEventINITialize(lhInvoice).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhInvoice).
   END.

END.

DEF VAR lasnimi    LIKE Invoice.CustName.
DEF VAR edlask     LIKE Invoice.InvAmt.
DEF VAR suoritettu LIKE Invoice.PaidAmt.
DEF VAR ke         AS LOG FORMAT "Yes/No" INIT TRUE NO-UNDO.
DEF VAR kodnim     AS CHAR FORMAT "x(30)".
DEF VAR laji       AS CHAR FORMAT "x(8)".
DEF VAR tila       AS CHAR FORMAT "x(8)".
DEF VAR edpvm      AS DATE FORMAT "99-99-99" INIT ?.
DEF VAR liVoucher  AS INT NO-UNDO. 
DEF VAR xi         AS INT NO-UNDO. 

DEF VAR lcInvType   AS CHAR NO-UNDO.
DEF VAR lcPaymState AS CHAR NO-UNDO.
DEF VAR lcCancelLst AS CHAR NO-UNDO.
DEF VAR lcStateLst  AS CHAR NO-UNDO.

DEF VAR lcCode        AS CHAR  NO-UNDO. 
DEF VAR lcFrameField  AS CHAR  NO-UNDO. 
DEF VAR lcCancel      AS CHAR  NO-UNDO. 
DEF VAR lcState       AS CHAR  NO-UNDO.
DEF VAR lcSep         AS CHAR  NO-UNDO.

DEF VAR lcDelType       AS CHAR  NO-UNDO.
DEF VAR lcDelTypeLst    AS CHAR  NO-UNDO.
DEF VAR lcSType         AS CHAR  NO-UNDO.
DEF VAR lcSTypeLst      AS CHAR  NO-UNDO.
DEF VAR lcChargeType    AS CHAR  NO-UNDO.
DEF VAR lcChargeTypeLst AS CHAR  NO-UNDO. 
DEF VAR lcDDState       AS CHAR  NO-UNDO. 
DEF VAR liDueDate       AS INT   NO-UNDO. 
DEF VAR ldtChkDate      AS DATE  NO-UNDO. 
DEF VAR ldOldClaim      AS INT   NO-UNDO. 


lcSep = CHR(1).

form
Invoice.InvNum    label "Invoice No. ......" AT 2  skip
Invoice.CustNum   label "Customer No. ....." AT 2
Invoice.CustName  NO-LABEL skip(1)

Invoice.InvDate   label "Invoice Date ....." AT 2
Invoice.DueDate    label "Dueday ............" AT 50
   SKIP

Invoice.InvType  label "Type of Invoice .." AT 2
   laji NO-LABEL FORMAT "x(25)" 
Invoice.ArAccNum   label "AR Account ........" AT 50 
   FORMAT ">>>>>>>9" 
   VALIDATE(CAN-FIND(FIRST Account WHERE 
                           Account.Brand   = Invoice.Brand AND
                           Account.AccNum  = INPUT Invoice.ArAccNum AND
                           Account.AccType = 1),
           "Unknown account or type of account is not AR (1)") 
   SKIP

Invoice.DelType  LABEL "Delivery Method .." AT 2 format ">9"
   lcDelType NO-LABEL FORMAT "X(25)"
Invoice.ClaimPerm label "Claim Permission .." at 50 format "Yes/No" SKIP
   SKIP

Invoice.SpecDel  LABEL "Specification Del." AT 2
   lcSType NO-LABEL FORMAT "X(25)"
Invoice.InterestPerm  label "Interest Permission" at 50 format "Yes/No" SKIP

Invoice.ChargeType    LABEL "Charge Method ...." AT 2
   lcChargeType NO-LABEL FORMAT "X(25)"
Invoice.InvCfg[1] 
                 label "Deny Printing ....." AT 50 
                 format "Yes/No" 
                 help   "Printing of invoice denied"
   SKIP

Invoice.DDBankAcc LABEL "DD Bank Account .." AT 2 
                  FORMAT "X(27)"
Invoice.InvCfg[2]
                 LABEL "Deny Claim Fee ...." AT 50
                 FORMAT "Yes/No"
                 HELP "Deny charging of Reminder Fee"   
   SKIP
lcDDState         LABEL "DD Send Status ..." AT 2
   FORMAT "X(15)"
   SKIP
Invoice.DDFile    LABEL "DD File .........." AT 2
   FORMAT "X(40)"
   SKIP

Invoice.PaymState label "Status of payment " AT 2 
   tila NO-LABEL FORMAT "X(25)" 
Invoice.PaymDate  label "Latest payment Date" AT 50 SKIP

Invoice.ClaimState label "Status of Claim .." at 2 format ">9.9<" 
   lcState NO-LABEL FORMAT "X(22)"
Invoice.ClaimDate label "Claimed at ........" AT 50 SKIP

Invoice.ClaimCancel label "Claiming cancelled" AT 2
                    format ">9" 
   lcCancel NO-LABEL FORMAT "X(25)" SKIP(1)

Invoice.InvAmt label "Invoiced Total ..." AT 2
                 format "zzzzzzzz9.99-"
Invoice.EndInv LABEL "End Invoice ......." AT 50
                 SKIP
suoritettu       label "Total payments ..." AT 2 
                 format "zzzzzzzz9.99-" SKIP
skip

with title color value(ctc) " " + ynimi + " UPDATE AN INVOICE "
+ string(pvm,"99-99-99") + " " COLOR value(cfc) ROW 1 col 1
width 80 side-LABELs
FRAME Invoice.

form
Invoice.CustName LABEL "Customer's Name....."
Invoice.Address  LABEL "Address ............"
Invoice.PostOffice  LABEL "Post office ........"
with title color value(ctc) " 9999: DIVERSE CUSTOMER "
COLOR value(cfc) ROW 5 centered side-LABELs OVERLAY
FRAME oso.

FUNCTION fClaimStateName RETURNS LOGIC
   (idClaimState AS DEC).
   
   lcState = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                              "Invoice",
                              "ClaimState",
                              REPLACE(STRING(idClaimState),",",".")).
   DISPLAY lcState WITH FRAME Invoice.
    
END FUNCTION.


cfc = "yri". RUN Syst/ufcolor.

liDueDate = fCParamI("DueDateTrans").

/* get invoice types and payment states */
DO xi = 0 TO 5:
   lcPaymState = lcPaymState + 
                 DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                  "Invoice","PaymState",STRING(xi)) +
                 lcSep.
   lcSTypeLst  = lcSTypeLst + 
                 DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                  "Invoice","SpecDel",STRING(xi)) +
                 lcSep.
END.

DO xi = 1 TO 9:
   lcInvType   = lcInvType + 
                 DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                  "Invoice","InvType",STRING(xi)) +
                 lcSep.
   lcCancelLst = lcCancelLst + 
                 DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                  "Invoice","ClaimCancel",STRING(xi)) +
                 lcSep.
   IF xi <= 4 THEN DO:
      lcChargeTypeLst = lcChargeTypeLst + 
                   DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                    "Invoice","ChargeType",STRING(xi)) +
                   lcSep.
      lcDelTypeLst = lcDelTypeLst + 
                   DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                    "Invoice","DelType",STRING(xi)) +
                   lcSep.
   END.                  
END.

LOOP:
repeat WITH FRAME Invoice ON ENDKEY UNDO LOOP, NEXT LOOP:
   view FRAME Invoice.
   PAUSE 0 no-message.
   assign lasnimi = ""
          kodnim = "".
   ehto = 9. RUN Syst/ufkey.
   PROMPT-FOR Invoice.InvNum /*WITH FRAME Invoice */
   VALIDATE(input InvNum = "" OR input InvNum ="0" OR
            CAN-FIND (FIRST Invoice WHERE 
                            Invoice.Brand  = gcBrand AND
                            Invoice.Invnum = INPUT InvNum),
            "Invoice not found !") .
   if input Invoice.InvNum = "" or input Invoice.InvNum = "0" THEN LEAVE LOOP.
   FIND Invoice where Invoice.InvNum = INPUT Invoice.InvNum
        exclusive-lock no-error.

   IF AVAILABLE Invoice THEN DO:
      RUN Ar/invbal(Invoice.InvNum,
                 OUTPUT suoritettu).
      suoritettu = Invoice.InvAmt - suoritettu.                 
   END.

   ELSE DO:
      BELL.  ke=true.
      message "Invoice  NOT FOUND !".
      NEXT.
   END.

   ldOldClaim = Invoice.ClaimState.
   
   toimi:
   repeat:

      IF Invoice.InvType > 0  AND 
         Invoice.InvType <= 9 
      THEN laji = ENTRY(Invoice.InvType,lcInvType,lcSep).
      ELSE laji = "Unknown".

      IF Invoice.PaymState >= 0 AND
         Invoice.PaymState <= 5
      THEN tila = ENTRY(Invoice.PaymState + 1,lcPaymState,lcSep).
      ELSE tila = "Unknown".

      IF Invoice.ClaimCancel >= 1 AND
         Invoice.ClaimCancel <= 9
      THEN lcCancel = ENTRY(Invoice.ClaimCancel,lcCancelLst,lcSep).
      ELSE lcCancel = "".

      IF Invoice.ChargeType >= 1 AND
         Invoice.ChargeType <= 4
      THEN lcChargeType = ENTRY(Invoice.ChargeType,lcChargeTypeLst,lcSep).
      ELSE lcChargeType = "". 

      IF Invoice.DelType >= 1 AND
         Invoice.DelType <= 4
      THEN lcDelType = ENTRY(Invoice.DelType,lcDelTypeLst,lcSep).
      ELSE lcDelType = "". 

      IF Invoice.SpecDel >= 0 AND
         Invoice.SpecDel <= 2
      THEN lcSType = ENTRY(Invoice.SpecDel + 1,lcSTypeLst,lcSep).
      ELSE lcSType = "". 

      lcDDState  = IF Invoice.DDBankAcc = ""
                   THEN ""
                   ELSE IF Invoice.ddState > 0
                        THEN "Sent to bank"
                        ELSE "Not sent".

      fClaimStateName(Invoice.ClaimState).
      
      DISPLAY Invoice.InvNum Invoice.CustNum Invoice.CustName
              Invoice.InvDate 
              Invoice.InvType laji 
              Invoice.PaymState tila
              Invoice.DueDate Invoice.ClaimPerm
              Invoice.PaymDate Invoice.InterestPerm Invoice.ClaimDate
              Invoice.ClaimState lcState
              Invoice.ClaimCancel lcCancel
              Invoice.InvCfg[1 FOR 2]
              Invoice.InvAmt suoritettu  
              Invoice.ChargeType lcChargeType
              Invoice.DelType lcDelType
              Invoice.SpecDel lcSType
              Invoice.DDBankAcc Invoice.DDFile
              lcDDState
              Invoice.EndInvoice
              WITH FRAME Invoice.

      ASSIGN
      ufk[1] = (IF lcRight = "RW" THEN 91 ELSE 0) 
      ufk[2] = 0 ufk[3] = 927 ufk[4] = 0 
      ufk[5] = (IF lcRight = "RW" THEN 15 ELSE 0)
      ufk[6] = (IF lcRight = "RW" THEN 12 ELSE 0)
      ufk[7] = 0  ufk[7] = 0   ufk[8] = 8   ufk[9] = 0 ehto = 0.

      ehto = 0. RUN Syst/ufkey.p.
      IF toimi = 1 AND lcRight = "RW" THEN DO:
         ehto = 9. RUN Syst/ufkey.

         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhInvoice).

         REPEAT WITH FRAME Invoice ON ENDKEY UNDO, LEAVE:

         UPDATE 
                Invoice.DueDate WHEN Invoice.DDState = 0
                  validate(INPUT Invoice.DueDate >= INPUT Invoice.InvDate AND
                          input Invoice.DueDate <> ?, "Too early a dueday !")
                Invoice.DelType
                  validate(INPUT Invoice.DelType >= 1 AND 
                           INPUT Invoice.DelType <= 4,
                           "Valid values are 1 - 4")
                Invoice.SpecDel
                  validate(INPUT Invoice.SpecDel >= 0 AND 
                           INPUT Invoice.SpecDel <= 2,
                           "Valid values are 0 - 2")
                Invoice.ChargeType     WHEN NOT (Invoice.ChargeType = 2 AND 
                                            Invoice.DDState > 0)
                  validate(INPUT Invoice.ChargeType >= 1 AND 
                           INPUT Invoice.ChargeType <= 4,
                           "Valid values are 1 - 4")
                Invoice.DDBankAcc WHEN Invoice.DDState = 0
                Invoice.ArAccNum
                Invoice.ClaimPerm
                Invoice.InterestPerm 
                Invoice.InvCfg[1 FOR 2]
                Invoice.ClaimState
                Invoice.ClaimCancel
                Invoice.ClaimDate

         WITH FRAME Invoice EDITING:
            READKEY.
            nap = KEYLABEL(LASTKEY).

            IF nap = "F9" AND 
               LOOKUP(FRAME-FIELD,
                      "ClaimState,ClaimCancel,DelType,ChargeType,SpecDel")
                      > 0
            THEN DO:

               IF FRAME-FIELD = "ClaimState"
               THEN DO:

                  RUN Help/h-tmscodes.p(INPUT "Invoice",    /* TableName */
                                       "ClaimState", /* FieldName */
                                       "AccRec",     /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ?
                     THEN DO:
                        DISPLAY DECIMAL(lcCode) ;& Invoice.ClaimState
                        WITH FRAME Invoice.   

                        fClaimStateName(DECIMAL(lcCode)).
                     END.   
               END.

               ELSE IF FRAME-FIELD = "ClaimCancel"
               THEN DO:

                  RUN Help/h-tmscodes.p(INPUT "Invoice",     /* TableName */
                                       "ClaimCancel", /* FieldName */
                                       "AccRec",      /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ?
                     THEN DO:
                        lcCancel = ENTRY(INTEGER(lcCode),lcCancelLst,lcSep).
                        DISPLAY INTEGER(lcCode) ;& Invoice.ClaimCancel
                        lcCancel
                        WITH FRAME Invoice.
                     END.
               END.

               ELSE IF FRAME-FIELD = "ChargeType" THEN DO:

                  RUN Help/h-tmscodes.p(INPUT "Invoice",     /* TableName*/
                                       "ChargeType",       /* FieldName */
                                       "AccRec",      /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ?
                  THEN DO:
                     lcChargeType = ENTRY(INTEGER(lcCode),
                                          lcChargeTypeLst,lcSep).
                     DISPLAY INTEGER(lcCode) ;& Invoice.ChargeType
                             lcChargeType WITH FRAME Invoice.
                  END.

               END.

               ELSE IF FRAME-FIELD = "DelType" THEN DO:

                  RUN Help/h-tmscodes.p(INPUT "Invoice",     /* TableName*/
                                       "DelType",       /* FieldName */
                                       "Billing",     /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ?
                  THEN DO:
                     lcDelType = ENTRY(INTEGER(lcCode),
                                       lcDelTypeLst,lcSep).
                     DISPLAY INTEGER(lcCode) ;& Invoice.DelType
                             lcDelType WITH FRAME Invoice.
                  END.

               END.

               ELSE IF FRAME-FIELD = "SpecDel" THEN DO:

                  RUN Help/h-tmscodes.p(INPUT "Invoice",     /* TableName*/
                                       "SpecDel",       /* FieldName */
                                       "Billing",     /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ?
                  THEN DO:
                     lcSType = ENTRY(INTEGER(lcCode) + 1,lcSTypeLst,lcSep).
                     DISPLAY INTEGER(lcCode) ;& Invoice.SpecDel
                             lcSType WITH FRAME invoice.
                  END.

               END.

               ehto = 9.
               RUN Syst/ufkey.
               NEXT. 
            END.

            ELSE IF LOOKUP(nap,poisnap) > 0 THEN DO WITH FRAME Invoice:

               IF FRAME-FIELD = "ClaimState" THEN DO:
                  fClaimStateName(INPUT INPUT FRAME Invoice
                                     Invoice.ClaimState).  
               END.

               ELSE IF FRAME-FIELD = "ClaimCancel" THEN DO:      
                  IF INTEGER(INPUT Invoice.ClaimCancel) >= 1 AND
                     INTEGER(INPUT Invoice.ClaimCancel) <= 9
                  THEN lcCancel = ENTRY(INTEGER(INPUT Invoice.ClaimCancel),
                                        lcCancelLst,lcSep).
                  ELSE lcCancel = "".

                  DISPLAY lcCancel WITH FRAME Invoice.
               END.

               ELSE IF FRAME-FIELD = "ChargeType" THEN DO:

                  /* check that cType and dType -combination is valid */
                  IF NOT fChkCType(INPUT INPUT Invoice.ChargeType,
                                   INPUT INPUT Invoice.delType,
                                   TRUE)
                  THEN NEXT. 

                  IF INPUT Invoice.ChargeType >= 1 AND
                     INPUT Invoice.ChargeType <= 4 
                  THEN DO:
                     lcChargeType = ENTRY(INPUT Invoice.ChargeType,
                                          lcChargeTypeLst,lcSep).
                     DISPLAY lcChargeType.
                  END.
               END.

               ELSE IF FRAME-FIELD = "DelType" THEN DO:

                  IF INPUT Invoice.DelType >= 1 AND
                     INPUT Invoice.DelType <= 4 
                  THEN DO:
                     lcDelType = ENTRY(INPUT Invoice.DelType,
                                       lcDelTypeLst,lcSep).
                     DISPLAY lcDelType.
                  END.
               END.

               ELSE IF FRAME-FIELD = "SpecDel" THEN DO:

                  IF INPUT Invoice.SpecDel >= 0 AND
                     INPUT Invoice.SpecDel <= 2 
                  THEN DO:
                     lcSType = ENTRY(INPUT Invoice.SpecDel + 1,
                                     lcSTypeLst,lcSep).
                     DISPLAY lcSType.
                  END.
               END.

               ELSE IF FRAME-FIELD = "DueDate" AND liDueDate > 0 THEN DO:
                  /* max transfer limit exceeded */ 
                  ldtChkDate =  fChkDueDate(Invoice.DueDate + liDueDate).
                  IF INPUT Invoice.DueDate > ldtChkDate THEN DO:
                      MESSAGE "Latest due date for this invoice is"
                              STRING(ldtChkDate,"99.99.9999")
                      VIEW-AS ALERT-BOX
                      ERROR.
                      DISPLAY ldtChkDate @ Invoice.DueDate WITH FRAME Invoice.
                      NEXT.
                  END.
               END.
               
            END.

            APPLY LASTKEY.
         END.

         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhInvoice).

         LEAVE.

         END.

         NEXT toimi.
      END.

      ELSE IF toimi = 3 THEN DO : /* memo */
         RUN Mc/memo(INPUT Invoice.Custnum,
                  INPUT "invoice",
                  INPUT STRING(Invoice.InvNum),
                  INPUT "Invoice Number").
         NEXT toimi.  
      END.

      ELSE IF toimi = 5 AND lcRight = "RW" THEN DO:

         /* create claiming history for claim cancellation */
         IF Invoice.ClaimState = 15 AND ldOldClaim < 15 THEN DO:
            IF NOT CAN-FIND(ClaimHist WHERE
                            ClaimHist.InvNum = Invoice.InvNum AND
                            ClaimHist.Claim  = INTEGER(10 * Invoice.ClaimState)
                           ) 
            THEN DO:
               CREATE ClaimHist.
               ASSIGN ClaimHist.Brand      = Invoice.Brand
                      ClaimHist.InvNum     = Invoice.InvNum
                      ClaimHist.CustNum    = Invoice.CustNum 
                      ClaimHist.ClaimState = Invoice.ClaimState
                      ClaimHist.Claim      = 10 * Invoice.ClaimState
                      ClaimHist.Memo       = "Claiming cancelled"
                      ClaimHist.ClaimDate  = TODAY
                      ClaimHist.ClaimAmt   = Invoice.InvAmt - Invoice.PaidAmt
                      ClaimHist.Handler    = katun.
            END.
         END. 

         CLEAR FRAME Invoice no-pause.

         NEXT LOOP.
      END.
      ELSE IF toimi = 6 AND lcRight = "RW" THEN DO:
         CLEAR FRAME Invoice no-pause.
         UNDO LOOP, NEXT LOOP.
      END.
      ELSE IF toimi = 8 THEN UNDO LOOP, LEAVE LOOP.
   END. /* toimi */
END. /* LOOP */
HIDE FRAME Invoice no-pause.

