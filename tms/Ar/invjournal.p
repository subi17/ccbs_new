/* ------------------------------------------------------
  MODULE .......: invjournal.p
  FUNCTION .....: Print invoice journal
  APPLICATION ..: TMS
  AUTHOR .......: 
  CREATED ......:  
  MODIFIED .....: 21.01.2002/aam format for dates dd.mm.yy,   
                                 VAT calculation corrected,
                                 "kr" -references removed
                  07.06.2002/aam use Invoice.OverPaym for overpayment,
                                 VAT handling for advance payment 
                  17.06.2002/aam skip invoices defined in parameter 
                                 InvTypeDenied
                  01.08.2002/aam invoice can contain lines both with or
                                 without VAT 
                  01.10.2002/aam use InvRow.VatPerc
                  14.10.2002/aam Currency handling, 
                                 fChgPage(), uprfeed,
                                 InvType and PaymState as selection criteria
                  05.03.2003/tk  tokens               
                  20.03.2003/jp  External customer group
                  11.09.2003/aam brand
                  28.11.2003/aam customer number selection
                  04.05.2004/aam account with 6 digits
                  09.06.2004/aam vat for invrows (negative amt) corrected
                  30.03.06/aam post difference within one invoice caused by 
                               the use of 3 decimals in invrows into roundacc
                  28.12.06 jp  latili.latno new format             
                  04.01.07/aam longer format for accounts in summary
                  31.01.07/aam Invoice.ExtInvID
                  01.02.07 kl  lcCustName (credit invoices)
                  26.06.07/aam separated from nnlalu.p,
                               invoice qty
                  
  VERSION ......: Yoigo
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/utumaa.i}
{Ar/invjournal.i}
{Func/cparam2.i}
{Func/fcurrency.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'invoice'}


DEF INPUT  PARAMETER TABLE FOR TCustGroup.
DEF INPUT  PARAMETER TABLE FOR ttCriter.
DEF OUTPUT PARAMETER oiInvQty AS INT NO-UNDO.

DEF VAR viiva1     AS CHAR FORMAT "x(114)" NO-UNDO.
DEF VAR viiva2     like viiva1.
DEF VAR viiva3     like viiva1.
DEF VAR jar        AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR order      AS INT  NO-UNDO.
DEF VAR sl         AS INT  NO-UNDO.
DEF VAR rl         AS INT  NO-UNDO.
DEF VAR rlx        AS INT  NO-UNDO.
DEF VAR lev        AS INT  init 114 NO-UNDO.
DEF VAR ke         AS LOG  FORMAT "Yes/No" init false NO-UNDO.
DEF VAR AccNum     AS INT  extent 15 NO-UNDO.                        
DEF VAR Posting    AS DEC  extent 15 NO-UNDO.
DEF VAR ysaldo     AS DEC  FORMAT "->>>>>>>9.99" init 0 NO-UNDO.
DEF VAR pvm1       AS DATE FORMAT "99-99-99" NO-UNDO.
DEF VAR pvm2       AS DATE FORMAT "99-99-99" NO-UNDO.
DEF VAR i          AS INT  NO-UNDO.
DEF VAR laskyht    AS DEC  FORMAT "->>>>>>>9.99" NO-UNDO.
DEF VAR debyht     AS DEC  FORMAT "->>>>>>>9.99" NO-UNDO.
DEF VAR kreyht     AS DEC  FORMAT "->>>>>>>9.99" NO-UNDO.

DEF VAR edInvNum     AS CHAR  NO-UNDO.
DEF VAR clapvm       AS CHAR  NO-UNDO.
DEF VAR claepvm      AS CHAR  NO-UNDO.
DEF VAR overpayacc   AS INT   NO-UNDO.
DEF VAR xVatAmt      AS DEC   NO-UNDO.
DEF VAR xVatTot      AS DEC   NO-UNDO.
DEF VAR lcTypeDenied AS CHAR  NO-UNDO.
DEF VAR kpl          AS INT   NO-UNDO.
DEF VAR CgCustno1    AS INT   NO-UNDO.
DEF VAR cgCustno2    AS INT   NO-UNDO.
DEF VAR ldInvTot     AS DEC   NO-UNDO.
DEF VAR liQty        AS INT   NO-UNDO.
DEF VAR liMax        AS INT   NO-UNDO.
DEF VAR liChkInv1    AS INT   NO-UNDO.
DEF VAR liChkInv2    AS INT   NO-UNDO.
DEF VAR liInvNum     AS INT   NO-UNDO.
DEF VAR lcCustName   AS CHAR  NO-UNDO.
DEF VAR llExtGrp     AS LOG   NO-UNDO. 
DEF VAR lcInvGroup   AS CHAR  NO-UNDO.
DEF VAR llStreamOpen AS LOG   NO-UNDO. 

def temp-table latili NO-UNDO
   field latno  AS INT FORMAT "zzzzzzz9"
   field latmk  AS DEC FORMAT "->,>>>,>>9.99"
   INDEX latno latno.

def temp-table yhtili NO-UNDO
   field yhtno  AS INT FORMAT "zzzzz9"
   field yhtmk  AS DEC FORMAT "->,>>>,>>9.99"
   INDEX yhtno yhtno.

DEF TEMP-TABLE ttTotal NO-UNDO
   FIELD Currency AS CHAR
   FIELD InvAmt   AS DEC
   FIELD HomeAmt  AS DEC
   INDEX Currency Currency.

DEF TEMP-TABLE TCGMember NO-UNDO
   FIELD custno LIKE Customer.CustNum
   INDEX custno custno.


FIND FIRST ttCriter NO-ERROR.
IF NOT AVAILABLE ttCriter THEN RETURN "ERROR:Criteria not defined".


form header
   viiva1 at 1 SKIP
   ynimi at 1 "INVOICE JOURNAL FROM PERIOD" at 46
      "Page" at 105 sl FORMAT "ZZZZ9" SKIP
   "Inv.group" at 1 lcInvGroup FORMAT "x(30)" 
      ttCriter.InvDate1 at 46 FORMAT "99-99-9999" "-" 
      ttCriter.InvDate2 FORMAT "99-99-9999"
   ttCriter.DenyPrint FORMAT "DENIED/" at 80  
   ttCriter.OnlyUnpaid FORMAT "UNPAID/" at 90
   STRING(pvm,"99-99-99") at 107 SKIP
   viiva2 at 1 SKIP(1)

   "Invoice"     at 1
   "Customer"    to 21
   "Customer's"  at 23
   "Date Of"     at 43
   "Dueday"      at 52
   "Total Ex"    TO 73
   "VAT"         TO 87
   "Rounding"    TO 96
   "To Be Paid"  TO 110

   "Number"      at 1
   "Number"      to 21
   "Name"        at 23
   "Invoice"     at 43
   "VAT"         TO 73
   "Incl VAT"    TO 110
 
   viiva3 at 1 SKIP(1)
with
   width 116 no-label no-box frame sivuots.

form header
   viiva1 at 1 SKIP
   ynimi at 1 "ACCOUNT SUMMARY / VOUCHER FOR GENERAL LEDGER" at 37
     "Page" at 105 sl FORMAT "ZZZZ9" SKIP
   "Inv.group" at 1 
      lcInvGroup FORMAT "x(25)"
     ttCriter.InvDate1 at 42 FORMAT "99-99-9999" "-" 
     ttCriter.InvDate2 FORMAT "99-99-9999"
     STRING(pvm,"99-99-99") at 107 SKIP
   viiva2 at 1 SKIP(1)
   "Account" at 1 "Amt Debit" at 22 "Amt Credit" at 38 SKIP
   viiva3 at 1 SKIP
with
   width 116 no-label no-box frame tilyht.


FUNCTION fChgPage RETURNS LOGICAL
   (iAddLine AS INT).

   if rl + iAddLine >= skayt1 then do:
      if sl > 0 then do:
         {Syst/uprfeed.i rl}
      end.
      ASSIGN rlx = 0 sl = sl + 1 rl = 9.
      view stream tul frame sivuots.
   end.

END FUNCTION.


FUNCTION fCollectAcc RETURNS LOGICAL
   (iAccNum AS INT,
    iAmount AS DEC). 

   /* totals (in home currency) */
   find first yhtili WHERE yhtili.yhtno = iAccNum no-error.
   if not available yhtili then do:
      create yhtili.
      ASSIGN yhtili.yhtno = iAccNum.
   end.
   ASSIGN yhtili.yhtmk = yhtili.yhtmk + 
                         fToHomeCurr(iAmount,Invoice.ExchRate). 

   /* invoice Level (in original currency) */
   find first latili WHERE latili.latno = iAccNum no-error.
   if not available latili then do:
      create latili.
      ASSIGN latili.latno = iAccNum.
   end.
   ASSIGN latili.latmk = latili.latmk + iAmount.

   ldInvTot = ldInvTot + iAmount.
   
   RETURN TRUE. 

END FUNCTION.

FUNCTION fStripInvID RETURNS INTEGER
   (icInvID AS CHAR):
   
   DEF VAR liStrip AS INT NO-UNDO.
   
   liStrip = 0.
   
   /* separate prefix */
   IF LENGTH(icInvID) > 4 THEN icInvID = SUBSTRING(icInvId,5).

   liStrip = INTEGER(icInvID) NO-ERROR.
                                       
   RETURN liStrip.

END FUNCTION.


ASSIGN OverPayAcc   = fCParamI("OverPayAcc")
       lcTypeDenied = fCParamC("InvTypeDenied")
       viiva1       = fill("=",lev)
       viiva2       = fill("=",lev)
       viiva3       = fill("-",lev)
       sl           = 0 
       rl           = skayt1
       llExtGrp     = (CAN-FIND(FIRST tCustGroup))
       llStreamOpen = FALSE.

EMPTY TEMP-TABLE ttTotal.

IF NOT llExtGrp THEN DO:
   ASSIGN
      cgcustno1 = 1
      cgcustno2 = 999999999.
END.

ELSE DO:
   ASSIGN cgcustno1 = 999999999
          cgcustno2 = 1. 

   FOR EACH TCustGroup,
       EACH cgmember NO-LOCK WHERE
            cgMember.Brand     = gcBrand AND
            cgmember.custgroup = Tcustgroup.custgroup:

      FIND FIRST tcgmember WHERE 
                 Tcgmember.custno = cgmember.custnum
      NO-LOCK NO-ERROR.

      IF NOT AVAIL tcgmember THEN DO:           
         CREATE Tcgmember.
         ASSIGN
            Tcgmember.custno = cgmember.custnum
            cgcustno1        = min(cgcustno1,cgmember.custnum)
            cgcustno2        = max(cgcustno2,cgmember.custnum).
      END.   
   END.

END. 

IF ttCriter.CustNum1 NE ttCriter.CustNum2 THEN ASSIGN 
   ttCriter.CustNum1 = MAX(ttCriter.CustNum1,cgCustno1)
   ttCriter.CustNum2 = MIN(ttCriter.CustNum2,cgCustno2).

IF ttCriter.InvGroup > "" THEN DO:
   FIND InvGroup WHERE 
        InvGroup.Brand    = gcBrand AND
        InvGroup.InvGroup = ttCriter.InvGroup NO-LOCK NO-ERROR.
   lcInvGroup = ttCriter.InvGroup + " " +   
                (IF AVAILABLE InvGroup THEN InvGroup.IGName ELSE "").
END.
ELSE lcInvGroup = "ALL".


runko:
FOR EACH Invoice NO-LOCK USE-INDEX InvDate WHERE             
         Invoice.Brand      = gcBrand             AND 
         Invoice.InvDate   >= ttCriter.InvDate1   AND 
         Invoice.InvDate   <= ttCriter.InvDate2   AND 
         Invoice.ExtInvID  >= ttCriter.ExtInvID1  AND
         Invoice.ExtInvID  <= ttCriter.ExtInvID2  AND
         Invoice.CustNum   >= ttCriter.CustNum1   AND
         Invoice.CustNum   <= ttCriter.CustNum2   AND
         Invoice.InvType   >= ttCriter.InvType1   AND
         Invoice.InvType   <= ttCriter.InvType2   AND
         Invoice.PaymState >= ttCriter.PaymState1 AND
         Invoice.PaymState <= ttCriter.PaymState2 AND 
         (IF ttCriter.InvGroup > "" 
          THEN Invoice.InvGroup = ttCriter.InvGroup 
          ELSE TRUE)                              AND
         (IF ttCriter.DenyPrint THEN Invoice.InvCfg[1] = TRUE ELSE TRUE)   AND
         (IF ttCriter.DenyRemind THEN Invoice.ClaimPerm = FALSE ELSE TRUE) AND
         (IF ttCriter.ZeroVat THEN Invoice.VatAmt = 0 ELSE TRUE)           AND
         /* use not denied (not noted if only one type chosen) */
         (IF ttCriter.InvType1 NE ttCriter.InvType2
          THEN LOOKUP(STRING(Invoice.InvType),lcTypeDenied) = 0
          ELSE TRUE),
   FIRST Customer NO-LOCK WHERE
         Customer.CustNum = Invoice.CustNum AND
         (IF llExtGrp
          THEN CAN-FIND(FIRST tcgmember WHERE
                              tcgmember.Custno = Customer.CustNum)
          ELSE TRUE)
BY Invoice.ExtInvID:

       IF ttCriter.OnlyUnpaid AND Invoice.PaidAmt = Invoice.InvAmt THEN NEXT.

       /* output to file */
       IF ttCriter.ToFile > "" AND NOT llStreamOpen THEN DO:
          OUTPUT STREAM tul TO VALUE(ttCriter.ToFile).
          llStreamOpen = TRUE.
       END.  

       IF ttCriter.Invoices THEN DO:
          fChgPage(5).
       END.

       ASSIGN laskyht = laskyht + Invoice.InvAmt
              liQty   = liQty + 1.
              
       IF NOT SESSION:BATCH AND liQty MOD 1000 = 0 THEN DO:
          PAUSE 0.
          DISP liQty LABEL "Invoices" 
          WITH OVERLAY ROW 12 COL 50 SIDE-LABELS TITLE " COLLECT " FRAME fQty.
       END.

       /* missing invoices (only if no extra criteria defined) */
       if edInvNum > ""           AND 
          NOT ttCriter.DenyPrint  AND 
          NOT ttCriter.DenyRemind AND
          NOT ttCriter.OnlyUnpaid AND
          NOT ttCriter.ZeroVat    AND
          ttCriter.Invoices
       then do:
          ASSIGN liChkInv1 = fStripInvID(edInvNum)
                 liChkInv2 = fStripInvID(Invoice.ExtInvID).
          if liChkInv1 + 1 NE liChkInv2 AND  
             Invoice.ExtInvID BEGINS SUBSTRING(edInvNum,1,4)
          then do:
             PUT STREAM tul "* * * INVOICE NUMBER(S) MISSING * * *" at 2 SKIP.
             ASSIGN rl = rl + 1.
          end.
       end.

       ASSIGN
          edInvNum   = Invoice.ExtInvID
          lcCustName = Invoice.CustName.
       
       /* temporary customer used */       
       IF Customer.CustNum < 1000 THEN DO:
      
          /* credit invoice */
          IF Invoice.InvType = 8 OR Invoice.InvType = 9 THEN
             liInvNum = Invoice.CrInvNum.
          ELSE liInvNum = Invoice.InvNum.
      
          FOR FIRST Order NO-LOCK WHERE
                    Order.InvNum = liInvNum,
              FIRST OrderCustomer OF Order NO-LOCK WHERE
                    OrderCustomer.RowType = Order.InvCustRole:
                
             lcCustName = DYNAMIC-FUNCTION("fPrintOrderName" IN ghFunc1,
                                           BUFFER OrderCustomer).
          END.                                     

       END.        
 
       IF ttCriter.Invoices THEN DO:
          PUT STREAM tul
             Invoice.ExtInvID   at 1   FORMAT "x(12)"
             Invoice.CustNum    at 14  FORMAT "zzzzzzz9"
             lcCustName         at 23  FORMAT "x(19)"
             Invoice.InvDate    at 43  FORMAT "99.99.99"
             Invoice.DueDate    at 52  FORMAT "99.99.99"
             Invoice.AmtExclVAT to 73  FORMAT "->,>>>,>>9.99"
             Invoice.VATAmt     to 87  FORMAT "->,>>>,>>9.99"
             Invoice.Rounding   to 96  FORMAT "->>>9.99"
             Invoice.InvAmt     to 110 FORMAT "->,>>>,>>9.99"
             Invoice.Currency   at 112 FORMAT "X(3)" 
             SKIP.
          rl = rl + 1.
       END.

       ASSIGN 
          oiInvQty = oiInvQty + 1  
          AccNum   = 0
          Posting  = 0
          ldInvTot = 0.

       EMPTY TEMP-TABLE latili.
       
       /* postings */
       if ttCriter.InvAccounts or ttCriter.Summary then do:

          ASSIGN
          AccNum[1] = Invoice.ARAccNum    Posting[1] = Invoice.InvAmt
          AccNum[2] = Invoice.RoundAccNum Posting[2] = 0 - Invoice.Rounding
          AccNum[3] = Invoice.IntAccNum   Posting[3] = 0 - Invoice.InterestAmt
          AccNum[4] = Invoice.APAccNum    Posting[4] = 0 - Invoice.AdvPaym
          AccNum[5] = Invoice.OPAccNum    Posting[5] = 0 - Invoice.OverPaym.
          
          /* VAT can be divided into several accounts  */
          xVatTot = 0.
                 
          do i = 1 to 10:
             ASSIGN AccNum[i + 5]  = Invoice.VATAccount[i] 
                    Posting[i + 5] = -1 * Invoice.VATAmount[i]
                    xVatTot        = xVatTot + Invoice.VatAmount[i].
          end.

          do i = 1 to 15:
             if Posting[i] = 0 then next.

             fCollectAcc(AccNum[i],
                         Posting[i]).
          end.

          /* invoice lines are posted without VAT */
          FOR EACH InvRow of Invoice NO-LOCK 
          break by InvRow.VatPerc
                by InvRow.Amt:

             /* if VAT is included then remove it */
             ASSIGN xVatAmt = 0.

             if InvRow.VatPerc > 0 AND Invoice.VatIncl then do:

                if last(InvRow.Amt) then 
                    ASSIGN xVatAmt = xVatTot
                           xVatTot = 0.
                else ASSIGN xVatAmt = round(InvRow.Amt * InvRow.VATPerc
                                            / (100 + InvRow.VATPerc),3)
                            xVatTot = xVatTot - xVatAmt.
             end.

             /* update temp-tables */
             fCollectAcc(InvRow.SlsAccNum,
                         -1 * (InvRow.Amt - xVatAmt)). 

          end.
              
          /* if there is a difference caused by the use of 3 decimals in
             invoice row amounts then post it to rounding account */
          IF ldInvTot >= -0.005 AND ldInvTot <= 0.005 AND ldInvTot NE 0  
          THEN DO:
             fCollectAcc(Invoice.RoundAccNum,
                         -1 * ldInvTot).
          END.

          /* tulostetaan laskun tilit */
          IF ttCriter.Invoices AND ttCriter.InvAccounts THEN 
          FOR EACH latili 
          BY latili.latno:

             IF ROUND(latili.latmk,2) NE 0 THEN DO:
                fChgPage(0).

                PUT STREAM tul
                   "Acct"       at 87
                   latili.latno at  93 FORMAT "zzzzzzz9"
                   latili.latmk at 102 FORMAT "->,>>>,>>9.99"
                SKIP.
                ASSIGN rl = rl + 1.
             END.
          end.

          IF ttCriter.Invoices AND ttCriter.invAccounts THEN DO:
             PUT STREAM tul SKIP(1).
             ASSIGN rl = rl + 1.
          END.

       end. /* postings */

       else IF ttCriter.Invoices THEN do:
          rlx = rlx + 1.
          if rlx = 5 then do:
             PUT STREAM tul SKIP(1).
             rl = rl + 1. rlx = 0.
          end.
       end.

       /* totals by currency */
       FIND FIRST ttTotal WHERE ttTotal.Currency = Invoice.Currency NO-ERROR.
       IF NOT AVAILABLE ttTotal THEN DO:
          CREATE ttTotal.
          ASSIGN ttTotal.Currency = Invoice.Currency.
       END.
       ASSIGN ttTotal.InvAmt  = ttTotal.InvAmt  + Invoice.InvAmt
              ttTotal.HomeAmt = ttTotal.HomeAmt + Invoice.CurrAmt.

end. /* print-line */

HIDE FRAME fQty NO-PAUSE.

/* totals */
IF ttCriter.Invoices AND oiInvQty > 0 THEN DO:

   fChgPage(2).

   PUT STREAM tul
      viiva1 at 1
      SKIP.
   ASSIGN rl = rl + 1.

   /* totals by currency */
   IF CAN-FIND(FIRST ttTotal WHERE ttTotal.Currency NE lcHomeCurr) THEN DO:
      FOR EACH ttTotal:

         fChgPage(1). 

         IF ttTotal.Currency NE lcHomeCurr THEN 
         PUT STREAM tul
            ttTotal.Currency  FORMAT "X(3)" TO 69
            ttTotal.InvAmt    FORMAT "->>>,>>>,>>9.99" TO 85.

         PUT STREAM tul 
            lcHomeCurr         FORMAT "X(3)" TO 94
            ttTotal.HomeAmt   FORMAT "->>>,>>>,>>9.99" TO 110
            SKIP.

         rl = rl + 1.

         ACCUMULATE ttTotal.HomeAmt (TOTAL).
      END.

      laskyht = (ACCUM TOTAL ttTotal.HomeAmt). 

      fChgPage(2).
      PUT STREAM tul 
         viiva1 AT 1 SKIP.
      rl = rl + 1.
   END.

   PUT STREAM tul UNFORMATTED 
      "**  TOTAL:" at 2 
      TRIM(STRING(oiInvQty,">>,>>>,>>>,>>9")) AT 20
      " invoices" 
      lcHomeCurr TO 94  FORMAT "X(3)" 
      laskyht   to 110 FORMAT "->>>,>>>,>>9.99"
      "**" SKIP.

   ASSIGN rl = rl + 1.

END. 

/* posting summary */
if ttCriter.Summary AND oiInvQty > 0 then do:

   IF ttCriter.Invoices THEN DO:
      {Syst/uprfeed.i rl}
   END.

   ASSIGN sl = sl + 1 rl = 7.
   view stream tul frame tilyht.

   ASSIGN debyht = 0.

   FOR EACH yhtili WHERE 
           ROUND(yhtili.yhtmk,2) > 0
   by yhtili.yhtno:
      PUT STREAM tul 
         yhtno at 1  FORMAT ">>>>>>>9"
         yhtmk to 30 FORMAT "->>>>>>>>9.99" SKIP.
      ASSIGN
         rl     = rl     + 1
         debyht = debyht + yhtmk.
   end.

   ASSIGN kreyht = 0.

   FOR EACH yhtili WHERE 
            ROUND(yhtili.yhtmk,2) < 0
   by yhtili.yhtno:     

      PUT STREAM tul
         yhtno at 1  FORMAT ">>>>>>>9"
         (0 - yhtmk) FORMAT "->>>>>>>>9.99" to 47 SKIP.
      
      ASSIGN
         rl     = rl     + 1
         kreyht = kreyht - yhtmk.
   end.

   PUT STREAM tul 
      viiva1 at 1 SKIP
      "**  TOTAL " at 1
      lcHomeCurr FORMAT "X(3)" 
      debyht to 30 kreyht to 47 
      "Invoice qty: " + TRIM(STRING(oiInvQty,">>,>>>,>>>,>>9"))
         FORMAT "X(30)" AT 60
      SKIP.
   ASSIGN rl = rl + 2.

end. /* summary */

IF ttCriter.ToFile = "" OR llStreamOpen THEN DO:
   {Syst/uprfeed.i rl}

   IF ttCriter.ToFile > "" THEN DO:
      OUTPUT STREAM tul CLOSE.
   END.
END.

RETURN "".





