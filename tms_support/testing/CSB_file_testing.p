DEF VAR liPicked    AS INT  NO-UNDO. 
DEF VAR liBankQty   AS INT  NO-UNDO EXTENT 3. 
DEF VAR ldBankAmt   AS DEC  NO-UNDO EXTENT 3. 
DEF VAR liTotalOthers   AS INT NO-UNDO.
DEF VAR liOtherInvCount AS INT NO-UNDO.

DEF TEMP-TABLE ttInvoice NO-UNDO
   FIELD InvNum   AS INT
   FIELD extinvid AS CHAR
   FIELD DueDate  AS DATE
   FIELD BankCode AS CHAR
   FIELD Movable  AS LOG 
   FIELD InvAmt   AS DEC 
   INDEX InvNum IS UNIQUE InvNum
   INDEX DueDate DueDate BankCode
   INDEX InvAmt Movable BankCode InvAmt.

for each invoice where
         invoice.brand = "1" and
         invoice.invdate = 08/01/2012 no-lock,
    first customer where
          customer.brand = "1" and
          customer.custnum = invoice.custnum no-lock:
   
    /* charge type is not direct debit */
    IF Invoice.ChargeType NE 2 THEN NEXT. 
             
    /* no negative invoices */
    IF Invoice.InvAmt <= 0 THEN NEXT. 
       
    /* printing denied */
    IF Invoice.InvCfg[1] = TRUE THEN NEXT.
    
    IF Invoice.InvType NE 1 THEN NEXT.

    IF Invoice.PrintState < 0 OR 
       Invoice.PrintState > 1 THEN NEXT. 

    CREATE ttInvoice.
    ASSIGN ttInvoice.InvNum   = Invoice.InvNum
           ttInvoice.extinvid = Invoice.extinvid
           ttInvoice.InvAmt   = Invoice.InvAmt
           ttInvoice.DueDate  = Invoice.DueDate
           ttInvoice.Movable  = FALSE
           liPicked           = liPicked + 1
           ttInvoice.BankCode = SUBSTRING(Customer.BankAcc,1,4).

       CASE ttInvoice.BankCode:
       WHEN "0049" THEN ASSIGN
          liBankQty[1] = liBankQty[1] + 1
          ldBankAmt[1] = ldBankAmt[1] + Invoice.InvAmt.
       WHEN "0030" THEN ASSIGN 
          liBankQty[2] = liBankQty[2] + 1
          ldBankAmt[2] = ldBankAmt[2] + Invoice.InvAmt.
       WHEN "0182" OR
       WHEN "2040" OR
       WHEN "2074" OR
       WHEN "2059" OR
       WHEN "2107" THEN ASSIGN
          liBankQty[3] = liBankQty[3] + 1
          ldBankAmt[3] = ldBankAmt[3] + Invoice.InvAmt.
       /* Rest invoices will be divided based on the algorithm */
       OTHERWISE ASSIGN
          ttInvoice.BankCode = ""
          ttInvoice.Movable  = TRUE.
       END CASE.
END.

OUTPUT TO "/apps/yoigo/tms_support/testing/CSB19_split_check_before_201208.xls".

PUT UNFORMATTED "Invoice Id" CHR(9)
                "InvAmt"     CHR(9)
                "BankCode"   SKIP.

FOR EACH ttInvoice NO-LOCK BY ttInvoice.InvAmt DESC:
   PUT UNFORMATTED ttInvoice.extinvid CHR(9)
                   ttInvoice.InvAmt   CHR(9)
                   ttInvoice.BankCode SKIP.
END. /* FOR EACH ttInvoice: */

PUT UNFORMATTED "liBankQty[1]: "  liBankQty[1] " liBankQty[2]: " liBankQty[2] " liBankQty[3]:" liBankQty[3] " Total Invoices: " liPicked SKIP.

OUTPUT CLOSE.

RUN pSplitOtherInvoices.

OUTPUT TO "/apps/yoigo/tms_support/testing/CSB19_split_check_after_201208.xls".

PUT UNFORMATTED "Invoice Id" CHR(9)
                "InvAmt"     CHR(9)
                "BankCode"   SKIP.

FOR EACH ttInvoice BY ttInvoice.InvAmt DESC:
   PUT UNFORMATTED ttInvoice.extinvid CHR(9)
                   ttInvoice.InvAmt   CHR(9)
                   ttInvoice.BankCode SKIP.
END. /* FOR EACH ttInvoice: */

PUT UNFORMATTED "liBankQty[1]: "  liBankQty[1] " liBankQty[2]: " liBankQty[2] " liBankQty[3]:" liBankQty[3] " Total Invoices: " liPicked SKIP.

OUTPUT CLOSE.

PROCEDURE pSplitOtherInvoices:

   DEF VAR liLimit    AS INT NO-UNDO.
   DEF VAR liCounter  AS INT NO-UNDO.
   DEF VAR liIndex    AS INT NO-UNDO.

   liLimit = 100 * (((0.35 * (liPicked - liBankQty[1])) - liBankQty[3]) /
                    (liPicked - (liBankQty[1] + liBankQty[2] + liBankQty[3]))).
    
   /* Fetch all other invoices by amount from higher to lower */
   /* and divide based on the algorithm                       */
   FOR EACH ttInvoice WHERE 
            ttInvoice.Movable = TRUE AND
            ttInvoice.BankCode = ""
            BY ttInvoice.InvAmt DESC:

       liIndex = liCounter MOD 100.

       IF liIndex <= liLimit THEN
          ASSIGN liBankQty[3] = liBankQty[3] + 1
                 ldBankAmt[3] = ldBankAmt[3] + ttInvoice.InvAmt
                 ttInvoice.BankCode = "0182"
                 ttInvoice.Movable  = FALSE.
       ELSE
          ASSIGN liBankQty[2] = liBankQty[2] + 1
                 ldBankAmt[2] = ldBankAmt[2] + ttInvoice.InvAmt
                 ttInvoice.BankCode = "0030"
                 ttInvoice.Movable  = FALSE.

       liCounter = liCounter + 1.

   END. /* FOR EACH ttInvoice WHERE */

END PROCEDURE. /* PROCEDURE pSplitOtherInvoices: */



