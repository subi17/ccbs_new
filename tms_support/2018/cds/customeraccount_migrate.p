def stream sout.

DEF VAR llSimulate AS LOG NO-UNDO. 
llSimulate = true.

output stream sout to value("/apps/yoigo/tms_support/2018/cds/customeraccount_migrate.log" + if llSimulate then ".simulate" else "").

DEF VAR liAccountID AS INT NO-UNDO.
liAccountID = CURRENT-VALUE(AccountID).

DEF VAR liLoop AS INT NO-UNDO.

FOR EACH Customer EXCLUSIVE-LOCK  WHERE
         Customer.brand EQ Syst.Var:gcBrand:

   IF CAN-FIND(FIRST CustomerAccount NO-LOCK WHERE
                     CustomerAccount.Custnum EQ Customer.Custnum) THEN NEXT.

   IF NOT CAN-FIND(FIRST msowner NO-LOCK WHERE
            msowner.custnum = customer.custnum and
            msowner.tsend >= 20180101) THEN NEXT.
   liLoop = liLoop + 1.

   if liLoop mod 100 eq 0 THEN DO:
      disp liLoop customer.custnum with frame a.
      pause 0.
   END.

   put stream sout unformatted skip(1).

   if not llSimulate then do:
      CREATE CustomerAccount.
      ASSIGN
         CustomerAccount.AccountID = NEXT-VALUE(AccountID)
         CustomerAccount.Custnum = Customer.Custnum
         CustomerAccount.DefaultAcc = TRUE
         CustomerAccount.FromDate = TODAY
         CustomerAccount.ToDate = 12/31/2049.

      liAccountID = CustomerAccount.AccountID.
   end.
   else liAccountID = liAccountID + 1.

   put stream sout unformatted
      "CUSTOMER:" customer.custnum ";" liAccountID skip.

   FOR EACH mobsub EXCLUSIVE-LOCK where
            mobsub.custnum = customer.custnum and
            mobsub.accountid eq 0:

      if not llSimulate then
         mobsub.accountid = liaccountid.
      put stream sout unformatted
         "MOBSUB:" mobsub.msseq ";" mobsub.custnum ";" liAccountID skip.
   END.

   FOR EACH msowner EXCLUSIVE-LOCK where
            msowner.custnum = customer.custnum and
            msowner.tsend >= 20180101:
      if not llSimulate then
         msowner.accountid = liaccountid.
      put stream sout unformatted
         "MSOWNER:"  msowner.msseq ";" msowner.custnum ";" recid(msowner) ";" msowner.tsend ";"
             liAccountID skip.
   END.

   FOR EACH InvoiceTargetGroup EXCLUSIVE-LOCK USE-INDEX Custnum WHERE
            InvoiceTargetGroup.Custnum = Customer.Custnum and
            InvoiceTargetGroup.Todate >= TODAY:

      put stream sout unformatted
         "INVOICETARGETGROUP:"  invoicetargetgroup.itgroupid ";" invoicetargetgroup.custnum ";"
             liAccountID skip.

      if not llSimulate then
      ASSIGN
         InvoiceTargetGroup.Currency = Customer.Currency
         InvoiceTargetGroup.BankAccount = Customer.BankAcct
         InvoiceTargetGroup.PaymentMethod = Customer.ChargeType
         InvoiceTargetGroup.BankAccount = Customer.BankAcct
         InvoiceTargetGroup.BillCycle = 1
         InvoiceTargetGroup.InvInterval = 1
         InvoiceTargetGroup.DelType = Customer.DelType
         InvoiceTargetGroup.AccountID = liAccountID
         InvoiceTargetGroup.DueDateOffSet = 0
         InvoiceTargetGroup.InvGroup = Customer.InvGroup
/*
         InvoiceTargetGroup.BankName = ""
         InvoiceTargetGroup.CustAccName = ""
         InvoiceTargetGroup.MandateDate = ?
         InvoiceTargetGroup.MandateID = ""
*/
         .
   END.
END.


