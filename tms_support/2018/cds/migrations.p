PROCEDURE pMigrateBillingAddressToAddress:

   DEF VAR liaddrID AS INT NO-UNDO.

   DEF BUFFER bCustomer FOR Customer.
   
   FOR EACH bCustomer NO-LOCK WHERE 
            bCustomer.brand EQ Syst.Var:gcBrand:   

      IF CAN-FIND(FIRST Address WHERE
          Address.Keyvalue = STRING(bCustomer.CustNum)) THEN NEXT.

      liaddrID = liaddrID + 1.
      CREATE Address.
      ASSIGN
         Address.AddressID = liaddrID
         Address.HostTable = "Customer"
         Address.KeyValue = STRING(bCustomer.CustNum)
         Address.AddressType = "Billing"
         Address.Address = bCustomer.Address
         Address.City = bCustomer.PostOffice
         Address.ZipCode = bCustomer.ZipCode
         Address.Region = bCustomer.Region
         Address.Country = bCustomer.Country.

      FIND FIRST CustomerReport WHERE
                 CustomerReport.Custnum = bCustomer.Custnum
      EXCLUSIVE-LOCK NO-ERROR.

      IF AVAIL CustomerReport THEN
         ASSIGN
            Address.StreetCode = CustomerReport.StreetCode
            Address.CityCode = CustomerReport.CityCode
            Address.TownCode = CustomerReport.TownCode.

   END.
END PROCEDURE.


PROCEDURE pMigrateSubsToCustAcc:

DEF BUFFER bOrderCustomer FOR OrderCustomer.

   FOR EACH bOrderCustomer NO-LOCK WHERE
            bOrderCustomer.brand EQ Syst.Var:gcBrand:

      IF CAN-FIND(FIRST CustomerAccount NO-LOCK WHERE
            CustomerAccount.Custnum EQ bOrderCustomer.Custnum) THEN NEXT.

      CREATE CustomerAccount.
      ASSIGN
         CustomerAccount.AccountID = NEXT-VALUE(AccountID)
         CustomerAccount.Custnum = bOrderCustomer.Custnum
         CustomerAccount.DefaultAcc = TRUE
         CustomerAccount.BillCycle = 1
         CustomerAccount.InvInterval = 1
         CustomerAccount.DelType = bOrderCustomer.DelType
         CustomerAccount.FromDate = TODAY
         CustomerAccount.ToDate = 12/31/2049.
/*
         CustomerAccount.AccountName =
         CustomerAccount.ShippingAddressID =
         CustomerAccount.DueDateOffset =
*/
   END.
END PROCEDURE.


PROCEDURE pPopulateInvoiceTargetGroup:

FOR EACH Customer NO-LOCK WHERE Customer.brand EQ Syst.Var:gcBrand:

   IF Customer.Custnum EQ Customer.AgrCust THEN DO:
      IF CAN-FIND(FIRST MobSub NO-LOCK WHERE MobSub.Brand EQ Customer.Brand AND
                                             MobSub.AgrCust EQ Customer.CustNum AND
                                             MobSub.PayType EQ FALSE) THEN DO:

         FIND FIRST InvoiceTargetGroup EXCLUSIVE-LOCK USE-INDEX Custnum WHERE
                    InvoiceTargetGroup.Custnum = Customer.Custnum NO-ERROR.
         IF AVAIL InvoiceTargetGroup THEN DO:                                   
            ASSIGN
               InvoiceTargetGroup.Currency = Customer.Currency
               InvoiceTargetGroup.BankAccount = Customer.BankAcct
               InvoiceTargetGroup.PaymentMethod = Customer.PaymMethod.

            FIND FIRST CustomerAccount NO-LOCK WHERE
               CustomerAccount.CustNum = InvoiceTargetGroup.CustNum NO-ERROR.
            IF AVAIL CustomerAccount THEN
               ASSIGN
                  InvoiceTargetGroup.AccountID = CustomerAccount.AccountID      
                  InvoiceTargetGroup.CustAccName = CustomerAccount.AccountName.

            FIND FIRST MsOwner NO-LOCK WHERE MsOwner.Brand = Syst.Var:gcBrand AND
                                             MsOwner.CustNum = Customer.CustNum NO-ERROR.
            IF AVAIL MsOwner THEN
               ASSIGN
                  InvoiceTargetGroup.MandateId = MsOwner.MandateId
                  InvoiceTargetGroup.MandateDate = MsOwner.MandateDate.         

         END.
      END.
   END.
END.
END PROCEDURE.


PROCEDURE pMigrateAccountIDToMobSub:

   DEF BUFFER bMobSub FOR Mobsub.

   FOR EACH bMobsub EXCLUSIVE-LOCK WHERE
            bMobsub.Brand EQ Syst.Var:gcBrand:

      FIND FIRST CustomerAccount WHERE
                 CustomerAccount.CustNum EQ bMobsub.CustNum NO-LOCK NO-ERROR.
      IF AVAIL CustomerAccount THEN
         ASSIGN bMobsub.AccountID = CustomerAccount.AccountID.
   END.
END PROCEDURE.


PROCEDURE pMigrateAccountIDToFixedFee:

DEF BUFFER bFixedFee FOR FixedFee.

   FOR EACH bFixedFee EXCLUSIVE-LOCK WHERE
            bFixedFee.Brand EQ Syst.Var:gcBrand:

      FIND FIRST CustomerAccount WHERE
                 CustomerAccount.Custnum EQ bFixedFee.Custnum NO-LOCK NO-ERROR.
      IF AVAIL CustomerAccount THEN
         ASSIGN bFixedFee.AccountID = CustomerAccount.AccountID.
   END.
END PROCEDURE.


PROCEDURE pMigrateAccountIDToSingleFee:

   DEF BUFFER bSingleFee FOR SingleFee.

   FOR EACH bSingleFee EXCLUSIVE-LOCK WHERE bSingleFee.Brand = Syst.Var:gcBrand:

      FIND FIRST CustomerAccount WHERE
                 CustomerAccount.Custnum EQ bSingleFee.Custnum NO-LOCK NO-ERROR.
      IF AVAIL CustomerAccount THEN
         ASSIGN bSingleFee.AccountID = CustomerAccount.AccountID.
   END.
END PROCEDURE.


PROCEDURE pMigrateAccountIDToFATime:

   DEF BUFFER bFATime FOR FATime.

   FOR EACH bFATime EXCLUSIVE-LOCK WHERE bFATime.Brand = Syst.Var:gcBrand:

      FIND FIRST CustomerAccount WHERE
                 CustomerAccount.Custnum EQ bFATime.Custnum NO-LOCK NO-ERROR.
      IF AVAIL CustomerAccount THEN
         ASSIGN bFATime.AccountID = CustomerAccount.AccountID.
   END.
END PROCEDURE.


PROCEDURE pMigrateAccountIDToInvoice:

   DEF BUFFER bInvoice FOR Invoice.

   FOR EACH bInvoice EXCLUSIVE-LOCK WHERE bInvoice.Brand = Syst.Var:gcBrand:

      FIND FIRST CustomerAccount WHERE
                 CustomerAccount.Custnum EQ bInvoice.Custnum NO-LOCK NO-ERROR.
      IF AVAIL CustomerAccount THEN
         ASSIGN bInvoice.AccountID = CustomerAccount.AccountID.
   END.
END PROCEDURE.


PROCEDURE pMigrateAccountIDToMsOwner:

   DEF BUFFER bMsOwner FOR MsOwner.

   FOR EACH bMsOwner EXCLUSIVE-LOCK WHERE bMsOwner.Brand = Syst.Var:gcBrand:

      FIND FIRST CustomerAccount WHERE
                 CustomerAccount.Custnum EQ bMsOwner.Custnum NO-LOCK NO-ERROR.
      IF AVAIL CustomerAccount THEN
         ASSIGN bMsOwner.AccountID = CustomerAccount.AccountID.
   END.
END PROCEDURE.


RUN pMigrateBillingAddressToAddress.
RUN pMigrateSubsToCustAcc.
RUN pPopulateInvoiceTargetGroup.

RUN pMigrateAccountIDToMobSub.

RUN pMigrateAccountIDToFixedFee.
RUN pMigrateAccountIDToSingleFee.

RUN pMigrateAccountIDToFATime.
RUN pMigrateAccountIDToInvoice.
RUN pMigrateAccountIDToMsOwner.
