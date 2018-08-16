{triggers/hpdwrite_generic.i Customer CUSTOMER Common CustNum}

IF NOT NEW(Customer) AND
  (oldCustomer.BankAcct NE Customer.BankAcct OR
   oldCustomer.InvGroup NE Customer.InvGroup OR
   oldCustomer.ChargeType NE Customer.ChargeType OR
   oldCustomer.Currency NE Customer.Currency)
THEN DO: 
   FOR EACH CustomerAccount NO-LOCK WHERE
            CustomerAccount.Custnum = Customer.Custnum:

      FOR EACH InvoiceTargetGroup EXCLUSIVE-LOCK WHERE
               InvoiceTargetGroup.Custnum = Customer.Custnum AND
               InvoiceTargetGroup.AccountID = CustomerAccount.AccountID:
         ASSIGN
            InvoiceTargetGroup.BankAccount = Customer.BankAcct
            InvoiceTargetGroup.PaymentMethod = Customer.ChargeType
            InvoiceTargetGroup.InvGroup = Customer.InvGroup.
      END.
   END.
      
END.
