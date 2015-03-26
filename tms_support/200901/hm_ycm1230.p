FIND Mobsub WHERE Mobsub.CLI = "622874148" NO-LOCK NO-ERROR.
IF Mobsub.AgrCust = Mobsub.CustNum AND Mobsub.InvCust = Mobsub.CustNum THEN
DO:

   FIND Customer WHERE Customer.CustNum = Mobsub.AgrCust NO-LOCK NO-ERROR.
   IF NOT AVAIL Customer THEN 
      MESSAGE "Customer with custnum " Mobsub.AgrCust " was not available!"
        VIEW-AS ALERT-BOX.
   ELSE
   DO:
      IF Customer.CustIdType = "NIF" AND
         Customer.OrgId = "01925862a" THEN
      DO:
         MESSAGE "CustIdType and OrgId matched" VIEW-AS ALERT-BOX.

         FIND CURRENT Customer EXCLUSIVE-LOCK.
         IF AVAIL Customer THEN
         DO:
            Customer.CustIdType = "NIE".
            Customer.OrgId = "x2997607v".
            MESSAGE "Customer changed" VIEW-AS ALERT-BOX.
         END.
         ELSE
            MESSAGE "Customer could not be locked" VIEW-AS ALERT-BOX.
      END.
      ELSE
         MESSAGE "No match! Customer.CustIdType was "
           Customer.CustIdType 
           " and Customer.OrgId was " Customer.OrgId
           VIEW-AS ALERT-BOX.
   END.
END.
ELSE
  MESSAGE "Customer records of the mobsub are not the same, Mobsub.AgrCust = "
    MobSub.AgrCust ", Mobsub.CustNum = " MobSub.CustNum 
    ", Mobsub.InvCust = " Mobsub.InvCust VIEW-AS ALERT-BOX.


