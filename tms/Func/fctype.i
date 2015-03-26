/* fctype.i         11.02.03/aam

   check that the chosen invoice charge type and delivery type combination
   is valid 
*/


/* dType: 1 Paper
          2 EMail (pdf)
          3 Direct Invoice
          4 Invoice Hotel

   cType: 1 Cash
          2 Direct Debit
          3 Credit Card
          4 ePayment
*/

FUNCTION fChkCType RETURNS LOGICAL
   (iiCType   AS INT,
    iiDType   AS INT,
    ilMessage AS LOGIC). 

   DEF VAR llValid AS LOGIC NO-UNDO. 

   llValid = TRUE.

   IF iiCType = 0 OR iiDType = 0 THEN RETURN llValid. 

   /* valid charge types are listed in delivery types' memo-field */
   FIND FIRST TMSCodes NO-LOCK WHERE
              TMSCodes.TableName = "Invoice" AND
              TMSCodes.FieldName = "DelType"   AND
              TMSCodes.CodeValue = STRING(iiDType)   NO-ERROR.

   IF AVAILABLE TMSCodes AND 
      LOOKUP(STRING(iiCType),TMSCodes.Memo) = 0
   THEN llValid = FALSE. 

   IF NOT llValid AND ilMessage THEN 
   MESSAGE "Charge type" iiCType "is not valid with invoice delivery type"
           iiDType 
   VIEW-AS ALERT-BOX
   ERROR
   TITLE "Charge Type". 

   RETURN llValid. 

END FUNCTION.

