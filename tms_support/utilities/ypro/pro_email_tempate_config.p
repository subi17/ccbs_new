/*This script is done make SVA email settings.
Manual line feeds must be needed after execution */


FUNCTION fAddText RETURNS CHAR
   (icKeyValue AS CHAR,
    icText AS CHAR,
    icTitle AS CHAR):
   DEF BUFFER InvText FOR InvText.

   FIND FIRST tmscodes NO-LOCK WHERE
              TMSCodes.TableName = "Invtext" AND
              TMSCodes.FieldName = "KEYVALUE" AND
              TMSCodes.CodeGroup = "EMAIL" AND
              TMSCodes.CodeValue = icKeyvalue NO-ERROR.
   IF NOT AVAIL TMSCodes THEN DO:
      CREATE TMSCodes.
      ASSIGN
         TMSCodes.TableName = "Invtext"
         TMSCodes.FieldName = "KEYVALUE" 
         TMSCodes.CodeGroup = "EMAIL" 
         TMSCodes.CodeValue = icKeyvalue.
 
   END.


   FIND FIRST InvText NO-LOCK  WHERE
              InvText.Brand EQ "1" AND
              InvText.Target EQ "EMAIL" AND
              InvText.KeyValue EQ icKeyValue AND
              InvText.ToDate GE TODAY NO-ERROR.
   IF AVAIL InvText THEN RETURN "InvText already exists " + icKeyValue.

   CREATE InvText.
   ASSIGN InvText.Brand = "1"
          InvText.Language = 1
          InvText.FromDate = TODAY
          InvText.ToDate = 12/31/49
          InvText.Target = "EMAIL"
          InvText.KeyValue = icKeyValue 
          InvText.TxtTitle = icTitle
          InvText.InvText = icText
          InvText.ITNum    = NEXT-VALUE(it-seq).

END.

def var lcRet as char no-undo.

lcRet = fAddText("SVA_FAXTOMAIL", 
"Nombre Cliente: #CUSTNUM 
ORDER ID: #ORDERID
#CUSTTYPE: #CUSTID
Dirección de correo electrónico: #EMAIL
Numeración: #NUMBER",
"SVA_ FAXTOMAIL YOIGO #STATUS"  ).

if lcRet NE "" THEN MESSAGE lcRet VIEW-AS ALERT-BOX.
lcRet = "".

lcRet = fAddText("SVA_OFFICE365", 
"Nombre Cliente: #CUSTNUM 
ORDER ID: #ORDERID
#CUSTTYPE: #CUSTID
Dirección de correo electrónico: #EMAIL",
"SVA_ OFFICE 365 YOIGO #STATUS"  ).

if lcRet NE "" THEN MESSAGE lcRet VIEW-AS ALERT-BOX.
lcRet = "".

lcRet = fAddText("SVA_IP_FIJA_YOIGO", 
"Nombre Cliente: #CUSTNUM 
ORDER ID: #ORDERID
#CUSTTYPE: #CUSTID",
"SVA_ IP FIJA YOIGO #STATUS"  ).

if lcRet NE "" THEN MESSAGE lcRet VIEW-AS ALERT-BOX.
lcRet = "".

lcRet = fAddText("SVA_Centralita_PRO", 
"Nombre Cliente: #CUSTNUM 
ORDER ID: #ORDERID
#CUSTTYPE: #CUSTID",
"Centralita PRO  #STATUS"  ).

if lcRet NE "" THEN MESSAGE lcRet VIEW-AS ALERT-BOX.
lcRet = "".






