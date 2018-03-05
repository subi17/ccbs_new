/*
   RES-885 - Add new values to TMSCodes table.

*/

FIND FIRST TMSCodes NO-LOCk WHERE
   TMSCodes.TableName EQ "Customer" AND
   TMSCodes.FieldName EQ "NWProfiles" NO-ERROR.

IF NOT AVAIL TMSCodes THEN DO:
/* For future use
   CREATE TMSCodes.
   ASSIGN 
      TMSCodes.TableName = "Customer"
      TMSCodes.FieldName = "NWProfiles"
      TMSCodes.CodeGroup = "NWProfile"
      TMSCodes.CodeValue = "1"
      TMSCodes.CodeName  = "Solo Yoigo"
      TMSCodes.InUse     = 1.
*/
   CREATE TMSCodes.
   ASSIGN 
      TMSCodes.TableName = "Customer"
      TMSCodes.FieldName = "NWProfiles"
      TMSCodes.CodeGroup = "NWProfile"
      TMSCodes.CodeValue = "2"
      TMSCodes.CodeName  = "Yoigo + Orange"
      TMSCodes.InUse     = 1.

   CREATE TMSCodes.
   ASSIGN 
      TMSCodes.TableName = "Customer"
      TMSCodes.FieldName = "NWProfiles"
      TMSCodes.CodeGroup = "NWProfile"
      TMSCodes.CodeValue = "3"
      TMSCodes.CodeName  = "Yoigo + Orange + Movistar"
      TMSCodes.InUse     = 1.
END.

