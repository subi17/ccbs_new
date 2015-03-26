{commali.i}
{email.i}
{tmsconst.i}
{cparam2.i}

FUNCTION fUpdateMaintBreak RETURNS INTEGER
   (idFrom AS DECIMAL,
    idTo   AS DECIMAL,
    iiMaintB AS DECIMAL):
   
   FIND TMSParam EXCLUSIVE-LOCK WHERE
        TMSParam.Brand      = gcBrand      AND
        TMSParam.ParamGroup = "sogrequest" AND
        TMSParam.ParamCode  = "MaintBreakFrom".
    
   TMSParam.Decval = idFrom. 
   RELEASE TMSParam.

   FIND TMSParam EXCLUSIVE-LOCK WHERE
        TMSParam.Brand      = gcBrand      AND
        TMSParam.ParamGroup = "sogrequest" AND
        TMSParam.ParamCode  = "MaintBreakTo".

   TMSParam.Decval = idTo.
   RELEASE TMSParam.
   
   FIND TMSParam EXCLUSIVE-LOCK WHERE
        TMSParam.Brand = gcBrand AND
        TMSParam.ParamGroup = "ServiceBreak" AND
        TMSParam.ParamCode  = "Activation".
   ASSIGN TMSParam.CharVal = STRING(iiMaintB).

   RELEASE TMSParam.

END FUNCTION.

PROCEDURE pMailMaintBreak:
   
   DEF INPUT PARAM icMessage AS CHAR NO-UNDO.
   
   DEFINE VARIABLE lcConfDir AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcReportFile AS CHARACTER NO-UNDO. 

   lcConfDir = fCParamC("RepConfDir").
   lcReportFile = fCParamC("ProvisionMessage").
   
   OUTPUT TO VALUE(lcReportFile).
   PUT UNFORMATTED 
       icMessage.
   OUTPUT CLOSE.    

   /* mail recipients */
   GetRecipients(lcConfDir + "provmaint.email").
   /* send via mail */
   SendMail(lcReportFile,"").
  
END PROCEDURE.
