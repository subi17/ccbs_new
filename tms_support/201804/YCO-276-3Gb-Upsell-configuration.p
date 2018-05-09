/* YCO-276-3Gb-Upsell-configuration.p 
   Purpose: create a total of 9 parameters 
*/
DEF VAR dfrom    AS DECIMAL FORMAT "99999999.99" NO-UNDO.
DEF VAR dto      AS DECIMAL FORMAT "99999999.99" NO-UNDO.
DEF VAR lSuccess AS LOGICAL NO-UNDO.

{Syst/commpaa.i}
Syst.Var:gcBrand = "1".

ASSIGN
   dfrom = 20180501
   dto   = 20491231.

FORM 
  SKIP
  "This program will create FROM and TO interval for upsells for YCO-276: 3Gb" SKIP(1)
  "with codes FID3GB_3m_R_UPSELL,FID3GB_6m_R_UPSELL and FID3GB_12m_R_UPSELL" SKIP
  "from a MONTHLY RENEW UPSELL control point of view" 
  SKIP(2)
  "Monthly Renew available from: " dfrom SKIP
  "Monthly Renew available to  : " dto skip(2)
  "and the paths for the logs of each of them" SKIP
  WITH OVERLAY CENTERED ROW 6 TITLE " Add parameters for YCO-276 " NO-LABELS
  FRAME f-yco276.

UPDATE dfrom
       dto WITH FRAME f-yco276.

MESSAGE "Are you sure you want to proceed with the process?" VIEW-AS ALERT-BOX QUESTION
   BUTTONS YES-NO UPDATE lGo AS LOGICAL.
   
IF lGo = FALSE THEN
DO:
   MESSAGE "Process cancelled by user" VIEW-AS ALERT-BOX INFORMATION.
   RETURN.
END.
     
blk-upsell:
DO TRANSACTION ON ERROR UNDO blk-upsell, LEAVE blk-upsell
               ON STOP  UNDO blk-upsell, LEAVE blk-upsell:
                  
  lsuccess = FALSE.

  /* Check if the program has already being executed */
  FIND FIRST TMSParam WHERE TMSParam.Brand     = Syst.Var:gcBrand 
                        AND TMSParam.ParamCode = "YCO-276-FID3GB_3m-FromDate" 
                      NO-LOCK NO-ERROR. 
  IF AVAILABLE daycampaign THEN
  DO:
     MESSAGE "YCO-276 Upsell 3Gb 'parameters' creation program has already being executed"
       VIEW-AS ALERT-BOX.
     RETURN.
  END.
  
  /* Creating parameters */
  CREATE TMSParam. 
  ASSIGN TMSParam.Brand      = Syst.Var:gcBrand
         TMSParam.Paramgroup = "YCO-276" 
         TMSParam.ParamCode  = "YCO-276-FID3GB_3m-FromDate"
         TMSParam.ParamName  = "3Gb 3m upsell YCO-276 promotion start" 
         TMSParam.ParamType  = "DE"  
         TMSParam.DecVal     = dfrom. 
  RELEASE TMSParam.
  CREATE TMSParam. 
  ASSIGN TMSParam.Brand      = Syst.Var:gcBrand 
         TMSParam.Paramgroup = "YCO-276" 
         TMSParam.ParamCode  = "YCO-276-FID3GB_3m-ToDate"
         TMSParam.ParamName  = "3Gb 3m upsell YCO-276 promotion end" 
         TMSParam.ParamType  = "DE"  
         TMSParam.DecVal     = dto. 
  RELEASE TMSParam.
  
  CREATE TMSParam. 
  ASSIGN TMSParam.Brand      = Syst.Var:gcBrand 
         TMSParam.Paramgroup = "YCO-276" 
         TMSParam.ParamCode  = "YCO-276-FID3GB_6m-FromDate"
         TMSParam.ParamName  = "3Gb 6m upsell YCO-276 promotion start" 
         TMSParam.ParamType  = "DE"  
         TMSParam.DecVal     = dfrom. 
  RELEASE TMSParam.
  CREATE TMSParam. 
  ASSIGN TMSParam.Brand      = Syst.Var:gcBrand 
         TMSParam.Paramgroup = "YCO-276" 
         TMSParam.ParamCode  = "YCO-276-FID3GB_6m-ToDate"
         TMSParam.ParamName  = "3Gb 6m upsell YCO-276 promotion end" 
         TMSParam.ParamType  = "DE"  
         TMSParam.DecVal     = dto. 
  RELEASE TMSParam.
         
  CREATE TMSParam. 
  ASSIGN TMSParam.Brand      = Syst.Var:gcBrand 
         TMSParam.Paramgroup = "YCO-276" 
         TMSParam.ParamCode  = "YCO-276-FID3GB_12m-FromDate"
         TMSParam.ParamName  = "3Gb 12m upsell YCO-276 promotion start" 
         TMSParam.ParamType  = "DE"  
         TMSParam.DecVal     = dfrom. 
  RELEASE TMSParam.
  CREATE TMSParam. 
  ASSIGN TMSParam.Brand      = Syst.Var:gcBrand 
         TMSParam.Paramgroup = "YCO-276" 
         TMSParam.ParamCode  = "YCO-276-FID3GB_12m-ToDate"
         TMSParam.ParamName  = "3Gb 12m upsell YCO-276 promotion end" 
         TMSParam.ParamType  = "DE"  
         TMSParam.DecVal     = dto. 
  RELEASE TMSParam.
  
  CREATE TMSParam. 
  ASSIGN TMSParam.Brand      = Syst.Var:gcBrand
         TMSParam.Paramgroup = "YCO-276-Dir" 
         TMSParam.ParamCode  = "YCO-276-FID3GB_Xm-Logs"
         TMSParam.ParamName  = "Directory for YCO-276 3Gb upsells log" 
         TMSParam.ParamType  = "C"  
         TMSParam.CharVal     = "/store/riftp/upsells/outgoing/logs/". 
  RELEASE TMSParam.
 
  /* Process OK */
  lSuccess = TRUE.
END.

IF lSuccess THEN
    MESSAGE "Parameters successfully created" VIEW-AS ALERT-BOX.
ELSE 
    MESSAGE "ERROR: Failed to create the upsells" VIEW-AS ALERT-BOX.

