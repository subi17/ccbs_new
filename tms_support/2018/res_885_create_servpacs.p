/*
   RES-885 National roaming traffic restrictions.
   Script for creating default service package.

*/


{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Func/cparam2.i}
{Syst/eventlog.i}
{Func/ftransdir.i}

DEF VAR liCTServEl AS INT NO-UNDO.

DEFINE BUFFER bCTServPac  FOR CTServPac.
DEFINE BUFFER bCTServEl   FOR CTServEl.
DEFINE BUFFER bCTServAttr FOR CTServAttr.


FOR EACH CLIType WHERE 
         CLIType.Brand = Syst.Var:gcBrand  AND 
         CLIType.CLIType BEGINS "CONT"  NO-LOCK:
   
   FOR EACH CTServPac WHERE 
            CTServPac.Brand   = Syst.Var:gcBrand         AND 
            CTServPac.CLIType = CLIType.CLIType NO-LOCK:

      CREATE bCTServPac.
      BUFFER-COPY CTServPac EXCEPT CTServPac.CLIType 
                                   CTServPac.FromDate
                                   TO bCTServPac.
      ASSIGN bCTServPac.CLIType  = CLIType.CLIType
             bCTServPac.FromDate = TODAY NO-ERROR.
                
      IF ERROR-STATUS:ERROR THEN DO:
         MESSAGE "ERROR: Creating CTServPac" VIEW-AS ALERT-BOX.
         RETURN "ERROR: Creating CTServPac".
      END.

      FOR EACH CTServEl WHERE
               CTServEl.Brand   = Syst.Var:gcBrand  AND 
               CTServEl.CLIType = CTServPac.CLIType AND 
               CTServEl.ServPac = CTServPac.ServPac NO-LOCK:
         CREATE bCTServEl.
         BUFFER-COPY CTServEl EXCEPT CTServEl.CTServEl
                                     CTServEl.CLIType
                                     CTServEl.FromDate
                                     TO bCTServEl.
         ASSIGN bCTServEl.CTServEl = NEXT-VALUE(CTServEl)
                bCTServEl.CLIType  = CLIType.CliType
                bCTServEl.FromDate = TODAY 
                liCTServEl         = bCTServEl.CTServEl NO-ERROR.                                   

         IF ERROR-STATUS:ERROR THEN DO: 
            MESSAGE "ERROR: Creating CTServEl" VIEW-AS ALERT-BOX.
            RETURN "ERROR: Creating CTServEl".
         END.
               
         FIND ServCom WHERE
              ServCom.Brand   = Syst.Var:gcBrand AND
              ServCom.ServCom = CTServEl.ServCom NO-LOCK NO-ERROR.

         IF AVAILABLE ServCom AND ServCom.ServAttr = TRUE THEN    
            FOR EACH CTServAttr WHERE 
                     CTServAttr.CTServEl = CTServEl.CTServEl NO-LOCK:
               CREATE bCTServAttr.
               BUFFER-COPY CTServAttr EXCEPT CTServAttr.CTServEl
                                             CTServAttr.FromDate
                                             TO bCTServAttr.
               ASSIGN bCTServAttr.CTServEl = liCTServEl
                      bCTServAttr.FromDate = TODAY NO-ERROR. 
                  
               IF ERROR-STATUS:ERROR THEN DO: 
                  MESSAGE "ERROR: Creating CTServAttr" VIEW-AS ALERT-BOX.
                  RETURN "ERROR: Creating CTServAttr".
               END.
            END.            
      END.                     
   END.
END.
