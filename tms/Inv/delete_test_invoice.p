/* ----------------------------------------------------------------------
  MODULE .......: delete_test_Invoice.p
  TASK .........: Delete type 99 Invoices
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 22.06.10
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/funcrunprocess_update.i}

DEF INPUT  PARAMETER icFromExtInvID   AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icToExtInvID     AS CHAR NO-UNDO.
DEF INPUT  PARAMETER iiFRProcessID    AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiUpdateInterval AS INT  NO-UNDO.
DEF INPUT  PARAMETER icRunMode        AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiHandled        AS INT  NO-UNDO.


FOR EACH Invoice NO-LOCK WHERE 
         Invoice.Brand   = gcBrand AND
         Invoice.InvType = 99:
      
   IF icFromExtInvID > "" AND Invoice.ExtInvID < icFromExtInvID THEN NEXT.
   IF icToExtInvID > "" AND Invoice.ExtInvID > icToExtInvID  THEN NEXT.  
      
   RUN Inv/del_inv.p(Invoice.InvNum).
   
   IF RETURN-VALUE BEGINS "ERROR" THEN RETURN RETURN-VALUE.
   
   oiHandled = oiHandled + 1.
 
   IF iiUpdateInterval > 0 AND oiHandled MOD iiUpdateInterval = 0 THEN DO:
      IF NOT fUpdateFuncRunProgress(iiFRProcessID,oiHandled) THEN
         RETURN "ERROR:Stopped".
   END.   
    
END.

RETURN "".



