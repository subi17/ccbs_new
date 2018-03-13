DEF VAR lcLine AS CHAR NO-UNDO.
DEF VAR liCont AS INT  NO-UNDO. 

DEF VAR lcFieldName  AS CHAR NO-UNDO.
DEF VAR lcFieldValue AS CHAR NO-UNDO.

DEF VAR lcAdditionalInfo AS CHAR NO-UNDO.

DEF VAR lcCita       AS CHAR NO-UNDO.
DEF VAR lcCanDS      AS CHAR NO-UNDO.
DEF VAR lcPortStat   AS CHAR NO-UNDO.
DEF VAR lcPortDate   AS CHAR NO-UNDO.
DEF VAR lcRouterStat AS CHAR NO-UNDO.

DEF VAR llUpdatedCita   AS LOGICAL NO-UNDO.
DEF VAR llUpdatedCancel AS LOGICAL NO-UNDO.

DEF BUFFER bFusionMessage FOR FusionMessage. 

INPUT FROM VALUE("/tmp/YPS-2467_4.txt").
OUTPUT TO VALUE("/tmp/YPS-2467_4.log").

REPEAT:
   IMPORT UNFORMATTED lcLine.
   lcLine = REPLACE (lcLine, "Y", "").
   lcLine = REPLACE (lcLine, "_MOSS", "").
   FIND FIRST Order WHERE
              Order.Brand = "1" AND
              Order.OrderId = INTEGER(lcLine) NO-LOCK NO-ERROR.
   IF NOT AVAILABLE order THEN DO:
     PUT UNFORMATTED INTEGER(lcLine) "|Order not found" SKIP.   
     NEXT. 
   END.    
   FOR EACH FusionMessage EXCLUSIVE-LOCK WHERE
            FusionMessage.orderid = order.orderid USE-INDEX OrderId BY updateTS DESC:
              
      lcAdditionalInfo = FusionMessage.additionalInfo.

      IF lcAdditionalInfo BEGINS "~{" THEN DO:
         
         llUpdatedCita =  CAN-FIND(FIRST bFusionMessage WHERE 
                                         bFusionMessage.orderid = FusionMessage.orderid AND 
                                         bFusionMessage.updateTS > FusionMessage.updateTS AND
                                         LOOKUP (bFusionMessage.FixedStatus, "CERRADA,CERRADA PARCIAL,CITADA,INCIDENCIA RED,INCIDENCIA TECNICO EN CASA") > 0  
                                   USE-INDEX OrderId).
         llUpdatedCancel = CAN-FIND(FIRST bFusionMessage WHERE 
                                          bFusionMessage.orderid = FusionMessage.orderid AND 
                                          bFusionMessage.updateTS > FusionMessage.updateTS AND
                                          LOOKUP (bFusionMessage.FixedStatus, "CANCELADA,PENDIENTE CANCELAR,CANCELACION EN PROCESO") > 0                                                 
                                          USE-INDEX OrderId).                                          
                         
         
         FIND FIRST OrderFusion EXCLUSIVE-LOCK WHERE
                    OrderFusion.Brand = "1" AND
                    OrderFusion.OrderId = FusionMessage.orderid NO-ERROR.
         IF NOT AVAILABLE OrderFusion THEN 
            NEXT.

         /* Writing a log with original values */  
         PUT UNFORMATTED "BEFORE|"                        
                         FusionMessage.MessageSeq "|" 
                         FusionMessage.OrderId "|"
                         FusionMessage.FixedStatus "|" 
                         FusionMessage.AdditionalInfo "|" 
                         OrderFusion.AppointmentDate "|"
                         OrderFusion.CancellationReason "|"
                         OrderFusion.portStat "|"
                         OrderFusion.portDate "|"
                         OrderFusion.routerStat "|"
                         SKIP.

         ASSIGN 
            lcCita      = ""
            lcCanDS     = ""
            lcPortStat  = ""
            lcPortDate  = ""
            lcRouterStat= "".
            
         lcAdditionalInfo = REPLACE(lcAdditionalInfo, '","', "|").
         lcAdditionalInfo = REPLACE(lcAdditionalInfo, '":"', "#").
         DO liCont = 1 TO NUM-ENTRIES(lcAdditionalInfo, "|"):
            lcFieldName = ENTRY(1, ENTRY(liCont, lcAdditionalInfo, '|' ), "#").
            lcFieldName = REPLACE(lcFieldName, "~{", "").
            lcFieldName = REPLACE(lcFieldName, '"', "").
            lcFieldValue = ENTRY(2, ENTRY(liCont, lcAdditionalInfo, '|' ), "#").
            lcFieldValue = REPLACE(lcFieldValue, "~}", "").
            lcFieldValue = REPLACE(lcFieldValue, '"', "").
            IF lcFieldValue = "" THEN 
              NEXT.                                    
                             
            CASE lcFieldName:
               WHEN "cita" THEN
                 ASSIGN OrderFusion.AppointmentDate = lcFieldValue WHEN (OrderFusion.AppointmentDate = "" OR NOT llUpdatedCita).
               WHEN "canDS" THEN 
                 ASSIGN OrderFusion.CancellationReason = lcFieldValue WHEN (OrderFusion.CancellationReason = "" OR NOT llUpdatedCancel).
               WHEN "portStat" THEN 
                 ASSIGN OrderFusion.portStat = lcFieldValue WHEN OrderFusion.portStat = "".
               WHEN "portDate" THEN 
                 ASSIGN OrderFusion.portDate = lcFieldValue OrderFusion.portDate = "".
               WHEN "routerStat" THEN
                  ASSIGN OrderFusion.routerStat = lcFieldValue WHEN OrderFusion.routerStat = "".       
            END.
         END.
         
         IF LOOKUP(FusionMessage.FixedStatus,"CERRADA,CERRADA PARCIAL,CITADA,INCIDENCIA RED,INCIDENCIA TECNICO EN CASA") <> 0 THEN
            ASSIGN FusionMessage.AdditionalInfo = OrderFusion.AppointmentDate WHEN OrderFusion.AppointmentDate <> "".
         IF LOOKUP(FusionMessage.FixedStatus,"CANCELADA,CANCELACION EN PROCESO") <> 0 THEN
            ASSIGN FusionMessage.AdditionalInfo = OrderFusion.CancellationReason WHEN OrderFusion.CancellationReason <> "".
         IF FusionMessage.AdditionalInfo BEGINS "~{" THEN 
            ASSIGN FusionMessage.AdditionalInfo = "".   

         /* Writing a log with original values */  
         PUT UNFORMATTED "AFTER|"
                         FusionMessage.MessageSeq "|" 
                         FusionMessage.OrderId "|"
                         FusionMessage.FixedStatus "|"
                         FusionMessage.AdditionalInfo "|" 
                         OrderFusion.AppointmentDate "|"
                         OrderFusion.CancellationReason "|"
                         OrderFusion.portStat "|"
                         OrderFusion.portDate "|"
                         OrderFusion.routerStat "|"
                         SKIP.             
 
      END.
      ELSE DO:
         PUT UNFORMATTED FusionMessage.MessageSeq "|" 
                         FusionMessage.OrderId "|" "Not incorrect format|" FusionMessage.AdditionalInfo SKIP.
      END. 
   END.
END. 
INPUT CLOSE.
OUTPUT CLOSE.