/* ----------------------------------------------------------------------
  MODULE .......: Gwy.MasmovilSender
  TASK .........: Sends online messages against Masmovil API
  APPLICATION ..: TMS
  AUTHOR .......: ansavola
  CREATED ......: 05.09.16
  Version ......: Yoigo
----------------------------------------------------------------------- */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.*.
{Syst/tmsconst.i}

CLASS Gwy.MasmovilSender INHERITS gearbox.daemons.Daemon:  

   DEF VAR liLoop AS INT NO-UNDO. 
   
   CONSTRUCTOR PUBLIC MasmovilSender( ):

      IF LOG-MANAGER:LOGGING-LEVEL < 2 AND LOG-MANAGER:LOGFILE-NAME <> ?
      THEN LOG-MANAGER:WRITE-MESSAGE("Daemon started", "INFO").

   END CONSTRUCTOR.     


   METHOD OVERRIDE PUBLIC VOID setUp ():
      lHeartbeatRate = 10000.
   END METHOD.


   METHOD OVERRIDE PUBLIC VOID shutDown ():
   END METHOD.
   

   DESTRUCTOR PUBLIC MasmovilSender( ):

      shutDown().

      IF LOG-MANAGER:LOGGING-LEVEL < 2 AND LOG-MANAGER:LOGFILE-NAME <> ?
      THEN LOG-MANAGER:WRITE-MESSAGE("Daemon stopped", "INFO").
                 
   END DESTRUCTOR.   

   METHOD PUBLIC OVERRIDE LOGICAL processOne():
      
      DEF BUFFER bFusionMessage FOR FusionMessage.

      DEF VAR liLoop AS INT NO-UNDO. 
      DEF VAR ldeNow AS DEC NO-UNDO. 
      DEF VAR lcReturnValue AS CHAR NO-UNDO. 
      
      IF Syst.Parameters:geti("OnlineEnabled","Masmovil") EQ 0 THEN DO:
         IF LOG-MANAGER:LOGFILE-NAME <> ?
            THEN LOG-MANAGER:WRITE-MESSAGE(
               "Sending is disabled (TMS parameter Masmovil->OnlineEnabled=0)").
         pauseUntilHeartbeat(10).
         RETURN FALSE.
      END.

      ldeNow = Class.timedate:MakeTS().

      MESSAGE_LOOP:
      FOR EACH FusionMessage NO-LOCK WHERE
               FusionMessage.Source EQ {&FUSIONMESSAGE_SOURCE_TMS} AND
               FusionMessage.MessageStatus EQ {&FUSIONMESSAGE_STATUS_NEW}
            BY FusionMessage.CreatedTS 
            liLoop = 1 TO 10:

         IF FusionMessage.CreatedTS > ldeNow THEN LEAVE.

         FIND bFusionMessage EXCLUSIVE-LOCK WHERE
              ROWID(bFusionMessage) = ROWID(FusionMessage) NO-WAIT NO-ERROR.
         IF LOCKED(bFusionMessage) THEN NEXT.

         lcReturnValue = "".

         CASE bFusionMessage.MessageType:

            WHEN {&FUSIONMESSAGE_TYPE_RESERVE_NUMBER} THEN DO:
               RUN Gwy/masmovil_reserve_number.p(FusionMessage.MessageSeq).
               lcReturnValue = RETURN-VALUE.
            END.
            
            WHEN {&FUSIONMESSAGE_TYPE_CREATE_ORDER} THEN DO:
               RUN Gwy/masmovil_create_order.p(FusionMessage.MessageSeq).
               lcReturnValue = RETURN-VALUE.
            END.
            
            WHEN {&FUSIONMESSAGE_TYPE_CANCEL_ORDER} THEN DO:
               RUN Gwy/masmovil_cancel_order.p(FusionMessage.MessageSeq). 
               lcReturnValue = RETURN-VALUE.
            END.
            
            WHEN {&FUSIONMESSAGE_TYPE_ADDRESS_CHANGE} THEN DO:
               RUN Gwy/masmovil_address_change.p(FusionMessage.MessageSeq). 
               lcReturnValue = RETURN-VALUE.
            END.

            WHEN {&FUSIONMESSAGE_TYPE_CANCEL_APPOINTMENT} THEN DO:
               RUN Gwy/masmovil_cancel_appointment.p(FusionMessage.MessageSeq).
               lcReturnValue = RETURN-VALUE.
            END.

            OTHERWISE DO:
               IF LOG-MANAGER:LOGFILE-NAME <> ?
               THEN LOG-MANAGER:WRITE-MESSAGE(
                  SUBST("OrderID: &1, MsgSeq: &2, Invalid message type: &3",
			               bFusionMessage.OrderID,
			               bFusionMessage.MessageSeq,
                        bFusionMessage.MessageType, "ERROR")).

               ASSIGN
                  bFusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_ERROR}
                  bFusionMessage.AdditionalInfo = "Invalid message type".

               RELEASE bFusionMessage.

               NEXT MESSAGE_LOOP.

            END.
         END.

         IF LOG-MANAGER:LOGFILE-NAME <> ?
         THEN LOG-MANAGER:WRITE-MESSAGE(
            SUBST("OrderID: &1, MsgSeq: &2, message type: &3: &4",
                  bFusionMessage.OrderID,
                  bFusionMessage.MessageSeq,
                  bFusionMessage.MessageType,
                  lcReturnValue)).
         
         IF lcReturnValue BEGINS "RETRY" THEN DO:
            
            CREATE bFusionMessage.
            ASSIGN
               bFusionMessage.MessageSeq = NEXT-VALUE(FusionMessageSeq)
               bFusionMessage.Source = FusionMessage.Source
               bFusionMessage.OrderID = FusionMessage.OrderID
               bFusionMessage.MsSeq = FusionMessage.MsSeq
               bFusionMessage.MessageType = FusionMessage.MessageType
               bFusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_NEW}
               bFusionMessage.CreatedTS = Class.timedate:OffSetTS(1)
               bFusionMessage.UpdateTS = bFusionMessage.CreatedTS.
               
            RELEASE bFusionMessage.

            LEAVE MESSAGE_LOOP.
         END.

      END.

      pauseUntilHeartbeat(5).

      RETURN FALSE.

   END METHOD.

END CLASS.
