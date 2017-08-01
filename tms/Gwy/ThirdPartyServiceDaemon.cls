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

CLASS Gwy.ThirdPartyServiceDaemon INHERITS gearbox.daemons.Daemon:  

   DEF VAR liLoop AS INT NO-UNDO. 
   
   CONSTRUCTOR PUBLIC ThirdPartyServiceDaemon( ):

      IF LOG-MANAGER:LOGGING-LEVEL < 2 AND LOG-MANAGER:LOGFILE-NAME <> ? THEN 
         LOG-MANAGER:WRITE-MESSAGE("Daemon started", "INFO").

   END CONSTRUCTOR.     


   METHOD OVERRIDE PUBLIC VOID setUp ():
      lHeartbeatRate = 10000.
   END METHOD.


   METHOD OVERRIDE PUBLIC VOID shutDown ():
   END METHOD.
   

   DESTRUCTOR PUBLIC ThirdPartyServiceDaemon( ):

      shutDown().

      IF LOG-MANAGER:LOGGING-LEVEL < 2 AND LOG-MANAGER:LOGFILE-NAME <> ? THEN 
         LOG-MANAGER:WRITE-MESSAGE("Daemon stopped", "INFO").
                 
   END DESTRUCTOR.   

   METHOD PUBLIC OVERRIDE LOGICAL processOne():

      DEF VAR liLoop        AS INT  NO-UNDO. 
      DEF VAR ldeNow        AS DEC  NO-UNDO. 
      DEF VAR lcReturnValue AS CHAR NO-UNDO. 
      
      IF Syst.Parameters:geti("OnlineEnabled","Accessory") EQ 0 THEN 
      DO:
         IF LOG-MANAGER:LOGFILE-NAME <> ? THEN 
             LOG-MANAGER:WRITE-MESSAGE("Sending is disabled (TMS parameter AccessoryActivation->OnlineEnabled=0)").

         pauseUntilHeartbeat(10).

         RETURN FALSE.
      END.

      ldeNow = Class.timedate:MakeTS().

      IF CAN-FIND(FIRST TPServiceMessage WHERE TPServiceMessage.Source      EQ {&SOURCE_TMS}             AND
                                               TPServiceMessage.Status      EQ {&WAITING_FOR_VENDOR_ACTIVATION} AND
                                               TPServiceMessage.MessageType EQ {&ACTIVATION}             NO-LOCK) THEN
          RUN Gwy/tp_service_activation.p.
      ELSE IF CAN-FIND(FIRST FusionMessage WHERE TPServiceMessage.Source        EQ {&SOURCE_TMS}               AND
                                                 TPServiceMessage.MessageStatus EQ {&WAITING_FOR_VENDOR_DEACTIVATION} AND
                                                 TPServiceMessage.MessageType   EQ {&DEACTIVATION}             NO-LOCK) THEN    
          RUN Gwy/tp_service_deactivation.p.

      pauseUntilHeartbeat(5).

      RETURN FALSE.

   END METHOD.

END CLASS.
