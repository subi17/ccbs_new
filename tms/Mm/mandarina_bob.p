/* ----------------------------------------------------------------------
  MODULE .......: mandarina_bob.p
  TASK .........: BOB / Read input file; send commands to Procera to 
                  set or remove a redirection to LP. Create logs files.
  APPLICATION ..: tms
  AUTHOR .......: jotorres & ilsavola
  CREATED ......: 08/2017
  Version ......: yoigo
----------------------------------------------------------------------
10/01/2018 ashok YDR-2754 activate/deactivate Cust_Lost barring
---------------------------------------------------------------------- */

/*---------------------------------------------------------------------- 
https://kethor.qvantel.com/browse/MANDLP-8
2.1 Yoigo employee can upload a file to bob tool to set or 
    remove a redirection to a LP
---------------------------------------------------------------------- */

/*---------------------------------------------------------------------- 
Modificated on 10/2017. New requeriments in YDR-2668 to
activate/deacivate Internet barring.
---------------------------------------------------------------------- */

/* Parameters */
DEF VAR lcProcessMode AS CHAR NO-UNDO. /* ["massive"|"priority"] */

lcProcessMode = SESSION:PARAMETER.

/* includes */
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".

{Syst/tmsconst.i}
{Func/cparam2.i}
{Func/lpfunctions.i}
{Func/ftransdir.i}
{Func/barrfunc.i}

/* Directories */
DEF VAR lcSpoolDirectory     AS CHAR NO-UNDO INITIAL "/tmp/". /* /tmp/mnt/store/riftp/mandarina/spool/     */
DEF VAR lcIncomingDirectory  AS CHAR NO-UNDO INITIAL "/tmp/". /* /tmp/mnt/store/riftp/mandarina/incoming/  */  
DEF VAR lcOutgoingDirectory  AS CHAR NO-UNDO INITIAL "/tmp/". /* /tmp/mnt/store/riftp/mandarina/outgoing/  */
DEF VAR lcProcessedDirectory AS CHAR NO-UNDO INITIAL "/tmp/". /* /tmp/mnt/store/riftp/mandarina/processed/ */
DEF VAR lcLogsDirectory      AS CHAR NO-UNDO INITIAL "/tmp/". /* /tmp/mnt/store/riftp/mandarina/logs/      */

/* Network delay */
DEF VAR liDelayNW     AS INTEGER NO-UNDO INITIAL 5.  /* Delay for network, in seconds */
DEF VAR liNumItemsNW  AS INTEGER NO-UNDO INITIAL 20. /* Items in batch for network */
DEF VAR liContItems   AS INTEGER NO-UNDO.    

/* Input file fields */
DEF VAR lcMSISDN AS CHAR NO-UNDO. /* MSISDN */
DEF VAR lcLP     AS CHAR NO-UNDO. /* ["Mandarina1"|"Mandarina2"|"InternetBarring"] */
DEF VAR lcAction AS CHAR NO-UNDO. /* ["on"|"off"] */

/* mandarina_bob status */ 
DEF VAR lcTableName     AS CHAR NO-UNDO.
DEF VAR lcActionID      AS CHAR NO-UNDO.
DEF VAR ldCurrentTimeTS AS DEC  NO-UNDO.
 
/* Streams */
DEF STREAM sFilesInDir.  /* Files in directory */ 
DEF STREAM sCurrentFile. /* Current processing file */
DEF STREAM sCurrentLog.  /* Log file for current processing file */
DEF STREAM sMandaLog.    /* Log file for mandarina_bob.p executions */

DEF VAR lcFileName        AS CHAR NO-UNDO. /* Files in directory */
DEF VAR lcCurrentFile     AS CHAR NO-UNDO. /* Current processing file */
DEF VAR lcCurrentLog      AS CHAR NO-UNDO. /* log for current processing file */
DEF VAR lcLine            AS CHAR NO-UNDO. /* Read line of the current file. */
DEF VAR lcMandarinaBobLog AS CHAR NO-UNDO. /* Log file for Mandarina Bob Tool executions */

DEF VAR lcErr      AS CHAR    NO-UNDO.
DEF VAR llSuccess  AS LOGICAL NO-UNDO.
DEF VAR lcBarrings AS CHAR    NO-UNDO.

/* Getting directories from CParams */
ASSIGN
   lcSpoolDirectory     = fCParamC("MandarinaSpoolDir")
   lcIncomingDirectory  = fCParamC("MandarinaIncomingDir")
   lcOutgoingDirectory  = fCParamC("MandarinaOutgoingDir")
   lcProcessedDirectory = fCParamC("MandarinaProcessedDir")
   lcLogsDirectory      = fCParamC("MandarinaLogsDir") NO-ERROR.

/* Getting NetWorks parameters from CParams */
ASSIGN
   liDelayNW    = fCParamI("MandarinaNetWorkDelay")
   liNumItemsNW = fCParamI("MandarinaNetWorkBatchItems") NO-ERROR.   

/* Log file for mandarina executions */
lcMandarinaBobLog = lcLogsDirectory + 
                    STRING(YEAR(TODAY), "9999") + 
                    STRING(MONTH(TODAY), "99" ) +
                    STRING(DAY(TODAY), "99") + 
                    "_mandarina_bob.log".                     

OUTPUT STREAM sMandaLog TO VALUE(lcMandarinaBobLog) APPEND.
PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";mandarina_bob_starts (" + lcProcessMode + ")" SKIP.

/* Verify input parameter */
IF lcProcessMode <> "massive" AND lcProcessMode <> "priority" THEN DO:
   PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";incorrect_input_parameter" SKIP.
   PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";mandarina_bob_finishes" SKIP.
   PUT STREAM sMandaLog UNFORMATTED "-------------------------------" SKIP.
   OUTPUT STREAM sMandaLog CLOSE.
   QUIT.
END.
  
/* Check if other mandarina_bob is running */
ASSIGN 
   lcTableName = "MANDARINA"
   lcActionID  = "file_reading_" + lcProcessMode
   ldCurrentTimeTS = Func.Common:mMakeTS(). 
 
DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  Syst.Var:gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName NO-ERROR.

   IF AVAIL ActionLog AND
      ActionLog.ActionStatus EQ {&ACTIONLOG_STATUS_PROCESSING} THEN DO:
      PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";another_mandarina_bob_running" SKIP.
      PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";mandarina_bob_finishing" SKIP.
      OUTPUT STREAM sMandaLog CLOSE.
      QUIT.
   END.

   IF NOT AVAIL ActionLog THEN DO:
      /*First execution stamp*/
      CREATE ActionLog.
      ASSIGN
         ActionLog.Brand        = Syst.Var:gcBrand
         ActionLog.TableName    = lcTableName
         ActionLog.ActionID     = lcActionID
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}
         ActionLog.UserCode     = Syst.Var:katun
         ActionLog.ActionTS     = ldCurrentTimeTS.
      RELEASE ActionLog.
      PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";mandarina_bob_first_run" SKIP.
      PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";mandarina_bob_finishes" SKIP.      
      OUTPUT STREAM sMandaLog CLOSE.
      QUIT. /*No reporting in first time.*/
   END.
   ELSE DO:
      ASSIGN
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_PROCESSING}
         ActionLog.UserCode     = Syst.Var:katun
         ActionLog.ActionTS     = ldCurrentTimeTS.
      RELEASE Actionlog.
   END.
END.

/* Processing files in incoming directory */
INPUT STREAM sFilesInDir THROUGH VALUE("ls -1tr " + lcInComingDirectory).
REPEAT:

   IMPORT STREAM sFilesInDir UNFORMATTED lcFileName.
   /* Only process the correct files */
   IF NOT (lcFileName BEGINS ("LP" + lcProcessMode)) THEN
     NEXT.
   lcCurrentFile = lcInComingDirectory + lcFileName.
   lcCurrentLog = lcLogsDirectory + lcFileName + ".log". 
   IF SEARCH(lcCurrentFile) NE ? THEN DO:
      INPUT  STREAM sCurrentFile FROM VALUE(lcCurrentFile).
      OUTPUT STREAM sCurrentLog TO VALUE(lcCurrentLog).
   END.
   ELSE 
      NEXT. 

   PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";" + lcCurrentFile + ";start_processing_file" SKIP.
   REPEAT:
      IMPORT STREAM sCurrentFile UNFORMATTED lcLine.
      IF NUM-ENTRIES(lcLine, ";") <> 3 THEN DO:
         PUT STREAM sCurrentLog UNFORMATTED
            lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";ERROR:incorrect_number_of_fields" SKIP.
         NEXT.  
      END.
      ASSIGN
         lcMSISDN = ENTRY(1, lcLine, ";")
         lcLP     = ENTRY(2, lcLine, ";")
         lcAction = ENTRY(3, lcLine, ";").      

      /* Check subscription */     
      FIND FIRST mobsub WHERE
                 mobsub.Brand EQ Syst.Var:gcBrand AND
                 mobsub.CLI   EQ lcMSISDN 
           USE-INDEX CLI NO-LOCK NO-ERROR.
      IF NOT AVAILABLE mobsub THEN DO:
         PUT STREAM sCurrentLog UNFORMATTED
            lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";ERROR:MSISDN_not_found" SKIP.
         NEXT.
      END.

      /* Check DEBT barring status */
      lcBarrings = Func.BarrMethod:mGetActiveBarrings(mobsub.MsSeq).
      IF lcBarrings <> "" THEN DO:
         IF LOOKUP("DEBT_LP", lcBarrings) <> 0 OR LOOKUP("DEBT_HOTLP", lcBarrings) <> 0 THEN DO:
            PUT STREAM sCurrentLog UNFORMATTED
               lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";WARNING:DEBT_barring_active" SKIP.
            NEXT.
         END.
      END. 

      /* Checking values */
      IF LOOKUP( lcLP , "Mandarina1,Mandarina2,InternetBarring,Cust_LOST") = 0 THEN DO:
         PUT STREAM sCurrentLog UNFORMATTED
            lcLine + ";" + STRING(TIME,"hh:mm:ss") 
                   + ";ERROR:incorrect_command(Mandarina1/Mandarina2/InternetBarring/Cust_LOST)" SKIP.
         NEXT.
      END.
      IF (lcAction <> "on" AND lcAction <> "off") THEN DO:
         PUT STREAM sCurrentLog UNFORMATTED
            lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";ERROR:incorrect_action(on/off)" SKIP.
         NEXT.
      END.
      lcErr = "".
      IF lcAction EQ "on" THEN DO:

         /* YBU-6046: Removed "Recently ICC Change" checking.  */
         /* "Recently ICC Change" mustn't to be check anymore. */ 
         
         /* Begin YDR-2668 */
         IF LcLP EQ "InternetBarring" THEN DO:
            IF LOOKUP("Internet", lcBarrings) <> 0 THEN DO:
               PUT STREAM sCurrentLog UNFORMATTED
                  lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";WARNING:Previous_Internet_barring_active" SKIP.
               NEXT.
            END.    
            RUN pSetInternetBarring("ON").
            IF RETURN-VALUE <> "OK" THEN DO:
               PUT STREAM sCurrentLog UNFORMATTED
                  lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";" + RETURN-VALUE SKIP.
               NEXT.
            END.
         END.
         /* end YDR-2668 */
         /* Begin YDR-2754 */
         ELSE IF lcLP EQ "Cust_LOST" THEN DO:
            IF LOOKUP("Cust_LOST", lcBarrings) <> 0 THEN DO:
               PUT STREAM sCurrentLog UNFORMATTED
                  lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";WARNING:Previous_Cust_LOST_barring_active" SKIP.
                  NEXT.
            END.
            ELSE DO:     
                RUN pSetCust_LOST("ON").
                IF RETURN-VALUE <> "OK" THEN DO:
                   PUT STREAM sCurrentLog UNFORMATTED
                      lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";" + RETURN-VALUE SKIP.
                   NEXT.
                END.
            END.
         END.
         /* End YDR-2754 */
         ELSE DO:
            llSuccess = fMakeLPCommandRequest (INPUT mobsub.MsSeq,                            /*Subscription identifier*/
                                               INPUT (IF LcLP EQ "Mandarina1"                  /*LP command to network*/
                                                      THEN "REDIRECTION_OTFAILED1" 
                                                      ELSE "REDIRECTION_OTFAILED2"),         
                                               INPUT mobsub.CustNum,                          /*Customer number for memo*/
                                               INPUT (IF LcLP EQ "Mandarina1"                  /*Memo title*/ 
                                                      THEN "LP GDPR Activada"
                                                      ELSE "LP GDPR Activada"),  
                                               INPUT  (IF LcLP EQ "Mandarina1"                 /*Memo text*/ 
                                                      THEN "Activada la LP de GDPR"
                                                      ELSE "Activada la LP de GDPR"),       
                                               INPUT "Sistema",                               /*Creator tag for memo*/
                                               INPUT "11",                                    /*Source, 11 -> Bob Tool*/ 
                                               INPUT-OUTPUT lcErr).                           /*Request creation info*/
            IF (NOT llSuccess) THEN DO:
               PUT STREAM sCurrentLog UNFORMATTED
                  lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";ERROR:" + STRING(mobsub.MsSeq) + "_COMMAND_REQUEST_" + lcErr SKIP.
               NEXT.
            END.
         END.
      END.  
      ELSE DO: /* lcAction EQ "off" */
         /* Begin YDR-2668 */
         IF LcLP EQ "InternetBarring" THEN DO:
            IF LOOKUP("Internet", lcBarrings) = 0 OR 
               NOT CAN-FIND(FIRST Memo WHERE
                                  Memo.Brand EQ Syst.Var:gcBrand AND
                                  Memo.CustNum EQ MobSub.CustNum AND
                                  Memo.HostTable EQ "MobSub" AND
                                  Memo.MemoTitle EQ "OTA Barring activado"
                                 USE-INDEX CustNum)
            THEN DO:
               PUT STREAM sCurrentLog UNFORMATTED
                  lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";WARNING:No_previous_internet_barring_by_Mandarina" SKIP.
               NEXT.
            END.  
            RUN pSetInternetBarring("OFF").
            IF RETURN-VALUE <> "OK" THEN DO:
               PUT STREAM sCurrentLog UNFORMATTED
                  lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";" + RETURN-VALUE SKIP.
               NEXT.
            END.    
         END.
         /* end YDR-2668 */
         /* Begin YDR-2754 */
         ELSE IF lcLP EQ "Cust_LOST" THEN DO:
            IF LOOKUP("Cust_LOST", lcBarrings) = 0 OR 
               fGetBarringRequestSource(MobSub.MsSeq , "Cust_LOST" , "ACTIVE") NE {&REQUEST_SOURCE_YOIGO_TOOL}  THEN DO:
               PUT STREAM sCurrentLog UNFORMATTED
                  lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";WARNING:No_Previous_Cust_LOST_barring_by_Mandarina" SKIP.
                  NEXT.
            END.
            ELSE DO:
                RUN pSetCust_LOST("OFF").
                IF RETURN-VALUE <> "OK" THEN DO:
                   PUT STREAM sCurrentLog UNFORMATTED
                      lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";" + RETURN-VALUE SKIP.
                   NEXT.
                END.
            END.
         END.
         /* End YDR-2754 */
         ELSE DO:
            llSuccess = fMakeLPCommandRequest (INPUT mobsub.MsSeq,                                /*Subscription identifier*/
                                               INPUT "remove",         
                                               INPUT mobsub.CustNum,                              /*Customer number for memo*/
                                               INPUT (IF LcLP EQ "Mandarina1"                      /*Memo title*/ 
                                                      THEN "LP GDPR Desactivada"
                                                      ELSE "LP GDPR Desactivada"),  
                                               INPUT  (IF LcLP EQ "Mandarina1"                     /*Memo text*/ 
                                                      THEN "Desactivada la LP de GDPR"
                                                      ELSE "Desactivada la LP de GDPR"),       
                                               INPUT "Sistema",                                   /*Creator tag for memo*/
                                               INPUT "11",                                        /*Source, 11 -> Bob Tool*/ 
                                               INPUT-OUTPUT lcErr).                               /*Request creation info*/
   
            IF NOT llSuccess THEN DO:
               PUT STREAM sCurrentLog UNFORMATTED
                  lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";ERROR:" + STRING(mobsub.MsSeq) + "_COMMAND_REQUEST_" + lcErr SKIP.
               NEXT.
            END. 
         END.
      END.
           
      PUT STREAM sCurrentLog UNFORMATTED
        lcLine + ";" + STRING(TIME,"hh:mm:ss") + ";OK" SKIP.

      /* Network delay */
      liContItems = liContItems + 1.
      IF liContItems EQ liNumItemsNW THEN DO:
         PAUSE liDelayNW NO-MESSAGE.
         liContItems = 0.
      END.

   END.
   INPUT STREAM sCurrentFile CLOSE.
   OUTPUT STREAM sCurrentLog CLOSE.
   fMove2TransDir(lcCurrentFile, "", lcProcessedDirectory).
   fMove2TransDir(lcCurrentLog, "", lcOutgoingDirectory).
   PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";" + lcCurrentFile + ";finish_processing_file" SKIP.

END. 
INPUT STREAM sFilesInDir CLOSE.

DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  Syst.Var:gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName    AND
              ActionLog.ActionStatus NE  {&ACTIONLOG_STATUS_SUCCESS}
   EXCLUSIVE-LOCK NO-ERROR.

   IF AVAIL ActionLog THEN DO:
      ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}.
   END.
   RELEASE ActionLog.
END.

PUT STREAM sMandaLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";mandarina_bob_finishes" SKIP.
PUT STREAM sMandaLog UNFORMATTED "-------------------------------" SKIP.
OUTPUT STREAM sMandaLog CLOSE.

/*-------------------------------------------------
 Internal Procedures
-------------------------------------------------*/

PROCEDURE pSetInternetBarring:

   DEF INPUT PARAMETER lcMode AS CHAR NO-UNDO. /* "ON" to active barring ; "OFF" to remove barring. */

   DEF VAR liRequest AS INT  NO-UNDO.
   DEF VAR lcResult  AS CHAR NO-UNDO.   

   RUN Mm/barrengine.p(mobsub.MsSeq,
                       (IF lcMode EQ "ON" THEN "Internet=1" 
                                          ELSE "Internet=0"), /* Barring */
                       "11",                /* source   */
                       "Sistema",           /* creator  */
                       Func.Common:mMakeTS(),           /* activate */
                       "",                  /* SMS      */
                       OUTPUT lcResult).

   liRequest = INTEGER(lcResult) NO-ERROR. 
                               
   IF liRequest > 0 THEN DO:     

   Func.Common:mWriteMemoWithType("Mobsub",
                     STRING(mobsub.MsSeq),
                     mobsub.CustNum,
                     (IF lcMode EQ "ON" THEN "OTA Barring activado" 
                                        ELSE "OTA Barring desactivado"),                       /* memo title */
                     (IF lcMode EQ "ON" THEN "Internet barring activado por fallo campaña OTA" 
                                        ELSE "Internet barring por fallo OTA desactivado"),    /* memo text  */
                     "Service",                                                                /* memo type  */   
                     "Sistema").                                                               /* memo creator    */        
      RETURN "OK".
   END.                      
   ELSE RETURN "ERROR:" + lcResult.

END PROCEDURE.

PROCEDURE pSetCust_LOST:

   DEF INPUT PARAMETER lcMode AS CHAR NO-UNDO. /* "ON" to active barring ; "OFF" to remove barring. */

   DEF VAR liRequest AS INT  NO-UNDO.
   DEF VAR lcResult  AS CHAR NO-UNDO.   

   RUN Mm/barrengine.p(mobsub.MsSeq,
                       (IF lcMode EQ "ON" THEN "Cust_LOST=1" 
                                          ELSE "Cust_LOST=0"), /* Barring   */
                       {&REQUEST_SOURCE_YOIGO_TOOL} ,          /* source    */
                       "Sistema",                              /* creator   */
                       Func.Common:mMakeTS(),                  /* TimeStamp */
                       "",                                     /* SMSText    */
                       OUTPUT lcResult).

   liRequest = INTEGER(lcResult) NO-ERROR. 
                               
   IF liRequest > 0 THEN DO:     

   Func.Common:mWriteMemoWithType("Mobsub",
                     STRING(mobsub.MsSeq),
                     mobsub.CustNum,
                     (IF lcMode EQ "ON" THEN "Bloqueo modificado por proceso de migración (OTA KO - MANDARINA)" 
                                        ELSE "Bloqueo modificado por proceso de migración (OTA KO - MANDARINA)"),               /* memo title */
                     (IF lcMode EQ "ON" THEN "Cambio automático por proceso de migración -  Cliente_Robo/Pérdida - Activar" 
                                        ELSE "Cambio automático por proceso de migración -  Cliente_Robo/Pérdida - Desactivar"),/* memo text  */
                     "Service",                                                                /* memo type  */   
                     "Sistema").                                                               /* memo creator    */        
      RETURN "OK".
   END.                      
   ELSE RETURN "ERROR:" + lcResult.

END PROCEDURE.

