/* This program will create or update HPD dumpfile, dffield and dumphpd records
   for source dump id to target dumpid */

DEFINE VARIABLE gcStartTime AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcFinalTime AS CHARACTER NO-UNDO.
DEFINE VARIABLE glContinous AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gcSpoolDir  AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcOutDir    AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcUnitType  AS CHARACTER NO-UNDO.
DEFINE VARIABLE giUnitsToDump AS INTEGER NO-UNDO.

/* PLEASE MODIFY THESE FIRST */

ASSIGN
   gcStartTime   = "04.09.2017 00:00:00"
   gcFinalTime   = ""
   glContinous   = FALSE
   gcUnitType    = "days"
   giUnitsToDump = 1
   gcSpoolDir    = "/mnt/qss/mobcdr_spool"
   gcOutDir      = "/mnt/qss/mobcdr_tmp".
   
RUN pCreateUpdateTempDumps(219, 500, "HPD_MobCDR2").
RUN pCreateUpdateTempDumps(229, 501, "HPD_PrepCDR2").
RUN pCreateUpdateTempDumps(230, 550, "HPD_PrepEDR2").

/* NO NEED TO MODIFY AFTER THIS LINE... */   

PROCEDURE pCreateUpdateTempDumps:

   DEFINE INPUT  PARAMETER iiSourceDumpId AS INTEGER   NO-UNDO.
   DEFINE INPUT  PARAMETER iiTargetDumpId AS INTEGER   NO-UNDO.
   DEFINE INPUT  PARAMETER icNewDumpName  AS CHARACTER NO-UNDO.

   DEFINE BUFFER lbDumpFile FOR DumpFile.
   DEFINE BUFFER lbDFField  FOR DFField.
   DEFINE BUFFER lbDumpHPD  FOR DumpHPD.
   
   DEFINE VARIABLE liCount AS INTEGER NO-UNDO.
   DEFINE VARIABLE lcTimeBasedDumps AS CHARACTER NO-UNDO.
   
   FIND DumpFile NO-LOCK WHERE DumpFile.DumpID = iiSourceDumpId NO-ERROR.
   
   IF NOT AVAILABLE DumpFile
   THEN DO:
      MESSAGE SUBSTITUTE("Cannot find DumpFile with id &1", iiSourceDumpId)
      VIEW-AS ALERT-BOX.
      RETURN.
   END.
   IF DumpFile.FileCategory NE "HPD"
   THEN DO:
      MESSAGE SUBSTITUTE("DumpFile with id &1 is not HPD dump", iiSourceDumpId)
      VIEW-AS ALERT-BOX.
      RETURN.   
   END.

   DO ON ERROR UNDO, THROW:
   
      lcTimeBasedDumps = Syst.Parameters:getc("HPD.TimeBasedDumps", "HPD.Interface").

      IF LOOKUP(icNewDumpName, lcTimeBasedDumps) EQ 0
      THEN Syst.Parameters:setc("HPD.TimeBasedDumps", "HPD.Interface", lcTimeBasedDumps + "," + icNewDumpName).

      /* Handler code for any error condition. */
      CATCH anyErrorObject AS Progress.Lang.Error:
         Syst.Parameters:setc("HPD.TimeBasedDumps", "HPD.Interface", icNewDumpName).
      END CATCH.

   END.
   
   FIND lbDumpFile EXCLUSIVE-LOCK WHERE lbDumpFile.DumpID = iiTargetDumpId NO-ERROR.
   
   IF NOT AVAILABLE lbDumpFile
   THEN CREATE lbDumpFile.
   ELSE IF lbDumpFile.FileCategory NE "HPD"
   THEN DO:
      MESSAGE SUBSTITUTE("DumpFile with id &1 is not HPD dump", iiTargetDumpId)
      VIEW-AS ALERT-BOX.
      RETURN.   
   END.
   
   BUFFER-COPY DumpFile EXCEPT DumpID SpoolDir TransDir Active DumpName TO lbDumpFile
      ASSIGN lbDumpFile.DumpID   = iiTargetDumpId
             lbDumpFile.SpoolDir = gcSpoolDir
             lbDumpFile.TransDir = gcOutDir
             lbDumpFile.Active   = FALSE
             lbDumpFile.DumpName = icNewDumpName.    
   
   FOR EACH lbDFField EXCLUSIVE-LOCK WHERE lbDFField.DumpID = iiTargetDumpId:
      
      FIND FIRST DFField NO-LOCK WHERE
         DFField.DumpID   = iiSourceDumpId AND
         DFField.OrderNbr = lbDFField.OrderNbr
      NO-ERROR.
   
      IF NOT AVAILABLE DFField
      THEN DELETE lbDFField.   
   END.
      
   FOR EACH DFField NO-LOCK WHERE DFField.DumpID = iiSourceDumpId:
   
      liCount = 0.
      
      FOR EACH lbDFField EXCLUSIVE-LOCK WHERE
         lbDFField.DumpID   = iiTargetDumpId  AND
         lbDFField.OrderNbr = DFField.OrderNbr:
         
         liCount = liCount + 1.
         
         IF liCount > 1
         THEN DELETE lbDFField.
         
      END.
   
      IF liCount = 0
      THEN CREATE lbDFField.
      ELSE FIND FIRST lbDFField EXCLUSIVE-LOCK WHERE
         lbDFField.DumpID   = iiTargetDumpId  AND
         lbDFField.OrderNbr = DFField.OrderNbr.
   
      BUFFER-COPY DFField EXCEPT DumpID TO lbDFField
         ASSIGN lbDFField.DumpID = iiTargetDumpId.
   END.
   
   FIND lbDumpHPD EXCLUSIVE-LOCK WHERE lbDumpHPD.DumpID = iiTargetDumpId NO-ERROR.
      
   IF NOT AVAILABLE lbDumpHPD
   THEN CREATE lbDumpHPD.
   
   ASSIGN
      lbDumpHPD.Active      = TRUE
      lbDumpHPD.UnitsToDump = giUnitsToDump
      lbDumpHPD.UnitType    = gcUnitType
      lbDumpHPD.StartTime   = gcStartTime
      lbDumpHPD.FinalTime   = gcFinalTime
      lbDumpHPD.Continuous  = glContinous
      lbDumpHPD.DumpID      = iiTargetDumpId.

END.