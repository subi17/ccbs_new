 
 /*------------------------------------------------------------------------
    File        : ReqRunner
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Tue Oct 24 11:18:14 EEST 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Syst.ReqRunner INHERITS gearbox.daemons.Daemon: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	DEFINE PRIVATE VARIABLE objReqHandler       AS CLASS Syst.ReqHandler  NO-UNDO.
	DEFINE PRIVATE VARIABLE ldttzNextReportTime AS DATETIME-TZ            NO-UNDO.
   DEFINE PRIVATE VARIABLE liQueue             AS INTEGER                NO-UNDO.
   DEFINE PRIVATE VARIABLE liInterval          AS INTEGER                NO-UNDO.
   DEFINE PRIVATE VARIABLE liPauseTime         AS INTEGER                NO-UNDO.
   DEFINE PRIVATE VARIABLE liPauseTotal        AS INTEGER                NO-UNDO.
   DEFINE PRIVATE VARIABLE liWaitTime          AS INTEGER                NO-UNDO.
   DEFINE PRIVATE VARIABLE llWait              AS LOGICAL INITIAL FALSE  NO-UNDO.
   DEFINE PRIVATE VARIABLE ldaLastCheckDate    AS DATE                   NO-UNDO.
      
	CONSTRUCTOR PUBLIC ReqRunner (  ):

      IF multitenancy.TenantInformation:RealTenant NE multitenancy.TenantInformation:SuperTenant
      THEN RETURN ERROR NEW Progress.Lang.AppError
            ("This daemon must be run using the super tenant", 1). 

      IF LOG-MANAGER:LOGGING-LEVEL < 2 AND LOG-MANAGER:LOGFILE-NAME <> ?
      THEN LOG-MANAGER:WRITE-MESSAGE("Daemon started", "INFO").
		
	END CONSTRUCTOR.


   DESTRUCTOR PUBLIC ReqRunner( ):

      shutDown().

      IF LOG-MANAGER:LOGGING-LEVEL < 2 AND LOG-MANAGER:LOGFILE-NAME <> ?
      THEN LOG-MANAGER:WRITE-MESSAGE("Daemon stopped", "INFO").
                 
   END DESTRUCTOR.


   METHOD OVERRIDE PUBLIC VOID setUp ():

      liQueue = INTEGER(REPLACE(lDaemonName, THIS-OBJECT:GetClass():TypeName, "")).

      FIND RequestQueue NO-LOCK WHERE 
           RequestQueue.Brand = "1" AND
           RequestQueue.Queue = liQueue
      NO-ERROR.
      
      IF NOT AVAILABLE RequestQueue
      THEN RETURN ERROR NEW Progress.Lang.AppError
            (SUBSTRING("Cannot find RequestQueue where Queue = &1",liQueue), 1).

      IF NOT RequestQueue.InUse
      THEN RETURN ERROR NEW Progress.Lang.AppError
            (SUBSTRING("RequestQueue where Queue = &1 is not in use",liQueue), 1).
       
      objReqHandler = NEW Syst.ReqHandler(liQueue).
      ASSIGN
         liInterval   = RequestQueue.Interval
         liWaitTime   = 0
         liPauseTotal = 0
         llWait       = FALSE.     
      mSetNewReportTime().

   END METHOD.


   METHOD OVERRIDE PUBLIC VOID shutDown ():
      
      IF VALID-OBJECT(objReqHandler)
      THEN DELETE OBJECT objReqHandler.
      
   END METHOD.


   METHOD PRIVATE VOID mSetNewReportTime():

      ldttzNextReportTime = ADD-INTERVAL(NOW, 5, "minutes").

   END METHOD.


   METHOD PUBLIC OVERRIDE LOGICAL processOne():

      IF ldaLastCheckDate NE TODAY
      THEN DO:
         objReqHandler:mUpdateProcess().
         ldaLastCheckDate = TODAY.
      END.

      IF NOW > ldttzNextReportTime
      THEN DO:
         LOG-MANAGER:WRITE-MESSAGE(objReqHandler:mGetStatistic()).
         mSetNewReportTime().
      END.

      IF llWait
      THEN DO:
         ASSIGN
            liWaitTime   = MIN(liWaitTime + 1, liInterval)
            liPauseTime  = MIN(liWaitTime, 5)
            liPauseTotal = MIN(liPauseTotal + liPauseTime, liInterval).

         pauseUntilHeartbeat(liPauseTime).
      END.
      
      IF liPauseTotal >= liWaitTime 
      THEN DO:
         ASSIGN
            liPauseTotal = 0
            llWait = objReqHandler:mProcess().
         IF NOT llWait
         THEN liWaitTime = 0.
      END.

      RETURN FALSE.

   END METHOD.

END CLASS.