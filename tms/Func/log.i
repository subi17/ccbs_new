&IF "{&LOGGER_FRAMEWORK_LOG_I}" NE "YES" &THEN
&GLOBAL-DEFINE LOGGER_FRAMEWORK_LOG_I YES

/* ----------------------------------------------------------------------
  MODULE .......: log.i
  TASK .........: Provide functions to manage the 4GL Logging Framework
  APPLICATION ..: TMS
  AUTHOR .......: mikko 
  CREATED ......: 29.10.07
  CHANGED ......:
  Version ......: general TMS version 10.1B
  Functions.....: fSetGlobalLoggingLevel
                  fSetLogTreshold
                  fGetLogEntryTypes
                  fSetLogEntryTypes
                  fGetValidLogEntryTypes
                  fEnableLogType
                  fSetLogFileName
                  fSetNumLogFiles
                  fClearLog
                  fCloseLog
                  fLog 
----------------------------------------------------------------------- */

/** The level at which log entries are written to the log file.
 *
 * the logging level you specify applies to all log entry types.
 */
FUNCTION fSetGlobalLoggingLevel RETURNS LOGICAL
(iiValue AS INTEGER):
   IF iiValue < 0 OR iiValue > 4 THEN RETURN FALSE.
   LOG-MANAGER:LOGGING-LEVEL = iiValue NO-ERROR.
   RETURN ERROR-STATUS:ERROR.
END FUNCTION. 

/** Set logfile Size 
 * 
 * @param iiSize logfile size in Bytes (0 or 500000-2147483647).
 *
 * valid values for file size are:
 * 0   This means there is no limit other than what the operating system imposes. 
 *     Specify 0 to ignore the Number of Log Files to Keep (-numlogfiles) 
 *     startup parameter setting. This is the default.
 *
 * Between 500,000 and 2,147,483,647 
 * 
 * Values are in bytes (one byte typically holds one character). 
 * You can specify a file size up to 2GB, inclusive, but not lower than 500,000.
 */
FUNCTION fSetLogTreshold RETURNS LOGICAL
(iiValue AS INTEGER):
   LOG-MANAGER:LOG-THRESHOLD = iiValue NO-ERROR.
   RETURN ERROR-STATUS:ERROR.
END FUNCTION. 




/** Get comma-separated list of one or more types 
 *  of log entries to write to the log file.
 *
 * example: "DB.Connects,4GLTrace:2,DynObjects.UI:3"
 */
FUNCTION fGetLogEntryTypes RETURNS CHAR:
   RETURN LOG-MANAGER:LOG-ENTRY-TYPES.
END FUNCTION. 

/** Set comma-separated list of one or more types 
 *  of log entries to write to the log file.
 *
 * example: "DB.Connects,4GLTrace:2,DynObjects.UI:3"
 */
FUNCTION fSetLogEntryTypes RETURNS LOGICAL
(icValue AS CHAR):
   LOG-MANAGER:LOG-ENTRY-TYPES = icValue NO-ERROR.
   RETURN ERROR-STATUS:ERROR.
END FUNCTION.

/** Returns a character string containing a comma-separated list of 
 * all valid entry types for the current OpenEdge environment.
 */
FUNCTION fGetValidLogEntryTypes RETURNS CHAR:
   RETURN LOG-MANAGER:ENTRY-TYPES-LIST. 
END FUNCTION. 



/** Toggle ON/OFF log entry types. 
 *
 * @param icLogEntryType the Log entry type to toggle.
 * @param iiLogLevel The level at which log entries are written to the log file.
 *
 * There are five logging levels:
 * 0 (None)     Log no entries. This is equivalent to turning logging off.
 * 1 (Errors)   Log ABL error messages. This includes all 
 *              error messages and is unrelated to the entry types specified. 
 *              Errors are logged at level 1 (Errors) and higher.
 * 2 (Basic)   
 * 3 (Verbose) 
 * 4 (Extended)
 */
FUNCTION fEnableLogType RETURNS LOGICAL
(icLogEntryType AS CHAR,
 iiLogLevel AS INTEGER):
   DEFINE VARIABLE liNumEntries AS INTEGER NO-UNDO. 
   DEFINE VARIABLE lcEntries AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcEntry AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE i AS INTEGER NO-UNDO. 

   /* incorrect log level */
   IF iiLogLevel < 0 OR iiLogLevel > 4 THEN RETURN FALSE.
   /* incorrect log entry type */
   lcEntries = fGetValidLogEntryTypes().
   IF LOOKUP(icLogEntryType,lcEntries) EQ 0 THEN RETURN FALSE.
   
   /* get the current log configuration */
   lcEntries = fGetLogEntryTypes().

   /* check if entry type is already being logged */
   liNumEntries = NUM-ENTRIES(lcEntries).
   DO i = 1 TO liNumEntries:
      lcEntry = ENTRY(i,lcEntries).
      /* search the entry to modify */
      IF ENTRY(1,lcEntry,":") EQ icLogEntryType THEN DO:
         /* wanted level already set as current value */
         IF ENTRY(2,lcEntry,":") EQ STRING(iiLogLevel) THEN RETURN TRUE.
         ENTRY(i,lcEntries) = icLogEntryType + ":" + STRING(iiLogLevel).
         /* set new entry to manager */
         RETURN fSetLogEntryTypes(lcEntries).
      END.
   END.
   /* not yet in list, add it and modify logger */
   lcEntries = lcEntries + icLogEntryType + ":" + STRING(iiLogLevel).
   fSetLogEntryTypes(lcEntries).
END FUNCTION. 



/** The name of log file OpenEdge uses to log messages and ABL stack trace information.
 *
 * If the filename you supply is a relative pathname, then a file is accessed 
 * relative to the current working directory. If the filename is an absolute pathname, 
 * then the specified file is accessed.
 *
 * Note: Do not include a numbered sequence in the filename. 
 * This might conflict with the rolled over log files OpenEdge creates based 
 * on your NUM-LOG-FILES attribute and LOG-THRESHOLD attribute settings.
 *
 * Note: When the specified log file is already open, this function returns FALSE.
 *
 * The LOGFILE-NAME attribute corresponds to the Client Logging (-clientlog) 
 * startup parameter.
 *
 * The AVM names log files based on a sequence number using the following format:
 *
 * filename.999999.extension 
 *
 * For example, if you specify a log file named my.log, 
 * the AVM renames the log file to my.000001.log before creating a new log file. 
 *
 */
FUNCTION fSetLogFileName RETURNS LOGICAL
(icLogFileName AS CHAR):
   LOG-MANAGER:LOGFILE-NAME = icLogFileName NO-ERROR.
   RETURN ERROR-STATUS:ERROR.
END FUNCTION. 





/**The number of rolled over log files to keep on disk at any one time, 
 * for ABL session, including the current log file.
 * Valid values are:
 * 0 . This means there is no limit on the number of log files to keep.
 * 2 or greater . The default is 3.
 */
FUNCTION fSetNumLogFiles RETURNS LOGICAL
(iiValue AS INTEGER):
   IF iiValue EQ ? OR iiValue < 0 THEN iiValue = 0.
   LOG-MANAGER:NUM-LOG-FILES = iiValue NO-ERROR.
   RETURN ERROR-STATUS:ERROR.
END FUNCTION. 

/** Clears all messages existing in the current client log file 
 *  and leaves the file open for writing. 
 * If the CLEAR-LOG( ) method successfully clears the open log file, it returns TRUE.
 *
 * If the CLEAR-LOG( ) method fails, it returns FALSE and displays 
 * a warning message indicating the reason for the failure.
 *
 * If there is no client log file, the CLEAR-LOG( ) method returns FALSE 
 * and displays a warning message that the operation is not valid when 
 * there is no log file.
 *
 * If you specified a log file threshold with either the Log Threshold (-logthreshold)
 * startup parameter or the LOG-THRESHOLD attribute of the LOG-MANAGER handle, 
 * the CLEAR-LOG( ) method deletes any existing log files that match the name of 
 * the LOGFILE-NAME attribute or Client Logging (-clientlog) startup parameter. 
 * The method then re-creates and opens the first log file in the sequence and 
 * changes the LOGFILE-NAME attribute to reflect this. 
 */
FUNCTION fClearLog RETURNS LOGICAL:
   RETURN LOG-MANAGER:CLEAR-LOG().
END FUNCTION. 


/** Closes the current log file, which stops an interactive or batch client 
 * from writing messages to the log file.
 *
 * The CLOSE-LOG( ) method writes a message to the log file indicating that the 
 * client intentionally closed the log file, so that the user knows why there are 
 * no more messages in the log file.
 *
 * If the CLOSE-LOG( ) method successfully closes the open log file, it returns TRUE.
 *
 * If the CLOSE-LOG( ) method fails, it returns FALSE and displays a warning 
 * message indicating the reason for the failure.
 *
 * If there is no client log file, the CLOSE-LOG( ) method returns TRUE and 
 * does not display a warning message. 
 */
FUNCTION fCloseLog RETURNS LOGICAL:
   RETURN LOG-MANAGER:CLOSE-LOG().
END FUNCTION. 


/** Writes a user message to the current log file.
 *
 * @param icMessage the message to write to the log file.
 * @param icSubSystem the subsystem identifier to write to the log file.
 *
 * If the WRITE-MESSAGE( ) method succeeds, it returns TRUE. 
 * If it fails, it returns FALSE.
 *
 * Example function call:
 *    fLog("Got here, x=" + STRING(x), "DEBUG1").
 * example line written: 
 *    [04/12/05@13:19:19.742-0500] P-003616 T-001984 1 4GL DEBUG1    Got here, x=5
 */
FUNCTION fLog RETURNS LOGICAL
(icMessage AS CHAR,
 icSubSystem AS CHAR):
   IF icSubSystem NE ? THEN
      RETURN LOG-MANAGER:WRITE-MESSAGE(icMessage,icSubSystem).
   ELSE
      RETURN LOG-MANAGER:WRITE-MESSAGE(icMessage,THIS-PROCEDURE:NAME).
END FUNCTION. 


/** Writes a user message to the current log file.
 *-------------------------------------------------------
 * PRIVATE METHOD!
 *
 * you shouldn't use _fLog directly but use
 * fLogExtended,fLogVerbose,fLogBasic,fLogError instead
 *--------------------------------------------------------
 * @param icMessage the message to write to the log file.
 * @param icSubSystem the subsystem identifier to write to the log file.
 *
 * If the WRITE-MESSAGE( ) method succeeds, it returns TRUE.
 * If it fails, it returns FALSE.
 *
 * Example function call:
 *    fLog("Got here, x=" + STRING(x), "DEBUG1").
 * example line written:
 *    [04/12/05@13:19:19.742-0500] P-003616 T-001984 1 4GL DEBUG1    Got here, x=5
 */
FUNCTION _fLog RETURNS LOGICAL
(icMessage AS CHAR,
 icSubSystem AS CHAR):
   IF LOG-MANAGER:LOGFILE-NAME NE ? THEN
      IF icSubSystem NE ? THEN
         RETURN LOG-MANAGER:WRITE-MESSAGE("[" + THIS-PROCEDURE:NAME + "] " +
            icMessage,icSubSystem).
      ELSE
         RETURN LOG-MANAGER:WRITE-MESSAGE("[" + THIS-PROCEDURE:NAME + "] " +
            icMessage,"").
   ELSE RETURN FALSE.
END FUNCTION.

FUNCTION fLogExtended RETURN LOGICAL
(INPUT icMsg AS CHAR):
   IF LOG-MANAGER:LOGGING-LEVEL LT 4 THEN RETURN TRUE.
   RETURN _fLog(icMsg,"EXTENDED").
END.

FUNCTION fLogVerbose RETURN LOGICAL
(INPUT icMsg AS CHAR):
   IF LOG-MANAGER:LOGGING-LEVEL LT 3 THEN RETURN TRUE.
   RETURN _fLog(icMsg,"VERBOSE").
END.
FUNCTION fLogBasic RETURN LOGICAL
(INPUT icMsg AS CHAR):
   IF LOG-MANAGER:LOGGING-LEVEL LT 2 THEN RETURN TRUE.
   RETURN _fLog(icMsg,"BASIC").
END.
FUNCTION fLogError RETURN LOGICAL
(INPUT icMsg AS CHAR):
   IF LOG-MANAGER:LOGGING-LEVEL LT 1 THEN RETURN TRUE.
   RETURN _fLog(icMsg,"ERROR").
END.


FUNCTION fFullLogging RETURN LOGICAL:
   fSetGlobalLoggingLevel(4).
   fSetLogEntryTypes(fGetValidLogEntryTypes()).
END.

FUNCTION fStackTrace RETURN LOGICAL:
   DEFINE VARIABLE i AS INTEGER NO-UNDO. 
   IF NOT ERROR-STATUS:ERROR THEN RETURN TRUE.
   fLogError("Stacktrace follows...").
   DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
      fLogError(ERROR-STATUS:GET-MESSAGE(i)).
   END.
   RETURN TRUE.
END.

&ENDIF

