/*------------------------------------------------------------------------
    File        : DumpHandler
    Purpose     : Base class for every dump handlers
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Wed Oct 29 15:10:34 EET 2014
    Notes       : You need to set lhMainTable elsewhere (subclass) if you
                  are going to use mProcessRepLog implemented in
                  this class.
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

{HPD/HPDEnvironment.i}

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.DumpHandler ABSTRACT:  
   
   DEFINE PROTECTED  VARIABLE objDumpBase      AS CLASS HPD.DumpBase    NO-UNDO.
   DEFINE PUBLIC     VARIABLE llAllOK          AS LOGICAL INITIAL FALSE NO-UNDO.  
   DEFINE PUBLIC     VARIABLE llInterrupted    AS LOGICAL INITIAL FALSE NO-UNDO.
   DEFINE PUBLIC     VARIABLE liEvents         AS INTEGER               NO-UNDO.      
   DEFINE PROTECTED  VARIABLE lhMainTable      AS HANDLE                NO-UNDO.
   DEFINE PRIVATE    VARIABLE llFindOK         AS LOGICAL               NO-UNDO.

   
   METHOD PUBLIC CHARACTER mSpecialHandling
     (INPUT icField AS CHARACTER,
      INPUT ilCheckOnly AS LOGICAL):
         
      RETURN "".
               
   END METHOD.
      

   METHOD PUBLIC CHARACTER mProcessRepLog
      (INPUT icRowID AS CHARACTER):

      llFindOK = lhMainTable:FIND-BY-ROWID(TO-ROWID(icRowID), NO-LOCK) NO-ERROR.

      IF llFindOK
      THEN DO:
         llFindOK = lhMainTable:FIND-CURRENT(NO-LOCK) NO-ERROR.
         IF llFindOK
         THEN RETURN objDumpBase:mGetData().
      END.

      RETURN "".

   END METHOD.
   
   
   DESTRUCTOR PUBLIC DumpHandler( ):  
     
      IF VALID-OBJECT(objDumpBase)
      THEN DELETE OBJECT objDumpBase.
      
      IF VALID-HANDLE(lhMainTable)
      THEN DELETE OBJECT lhMainTable.      
      
   END DESTRUCTOR.


   METHOD PROTECTED VOID mLog
      ( iiLogLevel   AS INTEGER,
        icLogType    AS CHARACTER, /* ERROR, INFO, DEBUG, ... */
        icLogMessage AS CHARACTER ):
      IF LOG-MANAGER:LOGGING-LEVEL GE iiLogLevel
      THEN LOG-MANAGER:WRITE-MESSAGE(icLogMessage, icLogType).
   END METHOD.

   
   METHOD PROTECTED CHARACTER mCode2Value
      (icTableName AS CHARACTER,
       icFieldName AS CHARACTER,
       icCodeValue AS CHARACTER):

      FIND TMSCodes NO-LOCK WHERE
         TMSCodes.TableName = icTableName AND
         TMSCodes.FieldName = icFieldName AND
         TMSCodes.CodeValue = icCodeValue
      NO-ERROR.
      
      IF AVAILABLE TMSCodes
      THEN RETURN TMSCodes.CodeName.
      
      RETURN "".         
         
   END METHOD.


   METHOD PROTECTED CHARACTER mGetExternalId
      (icType       AS CHARACTER,
       icInternalID AS CHARACTER):

      &IF "{&HPD_ENVIRONMENT}" = "Yoigo"
      &THEN
      &ELSEIF "{&HPD_ENVIRONMENT}" = "Salt"
      &THEN
         FIND FIRST ExternalID USE-INDEX Type_InternalID_ExternalID NO-LOCK WHERE
            ExternalID.Type       = icType AND
            ExternalID.InternalID = icInternalID AND
            ExternalID.ExtSource  = "CM"
         NO-ERROR.

         IF AVAILABLE ExternalID
         THEN RETURN ExternalID.ExternalID.
      &ELSE
         RETURN ERROR NEW Progress.Lang.AppError("Unsupported HPD environment", 1).
      &ENDIF

      RETURN "".

   END METHOD.

END CLASS.