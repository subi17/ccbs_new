 
 /*------------------------------------------------------------------------
    File        : TempTableXMLRPC
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Thu Jan 11 15:37:13 EET 2018
    Notes       : Before calling this class an include file
                  {fcgi_agent/xmlrpc/xmlrpc_access.i}
                  is needed.
                  
                  Then this class is initialized always using syntax:
                     
                  DEFINE VARIABLE objTTXMLRPC AS CLASS Syst.TempTableXMLRPC NO-UNDO.
                  
                  objTTXMLRPC = NEW Syst.TempTableXMLRPC(BUFFER tt_param:HANDLE,
                                                         BUFFER tt_lastpos:HANDLE,
                                                         add_to_tt).

                  /* After initialization phase... */
                  
                  /* For example serialize temp table ttTempTable to the top level */
                  objTTXMLRPC:mSerialize(response_toplevel_id, BUFFER ttTempTable:HANDLE).
                  
                  FINALLY:
                     IF VALID-OBJECT(objTTXMLRPC)
                     THEN DELETE objTTXMLRPC.
                  END FINALLY.  
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Syst.TempTableXMLRPC: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
   DEFINE PRIVATE VARIABLE ahttParam   AS HANDLE    NO-UNDO.
   DEFINE PRIVATE VARIABLE ahttLastPos AS HANDLE    NO-UNDO.
   DEFINE PRIVATE VARIABLE aiAddToTT   AS INTEGER   NO-UNDO.

	CONSTRUCTOR PUBLIC TempTableXMLRPC
	  ( ihtt_paramBufferHandle AS HANDLE,
       ihtt_lastposBufferHandle AS HANDLE,
       iiAddToTT AS INTEGER):
          
      ASSIGN
         ahttParam   = ihtt_paramBufferHandle 
		   ahttLastPos = ihtt_lastposBufferHandle
		   aiAddToTT   = iiAddToTT.

	END CONSTRUCTOR.

   METHOD PRIVATE INTEGER mGetNextPosition
      ( icParent  AS CHARACTER ):

      DEFINE VARIABLE liResult AS INTEGER NO-UNDO.
      DEFINE VARIABLE llFound  AS LOGICAL NO-UNDO.

      llFound = ahttLastPos:FIND-UNIQUE
                     (SUBSTITUTE("WHERE inout = &1 AND parent = &2",
                                 aiAddToTT, QUOTER(icParent))) NO-ERROR.

      IF llFound
      THEN ASSIGN
             liResult = ahttLastPos::lastposition
             ahttLastPos::lastposition = liResult + 1.  
      ELSE DO:
         ahttLastPos:BUFFER-CREATE().
         ASSIGN
            ahttLastPos::inout  = aiAddToTT
            ahttLastPos::parent = icParent
            ahttLastPos::lastposition = 1.
      END.

      RETURN liResult.

   END METHOD.

   METHOD PRIVATE CHARACTER mAddValue
      ( icParent AS CHARACTER,
        icPName AS CHARACTER,
        icPType AS CHARACTER,
        icPValue AS CHARACTER ):

      DEFINE VARIABLE liPosition AS INTEGER NO-UNDO.
      liPosition = mGetNextPosition(icParent).

      ahttParam:BUFFER-CREATE().

      ASSIGN
         ahttParam::inout    = aiAddToTT
         ahttParam::position = liPosition
         ahttParam::parent   = icParent
         ahttParam::name     = icPName
         ahttParam::type     = icPType
         ahttParam::cvalue   = icPValue.

      RETURN icParent + "," + STRING(liPosition).

   END METHOD.


   METHOD PUBLIC VOID mSerialize
      (icLevel           AS CHARACTER,
       ihTempTableBuffer AS HANDLE):
      
      DEFINE VARIABLE lcRespArray     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lcRespStruct    AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lii             AS INTEGER   NO-UNDO.
      DEFINE VARIABLE lcValue         AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lcType          AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lhQuery         AS HANDLE    NO-UNDO.
      DEFINE VARIABLE llOK            AS LOGICAL   NO-UNDO.
      
      IF NOT VALID-HANDLE(ihTempTableBuffer)
      THEN RETURN ERROR NEW Progress.Lang.AppError
                        ("mSerialize: Invalid buffer handle as parameter", 1).
      
      CREATE QUERY lhQuery.
      lhQuery:FORWARD-ONLY = TRUE.
      lhQuery:SET-BUFFERS(ihTempTableBuffer).
      lhQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1 NO-LOCK",ihTempTableBuffer:TABLE)).
      
      lcRespArray = mAddValue(icLevel,
                              "",
                              "array",
                              "").


      lhQuery:QUERY-OPEN().

      DO WHILE TRUE ON ERROR UNDO, THROW:
         
         llOK = lhQuery:GET-NEXT(NO-LOCK).
         
         IF llOK = ?
         THEN RETURN ERROR NEW Progress.Lang.AppError("Cannot process query", 1).

         /* No more records, or query is not open */
         IF NOT llOK
         THEN LEAVE.

         lcRespStruct = mAddValue(lcRespArray,
                                  "",
                                  "struct",
                                  "").

         DO lii = 1 TO ihTempTableBuffer:NUM-FIELDS:
            
            IF ihTempTableBuffer:BUFFER-FIELD(lii):SERIALIZE-HIDDEN
            THEN NEXT.
            
            IF ihTempTableBuffer:BUFFER-FIELD(lii):BUFFER-VALUE() EQ ?
            THEN NEXT.
            
            CASE ihTempTableBuffer:BUFFER-FIELD(lii):DATA-TYPE:
               WHEN "character"
               THEN ASSIGN
                       lcValue = CODEPAGE-CONVERT(ihTempTableBuffer:BUFFER-FIELD(lii):BUFFER-VALUE(), "UTF-8", SESSION:CHARSET)
                       lcType = "string".
               WHEN "logical"
               THEN ASSIGN
                       lcValue = STRING(ihTempTableBuffer:BUFFER-FIELD(lii):BUFFER-VALUE(), "1/0")
                       lcType = "boolean".
               WHEN "integer" OR WHEN "int64"
               THEN ASSIGN
                       lcValue = STRING(ihTempTableBuffer:BUFFER-FIELD(lii):BUFFER-VALUE())
                       lcType = "int".
               WHEN "decimal"
               THEN DO:
                  ASSIGN
                     lcValue = STRING(ihTempTableBuffer:BUFFER-FIELD(lii):BUFFER-VALUE())
                     lcValue = REPLACE(STRING(lcValue), SESSION:NUMERIC-DECIMAL-POINT, ".")
                     lcType = "double".
                  IF SUBSTRING(lcValue, 1, 1) EQ "."
                  THEN lcValue = "0" + lcValue.
                  ELSE IF SUBSTRING(lcValue, 1, 2) EQ "-."
                  THEN lcValue = "-0" + SUBSTRING(lcValue,2).
               END.
               WHEN "date" OR WHEN "datetime" OR WHEN "datetime-tz"
               THEN DO:
                   ASSIGN
                       lcValue = ENTRY(1, ISO-DATE(ihTempTableBuffer:BUFFER-FIELD(lii):BUFFER-VALUE()), ".")
                       lcType = "dateTime.iso8601".
                   ENTRY(1, lcValue, "T") = REPLACE(ENTRY(1, lcValue, "T"), "-", "").
               END.
               OTHERWISE NEXT.
            END.

            mAddValue(lcRespStruct,
                      ihTempTableBuffer:BUFFER-FIELD(lii):SERIALIZE-NAME,
                      lcType,
                      lcValue).
         END.
      END.

      FINALLY:
         IF VALID-HANDLE(lhQuery)
         THEN DO:
            lhQuery:QUERY-CLOSE().
            DELETE OBJECT lhQuery.
         END.
      END FINALLY.

   END METHOD.

END CLASS.