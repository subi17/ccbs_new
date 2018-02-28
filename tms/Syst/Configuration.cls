ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Syst.Configuration:

   DEF PRIVATE STATIC VAR instance AS CLASS Syst.Configuration NO-UNDO.
   DEF PUBLIC STATIC VAR last_error AS CHAR NO-UNDO.
   DEF PUBLIC STATIC VAR error_count AS INT NO-UNDO.
   DEF PUBLIC STATIC VAR gcBrand AS CHAR NO-UNDO INITIAL "1".

   METHOD PUBLIC STATIC CHAR getc ( iCode AS CHAR ):
      RETURN instance:_getc(iCode, ?).
   END METHOD.

   METHOD PUBLIC STATIC CHAR getc ( iCode AS CHAR, iGroup AS CHAR ):
      RETURN instance:_getc(iCode, iGroup).
   END METHOD.

   METHOD PUBLIC STATIC VOID setc ( iCode AS CHAR, iValue AS CHAR ):
      instance:_setc(iCode, ?, iValue).
   END METHOD.

   METHOD PUBLIC STATIC VOID setc
      ( iCode AS CHAR, iGroup AS CHAR, iValue AS CHAR ):
      instance:_setc(iCode, iGroup, iValue).
   END METHOD.

   METHOD PUBLIC STATIC CHAR getc
      ( iCode AS CHAR, iGroup AS CHAR, iDefault AS CHAR ):
      RETURN instance:_getc(iCode, iGroup, iDefault).
   END METHOD.

   METHOD PUBLIC STATIC INT geti ( iCode AS CHAR ):
      RETURN instance:_geti(iCode, ?).
   END METHOD.

   METHOD PUBLIC STATIC INT geti ( iCode AS CHAR, iGroup AS CHAR ):
      RETURN instance:_geti(iCode, iGroup).
   END METHOD.

   METHOD PUBLIC STATIC VOID incr (iCode AS CHAR ):
      instance:_incr(iCode, ?).
   END METHOD.

   METHOD PUBLIC STATIC VOID incr (iCode AS CHAR, iGroup AS CHAR ):
      instance:_incr(iCode, iGroup).
   END METHOD.

   METHOD PUBLIC STATIC VOID seti ( iCode AS CHAR, iValue AS INT ):
      instance:_seti(iCode, "", iValue).
   END METHOD.

   METHOD PUBLIC STATIC VOID seti
      ( iCode AS CHAR, iGroup AS CHAR, iValue AS INT ):
      instance:_seti(iCode, iGroup, iValue).
   END METHOD.

   METHOD PUBLIC STATIC INT geti ( iCode AS CHAR, iDefault AS INT ):
      RETURN instance:_geti(iCode, ?, iDefault).
   END METHOD.

   METHOD PUBLIC STATIC INT geti
      ( iCode AS CHAR, iGroup AS CHAR, iDefault AS INT ):
      RETURN instance:_geti(iCode, iGroup, iDefault).
   END METHOD.

   METHOD PUBLIC STATIC DEC getf ( iCode AS CHAR ):
      RETURN instance:_getf(iCode).
   END METHOD.

   METHOD PUBLIC STATIC DEC getf ( iCode AS CHAR, iGroup AS CHAR ):
      RETURN instance:_getf(iCode, iGroup).
   END METHOD.

   METHOD PUBLIC STATIC DEC getf ( iCode AS CHAR, iDefault AS DEC ):
      RETURN instance:_getf(iCode, ?, iDefault).
   END METHOD.

   METHOD PUBLIC STATIC DEC getf
      ( iCode AS CHAR, iGroup AS CHAR, iDefault AS DEC ):
      RETURN instance:_getf(iCode, iGroup, iDefault).
   END METHOD.

   METHOD PUBLIC STATIC DATE getd ( iCode AS CHAR ):
      RETURN instance:_getd(iCode).
   END METHOD.

   METHOD PUBLIC STATIC DATE getd ( iCode AS CHAR, iGroup AS CHAR ):
      RETURN instance:_getd(iCode, iGroup).
   END METHOD.

   METHOD PUBLIC STATIC DATE getd ( iCode AS CHAR, iDefault AS DATE ):
      RETURN instance:_getd(iCode, ?, iDefault).
   END METHOD.

   METHOD PUBLIC STATIC DATE getd
      ( iCode AS CHAR, iGroup AS CHAR, iDefault AS DATE ):
      RETURN instance:_getd(iCode, iGroup, iDefault).
   END METHOD.


   METHOD PUBLIC CHAR _getc ( iCode AS CHAR, iGroup AS CHAR ):
      IF NOT fetch_record(iCode, iGroup, "C", "a character") THEN RETURN ?.
      RETURN TMSParam.CharVal.
   END METHOD.

   METHOD PUBLIC VOID _setc ( iCode AS CHAR, iGroup AS CHAR, iValue AS CHAR ):
      IF fetch_record(iCode, iGroup, "C", "a character") THEN
         FIND CURRENT TmsParam EXCLUSIVE-LOCK.
      ELSE DO:
         IF NOT recover_notfound_error() THEN RETURN.
         CREATE TmsParam. ASSIGN
            TMSParam.Brand = gcBrand
            TMSParam.ParamCode = iCode
            TMSParam.ParamType = "C"
            TMSParam.ParamGroup = iGroup.
      END.
      TMSParam.CharVal = iValue.
      FIND CURRENT TmsParam NO-LOCK.
   END METHOD.

   METHOD PUBLIC CHAR _getc
      ( iCode AS CHAR, iGroup AS CHAR, iDefault AS CHAR ):
      DEF VAR lResult AS CHAR NO-UNDO.
      lResult = _getc(iCode, iGroup).
      IF recover_notfound_error() THEN
         lResult = iDefault.
      RETURN lResult.
   END METHOD.

   METHOD PUBLIC INT _geti ( iCode AS CHAR, iGroup AS CHAR ):
      IF NOT fetch_record(iCode, iGroup, "I", "an integer") THEN RETURN ?.
      RETURN TMSParam.IntVal.
   END METHOD.

   METHOD PUBLIC VOID _incr ( iCode AS CHAR, iGroup AS CHAR ):
      IF fetch_record(iCode, iGroup, "I", "an integer") THEN
         FIND CURRENT TmsParam EXCLUSIVE-LOCK.
      ELSE DO:
         IF NOT recover_notfound_error() THEN RETURN.
         CREATE TmsParam. ASSIGN
            TMSParam.Brand = gcBrand
            TMSParam.ParamCode = iCode
            TMSParam.ParamType = "I"
            TMSParam.ParamGroup = iGroup
            TMSParam.IntVal = 0.
      END.
      TMSParam.IntVal = TMSParam.IntVal + 1.
      FIND CURRENT TmsParam NO-LOCK.
   END METHOD.

   METHOD PUBLIC VOID _seti ( iCode AS CHAR, iGroup AS CHAR, iValue AS INT ):
      IF fetch_record(iCode, iGroup, "I", "an integer") THEN
         FIND CURRENT TmsParam EXCLUSIVE-LOCK.
      ELSE DO:
         IF NOT recover_notfound_error() THEN RETURN.
         CREATE TmsParam. ASSIGN
            TMSParam.Brand = gcBrand
            TMSParam.ParamCode = iCode
            TMSParam.ParamType = "I"
            TMSParam.ParamGroup = iGroup.
      END.
      TMSParam.IntVal = iValue.
      FIND CURRENT TmsParam NO-LOCK.
   END METHOD.

   METHOD PUBLIC INT _geti
      ( iCode AS CHAR, iGroup AS CHAR, iDefault AS INT ):
      DEF VAR lResult AS INT NO-UNDO.
      lResult = _geti(iCode, iGroup).
      IF recover_notfound_error() THEN
         lResult = iDefault.
      RETURN lResult.
   END METHOD.

   METHOD PUBLIC DEC _getf ( iCode AS CHAR ):
      IF NOT fetch_record(iCode, ?, "DE", "a decimal") THEN RETURN ?.
      RETURN TMSParam.DecVal.
   END METHOD.

   METHOD PUBLIC DEC _getf ( iCode AS CHAR, iGroup AS CHAR ):
      IF NOT fetch_record(iCode, iGroup, "DE", "a decimal") THEN RETURN ?.
      RETURN TMSParam.DecVal.
   END METHOD.

   METHOD PUBLIC DEC _getf
      ( iCode AS CHAR, iGroup AS CHAR, iDefault AS DEC ):
      DEF VAR lResult AS DEC NO-UNDO.
      IF iGroup EQ ? THEN
         lResult = _getf(iCode).
      ELSE
         lResult = _getf(iCode, iGroup).
      IF recover_notfound_error() THEN
         lResult = iDefault.
      RETURN lResult.
   END METHOD.

   METHOD PUBLIC DATE _getd ( iCode AS CHAR ):
      IF NOT fetch_record(iCode, ?, "DA", "a date") THEN RETURN ?.
      RETURN TMSParam.DateVal.
   END METHOD.

   METHOD PUBLIC DATE _getd ( iCode AS CHAR, iGroup AS CHAR ):
      IF NOT fetch_record(iCode, iGroup, "DA", "a date") THEN RETURN ?.
      RETURN TMSParam.DateVal.
   END METHOD.

   METHOD PUBLIC DATE _getd
      ( iCode AS CHAR, iGroup AS CHAR, iDefault AS DATE ):
      DEF VAR lResult AS DATE NO-UNDO.
      IF iGroup EQ ? THEN
         lResult = _getd(iCode).
      ELSE
         lResult = _getd(iCode, iGroup).
      IF recover_notfound_error() THEN
         lResult = iDefault.
      RETURN lResult.
   END METHOD.


   METHOD PRIVATE LOGICAL fetch_record
      ( iCode AS CHAR, iGroup AS CHAR,
        iType AS CHAR, iMsg AS CHAR ):
      DEF VAR lId AS CHAR NO-UNDO.
      IF iGroup EQ ? THEN
         FIND FIRST TmsParam NO-LOCK
         WHERE TMSParam.Brand EQ gcBrand
           AND TMSParam.ParamCode EQ iCode NO-ERROR.
      ELSE
         FIND FIRST TmsParam NO-LOCK
         WHERE TMSParam.Brand EQ gcBrand
           AND TMSParam.ParamCode EQ iCode
           AND TMSParam.ParamGroup EQ iGroup NO-ERROR.
      lId = IF iGroup EQ ? THEN iCode ELSE iGroup + ":" + iCode.
      IF NOT AVAILABLE TmsParam THEN DO:
         last_error = SUBST("Configuration parameter &1 not found.", lId).
         error_count = error_count + 1.
         RETURN FALSE.
      END.
      IF TMSParam.ParamType NE iType THEN DO:
         last_error = SUBST("Configuration parameter &1 is not &2.", lId, iMsg).
         error_count = error_count + 1.
         RETURN FALSE.
      END.
      last_error = "".
      RETURN TRUE.
   END METHOD.

   METHOD PRIVATE LOG recover_notfound_error ():
      IF last_error MATCHES "* not found." THEN ASSIGN
         last_error = ""
         error_count = error_count - 1.
      ELSE
         RETURN FALSE.
      RETURN TRUE.
   END METHOD.


   CONSTRUCTOR STATIC Configuration ():
      instance = NEW Syst.Configuration().
   END CONSTRUCTOR.

END CLASS.
