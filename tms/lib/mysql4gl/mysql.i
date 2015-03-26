&IF "{&MYSQL4GL_MYSQL_I}" NE "YES" &THEN
&GLOBAL-DEFINE MYSQL4GL_MYSQL_I YES

{lib/mysql4gl/mysql_procs.i}


FUNCTION fMysqlInit RETURNS {&MYSQL_PTR_TYPE}:
   DEFINE VARIABLE lpOutHandle AS MEMPTR NO-UNDO.

   RUN mysql_init(INPUT 0, OUTPUT lpOutHandle).
   
   LOG-MANAGER:WRITE-MESSAGE("mysql_init(0) => " + STRING(GET-POINTER-VALUE(lpOutHandle)),
                             "MySQL").
   
   /* FIXME: Handle error when pointer is NULL */
   RETURN GET-POINTER-VALUE(lpOutHandle).
END FUNCTION.

FUNCTION fMysqlConnect RETURNS LOGICAL
  (iiHandle     AS {&MYSQL_PTR_TYPE},
   icHost       AS CHAR,
   icUser       AS CHAR,
   icPasswd     AS CHAR,
   icDb         AS CHAR,
   iiPort       AS INT,
   icUnixSocket AS CHAR,
   iiClientFlag AS INT):

   DEFINE VARIABLE lpInHandle   AS MEMPTR NO-UNDO.
   DEFINE VARIABLE lcHost       AS CHAR NO-UNDO.
   DEFINE VARIABLE lcUser       AS CHAR NO-UNDO.
   DEFINE VARIABLE lcPasswd     AS CHAR NO-UNDO.
   DEFINE VARIABLE lcUnixSocket AS CHAR NO-UNDO.
   DEFINE VARIABLE lpOutHandle  AS MEMPTR NO-UNDO.

   SET-POINTER-VALUE(lpInHandle) = iiHandle.
   ASSIGN
      lcHost       = icHost
      lcUser       = icUser
      lcPasswd     = icPasswd
      lcUnixSocket = icUnixSocket.

   LOG-MANAGER:WRITE-MESSAGE("Connecting using"
                           + " lpInHandle=" + STRING(GET-POINTER-VALUE(lpInHandle))
			   + " lcHost=" + lcHost
			   + " lcUser=" + lcUser
			   + " lcPasswd=" + lcPasswd
			   + " icDb=" + icDb
			   + " iiPort=" + STRING(iiPort)
			   + " lcUnixSocket=" + lcUnixSocket
			   + " iiClientFlag=" + STRING(iiClientFlag),
			   "MySQL").
			   
   RUN mysql_real_connect(INPUT lpInHandle,
                          INPUT lcHost,
                          INPUT lcUser,
                          INPUT lcPasswd,
                          INPUT icDb,
                          INPUT iiPort,
                          INPUT lcUnixSocket,
                          INPUT iiClientFlag,
                          OUTPUT lpOutHandle).

   LOG-MANAGER:WRITE-MESSAGE("mysql_real_connect(...) => "
   			     + STRING(GET-POINTER-VALUE(lpOutHandle)),
   			     "MySQL").
			     
   /* FIXME: Handle error when pointer is NULL */
   RETURN (IF GET-POINTER-VALUE(lpOutHandle) EQ 0 THEN FALSE ELSE TRUE).
END FUNCTION.

FUNCTION fMysqlClose RETURNS LOGICAL
  (iiHandle AS {&MYSQL_PTR_TYPE}):

   DEFINE VARIABLE lpHandle AS MEMPTR NO-UNDO.

   LOG-MANAGER:WRITE-MESSAGE("Closing connection", "MySQL").
  
   SET-POINTER-VALUE(lpHandle) = iiHandle.
   RUN mysql_close(INPUT lpHandle).
   RETURN TRUE.
END FUNCTION.

FUNCTION fMysqlErrno RETURNS INT
  (iiHandle AS {&MYSQL_PTR_TYPE}):

   DEFINE VARIABLE lpHandle AS MEMPTR NO-UNDO.
   DEFINE VARIABLE liResult AS INT NO-UNDO.

   SET-POINTER-VALUE(lpHandle) = iiHandle.
   RUN mysql_errno(INPUT lpHandle, OUTPUT liResult).

   RETURN liResult.
END FUNCTION.

FUNCTION fMysqlError RETURNS CHAR
  (iiHandle AS {&MYSQL_PTR_TYPE}):

   DEFINE VARIABLE lpHandle AS MEMPTR NO-UNDO.
   DEFINE VARIABLE lpResult AS MEMPTR NO-UNDO.

   SET-POINTER-VALUE(lpHandle) = iiHandle.
   RUN mysql_error(INPUT lpHandle, OUTPUT lpResult).

   RETURN (IF GET-POINTER-VALUE(lpResult) EQ 0 THEN ? ELSE GET-STRING(lpResult, 1)).
END FUNCTION.

FUNCTION fMysqlQuery RETURNS INT
  (iiHandle AS {&MYSQL_PTR_TYPE},
   icStmt   AS CHAR):

   DEFINE VARIABLE lpHandle AS MEMPTR NO-UNDO.
   DEFINE VARIABLE liResult AS INT NO-UNDO.

   SET-POINTER-VALUE(lpHandle) = iiHandle.
   RUN mysql_query(INPUT lpHandle, INPUT icStmt, OUTPUT liResult).

   IF liResult NE 0 THEN DO:
      /* FIXME: Handle error here: (fMysqlErrno and/or fMysqlError) */
      RETURN -1.
   END.
   
   RETURN 0.
END FUNCTION.

FUNCTION fMysqlUseResult RETURNS {&MYSQL_PTR_TYPE}
  (iiHandle AS {&MYSQL_PTR_TYPE}):

   DEFINE VARIABLE lpHandle AS MEMPTR NO-UNDO.
   DEFINE VARIABLE lpResult AS MEMPTR NO-UNDO.

   SET-POINTER-VALUE(lpHandle) = iiHandle.
   RUN mysql_use_result(INPUT lpHandle, OUTPUT lpResult).

   IF GET-POINTER-VALUE(lpResult) EQ 0 THEN DO:
      /* FIXME: Handle error here: (fMysqlErrno and/or fMysqlError) */
      RETURN 0.
   END.
   
   RETURN GET-POINTER-VALUE(lpResult).
END FUNCTION.

FUNCTION fMysqlFetchRow RETURNS {&MYSQL_PTR_TYPE}
  (iiResult AS {&MYSQL_PTR_TYPE}):

   DEFINE VARIABLE lpHandle AS MEMPTR NO-UNDO.
   DEFINE VARIABLE lpResult AS MEMPTR NO-UNDO.

   SET-POINTER-VALUE(lpHandle) = iiResult.
   RUN mysql_fetch_row(INPUT lpHandle, OUTPUT lpResult).

   IF GET-POINTER-VALUE(lpResult) EQ 0 THEN DO:
      /* FIXME: Handle error here: (fMysqlErrno and/or fMysqlError) */
      RETURN 0.
   END.
   
   RETURN GET-POINTER-VALUE(lpResult).
END FUNCTION.


&ENDIF /* MYSQL4GL_MYSQL_I */
