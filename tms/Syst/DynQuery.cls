 
 /*------------------------------------------------------------------------
    File        : DynQuery
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon May 22 18:04:20 EEST 2017
    Notes       :
       
    Example use of the class:

    DEFINE VARIABLE objDynQuery AS CLASS Syst.DynQuery NO-UNDO.

    objDynQuery = NEW Syst.DynQuery().

    objDynQuery:mAddBuffer(BUFFER TableName:HANDLE, BUFFER TableName2:HANDLE).
    objDynQuery:mSetQuery("FOR EACH TableName NO-LOCK, FIRST TableName2 NO-LOCK").

    /* Before calling the mGetNext make sure that there are no old
       query left in the middle (by leaving for the loop before mGetNext returns
       false). I.e. before setting a new query use objDynQuery:mCloseQuery(). */
    DO WHILE objDynQuery:mGetNext():          
       DISPLAY TableName.FieldName.
       IF objDynQuery:QueryHandle:FIRST-OF(0)
       THEN DISPLAY "First".
    END.
    
    FINALLY:
       IF VALID-OBJECT(objDynQuery)
       THEN DELETE OBJECT objDynQuery.
    END FINALLY.
       
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Syst.DynQuery FINAL: 

   DEFINE PUBLIC PROPERTY QueryText AS CHARACTER NO-UNDO
      GET.
      PRIVATE SET.

   DEFINE PUBLIC PROPERTY QueryHandle AS HANDLE NO-UNDO
      GET.
      PRIVATE SET.

   DEFINE PUBLIC PROPERTY DataAvailable AS LOGICAL NO-UNDO
      GET:
         IF NOT VALID-HANDLE(QueryHandle)
         THEN RETURN FALSE.
   
         RETURN QueryHandle:NUM-RESULTS > 0.
      END GET.
      

   DEFINE PRIVATE VARIABLE alNextAvailable AS LOGICAL NO-UNDO.

   CONSTRUCTOR PUBLIC DynQuery():
   END CONSTRUCTOR.


   DESTRUCTOR PUBLIC DynQuery():

      mCleanUp().

      IF VALID-HANDLE(QueryHandle)
      THEN DO:
         QueryHandle:QUERY-CLOSE().
         DELETE OBJECT QueryHandle.
      END.
      
   END DESTRUCTOR.


   METHOD PUBLIC VOID mCloseQuery():

      IF VALID-HANDLE(QueryHandle)
      THEN QueryHandle:QUERY-CLOSE().
      
   END METHOD.


   METHOD PUBLIC VOID mCleanUp():

      DEFINE VARIABLE lii AS INTEGER   NO-UNDO.

      mCloseQuery().

      IF VALID-HANDLE(QueryHandle)
      THEN DO lii = 1 TO QueryHandle:NUM-BUFFERS:
         IF VALID-HANDLE(QueryHandle:GET-BUFFER-HANDLE(lii))
         THEN DELETE OBJECT QueryHandle:GET-BUFFER-HANDLE(lii).
      END.

   END METHOD.


   METHOD PUBLIC VOID mSetQuery
      (icQuery AS CHARACTER):

      IF NOT VALID-HANDLE(QueryHandle)
      THEN RETURN ERROR NEW Progress.Lang.AppError("Invalid query handle. Maybe mAddBuffer call is missing?", 1).

      QueryHandle:QUERY-CLOSE().
      QueryHandle:QUERY-PREPARE(icQuery).
      QueryHandle:QUERY-OPEN().

   END METHOD.


   METHOD PUBLIC VOID mAddBuffer
      ( icTableNames AS CHARACTER,
        icQuery      AS CHARACTER ):

      IF NUM-ENTRIES(icTableNames) > 8
      THEN RETURN ERROR NEW Progress.Lang.AppError("The maximum buffer amount is 8", 1).

      DEFINE VARIABLE lii             AS INTEGER         NO-UNDO.
      DEFINE VARIABLE lhBufferHandle  AS HANDLE EXTENT 8 NO-UNDO.

      DO lii = 1 TO NUM-ENTRIES(icTableNames):
         CREATE BUFFER lhBufferHandle[lii] FOR TABLE ENTRY(lii,icTableNames).
      END.

      mAddBuffer(lhBufferHandle[1],
                      lhBufferHandle[2],
                      lhBufferHandle[3],
                      lhBufferHandle[4],
                      lhBufferHandle[5],
                      lhBufferHandle[6],
                      lhBufferHandle[7],
                      lhBufferHandle[8]).

   END METHOD.


   METHOD PUBLIC VOID mAddBuffer
      ( ihBufferHandle AS HANDLE ):

      mAddBuffer(ihBufferHandle,
                 ?,
                 ?,
                 ?,
                 ?,
                 ?,
                 ?,
                 ?).

   END METHOD.


   METHOD PUBLIC VOID mAddBuffer
      ( ihBufferHandle1 AS HANDLE,
        ihBufferHandle2 AS HANDLE ):

      mAddBuffer(ihBufferHandle1,
                 ihBufferHandle2,
                 ?,
                 ?,
                 ?,
                 ?,
                 ?,
                 ?).

   END METHOD.


   METHOD PUBLIC VOID mAddBuffer
      ( ihBufferHandle1 AS HANDLE,
        ihBufferHandle2 AS HANDLE,
        ihBufferHandle3 AS HANDLE ):

      mAddBuffer(ihBufferHandle1,
                 ihBufferHandle2,
                 ihBufferHandle3,
                 ?,
                 ?,
                 ?,
                 ?,
                 ?).

   END METHOD.


   METHOD PUBLIC VOID mAddBuffer
      ( ihBufferHandle1 AS HANDLE,
        ihBufferHandle2 AS HANDLE,
        ihBufferHandle3 AS HANDLE,
        ihBufferHandle4 AS HANDLE ):

      mAddBuffer(ihBufferHandle1,
                 ihBufferHandle2,
                 ihBufferHandle3,
                 ihBufferHandle4,
                 ?,
                 ?,
                 ?,
                 ?).

   END METHOD.


   METHOD PUBLIC VOID mAddBuffer
      ( ihBufferHandle1 AS HANDLE,
        ihBufferHandle2 AS HANDLE,
        ihBufferHandle3 AS HANDLE,
        ihBufferHandle4 AS HANDLE,
        ihBufferHandle5 AS HANDLE ):

      mAddBuffer(ihBufferHandle1,
                 ihBufferHandle2,
                 ihBufferHandle3,
                 ihBufferHandle4,
                 ihBufferHandle5,
                 ?,
                 ?,
                 ?).

   END METHOD.


   METHOD PUBLIC VOID mAddBuffer
      ( ihBufferHandle1 AS HANDLE,
        ihBufferHandle2 AS HANDLE,
        ihBufferHandle3 AS HANDLE,
        ihBufferHandle4 AS HANDLE,
        ihBufferHandle5 AS HANDLE,
        ihBufferHandle6 AS HANDLE ):

      mAddBuffer(ihBufferHandle1,
                 ihBufferHandle2,
                 ihBufferHandle3,
                 ihBufferHandle4,
                 ihBufferHandle5,
                 ihBufferHandle6,
                 ?,
                 ?).

   END METHOD.


   METHOD PUBLIC VOID mAddBuffer
      ( ihBufferHandle1 AS HANDLE,
        ihBufferHandle2 AS HANDLE,
        ihBufferHandle3 AS HANDLE,
        ihBufferHandle4 AS HANDLE,
        ihBufferHandle5 AS HANDLE,
        ihBufferHandle6 AS HANDLE,
        ihBufferHandle7 AS HANDLE ):

      mAddBuffer(ihBufferHandle1,
                 ihBufferHandle2,
                 ihBufferHandle3,
                 ihBufferHandle4,
                 ihBufferHandle5,
                 ihBufferHandle6,
                 ihBufferHandle7,
                 ?).

   END METHOD.


   METHOD PUBLIC VOID mAddBuffer
      ( ihBufferHandle1 AS HANDLE,
        ihBufferHandle2 AS HANDLE,
        ihBufferHandle3 AS HANDLE,
        ihBufferHandle4 AS HANDLE,
        ihBufferHandle5 AS HANDLE,
        ihBufferHandle6 AS HANDLE,
        ihBufferHandle7 AS HANDLE,
        ihBufferHandle8 AS HANDLE ):

      IF NOT VALID-HANDLE(QueryHandle)
      THEN CREATE QUERY QueryHandle.

      mCleanUp().

      IF VALID-HANDLE(ihBufferHandle1)
      THEN QueryHandle:ADD-BUFFER(ihBufferHandle1). 
      
      IF VALID-HANDLE(ihBufferHandle2)
      THEN QueryHandle:ADD-BUFFER(ihBufferHandle2). 

      IF VALID-HANDLE(ihBufferHandle3)
      THEN QueryHandle:ADD-BUFFER(ihBufferHandle3). 

      IF VALID-HANDLE(ihBufferHandle4)
      THEN QueryHandle:ADD-BUFFER(ihBufferHandle4). 

      IF VALID-HANDLE(ihBufferHandle5)
      THEN QueryHandle:ADD-BUFFER(ihBufferHandle5). 

      IF VALID-HANDLE(ihBufferHandle6)
      THEN QueryHandle:ADD-BUFFER(ihBufferHandle6). 

      IF VALID-HANDLE(ihBufferHandle7)
      THEN QueryHandle:ADD-BUFFER(ihBufferHandle7). 

      IF VALID-HANDLE(ihBufferHandle8)
      THEN QueryHandle:ADD-BUFFER(ihBufferHandle8). 

   END METHOD.


   METHOD PUBLIC LOGICAL mGetNext():

      alNextAvailable = QueryHandle:GET-NEXT(NO-LOCK).

      IF alNextAvailable
      THEN RETURN TRUE.

      IF alNextAvailable = ?
      THEN DO:
         mCleanUp().
         RETURN ERROR NEW Progress.Lang.AppError("Cannot process a query", 1).
      END.

      mCloseQuery().

      RETURN FALSE.
   
   END METHOD.

END CLASS.