/* ----------------------------------------------------------------------
  MODULE .......: DB_TRANS.P
  TASK .........: Create log of database transactions
  APPLICATION ..: TMS
  AUTHOR .......: kl
  CREATED ......: 12.10.07 kl
  CHANGED ......: 20.11.07 kl Yoigo version

  Version ......: Yoigo
  ---------------------------------------------------------------------- */


FUNCTION fScreen RETURNS LOGICAL
  (INPUT lcText AS CHARACTER):

   PUT SCREEN ROW 22 COL 1 FILL(" ",40).

   PUT SCREEN ROW 22 COL 1 lcText.

END.

FUNCTION fSendSMS RETURNS LOGICAL
  (INPUT pcMsg AS CHARACTER):

   DEFINE VARIABLE lcEOF  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcMsg  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcResp AS CHARACTER NO-UNDO.

   lcEOF = FILL(CHR(13) + CHR(10),2).
   
   lcMsg = "safesend;0456703674;173601;" + pcMsg + lcEOF.

   RUN socwrite(lcMsg, 5, OUTPUT lcResp).

END FUNCTION.

DEFINE TEMP-TABLE ttTrans NO-UNDO
   FIELD TimeStamp AS CHARACTER
   FIELD DbFile    AS CHARACTER
   FIELD TransNum  AS CHARACTER
   FIELD TransTime AS CHARACTER
   FIELD Duration  AS CHARACTER
   FIELD PID       AS CHARACTER
   FIELD UserNum   AS CHARACTER
   FIELD UserName  AS CHARACTER
   FIELD ConnTime  AS CHARACTER
   FIELD Device    AS CHARACTER.

DEFINE TEMP-TABLE ttAlarm NO-UNDO
   FIELD Msg  AS CHARACTER
   FIELD Sent AS LOGICAL.

DEFINE VARIABLE llQuit      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lcTimeStamp AS CHARACTER NO-UNDO.
DEFINE VARIABLE liLoop      AS INTEGER   NO-UNDO.
DEFINE VARIABLE liLoop1     AS INTEGER   NO-UNDO.
DEFINE VARIABLE liLoop2     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcDB        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lhTable     AS HANDLE    NO-UNDO.    
DEFINE VARIABLE lhField     AS HANDLE    NO-UNDO. 
DEFINE VARIABLE lhQuery     AS HANDLE    NO-UNDO. 
DEFINE VARIABLE lcQuery     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcUser      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcAlarm     AS CHARACTER NO-UNDO.

CREATE QUERY lhQuery.

DO WHILE NOT llQuit:

   fScreen("Logging....").

   lcTimeStamp = STRING(TODAY,"9999.99.99") + "@" +
                 STRING(TIME,"HH:MM:SS").

   DO liLoop1 = 1 to NUM-DBS:

      lcDB = LDBNAME(liLoop1).

      CREATE BUFFER lhTable FOR TABLE lcDB + "._Trans".

      lcQuery = "FOR EACH " + lcDB + "._Trans WHERE " +
                "_Trans._Trans-Duration NE 0 AND
                 _Trans._Trans-Duration NE ?".

      lhQuery:SET-BUFFERS(lhTable).
      lhQuery:QUERY-PREPARE(lcQuery).
      lhQuery:QUERY-OPEN.

      REPEAT:
      
         lhQuery:GET-NEXT(NO-LOCK).

         IF lhTable:AVAILABLE THEN DO:
             
            CREATE ttTrans.

            ASSIGN
               ttTrans.DbFile    = lcDB
               ttTrans.TimeStamp = lcTimeStamp.

            DO liLoop2 = 1 TO lhTable:NUM-FIELDS:

               lhField = lhTable:BUFFER-FIELD(liLoop2).

               CASE lhField:NAME:
                  WHEN "_Trans-Num"    THEN
                     ttTrans.TransNum  = lhField:BUFFER-VALUE.
                  WHEN "_Trans-txtime"    THEN
                     ttTrans.TransTime = lhField:BUFFER-VALUE.
                  WHEN "_Trans-Duration"    THEN
                     ttTrans.Duration  = lhField:BUFFER-VALUE.
                  WHEN "_Trans-UsrNum"    THEN
                     lcUser            = lhField:BUFFER-VALUE.
               END.
         
            END.

         END.
         ELSE LEAVE.

      END.
      
      DELETE OBJECT lhTable NO-ERROR.

      IF CAN-FIND(FIRST ttTrans WHERE
                        ttTrans.DbFile = lcDB) THEN DO:
      
         CREATE BUFFER lhTable FOR TABLE lcDB + "._Connect".

         lcQuery = "FOR EACH " + lcDB + "._Connect WHERE " +
                   "_Connect._Connect-Usr = " + lcUser.

         lhQuery:SET-BUFFERS(lhTable).
         lhQuery:QUERY-PREPARE(lcQuery).
         lhQuery:QUERY-OPEN.

         REPEAT:
      
            lhQuery:GET-NEXT(NO-LOCK).
      
            IF lhTable:AVAILABLE THEN DO:
             
               DO liLoop2 = 1 TO lhTable:NUM-FIELDS:
    
                  lhField = lhTable:BUFFER-FIELD(liLoop2).

                  CASE lhField:NAME:
                     WHEN "_Connect-PID"    THEN
                        ttTrans.PID      = STRING(lhField:BUFFER-VALUE).
                     WHEN "_Connect-Name"   THEN
                        ttTrans.UserName = STRING(lhField:BUFFER-VALUE).
                     WHEN "_Connect-Time"   THEN
                        ttTrans.ConnTime = STRING(lhField:BUFFER-VALUE).
                     WHEN "_Connect-Device" THEN
                        ttTrans.Device   = STRING(lhField:BUFFER-VALUE).
                     WHEN "_Connect-Usr"    THEN
                        ttTrans.UserNum = STRING(lhField:BUFFER-VALUE).
                  END.

               END.

            END.
            ELSE LEAVE.

         END.

         DELETE OBJECT lhTable NO-ERROR.
      END.

   END.

   FOR EACH ttTrans NO-LOCK
   BREAK BY ttTrans.TransNum:

      IF FIRST-OF(ttTrans.TransNum) THEN DO:

         OUTPUT TO VALUE("/tmp/" + ttTrans.DBFile + "_trans.txt") APPEND.

         PUT UNFORMATTED 
            ttTrans.TimeStamp "|"
            ttTrans.TransNum  "|"
            ttTrans.TransTime "|"
            ttTrans.Duration  "|"
            ttTrans.PID       "|"    
            ttTrans.Usernum   "|"
            ttTrans.UserName  "|"
            ttTrans.ConnTime  "|"
            ttTrans.Device    CHR(10).

         OUTPUT CLOSE.

      END.

   END.

   EMPTY TEMP-TABLE ttTrans.
   
   ASSIGN
      liLoop  = liLoop + 1
      lcAlarm = "".
   
   fScreen("Loop " + STRING(liLoop) + "  - Press F8 to quit!").

   READKEY PAUSE 300.

   IF LOOKUP(KEYLABEL(LASTKEY),"8,F8") > 0 THEN llQuit = TRUE.

END.

DELETE OBJECT lhQuery NO-ERROR.

QUIT.

