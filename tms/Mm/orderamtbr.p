/* -----------------------------------------------
  MODULE .......: orderamtbr.p
  FUNCTION .....: TMS codes browser
  APPLICATION ..: TMS
  AUTHOR .......: PZ
  CREATED ......: 13.03.06
  CHANGED ......: 20.03.07 kl  MsRequest counting
                  16.04.07/aam icSkipValue,icRunParam

  Version ......: TMS Master
  ------------------------------------------------------ */

{Syst/commali.i}


DEF INPUT  PARAMETER /*VAR*/ icTableName AS CHAR NO-UNDO.
DEF INPUT  PARAMETER /*VAR*/ icFieldName AS CHAR NO-UNDO.
DEF INPUT  PARAMETER         icSkipValue AS CHAR NO-UNDO.
DEF INPUT PARAMETER          icRunParam  AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER         icRet       AS CHAR NO-UNDO.

DEF TEMP-TABLE ttData NO-UNDO
   FIELD CodeValue AS INT
   FIELD CodeName  AS CHAR
   FIELD CodeAmt   AS INTEGER
   FIELD CodeCount AS LOGICAL

   INDEX CodeValue CodeValue
   INDEX CodeName  CodeName.

DEF VAR lcCodeName   AS CHAR NO-UNDO.
DEF VAR liCodeValue  AS INT  NO-UNDO.

DEF VAR xrecid       AS RECID                  NO-UNDO  init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 2.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 13.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR icCoName2    AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 2.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR Memory       AS ROWID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS ROWID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR liParam      AS INT                    NO-UNDO EXTENT 3.
DEF VAR liCount      AS INT                    NO-UNDO.


form
    ttData.CodeValue    
    ttData.CodeName     FORMAT "X(30)"    
    ttData.CodeAmt
WITH ROW FrmRow OVERLAY FrmDown DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ac-hdr + " " 
    CENTERED FRAME sel.

form /* seek  CodeValue */
    "Code value:" liCodeValue
    HELP "Enter code value"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE VALUE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek CodeName */
    "Code Name:" lcCodeName
    HELP "Enter code name"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE NAME "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.


DO liCount = 1 TO NUM-ENTRIES(icRunParam,";"):
   IF liCount <= 3 THEN 
      liParam[liCount] = INTEGER(ENTRY(liCount,icRunParam,";"))
      NO-ERROR.
END.


CASE icTableName:
WHEN "Order"     THEN ac-hdr = "Amount Of Orders Per Status".
WHEN "MsRequest" THEN ac-hdr = "Amount Of Requests Per Status".
END CASE.

FOR EACH TMSCodes NO-LOCK WHERE
         TMSCodes.TableName = icTableName AND
         TMSCodes.FieldName = icFieldName AND
         TMSCodes.CodeValue > "":

   IF icTableName = "order" AND codevalue = "9" THEN NEXT.
   
   /* list for values that should not be shown has been given */
   IF icSkipValue > "" AND 
      LOOKUP(TMSCodes.CodeValue,icSkipValue) > 0 THEN NEXT.
   
   CREATE ttData.

   ASSIGN 
      ttData.CodeValue = INT(TMSCodes.CodeValue)
      ttData.CodeName  = TMSCodes.CodeName.

END.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

/* column-labels for parameters */
ASSIGN
   ttData.CodeValue:LABEL IN FRAME sel = "Status"
   ttData.CodeName:LABEL  IN FRAME sel = "Status Code Name".

RUN local-find-first.

FIND FIRST ttData
    NO-LOCK NO-ERROR.
       
IF AVAILABLE ttData THEN ASSIGN
   Memory       = ROWID(ttData)
   must-print   = TRUE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE.

LOOP:
REPEAT WITH FRAME sel:
                           
   IF order <> pr-order AND MaxOrder NE 1 THEN DO:
      pr-order = order.
   END.

   PrintPage:
   DO :
   
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ttData WHERE ROWID(ttData) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose ROWID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttData THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = ROWID(ttData).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-LINE] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.
        up FRAME-LINE - 1.
        DOWN FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = FALSE.
        PAUSE 0 NO-MESSAGE.

        /* Now there is one page DISPLAYed AND the cursor is on the
        upermost ROW, waiting FOR a 'choose' */
      END. /* must-print = TRUE */
      
   END. /* PrintPage */

   /* IF a ROW was recently DELETEd: */
   IF delrow > 0 THEN DOWN delrow - 1.
   ASSIGN delrow = 0.

   BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
        ASSIGN
           ufk    = 0
           ufk[1] = 9064
           ufk[2] = 30
           ufk[5] = 11
           ufk[8] = 8 
           ehto   = 3 
           ufkey  = FALSE.

        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW ttData.CodeValue ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) ttData.CodeValue WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW ttData.CodeName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) ttData.CodeName WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND ttData WHERE ROWID(ttData) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttData THEN
              ASSIGN FIRSTrow = i Memory = ROWID(ttData).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE ttData THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* PREVious was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = ROWID(ttData)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE ttData THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT ROW was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = ROWID(ttData).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttData WHERE ROWID(ttData) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE TMSCodes THEN DO:
           Memory = ROWID(ttData).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttData THEN Memory = ROWID(ttData).
              ELSE RowNo = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* is this the very FIRST record of the table ?  */
           MESSAGE "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
        END.
     END. /* PREVious page */

     /* NEXT page */
     ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND ttData WHERE ROWID(ttData) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

        cfc = "puyr". RUN Syst/ufcolor.
        ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
        CLEAR FRAME f1.
        liCodeValue = ?.
        UPDATE liCodeValue WITH FRAME f1.
        HIDE FRAME f1 NO-PAUSE.

        IF liCodeValue > 0 THEN DO:
           FIND FIRST ttData WHERE 
                      ttData.CodeValue = liCodeValue
           NO-LOCK NO-ERROR.

           IF NOT AVAILABLE ttData THEN DO:
              MESSAGE "Not found !".    
              PAUSE 2 NO-MESSAGE.
              NEXT BROWSE.
           END.

           ASSIGN
              memory     = ROWID(ttData)
              must-print = TRUE
              order      = 1.
              
           NEXT LOOP.
        END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

        cfc = "puyr". RUN Syst/ufcolor.
        ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
        CLEAR FRAME f2.
        lcCodeName = "".
        UPDATE lcCodeName WITH FRAME f2.
        HIDE FRAME f2 NO-PAUSE.

        IF LENGTH(lcCodeName) > 0 THEN DO:
           FIND FIRST ttData WHERE 
                      ttData.CodeName = lcCodeName
           NO-LOCK NO-ERROR.

           IF NOT AVAILABLE ttData THEN DO:
              MESSAGE "Not found !".    
              PAUSE 2 NO-MESSAGE.
              NEXT BROWSE.
           END.

           ASSIGN
              memory     = ROWID(ttData)
              must-print = TRUE
              order      = 2.
           HIDE FRAME f2 NO-PAUSE.   
           NEXT LOOP.
        END.
     END. /* Search-2 */


     ELSE IF LOOKUP(nap,"5,f5,enter,return") > 0 THEN DO: 
        RUN local-find-this(FALSE).

        IF AVAILABLE ttData THEN DO:
           icRet = STRING(ttData.CodeValue).
           LEAVE LOOP.
        END.
     END.

     ELSE IF LOOKUP(nap,"HOME,H") > 0 THEN DO : /* FIRST record */
        RUN local-find-FIRST.
        ASSIGN Memory = ROWID(ttData) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = ROWID(ttData) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND ttData WHERE ROWID(ttData) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttData WHERE ROWID(ttData) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN 
      FIND FIRST ttData USE-INDEX CodeValue NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND FIRST ttData USE-INDEX CodeName NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN 
       FIND LAST ttData USE-INDEX CodeValue NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
     FIND LAST ttData USE-INDEX CodeName NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF order = 1 THEN 
      FIND NEXT ttData USE-INDEX CodeValue NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND NEXT ttData USE-INDEX CodeName NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:

   IF order = 1 THEN 
      FIND PREV ttData USE-INDEX CodeValue NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND PREV ttData USE-INDEX CodeName NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:

    RUN local-find-others.
       
    CLEAR FRAME sel NO-PAUSE.
    DISPLAY 
       ttData.CodeValue
       ttData.CodeName
       ttData.CodeAmt
    WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

   /* count only once */
   IF ttData.CodeCount = FALSE THEN DO:

      IF icTableName = "Order" THEN DO:

         FOR EACH Order NO-LOCK WHERE
                  Order.Brand      = gcBrand AND
                  Order.StatusCode = STRING(ttData.CodeValue):

            ttData.CodeAmt = ttData.CodeAmt + 1.

            IF ttData.CodeAmt MOD 100 = 0 THEN
               PUT SCREEN ROW 23 COL 2 STRING(ttData.CodeAmt).
         
         END.
      
      END.

      ELSE IF icTableName = "MsRequest" THEN DO:

         FOR EACH MsRequest NO-LOCK WHERE
                  MsRequest.Brand     = gcBrand    AND
                  MsRequest.ReqType   = liParam[1] AND
                  MsRequest.ReqStatus = ttData.CodeValue:

            ttData.CodeAmt = ttData.CodeAmt + 1.

            IF ttData.CodeAmt MOD 100 = 0 THEN
               PUT SCREEN ROW 23 COL 2 STRING(ttData.CodeAmt).
         
         END.

      END.
      
      PUT SCREEN ROW 23 COL 2 "                    ".
   
      FIND CURRENT ttData EXCLUSIVE-LOCK.
      ttData.CodeCount = TRUE.

   END.

END PROCEDURE.

