/* ----------------------------------------------------------------------
  MODULE .......: viewmcdr2
  TASK .........: show raw cdr information (CallDetail)
  APPLICATION ..: nn
  AUTHOR .......: 
  CREATED ......: 
  CHANGED ......: 
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}        
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'mobsub'}
{Syst/eventval.i}
{Func/callquery.i}

DEF   INPUT PARAMETER   idtDate    AS DATE.
DEF   INPUT Parameter   iiDtlSeq   AS INT.
DEF   INPUT PARAMETER   icCDRTable AS CHAR.

DEF VAR liLoop   AS INT  NO-UNDO.
DEF VAR lcTitle  AS CHAR NO-UNDO.
DEF VAR lcValue  AS CHAR NO-UNDO.
DEF VAR lcTemp   AS CHAR NO-UNDO.
DEF VAR lcFormat AS CHAR No-UNDO.
DEF VAR lcVersion AS CHAR NO-UNDO.
DEF VAR lhDetail AS HANDLE NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.
DEF VAR lcttCallDetail AS CHAR FORMAT "X(25)"    NO-UNDO.
DEF VAR lcttValue      AS CHAR FORMAT "X(50)"    NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 2.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR Memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.

DEF TEMP-TABLE ttCDRDtl NO-UNDO LIKE MCDRDtl2.

DEF TEMP-TABLE ttCallDetail NO-UNDO
   FIELD ttName  AS  CHAR FORMAT "X(25)"
   FIELD ttValue AS CHAR  FORMAT "X(50)" 
INDEX ttName IS PRIMARY ttName 
INDEX ttValue ttvalue.    


form
    ttName                 COLUMN-LABEL "NAME"
    ttCallDetail.ttValue   COLUMN-LABEL "VALUE"

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    "  CALL DETAIL  "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    ttCallDetail.ttName  LABEL  "NAME:"
    ttCallDetail.ttValue LABEL  "VALUE" FORMAT "X(60)" 

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form /* seek  ttCallDetail */
    lcttCallDetail
    HELP "Enter Name Of the Column "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND NAME "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  ttValue */
    lcttValue
    HELP "Enter Value "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND VALUE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.


RUN pInitialize.

IF RETURN-VALUE BEGINS "ERROR" THEN RETURN.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.


FIND FIRST ttCallDetail
/* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE ttCallDetail THEN ASSIGN
   Memory       = recid(ttCallDetail)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE "No detail headers available" VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a ttCallDetail  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ttCallDetail WHERE recid(ttCallDetail) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttCallDetail THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttCallDetail).
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
        ufk[1]= 30  ufk[2]= 9064 ufk[3]= 0  ufk[4]= 0
        ufk[5]= 0
        ufk[6]= 0
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ttName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttName WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW ttCallDetail.ttValue {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttCallDetail.ttValue WITH FRAME sel.
      END.

      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND ttCallDetail WHERE recid(ttCallDetail) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttCallDetail THEN
              ASSIGN FIRSTrow = i Memory = recid(ttCallDetail).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE ttCallDetail THEN DO:
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
                rtab[1] = recid(ttCallDetail)
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
           IF NOT AVAILABLE ttCallDetail THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttCallDetail).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttCallDetail WHERE recid(ttCallDetail) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttCallDetail THEN DO:
           Memory = recid(ttCallDetail).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttCallDetail THEN Memory = recid(ttCallDetail).
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
           FIND ttCallDetail WHERE recid(ttCallDetail) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       SET lcttCallDetail WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF lcttCallDetail ENTERED THEN DO:
          FIND FIRST ttCallDetail WHERE ttName >= lcttCallDetail
          /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ttCallDetail THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some ttCallDetail/ttCallDetail was found */
          ASSIGN order = 1 Memory = recid(ttCallDetail) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME F2.
       SET lcttValue WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF lcttValue ENTERED THEN DO:
          FIND FIRST ttCallDetail WHERE ttCallDetail.ttValue >= lcttValue
          USE-INDEX ttValue /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ttCallDetail THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some ttCallDetail/ttValue was found */
          ASSIGN order = 2 Memory = recid(ttCallDetail) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */


     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.


     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).


       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY ttName.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.


       RUN local-disp-row.
       xrecid = recid(ttCallDetail).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttCallDetail) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttCallDetail) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

EMPTY TEMP-TABLE ttCDRDtl.
IF VALID-HANDLE(lhDetail) THEN DELETE OBJECT lhDetail NO-ERROR.


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND ttCallDetail WHERE recid(ttCallDetail) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttCallDetail WHERE recid(ttCallDetail) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST ttCallDetail
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST ttCallDetail USE-INDEX ttValue
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST ttCallDetail
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST ttCallDetail USE-INDEX ttValue
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT ttCallDetail
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT ttCallDetail USE-INDEX ttValue
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV ttCallDetail
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV ttCallDetail USE-INDEX ttValue
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       ttCallDetail.ttName 
       ttCallDetail.ttValue

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
          ttCallDetail.ttValue
          ttCallDetail.ttValue
      WITH FRAME lis.

      MESSAGE "PRESS ENTER TO CONTINUE". PAUSE   NO-MESSAGE.
      LEAVE.
   END.
END PROCEDURE.

PROCEDURE pInitialize:

   lhDetail = TEMP-TABLE ttCDRDtl:HANDLE.

   fGetCDRDtl(icCDRTable,
              idtDate,
              iiDtlSeq,
              INPUT-OUTPUT lhDetail).

   FIND FIRST ttCDRDtl NO-ERROR.
   IF NOT AVAILABLE ttCDRDtl THEN DO:
      MESSAGE "Details were not found"
      VIEW-AS ALERT-BOX INFORMATION.
      RETURN "ERROR".
   END.

   lcVersion = ttCDRDtl.version.
   IF INDEX(lcVersion,"YC") = 0 THEN 
      lcVersion = lcVersion + ENTRY(4,ttCDRDtl.Detail,"|").
 
   FIND FIRST CSVHeader WHERE
              CSVHeader.Version = lcVersion
   NO-LOCK NO-ERROR.
 
   IF NOT AVAIL CSVHeader THEN DO:
      MESSAGE
      "Header information missing!" + lcVersion
      VIEW-AS ALERT-BOX INFORMATION.
      RETURN "ERROR".
   END.

   DO liLoop = 1 TO NUM-ENTRIES(CSVHeader.CSV,"|"):
      ASSIGN
         lcTemp   = ENTRY(liLoop,CSVHeader.CSV,"|")
         lcTitle  = REPLACE(ENTRY(2,lcTemp,"<"),">","")
         lcFormat = REPLACE(ENTRY(3,lcTemp,"<"),">","")
         lcTitle  = ENTRY(2,lcTitle,"=")
         lcFormat = ENTRY(2,lcFormat,"=")
         lcValue  = ENTRY(liLoop,ttCDRDtl.detail,"|") NO-ERROR.

      CREATE ttCallDetail.
      ASSIGN
         ttCallDetail.ttName  = lcTitle
         ttCallDetail.ttValue = lcValue NO-ERROR.
   END.

   RETURN "".
   
END PROCEDURE.


