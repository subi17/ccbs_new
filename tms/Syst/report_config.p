/* ----------------------------------------------------------------------
  MODULE .......: report_config.p
  TASK .........: UPDATEs table ttConfig
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 25.05.09
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'ReportConfRow'}
{Syst/eventval.i}
{Func/timestamp.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhReportConfRow AS HANDLE NO-UNDO.
   lhReportConfRow = BUFFER ReportConfRow:HANDLE.
   RUN StarEventInitialize(lhReportConfRow).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhReportConfRow).
   END.

END.


DEF INPUT PARAMETER icReportID  AS CHAR NO-UNDO.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 12.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
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

DEF VAR lcField      AS CHAR NO-UNDO. 
DEF VAR lcCode       AS CHAR NO-UNDO. 
DEF VAR lcType       AS CHAR NO-UNDO.
DEF VAR lcFrameField AS CHAR NO-UNDO. 
DEF VAR lcCodeTable  AS CHAR NO-UNDO.

DEF BUFFER bReportConfRow FOR ReportConfRow.


DEF TEMP-TABLE ttConfig NO-UNDO
   FIELD RowOrder    AS INT  
   FIELD RowType     AS CHAR LABEL "Type"
   FIELD Description AS CHAR LABEL "Description"
   FIELD ConfCode    AS INT  LABEL "Code"
   FIELD Section     AS INT  LABEL "Section"
   FIELD DispDetails AS LOGIC LABEL "Details"
   FIELD ConfRowID   AS INT
   INDEX RowOrder RowOrder.

FORM
    ttConfig.RowType     FORMAT "X(15)"  
    ttConfig.Description FORMAT "X(20)"  
    ttConfig.ConfCode    FORMAT "->>>>>>9"
    ttConfig.Section     FORMAT "->>>>>9"
    ttConfig.DispDetails FORMAT "Yes/No"
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       " CONFIGURATION FOR REPORT " + STRING(icReportID) + " "
    FRAME sel.

FORM
    icReportID    COLON 20 FORMAT "X(20)" LABEL "Report ID"
       ReportConf.ReportName NO-LABEL FORMAT "X(40)" SKIP
    ttConfig.RowType     COLON 20 FORMAT "X(20)"
       lcType FORMAT "X(20)" NO-LABEL SKIP
    ttConfig.Description COLON 20 FORMAT "X(40)"
    ttConfig.ConfCode    COLON 20 FORMAT "->>>>>>9"
    ttConfig.Section     COLON 20 FORMAT "->>>>>9"
    ttConfig.DispDetails COLON 20 FORMAT "Yes/No"
        HELP "Disp details on report"
WITH  OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.


FUNCTION fRowType RETURNS LOGIC
   (icRowType AS CHAR):

   IF icRowType > "" THEN 
      lcType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                "ttConfig",
                                "RowType",
                                icRowType).
   ELSE lcType = "".
                                
   DISP lcType WITH FRAME lis.
   
END FUNCTION.


cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

FIND FIRST ReportConf WHERE 
           ReportConf.Brand = gcBrand AND
           ReportConf.ReportID = icReportID NO-LOCK NO-ERROR.
IF NOT AVAILABLE ReportConf THEN DO:
   MESSAGE "Report not available"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

RUN pInitTempTable.

RUN local-Find-First.

IF AVAILABLE ttConfig THEN ASSIGN
   Memory       = recid(ttConfig)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No configuration rows available!" VIEW-AS ALERT-BOX INFORMATION.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = FALSE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND ttConfig WHERE recid(ttConfig) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttConfig THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttConfig).
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
        ufk   = 0
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.

        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ttConfig.RowType {Syst/uchoose.i} NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttConfig.RowType WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"5,f5,8,f8") = 0 THEN DO:
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
        FIND ttConfig WHERE recid(ttConfig) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttConfig THEN
              ASSIGN FIRSTrow = i Memory = recid(ttConfig).
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
           IF NOT AVAILABLE ttConfig THEN DO:
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
                rtab[1] = recid(ttConfig)
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
           IF NOT AVAILABLE ttConfig THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttConfig).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttConfig WHERE recid(ttConfig) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttConfig THEN DO:
           Memory = recid(ttConfig).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttConfig THEN Memory = recid(ttConfig).
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
           FIND ttConfig WHERE recid(ttConfig) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0 THEN DO:  /* add */
        IF gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           LEAVE LOOP.
        END.
        
        ELSE DO:
           must-add = TRUE.
           NEXT LOOP.
        END.    
     END.

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANS
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSEndStamp */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(ttConfig).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttConfig) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttConfig) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

FOR EACH ttConfig,
   FIRST bReportConfRow NO-LOCK WHERE
         bReportConfRow.ConfRowID = ttConfig.ConfRowID:
         
   IF ttConfig.DispDetails NE bReportConfRow.LogicValue THEN DO TRANS:
            
      FIND FIRST ReportConfRow WHERE 
         RECID(ReportConfRow) = RECID(bReportConfRow) EXCLUSIVE-LOCK.

      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhReportConfRow).
      ReportConfRow.LogicValue = ttConfig.DispDetails.
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhReportConfRow).
      RELEASE ReportConfRow.
   END.
END.

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

ehto = 4.
RUN Syst/ufkey.p.

fCleanEventObjects().


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND ttConfig WHERE recid(ttConfig) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttConfig WHERE recid(ttConfig) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN FIND FIRST ttConfig NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN FIND LAST ttConfig NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN FIND NEXT ttConfig NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN FIND PREV ttConfig NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
   RUN local-find-others.
   CLEAR FRAME sel NO-PAUSE.
       
   DISPLAY 
      ttConfig.RowType
      ttConfig.Description
      ttConfig.ConfCode
      ttConfig.Section
      ttConfig.DispDetails
   WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      fRowType(ttConfig.RowType).
         
      DISP 
         icReportID        
         ReportConf.ReportName
         ttConfig.RowType
         ttConfig.Description        
         ttConfig.ConfCode
         ttConfig.Section
         ttConfig.DispDetails
      WITH FRAME lis.

      ASSIGN 
         ufk    = 0
         ufk[1] = 7 
         ufk[8] = 8
         ehto   = 0.
         
      RUN Syst/ufkey.p.
         
      IF toimi = 8 THEN LEAVE.

      UpdateField:
      REPEAT TRANS WITH FRAME lis ON ENDKEY UNDO, LEAVE:
                
         FIND CURRENT ttConfig EXCLUSIVE-LOCK.
         ehto = 9.
         RUN Syst/ufkey.p.
         
         UPDATE
            ttConfig.DispDetails
         WITH FRAME lis.

         LEAVE UpdateField.
         
      END.
      
      LEAVE.
   END.

END PROCEDURE.

PROCEDURE pInitTempTable:

   DEF VAR liRowOrder AS INT  NO-UNDO.
    
    
   FOR EACH ReportConfRow NO-LOCK WHERE
            ReportConfRow.Brand    = gcBrand AND
            ReportConfRow.ReportID = icReportID
   BY ReportConfRow.RowType 
   BY ReportConfRow.DecValue 
   BY ReportConfRow.IntValue:
      CREATE ttConfig.
      ASSIGN
         liRowOrder        = liRowOrder + 1
         ttConfig.RowOrder = liRowOrder
         ttConfig.RowType  = ReportConfRow.RowType
         ttConfig.Section  = ReportConfRow.DecValue
         ttConfig.ConfCode = ReportConfRow.IntValue
         ttConfig.Description = ReportConfRow.CharValue
         ttConfig.DispDetails = ReportConfRow.LogicValue
         ttConfig.ConfRowID   = ReportConfRow.ConfRowID.
   END.

END PROCEDURE.

