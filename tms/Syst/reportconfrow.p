/* ----------------------------------------------------------------------
  MODULE .......: ReportConfRow.p
  TASK .........: UPDATEs table ReportConfRow
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 22.05.09
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
      RUN Mc/eventview2(lhReportConfRow).
   END.

END.

DEF INPUT PARAMETER icReportID  AS CHAR NO-UNDO.
DEF INPUT PARAMETER ilUpdate AS LOG  NO-UNDO. 

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


FORM
    ReportConfRow.RowType   FORMAT "X(20)"
    ReportConfRow.CharValue FORMAT "X(20)"
    ReportConfRow.IntValue  FORMAT "->>>>>>>>9"
    ReportConfRow.DecValue  FORMAT "->>>>>9.999"
    ReportConfRow.LogicValue
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       " CONFIGURATION FOR REPORT " + STRING(icReportID) + " "
    FRAME sel.

FORM
    ReportConfRow.Brand      COLON 20
    ReportConfRow.ReportID   COLON 20 FORMAT "X(20)" SKIP
       ReportConf.ReportName NO-LABEL AT 22 FORMAT "X(40)" SKIP
    ReportConfRow.ConfRowID  COLON 20 SKIP(1)
    ReportConfRow.RowType    COLON 20 FORMAT "X(20)"
       lcType FORMAT "X(20)" NO-LABEL SKIP
    ReportConfRow.CharValue  COLON 20 FORMAT "X(55)"
    ReportConfRow.IntValue   COLON 20
    ReportConfRow.DecValue   COLON 20
    ReportConfRow.DateValue  COLON 20 
    ReportConfRow.LogicValue COLON 20
WITH  OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.


FUNCTION fRowType RETURNS LOGIC
   (icRowType AS CHAR):

   IF icRowType > "" THEN 
      lcType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                "ReportConfRow",
                                "RowType",
                                icRowType).
   ELSE lcType = "".
                                
   DISP lcType WITH FRAME lis.
   
END FUNCTION.


cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

FIND FIRST ReportConf WHERE 
           ReportConf.Brand = gcBrand AND
           ReportConf.ReportID = icReportID NO-LOCK NO-ERROR.
IF NOT AVAILABLE ReportConf THEN DO:
   MESSAGE "Report not available"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

RUN local-Find-First.

IF AVAILABLE ReportConfRow THEN ASSIGN
   Memory       = recid(ReportConfRow)
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

   IF must-add THEN DO:  /* Add a ReportConfRow  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis ALL NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.

        REPEAT TRANS WITH FRAME lis:

           DISPLAY gcBrand @ ReportConfRow.Brand
                   icReportID @ ReportConfRow.ReportID.

           i = 1. 
           FIND LAST ReportConfRow USE-INDEX ConfRowID NO-LOCK NO-ERROR.
           IF AVAILABLE ReportConfRow THEN 
              i = ReportConfRow.ConfRowID + 1.
           
           CREATE ReportConfRow.
           ASSIGN 
              ReportConfRow.Brand     = gcBrand 
              ReportConfRow.ConfRowID = i
              ReportConfRow.ReportID  = icReportID.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
              ReportConfRow.RowType = "" THEN 
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhReportConfRow).

           ASSIGN
           Memory = recid(ReportConfRow)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE ReportConfRow THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND ReportConfRow WHERE recid(ReportConfRow) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ReportConfRow THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ReportConfRow).
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
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)  
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0) 
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.
        
        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[5] = 11
           ufk[6] = 0.
 
        ELSE IF NOT ilUpdate THEN ASSIGN
           ufk[5] = 0
           ufk[6] = 0.
          
        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ReportConfRow.RowType {Syst/uchoose.i} NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ReportConfRow.RowType WITH FRAME sel.
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
        FIND ReportConfRow WHERE recid(ReportConfRow) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ReportConfRow THEN
              ASSIGN FIRSTrow = i Memory = recid(ReportConfRow).
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
           IF NOT AVAILABLE ReportConfRow THEN DO:
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
                rtab[1] = recid(ReportConfRow)
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
           IF NOT AVAILABLE ReportConfRow THEN DO:
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
              rtab[FRAME-DOWN] = recid(ReportConfRow).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ReportConfRow WHERE recid(ReportConfRow) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ReportConfRow THEN DO:
           Memory = recid(ReportConfRow).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ReportConfRow THEN Memory = recid(ReportConfRow).
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
           FIND ReportConfRow WHERE recid(ReportConfRow) = Memory NO-LOCK.
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

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0 
     THEN DO TRANS:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
          ReportConfRow.RowType
          ReportConfRow.CharValue
          ReportConfRow.IntValue.

       RUN local-find-NEXT.
       IF AVAILABLE ReportConfRow THEN Memory = recid(ReportConfRow).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE ReportConfRow THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(ReportConfRow).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          ReportConfRow.RowType
          ReportConfRow.CharValue
          ReportConfRow.IntValue.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhReportConfRow).

           DELETE ReportConfRow.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE ReportConfRow THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANS
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhReportConfRow).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSEndStamp */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhReportConfRow).

       RUN local-disp-row.
       xrecid = recid(ReportConfRow).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ReportConfRow) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ReportConfRow) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

ehto = 4.
RUN Syst/ufkey.

fCleanEventObjects().



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND ReportConfRow WHERE recid(ReportConfRow) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ReportConfRow WHERE recid(ReportConfRow) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN FIND FIRST ReportConfRow WHERE 
      ReportConfRow.Brand = gcBrand AND
      ReportConfRow.ReportID = icReportID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN FIND LAST ReportConfRow WHERE 
      ReportConfRow.Brand = gcBrand AND
      ReportConfRow.ReportID = icReportID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN FIND NEXT ReportConfRow WHERE 
      ReportConfRow.Brand = gcBrand AND
      ReportConfRow.ReportID = icReportID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN FIND PREV ReportConfRow WHERE 
      ReportConfRow.Brand = gcBrand AND
      ReportConfRow.ReportID = icReportID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
   RUN local-find-others.
   CLEAR FRAME sel NO-PAUSE.
       
   DISPLAY 
      ReportConfRow.RowType
      ReportConfRow.CharValue
      ReportConfRow.IntValue
      ReportConfRow.DecValue
      ReportConfRow.LogicValue
   WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      fRowType(ReportConfRow.RowType).
         
      DISP 
         ReportConfRow.Brand   
         ReportConfRow.ReportID        
         ReportConf.ReportName
         ReportConfRow.ConfRowID
         ReportConfRow.RowType
         ReportConfRow.CharValue        
         ReportConfRow.IntValue
         ReportConfRow.DecValue
         ReportConfRow.DateValue
         ReportConfRow.LogicValue
      WITH FRAME lis.

      IF NOT NEW ReportConfRow THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7 WHEN lcRight = "RW" AND ilUpdate
            ufk[8] = 8
            ehto   = 0.
         
         RUN Syst/ufkey.
         
         IF toimi = 8 THEN LEAVE.
      END.

      UpdateField:
      REPEAT TRANS WITH FRAME lis ON ENDKEY UNDO, LEAVE:
                
         FIND CURRENT ReportConfRow EXCLUSIVE-LOCK.
         ehto = 9.
         RUN Syst/ufkey.
         
         UPDATE
            ReportConfRow.RowType   WHEN NEW ReportConfRow
            ReportConfRow.CharValue 
            ReportConfRow.IntValue  
            ReportConfRow.DecValue
            ReportConfRow.DateValue
            ReportConfRow.LogicValue
         WITH FRAME lis EDITING:
 
            READKEY.

            IF KEYLABEL(LASTKEY) = "F9" AND 
               LOOKUP(FRAME-FIELD,"RowType") > 0 
            THEN DO:

               IF FRAME-FIELD = "RowType" THEN DO:
                  RUN Help/h-tmscodes.p(INPUT "ReportConfRow", 
                                       "RowType", 
                                       ?, 
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN 
                     DISP lcCode @ ReportConfRow.RowType WITH FRAME lis.
               END.
             
               ehto = 9.
               RUN Syst/ufkey.
               NEXT. 
            END.

            ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
            DO WITH FRAME lis:
               PAUSE 0.
 
               IF FRAME-FIELD = "RowType" THEN DO:
                  IF INPUT FRAME lis ReportConfRow.RowType = ""
                  THEN LEAVE. 
 
                  fRowType(INPUT INPUT FRAME lis ReportConfRow.RowType).
                  IF lcType = "" THEN DO:
                     MESSAGE "Unknown row type"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.
             END.
            
            APPLY LASTKEY.
         END.

         LEAVE UpdateField.
         
      END.
      
      IF NEW ReportConfRow THEN LEAVE.
   END.

END PROCEDURE.

