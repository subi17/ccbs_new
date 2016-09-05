/* ----------------------------------------------------------------------
  MODULE .......: DFTimeTable.p
  TASK .........: UPDATEs table DFTimeTable
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 29.10.08
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'DFTimeTable'}
{Func/timestamp.i}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhDFTimeTable AS HANDLE NO-UNDO.
   lhDFTimeTable = BUFFER DFTimeTable:HANDLE.
   RUN StarEventInitialize(lhDFTimeTable).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhDFTimeTable).
   END.

END.

DEF INPUT PARAMETER iiDumpID AS INT  NO-UNDO.

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

DEF VAR lcWeekDay      AS CHAR NO-UNDO.
DEF VAR lcLastRun      AS CHAR NO-UNDO.
DEF VAR llDumpMode     AS LOG  NO-UNDO.
DEF VAR lcOngoing      AS CHAR NO-UNDO.

FORM
    DFTimeTable.DumpDay      FORMAT "X(10)"
    DFTimeTable.DumpWeekDay  FORMAT "X(13)"
    DFTimeTable.DumpTime     FORMAT "X(13)"
    DFTimeTable.DumpMode     FORMAT "X(10)"
    lcLastRun                FORMAT "X(10)" COLUMN-LABEL "Last Run"
    DFTimeTable.FromDate
    DFTimeTable.ToDate
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       " TIMETABLE FOR DUMP FILE " + STRING(iiDumpID) + " "
    FRAME sel.

FORM
    DFTimeTable.DumpID       COLON 15
       DumpFile.Description NO-LABEL SKIP(1)
    DFTimeTable.DumpDay      COLON 15 LABEL "Days"
        HELP "Days of the month"
    DFTimeTable.DumpWeekDay  COLON 15 
    DFTimeTable.DumpTime FORMAT "X(25)" COLON 15
    llDumpMode               COLON 15 
        FORMAT "Full/Modified"
        LABEL "Dump Mode"
        HELP "Dump mode"
    DFTimeTable.LastRun      COLON 15
       lcLastRun NO-LABEL FORMAT "X(19)" SKIP
    DFTimeTable.Ongoing      COLON 15 LABEL "Ongoing Run"
       lcOngoing NO-LABEL FORMAT "X(19)" SKIP
    DFTimeTable.FromDate     COLON 15
    DFTimeTable.ToDate       COLON 15
    DFTimeTable.FileNameTag  COLON 15
    DFTimeTable.UseReplica   COLON 15
WITH  OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.


FUNCTION fLastRun RETURNS LOGIC
   (idLastRun AS DEC):

   IF idLastRun > 0 THEN 
      lcLastRun = fTS2HMS(idLastRun).
   ELSE lcLastRun = "".
   
END FUNCTION.

FUNCTION fOngoing RETURNS LOGIC
   (idOngoing AS DEC):

   IF idOngoing > 0 THEN 
      lcOngoing = fTS2HMS(idOngoing).
   ELSE lcOngoing = "".
   
END FUNCTION.


cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

FIND FIRST DumpFile WHERE DumpFile.DumpID = iiDumpID NO-LOCK NO-ERROR.
IF NOT AVAILABLE DumpFile THEN DO:
   MESSAGE "Dump file rule not available"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

RUN local-Find-First.

IF AVAILABLE DFTimeTable THEN ASSIGN
   Memory       = recid(DFTimeTable)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No timetables available!" VIEW-AS ALERT-BOX INFORMATION.
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

   IF must-add THEN DO:  /* Add a DFTimeTable  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis ALL NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.p.

        DO TRANSACTION:
           DISPLAY iiDumpID @ DFTimeTable.DumpID.

           PROMPT-FOR 
              DFTimeTable.DumpDay
              DFTimeTable.DumpWeekDay WITH FRAME lis.

           IF INPUT DFTimeTable.DumpDay = "" AND
              INPUT DFTimeTable.DumpWeekDay = "" THEN UNDO, LEAVE add-row.
           
           IF INPUT DFTimeTable.DumpDay > "" AND
              INPUT DFTimeTable.DumpWeekDay > "" THEN DO:
              MESSAGE "You cannot use both weekday and month level"
              VIEW-AS ALERT-BOX INFORMATION.
              NEXT.
           END.
           
           CREATE DFTimeTable.
           ASSIGN 
              DFTimeTable.Brand    = gcBrand 
              DFTimeTable.DumpID   = iiDumpID
              DFTimeTable.DumpDay  = INPUT FRAME lis DFTimeTable.DumpDay
              DFTimeTable.DumpWeekDay = INPUT FRAME lis DFTimeTable.DumpWeekDay
              DFTimeTable.FromDate = TODAY
              DFTimeTable.ToDate   = 12/31/2049
              DFTimeTable.UseReplica = NO.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
              DFTimeTable.DumpTime = ""  THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhDFTimeTable).

           ASSIGN
           Memory = recid(DFTimeTable)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE DFTimeTable THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND DFTimeTable WHERE recid(DFTimeTable) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE DFTimeTable THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(DFTimeTable).
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
           ufk[6] = 0
           ufk[7] = 0.
         
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW DFTimeTable.DumpWeekDay {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) DFTimeTable.DumpWeekDay WITH FRAME sel.
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
        FIND DFTimeTable WHERE recid(DFTimeTable) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE DFTimeTable THEN
              ASSIGN FIRSTrow = i Memory = recid(DFTimeTable).
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
           IF NOT AVAILABLE DFTimeTable THEN DO:
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
                rtab[1] = recid(DFTimeTable)
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
           IF NOT AVAILABLE DFTimeTable THEN DO:
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
              rtab[FRAME-DOWN] = recid(DFTimeTable).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND DFTimeTable WHERE recid(DFTimeTable) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE DFTimeTable THEN DO:
           Memory = recid(DFTimeTable).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE DFTimeTable THEN Memory = recid(DFTimeTable).
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
           FIND DFTimeTable WHERE recid(DFTimeTable) = Memory NO-LOCK.
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
          DFTimeTable.DumpWeekDay
          DFTimeTable.DumpMode
          lcLastRun.

       RUN local-find-NEXT.
       IF AVAILABLE DFTimeTable THEN Memory = recid(DFTimeTable).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE DFTimeTable THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(DFTimeTable).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(false).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          DFTimeTable.DumpWeekDay
          DFTimeTable.DumpMode
          lcLastRun.
       
       IF ok THEN DO:
       
           RUN local-find-this(true).

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhDFTimeTable).

           DELETE DFTimeTable.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE DFTimeTable THEN DO:
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
     REPEAT WITH FRAME lis /* TRANS */
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

       /* IF  User Wanted TO Cancel this Change TRANSToDate */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(DFTimeTable).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(DFTimeTable) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(DFTimeTable) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

ehto = 4.
RUN Syst/ufkey.p.

fCleanEventObjects().



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND DFTimeTable WHERE recid(DFTimeTable) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND DFTimeTable WHERE recid(DFTimeTable) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN FIND FIRST DFTimeTable WHERE 
      DFTimeTable.Brand  = gcBrand AND 
      DFTimeTable.DumpID = iiDumpID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN FIND LAST DFTimeTable WHERE 
      DFTimeTable.Brand  = gcBrand AND 
      DFTimeTable.DumpID = iiDumpID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN FIND NEXT DFTimeTable WHERE 
      DFTimeTable.Brand  = gcBrand AND 
      DFTimeTable.DumpID = iiDumpID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN FIND PREV DFTimeTable WHERE 
      DFTimeTable.Brand  = gcBrand AND 
      DFTimeTable.DumpID = iiDumpID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.

       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       DFTimeTable.DumpDay
       DFTimeTable.DumpWeekDay
       DFTimeTable.DumpTime
       DFTimeTable.DumpMode
       lcLastRun
       DFTimeTable.FromDate
       DFTimeTable.ToDate
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

   fLastRun(DFTimeTable.LastRun).
    
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF BUFFER bField FOR DFTimeTable.
   DEF VAR llNew AS LOG NO-UNDO.

   llNew = (NEW DFTimeTable).

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      llDumpMode = (DFTimeTable.DumpMode = "full").
      fOngoing(DFTimeTable.Ongoing).

      DISP 
         DFTimeTable.DumpID        
         DumpFile.Description
         DFTimeTable.DumpDay
         DFTimeTable.DumpWeekDay
         DFTimeTable.DumpTime        
         llDumpMode
         DFTimeTable.LastRun
         lcLastRun
         DFTimeTable.Ongoing
         lcOngoing
         DFTimeTable.FromDate
         DFTimeTable.ToDate
         DFTimeTable.FileNameTag
         DFTimeTable.UseReplica
      WITH FRAME lis.

      IF NOT NEW DFTimeTable THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7 WHEN lcRight = "RW"
            ufk[3] = 1984
            ufk[8] = 8
            ehto   = 0.
         
         RUN Syst/ufkey.p.
      
         FIND CURRENT DFTimeTable NO-LOCK.
         IF CURRENT-CHANGED DFTimeTable THEN DO:
            MESSAGE " This record has been changed elsewhere" VIEW-AS ALERT-BOX.
            NEXT.
         END.
         
      END.
      ELSE toimi = 1.

      IF toimi = 1 THEN DO:
         UpdateField:
         REPEAT TRANS WITH FRAME lis ON ENDKEY UNDO, LEAVE:
                
            FIND CURRENT DFTimeTable NO-LOCK.
            ehto = 9.
            RUN Syst/ufkey.p.
         
            PROMPT-FOR
               DFTimeTable.DumpDay     WHEN NOT NEW DFTimeTable 
               DFTimeTable.DumpWeekDay WHEN NOT NEW DFTimeTable
               DFTimeTable.DumpTime
               llDumpMode  
               DFTimeTable.LastRun
               DFTimeTable.Ongoing
               DFTimeTable.FromDate
               DFTimeTable.ToDate
               DFTimeTable.FileNameTag
               DFTimeTable.UseReplica
            WITH FRAME lis EDITING:
 
               READKEY.
      
               IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
               DO WITH FRAME lis:
                  PAUSE 0.
               
                  IF FRAME-FIELD = "LastRun" THEN DO:
                     fLastRun(INPUT INPUT DFTimeTable.LastRun).
                     DISPLAY lcLastRun WITH FRAME lis.
                  END.
                  ELSE IF FRAME-FIELD = "Ongoing" THEN DO:
                     fOngoing(INPUT INPUT DFTimeTable.Ongoing).
                     DISPLAY lcOngoing WITH FRAME lis.
                  END.
               END.
            
               APPLY LASTKEY.
            END.
 
            IF DFTimeTable.DumpDay > "" AND
               DFTimeTable.DumpWeekDay > "" THEN DO:
               MESSAGE "You cannot use both weekday and month level"
               VIEW-AS ALERT-BOX INFORMATION.
               NEXT.
            END.
               
            FIND CURRENT DFTimeTable NO-LOCK.
         
            IF CURRENT-CHANGED DFTimeTable THEN DO:
               
               MESSAGE 
                  "This record has been changed elsewhere while updating" 
               VIEW-AS ALERT-BOX TITLE "UPDATE CANCELLED".

               UNDO UpdateField, LEAVE UpdateField.  

            END. 
            ELSE DO: 
            
               FIND CURRENT DFTimeTable EXCLUSIVE-LOCK.
       
               IF llDoEvent AND NOT NEW DFTimeTable THEN
                  RUN StarEventSetOldBuffer(lhDFTimeTable).
               
               ASSIGN
                  DFTimeTable.DumpDay     WHEN NOT NEW DFTimeTable 
                  DFTimeTable.DumpWeekDay WHEN NOT NEW DFTimeTable
                  DFTimeTable.DumpTime
                  DFTimeTable.LastRun
                  DFTimeTable.Ongoing
                  DFTimeTable.FromDate
                  DFTimeTable.ToDate
                  DFTimeTable.FileNameTag
                  DFTimeTable.UseReplica.
                              
               IF INPUT llDumpMode 
               THEN DFTimeTable.DumpMode = "Full".
               ELSE DFTimeTable.DumpMode = "Modified".
       
               IF llDoEvent AND NOT NEW DFTimeTable THEN
                  RUN StarEventMakeModifyEvent(lhDFTimeTable).
           
            END.
            
            FIND CURRENT DFTimeTable NO-LOCK.
            LEAVE UpdateField.
 
         END.
      
         IF llNew THEN LEAVE.   
      END.
         
      ELSE IF toimi = 3 THEN DO:
         RUN Syst/dftimetable_sim.p (RECID(DFTimeTable)).
      END.
      
      ELSE IF toimi = 8 THEN LEAVE.
         
   END.
   
END PROCEDURE.


