/* ----------------------------------------------------------------------
  MODULE .......: DumpFile
  TASK .........: UPDATEs table DumpFile
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 20.10.08
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable DumpFile

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'DumpFile'}
{Syst/eventval.i}

DEF BUFFER bItemValue FOR TMRItemValue.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhDumpFile AS HANDLE NO-UNDO.
   lhDumpFile = BUFFER DumpFile:HANDLE.
   RUN StarEventInitialize(lhDumpFile).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhDumpFile).
   END.

END.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR liDumpID      AS INT                    NO-UNDO.
DEF VAR lcDescription AS CHAR                   NO-UNDO.
DEF VAR xrecid        AS RECID                           init ?.
DEF VAR FIRSTrow      AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow        AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown       AS INT                    NO-UNDO  init 15.
DEF VAR order         AS INT                    NO-UNDO  init 1.
DEF VAR maxOrder      AS INT                    NO-UNDO  init 1.
DEF VAR ufkey         AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow        AS INT                    NO-UNDO  init 0.
DEF VAR pr-order      AS INT                    NO-UNDO.
DEF VAR Memory        AS RECID                  NO-UNDO.
DEF VAR RowNo         AS INT                    NO-UNDO.
DEF VAR must-print    AS LOG                    NO-UNDO.
DEF VAR must-add      AS LOG                    NO-UNDO.
DEF VAR ac-hdr        AS CHAR                   NO-UNDO.
DEF VAR rtab          AS RECID EXTENT 24        NO-UNDO.
DEF VAR i             AS INT                    NO-UNDO.
DEF VAR ok            AS log format "Yes/No"    NO-UNDO.

DEF VAR lcField         AS CHAR NO-UNDO. 
DEF VAR lcCode          AS CHAR NO-UNDO. 
DEF VAR lcSrcName       AS CHAR NO-UNDO.
DEF VAR lcCounterAmount AS CHAR NO-UNDO.
DEF VAR lcPeriodName    AS CHAR NO-UNDO.
DEF VAR liLength        AS INT  NO-UNDO.
DEF VAR llShowHistory AS LOG NO-UNDO INIT TRUE. 

FORM
    DumpFile.DumpName    FORMAT "X(15)"
    DumpFile.DumpID      FORMAT ">>>>9" COLUMN-LABEL "ID"
    DumpFile.Description FORMAT "X(15)"
    DumpFile.FileName    FORMAT "X(20)"
    DumpFile.MainTable   FORMAT "X(12)" 
    DumpFile.Active
WITH ROW FrmRow width 80 OVERLAY FrmDown DOWN 
    COLOR VALUE(Syst.Var:cfc)   
    TITLE COLOR VALUE(Syst.Var:ctc) " " + Syst.Var:ynimi +
       "  DUMP FILES  " + "  " +
       string(TODAY,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

FORM
    DumpFile.Brand          COLON 20
    DumpFile.BatchID        COLON 60
    DumpFile.DumpID         COLON 20
    DumpFile.DumpName       COLON 20 FORMAT "X(22)"
    DumpFile.Active         COLON 20
    DumpFile.FileCategory   COLON 20
    DumpFile.MainTable      COLON 20 
    DumpFile.FileName       COLON 20
    DumpFile.SpoolDir       COLON 20
    DumpFile.TransDir       COLON 20
    DumpFile.DumpFormat     COLON 20 
    DumpFile.DumpDelimiter  COLON 60 FORMAT "X(12)"
    DumpFile.EmptyFile      COLON 20 
    DumpFile.DecimalPoint   COLON 60 
    DumpFile.ModFromEventLog COLON 20 
    DumpFile.ModFromField    COLON 20 FORMAT "X(24)"
    DumpFile.AllowReplica    COLON 60  SKIP(1)
    DumpFile.Description VIEW-AS EDITOR SIZE 60 BY 3
WITH  OVERLAY ROW 1 centered
    COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FORM
    DumpFile.UseIndex       COLON 20 
    DumpFile.SideTables     COLON 20
    DumpFile.LinkKey        COLON 20  FORMAT "X(30)"
    DumpFile.LogicModule    COLON 20
       HELP "Module for collecting and dumping data"
    DumpFile.ModCollModule  COLON 20 
    DumpFile.FullCollModule COLON 20 
    DumpFile.ConfigParam    COLON 20 
       LABEL "Parameters" FORMAT "x(256)" VIEW-AS FILL-IN SIZE 30 BY 1 SKIP(1)
    DumpFile.LogFile        COLON 20 
    DumpFile.DumpLineFeed   COLON 20 
       HELP "Ascii codes of line feed characters, e.g. 13,10"
    DumpFile.DumpCharSet    COLON 20 
    DumpFile.EventLogFields COLON 20 
       VIEW-AS EDITOR SIZE 50 BY 3
WITH  OVERLAY ROW 2 centered COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc) " ADDITIONAL SETTINGS " SIDE-LABELS  FRAME fAddit.

FORM
    liLength                COLON 65 LABEL "Length" SKIP
    DumpFile.QueryClause VIEW-AS EDITOR SIZE 60 BY 12
WITH  OVERLAY ROW 3 centered COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc) " QUERY CLAUSE " SIDE-LABELS FRAME fQuery.


FORM 
    "Brand .....:" lcBrand skip
    "Description:" lcDescription FORMAT "X(20)" 
    HELP "Enter description"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND Description "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f1.


IF Syst.Var:gcHelpParam > "" THEN ASSIGN
   FrmRow  = 3
   FrmDown = 11.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.


RUN local-Find-First.

IF AVAILABLE DumpFile THEN ASSIGN
   Memory       = recid(DumpFile)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No rules available!" VIEW-AS ALERT-BOX.
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

   IF must-add THEN DO:  /* Add a DumpFile  */
      ASSIGN Syst.Var:cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        Syst.Var:ehto = 9. RUN Syst/ufkey.p.

        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.

           DISPLAY lcBrand @ DumpFile.Brand.

           PROMPT-FOR DumpFile.DumpName WITH FRAME lis.
           IF INPUT DumpFile.DumpName = "" THEN UNDO, LEAVE ADD-ROW.
           
           IF CAN-FIND(FIRST DumpFile WHERE 
                             DumpFile.Brand = lcBrand AND
                             DumpFile.DumpName = INPUT DumpFile.DumpName)
           THEN DO:
              MESSAGE "File configuration already exists"
              VIEW-AS ALERT-BOX ERROR.
              NEXT.
           END.
            
           FIND LAST DumpFile USE-INDEX DumpID NO-LOCK NO-ERROR.
           IF AVAILABLE DumpFile
           THEN i = DumpFile.DumpID + 1.
           ELSE i = 1.

           CREATE DumpFile.
           ASSIGN 
              DumpFile.Brand    = lcBrand
              DumpFile.DumpID   = i
              DumpFile.DumpName = INPUT FRAME lis DumpFile.DumpName
              DumpFile.ModFromEventLog = TRUE
              DumpFile.AllowReplica    = NO.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0
           THEN UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhDumpFile).

           ASSIGN
           Memory = recid(DumpFile)
           xrecid = Memory.  
           LEAVE ADD-ROW.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE DumpFile THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND DumpFile WHERE recid(DumpFile) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE DumpFile THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(DumpFile).
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
        Syst.Var:ufk    = 0
        Syst.Var:ufk[1] = 816
        Syst.Var:ufk[4] = 1984
        Syst.Var:ufk[5] = (IF lcRight = "RW" THEN 5 ELSE 0)  
        Syst.Var:ufk[6] = (IF lcRight = "RW" THEN 4 ELSE 0)  
        Syst.Var:ufk[7] = (IF llShowHistory THEN 46 ELSE 1828) 
        Syst.Var:ufk[8] = 8 
        Syst.Var:ehto   = 3 
        ufkey  = FALSE.
        
        /* used as help */
        IF Syst.Var:gcHelpParam > "" THEN ASSIGN
           Syst.Var:ufk[5] = 11
           Syst.Var:ufk[6] = 0
           Syst.Var:ufk[7] = 0.
         
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW DumpFile.DumpName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) DumpFile.DumpName WITH FRAME sel.
      END.

      Syst.Var:nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(Syst.Var:nap,"5,f5,8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.


      IF LOOKUP(Syst.Var:nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(Syst.Var:nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND DumpFile WHERE recid(DumpFile) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE DumpFile THEN
              ASSIGN FIRSTrow = i Memory = recid(DumpFile).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* PREVious ROW */
      IF LOOKUP(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE DumpFile THEN DO:
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
                rtab[1] = recid(DumpFile)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(Syst.Var:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE DumpFile THEN DO:
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
              rtab[FRAME-DOWN] = recid(DumpFile).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND DumpFile WHERE recid(DumpFile) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE DumpFile THEN DO:
           Memory = recid(DumpFile).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE DumpFile THEN Memory = recid(DumpFile).
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
     ELSE IF LOOKUP(Syst.Var:nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND DumpFile WHERE recid(DumpFile) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(Syst.Var:nap,"1,f1") > 0 AND Syst.Var:ufk[1] > 0 THEN 
     DO ON ENDKEY UNDO, NEXT LOOP:
       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       SET lcBrand WHEN Syst.Var:gcAllBrand 
           lcDescription WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       
       IF lcDescription > "" THEN DO:
          FIND FIRST DumpFile WHERE 
                     DumpFile.Brand = lcBrand AND
                     DumpFile.Description >= lcDescription
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(Syst.Var:nap,"4,f4") > 0 AND Syst.Var:ufk[4] > 0 THEN DO:
        RUN Syst/dftimetable_sim_all.p.
        ufkey = TRUE.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"5,f5") > 0 AND Syst.Var:ufk[5] > 0 THEN DO:  /* add */
        IF Syst.Var:gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           LEAVE LOOP.
        END.
        
        ELSE DO:
           must-add = TRUE.
           NEXT LOOP.
        END.    
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"6,f6") > 0 AND Syst.Var:ufk[6] > 0  
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       IF CAN-FIND(FIRST DFField WHERE DFField.DumpID = DumpFile.DumpID)
       THEN DO:
          MESSAGE "Field definitions exist. Delete not allowed."
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.
 
       IF CAN-FIND(FIRST DFTimeTable WHERE 
                         DFTimeTable.Brand  = Syst.Var:gcBrand AND 
                         DFTimeTable.DumpID = DumpFile.DumpID)
       THEN DO:
          MESSAGE "Timetable definitions exist. Delete not allowed."
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.

       /* Highlight */
       COLOR DISPLAY VALUE(Syst.Var:ctc)
       DumpFile.DumpID DumpFile.Description
       DumpFile.FileName DumpFile.MainTable.
        
       RUN local-find-NEXT.
       IF AVAILABLE DumpFile THEN Memory = recid(DumpFile).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE DumpFile THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(DumpFile).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(Syst.Var:ccc)
       DumpFile.DumpID DumpFile.Description
       DumpFile.FileName DumpFile.MainTable.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhDumpFile).

           DELETE DumpFile.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE DumpFile THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */
     
     ELSE IF Syst.Var:nap = "7" OR Syst.Var:nap = "f7" THEN DO:
        llShowHistory = NOT llShowHistory.
        CLEAR FRAME sel ALL no-pause.
        RUN local-find-first.
        ASSIGN
           memory = recid(DumpFile)
           ufkey = true
           must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis /*  TRANSACTION */
     ON ENDKEY UNDO, LEAVE:

       /* change */
       RUN local-find-this(FALSE).

       IF Syst.Var:gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhDumpFile).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE Syst.Var:ehto = 9. RUN Syst/ufkey.p.
       Syst.Var:cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY DumpFile.DumpID.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhDumpFile).

       RUN local-disp-row.
       xrecid = recid(DumpFile).
       LEAVE.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(DumpFile) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(DumpFile) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.

Syst.Var:ehto = 4.
RUN Syst/ufkey.p.

fCleanEventObjects().



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND DumpFile WHERE recid(DumpFile) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND DumpFile WHERE recid(DumpFile) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN 
      FIND FIRST DumpFile USE-INDEX DumpName WHERE 
                 DumpFile.Brand = lcBrand AND
                (IF llShowHistory THEN TRUE ELSE DumpFile.Active)
      NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN 
      FIND LAST DumpFile USE-INDEX DumpName WHERE 
                DumpFile.Brand = lcBrand AND
                (IF llShowHistory THEN TRUE ELSE DumpFile.Active)
                NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN 
      FIND NEXT DumpFile USE-INDEX DumpName WHERE 
                DumpFile.Brand = lcBrand AND
                (IF llShowHistory THEN TRUE ELSE DumpFile.Active)
                NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN 
      FIND PREV DumpFile USE-INDEX DumpName WHERE 
                DumpFile.Brand = lcBrand AND
                (IF llShowHistory THEN TRUE ELSE DumpFile.Active)
                NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       DumpFile.DumpID 
       DumpFile.DumpName
       DumpFile.Description
       DumpFile.FileName
       DumpFile.MainTable
       DumpFile.Active
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF VAR lcValue AS CHAR NO-UNDO.
   
   MaintMenu:
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      DISP 
         DumpFile.Brand          
         DumpFile.BatchID
         DumpFile.DumpID        
         DumpFile.DumpName
         DumpFile.Active          
         DumpFile.FileCategory              
         DumpFile.MainTable           
         DumpFile.FileName        
         DumpFile.SpoolDir         
         DumpFile.TransDir    
         DumpFile.DumpFormat
         DumpFile.DumpDelimiter        
         DumpFile.DecimalPoint       
         DumpFile.EmptyFile
         DumpFile.ModFromEventLog
         DumpFile.ModFromField
         DumpFile.AllowReplica
         DumpFile.Description   
      WITH FRAME lis.

      IF NEW DumpFile THEN Syst.Var:toimi = 1.

      ELSE DO:
         ASSIGN 
            Syst.Var:ufk    = 0
            Syst.Var:ufk[1] = 7    WHEN lcRight = "RW"
            Syst.Var:ufk[2] = 1985
            Syst.Var:ufk[3] = 1982
            Syst.Var:ufk[4] = 1983
            Syst.Var:ufk[5] = 9829
            Syst.Var:ufk[6] = 9847 WHEN DumpFile.DumpName BEGINS "HPD_"
            Syst.Var:ufk[8] = 8
            Syst.Var:ehto   = 0.
         
         RUN Syst/ufkey.p.
      END.
                  
      IF Syst.Var:toimi = 1 THEN 
      REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE MaintMenu:

         FIND CURRENT DumpFile EXCLUSIVE-LOCK.
      
         Syst.Var:ehto = 9.
         RUN Syst/ufkey.p.
   
         UPDATE
            DumpFile.BatchID
            DumpFile.Active          
            DumpFile.FileCategory
            DumpFile.MainTable           
            DumpFile.FileName        
            DumpFile.SpoolDir         
            DumpFile.TransDir       
            DumpFile.DumpFormat
            DumpFile.DumpDelimiter        
            DumpFile.EmptyFile
            DumpFile.DecimalPoint       
            DumpFile.ModFromEventLog
            DumpFile.ModFromField
            DumpFile.AllowReplica
            DumpFile.Description
         WITH FRAME lis EDITING:
 
            READKEY.

            IF KEYLABEL(LASTKEY) = "F9" AND 
               LOOKUP(FRAME-FIELD,
                 "FileCategory,DumpFormat,MainTable,ModFromField,DecimalPoint") 
               > 0 
            THEN DO:

               IF FRAME-FIELD = "MainTable" THEN DO:
                  siirto = "".
                  RUN Syst/table_help.p.
                  IF siirto > "" THEN 
                     DISP siirto @ DumpFile.MainTable WITH FRAME lis.
               END.
                  
               ELSE IF FRAME-FIELD = "FileCategory" THEN DO:
                  RUN Help/h-tmscodes.p(INPUT "DumpFile",     /* TableName */
                                       "FileCategory",  /* FieldName */
                                       "DumpFile",     /* GroupCode */
                                OUTPUT lcCode).
              
                  IF lcCode ne "" AND lcCode NE ? THEN DO:
                     DISPLAY lcCode ;& DumpFile.FileCategory
                     WITH FRAME lis.   
                  END.
               END.
 
               ELSE IF FRAME-FIELD = "DumpFormat" THEN DO:
                  RUN Help/h-tmscodes.p(INPUT "DumpFile",     /* TableName */
                                       "DumpFormat",  /* FieldName */
                                       "DumpFile",     /* GroupCode */
                                OUTPUT lcCode).
              
                  IF lcCode ne "" AND lcCode NE ? THEN DO:
                     DISPLAY lcCode ;& DumpFile.DumpFormat WITH FRAME lis.   
                  END.
               END.
 
               ELSE IF FRAME-FIELD = "DecimalPoint" THEN DO:
                  RUN Help/h-tmscodes.p(INPUT "DumpFile",     /* TableName */
                                       "DecimalPoint",  /* FieldName */
                                       "DumpFile",     /* GroupCode */
                                OUTPUT lcCode).
              
                  IF lcCode NE ? THEN DO:
                     DISPLAY lcCode ;& DumpFile.DecimalPoint WITH FRAME lis.   
                  END.
               END.
           
               ELSE IF FRAME-FIELD = "ModFromField" THEN DO:
                  RUN Syst/fieldselection.p (INPUT INPUT FRAME lis DumpFile.MainTable,
                                      "SELECT FIELD",
                                      "",
                                      "",
                                      "",
                                      1,
                                      "h",
                                      OUTPUT lcCode).
          
                  IF lcCode NE "" AND lcCode NE ? THEN DO:
                     IF INPUT FRAME lis DumpFile.ModFromField > ""
                     THEN lcCode = INPUT FRAME lis DumpFile.ModFromField + 
                                   "," + lcCode.
                     DISP lcCode ;& DumpFile.ModFromField WITH FRAME lis.
                  END.
               END.
                
               Syst.Var:ehto = 9.
               RUN Syst/ufkey.p.

               NEXT. 
            END.

            ELSE IF LOOKUP(KEYLABEL(LASTKEY),Syst.Var:poisnap) > 0 THEN 
            DO WITH FRAME lis:
               PAUSE 0.

               IF FRAME-FIELD = "FileCategory" THEN DO:
                  IF Func.Common:mTMSCodeName("DumpFile",
                                      "FileCategory",
                                      INPUT INPUT DumpFile.FileCategory) = ""
                  THEN DO:
                     MESSAGE "Unknown category"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.

               ELSE IF FRAME-FIELD = "DumpFormat" THEN DO:
                  IF Func.Common:mTMSCodeName("DumpFile",
                                      "DumpFormat",
                                      INPUT INPUT DumpFile.DumpFormat) = ""
                  THEN DO:
                     MESSAGE "Unknown format"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.

               ELSE IF FRAME-FIELD = "DecimalPoint" THEN DO:
                  IF Func.Common:mTMSCodeName("DumpFile",
                                      "DecimalPoint",
                                      INPUT INPUT DumpFile.DecimalPoint) = ""
                  THEN DO:
                     MESSAGE "Invalid decimal point"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.
            END.
            
            APPLY LASTKEY.
         END.
   
         IF NEW DumpFile THEN LEAVE MaintMenu.
         LEAVE.
      END.

      ELSE IF Syst.Var:toimi = 2 THEN REPEAT WITH FRAME fAddit:
      
         PAUSE 0.
         DISPLAY        
            DumpFile.UseIndex
            DumpFile.SideTables
            DumpFile.LinkKey
            DumpFile.LogicModule
            DumpFile.ModCollModule
            DumpFile.FullCollModule
            DumpFile.ConfigParam
            DumpFile.LogFile
            DumpFile.DumpLineFeed
            DumpFile.DumpCharSet
            DumpFile.EventLogFields
         WITH FRAME fAddit.
         
         ASSIGN 
            Syst.Var:ufk    = 0
            Syst.Var:ufk[1] = 7    WHEN lcRight = "RW"
            Syst.Var:ufk[4] = 1986 
            Syst.Var:ufk[8] = 8
            Syst.Var:ehto   = 0.
         
         RUN Syst/ufkey.p.
      
         IF Syst.Var:toimi = 1 THEN 
         REPEAT WITH FRAME fAddit ON ENDKEY UNDO, LEAVE MaintMenu:

            FIND CURRENT DumpFile EXCLUSIVE-LOCK.
      
            Syst.Var:ehto = 9.
            RUN Syst/ufkey.p.
   
            UPDATE
               DumpFile.UseIndex 
               DumpFile.SideTables
               DumpFile.LinkKey
               DumpFile.LogicModule
               DumpFile.ModCollModule
               DumpFile.FullCollModule
               DumpFile.ConfigParam
               DumpFile.LogFile
               DumpFile.DumpLineFeed
               DumpFile.DumpCharSet
               DumpFile.EventLogFields
            WITH FRAME fAddit EDITING:
 
               READKEY.

               IF KEYLABEL(LASTKEY) = "F9" AND 
                  LOOKUP(FRAME-FIELD,"SideTables,LinkKey,EventLogFields") > 0 
               THEN DO:

                  IF FRAME-FIELD = "SideTables" THEN DO:
                     siirto = "".
                     RUN Syst/table_help.p.
                     IF siirto > "" THEN DO:
                        IF INPUT FRAME fAddit DumpFile.SideTables > ""
                        THEN siirto = INPUT FRAME fAddit DumpFile.SideTables +
                                      "," + siirto.
                        DISP siirto @ DumpFile.SideTables WITH FRAME fAddit.
                     END.   
                  END.
                  
                  ELSE IF FRAME-FIELD = "LinkKey" THEN DO:
                     RUN Syst/fieldselection.p (DumpFile.MainTable,
                                         "SELECT FIELD",
                                         "",
                                         "",
                                         "",
                                         1,
                                         "h",
                                         OUTPUT lcCode).
          
                     IF lcCode ne "" AND lcCode NE ? THEN DO:
                        DISP lcCode ;& DumpFile.LinkKey WITH FRAME fAddit.
                     END.
                  END.

                  ELSE IF FRAME-FIELD = "EventLogFields" THEN DO:
                     RUN Syst/fieldselection.p (DumpFile.MainTable,
                                         "SELECT FIELD",
                                         "",
                                         "",
                                         "",
                                         1,
                                         "h",
                                         OUTPUT lcCode).
          
                     IF lcCode NE "" AND lcCode NE ? THEN DO:
                        IF INPUT FRAME fAddit DumpFile.EventLogField > "" THEN
                           lcCode = INPUT FRAME fAddit DumpFile.EventLogField + 
                                    "," + lcCode.
                        DumpFile.EventLogField = lcCode.            
                        DISP DumpFile.EventLogField WITH FRAME fAddit.
                     END.   
                  END.
                
                  Syst.Var:ehto = 9.
                  RUN Syst/ufkey.p.

                  NEXT. 
               END.

               ELSE IF LOOKUP(KEYLABEL(LASTKEY),Syst.Var:poisnap) > 0 THEN 
               DO WITH FRAME fAddit:
                  PAUSE 0.
               END.
            
               APPLY LASTKEY.
            END.
            
            LEAVE.
         END.
             
         ELSE IF Syst.Var:toimi = 4 THEN DO:

            PAUSE 0.
            DISPLAY        
               DumpFile.QueryClause
               LENGTH(DumpFile.QueryClause) @ liLength
            WITH FRAME fQuery.
         
            ASSIGN 
               Syst.Var:ufk    = 0
               Syst.Var:ufk[1] = 7    WHEN lcRight = "RW"
               Syst.Var:ufk[8] = 8
               Syst.Var:ehto   = 0.
         
            RUN Syst/ufkey.p.
   
            IF Syst.Var:toimi = 1 THEN 
            REPEAT WITH FRAME fQuery ON ENDKEY UNDO, LEAVE:

               FIND CURRENT DumpFile EXCLUSIVE-LOCK.
      
               Syst.Var:ehto = 9.
               RUN Syst/ufkey.p.
   
               UPDATE DumpFile.QueryClause WITH FRAME fQuery.
               LEAVE.
            END.
            
            HIDE FRAME fQuery NO-PAUSE.
            NEXT. 
         END.   
         
         HIDE FRAME fAddit NO-PAUSE.
         LEAVE.
      END.
      
      ELSE IF Syst.Var:toimi = 3 THEN DO:
         RUN Syst/dffield.p (DumpFile.DumpID).
      END.
 
      ELSE IF Syst.Var:toimi = 4 THEN DO:
         RUN Syst/dftimetable.p (DumpFile.DumpID).
      END.
      
      ELSE IF Syst.Var:toimi = 5 THEN DO:
         RUN Syst/dumplog.p (DumpFile.DumpID).
      END.

      ELSE IF Syst.Var:toimi = 6 AND Syst.Var:ufk[6] > 0
      THEN RUN Syst/dumphpd.p (DumpFile.DumpID).

      ELSE IF Syst.Var:toimi = 8 THEN LEAVE.  

   END.
   
END PROCEDURE.

