/* ----------------------------------------------------------------------
  MODULE .......: DFField.p
  TASK .........: UPDATEs table DFField
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 20.10.08
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{commali.i} 
{lib/tokenlib.i}
{lib/tokenchk.i 'DFField'}
{eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhDFField AS HANDLE NO-UNDO.
   lhDFField = BUFFER DFField:HANDLE.
   RUN StarEventInitialize(lhDFField).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhDFField).
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

DEF VAR lcStatus       AS CHAR NO-UNDO.
DEF VAR lcField        AS CHAR NO-UNDO. 
DEF VAR lcCode         AS CHAR NO-UNDO. 
DEF VAR lcValueType    AS CHAR NO-UNDO.
DEF VAR lcRequest      AS CHAR NO-UNDO.
DEF VAR lcSMSText      AS CHAR NO-UNDO.
DEF VAR liFieldOrder   AS INT  NO-UNDO.

FORM
    DFField.DFTable   FORMAT "X(20)"
    DFField.DFField   FORMAT "X(15)"
    DFField.DFLabel   FORMAT "X(15)"
    DFField.OrderNbr
    DFField.FromDate
    DFField.ToDate
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       " FIELDS FOR DUMP FILE " + STRING(iiDumpID) + " "
    FRAME sel.

FORM
    DFField.DumpID       COLON 15
       DumpFile.Description NO-LABEL SKIP(1)
    DFField.DFTable      COLON 15 
    DFField.DFField      COLON 15
    DFField.DFLabel      COLON 15
    DFField.OrderNbr     COLON 15
    DFField.FromDate     COLON 15
    DFField.ToDate       COLON 15
WITH  OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.


cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

FIND FIRST DumpFile WHERE DumpFile.DumpID = iiDumpID NO-LOCK NO-ERROR.
IF NOT AVAILABLE DumpFile THEN DO:
   MESSAGE "Dump file rule not available"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

RUN local-Find-First.

IF AVAILABLE DFField THEN ASSIGN
   Memory       = recid(DFField)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No fields available!" VIEW-AS ALERT-BOX INFORMATION.
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

   IF must-add THEN DO:  /* Add a DFField  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

      liFieldOrder = 0.
      FOR EACH DFField NO-LOCK WHERE
               DFField.DumpID = iiDumpID 
      BY DFField.OrderNbr DESC:
         liFieldOrder = DFField.OrderNbr.
         LEAVE.
      END.
      
      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis ALL NO-PAUSE.
        ehto = 9. RUN ufkey.

        REPEAT TRANS WITH FRAME lis:

           DISPLAY iiDumpID @ DFField.DumpID.

           CREATE DFField.
           ASSIGN 
              DFField.DumpID   = iiDumpID
              DFField.DFTable  = DumpFile.MainTable
              DFField.FromDate = TODAY
              DFField.ToDate   = 12/31/2049
              /* encourage the user to add new fields always to the end */
              liFieldOrder     = liFieldOrder + 1
              DFField.OrderNbr = liFieldOrder.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
              DFField.DFField = ""  THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhDFField).

           ASSIGN
           Memory = recid(DFField)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE DFField THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND DFField WHERE recid(DFField) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE DFField THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(DFField).
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
         
        RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW DFField.DFField ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) DFField.DFField WITH FRAME sel.
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
        FIND DFField WHERE recid(DFField) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE DFField THEN
              ASSIGN FIRSTrow = i Memory = recid(DFField).
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
           IF NOT AVAILABLE DFField THEN DO:
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
                rtab[1] = recid(DFField)
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
           IF NOT AVAILABLE DFField THEN DO:
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
              rtab[FRAME-DOWN] = recid(DFField).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND DFField WHERE recid(DFField) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE DFField THEN DO:
           Memory = recid(DFField).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE DFField THEN Memory = recid(DFField).
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
           FIND DFField WHERE recid(DFField) = Memory NO-LOCK.
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
          DFField.DFTable
          DFField.DFField
          DFField.DFLabel
          DFField.OrderNbr.

       RUN local-find-NEXT.
       IF AVAILABLE DFField THEN Memory = recid(DFField).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE DFField THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(DFField).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          DFField.DFTable
          DFField.DFField
          DFField.DFLabel
          DFField.OrderNbr.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhDFField).

           DELETE DFField.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE DFField THEN DO:
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
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhDFField).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSToDate */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhDFField).

       RUN local-disp-row.
       xrecid = recid(DFField).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(DFField) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(DFField) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

ehto = 4.
RUN ufkey.

fCleanEventObjects().



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND DFField WHERE recid(DFField) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND DFField WHERE recid(DFField) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN FIND FIRST DFField WHERE 
      DFField.DumpID = iiDumpID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN FIND LAST DFField WHERE 
      DFField.DumpID = iiDumpID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN FIND NEXT DFField WHERE 
      DFField.DumpID = iiDumpID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN FIND PREV DFField WHERE 
      DFField.DumpID = iiDumpID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       DFField.DFTable
       DFField.DFField
       DFField.DFLabel
       DFField.OrderNbr
       DFField.FromDate
       DFField.ToDate
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF VAR lcHelpTables AS CHAR NO-UNDO.
   DEF VAR liCnt        AS INT  NO-UNDO.
   
   lcHelpTables = DumpFile.MainTable.
   IF DumpFile.SideTables > "" THEN 
   DO liCnt = 1 TO NUM-ENTRIES(DumpFile.SideTables):
      lcHelpTables = lcHelpTables + "," + ENTRY(liCnt,DumpFile.SideTables).
   END.

   DEF BUFFER bField FOR DFField.

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      DISP 
         DFField.DumpID        
         DumpFile.Description
         DFField.DFTable
         DFField.DFField        
         DFField.DFLabel
         DFField.OrderNbr
         DFField.FromDate
         DFField.ToDate
      WITH FRAME lis.

      IF NOT NEW DFField THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7 WHEN lcRight = "RW"
            ufk[8] = 8
            ehto   = 0.
         
         RUN ufkey.
         
         IF toimi = 8 THEN LEAVE.
      END.

      UpdateField:
      REPEAT TRANS WITH FRAME lis ON ENDKEY UNDO, LEAVE:
                
         FIND CURRENT DFField EXCLUSIVE-LOCK.
         ehto = 9.
         RUN ufkey.
         
         UPDATE
            DFField.DFField   WHEN NEW DFField       
            DFField.DFLabel  
            DFField.OrderNbr
            DFField.FromDate
            DFField.ToDate
         WITH FRAME lis EDITING:
 
            READKEY.

            IF KEYLABEL(LASTKEY) = "F9" AND 
               LOOKUP(FRAME-FIELD,"DFField") > 0 
            THEN DO:

               IF FRAME-FIELD = "DFField" THEN DO:

                  RUN fieldselection (lcHelpTables,
                                      "SELECT FIELD",
                                      "",
                                      "",
                                      "",
                                      1,
                                      "hlt",
                                      OUTPUT lcCode).
                  IF lcCode ne "" AND lcCode NE ? THEN DO:
                     DISP ENTRY(1,lcCode,"|") ;& DFField.DFField 
                        WITH FRAME lis.
                     IF NUM-ENTRIES(lcCode,"|") > 1 THEN 
                        DISP ENTRY(2,lcCode,"|") ;& DFField.DFLabel
                           WITH FRAME lis.
                     IF NUM-ENTRIES(lcCode,"|") > 2 THEN DO:
                        DFField.DFTable = ENTRY(3,lcCode,"|").
                        DISP DFField.DFTable WITH FRAME lis.
                     END.   
                  END.
                     
               END.
             
               ehto = 9.
               RUN ufkey.
               NEXT. 
            END.

            ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
            DO WITH FRAME lis:
               PAUSE 0.
            END.
            
            APPLY LASTKEY.
         END.

         LEAVE UpdateField.
         
      END.
      
      IF NEW DFField THEN DO:
         liFieldOrder = DFField.OrderNbr.
         LEAVE.   
      END.
   END.
   
END PROCEDURE.

