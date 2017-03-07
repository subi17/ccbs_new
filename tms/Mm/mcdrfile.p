/* ----------------------------------------------------------------------
  MODULE .......: MCDRFile.P
  TASK .........: UPDATE MCDRFileRecords
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 01-09-99
  CHANGED ......: 05-10-99 jp urights added
                  10.03.03 tk tokens, exit if no records available
                  08.10.03 jp frame lis
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'MCDRFile'}


IF gcAllBrand = FALSE THEN DO:
   MESSAGE
   "Only Super User can browse ALL cdr files!"
   VIEW-AS ALERT-BOX TITLE "ACCESS DENIED".
   NEXT.


END.



DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR Station  LIKE MCDRFile.Station  NO-UNDO.
DEF VAR FileSeq LIKE MCDRFile.FileSeq NO-UNDO.
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

form
    MCDRFile.Station  column-label "MTX" format "x(6)"  
    MCDRFile.FileSeq   
    MCDRFile.Station     /* COLUMN-LABEL FORMAT */
    MCDRFile.FromDate
    MCDRFile.ToDate     
    MCDRFile.RecordInQtu  format "zzz,zz9"  column-label "Calls"
    MCDRFile.FileName  format "x(22)"

             /* COLUMN-LABEL FORMAT */
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " Mobile CDR Files "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form

   mcdrfile.CMT   mcdrfile.Station   mcdrfile.FromDate   mcdrfile.fromTime    mcdrfile.ToDate   mcdrfile.ToTime    mcdrfile.FileNumber    mcdrfile.MTXID      mcdrfile.Qty     mcdrfile.RecordInQtu     mcdrfile.RecordQty  
   mcdrfile.BlockQty     mcdrfile.mc-seq    mcdrfile.FileName      mcdrfile.TaxRate    mcdrfile.Amount   mcdrfile.SPAmt    mcdrfile.FileSeq    mcdrfile.Soper     mcdrfile.Roper    mcdrfile.TaxTreat   mcdrfile.CrDate      mcdrfile.TransferDate    mcdrfile.CuttingTS   mcdrfile.UTC     mcdrfile.Version
   mcdrfile.AcCode    mcdrfile.Ccode    mcdrfile.Releas   mcdrfile.CrTime    mcdrfile.FileType 
   mcdrfile.TollFee   mcdrfile.CamelFee  


WITH  OVERLAY ROW 2 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    3 columns
    FRAME lis.

form /* seek Mobile CDR Sequence  BY  Station */
    station
    HELP "Enter Station"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND MTX Code "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek Mobile CDR Sequence  BY FileSeq */
    FileSeq
    HELP "Enter Sequence"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Sequence No. "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By MTX,By SEQ,By 3, By 4".


FIND FIRST MCDRFile
/* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE MCDRFile THEN ASSIGN
   Memory       = recid(MCDRFile)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE "No MCDRFiles available !" VIEW-AS ALERT-BOX.
   RETURN.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a MCDRFile  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.p.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR MCDRFile.Station
           VALIDATE
              (MCDRFile.Station NOT ENTERED OR
              NOT CAN-FIND(MCDRFile using  MCDRFile.Station),
              "Mobile CDR Sequence " + string(INPUT MCDRFile.Station) +
              " already exists !").
           IF INPUT FRAME lis MCDRFile.Station NOT ENTERED THEN 
           LEAVE add-row.
           CREATE MCDRFile.
           ASSIGN
           MCDRFile.Station = INPUT FRAME lis MCDRFile.Station.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           ASSIGN
           Memory = recid(MCDRFile)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST MCDRFile
      /* srule */ NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MCDRFile THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND MCDRFile WHERE recid(MCDRFile) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE MCDRFile THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(MCDRFile).
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
        ufk[1]= 35  ufk[2]= 266 ufk[3]= 0 ufk[4]= 0
        ufk[5]= 0 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW MCDRFile.Station {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MCDRFile.Station WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW MCDRFile.Station {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MCDRFile.Station WITH FRAME sel.
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
        FIND MCDRFile WHERE recid(MCDRFile) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE MCDRFile THEN
              ASSIGN FIRSTrow = i Memory = recid(MCDRFile).
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
           IF NOT AVAILABLE MCDRFile THEN DO:
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
                rtab[1] = recid(MCDRFile)
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
           IF NOT AVAILABLE MCDRFile THEN DO:
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
              rtab[FRAME-DOWN] = recid(MCDRFile).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND MCDRFile WHERE recid(MCDRFile) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE MCDRFile THEN DO:
           Memory = recid(MCDRFile).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE MCDRFile THEN Memory = recid(MCDRFile).
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
           FIND MCDRFile WHERE recid(MCDRFile) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       SET Station WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF Station ENTERED THEN DO:
          FIND FIRST MCDRFile WHERE MCDRFile.Station >= Station
          /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE MCDRFile THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some MCDRFile/Station was found */
          ASSIGN order = 1 Memory = recid(MCDRFile) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME F2.
       SET FileSeq WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF FileSeq ENTERED THEN DO:
          FIND FIRST MCDRFile WHERE MCDRFile.fileseq >= FileSeq
          USE-INDEX FileSeq /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE MCDRFile THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some MCDRFile/FileSeq was found */
          ASSIGN order = 2 Memory = recid(MCDRFile) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       MCDRFile.Station MCDRFile.Station .

       RUN local-find-NEXT.
       IF AVAILABLE MCDRFile THEN Memory = recid(MCDRFile).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE MCDRFile THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(MCDRFile).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       MCDRFile.Station MCDRFile.Station .
       IF ok THEN DO:

           DELETE MCDRFile.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST MCDRFile
           /* srule */) THEN DO:
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
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY MCDRFile.Station.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(MCDRFile).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(MCDRFile) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(MCDRFile) must-print = TRUE.
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
      FIND MCDRFile WHERE recid(MCDRFile) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND MCDRFile WHERE recid(MCDRFile) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST MCDRFile
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST MCDRFile USE-INDEX FileSeq
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST MCDRFile
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST MCDRFile USE-INDEX FileSeq
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT MCDRFile
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT MCDRFile USE-INDEX FileSeq
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV MCDRFile
       /* srule */ NO-LOCK NO-ERROR.            


       ELSE IF order = 2 THEN FIND PREV MCDRFile USE-INDEX FileSeq       
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       MCDRFile.Station
       MCDRFile.Station
       MCDRFile.FromDate
       MCDRFile.ToDate
       MCDRFile.RecordInQtu
       MCDRFile.FileSeq
       MCDRFile.FileName     WHEN NOT   MCDRFile.FileName BEGINS "gsm.domestic"
SUBSTR(MCDRFile.FileName,14) WHEN       MCDRFile.FileName BEGINS "gsm.domestic"
                                @       MCDRFile.FileName
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
   mcdrfile.CMT   mcdrfile.Station   mcdrfile.FromDate   mcdrfile.fromTime    mcdrfile.ToDate   mcdrfile.ToTime    mcdrfile.FileNumber    mcdrfile.MTXID      mcdrfile.Qty     mcdrfile.RecordInQtu     mcdrfile.RecordQty  
   mcdrfile.BlockQty     mcdrfile.mc-seq    mcdrfile.FileName      mcdrfile.TaxRate    mcdrfile.Amount   mcdrfile.SPAmt    mcdrfile.FileSeq    mcdrfile.Soper     mcdrfile.Roper    mcdrfile.TaxTreat   mcdrfile.CrDate      mcdrfile.TransferDate    mcdrfile.CuttingTS   mcdrfile.UTC     mcdrfile.Version
   mcdrfile.AcCode    mcdrfile.Ccode    mcdrfile.Releas   mcdrfile.CrTime    mcdrfile.FileType 
   mcdrfile.TollFee   mcdrfile.CamelFee 
      WITH FRAME lis.

MESSAGE "PRESS ENTER TO CONTINUE " . PAUSE NO-MESSAGE.

      LEAVE.
   END.
END PROCEDURE.

