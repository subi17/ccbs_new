/* ----------------------------------------------------------------------
  MODULE .......: MXItem
  TASK .........: UPDATEs table MXItem
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 12012006
  CHANGED ......: 16.04.07/aam new parameters to tmscodesbr
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'mobsub'}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMXItem AS HANDLE NO-UNDO.
   lhMXItem = BUFFER MXItem:HANDLE.
   RUN StarEventInitialize(lhMXItem).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhMXItem).
   END.

END.

DEF  INPUT PARAMETER    iiMXSeq    AS INTEGER  NO-UNDO.

DEF  /* NEW */  shared VAR siirto AS CHAR.

DEF VAR MXItem       LIKE MXItem.MXSeq         NO-UNDO.
DEF VAR MXName       AS CHAR                   NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 2.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 13.
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
DEF VAR lcStatusName AS CHAR                   NO-UNDO FORMAT "X(10)" .
DEF VAR lcMXSeqName   AS CHAR                  NO-UNDO FORMAT "X(10)" .
DEF VAR lcMXResponse  AS CHAR                  NO-UNDO.
DEF VAR lcPrior       AS INT                   NO-UNDO FORMAT ">>9".


DEF BUFFER xxMXItem FOR MXItem  .

form
    MXItem.MXSeq       COLUMN-LABEL "MX Seq" FORMAT ">>>>9"
    lcMXSeqName        COLUMN-LABEL "Matrix Name"
    MXItem.MXName      COLUMN-LABEL "MX Item"
    MXItem.MXValue     COLUMN-LABEL "MX Value"
    MAtrix.MXres       COLUMN-LABEL "MX Res"
    lcPrior            COLUMN-LABEL "MX Pr"

WITH ROW FrmRow width 72 CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    "  ORDER FUNCTION ITEMS MENU  "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    MXItem.MXSeq     LABEL "MX Seq"
    lcMXSeqName      LABEL "MX Seq Name" FORMAT "x(256)" VIEW-AS FILL-IN SIZE 30 BY 1
    MXItem.MXName    LABEL "MX Name"     FORMAT "x(256)" VIEW-AS FILL-IN SIZE 30 BY 1
    MXItem.MXValue   LABEL "MX Value"    FORMAT "x(256)" VIEW-AS FILL-IN SIZE 30 BY 1
    Matrix.MXSeq     LABEL "MX Response"
    lcMXResponse     LABEL "Response Name"
    lcPrior          LABEL "PRI"

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Code  ,  By Name  ,By 3, By 4".


FIND FIRST MXItem WHERE 
           MXItem.MXSeq = iiMXSeq NO-LOCK NO-ERROR.
IF AVAILABLE MXItem THEN ASSIGN
   Memory       = recid(MXItem)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No Order function items available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a MXItem  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        REPEAT TRANSACTION WITH FRAME lis:

           CREATE MXItem.
           ASSIGN
           MXItem.MXSeq = iiMXSeq.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhMXItem).

           ASSIGN
           Memory = recid(MXItem)
           xrecid = Memory.
           LEAVE add-row.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST MXItem WHERE 
                 MXItem.MXSeq = iiMXSeq NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MXItem THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND MXItem WHERE recid(MXItem) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE MXItem THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(MXItem).
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
        ufk[1]= 0  ufk[2]= 0 ufk[3]= 0  ufk[4]= 2952
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW MXItem.MXSeq {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MXItem.MXSeq WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW MXItem.MXName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MXItem.MXName WITH FRAME sel.
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
        FIND MXItem WHERE recid(MXItem) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE MXItem THEN
              ASSIGN FIRSTrow = i Memory = recid(MXItem).
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
           IF NOT AVAILABLE MXItem THEN DO:
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
                rtab[1] = recid(MXItem)
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
           IF NOT AVAILABLE MXItem THEN DO:
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
              rtab[FRAME-DOWN] = recid(MXItem).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND MXItem WHERE recid(MXItem) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE MXItem THEN DO:
           Memory = recid(MXItem).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE MXItem THEN Memory = recid(MXItem).
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
           FIND MXItem WHERE recid(MXItem) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"4,f4") > 0 AND lcRight = "RW" THEN DO:  /* DELETE */
     
        RUN local-find-this (FALSE).
             
        RUN Mm/matrixview(input MXItem.MXSeq).
                     
        ASSIGN ufkey = TRUE.
                             
       RUN Syst/ufkey.
 
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       MXItem.MXSeq MXItem.MXName  MXItem.MXValue lcMXSeqname matrix.mxres
       lcPrior                 .

       RUN local-find-NEXT.
       IF AVAILABLE MXItem THEN Memory = recid(MXItem).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE MXItem THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(MXItem).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       MXItem.MXSeq MXItem.MXName MXItem.MXValue lcMXSeqname matrix.mxres
       lcPrior.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhMXItem).

           DELETE MXItem.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST MXItem
           WHERE MXItem.MXSeq = iiMXSeq) THEN DO:
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

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMXItem).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY MXItem.MXSeq.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMXItem).

       RUN local-disp-row.
       xrecid = recid(MXItem).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(MXItem) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(MXItem) must-print = TRUE.
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
      FIND MXItem WHERE recid(MXItem) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND MXItem WHERE recid(MXItem) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST MXItem
       WHERE MXItem.MXSeq = iiMXSeq NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST MXItem
       WHERE MXItem.MXSeq = iiMXSeq NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT MXItem
       WHERE MXItem.MXSeq = iiMXSeq NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV MXItem
       WHERE MXItem.MXSeq = iiMXSeq NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.

       DISPLAY 
       MXItem.MXSeq 
       MXItem.MXName
       MXItem.MXValue
       lcMXSeqname
       matrix.mxres
       lcPrior

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   FIND FIRST TMSCodes NO-LOCK WHERE 
              TMSCodes.TableName = "MATRIX" AND
              TMSCodes.FieldName = "MXName" AND
              TMSCodes.CodeGroup = "MXName" AND
              TMSCodes.CodeValue = MXItem.MXName NO-ERROR.
              
   IF Avail TMSCodes THEN  lcStatusName = TmsCodes.CodeName.
   ELSE                    lcStatusName = "UNKNOWN MXName".           

   FIND FIRST Matrix WHERE
              Matrix.MXSeq = MXItem.MXSeq NO-LOCK NO-ERROR.
              
   IF AVAIL MAtrix then DO:
      ASSIGN
      lcMXSeqname  = Matrix.MXname
      lcPrior      = MAtrix.Prior.
      
      FIND FIRST TMSCodes NO-LOCK WHERE
                 TMSCodes.TableName = "MATRIX" AND
                 TMSCodes.FieldName = "MXRes"  AND
                 TMSCodes.CodeGroup = "MXRes"  AND
                 TMSCodes.CodeValue = STRING(MAtrix.MXRes) NO-ERROR.

      IF Avail TMSCodes THEN lcMXResponse  = TMSCodes.CodeName.
      ELSE                   lcMXResponse  = "".
   END.
   ELSE ASSIGN lcMXSeqname =  "" lcMXResponse = "" lcPrior = MAtrix.Prior.     

END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
          MXItem.MXSeq
          MXItem.MXName
          lcMXSeqname 
          lcMXResponse
          MAtrix.MXseq
          MXItem.MXValue
          lcPrior
      WITH FRAME lis.
      IF lcRight = "RW" THEN DO:
         UPDATE
            MXItem.MXName 
            MXItem.MXValue
         WITH FRAME lis
         EDITING:
            READKEY.
            IF FRAME-FIELD = "MXName" AND keylabel(lastkey) = "F9" 
            THEN DO:
               
               RUN Mm/tmscodebr2(INPUT   "MATRIX",
                              INPUT   "MXName",
                              INPUT   "",
                              INPUT   "",
                              INPUT   "",
                              OUTPUT  siirto).
                                                             
               ASSIGN
                  MXItem.MXName = siirto.
               DISP MXItem.MXName WITH FRAME lis. PAUSE 0.
            END.
             
            ELSE IF FRAME-FIELD = "MXName" AND 
             lookup(keylabel(lastkey),poisnap) > 0 THEN DO:
               FIND FIRST TMSCodes NO-LOCK WHERE
                          TMSCodes.TableName = "MATRIX" AND
                          TMSCodes.FieldName = "MXName" AND
                          TMSCodes.CodeGroup = "MXName" AND
                          TMSCodes.CodeValue = INPUT MXItem.MXName   NO-ERROR.
              
               IF NOT AVAIL TMSCOdes THEN DO:
                  MESSAGE 
                  "Unknown Matrix Item Name:" INPUT MXitem.MXName
                  VIEW-AS ALERT-BOX.
                  NEXT-PROMPT MXName.
                  NEXT.
               END.
         
            END.
            APPLY lastkey.
          END.
       END.
       ELSE PAUSE.
    LEAVE.
   END.
END PROCEDURE.

