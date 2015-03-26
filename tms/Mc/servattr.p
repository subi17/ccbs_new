/*/----------------------------------------------------------------------
  MODULE .......: ServAttr.P
  TASK .........: Attributes of service components
  APPLICATION ..: nn
  AUTHOR .......: jp
  CREATED ......: 14.01.03 jp
  CHANGED ......: 06.02.04 jp custnum for memo
                  03.12.04/aam redundant fields removed,
                               DefValue added,
                               tokens & eventlog,
                               correct indexes
  Version ......: M15
  ---------------------------------------------------------------------- */

{commali.i}

{eventval.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'ServAttr'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhServAttr AS HANDLE NO-UNDO.
   lhServAttr = BUFFER ServAttr:HANDLE.
   RUN StarEventInitialize(lhServAttr).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhServAttr).
   END.
END.


DEF INPUT PARAMETER  icServCom LIKE ServAttr.ServCom No-UNDO.
DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR ServAttr  LIKE ServAttr.ServAttr  NO-UNDO.
DEF VAR SAName    LIKE ServAttr.SAName NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 4.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 9.
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
def var Memo-exists  AS log format "M/"        NO-UNDO.
DEF VAR llfind       AS LOG                    NO-UNDO.

form
    ServAttr.ServAttr    format "x(14)" 
    ServAttr.DefValue    
    ServAttr.SAName      column-label "Service Name" format "x(35)"
    ServAttr.ScChgable   column-label "ChgA" format "Y/N"
    ServAttr.FeeModel    column-label "FeeModel" format "x(7)"
WITH ROW FrmRow centered OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) 
    " Attributes of Service Component '" + icServCom  + "'"
    FRAME sel.

form
    ServAttr.Servcom  COLON 20                 SKIP
    ServAttr.ServAttr COLON 20 FORMAT "X(14)"  SKIP
    ServAttr.SAName   COLON 20 FORMAT "x(55)"  SKIP
    ServAttr.SCValueRange[1] COLON 20          SKIP
    ServAttr.SCValueRange[2] COLON 20
       VALIDATE (INPUT ServAttr.SCValueRange[2] >= 
                 input ServAttr.SCValueRange[1],
                 "Invalid Order !") SKIP
    ServAttr.DefValue  COLON 20 FORMAT "X(30)" SKIP      
    ServAttr.ScChgable COLON 20     SKIP
    ServAttr.FeeModel  COLON 20 
       FeeModel.FeeName NO-LABEL    SKIP
WITH  OVERLAY ROW 5 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form /* seek ServAttr  BY  ServAttr */
    ServAttr
    HELP "Enter Code of ServAttr"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek ServAttr  BY SAName */
    SAName
    HELP "Enter Name of ServAttr"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Name "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

FIND ServCom WHERE
     ServCom.Brand   = gcBrand AND
     ServCom.ServCom = icServCom NO-LOCK NO-ERROR.


cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Code,By Name,By 3, By 4".


RUN local-find-first.
IF AVAILABLE ServAttr THEN ASSIGN
   Memory       = recid(ServAttr)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE
   must-add     = (lcRight = "RW").

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 36 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a ServAttr  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           disp icServCom @ servAttr.ServCom WITH FRAME lis.
           PROMPT-FOR ServAttr.ServAttr
           VALIDATE
              (ServAttr.ServAttr NOT ENTERED OR
              NOT CAN-FIND(ServAttr using  ServAttr.ServAttr WHERE  
                           ServAttr.ServCom = icServCom AND 
                           ServAttr.Brand = gcBrand ),
              "ServAttr " + string(INPUT ServAttr.ServAttr) +
              " already exists !").
           IF INPUT FRAME lis ServAttr.ServAttr NOT ENTERED THEN 
           LEAVE add-row.
           CREATE ServAttr.
           ASSIGN
           ServAttr.ServCom  = icServCom  
           ServAttr.Brand    = gcBrand 
           ServAttr.ServAttr = INPUT FRAME lis ServAttr.ServAttr.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhServAttr).

           ASSIGN
           Memory = recid(ServAttr)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.
      /* is there ANY record ? */
      FIND FIRST ServAttr WHERE 
                 ServAttr.ServCom = icServCom AND 
                 ServAttr.Brand   = gcBrand 
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ServAttr THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ServAttr WHERE recid(ServAttr) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel: 
           IF AVAILABLE ServAttr THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ServAttr).
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
        ufk[1]= 35  
        ufk[2]= 30 
        ufk[3]= 0
        ufk[4]= 927
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        {uright1.i '"4,5,6"'}
        RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ServAttr.ServAttr ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ServAttr.ServAttr WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW ServAttr.SAName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ServAttr.SAName WITH FRAME sel.
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
        FIND ServAttr WHERE recid(ServAttr) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ServAttr THEN
              ASSIGN FIRSTrow = i Memory = recid(ServAttr).
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
           IF NOT AVAILABLE ServAttr THEN DO:
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
                rtab[1] = recid(ServAttr)
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
           IF NOT AVAILABLE ServAttr THEN DO:
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
              rtab[FRAME-DOWN] = recid(ServAttr).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ServAttr WHERE recid(ServAttr) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ServAttr THEN DO:
           Memory = recid(ServAttr).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ServAttr THEN Memory = recid(ServAttr).
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
           FIND ServAttr WHERE recid(ServAttr) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       SET ServAttr WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF ServAttr ENTERED THEN DO:
          FIND FIRST ServAttr WHERE 
                     ServAttr.Brand    = gcBrand AND 
                     ServAttr.ServAttr >= ServAttr AND 
                     ServAttr.ServCom   = icServCom NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ServAttr THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some ServAttr/ServAttr was found */
          ASSIGN order = 1 Memory = recid(ServAttr) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f2.
       SET SAName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF SAName ENTERED THEN DO:
          FIND FIRST ServAttr WHERE 
                     ServAttr.Brand   = gcBrand AND 
                     ServAttr.SAName >= SAName AND 
                     ServAttr.ServCom  = icServCom NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ServAttr THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some ServAttr/SAName was found */
          ASSIGN order = 2 Memory = recid(ServAttr) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* UPDATE memo */
     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:
        RUN local-find-this(FALSE).
        RUN memo(INPUT 0,
                 INPUT "ServAttr",
                 INPUT STRING(ServAttr.ServAttr),
                 INPUT "Service component").
        ufkey = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW"
     THEN DO:  /* add */
        {uright2.i}
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSACTION:  /* DELETE */
       {uright2.i}
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       ServAttr.ServAttr ServAttr.SAName .

       RUN local-find-NEXT.
       IF AVAILABLE ServAttr THEN Memory = recid(ServAttr).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE ServAttr THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(ServAttr).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       ServAttr.ServAttr ServAttr.SAName .
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhServAttr).

           DELETE ServAttr.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST ServAttr
           WHERE ServAttr.ServCom = icServCom AND 
                 ServAttr.Brand = gcBrand) THEN DO:
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
       RUN local-find-this((lcRight = "RW")).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhServAttr).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY ServAttr.ServAttr.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhServAttr).

       RUN local-disp-row.
       xrecid = recid(ServAttr).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ServAttr) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ServAttr) must-print = TRUE.
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
      FIND ServAttr WHERE recid(ServAttr) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ServAttr WHERE recid(ServAttr) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST ServAttr
       WHERE ServAttr.ServCom = icServCom AND 
             ServAttr.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST ServAttr USE-INDEX SAName
       WHERE ServAttr.ServCom = icServCom AND 
             ServAttr.Brand = gcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST ServAttr
       WHERE ServAttr.ServCom = icServCom AND ServAttr.Brand = gcBrand
       NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST ServAttr USE-INDEX SAName
       WHERE ServAttr.ServCom = icServCom AND ServAttr.Brand = gcBrand 
       NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT ServAttr
       WHERE ServAttr.ServCom = icServCom AND ServAttr.Brand = gcBrand 
       NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT ServAttr USE-INDEX SAName
       WHERE ServAttr.ServCom = icServCom AND ServAttr.Brand = gcBrand 
       NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV ServAttr
       WHERE ServAttr.ServCom = icServCom AND ServAttr.Brand = gcBrand 
       NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV ServAttr USE-INDEX SAName
       WHERE ServAttr.ServCom = icServCom AND ServAttr.Brand = gcBrand 
       NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       ServAttr.ServAttr
       ServAttr.SAName
       ServAttr.DefValue
       ServAttr.ServAttr
       ServAttr.ScChgable
       ServAttr.Feemodel 
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   FIND FIRST FeeModel WHERE 
              FeeModel.Brand = gcBrand AND 
              FeeModel.FeeModel = ServAttr.FeeModel NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP 
      ServAttr.ServCom
      ServAttr.SAName
      ServAttr.SCValueRange           
      ServAttr.DefValue
      ServAttr.ScChgable    
      ServAttr.FeeModel
      feemodel.feename when avail feemodel
      WITH FRAME lis.

      IF lcRight = "RW" THEN DO:
      
         ehto = 9.
         RUN ufkey.

      
         UPDATE
           ServAttr.SAName
           ServAttr.SCValueRange WHEN ServCom.ActType = 0          
           ServAttr.DefValue
           ServAttr.ScChgable
           ServAttr.FeeModel
         WITH FRAME lis
         EDITING:
             READKEY.
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.

                IF FRAME-FIELD = "FeeModel" AND 
                   INPUT FRAME lis ServAttr.Feemodel NE "" THEN DO:
                   FIND Feemodel WHERE 
                        FeeModel.Brand    = gcBrand AND 
                        FeeModel.FeeModel = INPUT FRAME lis ServAttr.FeeModel
                   NO-LOCK NO-ERROR.
                   IF NOT AVAIL FeeModel THEN DO:
                      BELL.
                      MESSAGE "Unknown FeeModel !".
                      NEXT.
                   END.
                   DISP FeeModel.Feename with frame lis.
                END.

             END.
             APPLY LASTKEY.
         END. /* EDITING */
      END.
      
      ELSE PAUSE MESSAGE "Press ENTER to continue".
      
      LEAVE.
   END.
END PROCEDURE.

