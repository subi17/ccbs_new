/* ----------------------------------------------------------------------
  MODULE .......: FeeModel.P
  TASK .........: UPDATE Billing Events
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 28-09-99
  CHANGED ......: 04-10-99 jp urights added
                  04.11.99 pt F6 NOT allowed IF products assigned into Event
                  20.05.02/tk Event logging added
                  05.03.03 tk RUN Mc/memo,.p tokens
                  24.03.03 jp prompt-for not used 
                  05.09.03 aam brand 
                  06.02.04 jp input custnum for memo
  Version ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable FeeModel

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'feemodel'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhFeeModel AS HANDLE NO-UNDO.
   lhFeeModel = BUFFER FeeModel:HANDLE.
   RUN StarEventInitialize(lhFeeModel).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhFeeModel).
   END.

END.

DEF INPUT PARAMETER iigroup AS INT NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR FeeModel  LIKE FeeModel.FeeModel  NO-UNDO.
DEF VAR FeeName LIKE FeeModel.FeeName NO-UNDO.
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
DEF VAR memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.

form
    FeeModel.Brand 
    FeeModel.FeeModel   FORMAT "x(16)"
    FeeModel.FeeName    
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " Billing Events "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    FeeModel.Brand 
    FeeModel.FeeModel format "x(16)" 
    FeeModel.FeeName

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

{Func/brand.i}

form /* seek Billing Event  BY  FeeModel */
    "Brand:" lcBrand skip
    "Code :" FeeModel format "x(16)"
    HELP "Enter Billing Event Code"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek Billing Event  BY FeeName */
    "Brand:" lcBrand skip
    "Name :" FeeName
    HELP "Enter Billing Event Name"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Name "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.



form
    FeeModel.Memo

    WITH OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    " memo: " + FeeModel.FeeName + " " WITH NO-LABELS 1 columns
    FRAME f4.


cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "   By Code   ,   By Name   ,By 3, By 4".

 IF iigroup <> ? THEN
     FIND FIRST FeeModel WHERE FeeModel.Brand = lcBrand AND FeeModel.FMGroup = iigroup  NO-LOCK NO-ERROR.
 ELSE 
     FIND FIRST FeeModel WHERE FeeModel.Brand = lcBrand NO-LOCK NO-ERROR.

IF AVAILABLE FeeModel THEN ASSIGN
   memory       = recid(FeeModel)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No feemodels available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN
      memory       = ?
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

   IF must-add THEN DO:  /* Add a FeeModel  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.p.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           CREATE FeeModel.
           ASSIGN FeeModel.Brand = lcBrand
                  FeeModel.FMGroup = iigroup WHEN iigroup <> ? .
          
           RUN local-update-record.
           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.
           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhFeeModel).
           ASSIGN
           memory = recid(FeeModel)
           xrecid = memory.
           LEAVE ADD-ROW.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST FeeModel
      WHERE FeeModel.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE FeeModel THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND FeeModel WHERE recid(FeeModel) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE FeeModel THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(FeeModel).
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
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 294 ufk[4]= 927
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW FeeModel.FeeModel {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) FeeModel.FeeModel WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW FeeModel.FeeName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) FeeModel.FeeName WITH FRAME sel.
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
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-LINE].
        FIND FeeModel WHERE recid(FeeModel) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE FeeModel THEN
              ASSIGN FIRSTrow = i memory = recid(FeeModel).
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
           IF NOT AVAILABLE FeeModel THEN DO:
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
                rtab[1] = recid(FeeModel)
                memory  = rtab[1].
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
           IF NOT AVAILABLE FeeModel THEN DO:
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
              rtab[FRAME-DOWN] = recid(FeeModel).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND FeeModel WHERE recid(FeeModel) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE FeeModel THEN DO:
           memory = recid(FeeModel).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE FeeModel THEN memory = recid(FeeModel).
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
           memory = rtab[FRAME-DOWN].
           FIND FeeModel WHERE recid(FeeModel) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       UPDATE lcBrand WHEN gcAllBrand
              FeeModel WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       IF FeeModel ENTERED THEN DO:
          FIND FIRST FeeModel WHERE 
              FeeModel.Brand = lcBrand  AND
              FeeModel.FeeModel >= FeeModel
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME F2.
       DISPLAY lcBrand WITH FRAME F2.
       UPDATE lcBrand WHEN gcAllBrand
              FeeName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.

       IF FeeName ENTERED THEN DO:

          FIND FIRST FeeModel USE-INDEX FeeName WHERE 
             FeeModel.Brand = lcBrand AND
             FeeModel.FeeName >= FeeName
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(2) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-2 */


     /* UPDATE Event's Contents */
     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO:
        RUN local-find-this(FALSE).

        /* check if we are running for CCAdminTool */
       IF iigroup <> ? THEN DO:
       FIND TMSParam WHERE TMSParam.Brand = "1" AND
                           TMSParam.ParamGroup = "CCAdminTool" AND
                           TMSParam.ParamCode = "FMGroup" AND
                           TMSParam.IntVal = iigroup NO-LOCK NO-ERROR.
       IF AVAIL TMSParam THEN 
          RUN Mc/beitem-cc.p(INPUT FeeModel.FeeModel).

       END.
      
       ELSE
          RUN Mc/beitem.p(INPUT FeeModel.FeeModel).

        ufkey = TRUE.
        NEXT loop.
     END.

     /* UPDATE memo */
     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:

        RUN local-find-this(FALSE).
        RUN Mc/memo.p(INPUT 0,
                 INPUT "FeeModel",
                 INPUT STRING(FeeModel.FeeModel),
                 INPUT "FeeModel").
        ufkey = TRUE.
        ehto = 9.
        NEXT LOOP.

     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        {Syst/uright2.i}.
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW"  
     THEN DO TRANSAction:  /* DELETE */

       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       IF CAN-FIND(FIRST FMItem OF FeeModel) THEN DO:
          MESSAGE
          "There are products assigned into this" SKIP
          "event - remove them before deleting"   SKIP
          "the Event !"
          VIEW-AS ALERT-BOX ERROR.
          must-print = TRUE.
          memory = recid(FeeModel).
          delrow = 0.
          NEXT LOOP.
       END.


       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       FeeModel.FeeModel FeeModel.FeeName .

       RUN local-find-NEXT.
       IF AVAILABLE FeeModel THEN memory = recid(FeeModel).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE FeeModel THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(FeeModel).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       FeeModel.FeeModel FeeModel.FeeName .
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhFeeModel).

           DELETE FeeModel.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST FeeModel
           WHERE FeeModel.Brand = lcBrand) THEN DO:
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
       RUN local-find-this(FALSE).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY 
          FeeModel.Brand
          FeeModel.FeeModel
          FeeModel.FeeName.
       IF lcRight = "RW" THEN DO:

          RUN local-find-this(TRUE).
          IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFeeModel).

          RUN local-update-record.                                  

          IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFeeModel).

       END.   
       ELSE PAUSE.   
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(FeeModel).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(FeeModel) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(FeeModel) must-print = TRUE.
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
      FIND FeeModel WHERE recid(FeeModel) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND FeeModel WHERE recid(FeeModel) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
      IF iigroup <> ? THEN DO:

       IF order = 1 THEN FIND FIRST FeeModel
       WHERE FeeModel.Brand = lcBrand AND FeeModel.FMGroup = iigroup  NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST FeeModel USE-INDEX FeeName
       WHERE FeeModel.Brand = lcBrand AND FeeModel.FMGroup = iigroup NO-LOCK NO-ERROR.

      END.
      ELSE DO:

       IF order = 1 THEN FIND FIRST FeeModel
       WHERE FeeModel.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST FeeModel USE-INDEX FeeName
       WHERE FeeModel.Brand = lcBrand NO-LOCK NO-ERROR.
    /* ELSE IF order = 3 THEN FIND FIRST FeeModel USE-INDEX index-3
       WHERE FeeModel.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND FIRST FeeModel USE-INDEX index-4
       WHERE FeeModel.Brand = lcBrand NO-LOCK NO-ERROR.   */

     END.

END PROCEDURE.

PROCEDURE local-find-LAST:
     IF iigroup <> ? THEN DO:

        IF order = 1 THEN FIND LAST FeeModel
       WHERE FeeModel.Brand = lcBrand AND FeeModel.FMGroup = iigroup  NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST FeeModel USE-INDEX FeeName
       WHERE FeeModel.Brand = lcBrand AND FeeModel.FMGroup = iigroup NO-LOCK NO-ERROR.

     END.
     ELSE DO:

       IF order = 1 THEN FIND LAST FeeModel
       WHERE FeeModel.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST FeeModel USE-INDEX FeeName
       WHERE FeeModel.Brand = lcBrand NO-LOCK NO-ERROR.

     END.

END PROCEDURE.

PROCEDURE local-find-NEXT:
     IF iigroup <> ? THEN DO:

        IF order = 1 THEN FIND NEXT FeeModel
       WHERE FeeModel.Brand = lcBrand AND FeeModel.FMGroup = iigroup  NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT FeeModel USE-INDEX FeeName
       WHERE FeeModel.Brand = lcBrand AND FeeModel.FMGroup = iigroup NO-LOCK NO-ERROR.

     END.
     ELSE DO:

       IF order = 1 THEN FIND NEXT FeeModel
       WHERE FeeModel.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT FeeModel USE-INDEX FeeName
       WHERE FeeModel.Brand = lcBrand NO-LOCK NO-ERROR.

     END.
END PROCEDURE.

PROCEDURE local-find-PREV:

     IF iigroup <> ? THEN DO:

        IF order = 1 THEN FIND PREV FeeModel
       WHERE FeeModel.Brand = lcBrand AND FeeModel.FMGroup = iigroup  NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV FeeModel USE-INDEX FeeName
       WHERE FeeModel.Brand = lcBrand AND FeeModel.FMGroup = iigroup NO-LOCK NO-ERROR.

     END.
     ELSE DO:

       IF order = 1 THEN FIND PREV FeeModel
       WHERE FeeModel.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV FeeModel USE-INDEX FeeName
       WHERE FeeModel.Brand = lcBrand NO-LOCK NO-ERROR.

     END.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       FeeModel.Brand
       FeeModel.FeeModel
       FeeModel.FeeName

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP FeeModel.Brand
      WITH FRAME lis.
      UPDATE
          FeeModel.Feemodel WHEN NEW Feemodel
          FeeModel.FeeName


      WITH FRAME lis
      EDITING:
             READKEY.
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.
                IF FRAME-FIELD = "FeeModel" THEN DO:

                   IF INPUT FRAME lis FeeModel.FeeModel = "" THEN LEAVE.

                   IF CAN-FIND (FIRST Feemodel WHERE 
                                      FeeModel.Brand = lcBrand AND
                                      FeeModel.FeeModel =
                      INPUT FeeModel.FeeModel) THEN DO:

                      BELL.
                      MESSAGE "Billing Event " INPUT FRAME lis
                      FeeModel.FeeModel " already exists!".
                      NEXT.
                   END.

                END.
             END.
             APPLY LASTKEY.
          END. /* EDITING */

      LEAVE.
   END.
END PROCEDURE.

