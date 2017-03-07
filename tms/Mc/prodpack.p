/* ----------------------------------------------------------------
  MODULE .......: ProdPack.P
  TASK .........: UPDATE ProdPacks
  APPLICATION ..: 
  AUTHOR .......: tk
  CREATED ......: 10-05-02
  CHANGED ......: 13-05-02 tk eventlogging added
                  21.05.02/tk invoice texts
                  26.02.03 tk tokens
                  06.02.04 jp custnum for memo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'prodpack'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhProdPack AS HANDLE NO-UNDO.
   lhProdPack = BUFFER ProdPack:HANDLE.
   RUN StarEventInitialize(lhProdPack).

   DEFINE VARIABLE lhPPItem AS HANDLE NO-UNDO.
   lhPPItem = BUFFER PPItem:HANDLE.
   RUN StarEventInitialize(lhPPItem).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhProdPack).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR ProdPack         LIKE ProdPack.ProdPack        NO-UNDO.
DEF VAR PPName       LIKE ProdPack.PPName      NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR MaxOrder     AS i                      NO-UNDO  init 2.
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
DEF VAR ok           AS log format "Yes/No"  NO-UNDO.

form
    ProdPack.ProdPack       /* COLUMN-LABEL FORMAT */
    ProdPack.PPName     FORMAT "x(35)"
    ProdPack.FeeModel
    FeeModel.FeeName      FORMAT "x(24)"
    /* sd */            /* COLUMN-LABEL FORMAT */
    WITH width 80 OVERLAY SCROLL 1 15 DOWN
    COLOR VALUE(cfc)
    title COLOR VALUE(ctc) " " + ynimi +
    " ProdPacks "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    ProdPack.ProdPack    LABEL "ProductPack ID"  SKIP
    ProdPack.PPName  LABEL "Name of Pack ."  SKIP
    ProdPack.FeeModel LABEL "Billing Event "
    FeeModel.FeeName   NO-LABEL                SKIP
    WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    ac-hdr WITH side-labels 
    FRAME lis.

form /* seek ProdPack  BY  ProdPack */
    ProdPack
    help "Enter ProdPack Code"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek ProdPack BY PPName */
    PPName
    help "Enter ProdPack Name"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND Name "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.

orders = "By Code,By Name,By 3, By 4".
FIND FIRST ProdPack
/* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE ProdPack THEN ASSIGN
   memory       = recid(ProdPack)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No prodpacks available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN
      memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order THEN DO:
       pr-order = order.
       PUT SCREEN ROW 19 col 37 entry(order,orders).
    END.

   IF must-add THEN DO:  /* Add a ProdPack  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.
ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 no-MESSAGE.
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.p.
        DO TRANSAction:
           PROMPT-FOR ProdPack.ProdPack
           VALIDATE
              (ProdPack.ProdPack = "" OR
              NOT CAN-FIND(ProdPack using  ProdPack.ProdPack),
              "ProdPack " + string(INPUT ProdPack.ProdPack) +
              " already exists !").
           IF input ProdPack.ProdPack = "" THEN LEAVE ADD-ROW.
           CREATE ProdPack.
           ASSIGN
           ProdPack.ProdPack = INPUT FRAME lis ProdPack.ProdPack.
           UPDATE
           ProdPack.PPName
           ProdPack.FeeModel
           WITH FRAME lis EDITING:
              READKEY.
              IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                 PAUSE 0.

                 IF FRAME-FIELD = "FeeModel" THEN DO:
                    FIND FeeModel WHERE FeeModel.FeeModel =
                                  INPUT ProdPack.FeeModel
                    NO-LOCK NO-ERROR.
                    IF NOT AVAIL FeeModel THEN DO:
                       BELL.
                       MESSAGE "Unknown Billing Event !".
                       NEXT.
                    END.
                    DISP FeeModel.FeeName.
                 END.
              END.
              APPLY LASTKEY.
           END. /* EDITING */
                  /* sd */
                  /* ld */.
           ASSIGN
           memory = recid(ProdPack)
           xrecid = memory.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhProdPack).

        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST ProdPack
      /* srule */ NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ProdPack THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND ProdPack WHERE recid(ProdPack) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ProdPack THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ProdPack).
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
        PAUSE 0 no-MESSAGE.

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
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 250 ufk[4]= 927
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 1760 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ProdPack.ProdPack {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ProdPack.ProdPack WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW ProdPack.PPName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ProdPack.PPName WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-LINE].
        FIND ProdPack WHERE recid(ProdPack) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-prev.
           IF AVAILABLE ProdPack THEN
              ASSIGN FIRSTrow = i memory = recid(ProdPack).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 no-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           FIND ProdPack WHERE recid(ProdPack) = rtab[FRAME-LINE] NO-LOCK.
           RUN local-find-prev.
           IF NOT AVAILABLE ProdPack THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* previous was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(ProdPack)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND ProdPack WHERE recid(ProdPack) = rtab[FRAME-DOWN] NO-LOCK .
           RUN local-find-NEXT.
           IF NOT AVAILABLE ProdPack THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT ROW was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(ProdPack).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* prev page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND ProdPack WHERE recid(ProdPack) = memory NO-LOCK NO-ERROR.
        RUN local-find-prev.
        IF AVAILABLE ProdPack THEN DO:
           memory = recid(ProdPack).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-prev.
              IF AVAILABLE ProdPack THEN memory = recid(ProdPack).
              ELSE RowNo = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* is this the very FIRST record of the table ?  */
           MESSAGE "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
        END.
     END. /* previous page */

     /* NEXT page */
     ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           memory = rtab[FRAME-DOWN].
           FIND ProdPack WHERE recid(ProdPack) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ProdPack = "".
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       UPDATE ProdPack WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF ProdPack <> "" THEN DO:
          FIND FIRST ProdPack WHERE ProdPack.ProdPack >= ProdPack
          /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ProdPack THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 no-MESSAGE.
             NEXT BROWSE.
          END.
          /* some prodpack/PpId was found */
          ASSIGN order = 1 memory = recid(ProdPack) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       PPName = "".
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       UPDATE PPName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF PPName <> "" THEN DO:
          FIND FIRST ProdPack WHERE ProdPack.PPName >= PPName
          USE-INDEX PPName /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ProdPack THEN DO:
             bell. MESSAGE "NOT FOUND !".
             PAUSE 1 no-MESSAGE.
             NEXT BROWSE.
          END.
          /* some prodpack/PpName was found */
          ASSIGN order = 2 memory = recid(ProdPack) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */


     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO TRANS:  /* Package Contains */

       FIND ProdPack WHERE recid(ProdPack) = rtab[FRAME-LINE] NO-LOCK.
       RUN Mc/ppcomp.p(ProdPack.ProdPack).
       ufkey = TRUE.
       NEXT LOOP.
     END.

     IF LOOKUP(nap,"4,F4") > 0 THEN DO TRANS: /* memo */
       FIND ProdPack WHERE RECID(ProdPack) = rtab[FRAME-LINE(sel)]
       NO-LOCK NO-ERROR.
       RUN Mc/memo.p(INPUT 0,
                INPUT "PRODPACK",
                INPUT STRING(ProdPack.ProdPack),
                INPUT "Prodpack Id").
       ufkey = TRUE.
       NEXT.
     END.

     IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSAction:  /* DELETE */
       delrow = FRAME-LINE.
       FIND ProdPack WHERE recid(ProdPack) = rtab[FRAME-LINE] NO-LOCK.

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
          ProdPack.ProdPack 
          ProdPack.PPName 
          ProdPack.FeeModel
          Feemodel.FeeName.

       RUN local-find-NEXT.
       IF AVAILABLE ProdPack THEN memory = recid(ProdPack).
       ELSE DO:
          /* read back the record that is TO be  removed */
          FIND ProdPack WHERE recid(ProdPack) = rtab[FRAME-LINE] NO-LOCK.
          /* THEN previous record */
          RUN local-find-PREV.


          IF AVAILABLE ProdPack THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(ProdPack).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       FIND ProdPack WHERE recid(ProdPack) = rtab[FRAME-LINE]
       EXCLUSIVE-LOCK.

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          ProdPack.ProdPack
          ProdPack.PPName
          ProdPack.FeeModel
          Feemodel.FeeName.

       IF ok THEN DO:
           IF CAN-FIND(FIRST PPItem OF ProdPack) THEN DO:
              ok = FALSE.           
              MESSAGE "This ProdPack contains products" SKIP
                      " - still delete ? "
              VIEW-AS ALERT-BOX
              BUTTONS YES-NO UPDATE ok.
           END.       
       END.

       IF ok THEN DO:

           FOR EACH PPItem OF ProdPack:

              IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhPPItem).

              DELETE PPItem.
           END.

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhProdPack).

           DELETE ProdPack.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST ProdPack
           /* srule */) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 no-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF lookup(nap,"7,f7") > 0 THEN DO:
        FIND ProdPack WHERE recid(ProdPack) = rtab[FRAME-line(sel)] NO-LOCK.
        RUN Mc/invotxt.p("ProdPack",ProdPack.ProdPack).
        ASSIGN memory = recid(ProdPack) must-print = TRUE ufkey=true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     DO WITH FRAME lis TRANSAction:
       {Syst/uright2.i}
       /* change */
       FIND ProdPack WHERE recid(ProdPack) = rtab[FRAME-line(sel)]
       EXCLUSIVE-LOCK.
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9.
       RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p.

       DISPLAY 
          ProdPack.ProdPack.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhProdPack).

       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhProdPack).

       RUN local-disp-row.
       xrecid = recid(CustPP).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,h") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(ProdPack) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"end,e") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(ProdPack) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.


PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST ProdPack
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST ProdPack USE-INDEX PPName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST ProdPack
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST ProdPack USE-INDEX PPName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT ProdPack
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT ProdPack USE-INDEX PPName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-prev:
       IF order = 1 THEN FIND prev ProdPack
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND prev ProdPack USE-INDEX PPName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-others:
    FIND FeeModel WHERE FeeModel.FeeModel = ProdPack.FeeModel NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-disp-row:

       RUN local-find-others.

       DISPLAY
       ProdPack.ProdPack 
       ProdPack.PPName
       ProdPack.FeeModel
       FeeModel.FeeName        WHEN     AVAIL FeeModel
       "!! UNKNOWN Event !!" WHEN NOT AVAIL FeeModel @ FeeModel.FeeName 

       WITH FRAME  sel.
END PROCEDURE.


PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP 
        ProdPack.PPName
        ProdPack.FeeModel
        FeeModel.FeeName        WHEN     AVAIL FeeModel
        "!! UNKNOWN Event !!" WHEN NOT AVAIL FeeModel @ FeeModel.FeeName
      WITH FRAME lis.
      IF lcRight = "RW" THEN DO:
         UPDATE
            ProdPack.PPName
            ProdPack.FeeModel
         WITH FRAME lis EDITING:
            READKEY.
            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
               PAUSE 0.

               IF FRAME-FIELD = "be-code" THEN DO:
                  FIND FeeModel WHERE FeeModel.FeeModel = 
                                      INPUT ProdPack.FeeModel
                  NO-LOCK NO-ERROR.
                  IF NOT AVAIL FeeModel THEN DO:
                     BELL.
                     MESSAGE "Unknown Billing Event !".
                     NEXT.
                  END.
                  DISP FeeModel.FeeName.
               END.
            END.
            APPLY LASTKEY.
         END. /* EDITING */
         LEAVE.
      END.
      ELSE PAUSE.
   END.  

END PROCEDURE.

