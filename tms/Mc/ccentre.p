/* ----------------------------------------------------------------------
  MODULE .......: ccentre.p
  TASK .........: UPDATEs table CostCentre
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 15.10.04
  CHANGED ......: 
  Version ......: M15
  ---------------------------------------------------------------------- */
&GLOBAL-DEFINE BrTable CostCentre

{Syst/commali.i}

{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'CostCentre'}


IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhCostCentre AS HANDLE NO-UNDO.
   lhCostCentre = BUFFER CostCentre:HANDLE.
   RUN StarEventInitialize(lhCostCentre).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhCostCentre).
   END.

END.

DEF shared VAR siirto AS CHAR.

DEF VAR lcCostCentre LIKE CostCentre.CostCentre    NO-UNDO. 
DEF VAR lcName       LIKE CostCentre.CCName        NO-UNDO.

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
    CostCentre.Brand      FORMAT "X(5)"  COLUMN-LABEL "Brand"
    CostCentre.CostCentre                COLUMN-LABEL "Cost Centre"
    CostCentre.CCName    
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(Syst.Var:cfc)   
    TITLE COLOR VALUE(Syst.Var:ctc) " " + Syst.Var:ynimi +
       " Cost Centres "  + string(TODAY,"99-99-99") + " "
    FRAME sel.

form
    CostCentre.CostCentre   COLON 20 SKIP
    CostCentre.CCName     COLON 20   
WITH  OVERLAY ROW 3 centered
    COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

{Func/brand.i}

form /* seek  CostCentre */
    "Brand:" lcBrand skip
    "Code :" lcCostCentre
    HELP "Enter Cost Centre"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND Code "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  CCName */
    "Brand:" lcBrand skip
    "Name :" lcName 
    HELP "Enter name"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND name "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f2.


Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.

orders = "By Code  ," +
         "By Name  ,".

RUN local-find-first.

IF AVAILABLE CostCentre THEN ASSIGN
   Memory       = recid(CostCentre)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE
   must-add     = FALSE.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a CostCentre  */
      ASSIGN Syst.Var:cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        Syst.Var:ehto = 9. RUN Syst/ufkey.p.

        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE ADD-ROW:

           PROMPT-FOR CostCentre.CostCentre.

           IF INPUT FRAME lis CostCentre.CostCentre = ""               
           THEN LEAVE add-row.

           IF CAN-FIND(FIRST CostCentre USING FRAME lis CostCentre.CostCentre
                        WHERE CostCentre.Brand = lcBrand)
           THEN DO:
              MESSAGE 
              "CostCentre"
              INPUT FRAME lis CostCentre.CostCentre
              "already exists !"
              VIEW-AS ALERT-BOX
              ERROR.
              NEXT.
           END.

           CREATE CostCentre.
           ASSIGN
           CostCentre.Brand    = lcBrand
           CostCentre.CostCentre = INPUT FRAME lis CostCentre.CostCentre.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCostCentre).

           ASSIGN
           Memory = recid(CostCentre)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST CostCentre WHERE CostCentre.Brand = Syst.Var:gcBrand 
       NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CostCentre THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND CostCentre WHERE recid(CostCentre) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE CostCentre THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(CostCentre).
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
        Syst.Var:ufk[1]= 35  Syst.Var:ufk[2]= 30  Syst.Var:ufk[3]= 0 Syst.Var:ufk[4]= 0
        Syst.Var:ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        Syst.Var:ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        Syst.Var:ufk[7]= 0 Syst.Var:ufk[8]= 8 Syst.Var:ufk[9]= 1
        Syst.Var:ehto = 3 ufkey = FALSE.
 
        /* used as help */
        IF Syst.Var:gcHelpParam > "" THEN ASSIGN
           Syst.Var:ufk[5] = 11
           Syst.Var:ufk[6] = 0
           Syst.Var:ufk[7] = 0.
 
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW CostCentre.CostCentre {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(Syst.Var:ccc) CostCentre.CostCentre WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW CostCentre.CCName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(Syst.Var:ccc) CostCentre.CCName WITH FRAME sel.
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
        FIND CostCentre WHERE recid(CostCentre) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE CostCentre THEN
              ASSIGN FIRSTrow = i Memory = recid(CostCentre).
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
           IF NOT AVAILABLE CostCentre THEN DO:
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
                rtab[1] = recid(CostCentre)
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
           IF NOT AVAILABLE CostCentre THEN DO:
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
              rtab[FRAME-DOWN] = recid(CostCentre).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND CostCentre WHERE recid(CostCentre) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE CostCentre THEN DO:
           Memory = recid(CostCentre).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE CostCentre THEN Memory = recid(CostCentre).
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
           FIND CostCentre WHERE recid(CostCentre) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(Syst.Var:nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       /* Search BY column 1 */
          Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
          Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
          CLEAR FRAME f1.
          DISPLAY lcBrand WITH FRAME F1.
          UPDATE lcBrand WHEN Syst.Var:gcAllBrand
                 lcCostCentre WITH FRAME f1.
          HIDE FRAME f1 NO-PAUSE.

          IF lcCostCentre NE "" THEN DO:
             FIND FIRST CostCentre WHERE 
                        CostCentre.Brand     = lcBrand AND
                        CostCentre.CostCentre >= lcCostCentre
             NO-LOCK NO-ERROR.

             IF NOT fRecFound(1) THEN NEXT BROWSE.

             NEXT LOOP.
          END.
     END. /* Search-1 */

     /* Search BY column 2 */
     ELSE IF LOOKUP(Syst.Var:nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
          Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
          Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
          CLEAR FRAME f2.
          DISPLAY lcBrand WITH FRAME F2.
          UPDATE lcBrand WHEN Syst.Var:gcAllBrand
                 lcName WITH FRAME f2.
          HIDE FRAME f2 NO-PAUSE.

          IF lcName > "" THEN DO:

             FIND FIRST CostCentre WHERE 
                        CostCentre.Brand    = lcBrand AND
                        CostCentre.CCName >= lcName
             USE-INDEX CCN NO-LOCK NO-ERROR.

             IF NOT fRecFound(2) THEN NEXT BROWSE.

             NEXT LOOP.
          END.
     END. /* Search-2 */

     ELSE IF LOOKUP(Syst.Var:nap,"5,f5") > 0 AND lcRight = "RW" AND Syst.Var:ufk[5] > 0
     THEN DO:  /* add */
        IF Syst.Var:gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           LEAVE LOOP.
        END.
        
        ELSE DO:
           {Syst/uright2.i}
           must-add = TRUE.
           NEXT LOOP.
        END.    
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"6,f6") > 0 AND lcRight = "RW" AND Syst.Var:ufk[6] > 0
     THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       FOR FIRST CCRule NO-LOCK WHERE
                 CCRule.Brand      = Syst.Var:gcBrand AND
                 CCRule.BillCode   > ""               AND 
                 CCRule.ValidTo    >= TODAY           AND 
                 CCRule.CostCentre = CostCentre.CostCentre:
           MESSAGE "Cost centre is used on BillCode:" 
                   CCRule.BillCode ". Delete not allowed."
                  VIEW-AS ALERT-BOX ERROR.
           NEXT LOOP.
       END.
       
       /* Highlight */
       COLOR DISPLAY VALUE(Syst.Var:ctc)
       CostCentre.CostCentre CostCentre.CCName.

       RUN local-find-NEXT.
       IF AVAILABLE CostCentre THEN Memory = recid(CostCentre).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE CostCentre THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(CostCentre).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(Syst.Var:ccc)
       CostCentre.CostCentre CostCentre.CCName.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCostCentre).

           DELETE CostCentre.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE CostCentre THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0
     THEN REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
 
        
       IF Syst.Var:gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
       
       /* change */
       RUN local-find-this(IF lcRight = "RW" THEN TRUE ELSE FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCostCentre).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       Syst.Var:cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY CostCentre.CCName.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCostCentre).

       RUN local-disp-row.
       xrecid = recid(CostCentre).
       LEAVE.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(CostCentre) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(CostCentre) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN DO:
        xrecid = ?.
        LEAVE LOOP.
     END.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.

IF Syst.Var:gcHelpParam > "" AND xrecid NE ? THEN DO:
   FIND CostCentre WHERE RECID(CostCentre) = xrecid NO-LOCK.
   siirto = CostCentre.CostCentre.
END.
ELSE siirto = ?.


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND CostCentre WHERE recid(CostCentre) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND CostCentre WHERE recid(CostCentre) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

       IF order = 1 THEN FIND FIRST CostCentre WHERE 
         CostCentre.Brand = lcBrand USE-INDEX CostCentre
       NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST CostCentre WHERE 
          CostCentre.Brand = lcBrand USE-INDEX CCN
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:

       IF order = 1 THEN FIND LAST CostCentre WHERE 
          CostCentre.Brand = lcBrand USE-INDEX CostCentre
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST CostCentre WHERE 
          CostCentre.Brand = lcBrand USE-INDEX CCN
        NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:

       IF order = 1 THEN FIND NEXT CostCentre WHERE 
          CostCentre.Brand = lcBrand USE-INDEX CostCentre
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT CostCentre WHERE 
          CostCentre.Brand = lcBrand USE-INDEX CCN
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:

       IF order = 1 THEN FIND PREV CostCentre WHERE 
          CostCentre.Brand = lcBrand USE-INDEX CostCentre
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV CostCentre WHERE 
          CostCentre.Brand = lcBrand USE-INDEX CCN
        NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       CostCentre.Brand
       CostCentre.CostCentre
       CostCentre.CCName    
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      DISP CostCentre.CostCentre
           CostCentre.CCName
      WITH FRAME lis.

      IF lcRight = "RW" THEN DO:
      
         Syst.Var:ehto = 9. RUN Syst/ufkey.p.
         
         UPDATE
         CostCentre.CCName
         WITH FRAME lis
         EDITING:

            READKEY.

             IF LOOKUP(KEYLABEL(LASTKEY),Syst.Var:poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.
             END.
             APPLY LASTKEY.
         END. /* EDITING */
      END.
      
      ELSE PAUSE MESSAGE "Press ENTER to continue".
      
      LEAVE.
   END.
END PROCEDURE.

