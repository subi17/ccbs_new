/* ----------------------------------------------------------------------
  MODULE .......: FATGroup.P
  TASK .........: UPDATE FreeTime Group
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 28-09-99
  CHANGED ......: 05.11.02 jr Eventlog
                  15.01.03 Jippii fix
                  19.06.03/aam fatime instead of fatime1
                  15.09.03 jp  Brand 
                  29.11.04/aam AmtLimit
                  27.04.06 aam new fields; FatPerc, Amount, FatType etc.
  Version ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable fatgroup

{commali.i} 
{eventval.i} 
{lib/tokenlib.i}
{lib/tokenchk.i 'FatGroup'}

DEF NEW shared VAR siirto AS CHAR.

DEF VAR FATGroup   LIKE FATGroup.FTGrp  NO-UNDO.
DEF VAR FtgName LIKE FATGroup.FtgName NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 4.
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
DEF VAR lcFatType    AS CHAR                   NO-UNDO.
DEF VAR lcFatTarget  AS CHAR                   NO-UNDO.
DEF VAR lcQtyUnit    AS CHAR                   NO-UNDO.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhFATGroup AS HANDLE NO-UNDO.
   lhFATGroup = BUFFER FATGroup:HANDLE.
   RUN StarEventInitialize(lhFATGroup).

   ON F12 ANYWHERE 
   DO:
      RUN eventview2.p(lhFATGroup).
   END.
END.

form
    FATGroup.FTGrp      FORMAT "X(12)"
    FATGroup.FtgName    FORMAT "X(19)"
    FATGroup.BillCode
    FATGroup.FATType 
    FATGroup.Amount
    FATGroup.PeriodQty
    FATGroup.Priority
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " Free Air Time Groups "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

{brand.i}

form
    FATGroup.FTGrp       COLON 17 FORMAT "X(16)"
    FATGroup.FtgName     COLON 17 FORMAT "X(40)"
    FATGroup.BillCode    COLON 17
       BillItem.BIName NO-LABEL 
    FATGroup.FATType     COLON 17
       lcFATType NO-LABEL FORMAT "X(20)"
    FATGroup.FATTarget   COLON 17  FORMAT "X(2)"
       lcFatTarget NO-LABEL FORMAT "X(30)" 
    FATGroup.Transfer    COLON 17
    FATGroup.QtyUnit     COLON 17
       lcQtyUnit   NO-LABEL FORMAT "X(20)"
    FATGroup.Amount      COLON 17
    FATGroup.FatPerc     COLON 17
    FATGroup.PeriodQty   COLON 17
    FATGroup.Interval    COLON 17
    FATGroup.ValidPeriods COLON 17
    FATGroup.Priority    COLON 17
    FATGroup.AmtLimit    COLON 17
    FATGroup.InvMemo[1]  COLON 17 LABEL "Invoice Text" 
    FATGroup.InvMemo[2]  COLON 17 NO-LABEL 
    FATGroup.InvMemo[3]  COLON 17 NO-LABEL 
WITH  OVERLAY ROW 1 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form /* seek Billing Event  BY  FATGroup */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "FAT Group.:" FATGroup FORMAT "X(16)" HELP "Enter Fatime Group"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND GROUP "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek Billing Event  BY FtgName */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "Fat Name..:" Ftgname
    HELP "Enter Fatime Event Name"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Name "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.



FUNCTION fDispFATType RETURNS LOGICAL.

   lcFATType = "".
   IF FATGroup.FATType <= 3 THEN 
   lcFATType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                "FatGroup",
                                "FATType",
                                STRING(FATGroup.FATType)).

   DISPLAY lcFATType WITH FRAME lis.

END FUNCTION.

FUNCTION fDispFATTarget RETURNS LOGICAL.

   lcFATTarget = "".
   IF FATGroup.FATTarget <= "3" THEN 
   lcFATTarget = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                  "FatGroup",
                                  "FATTarget",
                                  FATGroup.FATTarget).

   DISPLAY lcFATTarget WITH FRAME lis.

END FUNCTION.

FUNCTION fDispQtyUnit RETURNS LOGICAL.

   lcQtyUnit = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                 "FatGroup",
                                 "QtyUnit",
                                 FATGroup.QtyUnit).

   DISPLAY lcQtyUnit WITH FRAME lis.

END FUNCTION.


cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "   By Code   ,   By Name   ,By 3, By 4".


FIND FIRST FATGroup WHERE 
           fatgroup.Brand = gcBrand NO-LOCK NO-ERROR.

IF AVAILABLE FATGroup THEN ASSIGN
   Memory       = recid(FATGroup)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE
   must-add     = TRUE.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a FATGroup  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR FATGroup.FTGrp
           VALIDATE
              (FATGroup.FTGrp NOT ENTERED OR
              NOT CAN-FIND(FATGroup using  FATGroup.FTGrp WHERE 
                           FATGroup.Brand = gcBrand ),
              "FreeAirTime Group " + string(INPUT FATGroup.FTGrp) +
              " already exists !").
           IF INPUT FRAME lis FATGroup.FTGrp NOT ENTERED THEN 
           LEAVE add-row.
           CREATE FATGroup.
           ASSIGN
           FatGroup.Brand = gcBrand 
           FATGroup.FTGrp = INPUT FRAME lis FATGroup.FTGrp.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhFATGroup).
           ASSIGN
           Memory = recid(FATGroup)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST FATGroup
      WHERE fatgroup.Brand = gcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE FATGroup THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND FATGroup WHERE recid(FATGroup) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE FATGroup THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(FATGroup).
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
        ufk[1]= 973 ufk[2]= 30 ufk[3]= 1452 ufk[4]= 1453
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 1069 
        ufk[8]= 8 
        ehto = 3 ufkey = FALSE.
        RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE. 
      IF order = 1 THEN DO:
        CHOOSE ROW FATGroup.FTgrp ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) FATGroup.FTgrp WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW FATGroup.FtgName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) FATGroup.FtgName WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
        CHOOSE ROW FATGroup.FatType ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) FATGroup.FatType WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
        CHOOSE ROW FATGroup.Priority  ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) FATGroup.Priority WITH FRAME sel.
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
        FIND FATGroup WHERE recid(FATGroup) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE FATGroup THEN
              ASSIGN FIRSTrow = i Memory = recid(FATGroup).
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
           IF NOT AVAILABLE FATGroup THEN DO:
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
                rtab[1] = recid(FATGroup)
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
           IF NOT AVAILABLE FATGroup THEN DO:
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
              rtab[FRAME-DOWN] = recid(FATGroup).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND FATGroup WHERE recid(FATGroup) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE FATGroup THEN DO:
           Memory = recid(FATGroup).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE FATGroup THEN Memory = recid(FATGroup).
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
           FIND FATGroup WHERE recid(FATGroup) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
        Disp lcBrand With FRAME f1.
       SET lcBrand WHEN gcAllBrand = TRUE
           FaTGroup WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF FATGroup ENTERED THEN DO:
          FIND FIRST FATGroup WHERE 
                     FATGroup.FTGrp >= FATGroup AND 
                     fatgroup.Brand = gcBrand 
          NO-LOCK NO-ERROR.

          IF NOT  fRecFound(1) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME F2.
       Disp lcBrand With FRAME f2.
       SET lcBrand WHEN gcAllBrand = TRUE
           FtgName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF FtgName ENTERED THEN DO:
          FIND FIRST FATGroup WHERE 
                     FATGroup.FtgName >= Ftgname AND 
                     fatgroup.Brand = gcBrand NO-LOCK NO-ERROR.

           IF NOT  fRecFound(2) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Search-2 */


     /* UPDATE members  */
     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO:
        RUN local-find-this(FALSE).
        RUN ftgmember1(INPUT FATGroup.FTGrp).
        ufkey = TRUE.
        NEXT loop.
     END.

     /* UPDATE FATime */
     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:
        RUN local-find-this(FALSE).
        RUN fatime (FATGroup.FTGrp,
                    0,
                    "",
                    0).
        ufkey = TRUE.
        NEXT loop.
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0 THEN DO:  /* add */
        {uright2.i}.
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0  THEN DO TRANSACTION:
       /* DELETE */
       {uright2.i}.
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       IF CAN-FIND(FIRST FATGMember OF FATGroup) THEN DO:
          MESSAGE
          "There are Products assigned into this" SKIP
          "group - remove them before deleting"   SKIP
          "the group !"
          VIEW-AS ALERT-BOX ERROR.
          NEXT LOOP.
       END.


       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       FATGroup.FTGrp FATGroup.FtgName .

       RUN local-find-NEXT.
       IF AVAILABLE FATGroup THEN Memory = recid(FATGroup).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE FATGroup THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(FATGroup).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       FATGroup.FTGrp FATGroup.FtgName .
       IF ok THEN DO:
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhFATGroup).
           DELETE FATGroup.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST FATGroup
           WHERE fatgroup.Brand = gcBrand) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"7,f7") > 0 AND ufk[7] > 0 THEN DO:
       {uright2.i}.
       RUN local-find-this (FALSE).
       
       ufkey = TRUE.
       IF AVAILABLE FatGroup THEN RUN fatconfig(FatGroup.FTGrp).
     END.

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       {uright2.i}
       RUN local-find-this(FALSE).
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY FATGroup.FTGrp.
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFATGroup).
       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFATGroup).
       RUN local-disp-row.
       xrecid = recid(FATGroup).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(FATGroup) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(FATGroup) must-print = TRUE.
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
      FIND FATGroup WHERE recid(FATGroup) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND FATGroup WHERE recid(FATGroup) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST FATGroup
       WHERE fatgroup.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST FATGroup USE-INDEX FtgName
       WHERE fatgroup.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND FIRST FATGroup USE-INDEX FATType
       WHERE fatgroup.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND FIRST FATGroup USE-INDEX Priority
       WHERE fatgroup.Brand = gcBrand NO-LOCK NO-ERROR. 
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST FATGroup
       WHERE fatgroup.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST FATGroup USE-INDEX FtgName
       WHERE fatgroup.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND LAST FATGroup USE-INDEX FATType
       WHERE fatgroup.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND LAST FATGroup USE-INDEX Priority
       WHERE fatgroup.Brand = gcBrand NO-LOCK NO-ERROR. 
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT FATGroup
       WHERE fatgroup.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT FATGroup USE-INDEX FtgName
       WHERE fatgroup.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND NEXT FATGroup USE-INDEX FATType
       WHERE fatgroup.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND NEXT FATGroup USE-INDEX Priority
       WHERE fatgroup.Brand = gcBrand NO-LOCK NO-ERROR.   
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV FATGroup
       WHERE fatgroup.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV FATGroup USE-INDEX FtgName
       WHERE fatgroup.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND PREV FATGroup USE-INDEX FATType
       WHERE fatgroup.Brand = gcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND PREV FATGroup USE-INDEX Priority
       WHERE fatgroup.Brand = gcBrand NO-LOCK NO-ERROR.   
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       FATGroup.FTGrp
       FATGroup.FtgName
       FATGroup.BillCode
       FATGroup.FATType 
       FATGroup.Amount
       FATGroup.PeriodQty
       FATGroup.Priority
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   FIND FIRST BillItem WHERE
              BillItem.Brand    = gcBrand AND 
              BillItem.BillCode = FATGroup.BillCode
   NO-LOCK NO-ERROR.           
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   MaintMenu:
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      DISP
          BillItem.BIName WHEN AVAIL BillItem
          FATGroup.FtgName
          FATGroup.BillCode
          FATGroup.FATType 
          FATGroup.FATTarget
          FATGroup.Transfer
          FATGroup.QtyUnit
          FATGroup.Amount
          FATGroup.FatPerc
          FATGroup.PeriodQty
          FATGroup.Interval
          FATGroup.ValidPeriods
          FATGroup.Priority
          FATGroup.AmtLimit 
          FATGroup.InvMemo[1 FOR 3]
      WITH FRAME lis.

      fDispFATType().
      fDispFatTarget().
      fDispQtyUnit().
     
      IF NEW FatGroup THEN toimi = 1.
      ELSE DO: 
         ASSIGN 
            ehto   = 0
            ufk    = 0
            ufk[1] = 7 WHEN lcRight = "RW" AND gcHelpParam = ""
            ufk[8] = 8.
         RUN ufkey.
      END.
      
      IF toimi = 1 THEN 
      REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE MaintMenu:

         FIND CURRENT FatGroup EXCLUSIVE-LOCK.
            
         ehto = 9.
         RUN ufkey.
 
      
         UPDATE
            FATGroup.FtgName
            FATGroup.BillCode
            FATGroup.FATType 
            FATGroup.FATTarget
            FATGroup.Transfer
            FATGroup.QtyUnit
            FATGroup.Amount
            FATGroup.FatPerc
            FATGroup.PeriodQty
            FATGroup.Interval
            FATGroup.ValidPeriods
            FATGroup.Priority
            FATGroup.AmtLimit 
            FATGroup.InvMemo[1 FOR 3]
         WITH FRAME lis EDITING:
      
             READKEY.

             IF KEYLABEL(LASTKEY) = "F9" AND 
                LOOKUP(FRAME-FIELD,"FatType,FatTarget,QtyUnit") > 0 
             THEN DO:

                RUN h-tmscodes(INPUT "FatGroup",  /* TableName*/
                                     FRAME-FIELD, /* FieldName */
                                     "FATime", /* GroupCode */
                               OUTPUT siirto).
                               
                IF siirto NE "" AND siirto NE ? THEN DO:
                   CASE FRAME-FIELD:
                   WHEN "FatType" THEN DO:
                      FATGroup.FATType = INTEGER(siirto).
                      fDispFATType().
                   END.
                   WHEN "FatTarget" THEN DO:
                      FATGroup.FatTarget = siirto.
                      fDispFatTarget().
                   END.
                   WHEN "QtyUnit" THEN DO:
                      FATGroup.QtyUnit = siirto.
                      fDispQtyUnit().
                   END.
                   END CASE.
                END.

                ehto = 9. 
                RUN ufkey.
             END.
             
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.
                IF FRAME-FIELD = "BillCode" THEN DO:
                   FIND BillItem WHERE 
                        BillItem.Brand    = lcBrand AND 
                        BillItem.BillCode =
                   INPUT FRAME lis FATGroup.BillCode NO-LOCK NO-ERROR.
                   IF NOT AVAIL BillItem THEN DO:
                      BELL.
                      MESSAGE "Unknown Billing Item !".
                      NEXT.
                   END.
                   DISP BillItem.BIName.
                END.
             END.

             ELSE IF FRAME-FIELD = "FatType" THEN DO:

                IF INPUT FRAME lis FATGroup.FATType = 2
                THEN DISPLAY "Amt" @ FATGroup.QtyUnit WITH FRAME lis. 

                ASSIGN FRAME lis FATGroup.FATType.
                fDispFATType().
             END.

             ELSE IF FRAME-FIELD = "FatTarget" THEN DO:
                ASSIGN FRAME lis FATGroup.FATTarget.
                fDispFATTarget().
             END.

             ELSE IF FRAME-FIELD = "QtyUnit" THEN DO:
                ASSIGN FRAME lis FATGroup.QtyUnit.
                fDispQtyUnit().
             END.

             ELSE IF FRAME-FIELD = "FatPerc" OR FRAME-FIELD = "Amount"
             THEN DO:
                IF INPUT FRAME lis FatGroup.Amount > 0 AND
                   INPUT FRAME lis FatGroup.FatPerc > 0
                THEN DO:
                   MESSAGE "You can't define both amount and percentage."
                   VIEW-AS ALERT-BOX ERROR.
                   NEXT.
                END.
             END.
                
             APPLY LASTKEY.
         END. /* EDITING */

         LEAVE.
      END.
      
      LEAVE.
   END.
END PROCEDURE.

