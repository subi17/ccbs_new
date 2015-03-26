/* ----------------------------------------------------------------------
  MODULE .......: Campaign
  TASK .........: UPDATEs table Campaign
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 22.01.04
  CHANGED ......: 03.01.05/aam CampType
                  16.11.05/tk longer format for campaign name
  Version ......: M15
  ---------------------------------------------------------------------- */
&GLOBAL-DEFINE BrTable Campaign

{commali.i}

{eventval.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'Campaign'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhCampaign AS HANDLE NO-UNDO.
   lhCampaign = BUFFER Campaign:HANDLE.
   RUN StarEventInitialize(lhCampaign).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhCampaign).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR lcCampaign   AS CHAR                   NO-UNDO.
DEF VAR lcName       AS CHAR                   NO-UNDO. 
DEF VAR ldtDate      AS DATE                   NO-UNDO. 
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 3.
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
    Campaign.Brand     COLUMN-LABEL "Brand" FORMAT "X(5)"
    Campaign.Campaign    
    Campaign.CaName   
    Campaign.FromDate
    Campaign.ToDate      
    Campaign.CampType

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       " CAMPAIGNS "  + string(pvm,"99-99-99") + " "
    FRAME sel.

{brand.i}

form
    Campaign.Campaign    COLON 20   
    Campaign.CaName      COLON 20 FORMAT "x(55)"
    Campaign.FromDate    COLON 20
       VALIDATE(INPUT Campaign.FromDate NE ?,
                "Date is mandatory")
    Campaign.ToDate      COLON 20 
       VALIDATE(INPUT Campaign.ToDate >= INPUT Campaign.FromDate,
                "End date cannot be before beginning date")
    Campaign.CampType    COLON 20             
WITH  OVERLAY ROW 6 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form /* seek  Campaign */
    "Brand ..:" lcBrand skip
    "Campaign:" Campaign
    HELP "Enter campaign code"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND campaign "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  CaName */
    "Brand:" lcBrand skip
    "Name :" lcName
    HELP "Enter name"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND name "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

form /* seek  ToDate */
    "Brand ..:" lcBrand skip
    "End Date:" ldTDate FORMAT "99-99-99"
    HELP "Enter ending date"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Date "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f3.

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Campaign ," +
         "  By Name     ," +
         "  By Date     ,".


RUN local-find-first.

IF AVAILABLE Campaign THEN ASSIGN
   Memory       = recid(Campaign)
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

   IF must-add THEN DO:  /* Add a Campaign  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN ufkey.

        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE:

           PROMPT-FOR Campaign.Campaign.

           IF INPUT FRAME lis Campaign.Campaign = ""
           THEN LEAVE add-row.

           IF CAN-FIND(FIRST Campaign WHERE 
                       Campaign.Brand    = lcBrand AND
                       Campaign.Campaign = INPUT FRAME lis Campaign.Campaign)
           THEN DO:
              MESSAGE 
              "Campaign" 
              INPUT FRAME lis Campaign.Campaign 
              "already exists !"
              VIEW-AS ALERT-BOX
              ERROR.
              NEXT.
           END.

           CREATE Campaign.
           ASSIGN
           Campaign.Brand    = lcBrand
           Campaign.Campaign = INPUT FRAME lis Campaign.Campaign.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCampaign).

           ASSIGN
           Memory = recid(Campaign)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST Campaign WHERE Campaign.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Campaign THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND Campaign WHERE recid(Campaign) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE Campaign THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(Campaign).
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
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 28  ufk[4]= 1728
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0  ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
         RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW Campaign.Campaign ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) Campaign.Campaign WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW Campaign.CaName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) Campaign.CaName WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
        CHOOSE ROW Campaign.ToDate ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) Campaign.ToDate WITH FRAME sel.
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
        FIND Campaign WHERE recid(Campaign) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE Campaign THEN
              ASSIGN FIRSTrow = i Memory = recid(Campaign).
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
           IF NOT AVAILABLE Campaign THEN DO:
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
                rtab[1] = recid(Campaign)
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
           IF NOT AVAILABLE Campaign THEN DO:
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
              rtab[FRAME-DOWN] = recid(Campaign).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND Campaign WHERE recid(Campaign) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE Campaign THEN DO:
           Memory = recid(Campaign).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE Campaign THEN Memory = recid(Campaign).
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
           FIND Campaign WHERE recid(Campaign) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       UPDATE lcBrand WHEN gcAllBrand
              lcCampaign WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       IF lcCampaign > "" THEN DO:
          FIND FIRST Campaign WHERE 
                     Campaign.Brand     = lcBrand AND
                     Campaign.Campaign >= lcCampaign
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME F2.
       DISPLAY lcBrand WITH FRAME F2.
       UPDATE lcBrand WHEN gcAllBrand
              lcName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.

       IF lcName > "" THEN DO:
          FIND FIRST Campaign WHERE 
                     Campaign.Brand   = lcBrand AND
                     Campaign.CaName >= lcName
          USE-INDEX CaName  NO-LOCK NO-ERROR.

          IF NOT fRecFound(2) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* Search BY col 3 */
     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME F3.
       DISPLAY lcBrand WITH FRAME F3.
       UPDATE lcBrand WHEN gcAllBrand
              ldtDate WITH FRAME f3.
       HIDE FRAME f3 NO-PAUSE.

       IF ldtDate NE ? THEN DO:
          FIND FIRST Campaign WHERE 
                     Campaign.Brand   = lcBrand AND
                     Campaign.ToDate >= ldtDate
          USE-INDEX ToDate  NO-LOCK NO-ERROR.

          IF NOT fRecFound(3) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-3 */

     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:  /* rows */
       {uright2.i}
       RUN local-find-this (FALSE).
       IF AVAILABLE Campaign 
       THEN RUN camprow.p (Campaign.Campaign). 
       ufkey = true. 
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
       Campaign.Campaign Campaign.CaName .

       RUN local-find-NEXT.
       IF AVAILABLE Campaign THEN Memory = recid(Campaign).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE Campaign THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(Campaign).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       FOR FIRST CampRow OF Campaign NO-LOCK:
             MESSAGE "There are rows defined for this campaign."
                     "Deletion is not allowed."
             VIEW-AS ALERT-BOX
             ERROR.
             NEXT LOOP.
       END.

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       Campaign.Campaign Campaign.CaName .

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCampaign).

           DELETE Campaign.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST Campaign WHERE Campaign.Brand = lcBrand) 
           THEN DO:
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
       RUN local-find-this(IF lcRight = "RW" THEN TRUE ELSE FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCampaign).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY Campaign.Campaign.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCampaign).

       RUN local-disp-row.
       xrecid = recid(Campaign).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(Campaign) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(Campaign) must-print = TRUE.
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
      FIND Campaign WHERE recid(Campaign) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND Campaign WHERE recid(Campaign) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST Campaign 
          WHERE Campaign.Brand = lcBrand USE-INDEX Campaign
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST Campaign 
          WHERE Campaign.Brand = lcBrand USE-INDEX CaName
        NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND FIRST Campaign 
          WHERE Campaign.Brand = lcBrand USE-INDEX ToDate
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST Campaign
          WHERE Campaign.Brand = lcBrand USE-INDEX Campaign
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST Campaign 
          WHERE Campaign.Brand = lcBrand USE-INDEX CaName
        NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND LAST Campaign 
          WHERE Campaign.Brand = lcBrand USE-INDEX ToDate
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT Campaign
          WHERE Campaign.Brand = lcBrand USE-INDEX Campaign
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT Campaign 
          WHERE Campaign.Brand = lcBrand USE-INDEX CaName
        NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND NEXT Campaign 
          WHERE Campaign.Brand = lcBrand USE-INDEX ToDate
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV Campaign
          WHERE Campaign.Brand = lcBrand USE-INDEX Campaign
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV Campaign 
          WHERE Campaign.Brand = lcBrand USE-INDEX CaName
        NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND PREV Campaign 
          WHERE Campaign.Brand = lcBrand USE-INDEX ToDate
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       Campaign.Brand
       Campaign.Campaign 
       Campaign.CaName
       Campaign.FromDate
       Campaign.ToDate
       Campaign.CampType
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.

      DISP Campaign.Campaign
           Campaign.CaName
           Campaign.FromDate
           Campaign.ToDate
           Campaign.CampType
      WITH FRAME lis.
      
      IF lcRight = "RW" THEN DO:
      
         ehto = 9. RUN ufkey.
      
         UPDATE
         Campaign.CaName
         Campaign.FromDate
         Campaign.ToDate
         Campaign.CampType
         WITH FRAME lis.
      END.
      
      ELSE PAUSE MESSAGE "Press ENTER to continue".
      
      LEAVE.
   END.
   
END PROCEDURE.

