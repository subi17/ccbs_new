/* ----------------------------------------------------------------------
  MODULE .......: CampStat
  TASK .........: Browses table CampStat
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 22.01.04
  CHANGED ......: 
  Version ......: M15
  ---------------------------------------------------------------------- */
&GLOBAL-DEFINE BrTable CampStat

{Syst/commali.i}

{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'CampStat'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhCampStat AS HANDLE NO-UNDO.
   lhCampStat = BUFFER CampStat:HANDLE.
   RUN StarEventInitialize(lhCampStat).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhCampStat).
   END.

END.

DEF INPUT PARAMETER icCampaign AS CHAR NO-UNDO.
DEF INPUT PARAMETER iiCustNum  AS INT  NO-UNDO. 
DEF INPUT PARAMETER iCCLI      AS CHAR NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR lcCampaign   AS CHAR                   NO-UNDO.
DEF VAR lcCLI        AS CHAR                   NO-UNDO. 
DEF VAR liCustNum    AS INT                    NO-UNDO. 
DEF VAR ldtDate      AS DATE                   NO-UNDO. 
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


form
    CampStat.Brand     COLUMN-LABEL "Brand" FORMAT "X(5)"
    CampStat.Campaign    
    Campaign.CaName    COLUMN-LABEL "Camp.Name" FORMAT "X(12)"
    CampStat.CLI   
    CampStat.CustNum
    Customer.CustName  COLUMN-LABE "Cust.Name" FORMAT "X(16)"
    CampStat.CampDate      

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       " CAMPAIGN STATISTICS "  + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

form
    CampStat.Campaign   COLON 20   
       Campaign.CaName NO-LABEL FORMAT "X(30)"
    CampStat.CLI        COLON 20 
    CampStat.CustNum    COLON 20
       Customer.CustName NO-LABEL FORMAT "x(30)"
    CampStat.CampDate   COLON 20 
WITH  OVERLAY ROW 6 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form /* seek  CampStat */
    "Brand ..:" lcBrand skip
    "Campaign:" lcCampaign FORMAT "X(12)"
    HELP "Enter Campaign code"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Campaign "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  Cust */
    "Brand ..:" lcBrand skip
    "Customer:" liCustNum FORMAT ">>>>>>>9"
       HELP "Enter Customer"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Customer "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.


form /* seek  CLI */
    "Brand:" lcBrand skip
    "CLI .:" lcCLI FORMAT "X(15)"
    HELP "Enter CLI"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CLI "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f3.

form /* seek  CampDate */
    "Brand:" lcBrand skip
    "Date :" ldTDate FORMAT "99-99-99"
    HELP "Enter usage date"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Date "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f4.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Campaign ," +
         "  By Customer ," +
         "  By CLI      ," +
         "  By Date     ,".


IF icCampaign > "" THEN ASSIGN 
   MaxOrder = 2
   Order    = 2.
ELSE IF icCLI > "" THEN ASSIGN 
   MaxOrder = 1
   Order    = 4. 
ELSE IF iiCustNum > 0 THEN ASSIGN 
   MaxOrder = 1
   Order    = 4.
ELSE ASSIGN 
   MaxOrder = 3
   Order    = 1. 

RUN local-find-first.

IF AVAILABLE CampStat THEN ASSIGN
   Memory       = recid(CampStat)
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

   IF must-add THEN DO:  /* Add a CampStat  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE CampStat THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND CampStat WHERE recid(CampStat) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE CampStat THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(CampStat).
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
        ufk[1]= 1729  ufk[2]= 714 ufk[3]= 653  ufk[4]= 28
        ufk[5]= 0 
        ufk[6]= 0 
        ufk[7]= 0  ufk[8]= 8 ufk[9]= 1.

        IF icCampaign > "" THEN ASSIGN 
           ufk[1] = 0
           ufk[4] = 0.
        ELSE IF icCLI > "" OR iiCustNum > 0 THEN ASSIGN 
           ufk[1] = 0
           ufk[2] = 0
           ufk[3] = 0.
        ELSE ASSIGN 
           ufk[4] = 0.
        
        ASSIGN 
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW CampStat.Campaign ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) CampStat.Campaign WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW CampStat.CustNum ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) CampStat.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
        CHOOSE ROW CampStat.CLI ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) CampStat.CLI WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
        CHOOSE ROW CampStat.CampDate ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) CampStat.CampDate WITH FRAME sel.
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


      IF LOOKUP(nap,"cursor-right") > 0 AND MaxOrder > 1 THEN DO:
        order = order + 1. 
        IF order > maxOrder THEN order = IF icCampaign > "" 
                                         THEN 2
                                         ELSE 1.
      END.
      ELSE IF LOOKUP(nap,"cursor-left") > 0 AND MaxOrder > 1 THEN DO:
        order = order - 1. 
        IF icCampaign > "" AND order = 1 THEN order = MaxOrder.
        ELSE IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND CampStat WHERE recid(CampStat) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE CampStat THEN
              ASSIGN FIRSTrow = i Memory = recid(CampStat).
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
           IF NOT AVAILABLE CampStat THEN DO:
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
                rtab[1] = recid(CampStat)
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
           IF NOT AVAILABLE CampStat THEN DO:
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
              rtab[FRAME-DOWN] = recid(CampStat).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND CampStat WHERE recid(CampStat) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE CampStat THEN DO:
           Memory = recid(CampStat).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE CampStat THEN Memory = recid(CampStat).
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
           FIND CampStat WHERE recid(CampStat) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       UPDATE lcBrand WHEN gcAllBrand
              lcCampaign WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       IF lcCampaign > "" THEN DO:
          FIND FIRST CampStat WHERE 
                     CampStat.Brand     = lcBrand AND
                     CampStat.Campaign >= lcCampaign
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */
     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME F2.
       DISPLAY lcBrand WITH FRAME F2.
       UPDATE lcBrand WHEN gcAllBrand
              liCustNum WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.

       IF liCustNum > 0 THEN DO:
       
          IF icCampaign > "" THEN 
          FIND FIRST CampStat WHERE 
                     CampStat.Brand    = lcBrand AND
                     CampStat.Campaign = icCampaign AND
                     CampStat.CustNum >= liCustNum
          USE-INDEX Campaign NO-LOCK NO-ERROR.
          FIND FIRST CampStat WHERE 
                     CampStat.Brand = lcBrand AND
                     CampStat.CustNum >= liCustNum
          USE-INDEX CustNum  NO-LOCK NO-ERROR.

          IF NOT fRecFound(2) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* Search BY col 3 */
     ELSE IF LOOKUP(nap,"3,f3") > 0 AND ufk[3] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME F3.
       DISPLAY lcBrand WITH FRAME F3.
       UPDATE lcBrand WHEN gcAllBrand
              lcCLI WITH FRAME f3.
       HIDE FRAME f3 NO-PAUSE.

       IF lcCLI > "" THEN DO:
          IF icCampaign > "" THEN 
          FIND FIRST CampStat WHERE 
                     CampStat.Brand    = lcBrand    AND
                     CampStat.Campaign = icCampaign AND
                     CampStat.CLI >= lcCLI
          USE-INDEX CampCLI  NO-LOCK NO-ERROR.

          FIND FIRST CampStat WHERE 
                     CampStat.Brand = lcBrand AND
                     CampStat.CLI >= lcCLI
          USE-INDEX CLI  NO-LOCK NO-ERROR.

          IF NOT fRecFound(3) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-3 */

     /* Search BY col 4 */
     ELSE IF LOOKUP(nap,"4,f4") > 0 AND ufk[4] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME F4.
       DISPLAY lcBrand WITH FRAME F4.
       UPDATE lcBrand WHEN gcAllBrand
              ldtDate WITH FRAME f4.
       HIDE FRAME f4 NO-PAUSE.

       IF ldtDate NE ? THEN DO:
          IF icCLI > "" THEN 
          FIND FIRST CampStat WHERE 
                     CampStat.Brand   = lcBrand AND
                     CampStat.CLI     = icCLI   AND
                     CampStat.CampDate >= ldtDate
          USE-INDEX CLI  NO-LOCK NO-ERROR.
          ELSE
          FIND FIRST CampStat WHERE 
                     CampStat.Brand   = lcBrand   AND
                     CampStat.CustNum = iiCustNum AND
                     CampStat.CampDate >= ldtDate
          USE-INDEX CustNum  NO-LOCK NO-ERROR.

          IF NOT fRecFound(4) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-4 */
     
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(IF lcRight = "RW" THEN TRUE ELSE FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCampStat).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY CampStat.Campaign.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCampStat).

       RUN local-disp-row.
       xrecid = recid(CampStat).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(CampStat) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(CampStat) must-print = TRUE.
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
      FIND CampStat WHERE recid(CampStat) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND CampStat WHERE recid(CampStat) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST CampStat 
          WHERE CampStat.Brand = lcBrand USE-INDEX Campaign
          NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN DO:
          IF icCampaign > "" THEN 
          FIND FIRST CampStat 
             WHERE CampStat.Brand = lcBrand AND CampStat.Campaign = icCampaign
             USE-INDEX Campaign NO-LOCK NO-ERROR.
          ELSE    
          FIND FIRST CampStat 
             WHERE CampStat.Brand = lcBrand USE-INDEX CustNum
             NO-LOCK NO-ERROR.
       END.
       ELSE IF ORDER = 3 THEN DO:
          IF icCampaign > "" THEN 
          FIND FIRST CampStat 
             WHERE CampStat.Brand = lcBrand AND CampStat.Campaign = icCampaign
             USE-INDEX CampCLI NO-LOCK NO-ERROR.
          ELSE    
          FIND FIRST CampStat 
             WHERE CampStat.Brand = lcBrand USE-INDEX CLI
             NO-LOCK NO-ERROR.
       END.
       ELSE IF order = 4 THEN DO:
          IF icCLI > "" THEN 
          FIND FIRST CampStat 
             WHERE CampStat.Brand = lcBrand AND CampStat.CLI = icCLI
             USE-INDEX CLI NO-LOCK NO-ERROR.
          ELSE    
          FIND FIRST CampStat 
             WHERE CampStat.Brand = lcBrand AND CampStat.CustNum = iiCustNum
             USE-INDEX CustNum NO-LOCK NO-ERROR.
       END.
       
END PROCEDURE.

PROCEDURE local-find-LAST:

       IF order = 1 THEN FIND LAST CampStat 
          WHERE CampStat.Brand = lcBrand USE-INDEX Campaign
          NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN DO:
          IF icCampaign > "" THEN 
          FIND LAST CampStat 
             WHERE CampStat.Brand = lcBrand AND CampStat.Campaign = icCampaign
             USE-INDEX Campaign NO-LOCK NO-ERROR.
          ELSE    
          FIND LAST CampStat 
             WHERE CampStat.Brand = lcBrand USE-INDEX CustNum
             NO-LOCK NO-ERROR.
       END.
       ELSE IF ORDER = 3 THEN DO:
          IF icCampaign > "" THEN 
          FIND LAST CampStat 
             WHERE CampStat.Brand = lcBrand AND CampStat.Campaign = icCampaign
             USE-INDEX CampCLI NO-LOCK NO-ERROR.
          ELSE    
          FIND LAST CampStat 
             WHERE CampStat.Brand = lcBrand USE-INDEX CLI
             NO-LOCK NO-ERROR.
       END.
       ELSE IF order = 4 THEN DO:
          IF icCLI > "" THEN 
          FIND LAST CampStat 
             WHERE CampStat.Brand = lcBrand AND CampStat.CLI = icCLI
             USE-INDEX CLI NO-LOCK NO-ERROR.
          ELSE    
          FIND LAST CampStat 
             WHERE CampStat.Brand = lcBrand AND CampStat.CustNum = iiCustNum
             USE-INDEX CustNum NO-LOCK NO-ERROR.
       END.

END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT CampStat 
          WHERE CampStat.Brand = lcBrand USE-INDEX Campaign
          NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN DO:
          IF icCampaign > "" THEN 
          FIND NEXT CampStat 
             WHERE CampStat.Brand = lcBrand AND CampStat.Campaign = icCampaign
             USE-INDEX Campaign NO-LOCK NO-ERROR.
          ELSE    
          FIND NEXT CampStat 
             WHERE CampStat.Brand = lcBrand USE-INDEX CustNum
             NO-LOCK NO-ERROR.
       END.
       ELSE IF ORDER = 3 THEN DO:
          IF icCampaign > "" THEN 
          FIND NEXT CampStat 
             WHERE CampStat.Brand = lcBrand AND CampStat.Campaign = icCampaign
             USE-INDEX CampCLI NO-LOCK NO-ERROR.
          ELSE    
          FIND NEXT CampStat 
             WHERE CampStat.Brand = lcBrand USE-INDEX CLI
             NO-LOCK NO-ERROR.
       END.
       ELSE IF order = 4 THEN DO:
          IF icCLI > "" THEN 
          FIND NEXT CampStat 
             WHERE CampStat.Brand = lcBrand AND CampStat.CLI = icCLI
             USE-INDEX CLI NO-LOCK NO-ERROR.
          ELSE    
          FIND NEXT CampStat 
             WHERE CampStat.Brand = lcBrand AND CampStat.CustNum = iiCustNum
             USE-INDEX CustNum NO-LOCK NO-ERROR.
       END.
 END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV CampStat 
          WHERE CampStat.Brand = lcBrand USE-INDEX Campaign
          NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN DO:
          IF icCampaign > "" THEN 
          FIND PREV CampStat 
             WHERE CampStat.Brand = lcBrand AND CampStat.Campaign = icCampaign
             USE-INDEX Campaign NO-LOCK NO-ERROR.
          ELSE    
          FIND PREV CampStat 
             WHERE CampStat.Brand = lcBrand USE-INDEX CustNum
             NO-LOCK NO-ERROR.
       END.
       ELSE IF ORDER = 3 THEN DO:
          IF icCampaign > "" THEN 
          FIND PREV CampStat 
             WHERE CampStat.Brand = lcBrand AND CampStat.Campaign = icCampaign
             USE-INDEX CampCLI NO-LOCK NO-ERROR.
          ELSE    
          FIND PREV CampStat 
             WHERE CampStat.Brand = lcBrand USE-INDEX CLI
             NO-LOCK NO-ERROR.
       END.
       ELSE IF order = 4 THEN DO:
          IF icCLI > "" THEN 
          FIND PREV CampStat 
             WHERE CampStat.Brand = lcBrand AND CampStat.CLI = icCLI
             USE-INDEX CLI NO-LOCK NO-ERROR.
          ELSE    
          FIND PREV CampStat 
             WHERE CampStat.Brand = lcBrand AND CampStat.CustNum = iiCustNum
             USE-INDEX CustNum NO-LOCK NO-ERROR.
       END.
 END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.

       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       CampStat.Brand
       CampStat.Campaign 
       Campaign.CaName WHEN AVAILABLE Campaign
       "" WHEN NOT AVAILABLE Campaign @ Campaign.CaName 
       CampStat.CustNum
       Customer.CustName WHEN AVAILABLE Customer
       "" WHEN NOT AVAILABLE Customer @ Customer.CustName 
       CampStat.CLI
       CampStat.CampDate
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

   FIND Campaign OF CampStat NO-LOCK NO-ERROR.
   FIND Customer OF CampStat NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      DO WITH FRAME lis:
      
         IF AVAILABLE Campaign THEN DISPLAY Campaign.CaName.
         ELSE DISPLAY "" @ Campaign.CaName.
         
         IF AVAILABLE Customer THEN DISPLAY Customer.CustName.
         ELSE DISPLAY "" @ Customer.CustName.
      END.
         
      DISP CampStat.Campaign
           CampStat.CustNum
           CampStat.CLI
           CampStat.CampDate
      WITH FRAME lis.
      
      PAUSE MESSAGE "Press ENTER to continue".
      
      LEAVE.
   END.
   
END PROCEDURE.

