/* ----------------------------------------------------------------------
  MODULE .......: EDRHistory
  TASK .........: browse table EDRHistory
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 15.03.12
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable EDRHistory

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'EDRHistory'}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhEDRHistory AS HANDLE NO-UNDO.
   lhEDRHistory = BUFFER EDRHistory:HANDLE.
   RUN StarEventInitialize(lhEDRHistory).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhEDRHistory).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR lcCLI        LIKE EDRHistory.CLI     NO-UNDO.
DEF VAR ldaRateDate  AS DATE NO-UNDO.

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

DEF VAR lcTime       AS CHAR NO-UNDO.
DEF VAR lcUpdateTime AS CHAR NO-UNDO. 
DEF VAR lcCustName   AS CHAR NO-UNDO.
DEF VAR lcBIName     AS CHAR NO-UNDO. 
DEF VAR lcBDestName  AS CHAR NO-UNDO.

form
    EDRHistory.CLI    FORMAT "X(11)"
    EDRHistory.DateSt                     
    lcTime    FORMAT "X(8)" COLUMN-LABEL "Time" 
    EDRHistory.BillCode 
    EDRHistory.BDest  FORMAT "X(12)"
    EDRHistory.Amount FORMAT ">>>>9.999" COLUMN-LABEL "Amount"
    EDRHistory.UpdateDate
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(Syst.CUICommon:cfc)   
    TITLE COLOR VALUE(Syst.CUICommon:ctc) " " + Syst.CUICommon:ynimi +
       " EDR History "  + string(TODAY,"99-99-99") + " "
    FRAME sel.

form
    EDRHistory.Brand       COLON 20
    EDRHistory.InvCust     COLON 20 
       lcCustName NO-LABEL FORMAT "X(40)" SKIP
    EDRHistory.MsSeq       COLON 20 
    EDRHistory.CLI         COLON 20 SKIP(1)
    EDRHistory.DateSt      COLON 20 
       lcTime NO-LABEL FORMAT "X(8)" SKIP
    EDRHistory.DtlSeq      COLON 20 FORMAT ">>>>>>>>>>" SKIP(1)
    EDRHistory.BillCode    COLON 20
       lcBIName NO-LABEL FORMAT "X(30)" SKIP
    EDRHistory.BDest       COLON 20   
       lcBDestName NO-LABEL FORMAT "X(30)" SKIP
    EDRHistory.DCEvent     COLON 20
    EDRHistory.TariffNum   COLON 20
    EDRHistory.Amount      COLON 20 FORMAT ">>>>>9.9999"
    EDRHistory.ErrorCode   COLON 20 SKIP(1)
    EDRHistory.UpdateDate  COLON 20
      lcUpdateTime NO-LABEL FORMAT "X(8)" SKIP
    EDRHistory.UpdateSource COLON 20  
WITH  OVERLAY ROW 1 centered
    COLOR VALUE(Syst.CUICommon:cfc)
    TITLE COLOR VALUE(Syst.CUICommon:ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

   
{Func/brand.i}

FORM
   "Brand:" lcBrand skip
   "MSISDN:" lcCLI
      HELP "Enter MSISDN"
   WITH row 4 col 2 TITLE COLOR VALUE(Syst.CUICommon:ctc) " FIND MSISDN "
       COLOR VALUE(Syst.CUICommon:cfc) NO-LABELS OVERLAY FRAME f1.

FORM
   "Brand:" lcBrand skip
   "Rated:" ldaRateDate
      HELP "Enter rating date"
   WITH row 4 col 2 TITLE COLOR VALUE(Syst.CUICommon:ctc) " FIND Rating Date "
       COLOR VALUE(Syst.CUICommon:cfc) NO-LABELS OVERLAY FRAME f2.



Syst.CUICommon:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.CUICommon:ccc = Syst.CUICommon:cfc.
VIEW FRAME sel.

RUN local-find-first.

IF AVAILABLE EDRHistory THEN ASSIGN
   Memory       = recid(EDRHistory)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No history available" VIEW-AS ALERT-BOX.
      RETURN.   
   END.
   ELSE ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = FALSE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.


   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND EDRHistory WHERE recid(EDRHistory) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE EDRHistory THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(EDRHistory).
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
        ufk    = 0
        ufk[1] = 209
        ufk[2] = 28
        ufk[8] = 8 
        Syst.CUICommon:ehto   = 3 
        ufkey  = FALSE.

        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW EDRHistory.CLI {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(Syst.CUICommon:ccc) EDRHistory.CLI WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW EDRHistory.UpdateDate {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(Syst.CUICommon:ccc) EDRHistory.UpdateDate WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"8,f8") = 0 THEN DO:
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
        FIND EDRHistory WHERE recid(EDRHistory) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE EDRHistory THEN
              ASSIGN FIRSTrow = i Memory = recid(EDRHistory).
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
           IF NOT AVAILABLE EDRHistory THEN DO:
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
                rtab[1] = recid(EDRHistory)
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
           IF NOT AVAILABLE EDRHistory THEN DO:
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
              rtab[FRAME-DOWN] = recid(EDRHistory).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND EDRHistory WHERE recid(EDRHistory) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE EDRHistory THEN DO:
           Memory = recid(EDRHistory).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE EDRHistory THEN Memory = recid(EDRHistory).
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
           FIND EDRHistory WHERE recid(EDRHistory) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
       Syst.CUICommon:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.CUICommon:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       
       UPDATE lcBrand WHEN Syst.CUICommon:gcAllBrand
              lcCLI WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       IF lcCLI > "" THEN DO:

          FIND FIRST EDRHistory USE-INDEX CLI WHERE 
                     EDRHistory.Brand = lcBrand    AND
                     EDRHistory.CLI >= lcCLI     
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.
                           
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY column 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
       Syst.CUICommon:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.CUICommon:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f2.
       DISPLAY lcBrand WITH FRAME F2.
       
       UPDATE lcBrand WHEN Syst.CUICommon:gcAllBrand
              ldaRateDate WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.

       IF ldaRateDate NE ? THEN DO:

          FIND FIRST EDRHistory USE-INDEX UpdateDate WHERE 
                     EDRHistory.Brand = lcBrand    AND
                     EDRHistory.UpdateDate >= ldaRateDate
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(2) THEN NEXT BROWSE.
                           
          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhEDRHistory).

       ASSIGN ac-hdr = " VIEW " ufkey = TRUE Syst.CUICommon:ehto = 5. RUN Syst/ufkey.p.
       Syst.CUICommon:cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY EDRHistory.InvCust.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhEDRHistory).

       RUN local-disp-row.
       xrecid = recid(EDRHistory).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(EDRHistory) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(EDRHistory) must-print = TRUE.
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
      FIND EDRHistory WHERE recid(EDRHistory) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND EDRHistory WHERE recid(EDRHistory) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN DO:
      FIND FIRST EDRHistory USE-INDEX CLI WHERE
                 EDRHistory.Brand = Syst.CUICommon:gcBrand  NO-LOCK NO-ERROR.
   END.
   ELSE IF order = 2 THEN DO:
      FIND FIRST EDRHistory USE-INDEX UpdateDate WHERE
                 EDRHistory.Brand = Syst.CUICommon:gcBrand  NO-LOCK NO-ERROR.
   END.
 
END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN DO:
      FIND LAST EDRHistory USE-INDEX CLI WHERE
                EDRHistory.Brand = Syst.CUICommon:gcBrand  NO-LOCK NO-ERROR.
   END.
   ELSE IF order = 2 THEN DO:
      FIND LAST EDRHistory USE-INDEX UpdateDate WHERE
                EDRHistory.Brand = Syst.CUICommon:gcBrand  NO-LOCK NO-ERROR.
   END.
 
END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF order = 1 THEN DO:
      FIND NEXT EDRHistory USE-INDEX CLI WHERE
                EDRHistory.Brand = Syst.CUICommon:gcBrand  NO-LOCK NO-ERROR.
   END.
   ELSE IF order = 2 THEN DO:
      FIND NEXT EDRHistory USE-INDEX UpdateDate WHERE
                EDRHistory.Brand = Syst.CUICommon:gcBrand  NO-LOCK NO-ERROR.
   END.
 
END PROCEDURE.

PROCEDURE local-find-PREV:

   IF order = 1 THEN DO:
      FIND PREV EDRHistory USE-INDEX CLI WHERE
                EDRHistory.Brand = Syst.CUICommon:gcBrand  NO-LOCK NO-ERROR.
   END.
   ELSE IF order = 2 THEN DO:
      FIND PREV EDRHistory USE-INDEX UpdateDate WHERE
                EDRHistory.Brand = Syst.CUICommon:gcBrand  NO-LOCK NO-ERROR.
   END.
 
END PROCEDURE.

PROCEDURE local-disp-row:

       RUN local-find-others.
       
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       EDRHistory.CLI
       EDRHistory.DateSt
       lcTime
       EDRHistory.BillCode
       EDRHistory.BDest
       EDRHistory.Amount
       EDRHistory.UpdateDate
       WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

   lcTime = STRING(EDRHistory.TimeStart,"hh:mm:ss").
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      lcUpdateTime = STRING(EDRHistory.UpdateTime,"hh:mm:ss").
         
      FIND FIRST Customer WHERE Customer.CustNum = EDRHistory.InvCust 
         NO-LOCK NO-ERROR.
      IF AVAILABLE Customer THEN 
          lcCustName = Func.Common:mDispCustName(BUFFER Customer).
      ELSE lcCustName = "".
      
      FIND FIRST BillItem WHERE
          BillItem.Brand = Syst.CUICommon:gcBrand AND
          BillItem.BillCode = EDRHistory.BillCode NO-LOCK NO-ERROR.
      IF AVAILABLE BillItem THEN lcBIName = BillItem.BIName.
      ELSE lcBIName = "".

      FIND FIRST BDest WHERE
          BDest.Brand = Syst.CUICommon:gcBrand AND
          BDest.BDest = EDRHistory.BDest NO-LOCK NO-ERROR.
      IF AVAILABLE BDest THEN lcBDestName = BDest.BDName.
      ELSE lcBDestName = "".
      
      DISP 
           EDRHistory.Brand
           EDRHistory.InvCust
           lcCustName
           EDRHistory.MsSeq
           EDRHistory.CLI
           EDRHistory.DateSt
           lcTime
           EDRHistory.DtlSeq
           EDRHistory.BillCode
           lcBIName
           EDRHistory.BDest
           lcBDestName
           EDRHistory.DCEvent
           EDRHistory.TariffNum
           EDRHistory.Amount
           EDRHistory.ErrorCode
           EDRHistory.UpdateDate
           lcUpdateTime
           EDRHistory.UpdateSource
      WITH FRAME lis.

      ASSIGN 
         Syst.CUICommon:ehto = 0
         ufk  = 0
         ufk[4] = 1925
         ufk[8] = 8.
      RUN Syst/ufkey.p.
      
      IF Syst.CUICommon:toimi = 4 THEN RUN Rate/edrhistory_one_edr.p(EDRHistory.CLI,
                                                 EDRHistory.DateSt,
                                                 EDRHistory.TimeSt,
                                                 EDRHistory.DtlSeq).

      ELSE IF Syst.CUICommon:toimi = 8 THEN LEAVE.
   END.

END PROCEDURE.

