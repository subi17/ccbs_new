/* ----------------------------------------------------------------------
  MODULE .......: edrbrowse.p
  TASK .........: Browse EDRs
  APPLICATION ..: tms
  AUTHOR .......: anttis
  CREATED ......: 18.2.2013
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}  
{Func/msisdn.i}
{Func/func.p}
{Func/callquery.i}
/*
DEF  TEMP-TABLE ttCall NO-UNDO LIKE  Mobcdr
   FIELD CDRTable AS CHAR 
   INDEX date     Datest TimeStart
   INDEX BillCode BillCode
   INDEX CustNum  CustNum Datest TimeStart
   INDEX Cli      Cli     Datest TimeStart gsmbnr spocmt
   INDEX gsmbnr   gsmbnr  Datest TimeStart
   INDEX ErrorCode ErrorCode.
*/
DEF  TEMP-TABLE ttCall NO-UNDO LIKE PrepEDR
   FIELD CDRTable AS CHAR 
   INDEX date     Datest TimeStart
   INDEX CustNum  CustNum Datest TimeStart 
   INDEX Cli      Cli     Datest TimeStart 
   INDEX ErrorCode ErrorCode.
   
DEF /* NEW */ shared VAR siirto AS CHAR.

/* custrole: "inv" or "user" */
DEFINE INPUT PARAMETER  icCDRType      AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER  dtStartDate    AS DATE      NO-UNDO.
DEFINE INPUT PARAMETER  dtEndDate      AS DATE      NO-UNDO.
DEFINE INPUT PARAMETER  liCustNum      AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER  icCustRole     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER  lcCLI          AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER  liInvSeq       AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER  liInvNum       AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER  lcBPref        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER  lcBillCode     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER  lcReasonCode   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER  liErrorCodeIn  AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER  liErrorCodeOut AS INTEGER   NO-UNDO.

DEFINE VARIABLE tthCDR     AS HANDLE    NO-UNDO.
DEFINE VARIABLE ldaYearLimit AS DATE    NO-UNDO.


/* max. 1 year backwards */
ASSIGN
   ldaYearLimit = IF MONTH(TODAY) = 2 AND DAY(TODAY) > 28 
                  THEN DATE(2,28,YEAR(TODAY) - 1)
                  ELSE DATE(MONTH(TODAY),DAY(TODAY),YEAR(TODAY) - 1)
   dtStartDate = MAX(dtStartDate,ldaYearLimit).

tthCDR = TEMP-TABLE ttCall:HANDLE.

fMobCDRCollect(INPUT icCDRType,
               INPUT gcBrand,
               INPUT katun,
               INPUT dtStartDate,
               INPUT dtEndDate,
               INPUT liCustNum,
               INPUT icCustRole,
               INPUT lcCLI,
               INPUT liInvSeq,
               INPUT liInvNum,
               INPUT lcBPref,
               INPUT lcBillCode,
               INPUT lcReasonCode,
               INPUT liErrorCodeIn,
               INPUT-OUTPUT liErrorCodeOut,
               INPUT-OUTPUT tthCDR). 
/*
IF liErrorCodeIn = 0 THEN DO:
   FOR EACH ttCall WHERE ttCall.ErrorCode > 0:
      DELETE ttCall.
   END.
END.
*/

DEF VAR DateSt   LIKE Mobcdr.DateSt               NO-UNDO.
DEF VAR timest   LIKE Mobcdr.timest               NO-UNDO.
DEF VAR timest1  LIKE Mobcdr.TimeStart            NO-UNDO.
DEF VAR CustNum  LIKE Mobcdr.CustNum              NO-UNDO.
DEF VAR CLI      LIKE Mobcdr.CLI                  NO-UNDO.

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
DEF VAR qmark        AS DA                     NO-UNDO  INIT ?.
DEF VAR stime        AS C                      NO-UNDO.
DEF VAR def-ccode    AS C                      NO-UNDO.
DEF VAR SL_prefix  AS C  NO-UNDO.

{Func/tmsparam.i SL_prefix      return}.  SL_prefix = TMSParam.CharVal.
{Func/tmsparam.i DefCCode       return}.  def-ccode = TMSParam.CharVal.

form
    ttCall.DateSt  
    STime               COLUMN-LABEL "Time"        FORMAT "x(8)"
    ttCall.CLI          COLUMN-LABEL "MSISDN "     FORMAT "x(11)"
    ttCall.CustNum      COLUMN-LABEL "A-Cust"      FORMAT "zzzzzzzz9"
    Customer.CustName   COLUMN-LABEL "Cust. Name"  FORMAT "x(11)"
    ttCall.ErrorCode    
    space(0)

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " Event Data Records "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    ttCall.DateSt     /* LABEL FORMAT */
    ttCall.CustNum    /* LABEL FORMAT */
                    
    WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr
    SIDE-LABELS  1 columns    FRAME lis.

form /* seek Mobile Call  BY  DateSt */
    DateSt timest
    HELP "Enter CallDate and time"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Date AND TIME "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek Mobile Call  BY CustNum */
    CustNum
    HELP "Enter A-Sub Customer No."
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND A-CUST "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

form /* seek Mobile Call  BY A-sub. */

    CLI FORMAT "x(12)"
    HELP "Enter calling MSISDN No."
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND MSISDN "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f3.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

FIND FIRST ttCall NO-LOCK NO-ERROR.

FIND FIRST ttCall NO-ERROR.

IF AVAILABLE ttCall THEN ASSIGN
   Memory       = recid(ttCall)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE 
   "No calls were found"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
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
        FIND ttCall WHERE recid(ttCall) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttCall THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttCall).
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
        ufk[1]= 713 ufk[2]= 702 ufk[3]= 209 ufk[4]= 0
        ufk[5]= 2421 ufk[6]= 1115 ufk[7]= 0   ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ttCall.DateSt {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttCall.DateSt WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW ttCall.CustNum {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttCall.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
        CHOOSE ROW ttCall.CLI {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttCall.CLI WITH FRAME sel.
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
        FIND ttCall WHERE recid(ttCall) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttCall THEN
              ASSIGN FIRSTrow = i Memory = recid(ttCall).
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
           IF NOT AVAILABLE ttCall THEN DO:
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
                rtab[1] = recid(ttCall)
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
           IF NOT AVAILABLE ttCall THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttCall).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttCall WHERE recid(ttCall) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttCall THEN DO:
           Memory = recid(ttCall).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttCall THEN Memory = recid(ttCall).
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
           FIND ttCall WHERE recid(ttCall) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       SET DateSt timest WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF DateSt NE qmark THEN DO:
       
       timest1 = ttCall.timest.

          FIND FIRST  ttCall WHERE 
                      ttCall.DateSt    = DateSt   AND
                      ttCall.TimeStart >= timest1    
          USE-INDEX Date  NO-LOCK  NO-ERROR.
          IF NOT AVAILABLE ttCall THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END. 
          /* some ttCall/DateSt was found */
          ASSIGN order = 1 Memory = recid(ttCall) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME F2.
       SET CustNum WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF CustNum ENTERED THEN DO:
          FIND FIRST ttCall WHERE 
                     ttCall.CustNum >= CustNum
          USE-INDEX CustNum  NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ttCall THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some ttCall/CustNum was found */
          ASSIGN order = 2 Memory = recid(ttCall) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME F3.
       SET CLI WITH FRAME f3.
       HIDE FRAME f3 NO-PAUSE.
       IF CLI NE "" THEN DO:

          /* substitute Country code in m_pref WITH 0 */
/*          CLI = SUBSTR(sl_prefix,LENGTH(def-ccode) + 1) + CLI. */
          FIND FIRST ttCall WHERE 
                     ttCall.datest ne ?              AND
                     ttCall.CLI >= CLI
          USE-INDEX CLI NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ttCall THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some ttCall/CLI was found */
          ASSIGN order = 3 Memory = recid(ttCall) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */
     ELSE IF LOOKUP(nap,"enter,return,5,f5") > 0 THEN DO:
        
        FIND FIRST ttCall WHERE
             recid(ttCall) = rtab[FRAME-LINE] NO-LOCK NO-ERROR.
        
        RUN Syst/viewtable.p((BUFFER ttcall:HANDLE)).
        ufkey = TRUE.
        NEXT loop.
     END.
     
     ELSE IF LOOKUP(nap,"6,f6") > 0 THEN DO:
        
        FIND FIRST ttCall WHERE
             recid(ttCall) = rtab[FRAME-LINE] NO-LOCK NO-ERROR.
        
        RUN Mm/viewmcdr2.p(INPUT ttCall.Datest, ttCall.Dtlseq,
                      IF ttCall.CDRTable > ""
                      THEN ttCall.CDRTable
                      ELSE "MobCdr").
        ufkey = TRUE.
        NEXT loop.
     END.


     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY ttCall.DateSt.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(ttCall).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttCall) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttCall) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

EMPTY TEMP-TABLE ttCall.
IF VALID-HANDLE(tthCDR) THEN DELETE OBJECT tthCDR NO-ERROR.


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND ttCall WHERE recid(ttCall) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttCall WHERE recid(ttCall) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

       IF order = 1 THEN FIND FIRST ttCall      USE-INDEX Date
       NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST ttCall USE-INDEX CustNum
       WHERE ttCall.custnum > 0
       NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND FIRST ttCall USE-INDEX CLI 
              NO-LOCK NO-ERROR.
       
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST ttCall      USE-INDEX Date
       NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST ttCall USE-INDEX CustNum
       NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND LAST ttCall USE-INDEX CLI
       NO-LOCK NO-ERROR.
       
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT ttCall      USE-INDEX Date
       NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT ttCall USE-INDEX CustNum
        NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND NEXT ttCall USE-INDEX CLI
       NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV ttCall      USE-INDEX Date
       NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV ttCall USE-INDEX CustNum
       NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND PREV ttCall USE-INDEX CLI
       NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       ttCall.DateSt
       stime
       ttCall.CustNum
       Customer.CustName    WHEN AVAIL Customer
       ttCall.CLI  
       ttCall.ErrorCode
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
       Stime = STRING(ttCall.TimeStart,"HH:MM:SS").
       FIND Customer WHERE Customer.CustNum = ttCall.CustNum NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
      WITH FRAME lis.
      UPDATE
          ttCall.CustNum


      WITH FRAME lis.
      LEAVE.
   END.
END PROCEDURE.

