/* ----------------------------------------------------------------------
  MODULE .......: itsendlo
  TASK .........: browse table ITSendLog
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 23.05.03
  CHANGED ......: 17.09.03/aam brand
                  30.09.03/aam RepType, eMail etc. 
                  10.10.03/aam input InvNum
                  12.02.04/aam input TxtType
                  11.11.04/aam allow negative invoice nbrs
                  06.06.05/aam TxtType 5 
                  21.02.06/aam change labels for orderid
  Version ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable ITSendLog

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'ITSendLog'}
{Func/timestamp.i}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhITSendLog AS HANDLE NO-UNDO.
   lhITSendLog = BUFFER ITSendLog:HANDLE.
   RUN StarEventInitialize(lhITSendLog).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhITSendLog).
   END.

END.

DEF INPUT PARAMETER iiCustNum AS INT NO-UNDO.
DEF INPUT PARAMETER iiInvNum  AS INT NO-UNDO. 
DEF INPUT PARAMETER iiTxtType AS INT NO-UNDO.  /* 1=IT, 2=memo */
DEF INPUT PARAMETER iiITNum   AS INT NO-UNDO. 

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR liCustNum    LIKE ITSendLog.CustNum    NO-UNDO.
DEF VAR liInvNum     LIKE ITSendLog.InvNum     NO-UNDO.
DEF VAR lcRepType    LIKE ITSendLog.RepType    NO-UNDO. 
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

DEF VAR lcTime    AS CHAR NO-UNDO.
DEF VAR ldtDate   AS DATE NO-UNDO.
DEF VAR liTime    AS INT  NO-UNDO. 
DEF VAR lcType    AS CHAR NO-UNDO. 
DEF VAR lcText    AS CHAR NO-UNDO. 
DEF VAR lcMethod  AS CHAR NO-UNDO. 

form
    ITSendLog.TxtType  COLUMN-LABEL "T"   FORMAT "9"
    ITSendLog.CustNum
    Customer.CustName  COLUMN-LABEL "Name" FORMAT "X(10)"
    ITSendLog.InvNum   COLUMN-LABEL "InvNbr/Order"  FORMAT "->>>>>>>>"
    ITSendLog.RepType                      FORMAT "X(6)"
    lcText             COLUMN-LABEL "Description" FORMAT "X(19)"
    lcTime             COLUMN-LABEL "Sent" FORMAT "X(14)"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       " TEXT SEND LOG "  + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    ITSendLog.CustNum    COLON 16   
       Customer.CustName  NO-LABEL SKIP
    ITSendLog.InvNum     COLON 16 FORMAT "->>>>>>>>"
    lcTime               COLON 16 LABEL "Sent" FORMAT "X(20)" 
      "(" SPACE(0) 
      ITSendLog.SendStamp NO-LABEL
      SPACE(0) ")" SKIP
    ITSendLog.UserCode   COLON 16 
    ITSendLog.SendMethod COLON 16
        lcMethod NO-LABEL FORMAT "X(30)" SKIP 
    ITSendLog.eMail      COLON 16 
    ITSendLog.TxtType    COLON 16 LABEL "Type"
        lcType NO-LABEL FORMAT "X(30)" SKIP
    ITSendLog.ITNum      COLON 16 LABEL "IT Seq"
    ITSendLog.RepType    COLON 16 FORMAT "X(15)" 
    lcText               COLON 16 LABEL "Description" FORMAT "X(50)"
    ITSendLog.SendInfo   COLON 16 
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

{Func/brand.i}

form /* seek  ITSendLog */
    "Brand ..:" lcBrand skip
    "Customer:" liCustnum
    HELP "Enter Customer nbr "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Customer "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  ITSendLog */
    "Brand .:" lcBrand skip
    "Invoice:" liInvnum
    HELP "Enter Invoice nbr "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Invoice "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

form /* seek  ITSendLog */
    "Brand ..:" lcBrand skip
    "Rep.Type:" lcRepType
    HELP "Enter report type "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Report Type "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f3.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "   By Customer   ," +
         "   By Invoice    ," +
         "   By Report     ,".

IF iiCustNum > 0 OR iiInvNum > 0 OR iiITNum > 0
THEN MaxOrder = 1.

RUN local-find-first.

IF AVAILABLE ITSendLog THEN ASSIGN
   Memory       = recid(ITSendLog)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No Send Logs available !" VIEW-AS ALERT-BOX.
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
        FIND ITSendLog WHERE recid(ITSendLog) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ITSendLog THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ITSendLog).
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
        ufk[1]= 702  ufk[2]= 92 ufk[3]= 1739 ufk[4]= 0
        ufk[5]= 0
        ufk[6]= 0 
        ufk[7]= 0  ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.

        IF iiCustNum > 0 OR iiInvNum > 0 THEN ASSIGN 
           ufk[1] = 0
           ufk[2] = 0
           ufk[3] = 0.
        IF iiITNum > 0 THEN ASSIGN
           ufk[2] = 0
           ufk[3] = 0. 

        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ITSendLog.CustNum {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ITSendLog.CustNum WITH FRAME sel.
      END.

      ELSE IF order = 2 THEN DO:
        CHOOSE ROW ITSendLog.InvNum {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ITSendLog.InvNum WITH FRAME sel.
      END.

      ELSE IF order = 3 THEN DO:
        CHOOSE ROW ITSendLog.RepType {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ITSendLog.RepType WITH FRAME sel.
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
        FIND ITSendLog WHERE recid(ITSendLog) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ITSendLog THEN
              ASSIGN FIRSTrow = i Memory = recid(ITSendLog).
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
           IF NOT AVAILABLE ITSendLog THEN DO:
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
                rtab[1] = recid(ITSendLog)
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
           IF NOT AVAILABLE ITSendLog THEN DO:
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
              rtab[FRAME-DOWN] = recid(ITSendLog).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ITSendLog WHERE recid(ITSendLog) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ITSendLog THEN DO:
           Memory = recid(ITSendLog).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ITSendLog THEN Memory = recid(ITSendLog).
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
           FIND ITSendLog WHERE recid(ITSendLog) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       UPDATE lcBrand WHEN gcAllBrand
              liCustNum WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       IF liCustNum > 0 THEN DO:

          IF iiITNum > 0 THEN 
          FIND FIRST ITSendLog USE-INDEX TxtType WHERE 
              ITSendLog.TxtType  = iiTxtType AND
              ITSendLog.ITNum    = iiITNum   AND 
              ITSendLog.CustNum >= liCustNum
          NO-LOCK NO-ERROR.
          ELSE            
          FIND FIRST ITSendLog USE-INDEX CustNum WHERE 
             ITSendLog.Brand    = lcBrand AND
             ITSendLog.CustNum >= liCustNum
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY column 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f2.
       DISPLAY lcBrand WITH FRAME F2.
       UPDATE lcBrand WHEN gcAllBrand
              liInvNum WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.

       IF liInvNum > 0 THEN DO:

          FIND FIRST ITSendLog USE-INDEX InvNum WHERE 
             ITSendLog.Brand    = lcBrand AND
             ITSendLog.InvNum >= liInvNum
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(2) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* Search BY column 3 */
     ELSE IF LOOKUP(nap,"3,f3") > 0 AND ufk[3] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f3.
       DISPLAY lcBrand WITH FRAME F3.
       UPDATE lcBrand WHEN gcAllBrand
              lcRepType WITH FRAME f3.
       HIDE FRAME f3 NO-PAUSE.

       IF lcRepType > "" THEN DO:

          FIND FIRST ITSendLog USE-INDEX RepType WHERE 
             ITSendLog.Brand    = lcBrand AND
             ITSendLog.RepType >= lcRepType
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(3) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-3 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" AND ufk[5] > 0 
     THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" AND ufk[6] > 0 
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       ITSendLog.CustNum Customer.CustName lcTime.

       RUN local-find-NEXT.
       IF AVAILABLE ITSendLog THEN Memory = recid(ITSendLog).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE ITSendLog THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(ITSendLog).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       ITSendLog.CustNum  Customer.CustName lcTime.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhITSendLog).

           DELETE ITSendLog.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE ITSendLog THEN DO:
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

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhITSendLog).

       ASSIGN ac-hdr = " VIEW " ufkey = TRUE ehto = 5. RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY ITSendLog.CustNum.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhITSendLog).

       RUN local-disp-row.
       xrecid = recid(ITSendLog).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ITSendLog) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ITSendLog) must-print = TRUE.
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
      FIND ITSendLog WHERE recid(ITSendLog) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ITSendLog WHERE recid(ITSendLog) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN DO:
          IF iiCustNum > 0 THEN 
             FIND FIRST ITSendLog USE-INDEX CustNum_s WHERE 
                        ITSendLog.CustNum = iiCustNum
             NO-LOCK NO-ERROR.
          ELSE IF iiInvNum > 0 THEN    
             FIND FIRST ITSendLog USE-INDEX InvNum WHERE 
                        ITSendLog.InvNum = iiInvNum
             NO-LOCK NO-ERROR.
          ELSE IF iiITNum > 0 THEN 
             FIND FIRST ITSendLog USE-INDEX TxtType WHERE 
                        ITSendLog.TxtType = iiTxtType AND
                        ITSendLog.ITNum = iiITNum
             NO-LOCK NO-ERROR.
          ELSE FIND FIRST ITSendLog USE-INDEX CustNum
             WHERE ITSendLog.Brand = lcBrand
             NO-LOCK NO-ERROR.
       END.

       ELSE IF order = 2 THEN FIND FIRST ITSendLog
          WHERE ITSendLog.Brand = lcBrand 
          USE-INDEX InvNum NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND FIRST ITSendLog 
          WHERE ITSendLog.Brand = lcBrand
          USE-INDEX RepType NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN DO:
          IF iiCustNum > 0 THEN 
             FIND LAST ITSendLog USE-INDEX CustNum_s WHERE 
                        ITSendLog.CustNum = iiCustNum
             NO-LOCK NO-ERROR.
          ELSE IF iiInvNum > 0 THEN    
             FIND LAST ITSendLog USE-INDEX InvNum WHERE 
                        ITSendLog.InvNum = iiInvNum
             NO-LOCK NO-ERROR.
          ELSE IF iiITNum > 0 THEN 
             FIND LAST ITSendLog USE-INDEX TxtType WHERE 
                        ITSendLog.TxtType = iiTxtType AND
                        ITSendLog.ITNum = iiITNum
             NO-LOCK NO-ERROR.
          ELSE FIND LAST ITSendLog USE-INDEX CustNum
             WHERE ITSendLog.Brand = lcBrand
             NO-LOCK NO-ERROR.
       END.
       ELSE IF order = 2 THEN FIND LAST ITSendLog
          WHERE ITSendLog.Brand = lcBrand 
          USE-INDEX InvNum NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND LAST ITSendLog 
          WHERE ITSendLog.Brand = lcBrand
          USE-INDEX RepType NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN DO:
          IF iiCustNum > 0 THEN 
             FIND NEXT ITSendLog USE-INDEX CustNum_s WHERE 
                        ITSendLog.CustNum = iiCustNum
             NO-LOCK NO-ERROR.
          ELSE IF iiInvNum > 0 THEN    
             FIND NEXT ITSendLog USE-INDEX InvNum WHERE 
                        ITSendLog.InvNum = iiInvNum
             NO-LOCK NO-ERROR.
          ELSE IF iiITNum > 0 THEN 
             FIND NEXT ITSendLog USE-INDEX TxtType WHERE 
                        ITSendLog.TxtType = iiTxtType AND
                        ITSendLog.ITNum = iiITNum
             NO-LOCK NO-ERROR.
          ELSE FIND NEXT ITSendLog USE-INDEX CustNum
             WHERE ITSendLog.Brand = lcBrand
             NO-LOCK NO-ERROR.
       END.
       ELSE IF order = 2 THEN FIND NEXT ITSendLog
          WHERE ITSendLog.Brand = lcBrand 
          USE-INDEX InvNum NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND NEXT ITSendLog 
          WHERE ITSendLog.Brand = lcBrand
          USE-INDEX RepType NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN DO:
          IF iiCustNum > 0 THEN 
             FIND PREV ITSendLog USE-INDEX CustNum_s WHERE 
                        ITSendLog.CustNum = iiCustNum
             NO-LOCK NO-ERROR.
          ELSE IF iiInvNum > 0 THEN    
             FIND PREV ITSendLog USE-INDEX InvNum WHERE 
                        ITSendLog.InvNum = iiInvNum
             NO-LOCK NO-ERROR.
          ELSE IF iiITNum > 0 THEN 
             FIND PREV ITSendLog USE-INDEX TxtType WHERE 
                        ITSendLog.TxtType = iiTxtType AND
                        ITSendLog.ITNum = iiITNum
             NO-LOCK NO-ERROR.
          ELSE FIND PREV ITSendLog USE-INDEX CustNum
             WHERE ITSendLog.Brand = lcBrand
             NO-LOCK NO-ERROR.
       END.
       ELSE IF order = 2 THEN FIND PREV ITSendLog
          WHERE ITSendLog.Brand = lcBrand 
          USE-INDEX InvNum NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND PREV ITSendLog 
          WHERE ITSendLog.Brand = lcBrand
          USE-INDEX RepType NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       ITSendLog.TxtType 
       ITSendLog.RepType
       ITSendLog.CustNum 
       Customer.CustName WHEN AVAILABLE Customer
       ITSendLog.InvNum
       lcTime
       lcText 

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

    lcText = "".

    IF ITSendLog.ITNum > 0 THEN DO:
       IF ITSendLog.TxtType = 1 THEN DO:
          FIND InvText WHERE InvText.ITNum = ITSendLog.ITNum NO-LOCK NO-ERROR.
          IF AVAILABLE InvText THEN lcText = IF InvText.MainTitle > ""
                                             THEN InvText.MainTitle
                                             ELSE InvText.TxtTitle.
       END.
       ELSE DO:
          FIND Memo WHERE Memo.MemoSeq = ITSendLog.ITNum NO-LOCK NO-ERROR.
          IF AVAILABLE Memo THEN lcText = Memo.MemoTitle.
       END.
    END.
    ELSE DO:
       lcText = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                 "ITSendLog","RepType",ITSendLog.RepType).
    END.                                 

    FIND Customer WHERE Customer.CustNum = ITSendLog.CustNum NO-LOCK NO-ERROR.

    fSplitTS(ITSendLog.SendStamp,
             OUTPUT ldtDate,
             OUTPUT liTime).

    lcTime = STRING(ldtDate,"99-99-99") + " " + STRING(liTime,"hh:mm:ss").

END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      ASSIGN lcType   = ""
             lcMethod = "".
             
      CASE ITSendLog.TxtType:
      WHEN 1 THEN lcType = "Information Text".
      WHEN 2 THEN lcType = "Memo".
      WHEN 3 THEN lcType = "Invoice".
      WHEN 4 THEN lcType = "Reminder".
      WHEN 5 THEN lcType = "Specification".
      END CASE.
      
      CASE ITSendLog.SendMethod:
      WHEN 1 THEN lcMethod = "eMail".
      WHEN 2 THEN lcMethod = "EPL".
      WHEN 3 THEN lcMethod = "Print Service". 
      WHEN 4 THEN lcMethod = "Local Print".
      END CASE.
      
      DISP 
           ITSendLog.CustNum
           Customer.CustName WHEN AVAILABLE Customer
           lcTime
           ITSendLog.SendStamp
           ITSendLog.TxtType
           lcType
           ITSendLog.ITNum  
           ITSendLog.InvNum  WHEN ITSendLog.InvNum >= 0
           -1 * ITSendLog.InvNum WHEN ITSendLog.InvNum < 0 @ ITSendLog.InvNum 
           ITSendLog.SendMethod
           lcMethod
           ITSendLog.RepType
           lcText
           ITSendLog.UserCode
           ITSendLog.eMail
           ITSendLog.SendInfo

      WITH FRAME lis.

      IF ITSendLog.InvNum < 0 
      THEN ITSendLog.InvNum:LABEL IN FRAME lis = "Order ID".
      ELSE ITSendLog.InvNum:LABEL IN FRAME lis = "Invoice Nbr".
      
      PAUSE MESSAGE "Press ENTER". 

      LEAVE.
   END.
END PROCEDURE.

