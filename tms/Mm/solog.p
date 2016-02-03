/* ----------------------------------------------------------------------
  MODULE .......: SOLog.P
  TASK .........: SOLog
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 22-07-99
  CHANGED ......: 03.08.99 pt user's Name from 2 fields
                  22.09.99 pt be prepared that MobSub may be deleted
                  07.10.99 jp urights added
                  05.07.00 pt DISP time stamps WITH TRUNC
                  16.01.01 pt NEW PARAM in RUN sog-gwy
                  31.08.01 pt NEW FIELD so-tslot
                  12.12.01 pt F2 search routine enhanced
                  10.03.03 tk tokens
                  09.04.03 jp re-send
                  04.09.03 jp brand
                  22.04.04 mk F7: get soc reply from sonic queue
                  03.06.04 jp f5, not make completed ts
                  10.06.04 mk commline double printing (60->61)
                  08.02.05 kl F7 commented
                  09.02.05 kl frame sel with delete
                  16.02.04 jp hlr text
                  25.02.05 kl check status 7 with resending
                  08.03.05 kl stnames1 & stnames2
                  14.03.05 kl err:61
                  10.05.05/aam don't run timestamp.i as persistent
                  27.01.06/jt little positioning for solog.i and Dynamic 
                              function for custname display.
                  28.11.06/jt commline and response extension
                  29.01.07 kl functions commented

  Version ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable Solog

{Syst/commali.i}
{Func/solog.i}
{Func/msisdn.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'SOLog'}
{Func/sog.i}
{Func/timestamp.i}

DEF /* NEW */ shared VAR siirto AS CHAR.

{Func/excel.i}
DEF VAR CLI   LIKE SOLog.CLI   NO-UNDO.
DEF VAR Solog  LIKE SOLog.SOLog  NO-UNDO FORMAT ">>>>>>>>>9".
DEF VAR Stat LIKE SOLog.Stat NO-UNDO.
DEF VAR TimeSlotTMS LIKE SOLog.TimeSlotTMS NO-UNDO.
DEF VAR msseq        AS INT                    NO-UNDO.
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
DEF VAR sog-resp     AS C                      NO-UNDO.
DEF VAR stnames1     AS C                      NO-UNDO.
DEF VAR stnames2     AS C                      NO-UNDO.
DEF VAR sog-line     AS C                      NO-UNDO.
DEF VAR xstat        AS C format "x(4)"        NO-UNDO.
DEF VAR BatchDate    AS DA                     NO-UNDO.

DEF BUFFER xxSolog FOR solog.

form
    Solog.Brand       FORMAT "x(3)"      column-label "Bra"
    SOLog.SOLog       format "zzzzzzzz9"   column-label "Seq"
    SOLog.CLI         format "x(11)"
    xstat                                column-label "Stat" 
    SOLog.CreatedTS   format "99999999"
    SOLog.TimeSlotTMS format "zzzzzzzz"  COLUMN-LABEL "BatchDay" 
    SOLog.CompletedTS format "99999999"      
    SOLog.CommLine    format "x(12)" 
    solog.response    format "x(5)" column-label "Resp"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " Service ORDER LOG "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

form /* seek SOLog  BY  CLI */
    "Brand Code:" lcBrand  HELP "Enter Brand  "    SKIP
    "MSISDN No.:" CLI  FORMAT "x(11)"  HELP "Enter MSISDN number "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND MSISDN "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

form /* seek SOLog  BY SoSeq */
    "Brand Code:" lcBrand  HELP "Enter Brand  " 
    VALIDATE(lcBrand = "*" OR CAN-FIND(Brand WHERE Brand.Brand = lcBrand),
    "Unknown brand")
    SKIP
    "SequenceNo:" SoLog  HELP "Enter Service OrderSeq"     
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND ORDSEQ "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek SOLog  BY Stat */
    Stat  HELP "Enter Status"  "(0:NEW  1:FAIL  2:OK)"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND STATUS "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f3.

FORM /* seek SOLog BY TimeSlotTMS */
    BatchDate FORMAT "99.99.99" 
    HELP "Enter Day of Future (Batch) Activation"
    WITH ROW 4 COL 2 TITLE COLOR VALUE(ctc) " FIND ACT. DAY "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f4.

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

ASSIGN
   orders   = "By OrdSeq   ,By number   ,By Status   , By BatchTime "
   stnames1 = "Not activated,"                     +
              "Activation attempted but failed,"   +
              "Activated succesfully,,,ONGOING,NWERR,HLR"
   stnames2 = "NEW,FAIL,OK,,,ONGOING,NWERR,HLR".


FIND FIRST SOLog USE-INDEX SOLog WHERE
           SOLOG.Brand = lcBrand 
/* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE SOLog THEN ASSIGN
   Memory       = recid(SOLog)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE "No service order logs available !" VIEW-AS ALERT-BOX.
   RETURN.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 35 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a SOLog  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR SOLog.SOLog
           VALIDATE
              (SOLog.SOLog NOT ENTERED OR
              NOT CAN-FIND(SOLog using  SOLog.SOLog),
              "SOLog " + string(INPUT SOLog.SOLog) +
              " already exists !").
           IF INPUT FRAME lis SOLog.SOLog NOT ENTERED THEN 
           LEAVE add-row.
           CREATE SOLog.

           ASSIGN
           SOLog.SOLog = INPUT FRAME lis SOLog.SOLog
           Solog.Brand = lcBrand.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           ASSIGN
           Memory = recid(SOLog)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST SOLog WHERE
                 solog.Brand = lcBrand 
      /* srule */ NO-LOCK NO-ERROR.
      IF NOT AVAILABLE SOLog THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.

        FIND SOLog WHERE recid(SOLog) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE SOLog THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(SOLog).
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
        ufk[1]= 266  ufk[2]= 209 ufk[3]= 559 ufk[4]= 0
        ufk[5]= 0
        ufk[6]= 0
        ufk[7]= 0  ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN ufkey.p.
      END.
      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW SOLog.SOLog ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) SOLog.SOLog WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW SOLog.CLI ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) SOLog.CLI WITH FRAME sel.
      END.
      IF order = 3 THEN DO:
        CHOOSE ROW xstat ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) xstat WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
        CHOOSE ROW SOLog.TimeSlotTMS  ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) SOLog.TimeSlotTMS WITH FRAME sel.
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
        FIND SOLog WHERE recid(SOLog) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE SOLog THEN
              ASSIGN FIRSTrow = i Memory = recid(SOLog).
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
           IF NOT AVAILABLE SOLog THEN DO:
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
                rtab[1] = recid(SOLog)
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
           IF NOT AVAILABLE SOLog THEN DO:
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
              rtab[FRAME-DOWN] = recid(SOLog).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND SOLog WHERE recid(SOLog) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE SOLog THEN DO:
           Memory = recid(SOLog).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE SOLog THEN Memory = recid(SOLog).
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
           FIND SOLog WHERE recid(SOLog) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       Disp lcBrand with frame f1.
       SET  lcBrand WHEN gcallbrand = TRUE Solog WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF Solog ENTERED THEN DO:

          IF lcBrand ne "*" THEN 
          FIND LAST SOLog USE-INDEX SOLOG WHERE 
                    SOLog.SOLog  >= Solog AND
                    Solog.Brand  = lcBrand 
          /* srule */ NO-LOCK NO-ERROR.
          ELSE 
          FIND LAST SOLog USE-INDEX SOLOG_s WHERE
                    SOLog.SOLog = Solog
          NO-LOCK NO-ERROR.           

           IF NOT fRecFound(1) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f2.
       DISP lcBrand With Frame f2.
       SET lcBrand WHEN gcallbrand = TRUE CLI WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF CLI ENTERED THEN DO:
          IF lcBrand ne "*" THEN 
          FIND FIRST SOLog USE-INDEX cli WHERE 
                     SOLog.CLI >= CLI   AND
                     Solog.Brand = lcBrand 
          NO-LOCK NO-ERROR.
          ELSE 
          FIND FIRST SOLog USE-INDEX cli WHERE
                     SOLog.CLI >= CLI
          NO-LOCK NO-ERROR.

           IF NOT fRecFound(1) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* Search BY col 3 */
     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f3.
       SET Stat WITH FRAME f3.
       HIDE FRAME f3 NO-PAUSE.
       IF Stat ENTERED THEN DO:
          FIND FIRST SOLog WHERE SOLog.Stat >= Stat
          USE-INDEX Stat /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE SOLog THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some SOLog/Stat was found */
          ASSIGN order = 3 Memory = recid(SOLog) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-3 */
     
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis 
     ON ENDKEY UNDO, LEAVE:
       /* change */      
       RUN local-find-this(FALSE ).
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY SOLog.SOLog.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(SOLog).
       LEAVE.
     END.
     
     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(SOLog) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(SOLog) must-print = TRUE.
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
      FIND SOLog WHERE recid(SOLog) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND SOLog WHERE recid(SOLog) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST SOLog USE-INDEX SOLog
      Where Solog.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST SOLog USE-INDEX CLI
      Where Solog.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND FIRST SOLog USE-INDEX Stat
      Where Solog.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND FIRST SOLog USE-INDEX TimeSlotTMS
      Where Solog.Brand = lcBrand NO-LOCK NO-ERROR.     
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST SOLog USE-INDEX SOLog
      Where Solog.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST SOLog USE-INDEX CLI
      Where Solog.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND LAST SOLog USE-INDEX Stat
      Where Solog.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND LAST SOLog USE-INDEX TimeSlotTMS
      Where Solog.Brand = lcBrand NO-LOCK NO-ERROR.   
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT SOLog USE-INDEX SOLog
      Where Solog.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT SOLog USE-INDEX CLI
      Where Solog.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND NEXT SOLog USE-INDEX Stat
      Where Solog.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND NEXT SOLog USE-INDEX TimeSlotTMS
      Where Solog.Brand = lcBrand NO-LOCK NO-ERROR.     
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV SOLog USE-INDEX SOLog
      Where Solog.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV SOLog USE-INDEX CLI
      Where Solog.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND PREV SOLog USE-INDEX Stat
      Where Solog.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND PREV SOLog USE-INDEX TimeSlotTMS
      Where Solog.Brand = lcBrand NO-LOCK NO-ERROR.   
END PROCEDURE.

PROCEDURE local-disp-row:

   RUN local-find-others.
   CLEAR FRAME sel NO-PAUSE.
   
   DISPLAY 
      Solog.Brand
      SOLog.SOLog
      TRUNC(SOLog.CreatedTS,0)   @ SOLog.CreatedTS
      TRUNC(SOLog.TimeSlotTMS,0) @ SOLog.TimeSlotTMS
      TRUNC(SOLog.CompletedTS,0) @ SOLog.CompletedTS
      SOLog.CLI
      entry(SOLog.Stat + 1,stnames2) @ xstat
      SOLog.CommLine
      SOLog.Response
   WITH FRAME sel.
      
END PROCEDURE.

PROCEDURE local-find-others.
     FIND MSISDN  WHERE 
          MSISDN.BRAND = Solog.Brand AND
          MSISDN.CLI   = SOLog.CLI     NO-LOCK NO-ERROR.
     FIND Customer WHERE Customer.CustNum = MSISDN.CustNum   NO-LOCK NO-ERROR.
     
     IF AVAIL Customer THEN
     lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                          BUFFER Customer).

     FIND MobSub  WHERE MobSub.MsSeq  = SOLog.MsSeq    NO-LOCK NO-ERROR.


          comm[1] = substring(SOLog.CommLine,1,65).
          comm[2] = substring(SOLog.CommLine,66,78).
          comm[3] = substring(SOLog.CommLine,144,78).
          comm[4] = substring(SOLog.CommLine,222,78).
          comm[5] = substring(SOLog.CommLine,300).
          lcresp[1] = substring(SOLog.response,1,65).
          lcresp[2] = substring(SOLog.response,66,78).
          lcresp[3] = substring(SOLog.response,144,78).
          
          stname = entry(SOLog.Stat + 1, stnames1).
 END PROCEDURE.
