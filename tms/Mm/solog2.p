/* ----------------------------------------------------------------------
  MODULE .......: ttSoLog2.P
  TASK .........: ttSoLog, gets AS PARAMETER MsSeq
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 13-08-99
  CHANGED ......: 16.01.01 pt NEW PARAM in 'run sog-gwy'
                  31.08.01 pt NEW FIELD TimeSlotTMS (batch day/time)
                  10.03.03 tk tokens
                  16.05.03 jp f7
                  16.05.03 tk eventlog
                  18.05.03 tk truncate timestamps to avoid rounding
                  03.06.04 jp f5 not make completets
                  10.06.04 mk ttSoLog commline double character 60->61
                  06.03.07 kl temp-table for browsing
                  
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'SoLog'}
{Func/solog.i}
{Func/timestamp.i}
{Syst/eventval.i}
{Func/sog.i}
{Gwy/solog_create.i}
{Func/fdss.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhSoLog AS HANDLE NO-UNDO.
   lhSoLog = BUFFER SoLog:HANDLE.
   RUN StarEventInitialize(lhSoLog).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhSoLog).
   END.

END.

DEF INPUT PARAMETER piMsSeq LIKE MobSub.MsSeq NO-UNDO.

DEFINE TEMP-TABLE ttSoLog NO-UNDO LIKE SoLog
  FIELD OrigRec AS RECID
  
  INDEX SoLog AS PRIMARY SoLog DESC
  INDEX Stat Stat.

FOR EACH SoLog NO-LOCK WHERE
         SoLog.MsSeq = piMsSeq:

   CREATE ttSoLog.

   BUFFER-COPY SoLog TO ttSoLog.
   ttSoLog.OrigRec = RECID(SoLog).
   
END.

DEF /* NEW */ shared VAR siirto AS CHAR.
DEF VAR i           AS  INT NO-UNDO.
DEF VAR SoLog2  LIKE SoLog.SoLog  NO-UNDO.
DEF VAR Stat  LIKE ttSoLog.Stat NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 13.
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
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR sog-resp     AS C                      NO-UNDO.
DEF VAR stnames      AS C                      NO-UNDO.
DEF VAR sog-line     AS C                      NO-UNDO.
DEF VAR xstat        AS C                      NO-UNDO.
DEF VAR llTerminated AS LOG NO-UNDO. 
DEF VAR lcCLI AS CHAR NO-UNDO.
DEF VAR liSologRequest AS INT NO-UNDO. 
DEF VAR llIsDSSActive AS LOG NO-UNDO. 

DEF buffer xxSoLog FOR ttSoLog.

form
    ttSoLog.SoLog   format ">>>>>>>>>" 
    xstat          format "x(4)"     column-label "Stat"  
    ttSoLog.CreatedTS   format "99999999"
    ttSoLog.TimeSlotTMS format "99999999" column-label "BatchDay"
    ttSoLog.CompletedTS  format "99999999"      
    ttSoLog.CommLine  format "x(17)" 
    ttSoLog.Response  format "x(10)"

WITH ROW FrmRow centered OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    " Service Order LOG of MobSub " + lcCLI + " "
    FRAME sel.

form /* seek ttSoLog  BY SoSeq */
    SoLog2
    HELP "Enter OrdSeq"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND ORDSEQ "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek ttSoLog  BY Stat */
    stat
    HELP "Enter Status"
    "(0:NEW  1:FAIL 2: PENDING 3:OK)"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND STATUS "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.


FIND MobSub WHERE MobSub.MsSeq = piMsSeq NO-LOCK NO-ERROR.
IF AVAIL MobSub THEN ASSIGN
   lcCLi = MobSub.CLI
   llTerminated = FALSE
   llISDSSActive = fIsDSSActive(mobsub.custnum, fmakets()).
ELSE DO:
   llTerminated = True.
   FIND TermMobSub WHERE TermMobSub.MsSeq = piMsSeq NO-LOCK NO-ERROR.
   IF AVAIL TermMobSub THEN lcCLi = TermMobSub.CLI.
END.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By OrdSeq,By Status,By 3,By 4".

stnames = "Not activated,"                        +
          "Activation attempted but failed,"    +
          "Activated succesfully,,,ONGOING,HLR,".


FIND FIRST ttSoLog WHERE /* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE ttSoLog THEN ASSIGN
   Memory       = recid(ttSoLog)
   must-print   = TRUE
   must-add     = FALSE.

ELSE DO:
  MESSAGE
  "There is not any SOG transactions with this mobile subscription !"
  VIEW-AS ALERT-BOX ERROR.
  LEAVE.
END.


LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 35 
       " " + ENTRY(order,orders) + " ".
    END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ttSoLog WHERE recid(ttSoLog) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttSoLog THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttSoLog).
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
        ufk[1]= 266  ufk[2]= 559
        
        ufk[3]= (IF NOT llTerminated THEN 9816 ELSE 0)
        ufk[4]= (IF NOT llTerminated AND llISDSSActive THEN 9815 ELSE 0)
        ufk[5]= 0 /* (IF lcRight = "RW" AND NOT llTerminated THEN 261 ELSE 0)*/
        ufk[6]= 0 /* (IF lcRight = "RW" AND NOT llTerminated THEN 4 ELSE 0) */
        ufk[7]= 0 /* 1855 = FETCH RESPONSE */ ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ttSoLog.SoLog ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttSoLog.SoLog WITH FRAME sel.
      END.
      IF order = 2 THEN DO:
        CHOOSE ROW xstat ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) xstat WITH FRAME sel.
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
        FIND ttSoLog WHERE recid(ttSoLog) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttSoLog THEN
              ASSIGN FIRSTrow = i Memory = recid(ttSoLog).
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
           IF NOT AVAILABLE ttSoLog THEN DO:
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
                rtab[1] = recid(ttSoLog)
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
           IF NOT AVAILABLE ttSoLog THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttSoLog).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttSoLog WHERE recid(ttSoLog) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttSoLog THEN DO:
           Memory = recid(ttSoLog).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttSoLog THEN Memory = recid(ttSoLog).
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
           FIND ttSoLog WHERE recid(ttSoLog) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       SET SoLog2 WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF SoLog2 ENTERED THEN DO:
          FIND LAST ttSoLog WHERE 
                     ttSoLog.SoLog >= SoLog2 AND
                     ttSoLog.MsSeq  = piMsSeq 
          USE-INDEX SoLog NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ttSoLog THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some ttSoLog/ttSoLog was found */
          ASSIGN order = 1 Memory = recid(ttSoLog) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       SET Stat WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF Stat ENTERED THEN DO:
          FIND FIRST ttSoLog WHERE ttSoLog.Stat >= stat
          AND ttSoLog.MsSeq = piMsSeq USE-INDEX Stat NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ttSoLog THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some ttSoLog/so-stat was found */
          ASSIGN order = 2 Memory = recid(ttSoLog) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */
     
     ELSE IF LOOKUP(nap,"3,f3") > 0 AND ufk[3] > 0 AND
      lcRight = "RW" AND NOT llTerminated
     THEN DO:
       
         ASSIGN ok = FALSE.
         MESSAGE "ARE YOU SURE YOU CREATE DISPLAY COMMAND (Y/N) ? " UPDATE ok.
         IF NOT ok THEN NEXT.
         
         liSologRequest = fCreateDisplaySolog(piMsSeq).
         IF liSologRequest > 0 THEN DO:
            MESSAGE "Display request created : " liSologRequest
              VIEW-AS ALERT-BOX. 
            LEAVE LOOP.
         END.
         ELSE DO:
            MESSAGE "Display request creation failed" VIEW-AS ALERT-BOX ERROR.
            NEXT LOOP.
         END.
     END.
     
     ELSE IF LOOKUP(nap,"4,f4") > 0 AND ufk[4] > 0 AND
      lcRight = "RW" AND NOT llTerminated
     THEN DO:
         
         ASSIGN ok = FALSE.
         MESSAGE "ARE YOU SURE YOU CREATE DISPLAY DSS COMMAND (Y/N) ? "
         UPDATE ok.
         IF NOT ok THEN NEXT.
         
         liSologRequest = fCreateDisplayDSSSolog(piMsSeq).
         IF liSologRequest > 0 THEN DO:
            MESSAGE "Display DSS request created : " liSologRequest
              VIEW-AS ALERT-BOX. 
            LEAVE LOOP.
         END.
         ELSE DO:
            MESSAGE "Display request creation failed" VIEW-AS ALERT-BOX ERROR.
            NEXT LOOP.
         END.
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0 AND 
         lcRight = "RW" AND NOT llTerminated
     THEN DO TRANSACTION:  /* SEND TO HLR */
       RUN local-find-this (TRUE).

       IF ttSoLog.Stat = 2  THEN DO:
          MESSAGE
          "This Service order has a Status Code" ttSoLog.Stat skip(1)
          "You can NOT re-send  a Service Order that" SKIP
          "already has been succesfully handled by"   SKIP
          "SOG / HLR"
          VIEW-AS ALERT-BOX ERROR
          TITLE " Service Order Not re-transmitted ".

          NEXT.
       END.

       ASSIGN ufk = 0 ehto = 3. 
       RUN Syst/ufkey. 

       RUN local-find-this (TRUE).
      
       CREATE xxSoLog.
       BUFFER-COPY ttSoLog except ttSoLog.SoLog ttSoLog.stat ttSoLog.activationTS
       ttSoLog.response  TO xxSoLog.
       ASSIGN
          xxSoLog.activationTS = fmakets()
          xxSoLog.SoLog        = NEXT-VALUE(SoLog)
          ttSoLog.response     = "RE-SENT " + string(today,"99-99-9999") + " " +
                                  STRING(time,"hh:mm:ss") +  "REASON " +
                                  ttSoLog.response
          ttSoLog.stat         = 2       
          xxSoLog.users        = katun  
          xxSoLog.CompletedTS  = 0.
          xxSoLog.Timeslottms  = fMakeTS() .
       
       FIND FIRST SoLog WHERE
            RECID(SoLog) = ttSoLog.OrigRec
       EXCLUSIVE-LOCK NO-ERROR.
       IF AVAILABLE SoLog THEN DO:
          SoLog.Stat = 2.
          RELEASE SoLog.
       END. /* IF AVAILABLE SoLog THEN DO: */
       
       MESSAGE
          "Service order request #" string(ttSoLog.SoLog) "is " SKIP
          "re-send to activation server."                     SKIP
          "New order request #" string(xxSoLog.SoLog)
       VIEW-AS ALERT-BOX TITLE "RE-SEND Order Request".


       FIND ttSoLog where
            ttSoLog.SoLog = xxSoLog.SoLog NO-LOCK .
                   
       RUN LOCAL-DISP-ROW.
       ufkey = TRUE.
       NEXT.

     
     END.  

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0 AND
      lcRight = "RW" AND NOT llTerminated
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       IF ttSoLog.stat ne 0 THEN DO:
          MESSAGE
          "Delete not allowed!" skip
          "ttSoLog already sent to HLR"
          VIEW-AS ALERT-BOX.
          NEXT.
       END. 

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
        ttSoLog.SoLog ttSoLog.CreatedTS ttSoLog.TimeSlotTMS ttSoLog.CompletedTS.

       RUN local-find-NEXT.
       IF AVAILABLE ttSoLog THEN Memory = recid(ttSoLog).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE ttSoLog THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(ttSoLog).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          ttSoLog.SoLog ttSoLog.CreatedTS ttSoLog.TimeSlotTMS ttSoLog.CompletedTS.
         IF ok THEN DO:

           FIND FIRST SoLog WHERE
                RECID(SoLog) = ttSoLog.OrigRec
           EXCLUSIVE-LOCK NO-ERROR.
           IF AVAIL SoLog THEN DO:
              IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhSoLog).
              DELETE ttSoLog.
              DELETE SoLog.
           END. /* IF AVAIL SoLog THEN DO: */

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST ttSoLog WHERE
                                 ttSoLog.MsSeq = piMsSeq) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE delrow = 0. /* UNDO DELETE */

     END. /* DELETE */

     ELSE IF LOOKUP(nap,"7,f7") > 0 AND lcRight = "RW"
     THEN DO TRANSACTION: 
     END.  

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */      
       RUN local-find-this(FALSE ).
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.

       FIND FIRST SoLog WHERE
            RECID(SoLog) = ttSoLog.OrigRec
       NO-LOCK NO-ERROR.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSoLog).

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSoLog).

       RUN local-disp-row.
       xrecid = recid(ttSoLog).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttSoLog) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttSoLog) must-print = TRUE.
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
      FIND ttSoLog WHERE recid(ttSoLog) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK NO-ERROR.
    ELSE
       FIND ttSoLog WHERE recid(ttSoLog) = rtab[frame-line(sel)] 
       NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-FIRST:
   IF      order = 1 THEN FIND FIRST ttSoLog NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN FIND FIRST ttSoLog USE-INDEX Stat NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF      order = 1 THEN FIND NEXT ttSoLog NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN FIND NEXT ttSoLog USE-INDEX Stat NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF      order = 1 THEN FIND PREV ttSoLog NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN FIND PREV ttSoLog USE-INDEX Stat NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF      order = 1 THEN FIND LAST ttSoLog NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN FIND LAST ttSoLog USE-INDEX Stat NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN tt-local-find-others.
       lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                      BUFFER Customer).
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       ttSoLog.SoLog
       truncate(ttSoLog.CreatedTS,0) @ ttSoLog.createdts
       truncate(ttSoLog.TimeSlotTMS,0) @ ttSoLog.timeslottms 
       truncate(ttSoLog.CompletedTS,0) @ ttSoLog.completedts
       entry(ttSoLog.Stat + 1,"NEW,FAIL,OK,,,ONGOING,NWERR,HLR") @ xstat
       ttSoLog.CommLine
       ttSoLog.Response

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE tt-local-find-others.
     FIND MSISDN  WHERE MSISDN.CLI   = ttSoLog.CLI     NO-LOCK NO-ERROR.
     FIND Customer WHERE Customer.CustNum = MSISDN.CustNum   NO-LOCK NO-ERROR.


          Comm[1] = substring(ttSoLog.CommLine,1,65).
          Comm[2] = substring(ttSoLog.CommLine,66,78).
          Comm[3] = substring(ttSoLog.CommLine,144,78).
          Comm[4] = substring(ttSoLog.CommLine,222,78).
          Comm[5] = substring(ttSoLog.CommLine,300).
          lcresp[1] = substring(ttSoLog.response,1,65).
          lcresp[2] = substring(ttSoLog.response,66,68).
          lcresp[3] = substring(ttSoLog.response,144,78).
          
          
          stname = entry(ttSoLog.Stat + 1, stnames).

END PROCEDURE.

PROCEDURE local-find-others.
     FIND FIRST SoLog WHERE 
          RECID(SoLog) = ttSoLog.OrigRec
     NO-LOCK NO-ERROR.
     FIND MSISDN  WHERE MSISDN.CLI   = SoLog.CLI     NO-LOCK NO-ERROR.
     FIND Customer WHERE Customer.CustNum = MSISDN.CustNum   NO-LOCK NO-ERROR.


          Comm[1] = substring(SoLog.CommLine,1,65).
          Comm[2] = substring(SoLog.CommLine,66,78).
          Comm[3] = substring(SoLog.CommLine,144,78).
          Comm[4] = substring(SoLog.CommLine,222,78).
          Comm[5] = substring(SoLog.CommLine,300).
          lcresp[1] = substring(SoLog.response,1,65).
          lcresp[2] = substring(SoLog.response,66,68).
          lcresp[3] = substring(SoLog.response,144,78).
          
          
          stname = entry(SoLog.Stat + 1, stnames).

END PROCEDURE.
