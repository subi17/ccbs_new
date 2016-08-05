/* ----------------------------------------------------------------------
  MODULE .......: CallAlarm
  TASK .........: Updates table CallAlarm
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 21-01-03
  CHANGED ......: 08.04.03 tk show credittype in frame lis
                  14.04.03 jp show delitype name
                  24.04.03 jp frame sel modified
                  02.05.03 tk substrings fixed in local-find-ohers
                  08.05.03 tk show with enter and wait for f1 to update
                  09.05.03 jp index actstamp, modified frame lis
                  19.09.03 jp Brand 
                  17.03.04 tk assign input parameters in create
                  16.12.05 aam username removed,
                               better find-routines
                  24.01.06 jt  DYNAMIC-FUNCTION("fDispCustName"
                  30.01.06 jp previous never tested
                  13.02.06 aam use fTMSCodeName
  VERSION ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable CallAlarm

DEF  INPUT PARAMETER    iiCustNum AS INT NO-UNDO.
DEF  INPUT PARAMETER    icCli     AS CHAR No-UNDO.

{Syst/commali.i}
{Syst/eventval.i}
{Func/timestamp.i}
if llDoEvent THEN DO:
    &GLOBAL-DEFINE STAR_EVENT_USER katun
    {Func/lib/eventlog.i}

    DEF VAR lhCallAlarm AS HANDLE NO-UNDO.
    lhCallAlarm = BUFFER CallAlarm:HANDLE.
    RUN StarEventInitialize(lhCallAlarm).

    ON F12 ANYWHERE DO:
        RUN Mc/eventview2.p(lhCallAlarm).
    END.
END.

def  new  shared var siirto AS char.
DEF VAR stname     AS  C                   NO-UNDO.
DEF VAR stnames     AS  C                  NO-UNDO init "NEW,FAIL,OK,CANCEL".
DEF VAR CustNo      like CallAlarm.CustNO  NO-UNDO.
DEF VAR lcCLI      like CallAlarm.CLI        NO-UNDO.
DEF VAR xrecid       AS recid                           init ?.
DEF VAR FIRSTrow     AS int                    NO-UNDO  init 0.
DEF VAR FrmRow       AS int                    NO-UNDO  init 1.
DEF VAR FrmDown      AS int                    NO-UNDO  init 15.
DEF VAR order        AS int                    NO-UNDO  init 1.
DEF VAR orders       AS char                   NO-UNDO.
DEF VAR maxOrder     AS int                    NO-UNDO  init 3.
DEF VAR ufkey        AS log                    NO-UNDO  init true.
DEF VAR delrow       AS int                    NO-UNDO  init 0.
DEF VAR pr-order     AS int                    NO-UNDO.
DEF VAR memory       AS recid                  NO-UNDO.
DEF VAR RowNo        AS int                    NO-UNDO.
DEF VAR must-print   AS log                    NO-UNDO.
DEF VAR must-add     AS log                    NO-UNDO.
DEF VAR ac-hdr       AS char                   NO-UNDO.
DEF VAR rtab         AS recid extent 24        NO-UNDO.
DEF VAR i            AS int                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR lcCustName   AS CHAR  FORMAT "X(20)"   NO-UNDO.
DEF VAR llfind       AS LOG                    NO-UNDO.
DEF VAR lcmsg        AS CHAR EXTENT 5          NO-UNDO.
DEF VAR realtime     AS CHAR                   NO-UNDO.
DEF VAR deliverytime AS CHAR                   NO-UNDO.
DEF VAR lcDeliType   AS CHAR                   NO-UNDO.
DEF VAR lcCreditName AS CHAR                   NO-UNDO .
DEF VAR actDate      AS DATE                   NO-UNDO FORMAT "99-99-99".
DEF VAR actTime      AS CHAR                   NO-UNDO FORMAT "00:00:00".
DEF BUFFER xCallAlarm FOR CallAlarm.

form
    CallAlarm.Brand      FORMAT "X(5)"
    CallAlarm.CustNO     /* column-label format */
    CallAlarm.CLI        /* column-label format */
    lcdelitype            format "x(5)"    column-label "Deliv"
    lcCreditName          FORMAT "x(6)"   Column-label "CrType"
    CallAlarm.DeliStat    Column-label "S"
    stname                format "X(8)" Column-label "StName"
    CallAlarm.actstamp   


WITH ROW FrmRow width 80 overlay FrmDown  down
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    "  ALARM MENU   "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

form
    "Alarm Sequence:" CallAlarm.CASeq FORMAT ">>>>>>>>>9" SKIP
    "Customer No...:" CallAlarm.CustNO     /* label format */ 
    lcCustName       NO-LABEL                        SKIP
    "CLI number ...:" CallAlarm.CLI    /* label format */ SKIP
    "Sender number.:" CallAlarm.Orig SKIP
    "Deliver Type..:" CallAlarm.Delitype     LcDeliType         
    HELP "F9 = Deliver types"                         SKIP
    "Deliver Status:" CallAlarm.DeliStat 
    stname NO-label                                   SKIP

    lcmsg[1] FORMAT "X(60)"  SKIP 
    lcmsg[2] FORMAT "X(60)" NO-LABEL   SKIP
    lcmsg[3] FORMAT "X(60)" NO-LABEL   SKIP 
    lcmsg[4] FORMAT "X(60)" NO-LABEL   SKIP
    lcmsg[5] FORMAT "X(60)" NO-LABEL   SKIP
    "Scheduled time:" realtime  FORMAT "X(16)" SKIP
    "Sending time..:" deliverytime  FORMAT "X(16)" SKIP
    "Limit % ......:" CallAlarm.Limit    SKIP
    "CreditType ...:" CallAlarm.CreditType lccreditname format "x(30)" 
WITH  overlay row 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS NO-LABEL

    FRAME lis.

form /* seek  CustNo */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(lcBrand = "*" OR CAN-FIND(Brand WHERE Brand.Brand = lcBrand),
    "Unknown brand")
    "CustomerNo:"  CustNo
    HELP "Enter Customer Number of the Call Limit "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CUSTOMER "
    COLOR VALUE(cfc) NO-labels overlay FRAME f1.

form /* seek  CLI */
   "Brand Code:" lcBrand  HELP "Enter Brand"
   VALIDATE(lcBrand = "*" OR CAN-FIND(Brand WHERE Brand.Brand = lcBrand),
   "Unknown brand")
   "Msisdn No.:"  lcCLI
    HELP "Enter CLI of the Call Limit"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CLI "
    COLOR VALUE(cfc) NO-labels overlay FRAME f2.

form /* seek  CLI */
   "Brand Code:" lcBrand  HELP "Enter Brand"
   VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand")

   "Time......:"  ActDate ActTime
    HELP "Enter Activated Date and time"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND TIMESTAMP "
    COLOR VALUE(cfc) NO-labels overlay FRAME f3.


cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Code  ,  By Name  ,  By Stamp  , By 4".

IF icCLI > "" OR iiCustNum > 0 THEN MaxOrder = 1.

RUN local-find-first.

IF AVAILABLE CallAlarm THEN ASSIGN
   memory       = recid(CallAlarm)
   must-print   = true
   must-add     = false.
ELSE DO:
   MESSAGE "Not Found" VIEW-AS ALERT-BOX.
   HIDE FRAME sel NO-PAUSE.
   RETURN.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder ne 1 THEN DO:
       pr-order = order.
       PUT SCREEN row FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a CallAlarm  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = false.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis on ENDkey undo ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           CREATE CallAlarm.
           ASSIGN
           CallAlarm.CAseq = NEXT-VALUE(CallAlarm)
           CallAlarm.Brand = lcBrand              .
           ASSIGN
           CallAlarm.ActStamp = fMakeTS().
           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCallAlarm).
           ASSIGN
           Memory = recid(CallAlarm)
           xrecid = Memory.
           LEAVE.



        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = true.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE CallAlarm THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   do :
      IF must-print THEN DO:
        UP FRAME-line - 1.
        FIND CallAlarm WHERE recid(CallAlarm) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose recid is saved into 'memory'.
        starting from row 'delrow' */

        /* IF a row was recently DELETEd ... */
        IF delrow > 0 THEN DOwn delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE CallAlarm THEN DO:
              RUN local-disp-row.
              rtab[FRAME-line] = recid(CallAlarm).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-line] = ?.
           END.
           IF FRAME-line = FRAME-down THEN LEAVE.
           down.
        END.
        up FRAME-line - 1.
        down FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = false.
        PAUSE 0 NO-MESSAGE.

        /* Now there is one page DISPLAYed and the cursor is on the
        upermost row, waiting for a 'choose' */
      END. /* must-print = true */
   END. /* PrintPage */

   /* IF a row was recently DELETEd: */
   IF delrow > 0 THEN DOwn delrow - 1.
   ASSIGN delrow = 0.

BROWSE:
   REPEAT WITH FRAME sel on ENDkey undo, return:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 702  ufk[2]= 653 ufk[3]= 0  ufk[4]= 0
        ufk[5]= 5  ufk[6]= 4 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = false.
        IF icCLI > "" OR iiCustNum > 0 THEN ASSIGN 
          ufk[1] = 0
          ufk[2] = 0
          ufk[3] = 0
          ufk[5] = 0
          ufk[6] = 0.
          
         RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        choose row CallAlarm.CustNO {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) CallAlarm.CustNO WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        choose row CallAlarm.CLI {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) CallAlarm.CLI WITH FRAME sel.
      END.

      ELSE IF order = 3 THEN DO:
        choose row CallAlarm.actstamp {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) CallAlarm.actstamp WITH FRAME sel.
      END.
      IF rtab[FRAME-line] = ? THEN NEXT.

      nap = keylabel(LASTkey).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-line].
        FIND CallAlarm WHERE recid(CallAlarm) = memory NO-LOCK.
        do i = 1 to FRAME-line - 1:
           RUN local-find-PREV.
           IF AVAILABLE CallAlarm THEN
              ASSIGN FIRSTrow = i memory = recid(CallAlarm).
           ELSE LEAVE.
        END.
        must-print = true.
        NEXT LOOP.
      END.

      IF rtab[FRAME-line] = ? and NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTkey).

      /* PREVious row */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-line = 1 THEN DO:
           RUN local-find-this(false).
           RUN local-find-PREV.
           IF NOT AVAILABLE CallAlarm THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* PREVious was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-down to 2 by -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(CallAlarm)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious row */

      /* NEXT row */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-line = FRAME-down THEN DO:
           RUN local-find-this(false).
           RUN local-find-NEXT.
           IF NOT AVAILABLE CallAlarm THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT row was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 to FRAME-down - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-down] = recid(CallAlarm).
              /* save recid of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND CallAlarm WHERE recid(CallAlarm) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE CallAlarm THEN DO:
           memory = recid(CallAlarm).

           /* reverse 1 page */
           DO RowNo = 1 to (FRAME-down - 1):
              RUN local-find-PREV.
              IF AVAILABLE CallAlarm THEN memory = recid(CallAlarm).
              ELSE RowNo = FRAME-down.
           END.
           must-print = true.
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
       /* Put Cursor on downmost Row */
       IF rtab[FRAME-down] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost row was NOT empty*/
           memory = rtab[FRAME-down].
           FIND CallAlarm WHERE recid(CallAlarm) = memory NO-LOCK.
           must-print = true.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search by column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0
     THEN DO on ENDkey undo, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = true.
       IF CustNo > 0 THEN DO:
          MESSAGE
          "Customer number search is not allowed with this functionality"
          VIEW-AS ALERT-BOX.
          NEXT BROWSE.
       ENd.
       
       CLEAR FRAME f1.
       DISP lcBrand WITH FRAME F1.

       SET   lcBrand WHEN gcAllBrand TRUE 
             CustNo WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF CustNo ENTERED THEN DO:
          IF lcBrand ne "*" THEN 
          FIND FIRST CallAlarm WHERE 
                     CallAlarm.CustNO >= CustNo AND 
                     CallAlarm.Brand = lcBrand NO-LOCK NO-ERROR.
          ELSE  FIND FIRST CallAlarm WHERE
                           CallAlarm.CustNO >= CustNo
          NO-LOCK NO-ERROR.

          IF NOT  fRecFound(1) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search by col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0
     THEN DO on ENDkey undo, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = true.
       CLEAR FRAME F2.
       DISP lcBrand WITH FRAME f2.
       SET lcBrand WHEN gcAllBrand TRUE 
           lcCLI WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF lcCLI ENTERED THEN DO:
          IF lcBrand ne "*" THEN 
          FIND FIRST CallAlarm USE-INDEX cli WHERE 
                     CallAlarm.CLI >= lcCLI   AND 
                     CallAlarm.Brand = lcBrand NO-LOCK NO-ERROR.
          ELSE FIND FIRST CallAlarm USE-INDEX cli WHERE
                          CallAlarm.CLI >= lcCLI  NO-LOCK NO-ERROR.

          IF NOT  fRecFound(2) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* Search by col 3 */
     ELSE IF LOOKUP(nap,"3,f3") > 0 AND ufk[3] > 0 
     THEN DO on ENDkey undo, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = true.
       CLEAR FRAME F3.
       Actdate = today.
       SET actDate ActTime WITH FRAME f3.
       HIDE FRAME f3 NO-PAUSE.
       IF actDate  ENTERED THEN DO:

          FIND FIRST CallAlarm WHERE 
              CallAlarm.Brand = gcBrand and
              CallAlarm.actstamp = actstamp
          USE-INDEX actstamp NO-LOCK NO-ERROR.
          IF NOT AVAILABLE CallAlarm THEN 
          FIND LAST CallAlarm WHERE 
              CallAlarm.Brand = gcBrand and
              CallAlarm.actstamp >= actstamp
          USE-INDEX actstamp NO-LOCK NO-ERROR.
          
          IF NOT AVAILABLE CallAlarm THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some CallAlarm/CLI was found */
          ASSIGN order = 3 memory = recid(CallAlarm) must-print = true.
          NEXT LOOP.
       END.
     END. /* Search-3 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0
     THEN DO:  /* add */
        {Syst/uright2.i}
        must-add = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0
     THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}
       delrow = FRAME-line.
       RUN local-find-this (false).

       IF CallAlarm.DeliStat = 3 THEN DO:
          MESSAGE
          "Alarm Status " delistat "  sent " stname "." skip
          "DELETE not allowed!"
          VIEW-AS ALERT-BOX TITLE "NOT ALLOWED".

       END.

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       CallAlarm.CustNO CallAlarm.CLI  CallAlarm.DeliStat
        stname callalarm.actstamp lcdelitype lccreditname.

       RUN local-find-NEXT.
       IF AVAILABLE CallAlarm THEN memory = recid(CallAlarm).
       ELSE DO:
          /* read back the record that is to be  removed */
          RUN local-find-this (false).                     

          RUN local-find-PREV.
          IF AVAILABLE CallAlarm THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(CallAlarm).
          END.
       END.

       /* FIND back the row that is to be removed */
       RUN local-find-this(true).

       ASSIGN ok = false.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       CallAlarm.CustNO CallAlarm.CLI lccreditname lcdelitype.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCallAlarm).

           DELETE CallAlarm.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE CallAlarm THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = true.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* undo DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(false).
       pause 0.
       run local-disp-lis.

ACTION: repeat with frame lis:
          ASSIGN 
          ufk = 0 ufk[1] = 7 ufk[2] = 0 ufk[3] = 0 ufk[4] = 0
          ufk[5] = 0 ufk[8] = 8 ehto = 0 
          ufkey = true.
          RUN Syst/ufkey.

          if toimi = 8 then do:
             hide frame lis.
             next loop. 
          end.
          else if toimi = 1 then do: 

             RUN local-find-this(true).
             ASSIGN ac-hdr = " CHANGE " ufkey = true ehto = 9. RUN Syst/ufkey.
             cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
             DISPLAY CallAlarm.CustNO.

             IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCallAlarm).

             RUN local-update-record.                                  
             HIDE FRAME lis NO-PAUSE.

             /* IF  User Wanted To Cancel this Change Transaction */
             IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
             KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

             IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCallAlarm).

          end.
       end.
       RUN local-disp-row.
       xrecid = recid(CallAlarm).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(CallAlarm) must-print = true.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(CallAlarm) must-print = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-this:

    def INPUT parameter exlock as lo NO-undo.

    IF exlock THEN
      find CallAlarm WHERE recid(CallAlarm) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       find CallAlarm WHERE recid(CallAlarm) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF icCLi > "" THEN DO:
      FIND FIRST CallAlarm WHERE
                 CallAlarm.Brand = gcBrand AND
                 CallAlarm.CLI   = icCLI 
      USE-INDEX CLI NO-LOCK NO-ERROR.
   END.
   
   ELSE IF iiCustNum > 0 THEN DO:
      FIND FIRST CallAlarm WHERE
                 CallAlarm.Brand  = gcBrand AND
                 CallAlarm.CustNo = iiCustNum 
      USE-INDEX CustNo NO-LOCK NO-ERROR.
   END. 
  
   ELSE DO:
       IF order = 1 THEN 
          FIND FIRST CallAlarm WHERE CallAlarm.Brand = gcBrand 
          NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN 
          FIND FIRST CallAlarm WHERE CallAlarm.Brand = gcBrand 
          USE-INDEX CLI  NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN 
          FIND FIRST CallAlarm WHERE CallAlarm.Brand = gcBrand
          USE-INDEX Actstamp NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF icCLi > "" THEN DO:
      FIND LAST CallAlarm WHERE
                CallAlarm.Brand = gcBrand AND
                CallAlarm.CLI   = icCLI 
      USE-INDEX CLI NO-LOCK NO-ERROR.
   END.
   
   ELSE IF iiCustNum > 0 THEN DO:
      FIND LAST CallAlarm WHERE
                CallAlarm.Brand  = gcBrand AND
                CallAlarm.CustNo = iiCustNum 
      USE-INDEX CustNo NO-LOCK NO-ERROR.
   END. 
  
   ELSE DO:
       IF order = 1 THEN 
          FIND LAST CallAlarm WHERE CallAlarm.Brand = gcBrand 
          NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN 
          FIND LAST CallAlarm WHERE CallAlarm.Brand = gcBrand 
          USE-INDEX CLI  NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN 
          FIND LAST CallAlarm WHERE CallAlarm.Brand = gcBrand
          USE-INDEX Actstamp NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF icCLi > "" THEN DO:
      FIND NEXT CallAlarm WHERE
                CallAlarm.Brand = gcBrand AND
                CallAlarm.CLI   = icCLI 
      USE-INDEX CLI NO-LOCK NO-ERROR.
   END.
   
   ELSE IF iiCustNum > 0 THEN DO:
      FIND NEXT CallAlarm WHERE
                CallAlarm.Brand  = gcBrand AND
                CallAlarm.CustNo = iiCustNum 
      USE-INDEX CustNo NO-LOCK NO-ERROR.
   END. 
  
   ELSE DO:
       IF order = 1 THEN 
          FIND NEXT CallAlarm WHERE CallAlarm.Brand = gcBrand 
          NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN 
          FIND NEXT CallAlarm WHERE CallAlarm.Brand = gcBrand 
          USE-INDEX CLI  NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN 
          FIND NEXT CallAlarm WHERE CallAlarm.Brand = gcBrand
          USE-INDEX Actstamp NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF icCLi > "" THEN DO:
      FIND PREV CallAlarm WHERE
                CallAlarm.Brand = gcBrand AND
                CallAlarm.CLI   = icCLI 
      USE-INDEX CLI NO-LOCK NO-ERROR.
   END.
   
   ELSE IF iiCustNum > 0 THEN DO:
      FIND PREV CallAlarm WHERE
                CallAlarm.Brand  = gcBrand AND
                CallAlarm.CustNo = iiCustNum 
      USE-INDEX CustNo NO-LOCK NO-ERROR.
   END. 
  
   ELSE DO:
       IF order = 1 THEN 
          FIND PREV CallAlarm WHERE CallAlarm.Brand = gcBrand 
          NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN 
          FIND PREV CallAlarm WHERE CallAlarm.Brand = gcBrand 
          USE-INDEX CLI  NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN 
          FIND PREV CallAlarm WHERE CallAlarm.Brand = gcBrand
          USE-INDEX Actstamp NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       CallAlarm.Brand 
       CallAlarm.CustNO  
       CallAlarm.CLI     
       CallAlarm.DeliStat
       CallAlarm.CreditType
       Stname
       lcdelitype
       lcCreditName
       CallAlarm.actstamp 
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   Find Customer where Customer.CustNum = CallAlarm.CustNO NO-LOCK NO-ERROR.
       
   IF avail Customer then
      lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1, 
                                      BUFFER Customer).
   ELSE lcCustName = "".

   Find Mobsub WHERE
        mobsub.CLI = CallAlarm.CLI NO-LOCK NO-ERROR.

   lcMsg[1] = substring(CallAlarm.DeliMsg,1,59).
   lcMsg[2] = substring(CallAlarm.DeliMsg,60,60).
   lcMsg[3] = substring(CallAlarm.DeliMsg,120,60).
   lcMsg[4] = substring(CallAlarm.DeliMsg,180,60).
   lcMsg[5] = substring(CallAlarm.DeliMsg,240).
   stname = entry(CallAlarm.DeliStat, stnames).


   lcDeliType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                 "CallLimit",
                                 "DeliType",
                                 STRING(CallAlarm.DeliType)).

   lcCreditName = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                   "CallAlarm",
                                   "CreditType",
                                   STRING(CallAlarm.CreditType)).

END PROCEDURE.

PROCEDURE local-disp-lis:
      RUN local-find-others.
      DISP
      CallAlarm.CAseq
      CallAlarm.CustNO   
      CallAlarm.CLI     
      CallAlarm.Delitype 
      CallAlarm.Orig 
      lcDeliType
      CallAlarm.DeliStat
      stname
      fTs2hms(CallAlarm.actstamp) @ realtime
      fTs2hms(CallAlarm.delistamp) @ deliverytime
      CallAlarm.Limit      
      lcCustName when avail customer
      lcmsg
      callalarm.credittype
      lccreditname
      WITH FRAME lis.
END PROCEDURE.



PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:

      ASSIGN 
         CallAlarm.CustNo = iiCustNum WHEN iiCustNum NE 0
         CallAlarm.CLI    = icCLI     WHEN icCLI NE "".
      
      RUN local-find-others.
      DISP
      CallAlarm.CAseq
      CallAlarm.CustNO   
      CallAlarm.CLI     
      CallAlarm.Delitype 
      CallAlarm.Orig 
      lcDeliType
      CallAlarm.DeliStat
      stname
      fTs2hms(CallAlarm.actstamp) @ realtime
      fTs2hms(CallAlarm.delistamp) @ deliverytime
      CallAlarm.Limit      
      lcCustName

      WITH FRAME lis.
      
      
      UPDATE
      CallAlarm.CustNO    WHEN  NEW CallAlarm
      CallAlarm.CLI       WHEN  NEW CallAlarm
      CallAlarm.Delitype 
      lcmsg

      CallAlarm.credittype WHEN NEW callalarm


      WITH FRAME lis
      EDITING:
         READKEY.
             IF FRAME-FIELD = "delitype" AND keylabel(lastkey) = "F9" 
             THEN DO:
                RUN Help/h-tmscodes.p(INPUT "CallLimit",  /* TableName*/
                                     "DeliType", /* FieldName */
                                     "Delitype", /* GroupCode */
                               OUTPUT siirto).
                ASSIGN
                   CallAlarm.Delitype = INT(siirto).
                   disp CallAlarm.DeliType with frame lis.
             END.

             ELSE IF FRAME-FIELD = "credittype" AND keylabel(lastkey) = "F9" 
             THEN DO:
                RUN Help/h-tmscodes.p(INPUT "CallAlarm",  /* TableName*/
                                     "CreditType", /* FieldName */
                                     "SMS", /* GroupCode */
                               OUTPUT siirto).
                ASSIGN
                   CallAlarm.Credittype = INT(siirto).
                   disp CallAlarm.Credittype with frame lis.
             END.


             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.
                IF FRAME-FIELD = "CustNo" THEN DO:
                   FIND Customer WHERE 
                        Customer.Brand   = gcBrand AND 
                        Customer.CustNum =
                   INPUT FRAME lis CallAlarm.CustNO NO-LOCK NO-ERROR.
                   IF NOT AVAIL Customer THEN DO:
                      BELL.
                      MESSAGE "Unknown Customer !".
                      NEXT.
                   END.
                   
                   IF Avail Customer THEN 
                   lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,                                                   BUFFER Customer).
                   
                   DISP lcCustName @ lcCustName WITH FRAME lis.
                END.

                ELSE IF FRAME-FIELD = "CLI" THEN DO:
                   FIND MobSub WHERE 
                        MobSub.Brand = lcBrand AND 
                        mobsub.CLI =
                   INPUT FRAME lis CallAlarm.CLI NO-LOCK NO-ERROR.

                   IF AVAIL mobsub AND 
                            mobsub.CustNum ne Customer.CustNum THEN DO:
                      MESSAGE
                      "Mismatched Customer/Mobile Subscription numbers!".
                      NEXT.
                   END.
                   ELSE IF NOT AVAIL Mobsub THEN DO:
                      BELL.
                      MESSAGE
                      "Unknown Mobile subscription!".
                      NEXT.
                   END.
                END.

                ELSE IF FRAME-FIELD = "delitype" THEN DO:
                   RUN Syst/v-tmscodes(INPUT "CallLimit",    /* TableName */
                                        "Delitype", /* FieldName */
                                        "delitype",     /* GroupCode */
                                  INPUT INPUT delitype,
                                  OUTPUT llFind).
                   IF NOT llFind THEN  DO:
                       NEXT-PROMPT delitype.
                       NEXT.
                   END.

                   lcDeliType = 
                      DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                       "CallLimit",
                                       "DeliType",
                                       STRING(INPUT CallAlarm.DeliType)).
                   pause 0.
                   DISP lcDelitype With FRAME lis.    
                END.

                ELSE IF FRAME-FIELD = "credittype" THEN DO:
                   RUN Syst/v-tmscodes(INPUT "CallAlarm",    /* TableName */
                                        "credittype", /* FieldName */
                                        "SMS",     /* GroupCode */
                                  INPUT INPUT callalarm.credittype,
                                  OUTPUT llFind).
                   IF NOT llFind THEN  DO:
                       NEXT-PROMPT callalarm.credittype.
                       NEXT.
                   END.

                   lcCreditName = 
                      DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                       "CallAlarm",
                                       "CreditType",
                                       STRING(INPUT CallAlarm.CreditType)).
                   pause 0.
                   DISP lccreditname With FRAME lis.    
                END.

             END.
             APPLY LASTKEY.
          END. /* EDITING */
      
      CallAlarm.DeliMsg = "".    
      DO i = 1 to 5:
         CallAlarm.DeliMsg = CallAlarm.DeliMsg + TRIM(lcMsg[i]) + " ".
      END.

      LEAVE.
   END.
END PROCEDURE.
