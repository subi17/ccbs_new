/* ----------------------------------------------------------------------
  MODULE .......: KillMs.p
  TASK .........: Browse MobSub kill requests
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 14.01.01
  CHANGED ......: 29.08.01 pt NEW FIELD outport
                  03.09.01 pt NEW FIELD KillDateTS (time stamp)
                  05.11.02 jr Eventlog
                  10.03.03 tk tokens
                  05.09.03 jp brand
                  13.01.06/aam name from customer
  Version ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable killms 

{Syst/commali.i}                  
{Syst/eventval.i}

{Func/msisdn.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'KillMS'}


DEF /* NEW */ SHARED VAR siirto AS C.

DEF VAR CLI        LIKE KillMs.CLI         NO-UNDO.
DEF VAR KillDate     LIKE KillMs.KillDate      NO-UNDO.
DEF VAR Stat       LIKE KillMs.Stat        NO-UNDO.
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
DEF VAR sog-resp     AS C                      NO-UNDO.
DEF VAR stnames      AS C                      NO-UNDO.
DEF VAR stname       AS C   format "x(25)"     NO-UNDO.
DEF VAR sog-line     AS C                      NO-UNDO.
DEF VAR xstat        AS C format "x(4)"        NO-UNDO.
DEF VAR lcName       AS CHAR                   NO-UNDO. 

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhKillMS AS HANDLE NO-UNDO.
   lhKillMS = BUFFER KillMS:HANDLE.
   RUN StarEventInitialize(lhKillMS).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhKillMS).
   END.
END.

form
    KillMs.Brand    FORMAT "x(4)" 
    KillMs.KillDate 
    KillMs.CLI
    lcName FORMAT "X(4)"  COLUMN-LABEL "Name"
    KillMs.Stat
    xstat           COLUMN-LABEL "Stat"
    KillMs.RequestTS    FORMAT "99999999"
    KillMs.UserCode    format "x(5)"  column-label "User"
    KillMs.ExecuteTS   FORMAT "99999999"
    KillMs.ErrorMsg   FORMAT "x(6)" column-label "error"

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " SCHEDULED KILL REQUESTS "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

form
    "MSISDN .....:" KillMs.CLI                              SKIP
    "Customer ...:" MSISDN.CustNum lcName AT 29       SKIP
    "SubSeq .....:" KillMs.MsSeq   format "zzzzzzz9"            
         SKIP
    "Requested ..:" KillMs.RequestTS  /* format "99999999"*/   
    "by user" KillMs.UserCode                                   SKIP
    "Kill Date ..:" KillMs.KillDateTS  KillMs.KillDate format "99-99-99"
    "at time"       KillMs.KillTime format "99.99"            SKIP 
    "Excecuted ..:" KillMs.ExecuteTS /* format "99999999"*/      SKIP
    "Status .....:" KillMs.Stat stname at 29 format "x(50)" SKIP
    "OUTport(MNP):" KillMs.OutPort                            SKIP
    " - to op. ..:" KillMs.OutOper                            SKIP
    "Response....:" KillMs.ErrorMsg  format "x(60)"                

WITH  OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) /*ac-hdr*/ " VIEW KILL REQUEST "
    NO-LABELS 
    /*1 columns*/
    FRAME lis.



form /* seek KillMs  BY  CLI */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(lcbrand = "*" OR 
    CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "MSISDN No :" CLI HELP "Enter MSISDN number "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND MSISDN "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

form /* seek KillMs  BY Date */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "Kill Date.:" killdate HELP "Enter KILL Date"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND KILL Date "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek KillMs  BY Stat */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    stat HELP "Enter Status"
    "(1: Pending 2: Failed 3: OK)"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND STATUS "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f3.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By KillDate,By MSISDN ,By Status, By 4".

stnames = "Pending,"                           +
          "Execution attempted but failed,"    +
          "Excecuted succesfully,,,,".


FIND FIRST KillMs 
WHERE killms.Brand = lcBrand USE-INDEX KillDate  NO-LOCK NO-ERROR.
IF AVAILABLE KillMs THEN ASSIGN
   Memory       = recid(KillMs)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   ASSIGN
   Memory       = ?
   must-print   = FALSE
   must-add     = FALSE. /* TRUE */
    MESSAGE 
    " There is not any SOG transactions " SKIP
    " with any mobile subscription !   "
    VIEW-AS ALERT-BOX.
    LEAVE.
END.   


LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 35 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a KillMs  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR KillMs.CLI
           VALIDATE
              (KillMs.CLI NOT ENTERED OR
              NOT CAN-FIND(KillMs using  KillMs.CLI),
              "KillMs " + string(INPUT KillMs.CLI) +
              " already exists !").
           IF INPUT FRAME lis KillMs.CLI NOT ENTERED THEN 
           LEAVE add-row.
           CREATE KillMs.
           ASSIGN
           KillMS.Brand = lcBrand 
           KillMs.CLI = INPUT FRAME lis KillMs.CLI.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhKillMS).
           ASSIGN
           Memory = recid(KillMs)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST KillMs
      WHERE killms.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE KillMs THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND KillMs WHERE recid(KillMs) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE KillMs THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(KillMs).
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
        ufk[1]= 28   ufk[2]= 209 ufk[3]= 559 ufk[4]= 0
        ufk[5]= 265  
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW KillMs.KillDate {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) KillMs.KillDate WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW KillMs.CLI {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) KillMs.CLI WITH FRAME sel.
      END.
      IF order = 3 THEN DO:
        CHOOSE ROW xstat {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
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
        FIND KillMs WHERE recid(KillMs) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE KillMs THEN
              ASSIGN FIRSTrow = i Memory = recid(KillMs).
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
           IF NOT AVAILABLE KillMs THEN DO:
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
                rtab[1] = recid(KillMs)
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
           IF NOT AVAILABLE KillMs THEN DO:
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
              rtab[FRAME-DOWN] = recid(KillMs).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND KillMs WHERE recid(KillMs) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE KillMs THEN DO:
           Memory = recid(KillMs).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE KillMs THEN Memory = recid(KillMs).
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
           FIND KillMs WHERE recid(KillMs) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       KillDate = TODAY.
       Disp lcBrand WITH FRAME f1.
       UPDATE  
           lcBrand WHEN gcAllBrand = TRUE
           KillDate WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF KillDate NE ? THEN DO:
          FIND LAST KillMs WHERE 
                    KillMs.KillDate >= killdate   AND 
                    killms.Brand = lcBrand 
          NO-LOCK NO-ERROR.

          IF NOT  fRecFound(1) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f2.
       DISP lcBrand WITH FRAME f2.
       SET lcBrand WHEN gcAllBrand = TRUE 
          CLI 
       WITH FRAME f2.                 
       HIDE FRAME f2 NO-PAUSE.
       IF CLI ENTERED THEN DO:
          IF lcBrand ne "*" THEN 
          FIND FIRST KillMs USE-INDEX cli WHERE 
                     KillMs.CLI = CLI AND 
                     killms.Brand = lcBrand 
          NO-LOCK NO-ERROR.
          ELSE 
          FIND FIRST KillMs USE-INDEX cli_s WHERE
                      KillMs.CLI = CLI
          NO-LOCK NO-ERROR.

          IF NOT  fRecFound(2) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* Search BY col 3 */
     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f3.
       DISP lcBrand WITH FRAME f3.

       SET 
          lcBrand WHEN gcAllBrand = TRUE 
          Stat WITH FRAME f3.

       HIDE FRAME f3 NO-PAUSE.
       IF Stat ENTERED THEN DO:
          FIND FIRST KillMs USE-INDEX stat WHERE 
                     KillMs.Stat >= stat AND 
                     killms.Brand = lcBrand NO-LOCK NO-ERROR.

          IF NOT  fRecFound(3) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Search-3 */

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
        KillMs.CLI 
        KillMs.RequestTS
        KillMs.ExecuteTS
        xstat
        KillMs.ErrorMsg
        KillMs.KillDate
        KillMs.UserCode
        KillMs.Stat.

       RUN local-find-NEXT.
       IF AVAILABLE KillMs THEN Memory = recid(KillMs).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE KillMs THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(KillMs).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
        KillMs.CLI 
        KillMs.KillDate
        KillMs.UserCode
        KillMs.ExecuteTS
        KillMs.RequestTS
        xstat
        KillMs.ErrorMsg
        KillMs.Stat.
       IF ok THEN DO:
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhKillMS).
           DELETE KillMs.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST KillMs
           WHERE killms.Brand = lcBrand) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return,5,f5") > 0 THEN  DO WITH FRAME lis:

       PAUSE 0.
       RUN local-find-this(FALSE ).

       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.

       DISPLAY KillMs.CLI WITH FRAME lis. 

       RUN local-UPDATE-record. /* view only  */

       ASSIGN
          ufk = 0 ufk[8] = 8 ehto = 0 ufkey = TRUE. 
       RUN Syst/ufkey.
       HIDE FRAME lis NO-PAUSE.
       NEXT loop.

     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(KillMs) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(KillMs) must-print = TRUE.
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
      FIND KillMs WHERE recid(KillMs) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND KillMs WHERE recid(KillMs) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST KillMs USE-INDEX KillDate
       WHERE killms.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST KillMs USE-INDEX CLI
       WHERE killms.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND FIRST KillMs USE-INDEX Stat
       WHERE killms.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST KillMs USE-INDEX KillDate
       WHERE killms.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST KillMs USE-INDEX CLI
       WHERE killms.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND LAST KillMs USE-INDEX Stat
       WHERE killms.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT KillMs USE-INDEX KillDate
       WHERE killms.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT KillMs USE-INDEX CLI
       WHERE killms.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND NEXT KillMs USE-INDEX Stat
       WHERE killms.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV KillMs USE-INDEX KillDate
       WHERE killms.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV KillMs USE-INDEX CLI
       WHERE killms.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND PREV KillMs USE-INDEX Stat
       WHERE killms.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       KillMS.Brand                                                         
       KillMs.CLI                                       
       lcName WHEN AVAIL MobSub
       KillMs.RequestTS                                             
       KillMs.ExecuteTS                         
       KillMs.KillDate
       KillMs.Stat
       KillMs.UserCode
       entry(KillMs.Stat,"NEW,FAIL,OK" ) @ xstat
       KillMs.ErrorMsg FORMAT "x(12)"

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
     FIND FIRST MSISDN  WHERE 
                MSISDN.CLI   = KillMs.CLI     NO-LOCK NO-ERROR.
     FIND Customer WHERE Customer.CustNum = MSISDN.CustNum    NO-LOCK NO-ERROR.
     FIND MobSub  WHERE MobSub.MsSeq  = KillMs.MsSeq    NO-LOCK NO-ERROR.
     
     stname = ENTRY(KillMs.Stat, stnames).

     lcName = "".
     IF AVAILABLE Customer THEN 
        lcName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                  BUFFER Customer).
                                  
END PROCEDURE.


PROCEDURE local-UPDATE-record.
     RUN local-find-others.
     DISP
        KillMs.CLI
        MSISDN.CustNum   WHEN AVAIL MSISDN
        lcName 
        KillMs.MsSeq
        KillMs.RequestTS
        KillMs.KillDateTS KillMs.KillTime KillMs.KillDate
        KillMs.ExecuteTS 
        KillMs.CLI
        KillMs.Stat 
        KillMs.UserCode
        stname 
        KillMs.OutPort KillMs.OutOper
        KillMs.ErrorMsg

       WITH FRAME lis.

END PROCEDURE.  

