/* ----------------------------------------------------------------------
  MODULE .......: mnpmessages.p
  TASK .........: Browse MNP messages for one process
  APPLICATION ..: TMS
  AUTHOR .......: tk
  CREATED ......: 01.12.06
  CHANGED ......: 15.03.07 kl F5 actions: resend
                  19.03.07 kl message after resend

  Version ......: TeleF
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable MNPMessage

{commali.i} 
{eventval.i}
{timestamp.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'MNPMessage'}
{xmlfunction.i}


IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhMNPMessage AS HANDLE NO-UNDO.
   lhMNPMessage = BUFFER MNPMessage:HANDLE.
   RUN StarEventInitialize(lhMNPMessage).

   ON F12 ANYWHERE DO:
      RUN eventview2.p(lhMNPMessage).
   END.

END.

DEFINE INPUT PARAMETER piMNPSeq LIKE MNPMessage.MNPSeq.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
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
DEF VAR lcTimeStamp  AS CHAR format "x(20)"    NO-UNDO.
DEF VAR lcReqType    AS CHAR FORMAT "x(3)"     NO-UNDO.
DEF VAR lcReqId      AS CHAR                   NO-UNDO.
DEF VAR lcStChange   AS CHAR FORMAT "X(4)"     NO-UNDO.

DEFINE BUFFER bufMessage FOR MNPMESSAGE.

form
    MNPMessage.MessageType format "x(20)"
    lcReqType                  COLUMN-LABEL "From"
    MNPMessage.CreatedTS       COLUMN-LABEL "NumPacStamp"
    MNPMessage.StatusCode      COLUMN-LABEL "St"
    lcStChange                 COLUMN-LABEL "Msg State"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " M2M messages "
    + string(pvm,"99-99-99") + " "
    FRAME sel.


cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = " By CLI ,  By Name  ,By 3, By 4".

RUN local-find-first. 


IF AVAILABLE MNPMessage THEN ASSIGN
   Memory       = recid(MNPMessage)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE "No M2M messages available !" VIEW-AS ALERT-BOX.
   RETURN.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND MNPMessage WHERE recid(MNPMessage) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE MNPMessage THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(MNPMessage).
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
        ufk = 0
        ufk[5] = 1968 WHEN lcRight = "RW"
        ufk[7] = 2808
        ufk[8] = 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
         RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW MNPMessage.MessageType ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MNPMessage.MessageType WITH FRAME sel.
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
        FIND MNPMessage WHERE recid(MNPMessage) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE MNPMessage THEN
              ASSIGN FIRSTrow = i Memory = recid(MNPMessage).
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
           IF NOT AVAILABLE MNPMessage THEN DO:
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
                rtab[1] = recid(MNPMessage)
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
           IF NOT AVAILABLE MNPMessage THEN DO:
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
              rtab[FRAME-DOWN] = recid(MNPMessage).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND MNPMessage WHERE recid(MNPMessage) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE MNPMessage THEN DO:
           Memory = recid(MNPMessage).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE MNPMessage THEN Memory = recid(MNPMessage).
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
           FIND MNPMessage WHERE recid(MNPMessage) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */
     
     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:
        
        RUN local-find-this(false).
        
        /* resend is not allowed two times for same record */
        IF MNPMessage.MsgTurn eq 99 THEN DO:
           MESSAGE
             "Resend can be done only for latest message"
           VIEW-AS ALERT-BOX MESSAGE.
           NEXT LOOP.
        END.
        
        /* does stateChanged message existt? */
        FIND FIRST bufMessage WHERE
                   bufMessage.MNPSeq      = MNPMessage.MNPSeq AND
                   bufMessage.MessageType = "stateChanged"
        NO-LOCK NO-ERROR.

        IF AVAIL bufMessage THEN DO:
           MESSAGE
              "\"stateChanged\" message exists, can't resend!"
           VIEW-AS ALERT-BOX MESSAGE.
           NEXT LOOP.
        END.
        
        /* resend is allowed 5 times maximum */
        IF MNPMessage.MsgTurn ge 4 THEN DO:
           MESSAGE
             "Resend can't be done since maximum amount of retries is exceeded!"
           VIEW-AS ALERT-BOX MESSAGE.
           NEXT LOOP.
        END.
     
        /* must be 1 hour old (does not understand daychange) */
        IF fMakeTS() - MNPMessage.SentTS < 0.03600 THEN DO:
           MESSAGE
             "Resend can't be done since original message is not 1 hour old!"
           VIEW-AS ALERT-BOX MESSAGE.
           NEXT LOOP.
        END.
        
        FIND FIRST MNPSub WHERE
                   MNPSub.MNPSeq = MNPMessage.MNPSeq
        NO-LOCK NO-ERROR.

        FIND FIRST Order WHERE
                   Order.MsSeq = MNPSub.MsSeq
        NO-LOCK NO-ERROR.

        /* order status must be 12 */
        IF Order.StatusCode NE "12" THEN DO:
           MESSAGE
              "Order status is not 12, can't resend!"
           VIEW-AS ALERT-BOX MESSAGE.
           NEXT LOOP.
        END.
        
        /* at this point all checks are passed */
        FIND FIRST bufMessage WHERE
             RECID(bufMessage) = RECID(MNPMessage)
        EXCLUSIVE-LOCK NO-ERROR.

        RELEASE MNPMessage.
        
        CREATE MNPMessage.
        
        BUFFER-COPY bufMessage EXCEPT
           bufMessage.StatusCode
           bufMessage.CreatedTS
           bufMessage.SentTS
        TO MNPMessage.

        bufMessage.MsgTurn = 99. /* marks history */
        RELEASE bufMessage.
                   
        ASSIGN  
           MNPMessage.CreatedTS = fMakeTS()
           MNPMessage.StatusCode = 1
           MNPMessage.MsgTurn = MNPMessage.MsgTurn + 1.
        
        IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhMNPMessage).

        MESSAGE
           "New MNP message is now created!"
        VIEW-AS ALERT-BOX.
        
        memory = RECID(MNPMessage).

        RUN local-find-this(false).
        
        must-print = TRUE.

        NEXT LOOP.

     END.

     ELSE IF LOOKUP(nap,"7,f7") > 0 AND lcRight = "RW" THEN DO:
        RUN local-find-this(false).
        IF MNPMessage.XMLMessage NE "" THEN RUN pShowSchema(MNPMessage.XMLMessage).
        ELSE MESSAGE "XML-message empty !" VIEW-AS ALERT-BOX.
        
        ufkey = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(MNPMessage) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(MNPMessage) must-print = TRUE.
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
      FIND MNPMessage WHERE recid(MNPMessage) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND MNPMessage WHERE recid(MNPMessage) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
   IF order = 1 THEN 
      FIND FIRST MNPMessage WHERE MNPMessage.MNPSeq = piMNPSeq NO-LOCK
      NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN 
      FIND LAST MNPMessage WHERE MNPMessage.MNPSeq = piMNPSeq NO-LOCK
      NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN 
      FIND NEXT MNPMessage WHERE MNPMessage.MNPSeq = piMNPSeq NO-LOCK
      NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN 
      FIND PREV MNPMessage WHERE MNPMessage.MNPSeq = piMNPSeq NO-LOCK
      NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
          MNPMessage.MessageType
          lcReqType         
          MNPMessage.CreatedTS 
          MNPMessage.StatusCode
          lcStChange WHEN lcStChange NE "N/A"
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   ASSIGN
      lcReqType   = "TMS" WHEN MNPMessage.Sender = 1
      lcReqType   = "MNP" WHEN MNPMessage.Sender = 2
      lcStChange  = fGetNodeValue(MNPMessage.XMLMessage,"portabilityState").

END PROCEDURE.


PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.

      DISP

      WITH FRAME lis.
      MESSAGE "PRESS ENTER TO CONTINUE!". PAUSE NO-MESSAGE.
      LEAVE.
   END.
END PROCEDURE.


