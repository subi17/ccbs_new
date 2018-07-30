/* -----------------------------------------------------------------------------
  MODULE .......: AckMessage.p
  TASK .........: Browse table AckMessage
  APPLICATION ..: tms
  CREATED ......: 12.03.15/aam 
  -------------------------------------------------------------------------- */

&GLOBAL-DEFINE TMSCodeDef NO

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'AckMessage'}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhAckMessage AS HANDLE NO-UNDO.
   lhAckMessage = BUFFER AckMessage:HANDLE.
   RUN StarEventInitialize(lhAckMessage).
END.


DEF INPUT PARAMETER icHostTable AS CHAR NO-UNDO.
DEF INPUT PARAMETER icKeyValue  AS CHAR NO-UNDO.

DEF VAR lcHostTable  AS CHAR                   NO-UNDO.
DEF VAR lcKeyValue   AS CHAR                   NO-UNDO. 
DEF VAR xRECID       AS RECID                           init ?.
DEF VAR FIRSTrow     AS int                    NO-UNDO  init 0.
DEF VAR FrmRow       AS int                    NO-UNDO  init 4.
DEF VAR FrmDOWN      AS int                    NO-UNDO  init 10.
DEF VAR order        AS int                    NO-UNDO  init 1.
DEF VAR orders       AS char                   NO-UNDO.
DEF VAR maxOrder     AS int                    NO-UNDO  init 1.
DEF VAR ufkey        AS log                    NO-UNDO  init true.
DEF VAR delrow       AS int                    NO-UNDO  init 0.
DEF VAR pr-order     AS int                    NO-UNDO.
DEF VAR memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS int                    NO-UNDO.
DEF VAR must-print   AS log                    NO-UNDO.
DEF VAR must-add     AS log                    NO-UNDO.
DEF VAR ac-hdr       AS char                   NO-UNDO.
DEF VAR rtab         AS RECID extent 24        NO-UNDO.
DEF VAR i            AS int                    NO-UNDO.
DEF VAR ok           AS log FORMAT "Yes/No"    NO-UNDO.
DEF VAR lcState      AS CHAR                   NO-UNDO.
DEF VAR lcKeyInfo    AS CHAR                   NO-UNDO.
DEF VAR ldaCreated   AS DATE                   NO-UNDO. 

form 
    AckMessage.HostTable  FORMAT "X(12)" COLUMN-LABEL "Table"
    AckMessage.KeyValue   FORMAT "X(10)" COLUMN-LABEL "Key"
    ldaCreated            FORMAT "99-99-9999" COLUMN-LABEL "Created"
    AckMessage.AckTarget  FORMAT "X(10)" 
    AckMessage.AckResult  
    AckMessage.ResultDescription FORMAT "X(15)"
    AckMessage.AckStatus  FORMAT ">9"
WITH ROW FrmRow width 80 overlay FrmDOWN  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc)
       "  ACKNOWLEDGEMENT MESSAGES  " 
    FRAME sel.

FORM
    AckMessage.HostTable COLON 20 FORMAT "X(20)"
    AckMessage.KeyValue  COLON 20 
       lcKeyInfo NO-LABEL FORMAT "X(30)" SKIP
    AckMessage.Created   COLON 20 FORMAT "99-99-9999 hh:mm:ss.sss"
    AckMessage.AckTarget COLON 20
    AckMessage.AckResult COLON 20
    AckMessage.ResultDescription COLON 20 
    AckMessage.AddInfo   COLON 20 
    AckMessage.QueueName COLON 20 
    AckMessage.AckStatus COLON 20
       lcState FORMAT "X(30)" NO-LABEL SKIP
 WITH  OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.
   
form /* seek  */
    "Table:" lcHostTable FORMAT "X(12)"
       HELP "Enter table name"
    "Key  :" lcKeyValue FORMAT "X(20)"
       HELP "Enter key value"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Key "
    COLOR VALUE(cfc) NO-labels overlay FRAME f1.


IF icHostTable > "" THEN MaxOrder = 1.

RUN local-find-first.

IF AVAILABLE AckMessage THEN ASSIGN
   memory       = RECID(AckMessage)
   must-print   = true
   must-add     = FALSE.
ELSE DO:
   BELL.
   MESSAGE "No acknowledgements available."
   VIEW-AS ALERT-BOX 
   INFORMATION.
   RETURN.      
END.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.


LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder ne 1 THEN DO:
       pr-order = order.
    END.

   PrintPage:
   do :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND AckMessage WHERE RECID(AckMessage) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'memory'.
        starting from row 'delrow' */

        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE AckMessage THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = RECID(AckMessage).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-LINE] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.
        UP FRAME-LINE - 1.
        DOWN FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = FALSE.
        PAUSE 0 NO-MESSAGE.

        /* Now there is one page DISPLAYed and the cursor is on the
        upermost row, waiting for a 'choose' */
      END. /* must-print = true */
   END. /* PrintPage */

   /* IF a row was recently DELETEd: */
   IF delrow > 0 THEN DOWN delrow - 1.
   ASSIGN delrow = 0.
   
   BROWSE:
   REPEAT WITH FRAME SEL ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
         ASSIGN
            ufk = 0 
            ufk[1] = 816
            ufk[8] = 8 
            ehto = 3 
            ufkey = FALSE.
         
         IF icHostTable > "" THEN ASSIGN 
            ufk[1] = 0
            ufk[2] = 0
            ufk[3] = 0.
                
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW AckMessage.KeyValue {Syst/uchoose.i} NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) AckMessage.KeyValue WITH FRAME sel.
      END.

      nap = KEYLABEL(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"5,f5,8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
         order = order + 1. 
         IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
         order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-LINE].
        FIND AckMessage WHERE RECID(AckMessage) = memory NO-LOCK.
        do i = 1 to FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE AckMessage THEN
              ASSIGN FIRSTrow = i memory = RECID(AckMessage).
           ELSE LEAVE.
        END.
        must-print = true.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? and NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      /* PREVious row */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE AckMessage THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* PREVious was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-DOWN to 2 by -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = RECID(AckMessage)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious row */

      /* NEXT row */
      ELSE IF LOOKUP(nap,"cursor-DOWN") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE AckMessage THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT row was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 to FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = RECID(AckMessage).
              /* save RECID of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND AckMessage WHERE RECID(AckMessage) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE AckMessage THEN DO:
           memory = RECID(AckMessage).

           /* reverse 1 page */
           DO RowNo = 1 to (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE AckMessage THEN memory = RECID(AckMessage).
              ELSE RowNo = FRAME-DOWN.
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
     ELSE IF LOOKUP(nap,"NEXT-page,page-DOWN,+") > 0 THEN DO WITH FRAME sel:
       /* Put Cursor on DOWNmost Row */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* DOWNmost row was NOT empty*/
           memory = rtab[FRAME-DOWN].
           FIND AckMessage WHERE RECID(AckMessage) = memory NO-LOCK.
           must-print = true.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search by column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0
     THEN DO on ENDkey undo, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = true.
       PAUSE 0.
       CLEAR FRAME f1.
       lcKeyValue = "".
       UPDATE lcHostTable
              lcKeyValue WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       IF lcHostTable > "" THEN DO:
            
          FIND FIRST AckMessage WHERE 
                     AckMessage.HostTable = lcHostTable AND
                     AckMessage.KeyValue >= lcKeyValue NO-LOCK NO-ERROR.
          
          IF NOT AVAILABLE AckMessage THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"enter,return") > 0
     THEN REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhAckMessage).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhAckMessage).

       RUN local-disp-row.
       xRECID = RECID(AckMessage).
       LEAVE.
     END.
        
     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = RECID(AckMessage) must-print = true.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = RECID(AckMessage) must-print = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
HIDE MESSAGE NO-PAUSE. 
si-RECID = xRECID.


PROCEDURE local-find-this:

    DEF INPUT parameter exlock AS LO NO-undo.

    IF exlock THEN
      FIND AckMessage WHERE RECID(AckMessage) = rtab[FRAME-LINE(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND AckMessage WHERE RECID(AckMessage) = rtab[FRAME-LINE(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
    IF icHostTable > "" THEN DO:
       FIND FIRST AckMessage WHERE 
                  AckMessage.HostTable = icHostTable AND
                  AckMessage.KeyValue = icKeyValue
       NO-LOCK NO-ERROR.
    END.
    ELSE DO:
       IF order = 1 THEN FIND FIRST AckMessage NO-LOCK NO-ERROR.
    END. 

END PROCEDURE.

PROCEDURE local-find-LAST:
    IF icHostTable > "" THEN DO:
       FIND LAST AckMessage WHERE 
                 AckMessage.HostTable = icHostTable AND
                 AckMessage.KeyValue = icKeyValue
       NO-LOCK NO-ERROR.
    END.
    ELSE DO:
       IF order = 1 THEN FIND LAST AckMessage NO-LOCK NO-ERROR.
    END. 


END PROCEDURE.

PROCEDURE local-find-NEXT:
    IF icHostTable > "" THEN DO:
       FIND NEXT AckMessage WHERE 
                 AckMessage.HostTable = icHostTable AND
                 AckMessage.KeyValue = icKeyValue
       NO-LOCK NO-ERROR.
    END.
    ELSE DO:
       IF order = 1 THEN FIND NEXT AckMessage NO-LOCK NO-ERROR.
    END. 


END PROCEDURE.

PROCEDURE local-find-PREV:
    IF icHostTable > "" THEN DO:
       FIND PREV AckMessage WHERE 
                 AckMessage.HostTable = icHostTable AND
                 AckMessage.KeyValue = icKeyValue
       NO-LOCK NO-ERROR.
    END.
    ELSE DO:
       IF order = 1 THEN FIND PREV AckMessage NO-LOCK NO-ERROR.
    END. 

END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.

       CLEAR FRAME sel NO-PAUSE.

       DISPLAY
          AckMessage.HostTable
          AckMessage.KeyValue
          ldaCreated
          AckMessage.AckTarget
          AckMessage.AckResult
          AckMessage.ResultDescription
          AckMessage.AckStatus 
       WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

    ASSIGN 
       lcKeyInfo  = ""
       ldaCreated = ?.
       
    IF AckMessage.Created NE ? THEN 
       ldaCreated = DATE(AckMessage.Created).
       
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      lcState = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                 "AckMessage",
                                 "AckStatus",
                                 STRING(AckMessage.AckStatus)).

      DISPLAY 
          AckMessage.HostTable
          AckMessage.KeyValue lcKeyInfo
          AckMessage.Created
          AckMessage.AckTarget
          AckMessage.AckResult
          AckMessage.ResultDescription
          AckMessage.AddInfo
          AckMessage.QueueName
          AckMessage.AckStatus lcState 
      WITH FRAME lis.

      ASSIGN
         ufk = 0
         ufk[8] = 8
         ehto = 0.
      RUN Syst/ufkey.p.
      
      LEAVE.
   END.
   
END PROCEDURE.

