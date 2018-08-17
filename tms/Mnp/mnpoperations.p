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

&GLOBAL-DEFINE BrTable MNPOperation

{Syst/commali.i} 
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'MNPOperation'}
{Func/xmlfunction.i}
{Mnp/mnpmessages.i}
{Syst/tmsconst.i}
{Mnp/mnpoperation.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMNPOperation AS HANDLE NO-UNDO.
   lhMNPOperation = BUFFER MNPOperation:HANDLE.
   RUN StarEventInitialize(lhMNPOperation).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhMNPOperation).
   END.

END.

PROCEDURE pPageQuery:

     UPDATE ttPageQuery 
      WITH 
        FRAME a overlay 1 col row 2 centered
        title "Create Pagina Query" .
     hide frame a NO-PAUSE.

     if lookup(keylabel(LASTKEY),"f1,return") > 0 then
      fSendPaginaQuery(INPUT TABLE ttPageQuery).
     else MESSAGE "Cancelled" VIEW-AS ALERT-BOX.

END PROCEDURE. 
        

DEFINE INPUT PARAMETER piMNPSeq LIKE MNPOperation.MNPSeq.

DEF /* NEW */ shared VAR siirto AS CHAR.

/*DEF VAR cli         LIKE MNPOperation.cli            NO-UNDO.*/
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
DEF VAR lcMNPStatus AS CHARACTER NO-UNDO FORMAT "x(25)". 
DEF VAR lcLongXML AS LONGCHAR NO-UNDO. 
DEF VAR lcErrorHandled AS CHAR NO-UNDO. 
DEF VAR lcError AS CHARACTER NO-UNDO. 

DEFINE BUFFER bufMessage FOR MNPOperation.

form
    MNPOperation.MessageType format "x(35)"
    lcReqType                  COLUMN-LABEL "From"
    MNPOperation.CreatedTS       COLUMN-LABEL "Created"
    MNPOperation.StatusCode      COLUMN-LABEL "St"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(Syst.Var:cfc)   
    TITLE COLOR VALUE(Syst.Var:ctc) " " + Syst.Var:ynimi +
    " MNP messages "
    + string(TODAY,"99-99-99") + " "
    FRAME sel.

form 
   MNPOperation.MessageType LABEL "Message Type" FORMAT "x(56)" 
   lcReqType LABEL "Source"
   MNPOperation.CreatedTS LABEL "Created Time"
   MNPOperation.SentTS LABEL "Handled Time"
   MNPOperation.StatusCode LABEL "Status Code"
   MNPOperation.MNPSeq LABEL "MNP process ID"
   MNPOperation.MNPOperationID LABEL "Message ID"
   MNPOperation.ErrorCode LABEL "Error Code"
   MNPOperation.ErrorDesc LABEL "Error Desc"
   lcErrorHandled LABEL "Error Handled"
   MNPOperation.MsgTurn LABEL "Resent"

WITH OVERLAY ROW 6 centered 1 columns 
   COLOR value(Syst.Var:cfc)
   TITLE COLOR value(Syst.Var:ctc) " View Message " WITH side-labels
   FRAME lis.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.

orders = " By CLI ,  By Name  ,By 3, By 4".

RUN local-find-first. 


IF AVAILABLE MNPOperation THEN ASSIGN
   Memory       = recid(MNPOperation)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE "No MNP messages available !" VIEW-AS ALERT-BOX.
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
        FIND MNPOperation WHERE recid(MNPOperation) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE MNPOperation THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(MNPOperation).
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
        Syst.Var:ufk = 0
        Syst.Var:ufk[1] = 2821 /* WHEN lcRight = "RW" */
        Syst.Var:ufk[3] = 0 /* WHEN lcRight = "RW" */ 
        Syst.Var:ufk[4] = 0 /* 2822 */ /* WHEN lcRight = "RW" */
        Syst.Var:ufk[5] = 1968 /* WHEN lcRight = "RW" */
        Syst.Var:ufk[6] = 2820
        Syst.Var:ufk[7] = 2819
        Syst.Var:ufk[8] = 8 Syst.Var:ufk[9]= 1
        Syst.Var:ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW MNPOperation.MessageType {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) MNPOperation.MessageType WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      Syst.Var:nap = keylabel(LASTKEY).

      IF LOOKUP(Syst.Var:nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(Syst.Var:nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND MNPOperation WHERE recid(MNPOperation) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE MNPOperation THEN
              ASSIGN FIRSTrow = i Memory = recid(MNPOperation).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.
      
      ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      ASSIGN Syst.Var:nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE MNPOperation THEN DO:
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
                rtab[1] = recid(MNPOperation)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(Syst.Var:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE MNPOperation THEN DO:
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
              rtab[FRAME-DOWN] = recid(MNPOperation).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND MNPOperation WHERE recid(MNPOperation) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE MNPOperation THEN DO:
           Memory = recid(MNPOperation).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE MNPOperation THEN Memory = recid(MNPOperation).
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
     ELSE IF LOOKUP(Syst.Var:nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND MNPOperation WHERE recid(MNPOperation) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */
     
     IF LOOKUP(Syst.Var:nap,"1,f1") > 0 AND lcRight = "RW" AND
        Syst.Var:ufk[1] > 0 THEN DO:
        
        RUN local-find-this(false).
        RUN Mnp/mnpfunc.p(MNPOperation.mnpseq). 
        
        RUN local-find-first.
        must-print = true.
        ufkey = true.
        
        NEXT LOOP.
     END.
     
     /* for fetching result pages (used in mnp testing phase) */
     IF LOOKUP(Syst.Var:nap,"3,f3") > 0 AND lcRight = "RW" AND
        Syst.Var:ufk[3] > 0 THEN DO:
        
        RUN local-find-this(false).
        find mnpprocess where mnpprocess.mnpseq = MNPOperation.mnpseq NO-LOCK.
         
        Syst.Var:ehto = 10.
        RUN Syst/ufkey.p.

        copy-lob MNPOperation.XMLResponse to lcLongXML.
        
        CREATE ttPageQuery.
        ttPageQuery.PageCode = fGetHashValue(lcLongXML, 
         "codigoPeticionPaginada").
        IF ttPageQuery.PageCode = "N/A" THEN DO:
          MESSAGE "No page code available!" VIEW-AS ALERT-BOX.
          empty temp-table ttPageQuery.
           RUN local-find-first.
           must-print = true.
           ufkey = true.
        
        NEXT LOOP.
        END.
        ttPageQuery.Operation = MNPOperation.messageType.
        
        RUN pPageQuery.
        
        RUN local-find-first.
        must-print = true.
        ufkey = true.
        
        NEXT LOOP.
     END.
     
     ELSE IF LOOKUP(Syst.Var:nap,"4,f4") > 0 AND lcRight = "RW" and Syst.Var:ufk[4] > 0 THEN DO:
        
        RUN local-find-this(true).
         
        if (MNPOperation.statuscode <= 5) and
           MNPOperation.sender = 1 then 
        MNPOperation.statuscode = 97.
        else MESSAGE "Cannot cancel message" VIEW-AS ALERT-BOX.
        
        RUN local-find-this(false).
        must-print = TRUE.
        NEXT LOOP.
     END.
     

     ELSE IF LOOKUP(Syst.Var:nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:
        
        RUN local-find-this(false).

        find mnpprocess where mnpprocess.mnpseq = MNPOperation.mnpseq NO-LOCK.
        IF MNPProcess.MNPType = {&MNP_TYPE_IN} THEN DO:
           
           FIND FIRST MNPSub WHERE
                      MNPSub.MNPSeq = MNPOperation.MNPSeq
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
        END.
        
        fResendMNPMessage(BUFFER MNPOperation, OUTPUT lcError).

        IF lcError NE "" THEN
        MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
        ELSE
        MESSAGE "New MNP message is now created!" VIEW-AS ALERT-BOX.
        
        memory = RECID(MNPOperation).

        RUN local-find-this(false).
        
        must-print = TRUE.

        NEXT LOOP.

     END.
      
      /* view */
      ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 THEN DO:
       
        RUN local-find-this(false).

        RUN local-UPDATE-record.
        ufkey = TRUE. 
        NEXT LOOP.

      END.
     
     ELSE IF LOOKUP(Syst.Var:nap,"6,f6") > 0 THEN DO:

        DEF BUFFER MNPBuzon FOR MNPOperation.

        RUN local-find-this(false).
        IF MNPOperation.XMLSeq > 0 THEN DO:
           FIND MNPBuzon where
            MNPBuzon.MNPOperationID = MNPOperation.XMLSeq no-lock.
           copy-lob from MNPBuzon.xmlrequest to lcLongXML.
           RELEASE MNPBuzon.
           RUN pShowSchema(lcLongXML).
        END.
        ELSE DO:
           
           copy-lob MNPOperation.XMLResponse to lcLongXML.
           
           IF lcLongXML BEGINS "<?xml" THEN RUN pShowSchema(lcLongXML).
           ELSE IF lcLongXML NE "" THEN MESSAGE STRING(lcLongXML) VIEW-AS ALERT-BOX.
           ELSE MESSAGE "XML-message empty !" VIEW-AS ALERT-BOX.
        END.
        
        ufkey = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"7,f7") > 0 THEN DO:
        RUN local-find-this(false).
        
        copy-lob MNPOperation.XMLRequest to lcLongXML.
        
        IF lcLongXML NE "" AND lcLongXML NE ? 
           THEN RUN pShowSchema(lcLongXML).
        ELSE MESSAGE "XML-message empty !" VIEW-AS ALERT-BOX.
        
        ufkey = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(MNPOperation) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(MNPOperation) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND MNPOperation WHERE recid(MNPOperation) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND MNPOperation WHERE recid(MNPOperation) = rtab[frame-line(sel)] 
       NO-LOCK.

END PROCEDURE.

PROCEDURE local-find-FIRST:
   IF order = 1 THEN 
      FIND FIRST MNPOperation WHERE MNPOperation.MNPSeq = piMNPSeq NO-LOCK
      USE-INDEX MNPSeq NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN 
      FIND LAST MNPOperation WHERE MNPOperation.MNPSeq = piMNPSeq NO-LOCK
      USE-INDEX MNPSeq NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN 
      FIND NEXT MNPOperation WHERE MNPOperation.MNPSeq = piMNPSeq NO-LOCK
      USE-INDEX MNPSeq NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN 
      FIND PREV MNPOperation WHERE MNPOperation.MNPSeq = piMNPSeq NO-LOCK
      USE-INDEX MNPSeq NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
          MNPOperation.MessageType
          lcReqType         
          Func.Common:mTS2HMS(MNPOperation.CreatedTS) FORMAT "x(20)" @ MNPOperation.CreatedTS
          MNPOperation.StatusCode
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   ASSIGN
      lcReqType   = "TMS" WHEN MNPOperation.Sender = 1
      lcReqType   = "MNP-NC" WHEN MNPOperation.Sender = 2 
      lcReqType   = "MNP-ADAPT" WHEN MNPOperation.Sender = 4.
   
   lcMNPStatus = Func.Common:mTMSCodeName("MNPMessage",
                               "StatusCode",
                             STRING(MNPOperation.StatusCode)).

END PROCEDURE.


PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      pause 0.
      
      CASE MNPOperation.ErrorHandled:
         WHEN 1 THEN lcErrorHandled = "No".
         WHEN 2 THEN lcErrorHandled = "Yes".
         OTHERWISE   lcErrorHandled = "".
      END.

      DISP
         MNPOperation.MessageType 
         lcReqType
         Func.Common:mTS2HMS(MNPOperation.CreatedTS) format "x(20)" @ MNPOperation.CreatedTS
         Func.Common:mTS2HMS(MNPOperation.SentTS) format "x(20)" @ MNPOperation.SentTS
         (STRING(MNPOperation.StatusCode) + " " + lcMNPStatus) FORMAT "x(30)"
            @ MNPOperation.StatusCode
         MNPOperation.MNPSeq 
         MNPOperation.MNPOperationID 
         MNPOperation.ErrorCode 
         MNPOperation.ErrorDesc 
         lcErrorHandled 
         MNPOperation.MsgTurn
      WITH FRAME lis.


      MESSAGE "PRESS ENTER TO CONTINUE!". PAUSE NO-MESSAGE.
      LEAVE.
   END.
END PROCEDURE.


