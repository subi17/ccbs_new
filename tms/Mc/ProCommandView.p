/* ----------------------------------------------------------------------
  MODULE .......: ProCommandView.p
  TASK .........: Display ProCommand data
  APPLICATION ..: TMS
  AUTHOR .......: Diego 
  CREATED ......: 22.6.2018
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/tmsconst.i}

/* Program expects at least one of the values */
DEF INPUT PARAMETER iiMsSeq     AS INT NO-UNDO.
DEF INPUT PARAMETER iiMsRequest AS INT NO-UNDO.

DEF VAR firstrow        AS INT          NO-UNDO INITIAL 0.
DEF VAR FrmRow          AS INT          NO-UNDO INITIAL 1.
DEF VAR FrmDown         AS INT          NO-UNDO INITIAL 15.
DEF VAR procommand      AS INT          NO-UNDO INITIAL 1.
DEF VAR commands        AS CHAR         NO-UNDO.
DEF VAR maxCommand      AS INT          NO-UNDO INITIAL 1.
DEF VAR memory          AS RECID        NO-UNDO.
DEF VAR RowNo           AS INT          NO-UNDO.
DEF VAR must-print      AS LOG          NO-UNDO.
DEF VAR pr-procommand   AS INT          NO-UNDO.
DEF VAR ufkey           AS LOG          NO-UNDO INITIAL TRUE.
DEF VAR rtab            AS RECID        NO-UNDO EXTENT 24.
DEF VAR i               AS INT          NO-UNDO.
DEF VAR ldtResponseTS   AS DATETIME-TZ  NO-UNDO.
DEF VAR lcCli           LIKE mobsub.cli NO-UNDO.
DEF VAR lcCommand_Ed    AS LONGCHAR     NO-UNDO 
   VIEW-AS EDITOR LARGE SIZE 76 BY 3.
DEF VAR lcResponse_Ed    AS LONGCHAR     NO-UNDO 
   VIEW-AS EDITOR LARGE SIZE 76 BY 3.

DEF STREAM strProCmd.

FORM
   ProCommand.msseq            COLUMN-LABEL "SubscrID"
   lcCli                       COLUMN-LABEL "MSISDN" FORMAT "x(11)"
   ProCommand.OrderId          COLUMN-LABEL "Order ID" 
   ProCommand.ProCommandTarget COLUMN-LABEL "Target" FORMAT "X(5)"
   ProCommand.ProCommandStatus COLUMN-LABEL "Sts" FORMAT ">>9"
   ldtResponseTS             COLUMN-LABEL "Updated" FORMAT "99/99/99 HH:MM:SS" 
   ProCommand.ProCommandId     COLUMN-LABEL "Command ID"
   WITH ROW FrmRow WIDTH 80 OVERLAY FrmDown DOWN
   COLOR VALUE(Syst.Var:cfc)
   TITLE COLOR VALUE(Syst.Var:ctc) "Procommands" + (IF iiMsSeq <> 0 THEN (" for " + STRING(iiMsSeq)) ELSE (IF iiMsRequest <> 0 THEN (" for " + STRING(iiMsRequest)) ELSE ""))
   FRAME sel.    

FORM
    "SubscrID    :" ProCommand.msseq          
    "Order Id:"     AT 27 ProCommand.Orderid
    "Request Id  :" AT 50 ProCommand.msRequest        SKIP
    "Command type:" ProCommand.procommandtype   
    "MSISDN      :" AT 50 lcCli  FORMAT "x(11)"       SKIP
    "Created     :" ProCommand.CreatedTS              SKIP   
    "Sent        :" ProCommand.SendTS                 SKIP   
    "Response    :" ProCommand.ResponseTS             SKIP
    "Target      :" ProCommand.ProCommandTarget    
    "Status:"       AT 27 ProCommand.ProCommandStatus  
    "HTTP method :" AT 50 ProCommand.ProCommandVerb   SKIP 
    "Target URL  :" ProCommand.ProCommandTargetURL FORMAT "X(60)" SKIP
    "Command     :" lcCommand_Ed                      SKIP 
    "Response    :" lcResponse_Ed 
    WITH  OVERLAY ROW 2 CENTERED 
    COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc) " Command details " 
    NO-LABELS FRAME lis.
    
ASSIGN Syst.Var:cfc = "sel". 
RUN Syst/ufcolor.p. 
ASSIGN Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.

commands = " By Creation". /* add other sortings (and the code) if needed */


/* Pre-checks ********************************************** */

/* Enough data? */
IF iiMsSeq = 0 AND iiMsRequest = 0 THEN
DO:
   MESSAGE "No data provided for filtering!" VIEW-AS ALERT-BOX.
   RETURN.
END.

/* Checking when getting more data than needed */
IF iiMsSeq <> 0 AND iiMsRequest <> 0 THEN
DO: 
   FIND FIRST MsRequest WHERE MsRequest.Msrequest = iiMsRequest NO-LOCK NO-ERROR.
   IF NOT AVAIL MsRequest THEN
   DO:
      MESSAGE "Request not found" iiMsRequest "!" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE 
   IF MsRequest.MsSeq <> iiMsSeq THEN
   DO:
      MESSAGE "Controversial SubscrID/RequestID data for filtering!" VIEW-AS ALERT-BOX.
      RETURN.
   END.
END.

/* Is there any ProCommand? */
IF iiMsSeq <> 0 THEN
   FIND FIRST ProCommand NO-LOCK WHERE
              ProCommand.MsSeq EQ iiMsSeq USE-INDEX Ix_MsSeq NO-ERROR.
ELSE 
   FIND FIRST ProCommand NO-LOCK WHERE
              Procommand.MsRequest EQ iiMsRequest USE-INDEX Ix_MsRequest NO-ERROR.
 
IF NOT AVAIL ProCommand THEN
DO:
   IF iiMsSeq <> 0 THEN 
       MESSAGE "ProCommands not found for SubscrID" iiMsSeq "!" VIEW-AS ALERT-BOX.
   ELSE 
       MESSAGE "ProCommands not found for REqeustId" iiMsRequest "!" VIEW-AS ALERT-BOX.
   
    RETURN.
END.

ASSIGN
   memory     = RECID(ProCommand)
   must-print = TRUE
   procommand = 1. 

LOOP:
REPEAT WITH FRAME sel:

   IF procommand <> pr-procommand AND maxCommand NE 1 THEN DO:
      pr-procommand = procommand.
      PUT SCREEN ROW FrmRow + FrmDown + 3 COL 35 ENTRY(procommand,commands).
   END.


   PrintPage:
   DO:
      IF must-print THEN DO:
         UP FRAME-LINE - 1.
         FIND ProCommand WHERE RECID(ProCommand) = memory NO-LOCK NO-ERROR.

         /* DISPLAY one page beginning the record
            whose RECID is saved into 'memory'.
            starting from ROW 'delrow' */

         REPEAT WITH FRAME sel:
            IF AVAILABLE ProCommand THEN DO:
               RUN local-disp-row.
               rtab[FRAME-LINE] = RECID(ProCommand).
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
         DOWN firstrow.
        
         ASSIGN firstrow = 0
                must-print = FALSE.
         PAUSE 0 no-MESSAGE.

         /* Now there is one page displayed and the cursor is on the
           upermost ROW, waiting FOR a 'choose' */
      END. /* must-print = TRUE */
   END. /* PrintPage */

   BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
         ASSIGN
            Syst.Var:ufk[1] = 0 
            Syst.Var:ufk[2] = 0 
            Syst.Var:ufk[3] = 0 
            Syst.Var:ufk[4] = 0
            Syst.Var:ufk[5] = 0
            Syst.Var:ufk[6] = 0
            Syst.Var:ufk[7] = 0
            Syst.Var:ufk[8] = 8
            Syst.Var:ehto = 3 
            ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      
      IF procommand = 1 THEN
      DO:
         CHOOSE ROW ProCommand.ProCommandId {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(Syst.Var:ccc) ProCommand.ProCommandId WITH FRAME sel.
      END.
      ELSE IF procommand = 2 THEN
      DO:
         /* add other displays if needed */
      END.
      
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      Syst.Var:nap = KEYLABEL(LASTKEY).

      IF LOOKUP(Syst.Var:nap,"cursor-right") > 0 THEN
      DO:
         procommand = procommand + 1. 
         IF procommand > maxCommand THEN procommand = 1.
      END.
      
      IF LOOKUP(Syst.Var:nap,"cursor-left") > 0 THEN
      DO:
         procommand = procommand - 1.
         IF procommand = 0 THEN procommand = maxCommand.
      END.

      IF procommand <> pr-procommand AND maxCommand > 1 THEN
      DO:
         ASSIGN 
            firstrow = 0 
            memory = rtab[FRAME-LINE].
         
         FIND ProCommand WHERE RECID(ProCommand) = memory NO-LOCK.
         DO i = 1 TO FRAME-LINE - 1:
            RUN local-find-prev.
            IF AVAILABLE ProCommand THEN
               ASSIGN 
                  firstrow = i 
                  memory = RECID(ProCommand).
            ELSE LEAVE.
         END.
         must-print = TRUE.
         NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? THEN
      DO:
         BELL.
         MESSAGE "You are on an empty row, move upwards !".
         PAUSE 1 no-MESSAGE.
         NEXT.
      END.

      Syst.Var:nap = KEYLABEL(LASTKEY).

      /* previous ROW */
      IF LOOKUP(Syst.Var:nap,"cursor-up") > 0 THEN
      DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN
         DO:
            RUN local-find-this(FALSE).
            RUN local-find-prev.
            IF NOT AVAILABLE ProCommand THEN
            DO:
               MESSAGE "YOU ARE ON THE FIRST ROW !".
               BELL. 
               PAUSE 1 NO-MESSAGE.
               NEXT BROWSE.
            END.
            ELSE
            DO:
               /* previous was found */
               SCROLL DOWN.
               RUN local-disp-row.
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
                  rtab[1] = RECID(ProCommand)
                  memory  = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(Syst.Var:nap,"cursor-down") > 0 THEN
      DO WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN
         DO:
            RUN local-find-this(FALSE).
            RUN local-find-NEXT.
            IF NOT AVAILABLE ProCommand THEN
            DO:
               MESSAGE "YOU ARE ON THE LAST ROW !".
               BELL.
               PAUSE 1 NO-MESSAGE.
               NEXT BROWSE.
            END.
            ELSE
            DO:
               /* NEXT ROW was found */
               SCROLL UP.
               RUN local-disp-row.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = RECID(ProCommand).
               /* save RECID of uppermost ROW */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* prev page */
      ELSE IF LOOKUP(Syst.Var:nap,"prev-page,page-up,-") > 0 THEN
      DO WITH FRAME sel:
         memory = rtab[1].
         FIND ProCommand WHERE RECID(ProCommand) = memory NO-LOCK NO-ERROR.
         RUN local-find-prev.
         IF AVAILABLE ProCommand THEN
         DO:
            memory = RECID(ProCommand).

            /* reverse 1 page */
            DO RowNo = 1 TO (FRAME-DOWN - 1):
               RUN local-find-prev.
               IF AVAILABLE ProCommand THEN memory = RECID(ProCommand).
               ELSE RowNo = FRAME-DOWN.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE DO:
            /* is this the very FIRST record of the table ?  */
            MESSAGE "YOU ARE ON THE FIRST PAGE !".
            BELL. 
            PAUSE 1 NO-MESSAGE.
         END.
     END. /* previous page */

     /* NEXT page */
     ELSE IF LOOKUP(Syst.Var:nap,"NEXT-page,page-down,+") > 0 THEN
     DO WITH FRAME sel:
        /* PUT Cursor on downmost ROW */
        IF rtab[FRAME-DOWN] = ? THEN
        DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL.
           PAUSE 1 NO-MESSAGE.
        END.
        ELSE
        DO: /* downmost ROW was NOT empty*/
           memory = rtab[FRAME-DOWN].
           FIND ProCommand WHERE RECID(ProCommand) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
        END.
     END. /* NEXT page */

     ELSE IF LOOKUP(Syst.Var:nap,"CTRL-P") > 0 THEN
     DO:
        FIND FIRST ProCommand NO-LOCK WHERE
                   RECID(ProCommand) = rtab[FRAME-LINE] NO-ERROR.
        IF AVAIL ProCommand THEN
        DO:
           OUTPUT STREAM strProCmd TO ProCommand.d APPEND.
           EXPORT STREAM strProCmd ProCommand.
           OUTPUT STREAM strProCmd CLOSE.
        END.
        APPLY LASTKEY.
     END.                           
     
     ELSE IF LOOKUP(Syst.Var:nap,"home,h") > 0 THEN
     DO:
        RUN local-find-FIRST.
        ASSIGN 
           memory = RECID(ProCommand) 
           must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,e") > 0 THEN
     DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN 
           memory = RECID(ProCommand) 
           must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis:
        RUN local-find-this(FALSE).

        CLEAR FRAME lis NO-PAUSE.
        PAUSE 0 NO-MESSAGE.
        
        ASSIGN   
           lcCommand_Ed  = ProCommand.CommandLine 
           lcResponse_Ed = ProCommand.Response.
        
        ASSIGN
           Syst.Var:ufk[1] = 2244 
           Syst.Var:ufk[2] = 9823 
           Syst.Var:ufk[3] = 0 
           Syst.Var:ufk[4] = 0
           Syst.Var:ufk[5] = 0
           Syst.Var:ufk[6] = 0
           Syst.Var:ufk[7] = 0
           Syst.Var:ufk[8] = 8
           Syst.Var:ehto = 3 
           ufkey = TRUE.
        RUN Syst/ufkey.p.
           
        DISP
           ProCommand.msseq
           ProCommand.OrderId
           ProCommand.msRequest
           ProCommand.procommandtype
           lcCli
           ProCommand.CreatedTS   
           ProCommand.SendTS   
           ProCommand.ResponseTS
           ProCommand.ProCommandTarget
           ProCommand.ProCommandTargetURL
           ProCommand.ProCommandStatus
           ProCommand.ProCommandVerb 
           lcCommand_Ed 
           lcResponse_Ed 
           WITH FRAME lis.
   
        ASSIGN 
           lcCommand_Ed:READ-ONLY  = TRUE
           lcCommand_Ed:SENSITIVE  = TRUE
           lcResponse_Ed:READ-ONLY = TRUE
           lcResponse_Ed:SENSITIVE = TRUE.
           
        WAIT-FOR "F1" OF FRAME lis OR
                 "F2" OF FRAME lis OR
                 "F4" OF FRAME lis OR
                 "F8" OF FRAME lis.

        /* Dialog box to show messages */         
        IF KEYLABEL(LASTKEY) = "F1" THEN 
           RUN Mc/ProCommandJsonView.p (INPUT "Json command",
                                        INPUT lcCommand_Ed).
        ELSE IF KEYLABEL(LASTKEY) = "F2" THEN
           RUN Mc/ProCommandJsonView.p (INPUT "Json response",
                                        INPUT lcresponse_Ed).
        ELSE  
        DO: 
           HIDE FRAME lis NO-PAUSE.
           LEAVE.
        END.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.


PROCEDURE local-find-this:
    DEF INPUT PARAMETER exlock AS LOGICAL NO-UNDO.
    IF exlock THEN
       FIND ProCommand WHERE RECID(ProCommand) = rtab[frame-line(sel)] EXCLUSIVE-LOCK.
    ELSE
       FIND ProCommand WHERE RECID(ProCommand) = rtab[frame-line(sel)] NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
   IF iiMsSeq <> 0 THEN
      FIND FIRST ProCommand NO-LOCK WHERE 
                 ProCommand.MsSeq EQ iiMsSeq USE-INDEX Ix_MsSeq NO-ERROR. 
   ELSE
      FIND FIRST ProCommand NO-LOCK WHERE
                 Procommand.MsRequest EQ iiMsRequest USE-INDEX Ix_MsRequest NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF iiMsSeq <> 0 THEN
      FIND LAST ProCommand NO-LOCK WHERE 
                ProCommand.MsSeq EQ iiMsSeq USE-INDEX Ix_MsSeq NO-ERROR. 
   ELSE
      FIND LAST ProCommand NO-LOCK WHERE
                Procommand.MsRequest EQ iiMsRequest USE-INDEX Ix_MsRequest NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF iiMsSeq <> 0 THEN
      FIND NEXT ProCommand NO-LOCK WHERE 
                ProCommand.MsSeq EQ iiMsSeq USE-INDEX Ix_MsSeq NO-ERROR.
   ELSE
      FIND NEXT ProCommand NO-LOCK WHERE
                Procommand.MsRequest EQ iiMsRequest USE-INDEX Ix_MsRequest NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-prev:
   IF iiMsSeq <> 0 THEN
      FIND PREV ProCommand NO-LOCK WHERE 
                ProCommand.MsSeq = iiMsSeq USE-INDEX Ix_MsSeq NO-ERROR.
   ELSE
      FIND PREV ProCommand NO-LOCK WHERE
                Procommand.MsRequest EQ iiMsRequest USE-INDEX Ix_MsRequest NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
   
   DEF BUFFER mobsub FOR mobsub. 
   
   RUN local-find-others.
    
   /* set values */
   FIND mobsub WHERE mobsub.msseq = ProCommand.Msseq NO-LOCK NO-ERROR.
   ASSIGN
      ldtResponseTS = DATETIME-TZ(ProCommand.ResponseTS,TIMEZONE)
      lcCli         = (IF AVAILABLE mobsub THEN mobsub.cli ELSE "").
   
   DISPLAY
      ProCommand.msseq
      lcCli
      ProCommand.orderid
      ProCommand.ProCommandTarget
      ProCommand.ProCommandStatus
      ldtResponseTS
      ProCommand.ProCommandId
      WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.
