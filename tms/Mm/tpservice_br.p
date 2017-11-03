/* ----------------------------------------------------------------------
  MODULE .......: fusionmessage.p
  TASK .........: Display fusionmessage 
  APPLICATION ..: tms
  AUTHOR .......: ilsavola
  CREATED ......: 10.10.2016
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */
DEF INPUT PARAMETER iiMsSeq  AS INT NO-UNDO.

{Syst/commali.i}

DEF VAR xrecid          AS RECID                           init ?.
DEF VAR FIRSTrow        AS INT                    NO-UNDO  init 0.
DEF VAR order           AS INT                    NO-UNDO  init 1.
DEF VAR maxOrder        AS INT                    NO-UNDO  init 1.
DEF VAR ufkey           AS LOG                    NO-UNDO  init TRUE.
DEF VAR pr-order        AS INT                    NO-UNDO.
DEF VAR Memory          AS RECID                  NO-UNDO.
DEF VAR RowNo           AS INT                    NO-UNDO.
DEF VAR must-print      AS LOG                    NO-UNDO.
DEF VAR must-add        AS LOG                    NO-UNDO.
DEF VAR ac-hdr          AS CHAR                   NO-UNDO.
DEF VAR rtab            AS RECID EXTENT 24        NO-UNDO.
DEF VAR i               AS INT                    NO-UNDO.
DEF VAR lcCreatedTS     AS CHAR                   NO-UNDO.
DEF VAR lcUpdatedTS     AS CHAR                   NO-UNDO.
DEF VAR lcVoucherStatus AS CHAR                   NO-UNDO.

FORM
   TPService.Product      FORMAT "X(20)" COLUMN-LABEL "Product"
   TPService.ServType     FORMAT "X(15)" COLUMN-LABEL "Type"   
   TPService.Operation    FORMAT "X(12)" COLUMN-LABEL "Operation"   
   TPService.ServStatus   FORMAT "X(8)"  COLUMN-LABEL "Status"
   lcUpdatedTS            FORMAT "X(10)" COLUMN-LABEL "Updated"
   WITH ROW 1 CENTERED OVERLAY 15 DOWN COLOR VALUE(Syst.Var:cfc) 
   TITLE COLOR VALUE(Syst.Var:ctc) "Third Party Services" FRAME sel.

FORM
    "MsSeq .............:" TPService.MsSeq
    SKIP
    "Service Seq. ......:" TPService.ServSeq
    SKIP
    "Service Provider ..:" TPService.Provider
    SKIP
    "Service Type ......:" TPService.ServType
    SKIP
    "Operation .........:" TPService.Operation
    SKIP
    "Product ...........:" TPService.Product
    SKIP
    "Offer .............:" TPService.Offer
    SKIP
    "Serial Number .....:" TPService.SerialNbr
    SKIP
    "Sky TV Voucher ....:" TPService.SkyTvVoucher lcVoucherStatus AT 38 
    SKIP
    "External Id .......:" TPService.MessageId
    SKIP
    "Service Status ....:" TPService.ServStatus FORMAT "X(35)"
    SKIP
    "User Activated ....:" TPService.UserCode
    SKIP
    "Created Date ......:" lcCreatedTS FORMAT "X(24)"
    SKIP
    "Updated Date ......:" lcUpdatedTS FORMAT "X(24)" 
    SKIP
    "Cancellation Reason:" TPService.TermReason
    SKIP(1)
WITH OVERLAY ROW 1 WIDTH 80 centered
    COLOR VALUE(Syst.Var:cfc) TITLE COLOR VALUE(Syst.Var:ctc) "Third party service data" NO-LABELS FRAME fDetails.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.

RUN local-find-first.

IF AVAILABLE TPService THEN ASSIGN
   Memory       = recid(TPService)
   must-print   = TRUE
   must-add     = FALSE.
ELSE 
DO:
    MESSAGE "No Third party services available!" VIEW-AS ALERT-BOX.
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
        FIND TPService WHERE recid(TPService) = Memory NO-LOCK NO-ERROR.

        REPEAT WITH FRAME sel:
           IF AVAILABLE TPService THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(TPService).
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

   BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
          ASSIGN
              Syst.Var:ufk    = 0
              Syst.Var:ufk[8] = 8 
              Syst.Var:ehto   = 3 
              ufkey  = FALSE.

          RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW TPService.Product {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) TPService.Product WITH FRAME sel.
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
        FIND TPService WHERE recid(TPService) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE TPService THEN
              ASSIGN FIRSTrow = i Memory = recid(TPService).
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

      ASSIGN Syst.Var:nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE TPService THEN DO:
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
                rtab[1] = recid(TPService)
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
           IF NOT AVAILABLE TPService THEN DO:
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
              rtab[FRAME-DOWN] = recid(TPService).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND TPService WHERE recid(TPService) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE TPService THEN DO:
           Memory = recid(TPService).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE TPService THEN Memory = recid(TPService).
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
           FIND TPService WHERE recid(TPService) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 THEN DO:
        RUN local-find-this(FALSE).
        PAUSE 0. 
        
        ASSIGN
            lcCreatedTS     = Func.Common:mTS2HMS(TPService.CreatedTS)  
            lcUpdatedTS     = Func.Common:mTS2HMS(TPService.UpdateTS)
            lcVoucherStatus = (IF TPService.VoucherStatus <> "" THEN TPService.VoucherStatus ELSE "Unlocked").

        DISP TPService.MsSeq
             TPService.ServSeq
             TPService.Provider
             TPService.ServType
             TPService.Operation
             TPService.Product
             TPService.Offer
             TPService.SerialNbr
             TPService.SkyTvVoucher
             lcVoucherStatus
             TPService.MessageId
             TPService.ServStatus
             TPService.UserCode
             TPService.TermReason
             lcCreatedTS
             lcUpdatedTS
             WITH FRAME fDetails.

        TPSERVICEBROWSE:
        REPEAT:
            ASSIGN
             ufkey  = TRUE
             Syst.Var:ufk    = 0  
             Syst.Var:ehto   = 1  
             Syst.Var:ufk[1] = 9853
             Syst.Var:ufk[8] = 8.      

            RUN Syst/ufkey.p. 

            IF Syst.Var:toimi = 1  THEN 
                RUN Mm/tpservicemessage.p(TPService.MsSeq, TPService.ServSeq).
            ELSE IF Syst.Var:toimi = 8 THEN 
               LEAVE.
            
            ASSIGN  Memory = recid(TPService) must-print = TRUE.
            NEXT LOOP.
        END.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(TPService) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(TPService) must-print = TRUE.
        NEXT LOOP.
     END.
     
     IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(Syst.Var:nap,"8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN 
        LEAVE LOOP.
  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.

PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN 
        FIND TPService WHERE recid(TPService) = rtab[frame-line(sel)] EXCLUSIVE-LOCK.
    ELSE
        FIND TPService WHERE recid(TPService) = rtab[frame-line(sel)] NO-LOCK.

END PROCEDURE.

PROCEDURE local-find-FIRST:

       IF order = 1 THEN FIND FIRST TPService WHERE TPService.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
         
END PROCEDURE.

PROCEDURE local-find-LAST:

       IF order = 1 THEN FIND LAST TPService WHERE TPService.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
 
END PROCEDURE.

PROCEDURE local-find-NEXT:

       IF order = 1 THEN FIND NEXT TPService WHERE TPService.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
          
END PROCEDURE.

PROCEDURE local-find-PREV:
 
       IF order = 1 THEN FIND PREV TPService WHERE TPService.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
  
END PROCEDURE.

PROCEDURE local-disp-row:
    
    CLEAR FRAME sel NO-PAUSE.
    
    ASSIGN lcUpdatedTS = Func.Common:mTS2HMS(TPService.UpdateTS).

    DISPLAY 
       TPService.Product
       TPService.ServType
       TPService.Operation
       TPService.ServStatus
       lcUpdatedTS
       WITH FRAME sel.

END PROCEDURE.
