/* ----------------------------------------------------------------------
  MODULE .......: MsRequestParam
  TASK .........: Additional MsRequest Parameters
  APPLICATION ..: 
  AUTHOR .......: Subhash Sanjeevi
  CREATED ......: 12-07-2018
  CHANGED ......: 
  Version ......: 
  ---------------------------------------------------------------------- */

{Syst/commali.i}  
{Syst/eventval.i}
{Func/cparam2.i}

DEF TEMP-TABLE ttMsRequestParam LIKE MsRequestParam
   FIELD FinalValue AS CHAR.

FUNCTION fCollect RETURNS LOGICAL
   (INPUT iiMsRequest AS INT):

   FOR EACH MsRequestParam NO-LOCK WHERE
            MsRequestParam.MsRequest EQ iiMsRequest:
      CREATE ttMsRequestParam.
      BUFFER-COPY MsRequestParam TO ttMsRequestParam.

      CASE MsRequestParam.ParamType:
         WHEN "C"  THEN ttMsRequestParam.FinalValue = MsRequestParam.CharValue.
         WHEN "I"  THEN ttMsRequestParam.FinalValue = STRING(MsRequestParam.IntValue). 
         WHEN "DE" THEN ttMsRequestParam.FinalValue = STRING(MsRequestParam.DecValue).
         WHEN "DT" THEN ttMsRequestParam.FinalValue = STRING(MsRequestParam.DateValue).
         OTHERWISE .
      END CASE.
   END.

END FUNCTION.

DEF INPUT PARAMETER iiMsRequest AS INT NO-UNDO.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR Memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.

FORM 
   ttMsRequestParam.MsRequest  COLUMN-LABEL "Request ID"
   ttMsRequestParam.ParamName  COLUMN-LABEL "Parameter Name"
   ttMsRequestParam.ParamType  COLUMN-LABEL "Parameter Type"
   ttMsRequestParam.FinalValue FORMAT "X(20)" COLUMN-LABEL "Parameter Value"

   WITH ROW 1 CENTERED OVERLAY 15 DOWN 
   COLOR VALUE(Syst.Var:cfc)
   TITLE COLOR VALUE(Syst.Var:ctc) " " + Syst.Var:ynimi + 
   " Additional Parameters " 
   + STRING(iiMsRequest) + " "
   FRAME sel.

FORM 
   "Request ID........" ttMsRequestParam.MsRequest
   SKIP
   "Parameter Name...." ttMsRequestParam.ParamName
   SKIP
   "Parameter Type...." ttMsRequestParam.ParamType   FORMAT "X(25)"
   SKIP
   "Parameter Value..." ttMsRequestParam.FinalValue  FORMAT "X(25)"
   SKIP(2)

   WITH OVERLAY ROW 6 WIDTH 65 centered
   COLOR VALUE(Syst.Var:cfc)
   TITLE COLOR VALUE(Syst.Var:ctc)
   " VIEW " NO-LABELS 
   FRAME fDetails.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.

fCollect(iiMsRequest).

RUN local-find-first.

IF AVAILABLE ttMsRequestParam THEN ASSIGN
   Memory       = recid(ttMsRequestParam)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE
   must-add     = FALSE.

LOOP:
REPEAT WITH FRAME sel:

   IF order <> pr-order AND MaxOrder NE 1 THEN DO:
      pr-order = order.
   END.
    
   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ttMsRequestParam WHERE recid(ttMsRequestParam) = Memory NO-LOCK NO-ERROR.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttMsRequestParam THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttMsRequestParam).
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
        Syst.Var:ufk[4] = 0
        Syst.Var:ufk[8] = 8 
        Syst.Var:ehto   = 3 
        ufkey  = FALSE.

        RUN Syst/ufkey.p.
        
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ttMsRequestParam.ParamName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) ttMsRequestParam.ParamName WITH FRAME sel.
      END.

      Syst.Var:nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(Syst.Var:nap,"8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.

      IF LOOKUP(Syst.Var:nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(Syst.Var:nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND ttMsRequestParam WHERE recid(ttMsRequestParam) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttMsRequestParam THEN
              ASSIGN FIRSTrow = i Memory = recid(ttMsRequestParam).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* PREVious ROW */
      IF LOOKUP(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE ttMsRequestParam THEN DO:
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
                rtab[1] = recid(ttMsRequestParam)
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
           IF NOT AVAILABLE ttMsRequestParam THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttMsRequestParam).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttMsRequestParam WHERE recid(ttMsRequestParam) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttMsRequestParam THEN DO:
           Memory = recid(ttMsRequestParam).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttMsRequestParam THEN Memory = recid(ttMsRequestParam).
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
           FIND ttMsRequestParam WHERE recid(ttMsRequestParam) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttMsRequestParam) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttMsRequestParam) must-print = TRUE.
        NEXT LOOP.
     END.
 
     ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 THEN DO:
        RUN local-find-this(FALSE).
        PAUSE 0. 
        DISPLAY
           ttMsRequestParam.MsRequest
           ttMsRequestParam.ParamName
           ttMsRequestParam.ParamType
           ttMsRequestParam.FinalValue
        WITH FRAME fDetails.
        
        RUN local-find-this(TRUE).
        Syst.Var:cfc = "fDetails". RUN Syst/ufcolor.p. CLEAR FRAME fDetails NO-PAUSE.

        RUN local-update-record.
        HIDE FRAME fDetails. /* NO-PAUSE.*/

        /* IF  User Wanted TO Cancel this Change TRANSACTION */
        IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
        KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

        RUN local-disp-row.
        xrecid = recid(ttMsRequestParam).
        LEAVE.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.

PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND ttMsRequestParam WHERE recid(ttMsRequestParam) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttMsRequestParam WHERE recid(ttMsRequestParam) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

       IF order = 1 THEN FIND FIRST ttMsRequestParam NO-LOCK NO-ERROR.
         
END PROCEDURE.

PROCEDURE local-find-LAST:

       IF order = 1 THEN FIND LAST ttMsRequestParam NO-LOCK NO-ERROR.
 
END PROCEDURE.

PROCEDURE local-find-NEXT:

       IF order = 1 THEN FIND NEXT ttMsRequestParam NO-LOCK NO-ERROR.
          
END PROCEDURE.

PROCEDURE local-find-PREV:
 
       IF order = 1 THEN FIND PREV ttMsRequestParam NO-LOCK NO-ERROR.
  
END PROCEDURE.

PROCEDURE local-disp-row:

       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
           ttMsRequestParam.MsRequest
           ttMsRequestParam.ParamName
           ttMsRequestParam.ParamType
           ttMsRequestParam.FinalValue
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-update-record:

   REPEAT ON ENDKEY UNDO, LEAVE:

       DISPLAY
           ttMsRequestParam.MsRequest
           ttMsRequestParam.ParamName
           ttMsRequestParam.ParamType
           ttMsRequestParam.FinalValue
      WITH FRAME fDetails.

      PAUSE MESSAGE "Press ENTER".

      LEAVE.
   END.
END PROCEDURE.


