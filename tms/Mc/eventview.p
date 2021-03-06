/* ----------------------------------------------------------------------
  MODULE .......: eventview.p
  TASK .........: show modified fields FOR an Event
  APPLICATION ..: TMS
  AUTHOR .......: tk
  CREATED ......: 03-05-02
  CHANGED ......: 23.09.02/jr  removed unworking function keys
                  23.09.02/jr  changed labels and title
                  25.09.02/aam f8 works also on empty screen 
                  26.10.04/aam input iiEventLog instead of table etc.,
                               change labels for EventLogStatus 4

                  31.01.07 kl  muutokset => Changed
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i} /* Syst.Var:qupd = TRUE. */

DEF INPUT PARAMETER iiEventLog AS RECID NO-UNDO.

DEF VAR TableName    LIKE Eventlog.TableName   NO-UNDO. 
DEF VAR UserCode     LIKE Eventlog.UserCode    NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 12.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR x            AS INT                    NO-UNDO.

DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEFINE VARIABLE Changed AS CHARACTER NO-UNDO.
DEF VAR valuecount AS INTEGER NO-UNDO.
DEF VAR lcFieldName  AS C                      NO-UNDO.

FIND EventLog WHERE RECID(EventLog) = iiEventLog NO-LOCK.

valuecount = num-entries(Eventlog.DataValues, CHR(255)) / 3. 

DEFINE TEMP-TABLE temp-event 
   FIELD FieldName AS CHAR
   FIELD fieldcont AS CHAR
   FIELD oldvalue  AS CHAR
   FIELD newvalue  AS CHAR.

DO i = 0 TO valuecount - 1 :
   CREATE temp-event.
   temp-event.FieldName = entry(3 * i + 1,Eventlog.DataValues,CHR(255)).
   temp-event.oldvalue  = entry(3 * i + 2,Eventlog.DataValues,CHR(255)).
   temp-event.newvalue  = entry(3 * i + 3,Eventlog.DataValues,CHR(255)).
   /* get description FOR FIELD from data dictionary */
   FIND _File WHERE _file._File-name = Eventlog.TableName NO-LOCK NO-ERROR.
   IF AVAIL _file THEN DO:
      LcFieldName = temp-event.FieldName.
      X = INDEX(lcFieldName,"[").
      IF x > 0 THEN lcFieldName = TRIM(SUBSTR(lCfieldName,1,x - 1)).

      FIND _field OF _file WHERE 
           _field._field-name = LCfieldname NO-LOCK NO-ERROR.
      IF AVAIL _field 
         THEN  temp-event.fieldcont = _field._label.
         ELSE  temp-event.fieldcont = "! UNKNOWN !".

   END. /* IF AVAIL _field */

END. 

form
   temp-event.FieldName format "x(15)" LABEL "Code"
   temp-event.fieldcont format "x(15)" LABEL "Name"
   temp-event.oldvalue  format "x(20)" LABEL "Old"
   temp-event.newvalue  format "x(20)" LABEL "New"
   WITH ROW FrmRow centered OVERLAY FrmDown  DOWN
    COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc) 
    " VIEW AN Event " + STRING(Eventlog.EventDate,"99-99-99") + " " + 
    Eventlog.eventtime
    FRAME sel.

form
    Eventlog.TableName     /* LABEL FORMAT */
    Eventlog.EventDate
    Eventlog.EventTime  
    Eventlog.Action        /* LABEL FORMAT */
    Eventlog.UserCode format "x(30)"
    Eventlog.Key      
    Changed VIEW-AS EDITOR size-chars 50 BY 10
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form /* seek Eventlog  BY  TableName */
    TableName
    HELP "Enter Tablename"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND TableName "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek Eventlog  BY UserCode */
    UserCode
    HELP "Enter Usercode"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND User "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f2.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.

/* routine run log */
IF EventLog.EventLogStatus = 4 THEN
ASSIGN temp-event.oldvalue:LABEL IN FRAME sel = "From"
       temp-event.newvalue:LABEL IN FRAME sel = "To".

orders = "By TableName,   By User  ,   By Date   ,   By time   ".


FIND FIRST temp-event
/* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE temp-event THEN ASSIGN
   memory       = recid(temp-event)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   memory       = ?
   must-print   = FALSE
   must-add     = FALSE.

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
        FIND temp-event WHERE recid(temp-event) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE temp-event THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(temp-event).
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
          Syst.Var:ufk[8]= 8 
          Syst.Var:ufk[9]= 1
        Syst.Var:ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW temp-event.FieldName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) temp-event.FieldName WITH FRAME sel.
      END.

      Syst.Var:nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(Syst.Var:nap,"5,f5,8,f8") = 0 THEN DO:
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
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-LINE].
        FIND temp-event WHERE recid(temp-event) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE temp-event THEN
              ASSIGN FIRSTrow = i memory = recid(temp-event).
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
           IF NOT AVAILABLE temp-event THEN DO:
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
                rtab[1] = recid(temp-event)
                memory  = rtab[1].
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
           IF NOT AVAILABLE temp-event THEN DO:
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
              rtab[FRAME-DOWN] = recid(temp-event).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND temp-event WHERE recid(temp-event) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE temp-event THEN DO:
           memory = recid(temp-event).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE temp-event THEN memory = recid(temp-event).
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
           memory = rtab[FRAME-DOWN].
           FIND temp-event WHERE recid(temp-event) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */
     ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).
       Syst.Var:cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.

       RUN local-update-record.                       
       HIDE FRAME lis. /* NO-PAUSE.*/

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(temp-event).
       LEAVE.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(temp-event) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(temp-event) must-print = TRUE.
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
      FIND temp-event WHERE recid(temp-event) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND temp-event WHERE recid(temp-event) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 2 THEN FIND FIRST temp-event /* USE-INDEX username*/
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST temp-event
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST temp-event /*USE-INDEX username*/
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT temp-event
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT temp-event /*USE-INDEX UserName */
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV temp-event
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV temp-event /*USE-INDEX username*/
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       /* FIND additional information from other tables FOR DISPLAY */
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
         temp-event.FieldName 
         temp-event.fieldcont
         temp-event.oldvalue  
         temp-event.newvalue  
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-update-record:

   Changed = REPLACE(EventLog.datavalues,CHR(255),CHR(10)).
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
      WITH FRAME lis. 
  /*    UPDATE  */
     DISP
          Eventlog.UserCode    
          Eventlog.EventDate
          Eventlog.EventTime
          Eventlog.Action
          Eventlog.Key    
          Changed
      WITH FRAME lis.
      LEAVE.
   END.
END PROCEDURE.

