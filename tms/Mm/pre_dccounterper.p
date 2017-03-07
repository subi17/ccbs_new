/* ----------------------------------------------------------------------
  MODULE .......: pre_dccounterper.p
  TASK .........: PrePaid DCCounter
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 25/05/2011
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

{Syst/commali.i}
{Rate/daycampaign.i}

DEF  INPUT PARAM iiMsseq       AS INT    NO-UNDO.
DEF  INPUT PARAM icEvent       AS CHAR   NO-UNDO.
DEF  INPUT PARAM iiSLSeq       AS INT    NO-UNDO.
DEF  INPUT PARAM iiMSID        AS INT    NO-UNDO.
DEF  INPUT PARAM idtFrom       AS DATE   NO-UNDO.
DEF  INPUT PARAM idtTo         AS DATE   NO-UNDO. 

DEF VAR lcCampaignEvent LIKE DayCampaign.dcName NO-UNDO.
DEF VAR xrecid     AS RECID                           init ?.
DEF VAR firstline  AS INT                    NO-UNDO  init 0.
DEF VAR order      AS INT                    NO-UNDO  init 1.
DEF VAR ordercount AS INT                    NO-UNDO  init 1.
DEF VAR ufkey      AS LOG                    NO-UNDO  init TRUE.
DEF VAR delline    AS INT                    NO-UNDO  init 0.
DEF VAR ex-order   AS INT                    NO-UNDO.
DEF VAR memory     AS RECID                  NO-UNDO.
def var line       as int format "99"        NO-UNDO.
DEF VAR must-print AS LOG                    NO-UNDO.
DEF VAR fr-header  AS CHAR                   NO-UNDO.
DEF VAR rtab       AS RECID EXTENT 24        NO-UNDO.
DEF VAR i          AS INT                    NO-UNDO.
def var ok         as log format "Yes/No"    NO-UNDO.
DEF VAR liPeriod   AS INT                    NO-UNDO FORMAT "999999".
DEF VAR lcStatus   AS CHAR                   NO-UNDO.

form
   liPeriod                            COLUMN-LABEL "Period"
   PPInvSeq.FromDate    format 99-99-99  Column-label "From"
   PPInvSeq.Todate      FORMAT 99-99-99  Column-label "To"
   lcStatus           FORMAT "X(12)"   COLUMN-LABEL "Status"

WITH OVERLAY CENTERED  scroll 3 15 DOWN
   COLOR value(cfc)
   title color value(ctc) " " +
   " COUNTERS for " + icEvent + " " 
   + string(pvm,"99-99-99") 
   FRAME sel.


cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.

IF idtFrom = ? THEN idtFrom = 12/1/2006.
IF idtTo = ? THEN idtTo = 12/31/2049.

RUN LOCAL-FIND-FIRST.

IF AVAILABLE PPInvSeq THEN ASSIGN
   memory       = recid(PPInvSeq)
   must-print = TRUE.
ELSE ASSIGN
   memory       = ?
   must-print = FALSE.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
    END.

   print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND PPInvSeq where recid(PPInvSeq) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose keyvalue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE PPInvSeq THEN DO:
              RUN LOCAL-DISP-ROW.
              rtab[FRAME-LINE] = recid(PPInvSeq).
              RUN LOCAL-FIND-NEXT.
           END.
           ELSE DO:
              CLEAR no-pause.
              rtab[FRAME-LINE] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.
        up FRAME-LINE - 1.
        DOWN firstline.
        ASSIGN firstline = 0
               must-print = FALSE
               .
        PAUSE 0 no-message.

        /* one page of data has been printed AND
        the cursor is in the upmost line FOR 'choose' */
      END. /* must-print = TRUE */
   END. /* print-line */

   /* IF lastly a line has been deleted */
   IF delline > 0 THEN DOWN delline - 1.
   ASSIGN delline = 0.

BROWSE:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
        ASSIGN
        ufk   = 0
        ufk[1]= 35
        ufk[5] = 968
        ufk[6]= 0 /* 4   */
        ufk[8]= 8.
        
        IF iiMsseq > 0 THEN ufk[1] = 0.
        ASSIGN ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      
      
      IF order = 1 THEN DO:
        CHOOSE ROW liPeriod {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) liPeriod WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW PPInvSeq.FromDate {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) PPInvSeq.FromDate WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"5,f5,8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.

      if lookup(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > ordercount THEN order = 1.
      END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = ordercount.
      END.

      IF order <> ex-order THEN DO:
        ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
        FIND PPInvSeq where recid(PPInvSeq) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           RUN LOCAL-FIND-PREV.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           FIND PPInvSeq where recid(PPInvSeq) = rtab[1] no-lock.
           RUN LOCAL-FIND-PREV.
           IF NOT AVAILABLE PPInvSeq THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* a previous one was found */
              scroll DOWN.
              RUN LOCAL-DISP-ROW.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(PPInvSeq)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND PPInvSeq where recid(PPInvSeq) = rtab[FRAME-DOWN] no-lock .
           RUN LOCAL-FIND-NEXT.
           IF NOT AVAILABLE PPInvSeq THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* yet another record was found */
              scroll up.
              RUN LOCAL-DISP-ROW.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(PPInvSeq).
              /* finally LAST line's keyvalue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND PPInvSeq where recid(PPInvSeq) = memory no-lock no-error.
        RUN LOCAL-FIND-PREV.
        IF AVAILABLE PPInvSeq THEN DO:
           memory = recid(PPInvSeq).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              RUN LOCAL-FIND-PREV.
              IF AVAILABLE PPInvSeq THEN memory = recid(PPInvSeq).
              ELSE line = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* this is the FIRST data page */
           message "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 no-message.
        END.
     END. /* previous page */

     /* NEXT page */
     else if lookup(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* cursor TO the downmost line */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           message "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-message.
       END.
       ELSE DO: /* the downmost line wasn't empty */
           memory = rtab[FRAME-DOWN].
           FIND PPInvSeq where recid(PPInvSeq) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */


     else if lookup(nap,"enter,return,f5") > 0 THEN DO WITH FRAME lis :
        /* change */

         FIND FIRST PPInvSeq where 
             recid(PPInvSeq) = rtab[frame-line(sel)] NO-LOCK NO-ERROR.
         
         RUN LOCAL-FIND-OTHERS.
         
         FIND FIRST DayCampaign WHERE
            DayCampaign.Brand = gcBrand AND
            DayCampaign.DCEvent = icEvent NO-LOCK. 

         IF DayCampaign.DCType EQ "8" AND
            DayCampaign.DurUnit = 1 THEN DO:
               liPeriod = YEAR(idtFrom) * 10000 +
                          MONTH(idtFrom) * 100 +
                          DAY(idtFrom).
         END.
         
         /* dccounter for rating limits */
         IF AVAIL DayCampaign AND DayCampaign.DCType = "2" THEN DO:
            RUN Mm/dccounter.p(
               PPInvSeq.MSSEQ,icEvent,PPInvSeq.fromdate,PPInvSeq.todate).
         END.
         ELSE DO:
            RUN Mm/servicelcounter.p(
               PPInvSeq.MSSEQ,0,iiSLSeq,icEvent,liPeriod,iimsid).
         END.     

         ufkey = True.
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       RUN LOCAL-FIND-FIRST.
       ASSIGN memory = recid(PPInvSeq) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       RUN LOCAL-FIND-LAST.
       ASSIGN memory = recid(PPInvSeq) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

PROCEDURE LOCAL-FIND-OTHERS:
   
   liperiod = YEAR(PPInvSeq.FromDate) * 100 +
              MONTH(PPInvSeq.FromDate).

END.

PROCEDURE LOCAL-DISP-ROW: 
   
   RUN LOCAL-FIND-OTHERS.
   
   IF PPInvSeq.Billed = FALSE THEN lcStatus = "Unbilled".
   ELSE                          lcStatus = "Billed".
   
   DISPLAY
      liPeriod
      PPInvSeq.FromDate
      PPInvSeq.Todate
      lcStatus
   WITH FRAME sel.
   
END PROCEDURE.

PROCEDURE LOCAL-FIND-NEXT.
   IF Order = 1 AND iiMSSEQ > 0 THEN 
      FIND NEXT PPInvSeq WHERE 
                PPInvSeq.MSSeq = iiMsseq AND
                PPInvSeq.ToDate >= idtFrom AND
                PPInvSeq.FromDate <= idtTo NO-LOCK NO-ERROR. 
   
   ELSE IF order = 1 THEN
      FIND NEXT PPInvSeq  NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-FIND-PREV.

   IF Order = 1 AND iiMSSEQ > 0 THEN
       FIND PREV PPInvSeq WHERE
                 PPInvSeq.MSSeq = iiMsseq AND
                 PPInvSeq.ToDate >= idtFrom AND
                 PPInvSeq.FromDate <= idtTo NO-LOCK NO-ERROR.
   ELSE 
   IF order = 1 THEN
      FIND PREV PPInvSeq  NO-LOCK NO-ERROR.
         
END PROCEDURE.

PROCEDURE LOCAL-FIND-FIRST.

   IF Order = 1 AND iiMSSEQ > 0 THEN
      FIND FIRST PPInvSeq WHERE
                 PPInvSeq.MSSeq     = iiMsseq AND
                 PPInvSeq.ToDate >= idtFrom AND
                 PPInvSeq.FromDate <= idtTo NO-LOCK NO-ERROR.
   ELSE IF order = 1 THEN
         FIND FIRST PPInvSeq  NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-FIND-LAST.
   IF Order = 1 AND iiMSSEQ > 0 THEN
      FIND LAST  PPInvSeq WHERE
                 PPInvSeq.MSSeq     = iiMsseq AND
                 PPInvSeq.ToDate >= idtFrom AND
                 PPInvSeq.FromDate <= idtTo NO-LOCK NO-ERROR.
      ELSE IF order = 1 THEN
         FIND LAST PPInvSeq  NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE LOCAL-UPDATE-RECORD. 

   DISP
      liPeriod 
      PPInvSeq.FromDate
      PPInvSeq.Todate
      lcStatus
   WITH FRAME lis.

   MESSAGE " - PRESS ENTER TO CONTINUE - " . PAUSE NO-MESSAGE.                               
   HIDE FRAME lis.
   
END.


