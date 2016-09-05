/* -----------------------------------------------
  MODULE .......: DCCounter
  FUNCTION .....: Dccounter browswe
  APPLICATION ..: TMS
  AUTHOR .......: JP
  CREATED ......: 22-11-05
  MODIFIED .....: 08.02.06/aam to TF
  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Rate/daycampaign.i}

DEF  INPUT PARAMETER iiMsseq       AS INT    NO-UNDO.
DEF  INPUT PARAMETER icEvent       AS CHAR   NO-UNDO.
DEF  INPUT PARAMETER idtFrom       AS DATE   NO-UNDO.
DEF  INPUT PARAMETER idtTo         AS DATE   NO-UNDO. 

DEF VAR liPeriod   AS INT   NO-UNDO.

liperiod = YEAR(idtFrom) * 100 +
           MONTH(idtTo).

DEF VAR lcEvent    LIKE DCCounter.DCEvent    NO-UNDO.
DEF VAR lccli      LIKE dccli.cli            NO-UNDO.
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
DEF VAR must-add   AS LOG                    NO-UNDO.
DEF VAR fr-header  AS CHAR                   NO-UNDO.
DEF VAR rtab       AS RECID EXTENT 24        NO-UNDO.
DEF VAR i          AS INT                    NO-UNDO.
DEF VAR iLoop      AS INT                    NO-UNDO.
def var ok         as log format "Yes/No"    NO-UNDO.
DEF VAR lcWeekDay  AS CHAR                   NO-UNDO.
DEF VAR lcCalcMethod AS CHAR                 NO-UNDO FORMAT "X(40)" .
DEF VAR lcUnit       AS CHAR                 NO-UNDO FORMAT "X(40)" .
DEF VAR lcTypeName AS CHAR                   NO-UNDO FORMAT "X(40)" .
DEF VAR lcAmount   AS CHAR                   NO-UNDO.
DEF VAR lcMaxCharge AS CHAR                   NO-UNDO.
DEF VAR ldaDCDateTo AS DATE                  NO-UNDO.


DEF BUFFER xxDCCounter FOR DCCounter.

form
   
   DCCounter.DCDate    format 99-99-99  Column-label "From"
   ldaDCDateTo    format 99-99-99  Column-label "To"
   lcUnit              FORMAT "x(8)"    Column-label "Unit"
   lcMaxcharge                         Column-label "Limit"
   lcAmount                            column-label "Usage"
 
WITH OVERLAY CENTERED  scroll 1 15 DOWN
   COLOR value(cfc)
   title color value(ctc) " " + /*ynimi +*/
   " COUNTERS for period " + STRING(liPeriod) + " " + icEvent + " "
   FRAME sel.

form 
   "Calculation method:" DayCampaign.CalcMethod lcCalcMethod AT 24 SKIP
   "Period type.......:" DayCampaign.DurUnit lcTypeName AT 24 SKIP
   "Counter Period....:" DCCounter.DCDate "-" ldaDCDateTo format 99-99-99 
   SKIP
"----------------------------------------------------------------------------"
   SKIP 
   "Included unit :" DayCampaign.InclUnit lcUnit SKIP
   "Counter limit :" lcMaxCharge SKIP
   "Usage ........:" lcAmount SKIP

WITH OVERLAY ROW 2 centered
   COLOR value(cfc)
   TITLE COLOR value(ctc)
   fr-header WITH no-labels side-labels
   FRAME lis.

form /*  search WITH FIELD DCCounter */
    lcEvent
    help "Give ...."
    with row 4 col 2 title color value(ctc) " FIND xxxxxxx "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME haku-f1.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.

FUNCTION fDispUnit RETURNS LOGICAL
   (iiUnit AS INT).
   IF iiUnit > 0 THEN
      lcUnit = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
               "DayCampaign","InclUnit",STRING(iiUnit)).
   ELSE lcUnit = "".
   DISPLAY lcUnit WITH FRAME lis.
   RETURN (iiUnit = 0 OR (iiUnit > 0 AND lcUnit > "")).
END FUNCTION.


FUNCTION fDispUnit2 RETURNS LOGICAL
   (iiUnit AS INT).
   
   IF iiUnit > 0 THEN
   lcUnit = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
   "Tariff","DataType",STRING(iiUnit)).
   ELSE lcUnit = "".
                                  
   DISPLAY lcUnit   WITH FRAME lis.
                                       
   RETURN (iiUnit = 0 OR (iiUnit > 0 AND lcUnit > "")).

END FUNCTION.




RUN LOCAL-FIND-FIRST.

IF AVAILABLE DCCounter THEN ASSIGN
   memory       = recid(DCCounter)
   must-print = TRUE
   must-add    = FALSE.
ELSE ASSIGN
   memory       = ?
   must-print = FALSE
   must-add    = FALSE.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
    END.

   IF must-add THEN DO:  /* DCCounter -ADD  */
      HIDE FRAME lis.
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.
      
      add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 5. RUN Syst/ufkey.p.
        DO TRANSACTION:

           CREATE DCCounter.

           RUN LOCAL-UPDATE-RECORD(true).
           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
           DCCounter.DCEvent = "" THEN
           UNDO add-new, LEAVE add-new.
           ASSIGN
              memory = recid(DCCounter)
              xrecid = memory
              must-add = false.
           LEAVE add-new.
        END.
      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      RUN LOCAL-FIND-FIRST.
      IF NOT AVAILABLE DCCounter THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND DCCounter where recid(DCCounter) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose keyvalue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE DCCounter THEN DO:
              RUN LOCAL-DISP-ROW.
              rtab[FRAME-LINE] = recid(DCCounter).
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
        ufk[5] = 11 
        ufk[6]= 0 /* 4   */
        ufk[8]= 8.
        
        IF iiMsseq > 0 THEN ufk[1] = 0.
        ASSIGN ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      
      
      IF order = 1 THEN DO:
        CHOOSE ROW DCCounter.dcdate {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) DCCounter.dcdate WITH FRAME sel.
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
        FIND DCCounter where recid(DCCounter) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           RUN LOCAL-FIND-PREV.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           FIND DCCounter where recid(DCCounter) = rtab[1] no-lock.
           RUN LOCAL-FIND-PREV.
           IF NOT AVAILABLE DCCounter THEN DO:
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
              rtab[1] = recid(DCCounter)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND DCCounter where recid(DCCounter) = rtab[FRAME-DOWN] no-lock .
           RUN LOCAL-FIND-NEXT.
           IF NOT AVAILABLE DCCounter THEN DO:
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
              rtab[FRAME-DOWN] = recid(DCCounter).
              /* finally LAST line's keyvalue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND DCCounter where recid(DCCounter) = memory no-lock no-error.
        RUN LOCAL-FIND-PREV.
        IF AVAILABLE DCCounter THEN DO:
           memory = recid(DCCounter).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              RUN LOCAL-FIND-PREV.
              IF AVAILABLE DCCounter THEN memory = recid(DCCounter).
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
           FIND DCCounter where recid(DCCounter) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     else if lookup(nap,"enter,return,f5") > 0 THEN DO WITH FRAME lis TRANSACTION:
       /* change */
       FIND FIRST DCCounter where 
            recid(DCCounter) = rtab[frame-line(sel)]
       /*exclusive-lock*/ no-lock.
       assign fr-header = " VIEW " ufkey = TRUE ehto = 5.
       RUN Syst/ufkey.p.

       cfc = "lis". RUN Syst/ufcolor.p.

       RUN LOCAL-UPDATE-RECORD(FALSE).
       
       xrecid = recid(DCCounter).

     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       RUN LOCAL-FIND-FIRST.
       ASSIGN memory = recid(DCCounter) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       RUN LOCAL-FIND-LAST.
       ASSIGN memory = recid(DCCounter) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

PROCEDURE LOCAL-DISP-ROW: 
   
   FIND FIRST BillItem WHERE 
              BillItem.Brand    = gcBrand AND
              BillItem.BillCode = DCCounter.BillCode 
   NO-LOCK NO-ERROR.
    
   FIND FIRST dccli WHERE 
              dccli.msseq = dccounter.msseq  AND 
              dccli.dcevent = dccounter.dcevent no-lock no-error.
   IF avail DCcli THEN lccli = dccli.cli.           
   
   RUN LOCAL-FIND-OTHERS.

   DISPLAY
      DCCounter.DCDate @ ldaDCDateTo 
      DCCounter.DCDate 
      lcUnit
      lcMaxCharge 
      lcAmount  
   WITH FRAME sel.
   
END PROCEDURE.

PROCEDURE LOCAL-FIND-NEXT.
   IF      ORder = 1 AND iiMSSEQ > 0 THEN 
      FIND NEXT DCCounter WHERE 
                dcCounter.Msseq   = iiMsseq AND 
                dccounter.DCDate >= idtFrom AND
                dccounter.DCDate <= idtTo   AND
                dccounter.dcevent = icEvent NO-LOCK NO-ERROR. 
   
   ELSE IF order = 1 THEN
      FIND NEXT DCCounter  NO-LOCK NO-ERROR.
   ELSE IF  ORder = 2 AND iiMSSEQ > 0 THEN
      FIND NEXT DCCounter WHERE
                dcCounter.Msseq = iiMsseq NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND NEXT DCCounter USE-INDEX DCEvent
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-FIND-PREV.

   IF      ORder = 1 AND iiMSSEQ > 0 THEN
       FIND PREV DCCounter WHERE
                 dcCounter.Msseq   = iiMsseq AND 
                 dccounter.DCDate >= idtFrom AND
                 dccounter.DCDate <= idtTo   AND
                 dccounter.dcevent = icEvent NO-LOCK NO-ERROR.
   ELSE 
   IF order = 1 THEN
      FIND PREV DCCounter  NO-LOCK NO-ERROR.
   ELSE IF  ORder = 2 AND iiMSSEQ > 0 THEN
      FIND PREV DCCounter WHERE
                dcCounter.Msseq = iiMsseq NO-LOCK NO-ERROR.
         
   ELSE
   IF order = 2 THEN 
      FIND PREV DCCounter USE-INDEX DCEvent
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-FIND-FIRST.

   IF      ORder = 1 AND iiMSSEQ > 0 THEN
      FIND FIRST DCCounter WHERE
                dcCounter.Msseq   = iiMsseq AND 
                dccounter.DCDate >= idtFrom AND
                dccounter.DCDate <= idtTo   AND
                dccounter.dcevent = icEvent
                NO-LOCK NO-ERROR.
   ELSE IF order = 1 THEN
         FIND FIRST DCCounter  NO-LOCK NO-ERROR.
   ELSE IF order = 2 AND iimsseq > 0 THEN
         FIND FIRST DCCounter USE-INDEX DCEvent
          NO-LOCK NO-ERROR.
                  
                  
   ELSE
   IF order = 2 THEN 
      FIND FIRST DCCounter USE-INDEX DCEvent
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-FIND-LAST.
   IF      ORder = 1 AND iiMSSEQ > 0 THEN
      FIND LAST  DCCounter WHERE
                 dcCounter.Msseq   = iiMsseq AND 
                 dccounter.DCDate >= idtFrom AND
                 dccounter.DCDate <= idtTo   AND
                 dccounter.dcevent = icEvent
                 NO-LOCK NO-ERROR.
   ELSE  IF order = 1 THEN
         FIND LAST DCCounter  NO-LOCK NO-ERROR.
   ELSE
   IF      ORder = 2 AND iiMSSEQ > 0 THEN
        FIND LAST  DCCounter WHERE
                   dcCounter.Msseq = iiMsseq NO-LOCK NO-ERROR.
   ELSE
   IF order = 2 THEN 
      FIND LAST DCCounter USE-INDEX DCEvent
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-others.
   
   FIND FIRST DayCampaign WHERE
      DayCampaign.Brand = gcBrand AND
      DayCampaign.DCEvent = dccli.dcevent NO-LOCK NO-ERROR.
   
   FIND FIRST TMSCodes WHERE
              TMSCodes.Tablename    = "Tariff"   AND
              TMSCodes.FieldName    = "DataType"      AND
              TMSCodes.CodeGroup    = "Tariff"          AND
              TMSCodes.CodeValue    = STRING(DayCampaign.InclUnit)
   NO-LOCK NO-ERROR. 
   IF AVAIL TMSCodes THEN lcUnit = TMSCodes.CodeName. 
   
   CASE DayCampaign.InclUnit:
      WHEN 1 THEN DO:
         lcMaxcharge = STRING(INT(DCCounter.MaxCharge * 60),"HH:MM:SS").
         lcAmount = STRING(INT(DCCounter.Amount * 60),"HH:MM:SS").
      END.
      WHEN 2 THEN DO:
         lcMaxcharge = STRING(INT(DCCounter.MaxCharge),"HH:MM:SS").
         lcAmount = STRING(INT(DCCounter.Amount),"HH:MM:SS").
      END.
      WHEN 6 THEN DO:
         lcMaxcharge = STRING(DCCounter.MaxCharge,">>9.999").
         lcAmount = STRING(DCCounter.Amount,">>9.999").
      END.
      OTHERWISE DO:
         lcMaxcharge = STRING(DCCounter.MaxCharge).
         lcAmount = STRING(DCCounter.Amount).
      END.
   END.

END.

PROCEDURE LOCAL-UPDATE-RECORD. 

   DEF INPUT PARAMETE bNew AS LO NO-UNDO.

   FIND FIRST TMSCodes WHERE
              TMSCodes.Tablename    = "DayCampaign"   AND
              TMSCodes.FieldName    = "CalcMethod"      AND
              TMSCodes.CodeGroup    = "DCCounter"  AND
              TMSCodes.CodeValue    = STRING(DayCampaign.Calcmethod)
   NO-LOCK NO-ERROR.
      
   IF AVAIL TMSCodes THEN lcCalcMethod = TMSCodes.CodeName.
   ELSE lcCalcMethod = "".
   
   RUN LOCAL-FIND-OTHERS.
   
   FIND FIRST TMSCodes WHERE
              TMSCodes.Tablename    = "DayCampaign"   AND
              TMSCodes.FieldName    = "DurUnit"      AND
              TMSCodes.CodeGroup    = "Percontr"  AND
              TMSCodes.CodeValue    = STRING(DayCampaign.DurUnit)
   NO-LOCK NO-ERROR.
                                                              
   IF AVAIL TMSCodes THEN lcTypeName = TMSCodes.CodeName.
   ELSE lcTypeName = "".
   
   DISP
      DayCampaign.CalcMethod lcCalcMethod
      DayCampaign.DurUnit lcTypeName 
      DCCounter.DCDate
      DCCounter.DCDate @ ldaDCDateTo 
      DayCampaign.InclUnit lcUnit
      lcMaxCharge
      lcAmount
   WITH FRAME lis.

   MESSAGE " - PRESS ENTER TO CONTINUE - " . PAUSE NO-MESSAGE.                               
   HIDE FRAME lis.
   
END.

