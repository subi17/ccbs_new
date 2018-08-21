/* -----------------------------------------------
  MODULE .......: DayCampaign
  FUNCTION .....: Maintain Day Campaign 
  APPLICATION ..: TMS
  AUTHOR .......: JP
  CREATED ......: 24-11-05
  MODIFIED .....: 
  VERSION ......: SCRUNKO3
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/tmsconst.i}
{Rate/daycampaign.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'DayCampaign'}

SESSION:SYSTEM-ALERT-BOXES = TRUE.

if llDoEvent THEN DO:
    &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
    {Func/lib/eventlog.i}
        
    DEF VAR lhDayCampaign AS HANDLE NO-UNDO.
    lhDayCampaign = BUFFER DayCampaign:HANDLE.
    RUN StarEventInitialize(lhDayCampaign).
                    
    ON F12 ANYWHERE DO:
        RUN Mc/eventview2.p(lhDayCampaign).
    END.
END.


DEF VAR lcEvent    LIKE DayCampaign.DCEvent  NO-UNDO.
DEF VAR xrecid     AS RECID                           init ?.
DEF VAR firstline  AS INT                    NO-UNDO  init 0.
DEF VAR order      AS INT                    NO-UNDO  init 1.
DEF VAR ordercount AS INT                    NO-UNDO  init 2.
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
def var ok         as log format "Yes/No"    NO-UNDO.
DEF VAR lcCode     AS CHAR                   NO-UNDO.
DEF VAR lcWeekday  AS cHAR                   NO-UNDO FORMAT "X(40)" .
DEF VAR lcTypeName AS CHAR                   NO-UNDO FORMAT "X(25)" .
DEF VAR lcCalcMethod AS CHAR                 NO-UNDO FORMAT "X(40)" .
DEF VAR lcUnit       AS CHAR                 NO-UNDO FORMAT "X(12)" .
DEF VAR lcdurType    AS CHAR                 NO-UNDO FORMAT "X(40)" .

DEF VAR lcDurUnit     AS CHAR NO-UNDO.
DEF VAR lcFee         AS CHAR NO-UNDO.
DEF VAR lcModifyFee   AS CHAR NO-UNDO.
DEF VAR lcTermFee     AS CHAR NO-UNDO.
DEF VAR lcEffective   AS CHAR NO-UNDO.
DEF VAR lcTermFeeCalc AS CHAR NO-UNDO.
DEF VAR lcStatus      AS CHAR NO-UNDO.
DEF VAR lcBundleType  AS CHAR NO-UNDO.

DEF BUFFER xxDayCampaign FOR DayCampaign.
DEF BUFFER bBillItem       FOR BillItem.

form
   DayCampaign.DCEvent                      COLUMN-LABEL "Event"
   DayCampaign.DCName        FORMAT "x(20)" COLUMN-LABEL "Name"
   DayCampaign.DCType        FORMAT "X"     COLUMN-LABEL "Type"
   lcTypeName                FORMAT "X(13)" COLUMN-LABEL "TypeName"
   DayCampaign.DSSPriority                  COLUMN-LABEL "DSS"
   DayCampaign.VAlidFrom 
   DayCampaign.ValidTo   

WITH width 80 OVERLAY scroll 1 15 DOWN ROW 1 
   COLOR value(Syst.Var:cfc)
   title color value(Syst.Var:ctc) " " + Syst.Var:ynimi +
   " PERIODICAL CONTRACTS " 
   + string(TODAY,"99-99-99") + " "
   FRAME sel.

form                              
   DayCampaign.DCEvent       COLON 23 FORMAT "X(20)" 
   DayCampaign.PayType       SKIP
   DayCampaign.DCName        COLON 23 FORMAT "X(40)" SKIP
   DayCampaign.ValidFrom     COLON 23 format 99-99-9999 LABEL "Valid" 
      "-"
      DayCampaign.ValidTo  format 99-99-9999 NO-LABEL SKIP
   DayCampaign.StatusCode    COLON 23
      HELP "0=Inactive,1=Active,2=Retired,3=Hidden"
      lcStatus NO-LABEL FORMAT "X(15)" 
   DayCampaign.BundleType  
      HELP "0=undefined, 1=Tariff bundle, 2=Additional bundle (voice or data)" 
      lcBundleType NO-LABEL  FORMAT "X(15)" SKIP
   DayCampaign.DCType        FORMAT "X(1)" COLON 23    
      lcTypeName    NO-LABEL SKIP
   DayCampaign.InstanceLimit COLON 23 
   DayCampaign.BillCode      COLON 23
      bBillItem.BIName NO-LABEL SKIP
   DayCampaign.CCN           COLON 23
      ccn.ccnname no-label

   DayCampaign.InclUnit      COLON 23 lcUnit NO-LABEL  SKIP
      DayCampaign.InclStartCharge COLON 23 
         LABEL "Incl.Start Charge" SKIP
   DayCampaign.MaxChargeIncl COLON 23 LABEL "Incl.Amount" 
      DayCampaign.MaxChargeExcl LABEL "Excl.Amount" SKIP
   DayCampaign.CalcMethod    COLON 23 
      lcCalcMethod NO-LABEL SKIP
   DayCampaign.Effective  FORMAT "9"  COLON 23 
      lcEffective NO-LABEL FORMAT "X(40)"
   DayCampaign.DurType       COLON 23 
      lcDurType  NO-LABEL SKIP
   DayCampaign.DurMonth      COLON 23 LABEL "Duration" 
      HELP "Duration (according to unit)" 
      DayCampaign.DurUnit LABEL "Unit" 
      lcDurUnit NO-LABEL FORMAT "X(15)" SKIP
   DayCampaign.WeekDay       COLON 23 
      lcWeekday NO-LABEL  SKIP   
WITH OVERLAY ROW 1 centered
   COLOR value(Syst.Var:cfc)
   TITLE COLOR value(Syst.Var:ctc)
   fr-header WITH SIDE-LABELS FRAME lis.

FORM
   SKIP(1) 
   DayCampaign.FeeModel  FORMAT "X(15)"    COLON 23 
      lcFee NO-LABEL FORMAT "X(30)" SKIP
   DayCampaign.ModifyFeeModel FORMAT "X(15)"  COLON 23 
      lcModifyFee NO-LABEL FORMAT "X(30)" SKIP
   DayCampaign.TermFeeModel FORMAT "X(15)"  COLON 23 
      lcTermFee NO-LABEL FORMAT "X(30)" SKIP
   DayCampaign.TermFeeCalc COLON 23 FORMAT "9" 
      lcTermFeeCalc NO-LABEL FORMAT "X(30)" 
   SKIP(1)   
WITH OVERLAY ROW 5 CENTERED   
   COLOR value(Syst.Var:cfc) TITLE COLOR value(Syst.Var:ctc)
   fr-header WITH SIDE-LABELS FRAME fFees.


form /*  search WITH FIELD DayCampaign */
    lcEvent
    help "Give ...."
    with row 4 col 2 title color value(Syst.Var:ctc) " FIND Event "
    COLOR value(Syst.Var:cfc) NO-LABELS OVERLAY FRAME haku-f1.


FUNCTION fDispUnit RETURNS LOGICAL
   (iiUnit AS INT):

   IF iiUnit > 0 THEN
      lcUnit = Func.Common:mTMSCodeName("DayCampaign",
                                "InclUnit",
                                STRING(iiUnit)).
   ELSE lcUnit = "".
   
   DISPLAY lcUnit WITH FRAME lis.
   RETURN (iiUnit = 0 OR (iiUnit > 0 AND lcUnit > "")).

END FUNCTION.

FUNCTION fDurUnit RETURNS LOGIC
   (iiDurUnit AS INT):
   
   lcDurUnit = Func.Common:mTMSCodeName("DayCampaign",
                                "DurUnit",
                                STRING(iiDurUnit)).
   DISP lcDurUnit WITH FRAME lis.
   
END FUNCTION.

FUNCTION fDurType RETURNS LOGIC
   (iiDurType AS INT):
   
   lcDurType = Func.Common:mTMSCodeName("DayCampaign",
                                "DurType",
                                STRING(iiDurType)).
   DISP lcDurType WITH FRAME lis.
   
END FUNCTION.

FUNCTION fEffective RETURNS LOGIC
   (iiEffective AS INT):
   
   lcEffective = Func.Common:mTMSCodeName("DayCampaign",
                                  "Effective",
                                  STRING(iiEffective)).
   DISP lcEffective WITH FRAME lis.
   
END FUNCTION.

FUNCTION fTermFeeCalc RETURNS LOGIC
   (iiTermFeeCalc AS INT):
   
   lcTermFeeCalc = Func.Common:mTMSCodeName("DayCampaign",
                                    "TermFeeCalc",
                                    STRING(iiTermFeeCalc)).
   DISP lcTermFeeCalc WITH FRAME fFees.
   
END FUNCTION.
  
FUNCTION fFeeModel RETURNS CHAR
   (icFeeModel AS CHAR):
  
   IF icFeeModel = "" THEN RETURN "".
   
   FIND FIRST FeeModel WHERE
              FeeModel.Brand    = Syst.Var:gcBrand AND
              FeeModel.FeeModel = icFeeModel NO-LOCK NO-ERROR.
   IF AVAILABLE FeeModel 
   THEN RETURN FeeModel.FeeName.
   ELSE RETURN "". 
   
END FUNCTION.

FUNCTION fStatusName RETURNS LOGIC
   (iiStatusCode AS INT):

   lcStatus = Func.Common:mTMSCodeName("CLIType",
                               "WebStatusCode",
                               STRING(iiStatusCode)).

   DISP lcStatus WITH FRAME lis.
END FUNCTION.


FUNCTION fBundleTypeName RETURNS LOGIC
   (iiBundleType AS INT):

   lcBundleType = Func.Common:mTMSCodeName("DayCampaign",
                               "BundleType",
                               STRING(iiBundleType)).

   DISP lcBundleType WITH FRAME lis.
END FUNCTION.


Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
view FRAME sel.


RUN LOCAL-FIND-FIRST.

IF AVAILABLE DayCampaign THEN ASSIGN
   memory     = recid(DayCampaign)
   must-print = TRUE
   must-add   = FALSE.
ELSE ASSIGN
   memory     = ?
   must-print = FALSE
   must-add   = FALSE.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
    END.

   IF must-add THEN DO:  /* DayCampaign -ADD  */
      HIDE FRAME lis.
      assign Syst.Var:cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.
      
      add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        Syst.Var:ehto = 9. RUN Syst/ufkey.p.
        DO TRANSACTION:

           CREATE DayCampaign.
           DayCampaign.Brand = Syst.Var:gcBrand.

           RUN LOCAL-UPDATE-RECORD(true).
           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
           DayCampaign.DCEvent = ""  THEN
           UNDO add-new, LEAVE add-new.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhDayCampaign).
           
           ASSIGN
              memory = recid(DayCampaign)
              xrecid = memory
              must-add = false.
           LEAVE add-new.
        END.
      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      RUN LOCAL-FIND-FIRST.
      IF NOT AVAILABLE DayCampaign THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND DayCampaign where recid(DayCampaign) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose keyvalue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE DayCampaign THEN DO:
              RUN LOCAL-DISP-ROW.
              rtab[FRAME-LINE] = recid(DayCampaign).
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
        Syst.Var:ufk[1]= 35  Syst.Var:ufk[2]= 0 Syst.Var:ufk[3]= 222 Syst.Var:ufk[4]= 0
        Syst.Var:ufk[5]= 5  Syst.Var:ufk[6]= 4 Syst.Var:ufk[7]= 814
        Syst.Var:ufk[8]= 8 Syst.Var:ufk[9]= 1
        Syst.Var:ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW DayCampaign.DCEvent {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(Syst.Var:ccc) DayCampaign.DCEvent WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW DayCampaign.DCEvent {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(Syst.Var:ccc) DayCampaign.DCEvent WITH FRAME sel.
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

      if lookup(Syst.Var:nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > ordercount THEN order = 1.
      END.
      if lookup(Syst.Var:nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = ordercount.
      END.

      IF order <> ex-order THEN DO:
        ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
        FIND DayCampaign where recid(DayCampaign) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           RUN LOCAL-FIND-PREV.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* previous line */
      if lookup(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           FIND DayCampaign where recid(DayCampaign) = rtab[1] no-lock.
           RUN LOCAL-FIND-PREV.
           IF NOT AVAILABLE DayCampaign THEN DO:
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
              rtab[1] = recid(DayCampaign)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(Syst.Var:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND DayCampaign where recid(DayCampaign) = rtab[FRAME-DOWN] no-lock .
           RUN LOCAL-FIND-NEXT.
           IF NOT AVAILABLE DayCampaign THEN DO:
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
              rtab[FRAME-DOWN] = recid(DayCampaign).
              /* finally LAST line's keyvalue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(Syst.Var:nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND DayCampaign where recid(DayCampaign) = memory no-lock no-error.
        RUN LOCAL-FIND-PREV.
        IF AVAILABLE DayCampaign THEN DO:
           memory = recid(DayCampaign).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              RUN LOCAL-FIND-PREV.
              IF AVAILABLE DayCampaign THEN memory = recid(DayCampaign).
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
     else if lookup(Syst.Var:nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* cursor TO the downmost line */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           message "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-message.
       END.
       ELSE DO: /* the downmost line wasn't empty */
           memory = rtab[FRAME-DOWN].
           FIND DayCampaign where recid(DayCampaign) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(Syst.Var:nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       lcEvent = "".
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       UPDATE lcEvent WITH FRAME haku-f1.
       HIDE FRAME haku-f1 no-pause.
       if lcEvent <> "" THEN DO:
          FIND FIRST DayCampaign where DayCampaign.DCEvent = lcEvent
          /* search condition */ no-lock no-error.
          IF NOT AVAILABLE DayCampaign THEN DO:
             BELL.
             message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  DayCampaign/DayCampaign was found */
          ASSIGN order = 1 memory = recid(DayCampaign) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     if lookup(Syst.Var:nap,"5,f5") > 0 THEN DO:  /* lisays */
        must-add = TRUE.
        NEXT LOOP.
     END.

     else if lookup(Syst.Var:nap,"6,f6") > 0 THEN DO TRANSACTION:  /* removal */
       delline = FRAME-LINE.
       FIND DayCampaign where recid(DayCampaign) = rtab[FRAME-LINE] no-lock.

       IF CAN-FIND(FIRST DCCLI WHERE 
                         DCCLI.Brand   = Syst.Var:gcBrand AND 
                         DCCLI.DCEvent = DayCampaign.DCEvent)
       THEN DO:
          MESSAGE "Day campaign is in use on subscriptions," SKIP
                  "delete is not allowed."
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.
       
       /* line TO be deleted is lightened */
       COLOR DISPLAY value(Syst.Var:ctc)
          DayCampaign.DCEvent 

          DayCampaign.ValidFrom
          DayCampaign.ValidTo
          DayCampaign.dcName
          DayCampaign.dctype
          lcTypeName
          DayCampaign.DSSPriority.

       RUN LOCAL-FIND-NEXT.
       
       IF AVAILABLE DayCampaign THEN memory = recid(DayCampaign).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND DayCampaign where recid(DayCampaign) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          RUN LOCAL-FIND-PREV.
          IF AVAILABLE DayCampaign THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             memory = recid(DayCampaign).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND DayCampaign where recid(DayCampaign) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.

       COLOR DISPLAY value(Syst.Var:ccc)
          DayCampaign.DCEvent 
          DayCampaign.ValidFrom
          DayCampaign.ValidTo
          DayCampaign.dcName
          DayCampaign.DCType
          lcTypeName
          DayCampaign.DSSPriority.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhDayCampaign).

           DELETE DayCampaign.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST DayCampaign
           /* search condition */) THEN DO:
              CLEAR FRAME sel no-pause.
              PAUSE 0 no-message.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delline = 0. /* wasn't the LAST one */
     END. /* removal */

     /* translations */
     ELSE IF LOOKUP(Syst.Var:nap,"7,f7") > 0 AND Syst.Var:ufk[7] > 0 THEN DO:  
        FIND DayCampaign WHERE RECID(DayCampaign) = rtab[FRAME-LINE] NO-LOCK.
        RUN Mc/invlang.p(14,DayCampaign.DCEvent).
          
        ufkey = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"3,F3") > 0 THEN DO:
       FIND FIRST DayCampaign WHERE 
            RECID(DayCampaign) = rtab[FRAME-LINE] 
       NO-LOCK NO-ERROR.
       IF LOOKUP(STRING(Daycampaign.dcType),{&PERCONTRACT_RATING_PACKAGE}) > 0 OR
          Daycampaign.dcType = {&DCTYPE_POOL_RATING} THEN DO:
          
          FIND ServiceLimit WHERE 
               ServiceLimit.GroupCode = daycampaign.dcevent
          NO-LOCK NO-ERROR.

          IF AMBIGUOUS(ServiceLimit)
          THEN DO:
             MESSAGE "Multiple ServiceLimit found under same DCEvent. " + CHR(10) +
                     "Please use ServiceLimitGroup view." VIEW-AS ALERT-BOX.
             NEXT LOOP.
          END.

          IF NOT AVAIL ServiceLimit THEN DO:
             MESSAGE 
             "Can't find any services for " Daycampaign.dcevent
             VIEW-AS ALERT-BOX.
             NEXT.
          END.
          RUN Mm/mservicelimit.p(INPUT  0 ,
                                   ServiceLimit.dialtype,
                                   Servicelimit.slseq ).


       END.
       ELSE RUN Mm/dccli.p (input 0, daycampaign.dcevent).
       ufkey = true.
       RUN Syst/ufkey.p.
       PAUSE 0.
     END.

     else if lookup(Syst.Var:nap,"enter,return") > 0 THEN DO WITH FRAME lis TRANSACTION:
       /* change */
       FIND FIRST DayCampaign where 
            recid(DayCampaign) = rtab[frame-line(sel)]
       no-lock.
       assign fr-header = " CHANGE " ufkey = TRUE Syst.Var:ehto = 9.
       RUN Syst/ufkey.p.

       Syst.Var:cfc = "lis". RUN Syst/ufcolor.p.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhDayCampaign).

       RUN LOCAL-UPDATE-RECORD(FALSE).

       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.
       
       xrecid = recid(DayCampaign).

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhDayCampaign).

     END.

     else if lookup(Syst.Var:nap,"home,h") > 0 THEN DO:
       RUN LOCAL-FIND-FIRST.
       ASSIGN memory = recid(DayCampaign) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(Syst.Var:nap,"end,e") > 0 THEN DO : /* LAST record */
       RUN LOCAL-FIND-LAST.
       ASSIGN memory = recid(DayCampaign) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
Syst.Var:si-recid = xrecid.

PROCEDURE LOCAL-DISP-ROW: 
   
   FIND FIRST BillItem WHERE
              BillItem.Brand    = Syst.Var:gcBrand AND
              BillItem.BillCode = DayCampaign.DCTarget
   NO-LOCK NO-ERROR.
 
   FIND FIRST bBillItem WHERE 
              bBillItem.Brand    = Syst.Var:gcBrand AND
              bBillItem.BillCode = DayCampaign.BillCode 
   NO-LOCK NO-ERROR.

   FIND FIRST TMSCodes WHERE
              TMSCodes.Tablename    = "DayCampaign"   AND
              TMSCodes.FieldName    = "DCType"      AND
              TMSCodes.CodeGroup    = "PerContr"  AND
              TMSCodes.CodeValue    = STRING(DayCampaign.dctype)
   NO-LOCK NO-ERROR.

   IF AVAIL TMSCodes THEN lcTypeName = TMSCodes.CodeName.
   ELSE lcTypeName = "".
   
   DISPLAY
      DayCampaign.DCEvent  
      DayCampaign.DCName
      DayCampaign.DCtype   
      lcTypeName
      DayCampaign.DSSPriority
      DayCampaign.ValidFrom
      DayCampaign.ValidTo
   WITH FRAME sel.
   
END PROCEDURE.

PROCEDURE LOCAL-FIND-NEXT.

   IF order = 1 THEN 
      FIND NEXT DayCampaign USE-INDEX DCEvent
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-FIND-PREV.

   IF order = 1 THEN 
      FIND PREV DayCampaign USE-INDEX DCEvent WHERE 
                DayCampaign.Brand = "1" 
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-FIND-FIRST.

   IF order = 1 THEN 
      FIND FIRST DayCampaign USE-INDEX DCEvent  WHERE
                 DayCampaign.Brand = "1"
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-FIND-LAST.

   IF order = 1 THEN 
      FIND LAST DayCampaign USE-INDEX DCEvent  WHERE
                DayCampaign.Brand = "1"
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-UPDATE-RECORD. 

   DEF INPUT PARAMETER ilNew AS LOG NO-UNDO.

   MaintMenu:
   REPEAT ON ENDKEY UNDO, LEAVE:

      FIND FIRST BillItem WHERE
                 BillItem.Brand    = Syst.Var:gcBrand AND
                 BillItem.BillCode = DayCampaign.DCTarget
      NO-LOCK NO-ERROR.
 
      FIND FIRST bBillItem WHERE
                 bBillItem.Brand    = Syst.Var:gcBrand AND
                 bBillItem.BillCode = DayCampaign.BillCode
      NO-LOCK NO-ERROR.

      FIND FIRST TMSCodes WHERE
                 TMSCodes.Tablename    = "DayCampaign"   AND
                 TMSCodes.FieldName    = "DCType"      AND
                 TMSCodes.CodeGroup    = "PerContr"  AND
                 TMSCodes.CodeValue    = STRING(DayCampaign.dctype)
      NO-LOCK NO-ERROR.
                 
      IF AVAIL TMSCodes THEN lcTypeName = TMSCodes.CodeName.
      ELSE lcTypeName = "".


      FIND FIRST TMSCodes WHERE
                 TMSCodes.Tablename    = "DayCampaign"   AND
                 TMSCodes.FieldName    = "CalcMethod"      AND
                 TMSCodes.CodeValue    = STRING(DayCampaign.Calcmethod)
      NO-LOCK NO-ERROR.
      
      IF AVAIL TMSCodes THEN lcCalcMethod = TMSCodes.CodeName.
      ELSE lcCalcMethod = "".
                  
      FIND FIRST ccn WHERE 
                 ccn.Brand = Syst.Var:gcBrand AND       
                 ccn.ccn   = DayCampaign.ccn NO-LOCK NO-ERROR.

      DISP 
         DayCampaign.DCEvent
         DayCampaign.PayType
         DayCampaign.DCName 
         DayCampaign.StatusCode
         DayCampaign.BundleType
         DayCampaign.DCType
         DayCampaign.InstanceLimit
         DayCampaign.BillCode 
         DayCampaign.CCN           
         DayCampaign.InclUnit
         DayCampaign.InclStartCharge
         DayCampaign.MaxChargeIncl
         DayCampaign.MaxChargeExcl
         DayCampaign.CalcMethod
         DayCampaign.Effective
         DayCampaign.DurType
         DayCampaign.DurMonth
         DayCampaign.DurUnit
         DayCampaign.WeekDay
         DayCampaign.ValidFrom
         DayCampaign.ValidTo
         fWeekDayName(INPUT DayCampaign.Weekday) @ lcWeekDay
         lcTypeName
         lcCalcMethod 
         ccn.ccnname        WHEN AVAIL ccn
         bBillItem.BIName   WHEN AVAIL bBillItem
         "" WHEN NOT AVAIL bBillItem @ bBillItem.BIName
      WITH FRAME lis.
      
      fDispUnit(DayCampaign.InclUnit).
      fDurUnit(DayCampaign.DurUnit).
      fDurType(DayCampaign.DurType).
      fEffective(DayCampaign.Effective).
      fStatusName(DayCampaign.StatusCode).
      fBundleTypeName(DayCampaign.BundleType).

      IF ilNew THEN Syst.Var:toimi = 1.
      
      ELSE ASSIGN 
         Syst.Var:ehto   = 0
         Syst.Var:ufk    = 0
         Syst.Var:ufk[1] = 7 WHEN lcRight = "RW" AND Syst.Var:gcHelpParam = ""
         Syst.Var:ufk[2] = 295
         Syst.Var:ufk[3] = 9858
         Syst.Var:ufk[4] = 253
         Syst.Var:ufk[8] = 8.
      RUN Syst/ufkey.p.
      
      IF Syst.Var:toimi = 1 THEN DO:
         RUN pUpdate(ilNew).
         IF RETURN-VALUE BEGINS "UNDO" THEN UNDO, LEAVE MaintMenu.
         IF ilNew THEN LEAVE MaintMenu.
      END.   

      ELSE IF Syst.Var:toimi = 2 THEN RUN pFeeData(FALSE).
      
      ELSE IF Syst.Var:toimi = 3 THEN RUN Mc/tmsrelation.p({&DCTABLENAME},{&DCKEYTYPE},DayCampaign.DCEvent).
      
      ELSE IF Syst.Var:toimi = 4 THEN RUN Mm/dcservicepackage.p(DayCampaign.DCEvent).  
      
      ELSE IF Syst.Var:toimi = 8 THEN LEAVE MaintMenu.
   END.

   HIDE FRAME lis NO-PAUSE.

END PROCEDURE.

PROCEDURE pUpdate:
   
   DEF INPUT PARAMETER ilNew AS LOG NO-UNDO.

   DEF VAR llUndo AS LOG  NO-UNDO.
      
   llUndo = FALSE.
   
   REPEAT WITH FRAME lis ON ENDKEY UNDO, RETRY:

      IF RETRY THEN DO:
         llUndo = TRUE.
         LEAVE.
      END.
 
      FIND CURRENT DayCampaign EXCLUSIVE-LOCK.
            
      Syst.Var:ehto = 9.
      RUN Syst/ufkey.p.
    
      UPDATE 
         DayCampaign.DCEvent WHEN ilNew
         DayCampaign.PayType
         DayCampaign.DCName 
         DayCampaign.ValidFrom
         DayCampaign.ValidTo
         DayCampaign.StatusCode
         DayCampaign.BundleType
         DayCampaign.DCType
         DayCampaign.InstanceLimit
         DayCampaign.BillCode 
         DayCampaign.CCN           
         DayCampaign.InclUnit
         DayCampaign.InclStartCharge
         DayCampaign.MaxChargeIncl
         DayCampaign.MaxChargeExcl
         DayCampaign.CalcMethod
         DayCampaign.Effective
         DayCampaign.DurType
         DayCampaign.DurMonth
         DayCampaign.DurUnit
         DayCampaign.WeekDay
         WHEN LOOKUP(DayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0
      WITH FRAME lis EDITING: 
      
         READKEY.
         IF keylabel(LASTKEY) = "F9" AND
            LOOKUP(FRAME-FIELD,
               "DCType,CalcMethod,InclUnit,DurUnit,DurType,Effective") > 0 
         THEN DO:
         
            IF FRAME-FIELD = "DCType"  THEN DO:
               RUN Help/h-tmscodes.p(INPUT "DayCampaign",    /* TableName */
                                    "DCType",       /* FieldName */
                                    "PerContr",   /* GroupCode */
                               OUTPUT lcCode).
                               
               IF lcCode ne "" AND lcCode NE ? THEN DO:
                  DISPLAY lcCode @ DayCampaign.dctype WITH FRAME lis.
               END.
            END.
         
            ELSE IF FRAME-FIELD = "CalcMethod"  THEN DO:
               RUN Help/h-tmscodes.p(INPUT "Daycampaign",  /* TableName */
                                    "CalcMethod",   /* FieldName */
                                    "DCCounter",    /* GroupCode */
                              OUTPUT lcCode).
                               
               IF lcCode ne "" AND lcCode NE ? THEN DO:
                  DISPLAY INTEGER(lcCode) @ DayCampaign.CalcMethod
                     WITH FRAME lis.
               END.
            END.

            ELSE IF FRAME-FIELD = "Effective"  THEN DO:
               RUN Help/h-tmscodes.p(INPUT "Daycampaign",  /* TableName */
                                    "Effective",      /* FieldName */
                                    "PerContr",    /* GroupCode */
                              OUTPUT lcCode).
                                
               IF lcCode ne "" AND lcCode NE ? THEN DO:
                  DISPLAY INTEGER(lcCode) @ DayCampaign.Effective
                     WITH FRAME lis.
                  fEffective(INTEGER(lcCode)). 
               END.
            END.
 
            ELSE IF FRAME-FIELD = "DurUnit"  THEN DO:
               RUN Help/h-tmscodes.p(INPUT "Daycampaign",  /* TableName */
                                    "DurUnit",      /* FieldName */
                                    "PerContr",    /* GroupCode */
                              OUTPUT lcCode).
                                
               IF lcCode ne "" AND lcCode NE ? THEN DO:
                  DISPLAY INTEGER(lcCode) @ DayCampaign.DurUnit
                     WITH FRAME lis.
                  fDurUnit(INTEGER(lcCode)). 
               END.
            END.

            ELSE IF FRAME-FIELD = "DurType"  THEN DO:
               RUN Help/h-tmscodes.p(INPUT "Daycampaign",  /* TableName */
                                    "DurType",      /* FieldName */
                                    "DCCounter",    /* GroupCode */
                              OUTPUT lcCode).
                               
               IF lcCode ne "" AND lcCode NE ? THEN DO:
                  DISPLAY INTEGER(lcCode) @ DayCampaign.DurType
                     WITH FRAME lis.
                  fDurType(INTEGER(lcCode)). 
               END.
            END.
          
            ELSE IF FRAME-FIELD = "InclUnit" THEN DO:
               RUN Help/h-tmscodes.p(INPUT "DayCampaign",    /* TableName */
                                    "InclUnit", /* FieldName */
                                    "Unit",     /* GroupCode */
                              OUTPUT lcCode).
                             
               IF lcCode ne "" AND lcCode NE ? THEN DO:
                  fDispUnit(INTEGER(lcCode)).
                  DISPLAY INTEGER(lcCode) ;& DayCampaign.InclUnit
                     WITH FRAME lis.
               END.
            END.
         
            Syst.Var:ehto = 9.
            RUN Syst/ufkey.p.
            NEXT.
         END.
       
         Syst.Var:nap = KEYLABEL(LASTKEY). 

         IF lookup(Syst.Var:nap,Syst.Var:poisnap) > 0 THEN DO:
            if keylabel(lastkey) = "F4" THEN LEAVE . 

            IF FRAME-FIELD = "DCEvent" THEN DO:
               if input frame lis DayCampaign.DCEvent  = "" THEN DO:
                  LEAVE .
               END.
               ELSE IF CAN-FIND(FIRST 
                     xxDayCampaign WHERE
                     xxDayCampaign.Brand    = Syst.Var:gcBrand AND
                     xxDayCampaign.DCEvent  = input DayCampaign.DCEvent  AND
                     RECID(xxDayCampaign)   ne RECID(DayCampaign))
               THEN DO:
                  BELL.
                  MESSAGE "Campaign already exists !".
                  NEXT-PROMPT DayCampaign.DCEvent.
                  NEXT.
               END. 
            END.

            ELSE IF FRAME-FIELD = "DCType" THEN DO:
               FIND FIRST TMSCodes WHERE 
                          TMSCodes.Tablename    = "DayCampaign"   AND 
                          TMSCodes.FieldName    = "DCType"      AND 
                          TMSCodes.CodeGroup    = "PerContr"  AND 
                          TMSCodes.CodeValue    =  INPUT DayCampaign.DCType
               NO-LOCK NO-ERROR.

               IF NOT AVAIL TMSCodes THEN DO:
                  BELL.
                  MESSAGE "Unknown type"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT-prompt Daycampaign.dctype.
                  NEXT.
               END.
               DISP  TMSCodes.codename @ lctypename  WITH FRAME lis. 
               PAUSE 0.
            END.

            ELSE IF FRAME-FIELD = "CalcMethod" THEN DO:

               FIND FIRST TMSCodes WHERE 
                          TMSCodes.Tablename = "Daycampaign"   AND 
                          TMSCodes.FieldName = "CalcMethod"   AND 
                          TMSCodes.CodeGroup = "DCCounter"  AND 
                          TMSCodes.CodeValue = INPUT DayCampaign.CalcMethod
               NO-LOCK NO-ERROR.
        
               IF NOT AVAIL TMSCodes THEN DO:
                  BELL.
                  MESSAGE "Unknown Calculation method"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT-prompt Daycampaign.CalcMethod.
                  NEXT.
               END.
               DISP  TMSCodes.CodeName  @ lcCalcMethod  WITH FRAME lis. 
               PAUSE 0.
            END.

            ELSE IF FRAME-FIELD = "BillItem" THEN DO:
               FIND FIRST bBillItem WHERE 
                   bBillItem.Brand    = Syst.Var:gcBrand AND
                   bBillItem.BillCode = input frame lis DayCampaign.BillCode
               NO-LOCK NO-ERROR.
               IF NOT AVAIL bBillItem THEN DO:
                  BELL.
                  MESSAGE "Unknown Billing Item" 
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT-prompt DayCampaign.BillCode.
                  NEXT.
               END.        
               DISP bBillItem.BIName WITH FRAME lis. PAUSE 0. 
            END.

            ELSE IF FRAME-FIELD = "InclUnit" THEN DO:
               IF NOT fDispUnit(INPUT INPUT DayCampaign.InclUnit) THEN DO:
                  BELL.
                  MESSAGE "Unknown unit"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
            END.
              
            ELSE IF FRAME-FIELD = "Effective" THEN DO:
               fEffective(INPUT INPUT DayCampaign.Effective).
               IF lcEffective = "" THEN DO:
                  BELL.
                  MESSAGE "Unknown effective value"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
            END.

            ELSE IF FRAME-FIELD = "DurUnit" THEN DO:
               fDurUnit(INPUT INPUT DayCampaign.DurUnit).
               IF lcDurUnit = "" THEN DO:
                  BELL.
                  MESSAGE "Unknown duration unit"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
            END.

            ELSE IF FRAME-FIELD = "DurType" THEN DO:
               fDurType(INPUT INPUT DayCampaign.DurType).
               IF lcDurType = "" THEN DO:
                  BELL.
                  MESSAGE "Unknown duration type"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
            END.

            ELSE IF FRAME-FIELD = "StatusCode" THEN DO:
               fStatusName(INPUT INPUT DayCampaign.StatusCode).
               IF lcStatus = "" THEN DO:
                  BELL.
                  MESSAGE "Unknown status code"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
            END.
            ELSE IF FRAME-FIELD = "BundleType" THEN DO:
               fBundleTypeName(INPUT INPUT DayCampaign.BundleType).
               IF lcBundleType = "" THEN DO:
                  BELL.
                  MESSAGE "Unknown Bundle Type"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
            END.
            

            ELSE IF FRAME-FIELD = "validfrom" THEN DO:
               if input frame lis DayCampaign.ValidFrom  = ? THEN DO:
                  BELL.
                  MESSAGE "from Date can't be empty!".
                  NEXT-prompt DayCampaign.ValidFrom.
                  NEXT.
               END.
            END.
         
            ELSE IF FRAME-FIELD = "validTo" THEN DO:
               if input frame lis DayCampaign.ValidTo  = ? THEN DO:
                  BELL.
                  MESSAGE "from Date can't be empty!".
                  NEXT-prompt DayCampaign.ValidTo.
                  NEXT.
               END.
            END.
         
            ELSE IF FRAME-FIELD = "InstanceLimit" THEN DO:
               if input frame lis DayCampaign.InstanceLimit <= 0 THEN DO:
                  BELL.
                  MESSAGE "instance limit must be >= 1".
                  NEXT-prompt DayCampaign.InstanceLimit.
                  NEXT.
               END.
               else if
                  input frame lis DayCampaign.DCType NE "6" AND
                  input frame lis DayCampaign.InstanceLimit > 1 THEN DO:
                  BELL.
                  MESSAGE "no more than one instance is supported".
                  NEXT-prompt DayCampaign.InstanceLimit.
                  NEXT.
               end.
            END.

            ELSE IF FRAME-FIELD = "Weekday" THEN DO:
               IF INPUT FRAME lis DayCampaign.Weekday NE "" AND
                  NOT fWeekDay(INPUT INPUT DayCampaign.Weekday) THEN DO:
   
                  BELL.
                  MESSAGE
                     "Invalid weekday parameter!" SKIP
                     "1 - sunday     "           SKIP
                     "2 - monday     "           SKIP
                     "3 - tuesday    "           SKIP
                     "4 - wednesday  "           SKIP
                     "5 - thursday   "           SKIP
                     "6 - friday     "           SKIP
                     "7 - saturday   "           SKIP
                  VIEW-AS ALERT-BOX.
                  NEXT-PROMPT DayCampaign.Weekday. NEXT.   
               END.

               DISP 
                  fWeekDayName(INPUT INPUT DayCampaign.Weekday) @ lcWeekDay
               WITH FRAME lis.
            END.
         END.
      
         APPLY LASTKEY. 
      END.  /* editing */

      LEAVE.
   END.  

   HIDE FRAME lis NO-PAUSE.
    
   IF llUndo THEN RETURN "UNDO".
   ELSE RETURN "". 
 
END PROCEDURE.
   
PROCEDURE pFeeData: 

   DEF INPUT PARAMETER ilNew AS LOG NO-UNDO.

   FeeMenu:
   REPEAT ON ENDKEY UNDO, LEAVE:

      ASSIGN
         lcFee       = fFeeModel(DayCampaign.FeeModel)
         lcModifyFee = fFeeModel(DayCampaign.ModifyFeeModel)
         lcTermFee   = fFeeModel(DayCampaign.TermFeeModel)
         .
      
      PAUSE 0.
      DISP 
         DayCampaign.FeeModel
         DayCampaign.ModifyFeeModel
         DayCampaign.TermFeeModel
         lcFee 
         lcModifyfee
         lcTermFee
         DayCampaign.TermFeeCalc
      WITH FRAME fFees.
      
      fTermFeeCalc(DayCampaign.TermFeeCalc).

      IF ilNew THEN Syst.Var:toimi = 1.
      ELSE DO: 
         ASSIGN 
            Syst.Var:ehto   = 0
            Syst.Var:ufk    = 0
            Syst.Var:ufk[1] = 7 WHEN Syst.Var:gcHelpParam = ""
            Syst.Var:ufk[8] = 8.
         RUN Syst/ufkey.p.
      END.
      
      IF Syst.Var:toimi = 1 THEN 
      REPEAT WITH FRAME fFees ON ENDKEY UNDO, LEAVE FeeMenu:

         FIND CURRENT DayCampaign EXCLUSIVE-LOCK.
            
         Syst.Var:ehto = 9.
         RUN Syst/ufkey.p.
    
         UPDATE 
            DayCampaign.FeeModel
            DayCampaign.ModifyFeeModel
            DayCampaign.TermFeeModel
            DayCampaign.TermFeeCalc
         WITH FRAME fFees EDITING: 
      
            READKEY.
            IF keylabel(LASTKEY) = "F9" AND
               LOOKUP(FRAME-FIELD,"TermFeeCalc") > 0 THEN DO:
         
               IF FRAME-FIELD = "TermFeeCalc"  THEN DO:
                  RUN Help/h-tmscodes.p(INPUT "Daycampaign",  /* TableName */
                                       "TermFeeCalc",  /* FieldName */
                                       "PerContr",    /* GroupCode */
                                 OUTPUT lcCode).
                               
                  IF lcCode ne "" AND lcCode NE ? THEN DO:
                     DISPLAY INTEGER(lcCode) @ DayCampaign.TermFeeCalc
                        WITH FRAME fFees.
                     fTermFeeCalc(INTEGER(lcCode)). 
                  END.
               END.
          
               Syst.Var:ehto = 9.
               RUN Syst/ufkey.p.
               NEXT.
            END.

            Syst.Var:nap = KEYLABEL(LASTKEY). 

            IF lookup(Syst.Var:nap,Syst.Var:poisnap) > 0 THEN DO:
               if keylabel(lastkey) = "F4" THEN LEAVE . 

               IF FRAME-FIELD = "TermFeeCalc" THEN DO:
                  fTermFeeCalc(INPUT INPUT DayCampaign.TermFeeCalc).
                  IF lcTermFeeCalc = "" THEN DO:
                     BELL.
                     MESSAGE "Unknown termination fee calculation method"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.

               ELSE IF FRAME-FIELD = "FeeModel" THEN DO:
                  lcFee = fFeeModel(INPUT INPUT DayCampaign.FeeModel).
            
                  IF INPUT DayCampaign.FeeModel > "" AND lcFee = "" THEN DO:
                     MESSAGE "Unknown fee model"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.

                  DISP lcFee WITH FRAME fFees. 
               END.

               ELSE IF FRAME-FIELD = "ModifyFeeModel" THEN DO:
                  lcModifyFee = 
                     fFeeModel(INPUT INPUT DayCampaign.ModifyFeeModel).
            
                  IF INPUT DayCampaign.ModifyFeeModel > "" AND lcModifyFee = "" 
                  THEN DO:
                     MESSAGE "Unknown fee model"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.

                  DISP lcModifyFee WITH FRAME fFees. 
               END.

               ELSE IF FRAME-FIELD = "TermFeeModel" THEN DO:
                  lcTermFee = fFeeModel(INPUT INPUT DayCampaign.TermFeeModel).
            
                  IF INPUT DayCampaign.TermFeeModel > "" AND lcTermFee = "" 
                  THEN DO:
                     MESSAGE "Unknown fee model"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.

                  DISP lcTermFee WITH FRAME fFees. 
               END.
                                       
            END.
      
            APPLY LASTKEY. 
                    
         END. /* editing */
         
         LEAVE.
     END.
     
     ELSE IF Syst.Var:toimi = 8 THEN LEAVE.
     
  END.  

  HIDE FRAME fFees NO-PAUSE.  
   
END PROCEDURE.

