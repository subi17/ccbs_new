/* ----------------------------------------------------------------------
  MODULE .......: TMRule
  TASK .........: UPDATEs table TMRule
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 05.05.08
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable TMRule

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'TMRule'}

{Syst/eventval.i}
{Syst/tmsconst.i}

DEF BUFFER bItemValue FOR TMRItemValue.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhTMRule AS HANDLE NO-UNDO.
   lhTMRule = BUFFER TMRule:HANDLE.
   RUN StarEventInitialize(lhTMRule).

   DEFINE VARIABLE lhTMRItemValue AS HANDLE NO-UNDO.
   lhTMRItemValue = BUFFER TMRItemValue:HANDLE.
   RUN StarEventInitialize(lhTMRItemValue).

   DEFINE VARIABLE lhbItemValue AS HANDLE NO-UNDO.
   lhbItemValue = BUFFER bItemValue:HANDLE.
   RUN StarEventInitialize(lhbItemValue).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhTMRule).
   END.

END.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR liTMRuleSeq  AS INT                    NO-UNDO.
DEF VAR lcName       AS CHAR                   NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
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

DEF VAR lcField         AS CHAR NO-UNDO. 
DEF VAR lcCode          AS CHAR NO-UNDO FORMAT "X". 
DEF VAR lcSrcName       AS CHAR NO-UNDO.
DEF VAR lcCounterAmount AS CHAR NO-UNDO.
DEF VAR lcPeriodName    AS CHAR NO-UNDO.
DEF VAR lcPayType       AS CHAR NO-UNDO.
DEF VAR lcTicketType    AS CHAR NO-UNDO.
DEF VAR lcCounterType   AS CHAR NO-UNDO.
DEF VAR lcLimitCompare  AS CHAR NO-UNDO.
DEF VAR llShowHistory AS LOG NO-UNDO INIT FALSE. 

FORM
    TMRule.Name         FORMAT "X(25)"
    TMRule.TMRuleSeq    FORMAT ">>>>>>9" COLUMN-LABEL "ID"
    TMRule.FromDate 
    TMRule.ToDate 
    TMRule.PayType      
    TMRule.CounterItems FORMAT "X(18)"
WITH ROW FrmRow width 80 OVERLAY FrmDown DOWN 
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       "  TM RULES " + "  " +
       string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

FORM
    TMRule.Brand          COLON 15
    TMRule.TMRuleSeq      COLON 15
    TMRule.Name           COLON 15
    TMRule.FromDate       COLON 15
    TMRule.ToDate         COLON 15 
    TMRule.TicketType     COLON 15 FORMAT "9"
       lcTicketType NO-LABEL FORMAT "X(30)"
    TMRule.PayType        COLON 15 
       lcPayType NO-LABEL FORMAT "X(30)"
    TMRule.CounterType    COLON 15 FORMAT "9" 
       lcCounterType NO-LABEL FORMAT "X(50)" 
    TMRule.CounterItems   COLON 15 FORMAT "X(60)"
    TMRule.CounterAmount  COLON 15 FORMAT "X"
           LABEL "Counter Unit"
           HELP "Counter unit"
        lcCounterAmount NO-LABEL FORMAT "X(30)" SKIP(1)
    TMRule.LimitSource    COLON 15 
       lcSrcName NO-LABEL FORMAT "X(40)"
    TMRule.LimitCompare   COLON 15  LABEL "Comparison" FORMAT ">9"
       lcLimitCompare NO-LABEL FORMAT "X(40)" 
    TMRule.CounterPeriod  COLON 15
       lcPeriodName NO-LABEL FORMAT "X(30)"
    TMRule.NewCustomer    COLON 15 FORMAT "Yes/No"
WITH  OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FORM 
    "Brand:" lcBrand skip
    "Rule :" lcName FORMAT "X(20)" 
    HELP "Enter rule name"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Rule "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

FUNCTION fLimitSourceName RETURNS LOGIC
   (iiLimitSource AS INT):

   lcSrcName = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                 "TMRule",
                                 "LimitSource",
                                 STRING(iiLimitSource)).
                                 
   DISP lcSrcName WITH FRAME lis.
END FUNCTION.

FUNCTION fCounterAmount RETURNS LOGIC
   (icCounterAmount AS CHAR):

   lcCounterAmount = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                      "TMRule",
                                      "CounterAmount",
                                      icCounterAmount).
                                 
   DISP lcCounterAmount WITH FRAME lis.
END FUNCTION.


FUNCTION fCounterPeriodName RETURNS LOGIC
   (iiCounterPeriod AS INT):

   lcPeriodName = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                   "TMRule",
                                   "CounterPeriod",
                                   STRING(iiCounterPeriod)).
                                 
   DISP lcPeriodName WITH FRAME lis.
END FUNCTION.

FUNCTION fLimitCompareName RETURNS LOGIC
   (iiLimitCompare AS INT):

   lcLimitCompare = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                     "TMRule",
                                     "LimitCompare",
                                     STRING(iiLimitCompare)).
                                 
   DISP lcLimitCompare WITH FRAME lis.
END FUNCTION.

FUNCTION fCounterTypeName RETURNS LOGIC
   (iiCounterType AS INT):

   lcCounterType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                    "TMRule",
                                    "CounterType",
                                    STRING(iiCounterType)).
                                 
   DISP lcCounterType WITH FRAME lis.
END FUNCTION.

FUNCTION fPayTypeName RETURNS LOGIC
   (iiPayType AS INT):

   lcPayType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                 "CLIType",
                                 "PayType",
                                 STRING(iiPayType)).
                                 
   DISP lcPayType WITH FRAME lis.
END FUNCTION.

FUNCTION fTicketTypeName RETURNS LOGIC
   (iiTicketType AS INT):

   lcTicketType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                 "TMRule",
                                 "TicketType",
                                 STRING(iiTicketType)).
                                 
   DISP lcTicketType WITH FRAME lis.
END FUNCTION.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.


RUN local-Find-First.

IF AVAILABLE TMRule THEN ASSIGN
   Memory       = recid(TMRule)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No rules available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = FALSE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a TMRule  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.

        REPEAT TRANSACTION WITH FRAME lis:

           DISPLAY lcBrand @ TMRule.Brand.

           PROMPT-FOR TMRule.Name WITH FRAME lis.
           IF INPUT TMRule.Name = "" THEN UNDO, LEAVE ADD-ROW.
           
           IF CAN-FIND(FIRST TMRule WHERE 
                             TMRule.Name = INPUT TMRule.Name)
           THEN DO:
              MESSAGE "Rule already exists"
              VIEW-AS ALERT-BOX ERROR.
              NEXT.
           END.
            
           FIND LAST TMRule USE-INDEX TMRuleSeq NO-LOCK NO-ERROR.
           IF AVAILABLE TMRule
           THEN i = TMRule.TMRuleSeq + 1.
           ELSE i = 1.

           CREATE TMRule.
           ASSIGN 
              TMRule.Brand       = lcBrand
              TMRule.TMRuleSeq   = i
              TMRule.Name        = INPUT FRAME lis TMRule.Name
              TMRule.FromDate    = TODAY
              TMRule.ToDate      = 12/31/2049
              TMRule.NewCustomer = TRUE.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhTMRule).

           ASSIGN
           Memory = recid(TMRule)
           xrecid = Memory.  
           LEAVE ADD-ROW.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE TMRule THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND TMRule WHERE recid(TMRule) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE TMRule THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(TMRule).
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
        ufk   = 0
        ufk[1]= 816
        ufk[4] = (IF llShowHistory THEN 38 ELSE 37) 
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)  
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)  
        ufk[7]= 0  
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.
        
        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[5] = 11
           ufk[6] = 0
           ufk[7] = 0.
         
        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW TMRule.Name ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) TMRule.Name WITH FRAME sel.
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


      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND TMRule WHERE recid(TMRule) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE TMRule THEN
              ASSIGN FIRSTrow = i Memory = recid(TMRule).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE TMRule THEN DO:
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
                rtab[1] = recid(TMRule)
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
           IF NOT AVAILABLE TMRule THEN DO:
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
              rtab[FRAME-DOWN] = recid(TMRule).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND TMRule WHERE recid(TMRule) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE TMRule THEN DO:
           Memory = recid(TMRule).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE TMRule THEN Memory = recid(TMRule).
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
           FIND TMRule WHERE recid(TMRule) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       SET lcBrand WHEN gcAllBrand 
           lcName WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       
       IF lcName > "" THEN DO:
          FIND FIRST TMRule WHERE 
                     TMRule.Brand = lcBrand AND
                     TMRule.Name >= lcName
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */
       
     ELSE IF nap = "4" OR nap = "f4" THEN DO:
        llShowHistory = NOT llShowHistory.
        CLEAR FRAME sel ALL no-pause.
        RUN local-find-first.
        ASSIGN
           memory = recid(TMRule)
           ufkey = true
           must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0 THEN DO:  /* add */
        IF gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           LEAVE LOOP.
        END.
        
        ELSE DO:
           must-add = TRUE.
           NEXT LOOP.
        END.    
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0  
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       TMRule.TMRuleSeq TMRule.Name
       TMRule.FromDate TMRule.ToDate TMRule.CounterItems .

       RUN local-find-NEXT.
       IF AVAILABLE TMRule THEN Memory = recid(TMRule).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE TMRule THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(TMRule).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       TMRule.TMRuleSeq TMRule.Name
       TMRule.FromDate TMRule.ToDate TMRule.CounterItems.
       
       IF ok THEN DO:

           FOR EACH TMRItemValue OF TMRule EXCLUSIVE-LOCK:
              IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhTMRItemValue).
              DELETE TMRItemValue.
           END.
           
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhTMRule).

           DELETE TMRule.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE TMRule THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 AND lcRight = "RW" THEN
     REPEAT WITH FRAME lis /*  TRANSACTION */
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhTMRule).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY TMRule.TMRuleSeq.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhTMRule).

       RUN local-disp-row.
       xrecid = recid(TMRule).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(TMRule) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(TMRule) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

ehto = 4.
RUN Syst/ufkey.

fCleanEventObjects().



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND TMRule WHERE recid(TMRule) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND TMRule WHERE recid(TMRule) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN DO:

      IF llShowHistory THEN
      FIND FIRST TMRule WHERE 
                 TMRule.Brand = lcBrand AND
                 TMRule.Todate < TODAY NO-LOCK NO-ERROR.
      ELSE FIND FIRST TMRule WHERE 
                      TMRule.Brand = lcBrand AND
                      TMRule.ToDate >= TODAY NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN DO:
      
      IF llShowHistory THEN
      FIND LAST TMRule WHERE 
                TMRule.Brand = lcBrand AND
                TMRule.ToDate < TODAY NO-LOCK NO-ERROR.
      ELSE FIND LAST TMRule WHERE 
                     TMRule.Brand = lcBrand AND
                     TMRule.ToDate >= TODAY NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-NEXT:
   
   IF order = 1 THEN DO:
      
      IF llShowHistory THEN
      FIND NEXT TMRule WHERE 
                TMRule.Brand = lcBrand AND
                TMRule.Todate < TODAY NO-LOCK NO-ERROR.
      ELSE FIND NEXT TMRule WHERE 
                     TMRule.Brand = lcBrand AND
                     TMRule.ToDate >= TODAY NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-PREV:
   
   IF order = 1 THEN DO:
      
      IF llShowHistory THEN
      FIND PREV TMRule WHERE 
                TMRule.Brand = lcBrand AND
                TMRule.Todate < TODAY NO-LOCK NO-ERROR.
      ELSE FIND PREV TMRule WHERE 
                     TMRule.Brand = lcBrand AND
                     TMRule.ToDate >= TODAY NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       TMRule.TMRuleSeq 
       TMRule.Name
       TMRule.FromDate
       TMRule.ToDate
       TMRule.PayType
       TMRule.CounterItems
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF VAR llDispMenu   AS LOG  NO-UNDO.
   DEF VAR lcNewValue   AS CHAR NO-UNDO.
   DEF VAR ldtNewDate   AS DATE NO-UNDO.

   llDispMenu = NOT NEW TMRule.

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      DISP 
         TMRule.Brand          
         TMRule.TMRuleSeq        
         TMRule.Name        
         TMRule.ToDate           
         TMRule.FromDate        
         TMRule.PayType
         TMRule.TicketType
         TMRule.CounterType
         TMRule.CounterItems          
         TMRule.CounterAmount        
         TMRule.LimitCompare
         TMRule.LimitSource          
         TMRule.CounterPeriod        
         TMRule.NewCustomer       
      WITH FRAME lis.

      fLimitSourceName(TMRule.LimitSource).
      fCounterPeriodName(TMRule.CounterPeriod).
      fCounterAmount(TMRule.CounterAmount).
      fPayTypeName(TMRule.PayType).
      fTicketTypeName(TMRule.TicketType).
      fCounterTypeName(TMRule.CounterType).
      fLimitCompareName(TMRule.LimitCompare).
      
      IF llDispMenu THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7    WHEN lcRight = "RW"
            ufk[2] = 1518 WHEN lcRight = "RW"   
            ufk[5] = 1520
            ufk[6] = 1521
            ufk[7] = 1522
            ufk[8] = 8
            ehto   = 0.
         
         RUN Syst/ufkey.
      END.
      ELSE ASSIGN toimi      = 1
                  llDispMenu = TRUE.
                  
      IF toimi = 1 THEN DO TRANS:
         RUN pUpdate.
      END.
            
      /* select items from which counter is accumulated */   
      ELSE IF toimi = 2 THEN DO TRANS:

         RUN Syst/fieldselection.p ("TMQueue",
                             "COUNTER ITEMS",
                             TMRule.CounterItems,
                             (IF TMRule.TicketType EQ {&TICKET_TYPE_FRAUD}
                              THEN "#FraudGroup" ELSE ""),
                             "",
                             6,
                             "a" + 
                             IF CAN-FIND(FIRST TMRItemValue OF TMRule)
                             THEN ""
                             ELSE "d",
                             OUTPUT lcNewValue).
                      
         IF lcNewValue > "" AND lcNewValue NE TMRule.CounterItems THEN DO:
   
            ASSIGN
               ok         = TRUE
               ldtNewDate = ?.
            
            IF CAN-FIND(FIRST TMRItemValue OF TMRule) THEN DO:
               ok = FALSE.
               MESSAGE "Rule already has item value definitions. Are You" 
                       "sure that you want to change item configuration?"
               VIEW-AS ALERT-BOX QUESTION
               BUTTONS YES-NO
               SET ok.
               
               IF ok THEN DO:
                  ldtNewDate = TODAY + 1.

                  PAUSE 0.
                  UPDATE ldtNewDate 
                     LABEL "Effective Date"
                     HELP "Date when new configuration becomes effective"
                     FORMAT "99-99-99"
                     VALIDATE (INPUT ldtNewDate NE ? AND 
                               INPUT ldtNewDate >= TODAY + 1,
                               "Date is mandatory and cannot be in the past")
                  WITH OVERLAY ROW 10 CENTERED SIDE-LABELS 
                     TITLE " NEW ITEMS " FRAME fNewItem.
                  
                  HIDE FRAME fNewItem.
               END.
            END.   
 
            IF ok THEN DO:
               FIND CURRENT TMRule EXCLUSIVE-LOCK.
                
               /* end current item values and create new ones */
               IF ldtNewDate NE ? THEN      
               FOR EACH TMRItemValue OF TMRule EXCLUSIVE-LOCK WHERE
                        TMRItemValue.ToDate >= ldtNewDate AND
                        TMRItemValue.FromDate < ldtNewDate:
               
                  IF llDoEvent THEN RUN StarEventSetOldBuffer(lhTMRItemValue).
                  
                  TMRItemValue.ToDate = ldtNewDate - 1.

                  IF llDoEvent THEN 
                     RUN StarEventMakeModifyEvent(lhTMRItemValue).

                  CREATE bItemValue.
                  BUFFER-COPY TMRItemValue TO bItemValue.
                  
                  ASSIGN
                     bItemValue.FromDate  = ldtNewDate
                     bItemValue.ToDate    = 12/31/2049.
                     
                  /* only new items can be added and always to the end of
                     old list, items cannot be deleted from the middle */
                  DO i = NUM-ENTRIES(TMRule.CounterItems) + 1 TO 
                         NUM-ENTRIES(lcNewValue):
                     bItemValue.CounterItemValues = 
                        bItemValue.CounterItemValues + ",*".
                  END.
 
                  IF llDoEvent THEN 
                     RUN StarEventMakeCreateEvent(lhbItemValue).
               END.

               TMRule.CounterItems = lcNewValue.
            END.
         END.
            
         NEXT.
      END.
         
      /* update item values */
      ELSE IF toimi = 5 THEN RUN Syst/tmritemvalue.p (TMRule.TMRuleSeq).
      
      /* update limits */
      ELSE IF toimi = 6 THEN RUN Syst/tmrlimit.p (TMRule.TMRuleSeq).
                         
      /* functions */
      ELSE IF toimi = 7 THEN do:
          RUN Syst/tmrulefunc (TMRule.TMRuleSeq).
      end.
      
      ELSE IF toimi = 8 THEN LEAVE.
   END.

END PROCEDURE.

PROCEDURE pUpdate:

   DEF VAR lcFrameField   AS CHAR NO-UNDO.
   DEF VAR llUpdateSource AS LOG  NO-UNDO.
   DEF VAR lcHelpFields   AS CHAR NO-UNDO.

   lcHelpFields = "CounterAmount,LimitSource,CounterPeriod,CounterType," +
                  "LimitCompare,PayType".
                  
   FIND CURRENT TMRule EXCLUSIVE-LOCK.
      
   llUpdateSource = (NEW TMRule OR
                     NOT CAN-FIND(FIRST TMRLimit OF TMRule)).
   ehto = 9.
   RUN Syst/ufkey.
   
   REPEAT ON ENDKEY UNDO, LEAVE:
   
      UPDATE
         TMRule.Name WHEN NOT NEW TMRule       
         TMRule.FromDate        
         TMRule.ToDate       
         TMRule.TicketType    WHEN NEW TMRule
         TMRule.PayType       WHEN NEW TMRule
         TMRule.CounterType   WHEN NEW TMRule
         TMRule.CounterAmount WHEN NEW TMRule
         TMRule.LimitSource   WHEN llUpdateSource
         TMRule.LimitCompare  WHEN NEW TMRule
         TMRule.CounterPeriod WHEN NEW TMRule       
         TMRule.NewCustomer   WHEN
                              LOOKUP(STRING(TMRule.LimitSource),"0,3,4") = 0
                                   
      WITH FRAME lis EDITING:
 
         READKEY.

         IF KEYLABEL(LASTKEY) = "F9" AND 
            LOOKUP(FRAME-FIELD,lcHelpFields) > 0 THEN DO:

            lcFrameField = FRAME-FIELD.

            IF FRAME-FIELD = "PayType" THEN 
               RUN Help/h-tmscodes(INPUT "CLIType", 
                                    lcFrameField, 
                                    ?, 
                                    OUTPUT lcCode).
            ELSE 
               RUN Help/h-tmscodes(INPUT "TMRule", 
                                    lcFrameField, 
                                    "TMR", 
                                    OUTPUT lcCode).

            IF lcCode ne "" AND lcCode NE ? 
            THEN DO WITH FRAME lis:
               CASE lcFrameField:
               WHEN "TicketType" THEN 
                  DISPLAY lcCode ;& TMRule.TicketType.
               WHEN "PayType" THEN 
                  DISPLAY lcCode ;& TMRule.PayType.
               WHEN "CounterType" THEN 
                  DISPLAY lcCode ;& TMRule.CounterType.
               WHEN "CounterAmount" THEN 
                  DISPLAY lcCode ;& TMRule.CounterAmount.
               WHEN "LimitSource" THEN 
                  DISPLAY INTEGER(lcCode) ;& TMRule.LimitSource.
               WHEN "LimitCompare" THEN 
                  DISPLAY INTEGER(lcCode) ;& TMRule.LimitCompare.
               WHEN "CounterPeriod" THEN 
                  DISPLAY INTEGER(lcCode) ;& TMRule.CounterPeriod.
               END CASE.   
            END.

            ehto = 9.
            RUN Syst/ufkey.

            NEXT. 
         END.

         IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
            PAUSE 0.
            
            IF FRAME-FIELD = "TicketType" THEN DO:

               IF NOT DYNAMIC-FUNCTION("fTMSCodeChk" IN ghFunc1,
                                       "TMRule",
                                       "TicketType",
                                       STRING(INPUT TMRule.TicketType))
               THEN DO:
                  MESSAGE "Unknown ticket type"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
         
               fTicketTypeName(INPUT INPUT TMRule.TicketType).
            END.
 
            ELSE IF FRAME-FIELD = "PayType" THEN DO:

               IF NOT DYNAMIC-FUNCTION("fTMSCodeChk" IN ghFunc1,
                                       "CLIType",
                                       "PayType",
                                       STRING(INPUT TMRule.PayType))
               THEN DO:
                  MESSAGE "Unknown payment type"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
         
               fPayTypeName(INPUT INPUT TMRule.PayType).
            END.
 
            ELSE IF FRAME-FIELD = "CounterAmount" THEN DO:

               IF NOT DYNAMIC-FUNCTION("fTMSCodeChk" IN ghFunc1,
                                       "TMRule",
                                       "CounterAmount",
                                       INPUT INPUT TMRule.CounterAmount)
               THEN DO:
                  MESSAGE "Unknown counter unit"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
         
               fCounterAmount(INPUT INPUT TMRule.CounterAmount).
            END.
 
            ELSE IF FRAME-FIELD = "LimitSource" THEN DO:

               IF NOT DYNAMIC-FUNCTION("fTMSCodeChk" IN ghFunc1,
                                       "TMRule",
                                       "LimitSource",
                                       STRING(INPUT TMRule.LimitSource))
               THEN DO:
                  MESSAGE "Unknown limit source"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
         
               fLimitSourceName(INPUT INPUT TMRule.LimitSource).
               
               IF INPUT TMRule.LimitSource = 3 OR 
                  INPUT TMRule.LimitSource = 0 
               THEN DISPLAY FALSE @ TMRule.NewCustomer.
            END.
 
            ELSE IF FRAME-FIELD = "CounterPeriod" THEN DO:

               IF NOT DYNAMIC-FUNCTION("fTMSCodeChk" IN ghFunc1,
                                       "TMRule",
                                       "CounterPeriod",
                                       STRING(INPUT TMRule.CounterPeriod))
               THEN DO:
                  MESSAGE "Unknown counter period"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
         
               fCounterPeriodName(INPUT INPUT TMRule.CounterPeriod).
            END.
 
            ELSE IF FRAME-FIELD = "CounterType" THEN DO:

               IF NOT DYNAMIC-FUNCTION("fTMSCodeChk" IN ghFunc1,
                                       "TMRule",
                                       "CounterType",
                                       STRING(INPUT TMRule.CounterType))
               THEN DO:
                  MESSAGE "Unknown counter type"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
         
               fCounterTypeName(INPUT INPUT TMRule.CounterType).
            END.

            ELSE IF FRAME-FIELD = "LimitCompare" THEN DO:

               IF NOT DYNAMIC-FUNCTION("fTMSCodeChk" IN ghFunc1,
                                       "TMRule",
                                       "LimitCompare",
                                       STRING(INPUT TMRule.LimitCompare))
               THEN DO:
                  MESSAGE "Unknown limit comparison method"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
         
               fLimitCompareName(INPUT INPUT TMRule.LimitCompare).
            END.

         END.
            
         APPLY LASTKEY.
      END.
   
      IF LOOKUP(STRING(TMRule.LimitSource),"3,4") > 0
         THEN TMRule.NewCustomer = FALSE.
        
      LEAVE.
   END.

END PROCEDURE.

