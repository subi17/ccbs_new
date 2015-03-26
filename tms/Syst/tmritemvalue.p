/* ----------------------------------------------------------------------
  MODULE .......: tmritemvalue.p
  TASK .........: UPDATEs table TMRItemValue
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 09.05.08
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{commali.i} 
{lib/tokenlib.i}
{lib/tokenchk.i 'TMRItemValue'}

{eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhTMRItemValue AS HANDLE NO-UNDO.
   lhTMRItemValue = BUFFER TMRItemValue:HANDLE.
   RUN StarEventInitialize(lhTMRItemValue).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhTMRItemValue).
   END.

END.

DEF INPUT PARAMETER iiTMRuleSeq AS INT  NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 5.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 10.
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

DEF VAR lcStatus       AS CHAR NO-UNDO.
DEF VAR lcField        AS CHAR NO-UNDO. 
DEF VAR lcCode         AS CHAR NO-UNDO. 
DEF VAR lcItemField    AS CHAR NO-UNDO EXTENT 6.
DEF VAR lcItemValue    AS CHAR NO-UNDO EXTENT 6.
DEF VAR lcItemHelp     AS CHAR NO-UNDO EXTENT 6.
DEF VAR lcItemLabel    AS CHAR NO-UNDO EXTENT 6.
DEF VAR lcItemName     AS CHAR NO-UNDO EXTENT 6.

DEF VAR liCnt        AS INT    NO-UNDO.
DEF VAR lhTable      AS HANDLE NO-UNDO.
DEF VAR lhField      AS HANDLE NO-UNDO.
DEF VAR liItemQty    AS INT    NO-UNDO.

DEF VAR lcCounterItem AS CHAR NO-UNDO. 
DEF VAR lcColumnLabel AS CHAR NO-UNDO. 
DEF VAR lcHelp AS CHAR NO-UNDO. 

FORM
    TMRItemValue.FromDate
    TMRItemValue.ToDate  
    lcItemValue[1]  COLUMN-LABEL " " FORMAT "X(14)"
    lcItemValue[2]  COLUMN-LABEL " " FORMAT "X(14)"
    lcItemValue[3]  COLUMN-LABEL " " FORMAT "X(14)" 
    lcItemValue[4]  COLUMN-LABEL " " FORMAT "X(4)" 
    lcItemValue[5]  COLUMN-LABEL " " FORMAT "X(4)" 
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       " ITEM VALUES FOR RULE " + STRING(iiTMRuleSeq) + " "
    FRAME sel.

FORM
    TMRItemValue.TMRuleSeq       COLON 15
       TMRule.Name NO-LABEL SKIP(1)
    TMRItemValue.FromDate        COLON 15 
    TMRItemValue.ToDate          COLON 15
       SKIP(1)

    lcItemLabel[1] TO 15
       NO-LABEL
       FORMAT "X(14)"
    lcItemValue[1]               
       NO-LABEL 
       FORMAT "X(20)" 
    lcItemName[1] 
       NO-LABEL
       FORMAT "X(25)"
       SKIP  

    lcItemLabel[2] TO 15
       NO-LABEL
       FORMAT "X(14)"
    lcItemValue[2]               
       NO-LABEL 
       FORMAT "X(20)" 
    lcItemName[2] 
       NO-LABEL
       FORMAT "X(25)"
       SKIP
 
    lcItemLabel[3] TO 15
       NO-LABEL
       FORMAT "X(14)"
    lcItemValue[3]               
       NO-LABEL 
       FORMAT "X(20)" 
    lcItemName[3] 
       NO-LABEL
       FORMAT "X(25)"
       SKIP
 
    lcItemLabel[4] TO 15
       NO-LABEL
       FORMAT "X(14)"
    lcItemValue[4]               
       NO-LABEL 
       FORMAT "X(20)"   
    lcItemName[4] 
       NO-LABEL
       FORMAT "X(25)"
       SKIP
 
    lcItemLabel[5] TO 15
       NO-LABEL
       FORMAT "X(14)"
    lcItemValue[5]               
       NO-LABEL 
       FORMAT "X(20)"  
    lcItemName[5] 
       NO-LABEL
       FORMAT "X(25)"
       SKIP
 
    lcItemLabel[6] TO 15
       NO-LABEL
       FORMAT "X(14)"
    lcItemValue[6]               
       NO-LABEL
       FORMAT "X(20)" 
    lcItemName[6] 
       NO-LABEL
       FORMAT "X(25)"
       SKIP
 
WITH  OVERLAY ROW 7 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.


FUNCTION fDispItemName RETURNS LOGIC
   (icItemValue AS CHAR,
    iiExtent    AS INT):

   DEF VAR llValid AS LOG NO-UNDO.
   
   ASSIGN 
      llValid              = TRUE
      lcItemName[iiExtent] = "".
   
   IF icItemValue = "" THEN DO:
      MESSAGE "Value is mandatory"
      VIEW-AS ALERT-BOX ERROR.
      RETURN FALSE.
   END.
   
   /* if asterisk is used then user is responsible for the given value */
   IF INDEX(icItemValue,"*") > 0 THEN DO:
      DISP lcItemName[iiExtent] WITH FRAME lis.
      RETURN TRUE.
   END.
   
   CASE lcItemField[iiExtent]:
   WHEN "RateCCN"  THEN DO:
      FIND FIRST CCN WHERE
                 CCN.Brand = gcBrand AND
                 CCN.CCN   = INTEGER(icItemValue) NO-LOCK NO-ERROR.
      IF AVAILABLE CCN THEN lcItemName[iiExtent] = CCN.CCNName.
      ELSE llValid = FALSE.
   END.
      
   WHEN "BillCode" THEN DO:
      IF icItemValue = "TopupEvent" THEN 
         lcItemName[iiExtent] = "Use Topup Events".
      ELSE DO:
         FIND FIRST BillItem WHERE
                    BillItem.Brand    = gcBrand AND
                    BillItem.BillCode = icItemValue NO-LOCK NO-ERROR.
         IF AVAILABLE BillItem THEN lcItemName[iiExtent] = BillItem.BIName.
         ELSE llValid = FALSE.
      END.    
   END.
   
   WHEN "BDest" THEN DO:
      FIND FIRST BDest WHERE
                 BDest.Brand = gcBrand AND
                 BDest.BDest = icItemValue NO-LOCK NO-ERROR.
      IF AVAILABLE BDest THEN lcItemName[iiExtent] = BDest.BDName.
   
      /* should a complete b-number analysis be made? */
      llValid = TRUE.
   END.

   WHEN "CLIType" THEN DO:
      FIND FIRST CLIType WHERE
                 CLIType.Brand    = gcBrand AND
                 CLIType.CLIType = icItemValue NO-LOCK NO-ERROR.
      IF AVAILABLE CLIType THEN lcItemName[iiExtent] = CLIType.CLIName.
      ELSE llValid = FALSE.
   END.
    
   WHEN "SpoCMT"   THEN DO:
   END.
      
   WHEN "CustNum"  THEN DO:
      FIND FIRST Customer WHERE
                 Customer.CustNum = INTEGER(icItemValue) NO-LOCK NO-ERROR.
      IF AVAILABLE Customer THEN 
         lcItemName[iiExtent] = Customer.FirstName + " " + Customer.CustName.
      ELSE llValid = FALSE.   
   END.
   
   END CASE.
       
   IF NOT llValid THEN 
      MESSAGE "Unknown value"
      VIEW-AS ALERT-BOX ERROR.
      
   DISP lcItemName[iiExtent] WITH FRAME lis.
   
   RETURN llValid.
       
END FUNCTION.


cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

FIND FIRST TMRule WHERE TMRule.TMRuleSeq = iiTMRuleSeq NO-LOCK NO-ERROR.
IF NOT AVAILABLE TMRule THEN DO:
   MESSAGE "Rule not available"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

ASSIGN
   lhTable   = BUFFER TMQueue:HANDLE
   liItemQty = NUM-ENTRIES(TMRule.CounterItems).

IF liItemQty < 1 THEN DO:
   MESSAGE "Item configuration missing for this rule"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

/* this is just an arbitrary limit, can be increased when necessary */    
IF liItemQty > 6 THEN DO:
   MESSAGE "6 is the maximum nbr of items"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.
    
DO liCnt = 1 TO liItemQty:
 
   lcCounterItem = ENTRY(liCnt,TMRule.CounterItems).

   IF lcCounterItem BEGINS "#" THEN ASSIGN
      lcItemField[liCnt] = lcCounterItem
      lcItemLabel[liCnt] = FILL(" ",13 - LENGTH(lcCounterItem)) +
                        lcCounterItem + ":"
      lcColumnLabel = lcCounterItem
      lcHelp = "".
   ELSE ASSIGN
      lhField = lhTable:BUFFER-FIELD(lcCounterItem)
      lcItemField[liCnt] = lhField:NAME
      lcColumnLabel = lhField:COLUMN-LABEL
      lcHelp = lhField:HELP
      /* side-label for update screen */
      lcItemLabel[liCnt] = FILL(" ",13 - LENGTH(lhField:LABEL)) + 
                           lhField:LABEL + ":".

   /* column-label for browser, 
      help message for update screen */
   CASE liCnt:
   WHEN 1 THEN ASSIGN
      lcItemValue[1]:LABEL IN FRAME sel = lcColumnLabel 
      lcItemValue[1]:HELP IN FRAME lis  = lcHelp. 
   WHEN 2 THEN ASSIGN
      lcItemValue[2]:LABEL IN FRAME sel = lcColumnLabel
      lcItemValue[2]:HELP IN FRAME lis  = lcHelp. 
   WHEN 3 THEN ASSIGN
      lcItemValue[3]:LABEL IN FRAME sel = lcColumnLabel
      lcItemValue[3]:HELP IN FRAME lis  = lcHelp. 
   WHEN 4 THEN ASSIGN
      lcItemValue[4]:LABEL IN FRAME sel = lcColumnLabel
      lcItemValue[4]:HELP IN FRAME lis  = lcHelp. 
   WHEN 5 THEN ASSIGN
      lcItemValue[5]:LABEL IN FRAME sel = lcColumnLabel
      lcItemValue[5]:HELP IN FRAME lis  = lcHelp. 
   WHEN 6 THEN ASSIGN
      lcItemValue[6]:HELP IN FRAME lis  = lcHelp. 
   END CASE.
   
   /* F9 help program for update screen */
   CASE lcItemField[liCnt]:
   WHEN "RateCCN"  THEN lcItemHelp[liCnt] = "nnmase".
   WHEN "BillCode" THEN lcItemHelp[liCnt] = "nntuse".
   WHEN "BDest"    THEN lcItemHelp[liCnt] = "nnbtse".
   WHEN "SpoCMT"   THEN lcItemHelp[liCnt] = "".
   WHEN "CustNum"  THEN lcItemHelp[liCnt] = "nnasel".
   END CASE.
   
END.


RUN local-Find-First.

IF AVAILABLE TMRItemValue THEN ASSIGN
   Memory       = recid(TMRItemValue)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No item values available !" VIEW-AS ALERT-BOX.
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

   IF must-add THEN DO:  /* Add a TMRItemValue  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis ALL NO-PAUSE.
        ehto = 9. RUN ufkey.

        REPEAT TRANSACTION WITH FRAME lis:

           DISPLAY iiTMRuleSeq @ TMRItemValue.TMRuleSeq.

           CREATE TMRItemValue.
           ASSIGN 
              TMRItemValue.TMRuleSeq = iiTMRuleSeq
              TMRItemValue.FromDate  = TODAY
              TMRItemValue.ToDate    = 12/31/2049.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
              TMRItemValue.CounterItemValues = "" 
           THEN UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhTMRItemValue).

           ASSIGN
           Memory = recid(TMRItemValue)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE TMRItemValue THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND TMRItemValue WHERE recid(TMRItemValue) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE TMRItemValue THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(TMRItemValue).
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
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)  
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0) 
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.
        
        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[5] = 11
           ufk[6] = 0
           ufk[7] = 0.
         
        RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW TMRItemValue.FromDate ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) TMRItemValue.FromDate WITH FRAME sel.
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
        FIND TMRItemValue WHERE recid(TMRItemValue) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE TMRItemValue THEN
              ASSIGN FIRSTrow = i Memory = recid(TMRItemValue).
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
           IF NOT AVAILABLE TMRItemValue THEN DO:
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
                rtab[1] = recid(TMRItemValue)
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
           IF NOT AVAILABLE TMRItemValue THEN DO:
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
              rtab[FRAME-DOWN] = recid(TMRItemValue).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND TMRItemValue WHERE recid(TMRItemValue) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE TMRItemValue THEN DO:
           Memory = recid(TMRItemValue).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE TMRItemValue THEN Memory = recid(TMRItemValue).
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
           FIND TMRItemValue WHERE recid(TMRItemValue) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        IF gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           LEAVE LOOP.
        END.
        
        ELSE DO:
           must-add = TRUE.
           NEXT LOOP.
        END.    
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
          TMRItemValue.FromDate
          TMRItemValue.ToDate
          lcItemValue[1 for 5].

       RUN local-find-NEXT.
       IF AVAILABLE TMRItemValue THEN Memory = recid(TMRItemValue).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE TMRItemValue THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(TMRItemValue).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          TMRItemValue.FromDate
          TMRItemValue.ToDate
          lcItemValue[1 for 5].
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhTMRItemValue).

           DELETE TMRItemValue.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE TMRItemValue THEN DO:
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
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhTMRItemValue).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhTMRItemValue).

       RUN local-disp-row.
       xrecid = recid(TMRItemValue).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(TMRItemValue) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(TMRItemValue) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

ehto = 4.
RUN ufkey.

fCleanEventObjects().



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND TMRItemValue WHERE recid(TMRItemValue) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND TMRItemValue WHERE recid(TMRItemValue) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN FIND FIRST TMRItemValue WHERE 
      TMRItemValue.TMRuleSeq = iiTMRuleSeq
      USE-INDEX CounterItemValues NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN FIND LAST TMRItemValue WHERE 
      TMRItemValue.TMRuleSeq = iiTMRuleSeq
      USE-INDEX CounterItemValues NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN FIND NEXT TMRItemValue WHERE 
      TMRItemValue.TMRuleSeq = iiTMRuleSeq
      USE-INDEX CounterItemValues NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN FIND PREV TMRItemValue WHERE 
      TMRItemValue.TMRuleSeq = iiTMRuleSeq
      USE-INDEX CounterItemValues NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       TMRItemValue.FromDate
       TMRItemValue.ToDate
       lcItemValue[1 FOR 5]
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

   lcItemValue = "". 
   DO liCnt = 1 TO MIN(NUM-ENTRIES(TMRItemValue.CounterItemValues),liItemQty):
      lcItemValue[liCnt] = ENTRY(liCnt,TMRItemValue.CounterItemValues).
   END.
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF VAR liExtent  AS INT  NO-UNDO.
   DEF VAR lcNewList AS CHAR NO-UNDO.
   
   DEF BUFFER bItemValue FOR TMRItemValue.
   
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      DISP 
         TMRItemValue.TMRuleSeq        
         TMRule.Name
         TMRItemValue.FromDate
         TMRItemValue.ToDate        
         lcItemLabel[1 for 6] 
         lcItemValue[1 for 6]
       WITH FRAME lis.

      lcItemName = "".
      
      DO liCnt = 1 TO 6:
         IF lcItemValue[liCnt] = "" THEN DO:
            DISP lcItemName[liCnt] WITH FRAME lis.
            NEXT.
         END.
         
         fDispItemName(lcItemValue[liCnt],
                       liCnt).
      END.
      
      IF NOT NEW TMRItemValue THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7 WHEN lcRight = "RW"
            ufk[8] = 8
            ehto   = 0.
         
         RUN ufkey.
         
         IF toimi = 8 THEN LEAVE.
      END.

      FIND CURRENT TMRItemValue EXCLUSIVE-LOCK.
      
      UPDATE
         TMRItemValue.FromDate WHEN NEW TMRItemValue
         TMRItemValue.ToDate        
         lcItemValue[1] 
         lcItemValue[2] WHEN liItemQty >= 2
         lcItemValue[3] WHEN liItemQty >= 3
         lcItemValue[4] WHEN liItemQty >= 4
         lcItemValue[5] WHEN liItemQty >= 5
         lcItemValue[6] WHEN liItemQty >= 6
      WITH FRAME lis EDITING:
 
         READKEY.

         IF KEYLABEL(LASTKEY) = "F9" AND 
            LOOKUP(FRAME-FIELD,"lcItemValue") > 0 
         THEN DO:

            IF lcItemHelp[FRAME-INDEX] > "" THEN DO:
               ASSIGN
                  liExtent = FRAME-INDEX
                  siirto   = ?.
               RUN VALUE(lcItemHelp[liExtent]).
               
               IF siirto NE ? THEN 
               DISP siirto @ lcItemValue[liExtent] WITH FRAME lis.
            END.
               
            ehto = 9.
            RUN ufkey.
            NEXT. 
         END.

         IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
            PAUSE 0.

            IF FRAME-FIELD = "lcItemValue" THEN DO:
               IF NOT fDispItemName(INPUT INPUT lcItemValue[FRAME-INDEX],
                                    FRAME-INDEX)
               THEN NEXT.
            END.
         END.
            
         APPLY LASTKEY.
      END.

      lcNewList = "".
      DO liCnt = 1 TO 6:
         IF lcItemValue[liCnt] = "" THEN NEXT.
         
         lcNewList = lcNewList +
                     (IF lcNewList > "" THEN "," ELSE "") + 
                     lcItemValue[liCnt].
      END.
      
      IF CAN-FIND(FIRST bItemValue WHERE
                        bItemValue.TMRuleSeq = iiTMRuleSeq AND
                        bItemValue.CounterItemValues = lcNewList AND
                        bItemValue.ToDate   >= TMRItemValue.FromDate AND
                        bItemValue.FromDate <= TMRItemValue.ToDate AND
                        RECID(bItemValue) NE RECID(TMRItemValue))
      THEN DO:
         MESSAGE "A similar item row already exists for given period"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      TMRItemValue.CounterItemValues = lcNewList.
      
      LEAVE.
   
   END.
   
END PROCEDURE.

