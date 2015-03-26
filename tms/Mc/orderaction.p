/* ----------------------------------------------------------------------
  MODULE .......: OrderAction.p
  TASK .........: OrderAction list and view CUI
  APPLICATION ..: tms
  AUTHOR .......: anttis
  CREATED ......: 16.08.11
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{commali.i} 
{lib/tokenlib.i}
{lib/tokenchk.i 'OrderAction'}
{eventval.i}
{timestamp.i}
{cparam2.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhOrderAction AS HANDLE NO-UNDO.
   lhOrderAction = BUFFER OrderAction:HANDLE.
   RUN StarEventInitialize(lhOrderAction).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhOrderAction).
   END.

END.

DEF INPUT PARAMETER iiOrder AS INT NO-UNDO.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 12.
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

DEF VAR lcType       AS CHAR NO-UNDO.
DEF VAR lcBegin      AS CHAR NO-UNDO.
DEF VAR lcEnd        AS CHAR NO-UNDO.
DEF VAR lcHelp       AS CHAR NO-UNDO.
DEF VAR lcItemName   AS CHAR NO-UNDO. 

FORM
    OrderAction.ItemType FORMAT "X(12)"
    OrderAction.ItemKey  FORMAT "X(15)"
    lcItemName         FORMAT "X(30)"       COLUMN-LABEL "Name"
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       " ORDER ACTION ITEMS FOR ORDER " + STRING(iiOrder) + " "
    FRAME sel.

FORM
    OrderAction.OrderId  COLON 20 
    OrderAction.ItemType COLON 20 FORMAT "x(12)"  
       lcType COLON 41 FORMAT "X(30)" NO-LABEL SKIP
    OrderAction.ItemKey  COLON 20 FORMAT "x(15)"  
       lcItemName COLON 41 NO-LABEL FORMAT "X(30)" SKIP
WITH  OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FUNCTION fItemType RETURNS LOGIC
   (icItemType AS CHAR):

   IF icItemType > "" THEN 
      lcType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                "OfferItem",
                                "ItemType",
                                icItemType).
   ELSE lcType = "".
                                
END FUNCTION.

FUNCTION fItemName RETURNS LOGIC
   (icItemType  AS CHAR,
    icItemValue AS CHAR):

   ASSIGN
      lcItemName = "".
   
   CASE icItemType:
   WHEN "BundleItem" THEN lcItemName = fCParamC(icItemValue).
   END CASE.
   
END.

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

FIND FIRST Order WHERE 
           Order.Brand = gcBrand AND
           Order.OrderID = iiOrder NO-LOCK NO-ERROR.
IF NOT AVAILABLE Order THEN DO:
   MESSAGE "Order not available"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

RUN local-Find-First.

IF AVAILABLE OrderAction THEN ASSIGN
   Memory       = recid(OrderAction)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No items available" VIEW-AS ALERT-BOX INFORMATION.
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

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND OrderAction WHERE recid(OrderAction) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE OrderAction THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(OrderAction).
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
        ufk[5]= 0  
        ufk[6]= 0 
/*        ufk[7] = 1752 */ /* eventlog */
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.
          
        RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW OrderAction.ItemType ;(uchoose.i;) NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) OrderAction.ItemType WITH FRAME sel. 
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"5,f5,7,f7,8,f8") = 0 THEN DO:
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
        FIND OrderAction WHERE recid(OrderAction) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE OrderAction THEN
              ASSIGN FIRSTrow = i Memory = recid(OrderAction).
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
           IF NOT AVAILABLE OrderAction THEN DO:
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
                rtab[1] = recid(OrderAction)
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
           IF NOT AVAILABLE OrderAction THEN DO:
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
              rtab[FRAME-DOWN] = recid(OrderAction).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND OrderAction WHERE recid(OrderAction) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE OrderAction THEN DO:
           Memory = recid(OrderAction).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE OrderAction THEN Memory = recid(OrderAction).
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
           FIND OrderAction WHERE recid(OrderAction) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

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
     THEN DO TRANS:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
          OrderAction.ItemType
          OrderAction.ItemKey
          lcItemName.

       RUN local-find-NEXT.
       IF AVAILABLE OrderAction THEN Memory = recid(OrderAction).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE OrderAction THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(OrderAction).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          OrderAction.ItemType
          OrderAction.ItemKey
          lcItemName .
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhOrderAction).

           DELETE OrderAction.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE OrderAction THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANS
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOrderAction).

       ASSIGN ac-hdr = " VIEW " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSEndStamp */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrderAction).

       RUN local-disp-row.
       xrecid = recid(OrderAction).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(OrderAction) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(OrderAction) must-print = TRUE.
        NEXT LOOP.
     END.
         
     ELSE IF LOOKUP(nap,"7,f7") > 0 THEN DO: 
        RUN eventsel.p("OrderAction", "#BEGIN" + chr(255) + gcBrand + chr(255) + STRING(iiOrder)).
        ufkey = TRUE.
        NEXT.
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
      FIND OrderAction WHERE recid(OrderAction) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND OrderAction WHERE recid(OrderAction) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN FIND FIRST OrderAction WHERE 
      OrderAction.Brand = gcBrand AND
      OrderAction.OrderId = iiOrder NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN FIND LAST OrderAction WHERE 
      OrderAction.Brand = gcBrand AND
      OrderAction.OrderId = iiOrder NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN FIND NEXT OrderAction WHERE 
      OrderAction.Brand = gcBrand AND
      OrderAction.OrderId = iiOrder NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN FIND PREV OrderAction WHERE 
      OrderAction.Brand = gcBrand AND
      OrderAction.OrderId = iiOrder NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
   RUN local-find-others.
   CLEAR FRAME sel NO-PAUSE.
       
   DISPLAY 
      OrderAction.ItemType
      OrderAction.ItemKey
      lcItemName
   WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

   DEF VAR liTime AS INT NO-UNDO.
   
   fItemName(OrderAction.ItemType,
             OrderAction.ItemKey).

END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF VAR llUpdateAmount AS LOG  NO-UNDO.

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      fItemType(OrderAction.ItemType).
         
      DISP 
         OrderAction.OrderId        
         OrderAction.ItemType
         OrderAction.ItemKey        
         lcType
         lcItemName
      WITH FRAME lis.

      IF NOT NEW OrderAction THEN DO:
      
         ASSIGN 
            ufk    = 0
            ufk[8] = 8
            ehto   = 0.
         
         RUN ufkey.
         
         IF toimi = 8 THEN LEAVE.
      END.

   END.

END PROCEDURE.

