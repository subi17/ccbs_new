/* -----------------------------------------------
  MODULE .......: tmscodespr.p
  FUNCTION .....: TMS codes browser
  APPLICATION ..: TMS
  AUTHOR .......: 
  CREATED ......: 12.01.06
  CHANGED ......: 28.11.06/aam nnasla
  Version ......: TMS Master
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/tmsconst.i}

DEF INPUT PARAMETER   icStatusCode  AS CHAR NO-UNDO.
DEF INPUT PARAMETER   iiOrderID     AS INT  NO-UNDO.
DEF INPUT PARAMETER   ilTrue        AS LOG  NO-UNDO FORMAT "TRUE/FALSE".

DEF TEMP-TABLE ttBrowser NO-UNDO
   FIELD OFName    AS CHAR
   FIELD OFID      AS INT 
   FIELD OFModule  AS CHAR 

   INDEX OFName    OFName
   INDEX OFID      OFID.

if iiorderid > 0 THEN 
FIND FIRST Order NO-LOCK WHERE 
           Order.Brand   = gcBrand AND 
           Order.OrderID = iiOrderID NO-ERROR.
           
DEF VAR lcCodeName   AS CHAR NO-UNDO.
DEF VAR liOFName     AS INT  NO-UNDO.
DEF VAR lcError      AS CHAR NO-UNDO.
DEF VAR llTrue       AS LOG  NO-UNDO.
DEF VAR i            AS INT  NO-UNDO.
DEF VAR lcAction AS CHAR NO-UNDO. 
DEF VAR lcOrigStatusCode AS CHAR NO-UNDO. 
lcOrigStatusCode = icStatusCode.

DO i = 1 TO 2:
   
   IF i = 2 THEN icStatusCode = "*".

   FOR EACH OFItem NO-LOCK  WHERE 
            OFItem.StatusCode = icStatusCode,
      FIRST OrderFunction NO-LOCK WHERE 
            OrderFunction.OFID = OFItem.OFID:

      IF AVAIL Order AND
               OrderFunction.OfModule EQ "Mc/orderinctrl.p,iiOrderId,1" AND
               (INDEX(Order.OrderChannel,"pos") > 0 OR
                Order.OrderType > 2) THEN NEXT.

      IF AVAIL Order AND Order.DeliverySecure EQ 1 AND
               OrderFunction.OfModule EQ "Mc/orderinctrl.p,iiOrderId,0" THEN NEXT.
      
      CREATE ttBrowser.     
      ASSIGN
         ttBrowser.OFName   = OrderFunction.OFName
         ttBrowser.OFID     = OFItem.OFID
         ttBrowser.OFModule = OrderFunction.OFModule.
            
   END.         
END.

IF NOT CAN-FIND(FIRST ttBrowser ) THEN DO:
   MESSAGE 
   "CANNOT FIND ANY ORDER FUNCTION ITEMS!"
   VIEW-AS ALERT-BOX.
   RETURN.
END.
                      

DEF VAR xrecid       AS RECID                  NO-UNDO  init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 13.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR icCoName2    AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR Memory       AS ROWID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS ROWID EXTENT 24        NO-UNDO.

form
    ttBrowser.OFName     FORMAT "X(38)" 
        COLUMN-LABEL "            ORDER FUNCTION"    
   
WITH ROW FrmRow width 40 OVERLAY FrmDown DOWN
    CENTERED
    FRAME sel.


cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first.

FIND FIRST ttBrowser  NO-LOCK NO-ERROR.
       
IF AVAILABLE ttBrowser THEN ASSIGN
   Memory       = ROWID(ttBrowser)
   must-print   = TRUE.
ELSE DO:
    MESSAGE 
    "NOT ALLOWED ORDER FUNCTIONS!"
    VIEW-AS ALERT-BOX.
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
        FIND ttBrowser WHERE ROWID(ttBrowser) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose ROWID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttBrowser THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = ROWID(ttBrowser).
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
           ufk    = 0
           ufk[1] = 0
           ufk[2] = 0
           ufk[5] = 11
           ufk[8] = 8 
           ehto   = 3 
           ufkey  = FALSE.

        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW ttBrowser.OFName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) ttBrowser.OFName WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW ttBrowser.OFName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) ttBrowser.OFName WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"8,f8") = 0 THEN DO:
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
        FIND ttBrowser WHERE ROWID(ttBrowser) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttBrowser THEN
              ASSIGN FIRSTrow = i Memory = ROWID(ttBrowser).
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
           IF NOT AVAILABLE ttBrowser THEN DO:
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
                rtab[1] = ROWID(ttBrowser)
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
           IF NOT AVAILABLE ttBrowser THEN DO:
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
              rtab[FRAME-DOWN] = ROWID(ttBrowser).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttBrowser WHERE ROWID(ttBrowser) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE TMSCodes THEN DO:
           Memory = ROWID(ttBrowser).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttBrowser THEN Memory = ROWID(ttBrowser).
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
           FIND ttBrowser WHERE ROWID(ttBrowser) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"5,f5,enter,return") > 0 THEN DO: 
        RUN local-find-this(FALSE).
        DEF VAR lcModule AS CHAR NO-UNDO .
        DEF VAR lcInput1 AS CHAR NO-UNDO.
        DEF VAR lcInput2 AS CHAR NO-UNDO .
        DEF VAR lcInput3 AS CHAR NO-UNDO.

        ASSIGN lcmodule = "" lcinput1 = "" lcinput2 = "" lcinput3 = "".

        
        IF AVAILABLE ttBrowser THEN DO:

           IF AVAIL Order THEN DO:
              
              FIND CURRENT Order NO-LOCK.

              /* YBU-1275/YTS-3277 */
              IF Order.StatusCode NE lcOrigStatusCode THEN DO:
                 MESSAGE "Order status was changed by other process. Try again"
                 VIEW-AS ALERT-BOX ERROR.
                 RETURN.
              END.
           END.
        
           lcModule = ENTRY(1,ttbrowser.ofmodule,",").
           
           IF lcModule = "Mc/prinoinf.p" THEN DO:
              RUN Mc/prinoinf.p (INPUT 0,iiOrderID,FALSE,OUTPUT lcError).           

              IF lcError > "" THEN MESSAGE
              lcerror
              VIEW-AS ALERT-BOX.
           END.
           ELSE IF lcModule = "Mc/orderhold.p" THEN DO:
              lcAction = "".
              lcAction = ENTRY(3,ttbrowser.ofmodule,",") NO-ERROR.
              RUN VALUE(lcModule) (iiOrderid, lcAction).

           END.
           ELSE IF lcModule = "Mc/credithold.p" THEN DO:
              llTrue = ilTrue =
                (STRING(ENTRY(3,ttbrowser.ofmodule,",")) = "TRUE").

              RUN VALUE(lcModule) (iiOrderid, llTrue).
           END.
           
           ELSE IF lcModule = "Eventsel" OR lcModule = "Mc/eventsel.p" THEN DO:
              RUN Mc/eventsel.p("Order", Order.Brand + CHR(255) + STRING(Order.OrderID)).
           END.

           ELSE IF lcModule = "nnasla" OR lcModule = "Mc/nnasla.p" THEN DO:
              RUN Mc/nnasla.p(0,Order.OrderID).
           END. 
           
           ELSE IF lcModule = "mnpbr" OR lcModule = "Mnp/mnpbr.p" THEN DO:
              RUN Mnp/mnpbr.p(Order.OrderId,0,0).
           END. 
           
           ELSE IF lcModule = "offer" OR lcModule = "Mc/offer.p" THEN DO:
              IF Order.Offer NE "" THEN RUN Mc/offer.p(Order.Offer,FALSE).
           END. 
           
           ELSE IF lcModule = "Mc/orderinctrl.p" THEN DO:
              IF TRIM(ENTRY(3,ttbrowser.ofmodule,",")) EQ "1"
              THEN RUN VALUE(lcModule) (iiOrderid, 1, FALSE).
              ELSE RUN VALUE(lcModule) (iiOrderid, 0, FALSE).
           END.
           
           ELSE IF lookup(lcModule,"Mc/closeorder.p,Mc/orderneeddoc.p") > 0 THEN DO:
              RUN VALUE(lcModule) (iiOrderid,FALSE).
           END. 

           ELSE IF lcModule = "Mc/orderbyfraud.p" THEN DO:
              RUN VALUE(lcModule) (iiOrderid,FALSE,{&ORDER_STATUS_CLOSED_BY_FRAUD}).
           END.

           ELSE IF Num-ENTRIES(ttbrowser.ofmodule,",") = 2 THEN DO:
              RUN VALUE(lcModule) (iiOrderid).
           END.

           ELSE IF lcModule = "Mc/dpmember.p" THEN DO:
              RUN VALUE(lcModule) (0,"MobSub",Order.MsSeq).
           END.

           ELSE IF lcModule = "dms" OR lcModule = "Mc/dms.p" THEN DO:
              RUN Mc/dms.p(Order.OrderId,Order.ContractID).
           END.

           ELSE 
           MESSAGE
           "Not use yet!"
           VIEW-AS ALERT-BOX.
           LEAVE LOOP.
        END.
     END.

     ELSE IF LOOKUP(nap,"HOME,H") > 0 THEN DO : /* FIRST record */
        RUN local-find-FIRST.
        ASSIGN Memory = ROWID(ttBrowser) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = ROWID(ttBrowser) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND ttBrowser WHERE ROWID(ttBrowser) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttBrowser WHERE ROWID(ttBrowser) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN 
      FIND FIRST ttBrowser USE-INDEX OFName NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN 
       FIND LAST ttBrowser USE-INDEX OFName NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF order = 1 THEN 
      FIND NEXT ttBrowser USE-INDEX OFName NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:

   IF order = 1 THEN 
      FIND PREV ttBrowser USE-INDEX OFName NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:

    RUN local-find-others.
       
    CLEAR FRAME sel NO-PAUSE.
    DISPLAY 
       
       ttBrowser.OFName
    WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

END PROCEDURE.


