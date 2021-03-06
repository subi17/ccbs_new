/* ----------------------------------------------------------------------
  MODULE .......: OrderAccessory
  TASK .........: UPDATEs table OrderAccessory
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 24.11.06
  CHANGED ......: 28.08.07/aam Discount
  Version ......: yoigo
  ---------------------------------------------------------------------- */
{Syst/commali.i}

{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'OrderAccessory'}


IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhOrderAccessory AS HANDLE NO-UNDO.
   lhOrderAccessory = BUFFER OrderAccessory:HANDLE.
   RUN StarEventInitialize(lhOrderAccessory).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhOrderAccessory).
   END.

END.

DEF INPUT PARAMETER iiOrderID AS INT  NO-UNDO.
DEF INPUT PARAMETER iiCustNum AS INT  NO-UNDO.


DEF VAR lcBillItem    AS CHAR                   NO-UNDO.
DEF VAR lcRgName     AS CHAR                   NO-UNDO.
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
DEF VAR Memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR lcBIName     AS CHAR                   NO-UNDO.
DEF VAR lcHeader     AS CHAR                   NO-UNDO.

DEF TEMP-TABLE ttAccessory NO-UNDO LIKE OrderAccessory  
   FIELD DbRec       AS INT 
   INDEX ProductCode ProductCode
   INDEX OrderID OrderID.

DEF BUFFER bttAccessory FOR ttAccessory.
    
form
    ttAccessory.ProductCode  FORMAT "X(14)" COLUMN-LABEL "Bill.Item"
    lcBIName             FORMAT "X(18)" COLUMN-LABEL "Name"
    ttAccessory.IMEI     FORMAT "X(16)"
    ttAccessory.Amount   FORMAT "->>>>9.99"
    ttAccessory.Discount FORMAT "->>>9.99" 
    ttAccessory.OrderID FORMAT ">>>>>>>9" COLUMN-LABEL "Order"
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(Syst.Var:cfc)   
    TITLE COLOR VALUE(Syst.Var:ctc) lcHeader FRAME sel.

form
    ttAccessory.OrderID      COLON 22 FORMAT ">>>>>>>9" 
    ttAccessory.ProductCode  COLON 22
       FORMAT "X(16)"
       VALIDATE(CAN-FIND(BillItem WHERE 
                         BillItem.Brand   = Syst.Var:gcBrand AND
                         BillItem.BillCode = INPUT ttAccessory.ProductCode),
                "Unknown billing item")
    lcBIName      
       FORMAT "X(30)" 
       NO-LABEL
    ttAccessory.IMEI          COLON 22 FORMAT "X(20)"
    ttAccessory.Amount        COLON 22 FORMAT "->>>>>9.99"
    ttAccessory.VatAmount     COLON 22 FORMAT "->>>>>9.99"
    ttAccessory.Discount      COLON 22 FORMAT "->>>>>9.99"
    ttAccessory.HardBook      COLON 22 FORMAT "9"
    ttAccessory.HardBookState COLON 22 FORMAT "X(20)" 
WITH  OVERLAY ROW 6 CENTERED
    COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form /* seek  ttAccessory */
    "Billing Item:" lcBillItem FORMAT "x(12)"
    HELP "Enter billing item"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND Billing Item "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f1.


FUNCTION fBIName RETURNS LOGIC
   (icBillItem AS CHAR):
   
   lcBIName = "".
   
   FIND BillItem WHERE   
        BillItem.Brand   = Syst.Var:gcBrand AND
        BillItem.BillCode = icBillItem NO-LOCK NO-ERROR.
   IF AVAILABLE BillItem THEN lcBIName = BillItem.BIName. 
   
END FUNCTION.


/* collect accessories to temp-table */
IF iiCustNum > 0 THEN DO:
   FOR EACH OrderCustomer NO-LOCK WHERE
            OrderCustomer.CustNum = iiCustNum,
       EACH OrderAccessory NO-LOCK WHERE
            OrderAccessory.Brand   = Syst.Var:gcBrand AND
            OrderAccessory.OrderID = OrderCustomer.OrderID:
            
      IF CAN-FIND(FIRST ttAccessory WHERE
                        ttAccessory.OrderID = OrderAccessory.OrderID AND
                        ttAccessory.IMEI    = OrderAccessory.IMEI)
      THEN NEXT.
      
      CREATE ttAccessory.
      BUFFER-COPY OrderAccessory TO ttAccessory.
      ttAccessory.DbRec = RECID(OrderAccessory).
   END.   

   lcHeader = " TERMINALS OF CUSTOMER " + STRING(iiCustNum) + " ".

END.

ELSE DO:
   FOR EACH OrderAccessory NO-LOCK WHERE
            OrderAccessory.Brand   = Syst.Var:gcBrand AND
            OrderAccessory.OrderID = iiOrderID:
      CREATE ttAccessory.
      BUFFER-COPY OrderAccessory TO ttAccessory.
      ttAccessory.DbRec = RECID(OrderAccessory).
   END.   
   
   lcHeader = " TERMINALS OF ORDER " + STRING(iiOrderID) + " ".

END.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.


RUN local-find-first.

IF AVAILABLE ttAccessory THEN ASSIGN
   Memory       = recid(ttAccessory)
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
    
   IF must-add THEN DO:  /* Add a ttAccessory  */
      ASSIGN Syst.Var:cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        Syst.Var:ehto = 9. RUN Syst/ufkey.p.

        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE:

           PROMPT-FOR ttAccessory.ProductCode WITH FRAME lis EDITING:
           
               READKEY. 
               Syst.Var:nap = KEYLABEL(LASTKEY).
               APPLY LASTKEY.
           END.

           IF INPUT FRAME lis ttAccessory.ProductCode = ""
           THEN LEAVE add-row.

           IF CAN-FIND(FIRST OrderAccessory WHERE
                       OrderAccessory.Brand   = Syst.Var:gcBrand   AND
                       OrderAccessory.OrderID = iiOrderID AND
                       OrderAccessory.ProductCode = 
                                   INPUT ttAccessory.ProductCode)
           THEN DO:
              MESSAGE "ttAccessory already exists with code"     
                      INPUT FRAME lis ttAccessory.ProductCode
              VIEW-AS ALERT-BOX ERROR.
              NEXT. 
           END.
           
           
           CREATE ttAccessory.
           ASSIGN ttAccessory.ProductCode = INPUT ttAccessory.ProductCode.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhOrderAccessory).

           ASSIGN
           Memory = recid(ttAccessory)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE ttAccessory THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ttAccessory WHERE recid(ttAccessory) = Memory 
          NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttAccessory THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttAccessory).
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
        Syst.Var:ufk    = 0
        Syst.Var:ufk[1] = 35
        Syst.Var:ufk[8] = 8 
        Syst.Var:ehto   = 3 
        ufkey  = FALSE.

        RUN Syst/ufkey.p.
        
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ttAccessory.ProductCode {Syst/uchoose.i} NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) ttAccessory.ProductCode WITH FRAME sel.
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
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND ttAccessory WHERE recid(ttAccessory) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttAccessory THEN
              ASSIGN FIRSTrow = i Memory = recid(ttAccessory).
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
           IF NOT AVAILABLE ttAccessory THEN DO:
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
                rtab[1] = recid(ttAccessory)
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
           IF NOT AVAILABLE ttAccessory THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttAccessory).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttAccessory WHERE recid(ttAccessory) = Memory
            NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttAccessory THEN DO:
           Memory = recid(ttAccessory).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttAccessory THEN Memory = recid(ttAccessory).
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
           FIND ttAccessory WHERE recid(ttAccessory) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(Syst.Var:nap,"1,f1") > 0 AND Syst.Var:ufk[1] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       UPDATE lcBillItem WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
                         
       IF lcBillItem > "" THEN DO:
       
          FIND FIRST ttAccessory WHERE 
                     ttAccessory.ProductCode >= lcBillItem
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE ttAccessory THEN DO:
             MESSAGE "Not found"
             VIEW-AS ALERT-BOX.
             NEXT BROWSE.
          END.

          ASSIGN Memory     = RECID(ttAccessory) 
                 must-print = TRUE
                 order      = 1.
          NEXT LOOP.

       END.
     END. /* Search-1 */


     ELSE IF LOOKUP(Syst.Var:nap,"5,f5") > 0 AND Syst.Var:ufk[5] > 0  
     THEN DO:  /* add */
        {Syst/uright2.i}
        must-add = TRUE.
        NEXT LOOP.
     END.
     
     ELSE IF LOOKUP(Syst.Var:nap,"6,f6") > 0 AND Syst.Var:ufk[6] > 0
     THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       COLOR DISPLAY VALUE(Syst.Var:ctc)
       ttAccessory.ProductCode ttAccessory.IMEI.

       RUN local-find-NEXT.
       IF AVAILABLE ttAccessory THEN Memory = recid(ttAccessory).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE ttAccessory THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(ttAccessory).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(Syst.Var:ccc)
       ttAccessory.ProductCode ttAccessory.IMEI.

       IF ok THEN DO:

           FIND OrderAccessory WHERE RECID(OrderAccessory) = 
                ttAccessory.DbRec EXCLUSIVE-LOCK.
           
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhOrderAccessory).

           DELETE ttAccessory.
           DELETE OrderAccessory.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE ttAccessory THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:

       /* change */
       RUN local-find-this((lcRight = "RW")).

       FIND OrderAccessory WHERE RECID(OrderAccessory) = 
               ttAccessory.DbRec NO-LOCK.
           
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOrderAccessory).

       ASSIGN ac-hdr = " TERMINAL " ufkey = TRUE.
       Syst.Var:cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrderAccessory).

       RUN local-disp-row.
       xrecid = recid(ttAccessory).
       LEAVE.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttAccessory) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttAccessory) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.

fCleanEventObjects().


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND ttAccessory WHERE recid(ttAccessory) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttAccessory WHERE recid(ttAccessory) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

       IF order = 1 THEN 
       FIND FIRST ttAccessory NO-ERROR.
         
END PROCEDURE.

PROCEDURE local-find-LAST:

       IF order = 1 THEN 
       FIND LAST ttAccessory NO-ERROR.
  
END PROCEDURE.

PROCEDURE local-find-NEXT:

       IF order = 1 THEN 
       FIND NEXT ttAccessory NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:
 
       IF order = 1 THEN 
       FIND PREV ttAccessory NO-ERROR.
    
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.

       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       ttAccessory.OrderID
       ttAccessory.ProductCode
       ttAccessory.IMEI
       lcBIName
       ttAccessory.Amount
       ttAccessory.Discount
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   fBIName(ttAccessory.ProductCode).
   IF lcBIName = "" AND ttAccessory.Model NE "" THEN DO:
      lcBINAme = ttAccessory.Manufacturer + " " + ttAccessory.Model + " " 
         + ttAccessory.ModelColor.
   END.
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:
   
      RUN local-find-others.

      CLEAR FRAME lis NO-PAUSE.
      
      DISP 
      ttAccessory.OrderID
      ttAccessory.ProductCode
      ttAccessory.IMEI
      lcBIName
      ttAccessory.Amount
      ttAccessory.VATAmount
      ttAccessory.Discount
      ttAccessory.HardBook
      ttAccessory.HardBookState
      WITH FRAME lis.
      
      IF lcRight = "RW" AND FALSE
      THEN REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:
      
         Syst.Var:ehto = 9. RUN Syst/ufkey.p.
      
         UPDATE
         ttAccessory.IMEI    
         ttAccessory.Amount
         ttAccessory.VATAmount
         WITH FRAME lis EDITING:
            
            READKEY.
 
            IF LOOKUP(KEYLABEL(LASTKEY),Syst.Var:poisnap) > 0 
            THEN DO WITH FRAME lis:
             
               PAUSE 0.
                
               IF FRAME-FIELD = "BillItem" THEN DO:
                  fBIName(INPUT INPUT FRAME lis ttAccessory.IMEI).
                  DISPLAY lcBIName WITH FRAME lis.
               END.

            END.
                             
            APPLY LASTKEY.
       
         END. /* EDITING */

         LEAVE. 
      END.
      
      ELSE DO:
         
         VIEW-LOOP:
         REPEAT ON ENDKEY UNDO, LEAVE:
       
           Syst.Var:ufk = 0.
           Syst.Var:ufk[6] = 1752.
           Syst.Var:ufk[8] = 8.
           Syst.Var:ehto = 1. RUN Syst/ufkey.p.
           
           IF Syst.Var:toimi = 6 THEN DO:
             
              FIND OrderAccessory WHERE RECID(OrderAccessory) = 
                     ttAccessory.DbRec NO-LOCK.
              RUN Mc/eventsel.p ("OrderAccessory",OrderAccessory.Brand +  CHR(255)
                  + STRING(OrderAccessory.OrderID)). 
              NEXT VIEW-LOOP.

           END.
           
           ELSE IF Syst.Var:toimi = 8 THEN DO:
              LEAVE VIEW-LOOP.
           END.
         
         END. 
         
         LEAVE.
      
      END.
   END.
   
END PROCEDURE.

