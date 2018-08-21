/* ----------------------------------------------------------------------
  MODULE .......: mobilesubscription.P
  TASK .........: Mobile Line Details
  APPLICATION ..: TMS
  AUTHOR .......: srvuddan
  CREATED ......: 27-07-2018
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i} 
{Syst/tmsconst.i}
{Func/lib/accesslog.i}
{Mc/lib/tokenlib.i}

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
   
   {Func/lib/eventlog.i}
      
   DEFINE VARIABLE lhOrderMobile AS HANDLE NO-UNDO.
   lhOrderMobile = BUFFER OrderMobile:HANDLE.
   RUN StarEventInitialize(lhOrderMobile).
            
ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhOrderMobile).
   END.
END.

DEF INPUT PARAMETER iiOrderID AS INT  NO-UNDO.
DEF INPUT PARAMETER iiOrderProductID AS INT  NO-UNDO.

DEF /* NEW */ shared VAR siirto       AS CHAR.

DEF VAR OrderMobile LIKE OrderMobile.OrderProductID NO-UNDO.
DEF VAR xrecid       AS RECID init ?.
DEF VAR FIRSTrow     AS INT   NO-UNDO init 0.
DEF VAR FrmRow       AS INT   NO-UNDO init 1.
DEF VAR FrmDown      AS INT   NO-UNDO init 15.
DEF VAR order        AS INT   NO-UNDO init 1.
DEF VAR orders       AS CHAR  NO-UNDO.
DEF VAR maxOrder     AS INT   NO-UNDO init 2.
DEF VAR ufkey        AS LOG   NO-UNDO init TRUE.
DEF VAR delrow       AS INT   NO-UNDO init 0.
DEF VAR pr-order     AS INT   NO-UNDO.
DEF VAR Memory       AS RECID NO-UNDO.
DEF VAR RowNo        AS INT   NO-UNDO.
DEF VAR must-print   AS LOG   NO-UNDO.
DEF VAR must-add     AS LOG   NO-UNDO.
DEF VAR ac-hdr       AS CHAR  NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24 NO-UNDO.
DEF VAR i            AS INT   NO-UNDO.
DEF VAR ok           AS log   format "Yes/No" NO-UNDO.

DEF VAR lcHeader     AS CHAR  NO-UNDO.

DEF BUFFER bOrderMobile FOR OrderMobile.   

form
    OrderMobile.CreatedTS      COLUMN-LABEL "Created"
    OrderMobile.MsSeq
    OrderMobile.OrderProductID
    OrderMobile.ICC
    OrderMobile.NumberType
    OrderMobile.CLI
    WITH OVERLAY ROW FrmRow WIDTH 80 CENTERED SCROLL 1 FrmDown DOWN
    COLOR VALUE(Syst.Var:cfc)   
    TITLE COLOR VALUE(Syst.Var:ctc) " MOBILE SUBSCRIPTION OF ORDER :" + STRING(iiOrderID) + " " FRAME sel.

form
  OrderMobile.OrderID   
     LABEL "Order ID ......" 
  OrderMobile.OrderProductID AT 38
     LABEL "Product ID ...."  
     help "Order Product ID"     
     SKIP
     
  OrderMobile.MsSeq 
     LABEL "MsSeq ........." 
  OrderMobile.ICC AT 38
     LABEL "ICC ..........."
     SKIP
  
  OrderMobile.NumberType 
     LABEL "Number Type ..." 
     FORMAT "X(12)"    
  OrderMobile.Product AT 38
     LABEL "Order Product ." 
     SKIP

  OrderMobile.StatusCode 
     LABEL "StatusCode ...."
     HELP  "StatusCode" 
     FORMAT "X(12)"
  OrderMobile.CurrOper AT 38
     LABEL "Curr.Operator ." 
     SKIP
 
  OrderMobile.CreatedTS 
     LABEL "CreatedTS ....."
     FORMAT "99999999.99999"
  OrderMobile.CLI AT 38
     LABEL "CLI ..........."
     FORMAT "X(12)"
     SKIP
  
  OrderMobile.UpdatedTS 
     LABEL "UpdatedTS ....."
     FORMAT "99999999.99999"
  OrderMobile.ActivationTS AT 38
     LABEL "ActivationTS .."
     FORMAT "99999999.99999"
     SKIP
     
  OrderMobile.RequestedPortingDate
     LABEL "REQ.PortDate .."
     FORMAT "99-99-9999"
  OrderMobile.PortingTime AT 38
     LABEL "PortTime ......"
     FORMAT "99.99"
     SKIP    
     
  OrderMobile.MNPStatus 
     LABEL "MNP Status ...." 
     FORMAT "99"     
  OrderMobile.OldICC AT 38
     LABEL "Old ICC ......."
     FORMAT "X(12)"
     SKIP          
     
  OrderMobile.PayType       
     LABEL "PayType ......."
  
  WITH  CENTERED OVERLAY ROW 3 WIDTH 80 
  SIDE-LABELS TITLE COLOR VALUE(Syst.Var:ctc) ac-hdr
  FRAME lis.
  
{Mc/updateordcustomer.i}

IF iiOrderID > 0 THEN ASSIGN 
   order = 1
   maxorder = 1
   FrmRow = 3
   FrmDown = 10.
ELSE IF iiOrderProductID > 0 THEN ASSIGN
   order = 2
   maxorder = 1
   FrmRow = 3
   FrmDown = 10.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. 
ASSIGN 
    Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.

RUN local-find-first.
IF AVAILABLE OrderMobile THEN 
   ASSIGN
      Memory     = recid(OrderMobile)
      must-print = TRUE
      must-add   = FALSE.
ELSE DO:
   MESSAGE "Mobile Connection is Not Available !" VIEW-AS ALERT-BOX.
   ASSIGN
      Memory     = ?
      must-print = FALSE
      must-add   = FALSE.
END.

LOOP:
REPEAT WITH FRAME sel:
   IF order <> pr-order AND MaxOrder NE 1 THEN DO:
      pr-order = order.
   END.

   IF must-add THEN 
   DO:  /* Add a OrderMobile  */
       ASSIGN 
          Syst.Var:cfc = "lis" 
          ufkey        = true 
          ac-hdr       = " ADD " 
          must-add     = FALSE.
       RUN Syst/ufcolor.p.
       ADD-ROW:
       REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
           
          PAUSE 0 NO-MESSAGE.
          Syst.Var:ehto = 9. RUN Syst/ufkey.p.
            
          REPEAT TRANSACTION WITH FRAME lis
          ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW:
            
             CLEAR FRAME lis NO-PAUSE.

             PROMPT-FOR OrderMobile.OrderProductID
                VALIDATE(OrderMobile.OrderProductID NOT ENTERED OR
                NOT CAN-FIND(OrderMobile WHERE
                   OrderMobile.OrderProductID = INPUT FRAME lis OrderMobile.OrderProductID ),
                   "OrderMobiles " + string(INPUT OrderMobile.OrderProductID) +
                   " already exists !").
             IF INPUT OrderMobile.OrderProductID NOT ENTERED THEN 
             LEAVE add-row.
               
             CREATE OrderMobile.
             ASSIGN
                OrderMobile.OrderProductID = INPUT FRAME lis 
                                    OrderMobile.OrderProductID.

             RUN local-UPDATE-record.

             IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
                UNDO add-row, LEAVE add-row.

             IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhOrderMobile).
                ASSIGN
                   Memory = recid(OrderMobile)
                   xrecid = Memory.
             LEAVE.
          END.
       END.  /* ADD-ROW */
        
       HIDE FRAME lis NO-PAUSE.
       ASSIGN 
          must-print = TRUE.

       /* is there ANY record ? */
       FIND FIRST OrderMobile WHERE 
                  OrderMobile.OrderProductID = iiOrderProductID
                  NO-LOCK NO-ERROR.
       IF NOT AVAILABLE OrderMobile THEN LEAVE LOOP.
       NEXT LOOP.
    END.
    
PrintPage:
   DO:
      IF must-print THEN DO:
         UP FRAME-LINE - 1.
         FIND OrderMobile WHERE recid(OrderMobile) = Memory NO-LOCK NO-ERROR.

         /* DISPLAY one page beginning the record 
         whose RECID is saved into 'Memory'.
         starting from ROW 'delrow' */

         /* IF a ROW was recently DELETEd ... */
         IF delrow > 0 THEN DOWN delrow - 1.

         REPEAT WITH FRAME sel:
            IF AVAILABLE OrderMobile THEN DO:
               RUN local-disp-row.
               rtab[FRAME-LINE] = recid(OrderMobile).
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
         ASSIGN 
            FIRSTrow   = 0
            must-print = FALSE.
         PAUSE 0 NO-MESSAGE.

        /* Now there is one page DISPLAYed AND the cursor is on the
        upermost ROW, waiting FOR a 'choose' */
      END. /* must-print = TRUE */
   END. /* PrintPage */
    
   IF delrow > 0 THEN DOWN delrow - 1.
   ASSIGN 
      delrow = 0.

BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
         ASSIGN
            Syst.Var:ufk[1]= (IF CAN-FIND(FIRST OrderCustomer OF Order WHERE
                              RowType = {&ORDERCUSTOMER_ROWTYPE_MOBILE_POUSER})
                        THEN 2248 /* mobile donor (holder) */
                        ELSE 0) 
            Syst.Var:ufk[2]= 0 Syst.Var:ufk[3]= 0  Syst.Var:ufk[4]= 0
            Syst.Var:ufk[5]= 0 Syst.Var:ufk[6]= 0 Syst.Var:ufk[7]= 0 Syst.Var:ufk[8]= 8 Syst.Var:ufk[9]= 1
            Syst.Var:ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.
      
      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW OrderMobile.CreatedTS {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(Syst.Var:ccc) OrderMobile.CreatedTS WITH FRAME sel.
      END. 
     /* ELSE IF order = 2 THEN DO:
         CHOOSE ROW ttOrdProductParam.ParamName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(Syst.Var:ccc) ttOrdProductParam.ParamName WITH FRAME sel.
      END. */ 

      Syst.Var:nap = keylabel(LASTKEY).
    
      IF rtab[FRAME-LINE] = ? THEN DO:
         IF LOOKUP(Syst.Var:nap,"5,f5,8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.   
      END. 

      IF LOOKUP(Syst.Var:nap,"cursor-right") > 0 AND maxorder > 1 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(Syst.Var:nap,"cursor-left") > 0 AND maxorder > 1 THEN DO:
         order = order - 1. IF order = 0 THEN order = maxOrder.
      END.
      
      IF order <> pr-order AND MaxOrder > 1 THEN DO:
         ASSIGN 
            FIRSTrow = 0 
            Memory   = rtab[FRAME-LINE].
         FIND OrderMobile WHERE recid(OrderMobile) = Memory NO-LOCK.
         DO i = 1 TO FRAME-LINE - 1:
            RUN local-find-PREV.
            IF AVAILABLE OrderMobile THEN
               ASSIGN FIRSTrow = i Memory   = recid(OrderMobile).
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
            IF NOT AVAILABLE OrderMobile THEN DO:
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
                  rtab[1] = recid(OrderMobile)
                  Memory  = rtab[1].
            END.
        END.
         ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(Syst.Var:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND OrderMobile WHERE RECID(OrderMobile) = rtab[FRAME-DOWN] NO-LOCK .
            RUN local-find-NEXT.
            IF NOT AVAILABLE OrderMobile THEN DO:
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
               rtab[FRAME-DOWN] = recid(OrderMobile).
               /* save RECID of uppermost ROW */
               Memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT ROW */
      
      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
         Memory = rtab[1].
         FIND OrderMobile WHERE recid(OrderMobile) = Memory NO-LOCK NO-ERROR.
         RUN local-find-PREV.
         IF AVAILABLE OrderMobile THEN DO:
            Memory = recid(OrderMobile).

           /* reverse 1 page */
            DO RowNo = 1 TO (FRAME-DOWN - 1):
               RUN local-find-PREV.
               IF AVAILABLE OrderMobile THEN Memory = recid(OrderMobile).
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
            FIND OrderMobile WHERE recid(OrderMobile) = Memory NO-LOCK.
            must-print = TRUE.
            NEXT LOOP.
         END.
      END. /* NEXT page */
     
      ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 THEN DO WITH FRAME lis TRANSACTION:
       /* change */
         FIND FIRST OrderMobile where 
              recid(OrderMobile) = rtab[frame-line(sel)]
         no-lock.
         assign ac-hdr = " VIEW " ufkey = TRUE Syst.Var:ehto = 9.

         Syst.Var:cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.

         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOrderMobile).

         RUN local-disp-lis.

         IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
         KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.
       
         RUN local-disp-row.
         
         xrecid = recid(OrderMobile).
         LEAVE.
      END.

      ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
         RUN local-find-FIRST.
         ASSIGN Memory = recid(OrderMobile) must-print = TRUE.
         NEXT LOOP.
      END.

      ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO: /* LAST record */
         RUN local-find-LAST.
         ASSIGN Memory = recid(OrderMobile) must-print = TRUE.
         NEXT LOOP.
      END.

      ELSE IF LOOKUP(Syst.Var:nap,"1,f1") > 0 THEN DO: 
         RUN local-disp-customer({&ORDERCUSTOMER_ROWTYPE_MOBILE_POUSER},FALSE).
         ufkey = TRUE.  
         NEXT loop.
      END.
      
      
      ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.

PROCEDURE local-find-this:
   DEF INPUT PARAMETER exlock AS lo NO-UNDO.

   IF exlock THEN
      FIND OrderMobile WHERE recid(OrderMobile) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK NO-ERROR.
    ELSE
       FIND OrderMobile WHERE recid(OrderMobile) = rtab[frame-line(sel)] 
       NO-LOCK NO-ERROR.
END PROCEDURE.
        
PROCEDURE local-find-FIRST:
   IF order = 1 THEN DO:
      IF iiOrderID > 0 AND iiOrderProductID > 0 THEN 
         FIND FIRST OrderMobile WHERE 
                    OrderMobile.OrderID = iiOrderID AND
                    OrderMobile.OrderProductID = iiOrderProductID
                    NO-LOCK NO-ERROR.
      ELSE
         FIND FIRST OrderMobile USE-INDEX OrderProductID NO-LOCK NO-ERROR.
   END.         
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN DO:
      IF iiOrderID > 0 AND iiOrderProductID > 0 THEN 
         FIND LAST OrderMobile WHERE 
                   OrderMobile.OrderID = iiOrderID AND
                   OrderMobile.OrderProductID = iiOrderProductID
                   NO-LOCK NO-ERROR.
      ELSE
         FIND LAST OrderMobile USE-INDEX OrderProductID NO-LOCK NO-ERROR.
   END.    
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN DO:
      IF iiOrderID > 0 AND iiOrderProductID > 0 THEN 
         FIND NEXT OrderMobile WHERE 
                   OrderMobile.OrderID = iiOrderID AND
                   OrderMobile.OrderProductID = iiOrderProductID
                   NO-LOCK NO-ERROR.
      ELSE
         FIND NEXT OrderMobile USE-INDEX OrderProductID NO-LOCK NO-ERROR.
   END.            
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN DO:
      IF iiOrderID > 0 AND iiOrderProductID > 0 THEN 
         FIND PREV OrderMobile WHERE 
                   OrderMobile.OrderID = iiOrderID AND
                   OrderMobile.OrderProductID = iiOrderProductID
                   NO-LOCK NO-ERROR.
      ELSE
         FIND PREV OrderMobile USE-INDEX OrderProductID NO-LOCK NO-ERROR.
   END.     
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY OrderMobile.CreatedTS      
               OrderMobile.MsSeq
               OrderMobile.OrderProductID
               OrderMobile.ICC
               OrderMobile.NumberType
               OrderMobile.CLI   
               WITH FRAME sel.  
END PROCEDURE.

PROCEDURE local-find-others:
   FIND FIRST Order WHERE 
              Order.Brand   = Syst.Var:gcBrand  AND 
              Order.OrderId = iiOrderID NO-LOCK NO-ERROR.

   FIND FIRST OrderCustomer WHERE 
              OrderCustomer.Brand   = Syst.Var:gcBrand  AND 
              OrderCustomer.OrderId = iiOrderID NO-LOCK NO-ERROR.
    
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   
   DEF INPUT PARAMETER ilNew AS LOG NO-UNDO.
   
   MaintMenu:
   REPEAT ON ENDKEY UNDO, LEAVE:
       
      DISPLAY 
         OrderMobile.OrderID          
         OrderMobile.OrderProductID   
         OrderMobile.MsSeq            
         OrderMobile.ICC              
         OrderMobile.Product          
         OrderMobile.NumberType      
         OrderMobile.CurrOper         
         OrderMobile.StatusCode       
         OrderMobile.CreatedTS        
         OrderMobile.UpdatedTS        
         OrderMobile.CLI              
         OrderMobile.ActivationTS      
         OrderMobile.RequestedPortingDate  
         OrderMobile.PortingTime      
         OrderMobile.MNPStatus        
         OrderMobile.OldICC           
         OrderMobile.PayType   
         WITH FRAME lis.
         
      IF ilNew THEN Syst.Var:toimi = 1.
      
      ELSE ASSIGN 
         Syst.Var:ehto   = 0
         Syst.Var:ufk    = 0
         Syst.Var:ufk[1] = 7 
         Syst.Var:ufk[8] = 8.
      RUN Syst/ufkey.p.  
      
      IF Syst.Var:toimi = 1 THEN DO:
         RUN pUpdate(ilNew).
         IF RETURN-VALUE BEGINS "UNDO" THEN UNDO, LEAVE MaintMenu.
         IF ilNew THEN LEAVE MaintMenu.
      END.   
      
      ELSE IF Syst.Var:toimi = 8 THEN LEAVE MaintMenu. 
      END.

   HIDE FRAME lis NO-PAUSE.

END PROCEDURE.

PROCEDURE local-disp-lis:
      
   RUN local-find-others.
   DISP
      OrderMobile.OrderID          
      OrderMobile.OrderProductID   
      OrderMobile.MsSeq            
      OrderMobile.ICC              
      OrderMobile.Product          
      OrderMobile.NumberType       
      OrderMobile.CurrOper         
      OrderMobile.StatusCode       
      OrderMobile.CreatedTS        
      OrderMobile.UpdatedTS        
      OrderMobile.CLI              
      OrderMobile.ActivationTS      
      OrderMobile.RequestedPortingDate  
      OrderMobile.PortingTime      
      OrderMobile.MNPStatus        
      OrderMobile.OldICC           
      OrderMobile.PayType   
      WITH FRAME lis.
      
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
 
      FIND CURRENT OrderMobile EXCLUSIVE-LOCK.
            
      Syst.Var:ehto = 9.
      RUN Syst/ufkey.p. 
      
      UPDATE
         OrderMobile.UpdatedTS        
         OrderMobile.CLI              
         OrderMobile.ActivationTS      
         OrderMobile.RequestedPortingDate  
         OrderMobile.PortingTime         
         WITH FRAME lis EDITING: 
      
      READKEY.
      Syst.Var:nap = KEYLABEL(LASTKEY). 

      IF lookup(Syst.Var:nap,Syst.Var:poisnap) > 0 THEN DO:
         if keylabel(lastkey) = "F4" THEN LEAVE . 
      
      END.
      
         APPLY LASTKEY. 
      END.  /* editing */

      LEAVE.
   END.  

   HIDE FRAME lis NO-PAUSE.
    
   IF llUndo THEN RETURN "UNDO".
   ELSE RETURN "". 
 
END PROCEDURE.