/* ----------------------------------------------------------------------
  MODULE .......: orderproduct.P
  TASK .........: ORDER PRODUCTS
  APPLICATION ..: TMS
  AUTHOR .......: srvuddan
  CREATED ......: 23-07-2018
  ---------------------------------------------------------------------- */
{Syst/commali.i}
{Syst/eventval.i} 
{Syst/tmsconst.i}
{Func/lib/accesslog.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'OrderProduct'}

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
   
   {Func/lib/eventlog.i}
      
   DEFINE VARIABLE lhOrderProduct AS HANDLE NO-UNDO.
   lhOrderProduct = BUFFER OrderProduct:HANDLE.
   RUN StarEventInitialize(lhOrderProduct).
   
ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhOrderProduct).
   END.
   
END.

DEF INPUT PARAMETER iiOrderID AS INT  NO-UNDO.

DEF /* NEW */ shared VAR siirto       AS CHAR.

DEF VAR OrderProduct LIKE OrderProduct.ProductID NO-UNDO.
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

DEF VAR lcStatus     AS CHAR                   NO-UNDO.

form
    OrderProduct.OrderProductID    FORMAT ">>>>>9" COLUMN-LABEL "OrderProductID"
    OrderProduct.ProductID         FORMAT "X(12)"
    OrderProduct.ParentID          FORMAT ">>>9" 
    OrderProduct.ActionType        FORMAT "X(20)"
    lcStatus                       FORMAT "X(14)" COLUMN-LABEL "Status"
    WITH OVERLAY ROW FrmRow WIDTH 80 CENTERED FrmDown DOWN
    COLOR VALUE(Syst.Var:cfc)   
    TITLE COLOR VALUE(Syst.Var:ctc) " PRODUCTS OF ORDER:" + STRING(iiOrderID) + " " FRAME sel.
    
form
    OrderProduct.OrderID           COLON 20
    OrderProduct.OrderProductID    COLON 20 FORMAT ">>>>>>>9" COLUMN-LABEL "ProductID"
    OrderProduct.ProductID         COLON 20 FORMAT "X(20)"
    OrderProduct.ProductOfferingID COLON 20 FORMAT "X(24)" 
    OrderProduct.ActionType        COLON 20 FORMAT "X(24)"
    OrderProduct.ParentID          COLON 20 FORMAT ">>>>>>>>9"
    OrderProduct.ITGroupID         COLON 20 FORMAT ">>>>>>>>>9"
    OrderProduct.StatusCode        COLON 20 FORMAT "X(2)" "/" lcStatus FORMAT "X(15)" NO-LABEL      
    WITH  OVERLAY ROW 4 CENTERED 
    COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.
    
{Mc/updateordcustomer.i}
    
IF iiOrderID > 0 THEN ASSIGN 
   order = 1
   maxorder = 1
   FrmRow = 3
   FrmDown = 10.
       
Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. 
ASSIGN 
   Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.

RUN local-find-first.

IF AVAILABLE OrderProduct THEN ASSIGN
        Memory     = recid(OrderProduct)
        must-print = TRUE
        must-add   = FALSE.
ELSE DO: 
MESSAGE "No Order Products available !" VIEW-AS ALERT-BOX.
ASSIGN
        Memory     = ?
        must-print = FALSE
        must-add   = FALSE.
END.

FUNCTION fStatusText RETURNS CHAR
   (iiStatusCode AS CHAR):
   FIND FIRST TMSCodes WHERE
              TMSCodes.TableName = "Order" AND
              TMSCodes.FieldName = "StatusCode" AND
              TMSCodes.CodeGroup = "Orders" AND
              TMSCodes.CodeValue = OrderProduct.StatusCode
   NO-LOCK NO-ERROR.
   IF AVAIL TMSCodes THEN RETURN TMSCodes.CodeName.

   RETURN "".
END.


LOOP:
REPEAT WITH FRAME sel:

   IF order <> pr-order AND MaxOrder NE 1 THEN 
   DO:
      pr-order = order.
   END.

   IF must-add THEN 
   DO:  /* Add a OrderProduct  */
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
              
              PROMPT-FOR OrderProduct.OrderProductID
                 VALIDATE(OrderProduct.OrderProductID = "" OR
                 NOT can-find(OrderProduct WHERE 
                 OrderProduct.OrderProductID = INPUT FRAME lis OrderProduct.OrderProductID),
                 "OrderProducts " + string(INPUT OrderProduct.OrderProductID) +
                 " already exists !").
              IF INPUT OrderProduct.OrderProductID NOT ENTERED THEN 
                 LEAVE add-row.
               
              CREATE OrderProduct.
              ASSIGN
                 OrderProduct.OrderProductID = INPUT FRAME lis 
                                         OrderProduct.OrderProductID.

              RUN local-UPDATE-record.

              IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
                 UNDO add-row, LEAVE add-row.

              IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhOrderProduct).
              
              ASSIGN
                 Memory = recid(OrderProduct)
                 xrecid = Memory.
              LEAVE.
           END.
        END.  /* ADD-ROW */
        
        HIDE FRAME lis NO-PAUSE.
        ASSIGN 
           must-print = TRUE.

        /* is there ANY record ? */
        FIND FIRST OrderProduct WHERE 
                   OrderProduct.OrderID = iiOrderID
       /* srule */ NO-LOCK NO-ERROR.
        IF NOT AVAILABLE OrderProduct THEN LEAVE LOOP.
        NEXT LOOP.
    END.
    
    PrintPage:
    DO:
       IF must-print THEN 
       DO:
          UP FRAME-LINE - 1.
          FIND OrderProduct WHERE recid(OrderProduct) = Memory NO-LOCK NO-ERROR.

          /* DISPLAY one page beginning the record 
          whose RECID is saved into 'Memory'.
          starting from ROW 'delrow' */

         /* IF a ROW was recently DELETEd ... */
          IF delrow > 0 THEN DOWN delrow - 1.

          REPEAT WITH FRAME sel:
             IF AVAILABLE OrderProduct THEN 
                DO:
                   RUN local-disp-row.
                   rtab[FRAME-LINE] = recid(OrderProduct).
                   RUN local-find-NEXT.
                END.
             ELSE 
             DO:
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


RUN local-find-this(FALSE).

BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
         ASSIGN
            Syst.Var:ufk[1]= 0 
            Syst.Var:ufk[2]= 0 
            Syst.Var:ufk[3]= 0
            Syst.Var:ufk[4]= 0
            Syst.Var:ufk[5]= 0 
            Syst.Var:ufk[6]= 0 
            Syst.Var:ufk[7]= 0
            Syst.Var:ufk[8]= 8 Syst.Var:ufk[9]= 1
            Syst.Var:ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.

      END.
      
      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW OrderProduct.OrderProductID {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(Syst.Var:ccc) OrderProduct.OrderProductID WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW OrderProduct.ProductID {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(Syst.Var:ccc) OrderProduct.ProductID WITH FRAME sel.
      END.  

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
         FIND OrderProduct WHERE recid(OrderProduct) = Memory NO-LOCK.
         DO i = 1 TO FRAME-LINE - 1:
            RUN local-find-PREV.
            IF AVAILABLE OrderProduct THEN
               ASSIGN FIRSTrow = i Memory   = recid(OrderProduct).
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
            IF NOT AVAILABLE OrderProduct THEN DO:
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
                  rtab[1] = recid(OrderProduct)
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
            IF NOT AVAILABLE OrderProduct THEN DO:
               
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
               rtab[FRAME-DOWN] = recid(OrderProduct).
               /* save RECID of uppermost ROW */
               Memory = rtab[1].
               
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT ROW */
      
      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
         Memory = rtab[1].
         FIND OrderProduct WHERE recid(OrderProduct) = Memory NO-LOCK NO-ERROR.
         RUN local-find-PREV.
         IF AVAILABLE OrderProduct THEN DO:
            Memory = recid(OrderProduct).

            /* reverse 1 page */
            DO RowNo = 1 TO (FRAME-DOWN - 1):
               RUN local-find-PREV.
               IF AVAILABLE OrderProduct THEN Memory = recid(OrderProduct).
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
            FIND OrderProduct WHERE recid(OrderProduct) = Memory NO-LOCK.
            must-print = TRUE.
            NEXT LOOP.
         END.
      END. /* NEXT page */
     
      ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 THEN DO:
         RUN local-find-this(FALSE).
         RUN pOrderProductView.
         NEXT LOOP.
      END.
      
      ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
         RUN local-find-FIRST.
         ASSIGN Memory = recid(OrderProduct) must-print = TRUE.
         NEXT LOOP.
      END.

      ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
         RUN local-find-LAST.
         ASSIGN Memory = recid(OrderProduct) must-print = TRUE.
         NEXT LOOP.
      END.

      ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.

PROCEDURE pOrderProductView:
   
   PAUSE 0.

   RUN local-disp-lis.

   ACTION: 
   repeat with frame lis:

      ASSIGN
         Syst.Var:ufk[1]= 9861 
         Syst.Var:ufk[2]= (IF CAN-FIND(FIRST OrderMobile WHERE
                            OrderMobile.OrderID = OrderProduct.OrderID AND
                            OrderMobile.OrderProductID = OrderProduct.OrderProductID)
                            THEN 9862 /* Order Mobile */
                            ELSE 0) 
         Syst.Var:ufk[3]= (IF CAN-FIND(FIRST OrderFusion WHERE
                              OrderFusion.OrderID = OrderProduct.OrderID AND
                              OrderFusion.OrderProductID = OrderProduct.OrderProductID)
                            THEN 9863 /* Order Fusion */
                            ELSE 0)  
         Syst.Var:ufk[4]= (IF CAN-FIND(FIRST InvoiceTargetGroup WHERE
                              InvoiceTargetGroup.ITGroupID = OrderProduct.ITGroupID)
                        THEN 9865 /* Billing Account */
                        ELSE 0) 
         Syst.Var:ufk[5]= (IF CAN-FIND(FIRST OrderCustomer OF Order WHERE
                           RowType = {&ORDERCUSTOMER_ROWTYPE_ACC})
                        THEN 2247 /* Target/ACC/User customer */
                        ELSE 0)  
         Syst.Var:ufk[6]= (IF CAN-FIND(FIRST OrderCustomer OF Order WHERE
                           RowType = {&ORDERCUSTOMER_ROWTYPE_DELIVERY})
                        THEN 1071 /* delivery address */
                        ELSE 0) 
         Syst.Var:ufk[7]= (IF CAN-FIND(FIRST OrderCustomer OF Order WHERE 
                           RowType = {&ORDERCUSTOMER_ROWTYPE_LOGISTICS})
                        THEN 9844 
                        ELSE 0)
         Syst.Var:ufk[8]= 8 
         Syst.Var:ufk[9]= 1
         Syst.Var:ehto = 0 
   
         ufkey = true.
         RUN Syst/ufkey.p.
  
      IF Syst.Var:toimi = 8 then do:
         hide frame lis.
         leave.
      end.
      
      ELSE IF Syst.Var:toimi = 1 THEN DO:
         FIND OrderProduct where recid(OrderProduct) = rtab[FRAME-LINE] NO-LOCK.
         RUN Mc/orderproductparam.p(iiOrderID,OrderProduct.OrderProductID).
         NEXT ACTION.
      END.
      
      /* Order Mobile */
      ELSE IF Syst.Var:toimi = 2 THEN DO:
         FIND OrderProduct where recid(OrderProduct) = rtab[FRAME-LINE] NO-LOCK.  
         RUN local-disp-ordermobile(iiOrderID,OrderProduct.OrderProductID).  
         NEXT Action. 
      END. 
      
      ELSE IF Syst.Var:toimi = 3 THEN DO:
         FIND OrderProduct where recid(OrderProduct) = rtab[FRAME-LINE] NO-LOCK.
         RUN local-disp-orderfusion(iiOrderID,OrderProduct.OrderProductID).
         NEXT ACTION.
      END. 
      
      ELSE IF Syst.Var:toimi = 4 THEN DO:
         RUN Mc/invoicetargetgroup.p(0,OrderProduct.ITGroupID).
         NEXT ACTION.
      END.
      
      ELSE IF Syst.Var:toimi = 5 THEN DO:
         RUN local-disp-customer({&ORDERCUSTOMER_ROWTYPE_ACC},FALSE).
         NEXT ACTION.
      END.
      
      ELSE IF Syst.Var:toimi = 6 THEN DO:
         RUN local-disp-customer({&ORDERCUSTOMER_ROWTYPE_DELIVERY},FALSE).
         NEXT ACTION.
      END.
      
      /* logistic address */
      ELSE IF Syst.Var:toimi = 7 THEN DO:
         RUN local-disp-customer({&ORDERCUSTOMER_ROWTYPE_LOGISTICS},FALSE).
         NEXT ACTION.
      END. 
      
      RUN local-disp-row.
      xrecid = recid(Order).
      LEAVE.
   END.  /* action */

END PROCEDURE.
    
PROCEDURE local-find-this:
   DEF INPUT PARAMETER exlock AS lo NO-UNDO.

   IF exlock THEN
      FIND OrderProduct WHERE recid(OrderProduct) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
   ELSE
      FIND OrderProduct WHERE recid(OrderProduct) = rtab[frame-line(sel)] 
      NO-LOCK.
END PROCEDURE.
        
PROCEDURE local-find-FIRST:
   FIND FIRST OrderProduct WHERE
              OrderProduct.OrderID = iiOrderID
              NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN FIND LAST OrderProduct WHERE
      OrderProduct.OrderID = iiOrderID
      NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN FIND NEXT OrderProduct WHERE
      OrderProduct.OrderID = iiOrderID
      NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN FIND PREV OrderProduct WHERE
      OrderProduct.OrderID = iiOrderID
      NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
   RUN local-find-others.
   CLEAR FRAME sel NO-PAUSE.
   DISPLAY 
      OrderProduct.OrderProductID    
      OrderProduct.ProductID         
      OrderProduct.ParentID           
      OrderProduct.ActionType        
      lcStatus  
      WITH FRAME sel.       
            
END PROCEDURE.

PROCEDURE local-find-others:
   FIND FIRST Order WHERE 
              Order.Brand   = Syst.Var:gcBrand  AND 
              Order.OrderId = iiOrderID NO-LOCK NO-ERROR.
              
   FIND FIRST OrderMobile WHERE
              OrderMobile.OrderID = OrderProduct.OrderID AND
              OrderMobile.OrderProductID = OrderProduct.OrderProductID
              NO-LOCK NO-ERROR.
              
   lcStatus = fStatusText(OrderProduct.StatusCode).           
    
END PROCEDURE.

PROCEDURE local-disp-lis:
   
   RUN local-find-others.
   CLEAR FRAME lis NO-PAUSE.
   DISPLAY
      OrderProduct.OrderID           
      OrderProduct.OrderProductID    
      OrderProduct.ProductID         
      OrderProduct.ProductOfferingID  
      OrderProduct.ActionType        
      OrderProduct.ParentID          
      OrderProduct.ITGroupID         
      OrderProduct.StatusCode 
      lcStatus
      with frame lis. 
    
END PROCEDURE.    

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISPLAY
         OrderProduct.OrderID           
         OrderProduct.OrderProductID    
         OrderProduct.ProductID         
         OrderProduct.ProductOfferingID  
         OrderProduct.ActionType        
         OrderProduct.ParentID          
         OrderProduct.ITGroupID         
         OrderProduct.StatusCode 
         lcStatus
         with frame lis. 
      
      UPDATE
         OrderProduct.OrderID           
         OrderProduct.OrderProductID    
         OrderProduct.ProductID         
         OrderProduct.ProductOfferingID  
         OrderProduct.ActionType        
         OrderProduct.ParentID          
         OrderProduct.ITGroupID         
         OrderProduct.StatusCode 
         with frame lis. 
      LEAVE.
   END.
END PROCEDURE.
PROCEDURE local-disp-ordermobile:
   
   DEF INPUT PARAMETER iiOrderID AS INT NO-UNDO.
   DEF INPUT PARAMETER iiOrdProductID AS INT NO-UNDO.
   
   FIND FIRST OrderMobile WHERE 
              OrderMobile.OrderID = iiOrderID AND
              OrderMobile.OrderProductID = iiOrdProductID
              NO-LOCK NO-ERROR.
   IF NOT AVAILABLE OrderMobile THEN DO:
      MESSAGE "Order Mobile is not available"
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END. 
   
   ACTION: 
   repeat with frame fOrdMobile:
                    
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
      WITH FRAME fOrdMobile.    
      
   ASSIGN
      Syst.Var:ufk = 0
      Syst.Var:ufk[1] = (IF CAN-FIND(FIRST OrderCustomer OF Order WHERE
                              RowType = {&ORDERCUSTOMER_ROWTYPE_MOBILE_POUSER})
                        THEN 2248 /* mobile donor (holder) */
                        ELSE 0) 
      Syst.Var:ufk[5] = 0
      Syst.Var:ufk[8] = 8
      Syst.Var:ehto = 0
      ufkey = true.
   RUN Syst/ufkey.p.
                                                             
   IF Syst.Var:toimi = 8 then do:
      hide frame fOrdMobile.
      LEAVE ACTION.
   END.
   ELSE IF Syst.Var:toimi = 1 THEN 
      RUN local-disp-customer({&ORDERCUSTOMER_ROWTYPE_MOBILE_POUSER},FALSE).

   END. 

   HIDE FRAME fOrdMobile no-pause.
   view frame lis.

END PROCEDURE. 

PROCEDURE local-disp-orderfusion:
      
   DEF INPUT PARAMETER iiOrderID AS INT NO-UNDO.
   DEF INPUT PARAMETER iiOrdProductID AS INT NO-UNDO.
   
   FIND FIRST OrderFusion WHERE 
              OrderFusion.OrderID = iiOrderID AND
              OrderFusion.OrderProductID = iiOrdProductID
              NO-LOCK NO-ERROR.
   IF NOT AVAILABLE OrderFusion THEN DO:
      MESSAGE "Order Mobile is not available"
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END. 
   
   ACTION: 
   repeat with frame fOrdFusion:
                    
   DISPLAY 
         OrderFusion.Brand            
         OrderFusion.OrderID          
         OrderFusion.OrderProductID   
         OrderFusion.FixedNumberType            
         OrderFusion.FusionStatus              
         OrderFusion.Product          
         OrderFusion.FixedNumber      
         OrderFusion.FixedCurrOper         
         OrderFusion.FixedOrderId       
         OrderFusion.FixedStatus        
         OrderFusion.FixedSubStatus        
         OrderFusion.OrderDate              
         OrderFusion.Salesman      
         OrderFusion.FixedMNPTime  
         OrderFusion.CustomerType      
         OrderFusion.PhoneBook        
         OrderFusion.FixedContractID           
         OrderFusion.EstimatedDataSpeed
         OrderFusion.FixedInstallationTS
         OrderFusion.ADSLLinkState
         OrderFusion.FixedStatusTS
         OrderFusion.portStat
         OrderFusion.portDate
         OrderFusion.routerStat
         OrderFusion.IUA
         OrderFusion.SerialNumber
         OrderFusion.AppointmentDate
         WITH FRAME fOrdFusion.   
      
   ASSIGN
      Syst.Var:ufk = 0
      Syst.Var:ufk[1] = (IF CAN-FIND(FIRST OrderCustomer OF Order WHERE
                              RowType = {&ORDERCUSTOMER_ROWTYPE_FIXED_POUSER})
                        THEN 2249 /* fixed donor (holder) */
                        ELSE 0) 
      Syst.Var:ufk[5] = 0
      Syst.Var:ufk[8] = 8
      Syst.Var:ehto = 0
      ufkey = true.
   RUN Syst/ufkey.p.
                                                             
   IF Syst.Var:toimi = 8 then do:
      hide frame fOrdFusion.
      LEAVE ACTION.
   END.
   ELSE IF Syst.Var:toimi = 1 THEN 
      RUN local-disp-customer({&ORDERCUSTOMER_ROWTYPE_FIXED_POUSER},FALSE).
   
   END. 

   HIDE FRAME fOrdFusion no-pause.
   view frame lis.

END PROCEDURE.       
