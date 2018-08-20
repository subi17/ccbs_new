/* ----------------------------------------------------------------------
  MODULE .......: fixedlinesubscription.P
  TASK .........: Fixed Line Details
  APPLICATION ..: TMS
  AUTHOR .......: srvuddan
  CREATED ......: 27-07-2018
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i} 

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
   
   {Func/lib/eventlog.i}
      
   DEFINE VARIABLE lhOrderFusion AS HANDLE NO-UNDO.
   lhOrderFusion = BUFFER OrderFusion:HANDLE.
   RUN StarEventInitialize(lhOrderFusion).
               
ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhOrderFusion).
   END.
END.

DEF INPUT PARAMETER iiOrderID AS INT  NO-UNDO.
DEF INPUT PARAMETER iiOrderProductID AS INT NO-UNDO.

DEF /* NEW */ shared VAR siirto       AS CHAR.

DEF VAR Orderid LIKE Order.OrderID NO-UNDO.
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
DEF VAR lcStamp      AS CHAR format "x(19)"    NO-UNDO.

DEF VAR lcHeader     AS CHAR  NO-UNDO.

DEF BUFFER bOrderFusion FOR OrderFusion.   

form
    lcStamp      COLUMN-LABEL "Created"
    OrderFusion.MsSeq
    OrderFusion.OrderProductID
    OrderFusion.Product
    OrderFusion.FixedNumberType
    WITH OVERLAY ROW FrmRow WIDTH 80 CENTERED SCROLL 1 FrmDown DOWN
    COLOR VALUE(Syst.Var:cfc)   
    TITLE COLOR VALUE(Syst.Var:ctc) " FIXEDLINE SUBSCRIPTION OF ORDER :" + STRING(iiOrderID) + " " FRAME sel.

form
  OrderFusion.Brand  
     LABEL "Brand ........."
  OrderFusion.OrderID   AT 46
     LABEL "Order ID ......" 
     SKIP

  OrderFusion.Product 
     LABEL "Order Product ."  
     help "Order Product "     
  OrderFusion.OrderProductID AT 46
     LABEL "Product ID ...." 
     SKIP

  OrderFusion.FusionStatus 
     LABEL "FusionStatus .."
     FORMAT "X(12)"
  OrderFusion.FixedNumberType AT 46
     LABEL "FixedN. Type .." 
     SKIP

  OrderFusion.FixedNumber 
     LABEL "Fix Number ...." 
     FORMAT "X(12)" 
  OrderFusion.FixedCurrOper AT 46
     LABEL "Curr.Operator ." 
     SKIP
 
  OrderFusion.FixedOrderId 
     LABEL "Fixed Order ID "
     FORMAT "X(12)"
  OrderFusion.OrderDate AT 46
     LABEL "Date .........."
     FORMAT "99-99-9999"
     SKIP
  
  OrderFusion.FixedStatus 
     LABEL "Fixed Status .."
     FORMAT "X(12)"
  OrderFusion.FixedSubStatus AT 46
     LABEL "FixedSubStatus "
     FORMAT "X(12)"
     SKIP
     
  OrderFusion.Salesman
     LABEL "Salesman ......"
     FORMAT "X(12)"
     SKIP
     
  OrderFusion.FixedMNPTime
     LABEL "FixL. MNPTime ."
     FORMAT "X(12)"
  OrderFusion.CustomerType AT 46
     LABEL "CustomerType .."
     FORMAT "X(12)"
     SKIP    
     
  OrderFusion.PhoneBook 
     LABEL "PhoneBook ....."
     FORMAT "YES/NO"     
  OrderFusion.FixedContractID AT 46
     LABEL "Contract ID ..."
     FORMAT "X(12)"
     SKIP          
  
  OrderFusion.CreatedTS
     LABEL "Created TS ...."
  OrderFusion.UpdateTS AT 46      
     LABEL "UpdateTS ......"
     FORMAT "99999999.99999"
     SKIP
     
  OrderFusion.FixedInstallationTS 
     LABEL "Installat. TS ."
     FORMAT "99999999.99999"
  OrderFusion.FixedStatusTS AT 46
     LABEL "Status TS ....."
     FORMAT "99999999.99999"
     SKIP
     
  OrderFusion.routerStat
     LABEL "Router Status ."
     FORMAT "X(10)"   
  OrderFusion.EstimatedDataSpeed AT 46
     LABEL "Data Speed....."
     FORMAT ">>>>>>>>9"
     SKIP
     
  OrderFusion.portStat
     LABEL "Port Status ..."
     FORMAT "X(15)"
  OrderFusion.portDate AT 46
     LABEL "Port Date ....."
     FORMAT "X(15)"
     SKIP   
     
  OrderFusion.ADSLLinkState
     LABEL "ADSL L.Status ."
     FORMAT "X(15)"
  OrderFusion.IUA AT 46
     LABEL "IUA ..........."
     FORMAT "X(14)"
     SKIP      
     
 OrderFusion.SerialNumber
     LABEL "Serial Number ."
     FORMAT "X(15)"
  OrderFusion.AppointmentDate AT 46
     LABEL "AppointmentDate"
     FORMAT "X(14)"
     SKIP(2)          
    
    
    
  WITH  CENTERED OVERLAY ROW 1 WIDTH 80 
  SIDE-LABELS TITLE COLOR VALUE(Syst.Var:ctc) ac-hdr
  FRAME lis.

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
IF AVAILABLE OrderFusion THEN 
   ASSIGN
      Memory     = recid(OrderFusion)
      must-print = TRUE
      must-add   = FALSE.
ELSE DO:
   MESSAGE "FixedLine Connection is Not Available !" VIEW-AS ALERT-BOX.
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
   DO:  /* Add a OrderFusion  */
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

             PROMPT-FOR OrderFusion.OrderProductID
                VALIDATE(OrderFusion.OrderProductID NOT ENTERED OR
                NOT CAN-FIND(OrderFusion WHERE
                   OrderFusion.OrderProductID = INPUT FRAME lis OrderFusion.OrderProductID ),
                   "OrderFusions " + string(INPUT OrderFusion.OrderProductID) +
                   " already exists !").
             IF INPUT OrderFusion.OrderProductID NOT ENTERED THEN 
             LEAVE add-row.
               
             CREATE OrderFusion.
             ASSIGN
                OrderFusion.OrderProductID = INPUT FRAME lis 
                                    OrderFusion.OrderProductID.

             RUN local-UPDATE-record.

             IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
                UNDO add-row, LEAVE add-row.

             IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhOrderFusion).
                ASSIGN
                   Memory = recid(OrderFusion)
                   xrecid = Memory.
             LEAVE.
          END.
       END.  /* ADD-ROW */
        
       HIDE FRAME lis NO-PAUSE.
       ASSIGN 
          must-print = TRUE.

       /* is there ANY record ? */
       FIND FIRST OrderFusion WHERE 
                  OrderFusion.OrderID = iiOrderID
                  NO-LOCK NO-ERROR.
       IF NOT AVAILABLE OrderFusion THEN LEAVE LOOP.
       NEXT LOOP.
    END.
    
PrintPage:
   DO:
      IF must-print THEN DO:
         UP FRAME-LINE - 1.
         FIND OrderFusion WHERE recid(OrderFusion) = Memory NO-LOCK NO-ERROR.

         /* DISPLAY one page beginning the record 
         whose RECID is saved into 'Memory'.
         starting from ROW 'delrow' */

         /* IF a ROW was recently DELETEd ... */
         IF delrow > 0 THEN DOWN delrow - 1.

         REPEAT WITH FRAME sel:
            IF AVAILABLE OrderFusion THEN DO:
               RUN local-disp-row.
               rtab[FRAME-LINE] = recid(OrderFusion).
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
            Syst.Var:ufk[1]= 0 Syst.Var:ufk[2]= 0 Syst.Var:ufk[3]= 0  Syst.Var:ufk[4]= 0
            Syst.Var:ufk[5]= 0 Syst.Var:ufk[6]= 0 Syst.Var:ufk[7]= 0 Syst.Var:ufk[8]= 8 Syst.Var:ufk[9]= 1
            Syst.Var:ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.
      
      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW lcStamp {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(Syst.Var:ccc) lcStamp WITH FRAME sel.
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
         FIND OrderFusion WHERE recid(OrderFusion) = Memory NO-LOCK.
         DO i = 1 TO FRAME-LINE - 1:
            RUN local-find-PREV.
            IF AVAILABLE OrderFusion THEN
               ASSIGN FIRSTrow = i Memory   = recid(OrderFusion).
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
            IF NOT AVAILABLE OrderFusion THEN DO:
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
                  rtab[1] = recid(OrderFusion)
                  Memory  = rtab[1].
            END.
        END.
         ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(Syst.Var:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND OrderFusion WHERE RECID(OrderFusion) = rtab[FRAME-DOWN] NO-LOCK .
            RUN local-find-NEXT.
            IF NOT AVAILABLE OrderFusion THEN DO:
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
               rtab[FRAME-DOWN] = recid(OrderFusion).
               /* save RECID of uppermost ROW */
               Memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT ROW */
      
      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
         Memory = rtab[1].
         FIND OrderFusion WHERE recid(OrderFusion) = Memory NO-LOCK NO-ERROR.
         RUN local-find-PREV.
         IF AVAILABLE OrderFusion THEN DO:
            Memory = recid(OrderFusion).

           /* reverse 1 page */
            DO RowNo = 1 TO (FRAME-DOWN - 1):
               RUN local-find-PREV.
               IF AVAILABLE OrderFusion THEN Memory = recid(OrderFusion).
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
            FIND OrderFusion WHERE recid(OrderFusion) = Memory NO-LOCK.
            must-print = TRUE.
            NEXT LOOP.
         END.
      END. /* NEXT page */
     
     ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 THEN DO WITH FRAME lis TRANSACTION:
       /* change */
       FIND FIRST OrderFusion where 
            recid(OrderFusion) = rtab[frame-line(sel)]
       no-lock.
       assign ac-hdr = " VIEW " ufkey = TRUE Syst.Var:ehto = 9.

       Syst.Var:cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       
       RUN local-disp-lis.
        
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(LASTKEY) = "F4" THEN UNDO, LEAVE.
       
       RUN local-disp-row.
          
       xrecid = recid(OrderFusion).
       LEAVE.

     END.

     ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(OrderFusion) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO: /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(OrderFusion) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.

PROCEDURE local-find-this:
   DEF INPUT PARAMETER exlock AS lo NO-UNDO.

   IF exlock THEN
      FIND OrderFusion WHERE recid(OrderFusion) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK NO-ERROR.
    ELSE
       FIND OrderFusion WHERE recid(OrderFusion) = rtab[frame-line(sel)] 
       NO-LOCK NO-ERROR.
END PROCEDURE.
        
PROCEDURE local-find-FIRST:
   IF order = 1 THEN DO:
      IF iiOrderID > 0 AND iiOrderProductID > 0 THEN 
         FIND FIRST OrderFusion WHERE 
                    OrderFusion.OrderID = iiOrderID AND
                    OrderFusion.OrderProductID = iiOrderProductID 
                    NO-LOCK NO-ERROR.
      ELSE
         FIND FIRST OrderFusion USE-INDEX OrderProductID NO-LOCK NO-ERROR.
   END.         
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN DO:
      IF iiOrderID > 0 AND iiOrderProductID > 0 THEN 
         FIND LAST OrderFusion WHERE 
                   OrderFusion.OrderID = iiOrderID AND
                    OrderFusion.OrderProductID = iiOrderProductID
                   NO-LOCK NO-ERROR.
      ELSE
         FIND LAST OrderFusion USE-INDEX OrderProductID NO-LOCK NO-ERROR.
   END.    
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN DO:
      IF iiOrderID > 0 AND iiOrderProductID > 0 THEN 
         FIND NEXT OrderFusion WHERE 
                   OrderFusion.OrderID = iiOrderID AND
                    OrderFusion.OrderProductID = iiOrderProductID
                   NO-LOCK NO-ERROR.
      ELSE
         FIND NEXT OrderFusion USE-INDEX OrderProductID NO-LOCK NO-ERROR.
   END.            
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN DO:
      IF iiOrderID > 0 AND iiOrderProductID > 0 THEN 
         FIND PREV OrderFusion WHERE 
                   OrderFusion.OrderID = iiOrderID AND
                    OrderFusion.OrderProductID = iiOrderProductID
                   NO-LOCK NO-ERROR.
      ELSE
         FIND PREV OrderFusion USE-INDEX OrderProductID NO-LOCK NO-ERROR.
   END.     
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY lcStamp     
               OrderFusion.MsSeq
               OrderFusion.OrderProductID
               OrderFusion.Product
               OrderFusion.FixedNumberType
               WITH FRAME sel.  
END PROCEDURE.

PROCEDURE local-find-others:
   ASSIGN lcStamp = Func.Common:mTS2HMS(OrderFusion.CreatedTS).
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   
   DEF INPUT PARAMETER ilNew AS LOG NO-UNDO.
   
   MaintMenu:
   REPEAT ON ENDKEY UNDO, LEAVE:
       
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
         WITH FRAME lis.

      IF Syst.Var:toimi = 8 THEN LEAVE MaintMenu. 
      END.

   HIDE FRAME lis NO-PAUSE.

END PROCEDURE.

PROCEDURE local-disp-lis:
      
   RUN local-find-others.
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
 
      FIND CURRENT OrderFusion EXCLUSIVE-LOCK.
            
      Syst.Var:ehto = 9.
      RUN Syst/ufkey.p. 
      
      UPDATE
         OrderFusion.UpdateTS        
         OrderFusion.FixedInstallationTS      
         OrderFusion.FixedStatusTS  
         OrderFusion.FixedStatus         
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