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
{Func/fcontrolupd.i}
{Func/fcustdata.i}
{Mc/lib/tokenchk.i 'OrderProduct'}

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
   
   {Func/lib/eventlog.i}
      
   DEFINE VARIABLE lhOrderProduct AS HANDLE NO-UNDO.
   lhOrderProduct = BUFFER OrderProduct:HANDLE.
   RUN StarEventInitialize(lhOrderProduct).
   
   DEFINE VARIABLE lhOrderCustomer AS HANDLE NO-UNDO.
   lhOrderCustomer = BUFFER OrderCustomer:HANDLE.
   RUN StarEventInitialize(lhOrderCustomer).
   
              
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

DEF VAR lcHeader     AS CHAR  NO-UNDO.
DEF VAR lcBankName1  AS CHAR                   NO-UNDO.
DEF VAR lcBankAddr   AS CHAR                   NO-UNDO.
DEF VAR lcBankPost   AS CHAR                   NO-UNDO.
DEF VAR lcRegion     AS CHAR                   NO-UNDO. 
DEF VAR lcCountry    AS CHAR                   NO-UNDO. 
DEF VAR lcNationality AS CHAR                  NO-UNDO.
DEF VAR lcProfession  AS CHAR                  NO-UNDO.
DEF VAR llAccess     AS LOG  NO-UNDO.
DEF VAR liCustRole   AS INT                    NO-UNDO.
DEF VAR lcProgram    AS CHAR NO-UNDO.
DEF VAR lcOldAddressChk AS CHARACTER NO-UNDO. 
DEF VAR lcCode       AS CHAR                   NO-UNDO.
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
    
form
    "Title ........:" OrderCustomer.CustTitle                
      "Cust. Nbr .:" AT 50 OrderCustomer.CustNum
    SKIP
  
    "First Name ...:" OrderCustomer.FirstName
      "ID Type ...:" AT 50 OrderCustomer.CustIDType 
       VALIDATE(CAN-FIND(FIRST TMSCodes WHERE 
                               TMSCodes.TableName = "Customer"   AND
                               TMSCodes.FieldName = "CustIDType" AND
                               TMSCodes.CodeValue =         
                               INPUT OrderCustomer.CustIDType),
                               "Unknown ID type")
    SKIP
 
    "SurName1 .....:" OrderCustomer.SurName1                
      "Customer ID:" AT 50 OrderCustomer.CustID 
    SKIP

    "SurName2 .....:" OrderCustomer.SurName2                 
      "Birthday ..:" AT 50 OrderCustomer.Birthday FORMAT "99-99-9999" 
    SKIP

    "Company ......:" OrderCustomer.Company                  
      "Founding date:" AT 50 OrderCustomer.FoundationDate FORMAT "99-99-9999"
    SKIP

    "Address ......:" OrderCustomer.Street FORMAT "X(60)" 
    SKIP

    "Building Num .:" OrderCustomer.BuildingNum
      "Language ..:" AT 50 OrderCustomer.Language  
       VALIDATE(CAN-FIND(Language WHERE 
                         Language.Language = 
                         INTEGER(INPUT OrderCustomer.Language)),
                         "Unknown language") 
    SKIP

    "Floor ........:" OrderCustomer.AddressCompl
      "Nationality:" AT 50 OrderCustomer.Nationality FORMAT "X(2)"
       VALIDATE(CAN-FIND(Nationality WHERE 
                         Nationality.Nationality = 
                         INPUT OrderCustomer.Nationality),
                         "Unknown nationality")
       lcNationality NO-LABEL FORMAT "X(10)" 
    SKIP

    "Zip Code .....:" OrderCustomer.ZipCode     
      "Fixed Num .:" AT 50 OrderCustomer.FixedNumber FORMAT "X(10)" 
    SKIP

    "City .........:" OrderCustomer.PostOffice 
      "Mobile Num :"  AT 50 OrderCustomer.MobileNumber  FORMAT "X(10)" 
    SKIP

    "Region .......:" OrderCustomer.Region FORMAT "X(2)"
       VALIDATE(CAN-FIND(Region WHERE 
                         Region.Region = INPUT OrderCustomer.Region),
                         "Unknown region")
       lcRegion NO-LABEL FORMAT "X(15)"
      "CCReference:" AT 50 OrderPayment.CCReference
    SKIP 

    "Country ......:" OrderCustomer.Country FORMAT "X(2)" 
       VALIDATE(CAN-FIND(Country WHERE 
                         Country.Country = INPUT OrderCustomer.Country),
                         "Unknown country")
       lcCountry NO-LABEL FORMAT "X(20)"
      "Profession :" AT 50 OrderCustomer.Profession FORMAT "X(2)"
        lcProfession NO-LABEL FORMAT "X(12)"
    SKIP     
     
    "Courier Code .:" OrderCustomer.KialaCode NO-LABEL format "X(25)"
      "Marketing  :" AT 50
    SKIP

    "Email ........:" OrderCustomer.Email FORMAT "X(30)"
      "DontSharePD:" AT 50 OrderCustomer.DontSharePersData
      "Bank:" AT 69  OrderCustomer.OutBankMarketing
    SKIP
    
    "Bank Code ....:" OrderCustomer.BankCode  FORMAT "X(24)" 
      "SMS   Yoigo:" AT 50 OrderCustomer.OperSMSMarketing 
        "3rd:" AT 70 OrderCustomer.OutSMSMarketing
    SKIP

    lcBankName1 FORMAT "X(20)" AT 1
      "Email Yoigo:" AT 50 OrderCustomer.OperEMailMarketing
        "3rd:" AT 70 OrderCustomer.OutEMailMarketing
    SKIP

    lcBankAddr FORMAT "X(20)" AT 1
    lcBankPost FORMAT "X(22)" AT 26
    /*zip x(5) and city x(16) separated with space*/
      "Post  Yoigo:" AT 50  OrderCustomer.OperPostMarketing
        "3rd:" AT 70 OrderCustomer.OutPostMarketing
        
 WITH OVERLAY ROW 1 WIDTH 80 centered
    COLOR VALUE(Syst.Var:cfc) TITLE COLOR VALUE(Syst.Var:ctc) ac-hdr 
    NO-LABELS SIDE-LABEL FRAME fCustomer.
    
    
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
            Syst.Var:ufk[1]= 9861 
            Syst.Var:ufk[2]= 9862 
            Syst.Var:ufk[3]= 9863 
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
     
      ELSE IF LOOKUP(Syst.Var:nap,"1,f1") > 0 THEN DO:
         FIND OrderProduct where recid(OrderProduct) = rtab[FRAME-LINE] NO-LOCK.
         RUN Mc/orderproductparam.p(iiOrderID,OrderProduct.OrderProductID).
         ufkey = TRUE.  
         NEXT loop.
      END. 
     
      ELSE IF LOOKUP(Syst.Var:nap,"2,f2") > 0 THEN DO:
         FIND OrderProduct where recid(OrderProduct) = rtab[FRAME-LINE] NO-LOCK.
         RUN Mc/ordermobile.p(iiOrderID,OrderProduct.OrderProductID).
         ufkey = TRUE.  
         NEXT loop.
      END. 
      
      ELSE IF LOOKUP(Syst.Var:nap,"3,f3") > 0 THEN DO:
         FIND OrderProduct where recid(OrderProduct) = rtab[FRAME-LINE] NO-LOCK.
         RUN Mc/orderfusion.p(iiOrderID,OrderProduct.OrderProductID).
         ufkey = TRUE.  
         NEXT loop.
      END. 
      
      ELSE IF LOOKUP(Syst.Var:nap,"4,f4") > 0 THEN DO:
         RUN Mc/invoicetargetgroup.p(0,OrderProduct.ITGroupID).
         
         ufkey = TRUE.  
         NEXT loop.
      END.
      
      ELSE IF LOOKUP(Syst.Var:nap,"5,f5") > 0 THEN DO:
         RUN local-update-customer({&ORDERCUSTOMER_ROWTYPE_ACC},FALSE).
         ufkey = TRUE.  
         NEXT loop.
      END.
      
      ELSE IF LOOKUP(Syst.Var:nap,"6,f6") > 0 THEN DO:
         RUN local-update-customer({&ORDERCUSTOMER_ROWTYPE_DELIVERY},FALSE).
         ufkey = TRUE.  
         NEXT loop.
      END.
      
      /* logistic address */
      ELSE IF LOOKUP(Syst.Var:nap,"7,f7") > 0 THEN DO:
         RUN local-update-customer({&ORDERCUSTOMER_ROWTYPE_LOGISTICS},FALSE).
         ufkey = TRUE.  
         NEXT loop.
      END.
     
      ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 THEN
         REPEAT WITH FRAME lis TRANSACTION
         ON ENDKEY UNDO, LEAVE:
         
         /* change */
         {Syst/uright2.i}
         RUN local-find-this(FALSE).
         ASSIGN ac-hdr = " VIEW " ufkey = TRUE Syst.Var:ehto = 9.
         Syst.Var:cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
         CLEAR FRAME fCustomer NO-PAUSE.
         RUN local-disp-lis.
        
         IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
         KEYLABEL(LASTKEY) = "F4" THEN UNDO, LEAVE.
        
         RUN local-disp-row.
          
         xrecid = recid(OrderProduct).
         LEAVE.
         
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

PROCEDURE local-update-customer:

   DEF INPUT PARAMETER iiRole AS INT NO-UNDO.
   DEF INPUT PARAMETER lNew   AS LOG NO-UNDO.

   DEF VAR lcCurrHeader AS CHAR NO-UNDO.
   DEF VAR lcNewHeader  AS CHAR NO-UNDO.
   DEFINE VARIABLE llCustIdUpdateOK AS LOGICAL INITIAL FALSE NO-UNDO.
   
   HIDE FRAME sel no-pause.
   
   ASSIGN liCustRole   = iiRole
          lcCurrHeader = ac-hdr.
          
   llAccess = FALSE. 
   
   CASE iiRole:
      WHEN {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} THEN DO:
         ASSIGN
            lcNewHeader = " AGREEMENT"
            llAccess    = TRUE
            .      
      END. 
      WHEN {&ORDERCUSTOMER_ROWTYPE_INVOICE} THEN DO:
         IF Order.InvCustRole NE 2 THEN DO:
            MESSAGE "Invoice customer role is" Order.InvCustRole
            VIEW-AS ALERT-BOX INFORMATION.
            RETURN.
         END.
         lcNewHeader = " INVOICE".
      END.
      WHEN {&ORDERCUSTOMER_ROWTYPE_USER} THEN DO:
         IF Order.UserRole NE 3 THEN DO:
            MESSAGE "User role is" Order.UserRole
            VIEW-AS ALERT-BOX INFORMATION.
            RETURN.
         END.
         lcNewHeader = " USER".
      END.
      WHEN {&ORDERCUSTOMER_ROWTYPE_DELIVERY} THEN DO:
         lcNewHeader = " DELIVERY".
      END.
      WHEN {&ORDERCUSTOMER_ROWTYPE_CIF_CONTACT} THEN DO:
         lcNewHeader = " CONTACT".
      END.
      WHEN {&ORDERCUSTOMER_ROWTYPE_LOGISTICS} THEN DO:
         lcNewHeader = " LOGISTICS".
      END.
      WHEN {&ORDERCUSTOMER_ROWTYPE_MOBILE_POUSER} THEN DO:
         lcNewHeader = " MOB DONOR".
      END.
      WHEN {&ORDERCUSTOMER_ROWTYPE_FIXED_POUSER} THEN DO:
         lcNewHeader = " FIX DONOR".
      END.
      WHEN {&ORDERCUSTOMER_ROWTYPE_ACC} THEN DO:
         lcNewHeader = " ACC".
      END.
   END CASE.

   IF Order.StatusCode = "73" AND
      iiRole = {&ORDERCUSTOMER_ROWTYPE_MOBILE_POUSER}
   THEN llCustIDUpdateOK = TRUE.

   FIND FIRST OrderCustomer NO-LOCK WHERE
              OrderCustomer.Brand   = Syst.Var:gcBrand AND
              OrderCustomer.OrderID = Order.OrderID AND
              OrderCustomer.RowType = iiRole NO-ERROR.
   IF NOT AVAILABLE OrderCustomer THEN DO:
      MESSAGE "Customer data is not available"
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.

   IF llAccess THEN
      RUN CreateReadAccess("OrderCustomer", Syst.Var:katun, OrderCustomer.OrderId, lcProgram, "OrderId" ).

   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOrderCustomer).

   ACTION: 
   repeat with frame fCustomer:
 
      ASSIGN lcBankName1  = ""
             lcBankAddr   = ""
             lcBankPost   = ""
             lcProfession = "".
      IF OrderCustomer.BankCode > "" THEN DO:
   
         IF LENGTH(OrderCustomer.BankCode) = 24 THEN
            FIND FIRST Bank WHERE
                       Bank.Brand      = Syst.Var:gcBrand AND
                       Bank.BankID     = SUBSTRING(OrderCustomer.BankCode,5,4) AND
                       Bank.BankOffice = SUBSTRING(OrderCustomer.BankCode,9,4) 
            NO-LOCK NO-ERROR.
         ELSE
            FIND FIRST Bank WHERE
                       Bank.Brand      = Syst.Var:gcBrand AND
                       Bank.BankID     = SUBSTRING(OrderCustomer.BankCode,1,4) AND
                       Bank.BankOffice = SUBSTRING(OrderCustomer.BankCode,5,4) 
            NO-LOCK NO-ERROR.
            
         IF AVAILABLE Bank THEN ASSIGN 
            lcBankName1 = Bank.Name
            lcBankAddr  = Bank.Address
            lcBankPost  = Bank.ZipCode + " " + Bank.City.
      END.

      ac-hdr = lcNewHeader + " CUSTOMER ON ORDER " + 
               STRING(Order.OrderID) + " ".
   
      FIND Region WHERE
           Region.Region = OrderCustomer.Region NO-LOCK NO-ERROR.
      lcRegion = IF AVAILABLE Region THEN Region.RgName ELSE "".
      FIND Country WHERE
           Country.Country = OrderCustomer.Country NO-LOCK NO-ERROR.
      lcCountry = IF AVAILABLE Country THEN Country.COName ELSE "".
      FIND Nationality WHERE
           Nationality.Nationality = OrderCustomer.Nationality NO-LOCK NO-ERROR.
      lcNationality = IF AVAILABLE Nationality THEN Nationality.NtName ELSE "".
          
      IF OrderCustomer.Profession > "" THEN DO:
         FIND FIRST TMSCodes WHERE
                    TMSCodes.TableName = "OrderCustomer" AND
                    TMSCodes.FieldName = "Profession"    AND
                    TMSCodes.CodeValue = OrderCustomer.Profession
              NO-LOCK NO-ERROR.
         IF AVAILABLE TMSCodes THEN lcProfession = TMSCodes.CodeName.
      END.
          
      FIND FIRST orderpayment NO-LOCK WHERE
                 orderpayment.brand = Syst.Var:gcBrand AND
                 orderpayment.orderid = Order.OrderID
                 NO-ERROR.
      DISP 
         OrderCustomer.CustNum  
         OrderCustomer.CustIDType
         OrderCustomer.CustID
         OrderCustomer.SurName1  
         OrderCustomer.SurName2 
         OrderCustomer.CustTitle 
         OrderCustomer.FirstName 
         OrderCustomer.Company
         OrderCustomer.Street
         OrderCustomer.BuildingNum
         OrderCustomer.AddressCompl 
         OrderCustomer.ZipCode    
         OrderCustomer.Region
         lcRegion
         OrderCustomer.PostOffice    
         OrderCustomer.Country  
         lcCountry
         OrderCustomer.Nationality
         lcNationality
         OrderCustomer.Language
         OrderCustomer.FixedNumber
         OrderCustomer.MobileNumber
         OrderCustomer.EMail
         OrderCustomer.Birthday
         OrderCustomer.BankCode
         lcBankName1
         lcBankAddr
         lcBankPost
         OrderCustomer.OperSMSMarketing
         OrderCustomer.OperEMailMarketing
         OrderCustomer.OperPostMarketing
         OrderCustomer.OutSMSMarketing
         OrderCustomer.OutEMailMarketing
         OrderCustomer.OutPostMarketing
         OrderCustomer.OutBankMarketing
         OrderCustomer.FoundationDate
         OrderCustomer.Profession lcProfession
         OrderCustomer.KialaCode
         OrderCustomer.DontSharePersData         
      WITH FRAME fCustomer.
  
      IF AVAIL OrderPayment THEN 
         DISPLAY orderpayment.CCReference WITH FRAME fCustomer.
     
      ASSIGN
      Syst.Var:ufk = 0
      Syst.Var:ufk[1] = 0
      Syst.Var:ufk[5] = 0
      Syst.Var:ufk[8] = 8
      Syst.Var:ehto = 0
      ufkey = true.
      RUN Syst/ufkey.p.
                                                             
      IF Syst.Var:toimi = 8 then do:
         hide frame fCustomer NO-PAUSE.
         ac-hdr = lcCurrHeader.
         
         LEAVE.
      END.
   END. 

   HIDE FRAME fCustomer no-pause.
   view frame sel.
   FIND Current OrderCustomer NO-LOCK.

END PROCEDURE.