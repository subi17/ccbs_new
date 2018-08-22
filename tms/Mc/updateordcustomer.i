{Syst/tmsconst.i}
{Func/lib/accesslog.i}
{Func/fcontrolupd.i}
{Func/fcustdata.i}

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
   
   {Func/lib/eventlog.i}
   
   DEFINE VARIABLE lhOrderCustomer AS HANDLE NO-UNDO.
   lhOrderCustomer = BUFFER OrderCustomer:HANDLE.
   RUN StarEventInitialize(lhOrderCustomer).

ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhOrderCustomer).
   END.
   
END.

DEF VAR lcNewHeader  AS CHAR NO-UNDO.
DEF VAR lcNationality AS CHAR                  NO-UNDO. 
DEF VAR lcRegion     AS CHAR                   NO-UNDO. 
DEF VAR lcCountry    AS CHAR                   NO-UNDO. 
DEF VAR lcProfession  AS CHAR                  NO-UNDO.
DEF VAR lcBankName1  AS CHAR                   NO-UNDO.
DEF VAR lcBankAddr   AS CHAR                   NO-UNDO.
DEF VAR lcBankPost   AS CHAR                   NO-UNDO.
DEF VAR liCustRole   AS INT                    NO-UNDO.
DEF VAR lcCustID     AS CHAR                   NO-UNDO. 
DEF VAR llAccess     AS LOG  NO-UNDO.
DEF VAR lcProgram    AS CHAR NO-UNDO.

DEFINE VARIABLE llCustIdUpdateOK AS LOGICAL INITIAL FALSE NO-UNDO.
   
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
  FRAME fOrdMobile.   
  
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
  FRAME fOrdFusion.
    
PROCEDURE local-disp-customer:
   
   
   DEF INPUT PARAMETER iiRole AS INT NO-UNDO.
   DEF INPUT PARAMETER lNew   AS LOG NO-UNDO.

   DEF VAR lcCurrHeader AS CHAR NO-UNDO.
   DEF VAR lcNewHeader  AS CHAR NO-UNDO.
   DEFINE VARIABLE llCustIdUpdateOK AS LOGICAL INITIAL FALSE NO-UNDO.
   
  // HIDE FRAME sel no-pause.
   
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
         LEAVE action. 
      END.
   END. 

   HIDE FRAME fCustomer no-pause.
   FIND Current OrderCustomer NO-LOCK.
    
END PROCEDURE.        
   