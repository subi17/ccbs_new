/*----------------------------------------------------------------------
  MODULE .......: order.p
  TASK .........: browser for table Order
  APPLICATION ..: tms
  AUTHOR .......: tk
  CREATED ......: 02.04.03
  CHANGED ......: 07.04.03 tk New fields added
                  11.04.03 tk check lock status
                  14.04.03 tk find by date,cli,orderid
                  23.04.03 tk f6 - closing of order
                  24.04.03 tk update order.clstamp
                  25.04.03 tk allow close when subscription not created
                  30.04.03 tk find with personid
                  13.05.03 tk wait for f1 after enter
                  16.05.03 tk order 2 for personid
                  19.05.03 tk f7 misc actions, statuscode HOLD
                  05.06.03 tk eventlog added to close
                  10.06.03 tk create billing event for lost sim card
                              longer format for services in frame lis
                  17.06.03 tk update chstamp in creditcheck
                  30.06.03 tk format "99-99-9999" for birthday
                  18.07.03 tk MNP changes in frames
                  24.07.03 tk allow update of cli in mnp orders
                  29.07.03 tk (print) mnp confirmation -button
                  07.08.03 tk double orders can be closed
                  21.08.03 tk ask before printing mnp confirmation
                  22.08.03 tk order.salesman
                  22.08.03 jp close mnp order
                  07.10.03 tk input parameter Tupas
                  08.10.03 tk two input parameters
                  22.01.04 tk brand
                  26.01.04 tk brand in memo and salesman finds
                              create customers before running mscuus1
                  06.02.04 jp custnum for memo
                  12.02.04 tk new credit handling
                  13.02.04 tk new frame lis layout, pnpnumbers, services
                  17.02.04 tk PNPNumber showing corrected
                  24.02.04 tk order.payer
                  26.02.04 tk allow creation from closed order
                  05.03.04 aam view deposit invoice (nnasla)          
                  23.03.04 tk user info updating possible
                  24.03.04 tk allow update for statuscode 4
                  25.03.04 aam transfer deposit (depotran)
                  21.04.04 jp  pnp-button disabled (f6)
                  26.04.04 aam check parameter InvCreType
                  26.04.04 tk vrk checking
                  07.05.04 tk function buttons rearranged
                  11.05.04 tk personid for creditrate
                  24.05.04 aam print information texts (prinoinf)
                  03.06.04 tk allow clitype updating
                  14.06.04 aam new parameters to prinoinf,
                               RUN Mc/prinoinf.p for sms-orders (liSMSTxt)
                  15.07.04 tk check cli with DEC             
                  10.08.04 jp  show orderchannel
                  17.08.04 aam campaign run (campruno)
                  06.09.04 aam commission for referee (corefo)
                  03.11.04 aam order confirmation for unknown orders (prinoinf)
                  10.11.04 aam confirmation letter for saldo agr. (prinoinf)
                  23.11.04 aam disp Referee
                  25.11.04 aam corefo commented out (moved to mobsub and mnpact)
                  03.12.04 tk delete fees with custnum in close
                  10.12.04 tk find mobsub with cli in f4
                  14.12.04 tk check clitype
                  14.12.04 tk statuscode 74 handling
                  17.12.04 jp find mobsub use msseq
                  27.12.04 aam don't update CLIType  
                  20.01.05 aam saldo agr. confirmation removed
                  03.02.05 aam campaign run removed 
                  31.03.05 tk do not release MSISDN if Mobsub found
                  07.04.05 tk do not release MSISDN if unhandled orders 
                              exist with same CLI
                  23.09.05 tk disp only * in NP column, NP status in frame lis
                  16.01.05 PZ FRAME lis layout changes
                  10.02.06 aam use backwards order for 'order=5'
                  22.03.06 PZ InvCustomer NO-LOCK search moved under F1
                  20.04.06 aam only one parameter to fCParamC
                  11.05.06 aam assign order.custnum if userrole=3 and old
                               customer has been chosen
                  20.11.06 aam new db structure             
                  28.11.06 jp  new input parameter to fActivateSubscription 
                  30.11.06 aam more new fields
                  01.12.06 tk  mnprequest
                  11.12.06 aam correct order for finds
                  12.12.06 tk  show mnp statuscode in MNPStatus
                  19.12.06 jt  icStatus check removed from F7
                  21.12.06 jt  fixed bug in finding date lcStatus -> icStatus
                  17.01.07 aam msrequest nbr view corrected
                  26.01.07 kl  ICC updating not allowd
                  06.02.07 kl  OrderCustomer nationality update allowed
                  14.03.07 kl  Col. 1 browse: oldest first
                  16.05.07/aam normal id types cannot be changed into passport 
                  21.06.07/mvi added Orderpayment.CCReference to customer frame
                  28.08.07/aam ordertopup, 
                               Order.FatAmount
                  31.10.07 jp  new parameter for msrequest
                  
  Version ......: yoigo
   ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable order      

{Syst/commali.i} 

DEFINE  INPUT PARAMETER  ipTupas1  AS INTEGER NO-UNDO.
DEFINE  INPUT PARAMETER  ipTupas2  AS INTEGER NO-UNDO.
DEFINE  INPUT PARAMETER  icStatus  AS CHAR    NO-UNDO.
DEFINE  INPUT PARAMETER  iiOrderID AS INT     NO-UNDO.

{Func/cparam2.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Order'}
{Syst/eventval.i}
{Func/finvtxt.i}
{Func/fcustdata.i}
{Func/fmakemsreq.i}
{Func/msisdn.i}
{Func/forderstamp.i}
{Func/fcontrolupd.i}
{Func/transname.i}
{Syst/tmsconst.i}
{Func/orderfunc.i}
{Mnp/mnp.i}
{Func/freacmobsub.i}
{Func/lib/accesslog.i}

session:system-alert-boxes = true.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhOrder AS HANDLE NO-UNDO.
   lhOrder = BUFFER Order:HANDLE.
   RUN StarEventInitialize(lhOrder).

   DEFINE VARIABLE lhOrderCustomer AS HANDLE NO-UNDO.
   lhOrderCustomer = BUFFER OrderCustomer:HANDLE.
   RUN StarEventInitialize(lhOrderCustomer).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhOrder).
   END.   

END.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR CLI          LIKE Order.CLI            NO-UNDO.
DEF VAR orderid      LIKE order.orderid        NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR lcCrStamp    AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 4.
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
DEF VAR lcStamp      AS CHAR format "x(19)"    NO-UNDO.
DEF VAR lcDlStamp    AS CHAR                   NO-UNDO.
DEF VAR lcStatus     AS CHAR                   NO-UNDO.
DEF VAR lDate        AS DATE format "99-99-99" NO-UNDO.
def var credok       AS LOG                    NO-UNDO.
def var newstatus    AS CHAR                   NO-UNDO.
def var memoch       as char format "x(1)"     NO-UNDO.
def var miscact      as log                    no-undo init false.
def var npstat       as char                   no-undo.
def var SMName       like salesman.smname      no-undo.
def var llclaim      as log                    no-undo.
def var lcKnown      AS CHAR                   NO-UNDO.
DEF VAR liInvType    AS INT                    NO-UNDO.
DEF VAR lcDepoLabel  AS CHAR                   NO-UNDO. 
DEF VAR liSMSTxt     AS INT                    NO-UNDO. 
DEF VAR liOCTxt      AS INT                    NO-UNDO. 
DEF VAR NPStatName   AS CHAR                   NO-UNDO.
DEF VAR lcUser       AS CHAR                   NO-UNDO.
DEF VAR lcAuthCustId AS CHAR                   NO-UNDO.
DEF VAR lcAuthCustIdType AS CHAR               NO-UNDO.
DEF VAR lcInvCust    AS CHAR                   NO-UNDO.
DEF VAR lcAgrCust    AS CHAR                   NO-UNDO. 
DEF VAR ldeSwitchTS  AS DE                     NO-UNDO.
DEF VAR lcFixedNumber AS CHAR                  NO-UNDO.
DEF VAR new-custnum  as int                    NO-UNDO.

DEF VAR liChannel    AS INT FORMAT "9"         NO-UNDO INIT "2".
DEF VAR llknown      AS LOG                    NO-UNDO.
DEF VAR liOrderID    AS INT                    NO-UNDO FORMAT ">>>>>>>>9".
DEF VAR ocResult     AS CHAR                   NO-UNDO.
DEF VAR liCustRole   AS INT                    NO-UNDO.
DEF VAR lcCustID     AS CHAR                   NO-UNDO. 
DEF VAR lcBankName1  AS CHAR                   NO-UNDO.
DEF VAR lcBankAddr   AS CHAR                   NO-UNDO.
DEF VAR lcBankPost   AS CHAR                   NO-UNDO.
DEF VAR lcCode       AS CHAR                   NO-UNDO.
DEF VAR lcDefCountry AS CHAR                   NO-UNDO.
DEF VAR lcPrtStamp   AS CHAR                   NO-UNDO. 
DEF VAR lcClStamp    AS CHAR                   NO-UNDO. 
DEF VAR lcOrdPayMeth AS CHAR                   NO-UNDO. 
DEF VAR lcStock      AS CHAR                   NO-UNDO. 
DEF VAR lcRegion     AS CHAR                   NO-UNDO. 
DEF VAR lcCountry    AS CHAR                   NO-UNDO. 
DEF VAR lcNationality AS CHAR                  NO-UNDO. 
DEF VAR lcContid      AS CHAR                  NO-UNDO.
DEF VAR ldAmount      AS DEC                   NO-UNDO. 
DEF VAR lcCustIdType  AS CHAR                  NO-UNDO.
DEF VAR lcCustType    AS CHAR EXTENT 5         NO-UNDO.
DEF VAR liCounter     AS INT                   NO-UNDO.
DEF VAR liLoop        AS INT                   NO-UNDO.
DEF VAR lcCustomerId  AS CHAR                  NO-UNDO.
DEF VAR oOrderId      AS INTEGER               NO-UNDO.
DEF VAR liMSRequest   AS INT                   NO-UNDO. 
DEF VAR lcProfession  AS CHAR                  NO-UNDO.
DEF VAR lcOldAddressChk AS CHARACTER NO-UNDO. 
DEF VAR lcLOStatus    AS CHARACTER NO-UNDO. 
DEF VAR lcFatGroup    AS CHAR      NO-UNDO.
DEF VAR lcMultiSimType AS CHAR NO-UNDO. 
DEF VAR liMultiSimType AS INT NO-UNDO. 
DEF VAR liMultiSimOrder LIKE Order.OrderID NO-UNDO. 
DEF VAR liRequestId AS INT NO-UNDO. 
DEF VAR lcDeliveryType AS CHAR NO-UNDO. 
DEF VAR liDeliveryType AS INT NO-UNDO. 
DEF VAR lcSIMonlyMNP AS CHAR NO-UNDO.   /* Added since this is used in ordersender.i */
DEF VAR lcProgram    AS CHAR NO-UNDO.
DEF VAR llAccess     AS LOG  NO-UNDO.

DEF BUFFER bOldOrder FOR Order.
DEF BUFFER bSIM FOR SIM.
DEF BUFFER lbOrder FOR Order.

lcProgram = PROGRAM-NAME(1).

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

    "Floor ........:" OrderCustomer.floor FORMAT "X(10)"  
      "Nationality:" AT 50 OrderCustomer.Nationality FORMAT "X(2)"
       VALIDATE(CAN-FIND(Nationality WHERE 
                         Nationality.Nationality = 
                         INPUT OrderCustomer.Nationality),
                         "Unknown nationality")
       lcNationality NO-LABEL FORMAT "X(10)" 
	SKIP

    "Zip Code .....:" OrderCustomer.ZipCode     
      "Fixed Num:" AT 50 OrderCustomer.FixedNumber FORMAT "X(10)" 
	SKIP

    "City .........:" OrderCustomer.PostOffice 
	  "Mobile Num:"  AT 50 OrderCustomer.MobileNumber  FORMAT "X(10)" 
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
      "Profession:" AT 50 OrderCustomer.Profession FORMAT "X(2)"
        lcProfession NO-LABEL FORMAT "X(14)"
    SKIP	 
	 
    "Courier Code .:" OrderCustomer.KialaCode NO-LABEL format "X(25)"
      "Marketing:" AT 50
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

FORM
   "MNP Channel:" liChannel  
   HELP "0=NEW  1=WEB  2=M2M"           SKIP
   WITH OVERLAY CENTERED ROW 10 TITLE " MNP CHANNEL " NO-LABELS
   FRAME mnpchannel.

ASSIGN liInvType    = fCParamI("InvCreType")
       lcDepoLabel  = IF liInvType = 4
                      THEN "Adv.payment fee amount:"
                      ELSE "Deposit fee amount ...:"
       lcKnown      = "UNKNOWN" WHEN ipTupas1 = 2
       lcKnown      = "KNOWN" WHEN ipTupas1 NE 2
       lcDefCountry = fCParamC("CountryCodeDef").

FORM
    lcStamp         LABEL "Created"
    lcCustId        COLUMN-LABEL "CustID" FORMAT "X(9)"
    Order.CLI       COLUMN-LABEL "MSISDN" FORMAT "X(9)"
    Order.ContractID COLUMN-LABEL "Contract" FORMAT "X(8)" 
    Order.OrderId   FORMAT ">>>>>>>9"
    lcStatus        LABEL "Status"     FORMAT "x(12)"
    Order.CredOk    COLUMN-LABEL "Cred"
    memoch          LABEL "M"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(Syst.Var:cfc)   
    TITLE COLOR VALUE(Syst.Var:ctc) " " + Syst.Var:ynimi + " " +
    " ORDERS "
    + string(TODAY,"99-99-99") + " "
    FRAME sel.

FORM
    Order.MultiSimID COLON 25 SKIP
    Order.MultiSimType LABEL "Multi SIM Order Type" 
    COLON 25 lcMultiSIMType NO-LABEL FORMAT "X(9)" SKIP
    liMultiSimOrder COLON 25 LABEL "Primary/Secondary Order" 
 WITH OVERLAY ROW 5 WIDTH 50 centered
    COLOR VALUE(Syst.Var:cfc) TITLE COLOR VALUE(Syst.Var:ctc) "Multi SIM Info" 
    SIDE-LABEL FRAME frMultiSIM.

{Func/brand.i}

{Func/order.i}

form
    "OrderID/status:" Order.OrderID "/" Order.Statuscode FORMAT "X(2)"
        lcStatus FORMAT "X(15)"
    "OrderType:" AT 48 Order.OrderType FORMAT ">9"
    SKIP

    "ContractID ...:" Order.ContractID 
    "Custnum .:" AT 48 Order.Custnum FORMAT ">>>>>>>>9" 
    SKIP

    "MSISDN .......:" Order.CLI FORMAT "X(30)"
    "AuthCType:" AT 48 lcAuthCustIdType
    SKIP
    
    "ICC ..........:" Order.ICC FORMAT "X(30)" 
    "AuthCusID:" AT 48 lcAuthCustId
    SKIP

    "CLIType ......:" Order.CLIType FORMAT "X(12)"                     
    "OrdererIP:" AT 48 Order.OrdererIP
    SKIP                     
    
    "Payment Method:" Order.PayType 
    "Campaign :" AT 48 Order.Campaign FORMAT "X(15)"
    SKIP
     
    "SubscriptionID:" Order.MsSeq 
    "FATime ..:" AT 48 Order.FATAmount FORMAT "->>>9.99"
       lcFatGroup FORMAT "X(11)"
    SKIP

    "Create request:" liMSrequest FORMAT ">>>>>>>>>"
    "Reseller :" AT 48 Order.Reseller
    SKIP
    
    "MNP ..........:" NPStatName                         
    "RiskCode:" AT 48 Order.RiskCode
    SKIP
 
    "Operator .....:" Order.CurrOper FORMAT "x(30)"                    
       HELP "Current Operator. Choose entry with F9"
    "Salesman.:" AT 48  Order.SalesMan FORMAT "x(20)"
    SKIP
     
    "Old ICC ......:" Order.OldICC FORMAT "X(30)" 
    "OrderCh..:" AT 48  Order.OrderChannel                
    SKIP

    "Old P.Method .:" Order.OldPayType
    "Source ..:" AT 48  Order.Source  
    SKIP

    "Referee ......:" Order.Referee 
    "O.Payment:" AT 48 lcOrdPayMeth FORMAT "X(20)"
    SKIP

    "Curr LO Status:" lcLOStatus FORMAT "x(30)"
    "Created .:" AT 48 lcCrStamp FORMAT "x(16)" 
    SKIP

    "Delivery Type :" liDeliveryType FORMAT "9" lcDeliveryType FORMAT "x(25)"
    "Printed .:" AT 48 lcPrtStamp FORMAT "X(16)" 
    SKIP

    "Agr.Customer .:" lcAgrCust FORMAT "X(30)"
    "Delivered:" AT 48 lcDlStamp FORMAT "x(16)" 
    SKIP
    
    "Inv.Customer .:" lcInvCust FORMAT "X(30)"
    /* "User .........:" lcUser    FORMAT "X(30)"  */

    "Closed ..:" AT 48 lcClStamp FORMAT "x(16)" 
    SKIP
    
WITH OVERLAY ROW 1 WIDTH 80 centered
    COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc) ac-hdr 
    NO-LABELS 
    FRAME lis.

form
   "Credit rating ........:" Order.CreditRate   skip
   "Credit event quantity :" Order.CREventQty   skip
   "Foreign user .........:" Order.Foreign      skip
   "Class for foreigners .:" Order.InhabitClass skip
   "Invoices in claiming .:" llClaim            skip 
   lcDepoLabel FORMAT "X(23)" 
                             Order.DepoFee      skip
   "Credit record OK .....:" Order.CredOk     
   
WITH OVERLAY ROW 4 centered
    TITLE " Credit information "
    NO-LABELS 
    FRAME cred.


form /* seek  Date */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE 
                      Brand.Brand = lcBrand),"Unknown brand") SKIP
    "Order date:" lDate HELP "Enter date of order"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND DATE "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  CLI */
    "Brand Code:" lcBrand  HELP "Enter Brand"
     VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "Msisdn no :" CLI HELP "Enter CLI Number"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND NUMBER "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f3.

form /* seek  OrderId */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(lcbrand = "*"  OR
             CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "OrderId ..:" liOrderid
    HELP "Enter Order Id"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND ID "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f4.

form /* seek  With CustId */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(lcbrand = "*"  OR
    CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "Customer ID .......:" lcCustomerId FORMAT "x(11)"
    HELP "Customers ID" SKIP
    "Customer ID Type ..:" lcCustIdType
    HELP "CIF N/A NIE NIF Passport"  SKIP
        
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND Customer ID "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f5.


form /* seek  With Fixed number */ 
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(lcbrand = "*"  OR
    CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "Fixed number .......:" lcFixedNumber FORMAT "x(11)"
    HELP "Fixed number" SKIP
        
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND Customer ID "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME fFixed.

form /* seek  PersonId */
    "Brand Code :" lcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "Contract Id:"  lcContId
    HELP "Enter Contract Id"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND ID "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f2.



/* Init values for Customer ID type */

liCounter = 0.

FOR EACH TMSCodes WHERE
         TMSCodes.TableName = "Customer"   AND
         TMSCodes.FieldName = "CustIDType" NO-LOCK:
   ASSIGN liCounter = liCounter + 1
          lcCustType[liCounter] = TMSCodes.CodeValue.
END.




FUNCTION fCheckCustomerData RETURNS LOGICAL
   (BUFFER bChkCustomer FOR OrderCustomer):
   
   /* cross reference checks */  
   IF bChkCustomer.ZipCode > "" AND SUBSTRING(bChkCustomer.ZipCode,1,2) NE 
      bChkCustomer.Region THEN DO:
      MESSAGE "There is a conflict between zipcode and region"
      VIEW-AS ALERT-BOX ERROR.
      RETURN FALSE.
   END. 
      
   FIND Region WHERE Region.Region = bChkCustomer.Region NO-LOCK NO-ERROR.
      
   IF bChkCustomer.FirstName + bChkCustomer.SurName1 + 
      bChkCustomer.SurName2 > "" AND
      bChkCustomer.Company > "" AND bChkCustomer.CustIdType NE "CIF"
   THEN DO:
      MESSAGE "You can't give both company name and consumer name"
      VIEW-AS ALERT-BOX ERROR.
      RETURN FALSE.
   END.
      
   IF bChkCustomer.CustTitle > "" AND 
      NOT Func.Common:mTMSCodeChk("Customer",
                           "Title",
                            bChkCustomer.CustTitle)
   THEN DO:                              
      MESSAGE "Unknown title"
      VIEW-AS ALERT-BOX ERROR.
      RETURN FALSE.
   END.

   /* company vrs. consumer */  
   IF (bChkCustomer.CustIDType = "CIF"   AND 
       (bChkCustomer.Company = "" ))       OR
      (LOOKUP(bChkCustomer.CustIDType,"CIF,N/A") = 0 AND 
       bChkCustomer.Company > "")
   THEN DO:
      MESSAGE "There is a conflict between ID type and given names"
      VIEW-AS ALERT-BOX ERROR.
      RETURN FALSE.
   END. 

   IF LOOKUP(bChkCustomer.CustIDType,"N/A,CIF") = 0 AND 
      (bChkCustomer.FirstName = ""  OR
       bChkCustomer.SurName1  = "")
   THEN DO:
      MESSAGE "Name data is missing"
      VIEW-AS ALERT-BOX ERROR.
      RETURN FALSE.
   END.

   /* country vrs. id type */
   IF (LOOKUP(bChkCustomer.CustIDType,"NIF,CIF") > 0 AND 
       bChkCustomer.Country NE lcDefCountry)                      OR
      (LOOKUP(bChkCustomer.CustIDType,"NIF,CIF,N/A") = 0 AND 
       bChkCustomer.Country = lcDefCountry) 
   THEN DO:
      MESSAGE "There is a conflict between ID type and country"
      VIEW-AS ALERT-BOX ERROR.
      RETURN FALSE.
   END.

   RETURN TRUE. 
   
END FUNCTION.

IF iiOrderId > 0 THEN DO:
   FIND FIRST Order WHERE 
              Order.Brand   = Syst.Var:gcBrand  AND 
              Order.OrderId = iiOrderID NO-LOCK NO-ERROR.
   RUN pOrderView.
   fCleanEventObjects().
   LEAVE.
END.          

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.

orders = "  By Date  ,  By OrgId , By Name  ,By OrderId , By Status , By OrgID , By  Cli ,  By orderid  ".

/* text for sms-order confirmation */
liSMSTxt = fGetInvTextID("General",
                         "smstilaus",
                         1,     /* so far only one language used */
                         TODAY).

/* text for order confirmation of unknown orders */
liOCTxt = fGetInvTextID("General",
                        "unknownorder",
                        1,   
                        TODAY).

RUN local-find-first.

IF AVAILABLE Order THEN ASSIGN
   Memory       = recid(Order)
   must-print   = TRUE
   llKnown      = FALSE WHEN Order.Tupas = 2
   llKnown      = TRUE WHEN Order.Tupas NE 2
   must-add     = FALSE.
ELSE DO:
      MESSAGE " No orders available ! " VIEW-AS ALERT-BOX.
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
        UP FRAME-LINE(sel) - 1.
        FIND Order WHERE recid(Order) = Memory NO-LOCK NO-ERROR.
        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE Order THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(Order).
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

      IF ufkey AND iiOrderID = 0 THEN DO:
        
         IF not miscact then ASSIGN
            Syst.Var:ufk[1] = 28 
            Syst.Var:ufk[2] = 653 
            Syst.Var:ufk[3] = 1045 
            Syst.Var:ufk[4] = 2211
            Syst.Var:ufk[5] = 9796
            Syst.Var:ufk[6] = 9852
            Syst.Var:ufk[7] = 0
            Syst.Var:ufk[8] = 8 Syst.Var:ufk[9]= 1
            Syst.Var:ehto   = 3 ufkey = FALSE.
          RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.

      IF iiOrderID = 0 THEN DO:
         IF order = 1 OR ORDER = 5 THEN DO:
            CHOOSE ROW lcStamp {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
            COLOR DISPLAY VALUE(Syst.Var:ccc) lcStamp WITH FRAME sel.
         END.
         ELSE IF order = 2 OR Order = 7 THEN DO:
            CHOOSE ROW Order.CLI {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
            COLOR DISPLAY VALUE(Syst.Var:ccc) Order.CLI WITH FRAME sel.
         END.
         ELSE IF order = 3 OR Order = 6 THEN DO:
            CHOOSE ROW Order.ContractID {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
            COLOR DISPLAY VALUE(Syst.Var:ccc) Order.ContractID WITH FRAME sel.
         END.
         IF order = 4 THEN DO:
            CHOOSE ROW Order.OrderId {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
            COLOR DISPLAY VALUE(Syst.Var:ccc) Order.OrderId WITH FRAME sel.
         END.
      END.

      IF rtab[FRAME-LINE] = ? THEN NEXT.

      Syst.Var:nap = keylabel(LASTKEY).

      IF LOOKUP(Syst.Var:nap,"cursor-right") > 0 THEN DO:
        order = order + 1. 
        IF order > maxOrder THEN order = 1.

      END.
      IF LOOKUP(Syst.Var:nap,"cursor-left") > 0 THEN DO:
         order = order - 1. 
         IF order = 0 THEN order = maxOrder - 3.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND Order WHERE recid(Order) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE Order THEN
              ASSIGN FIRSTrow = i Memory = recid(Order).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      ASSIGN Syst.Var:nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE Order THEN DO:
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
                rtab[1] = recid(Order)
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
           IF NOT AVAILABLE Order THEN DO:
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
              rtab[FRAME-DOWN] = recid(Order).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND Order WHERE recid(Order) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE Order THEN DO:
           Memory = recid(Order).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE Order THEN Memory = recid(Order).
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
           FIND Order WHERE recid(Order) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

        /* Search BY column 1 */
     ELSE IF LOOKUP(Syst.Var:nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
        Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
        Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
        CLEAR FRAME f1.
        Disp lcBrand With FRAME f1.
        SET lcBrand WHEN Syst.Var:gcAllBrand = TRUE
            lDate WITH FRAME f1.
        HIDE FRAME f1 NO-PAUSE.
        IF lDate ENTERED THEN DO:
           IF icStatus = "" THEN DO:
              FIND LAST Order WHERE 
                        Order.Brand =  lcBrand  AND
                        Order.CrStamp >= Func.Common:mHMS2TS(lDate,"00:00:00")
              USE-INDEX Stamp NO-LOCK NO-ERROR.
           END.
           ELSE              
           FIND LAST Order WHERE
                     Order.Brand      =  lcBrand  AND
                     Order.CrStamp   >= Func.Common:mHMS2TS(lDate,"00:00:00") AND 
                     order.StatusCode = icStatus
           USE-INDEX StatusCode NO-LOCK NO-ERROR.

           IF NOT  fRecFound(1) THEN NEXT Browse.
           
           NEXT LOOP.
        END.
     END. /* Search-1 */

        /* Search BY col 3 */
     ELSE IF LOOKUP(Syst.Var:nap,"2,f2") > 0 AND Syst.Var:ufk[2] > 0 THEN 
     DO ON ENDKEY UNDO, NEXT LOOP:
        Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
        Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
        CLEAR FRAME F3.
        Disp lcBrand With FRAME f3.
        SET  lcBrand WHEN Syst.Var:gcAllBrand = TRUE
             CLI WITH FRAME f3.
        HIDE FRAME f3 NO-PAUSE.
        IF CLI ENTERED THEN DO:
           IF icStatus = "" THEN DO:
              FIND FIRST Order WHERE 
                         Order.Brand  = lcBrand  AND
                         Order.CLI = CLI
              USE-INDEX CLI NO-LOCK NO-ERROR.
              IF NOT AVAILABLE Order THEN 
              FIND FIRST Order WHERE 
                              Order.Brand  = lcBrand  AND
                              Order.CLI BEGINS CLI
              USE-INDEX CLI NO-LOCK NO-ERROR.
           END.
           ELSE DO:
              FIND FIRST Order WHERE 
                         Order.Brand  = lcBrand  AND
                         Order.CLI    = CLI    AND 
                         Order.statusCode = icStatus 
              USE-INDEX StatusCLI NO-LOCK NO-ERROR.
              IF NOT AVAILABLE Order THEN 
              FIND FIRST Order WHERE 
                         Order.Brand  = lcBrand  AND
                         Order.statusCode = icStatus AND
                         Order.CLI    BEGINS CLI 
              USE-INDEX StatusCLI NO-LOCK NO-ERROR.
           END.
           
           IF NOT  fRecFound(2) THEN NEXT Browse.
           NEXT LOOP.
        END.
     END. /* Search-3 */

    ELSE IF LOOKUP(Syst.Var:nap,"6,f6") > 0 THEN 
     DO ON ENDKEY UNDO, NEXT LOOP:
               lcFixedNumber = "".
               
        Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
        Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
        CLEAR FRAME fFixed.
        SET  lcBrand   WHEN Syst.Var:gcAllBrand = TRUE
             lcFixedNumber 
             WITH FRAME fFixed.
        HIDE FRAME fFixed NO-PAUSE.

        FIND FIRST OrderFusion NO-LOCK where
                   OrderFusion.FixedNumber BEGINS lcFixedNumber NO-ERROR.

        IF NOT AVAILABLE OrderFusion THEN DO:
           MESSAGE "Fixed number not found!" VIEW-AS ALERT-BOX.
           NEXT Browse.
        END.

        RUN Mc/orderbrfixed.p(lcFixedNumber, OUTPUT oOrderID).
        FIND FIRST Order WHERE
                   Order.OrderId    = oOrderId AND
                   Order.Brand      = lcBrand NO-LOCK NO-ERROR.
           IF NOT fRecFound(4) THEN NEXT Browse.
           NEXT LOOP.
     END.
        
     /* Search BY col 4 */
     ELSE IF LOOKUP(Syst.Var:nap,"4,f4") > 0 THEN 
     DO ON ENDKEY UNDO, NEXT LOOP:
        Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
        Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
        CLEAR FRAME F4.
        Disp lcBrand With FRAME f4.
        SET  lcBrand   WHEN Syst.Var:gcAllBrand = TRUE 
             liorderid WITH FRAME f4.
        HIDE FRAME f4 NO-PAUSE.
        IF liorderid ENTERED THEN DO:
           if lcBrand ne "*" AND icStatus = "" THEN 
           FIND FIRST Order WHERE 
                      Order.Brand   = lcBrand AND
                      Order.orderid  = liorderid 
           NO-LOCK NO-ERROR.
           ELSE if icStatus = "" THEN 
           FIND FIRST Order WHERE
                      Order.Brand   = Syst.Var:gcBrand   AND 
                      Order.orderid = liorderid 
           NO-LOCK NO-ERROR.
           ELSE
           FIND FIRST Order WHERE
                      Order.Brand      = Syst.Var:gcBrand  AND 
                      Order.orderid    = liorderid  AND
                      Order.StatusCode = icStatus 
           NO-LOCK NO-ERROR.

           IF NOT  fRecFound(4) THEN NEXT Browse.
           NEXT LOOP.
        END.
     END. /* Search-3 */

     ELSE IF LOOKUP(Syst.Var:nap,"5,f5") > 0 THEN 
     DO ON ENDKEY UNDO, NEXT LOOP:
        ASSIGN lcCustomerId = ""
               lcCustIdType = "".
               
        Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
        Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
        CLEAR FRAME F5.
        SET  lcBrand   WHEN Syst.Var:gcAllBrand = TRUE
             lcCustomerId
             lcCustIdType 
             WITH FRAME f5.
        HIDE FRAME f5 NO-PAUSE.
        /* Find with given customer id type */
        IF lcCustIdType ENTERED THEN DO:     
     
           FIND FIRST TMSCodes WHERE
                      TMSCodes.TableName = "Customer" AND
                      TMSCodes.FieldName = "CustIdType" AND
                      TMSCodes.CodeValue = lcCustIdType
           NO-LOCK NO-ERROR.
           
           IF NOT AVAILABLE TMSCodes THEN DO:
              MESSAGE "Unknown customer ID type" VIEW-AS ALERT-BOX.
              NEXT LOOP.
           END.
           
           FIND FIRST OrderCustomer WHERE
                      OrderCustomer.CustIdType = TMSCodes.CodeValue AND
                      OrderCustomer.CustId     = lcCustomerId AND
                      OrderCustomer.Brand      = lcBrand
           NO-LOCK NO-ERROR.
           
           IF NOT AVAILABLE OrderCustomer THEN DO:
              MESSAGE "Customer ID not found!" VIEW-AS ALERT-BOX.
              NEXT Browse.
           END.
           
           RUN CreateReadAccess("OrderCustomer", Syst.Var:katun, OrderCustomer.OrderId, lcProgram, "OrderId" ).
           
           RUN Mc/orderbr.p(lcCustomerId,lcCustIdType,icStatus,OUTPUT oOrderID).
                 
           FIND FIRST Order WHERE
                      Order.OrderId    = oOrderId AND
                      Order.Brand      = lcBrand
           NO-LOCK NO-ERROR.
           IF NOT fRecFound(4) THEN NEXT Browse.
           NEXT LOOP.
                     
        END.
        ELSE DO:
           /* Find Customer ID with any type */
           liCounter = 0.
           DO liLoop = 1 TO 5:
              FIND FIRST ordercustomer WHERE
                         ordercustomer.custidtype = lcCustType[liLoop] AND
                         ordercustomer.custid     = lcCustomerId    AND
                         ordercustomer.brand      = lcBrand
              NO-LOCK NO-ERROR.
              IF AVAILABLE OrderCustomer THEN DO:
                 RUN CreateReadAccess("OrderCustomer", Syst.Var:katun, OrderCustomer.OrderId, lcProgram, "OrderId" ).
                 
                 RUN Mc/orderbr.p(OrderCustomer.CustId,
                             OrderCustomer.CustIdType,icStatus,
                             OUTPUT oOrderID).
                 FIND FIRST Order WHERE
                            Order.OrderId    = oOrderId AND
                            Order.Brand      = lcBrand
                 NO-LOCK NO-ERROR.
                 LEAVE.
              END.
           END.
           IF NOT AVAILABLE OrderCustomer THEN DO: 
              MESSAGE "Customer ID NOT FOUND!" VIEW-AS ALERT-BOX.
              LEAVE.
           END.
           IF NOT fRecFound(4) THEN NEXT Browse.
           NEXT LOOP.
        END.
        
     END.
     
     ELSE IF LOOKUP(Syst.Var:nap,"3,f3") > 0 AND Syst.Var:ufk[3] > 0 THEN 
     DO ON ENDKEY UNDO, NEXT LOOP:
        Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
        Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
        CLEAR FRAME F2.
        Disp lcBrand With FRAME f2.
        SET lcBrand WHEN Syst.Var:gcAllBrand = TRUE
            lcContid WITH FRAME f2.
        HIDE FRAME f2 NO-PAUSE.
        IF lcContId ENTERED THEN DO:
           IF icStatus = "" THEN DO:
              FIND FIRST Order WHERE 
                         Order.Brand = lcBrand AND
                         Order.ContractId = lcContId
              USE-INDEX ContractId NO-LOCK NO-ERROR.
              IF NOT AVAILABLE Order THEN 
              FIND FIRST Order WHERE 
                         Order.Brand = lcBrand AND
                         Order.ContractId >= lcContId
              USE-INDEX ContractId NO-LOCK NO-ERROR.
           END.
           ELSE DO:
              FIND FIRST Order WHERE
                         Order.Brand = lcBrand        AND
                         Order.ContractId = lcContId  AND
                         order.StatusCode = icStatus
              USE-INDEX StContractId NO-LOCK NO-ERROR.
              IF NOT AVAILABLE Order THEN 
              FIND FIRST Order WHERE
                         Order.Brand = lcBrand          AND
                         order.StatusCode = icStatus    AND
                         Order.ContractId >= lcContId  
              USE-INDEX StContractId NO-LOCK NO-ERROR.
           END.
           
           IF NOT  fRecFound(3) THEN NEXT Browse.
           NEXT LOOP.
        END.
     END.
     
     ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 THEN DO:
       RUN local-find-this(FALSE).
       RUN pOrderView.
       NEXT LOOP.

     END.
  
  ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
     RUN local-find-FIRST.
     ASSIGN Memory = recid(Order) must-print = TRUE.
     NEXT LOOP.
  END.

  ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
     RUN local-find-LAST.
     ASSIGN Memory = recid(Order) must-print = TRUE.
     NEXT LOOP.
  END.

  ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 AND NOT miscact THEN LEAVE LOOP.
  
  ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 AND miscact THEN DO:
     miscact = FALSE.
     ufkey = true.
     NEXT LOOP.
  
  END.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.

PROCEDURE pOrderView:

   pause 0.

   ac-hdr = " ORDER ".

   RUN local-disp-lis.

   ACTION: 
   repeat with frame lis:

      ASSIGN
      Syst.Var:ufk = 0
      Syst.Var:ufk[1] = 7             
      Syst.Var:ufk[2] = 2250
      Syst.Var:ufk[3] = (IF CAN-FIND(FIRST OrderCustomer OF Order WHERE 
                                  RowType = {&ORDERCUSTOMER_ROWTYPE_LOGISTICS})
               THEN 9844 ELSE 0)
      Syst.Var:ufk[4] = (IF Order.MultiSIMId > 0 THEN 9827 ELSE 0)
      Syst.Var:ufk[5] = 1152
      Syst.Var:ufk[6] = 9860
      Syst.Var:ufk[7] = 2243 
      Syst.Var:ufk[8] = 8
      Syst.Var:ehto = 0               
   
      ufkey = true.
      RUN Syst/ufkey.p.
  
     IF Syst.Var:toimi = 8 then do:
        hide frame lis.
        leave.
     end.
  
     /* customer management */
     ELSE IF Syst.Var:toimi = 2 THEN DO:

        SUBACTION:
        repeat with frame lis:

           ASSIGN
              Syst.Var:ufk = 0
              Syst.Var:ufk[1] = (IF CAN-FIND(FIRST OrderCustomer OF Order WHERE
                              RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT})
                        THEN 2246 /* agreement customer */
                        ELSE 0)
              Syst.Var:ufk[2] = (IF CAN-FIND(FIRST CustomerAccount OF Order)
                        THEN 9864 /* customer account */
                        ELSE 0)
              Syst.Var:ufk[3] = 0
              Syst.Var:ufk[4] = (IF CAN-FIND(FIRST OrderCustomer OF Order WHERE
                              RowType = {&ORDERCUSTOMER_ROWTYPE_DELIVERY})
                        THEN 1071 /* delivery address */
                        ELSE 0)
              Syst.Var:ufk[5] = (IF CAN-FIND(FIRST OrderCustomer OF Order WHERE
                              RowType = {&ORDERCUSTOMER_ROWTYPE_CIF_CONTACT})
                        THEN 1096 /* contact data */
                        ELSE 0)
              Syst.Var:ufk[6] = (IF CAN-FIND(FIRST OrderCustomer OF Order WHERE
                              RowType = {&ORDERCUSTOMER_ROWTYPE_MOBILE_POUSER})
                        THEN 2248 /* mobile donor (holder) */
                        ELSE 0)
              Syst.Var:ufk[7] = (IF CAN-FIND(FIRST OrderCustomer OF Order WHERE
                              RowType = {&ORDERCUSTOMER_ROWTYPE_FIXED_POUSER})
                        THEN 2249 /* fixed donor (holder) */
                        ELSE 0)
              Syst.Var:ufk[8] = 8
              Syst.Var:ehto = 0
              ufkey = TRUE.

           RUN Syst/ufkey.p.

           IF Syst.Var:toimi = 8 THEN LEAVE SubAction.

           ELSE IF Syst.Var:toimi = 1
           THEN RUN local-update-customer({&ORDERCUSTOMER_ROWTYPE_AGREEMENT},FALSE).

           ELSE IF Syst.Var:toimi = 2 THEN DO:
              RUN Mc/customeraccount.p(Order.AccountID).
           END.

           ELSE IF Syst.Var:toimi = 4 THEN DO:
              RUN local-update-customer({&ORDERCUSTOMER_ROWTYPE_DELIVERY},FALSE).
           END.

           ELSE IF Syst.Var:toimi = 5 THEN DO:
              RUN local-update-customer({&ORDERCUSTOMER_ROWTYPE_CIF_CONTACT},FALSE).
           END.

           ELSE IF Syst.Var:toimi = 6 THEN DO:
              RUN local-update-customer({&ORDERCUSTOMER_ROWTYPE_MOBILE_POUSER},FALSE).
           END.

           ELSE IF Syst.Var:toimi = 7 THEN DO:
              RUN local-update-customer({&ORDERCUSTOMER_ROWTYPE_FIXED_POUSER},FALSE).
           END.

        END.

        NEXT Action.
     END.

     /* logistic address */
     ELSE IF Syst.Var:toimi = 3 THEN DO:
        RUN local-update-customer({&ORDERCUSTOMER_ROWTYPE_LOGISTICS},FALSE).
        NEXT Action.
     END.

     else if Syst.Var:toimi = 4 and Syst.Var:ufk[4] > 0 then do:

        liMultiSimType = (IF Order.MultiSimType = 1 THEN 2 
                          ELSE IF Order.MultiSimType = 3 THEN 3 
                          ELSE 1).

        FIND FIRST lbOrder NO-LOCK WHERE
                   lbOrder.Brand = Syst.Var:gcBrand AND
                   lbOrder.MultiSimID = Order.MultiSimId AND
                   lbOrder.MultiSimType = liMultiSimType
        NO-ERROR.
        IF AVAIL lbOrder THEN
           liMultiSimOrder = lbOrder.OrderId.
        ELSE liMultiSimOrder = 0.
      
        lcMultiSIMType = Func.Common:mTMSCodeName("Order",
                                        "MultiSimType",
                                        STRING(Order.MultiSimType)).
        
        DISP Order.MultiSimId
             Order.MultiSimType lcMultiSIMType
             liMultiSimOrder WITH FRAME frMultiSIM.
        
        ASSIGN 
           Syst.Var:ufk    = 0
           Syst.Var:ufk[8] = 8
           Syst.Var:ehto   = 0.
         
        RUN Syst/ufkey.p.
        
        HIDE FRAME frMultiSIM NO-PAUSE.
        
        ufkey = true.
        next action.
     end.
          
     ELSE IF Syst.Var:toimi = 6 THEN DO: 
        
        RUN Mc/orderproduct.p (Order.OrderID).
        
        ASSIGN
           memory = recid(order)
           must-print = true
           ufkey = TRUE.
  
        LEAVE.
     END.

     ELSE IF Syst.Var:toimi = 5 THEN DO:
     
        SUBACTION: 
        repeat with frame lis:

           ASSIGN
           Syst.Var:ufk = 0
           Syst.Var:ufk[1] = 0
           Syst.Var:ufk[2] = 1070
           Syst.Var:ufk[3] = 1072
           Syst.Var:ufk[4] = 927           
           Syst.Var:ufk[5] = 2851
           Syst.Var:ufk[6] = 9855 
           Syst.Var:ufk[7] = 9019 
           Syst.Var:ufk[8] = 8
           Syst.Var:ehto = 0               
   
           ufkey = TRUE.
           RUN Syst/ufkey.p.
  
           IF Syst.Var:toimi = 8 THEN LEAVE SubAction.
           
           ELSE IF Syst.Var:toimi = 2 THEN DO:
              RUN Mc/orderaccessory.p (Order.OrderID,0).
           END.

           ELSE IF Syst.Var:toimi = 3 THEN DO:
              RUN Mc/ordertopup.p (Order.OrderID).
           END.
                
           ELSE IF Syst.Var:toimi = 4 THEN DO:
              RUN Mc/memo.p(INPUT 0,
                       INPUT "Order",
                       INPUT STRING(Order.OrderId),
                       INPUT "Order").

              ASSIGN
                 memory = recid(order)
                 ufkey = true
                 must-print = true.
           END.            
           ELSE IF Syst.Var:toimi = 5 THEN DO:

              IF liMSRequest EQ 0 THEN DO:
                 MESSAGE "Order request not found" VIEW-AS ALERT-BOX.
                 NEXT.
              END.

              RUN local-find-this(FALSE).
              
              RUN Mm/msrequest.p(?,?,Order.MsSeq,0,liMsRequest,"").

           END.
           ELSE IF Syst.Var:toimi = 6 THEN DO:
              RUN local-find-this(FALSE).
              RUN Mc/ordergroup.p(Order.OrderId).
           END.
           ELSE IF Syst.Var:toimi = 7 THEN DO:
              RUN local-find-this(FALSE).
              RUN Mc/orderdelivery.p(Order.OrderId).
           END.
        END.

        NEXT Action.
           
     END.
     
     
     ELSE IF Syst.Var:toimi = 7 THEN DO:
        find current order NO-LOCK.
        RUN Mc/orderfunc.p(INPUT order.statuscode, Order.OrderID, TRUE).       
        ASSIGN
        memory = recid(order)
        ufkey  = true
        must-print = true.

        NEXT Action.
     END.                                                    
  
     ELSE IF Syst.Var:toimi = 1 THEN DO:
        RUN local-UPDATE-record(FALSE).                                  
     
        RUN local-find-this(FALSE).
        RUN local-disp-lis.

        ufkey = true.
        
        NEXT Action.  

     END.

     RUN local-disp-row.
     xrecid = recid(Order).
     LEAVE.
   END.  /* action */
  
END PROCEDURE. /* order view */ 

PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.
    
    IF exlock THEN
      IF iiOrderId > 0 THEN 
      FIND CURRENT Order EXCLUSIVE-LOCK NO-ERROR.
      ELSE
      FIND Order WHERE recid(Order) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK NO-ERROR.
    ELSE
      IF iiOrderId > 0 THEN 
      FIND CURRENT Order NO-LOCK NO-ERROR.
      ELSE
       FIND Order WHERE recid(Order) = rtab[frame-line(sel)] 
       NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF icStatus > "" THEN DO:
       IF order = 1 THEN 
          FIND LAST Order USE-INDEX StatusCode WHERE 
                    Order.Brand      = lcBrand  AND
                    Order.StatusCode = icStatus
          NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN 
          FIND FIRST Order USE-INDEX StContractId WHERE 
                     Order.Brand      = lcBrand  AND
                     Order.StatusCode = icStatus
          NO-LOCK NO-ERROR.   
       ELSE IF order = 2 THEN 
          FIND FIRST Order USE-INDEX StatusCLI WHERE 
                     Order.Brand      = lcBrand  AND
                     Order.StatusCode = icStatus
          NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN 
          FIND FIRST Order USE-INDEX StOrderId WHERE 
                     Order.Brand      = lcBrand  AND
                     Order.StatusCode = icStatus
          NO-LOCK NO-ERROR.
   END.
   ELSE DO:
       IF order = 1 THEN 
          FIND FIRST Order USE-INDEX Stamp WHERE 
                     Order.Brand      = lcBrand
          NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN 
          FIND FIRST Order USE-INDEX ContractId WHERE 
                     Order.Brand      = lcBrand  
          NO-LOCK NO-ERROR.   
       ELSE IF order = 2 THEN 
          FIND FIRST Order USE-INDEX CLI WHERE 
                     Order.Brand      = lcBrand  
          NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN 
          FIND FIRST Order USE-INDEX OrderId WHERE 
                     Order.Brand      = lcBrand  
          NO-LOCK NO-ERROR.
   END.
          
END PROCEDURE.

PROCEDURE local-find-LAST:
       
   IF icStatus > "" THEN DO:
       IF order = 1 THEN 
          FIND FIRST Order USE-INDEX StatusCode WHERE 
                     Order.Brand      = lcBrand  AND
                     Order.StatusCode = icStatus
          NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN 
          FIND LAST Order USE-INDEX StContractId WHERE 
                    Order.Brand      = lcBrand  AND
                    Order.StatusCode = icStatus
          NO-LOCK NO-ERROR.   
       ELSE IF order = 2 THEN 
          FIND LAST Order USE-INDEX StatusCLI WHERE 
                    Order.Brand      = lcBrand  AND
                    Order.StatusCode = icStatus
          NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN 
          FIND LAST Order USE-INDEX StOrderId WHERE 
                    Order.Brand      = lcBrand  AND
                    Order.StatusCode = icStatus
          NO-LOCK NO-ERROR.
   END.
   ELSE DO:
       IF order = 1 THEN 
          FIND LAST Order USE-INDEX Stamp WHERE 
                    Order.Brand      = lcBrand
          NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN 
          FIND LAST Order USE-INDEX ContractId WHERE 
                    Order.Brand      = lcBrand  
          NO-LOCK NO-ERROR.   
       ELSE IF order = 2 THEN 
          FIND LAST Order USE-INDEX CLI WHERE 
                   Order.Brand      = lcBrand  
          NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN 
          FIND LAST Order USE-INDEX OrderId WHERE 
                    Order.Brand      = lcBrand  
          NO-LOCK NO-ERROR.
   END.
 
END PROCEDURE.

PROCEDURE local-find-NEXT:
 
   IF icStatus > "" THEN DO:
       IF order = 1 THEN 
          FIND PREV Order USE-INDEX StatusCode WHERE 
                    Order.Brand      = lcBrand  AND
                    Order.StatusCode = icStatus
          NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN 
          FIND NEXT Order USE-INDEX StContractId WHERE 
                    Order.Brand      = lcBrand  AND
                    Order.StatusCode = icStatus
          NO-LOCK NO-ERROR.   
       ELSE IF order = 2 THEN 
          FIND NEXT Order USE-INDEX StatusCLI WHERE 
                    Order.Brand      = lcBrand  AND
                    Order.StatusCode = icStatus
          NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN 
          FIND NEXT Order USE-INDEX StOrderId WHERE 
                    Order.Brand      = lcBrand  AND
                    Order.StatusCode = icStatus
          NO-LOCK NO-ERROR.
   END.
   ELSE DO:
       IF order = 1 THEN 
          FIND NEXT Order USE-INDEX Stamp WHERE 
                    Order.Brand      = lcBrand
          NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN 
          FIND NEXT Order USE-INDEX ContractId WHERE 
                    Order.Brand      = lcBrand  
          NO-LOCK NO-ERROR.   
       ELSE IF order = 2 THEN 
          FIND NEXT Order USE-INDEX CLI WHERE 
                   Order.Brand      = lcBrand  
          NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN 
          FIND NEXT Order USE-INDEX OrderId WHERE 
                    Order.Brand      = lcBrand  
          NO-LOCK NO-ERROR.
   END.
 
END PROCEDURE.

PROCEDURE local-find-PREV:
       
   IF icStatus > "" THEN DO:
       IF order = 1 THEN 
          FIND NEXT Order USE-INDEX StatusCode WHERE 
                    Order.Brand      = lcBrand  AND
                    Order.StatusCode = icStatus
          NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN 
          FIND PREV Order USE-INDEX StContractId WHERE 
                    Order.Brand      = lcBrand  AND
                    Order.StatusCode = icStatus
          NO-LOCK NO-ERROR.   
       ELSE IF order = 2 THEN 
          FIND PREV Order USE-INDEX StatusCLI WHERE 
                    Order.Brand      = lcBrand  AND
                    Order.StatusCode = icStatus
          NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN 
          FIND PREV Order USE-INDEX StOrderId WHERE 
                    Order.Brand      = lcBrand  AND
                    Order.StatusCode = icStatus
          NO-LOCK NO-ERROR.
   END.
   ELSE DO:
       IF order = 1 THEN 
          FIND PREV Order USE-INDEX Stamp WHERE 
                    Order.Brand      = lcBrand
          NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN 
          FIND PREV Order USE-INDEX ContractId WHERE 
                    Order.Brand      = lcBrand  
          NO-LOCK NO-ERROR.   
       ELSE IF order = 2 THEN 
          FIND PREV Order USE-INDEX CLI WHERE 
                   Order.Brand      = lcBrand  
          NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN 
          FIND PREV Order USE-INDEX OrderId WHERE 
                    Order.Brand      = lcBrand  
          NO-LOCK NO-ERROR.
   END.
 
END PROCEDURE.

PROCEDURE local-disp-row:

   RUN local-find-others-common.
   
   CLEAR FRAME sel NO-PAUSE.
   DISPLAY 
      lcStamp
      lcCustId
      Order.CLI
      Order.OrderId
      Order.ContractID
      lcStatus
      Order.CredOk WHEN Order.CREventQty > 0
      memoch
   WITH FRAME sel.

END PROCEDURE.

FUNCTION fStatusText RETURNS CHAR
   (iiStatusCode AS CHAR):
   FIND FIRST TMSCodes WHERE
              TMSCodes.TableName = "Order" AND
              TMSCodes.FieldName = "StatusCode" AND
              TMSCodes.CodeGroup = "Orders" AND
              TMSCodes.CodeValue = Order.StatusCode
   NO-LOCK NO-ERROR.
   IF AVAIL TMSCodes THEN RETURN TMSCodes.CodeName.

   RETURN "".
END.

PROCEDURE local-find-others-common.

   ASSIGN lcStamp = Func.Common:mTS2HMS(Order.CrStamp)
          lcStatus = fStatusText(Order.StatusCode).
   FIND FIRST OrderCustomer OF Order NO-LOCK WHERE
              OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} NO-ERROR.
   IF AVAIL OrderCustomer THEN
      lcCustID = OrderCustomer.CustID.
   ELSE DO:
      FIND FIRST OrderCustomer OF Order NO-LOCK WHERE
           OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_ACC} NO-ERROR.
      IF AVAIL OrderCustomer THEN
      lcCustID = OrderCustomer.CustID.
      ELSE lcCustID = "".
   END.    
   
   IF CAN-FIND(FIRST Memo WHERE
                     Memo.Brand     = Order.Brand AND
                     Memo.Hosttable = "Order" AND
                     Memo.KeyValue = STRING(Order.OrderId)) 
   THEN memoch = "M".
   ELSE memoch = "".
END.

PROCEDURE local-find-others.
   
   RUN local-find-others-common.

   FIND FIRST OrderFusion NO-LOCK WHERE
              OrderFusion.Brand = Syst.Var:gcBrand AND
              OrderFusion.OrderID = Order.OrderID NO-ERROR.
   
   FIND FIRST OrderDelivery WHERE
      OrderDelivery.Brand   = Syst.Var:gcBrand AND
      OrderDelivery.OrderID = Order.OrderId NO-LOCK NO-ERROR.
   IF AVAIL OrderDelivery THEN DO:
      
      lcLOStatus = fGetItemName(Syst.Var:gcBrand, 
               "LOStatusId", 
               STRING(OrderDelivery.LOStatusId),
               5,
               TODAY).
      IF lcLOStatus EQ "" THEN lcLOStatus = STRING(OrderDelivery.LOStatusId).
   END.
   ELSE IF Order.Logistics > "" THEN lcLOStatus = Order.Logistics.
   ELSE lcLOStatus = "".


   FIND FIRST TMSCodes WHERE 
              TMSCodes.TableName = "MNPProcess" AND
              TMSCodes.FieldName = "StatusCode" AND
              TMSCodes.CodeGroup = "MNP" AND
              TMSCodes.CodeValue = STRING(Order.MNPStatus - 1)
   NO-LOCK NO-ERROR.

   IF AVAIL TMSCodes THEN NPStatName = TMSCodes.CodeName.
   ELSE NPStatName = "".
   
   IF Order.MNPStatus = 0 THEN npstat = "".
   ELSE                        npstat = "*".
   
   ASSIGN
      lcKnown     = "UNKNOWN" WHEN Order.tupas =  2
      lcKnown     = "KNOWN"   WHEN Order.tupas NE 2.
                 
   FIND salesman WHERE 
        salesman.brand    = order.brand and
        salesman.salesman = order.salesman no-lock no-error.
   if avail salesman then SMName = salesman.smname.
   else SMName = "".

   ASSIGN
      lcAuthCustId     = ""
      lcAuthCustIdType = "".

   FOR EACH OrderCustomer NO-LOCK USE-INDEX OrderId WHERE
            OrderCustomer.Brand = Order.Brand AND
            OrderCustomer.OrderId = Order.OrderId:

      CASE OrderCustomer.RowType:
          
      WHEN {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} OR
      WHEN {&ORDERCUSTOMER_ROWTYPE_ACC} THEN DO:
         lcAgrCust = Func.Common:mDispOrderName(BUFFER OrderCustomer).
         ASSIGN
            lcAuthCustId     = OrderCustomer.AuthCustId
            lcAuthCustIdType = OrderCustomer.AuthCustIdType.

         IF Order.InvCustRole = 1 THEN lcInvCust = lcAgrCust.
         IF Order.UserRole    = 1 THEN lcUser    = lcAgrCust.
         
      END. 
      WHEN {&ORDERCUSTOMER_ROWTYPE_INVOICE} THEN DO:
         lcInvCust = Func.Common:mDispOrderName(BUFFER OrderCustomer).
         IF Order.UserRole = 2 THEN lcUser = lcInvCust.
      END. 
      WHEN {&ORDERCUSTOMER_ROWTYPE_USER} THEN DO:
         lcUser = Func.Common:mDispOrderName(BUFFER OrderCustomer).
      END.
      END CASE.
   END.
        
   /* timestamps */
   ASSIGN lcCrStamp  = Func.Common:mTS2HMS(Order.CrStamp)
          lcDlStamp  = Func.Common:mTS2HMS(fGetOrderStamp(Order.OrderID,"Delivery"))
          lcPrtStamp = Func.Common:mTS2HMS(fGetOrderStamp(Order.OrderID,"Print"))
          lcClStamp  = Func.Common:mTS2HMS(fGetOrderStamp(Order.OrderID,"Close")).
   
   lcOrdPayMeth = "".
   FOR FIRST OrderPayment OF Order NO-LOCK:
   
      lcOrdPayMeth = Func.Common:mTMSCodeName("OrderPayment",
                                      "Method",
                                      STRING(OrderPayment.Method)).
   END.
      
   IF Order.DeliverySecure EQ 1
   THEN liDeliveryType = {&ORDER_DELTYPE_POST_SECURE}.
   ELSE IF Order.DeliverySecure EQ 2
   THEN liDeliveryType = {&ORDER_DELTYPE_POS_SECURE}.
   ELSE liDeliveryType = Order.DeliveryType.

   lcDeliveryType = Func.Common:mTMSCodeName("Order",
                                   "DeliveryType",
                                   STRING(liDeliveryType)).

   liMsRequest = 0.
   IF Order.MSSeq > 0 THEN DO:
      IF Order.OrderType = {&ORDER_TYPE_ROLLBACK} THEN
         FIND FIRST Msrequest WHERE 
                    MSRequest.MSSeq      = Order.MSSeq    AND 
                    MSrequest.ReqType    = 82             AND 
                    MSrequest.ReqCparam1 = "REACTIVATE"   AND
                    MSrequest.ReqIparam1 = Order.OrderId No-LOCK NO-ERROR.
      ELSE IF Order.OrderType = {&ORDER_TYPE_STC} THEN
         FIND FIRST Msrequest WHERE 
                    MSRequest.MSSeq      = Order.MSSeq    AND 
                    MSrequest.ReqType    = 0             AND 
                    MSrequest.ReqIparam2 = Order.OrderId No-LOCK NO-ERROR.
      ELSE IF Order.OrderType = {&ORDER_TYPE_RENEWAL} THEN
         FIND FIRST Msrequest WHERE 
                    MSRequest.MSSeq      = Order.MSSeq    AND 
                    MSrequest.ReqType    = 46             AND 
                    MSrequest.ReqIparam1 = Order.OrderId No-LOCK NO-ERROR.
      ELSE IF Order.OrderType = {&ORDER_TYPE_ACC} THEN
         FIND FIRST Msrequest WHERE 
                    MSRequest.MSSeq      = Order.MSSeq    AND 
                    MSrequest.ReqType    = 10             AND 
                    MSrequest.ReqIparam4 = Order.OrderId No-LOCK NO-ERROR.
      ELSE DO:
         FIND FIRST Msrequest WHERE 
                    MSRequest.MSSeq      = Order.MSSeq    AND 
                    MSrequest.ReqType    = 13             AND 
                    MSrequest.ReqCparam1 = "CREATE" No-LOCK NO-ERROR.

         IF NOT AVAIL MSrequest THEN
            FIND FIRST Msrequest WHERE 
                       MSRequest.MSSeq      = Order.MSSeq    AND
                       MSrequest.ReqType    = 14 NO-ERROR. 
      END.

      IF AVAILABLE MsRequest THEN liMsRequest = MsRequest.MsRequest.
   END. /* IF Order.MSSeq > 0 THEN DO: */

END PROCEDURE.

PROCEDURE local-disp-lis:
      
      RUN local-find-others.
      
      IF Order.FtGrp > "" THEN 
         lcFatGroup = "(" + Order.FtGrp + ")".
      ELSE lcFatGroup = "".

      lcStatus = fStatusText(Order.Statuscode).
 
      DISP
         Order.PayType
         Order.ICC 
         Order.OldICC
         Order.OldPayType WHEN Order.MNPStatus > 0
         "" WHEN Order.MNPStatus = 0 @ Order.OldPayType 
         Order.Referee
         Order.OrderId
         Order.StatusCode
         lcStatus
         Order.ContractID
         Order.Custnum
         lcAuthCustIdType
         lcAuthCustId
         Order.OrderType
         Order.OrdererIP
         Order.CLIType
         Order.CLI 
         Order.CLI + " / " + OrderFusion.FixedNumber WHEN 
            AVAIL OrderFusion AND OrderFusion.FixedNumber > "" @ Order.CLI
         Order.MsSeq
         Order.OrderChannel 
         ENTRY(1,Order.Campaign,";") @ Order.Campaign
         Order.FATAmount
         lcFatGroup
         order.Source
         NPStatName
         Order.Curroper
         Order.Reseller
         Order.RiskCode 
         Order.Salesman
         lcCrStamp
         lcDlStamp
         lcPrtStamp
         lcClStamp
         lcAgrCust
         lcInvCust
         /* lcUser */
         lcOrdPayMeth
         liMsRequest 
         lcLOStatus
         liDeliveryType lcDeliveryType
      WITH FRAME lis.
END PROCEDURE.

PROCEDURE local-disp-cred:
   
   RUN local-find-others.
           
   DISP
      Order.CreditRate   
      Order.CREventQty   
      Order.Foreign      
      Order.InhabitClass 
      Order.ClaimState NE 0 @ llClaim  
      lcDepoLabel 
      Order.DepoFee      
      Order.CredOk WHEN Order.CREventQty > 0
   WITH FRAME cred.

END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF INPUT PARAMETER lNew AS LOG NO-UNDO. 
   
   DEFINE VARIABLE lcCurrOper AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcRiskCode AS CHARACTER NO-UNDO. 

   RUN local-find-this(FALSE).
   
   CLEAR FRAME lis NO-PAUSE.
   Syst.Var:ehto = 9. RUN Syst/ufkey.p.
   
   REPEAT ON ENDKEY UNDO, LEAVE:
   
      RUN local-find-others.
      RUN local-disp-lis.

      IF lcRight = "RW" THEN DO:

         PROMPT
            Order.CurrOper WHEN Order.StatusCode EQ "73"
            Order.OldICC   WHEN Order.StatusCode EQ "73"
            Order.RiskCode
         WITH FRAME lis EDITING:
            
            READKEY.
      
            IF KEYLABEL(lastkey) = "F2" THEN NEXT.

            IF FRAME-FIELD = "CurrOper" AND LOOKUP( KEYLABEL(LASTKEY),"F9") > 0 THEN
            DO:
               
               RUN Help/h-mnpoperator.p.
               
               IF siirto NE ? AND siirto NE "" THEN DO:
                  lcCurrOper = siirto NO-ERROR.
                  DISPLAY lcCurrOper @ Order.CurrOper WITH FRAME lis.
               END. /* IF lcCurrOper NE "" ... */
               Syst.Var:ehto = 9. RUN Syst/ufkey.p.
            END.
      
            IF LOOKUP(KEYLABEL(LASTKEY),Syst.Var:poisnap) > 0 THEN DO:
               
               IF FRAME-FIELD = "CurrOper" THEN
               DO:
                  FIND FIRST MNPOperator WHERE
                             MNPOperator.Brand = Syst.Var:gcBrand AND
                             MNPOperator.OperName = INPUT Order.CurrOper AND
                             MNPOperator.Active = True
                  NO-LOCK NO-ERROR.
                  IF NOT AVAIL MNPOperator THEN
                     FIND FIRST MNPOperator WHERE
                                MNPOperator.Brand = Syst.Var:gcBrand AND
                                MNPOperator.OperName = INPUT Order.CurrOper
                     NO-LOCK NO-ERROR.
                  IF AVAILABlE MNPOperator THEN
                  DO:
                     DISP MNPOperator.OperName @ Order.CurrOper WITH FRAME lis.
                  END.
                  ELSE DO:
                     MESSAGE {&MSG_INCORRECT_VALUE} VIEW-AS ALERT-BOX.
                     NEXT.
                  END.
               END.
               
            END.

            APPLY LASTKEY.
         END.
      END.
   
      FIND CURRENT Order EXCLUSIVE-LOCK.
      
      IF CURRENT-CHANGED Order THEN DO:
         
         FIND CURRENT Order NO-LOCK.
         
         MESSAGE ({&MSG_RECORD_CHANGED})
         VIEW-AS ALERT-BOX TITLE "UPDATE CANCELLED".

      END. 
      ELSE DO: 
         
         IF NOT lNew AND llDoEvent THEN RUN StarEventSetOldBuffer ( lhOrder ).
         
         ASSIGN
            Order.CurrOper WHEN Order.StatusCode EQ "73"
            Order.OldIcc WHEN Order.StatusCode EQ "73"
            lcRiskCode = INPUT Order.RiskCode.

         IF Order.RiskCode NE INPUT Order.RiskCode THEN 
            fSetOrderRiskCode(Order.OrderId,lcRiskCode).

         IF NOT lNew AND llDoEvent THEN RUN StarEventMakeModifyEvent ( lhOrder ).
      END.
     
      FIND CURRENT Order NO-LOCK.

      LEAVE.
   END.
END PROCEDURE.


PROCEDURE local-update-customer:

   DEF INPUT PARAMETER iiRole AS INT NO-UNDO.
   DEF INPUT PARAMETER lNew   AS LOG NO-UNDO.

   DEF VAR lcCurrHeader AS CHAR NO-UNDO.
   DEF VAR lcNewHeader  AS CHAR NO-UNDO.
   DEFINE VARIABLE llCustIdUpdateOK AS LOGICAL INITIAL FALSE NO-UNDO.
    
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
   END CASE.

   IF Order.StatusCode = "73" AND
      iiRole = {&ORDERCUSTOMER_ROWTYPE_MOBILE_POUSER}
   THEN llCustIDUpdateOK = TRUE.

   FIND FIRST OrderCustomer NO-LOCK WHERE
              OrderCustomer.Brand   = Syst.Var:gcBrand       AND
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
      Syst.Var:ufk[1] = (IF lcRight = "RW" AND 
         iiRole NE {&ORDERCUSTOMER_ROWTYPE_LOGISTICS} AND
         LOOKUP(Order.StatusCode,"20,21,31,73") > 0 THEN 7 ELSE 0)
      Syst.Var:ufk[5] = 0
      Syst.Var:ufk[8] = 8
      Syst.Var:ehto = 0
      ufkey = true.
      RUN Syst/ufkey.p.
                                                             
      IF Syst.Var:toimi = 8 then do:
         hide frame fCustomer NO-PAUSE.
         ac-hdr = lcCurrHeader.
         
         LEAVE.
      end.

      ELSE IF Syst.Var:toimi = 5 AND lNew   THEN DO:
  
         MESSAGE "Don't create here"
         VIEW-AS ALERT-BOX INFORMATION.
         
      END.
 
      ELSE IF Syst.Var:toimi = 1 AND lcRight = "RW" AND Syst.Var:ufk[1] NE 0 THEN
      REPEAT WITH FRAME fCustomer ON ENDKEY UNDO, LEAVE:
         
         Syst.Var:ehto = 9.
         RUN Syst/ufkey.p.
         
         lcOldAddressChk = 
            OrderCustomer.Address + 
            OrderCustomer.ZipCode +
            OrderCustomer.PostOffice.
         
         FIND CURRENT OrderCustomer EXCLUSIVE-LOCK.

         UPDATE 
         
         OrderCustomer.FirstName WHEN LOOKUP(Order.StatusCode,"20,21,31") > 0 OR llCustIDUpdateOK
         OrderCustomer.SurName1 WHEN LOOKUP(Order.StatusCode,"20,21,31") > 0 OR llCustIDUpdateOK
         OrderCustomer.SurName2 WHEN LOOKUP(Order.StatusCode,"20,21,31") > 0 OR llCustIDUpdateOK
         OrderCustomer.Company WHEN 
            OrderCustomer.Custidtype = "cif" and
            LOOKUP(Order.StatusCode,"20,21,31") > 0 and
            OrderCustomer.RowType = 1
         OrderCustomer.FoundationDate WHEN 
            OrderCustomer.Custidtype = "cif" and
            LOOKUP(Order.StatusCode,"20,21,31") > 0 AND
            OrderCustomer.RowType = 1
         OrderCustomer.Street WHEN LOOKUP(Order.StatusCode,"20,21,31") > 0 OR llCustIDUpdateOK
         OrderCustomer.BuildingNum WHEN LOOKUP(Order.StatusCode,"20,21,31") > 0 OR llCustIDUpdateOK
         OrderCustomer.AddressCompl WHEN LOOKUP(Order.StatusCode,"20,21,31") > 0 OR llCustIDUpdateOK
         
         OrderCustomer.ZipCode WHEN LOOKUP(Order.StatusCode,"20,21,31") > 0  OR llCustIDUpdateOK
         OrderCustomer.PostOffice WHEN LOOKUP(Order.StatusCode,"20,21,31") > 0 OR llCustIDUpdateOK
         
         OrderCustomer.CustIDType WHEN llCustIDUpdateOK
         OrderCustomer.CustID     WHEN llCustIDUpdateOK
         
         OrderCustomer.Region WHEN LOOKUP(Order.StatusCode,"31") > 0 OR llCustIDUpdateOK
         OrderCustomer.CustTitle WHEN LOOKUP(Order.StatusCode,"31") > 0 OR llCustIDUpdateOK
         OrderCustomer.Nationality WHEN LOOKUP(Order.StatusCode,"31") > 0 OR llCustIDUpdateOK
         OrderCustomer.MobileNumber WHEN LOOKUP(Order.StatusCode,"31") > 0 OR llCustIDUpdateOK
         OrderCustomer.FixedNumber WHEN LOOKUP(Order.StatusCode,"31") > 0 OR llCustIDUpdateOK
         OrderCustomer.Email WHEN LOOKUP(Order.StatusCode,"31") > 0 OR llCustIDUpdateOK
         OrderCustomer.Country WHEN LOOKUP(Order.StatusCode,"31") > 0 OR llCustIDUpdateOK
         OrderCustomer.BankCode WHEN LOOKUP(Order.StatusCode,"31") > 0 OR llCustIDUpdateOK
         OrderCustomer.Birthday WHEN LOOKUP(Order.StatusCode,"31") > 0 OR llCustIDUpdateOK
         OrderCustomer.Language WHEN LOOKUP(Order.StatusCode,"31") > 0 OR llCustIDUpdateOK
         WITH FRAME fCustomer EDITING:
            
            READKEY PAUSE liWaitKey.

            /* kick user out if nothing is done */
            IF LASTKEY = -1 AND TRANSACTION THEN DO:
               PAUSE 0.
               MESSAGE lcWaitMessage.
               PAUSE liWaitMessage NO-MESSAGE.
               IF LASTKEY NE -1 THEN NEXT.
               UNDO Action, LEAVE Action.
            END.
            
            Syst.Var:nap = keylabel(LASTKEY).
            
            IF Syst.Var:nap = "F9" AND 
               LOOKUP(FRAME-FIELD,"CustIDType,CustTitle,ZipCode") > 0
            THEN DO:

               IF FRAME-FIELD = "CustIDType" THEN DO:           
                  RUN Help/h-tmscodes.p(INPUT "Customer",    /* TableName */
                                       "CustIDType",  /* FieldName */
                                       "CustCare",  /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN DO:
                     DISPLAY lcCode @ OrderCustomer.CustIDType
                     WITH FRAME fCustomer.   
                  END.   
               END.
                  
               ELSE IF FRAME-FIELD = "CustTitle" THEN DO:
                  RUN Help/h-tmscodes.p(INPUT "Customer",    /* TableName */
                                       "Title",      /* FieldName */
                                       "CustCare",   /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN DO:
                     DISPLAY lcCode @ OrderCustomer.CustTitle
                     WITH FRAME fCustomer.   
                  END.   
               END. 

               ELSE IF FRAME-FIELD = "ZipCode" THEN DO:
                  ASSIGN Syst.Var:si-recid = ?
                         siirto   = "".
                  RUN Help/h-postcode.p.

                  /* several rows with same zipcode */
                  IF Syst.Var:si-recid NE ? THEN DO:
                     DISPLAY siirto @ OrderCustomer.ZipCode 
                        WITH FRAME fCustomer.
                     FIND PostCode WHERE RECID(PostCode) = Syst.Var:si-recid
                        NO-LOCK NO-ERROR.
                     IF AVAILABLE PostCode THEN DO:
                        DISPLAY PostCode.PostOffice @ OrderCustomer.PostOffice
                                PostCode.Region     @ OrderCustomer.Region
                                PostCode.Country    @ OrderCustomer.Country
                                PostCode.Country    @ OrderCustomer.Nationality
                        WITH FRAME fCustomer.

                     END.
                  END.      
               END.
                  
               Syst.Var:ehto = 9.
               RUN Syst/ufkey.p.
               NEXT. 
            END.

            ELSE IF LOOKUP(Syst.Var:nap,Syst.Var:poisnap) > 0 THEN DO WITH FRAME fCustomer:

               HIDE MESSAGE no-pause.

               IF FRAME-FIELD = "CustId" THEN DO:

                  IF NOT fChkCustID(INPUT INPUT OrderCustomer.CustIDType,
                                    INPUT INPUT OrderCustomer.CustId)
                  THEN DO:
                     MESSAGE "Invalid customer ID"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.

               ELSE IF FRAME-FIELD = "CustIDType" THEN DO:
                 
                  lcCode = Func.Common:mTMSCodeName("Customer",
                                            "CustIDType",
                                            INPUT INPUT FRAME fCustomer
                                                 OrderCustomer.CustIDType).
                  IF lcCode = "" THEN DO:
                     MESSAGE "Unknown ID type"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.

                  /* passport cannot always be used */
                  IF LOOKUP(OrderCustomer.CustIDType,"NIE,NIF,CIF") > 0 AND
                    INPUT FRAME fCustomer OrderCustomer.CustIDType = "Passport"
                  THEN DO:
                     MESSAGE "Normal ID type cannot be changed into passport"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
                  /* Person id cannot change to company id and vice versa  */
                  IF ( LOOKUP(OrderCustomer.CustIDType,"NIE,NIF,PASSPORT") > 0 AND
                       INPUT FRAME fCustomer OrderCustomer.CustIDType = "CIF" ) OR
                     ( OrderCustomer.CustIDType = "CIF" AND
                       LOOKUP(INPUT FRAME fCustomer OrderCustomer.CustIDType, "NIE,NIF,PASSPORT") > 0 )
                  THEN DO:
                     MESSAGE "Company ID type cannot be changed into Person ID type and vice versa"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.
             
            END.
            
            APPLY LASTKEY.
        
         END.   

         IF NOT fCheckCustomerData(BUFFER OrderCustomer) THEN UNDO Action, NEXT.
         
         LEAVE.
      END.   
      /* change is complete */
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrderCustomer).
      
      OrderCustomer.Address = OrderCustomer.Street .
      IF OrderCustomer.BuildingNum NE "" THEN 
         OrderCustomer.Address = OrderCustomer.Address + " " + OrderCustomer.BuildingNum .
      IF OrderCustomer.AddressCompl NE "" THEN 
         OrderCustomer.Address = OrderCustomer.Address + " " + OrderCustomer.AddressCompl .
     
      IF lcOldAddressChk NE (OrderCustomer.Address + OrderCustomer.ZipCode +
         OrderCustomer.PostOffice) THEN DO:
         ASSIGN
            OrderCustomer.AddressCodC = ""
            OrderCustomer.AddressCodP = ""
            OrderCustomer.AddressCodM = "".
      END.
      
   END. 


   HIDE FRAME fCustomer no-pause.

   FIND Current OrderCustomer NO-LOCK.

END PROCEDURE.

