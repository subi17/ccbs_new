/* order.i      2006/jp

   changes:     08.02.06/aam create user account for agr.customer
                14.02.06/aam assign firstname
                20.11.06/aam new db structure
*/
{Syst/commali.i}   
{Func/fwebuser.i}
{Func/fcustdata.i}
{Func/ftmrlimit.i}
{Func/forderstamp.i}

DEF BUFFER bUpdOrderCustomer FOR OrderCustomer.


FUNCTION fInvGroup RETURN CHARACTER
   (INPUT idtDate AS DATE).

   DEF VAR lcGroup AS CHAR NO-UNDO.
   
   IF      day(idtDate) < 5  THEN lcGroup = "INV1".
   ELSE if day(idtDate) < 9  THEN lcGroup = "INV2".
   ELSE if day(idtDate) < 12 THEN lcGroup = "INV3".
   ELSE if day(idtDate) < 16 THEN lcGroup = "INV4".      
   ELSE if day(idtDate) < 20 THEN lcGroup = "INV5".
   ELSE if day(idtDate) < 24 THEN lcGroup = "INV6".
   ELSE if day(idtDate) < 28 THEN lcGroup = "INV7".
   ELSE  lcGroup = "INV8".

   RETURN lcGroup.            

END.

FUNCTION fMakeCustomer RETURNS LOGICAL
  (INPUT iiOrder   AS INT,
   INPUT iiTarget  AS INT,
   INPUT ilNewCust AS LOG,
   INPUT iiCustNum AS INT):

   DEF BUFFER AgrCust  FOR Customer.
   DEF BUFFER InvCust  FOR Customer.
   DEF BUFFER UserCust FOR Customer.

   FIND Customer WHERE
        Customer.CustNum = iiCustNum EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE Customer THEN RETURN FALSE.     
   
   /* iiTarget 1 = agr.cust
               2 = inv.cust
               3 = user cust
   */
   FIND FIRST OrderCustomer EXCLUSIVE-LOCK WHERE
              OrderCustomer.Brand   = gcBrand AND
              OrderCustomer.OrderID = iiOrder AND
              OrderCustomer.RowType = iiTarget NO-ERROR.
   IF NOT AVAILABLE OrderCustomer THEN RETURN FALSE. 

   ASSIGN
                         
   Customer.CustName     = TRIM(OrderCustomer.Surname1)
   Customer.SurName2     = TRIM(OrderCustomer.SurName2)
   Customer.CompanyName  = TRIM(OrderCustomer.Company)
   Customer.HonTitle     = OrderCustomer.CustTitle
   Customer.FirstName    = TRIM(OrderCustomer.FirstName)
   Customer.SearchName   = SUBSTR(Customer.CustName + 
                                  Customer.FirstName,1,8)
   Customer.Address      = OrderCustomer.Address
   Customer.ZipCode      = OrderCustomer.ZipCode
   Customer.PostOffice   = OrderCustomer.PostOffice
   Customer.Country      = OrderCustomer.Country
   Customer.BankAcc      = OrderCustomer.BankCode
   Customer.Region       = OrderCustomer.Region
   Customer.Nationality  = OrderCustomer.Nationality
   Customer.Language     = INTEGER(OrderCustomer.Language)
   Customer.Email        = OrderCustomer.EMail
   Customer.Phone        = OrderCustomer.FixedNumber
   Customer.SMSNumber    = OrderCustomer.MobileNumber
   Customer.CustIDType   = OrderCustomer.CustIDType
   Customer.OrgId        = /* IF OrderCustomer.CustIDType = "Passport" AND
                              OrderCustomer.Passport > ""
                           THEN OrderCustomer.Passport
                           ELSE 
                           */
                           OrderCustomer.CustId
   Customer.BirthDay     = OrderCustomer.BirthDay
   Customer.Sex          = OrderCustomer.Sex
   
   Customer.ExtInvRef    = OrderCustomer.ExtInvRef
   Customer.AddressCodP  = OrderCustomer.AddressCodP
   Customer.AddressCodC  = OrderCustomer.AddressCodC
   Customer.FoundationDate = OrderCustomer.FoundationDate

   /* marketing values */
   Customer.DirMarkSMS   = OrderCustomer.OperSMSMarketing
   Customer.DirMarkEmail = OrderCustomer.OperEMailMarketing
   Customer.DirMarkPOST  = OrderCustomer.OperPostMarketing
   Customer.OutMarkSMS   = OrderCustomer.OutSMSMarketing
   Customer.OutMarkEmail = OrderCustomer.OutEMailMarketing
   Customer.OutMarkPOST  = OrderCustomer.OutPostMarketing.
   
   IF iiTarget = 1 THEN
      Customer.InvGroup = fDefInvGroup(OrderCustomer.Region).
   
   IF ilNewCust THEN DO:
      ASSIGN
         OrderCustomer.CustNum = iiCustNum
         Customer.InvCode      = (IF DAY(today) > 15 THEN 12 ELSE 11).

      /* category according to id type */
      Customer.Category = OrderCustomer.Category.      
      FIND FIRST CustCat NO-LOCK WHERE
         CustCat.Brand = gcBrand AND
         LOOKUP(OrderCustomer.CustIDType,CustCat.CustIDType) > 0 AND
         CustCat.SelfEmployed = OrderCustomer.SelfEmployed NO-ERROR.  
      IF NOT AVAILABLE CustCat THEN    
      FIND FIRST CustCat NO-LOCK WHERE
         CustCat.Brand = gcBrand AND
         LOOKUP(OrderCustomer.CustIDType,CustCat.CustIDType) > 0 NO-ERROR.
      IF AVAIL CustCat THEN
         Customer.Category = CustCat.Category.

      IF iiTarget = 1 THEN DO:
         /* new user account */
         create_account(Customer.CustNum,?,?).
      END.   
         
      ELSE DO:

         FOR FIRST bUpdOrderCustomer NO-LOCK WHERE
                   bUpdOrderCustomer.Brand   = gcBrand AND
                   bUpdOrderCustomer.OrderID = iiOrder AND
                   bUpdOrderCustomer.RowType = 1,
             FIRST AgrCust NO-LOCK WHERE
                   AgrCust.CustNum = bUpdOrderCustomer.CustNum:
            ASSIGN Customer.InvGroup = AgrCust.InvGroup
                   Customer.AgrCust  = AgrCust.CustNum.
            
            /* user */            
            IF iiTarget = 3 THEN Customer.InvCust = AgrCust.CustNum.
         END.

         IF Customer.InvGroup = "" THEN    
            Customer.InvGroup = fDefInvGroup(OrderCustomer.Region).
      END.
      
      /* default counter limits; for all, also prepaids */
      fTMRLimit2Customer(Customer.CustNum).
               
      fMarkOrderStamp(iiOrder,
                      "Delivery",
                      0.0).
   END.
   
   RETURN TRUE.

END.


