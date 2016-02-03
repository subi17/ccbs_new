 /* ------------------------------------------------------
  MODULE .......: ownerinv.p
  FUNCTION .....: inv.customer data for changing agreement customer
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 23.02.06
  MODIFIED .....: 
  Version ......: TF
  ------------------------------------------------------ */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'MobSub'}
{Func/cparam2.i}
{Func/timestamp.i}
{Func/fctserval.i}
{Func/fctchange.i}
{Func/fmakemsreq.i}
{Func/fcustdata.i}
{Func/finvtxt.i}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhMsRequest AS HANDLE NO-UNDO.
   lhMsRequest = BUFFER MsRequest:HANDLE.
   RUN StarEventInitialize(lhMsRequest).
END.


DEF INPUT PARAMETER iiRequest AS INT  NO-UNDO.
DEF INPUT PARAMETER icAction  AS CHAR NO-UNDO.  /* new, change, view */

DEF NEW shared VAR siirto AS CHAR.

DEF VAR ufkey          AS LOG  NO-UNDO.
DEF VAR lcError        AS CHAR NO-UNDO. 
DEF VAR llOk           AS LOG  NO-UNDO.
DEF VAR lcNewCategory  AS CHAR NO-UNDO. 
DEF VAR lcCountName    AS CHAR NO-UNDO.
DEF VAR lcNewLast      AS CHAR NO-UNDO.
DEF VAR lcNewFirst     AS CHAR NO-UNDO.
DEF VAR lcNewCOName    AS CHAR NO-UNDO.
DEF VAR lcNewAddress   AS CHAR NO-UNDO.
DEF VAR lcNewZipCode   AS CHAR NO-UNDO.
DEF VAR lcNewPost      AS CHAR NO-UNDO.
DEF VAR lcNewCountry   AS CHAR NO-UNDO.
DEF VAR lcNewCountName AS CHAR NO-UNDO. 
DEF VAR lcNewEMail     AS CHAR NO-UNDO.
DEF VAR lcNewTel       AS CHAR NO-UNDO.
DEF VAR lcCatName      AS CHAR NO-UNDO.
DEF VAR lcNewCatName   AS CHAR NO-UNDO.
DEF VAR liRequest      AS INT  NO-UNDO. 
DEF VAR ldChgStamp     AS DEC  NO-UNDO.
DEF VAR lcCurrHeader   AS CHAR NO-UNDO.
DEF VAR lcNewHeader    AS CHAR NO-UNDO.
DEF VAR lcMainHeader   AS CHAR NO-UNDO. 
DEF VAR liNewCust1     AS INT  NO-UNDO.
DEF VAR llUpdCustNum   AS LOG  NO-UNDO. 
DEF VAR lcAgrCategory  AS CHAR NO-UNDO.
DEF VAR lcAgrCountry   AS CHAR NO-UNDO.
DEF VAR liAgrCust      AS INT  NO-UNDO.
DEF VAR liCnt          AS INT  NO-UNDO.
DEF VAR llSameUser     AS LOG  NO-UNDO. 
DEF VAR llAgrIsUser    AS LOG  NO-UNDO. 
DEF VAR lcCustName     AS CHAR NO-UNDO.

DEF BUFFER bNewCust  FOR Customer.
DEF BUFFER bCustomer FOR Customer.

FORM
   SKIP(1) 
   liNewCust1 COLON 20
      LABEL "Customer Nbr" 
      FORMAT ">>>>>>>>"
      HELP "Customer nbr, 0=create a new one"
   SKIP
   
   lcNewCategory COLON 20
      LABEL "Category" 
      FORMAT "X(4)"
   lcNewCatName 
      NO-LABEL
      FORMAT "X(20)" 
   SKIP
      
   lcNewLast COLON 20
      LABEL "Lastname"
      FORMAT "X(30)"
      HELP "Lastname for private customer or company name"
      SKIP

   lcNewFirst COLON 20
      LABEL "FirstName"
      FORMAT "X(30)"
      HELP "Firstname"
   SKIP
      
   lcNewCOName COLON 20
      LABEL "C/O"
      FORMAT "X(30)"
      HELP "C/O"
      SKIP

   lcNewAddress COLON 20
      LABEL "Address"
      FORMAT "X(30)"
      HELP "Address"
   SKIP
      
   lcNewZipCode COLON 20
      LABEL "Zip Code"
      FORMAT "X(8)"
      HELP "Zip code"
   SKIP

   lcNewPost COLON 20
      LABEL "Post Office"
      FORMAT "X(30)"
      HELP "Post office"
   SKIP

   lcNewCountry COLON 20
      LABEL "Country"
      FORMAT "X(2)"
      HELP "Country"
   lcNewCountName
      NO-LABEL
      FORMAT "X(20)"
   SKIP

   lcNewEMail COLON 20
      LABEL "Email"
      FORMAT "X(45)"
      HELP "Email address"
   SKIP
      
   lcNewTel COLON 20
      LABEL "Tel."
      FORMAT "X(30)"
      HELP "Tel.nbr, for SMS and other contacts"

   llSameUser COLON 20
      LABEL "Use As User"
      HELP "Invoice customer is also the user"
      FORMAT "Yes/No"
   SKIP(1)

WITH ROW 3 OVERLAY SIDE-LABELS CENTERED 
     TITLE " " + lcMainHeader + " "  FRAME fCriter.


FUNCTION fRequestValues RETURNS LOGIC:

   IF NUM-ENTRIES(MsRequest.ReqCParam2,";") > 1 THEN ASSIGN 
      lcNewLast      = ENTRY(1,MsRequest.ReqCParam2,";")
      lcNewFirst     = ENTRY(2,MsRequest.ReqCParam2,";")
      lcNewCOName    = ENTRY(3,MsRequest.ReqCParam2,";")
      lcNewAddress   = ENTRY(4,MsRequest.ReqCParam2,";")
      lcNewZipCode   = ENTRY(5,MsRequest.ReqCParam2,";")
      lcNewPost      = ENTRY(6,MsRequest.ReqCParam2,";")
      lcNewCountry   = ENTRY(7,MsRequest.ReqCParam2,";")
      lcNewEMail     = ENTRY(8,MsRequest.ReqCParam2,";")
      lcNewTel       = ENTRY(9,MsRequest.ReqCParam2,";")
      lcNewCategory  = ENTRY(10,MsRequest.ReqCParam2,";").

   ASSIGN 
      liNewCust1     = MsRequest.ReqIParam2
      liAgrCust      = MsRequest.ReqIParam1
      lcAgrCountry   = ENTRY(7,MsRequest.ReqCParam1,";")
      lcAgrCategory  = ENTRY(10,MsRequest.ReqCParam1,";")
      llAgrIsUser    = (SUBSTRING(MsRequest.ReqCParam4,3,1) = "1")
      llSameUser     = (SUBSTRING(MsRequest.ReqCParam4,3,1) = "2").
      
END FUNCTION.
 

IF icAction = "new"
THEN ufkey = FALSE.
ELSE ufkey = TRUE.

FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest THEN DO:
   MESSAGE "Unknown request"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

lcMainHeader = "NEW INVOICE CUSTOMER FOR " + MsRequest.CLI .

fRequestValues().

PAUSE 0.
VIEW FRAME fCriter.

ChooseInvCust:
REPEAT WITH FRAME fCriter ON ENDKEY UNDO ChooseInvCust, NEXT ChooseInvCust:

   ASSIGN lcNewCountName = ""
          lcNewCatName   = "".
          
   IF lcNewCountry > "" THEN DO:
      FIND Country WHERE Country.Country = lcNewCountry NO-LOCK NO-ERROR.
      IF AVAILABLE Country THEN lcNewCountName = Country.COName.
   END.   
   
   IF lcNewCategory > "" THEN DO:
      FIND CustCat WHERE 
           CustCat.Brand    = gcBrand AND
           CustCat.Category = lcNewCategory NO-LOCK NO-ERROR.
      IF AVAILABLE CustCat THEN lcNewCatName = CustCat.CatName.
   END. 

   PAUSE 0.

   DISPLAY 
      liNewCust1
      lcNewCategory
      lcNewCatName
      lcNewLast
      lcNewFirst
      lcNewCOName
      lcNewAddress
      lcNewZipCode
      lcNewPost
      lcNewCountry
      lcNewCountName
      lcNewEMail
      lcNewTel
      llSameUser
   WITH FRAME fCriter.

   IF liNewCust1 = 0 THEN DISPLAY "NEW" @ liNewCust1 WITH FRAME fCriter.

   llUpdCustNum = FALSE.
   IF icAction = "change" AND liAgrCust > 0 THEN DO:
      IF MsRequest.ReqStatus >= 12 AND MsRequest.ReqStatus <= 13 
      THEN llUpdCustNum = TRUE.
   END. 
   
   IF ufkey THEN DO:
      ASSIGN
         ufk    = 0 
         ufk[1] = 7  
         ufk[8] = 2 
         ehto   = 0.

      IF liNewCust1 = 0 THEN ufk[3] = 1056.   
      
      IF icAction = "view" THEN ASSIGN 
         ufk[1] = 0
         ufk[3] = 0.

      RUN ufkey.
   END.

   ELSE ASSIGN toimi = 1  
               ufkey = TRUE.

   /* update new inv.customer */ 
   IF toimi = 1 THEN DO:

      REPEAT WITH FRAME fCriter ON ENDKEY UNDO, LEAVE:
         
         ehto = 9. RUN ufkey.p.
         
         UPDATE liNewCust1 WHEN llUpdCustNum
         WITH FRAME fCriter EDITING:

            READKEY.
            
            IF KEYLABEL(LASTKEY) = "F9" AND
               LOOKUP(FRAME-FIELD,"liNewCust1") > 0 AND
               liAgrCust > 0 
            THEN DO:

               IF FRAME-FIELD = "liNewCust1" THEN DO:

                  RUN h-customer (liAgrCust,
                                  "agrcust",
                                  "all").
                   
                  IF siirto NE ? THEN DO:
                     liNewCust1 = INTEGER(siirto) NO-ERROR.
                     DISPLAY liNewCust1 WITH FRAME fCriter.
                  END.
                   
               END. 

               ehto = 9.
               RUN ufkey.
               NEXT.
            END. 
 
            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO:
            END.
            
            APPLY LASTKEY.
           
         END.
         
         IF liNewCust1 = 0 THEN DO:
         
            DISPLAY "NEW" @ liNewCust1 WITH FRAME fCriter.
            
            IF lcNewCategory = "" THEN lcNewCategory = lcAgrCategory.
            IF lcNewCountry  = "" THEN lcNewCountry  = lcAgrCountry.
         END.

         ELSE IF liNewCust1 > 0 THEN DO:
            FIND bNewCust WHERE bNewCust.CustNum = liNewCust1 NO-LOCK NO-ERROR.
            IF NOT AVAILABLE bNewCust THEN DO:
               MESSAGE "Unknown customer"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
            END.

            IF bNewCust.AgrCust NE liAgrCust THEN DO:
               MESSAGE "This customer has a different agreement customer"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
            END.

            ASSIGN lcNewCategory = bNewCust.Category
                   lcNewLast     = bNewCust.CustName
                   lcNewFirst    = bNewCust.FirstName
                   lcNewCOName   = bNewCust.COName
                   lcNewAddress  = bNewCust.Address
                   lcNewZipCode  = bNewCust.ZipCode
                   lcNewPost     = bNewCust.PostOffice
                   lcNewCountry  = bNewCust.Country
                   lcNewEMail    = bNewCust.EMail
                   lcNewTel      = bNewCust.SMSNumber.

            DISPLAY lcNewCategory
                    lcNewCatName
                    lcNewLast
                    lcNewFirst
                    lcNewCOName
                    lcNewAddress
                    lcNewZipCode
                    lcNewPost
                    lcNewCountry
                    lcNewCountName
                    lcNewEMail
                    lcNewTel 
                    WITH FRAME fCriter.
         END. 
                 
         UPDATE lcNewCategory  WHEN liNewCust1 = 0
                lcNewLast      WHEN liNewCust1 = 0
                lcNewFirst     WHEN liNewCust1 = 0
                lcNewCOName    WHEN liNewCust1 = 0
                lcNewAddress   WHEN liNewCust1 = 0
                lcNewZipCode   WHEN liNewCust1 = 0
                lcNewPost      WHEN liNewCust1 = 0
                lcNewCountry   WHEN liNewCust1 = 0
                lcNewEMail     WHEN liNewCust1 = 0
                lcNewTel       WHEN liNewCust1 = 0
                llSameUser     WHEN NOT llAgrIsUser
         WITH FRAME fCriter EDITING:
            
            READKEY.

            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO:
                  
               IF FRAME-FIELD = "lcNewCategory" THEN DO:
                  FIND CustCat WHERE 
                       CustCat.Brand    = gcBrand AND
                       CustCat.Category = INPUT lcNewCategory 
                  NO-LOCK NO-ERROR.

                  IF NOT AVAIL CustCat THEN DO:
                     MESSAGE "Unknown category"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
                     
                  lcNewCatName = CustCat.CatName.
                  DISPLAY lcNewCatName WITH FRAME fCriter.
               
               END. 
                  
               ELSE IF FRAME-FIELD = "lcNewCountry" THEN DO:

                  FIND Country WHERE 
                       Country.Country = INPUT lcNewCountry
                  NO-LOCK NO-ERROR.
                     
                  IF NOT AVAIL Country THEN DO:
                     MESSAGE "Unknown country"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
                     
                  lcNewCountName = Country.COName.
                  DISPLAY lcNewCountName WITH FRAME fCriter.
               END.

               ELSE IF FRAME-FIELD = "lcNewFirst" THEN DO:
                  
                  IF INPUT lcNewFirst = "" AND
                     LOOKUP(INPUT lcNewCategory,"1,3") > 0
                  THEN DO:
                     MESSAGE "First name is mandatory"
                     VIEW-AS ALERT-BOX.
                     NEXT.
                  END.

                  ELSE IF INPUT lcNewFirst > "" AND
                       INPUT lcNewCategory = "2"
                  THEN DO:
                     MESSAGE "First name should be empty for companys"
                     VIEW-AS ALERT-BOX.
                     NEXT.
                  END.
                  
               END.

               ELSE IF FRAME-FIELD = "lcNewZipCode" THEN DO:
                  IF NOT fChkZipCode(INPUT INPUT lcNewZipCode,    
                                     INPUT INPUT lcNewCountry)
                  THEN DO:
                     MESSAGE "Invalid zipcode"
                     VIEW-AS ALERT-BOX.
                     NEXT.
                  END.
               END. 
                  
            END. 
                
            APPLY LASTKEY.                              
         END.
        
         LEAVE.
      END.

   END.
   
   /* copy name and address from current invoice customer  */
   ELSE IF toimi = 3 THEN DO:
   
      FIND MobSub WHERE MobSub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MobSub THEN DO:
         MESSAGE "Subscription was not found"
         VIEW-AS ALERT-BOX ERROR.
      END.   
      
      FIND Customer WHERE Customer.CustNum = MobSub.InvCust NO-LOCK NO-ERROR.
      lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                    BUFFER Customer).
      
      llOk = (lcNewLast = "").
      
      MESSAGE "Current invoice customer is:" SKIP
              lcCustName  SKIP
              RIGHT-TRIM(Customer.Address) SKIP
              Customer.ZipCode + " " + Customer.PostOffice SKIP(1)
              "Copy these values to new invoice customer ?" 
      VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO
      TITLE " COPY "
      SET llOk.
      
      IF llOk THEN ASSIGN 
         lcNewCategory = Customer.Category
         lcNewLast     = Customer.CustName
         lcNewFirst    = Customer.FirstName
         lcNewCOName   = Customer.COName
         lcNewAddress  = Customer.Address
         lcNewZipCode  = Customer.ZipCode
         lcNewPost     = Customer.PostOffice
         lcNewCountry  = Customer.Country
         lcNewEMail    = Customer.EMail
         lcNewTel      = Customer.SMSNumber.
   END. 
   
   ELSE IF toimi = 8 THEN DO:

      /* update msrequest */
      IF icAction = "change" AND AVAILABLE MsRequest THEN DO:
      
         IF lcNewLast = "" THEN DO:
            llOk = FALSE.
            MESSAGE "Data for invoice customer has not been entered." SKIP
                    "Are You sure You want to continue?"
            VIEW-AS ALERT-BOX QUESTION
            BUTTONS YES-NO
            TITLE " MISSING DATA " 
            SET llOK.
            
            IF NOT llOk THEN NEXT.
         END.
            
         FIND CURRENT MsRequest EXCLUSIVE-LOCK.
         
         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMsRequest).
         
         ASSIGN MsRequest.ReqIParam2 = liNewCust1 
                MsRequest.ReqCParam2 = lcNewLast     + ";" +
                                       lcNewFirst    + ";" +
                                       lcNewCOName   + ";" +
                                       lcNewAddress  + ";" + 
                                       lcNewZipCode  + ";" +
                                       lcNewPost     + ";" +
                                       lcNewCountry  + ";" +
                                       lcNewEMail    + ";" +
                                       lcNewTel      + ";" +
                                       lcNewCategory.

         IF llSameUser THEN SUBSTRING(MsRequest.ReqCParam4,3,1) = "2".
         ELSE DO:
            IF SUBSTRING(MsRequest.ReqCParam4,3,1) = "2" THEN 
               SUBSTRING(MsRequest.ReqCParam4,3,1) = "3".
         END.
                                        
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMsRequest).
      
         RELEASE MsRequest.
      END. 
      
      LEAVE ChooseInvCust.
   END.

END. /* ChooseInvCust */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCriter NO-PAUSE.    

