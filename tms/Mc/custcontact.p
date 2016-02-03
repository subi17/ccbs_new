/* ------------------------------------------------------
  MODULE .......: custcontact.p
  FUNCTION .....: View corporate customer's contact data
  APPLICATION ..: TMS
  AUTHOR .......: as
  CREATED ......: 02/2009
  MODIFIED .....: 
  Version ......: xfera
  ------------------------------------------------------ */
{Syst/commali.i}
{Syst/tmsconst.i}

DEF INPUT PARAMETER iiCustNum  AS INT NO-UNDO.
DEF INPUT PARAMETER iiCustType AS INT NO-UNDO.

DEFINE VARIABLE lcLanguage AS CHARACTER NO-UNDO. 

FIND Customer WHERE Customer.CustNum = iiCustNum NO-LOCK.
IF NOT AVAIL Customer THEN DO:
   MESSAGE "Customer was not found" VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.
      
FIND CustContact WHERE
     CustContact.Brand = gcBrand AND
     CustContact.Custnum = Customer.Custnum AND
     CustContact.CustType = iiCustType NO-LOCK NO-ERROR.

IF NOT AVAIL CustContact THEN DO:
   MESSAGE ({&MSG_DATA_NOT_FOUND}) VIEW-AS ALERT-BOX.
   RETURN.
END.
   
FIND Language WHERE Language.Language = CustContact.Language 
NO-LOCK NO-ERROR.
IF AVAIL Language THEN ASSIGN lcLanguage = Language.LangName.
ELSE lcLanguage = "".

FORM
   CustContact.CustIdType  LABEL "ID Type" COLON 15 SKIP
   CustContact.OrgId       LABEL "Customer ID" COLON 15 SKIP
   CustContact.HonTitle    LABEL "Title" COLON 15 SKIP
   CustContact.FirstName   LABEL "Firstname" COLON 15 SKIP
   CustContact.CustName    LABEL "Surname 1" COLON 15 SKIP
   CustContact.SurName2    LABEL "Surname 2" COLON 15 SKIP
   CustContact.SMSNumber   LABEL "Mob. Number" COLON 15 SKIP
   CustContact.Email       LABEL "Email" COLON 15  SKIP
   CustContact.Address     LABEL "Address" COLON 15 SKIP
   CustContact.ZipCode     LABEL "Zip Code" COLON 15 FORMAT "x(60)" SKIP
   CustContact.PostOffice  LABEL "City" COLON 15 SKIP
   CustContact.Region      LABEL "Region" COLON 15 SKIP
   CustContact.Country     LABEL "Country" COLON 15 SKIP
   CustContact.Nationality LABEL "Nationality" COLON 15 SKIP
   CustContact.Language    LABEL "Language" COLON 15  lcLanguage NO-LABEL SKIP

   WITH ROW 3 OVERLAY SIDE-LABELS CENTERED 
        TITLE " CONTACT PERSON, CUSTOMER " + STRING(iiCustNum) + " " 
        FRAME fCustContact.

VIEW FRAME fCustContact.
PAUSE 0 NO-MESSAGE.

lCustMark:
REPEAT WITH FRAME fCustContact ON ENDKEY UNDO lCustMark, NEXT lCustMark:

   PAUSE 0.
   DISPLAY 
      CustContact.CustIdType
      CustContact.OrgId
      CustContact.HonTitle
      CustContact.FirstName
      CustContact.CustName
      CustContact.SurName2
      CustContact.Address
      CustContact.ZipCode
      CustContact.PostOffice
      CustContact.Region
      CustContact.Country
      CustContact.Nationality
      CustContact.Email
      CustContact.SMSNumber
      CustContact.Language lcLanguage.

   ASSIGN
      ufk   = 0  
      ufk[8]= 8 
      ehto = 0.
   RUN ufkey.

   IF toimi = 8 THEN LEAVE.

END. /* lCustMark */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCustContact NO-PAUSE.    

