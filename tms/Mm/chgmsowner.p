 /* ------------------------------------------------------
  MODULE .......: chgmsowner.p
  FUNCTION .....: change agreement customer for subscription
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 20.02.06
  MODIFIED .....: 27.11.06/aam inv.customer update not allowed
  Version ......: 
  ------------------------------------------------------ */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'MobSub'}
{Func/cparam2.i}
{Func/fcustdata.i}
{Func/finvtxt.i}
{Syst/eventval.i}
{Func/barrfunc.i}
{Func/fbankdata.i}
{Mm/msagrcustchg.i}
{Func/fcustchangereq.i}
{Func/fuserright.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMsRequest AS HANDLE NO-UNDO.
   lhMsRequest = BUFFER MsRequest:HANDLE.
   RUN StarEventInitialize(lhMsRequest).
END.


DEF INPUT PARAMETER iiMsSeq   AS INT  NO-UNDO. 
DEF INPUT PARAMETER iiRequest AS INT  NO-UNDO.
DEF INPUT PARAMETER icAction  AS CHAR NO-UNDO.  /* new, change, view */

DEF NEW shared VAR siirto AS CHAR.

DEF VAR ufkey          AS LOG  NO-UNDO.
DEF VAR lcError        AS CHAR NO-UNDO. 
DEF VAR llOk           AS LOG  NO-UNDO.
DEF VAR lcNewLast      AS CHAR NO-UNDO.
DEF VAR lcNewFirst     AS CHAR NO-UNDO.
DEF VAR lcNewCOName    AS CHAR NO-UNDO.
DEF VAR lcNewAddress   AS CHAR NO-UNDO.
DEF VAR lcNewZipCode   AS CHAR NO-UNDO.
DEF VAR lcNewPost      AS CHAR NO-UNDO.
DEF VAR lcNewCountry   AS CHAR NO-UNDO.
DEF VAR lcNewEMail     AS CHAR NO-UNDO.
DEF VAR ldtChgDate     AS DATE NO-UNDO.
DEF VAR liChgHour      AS INT  NO-UNDO. 
DEF VAR liChgMin       AS INT  NO-UNDO.
DEF VAR liTime         AS INT  NO-UNDO. 
DEF VAR liRequest      AS INT  NO-UNDO. 
DEF VAR ldChgStamp     AS DEC  NO-UNDO.
DEF VAR lcCurrHeader   AS CHAR NO-UNDO.
DEF VAR lcNewHeader    AS CHAR NO-UNDO.
DEF VAR lcMainHeader   AS CHAR NO-UNDO EXTENT 2.
DEF VAR liNewCust1     AS INT  NO-UNDO.
DEF VAR lcChanged      AS CHAR NO-UNDO.
DEF VAR liChooseCust   AS INT  NO-UNDO. 
DEF VAR lcNewOrgID     AS CHAR NO-UNDO. 
DEF VAR liContrTxt     AS INT  NO-UNDO. 
DEF VAR llUpdCustNum   AS LOG  NO-UNDO. 
DEF VAR llUpdCustData  AS LOG  NO-UNDO.
DEF VAR llUpdDate      AS LOG  NO-UNDO. 
DEF VAR llUpdTime      AS LOG  NO-UNDO. 
DEF VAR llSameInvCust  AS LOG  NO-UNDO.
DEF VAR llSameUser     AS LOG  NO-UNDO.
DEF VAR lcVRK          AS CHAR NO-UNDO.
DEF VAR lcSAT          AS CHAR NO-UNDO. 
DEF VAR liQty          AS INT  NO-UNDO.
DEF VAR lcChooseData   AS CHAR NO-UNDO EXTENT 3. 
DEF VAR ldActTime      AS DEC  NO-UNDO. 
DEF VAR llCreateFees   AS LOG  NO-UNDO. 
DEF VAR liCurrentUser  AS INT  NO-UNDO.
DEF VAR ldeFee         AS DEC  NO-UNDO.

DEF VAR lcRegion        AS CHAR NO-UNDO.
DEF VAR lcSalesman      AS CHAR NO-UNDO.
DEF VAR lcNewCustIdType AS CHAR NO-UNDO.
DEF VAR lcNewCustId     AS CHAR NO-UNDO.
DEF VAR ldaNewBirthday  AS DATE no-UNDO.
DEF VAR lcNewTitleLabel AS CHAR NO-UNDO.
DEF VAR lcNewTitle      AS CHAR NO-UNDO.
DEF VAR lcNewSurname2   AS CHAR NO-UNDO.
DEF VAR lcNewCompanyname AS CHAR NO-UNDO.
DEF VAR lcNewRegion     AS CHAR NO-UNDO.
DEF VAR lcNewRegionName AS CHAR NO-UNDO.
DEF VAR liNewLanguage   AS INT  NO-UNDO.
DEF VAR llNewPostHelp   AS LOG  NO-UNDO.
DEF VAR lcNewBirthday   AS CHAR NO-UNDO.
DEF VAR lcCode          AS CHAR NO-UNDO.
DEF VAR llReady         AS LOG  NO-UNDO.
DEF VAR liActDate       AS INT  NO-UNDO.
DEF VAR ldaMaxDate      AS DATE NO-UNDO.
DEF VAR llUpdBankAcc    AS LOG  NO-UNDO.
DEF VAR lcNewBankAcc    AS CHAR NO-UNDO.
DEF VAR llPreActivated  AS LOG  NO-UNDO.
DEF VAR lcNewNationality AS CHAR NO-UNDO.
DEF VAR ldaBirthDay      AS DATE NO-UNDO.
DEF VAR lcSmsnumber     AS CHAR NO-UNDO.
DEF VAR lcPhone         AS CHAR NO-UNDO.
DEF VAR llDirMarkSMS    AS LOG NO-UNDO.
DEF VAR llDirMarkEmail  AS LOG NO-UNDO.
DEF VAR llDirMarkPost   AS LOG NO-UNDO.
DEF VAR llOutMarkSMS    AS LOG NO-UNDO.
DEF VAR llOutMarkEmail  AS LOG NO-UNDO.
DEF VAR llOutMarkPost   AS LOG NO-UNDO.
DEF VAR lcAddressCodC   AS CHAR NO-UNDO.
DEF VAR lcAddressCodP   AS CHAR NO-UNDO.
DEF VAR lcCIFAgrCustIDType AS CHAR NO-UNDO. 
DEF VAR lcCIFAgrCustID AS CHAR NO-UNDO. 
DEF VAR lcAddressCodM   AS CHAR NO-UNDO. 
DEF VAR liDelType       AS INT NO-UNDO.
DEF VAR lcPro           AS CHAR NO-UNDO.

DEF BUFFER bNewCust     FOR Customer.
DEF BUFFER bCustomer    FOR Customer.
DEF BUFFER bCurrentCust FOR Customer.
DEF BUFFER bMobSub      FOR MobSub.

FORM

   MobSub.MsSeq AT 2
         LABEL "Subscr.ID .." 
   SKIP
   MobSub.CLI AT 2 
         LABEL "MSISDN......"
   SKIP
   "----------------------------------" AT 2
   SKIP
   bCurrentCust.CustNum AT 2
         LABEL "Cust. Number"
         FORMAT ">>>>>>>>9"
         SKIP 
   bCurrentCust.CustIdType AT 2 
         LABEL "Customer ID "
         FORMAT "X(8)"
   bCurrentCust.OrgId 
         NO-LABEL 
         FORMAT "X(13)"
         SKIP
   
   ldaBirthday
         LABEL "Birthday/FD." AT 2
         FORMAT "99-99-9999"
         SKIP
   
   bCurrentCust.FirstName AT 2          
         LABEL "FirstName .."
         SKIP
   
   bCurrentCust.CustName AT 2 
         LABEL "Surname 1 .."
         FORMAT "x(22)"
         SKIP
   
   bCurrentCust.Surname2 AT 2
         LABEL "Surname 2 .."
         FORMAT "X(22)"
         SKIP
   
   bCurrentCust.Companyname AT 2
         LABEL "Company Name"
         FORMAT "X(22)"
         SKIP
   
   bCurrentCust.COName AT 2
         LABEL "C/O ........"
         FORMAT "X(22)"
         SKIP
   
   bCurrentCust.Address AT 2
         LABEL "Address ...."
         FORMAT "X(22)"
         SKIP
   
   bCurrentCust.ZipCode AT 2
         FORMAT "X(5)"
         LABEL "Zip Code ..."
      bCurrentCust.PostOffice
         FORMAT "x(16)"
         NO-LABEL
         SKIP
   
   bCurrentCust.Region AT 2
         LABEL "Region ....."
         FORMAT "X(2)"
         lcRegion
         NO-LABEL
         FORMAT "X(20)"
         SKIP
   
   bCurrentCust.Country AT 2
         LABEL "Country/Nat."
         FORMAT "X(2)"
         "/"
      bCurrentCust.Nationality
         NO-LABEL
         FORMAT "X(2)"
         SPACE(2)
      bCurrentCust.Language
            LABEL "Language"
            FORMAT ">9" 
      SKIP      
   
   bCurrentCust.EMail AT 2
         LABEL "Email ......"
         FORMAT "X(22)" 
         SKIP
         
   bCurrentCust.BankAcc AT 2
        LABEL "Bank Acc.."
        FORMAT "X(24)"

WITH ROW 1 OVERLAY SIDE-LABELS COL 1
     TITLE " " + lcCurrHeader + " " 
     FRAME fOldCriter.

FORM
   ldtChgDate AT 2 
         FORMAT "99-99-9999" 
         LABEL "Change Date"
         HELP "Date when change is scheduled to happen"
   liChgHour 
      NO-LABEL
      HELP "Time for change (hour)"
      FORMAT "99"
      AUTO-RETURN
      SPACE(0)
      ":"
      SPACE(0)
   liChgMin
      NO-LABEL
      HELP "Time for change (minute)"
      FORMAT "99"
   SKIP

         
   lcSalesman AT 2 
      FORMAT "X(20)"
      LABEL "Salesman .."
      HELP "Salesman code (F9)"
      VALIDATE(lcSalesMan NE "", lcError)

   "-------------------------------------" AT 2 
   SKIP
   
   liNewCust1 AT 2
      NO-LABEL 
      FORMAT ">>>>>>>>9"
      HELP "Customer nbr, 0=create a new one"
   SKIP
      lcNewCustIdType AT 2 
         NO-LABEL
         FORMAT "X(8)"
         HELP "Customer's ID Type (F9)"
         VALIDATE(lcNewCustIdType NE "", lcError)
      lcNewCustId 
         NO-LABEL
         FORMAT "X(10)"
         HELP "Customer ID"
         VALIDATE(lcNewCustId NE "", lcError)
      SKIP

      ldaNewBirthday AT 2 
         NO-LABEL
         FORMAT "99-99-9999"
         HELP "Birthday"
         VALIDATE(ldaNewBirthDay NE ?, lcError)
      lcNewTitleLabel AT 16
         FORMAT "x(6)"
         NO-LABEL
      lcNewTitle
         NO-LABEL 
         FORMAT "X(8)"
         HELP "Customer's title (F9)"
         VALIDATE(lcNewTitle NE "", lcError)
         SKIP
   
      lcNewFirst AT 2 
         NO-LABEL
         FORMAT "X(37)"
         HELP "Firstname"
         VALIDATE(lcNewFirst NE "", lcError)
      SKIP

      lcNewLast AT 2 
         NO-LABEL 
         FORMAT "X(37)"
         HELP "1st Surname"
         VALIDATE(lcNewLast NE "", lcError)
      SKIP
  
      lcNewSurname2 AT 2 
         NO-LABEL
         FORMAT "X(37)"
         HELP "2nd Surname" 
        
      SKIP
   
      lcNewCompanyname AT 2 
         NO-LABEL
         FORMAT "X(37)"
         HELP "Company customer's name"
       SKIP

      lcNewCOName AT 2 
         NO-LABEL
         FORMAT "X(37)"
         HELP "C/O"
       SKIP
   
      lcNewAddress AT 2 
         NO-LABEL
         FORMAT "X(37)"
         HELP "Address"
         VALIDATE(lcNewAddress NE "", lcError)
      SKIP
      
      lcNewZipCode AT 2 
         NO-LABEL    
         FORMAT "X(5)"
         HELP "Zip code (F9)"
         VALIDATE(lcNewZipCode NE "", lcError)
      lcNewPost 
         NO-LABEL
         FORMAT "X(20)"
         HELP "City"
       SKIP

         lcNewRegion AT 2 
         NO-LABEL
         FORMAT "X(2)"
         lcNewRegionName
         NO-LABEL
         FORMAT "X(30)"
   
      lcNewCountry AT 2 
         NO-LABEL
         FORMAT "X(2)"
         HELP "Country (F9)"
         "/"
      lcNewNationality
         NO-LABEL 
         FORMAT "X(2)"
         HELP "Nationality (F9)"
         SPACE(2)
      liNewLanguage
         LABEL "Language"
         FORMAT ">9"
         HELP "Language (F9)"
      SKIP 

      lcNewEMail AT 2 
         NO-LABEL
         FORMAT "X(37)"
         HELP "Email address"
      SKIP

      lcNewBankAcc AT 2
         NO-LABEL
         FORMAT "X(24)"
         HELP "Bank account"
      
WITH ROW 1 OVERLAY SIDE-LABELS COL 41 
     TITLE " " + lcNewHeader + " "
     FRAME fNewCriter.


FUNCTION fChkTime RETURNS LOGICAL
   (iiHour AS INT,
    iiMin  AS INT):
   
   /* minutes */
   IF iiMin > 59 THEN DO:
      MESSAGE "Minutes can't be more than 59"
      VIEW-AS ALERT-BOX ERROR.
      RETURN FALSE.
   END.
               
   /* hours */
   IF iiHour > 23 THEN DO:
      MESSAGE "Hours can't be more than 23"
      VIEW-AS ALERT-BOX ERROR.
      RETURN FALSE.
   END.
   
   RETURN TRUE.
   
END FUNCTION.

FUNCTION fChkDate RETURNS LOGICAL
   (idaDate AS DATE):

   DEF VAR llError AS LOG NO-UNDO.
   
   llError = FALSE.

   IF idaDate = ? OR idaDate <= MobSub.CreationDate THEN llError = TRUE.
   
   ELSE IF MobSub.PayType = TRUE THEN DO:
      IF idaDate > TODAY THEN llError = TRUE.
   END.
   ELSE DO:
      IF idaDate < TODAY OR 
         (liActDate NE 0 AND DAY(idaDate) NE liActDate) OR
         (ldaMaxDate NE ? AND idaDate > ldaMaxDate)
      THEN llError = TRUE.
   END.

   IF llError THEN DO:
      MESSAGE "Invalid change date"
      VIEW-AS ALERT-BOX ERROR.
      RETURN FALSE.
   END.
      
   RETURN TRUE.

END FUNCTION.

FUNCTION fParam1Data RETURNS CHAR:

   RETURN
      lcNewLast  + ";" + 
      lcNewFirst + ";" +
      lcNewSurname2 + ";" +
      lcNewCOName + ";" + 
      lcNewCompanyname + ";" +
      lcNewAddress    + ";" + 
      lcNewZipCode + ";" +
      lcNewPost    + ";" +
      lcNewCountry + ";" +
      lcNewEMail   + ";" +
      lcSalesMan + ";" +
      lcNewCustIdType + ";" +
      lcNewCustId + ";" +
      (IF lcNewCustIdType NE "CIF" THEN lcNewBirthday ELSE "") + ";" + 
      STRING(liNewLanguage) + ";" + 
      lcNewTitle + ";" +
      lcNewRegion + ";" +
      lcNewBankAcc + ";" +
      lcNewNationality + ";" +
      (IF lcNewCustIdType = "CIF" THEN lcNewBirthday ELSE "") + ";" +
      lcSmsnumber     + ";" +
      lcPhone         + ";" +
      STRING(llDirMarkSMS)    + ";" +
      STRING(llDirMarkEmail)  + ";" +
      STRING(llDirMarkPost)   + ";" +
      STRING(llOutMarkSMS)    + ";" +
      STRING(llOutMarkEmail)  + ";" +
      STRING(llOutMarkPost)   + ";" +
      lcAddressCodC   + ";" +
      lcAddressCodP   + ";" +
      STRING(liDelType) + ";" + 
      lcCIFAgrCustIDType + ";" + 
      lcCIFAgrCustID + ";" + 
      lcAddressCodM.

END FUNCTION.

FUNCTION fChkTitle RETURNS LOGICAL
   (icTitle AS CHAR):

   IF Func.Common:mTMSCodeName("Customer",
                       "Title",
                       icTitle) = "" 
   THEN RETURN FALSE.
   ELSE RETURN TRUE.
END.
         
/* needs update... */
FUNCTION fRequestValues RETURNS LOGIC:

   lcAddressCodM = "".
   
   ASSIGN
      liNewCust1      = MsRequest.ReqIParam1
      lcNewLast       = ENTRY(1,MsRequest.ReqCParam1,";")
      lcNewFirst      = ENTRY(2,MsRequest.ReqCParam1,";")
      lcNewSurname2   = ENTRY(3,MsRequest.ReqCParam1,";")
      lcNewCOName     = ENTRY(4,MsRequest.ReqCParam1,";")
      lcNewCompanyname = ENTRY(5,MsRequest.ReqCParam1,";")
      lcNewAddress    = ENTRY(6,MsRequest.ReqCParam1,";")
      lcNewZipCode    = ENTRY(7,MsRequest.ReqCParam1,";")
      lcNewPost       = ENTRY(8,MsRequest.ReqCParam1,";")
      lcNewCountry    = ENTRY(9,MsRequest.ReqCParam1,";")
      lcNewEMail      = ENTRY(10,MsRequest.ReqCParam1,";")
      lcNewCustIdType = ENTRY(12,MsRequest.ReqCParam1,";")
      lcNewCustId     = ENTRY(13,MsRequest.ReqCParam1,";")
      liNewLanguage   = INT(ENTRY(15,MsRequest.ReqCParam1,";"))
      lcNewTitle      = ENTRY(16,MsRequest.ReqCParam1,";")
      lcNewRegion     = ENTRY(17,MsRequest.ReqCParam1,";")
      lcNewBankAcc    = ENTRY(18,MsRequest.ReqCParam1,";")
      lcNewNationality = ENTRY(19,MsRequest.ReqCParam1,";")
       
      llSameInvCust  = SUBSTRING(MsRequest.ReqCParam4,2,1) = "1"
      llSameUser     = SUBSTRING(MsRequest.ReqCParam4,3,1) = "1"

      lcSmsnumber    = ENTRY(21,MsRequest.ReqCParam1,";")
      lcPhone        = ENTRY(22,MsRequest.ReqCParam1,";")
      llDirMarkSMS   = LOGICAL(ENTRY(23,MsRequest.ReqCParam1,";"))
      llDirMarkEmail = LOGICAL(ENTRY(24,MsRequest.ReqCParam1,";"))
      llDirMarkPost  = LOGICAL(ENTRY(25,MsRequest.ReqCParam1,";"))
      llOutMarkSMS   = LOGICAL(ENTRY(26,MsRequest.ReqCParam1,";"))
      llOutMarkEmail = LOGICAL(ENTRY(27,MsRequest.ReqCParam1,";"))
      llOutMarkPost  = LOGICAL(ENTRY(28,MsRequest.ReqCParam1,";"))
      lcAddressCodC  = ENTRY(29,MsRequest.ReqCParam1,";")
      lcAddressCodP  = ENTRY(30,MsRequest.ReqCParam1,";")
      liDelType      = INT(ENTRY(31,MsRequest.ReqCParam1,";"))
      lcCIFAgrCustIDType = ENTRY(32,MsRequest.ReqCParam1,";")
      lcCIFAgrCustID = ENTRY(33,MsRequest.ReqCParam1,";")
      lcAddressCodM  = ENTRY(34,MsRequest.ReqCParam1,";") WHEN
                       NUM-ENTRIES(MsRequest.ReqCParam1,";") >= 34.
   
   IF lcNewCustIdType = "CIF" THEN 
      ldaNewBirthDay  = DATE(ENTRY(20,MsRequest.ReqCParam1,";")).
   ELSE   
      ldaNewBirthDay  = DATE(ENTRY(14,MsRequest.ReqCParam1,";")).
      

END FUNCTION.

FUNCTION fUpdateRequest RETURNS LOGIC:

   FIND CURRENT MsRequest EXCLUSIVE-LOCK.
         
   IF ldtChgDate = ? 
   THEN ldChgStamp = Func.Common:mMakeTS().
   ELSE ldChgStamp = Func.Common:mMake2DT(ldtChgDate,liTime).
                                            
   ASSIGN 
      MsRequest.ReqIParam1 = liNewCust1 
      MsRequest.ReqCParam1 = fParam1Data()
      MsRequest.ReqDParam1 = ldChgStamp.

   IF llSameInvCust THEN SUBSTRING(MsRequest.ReqCParam4,2,1) = "1".      
   ELSE IF SUBSTRING(MsRequest.ReqCParam4,2,1) = "1"
   THEN SUBSTRING(MsRequest.ReqCParam4,2,1) = "2".
 
   IF llSameUser THEN SUBSTRING(MsRequest.ReqCParam4,3,1) = "1".      
   ELSE IF SUBSTRING(MsRequest.ReqCParam4,3,1) = "1"
   THEN SUBSTRING(MsRequest.ReqCParam4,3,1) = "3".
              
END.

FUNCTION fDispPostOffice RETURNS LOGICAL 
   (iiZipCodeRec AS RECID):
   
   FIND FIRST PostCode WHERE recid(PostCode) = iiZipCodeRec NO-LOCK
   NO-ERROR.
   lcNewZipcode = PostCode.ZipCode.
   lcNewPost    = PostCode.PostOffice.
   lcNewRegion  = Postcode.Region.
   
   FIND Region OF PostCode NO-LOCK NO-ERROR. 
   IF AVAILABLE Region THEN lcNewRegionName = Region.RgName.
   
   DISPLAY
      lcNewZipcode
      lcNewPost
      lcNewRegion
      lcNewRegionName
   WITH FRAME fNewCriter.

END FUNCTION. 

FUNCTION fCopyCustData RETURNS LOGICAL
   (BUFFER ibCopyFrom FOR Customer):

   ASSIGN 
      liNewCust1    = ibCopyFrom.Custnum
      lcNewCustIDType = ibCopyFrom.CustIDType
      lcNewCustId   = ibCopyFrom.OrgID
      lcNewCOName   = ibCopyFrom.COName
      lcNewAddress  = ibCopyFrom.Address
      lcNewZipCode  = ibCopyFrom.ZipCode
      lcNewPost     = ibCopyFrom.PostOffice
      lcNewRegion   = ibCopyFrom.Region
      liNewLanguage = ibCopyFrom.Language
      lcNewCountry  = ibCopyFrom.Country
      lcNewEMail    = ibCopyFrom.EMail
      lcNewBankAcc  = ibCopyFrom.BankAcc
      lcNewNationality = ibCopyFrom.Nationality
      lcSmsnumber    = ibCopyFrom.SMSnumber
      lcPhone        = ibCopyFrom.Phone
      llDirMarkSMS   = ibCopyFrom.DirMarkSMS
      llDirMarkEmail = ibCopyFrom.DirMarkEmail
      llDirMarkPost  = ibCopyFrom.DirMarkPost
      llOutMarkSMS   = ibCopyFrom.OutMarkSMS
      llOutMarkEmail = ibCopyFrom.OutMarkEmail
      llOutMarkPost  = ibCopyFrom.OutMarkPost
      lcAddressCodC  = ibCopyFrom.AddressCodC
      lcAddressCodP  = ibCopyFrom.AddRessCodP
      liDelType      = ibCopyFrom.DelType.

   FIND FIRST CustomerReport WHERE
              CustomerReport.Custnum = Customer.Custnum NO-LOCK NO-ERROR.
   IF AVAIL CustomerReport THEN ASSIGN
      lcAddressCodC  = CustomerReport.StreetCode
      lcAddressCodP  = CustomerReport.CityCode
      lcAddressCodM  = CustomerReport.TownCode.

   IF ibCopyFrom.CustIDType = "CIF" THEN DO:
      ASSIGN
      lcNewCompanyname = ibCopyFrom.CompanyName
      ldaNewBirthday   = ibCopyFrom.FoundationDate
      lcNewFirst       = ""
      lcNewLast        = ""
      lcNewSurname2    = ""
      lcNewTitle       = ""
      lcCIFAgrCustIDType = ibCopyFrom.AuthCustIdType
      lcCIFAgrCustID     = ibCopyFrom.AuthCustId.
   END.
   ELSE ASSIGN 
      lcNewCompanyname = ""
      ldaNewBirthday   = ibCopyFrom.BirthDay
      lcNewFirst       = ibCopyFrom.FirstName
      lcNewLast        = ibCopyFrom.CustName 
      lcNewSurname2    = ibCopyFrom.SurName2
      lcNewTitle       = ibCopyFrom.HonTitle.
               
   FIND FIRST Region WHERE Region.Region = lcNewRegion NO-LOCK NO-ERROR. 
   IF AVAILABLE Region THEN lcNewRegionName = Region.RgName.

   DISPLAY
      liNewCust1   
      lcNewCustId    
      lcNewLast      
      lcNewFirst     
      lcNewSurname2  
      lcNewTitle
      lcNewCompanyname
      lcNewCOName    
      lcNewAddress   
      lcNewZipCode   
      lcNewPost      
      lcNewRegion    
      lcNewRegionName
      liNewLanguage  
      lcNewCountry   
      lcNewEMail     
      lcNewBankAcc
      ldaNewBirthday
      lcNewNationality
   WITH FRAME fNewCriter.

END FUNCTION.


/******** Main start **********/

IF icAction NE "view" THEN DO:
   
   lcError = Func.ValidateACC:mPreCheckSubscriptionForACC(iiMsSeq).
   
   IF lcError > "" THEN DO:
      MESSAGE lcerror SKIP
             "Function not allowed."
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.
   
   lcError = Func.ValidateACC:mCheckSubscriptionForACC(iiMsSeq,
                                                       0,
                                                       0,
                                                       "").

   IF lcError > "" THEN DO:
      
      /* 'superuser' can skip some rules */
      IF ENTRY(1,lcError,"|") EQ "CHECK" AND
         fTokenRights(Syst.Var:katun,"CCSUPER") = "RW"
      THEN DO:
         llOk = FALSE.
         MESSAGE SUBSTRING(lcError,INDEX(lcError,"|") + 1) SKIP
                 "Do You still want to start ACC process?"
         VIEW-AS ALERT-BOX QUESTION
         BUTTONS YES-NO
         SET llOk.
         IF NOT llOk THEN RETURN.
      END.
    
      ELSE DO:
         MESSAGE SUBSTRING(lcError,INDEX(lcError,"|") + 1) SKIP
                "Function not allowed."
         VIEW-AS ALERT-BOX ERROR.
         RETURN.
      END.   
   END.
END.   

FIND MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.

RUN pInitialize.
IF RETURN-VALUE BEGINS "ERROR" THEN RETURN.

PAUSE 0.
VIEW FRAME fOldCriter.
VIEW FRAME fNewCriter.

ChooseOwner:
REPEAT WITH FRAME fNewCriter ON ENDKEY UNDO ChooseOwner, NEXT ChooseOwner:

   FIND Region OF bCurrentCust NO-LOCK NO-ERROR.
   IF AVAILABLE Region THEN lcRegion = Region.RgName.
   
   IF iiRequest > 0 AND AVAILABLE MsRequest THEN DO:
      /* request already made, values from there */
      fRequestValues().
   END.

   PAUSE 0.

   IF bCurrentCust.CustIDType = "CIF" THEN 
      ldaBirthDay = bCurrentCust.FoundationDate.
   ELSE ldaBirthDay = bCurrentCust.BirthDay.
   
   DISPLAY 
      MobSub.MsSeq WHEN AVAILABLE MobSub
      MobSub.CLI   WHEN AVAILABLE MobSub
      MsRequest.MsSeq WHEN icAction = "view" @ MobSub.MsSeq
      MsRequest.CLI   WHEN icAction = "view" @ MobSub.CLI

      bCurrentCust.CustNum
      bCurrentCust.CustIdType
      bCurrentCust.OrgID
      ldaBirthday
      bCurrentCust.FirstName
      bCurrentCust.CustName 
      bCurrentCust.SurName2
      bCurrentCust.Companyname
      bCurrentCust.COName
      bCurrentCust.Address
      bCurrentCust.ZipCode
      bCurrentCust.PostOffice
      bCurrentCust.Region
      lcRegion
      bCurrentCust.Country
      bCurrentCust.Language
      bCurrentCust.Nationality
      bCurrentCust.EMail
      bCurrentCust.BankAcc
   WITH FRAME fOldCriter.

   DISPLAY
      ldtChgDate
      liChgHour
      liChgMin
      lcSalesMan
      lcNewCustIdtype
      lcnewCustId
      lcNewFirst      
      ldaNewBirthday 
      lcNewLast       
      lcNewSurname2   
      lcNewCompanyname
      lcNewCoName     
      lcNewAddress    
      lcNewZipCode    
      lcNewPost       
      lcNewRegion     
      lcNewREgionName
      liNewLanguage 
      lcNewCountry 
      lcNewNationality
      lcNewEMail
      lcNewCustId
      liNewCust1
      lcNewBankAcc
   WITH FRAME fNewCriter.

   IF llUpdCustNum = ? THEN llUpdCustNum = FALSE.
   ELSE DO:
      llUpdCustNum = FALSE.
      IF icAction = "change" AND AVAILABLE MsRequest THEN DO:
         IF MsRequest.ReqStatus >= 12 AND MsRequest.ReqStatus <= 14 AND
            lcNewCustId > "" 
         THEN llUpdCustNum = TRUE.
      END.
   END. 

   IF liNewCust1 = 0 THEN DISPLAY "NEW" @ liNewCust1 WITH FRAME fNewCriter. 
   
   IF ufkey THEN DO:
      ASSIGN
         Syst.Var:ufk   = 0 
         Syst.Var:ufk[1]= 7   
         Syst.Var:ufk[8]= 8 
         Syst.Var:ehto = 0.
         
      IF icAction = "new" and llReady THEN Syst.Var:ufk[5]= 1027.   
      IF icAction = "new" and not llReady THEN Syst.Var:ufk[5]= 0.   
      ELSE IF icAction = "view" THEN Syst.Var:ufk[1] = 0.

      ELSE IF icAction = "change" THEN DO:
      
         IF AVAILABLE MsRequest THEN DO:

            /* print contract */
            IF LOOKUP(STRING(MsRequest.ReqStat),"0,11") > 0  
            THEN Syst.Var:ufk[3] = 1863.
            
            /* confirm (contract returned) */
            IF MsRequest.ReqStat < 12 THEN Syst.Var:ufk[4] = 1054.
 
             /* vrk / sat */    
            IF LOOKUP(STRING(MsRequest.ReqStat),"12,13") > 0 AND
               lcNewCustId > "" 
            THEN ASSIGN Syst.Var:ufk[6] = 2232
                        Syst.Var:ufk[7] = 2234.
             
         END.
      END.

      IF AVAILABLE MsRequest THEN Syst.Var:ufk[2] = 927.  
      
      RUN Syst/ufkey.p.
   END.

   ELSE ASSIGN Syst.Var:toimi = 1  
               ufkey = TRUE.

   /* update new agr.customer */ 
   IF Syst.Var:toimi = 1 THEN RUN pUpdateNewOwner.
      
   /* memo */
   ELSE IF Syst.Var:toimi = 2 THEN DO:
      RUN Mc/memo.p(MobSub.CustNum,
               "MsRequest",
               STRING(MsRequest.MsRequest),
               "Owner Change").
   END. 
   
   /* create request */ 
   ELSE IF Syst.Var:toimi = 5 AND icAction = "new" THEN DO:

      IF ldtChgDate = ? OR 
         (lcNewLast = "" AND lcNewCompanyName = "") OR 
         lcNewAddress = "" OR lcNewZipCode = "" OR lcNewPost = "" 
      THEN DO:
         MESSAGE "All parameters for creation have not been defined"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.

      lcCode = lcNewLast + lcNewFirst + lcNewSurName2 + lcNewCOName +
               lcNewCompanyName + lcNewAddress + lcNewZipCode + lcNewPost +
               lcNewCountry + lcNewEMail + lcSalesman + lcNewCustIDType +
               lcNewCustID + lcNewTitle + lcNewRegion + lcNewBankAcc.
      IF INDEX(lcCode,";") > 0 THEN DO:
         MESSAGE "Check data, it cannot contain semicolons (;)"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.

      IF ldtChgDate < TODAY THEN DO:
         MESSAGE "Change cannot be dated into past"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      IF NOT fChkTime(liChgHour,liChgMin) THEN NEXT.
      
      IF liNewCust1 = MobSub.AgrCust THEN DO:
         MESSAGE "New agreement customer is the same as current one"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
/*
      /*Pro customer check*/
      lcProCheck = fCheckACCCompability(bOriginalCustomer.Custnun,
                                     Customer.Custnum).
  */    

      IF liNewCust1 > 0 THEN DO:
         FIND Customer WHERE Customer.CustNum = liNewCust1 NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Customer OR Customer.Roles = "inactive" THEN DO:
            MESSAGE "Customer " + STRING(liNewCust1) + " is not active"
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.
      END.
      
      IF lcNewCustId = "" THEN DO:
         MESSAGE "Person ID / company ID for new agreement customer"
                 "has not been given."
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END. 
        
      IF llUpdBankAcc THEN DO:
         IF TRIM(lcNewBankAcc,"0") = "" THEN DO:
            MESSAGE "Bank account is mandatory"
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.

         IF NOT fCheckBankAcc(lcNewBankAcc) THEN DO:
            MESSAGE "Bank account is not valid"
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.
      END.
      
      lcError = Func.ValidateACC:mCheckTargetCustomerForACC(liNewCust1).
     
      IF lcError NE "" THEN DO:
         MESSAGE SUBSTRING(lcError,INDEX(lcError,"|") + 1) 
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.

      RUN Mc/charge_dialog.p(
       MobSub.MsSeq,
       (IF MobSub.PayType THEN "ACC_PREPAID" ELSE "ACC_POSTPAID"),
       OUTPUT ldeFee).
     
      llCreateFees = (ldeFee > 0).
         
      llOk = FALSE. 

      MESSAGE "Make a request for changing the agreement customer?"
      VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO
      SET llOk.

      IF NOT llOk THEN NEXT.

      liCurrentUser = Mobsub.AgrCust.
      FIND CURRENT Mobsub NO-LOCK.
      IF liCurrentUser NE MobSub.AgrCust THEN DO:
         MESSAGE "Agreement customer change was done by other user!"
         VIEW-AS ALERT-BOX TITLE "CANCELLED".
         NEXT.
      END.
      
      Syst.Var:ehto = 5.   
      RUN Syst/ufkey.p.

      IF ldtChgDate = ? 
      THEN ldChgStamp = Func.Common:mMakeTS().
      ELSE ldChgStamp = Func.Common:mMake2DT(ldtChgDate,liChgHour * 3600 + 
                                            liChgMin * 60).
       
      IF ldaNewBirthday = ? THEN lcNewBirthday = "".
      ELSE lcNewBirthday = STRING(ldaNewBirthday,"99-99-9999").

      liRequest = fMSCustChangeRequest(MobSub.MsSeq,
                                       "agrcust",
                                       liNewCust1,
                                       MobSub.AgrCust,
                                       fParam1Data(),
                                       ldChgStamp,
                                       llCreateFees,    
                                       ldeFee,
                                       TRUE, /* send SMS */
                                       "",
                                       "4",
                                       0, /* orig. request */
                                       "", /*contract id*/
                                       OUTPUT lcError).
        
      IF liRequest = 0 THEN 
         MESSAGE "Request could not be done;" SKIP
                 lcError
         VIEW-AS ALERT-BOX ERROR.
         
      ELSE 
         MESSAGE "Request ID for agreement customer change is" liRequest
         VIEW-AS ALERT-BOX 
         TITLE " Request Done ".
         
      LEAVE ChooseOwner.

   END.

   ELSE IF Syst.Var:toimi = 8 THEN DO:

      /* update msrequest */
      IF AVAILABLE MsRequest AND MsRequest.ReqStat = 0 THEN DO:
      
         fUpdateRequest().
             
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMsRequest).
         
         RELEASE MsRequest.
      END. 
      
      LEAVE ChooseOwner.
   END.

END. /* ChooseOwner */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fOldCriter NO-PAUSE.    
HIDE FRAME fNewCriter NO-PAUSE.    

/*********  Main end ***********/


PROCEDURE pInitialize:

   ASSIGN
      lcError          = "Mandatory value missing or in wrong format"
      liNewLanguage    = 1
      lcNewCountry     = "ES"
      lcNewNationality = "ES"
      ldaMaxDate       = ?
      llPreActivated   = FALSE.


   IF icAction = "new" THEN DO:
      ASSIGN 
         ldtChgDate    = TODAY
         ufkey         = FALSE
         llSameInvCust = TRUE
         llSameUser    = TRUE.
   END.
   ELSE ufkey = TRUE.

   IF AVAILABLE MobSub THEN DO:
   
      llPreActivated = (MobSub.CLIType = "TARJ3").
      
      CASE MobSub.PayType:
      /* prepaid */
      WHEN TRUE THEN ASSIGN 
         liActDate    = 0
         llUpdBankAcc = FALSE
         ldtChgDate   = TODAY
         liChgHour    = IF llPreActivated 
                        THEN 0 
                        ELSE INTEGER(ENTRY(1,STRING(TIME,"hh:mm:ss"),":"))
         liChgMin     = IF llPreActivated
                        THEN 0
                        ELSE INTEGER(ENTRY(2,STRING(TIME,"hh:mm:ss"),":"))
         llUpdTime    = TRUE.

      /* postpaid */
      WHEN FALSE THEN ASSIGN
         liActDate    = 1
         llUpdBankAcc = TRUE
         ldtChgDate   = IF MONTH(TODAY) = 12
                        THEN DATE(1,1,YEAR(TODAY) + 1)
                        ELSE DATE(MONTH(TODAY) + 1,1,YEAR(TODAY))
         liChgHour    = 0
         liChgMin     = 0
         llUpdTime    = FALSE. 
      END CASE.
   END.
   
   /* max 2 months onwards */
   ldaMaxDate = IF MONTH(ldtChgDate) >= 11 
                THEN DATE(MONTH(ldtChgDate) - 10,1,YEAR(ldtChgDate) + 1)
                ELSE DATE(MONTH(ldtChgDate) + 2,1,YEAR(ldtChgDate)).
                
   IF icAction = "view" THEN lcCurrHeader = "OLD OWNER".
   ELSE DO:
      lcCurrHeader = "CURRENT OWNER". 

      FIND bCurrentCust WHERE bCurrentCust.CustNum = MobSub.AgrCust 
         NO-LOCK NO-ERROR.
   END.

   ASSIGN 
      lcNewHeader     = "NEW OWNER"
      lcMainHeader[1] = "OWNER CHANGE"
      lcMainHeader[2] = "MAKE REQUEST"
      llUpdDate       = (MobSub.PayType = FALSE OR MobSub.CLIType = "TARJ3")
      llUpdCustData   = TRUE.
    
   IF iiRequest > 0 THEN DO:
      lcMainHeader[2] = "REQUEST " + STRING(iiRequest).

      FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MsRequest THEN DO:
         MESSAGE "Unknown request"
         VIEW-AS ALERT-BOX ERROR.
         RETURN "ERROR".
      END.

      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMsRequest).

      Func.Common:mSplitTS(MsRequest.ActStamp,
               OUTPUT ldtChgDate,
               OUTPUT liTime).
          
      ASSIGN 
         liChgHour = INTEGER(ENTRY(1,STRING(liTime,"hh:mm:ss"),":"))
         liChgMin  = INTEGER(ENTRY(2,STRING(liTime,"hh:mm:ss"),":")).
      
      FIND bCurrentCust WHERE bCurrentCust.CustNum = MsRequest.CustNum 
         NO-LOCK NO-ERROR.
   END.
 
END PROCEDURE. /* pInitialize */

PROCEDURE pUpdateNewOwner:
   
   DEF VAR lcErrMsg     AS CHAR NO-UNDO.

   DEFINE BUFFER bf_NewCustomer FOR Customer.
   DEFINE BUFFER bf_NewCustCat  FOR CustCat.
   
   llReady = FALSE.

   UpdateAgrCust:
   REPEAT WITH FRAME fNewCriter ON ENDKEY UNDO, LEAVE:
         
      Syst.Var:ehto = 9. RUN Syst/ufkey.p.
         
      /* change time cannot be in the past */   
      IF NOT llPreActivated AND ldtChgDate <= TODAY THEN ASSIGN 
         ldtChgDate = TODAY
         liChgHour  = INTEGER(ENTRY(1,STRING(TIME,"hh:mm:ss"),":"))
         liChgMin   = INTEGER(ENTRY(2,STRING(TIME,"hh:mm:ss"),":")).
          
      DISPLAY liChgHour liChgMin WITH FRAME fNewCriter.
            
      UPDATE ldtChgDate WHEN llUpdDate
             liChgHour  WHEN llUpdDate AND llUpdTime
             liChgMin   WHEN llUpdDate AND llUpdTime
             lcSalesman WHEN llUpdDate AND llPreActivated
             liNewCust1 WHEN llUpdCustNum
      WITH FRAME fNewCriter EDITING:

         READKEY.
            
         IF KEYLABEL(LASTKEY) = "F9" AND FRAME-FIELD = "lcSalesman" THEN DO:
               
            RUN Help/h-salesman.p.
                   
            IF siirto NE ? THEN DO:
               lcSalesman = siirto NO-ERROR.
               DISPLAY lcSalesman WITH FRAME fNewCriter.
            END.
               
            Syst.Var:ehto = 9.
            RUN Syst/ufkey.p.
            NEXT.

         END. 
            
         IF KEYLABEL(LASTKEY) = "F9" AND
            LOOKUP(FRAME-FIELD,"liNewCust1") > 0 
         THEN DO:

            IF FRAME-FIELD = "liNewCust1" THEN DO:
               RUN Help/h-agrcust.p(lcNewCustId).
                   
               IF siirto NE ? THEN DO:
                  liNewCust1 = INTEGER(siirto) NO-ERROR.
                  DISPLAY liNewCust1 WITH FRAME fNewCriter.
               END.
                   
            END. 

            Syst.Var:ehto = 9.
            RUN Syst/ufkey.p.
            NEXT.
         END. 
 
         IF LOOKUP(KEYLABEL(LASTKEY),Syst.Var:poisnap) > 0 THEN DO:
               
            IF FRAME-FIELD = "ldtChgDate" THEN DO:
               IF NOT fChkDate(INPUT INPUT ldtChgDate) THEN NEXT. 
            END.
            
            ELSE IF FRAME-FIELD = "liChgHour" OR FRAME-FIELD = "liChgMin"
            THEN DO:
               
               IF NOT fChkTime(INPUT INPUT liChgHour,
                               INPUT INPUT liChgMin) 
               THEN NEXT. 
            END.
               
            ELSE IF FRAME-FIELD = "lcSalesman" THEN DO:
                
               FIND FIRST Salesman WHERE 
                    Salesman.Brand = Syst.Var:gcBrand AND
                    Salesman.Salesman = INPUT lcSalesman 
                  NO-LOCK NO-ERROR.
               IF NOT AVAIL Salesman OR
                  Salesman.Salesman = Mobsub.SalesMan THEN DO:
                  MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
               ELSE DO:
                  lcSalesman = Salesman.Salesman.
                  DISPLAY lcSalesman WITH FRAME fNewCriter.
               END.

            END.
         END.
            
         APPLY LASTKEY.
           
      END.
         
      UPDATE
         lcNewCustIdType WHEN liNewCust1 = 0
         lcNewCustId     WHEN liNewCust1 = 0 
      WITH FRAME fNewCriter EDITING:
            
         READKEY.
            
         IF KEYLABEL(LASTKEY) = "F9" AND
            FRAME-FIELD = "lcNewCustIdType" THEN DO:
            RUN Syst/tmscodesbr.p(INPUT "Customer",  
                           INPUT "CustIDType",
                           INPUT "N/A",
                           INPUT "ID Type",
                           INPUT "",
                           OUTPUT lcCode). 
            Syst.Var:ehto = 9.
            RUN Syst/ufkey.p.
            lcNewCustIdType = lcCode.
            DISP lcNewCustIdType WITH FRAME fNewCriter.
         END.   

         IF LOOKUP(KEYLABEL(LASTKEY),Syst.Var:poisnap) > 0 THEN DO:
               
            IF FRAME-FIELD = "lcNewCustIdType" THEN DO:
                  
               IF NOT CAN-FIND(FIRST TMSCodes WHERE
                           TMSCodes.TableName = "Customer" AND
                           TMSCodes.FieldName = "CustIDType" AND
                           TMSCodes.CodeValue = INPUT lcNewCustIdType)
               THEN DO:
                  MESSAGE lcError 
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
               lcNewCustIdType = TRIM(INPUT lcNewCustIdType).
               DISP lcNewCustIdType WITH FRAME fNewCriter.
            END.
               
            IF FRAME-FIELD = "lcNewCustId" THEN DO:
                                    
               IF NOT fChkCustId(lcNewCustIdType,INPUT INPUT lcNewCustId)
               THEN DO:
                  MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.

               /* Validate, if existing customer*/   
               FIND FIRST bf_NewCustomer WHERE bf_NewCustomer.Brand      = Syst.Var:gcBrand                 AND 
                                               bf_NewCustomer.CustIdType = lcNewCustIDType         AND 
                                               bf_NewCustomer.OrgId      = INPUT lcNewCustId       NO-LOCK NO-ERROR.
               IF AVAIL bf_NewCustomer THEN
               DO:
                   ASSIGN lcErrMsg = Func.ValidateACC:mExistingCustomerACCCompability
                                             (bCurrentCust.Category,
                                              bf_NewCustomer.Category,
                                              bf_NewCustomer.CustNum,
                                              bf_NewCustomer.CustIdType,
                                              bf_NewCustomer.OrgId). 

                   IF lcErrMsg <> "" THEN
                   DO:
                       MESSAGE lcErrMsg VIEW-AS ALERT-BOX ERROR.
                       NEXT.
                   END.       
               END.

            END.
             
         END.
         
         APPLY LASTKEY.                              
         
      END.
         
    
      IF MobSub.CLIType NE "TARJ3" AND ldtChgDate < TODAY THEN DO:
          MESSAGE "Change cannot be dated into past"
          VIEW-AS ALERT-BOX ERROR.
          NEXT.
      END. 

      IF MobSub.PayType AND ldtChgDate > TODAY THEN DO:
         MESSAGE "Change date cannot be in the future"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
         
      IF MobSub.PayType = FALSE AND lcNewCustIDType = "Passport" THEN DO:
         MESSAGE "Passport is not accepted for postpaid subscription"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      IF liNewCust1 = 0 AND lcNewCustId ne "" THEN
         DISPLAY "NEW" @ liNewCust1 WITH FRAME fNewCriter.
        
      IF lcNewCustIdType = "CIF" THEN ASSIGN
         lcNewTitleLabel = ""
         ldaNewBirthday:HELP IN FRAME fNewCriter = "Foundation date"
         lcNewTitle = ""
         lcNewFirst = ""
         lcNewLast  = ""
         lcNewSurname2 = "".
      ELSE ASSIGN
         ldaNewBirthday = DATE(MONTH(TODAY),1,YEAR(TODAY) - 18)
                          WHEN ldaNewBirthDay = ?
         ldaNewBirthday:HELP IN FRAME fNewCriter = "Birthday"
         lcNewCompanyname = ""
         lcNewTitleLabel = "Title:".

      DISPLAY   
        lcNewFirst
        lcNewLast
        lcNewSurName2
        lcNewCompanyName
        lcNewTitle lcNewTitleLabel WITH FRAME fNewCriter.
         
      FIND FIRST bNewCust WHERE 
                 bNewCust.Brand = Syst.Var:gcBrand AND
                 bNewCust.CustIdType = lcNewCustIdType AND
                 bNewCust.OrgId = lcNewCustId AND
                 bNewCust.Roles NE "inactive" NO-LOCK NO-ERROR.
                    
      IF AVAIL bNewCust AND bNewCust.CustNum NE liNewCust1 THEN DO: 
         IF CAN-FIND(FIRST bMobSub WHERE
                           bMobSub.Brand     = Syst.Var:gcBrand AND
                           bMobSub.MsSeq    <> MobSub.MsSeq AND
                           bMobSub.CustNum   = bNewCust.CustNum AND
                           bMobSub.PayType   = FALSE) THEN
            fCopyCustData (BUFFER bNewCust).
         llReady = TRUE.
      END.
         
      UPDATE 
          ldaNewBirthday    WHEN llUpdCustData  
          lcNewTitle        WHEN llUpdCustData AND lcNewCustIdType NE "CIF"
          lcNewFirst        WHEN llUpdCustData AND lcNewCustIdType NE "CIF"
          lcNewLast         WHEN llUpdCustData AND lcNewCustIdType NE "CIF"
          lcNewSurname2     WHEN llUpdCustData AND lcNewCustIdType NE "CIF"
          lcNewCompanyname  WHEN llUpdCustData AND lcNewCustIdType EQ "CIF"
          lcNewCOName       WHEN llUpdCustData
          lcNewAddress      WHEN llUpdCustData
          lcNewZipCode      WHEN llUpdCustData
          lcNewCountry      WHEN llUpdCustData
          lcNewNationality  WHEN llUpdCustData
          liNewLanguage     WHEN llUpdCustData
          lcNewEmail        WHEN llUpdCustData
          lcNewBankAcc      WHEN llUpdCustData AND llUpdBankAcc
      WITH FRAME fNewCriter EDITING:
            
         READKEY.
            
         IF KEYLABEL(LASTKEY) = "F9" AND
            FRAME-FIELD = "lcNewTitle" THEN DO:
            
            RUN Help/h-tmscodes.p(INPUT "Customer",  /* TableName */                  
                                 "Title",  /* FieldName */
                                 "CustCare",   /* GroupCode */
                           OUTPUT lcCode).
            Syst.Var:ehto = 9.
            RUN Syst/ufkey.p.
               
            lcNewTitle = lcCode.
            DISP lcNewTitle WITH FRAME fNewCriter.
            NEXT.
         END.   
            
         IF FRAME-FIELD = "lcNewZipcode" AND
            LOOKUP(KEYLABEL(LASTKEY),Syst.Var:poisnap) = 0 AND
            NOT KEY-LABEL(LASTKEY) = "F9" THEN DO:
              
            MESSAGE "Press F9 for options" VIEW-AS ALERT-BOX. 
            NEXT.              
         END.

         IF KEY-LABEL(LASTKEY) = "F9" AND  
            FRAME-FIELD = "lcNewZipcode" THEN DO:
               
            RUN Help/h-postcode.p.
               
            IF Syst.Var:si-recid NE ? THEN DO:
               fDispPostOffice(Syst.Var:si-recid).
            END.   
            Syst.Var:ehto = 9.
            RUN Syst/ufkey.p.
            NEXT.
         END.
            
         IF KEY-LABEL(LASTKEY) = "F9" AND 
            FRAME-FIELD = "liNewLanguage" THEN DO:
               
            RUN Help/h-language.p.
               
            IF siirto NE ? THEN DO:
               liNewLanguage = int(siirto) NO-ERROR.
               DISPLAY liNewLanguage WITH FRAME fNewCriter.
            END.
               
            Syst.Var:ehto = 9.
            RUN Syst/ufkey.p.
            NEXT.
         END.

         IF LOOKUP(KEYLABEL(LASTKEY),Syst.Var:poisnap) > 0 THEN DO:
                  
            IF FRAME-FIELD = "ldaNewBirthday" THEN DO:
                  
               IF (lcNewCustIdType = "CIF" AND INPUT ldaNewBirthday > TODAY) OR
                  (lcNewCustIdType NE "CIF" AND
                   INPUT ldaNewBirthday >
                      DATE(MONTH(TODAY),DAY(TODAY),YEAR(TODAY) - 18)) 
               THEN DO:
                  MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.

            END.
               
            IF FRAME-FIELD = "lcNewTitle" THEN DO:
                  
               IF NOT fChkTitle(INPUT INPUT lcNewTitle) THEN DO: 
                  MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.

            END.
                  
            ELSE IF FRAME-FIELD = "lcNewFirst" THEN DO:
               
               IF INPUT lcNewFirst = "" THEN DO:
                  MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
               lcNewFirst = TRIM(INPUT lcNewFirst).
               DISPLAY lcNewFirst WITH FRAME fNewCriter.

            END.
               
            ELSE IF FRAME-FIELD = "lcNewLast" THEN DO:
               
               IF INPUT lcNewLast = "" THEN DO:
                  MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
               lcNewLast = TRIM(INPUT lcNewLast).
               DISPLAY lcNewLast WITH FRAME fNewCriter.
            END.
   
            ELSE IF FRAME-FIELD = "lcNewCompanyName" THEN DO:
               
               IF INPUT lcNewCompanyName = "" THEN DO:
                  MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
               lcNewCompanyName = TRIM(INPUT lcNewCompanyName).
               DISPLAY lcNewCompanyName WITH FRAME fNewCriter.
            END.

            ELSE IF FRAME-FIELD = "lcNewAddress" THEN DO:
               IF INPUT lcNewAddress EQ "" THEN DO:
                  MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
               lcNewAddress = TRIM(INPUT lcNewAddress).
               DISPLAY lcNewAddress WITH FRAME fNewCriter.
            END.

            ELSE IF FRAME-FIELD = "lcNewZipCode" THEN DO:
                  
               FIND FIRST Postcode WHERE 
                  Postcode.Country = lcNewCountry AND
                  Postcode.ZipCode = lcNewZipCode NO-LOCK NO-ERROR.
                  
               IF NOT AVAIL PostCode THEN DO:
                  MESSAGE lcError 
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.

               fDispPostOffice(RECID(PostCode)).
            END. 
               
            ELSE IF FRAME-FIELD = "liNewLanguage" THEN DO:

               FIND Language WHERE 
                    Language.Language = INPUT liNewLanguage
               NO-LOCK NO-ERROR.
                  
               IF NOT AVAIL Language THEN DO:
                  MESSAGE lcError
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
                  
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
            END.

            ELSE IF FRAME-FIELD = "lcNewNationality" THEN DO:
               FIND FIRST Nationality WHERE 
                          Nationality.Nationality = INPUT lcNewNationality
               NO-LOCK NO-ERROR.
               IF NOT AVAILABLE Nationality THEN DO:
                  MESSAGE "Unknown nationality"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
            END.
               
            ELSE IF FRAME-FIELD = "lcNewRegion" THEN DO:
               FIND FIRST Region WHERE Region.Region = INPUT lcNewRegion
                  NO-LOCK NO-ERROR.
               IF NOT AVAILABLE Region THEN DO:
                  MESSAGE "Unknown region" 
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
               lcNewRegionName = Region.RGName.
               DISPLAY lcNewRegionName WITH FRAME fNewCriter.
            END.
               
            ELSE IF FRAME-FIELD = "lcNewBankAcc" THEN DO:
               IF INPUT lcNewBankAcc > "" THEN DO:
                  IF NOT fCheckBankAcc(INPUT INPUT lcNewBankAcc) THEN DO:
                     MESSAGE "Given bank account is not valid."
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.
               ELSE DO:
                  MESSAGE "Bank account is mandatory"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
            END.

         END. 
                
         APPLY LASTKEY.                              
      END. 



      IF (lcNewCustIdType NE "CIF" AND ldaNewBirthday >
          DATE(MONTH(TODAY), DAY(TODAY), YEAR(TODAY) - 18)) OR
         (lcNewCustIdType = "CIF" AND ldaNewBirthDay > TODAY) 
      THEN DO:
         MESSAGE lcError + " (Birthday)" VIEW-AS ALERT-BOX ERROR.
         NEXT UpdateAgrCust.
      END.

      IF lcNewCustIdType NE "CIF" AND NOT fChkTitle(lcNewTitle) THEN DO:
         MESSAGE lcError + " (Title)" VIEW-AS ALERT-BOX ERROR.
         NEXT UpdateAgrCust.
      END.
        
      llReady = TRUE.
      LEAVE UpdateAgrCust.

   END.  /* UpdateAgrCust */

   IF AVAILABLE MsRequest THEN DO:
      fUpdateRequest().
   END.
      
END PROCEDURE. /* pUpdateNewOwner */


