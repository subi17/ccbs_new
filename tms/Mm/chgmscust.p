 /* ------------------------------------------------------
  MODULE .......: chgmscust.p
  FUNCTION .....: change user/inv.customer for subscription
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 09.01.06
  MODIFIED .....: 18.04.06/aam check if old invcust has other subscriptions
                  27.11.06/aam don´t allow inv.customer change when user is
                               changed
  Version ......: TF
  ------------------------------------------------------ */

{commali.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'MobSub'}
{timestamp.i}
{cparam2.i}
{fcustchangereq.i}
{fcustdata.i}

DEF INPUT PARAMETER iiMsSeq   AS INT  NO-UNDO. 
DEF INPUT PARAMETER icChgType AS CHAR NO-UNDO.   /* user / invcust */

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
DEF VAR ldtChgDate     AS DATE NO-UNDO.
DEF VAR ldChgTime      AS DEC  NO-UNDO. 
DEF VAR lcAgrCustName  AS CHAR NO-UNDO.
DEF VAR lcCustName2    AS CHAR NO-UNDO.
DEF VAR lcNewCustName2 AS CHAR NO-UNDO.
DEF VAR lcTime         AS CHAR NO-UNDO.
DEF VAR liRequest      AS INT  NO-UNDO. 
DEF VAR ldChgStamp     AS DEC  NO-UNDO.
DEF VAR liPrev         AS INT  NO-UNDO.
DEF VAR lcCurrHeader   AS CHAR NO-UNDO.
DEF VAR lcNewHeader    AS CHAR NO-UNDO.
DEF VAR lcMainHeader   AS CHAR NO-UNDO. 
DEF VAR liCustNum2     AS INT  NO-UNDO.
DEF VAR liNewCust1     AS INT  NO-UNDO.
DEF VAR liNewCust2     AS INT  NO-UNDO.
DEF VAR lcChanged      AS CHAR NO-UNDO.
DEF VAR lcCLIList      AS CHAR NO-UNDO.

DEF BUFFER bNewCust  FOR Customer.
DEF BUFFER bCustName FOR Customer.
DEF BUFFER bInvSub   FOR MobSub.

FORM
   Customer.AgrCust AT 2
         LABEL "Agreement Cust."
         FORMAT ">>>>>>>9"
      lcAgrCustName
         NO-LABEL
         FORMAT "X(30)"
      SKIP
      
   MobSub.MsSeq AT 2
         LABEL "Subscription ID" 
         SKIP

   MobSub.CLI AT 2 
         LABEL "MSISDN ........" 
      ldtChgDate AT 48
         LABEL "Change Date"
         HELP "Date when change is scheduled to happen"
      ldChgTime 
         NO-LABEL
         HELP "Time for change"
         FORMAT "99.99"
      SKIP(1)

   lcCurrHeader AT 2 
         NO-LABEL 
         FORMAT "X(30)"
      lcNewHeader  AT 48 
         NO-LABEL 
         FORMAT "X(30)"
      SKIP
  
   Customer.CustNum AT 3  
         LABEL "Customer Nbr ."
      liNewCust1 AT 49
         NO-LABEL 
         FORMAT ">>>>>>>>"
         HELP "Customer nbr, 0=create a new one"
      SKIP
   
   Customer.Category AT 3 
         LABEL "Category ....."
      lcCatName 
         NO-LABEL
         FORMAT "X(20)" 
      lcNewCategory AT 49
         NO-LABEL
         FORMAT "X(4)"
      lcNewCatName 
         NO-LABEL
         FORMAT "X(20)" 
      SKIP
      
   Customer.CustName AT 3 
         LABEL "Lastname/Comp."
      lcNewLast AT 49
         NO-LABEL 
         FORMAT "X(30)"
         HELP "Lastname for private customer or company name"
         SKIP

   Customer.FirstName AT 3          
         LABEL "FirstName ...."
      lcNewFirst AT 49
         NO-LABEL
         FORMAT "X(30)"
         HELP "Firstname"
      SKIP
      
   Customer.COName AT 3
         LABEL "C/O .........."
      lcNewCOName AT 49
         NO-LABEL
         FORMAT "X(30)"
         HELP "C/O"
         SKIP
       
   Customer.Address AT 3
         LABEL "Address ......"
      lcNewAddress AT 49
         NO-LABEL
         FORMAT "X(30)"
         HELP "Address"
      SKIP
      
   Customer.ZipCode AT 3
         LABEL "Zip Code ....."
      lcNewZipCode AT 49
         NO-LABEL    
         FORMAT "X(8)"
         HELP "Address"
      SKIP

   Customer.PostOffice AT 3
         LABEL "Post Office .."
      lcNewPost AT 49
         NO-LABEL
         FORMAT "X(30)"
         HELP "Post office"
      SKIP

   Customer.Country AT 3
         LABEL "Country ......"
         FORMAT "X(2)"
      lcCountName 
         NO-LABEL   
         FORMAT "X(20)" 
      lcNewCountry AT 49
         NO-LABEL
         FORMAT "X(2)"
         HELP "Country"
      lcNewCountName
         NO-LABEL
         FORMAT "X(20)"
      SKIP

   Customer.EMail AT 3
         LABEL "EMail ........"
         FORMAT "X(30)" 
      lcNewEMail AT 49
         NO-LABEL
         FORMAT "X(30)"
         HELP "Email address"
      SKIP
      
   Customer.SMSNumber AT 3
         LABEL "Tel .........."
      lcNewTel AT 49
         NO-LABEL
         FORMAT "X(30)"
         HELP "Tel.nbr, for SMS and other contacts"
      SKIP

   liCustNum2 AT 3
         LABEL "Invoice Cust. " 
         FORMAT ">>>>>>>9"
      lcCustName2 
         NO-LABEL 
         FORMAT "X(19)"
      liNewCust2 AT 49
         NO-LABEL 
         FORMAT ">>>>>>>>"
         HELP "Invoice customer, 0=same as new user that is created"
      lcNewCustName2
         NO-LABEL
         FORMAT "X(20)"
      SKIP
         
WITH ROW 1 OVERLAY SIDE-LABELS CENTERED 
     TITLE " " + lcMainHeader + " " + STRING(pvm,"99-99-99") + " "
     FRAME fCriter.


FUNCTION fChkTime RETURNS LOGICAL
   (idTime AS DEC):
   
   /* minutes */
   lcTime = STRING(idTime - TRUNCATE(idTime,0)).
                  
   IF 100 * DECIMAL(lcTime) > 59 THEN DO:
      MESSAGE "Minutes can't be more than 59"
      VIEW-AS ALERT-BOX ERROR.
      RETURN FALSE.
   END.
               
   /* hours */
   lcTime = STRING(TRUNCATE(idTime,0)).
   IF INTEGER(lcTime) > 23 THEN DO:
      MESSAGE "Hours can't be more than 23"
      VIEW-AS ALERT-BOX ERROR.
      RETURN FALSE.
   END.
   
   RETURN TRUE.
   
END FUNCTION.

FUNCTION fNewCustName2 RETURNS LOGIC:

   IF liNewCust2 > 0 THEN DO:
      FIND bCustName WHERE bCustName.CustNum = liNewCust2 
         NO-LOCK NO-ERROR.
      IF AVAILABLE bCustName THEN 
         lcNewCustName2 = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,  
                                           BUFFER bCustName).
   END.
   ELSE lcNewCustName2 = lcNewLast + " " + lcNewFirst.

END FUNCTION.
 

ASSIGN ufkey      = FALSE
       ldtChgDate = TODAY
       lcTime     = STRING(TIME,"HH:MM").
       
IF SESSION:NUMERIC-FORMAT = "EUROPEAN"
THEN ldChgTime = DECIMAL(REPLACE(lcTime,":",",")).
ELSE ldChgTime = DECIMAL(REPLACE(lcTime,":",".")).

FIND MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
IF NOT AVAILABLE MobSub THEN DO:
   MESSAGE "Unknown subscription"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

IF LOOKUP(STRING(MobSub.MsStat),"4,7,8") = 0 THEN DO:
   MESSAGE "Status of subscription is not valid for changes"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.
 
IF NOT CAN-FIND(Customer WHERE Customer.CustNum = MobSub.AgrCust) THEN DO:
   MESSAGE "Current agreement customer for subscription is invalid"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

IF NOT CAN-FIND(Customer WHERE Customer.CustNum = MobSub.InvCust) THEN DO:
   MESSAGE "Current invoicing customer for subscription is invalid"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

FIND Customer WHERE Customer.CustNum = MobSub.CustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN DO:
   MESSAGE "Current user for subscription is invalid"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

FIND FIRST MsOwner WHERE MsOwner.MsSeq = MobSub.MsSeq NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsOwner    OR 
   MsOwner.TsEnd < 99999999 OR
   MsOwner.CustNum NE MobSub.CustNum OR
   MsOwner.InvCust NE MobSub.InvCust OR
   MsOwner.AgrCust NE MobSub.AgrCust
THEN DO:
   MESSAGE "Timestamp history data for subscription is invalid"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

IF MsOwner.AgrCust NE Customer.AgrCust THEN DO:
   MESSAGE "Agreement customer definitions need to be checked"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

IF fPendingRequest(MobSub.MsSeq,?) THEN DO:
   MESSAGE "There is a pending change request for this subscription"
   VIEW-AS ALERT-BOX INFORMATION.
   RETURN.
END. 
      
FIND bCustName WHERE bCustName.CustNum = Customer.AgrCust 
   NO-LOCK NO-ERROR.
IF AVAILABLE bCustName THEN 
   lcAgrCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,  
                                    BUFFER bCustName).

liPrev = -1.                  

/* if user is changed, then cust1 = user customer, cust2 = inv.customer */
IF icChgType = "user" THEN DO:

   FIND Customer WHERE Customer.CustNum = MobSub.CustNum NO-LOCK NO-ERROR.

   ASSIGN lcCurrHeader = "CURRENT USER"
          lcNewHeader  = "NEW USER"
          lcMainHeader = "USER CHANGE"
          lcChanged    = "user".
          
   PAUSE 0.
   VIEW FRAME fCriter.

   /* default labels and help-texts can be used */
END.

/* if inv.cust is changed, then cust1 = inv.customer, cust2 = user customer */
ELSE DO:

   FIND Customer WHERE Customer.CustNum = MobSub.InvCust NO-LOCK NO-ERROR. 

   ASSIGN lcCurrHeader    = "CURRENT INV.CUSTOMER"
          lcNewHeader     = "NEW INV.CUSTOMER"
          lcMainHeader    = "INVOICE CUSTOMER CHANGE"
          lcChanged       = "invoice customer".
          
   PAUSE 0.
   VIEW FRAME fCriter.

   DO WITH FRAME fCriter:
      ASSIGN 
      liCustNum2:LABEL = "User ........."
      liNewCust2:HELP  = "User, 0=same as new inv.customer " + 
                         "that is created".
   END.

END.

ChooseUser:
REPEAT WITH FRAME fCriter ON ENDKEY UNDO ChooseUser, NEXT ChooseUser:

   ASSIGN lcCountName    = ""
          lcNewCountName = ""
          lcCatName      = ""
          lcNewCatName   = ""
          lcCustName2    = ""
          lcNewCustName2 = "".                                  
          
   FIND Country OF Customer NO-LOCK NO-ERROR.
   IF AVAILABLE Country THEN lcCountName = Country.COName.
   
   IF lcNewCountry > "" THEN DO:
      FIND Country WHERE Country.Country = lcNewCountry NO-LOCK NO-ERROR.
      IF AVAILABLE Country THEN lcNewCountName = Country.COName.
   END.   
   
   FIND CustCat OF Customer NO-LOCK NO-ERROR.
   IF AVAILABLE CustCat THEN lcCatName = CustCat.CatName.
    
   IF lcNewCategory > "" THEN DO:
      FIND CustCat WHERE 
           CustCat.Brand    = gcBrand AND
           CustCat.Category = lcNewCategory NO-LOCK NO-ERROR.
      IF AVAILABLE CustCat THEN lcNewCatName = CustCat.CatName.
   END. 

   IF icChgType = "user" 
   THEN liCustNum2 = MobSub.InvCust.
   ELSE liCustNum2 = MobSub.CustNum.
 
   FIND bCustName WHERE bCustName.CustNum = liCustNum2 
      NO-LOCK NO-ERROR.
   IF AVAILABLE bCustName THEN 
      lcCustName2 = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,  
                                     BUFFER bCustName).

   fNewCustName2().
  
   PAUSE 0.

   DISPLAY 
      Customer.AgrCust
      lcAgrCustName
      MobSub.MsSeq
      MobSub.CLI

      lcCurrHeader
      Customer.CustNum
      Customer.Category
      lcCatName
      Customer.CustName 
      Customer.FirstName
      Customer.COName
      Customer.Address
      Customer.ZipCode
      Customer.PostOffice
      Customer.Country
      lcCountName 
      Customer.EMail
      Customer.SMSNumber
      liCustNum2
      lcCustName2

      ldtChgDate
      ldChgTime
      lcNewHeader
      lcNewCategory
      lcNewCatName
      liNewCust1
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
      liNewCust2
      lcNewCustName2
   WITH FRAME fCriter.

   IF liNewCust1 = 0 THEN DISPLAY "NEW" @ liNewCust1 WITH FRAME fCriter.
   IF liNewCust2 = 0 THEN DISPLAY "NEW" @ liNewCust2 WITH FRAME fCriter.

   IF ufkey THEN DO:
      ASSIGN
         ufk[1]= 7    ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
         ufk[5]= 1027 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 
         ufk[9]= 1
         ehto = 0.
      RUN ufkey.
   END.

   ELSE ASSIGN toimi = 1  
               ufkey = TRUE.

   IF toimi = 1 THEN DO:

      REPEAT WITH FRAME fCriter ON ENDKEY UNDO, LEAVE:
         
         ehto = 9. RUN ufkey.p.
         
         UPDATE ldtChgDate 
                ldChgTime
                liNewCust1
         WITH FRAME fCriter EDITING:

            READKEY.
            
            IF KEYLABEL(LASTKEY) = "F9" AND
               LOOKUP(FRAME-FIELD,"liNewCust1") > 0 
            THEN DO:

               IF FRAME-FIELD = "liNewCust1" THEN DO:

                  RUN h-customer (Customer.AgrCust,
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
            
               IF FRAME-FIELD = "ldChgTime" THEN DO:
               
                  IF NOT fChkTime(INPUT INPUT FRAME fCriter ldChgTime) 
                  THEN NEXT. 
                END.
            END.
            
            APPLY LASTKEY.
           
         END.
         
         IF liNewCust2 = 0  AND liPrev NE liNewCust1 THEN DO:
            IF icChgType = "user" 
            THEN liNewCust2 = Customer.InvCust.
            ELSE liNewCust2 = MobSub.CustNum.
         END.    
 
         IF liNewCust1 = 0 THEN DO:
         
            DISPLAY "NEW" @ liNewCust1 WITH FRAME fCriter.
            
            IF lcNewCategory = "" THEN lcNewCategory = Customer.Category.
            IF lcNewCountry  = "" THEN lcNewCountry  = Customer.Country.
         END.

         IF liNewCust1 > 0 THEN DO:
            FIND bNewCust WHERE bNewCust.CustNum = liNewCust1 NO-LOCK NO-ERROR.
            IF NOT AVAILABLE bNewCust THEN DO:
               MESSAGE "Unknown" lcChanged
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
            END.
            
            IF bNewCust.AgrCust NE Customer.AgrCust THEN DO:
               MESSAGE "New" lcChanged
                       "has a different agreement customer"
                       "than the current" lcChanged
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
            END.
            
            IF icChgType = "user" THEN DO:
            
               IF bNewCust.InvCust NE Customer.InvCust THEN DO:
               
                  llOk = FALSE. 
                  MESSAGE "New user has a different invoice customer"
                          "than the current user."
                  VIEW-AS ALERT-BOX ERROR.
                  
                  IF NOT llOk THEN NEXT.
               END.

               liNewCust2 = bNewCust.InvCust. 
            END. 
            ELSE IF icChgType = "invcust" THEN DO:
            
               /* not already the user */ 
               IF liNewCust1 NE MobSub.CustNum THEN DO:
               
                  llOk = FALSE.
                  MESSAGE "Do You want to make this invoicing customer" SKIP
                          "also the user of this subcription ?"
                  VIEW-AS ALERT-BOX QUESTION
                  BUTTONS YES-NO
                  SET llOk.
                  
                  IF llOk THEN liNewCust2 = liNewCust1.
               END.
                         
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

            fNewCustName2().
            
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
                    liNewCust2
                    lcNewCustName2
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
                liNewCust2     WHEN liNewCust1 = 0 OR icChgType = "invcust"
         WITH FRAME fCriter EDITING:
            
                READKEY.

                IF KEYLABEL(LASTKEY) = "F9" AND
                   LOOKUP(FRAME-FIELD,"liNewCust2") > 0 
                THEN DO:
                   IF FRAME-FIELD = "liNewCust2" THEN DO:
                       
                      IF icChgType = "user" THEN DO:
                      
                         RUN h-customer (Customer.AgrCust,
                                         "agrcust",
                                         "invcust").
                      END.
                                         
                      ELSE DO:
                            
                         NEXT.
                      END.
                       
                      IF siirto NE ? THEN DO:
                         liNewCust2 = INTEGER(siirto) NO-ERROR.
                         DISPLAY liNewCust2 WITH FRAME fCriter.
                      END.
                   END. 
    
                   ehto = 9.
                   RUN ufkey.
                   NEXT.
                END. 
                
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
                  
                  ELSE IF FRAME-FIELD = "liNewCust2" THEN DO:
                  
                     IF INPUT liNewCust2 = 0 THEN DO:
                        IF liNewCust1 > 0 THEN DO:
                           MESSAGE "Customer nbr is mandatory"
                           VIEW-AS ALERT-BOX ERROR.
                           NEXT.
                        END. 
                     END.

                     ELSE DO:
                        FIND bCustName WHERE 
                             bCustName.CustNum = INPUT liNewCust2
                        NO-LOCK NO-ERROR.

                        IF NOT AVAILABLE bCustName THEN DO:
                           MESSAGE "Unknown" lcChanged 
                           VIEW-AS ALERT-BOX ERROR.
                           NEXT.
                        END.
                     
                        IF bCustName.AgrCust NE Customer.AgrCust THEN DO:
                           MESSAGE "New" lcChanged
                                   "has a different"
                                   "agreement customer than current one"
                           VIEW-AS ALERT-BOX ERROR.
                           NEXT.
                        END.

                        IF icChgType = "invcust" AND 
                           INPUT liNewCust2 NE liNewCust1 AND 
                           INPUT liNewCust2 NE MobSub.CustNum
                        THEN DO:
                           MESSAGE "User must be either the current user or"
                                   "the new invoice customer"
                           VIEW-AS ALERT-BOX ERROR.
                           NEXT.
                        END.   
                     END.
                  END. 
                  
                END. 
                
                APPLY LASTKEY.                              
         END.
        
         liPrev = liNewCust1.
         
         LEAVE.
      END.

   END.

   ELSE IF toimi = 5 THEN DO:

      IF ldtChgDate = ? OR lcNewLast = "" OR 
         (liNewCust2 = 0 AND liNewCust1 > 0) THEN DO:
         MESSAGE "All parameters for creation have not been defined"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
         
      IF ldtChgDate < TODAY THEN DO:
         MESSAGE "Change cannot be dated into past"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      IF NOT fChkTime(ldChgTime) THEN NEXT.
      
      IF icChgType = "user" AND liNewCust1 = MobSub.CustNum THEN DO:
         MESSAGE "New user is the same as current one"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      ELSE IF icChgType = "invcust" AND liNewCust1 = MobSub.InvCust THEN DO:
         MESSAGE "New invoice customer is the same as current one"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
     
      /* if user is not changed then check possible other subscriptions */ 
      IF icChgType = "invcust" AND liNewCust2 = MobSub.CustNum THEN DO:
         lcCLIList = "".
         FOR EACH bInvSub NO-LOCK WHERE 
                  bInvSub.Brand   = gcBrand        AND
                  bInvSub.CustNum = MobSub.CustNum:
            IF bInvSub.CLI NE MobSub.CLI THEN 
               lcCLIList = lcCLIList + (IF lcCLIList > "" THEN ", " ELSE "") +
                           bInvSub.CLI.
         END.

         IF lcCLIList > "" THEN DO:
            llOk = FALSE.
         
            MESSAGE "Current user" MobSub.CustNum
                    "is the user also for the following subscriptions:" SKIP
                    lcCLIList SKIP
                    "Is it ok to change the new invoicing customer also"
                    "to them?"
            VIEW-AS ALERT-BOX QUESTION
            BUTTONS YES-NO
            TITLE " AFFECT TO OTHERS "
            SET llOk.
            
            IF NOT llOk THEN DO:
               MESSAGE "Change first a new user to the subscription, so"
                       "that the other subscriptions will not be affected"
                       "by the invoicing customer change."
               VIEW-AS ALERT-BOX 
               TITLE " CHANGE CANCELLED ".
               NEXT.
            END.
         END.         
      END.

      
      IF liNewCust1 = 0 THEN DO:
         llOk = FALSE.
           
         MESSAGE "A new customer nbr will be issued to this" 
                 lcChanged "." SKIP
                 "Go on with the change ?" 
         VIEW-AS ALERT-BOX QUESTION
         BUTTONS YES-NO
         SET llOk.
            
         IF NOT llOk THEN NEXT.
         
         /* is there already another customer with the same data */
         FOR EACH bCustName NO-LOCK WHERE
                  bCustName.AgrCust = Customer.AgrCust:

            IF bCustName.CustName  = lcNewLast    AND
               bCustName.FirstName = lcNewFirst   AND
               bCustName.COName    = lcNewCOName  AND
               bCustName.Address   = lcNewAddress AND
               bCustName.ZipCode   = lcNewZipCode
            THEN DO:
               llOk = FALSE.

               MESSAGE "A customer already exists with given name and"
                       "address data, with nbr" bCustName.CustNum 
                       ". Do You still want to create a new customer nbr"
                       "for this" lcChanged "?"
               VIEW-AS ALERT-BOX QUESTION
               BUTTONS YES-NO
               SET llOk.
                  
               IF NOT llOk THEN NEXT ChooseUser.
            END. 
                     
         END. 
      END. 

      ELSE DO:  
         llOk = FALSE. 

         MESSAGE "Make a request for changing the" lcChanged "?"
         VIEW-AS ALERT-BOX
         QUESTION
         BUTTONS YES-NO
         SET llOk.

         IF NOT llOk THEN NEXT.
      END.
      
      ehto = 5.   
      RUN ufkey.

      IF ldtChgDate = ? 
      THEN ldChgStamp = fMakeTS().
      ELSE ldChgStamp = fMake2DT(ldtChgDate,
                                 INTEGER(TRUNCATE(ldChgTime,0) * 3600 +
                                         (ldChgTime - TRUNCATE(ldChgTime,0))
                                          * 100 * 60)).
        
      /* create the request */      
       liRequest = fMSCustChangeRequest(MobSub.MsSeq,
                                      icChgType,
                                      IF icChgType = "user"
                                      THEN liNewCust1
                                      ELSE liNewCust2,
                                      IF icChgType = "user"
                                      THEN liNewCust2
                                      ELSE liNewCust1,
                                      lcNewLast     + ";" + 
                                         lcNewFirst + ";" +
                                         lcNewCOName + ";" + 
                                      lcNewAddress    + ";" + 
                                         lcNewZipCode + ";" +
                                         lcNewPost    + ";" +
                                         lcNewCountry + ";" +
                                      lcNewEMail + ";" +
                                         lcNewTel + ";" + 
                                      lcNewCategory,
                                      ldChgStamp,
                                      TRUE,    /* create fees */
                                      0.00,
                                      FALSE,   /* send SMS */
                                      "4",
                                      "",
                                      0, /* orig. request */
                                      OUTPUT lcError).
        
      IF liRequest = 0 THEN 
         MESSAGE "Request could not be done;" SKIP
                 lcError
         VIEW-AS ALERT-BOX ERROR.
         
      ELSE 
         MESSAGE "Request ID for" lcChanged "change is" liRequest
         VIEW-AS ALERT-BOX 
         TITLE " Request Done ".
         
      LEAVE ChooseUser.

   END.

   ELSE IF toimi = 8 THEN DO:
      LEAVE ChooseUser.
   END.

END. /* ChooseUser */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCriter NO-PAUSE.    

