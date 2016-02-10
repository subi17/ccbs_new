/* ----------------------------------------------------------------------
  MODULE .......: cust_timing
  TASK .........: 
  APPLICATION ..: nn
  AUTHOR .......: jp
  CREATED ......: 10-02-04
  CHANGED ......: 
  Version ......: 
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}
{Func/sog.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'MSOwner'}

DEF BUFFER bCustomer for Customer.

IF lcRight NE "RW" THEN DO:
   MESSAGE 
      "You don't have right" SKIP
      "to change SIM cars !"
   VIEW-AS ALERT-BOX.
   RETURN.   
END.

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMobSub AS HANDLE NO-UNDO.
   lhMobSub = BUFFER MobSub:HANDLE.
   RUN StarEventInitialize(lhMobSub).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhMobSub).
   END.

END.

DEF INPUT PARAMETER  iicustnum  AS INT  NO-UNDO.

FIND FIRST customer NO-LOCK WHERE 
           Customer.Brand   = gcBrand  AND 
           customer.CustNum = iiCustNum NO-ERROR.

FIND FIRST EventLog WHERE 
           eventlog.KEY            = String(Customer.custnum) AND 
           eventlog.tablename      = "Customer"               AND 
           EventLog.EventLogStatus = 1 NO-LOCK NO-ERROR.
           
IF AVAIL EventLog THEN DO:
   MESSAGE
   "scheduled change of customer data allready exists! (" timingdate ")"  SKIP
   VIEW-aS ALERT-BOX.        
ENd.           

DEF VAR old-icc       LIKE imsi.icc                 NO-UNDO.
DEF VAR quar          AS lo format "Yes/No"         NO-UNDO.
DEF VAR FrmRow        AS i                          NO-UNDO  init 1.
DEF VAR UserName      AS c                          NO-UNDO.
DEF VAR ok            AS LO                         NO-UNDO FORMAT "Yes/No".
DEF VAR lcCustName    AS CHAR                       NO-UNDO FORMAT "X(20)".
DEF VAR ldaTimingDate AS DATE                       NO-UNDO FORMAT "99-99-99".
DEF VAR ldaTimingTime AS DEC                        NO-UNDO FORMAT "99.99".
DEF VAR lcInvCode    AS CHAR                       NO-UNDO.

DEF TEMP-TABLE ttCustomer   NO-UNDO
FIELD   TTCustName       LIKE Customer.CustName 
FIELD   TTInvCust        LIKE customer.InvCust  
FIELD   TTContact        LIKE Customer.Contact  
FIELD   TTAddress        LIKE Customer.Address  
FIELD   TTZipCode        LIKE Customer.ZipCode  
FIELD   TTPostOffice     LIKE Customer.PostOffice
FIELD   TTEMail          LIKE Customer.Email     
FIELD   TTInvCode        LIKE Customer.InvCode
FIELD   TTIDelName       LIKE Customer.IDelName  
FIELD   TTIDelAddr       LIKE Customer.IDelAddr   
FIELD   TTIDelZipCode    LIKE Customer.IDelZipCode 
FIELD   TTIDelPost       LIKE Customer.IDelPost    
FIELD   TTIDelCountry    LIKE Customer.IdelCountry 
FIELD   TTIDelCOName     LIKE Customer.IDelConame.


FORM
   "CUSTOMER DATA                                  NEW CUSTOMER DATA     " SKIP
"------------------------------------------------------------------------------" 
                                                                     SKIP
   Customer.CustNum     Label "CustomerNumber "                      
      "Scheduled time:" at 48  ldaTimingDate no-label   
        ldaTimingTime no-label                      SKIP
   Customer.CustName    Label "Customer Name.."                      
                         ttCustname NO-LABEL at 48                   SKIP
   Customer.InvCust     LABEL "InvoiceCustomer"                      
                        ttInvCust NO-label   at 48  
                        lcCustName no-label  TO 78                   SKIP
   Customer.Contact     LABEL "ContactName ..."                      
                        ttcontact  no-label  at 48                   SKIP  
   Customer.Address     LABEL "Address ......."                      
                        ttAddress no-label   at 48                   SKIP
   Customer.ZipCode     LABEL "Zip ..........."                      
                        ttzipcode  no-label  at 48                   SKIP
   Customer.PostOffice  LABEL "City .........."                      
                        ttPostoffice no-label at 48                  SKIP
   Customer.InvCode    LABEL "Invoice Code .."       
                        TTInvCode no-label at 48          
                        lcInvCode no-label to 78                    SKIP
                     
   Customer.IDelName    LABEL "InvDel Name...."              
                        ttidelname no-label  at 48                   SKIP
   Customer.IDelAddr    LABEL "InvDel Addr...."              
                        ttideladdr no-label  at 48                   SKIP
   Customer.IDelZipCode LABEL "InvDel zip....."              
                        ttidelzipcode no-label at 48                 SKIP
   Customer.IDelPost    LABEL "InvDel Post...."              
                        ttidelpost no-label at 48                    SKIP
   Customer.IDelCountry LABEL "InvDel Country."              
                        ttidelcountry no-label at 48                 SKIP
   Customer.IDelCOName  LABEL "InvDel Contact."              
                        ttidelconame no-label at 48                  SKIP 

WITH
  OVERLAY CENTERED ROW 1 TITLE " TIMING OF CUSTOMER DATA "   
  SIDE-LABELS  FRAME main.


FORM
"Note that this MSISDN has a porting Time Slot" SKIP(1)
"         Date :" LdaTimingDate            SKIP
"         Time :" LdaTimingTime            SKIP(1)
WITH OVERLAY CENTERED ROW 10 TITLE "SCHEDULED CHANGE OF CUSTOMER DATA" 
NO-LABELS FRAME TIming.


PAUSE 0.

ASSIGN
   ldatimingdate = today + 1.
DISP 
   ldatimingdate
   ldatimingtime
   
   Customer.CustNum      
   Customer.CustName   
   Customer.InvCust   
   Customer.Contact   
   Customer.Address   
   Customer.ZipCode    
   Customer.PostOffice 
   Customer.InvCode

   Customer.IDelName   
   Customer.IDelAddr   
   Customer.IDelZipCode
   Customer.IDelPost   
   Customer.IDelCountry
   Customer.IDelCOName 


WITH FRAME main.
     
MAIN:
REPEAT WITH FRAME main:

   ehto = 9. RUN Syst/ufkey.
   
   CREATE ttCustomer. 

   UPDATE
   ldatimingDAte
   ldatimingtime
   ttCustomer.TTCustName   
   ttCustomer.TTContact   
   ttCustomer.TTAddress   
   ttCustomer.TTZipCode    
   ttCustomer.TTPostOffice 
   ttCustomer.TTInvCode
   ttCustomer.TTIDelName   
   ttCustomer.TTIDelAddr   
   ttCustomer.TTIDelZipCode
   ttCustomer.TTIDelPost   
   ttCustomer.TTIDelCountry
   ttCustomer.TTIDelCOName  
   
   WITH FRAME main EDITING:
             READKEY.
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME main:
                PAUSE 0.
 
                IF frame-field = "ldaTimingDate" THEN DO:
                   ASSIGN input Frame main ldaTimingDate.  
                   
                   IF ldatimingDate = ? THEN LEAVE main.
                   
                   IF ldatimingDate < today THEN DO:
                      BELL.
                      MESSAGE
                      "Scheduled date must be greater than "  today   "!". 
                      NEXT-PROMPT ldatimingDAte. NEXT.
                   ENd.
                END.

                ELSE IF frame-field = "ldaTimingTime" THEN DO:
                   ASSIGN input frame main ldaTimingTime.

                     
                   IF  ldaTimingTime - 
                      trunc(ldaTimingTime,0) > 0.59 
                   THEN DO:
                      BELL.
                      MESSAGE
                        "Scheduled minutes must be set between 0 and 59 "
                        VIEW-AS ALERT-BOX.
                      NEXT-PROMPT ldaTimingTime.
                      NEXT.
                   END.
                     
                   IF  ldaTimingTime > 23.59 
                   THEN DO:
                      BELL.
                      MESSAGE
                        "Scheduledg hours must be set between 0 and 23 "
                      VIEW-AS ALERT-BOX.
                      NEXT-PROMPT ldaTimingTime.
                      NEXT.
                   END.
                END.
                
                IF FRAME-FIELD = "ttInvCust" THEN DO:
                   FIND FIRST bCustomer WHERE 
                              bCustomer.Brand   = gcBRand AND 
                              bCustomer.CustNum = INPUT ttCustomer.TTInvCust
                   NO-LOCK NO-ERROR.
                   IF NOT AVAIL bCustomer THEN DO:
                      BELL.
                      MESSAGE
                      "Unknown customer no: " INPUT ttCustomer.TTInvCust.
                      NEXT-PROMPT ttCustomer.ttInvCust. NEXT.
                   END.          
                    DISP bcustomer.Custname @ lcCustname with frame main.
                END.
                
                ELSE IF FRAME-FIELD = "TTInvCode" THEN DO:
                   IF INPUT FRAME main TTInvCode ne "00" THEN DO:
                      FIND FIRST InvRunlog  WHERE 
                                 InvRunLog.Brand    = gcBRand AND 
                                 InvRunLog.InvCode = INPUT ttCustomer.TTInvCode
                      NO-LOCK NO-ERROR.
                      IF NOT AVAIL InvRunLog THEN DO:
                         BELL.
                         MESSAGE
                         "Unknown Invoice Code: " INPUT ttCustomer.TTInvCode.
                         NEXT-PROMPT ttCustomer.TTInvCode. NEXT.
                      END.          
                   END.   
                END.
             END.
             APPLY LASTKEY.
   END. /* EDITING */
   

ACTION:                            
   REPEAT WITH FRAME main:
      ASSIGN
      ufk = 0 ehto = 0
      ufk[1] = 7 
      ufk[5] = 795
      ufk[8] = 8.


      RUN Syst/ufkey.

      IF toimi = 1 THEN NEXT  main.
      IF toimi = 8 THEN LEAVE main.
      IF TOIMI = 5 THEN DO:


         ok = FALSE.
         MESSAGE "Do You REALLY want to change (Y/N) ?" UPDATE ok.
         IF NOT ok THEN NEXT Action.


         CREATE eventlog.
         ASSIGN
           eventlog.eventdate      = TODAY                      
           eventlog.eventtime      = STRING(TIME,"HH:MM:SS")
           eventlog.usercode       = Katun
           eventlog.action         = 'Timing'.
         ASSIGN
            eventlog.KEY            = String(Customer.custnum)
            eventlog.tablename      = "Customer" 
            EventLog.EventLogStatus = 1
            EventLog.TimingDate     = ldaTimingDate
            EventLog.TimingTime     = ldaTimingTime.
         ASSIGN
            EventLog.TimingTS       =  fHMS2TS(INPUT EventLog.TimingDate,
            
                                 INPUT STRING(truncate(EventLog.TimingTime,0)
                                 ,"99") +
                                       ":"  + 
                                 STRING(string(
                                 (EventLog.Timingtime - 
                                  truncate(EventLog.Timingtime,0)
                                  ) * 100),"99")
                                  + ":00")  .

         IF  TTCustomer.TTCustName  ne Customer.CustName AND 
             TtCustomer.TTCustName  ne "" THEN 
            
            ASSIGN
            eventlog.ModifiedFields = EventLog.ModifiedFields + "CustName," 
            
            EventLog.DataValues     = Eventlog.Datavalues + "CustName" + 
                                      chr(255) + 
                                      STRING(TTCustomer.ttCustname) + chr(255) +
                                      chr(255).
         IF  TTCustomer.TTInvCust   ne Customer.InvCust AND 
             TTCustomer.TTInvCust   ne 0 THEN 
            ASSIGN
            eventlog.ModifiedFields = EventLog.ModifiedFields + "InvCust," 
            
            EventLog.DataValues     = Eventlog.Datavalues + "InvCust" + 
                                      chr(255) + 
                                      STRING(TTCustomer.ttInvCust) + chr(255) +
                                      chr(255).

         IF TTCustomer.TTcontact   ne Customer.Contact AND 
            TTCustomer.TTContact   ne "" THEN 
            ASSIGN
            eventlog.ModifiedFields = EventLog.ModifiedFields + "Contact," 
            
            EventLog.DataValues     = Eventlog.Datavalues + "Contact" + 
                                      chr(255) + 
                                      STRING(TTCustomer.ttContact) + chr(255) +
                                      chr(255).

         IF TTCustomer.TTAddress   ne Customer.Address  AND 
            TTCustomer.TTAddress   ne "" THEN 
            ASSIGN
            eventlog.ModifiedFields = EventLog.ModifiedFields + "Address," 
            
            EventLog.DataValues     = Eventlog.Datavalues + "Address" + 
                                      chr(255) + 
                                      STRING(TTCustomer.ttAddress) + chr(255) +
                                      chr(255).

         IF TTCustomer.TTZipCode   ne Customer.ZipCode AND 
            TTCustomer.TTZipCode   ne "" THEN 
            ASSIGN
            eventlog.ModifiedFields = EventLog.ModifiedFields + "ZipCode," 
            
            EventLog.DataValues     = Eventlog.Datavalues + "ZipCode" + 
                                      chr(255) + 
                                      STRING(TTCustomer.ttZipCode) + chr(255) +
                                      chr(255).

         IF TTCustomer.TTPostOffice ne Customer.Postoffice  AND 
            TTCustomer.TTPostOffice ne "" THEN 
            ASSIGN
            eventlog.ModifiedFields = EventLog.ModifiedFields + "PostOffice," 
            
            EventLog.DataValues     = Eventlog.Datavalues + "PostOffice" + 
                                      chr(255) + 
                                      STRING(TTCustomer.ttPostOffice) + 
                                      chr(255) + chr(255).

         IF TTCustomer.TTEmail ne Customer.Email AND 
            TTCustomer.TTEmail ne "" THEN 
            ASSIGN
            eventlog.ModifiedFields = EventLog.ModifiedFields + "Email," 
            
            EventLog.DataValues     = Eventlog.Datavalues + "Email" + 
                                      chr(255) + 
                                      STRING(TTCustomer.TTEmail) + chr(255) +
                                      chr(255).
                                      
         IF TTCustomer.TTInvCode ne Customer.InvCode AND 
            TTCustomer.TTInvCode ne 0 THEN 
            ASSIGN
            eventlog.ModifiedFields = EventLog.ModifiedFields + "InvCode," 
            
            EventLog.DataValues     = Eventlog.Datavalues + "InvCode" + 
                                      chr(255) + 
                                      STRING(TTCustomer.TTInvCode) + chr(255) +
                                      chr(255).

         IF TTCustomer.TTIDelName ne Customer.IdelName AND 
            TTCustomer.TTIDelName ne "" THEN 
            ASSIGN
            eventlog.ModifiedFields = EventLog.ModifiedFields + "IDelName," 
            
            EventLog.DataValues     = Eventlog.Datavalues + "IDelName" + 
                                      chr(255) + 
                                      STRING(TTCustomer.TTIDelName) + chr(255) +
                                      chr(255).
                                      
         IF TTCustomer.TTIDelAddr ne Customer.IDelAddr AND 
            TTCustomer.TTIDelAddr ne "" THEN 
            ASSIGN
            eventlog.ModifiedFields = EventLog.ModifiedFields + "IDelAddr," 
            
            EventLog.DataValues     = Eventlog.Datavalues + "IDelAddr" + 
                                      chr(255) + 
                                      STRING(TTCustomer.TTIDelAddr) + chr(255) +
                                      chr(255).

         IF TTCustomer.TTIDelZipCode ne Customer.IDelZipCode AND 
            TTCustomer.TTIDelZipCode ne "" THEN 
            ASSIGN
            eventlog.ModifiedFields = EventLog.ModifiedFields + "IDelZipCode," 
            
            EventLog.DataValues     = Eventlog.Datavalues + "IDelZipCode" + 
                                      chr(255) + 
                                      STRING(TTCustomer.TTIDelZipCode) + 
                                      chr(255) + chr(255).
         IF TTCustomer.TTIDelPost ne Customer.IDelPost AND 
            TTCustomer.TTIDelPost ne "" THEN 
            ASSIGN
            eventlog.ModifiedFields = EventLog.ModifiedFields + "IDelPost," 
            
            EventLog.DataValues     = Eventlog.Datavalues + "IDelPost" + 
                                      chr(255) + 
                                      STRING(TTCustomer.TTIDelPost) + chr(255) +
                                      chr(255).
         IF TTCustomer.TTIDelCountry ne Customer.IDelCountry AND 
            TTCustomer.TTIDelCountry ne "" THEN 
            ASSIGN
            eventlog.ModifiedFields = EventLog.ModifiedFields + "IDelCountry," 
            
            EventLog.DataValues     = Eventlog.Datavalues + "IDelCountry" + 
                                   chr(255) + 
                                   STRING(TTCustomer.TTIDelCountry) + chr(255) +
                                    chr(255).

         IF TTCustomer.TTIDelCOName ne Customer.IDelCOName AND 
            TTCustomer.TTIDelCOName ne "" THEN 
            ASSIGN
            eventlog.ModifiedFields = EventLog.ModifiedFields + "IDelCOName," 
            
            EventLog.DataValues     = Eventlog.Datavalues + "IDelCOName" + 
                                      chr(255) + 
                                     STRING(TTCustomer.TTIDelCOName) + chr(255)                                       + chr(255).

         IF eventlog.ModifiedFields = "" THEN DO:
            MESSAGE
            "Scheduled change of customer daata NOT created"
            VIEW-AS ALERT-BOX.
            DELETE eventlog.
         END.
         ELSE
         MESSAGE
         "scheduled request of customer data has been saved to the system."                                                                   SKIP
         "ALL Timing  requests and their current status "     SKIP
         "can be browsed from Event log."
         VIEW-AS ALERT-BOX TITLE "TIMING".
         
         LEAVE.                             
      END.
   END. /* Action */      

   LEAVE main.
END. /* MAIN */
HIDE FRAME main NO-PAUSE.
HIDE MESSAGE.
