/* - MNP donate operation must do later 
   - Barring */

/* ----------------------------------------------------------------------
  MODULE .......: mobtypech.p
  TASK .........: Change Mobiletype of  MobSub
  APPLICATION ..: nn
  AUTHOR .......: jp
  CREATED ......: 04-01-00
  CHANGED ......: 17.09.02/aam RatePlan instead of PriceList on BillTarget
                  14.10.02/jr  Removed BillLevel, use BillTarg
                  13.03.03/tk  tokens
                  10.06.02 jp  dont updat billing target,
                               create new 
                               warning box if different VAT Handling
                  15.07.03 jp  dont delete fixedfee, if can find billed ffitem
                  12.09.03 jp  Brand 
                  02.02.04 jp eventlog
                  09.02.04 jp change timestamp for msowner
                  07.06.04 tk do not allow change if msstat ne 4
                  05.11.04 tk buffer-copy, current msowner finding changed
                  16.12.04/aam create MsRequest,
                               ask about fee creation and SMS,
                               check free change qty and changes per month etc.
                  13.10.05/aam Salesman (empty) to fCTChangeRequest
                  19.12.05 jp  userCustomer
                  03.04.06/aam not allowed if a periodical contract active
                  15.05.07 PZ  this module has been disabled temporarily
                  24.09.07/ JP new business logic & layout
                  25.08.15 ilkkasav new parameter to fCTChangeRequest
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'MobSub'}
{Func/cparam2.i}
{Func/fctchange.i}
{Func/fmakemsreq.i}
{Syst/eventval.i}
{Func/fbankdata.i}
{Func/matrix.i}
{Syst/tmsconst.i}
{Func/profunc.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMobsub AS HANDLE NO-UNDO.
   lhMobsub = BUFFER mobsub:HANDLE.
   RUN StarEventInitialize(lhMobsub).

END.

DEF INPUT PARAMETER    iiMsSeq LIKE MobSub.MsSeq NO-UNDO. 
DEF NEW SHARED VAR siirto AS CHAR.

DEF VAR ok            AS LOG                          NO-UNDO FORMAT "Yes/No".
DEF VAR new-type      AS CHAR  FORMAT "X(16)"          NO-UNDO.
DEF VAR new-type-nimi AS CHAR  FORMAT "X(16)"         NO-UNDO.
DEF VAR i             AS INT                          NO-UNDO.
DEF VAR UserName      AS CHAR                         NO-UNDO. 
DEF VAR minutes       AS INT                          NO-UNDO. 
DEF VAR new-ts-begin  AS DEC                          NO-UNDO.
DEF VAR new-ts-date   AS DATE FORMAT "99-99-9999"     NO-UNDO.
DEF VAR new-ts-time   AS DEC  FORMAT "99999999.99999" NO-UNDO.

DEF VAR lcInfo        AS CHAR                         NO-UNDO.
DEF VAR llCreateFees  AS LOG  FORMAT "YES/NO"         NO-UNDO.
DEF VAR llSendSMS     AS LOG                          NO-UNDO. 
DEF VAR liFreeChange  AS INT                          NO-UNDO.
DEF VAR lcTimeLimit   AS CHAR                         NO-UNDO. 
DEF VAR ldDefTime     AS DEC                          NO-UNDO. 
DEF VAR odtDate       AS DATE                         NO-UNDO.
DEF VAR oiTime        AS INT                          NO-UNDO.
DEF VAR liAge         AS INT                          NO-UNDO.
DEF VAR liPerType     AS INT  FORMAT "9"              NO-UNDO.
DEF VAR lcPerTypeName AS CHAR FORMAT "X(12)"          NO-UNDO.
DEF VAR liPerValid    AS DATE FORMAT "99-99-9999"     NO-UNDO.
DEF VAR lcBankName    AS CHAR FORMAT "X(20)"          NO-UNDO.

DEF VAR lcChangeDate      AS CHAR                     NO-UNDO.
DEF VAR lcMobsubInDays    AS CHAR                     NO-UNDO.
DEF VAR liLoop            AS INT                      NO-UNDO.
DEF VAR lcOperatorBarring AS CHAR FORMAT "X(2)"       NO-UNDO.
DEF VAR lcFailedRequest   AS CHAR FORMAT "X(2)"       NO-UNDO.
DEF VAR liBankAccount     AS INT                      NO-UNDO.
DEF VAR lcBankAccount     LIKE Customer.BankAcct      NO-UNDO.
DEF VAR llBankAccount     AS LOG                      NO-UNDO.
DEF VAR llPenalty         AS LOG                      NO-UNDO FORMAT "YES/NO".
DEF VAR llActiveReq       AS LOG                      NO-UNDO FORMAT "YES/NO".
DEF VAR llBarring         AS LOG                      NO-UNDO FORMAT "YES/NO".
DEF VAR llMNP             AS LOG                      NO-UNDO FORMAT "YES/NO".
DEF VAR lcMNPOngoing      AS CHAR                     NO-UNDO.
DEF VAR lcCreditCheck     AS CHAR                     NO-UNDO.
DEF VAR llCreditCheck     AS LOG                      NO-UNDO FORMAT "YES/NO".
DEF VAR liCreditCheck     AS INT                      NO-UNDO.
DEF VAR lcBankAccountOK   AS CHAR FORMAT "X(3)"       NO-UNDO.
DEF VAR ocResult          AS CHAR                     NO-UNDO.
DEF VAR liReq             AS INT                      NO-UNDO.
DEF VAR lcAgrCustName     AS CHAR FORMAT "X(50)"      NO-UNDO.
DEF VAR oiRes             AS INT                      NO-UNDO.
DEF VAR liPostpaid        AS INT                      NO-UNDO.
DEF VAR liPrepaid         AS INT                      NO-UNDO.
DEF VAR lcError           AS CHAR                     NO-UNDO.
DEF VAR liError           AS INT                      NO-UNDO.
DEF VAR ldeFee            AS DECIMAL                  NO-UNDO.  
DEF VAR lcBundle          AS CHAR                     NO-UNDO.
DEF VAR llUpdateBundle    AS LOG                      NO-UNDO. 
DEF VAR llUpdateBankAcc   AS LOG                      NO-UNDO.
DEF VAR lcBundleCLITypes  AS CHAR                     NO-UNDO.
DEF VAR lcBONOContracts   AS CHAR                     NO-UNDO.
DEF VAR lcProValidation   AS CHAR                     NO-UNDO.
DEF VAR llAddLineTerm     AS LOG                      NO-UNDO.

DEF BUFFER UserCustomer FOR CUstomer .
DEF BUFFER NewCliType   FOR CliType.
DEF BUFFER AgrCust      FOR Customer.
DEF BUFFER bMobsub      FOR Mobsub.
DEF BUFFER bbMobsub     FOR Mobsub.
DEF BUFFER bCliType     FOR CLiType.

FIND FIRST MobSub WHERE
           MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.

FIND MSISDN WHERE
     MSISDN.CLI  = MobSub.CLI NO-LOCK NO-ERROR.
     
FIND FIRST BillTarg WHERE
           BillTarg.CustNum    = Mobsub.CustNum AND
           BillTarg.BillTarget = Mobsub.BillTarget NO-ERROR.
                           
FIND FIRST Customer WHERE
           Customer.CustNum  = MobSub.CustNum NO-LOCK NO-ERROR.
                                      
FIND FIRST AgrCust WHERE 
           AgrCust.CustNum = Mobsub.AGrCust NO-LOCK NO-ERROR.

FIND FIRST CLIType WHERE
           CLIType.Clitype = MobSub.CliType  AND
           CliType.Brand   = Syst.Var:gcBrand NO-LOCK NO-ERROR.
           
lcAgrCustName = Func.Common:mDispCustName(BUFFER Agrcust).

ASSIGN lcBONOContracts       = fCParamC("BONO_CONTRACTS")
       lcBundleCLITypes      = fCParamC("BUNDLE_BASED_CLITYPES").

form /* asks MSISDN number */
   " Subscription ID:" Mobsub.MSSeq
       "Subscr. Active :" AT 41 liAge FORMAT ">>>9" "(Days)"            SKIP
 
   " MSISDN ........:" MobSub.CLI  no-label format "x(30)"      SKIP
   
   " ICC number ....:" Mobsub.ICC 
       "Per.contr.type :" AT 41 liPerType lcPerTypeName                 SKIP
       
   " VALID CliType..:"  MobSub.CliType CLIType.CliName FORMAT "X(11)"                   "Per.contr.valid:" AT 41 liPerValid                              SKIP 

   " Active requests:" llActiveReq   
       "Penalty fee....:" AT 41 llPenalty                               SKIP
                                      
   " Barrings ......:" llBarring                                     
       "Credit check...:" AT 41 llCreditCheck                           SKIP   
   " Agr.customer...:" AgrCust.CustNum  lcAgrCustName                   SKIP
   " Cust.ID type...:" AgrCust.CustIDType                 
       "Active prepaid.:" AT 41 liPrepaid                               SKIP
   " Customer ID....:" AgrCust.OrgID                                    
       "Active postpaid:" AT 41 liPostPaid                              SKIP
"------------------------------------------------------------------------------"                                                                       SKIP
   " New CLIType....:" new-type FORMAT "X(16)"
        HELP "Enter new mobile CONNECTION Cli Type"
       "(F9)"     
      "Bank Acc..:" AT 41 lcBankAccount FORMAT "X(24)"              SKIP
   " Change date....:" new-ts-date  FORMAT "99-99-99"                  
      "(F9)"
       "Bank name...:" AT 41 lcBankName                              SKIP
   " Change time....:" new-ts-time  FORMAT "99.99"
     HELP "Time format (hh.mm)"                                         SKIP
   
   " Change fee ....:"   llCreateFees SKIP
   " Data Bundle....:" lcBundle FORMAT "x(12)" 
   WITH  OVERLAY ROW 2 centered
   COLOR VALUE(Syst.Var:cfc)TITLE COLOR VALUE(Syst.Var:ctc) " Subscription type change " 
   NO-LABELS SIDE-LABEL FRAME main.

FUNCTION FTime2Dec RETURNS DECIMAL
   (itime AS INT ).

   DEF VAR ctime AS CHAR NO-UNDO.
   
   ctime = string(itime,"hh:mm").
   IF SESSION:NUMERIC-DECIMAL-POINT = "," 
   THEN substr(ctime,3,1) = ",".
   ELSE substr(ctime,3,1) = ".".

   RETURN  DECIMAL(ctime).

END.

FUNCTION FDec2Time RETURNS INTEGER
    (dtime AS DECIMAL).

    DEF VAR hh AS I NO-UNDO.
    DEF VAR mm AS I NO-UNDO.
   
    hh = TRUNCATE (dtime,0).
    mm = (dtime - hh) * 100.
    RETURN hh * 3600 + mm * 60.
END.

IF lcRight NE "RW" THEN DO:
   MESSAGE 
      "You don't have right to change" SKIP
      "the CLI Type of this subscription !"
   VIEW-AS ALERT-BOX.
   RETURN.
END.

ASSIGN   
   llCreateFees  = TRUE
   llSendSMS     = TRUE
   llPenalty     = FALSE
   llBankAccount = FALSE
   llBarring     = FALSE
   llMNP         = FALSE
   llCreditCheck = TRUE
   liCreditCheck = 1
   new-ts-time   = FTime2Dec(time).

Func.Common:mSplitTS(Mobsub.ActivationTS, OUTPUT odtDate, oiTime).

liAge = today - odtDate.

IF fChkSTCPerContr() EQ TRUE THEN DO:
   
   ASSIGN 
      llPenalty     = TRUE
      liPerType     = 1
      lcPerTypeName = "Terminaldiscount"
      liPerValid    = DCCli.ValidTo.
END.

FIND FIRST UserCustomer WHERE 
           UserCustomer.Custnum = Mobsub.Custnum NO-LOCK NO-ERROR.
           
IF Avail UserCustomer THEN UserName = Func.Common:mDispCustName(BUFFER UserCustomer).
ELSE UserName = "".                                       

/* is there a time limit for scheduling (on old type) */
lcTimeLimit = fServAttrValue(MobSub.CLIType,
                             "TypeChg",
                             "SchedTime",
                             OUTPUT ok). 

/* is there a time limit for scheduling (on old type) */
liBankAccount = INT(fServAttrValue(MobSub.CLIType,
                             "TypeChg",
                             "BankAccount",
                             OUTPUT ok)).

/* is there a time limit for scheduling (on old type) */
lcCreditCheck = fServAttrValue(MobSub.CLIType,
                             "TypeChg",
                             "CreditCheck",
                              OUTPUT ok).

IF lcCreditCheck = "0" then liCreditCheck = 0.


ASSIGN 
   lcBankAccount = Customer.BankAcct.
   

IF lcBankAccount ne "" THEN DO:
   
   FIND FIRST Bank WHERE
              Bank.Brand      = Syst.Var:gcBrand AND
              Bank.BankID     = SUBSTRING(lcBankAccount,5,4) AND
              Bank.BankOffice = SUBSTRING(lcBankAccount,9,4) 
   NO-LOCK NO-ERROR.
   
   IF AVAILABLE Bank THEN ASSIGN lcBankName = Bank.Name.
   ELSE                          lcBankName = "".

END.

FOR EACH bbMobsub WHERE 
         bbMobsub.CustNum = Mobsub.AgrCust  
No-LOCK.

   IF   bbMobsub.Paytype = false THEN liPostPaid = liPostPaid + 1.
   ELSE                               liPrePaid  = liPrePaid  + 1.
ENd.

IF lcTimeLimit NE ? AND lcTimeLimit > "" THEN DO:
   ldDefTime = INTEGER(ENTRY(1,lcTimeLimit,".")) + 
               INTEGER(ENTRY(2,lcTimeLimit,".")) / 100.

END. 
/* current time if no rules */
ELSE ASSIGN lcInfo    = STRING(TIME,"hh:mm")
            ldDefTime = INTEGER(ENTRY(1,lcInfo,":")) +
                        INTEGER(ENTRY(2,lcInfo,":")) / 100.
 
new-ts-time = ldDefTime.

PAUSE 0.
DISP 
   Mobsub.MSSeq 
   MobSub.CLI  
   Mobsub.ICC 
   llActiveReq
   MobSub.CliType
   CLIType.CliName WHEN AVAIL CLIType
   liPerValid
   lcBankAccount
   new-ts-time
   liAge
   lcOperatorBarring
   lcMobsubInDays
   llPenalty
   lcFailedRequest
   llBarring
   AgrCust.OrgID
   AgrCust.CustIDType
   lcMNPOngoing
   AgrCust.CustNum
   lcAgrCustName
   lcBankAccount
   llCreateFees
   lcBankName
   llCreditCheck
   lipertype
   lcpertypename
   liPrePaid
   liPostPaid
WITH FRAME main.

/* Second loop */ 

MAIN:
REPEAT  WITH FRAME main:

   Syst.Var:ehto = 9. RUN Syst/ufkey.p.
   IF llBarring OR 
      llMnp THEN DO:
      MESSAGE {&MSG_NOT_ALLOWED}. PAUSE NO-MESSAGE.
      RETURN.
   END.   

   lcBundle = "".

   UPDATE
   new-type
   new-ts-date
   WITH FRAME main  EDITING:

             READKEY.

             IF FRAME-FIELD = "new-type" AND KEYLABEL(LASTKEY) = "F9" THEN DO:
                RUN Help/h-mobtype.p.
                Syst.Var:ehto = 9. RUN Syst/ufkey.p.
                IF siirto NE ? THEN DO:
                   DISP siirto @ new-type WITH FRAME main.
                   NEXT.
                END.
             END.
             
             ELSE IF FRAME-FIELD = "new-ts-date" AND 
                  KEYLABEL(LASTKEY) = "F9" THEN DO:
             
                RUN Help/h-date.p(lcchangedate,mobsub.msseq,
                           INPUT FRAME main new-type).

                Syst.Var:ehto = 9. RUN Syst/ufkey.p.
                
                IF siirto NE ? THEN DO:
                   DISP siirto @ new-ts-date WITH FRAME main.
                   LEAVE.
                END.
             END.
             
             DO WITH FRAME main:
                PAUSE 0.
                IF LOOKUP(KEYLABEL(LASTKEY),"F4") > 0 THEN RETURN.
             
                IF FRAME-FIELD ="new-type" THEN DO:
                
                   IF INPUT FRAME main new-type = "" THEN DO:
                      Syst.Var:ehto = 9. RUN Syst/ufkey.p.
                      IF siirto NE ? THEN DO:
                         DISP siirto @ new-type WITH FRAME main.
                         NEXT.
                      END.
                   END.
                   liError = fValidateNewCliType(
                                       INPUT FRAME main new-type,
                                       INPUT "", /* Subs. data bundle */
                                       INPUT TRUE,  /* ByPass */
                                       OUTPUT lcError).
                   IF liError > 0 THEN DO:
                        BELL.
                        MESSAGE
                        lcError
                        VIEW-AS ALERT-BOX.
                        NEXT-PROMPT new-type.
                        NEXT.
                   END.     
                   
                   FIND FIRST PriceList WHERE 
                              PriceList.Brand     = Syst.Var:gcBrand AND 
                              Pricelist.PriceList = Clitype.PricePlan 
                   NO-LOCK NO-ERROR.

                   IF AVAIL PriceList AND 
                            PriceList.InclVAT NE Customer.VATIncl
                   THEN DO:
                      BELL.
                      MESSAGE
                      "VAT Handling of customer is not consistent with"
                      "new CLI Type's Price List "
                      VIEW-AS ALERT-BOX.
                   END.
                   
                   FIND FIRST NewCliType WHERE
                              NewCLIType.Brand = Syst.Var:gcBrand AND
                              NewCLIType.CLIType = INPUT new-type NO-LOCK.
                   IF NewCLIType.PayType = 2 THEN 
                      liCreditcheck = 0.
                   
                   ASSIGN new-type-nimi = NewCLIType.CliName .
                END.
                
                ELSE IF FRAME-FIELD ="new-ts-date" THEN DO:
      
                   IF INPUT new-ts-date = ? THEN DO:
                      RUN Help/h-date.p(lcchangedate,mobsub.msseq,
                                 INPUT FRAME main new-type).
                      Syst.Var:ehto = 9. RUN Syst/ufkey.p.
                                                 
                      IF siirto NE ? THEN DO:
                         DISP siirto @ new-ts-date WITH FRAME main.
                         LEAVE.
                      END.   
                   END.
                   
                   IF INPUT FRAME main new-ts-date = ? THEN DO:
                      BELL.
                      MESSAGE 
                      "Mandatory field value missing or value in wrong format".
                      NEXT.
                   END.
             
                   RUN Help/h-date.p(lcchangedate,mobsub.msseq,
                              INPUT FRAME main new-type).
                    
                   Syst.Var:ehto = 9. RUN Syst/ufkey.p.
                                    
                   IF siirto NE ? THEN DO:
                      DISP siirto @ new-ts-date WITH FRAME main.
                      LEAVE.
                   END.
                                   
                   
                   ASSIGN FRAME main new-ts-date.
                END.
                ELSE IF FRAME-FIELD ="new-ts-time" THEN DO:

                   IF INPUT FRAME main new-ts-time > 24 OR
                      INPUT new-ts-time < 0 THEN DO:
                
                      BELL.
                      MESSAGE "Hour Must Be Between 0 To 23 !".
                      NEXT.
                   END.
                   ELSE DO: 
                      ASSIGN 
                         new-ts-time = INPUT new-ts-time
                         minutes     = int(substring(string(new-ts-time),4,2)).
   
                      IF Minutes >= 60 THEN DO:
                         BELL.
                         message "Minutes Must Be Less Than 60 !".
                         NEXT.
                      END.
            
                      ASSIGN 
                         new-ts-time  = INPUT new-ts-time
                         new-ts-begin = YEAR (new-ts-date) * 10000 + 
                                        month(new-ts-date) * 100   +
                                        DAY  (new-ts-date)         +
                                        (int(new-ts-time) * 3600    +
                                        minutes * 60)  / 100000  .
                   END.
                
                END.

             END.
   
             APPLY LASTKEY. 

      END. /* EDITING */

      FIND FIRST NewCliType WHERE
                 NewCLIType.Brand = Syst.Var:gcBrand AND
                 NewCLIType.CLIType = new-type NO-LOCK NO-ERROR.
      
      ASSIGN
         llUpdateBankAcc = (lcBankAccount = "" AND 
                            AVAILABLE NewCLIType AND NewCLIType.PayType NE 2).

      IF LOOKUP(NewCliType.CLIType,lcBundleCLITypes) > 0 THEN
         llUpdateBundle = TRUE.

      IF llUpdateBankAcc OR llUpdateBundle
      THEN DO: 
         
         UPDATE lcBankAccount WHEN llUpdateBankAcc
                lcBundle WHEN llUpdateBundle
         WITH FRAME main  EDITING:
          
            READKEY.

            IF LOOKUP(KEYLABEL(LASTKEY),Syst.Var:poisnap) > 0 THEN DO WITH FRAME main:
            
               PAUSE 0.
                                           
               IF FRAME-FIELD = "lcBankAccount" THEN DO:
                  IF fCheckBankAcc(INPUT Frame Main lcBankAccount) = FALSE 
                     THEN DO:
                        MESSAGE 
                        "Mandatory value missing or in wrong format" 
                        VIEW-AS ALERT-BOX.
                        NEXT-PROMPT lcBankAccount. 
                        NEXT.
                  END.
               END.
               
               ELSE IF FRAME-FIELD = "lcBundle" THEN DO:
                  IF NOT fCheckSTCBundle(new-type,  
                                         INPUT INPUT lcBundle,
                                         OUTPUT lcInfo) THEN DO:
                     MESSAGE lcInfo VIEW-AS ALERT-BOX.
                     NEXT-PROMPT lcBundle. 
                     NEXT.
                  END.
               END.
            END.

            /* DCH */
            IF MobSub.Paytype = TRUE AND
               NewCLIType.PayType = 1 AND
               NOT CAN-FIND(FIRST bMobSub WHERE
                                  bMobSub.Brand     = Syst.Var:gcBrand AND
                                  bMobSub.MsSeq    <> MobSub.MsSeq AND
                                  bMobSub.CustNum   = Customer.CustNum AND
                                  bMobSub.PayType   = FALSE) THEN DO:

               llBankAccount = TRUE.

            END.

            APPLY LASTKEY.
         END.
      END.



   IF new-ts-date = ? THEN DO:
      MESSAGE "Invalid change date"
         VIEW-AS ALERT-BOX ERROR.
      NEXT.
   END.   

   new-ts-begin = YEAR (new-ts-date) * 10000 + /*yyyymmdd*/
                  month(new-ts-date) * 100   +
                  DAY  (new-ts-date)         +
                  (int(new-ts-time) * 3600    +
                   minutes * 60)  / 100000  .

   /* is timing correct; if either type should be changed only on 
      1. of month then accept only that */
   lcInfo = fChkTiming(MobSub.CLIType,
                       new-type,
                       new-ts-date). 
   IF lcInfo > "" THEN DO:
      MESSAGE "Check beginning day;" SKIP
              lcInfo 
      VIEW-AS ALERT-BOX ERROR.
      NEXT.
   END.

   IF fValidateNewCliType(new-type, INPUT lcBundle, TRUE, lcError) > 0 THEN DO:
      MESSAGE
         lcError 
      VIEW-AS ALERT-BOX TITLE "NO CHANGES".
      NEXT.
  END.

  IF fValidateMobTypeCh(
      MobSub.Msseq,
      INPUT new-type,
      new-ts-begin,
      FALSE, /* extend contract */
      FALSE, /* bypass stc type check */
      0, /* stc order id */
      {&REQUEST_SOURCE_MANUAL_TMS},
      FALSE,
      OUTPUT lcError) EQ FALSE THEN DO:
     MESSAGE
         lcError 
     VIEW-AS ALERT-BOX TITLE " Change Cancelled ".
     RETURN.
  END.

  /*YPRO*/
  lcProValidation = fValidateProSTC(MobSub.Custnum, 
                                    MobSub.CliType,
                                    new-type). 
  IF lcProValidation NE "" THEN DO: 
     MESSAGE
         "Pro customer validation failed: " + lcProValidation
     VIEW-AS ALERT-BOX TITLE " Change Cancelled ".
     RETURN. 
  END.


  
  if liCreditCheck = 1 THEN DO:
     FIND FIRST NewCliType WHERE
                NewCLIType.Brand = Syst.Var:gcBrand AND
                NewCLIType.CLIType = INPUT new-type NO-LOCK NO-ERROR.
     IF AVAILABLE NewCLIType AND NewCLIType.PayType = 2 THEN 
        liCreditcheck = 0.
  END.
  
   /* time limit has been set for scheduling */
   IF ldDefTime > 0 AND ldDefTime < new-ts-time THEN DO:
      MESSAGE "Change should be scheluded to be done before"
              lcTimeLimit SKIP
              "Otherwise limits and monthly fee for old"
              "CLI type will be billed also for the period that covers"
              "time after the scheduled date."
      VIEW-AS ALERT-BOX INFORMATION.
   END.

   ACTION:
   REPEAT WITH FRAME main:

      ASSIGN
      Syst.Var:ufk = 0 Syst.Var:ehto = 0
      Syst.Var:ufk[1] = 7 
      Syst.Var:ufk[5] = 795
      Syst.Var:ufk[8] = 8.
      RUN Syst/ufkey.p.

      IF Syst.Var:toimi = 1 THEN NEXT  main.
      
      IF Syst.Var:toimi = 8 THEN LEAVE main.
      
      IF Syst.Var:toimi = 5 THEN DO TRANS:
         
         RUN Mc/charge_dialog.p(
            MobSub.MsSeq,
             (IF MobSub.PayType THEN "STC_PREPAID" ELSE "STC_POSTPAID"),
            OUTPUT ldeFee).
         
         llCreateFees = (ldeFee > 0).
         
         IF Mobsub.MultiSimType EQ {&MULTISIMTYPE_PRIMARY} AND
            Mobsub.MultiSimID > 0 THEN DO:
      
            FIND FIRST bbMobSub NO-LOCK USE-INDEX MultiSIM WHERE
                       bbMobSub.Brand = Syst.Var:gcBrand AND
                       bbMobSub.MultiSimId = Mobsub.MultiSimId AND
                       bbMobSub.MultiSimType = {&MULTISIMTYPE_SECONDARY} AND
                       bbMobSub.Custnum = Mobsub.Custnum
            NO-ERROR.
            IF AVAIL bbMobSub AND
               NOT CAN-FIND (FIRST MsRequest WHERE
                   MsRequest.MsSeq   = bbMobSub.Msseq AND
                   MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
                   LOOKUP(STRING(MsRequest.ReqStatus),
                          {&REQ_INACTIVE_STATUSES}) = 0) AND
               NOT CAN-FIND (FIRST MsRequest WHERE
                   MsRequest.MsSeq   = bbMobSub.Msseq AND
                   MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
                   LOOKUP(STRING(MsRequest.ReqStatus),
                   {&REQ_INACTIVE_STATUSES}) = 0) AND
               NOT Mnp.MNPOutGoing:mIsMNPOutOngoing(INPUT bbMobSub.CLI) THEN DO:
               MESSAGE "STC will also trigger subscription " +
                       bbMobSub.CLI + " termination (multisim secondary subscription)"
               VIEW-AS ALERT-BOX.
            END.
         END.
         ELSE IF CAN-FIND(FIRST CLIType NO-LOCK WHERE
                                CLIType.Brand = Syst.Var:gcBrand AND
                                CLIType.CLIType = Mobsub.TariffBundle AND
                                CLIType.LineType = {&CLITYPE_LINETYPE_MAIN})
                AND
                NOT CAN-FIND(FIRST CLIType NO-LOCK WHERE
                                   CLIType.Brand = Syst.Var:gcBrand AND
                                   CLIType.CLIType = lcBundle AND
                                   CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}) THEN DO:

            llAddLineTerm = FALSE.

            FOR EACH bbMobSub NO-LOCK WHERE
                     bbMobSub.Brand   = Syst.Var:gcBrand AND
                     bbMobSub.InvCust = Mobsub.CustNum AND
                     bbMobSub.PayType = FALSE AND
                     bbMobSub.MsSeq NE Mobsub.MsSeq,
               FIRST bCliType NO-LOCK WHERE
                     bCliType.Brand = Syst.Var:gcBrand AND
                     bCliType.CLiType = (IF bbMobSub.TariffBundle > ""
                                        THEN bbMobSub.TariffBundle
                                        ELSE bbMobSub.CLIType) AND
                     bCliType.LineType > 0:

               IF bCliType.LineType EQ {&CLITYPE_LINETYPE_MAIN} THEN DO:
                  llAddLineTerm = FALSE.
                  LEAVE.
               END.
                     
               IF bCliType.LineType EQ {&CLITYPE_LINETYPE_ADDITIONAL} THEN DO:
                  IF fHasPendingRequests(
                        bbMobSub.MsSeq,
                        bbMobSub.CLI,
                        bCliType.LineType) THEN NEXT.
                  llAddLineTerm = TRUE.
               END.
            END.

            IF llAddLineTerm THEN
               MESSAGE "STC will trigger STC to CONT9 for additional line(s)"
               VIEW-AS ALERT-BOX.
         END. /* ELSE IF CAN-FIND(FIRST CLIType NO-LOCK WHERE */

         ok = FALSE.
         MESSAGE 
         "Make the change request?"  
         VIEW-AS ALERT-BOX QUESTION
         BUTTONS YES-NO
         UPDATE ok.

         IF NOT ok THEN NEXT Action.

         if llBankAccount = FALSE THEN lcBankAccount = "".
         
         /* add entry to request spool, THIS IS MAIN Request */                
         i = fCTChangeRequest(MobSub.MsSeq,
                              new-type,
                              lcBundle,
                              lcBankAccount ,
                              new-ts-begin,
                              liCreditCheck,  /* 0 = Credit check ok */
                              0, /* extend contract 0=no extend_term_contract */
                              "", /* salesman */
                              llCreateFees,
                              llSendSMS,
                              "",
                              ldeFee,
                              {&REQUEST_SOURCE_MANUAL_TMS},
                              0, /* order id */
                              0,
                              "", /*request_id*/
                              OUTPUT lcInfo).
      
         MESSAGE "Request ID for CLI Type change is" i
         VIEW-AS ALERT-BOX
         TITLE " Request Added ".
      END.
      LEAVE main.   
   END.
END. /* MAIN */
HIDE FRAME main NO-PAUSE.
HIDE MESSAGE.

