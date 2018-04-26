/* ===========================================================================
 MODULE ........: deletems1.p
 APPLICATION ...: CLOSE DOWN A MOBILE SUBSCRIPTION 
 TASK ..........:
 CREATED .......: 06.08.99 jp 
 CHANGED .......: 22.09.99 pt final fixes; activated
                  10.11.99 pt confirm deletion of a SIM
                  13.12.99 pt RETURN code from deletems2
                  22.02.00 pt UNDO TRANSACTION IF deletems2 failed
                  15.03.00 pt corrected forced UNDO after EACH deletems
                  14.01.01 pt scheduled kill
                  29.08.01 pt OutPort option (mnp)
                  27.09.01 pt fix calc of earliest outporting time
                  26.10.01/aam save the porting time AS it is, LEAVE the 
                               logic FOR porting time slot TO mnpbatch.p
                  18.12.01/pt  only forced kill allowed IF status = 3    
                  13.03.03/tk  tokens
                  15.05.03/tk  immediate by default
                  13.09.03 jp   Brand 
                  20.04.06/aam check for valid periodical contract
                  28.09.06/tk  allow porting time change for M2M_049
                  17.01.07/aam valid periodical contract doesn't prevent
                               killing,
                               ask whether a penalty fee should be created
                               from periodical contract
                  05.10.07/as  Changed and added new termination parameters
 Version .......: xfera 
 ============================================================================*/

DEFINE INPUT PARAMETER piMsSeq LIKE MobSub.MsSeq.

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'MobSub'}
{Func/fsubstermreq.i}
{Func/msisdn_prefix.i}
{Syst/tmsconst.i}
{Func/cparam2.i}
{Func/add_lines_request.i}

IF lcRight NE "RW" THEN DO:
   MESSAGE
      "You don't have right to" SKIP
      "kill this subscription !"
   VIEW-AS ALERT-BOX.
   RETURN.
END.

DEFINE VARIABLE ok            AS LOG   NO-UNDO FORMAT "Yes/No".
DEFINE VARIABLE ldtKillDate   AS DATE  NO-UNDO FORMAT "99.99.99".
DEFINE VARIABLE ldtKillDateMax AS DATE NO-UNDO FORMAT "99.99.99".
DEFINE VARIABLE KillTime      AS DEC   NO-UNDO FORMAT "99.99".
DEFINE VARIABLE lcOutOper     AS CHAR  NO-UNDO FORMAT "x(12)".
DEFINE VARIABLE hh            AS INT   NO-UNDO.
DEFINE VARIABLE mm            AS INT   NO-UNDO.

DEFINE VARIABLE lcUserCode       AS CHAR  NO-UNDO.
DEFINE VARIABLE ldtPContr        AS DATE  NO-UNDO.
DEFINE VARIABLE llPenalty        AS LOG   NO-UNDO. 
DEFINE VARIABLE lcUsrName        AS CHAR  NO-UNDO.
DEFINE VARIABLE lcAgrName        AS CHAR  NO-UNDO.
DEFINE VARIABLE lcInvName        AS CHAR  NO-UNDO.
DEFINE VARIABLE ocResult         AS CHAR  NO-UNDO.
DEFINE VARIABLE ldeKillTS        AS DEC   NO-UNDO.
DEFINE VARIABLE liMsReq          AS INT   NO-UNDO.
DEFINE VARIABLE liOrderer        AS INT   NO-UNDO.
DEFINE VARIABLE lcOrderer        AS CHAR  NO-UNDO.
DEFINE VARIABLE liQuarTime       AS INT   NO-UNDO.
DEFINE VARIABLE lcQuarTime       AS CHAR  NO-UNDO.
DEFINE VARIABLE lcCode           AS CHAR  NO-UNDO.
DEFINE VARIABLE lcCodeName       AS CHAR  NO-UNDO.
DEFINE VARIABLE liMsisdnStat     AS INT   NO-UNDO.
DEFINE VARIABLE lcMsisdnStat     AS CHAR  NO-UNDO.
DEFINE VARIABLE liLastKey        AS INT   NO-UNDO.
DEFINE VARIABLE llPContr         AS LOG   NO-UNDO.
DEFINE VARIABLE llAdmin          AS LOG   NO-UNDO.
DEFINE VARIABLE llYoigoCLI       AS LOG   NO-UNDO.
DEFINE VARIABLE llMasmovilCLI    AS LOG   NO-UNDO.
DEFINE VARIABLE liSimStat        AS INT   NO-UNDO.
DEFINE VARIABLE lcSimStat        AS CHAR  NO-UNDO.
DEFINE VARIABLE llHelp           AS LOG   NO-UNDO INIT TRUE.
DEFINE VARIABLE lcError          AS CHAR  NO-UNDO.
DEFINE VARIABLE liError          AS INT   NO-UNDO.
DEFINE VARIABLE llBillPer        AS LOGICAL NO-UNDO.
DEFINE VARIABLE lcTermType       AS CHAR  NO-UNDO INIT "Full".
DEFINE VARIABLE lcTenant         AS CHAR NO-UNDO.
DEFINE VARIABLE llYoigoTenant    AS LOGI NO-UNDO INIT FALSE.
DEFINE VARIABLE llMasmovilTenant AS LOGI NO-UNDO INIT FALSE.
DEFINE VARIABLE llAddLineTerm    AS LOG  NO-UNDO.

DEFINE BUFFER UsrCustomer FOR Customer.
DEFINE BUFFER AgrCustomer FOR Customer.
DEFINE BUFFER InvCustomer FOR Customer.
DEFINE BUFFER lbMobsub    FOR Mobsub.
DEFINE BUFFER bbMobsub    FOR Mobsub.

FORM
   " Subscription ID ..:" MobSub.MsSeq 
   " Per.contr.type :" AT 42 llPContr FORMAT "1 Terminaldiscount/0 No Contracts"
   SKIP
   " MSISDN ...........:" MobSub.CLI
   " Per.contr.valid:" AT 42 ldtPContr FORMAT "99-99-99"
   SKIP
   " ICC / SIM.........:" MobSub.ICC                                   
   " Penalty Fee ...:" AT 42 llPenalty 
      HELP "Create a penalty fee for terminating periodical contract"
      FORMAT "Yes/No"
   SKIP
   " Agr. Customer ....:" AgrCustomer.CustNum lcAgrName FORMAT "X(40)" SKIP
   " Inv. Customer ....:" InvCustomer.CustNum lcInvName FORMAT "X(40)" SKIP
   " User .............:" UsrCustomer.CustNum lcUsrName FORMAT "X(40)" SKIP
   " --------------------------------------------------------------------- "
   SKIP

   " Orderer ..........:" liOrderer 
      HELP "Termination orderer (F9)"
      FORMAT ">9" lcOrderer FORMAT "x(32)"
                                                                       SKIP 
   " MNP Operator .....:" lcOutOper 
      HELP "To which operator ?"                   
      FORMAT "x(6)"                                                    SKIP
   
   " Deactivation Date :" ldtKillDate
      HELP "Date when subscription shall be killed"                    SKIP

   " Deactivation Time :" KillTime                                       
      HELP "Exact time when a SCHEDULED kill shall be performed"       SKIP

   " Termination type :" lcTermType
      HELP "Termination type Full / Partial"                           SKIP
   
   " MSISDN status ....:" liMsisdnStat FORMAT ">9"
      HELP "MSISDN status after termination (F9)"
      lcMsisdnStat FORMAT "x(32)"
   SKIP
   
   " Quarantine time ..:" liQuarTime FORMAT ">9" lcQuarTime FORMAT "x(16)"                    SKIP
  
   " ICC status .......:" liSimStat FORMAT ">9" lcSimStat FORMAT "x(16)"
      HELP "ICC status after termination (F9)" SKIP

WITH
   OVERLAY ROW 2 CENTERED COLOR VALUE(Syst.Var:cfc)
   TITLE COLOR VALUE(Syst.Var:ctc) 
      " Subscription Termination / De-activation " + MobSub.CLI + " "
   NO-LABELS  
FRAME main.

IF getTMSRight("CCSUPER,SYST") EQ "RW" THEN llAdmin = TRUE.

FIND MobSub WHERE MobSub.MsSeq = piMsSeq NO-LOCK.

liError = fDeleteMsValidation(piMsSeq, 
                              ?, /* termination reason not yet known */
                              OUTPUT lcError).
IF lcError NE "" THEN DO:
   MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
   IF liError NE 0 THEN RETURN.
END.

ASSIGN
   lcUserCode = Syst.Var:katun
   ldtPContr  = ?.

llPenalty = fIsPenalty(0, Mobsub.MsSeq).
IF AVAIL DCCLI THEN ldtPContr = DCCLI.ValidTo.

/* not for prepaid */
IF MobSub.PayType = TRUE THEN llPenalty = FALSE.

FIND FIRST UsrCustomer WHERE 
           UsrCustomer.CustNum = Mobsub.CustNum
NO-LOCK NO-ERROR.

FIND FIRST InvCustomer WHERE 
           InvCustomer.CustNum = Mobsub.InvCust
NO-LOCK NO-ERROR.

FIND FIRST AgrCustomer WHERE
           AgrCustomer.CustNum  = MobSub.AgrCust
NO-LOCK NO-ERROR.

IF Avail UsrCustomer THEN
   lcUsrName = Func.Common:mDispCustName(BUFFER UsrCustomer).
ELSE
   lcUsrName = "".

IF Avail InvCustomer THEN
   lcInvName = Func.Common:mDispCustName(BUFFER InvCustomer).
ELSE
   lcInvName = "".

IF Avail AgrCustomer THEN
   lcAgrName = Func.Common:mDispCustName(BUFFER AgrCustomer).
ELSE
   lcAgrName = "".

PAUSE 0.

IF MobSub.MSStatus = 3 /* NOT activated yet */ THEN DO:
   
   MESSAGE
      "Subscription status is '3'"                   SKIP(1)   
      "This Subscription is NOT YET ACTIVATED"       SKIP
      "in HLR.  Hence the only kill method You can"  SKIP
      "use is the 'forced' kill that only cleans up" SKIP
      "this subscription from billing database but"  SKIP
      "does not commence any HLR Delete Command."    SKIP(1)
      "You can still choose 2 different actions:"    SKIP
      " - BATCH     if the MSISDN was outported or"  SKIP
      " - IMMEDIATE if no outporting was done     "
   VIEW-AS ALERT-BOX WARNING.

END.

/* Yoigo MSISDN? */
ASSIGN
   lcTenant         = BUFFER-TENANT-NAME(MobSub)
   llYoigoCLI       = fIsYoigoCLI(Mobsub.CLI) 
   llMasmovilCLI    = fIsMasmovilCLI(Mobsub.CLI)   
   llYoigoTenant    = (IF lcTenant = {&TENANT_YOIGO}    THEN TRUE ELSE FALSE)  
   llMasmovilTenant = (IF lcTenant = {&TENANT_MASMOVIL} THEN TRUE ELSE FALSE).

DISPLAY
   MobSub.MsSeq
   MobSub.CLI
   MobSub.ICC
   AgrCustomer.CustNum lcAgrName
   InvCustomer.CustNum lcInvName
   UsrCustomer.CustNum lcUsrName
   ldtPContr
   llPenalty
   ldtPContr NE ? @ llPContr
WITH FRAME main.

   ldtKillDate = today.
   killtime = TRUNC(time / 3600,0) + 1.
MAIN:
REPEAT WITH FRAME main:

   Syst.Var:ehto = 9. RUN Syst/ufkey.p.
   UPDATE 
      liOrderer 
      lcOutOper WHEN liOrderer EQ 2
      ldtKillDate
         VALIDATE(INPUT ldtKillDate = ? OR INPUT ldtKillDate >= TODAY,
         "Date other than EMPTY must not be earlier than today !")
      KillTime
      lcTermType     WHEN fIsConvergenceTariff(Mobsub.clitype)
      liMsisdnStat   WHEN ((llYoigoCLI AND llYoigoTenant) OR (llMasmovilCLI AND llMasmovilTenant)) AND liOrderer EQ 5
      liQuarTime     WHEN liOrderer EQ 5 AND liMsisdnStat EQ 4 AND
                          liQuarTime > -1
      liSimStat      WHEN liOrderer EQ 5
   WITH FRAME main 
   EDITING:
      
      on f2 bell.
      
      IF liLastKey NE 0 THEN DO:
         APPLY liLastKey.
         liLastKey = 0.
      END.
      READKEY.
      
      IF FRAME-FIELD = "liOrderer" AND KEYLABEL(LASTKEY) = "F9" THEN DO:
            
         RUN Help/h-tmscodes.p(INPUT "MsRequest",  /* TableName */
                             "TermReason",  /* FieldName */
                             "Request",   /* GroupCode */
                       OUTPUT lcCode).


         IF lcCode ne "" AND lcCode NE ? THEN DO:
               
               IF INT(lcCode) = 3 AND ((NOT llYoigoCLI AND llYoigoTenant) OR (NOT llMasmovilCLI AND llMasmovilTenant)) THEN DO:
                  MESSAGE "Cannot choose Order cancellation with MNP numbers!"
                  VIEW-AS ALERT-BOX.
                  NEXT.
               END.
               
               liOrderer = INT(lcCode). 
               lcOrderer = Func.Common:mTMSCodeName("MsRequest",
                              "TermReason",
                              lcCode).
           
            DISPLAY liOrderer lcOrderer WITH FRAME main.   
         END.   

         llHelp = TRUE. 
         Syst.Var:ehto = 9.
         RUN Syst/ufkey.p.
         NEXT. 
      END.
      
      IF FRAME-FIELD = "liMsisdnStat" AND KEYLABEL(LASTKEY) = "F9" THEN DO:
         
         RUN Syst/tmscodesel.p(INPUT "MSISDN",  
                              "StatusCode",
                              "MSISDN",
                              "",
                              TRUE,
                              "2,4,5", 
                       OUTPUT lcCode,
                       OUTPUT lcCodeName).
         
         IF lcCode ne "" AND lcCode NE ? THEN DO:
            liMsisdnStat = INT(lcCode). 
            lcMsisdnStat = lcCodeName.
            DISPLAY liMsisdnStat lcMsisdnStat WITH FRAME main.   
            llHelp = TRUE. 
         END.   
         
         Syst.Var:ehto = 9.
         RUN Syst/ufkey.p.
         NEXT. 
      
      END.

      IF FRAME-FIELD = "liSimStat" AND KEYLABEL(LASTKEY) = "F9" THEN DO:
            
         RUN Syst/tmscodesel.p(INPUT "SIM",  
                              "SimStat",
                              "SIM",
                              "",
                              TRUE,
                              "7,9", 
                       OUTPUT lcCode,
                       OUTPUT lcCodeName).
         
         IF lcCode ne "" AND lcCode NE ? THEN DO:
               liSimStat = INT(lcCode). 
               lcSimStat = lcCodeName.
            DISPLAY liSimStat lcSimStat WITH FRAME main.
            llHelp = TRUE.
         END.   

         Syst.Var:ehto = 9.
         RUN Syst/ufkey.p.
         NEXT. 
         
      END.
      
      IF LOOKUP(KEYLABEL(LASTKEY),Syst.Var:poisnap) > 0  THEN DO WITH FRAME main:
         PAUSE 0.
        
         IF FRAME-FIELD = "liOrderer" THEN DO: 
            IF INPUT liOrderer EQ liOrderer AND llHelp = FALSE THEN DO:
               APPLY LASTKEY.
               NEXT.
            END. 

            IF INPUT liOrderer = 3 AND ((NOT llYoigoCLI AND llYoigoTenant) OR (NOT llMasmovilCLI AND llMasmovilTenant)) THEN DO:
               MESSAGE "Cannot choose Order cancellation with MNP numbers!"
               VIEW-AS ALERT-BOX.
               NEXT-PROMPT liOrderer.
               NEXT.
            END.

            lcOrderer = "".

            FIND FIRST TMSCodes WHERE
               TMSCodes.TableName = "MsRequest" AND
               TMSCodes.FieldName = "TermReason" AND
               TMSCodes.CodeValue = INPUT liOrderer AND
               TMSCodes.CodeGroup = "Request" NO-LOCK NO-ERROR.
            IF AVAIL TMSCodes THEN lcOrderer = TMSCodes.CodeName.
            
            IF lcOrderer EQ "" THEN DO:
               MESSAGE "Unknown Orderer" VIEW-AS ALERT-BOX.   
               NEXT-PROMPT liOrderer.
               NEXT.
            END.
            ELSE DO:
               
               IF NOT llAdmin AND INPUT liOrderer EQ 5 THEN DO:
                  MESSAGE 
                     "You have no access to this option!"
                  VIEW-AS ALERT-BOX.
                  NEXT.
               END.
               
               fInitialiseValues(
                  INPUT INPUT liOrderer,
                  INPUT llYoigoCLI,
                  INPUT llMasmovilCLI,
                  OUTPUT liMsisdnStat,
                  OUTPUT liSimStat,
                  OUTPUT liQuarTime).
               
               llPenalty = fIsPenalty(INPUT INPUT liOrderer, Mobsub.MsSeq).
               
               ASSIGN
                  ldtKillDate    = TODAY
                  lcMsisdnStat   = Func.Common:mTMSCodeName("MSISDN",
                           "StatusCode",
                           STRING(liMSISDNStat))
                  lcSimStat      = Func.Common:mTMSCodeName("SIM",
                           "SimStat",
                           STRING(liSimStat)).
               
               IF liQuartime >= 0 THEN DO:
                  IF liQuartime = 1 THEN lcQuarTime = "day".
                  ELSE lcQuarTime = "days".
               END.   
               ELSE DO:
                  lcQuarTime = "Indefinitely". 
               END.

               IF INPUT liOrderer NE 2 THEN 
                  lcOutOper = "".
               DISPLAY
                  lcOutOper
                  liMsisdnStat lcMsisdnStat
                  lcOrderer
                  ldtKillDate
                  liQuartime WHEN liQuarTime > -1 lcQuarTime
                  liSimStat lcSimStat 
                  llPenalty
                  WITH FRAME main.
             
               IF liQuarTime EQ -1 THEN HIDE liQuarTime IN FRAME MAIN.
               /* Enable or disable other fields */
              
               IF INPUT liOrderer NE liOrderer OR llHelp EQ TRUE THEN DO:
                  llHelp = FALSE.
                  liLastKey = LASTKEY.
                  NEXT MAIN.
               END.   
            END. 
         END.
        
         ELSE IF FRAME-FIELD = "liSimStat" THEN DO:
            IF fCheckSimStat(INPUT INPUT liSimStat, OUTPUT lcError) NE 0
            THEN DO:
               MESSAGE lcError VIEW-AS ALERT-BOX.
               lcSimStat = "".
               DISPLAY lcSimStat WITH FRAME MAIN.
               NEXT.
            END.
            
            lcSimStat = Func.Common:mTMSCodeName("SIM",
                        "SimStat",
                        STRING(INPUT liSimStat)).
            DISPLAY lcSimStat WITH FRAME MAIN.

         END.

         ELSE IF FRAME-FIELD = "lcOutOper" THEN DO:
            
            IF INPUT lcOutOper = "" THEN DO:
               MESSAGE
               "Operator number missing!" SKIP
               "Outporting this MSISDN requires receiving operator's number!"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
            END.    
            
            ELSE IF INPUT lcOutOper NE "" THEN DO:
             
               INT(INPUT lcOutOper) NO-ERROR.
             
               IF ERROR-STATUS:ERROR THEN DO:
                  MESSAGE
                     "Operator code must be an integer!"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
             
               IF fCheckOpCode(INT(INPUT lcOutOper), OUTPUT lcError) NE 0
               THEN DO:
                  MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.

            END.
         
             ASSIGN lcOutOper.

         END.

         ELSE IF FRAME-FIELD = "liMsisdnStat" THEN DO:
            
            IF fCheckMsisdnStat(INPUT INPUT liMSISDNStat, OUTPUT lcError) NE 0 
            THEN DO: 
               MESSAGE lcError VIEW-AS ALERT-BOX.
               lcMSISDNStat = "".
               DISPLAY lcMSISDNStat WITH FRAME MAIN.
               NEXT.
            END.
            
            lcMsisdnStat = Func.Common:mTMSCodeName("MSISDN",
                        "StatusCode",
                        STRING(INPUT liMSISDNStat)).
            DISPLAY lcMSISDNStat WITH FRAME MAIN.
            
            /* Enable or disable other fields */
            IF INPUT liMsisdnStat NE liMsisdnStat OR llHelp THEN DO:
               IF INPUT liMsisdnStat NE 4
                  THEN liQuarTime = 90.
               DISPLAY  liQuarTime WITH FRAME MAIN.
               llHelp = FALSE.
               liLastKey = LASTKEY.
               NEXT MAIN.
            END.
         END.

         ELSE IF FRAME-FIELD = "ldtKillDate" THEN DO:
            IF INPUT ldtKillDate = ? THEN DO:
               MESSAGE
                  "Kill Date MUST be defined for Batch Kill !"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
            END.
            
            CASE liOrderer:
               WHEN 1 OR WHEN 5  THEN ldtKillDateMax = TODAY + 60.
               WHEN 3 OR WHEN 4  THEN ldtKillDateMax = TODAY. 
               WHEN 2            THEN ldtKillDateMax = TODAY + 30.
            END.
            
            IF INPUT ldtKillDate > ldtKillDateMax THEN DO:
               MESSAGE
               "Incorrect or missing date value!"
               VIEW-AS ALERT-BOX.
               NEXT.
            END.
            
            ASSIGN ldtKillDate.
         END.   

         ELSE IF FRAME-FIELD = "KillTime" THEN DO:

            hh = INT(SUBSTR(FRAME-VALUE,1,2)).
            mm = INT(SUBSTR(FRAME-VALUE,4,2)).
            
            IF hh > 23 OR mm > 59 THEN DO:
               MESSAGE
                  "Time must be 00.00 ... 23.59 !"
               VIEW-AS ALERT-BOX
               TITLE "INVALID TIME".
               NEXT.
            END.

            IF (hh > 0 OR mm > 0) AND INPUT ldtKillDate = ? THEN DO:
               MESSAGE
                  "Kill Date is missing - please correct !".
               NEXT-PROMPT ldtKillDate.
               NEXT.
            END.   
            
            ldeKillTS = YEAR (ldtKillDate) * 10000 +
                        MONTH(ldtKillDate) * 100   +
                        DAY  (ldtKillDate) +
                             (hh * 3600 + mm * 60) / 100000.
            
            liError = fCheckKillTS(liOrderer,ldeKillTS,OUTPUT lcError).
            
            IF liError NE 0 THEN DO:
               MESSAGE lcError VIEW-AS ALERT-BOX.
               IF liError EQ 1 THEN NEXT-PROMPT ldtKillDate.
               NEXT.
            END.

         END.
         
         ELSE IF FRAME-FIELD = "lcTermType" THEN DO:
            IF lcTermType NE "Full" AND
               lcTermType NE "Partial" THEN DO:
               MESSAGE "Value must be Full or Partial!"
                  VIEW-AS ALERT-BOX.
               NEXT.
            END.   
            IF MobSub.msstatus NE {&MSSTATUS_MOBILE_NOT_ACTIVE} AND
               lcTermType EQ "Partial" THEN DO:
               MESSAGE "Mobile already terminated, only Full possible"
                  VIEW-AS ALERT-BOX.
               NEXT.
            END.
         END.

         ELSE IF FRAME-FIELD = "liQuarTime" THEN DO:
            IF INPUT liQuarTime < 1 OR INPUT liQuarTime > 90 THEN DO:
               MESSAGE "Value must be between 1 and 90!"
               VIEW-AS ALERT-BOX.
               NEXT.
            END.
         
         END.
      
      END.

      APPLY LASTKEY.

   END.    

   ACTION:
   REPEAT WITH FRAME main:
      
      ASSIGN
         Syst.Var:ufk = 0 
         Syst.Var:ehto = 0
         Syst.Var:ufk[1] = 7 
         Syst.Var:ufk[5] = 795
         Syst.Var:ufk[8] = 8.

      RUN Syst/ufkey.p.
      
      IF Syst.Var:toimi = 1 THEN NEXT  main.
      
      IF Syst.Var:toimi = 8 THEN LEAVE main.
      
      IF Syst.Var:toimi = 5 THEN DO:
         
         IF llPenalty THEN
            MESSAGE
               "This function will generate Discount penalty fee" SKIP 
               "calculation based on termination of valid" SKIP
               "Periodical contract"
            VIEW-AS ALERT-BOX. 
         
         ok = FALSE.
         
         IF Mobsub.MultiSimType EQ {&MULTISIMTYPE_PRIMARY} AND
            Mobsub.MultiSimID > 0 THEN DO:
      
            FIND FIRST lbMobSub NO-LOCK USE-INDEX MultiSimID WHERE
                       lbMobSub.Brand = Syst.Var:gcBrand AND
                       lbMobSub.MultiSimId = Mobsub.MultiSimId AND
                       lbMobSub.MultiSimType = {&MULTISIMTYPE_SECONDARY} AND
                       lbMobSub.Custnum = Mobsub.Custnum
            NO-ERROR.
            IF AVAIL lbMobSub AND
               NOT CAN-FIND (FIRST MsRequest WHERE
                   MsRequest.MsSeq   = lbMobSub.Msseq AND
                   MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
                   LOOKUP(STRING(MsRequest.ReqStatus),
                          {&REQ_INACTIVE_STATUSES}) = 0) AND
               NOT CAN-FIND (FIRST MsRequest WHERE
                   MsRequest.MsSeq   = lbMobSub.Msseq AND
                   MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
                   LOOKUP(STRING(MsRequest.ReqStatus),
                   {&REQ_INACTIVE_STATUSES}) = 0) AND
               NOT Mnp.MNPOutGoing:mIsMNPOutOngoing(INPUT lbMobSub.CLI) THEN DO:
               MESSAGE "Termination will also trigger subscription " +
                       lbMobSub.CLI + " termination (multisim secondary subscription)"
               VIEW-AS ALERT-BOX.
            END.
         END.
         ELSE IF CAN-FIND(FIRST CLIType NO-LOCK WHERE
                                CLIType.Brand = Syst.Var:gcBrand AND
                                CLIType.CLIType = Mobsub.TariffBundle AND
                                CLIType.LineType = {&CLITYPE_LINETYPE_MAIN})
                                THEN DO:
            
            llAddLineTerm = FALSE.

            FOR EACH bbMobSub NO-LOCK WHERE
                     bbMobSub.Brand   = Syst.Var:gcBrand AND
                     bbMobSub.InvCust = Mobsub.CustNum AND
                     bbMobSub.PayType = FALSE AND
                     bbMobSub.MsSeq NE Mobsub.MsSeq,
               FIRST CLIType NO-LOCK WHERE
                     CLIType.Brand = Syst.Var:gcBrand ANd
                     CLIType.CLIType = (IF Mobsub.TariffBundle > ""
                                        THEN Mobsub.TariffBundle
                                        ELSE Mobsub.CLIType) AND
                     CLIType.LineType > 0:
               
               CASE CLIType.LineType:
                  WHEN {&CLITYPE_LINETYPE_MAIN} THEN DO:
                     llAddLineTerm = FALSE.
                     LEAVE.
                  END.
                  WHEN {&CLITYPE_LINETYPE_ADDITIONAL} THEN DO:
                     IF fHasPendingRequests
                        (bbMobSub.MsSeq,
                         bbMobSub.CLI,
                         CLIType.LineType) THEN NEXT.
                     llAddLineTerm = TRUE.
                  END.
               END.
            END.

            IF llAddLineTerm THEN
               MESSAGE "Termination will trigger STC to CONT9 for additional line(s)"
               VIEW-AS ALERT-BOX.
         END.
      
         liError = fDeleteMsValidation(Mobsub.MsSeq, 
                                       liOrderer, /* not yet known */
                                       OUTPUT lcError).
         IF liError NE 0 THEN DO:
            MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
            NEXT ACTION.
         END.
         ELSE IF lcError > "" THEN MESSAGE lcError VIEW-AS ALERT-BOX.

         llBillPer = FALSE.
         liError = fCheckBillingPermission(MobSub.Msseq, OUTPUT lcError).
         IF liError > 0 THEN DO:
            
            IF liError EQ 2 THEN DO:
               MESSAGE lcError VIEW-AS ALERT-BOX BUTTONS YES-NO TITLE
               " Billing Permission " UPDATE llBillPer.
               IF llBillPer AND getTMSRight("BILL") NE "RW" THEN DO:
                  MESSAGE ({&MSG_NOT_RIGHTS}) VIEW-AS ALERT-BOX ERROR.
                  llBillPer = FALSE.
               END.
            END.
            ELSE DO:
               MESSAGE lcError VIEW-AS ALERT-BOX.
               NEXT ACTION.
            END.

         END.
   
         MESSAGE
            "Do You REALLY want to de-activate subscription with MSISDN: "
            MobSub.CLI 
         VIEW-AS ALERT-BOX BUTTONS YES-NO TITLE " CONFIRMATION " UPDATE ok .
         
         IF NOT ok THEN NEXT Action.
               
         LEAVE Action.
   
      END.

   END. /* Action */      

   DO:
      ldeKillTS = YEAR (ldtKillDate) * 10000 +
                  MONTH(ldtKillDate) * 100   +
                  DAY  (ldtKillDate) +
                       (hh * 3600 + mm * 60) / 100000.

      IF fCheckKillTS(liOrderer,ldeKillTS, OUTPUT lcError) NE 0 THEN DO:
         MESSAGE lcError VIEW-AS ALERT-BOX.
         NEXT MAIN.
      END.

      /* create DELETE MsRequest */
      liMsReq = fTerminationRequest(Mobsub.MSSeq,
                                    ldeKillTS,
                                    liMsisdnStat,
                                    liSimStat,
                                    liQuarTime,
                                    INT(llPenalty),
                                    lcOutOper,
                                    STRING(liOrderer),
                                    "4",
                                    lcUserCode,
                                    0,
                                    lcTermType,
                                    OUTPUT ocResult).
                                    
      IF liMsReq = 0 THEN
         MESSAGE "Request creation failed:" CHR(10)
                 ocResult
         VIEW-AS ALERT-BOX ERROR.

      ELSE DO:
         
         IF llBillPer THEN
            fCreateLimitHistory(
               MobSub.InvCust,
               MobSub.MsSeq,
               {&LIMIT_TYPE_BILLPERM},
               0,
               0,
               0,
               FALSE,
               TODAY,
               12/31/2049).

         fAdditionalLineSTC(liMsReq,
                            Func.Common:mMake2DT(ldtKillDate + 1, 0),
                            "DELETE").
         MESSAGE
            "Request ID for subscription termination is:" liMsReq
         VIEW-AS ALERT-BOX TITLE " REQUEST ADDED ".
      END.
   END.

   LEAVE.

END. /* MAIN */

HIDE FRAME main NO-PAUSE.
HIDE MESSAGE.
