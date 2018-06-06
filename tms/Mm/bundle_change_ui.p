/*------------------------------------------------------
  MODULE .......: bundle_change_ui.p
  FUNCTION .....: create a request for bundle change 
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 09.11.10
  Version ......: Yoigo
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/cparam2.i}
{Mm/fbundle.i}
{Func/fbtc.i}
{Func/main_add_lines.i}

DEF INPUT PARAMETER iiMsSeq   AS INT NO-UNDO. 

DEF NEW SHARED VAR siirto AS CHAR. 

DEF VAR llOk            AS LOG  NO-UNDO.
DEF VAR liCreated       AS INT  NO-UNDO.
DEF VAR lcError         AS CHAR NO-UNDO. 
DEF VAR ldActStamp      AS DEC  NO-UNDO.
DEF VAR lcCustName      AS CHAR NO-UNDO.
DEF VAR ldCurrent       AS DEC  NO-UNDO.
DEF VAR ldaDate         AS DATE NO-UNDO.
DEF VAR liTime          AS INT  NO-UNDO.
DEF VAR llCreateFees    AS LOG  NO-UNDO.
DEF VAR lcDCEvent       AS CHAR NO-UNDO.
DEF VAR lcCurrentBundle AS CHAR NO-UNDO.
DEF VAR lcNewName       AS CHAR NO-UNDO.
DEF VAR ldaChangeDate   AS DATE NO-UNDO.
DEF VAR lcResult        AS CHAR NO-UNDO.
DEF VAR lcActiveBundles AS CHAR NO-UNDO.
DEF VAR llAddLineTerm   AS LOG NO-UNDO. 

DEF BUFFER bNewBundle FOR DayCampaign.
DEF BUFFER bbMobSub   FOR MobSub.

FORM
   SKIP(1)
   "Bundle will be changed, i.e. the current bundle will be" AT 5 
   "terminated and the new bundle will be activated." AT 5 
   SKIP(1)

   MobSub.CLI COLON 20
      LABEL "MSISDN" 
      SKIP
   MobSub.MsSeq COLON 20
      LABEL "Subscription ID"
      FORMAT ">>>>>>>>>>9"
      SKIP
   MobSub.CustNum COLON 20
      LABEL "Customer"
   lcCustName 
      NO-LABEL
      FORMAT "X(40)"
      SKIP(1)

   lcCurrentBundle COLON 20
      LABEL "Current Bundle"
      HELP "Periodical contract ID"
      FORMAT "X(12)"
   DayCampaign.DCName
      FORMAT "X(30)"
      NO-LABEL
      SKIP(1)
   
   lcDCEvent COLON 20 
      LABEL "New Bundle" 
      HELP "Periodical contract ID"
      FORMAT "X(12)"
   lcNewName    
      FORMAT "X(30)"
      NO-LABEL
      SKIP
   ldaChangeDate COLON 20
      LABEL "Change Date" 
      HELP "Date when new bundle will become effective"
      FORMAT "99-99-99"
      SKIP(1)
WITH ROW 3 OVERLAY SIDE-LABELS CENTERED 
     TITLE " BUNDLE CHANGE " FRAME fCriter.

/*********** Main start *********/

RUN pInitialize.
IF RETURN-VALUE BEGINS "ERROR" THEN DO:
   MESSAGE ENTRY(2,RETURN-VALUE,":")
   VIEW-AS ALERT-BOX ERROR.
   RETURN RETURN-VALUE.
END.

PAUSE 0.
VIEW FRAME fCriter. 

Syst.Var:toimi = -1.

FIND MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK.

MakeReq:
REPEAT WITH FRAME fCriter ON ENDKEY UNDO MakeReq, NEXT MakeReq:

   FIND FIRST DayCampaign WHERE 
              DayCampaign.Brand = Syst.Var:gcBrand AND
              DayCampaign.DCEvent = lcCurrentBundle NO-LOCK NO-ERROR.

   PAUSE 0.
   DISPLAY MobSub.CLI
           MobSub.MsSeq
           MobSub.CustNum
           lcCustName
           lcCurrentBundle
           DayCampaign.DCName WHEN AVAILABLE DayCampaign
           lcDCEvent
           lcNewName
           ldaChangeDate
   WITH FRAME fCriter.

   IF Syst.Var:toimi < 0 THEN Syst.Var:toimi = 1.
   ELSE DO:
      ASSIGN
         Syst.Var:ufk    = 0  
         Syst.Var:ufk[1] = 7
         Syst.Var:ufk[5] = 1027 
         Syst.Var:ufk[8] = 8 
         Syst.Var:ehto   = 0.
      RUN Syst/ufkey.p.
   END.
   
   IF Syst.Var:toimi = 1 THEN DO:
   
      REPEAT WITH FRAME fCriter ON ENDKEY UNDO, LEAVE:
      
         Syst.Var:ehto = 9.
         RUN Syst/ufkey.p.
         
         UPDATE 
            lcCurrentBundle    
            lcDCEvent    
            ldaChangeDate
         WITH FRAME fCriter EDITING:
         
            READKEY.
            
            IF KEYLABEL(LASTKEY) = "F9" AND FRAME-FIELD = "lcDCEvent"
            THEN DO:
               Syst.Var:gcHelpParam = "DCType:1,4".
               RUN Help/h-daycamp.p.
               IF siirto NE ? THEN 
               DO: 
                   ASSIGN lcDCEvent = siirto.
                   DISP lcDCEvent.
               END.    
               Syst.Var:ehto = 9.
               RUN Syst/ufkey.p.
               NEXT.
            END.
                
            ELSE IF LOOKUP(KEYLABEL(LASTKEY),Syst.Var:poisnap) > 0 THEN DO:
               PAUSE 0.

               IF FRAME-FIELD = "lcCurrentBundle" THEN DO:
                  FIND FIRST DayCampaign WHERE 
                             DayCampaign.Brand = Syst.Var:gcBrand AND
                             DayCampaign.DCEvent = INPUT lcCurrentBundle 
                  NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE DayCampaign THEN DO:
                     MESSAGE "Unknown bundle"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
                  
                  IF INPUT lcCurrentBundle = "DUB" THEN DO:
                     MESSAGE "DUB is not considered as exchangable bundle"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.

                  IF LOOKUP(INPUT lcCurrentBundle,lcActiveBundles) = 0 THEN DO:
                     MESSAGE "This bundle is not currently active"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.
         
               ELSE IF FRAME-FIELD = "lcDCEvent" THEN DO:
                  FIND FIRST bNewBundle WHERE 
                             bNewBundle.Brand = Syst.Var:gcBrand AND
                             bNewBundle.DCEvent = INPUT lcDCEvent 
                  NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE bNewBundle THEN DO:
                     MESSAGE "Unknown bundle"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.
               
               ELSE IF FRAME-FIELD = "ldaChangeDate" THEN DO:
                  IF DAY(INPUT ldaChangeDate) NE 1 AND
                     (bNewBundle.DCEvent BEGINS "MDUB" OR
                      bNewBundle.DCEvent BEGINS "DATA") THEN DO:
                     MESSAGE "Change can only be done on 1. day of the month"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
                  ELSE IF INPUT ldaChangeDate <= TODAY THEN DO:
                     MESSAGE "Invalid change date"
                     VIEW-AS ALERT-BOX INFORMATION.
                     NEXT.
                  END.
               END.

            END.

            APPLY LASTKEY.
         END.
      
         LEAVE.
      END.
      
   END.

   ELSE IF Syst.Var:toimi = 5 THEN DO:

      IF NOT fValidateBTC(MobSub.MsSeq,
                          lcCurrentBundle,
                          lcDCEvent,
                          ldaChangeDate,
                          MobSub.CliType,
                          FALSE, /* extend contract */
                          OUTPUT lcResult) THEN DO:
          MESSAGE lcResult
          VIEW-AS ALERT-BOX ERROR.
          NEXT.
      END.

      IF Mobsub.MultiSimType EQ {&MULTISIMTYPE_PRIMARY} AND
         Mobsub.MultiSimID > 0 THEN DO:
      
         FIND FIRST bbMobSub NO-LOCK USE-INDEX MultiSIM WHERE
                    bbMobSub.Brand = Syst.Var:gcBrand AND
                    bbMobSub.MultiSimId = Mobsub.MultiSimId AND
                    bbMobSub.MultiSimType = {&MULTISIMTYPE_SECONDARY} AND
                    bbMobSub.Custnum = Mobsub.Custnum NO-ERROR.
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
            MESSAGE "BTC will also trigger subscription " +
                    bbMobSub.CLI + " termination (multisim secondary subscription)"
            VIEW-AS ALERT-BOX.
        END.
      END.
      ELSE IF CAN-FIND(FIRST CLIType NO-LOCK WHERE
                             CLIType.Brand = Syst.Var:gcBrand AND
                             CLIType.CLIType = lcCurrentBundle AND
                             CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}) AND
          NOT CAN-FIND(FIRST CLIType NO-LOCK WHERE
                             CLIType.Brand = Syst.Var:gcBrand AND
                             CLIType.CLIType = lcDCEvent AND
                             CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}) THEN DO:

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
                                     ELSE Mobsub.CLIType):
      
            IF CLIType.LineType EQ {&CLITYPE_LINETYPE_MAIN} THEN DO:
               llAddLineTerm = FALSE.
               LEAVE.
            END.
                  
            IF CLIType.LineType EQ {&CLITYPE_LINETYPE_ADDITIONAL} THEN DO:
               IF fHasPendingRequests
                 (bbMobSub.MsSeq,
                  bbMobSub.CLI,
                  CLIType.LineType) THEN NEXT.
               llAddLineTerm = TRUE.
            END.

         END.
            
         IF llAddLineTerm THEN
            MESSAGE "BTC will trigger subscription termination for additional line(s)"
               VIEW-AS ALERT-BOX.
      END.
      
      llOk = FALSE.
      MESSAGE "Bundle will be changed from " + lcCurrentBundle +
              " to " + lcDCEvent SKIP
              "Continue with request creation ?" 
      VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO 
      SET llOk.
      
      IF NOT llOk THEN NEXT.
 
      ldActStamp = Func.Common:mMake2DT(ldaChangeDate,
                            IF ldaChangeDate = TODAY
                            THEN TIME
                            ELSE 0).
                            
      liCreated = fBundleChangeRequest(MobSub.MsSeq,
                                       lcCurrentBundle, 
                                       lcDCEvent,
                                       ldActStamp,
                                       {&REQUEST_SOURCE_MANUAL_TMS},
                                       "",
                                       llCreateFees,
                                       0,
                                       FALSE,
                                       FALSE,
                                       0, /* extend contract 0=no extend_term_contract */
                                       "",
                                       OUTPUT lcError).

      IF liCreated > 0 THEN 
         MESSAGE "Change request was created with ID" liCreated
         VIEW-AS ALERT-BOX 
         TITLE " DONE ".
      ELSE 
         MESSAGE "Change request could not be created;" SKIP
                 lcError
         VIEW-AS ALERT-BOX ERROR.

      LEAVE.
   END.
   
   ELSE IF Syst.Var:toimi = 8 THEN LEAVE.

END. /* MakeReq */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCriter NO-PAUSE.    


PROCEDURE pInitialize:

   FIND MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MobSub THEN 
      RETURN "ERROR:Unknown subscription".

   FIND Customer WHERE Customer.CustNum = MobSub.CustNum NO-LOCK.
 
   ASSIGN 
      ldCurrent     = Func.Common:mMakeTS()
      llCreateFees  = FALSE
      lcCustName    = Func.Common:mDispCustName(BUFFER Customer)
      ldaChangeDate = IF MONTH(TODAY) = 12 
                      THEN DATE(1,1,YEAR(TODAY) + 1)
                      ELSE DATE(MONTH(TODAY) + 1,1,YEAR(TODAY)).
    
   /* current active bundles */
   lcActiveBundles = fGetCurrentBundle(MobSub.MsSeq).
   
   IF lcActiveBundles = "" THEN
      RETURN "ERROR:No active bundles for subscription".

END PROCEDURE.  /* pInitialize */

