{Syst/commpaa.i}
Syst.Var:katun = "Qvantel".
Syst.Var:gcBrand = "1".
{Func/cparam2.i}
{Syst/tmsconst.i}

DEFINE VARIABLE lcOutputFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE liCount            AS INTEGER   NO-UNDO. 
DEFINE VARIABLE i                  AS INTEGER   NO-UNDO. 
DEFINE VARIABLE lcKey              AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liCustnum          AS INTEGER   NO-UNDO. 
DEFINE VARIABLE ldeActStamp        AS DECIMAL   NO-UNDO. 
DEFINE VARIABLE liPeriod           AS INTEGER   NO-UNDO. 
DEFINE VARIABLE ldaFromDate        AS DATE      NO-UNDO.
DEFINE VARIABLE ldaToDate          AS DATE      NO-UNDO.
DEFINE VARIABLE ldeFromStamp       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE ldaContractDate    AS DATE      NO-UNDO. 
DEFINE VARIABLE liContractTime     AS INTEGER   NO-UNDO. 
DEFINE VARIABLE liContractPeriod   AS INTEGER   NO-UNDO. 
DEFINE VARIABLE lcCli              AS CHARACTER NO-UNDO format "x(10)". 
DEFINE VARIABLE lcDataContracts    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcFFItemKey        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcEventlogDetails  AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldeTerminated      AS DECIMAL   NO-UNDO. 
DEFINE VARIABLE lcBundleOutputFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcSubBundle        AS CHARACTER NO-UNDO. 

DEFINE TEMP-TABLE ttServicelimit NO-UNDO
   FIELD groupcode AS char
   INDEX groupcode IS PRIMARY UNIQUE groupcode. 

DEFINE TEMP-TABLE ttSubDetails NO-UNDO
   FIELD MsSeq  AS INT
   FIELD CLI    AS CHAR
   FIELD Bundle AS CHAR
   INDEX MsSeq MsSeq. 

DEFINE BUFFER beventlog FOR eventlog.

DEF STREAM sout.
DEF STREAM strout.

IF DAY(TODAY) = 1 THEN
   ldaToDate = Func.Common:mLastDayOfMonth(TODAY - 1).
ELSE
   ldaToDate = Func.Common:mLastDayOfMonth(TODAY).

ldaFromDate = DATE(MONTH(ldaToDate),1,YEAR(ldaToDate)).

ASSIGN ldeFromStamp = Func.Common:mMake2DT(ldaFromDate,0)
       liPeriod     = YEAR(ldaFromDate) * 100 + MONTH(ldaFromDate).

ASSIGN 
   lcOutputFile = "/apps/yoigo/tms_support/billing/monthly_fee_check_" + STRING(liPeriod) + ".txt"
   lcBundleOutputFile = "/apps/yoigo/tms_support/billing/monthly_fee_check_bundle_" + STRING(liPeriod) + ".txt".

UPDATE 
   lcOutputFile FORMAT "x(60)" LABEL "Output" skip(1)
with width 80  overlay side-labels 1 column row 1 title " Missing monthly fee check "
frame fcontrolrep.

if lcOutputFile eq "" or lcOutputFile eq ? then quit.

disp "running.." with frame fcontrolrep.

OUTPUT STREAM sout   TO value(lcOutputFile).
OUTPUT STREAM strout TO value(lcBundleOutputFile).

put stream sout unformatted 
      "CUSTNUM|MSSEQ|MSISDN|CONTRACT|CONTRACT_FROM|CONTRACT_TO|MSREQUEST_CREATE_FEES|DELETE_FIXEDFEE_EVENTLOG|DELETE_FFITEM_EVENTLOG|EVENTLOG_DETAIL|TERMINATION_TIME" skip.

put STREAM strout unformatted 
    "CUSTNUM|MsSEQ|MSISDN|TariffBundle/BaseBundle" SKIP.

looppi:
FOR EACH daycampaign where
         daycampaign.brand = Syst.Var:gcBrand AND
         daycampaign.dcevent begins "PAYTERM" NO-LOCK,
    EACH dccli where
         dccli.brand    = Syst.Var:gcBrand             and
         dccli.dcevent  = daycampaign.dcevent and
         dccli.validto >= ldaFromDate NO-LOCK:
   
   i = i + 1.

   if i mod 1000 = 0 then do:
      disp i                    label "checked" 
           liCount              label "found" 
           dccli.dcevent column-label "contract".
      pause 0.
   end.

   ASSIGN
      lcEventlogDetails = ""
      lcFFItemKey       = ""
      ldeTerminated     = 0.

   release beventlog.
   
   FIND FIRST mobsub where
              mobsub.msseq = dccli.msseq NO-LOCK no-error.

   IF NOT AVAIL mobsub then do:
      FIND FIRST termmobsub where
                 termmobsub.msseq = dccli.msseq NO-LOCK no-error.
      liCustnum = termmobsub.custnum.
      
      FIND FIRST msowner NO-LOCK WHERE
                 msowner.msseq = termmobsub.msseq USE-INDEX msseq.
      ldeTerminated = msowner.tsend.
   END.     
   ELSE liCustnum = mobsub.custnum.
         
   FIND FIRST fixedfee where
              fixedfee.brand     = Syst.Var:gcBrand             and
              fixedfee.custnum   = liCustnum           and
              fixedfee.hosttable = "mobsub"            and
              fixedfee.keyvalue  = string(dccli.msseq) and
              fixedfee.calcobj   = dccli.dcevent       and
              fixedfee.begdate  >= dccli.validfrom
   NO-LOCK no-error.
   
   IF AVAIL fixedfee then do:
      FIND FIRST ffitem where
                 ffitem.FFNum      = fixedfee.ffnum and
                 ffitem.billperiod = liPeriod NO-LOCK no-error.
      IF AVAIL ffitem then do:
         if (ffitem.billed eq true  and ffitem.invnum = 0) or
            (ffitem.billed eq false and ffitem.invnum > 0) then do:
            put stream sout unformatted 
                liCustnum       "|" 
                dccli.msseq     "|"
                dccli.dcevent   "|"
                dccli.validfrom "|"
                dccli.validto   "|"
                "ERROR:FFItem billed and invoice not found (or vice versa)"
            skip.
         end.
         else if ffitem.invnum > 0 and 
              not can-find(first invoice where
                                 invoice.invnum = ffitem.invnum) then do:
            put stream sout unformatted 
               liCustnum       "|" 
               dccli.msseq     "|"
               dccli.dcevent   "|"
               dccli.validfrom "|"
               dccli.validto   "|"
               "ERROR:FFItem invoice not found" skip.
         end.

         next.
      end.
   
      if dccli.amount > 0 then do:
         FIND FIRST SingleFee NO-LOCK WHERE
                    SingleFee.Brand       = Syst.Var:gcBrand                     AND
                    SingleFee.Custnum     = liCustnum                   AND
                    SingleFee.HostTable   = "mobsub"                    AND
                    SingleFee.KeyValue    = string(dccli.msseq)         AND
                    SingleFee.SourceKey   = STRING(dccli.PerContractID) AND
                    SingleFee.SourceTable = "DCCLI"                     AND
                    SingleFee.CalcObj     = "RVTERM" NO-ERROR.
         IF NOT AVAIL SingleFee then do:
            put stream sout unformatted 
                liCustnum       "|" 
                dccli.msseq     "|"
                dccli.dcevent   "|"
                dccli.validfrom "|"
                dccli.validto   "|"
                "ERROR:Residual singlefee not found"
             skip.
         end.
      end.

   END.

   ASSIGN lcKey = "1"      + CHR(255) + string(liCustnum) + CHR(255) +
                  "MobSub" + CHR(255) + string(dccli.msseq)
          ldeActStamp = Func.Common:mMake2DT(dccli.validfrom, 0).

   IF AVAIL fixedfee THEN
      lcFFItemKey = string(fixedfee.FFNum) + CHR(255) + string(liPeriod).

   FIND FIRST msrequest where
              msrequest.msseq      = dccli.msseq   and
              msrequest.reqtype    = 8             and
              msrequest.reqstatus  = 2             and
              msrequest.reqcparam3 = dccli.dcevent and
              msrequest.actstamp  >= ldeActStamp NO-LOCK.

   FIND FIRST eventlog where
              eventlog.tablename = "FixedFee" and
              eventlog.key begins lcKey       and
              eventlog.action    = "delete"   NO-LOCK no-error.
   
   IF AVAIL eventlog THEN
      lcEventlogDetails = eventlog.usercode + "-" + STRING(eventlog.eventdate).
   ELSE IF lcFFItemKey > "" THEN DO:
      FIND FIRST beventlog where
                 beventlog.tablename = "FFItem"   and
                 beventlog.key begins lcFFItemKey and
                 beventlog.action    = "delete"   NO-LOCK no-error.
      IF NOT AVAIL beventlog AND
             AVAIL fixedfee  AND 
             fixedfee.endperiod < liPeriod THEN NEXT.
      ELSE IF AVAIL beventlog THEN
         lcEventlogDetails = beventlog.usercode + "-" + STRING(beventlog.eventdate).
   END.
   
   liCount = liCount + 1.

   put stream sout unformatted 
      liCustnum                                 "|"
      dccli.msseq                               "|" 
      dccli.cli                                 "|" 
      dccli.dcevent                             "|" 
      dccli.validfrom                           "|"
      dccli.validto                             "|"
      (if avail msrequest then
       string(msrequest.createfees) else "N/A") "|"
      avail(eventlog)                           "|"
      avail(beventlog)                          "|"
      lcEventlogDetails                         "|"
      (IF ldeTerminated > 0 THEN Func.Common:mTS2HMS(ldeTerminated) ELSE "") skip.

end.

looppi2:
FOR EACH servicelimit NO-LOCK,
    FIRST daycampaign NO-LOCK WHERE
          daycampaign.brand   = "1" and
          daycampaign.dcevent = servicelimit.groupcode:

   if daycampaign.feemodel eq "" then next.
   if daycampaign.dcevent begins "dss" then next.

   FIND FIRST fmitem NO-LOCK where
              fmitem.brand = "1" and
              fmitem.feemodel = daycampaign.feemodel and
              fmitem.todate > today no-error.

   if fmitem.billmethod then next.
   if fmitem.billtype eq "NF" then next. /* no fee (prepaid) */

   FIND FIRST ttServicelimit NO-LOCK where
              ttServicelimit.groupcode  = servicelimit.groupcode no-error.

   IF AVAIL ttServicelimit then next.

   create ttServicelimit.
   assign
      ttServicelimit.groupcode = servicelimit.groupcode.

   FOR EACH mservicelimit where
            mservicelimit.slseq    = servicelimit.slseq    and
            mservicelimit.dialtype = servicelimit.dialtype and
            mservicelimit.endts   >= ldeFromStamp NO-LOCK:

      if mservicelimit.endts < mservicelimit.fromts then next.
      
      if servicelimit.groupcode = "contdata" and
         mservicelimit.fromts = 20090301.00000 then next.

      i = i + 1.
      if i mod 1000 = 0 then do:
         disp i label "checked" liCount label "found" servicelimit.groupcode column-label "contract".
         pause 0.
      end.

      ASSIGN
         lcFFItemKey       = ""
         lcEventlogDetails = ""
         ldeTerminated     = 0
         lcSubBundle       = "".
   
      release beventlog.
       
      Func.Common:mSplitTS(mservicelimit.fromts, 
               output ldaContractDate, 
               output liContractTime).

      liContractPeriod = year(ldaContractDate) * 100 + month(ldaContractDate).
      
      FIND FIRST mobsub where
                 mobsub.msseq = mservicelimit.msseq NO-LOCK no-error.

      IF NOT AVAIL mobsub then do:
         FIND FIRST termmobsub where
                    termmobsub.msseq = mservicelimit.msseq NO-LOCK no-error.
         
         FIND FIRST CliType WHERE 
                    CliType.CLitype = termmobsub.CLIType NO-LOCK no-error.
         /* for staging */
         IF NOT AVAIL CLIType THEN NEXT.
         
         assign liCustnum = termmobsub.custnum
                lcCli = termmobsub.cli.

         FIND FIRST msowner NO-LOCK WHERE
                    msowner.msseq = termmobsub.msseq USE-INDEX msseq.
         ldeTerminated = msowner.tsend.

         IF termmobsub.TariffBundle <> "" THEN 
            lcSubBundle = termmobsub.TariffBundle.
         ELSE IF CLIType.BaseBundle <> "" THEN 
            lcSubBundle = CLIType.BaseBundle.

         IF lcSubBundle EQ servicelimit.groupcode THEN DO: 
            CREATE ttSubDetails.
            assign ttSubDetails.msseq  = termmobsub.MsSeq 
                   ttSubDetails.cli    = termmobsub.CLI
                   ttSubDetails.Bundle = lcSubBundle.
         END.                               
      END.     
      ELSE DO: 
         ASSIGN liCustnum = mobsub.custnum
                lcCli     = mobsub.cli.
      
         FIND FIRST CliType WHERE 
                    CliType.CLitype = MobSub.CLIType NO-LOCK no-error.
         
         IF MobSub.TariffBundle <> "" THEN 
            lcSubBundle = MobSub.TariffBundle.
         ELSE IF CLIType.BaseBundle <> "" THEN 
            lcSubBundle = CLIType.BaseBundle.

         IF lcSubBundle EQ servicelimit.groupcode THEN DO: 
            CREATE ttSubDetails.
            assign ttSubDetails.msseq  = MobSub.MsSeq 
                   ttSubDetails.cli    = MobSub.CLI
                   ttSubDetails.Bundle = lcSubBundle.
         END.          
      END.

      FIND FIRST fixedfee NO-LOCK WHERE
                 fixedfee.brand      = Syst.Var:gcBrand                     and
                 fixedfee.hosttable  = "mobsub"                    and
                 fixedfee.custnum    = liCustnum                   and
                 fixedfee.keyvalue   = string(mservicelimit.msseq) and
                 fixedfee.CalcObj    = servicelimit.groupcode      and
                 fixedfee.endperiod >= liContractPeriod            NO-ERROR.

      IF AVAIL fixedfee THEN DO:
         FIND FIRST ffitem WHERE
                    ffitem.FFNum      = fixedfee.ffnum   AND
                    ffitem.billperiod = liPeriod NO-LOCK NO-ERROR.
      
         IF AVAIL ffitem THEN DO:
            IF (ffitem.billed          AND ffitem.invnum = 0) OR
               (ffitem.billed eq false and ffitem.invnum > 0) THEN DO:
               put stream sout unformatted 
                  liCustnum "|" mservicelimit.msseq "|"
                   "ERROR:FFItem billed and invoice not found (or vice versa)"
               skip.
            END.
            ELSE IF ffitem.invnum > 0 AND
                 NOT CAN-FIND(FIRST invoice WHERE
                                    invoice.invnum = ffitem.invnum) THEN DO:
               put stream sout unformatted 
                  liCustnum "|" mservicelimit.msseq "|"
                   "ERROR:FFItem invoice not found" skip.
            END.

            NEXT.
         END.
      END.
      
      IF AVAIL fixedfee THEN
         lcFFItemKey = string(fixedfee.FFNum) + CHR(255) + string(liPeriod).

      FIND FIRST msrequest NO-LOCK WHERE
                 msrequest.msseq      = mservicelimit.msseq    AND
                 msrequest.reqtype    = 8                      AND 
                 msrequest.reqstatus  = 2                      AND
                 msrequest.reqcparam3 = servicelimit.groupcode AND
                 msrequest.actstamp   = mservicelimit.fromts   NO-ERROR.
   
      lcKey = "1"      + CHR(255) + string(liCustnum) + CHR(255) + 
              "MobSub" + CHR(255) + string(mservicelimit.msseq). 
      
      FIND FIRST eventlog NO-LOCK WHERE
                 eventlog.tablename = "FixedFee" AND 
                 eventlog.key       begins lcKey AND 
                 eventlog.action    = "delete"   NO-ERROR.

      IF AVAIL eventlog THEN
         lcEventlogDetails = eventlog.usercode + "-" + STRING(eventlog.eventdate).
      ELSE IF lcFFItemKey > "" THEN DO:
         FIND FIRST beventlog NO-LOCK WHERE
                    beventlog.tablename = "FFItem"         AND 
                    beventlog.key       begins lcFFItemKey AND 
                    beventlog.action    = "delete"         NO-ERROR.
         IF AVAIL beventlog THEN
            lcEventlogDetails = beventlog.usercode + "-" + STRING(beventlog.eventdate).
      END.
      
      PUT STREAM sout UNFORMATTED
         liCustnum                        "|"
         mservicelimit.msseq              "|" 
         lcCli                            "|"
         servicelimit.groupcode           "|"
         mservicelimit.fromts             "|"
         mservicelimit.endts              "|"
        (if avail msrequest then
             string(msrequest.createfees) 
         else "N/A")                      "|"
         avail(eventlog)                  "|"
         avail(beventlog)                 "|"
         lcEventlogDetails                "|"
        (IF ldeTerminated > 0 THEN 
            Func.Common:mTS2HMS(ldeTerminated) 
         ELSE "")                         SKIP.
   
      liCount = liCount + 1.
   END.
END.

DEF VAR lcBundleBasedCLITypes AS CHAR NO-UNDO.  
DEF VAR lcBundle              AS CHAR NO-UNDO. 
DEF VAR liBundleCount         AS INT  NO-UNDO. 

ASSIGN liBundleCount = 0
       lcBundleBasedCLITypes = fCParam("Bundles","BUNDLE_BASED_CLITYPES").

FOR EACH MobSub NO-LOCK WHERE 
         MobSub.Brand = Syst.Var:gcBrand:             
    RUN pCheckSubscription((BUFFER Mobsub:HANDLE)).
END.

FOR EACH MsRequest NO-LOCK WHERE
         MsRequest.Brand     = Syst.Var:gcBrand AND 
         MsRequest.ReqType   = 18      AND 
         MsRequest.ReqStatus = 2       AND 
         MsRequest.ActStamp >= ldeFromStamp:

   FIND FIRST TermMobSub NO-LOCK WHERE 
              TermMobSub.MsSeq = MsRequest.MsSeq NO-ERROR.
   
   IF NOT AVAIL TermMobSub THEN NEXT.

    RUN pCheckSubscription((BUFFER TermMobSub:HANDLE)).
END.

PROCEDURE pCheckSubscription:

   DEF INPUT PARAM ihSub AS HANDLE.
   
   FIND FIRST CLIType NO-LOCK WHERE 
              CLIType.CLIType = ihSub::CLIType NO-ERROR.
     
   IF NOT AVAIL CLIType    THEN NEXT.
   
   IF CLIType.FixedLineDownLoad > "" THEN DO:

      FIND RequestAction NO-LOCK WHERE
           RequestAction.Brand = Syst.Var:gcBrand AND
           RequestAction.CLIType = ihSub::CLIType AND
           RequestAction.ReqType = 14 AND
           RequestAction.ValidTo >= TODAY AND
           RequestAction.ActionType = "DayCampaign" AND
           RequestAction.Action = 1 NO-ERROR.

      IF AVAIL RequestAction THEN
        RUN pBundleCheck(ihSub::MsSeq,
                         ihSub::CustNum,
                         ihSub::CLI,
                         RequestAction.ActionKey).

      IF ihSub::MSStatus EQ {&MSSTATUS_MOBILE_PROV_ONG} THEN NEXT.

      /* TODO: missing partial convergent termination support */
      IF ihSub::MSStatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE} THEN NEXT.
   END.

   IF ihSub::TariffBundle EQ "" AND
      CLIType.BaseBundle  EQ "" THEN NEXT.
  
   IF LOOKUP(CLIType.CLIType,lcBundleBasedCLITypes) > 0 AND 
      ihSub::TariffBundle EQ ""                         THEN  
   DO:
      PUT STREAM strout unformatted 
         ihSub::MsSeq                  "|"
         ihSub::CustNum                "|"
         ihSub::CLI                    "|"
         "Tariff Bundle not available" SKIP.

      liBundleCount = liBundleCount + 1.
      NEXT.
   END.

   IF ihSub::TariffBundle <> "" THEN 
      lcBundle = ihSub::TariffBundle. 
   ELSE IF CLIType.BaseBundle <> "" THEN
      lcBundle = CLIType.BaseBundle.
   ELSE lcBundle = "".
     
   RUN pBundleCheck(ihSub::MsSeq,
                    ihSub::CustNum,
                    ihSub::CLI,
                    lcBundle).

END PROCEDURE. 

PROCEDURE pBundleCheck:

   define INPUT parameter iiMsSeq   as integer   no-undo.
   define INPUT parameter iiCustNum AS integer   no-undo.
   define INPUT parameter icCLI     as character no-undo.
   define INPUT parameter icBundle  as character no-undo.

   DEF VAR llgAvailable AS LOGICAL NO-UNDO. 

  IF NOT CAN-FIND(FIRST ttSubDetails WHERE 
                        ttSubDetails.MsSeq  EQ iiMsSeq   AND 
                        ttSubDetails.Bundle EQ icBundle) THEN DO:
     
     llgAvailable = FALSE.

     /* Double check in mservicelimit */
     FOR EACH mservicelimit NO-LOCK WHERE 
              mservicelimit.MsSeq  = iiMsSeq AND
              mservicelimit.endts >= ldeFromStamp:
        
        FIND FIRST servicelimit NO-LOCK WHERE 
                   servicelimit.slseq     = mservicelimit.slseq    AND 
                   servicelimit.dialtype  = mservicelimit.dialtype AND 
                   servicelimit.groupcode = icBundle               NO-ERROR.
        IF AVAIL servicelimit THEN DO:
           llgAvailable = TRUE.
           LEAVE.
        END.
     END.         
     
     IF NOT llgAvailable THEN DO:
        PUT STREAM strout unformatted 
           iiCustNum "|"
           iiMsSeq   "|"
           icCLI     "|"
           lcBundle  SKIP.

        liBundleCount = liBundleCount + 1.
     END.
  END.                      

END PROCEDURE.

output stream sout close.
OUTPUT STREAM strout CLOSE.

MESSAGE "Done," liCount "missing contracts found" VIEW-AS ALERT-BOX.

MESSAGE "Bundle tariff Count : " liBundleCount VIEW-AS ALERT-BOX.
