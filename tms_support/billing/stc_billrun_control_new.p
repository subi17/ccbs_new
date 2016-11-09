/* ----------------------------------------------------------------------
  MODULE .......: stc_billrun_control_new.p
  TASK .........: Validates and reports Terminal contract terminations after BTC/STC 
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 24.08.11
  Version ......: Yoigo
----------------------------------------------------------------------- */

{commpaa.i}
katun  = "Qvantel".
gcBrand = "1".
{date.i}
{tmsconst.i}
{timestamp.i}
{cparam2.i}
{contract_end_date.i}
{fixedfee.i}
{email.i}
{Func/fixedlinefunc.i}

DEF VAR ldBeginStamp         AS DEC  NO-UNDO.
DEF VAR ldEndStamp           AS DEC  NO-UNDO.
DEF VAR ldtActDate           AS DATE NO-UNDO.
DEF VAR liActTime            AS INT  NO-UNDO.
DEf VAR lcOrigCLIType        AS CHAR NO-UNDO. 
DEF VAR lcOrigBundle         AS CHAR NO-UNDO. 
DEF VAR liTermReq            AS INT  NO-UNDO. 
DEF VAR liFeeCreated         AS INT  NO-UNDO. 
DEF VAR llRenewal            AS LOG  NO-UNDO. 
DEF VAR ldaCalculatedEndDate AS DATE NO-UNDO.
DEF VAR lcNewType            AS CHAR NO-UNDO. 
DEF VAR llError              AS LOG  NO-UNDO. 
DEF VAR lcIPLContracts       AS CHAR NO-UNDO.
DEF VAR lcCONTDContracts     AS CHAR NO-UNDO.
DEF VAR lcFLATContracts      AS CHAR NO-UNDO.
DEF VAR lcCONTSContracts     AS CHAR NO-UNDO.
DEF VAR lcCONTSFContracts    AS CHAR NO-UNDO.
DEF VAR lcBONOContracts      AS CHAR NO-UNDO. 
DEF VAR lcConfDir            AS CHAR NO-UNDO. 
DEF VAR lcLogFolder          AS CHAR NO-UNDO. 
DEF VAR lcSumFile            AS CHAR NO-UNDO.
DEF VAR lcDetFile            AS CHAR NO-UNDO.
DEF VAR lcErrors             AS CHAR NO-UNDO. 
DEF VAR lcFileDate           AS CHAR NO-UNDO.
DEF VAR ldtInputDate         AS DATE NO-UNDO. 
DEF VAR lcMail               AS CHAR NO-UNDO. 
DEF VAR llErrorStatus        AS LOG  NO-UNDO. 
DEF VAR lcStatusMsg          AS CHAR NO-UNDO. 
DEF VAR lcRequestTypes       AS CHAR NO-UNDO. 
DEF VAR i                    AS INT  NO-UNDO. 
DEF VAR liRequestType        AS INT  NO-UNDO. 
DEF VAR liHandled            AS INT  NO-UNDO. 
DEF VAR ldaNoFeesAfter       AS DATE NO-UNDO. 

def buffer bOrigCLIType for CLIType.
def buffer bNewCLiType  for clitype.
def buffer bResidualFee for singlefee.
def buffer bMsrequest   for msrequest.

DEF STREAM sLogMSISDN.
DEF STREAM sLogStat.
DEF STREAM slog.
DEF STREAM sErrors.

DEF TEMP-TABLE ttStatistics NO-UNDO
   FIELD DCEvent      LIKE DayCampaign.DCEvent
   FIELD OrigCLIType  LIKE Mobcdr.CLIType
   FIELD NewCLIType   LIKE Mobcdr.CLIType
   FIELD DCEventQty   AS INT
   FIELD FeeQty       AS INT   
   FIELD Renewal      AS LOGICAL
   FIELD Extension    AS INT
   FIELD ResidualFee  AS LOG
INDEX DCEvent DCEvent OrigCLIType NewCLIType Renewal.

ASSIGN lcIPLContracts    = fCParamC("IPL_CONTRACTS")
       lcCONTDContracts  = fCParamC("CONTD_CONTRACTS")
       lcFLATContracts   = fCParamC("FLAT_CONTRACTS")
       lcCONTSContracts  = fCParamC("CONTS_CONTRACTS")
       lcCONTSFContracts = fCParamC("CONTSF_CONTRACTS")
       lcBONOContracts   = fCParamC("BONO_CONTRACTS")
       lcLogFolder       = fCParamC("StcContRepFolder").

ASSIGN
       lcSumFile    = lcLogFolder + "/STC_ControlReport_Summary_" + "#USER_" + "#DATE" + ".txt"
       lcDetFile    = lcLogFolder + "/STC_ControlReport_Details_" + "#USER_" + "#DATE" + ".txt"
       lcErrors     = lcLogFolder + "/STC_ControlReport_Errors_"  + "#USER_" + "#DATE" + ".txt"
       lcMail       = lcLogFolder + "/Mail_"                      + "#USER_" + "#DATE" + ".txt"
       ldtInputDate = TODAY.

FUNCTION fGetOrigCLIType RETURNS CHARACTER
         (INPUT pcDCEvent     AS CHAR,
          INPUT pdtValidFrom  AS DATE,
          INPUT pdtExtentDate AS DATE,
          INPUT piMsSeq       AS INT,
          OUTPUT olRenewal    AS LOG):

   DEF VAR ldTS        AS DEC  NO-UNDO.
   DEF VAR lcCLIType   AS CHAR NO-UNDO.
   DEF VAR lcReqParam2 AS CHAR NO-UNDO. 

   DEF BUFFER bufMsRequest FOR MsRequest .

   IF pdtExtentDate <> ? THEN
      ASSIGN
         ldTS        = fHMS2TS(pdtExtentDate,"00:00:00")
         lcReqParam2 = "update".
   ELSE
      ASSIGN
         ldTS        = fHMS2TS(pdtValidFrom,"00:00:00")
         lcReqParam2 = "act,recreate".
 
   FIND FIRST bufMsRequest WHERE
              bufMsRequest.MsSeq      = piMsseq                        AND
              bufMsRequest.ReqType    = {&REQTYPE_CONTRACT_ACTIVATION} AND
              bufMsRequest.ReqStatus  = {&REQUEST_STATUS_DONE}         AND
              bufMsRequest.DoneStamp >= ldTS                           AND
              bufMsRequest.ReqCParam3 = pcDCEvent                      AND
       LOOKUP(bufMsRequest.ReqCparam2,lcReqParam2) > 0
   NO-LOCK USE-INDEX MsSeq NO-ERROR.

   IF AVAIL bufMsRequest THEN 
      ldTS = bufMsRequest.DoneStamp.

   FIND FIRST MsOwner WHERE
              MsOwner.MsSeq = piMsSeq AND 
              MsOwner.TSBegin <= ldTS NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE MsOwner THEN 
      FIND LAST MsOwner WHERE
                MsOwner.MsSeq = piMsSeq AND 
                MsOwner.TSBegin > ldTS NO-LOCK NO-ERROR. 
   
   IF AVAIL MsOwner THEN DO:
      lcCLIType = MsOwner.CLIType.
 
      IF MsOwner.TariffBundle <> "" THEN 
         lcCLIType = MsOwner.TariffBundle.
   END.

   IF bufMsRequest.ReqSource = {&REQUEST_SOURCE_RENEWAL} THEN 
      olRenewal = YES.   

   RETURN lcCLIType.  
END FUNCTION.


FUNCTION fGetTermReq RETURNS INT
         (INPUT piOrigReq AS INT,
          INPUT pcDCEvent AS CHAR):

   DEF BUFFER bufMsRequest FOR MsRequest .
   FIND FIRST bufMsRequest WHERE
              bufMsRequest.OrigRequest  = piOrigReq AND
              bufMsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
              bufMsRequest.ReqStatus =  {&REQUEST_STATUS_DONE} AND
              bufMsRequest.ReqCParam3 = pcDCEvent NO-LOCK NO-ERROR.
   
   IF AVAIL bufMsRequest THEN RETURN bufMsRequest.MsRequest.

   RETURN 0.

END FUNCTION.

/* Modify filename to separate CRON run */
FUNCTION fModifyFileName RETURNS CHAR
         (INPUT icFileName AS CHAR,
          INPUT idaDate AS DATE,
          INPUT icMode AS CHAR):
   DEF VAR lcFilename AS CHAR NO-UNDO.
   DEF VAR lcDate AS CHAR NO-UNDO. 
   ASSIGN
      lcDate = STRING(YEAR(idaDate),"9999") +
                   STRING(MONTH(idaDate),"99") +
                   STRING(DAY(idaDate),"99")
      lcFileName = REPLACE(icFileName,"#USER",icMode)
      lcFileName = REPLACE(lcFileName,"#DATE",lcDate).

   RETURN lcFileName.

END FUNCTION.


/* WHICH SESSION MODE */

IF NOT SESSION:BATCH THEN DO:
   UPDATE 
      ldtInputDate FORMAT "99-99-9999" LABEL "STC Date: "     SKIP(1)
      lcSumFile    FORMAT "X(50)"      LABEL "Summary File: " SKIP(1)
      lcDetFile    FORMAT "X(50)"      LABEL "Details File: "
      lcErrors     FORMAT "X(50)"      LABEL "Errors File: "
   with overlay side-labels 1 column row 4 centered title "STC/BTC BillRun Control"
   frame fcontrolrep.
   hide frame fcontrolrep no-pause.

   IF ldtInputDate  = ?  OR
      NOT lcSumFile > "" OR
      NOT lcDetFile > "" OR
      NOT lcErrors  > "" THEN RETURN.

   pause 0.
   disp "Processing Data ...."
   with overlay row 10 centered side-labels frame freading title
   " STC/BTC BillRun Control ".
 
   ASSIGN
      lcSumFile    = fModifyFileName(lcSumFile,ldtInputDate,"USER")
      lcDetFile    = fModifyFileName(lcDetFile,ldtInputDate,"USER")
      lcErrors     = fModifyFileName(lcErrors,ldtInputDate,"USER")
      lcMail       = fModifyFileName(lcMail,ldtInputDate,"USER")
      ldBeginStamp = fHMS2TS(ldtInputDate,"00:00:00")
      ldEndStamp   = fHMS2TS(ldtInputDate,"23:59:59").
END.
ELSE DO:
   
   IF DAY(TODAY) EQ 1 THEN RETURN.

   ASSIGN
      lcSumFile    = fModifyFileName(lcSumFile,TODAY,"CRON")
      lcDetFile    = fModifyFileName(lcDetFile,TODAY,"CRON")
      lcErrors     = fModifyFileName(lcErrors,TODAY,"CRON")
      lcMail       = fModifyFileName(lcMail,TODAY,"CRON")
      ldBeginStamp = fHMS2TS(DATE(MONTH(Today),1,YEAR(TODAY)),"00:00:00")
      ldEndStamp   = fMakeTS().
END.

OUTPUT STREAM sErrors TO VALUE(lcErrors).
PUT STREAM sErrors UNFORMATTED
      "MSISDN;PrevSubsType/Bundle;OrigSubsType/Bundle;NewSubsType/Bundle;Contract;FromDate;EndDate;TermDate;Fee created(0=no,1=yes);Renewal(no/yes);EndDateIsSameAsContractLength;Extension(0=no,1=yes);ResidualFeeAmt" SKIP.

OUTPUT STREAM sLogMSISDN TO VALUE(lcDetFile).
PUT STREAM sLogMSISDN UNFORMATTED
      "MSISDN;PrevSubsType/Bundle;OrigSubsType/Bundle;NewSubsType/Bundle;Contract;Fee created(0=no,1=yes);Renewal(no/yes);Extension(0=no,1=yes);ResidualFeeAmt" SKIP.


lcRequestTypes = "0,81".

DO i = 1 TO NUM-ENTRIES(lcRequestTypes):

   liRequestType = INT(ENTRY(i,lcRequestTypes)).

   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.Brand     = gcBrand       AND
            MsRequest.ReqType   = liRequestType AND
            MsRequest.ReqStatus = 2             AND
            MsRequest.ActStamp >= ldBeginStamp  AND 
            MSRequest.ActStamp <= ldEndStamp:
      
      /* must be from postpaid to any other subscription */
      IF MSRequest.reqtype EQ 0 THEN DO:
         IF NOT CAN-FIND (CLIType WHERE 
                          CLIType.Brand   = gcBrand              AND
                          CLIType.CLIType = MsRequest.ReqCParam1 AND
                          CLIType.PayType = 1) THEN NEXT. 
      END.
      /* exclude voice btc */
      ELSE IF MsRequest.REqType EQ 81 THEN DO:
          IF LOOKUP(MsRequest.reqcparam2,lcBONOContracts) > 0 THEN NEXT.
      END.
      ELSE NEXT.

      liHandled = liHandled + 1.

      if NOT SESSION:BATCH AND liHandled mod 10 = 0 then do:
         disp liHandled label "Handled:" with frame freading.
         pause 0.
      end.

      fSplitTS(MsRequest.ActStamp,
               OUTPUT ldtActDate,
               OUTPUT liActTime).

      IF DAY(ldtActDate) EQ 1 OR 
        liActTime = 0 THEN ldtActDate = ldtActDate - 1 .
      
      /* had a DCType 3 periodical contract before stc */
      FOR EACH DCCLI NO-LOCK WHERE
               DCCLI.MsSeq      = MsRequest.MsSeq AND
               DCCLI.ValidTo   >= ldtActDate      AND 
               DCCLI.ValidFrom <= ldtActDate      AND
	       DCCLI.DCEvent BEGINS "TERM" USE-INDEX MsSeq,
         FIRST DayCampaign NO-LOCK WHERE
               DayCampaign.Brand   = gcBrand       AND 
               DayCampaign.DCEvent = DCCLI.DCEvent AND
               DayCampaign.DCType  = ({&DCTYPE_DISCOUNT}):

         /* find termination request from it */
         liTermReq = fGetTermReq(MsRequest.MsRequest,
                                 DCCLI.DCEvent). 
         
         IF liTermReq > 0 THEN DO:
            /* find singlefee associated */
            FIND FIRST SingleFee WHERE
                       SingleFee.Brand      = gcBrand                   AND 
                       SingleFee.HostTable  = "MobSub"                  AND 
                       SingleFee.KeyValue   = STRING(MsRequest.MsSeq)   AND
                       SingleFee.FeeModel   = DayCampaign.TermFeeModel  AND
                       SingleFee.BillPeriod = (YEAR(ldtActDate) * 100 + 
                                               MONTH(ldtActDate))
                       NO-LOCK NO-ERROR. 
            liFeeCreated = (IF AVAIL SingleFee THEN 1 ELSE 0). 
         END.
         ELSE liFeeCreated = 0.
         
         ASSIGN
            llRenewal     = FALSE
            llError       = FALSE 
            lcOrigBundle  = ""
            lcOrigCLIType = "".
                  
         lcOrigCLIType = fGetOrigCLIType(DCCLI.DCEvent,
                                         DCCLI.ValidFrom,
                                         DCCLI.RenewalDate,
                                         MsRequest.MsSeq,
                                         OUTPUT llRenewal).

         FIND FIRST bOrigCLIType NO-LOCK where
                    bOrigCLIType.brand   = "1"           and
                    bOrigCLIType.clitype = lcOrigCLIType no-error.
         IF NOT AVAIL bOrigCLIType THEN llError = true.

         FIND FIRST bNewCLiType NO-LOCK where
                    bNewCLiType.brand   = "1"                        and
                    bNewCLiType.clitype = (if msrequest.reqtype eq 0 and
                                              msrequest.reqcparam5 > ""
                                           then msrequest.reqcparam5 
                                           else msrequest.reqcparam2) no-error.
         IF NOT AVAIL bNewCLiType THEN llError = true.

         /* TERMINAL CONTRACT TERMINATION RULE CHECKS FOR STC */
         IF llError THEN .
         
         ELSE IF msrequest.reqtype = 0 then do:
            
            /* CONTM verification */
            IF msrequest.reqcparam2 = "contm" then llError = true.

            /* fee should exist with these cases */
            ELSE IF liFeecreated = 0 THEN DO:

               IF msrequest.reqcparam1 begins "cont" and
                  msrequest.reqcparam2 begins "tarj" then 
                  llError  = true.
               else if llRenewal eq true then .
               else if msrequest.crestamp <= 20130905.18000 and 
                  lookup(DCCLI.dcevent,"term18,term24,term18-50,term24-50") > 0 then . 
               else if DCCLI.dcevent begins "fterm" then do:
                  
                  IF not (fIsConvergenceTariff(bOrigCLIType.CLIType) AND
                          fIsConvergenceTariff(bNewCLiType.CLIType)) THEN llError = true.
                  else if bOrigCLIType.CompareFee > bNewCLiType.CompareFee AND
                          msrequest.reqiparam5 ne 2 then llError = true.
               end.
               else if (bOrigCLIType.CompareFee > bNewCLiType.CompareFee OR
                       (LOOKUP(bOrigCLIType.CLIType,"CONT7,CONTD9") > 0 AND bNewCLiType.CLIType EQ "CONT8")) AND
                       (msrequest.reqiparam5 ne 1 and msrequest.reqiparam5 ne 2) then 
                  llError = true.
            end. /* ELSE IF liFeecreated = 0 THEN DO: */

            /* fee should NOT exist with these cases */
            ELSE if liFeecreated = 1 then do:

               if msrequest.reqcparam2 begins "tarj" then llError = FALSE.
               /* TERM contract rule changed  3.8.2015 */
               else if llRenewal eq true AND 
                      MsRequest.ActStamp >= 20150804 then llError = FALSE.
               else if llRenewal eq true AND 
                     MsRequest.ActStamp < 20150804 then llError = true.
               else if DCCLI.dcevent begins "fterm" then do:
                  
                  IF fIsConvergenceTariff(bOrigCLIType.CLIType) AND
                     fIsConvergenceTariff(bNewCLiType.CLIType) AND  
                    (bOrigCLIType.CompareFee <= bNewCLiType.CompareFee OR
                     msrequest.reqiparam5 EQ 2) then llError = true.
               end.
               else if msrequest.reqiparam5 eq 1 or
                       msrequest.reqiparam5 eq 2 then llError = true. 
               else if msrequest.reqsource eq 
                  {&REQUEST_SOURCE_FUSION_ORDER_FALLBACK} then llError = true.
               else if msrequest.crestamp <= 20130905.18000 and 
                  lookup(DCCLI.dcevent,"term18,term24,term18-50,term24-50") > 0 then llError = true. 
               else if bOrigCLIType.CompareFee <= bNewCLiType.CompareFee AND NOT
                      (LOOKUP(bOrigCLIType.CLIType,"CONT7,CONTD9") > 0 AND bNewCLiType.CLIType EQ "CONT8")
                  then llError = true.
            end.
         END.
         
         /* TERMINAL CONTRACT TERMINATION RULE CHECKS FOR BTC */
         ELSE IF msrequest.reqtype = 81 then do:
            
            /* fee should exist with these cases */
            IF liFeecreated = 0 then do:

               if llRenewal eq true then .
               else if msrequest.crestamp <= 20130905.18000 and 
                       lookup(DCCLI.dcevent,"term18,term24,term18-50,term24-50") > 0 then .
               else if bOrigCLIType.CompareFee > bNewCLiType.CompareFee AND
                       (msrequest.reqiparam5 ne 1 and msrequest.reqiparam5 ne 2)
                  then llError = true.
            end.

            /* fee should NOT exist with these cases */
            else if liFeecreated = 1 then do:
               
               /* TERM contract rule changed  3.8.2015 */
               if llRenewal eq true AND 
                      MsRequest.ActStamp >= 20150804 then llError = FALSE.
               else if llRenewal eq true AND 
                     MsRequest.ActStamp < 20150804 then llError = true.
               else if msrequest.reqiparam5 eq 1 or
                       msrequest.reqiparam5 eq 2 then llError = true. 
               else if msrequest.crestamp <= 20130905.18000 and 
                    lookup(DCCLI.dcevent,"term18,term24,term18-50,term24-50") > 0 then llError = true.
               else if bOrigCLIType.CompareFee <= bNewCLiType.CompareFee 
                  then llError = true.
            end.
         end.

         IF msrequest.reqtype    = 0  and 
            msrequest.reqcparam5 > "" then
            lcNewType = msrequest.reqcparam2 + "/" + msrequest.reqcparam5.
         ELSE lcNewType = msrequest.reqcparam2.

         IF llError THEN DO:
            ldaCalculatedEndDate = fcontract_end_date (DCCLI.DCEvent, DCCLI.ValidFrom).
             
            IF NOT (liFeecreated EQ 0 AND 
                    ldaCalculatedEndDate EQ DCCLI.ValidTo AND
                    ldaCalculatedEndDate EQ ldtActDate)   THEN DO:

               IF liFeecreated EQ 0 THEN DO:
                  FIND FIRST bMsrequest NO-LOCK WHERE
                             bMsrequest.Msseq      = msrequest.msseq       AND
                             bMsrequest.ReqType    = 8                     AND
                             bMsrequest.Reqcparam1 = "ValidTo"             AND
                             bMsrequest.ReqStatus  = 2                     AND
                             bMsrequest.ReqCparam3 = DCCLI.dcevent         AND
                             bMsrequest.REqcparam2 = "Update"              AND
                             bMsrequest.Reqcparam4 = STRING(DCCLI.ValidTo) AND
                             bMsrequest.ReqSource  = {&REQUEST_SOURCE_NEWTON}
                  NO-ERROR.

               END.
               ELSE RELEASE bMsrequest.

               IF NOT AVAIL bMsrequest THEN DO:
                  llErrorStatus = True.
                  PUT STREAM sErrors UNFORMATTED
                      DCCLI.CLI                                 ";" 
                      MsRequest.ReqCParam1                      ";"
                      lcOrigCLIType                             ";"
                      lcNewType                                 ";"
                      DCCLI.DCEvent                             ";"
                      STRING(DCCLI.ValidFrom,"99-99-9999")      ";"
                      STRING(ldaCalculatedEndDate,"99-99-9999") ";"
                      STRING(DCCLI.ValidTo,"99-99-9999")        ";"
                      liFeeCreated                              ";"
                      llRenewal                                 ";"
                      (ldaCalculatedEndDate EQ DCCLI.ValidTo AND
                      ldaCalculatedEndDate EQ ldtActDate)       ";"
                      MsRequest.ReqIParam5                      ";"
                      "N/A" SKIP.
                END.
             END.
         END.
         PUT STREAM sLogMSISDN UNFORMATTED
             DCCLI.CLI            ";" 
             MsRequest.ReqCParam1 ";" 
             lcOrigCLIType        ";"
             lcNewType            ";"
             DCCLI.DCEvent        ";"
             liFeeCreated         ";"
             llRenewal            ";"
             MsRequest.ReqIParam5 ";"
             "N/A" SKIP. 

         /* update statistics */
         FIND FIRST ttStatistics NO-LOCK WHERE
                    ttStatistics.DCEvent     = DCCLI.DCEvent AND 
                    ttStatistics.OrigCLIType = lcOrigCLIType AND
                    ttStatistics.NewCLIType  = lcNewType     AND
                    ttStatistics.Renewal     = llRenewal     AND
                    ttStatistics.Extension   = Msrequest.ReqIParam5 NO-ERROR. 
         IF NOT AVAIL ttStatistics THEN DO:
            CREATE ttStatistics .
            ASSIGN ttStatistics.DCEvent     = DCCLI.DCEvent
                   ttStatistics.OrigCLIType = lcOrigCLIType
                   ttStatistics.NewCLIType  = lcNewType
                   ttStatistics.Renewal     = llRenewal 
                   ttStatistics.Extension   = Msrequest.ReqIParam5.
         END. 
         ASSIGN ttStatistics.DCEventQty = ttStatistics.DCEventQty + 1 
                ttStatistics.FeeQty     = ttStatistics.FeeQty + liFeeCreated.

      END.
    
      /******************************************************/

      llRenewal = ?.

      /* had a DCType 5 periodical contract before stc */
      FOR EACH DCCLI NO-LOCK WHERE
               DCCLI.MsSeq      = MsRequest.MsSeq AND
               DCCLI.ValidTo   >= ldtActDate      AND 
               DCCLI.ValidFrom <= ldtActDate      USE-INDEX MsSeq,
         FIRST DayCampaign NO-LOCK WHERE
               DayCampaign.Brand   = gcBrand       AND 
               DayCampaign.DCEvent = DCCLI.DCEvent AND
               DayCampaign.DCType  = ({&DCTYPE_INSTALLMENT}):
         
         /* find termination request from it */
         liTermReq = fGetTermReq(MsRequest.MsRequest,
                                 DCCLI.DCEvent). 

         /* find singlefee associated */
         IF liTermReq > 0 THEN DO:

            FIND FIRST SingleFee WHERE
                       SingleFee.Brand      = gcBrand                  AND 
                       SingleFee.HostTable  = "MobSub"                 AND 
                       SingleFee.KeyValue   = STRING(MsRequest.MsSeq)  AND
                       SingleFee.FeeModel   = DayCampaign.TermFeeModel AND
                       SingleFee.BillPeriod = (YEAR(ldtActDate) * 100 + 
                                               MONTH(ldtActDate))
                       NO-LOCK NO-ERROR. 

            liFeeCreated = (IF AVAIL SingleFee THEN 1 ELSE 0). 

         END.
         ELSE liFeeCreated = 0.
            
         llError = false.

         /* fee should exist */
         if MsRequest.reqcparam2 begins "tarj" and
            liFeeCreated = 0 then do:

            ldaCalculatedEndDate = fcontract_end_date (DCCLI.DCEvent,
                                                       DCCLI.ValidFrom).
/*
            IF not (ldaCalculatedEndDate EQ DCCLI.ValidTo AND
                    ldaCalculatedEndDate EQ ldtActDate) then llError = true.
*/
            ldaNoFeesAfter = DATE(month(ldaCalculatedEndDate),
                                 1,
                                 YEAR(ldaCalculatedEndDate)).
            IF DAY(DCCLI.ValidFrom) > 1 THEN
               ldaNoFeesAfter = ADD-INTERVAL(ldaNoFeesAfter, -1, "months").

            IF ldtActDate < ldaNoFeesAfter then llError = true.
         end.
         /* fee should not exist */
         else if not MsRequest.reqcparam2 begins "tarj" and
             liFeeCreated = 1 then llError = true.
            
         IF llError eq false and 
             MsRequest.reqcparam2 begins "tarj" and
             DCCLI.Amount > 0 then do:

            find bResidualFee NO-LOCK where
                 bResidualFee.brand       = gcBrand                     and
                 bResidualFee.hosttable   = "mobsub"                    and
                 bResidualFee.keyvalue    = string(DCCLI.msseq)         and
                 bResidualFee.sourcetable = "dccli"                     and
                 bResidualFee.sourcekey   = string(dccli.PerContractID) and
                 bResidualFee.CalcObj     = "RVTERM" no-error.

             IF NOT AVAIL bResidualFee then llError = true.
             else IF bResidualFee.BillPeriod NE (YEAR(ldtActDate) * 100 + 
                  MONTH(ldtActDate)) THEN llError = true.

         end.

         if llError THEN DO:
         
            ldaCalculatedEndDate = fcontract_end_date (DCCLI.DCEvent,
                                                    DCCLI.ValidFrom).

            IF NOT (liFeecreated EQ 0 AND 
                    ldaCalculatedEndDate EQ DCCLI.ValidTo AND
                    ldaCalculatedEndDate EQ ldtActDate) THEN DO:
               llErrorStatus = True.
               PUT STREAM sErrors UNFORMATTED
                   DCCLI.CLI                                 ";" 
                   MsRequest.ReqCParam1                      ";" 
                   "N/A"                                     ";"
                   MsRequest.ReqCParam2                      ";"
                   DCCLI.DCEvent                             ";"
                   STRING(DCCLI.ValidFrom,"99-99-9999")      ";"
                   STRING(ldaCalculatedEndDate,"99-99-9999") ";"
                   STRING(DCCLI.ValidTo,"99-99-9999")        ";"
                   liFeeCreated                              ";"
                   "N/A"                                     ";"
                   ldaCalculatedEndDate EQ DCCLI.ValidTo AND
                   ldaCalculatedEndDate EQ ldtActDate        ";" 
                   DCCLI.amount skip.
             END.
         END.

         PUT STREAM sLogMSISDN UNFORMATTED
             DCCLI.CLI            ";" 
             MsRequest.ReqCParam1 ";" 
             "N/A"                ";"
             MsRequest.ReqCParam2 ";"
             DCCLI.DCEvent        ";"
             liFeeCreated         ";"
             "N/A"                ";"
             DCCLI.Amount SKIP. 

         /* update statistics */
         FIND FIRST ttStatistics NO-LOCK WHERE
                    ttStatistics.DCEvent     = DCCLI.DCEvent        AND 
                    ttStatistics.OrigCLIType = MsRequest.reqcparam1 AND
                    ttStatistics.NewCLIType  = msrequest.reqcparam2 AND
                    ttStatistics.ResidualFee = (DCCLI.amount ne ? AND DCCLI.amount > 0) NO-ERROR.
         IF NOT AVAIL ttStatistics THEN DO:
            CREATE ttStatistics .
            ASSIGN ttStatistics.DCEvent     = DCCLI.DCEvent
                   ttStatistics.OrigCLIType = msrequest.reqcparam1
                   ttStatistics.NewCLIType  = msrequest.reqcparam2
                   ttStatistics.ResidualFee = (DCCLI.amount ne ? AND DCCLI.amount > 0)
                   ttStatistics.REnewal     = ?.
         END. 
         ASSIGN ttStatistics.DCEventQty = ttStatistics.DCEventQty + 1 
                ttStatistics.FeeQty     = ttStatistics.FeeQty + liFeeCreated.

      END.

   END.
END.

OUTPUT STREAM sLogMSISDN CLOSE.
OUTPUT STREAM sErrors    CLOSE.

OUTPUT STREAM sLogStat TO VALUE(lcSumFile).
PUT STREAM sLogStat UNFORMATTED
      "Contract;OrigType;NewType;Qty;FeeQty;Renewal;Extension(0=no,1=yes);ResidualFee" SKIP. 

FOR EACH ttStatistics NO-LOCK :
     PUT STREAM sLogStat UNFORMATTED 
        ttStatistics.DCEvent     ";"
        ttStatistics.OrigCLIType ";"
        ttStatistics.NewCLIType  ";"
        ttStatistics.DCEventQty  ";"
        ttStatistics.FeeQty      ";"
        ttStatistics.Renewal     ";"
        ttStatistics.Extension   ";"
        ttStatistics.ResidualFee SKIP.
END.

OUTPUT STREAM sLogStat CLOSE.
IF NOT SESSION:BATCH THEN
   hide frame freading no-pause.

/* Send report as email */
lcConfDir = fCParamC("RepConfDir").

/* mail recipients AND actual sending */
GetRecipients(lcConfDir + "stc_control_report.email").

IF llErrorStatus THEN
   lcStatusMsg = "Errors occured! See the attachment or link for further information.".
ELSE
   lcStatusMsg = "No errors found!".

IF SESSION:BATCH AND
   xMailAddr > "" THEN DO:
   xMailAttach = lcErrors.

   OUTPUT STREAM slog TO VALUE(lcMail).
   PUT STREAM slog UNFORMATTED
      "Greetings " SKIP(1)
      lcStatusMsg  SKIP(1)
      "STC Control Report is finished at " + fTS2HMS(fMakeTS()) SKIP
      "For period: "  STRING(TODAY - 1,"99.99.9999") " - " STRING(TODAY,"99.99.9999") SKIP(1)
      "Here are the reports:" SKIP
      lcErrors                SKIP
      lcSumFile               SKIP
      lcDetFile               SKIP(1)
      "Best regards"          SKIP(1).
   OUTPUT STREAM slog CLOSE.

   SendMail(lcMail,xMailAttach).
END.
