/* ----------------------------------------------------------------------------
  MODULE .......: servicereq.p
  FUNCTION .....: service change requests
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 09.11.07 (separated from msrequest.i)
  Changed . ....: 
  Version ......: Yoigo
  --------------------------------------------------------------------------- */

{Func/msreqfunc.i}
{Func/fmakemsreq.i}
{Func/ffeecont.i}
{Func/fsubser.i}
{Func/fctserval.i}
{Func/servcomfee.i}
{Mm/barrgrp.i}
{Func/fnumberinq.i}
{Func/msopenbatch.i}
{Func/remfees.i}
{Func/barrfunc.i}
{Func/fmakesms.i}

DEF INPUT PARAMETER iiRequest AS INT NO-UNDO.

DEF VAR llDenyReRate AS LOG  NO-UNDO INIT FALSE.
DEF VAR lcError AS CHAR NO-UNDO. 

DEF BUFFER bSubSer   FOR SubSer.
DEF BUFFER bSSPara   FOR SubSerPara.


FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest OR MsRequest.ReqType NE 1 OR
   MsRequest.ReqCparam1 EQ "" THEN RETURN "ERROR".

/* pending for subrequest (used in reset process) */
IF MsRequest.ReqStatus = 8 THEN DO:
      CASE fGetSubRequestState(MsRequest.MsRequest):
      WHEN 3 THEN fReqStatus(3,"SubRequest in error").
      WHEN 2 THEN fReqStatus(0,"").
      END CASE.
      RETURN.
END.


IF NUM-ENTRIES(MsRequest.ReqCParam1,".") = 1 THEN DO:
   CASE MsRequest.ReqStatus:
   WHEN 0 THEN RUN pServCompSolog.
   WHEN 6 THEN RUN pServCompUpdate.
   END CASE.
END.

ELSE DO:
   CASE MsRequest.ReqStatus:
   WHEN 0 THEN RUN pServAttrSolog.
   WHEN 6 THEN RUN pServAttrUpdate.
   END CASE.
END.
 
RETURN RETURN-VALUE.


PROCEDURE pServCompSolog:

   DEF VAR liMarkStatus AS INT NO-UNDO.
    
   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".
   
   FIND MobSub WHERE MobSub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.
      
   IF NOT AVAILABLE MobSub THEN DO:
      fReqError("MobSub not found").
      RETURN.
   END.

   Func.Common:mSplitTS(MsRequest.ActStamp,
            OUTPUT ldtActDate,
            OUTPUT liActTime).
        
   FIND ServCom NO-LOCK WHERE
        ServCom.Brand   = Syst.Var:gcBrand AND
        ServCom.ServCom = MsRequest.ReqCParam1 NO-ERROR.
   
   IF NOT AVAILABLE ServCom THEN DO:
      fReqError("Unknown service " + MsRequest.ReqCParam1).
      RETURN.
   END. 
   
   /* is component / change allowed for clitype */
   liReqCnt = fServComValue(MobSub.CLIType,
                            MsRequest.ReqCParam1,
                            OUTPUT llAllowed).
     
   IF liReqCnt = ? AND MsRequest.ReqIParam1 > 0 THEN DO:
      fReqError("Service is not allowed for " + MobSub.CLIType).
      RETURN.
   END. 

   /* needed in solog creation */
   IF NOT CAN-FIND(FIRST SIM WHERE SIM.ICC = MobSub.ICC) OR
      NOT CAN-FIND(FIRST IMSI WHERE IMSI.ICC = MobSub.ICC) 
   THEN DO:
      fReqError("Invalid SIM/IMSI definitions on subscription").
      RETURN.
   END.
      
   DEF VAR ldeReqAmt AS DEC NO-UNDO. 
   DEF VAR liReqCnt AS INT NO-UNDO. 

   /* delete BB fees before provisioning,
      in case if provsioning fails */
   IF Msrequest.ReqCParam1 EQ "BB" AND
      Msrequest.ReqIParam1 EQ 0 AND
      ServCom.FeeModel > "" THEN
      /* remove fees concerning this service */
      RUN pDelFixedFee("",
                       ServCom.FeeModel,
                       Func.Common:mLastDayOfMonth(ldtActDate),
                       ?,
                       TRUE,  /* clean credit fees also */
                       FALSE,  /* credit singlefee for billed items */
                       MsRequest.UserCode,
                       "ServiceTermination",
                       OUTPUT ldeReqAmt,
                       OUTPUT liReqCnt).
   
   /* create solog */
   RUN Mm/setms.p(MsRequest.MSRequest,
             TRUE,
             OUTPUT liReqCnt,
             OUTPUT lcError).

   /* error occurred */
   IF liReqCnt < 0 OR lcError BEGINS "ERROR" THEN DO:
      fReqError("Solog creation failed:" + lcError).
      RETURN.
   END.
      
   /* solog was not needed -> direct additional handling */
   IF liReqCnt = 0 THEN liMarkStatus = 6.
   /* if solog was created then mark request to pending state */
   ELSE liMarkStatus = 5.
   
   fReqStatus(liMarkStatus,"").

END PROCEDURE.

PROCEDURE pServAttrSolog:

   DEF VAR lcServCom    AS CHAR NO-UNDO.
   DEF VAR lcServAttr   AS CHAR NO-UNDO.
   DEF VAR lcDefValue   AS CHAR NO-UNDO. 
   DEF VAR liMarkStatus AS INT  NO-UNDO.
       
   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".
   
   IF NUM-ENTRIES(MsRequest.ReqCParam1,".") NE 2 THEN DO:
      fReqError("Invalid attribute definition").
      RETURN.
   END.

   ASSIGN lcServCom  = ENTRY(1,MsRequest.ReqCParam1,".")
          lcServAttr = ENTRY(2,MsRequest.ReqCParam1,".").
   
   Func.Common:mSplitTS(MsRequest.ActStamp,
            OUTPUT ldtActDate,
            OUTPUT liActTime).
        
   FIND ServCom NO-LOCK WHERE
        ServCom.Brand   = Syst.Var:gcBrand AND
        ServCom.ServCom = lcServCom NO-ERROR.
   IF NOT AVAILABLE ServCom THEN DO:
      fReqError("Unknown service " + lcServCom).
      RETURN.
   END. 

   FIND ServAttr NO-LOCK WHERE
        ServAttr.Brand    = Syst.Var:gcBrand   AND
        ServAttr.ServCom  = lcServCom AND
        ServAttr.ServAttr = lcServAttr NO-ERROR.
   IF NOT AVAILABLE ServAttr THEN DO:
      fReqError("Unknown attribute " + lcServAttr).
      RETURN.
   END. 

   /* is component / change allowed for clitype */
   liReqCnt = fServComValue(MobSub.CLIType,
                            lcServCom,
                            OUTPUT llAllowed).

   IF liReqCnt = ? THEN DO:
      fReqError("Service is not allowed for " + MobSub.CLIType).
      RETURN.
   END. 

   lcDefValue = fServAttrValue(MobSub.CLIType,
                               lcServCom,
                               lcServAttr,
                               OUTPUT llAllowed).
   IF lcDefValue = ? THEN DO:
      fReqError("Attribute is not allowed for " + MobSub.CLIType).
      RETURN.
   END. 

   /* needed in solog creation */
   IF NOT CAN-FIND(FIRST SIM WHERE SIM.ICC = MobSub.ICC) OR
      NOT CAN-FIND(FIRST IMSI WHERE IMSI.ICC = MobSub.ICC) 
   THEN DO:
      fReqError("Invalid SIM/IMSI definitions on subscription").
      RETURN.
   END.
 
   /* create solog */
   RUN Mm/setms.p(MsRequest.MSRequest,
             TRUE,
             OUTPUT liReqCnt,
             OUTPUT lcError).

   /* error occurred */
   IF liReqCnt < 0 OR lcError BEGINS "ERROR" THEN DO:
      fReqError("Solog creation failed:" + lcError).
      RETURN.
   END.
      
   /* solog was not needed -> direct additional handling */
   IF liReqCnt = 0 THEN liMarkStatus = 6.
   /* if solog was created then mark request to pending state */
   ELSE liMarkStatus = 5.
    
   fReqStatus(liMarkStatus,"").

END PROCEDURE.

PROCEDURE pServCompUpdate:

   DEF VAR ldtFeeDate   AS DATE NO-UNDO. 
   DEF VAR liOldValue   AS INT  NO-UNDO. 
   DEF VAR liNewValue   AS INT  NO-UNDO.
   DEF VAR lcOldParam   AS CHAR NO-UNDO. 
   DEF VAR llAlleviate  AS LOG  NO-UNDO. 
   DEF VAR liOldRule    AS INT  NO-UNDO.
   DEF VAR liNewRule    AS INT  NO-UNDO. 
   DEF VAR lcReqContr   AS CHAR NO-UNDO. 
   DEF VAR llReqFound   AS LOG  NO-UNDO. 
   DEF VAR lcNICAddress AS CHAR NO-UNDO.
   DEF VAR lcNICSex     AS CHAR NO-UNDO.
   DEF VAR lcNICPublish AS CHAR NO-UNDO. 
   DEF VAR lcNIDAddress AS CHAR NO-UNDO.
   DEF VAR lcNIDSex     AS CHAR NO-UNDO.
   DEF VAR lcNIDPublish AS CHAR NO-UNDO. 
   DEF VAR lcSMSTxt     AS CHAR NO-UNDO. 
   DEF VAR llCurrMonth  AS LOG  NO-UNDO.
   DEF VAR liCustLang   AS INT  NO-UNDO.
   DEF VAR lcSender     AS CHAR NO-UNDO.
   DEF VAR lcError      AS CHAR NO-UNDO.
   DEF VAR liRequest    AS INT  NO-UNDO.
   DEF VAR llBBFeeReStore AS LOG NO-UNDO.

   DEF VAR lcPostpaidVoiceTariffs AS CHAR NO-UNDO.
   DEF VAR lcPrepaidVoiceTariffs  AS CHAR NO-UNDO.
   DEF VAR lcOnlyVoiceContracts   AS CHAR NO-UNDO.
   DEF VAR lcDataBundleCLITypes   AS CHAR NO-UNDO.
   DEF VAR lcGrpsBarring          AS CHAR NO-UNDO. 
   /*YPR-1965 - added D_HOTLP*/
   DEF VAR lcBarring AS CHAR NO-UNDO. 
   DEF VAR lrBarring AS ROWID no-undo.
   DEF VAR llOngoing AS LOG NO-UNDO. 

   DEF BUFFER bOrigRequest    FOR MsRequest.
   DEF BUFFER bMobsub         FOR MobSub.
   DEF BUFFER bOrder          FOR Order.
   DEF BUFFER bOrderAccessory FOR OrderAccessory.
   DEF BUFFER bSubSer         FOR SubSer.
   DEF BUFFER bRequest        FOR MsRequest.


   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   FIND MobSub WHERE MobSub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.

   IF NOT AVAILABLE MobSub THEN DO:
      fReqError("MobSub not found").
      RETURN.
   END.

   Func.Common:mSplitTS(MsRequest.ActStamp,
            OUTPUT ldtActDate,
            OUTPUT liActTime).

   FIND ServCom NO-LOCK WHERE
        ServCom.Brand   = Syst.Var:gcBrand AND
        ServCom.ServCom = MsRequest.ReqCParam1 NO-ERROR.
   IF NOT AVAILABLE ServCom THEN DO:
      fReqError("Unknown service " + MsRequest.ReqCParam1).
      RETURN.
   END. 
   
   /* is component / change allowed for clitype */
   liReqCnt = fServComValue(MobSub.CLIType,
                            MsRequest.ReqCParam1,
                            OUTPUT llAllowed).
   IF liReqCnt = ? AND MsRequest.ReqIParam1 > 0 THEN DO:
      fReqError("Service is not allowed for " + MobSub.CLIType).
      RETURN.
   END. 

   /* Bono VoIP  */
   IF ServCom.Target EQ {&SERVCOM_TARGET_STATELESS} THEN DO:
      fReqStatus(2,"").
      RETURN. 
   END.

   /* YPR-559-Resend internet or hotling barring to network after LTE activation */
   IF MsRequest.ReqCParam1 EQ "LTE" AND
      MsRequest.ReqIParam1 EQ 1 AND
      MsRequest.ReqSource NE {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION} 
      THEN DO:

      llOngoing = fCheckBarrStatus(MobSub.MsSeq, OUTPUT lcBarring, OUTPUT lrBarring).
      IF NOT llOngoing AND
          fIsInList("Debt_HOTL,Debt_HOTLP,Internet", lcBarring) EQ TRUE THEN DO:
      
         RUN Mm/barrengine.p(MobSub.MsSeq,
                          "#REFRESH",
                          {&REQUEST_SOURCE_SERVICE_CHANGE},
                          Syst.Var:katun,               /* creator */
                          Func.Common:mMakeTS(),           /* activate */
                          "",                  /* sms */
                          OUTPUT lcError).

         liRequest = 0.
         liRequest = INTEGER(lcError) NO-ERROR.
         /* Write possible error to a memo */
         IF liRequest = 0 OR liRequest = ? THEN
            Func.Common:mWriteMemo("MobSub",
                             STRING(Mobsub.MsSeq),
                             Mobsub.CustNum,
                             "Barring and suspension",
                             "Barring resend " + MobSub.BarrCode +
                             " request failed: " + lcError).
      END. /* IF MobSub.BarrCode = "D_HOTL" OR MobSub.BarrCode = "D_HOTLP THEN DO: */
   END. /* IF MsRequest.ReqCParam1 EQ "LTE" AND */

   FIND FIRST SubSer NO-LOCK WHERE
              SubSer.MsSeq   = MobSub.MsSeq         AND
              SubSer.ServCom = MsRequest.ReqCParam1 NO-ERROR.
   IF AVAILABLE SubSer THEN DO:
         
      IF Subser.SSStat  = MsRequest.ReqIParam1 AND
         SubSer.SSParam = MsRequest.ReqCParam2 
      THEN DO:
         /* accept request as done, but mark that TMS was not updated */
         fReqStatus(2,"Nothing to do").
         RETURN. 
      END.

      /* new request is further in the past than latest one */
      IF SubSer.SSDate > ldtActDate THEN DO:
         fReqError("A newer setting exists for service").
         RETURN.
      END. 

      ASSIGN liOldValue = SubSer.SSStat
             lcOldParam = SubSer.SSParam.
   END.

   IF AVAILABLE SubSer AND SubSer.SSDate = ldtActDate THEN DO:
      FIND CURRENT SubSer EXCLUSIVE-LOCK.
   END.
   ELSE DO:
      CREATE SubSer.
      ASSIGN SubSer.MsSeq   = MobSub.MsSeq
             SubSer.ServCom = MsRequest.ReqCParam1
             SubSer.SSDate  = ldtActDate
             Subser.ssParam = MSrequest.ReqCParam2.
   END.
    
   ASSIGN SubSer.SSStat  = MsRequest.ReqIParam1
          liNewValue     = SubSer.SSStat
          SubSer.SSParam = MsRequest.ReqCParam2
          SubSer.ServPac = ""
          llReRate       = FALSE
          lcSMSTxt       = "".

   ldCurrStamp = Func.Common:mMakeTS().

   /* Note: Consider 2 (suspend status) as Close status for BB service */
   IF SubSer.ServCom = "BB" THEN DO:
      IF liNewValue = 2 THEN liNewValue = 0.
      ELSE IF liOldValue = 2 THEN liOldValue = 0.
   END. /* IF SubSer.ServCom = "BB" THEN DO: */

   /* attributes for number inquiry */
   IF SubSer.ServCom = "NumberInq" THEN DO:
      
      llReqFound = FALSE.
      /* are there already requests for attribute changes */
      FOR EACH bRequest NO-LOCK USE-INDEX MsSeq WHERE
               bRequest.MsSeq      = MobSub.MsSeq          AND
               bRequest.ReqType    = 1                     AND
               bRequest.MsRequest  > MsRequest.MsRequest   AND
               bRequest.ReqCparam1 BEGINS SubSer.ServCom + ".":
         llReqFound = TRUE.
         LEAVE.
      END. 

      IF NOT llReqFound THEN DO:

         /* get current values */
         liReqCnt = fNumberInqValues(MobSub.MsSeq,
                                     OUTPUT lcNICPublish,
                                     OUTPUT lcNICAddress,
                                     OUTPUT lcNICSex).
                                     
         /* service set as public -> get default values for attributes */
         IF liNewValue = 0 
         THEN lcNIDPublish = fNumberInqOpened(Mobsub.CLIType,
                                              OUTPUT lcNIDAddress,
                                              OUTPUT lcNIDSex).

         /* service set as secret; attribute values have no real meaning then,
            but set attribute values anyway -> clearer view for user */
         ELSE lcNIDPublish = fNumberInqClosed(OUTPUT lcNIDAddress,
                                              OUTPUT lcNIDSex).

         /* make requests from changes */
         fNumberInqAttrChanged(MobSub.MsSeq,
                               ldCurrStamp,
                               lcNICPublish,
                               lcNICAddress,
                               lcNICSex,
                               lcNIDPublish,
                               lcNIDAddress,
                               lcNIDSex).
      END.
   END.
   
   /* delete fees associated with closed service */
   IF liOldValue > 0 AND liNewValue = 0 AND ServCom.FeeModel > "" 
   THEN DO:
      ASSIGN ldtFeeDate  = SubSer.SSDate
             llCurrMonth = FALSE.

      /* has request been originally made before 1. day's time limit */
      IF MsRequest.ReqCParam2 BEGINS "CMC=" THEN
         llCurrMonth = (SUBSTRING(MsRequest.ReqCParam2,5) = "yes").
      
      /* service is always billed to the end of change month */
      IF DAY(ldtFeeDate) NE 1 OR llCurrMonth THEN DO:
          IF MONTH(ldtFeeDate) = 12
          THEN ldtFeeDate = DATE(1,1,YEAR(ldtFeeDate) + 1).
          ELSE ldtFeeDate = DATE(MONTH(ldtFeeDate) + 1,1,YEAR(ldtFeeDate)).
      END.
               
      /* remove fees concerning this service */
      RUN pDelFixedFee("",
                       ServCom.FeeModel,
                       ldtFeeDate,
                       ?,
                       TRUE,  /* clean credit fees also */
                       FALSE,  /* credit singlefee for billed items */
                       MsRequest.UserCode,
                       "ServiceTermination",
                       OUTPUT ldReqAmt,
                       OUTPUT liReqCnt).
   END.

   /* service closed */
   IF liOldValue > 0 AND SubSer.SSStat = 0 THEN DO:
      /* fee for closing service */
      IF ServCom.ClFeeModel > "" THEN DO:
         fServiceChangeFee(ServCom.ClFeeModel,
                           SubSer.SSDate,
                           "",
                           "",
                           MsRequest.UserCode,
                           "ServiceTermination"). 
      END.

      /* SMS */
      lcSMSTxt = ServCom.ClSMSTxt.
   END.                
   
   /* service opened */       
   IF liOldValue = 0 AND liNewValue > 0 THEN DO:

      /* fee for activating service */
      IF ServCom.FeeModel > "" THEN DO:

         /* Additional logic specific to BB service               */
         /* If we activate the BB service again in the same month */
         /* when deactivation happened then don't create separate */
         /* fixedfee and restore the existing fixedfee itself     */
         IF SubSer.ServCom = "BB" THEN
            FOR FIRST FixedFee WHERE
                      FixedFee.Brand     = Syst.Var:gcBrand        AND
                      FixedFee.Custnum   = MobSub.CustNum AND
                      FixedFee.HostTable = "MobSub"       AND
                      FixedFee.KeyValue  = STRING(MobSub.MsSeq) AND
                      FixedFee.EndPeriod = (YEAR(SubSer.SSDate) * 100 +
                                            MONTH(SubSer.SSDate)) AND
                      FixedFee.FeeModel  = "BBMF" NO-LOCK:
               llBBFeeReStore = TRUE.
            END. /* FOR FIRST FixedFee WHERE */

         ldtFeeDate = SubSer.SSDate.
            
         /* service is always billed from the beginning of month 
            when opening for an old subscription, except BB service */
         IF DAY(ldtFeeDate) NE 1 AND SubSer.ServCom <> "BB" THEN 
            ldtFeeDate = DATE(MONTH(ldtFeeDate),1,YEAR(ldtFeeDate)).
             
         lcReqContr = "".
         /* if salesman defined then get contract id */
         IF MsRequest.Salesman > "" THEN 
            lcReqContr = fFeeContract(Syst.Var:gcBrand,
                                      MobSub.CustNum,
                                      MsRequest.Salesman,
                                      ldtFeeDate,
                                      "").
      
         IF NOT llBBFeeReStore THEN
            fServiceOpenFee(ServCom.FeeModel,
                         ldtFeeDate,
                         "",
                         lcReqContr,
                         MsRequest.UserCode,
                         "ServiceActivation"). 
         ELSE DO:
            /* Open the existing fixed fee related to the BB service */
            FOR FIRST FixedFee WHERE
                      FixedFee.Brand     = Syst.Var:gcBrand        AND
                      FixedFee.Custnum   = MobSub.CustNum AND
                      FixedFee.HostTable = "MobSub"       AND
                      FixedFee.KeyValue  = STRING(MobSub.MsSeq) AND
                      FixedFee.EndPeriod = (YEAR(SubSer.SSDate) * 100 +
                                            MONTH(SubSer.SSDate)) AND
                      FixedFee.FeeModel  = "BBMF" EXCLUSIVE-LOCK:

               ASSIGN FixedFee.EndPeriod = 999999
                      FixedFee.CustPP    = 0.

               /* At least 1 year unbilled fixed fee items */
               FIND LAST FFItem OF FixedFee NO-LOCK NO-ERROR.
               IF AVAILABLE FFItem THEN DO:
                  IF FFItem.BillPeriod <= YEAR(TODAY) * 100 + MONTH(TODAY) + 100 THEN
                     fMakeContractMore(INPUT Fixedfee.FFNum, 
                                       INPUT FFItem.Concerns[2]).
               END. /* IF AVAILABLE FFItem THEN DO: */
            END. /* FOR FIRST FixedFee WHERE */
         END. /* ELSE DO: */
      END.

      /* SMS */
      lcSMSTxt = ServCom.SMSTxt.
                         
   END.

   /* service's value has been changed */
   IF (liOldValue > 0 AND SubSer.SSStat > 0 AND
       liOldValue NE SubSer.SSStat) OR
      (liOldValue = SubSer.SSStat AND
       liOldValue > 0             AND
       lcOldParam NE SubSer.SSParam)  
   THEN DO:
   
      /* fee for changing value of service */
      IF ServCom.ChgFeeModel > "" THEN DO:
         fServiceChangeFee(ServCom.ChgFeeModel,
                           SubSer.SSDate,
                           "",
                           "",
                           MsRequest.UserCode,
                           "ServiceChange"). 
      END.      

      /* SMS */
      lcSMSTxt = ServCom.ChgSMSTxt.
      
   END.

   /* confirmation letter for activating saldo agreement */
   IF SubSer.ServCom = "SALDOAGR" AND
      liOldValue = 0 AND SubSer.SSStat > 0 
   THEN DO:
      RUN Mc/prinmsal.p (MobSub.MsSeq,
                    OUTPUT lcReqChar).
      IF lcReqChar > "" THEN DO:
         fReqLog("Saldo Agr. confirmation print failed: " + lcReqChar).
      END. 
   END. 
   
   /* check links to other components */
   RUN pCheckServiceLinks (MobSub.MsSeq,
                           SubSer.ServCom,
                           liOldValue,
                           SubSer.SSStat,
                           MsRequest.ActStamp,
                           MsRequest.Salesman,
                           "",
                           MsRequest.ReqSource,
                           MsRequest.MsRequest,
                           FALSE,
                           OUTPUT lcReqChar).
 
   /* mark the list of created subrequests */
   IF lcReqChar > "" THEN DO:
      FIND CURRENT MsRequest EXCLUSIVE-LOCK.
      MsRequest.ReqCParam4 = lcReqChar.
   END. 
   
   /* alleviation of barring level */
   FIND FIRST ttBarring WHERE
              ttBarring.ttServCom = SubSer.ServCom NO-ERROR.
   IF AVAILABLE ttBarring AND
      SubSer.SSStat = 0
   THEN DO:
    
      /* only one barring component per group can be active simultaneously
         -> check other components in same group */
      ASSIGN lcReqChar   = ttBarring.ttGroup
             liReqCnt    = ttBarring.ttPrior
             llAlleviate = ?.
             
      FOR EACH ttBarring WHERE
               ttBarring.ttGroup    = lcReqChar      AND
               ttBarring.ttServCom NE SubSer.ServCom,
         FIRST bSubSer NO-LOCK WHERE
               bSubSer.MsSeq   = SubSer.MsSeq AND
               bSubSer.ServCom = ttBarring.ttServCom:
               
         /* unhandled request */
         FIND FIRST bRequest NO-LOCK WHERE
                    bRequest.MsSeq      = bSubSer.MsSeq   AND
                    bRequest.ReqType    = 1               AND
                    bRequest.ReqCParam1 = bSubSer.ServCom AND
                    bRequest.ReqStatus  = 0 NO-ERROR.
                   
         IF (AVAILABLE bRequest AND
             bRequest.ReqIParam1 = 1)   OR
            (NOT AVAILABLE bRequest AND
             bSubSer.SSStat = 1)
         THEN DO:
            /* barring with lower priority is/will be active (1=highest) 
               -> create fee */
            llAlleviate = (ttBarring.ttPrior > liReqCnt).
            LEAVE.
         END.
      END. 

         /* barring was closed and no other one was activated */
      IF llAlleviate = ? OR 
         /* barring with lower priority was activated */
         llAlleviate = TRUE 
      THEN DO:
          RUN Mc/creasfee.p (MobSub.CustNum,
                        MobSub.MsSeq,
                        SubSer.SSDate,
                        "MobSub",
                        "ESTO_POISTO",
                        1,
                        ?,
                        "",
                        FALSE,
                        Syst.Var:katun,
                        "",
                        0,
                        "",
                        "",
                        OUTPUT lcReqChar).
      END.     
      
   END.    

   /* close service limit counters */
   IF SubSer.SSStat = 0 AND ServCom.ServiceLimit > "" THEN 
   FOR EACH MServiceLimit EXCLUSIVE-LOCK WHERE
            MServiceLimit.MsSeq = MobSub.MsSeq AND
            MServiceLimit.EndTS > MsRequest.ActStamp,
      FIRST ServiceLimit NO-LOCK WHERE
            ServiceLimit.SLSeq     = MServiceLimit.SLSeq  AND
            ServiceLimit.GroupCode = ServCom.ServiceLimit:

      ASSIGN MServiceLimit.EndTS = MsRequest.ActStamp
             llReRate            = TRUE.
   END.

   /* answering service activated */
   IF SubSer.ServCom = "PP2" AND liOldValue = 0 AND SubSer.SSStat = 1
   THEN DO:
      RUN Mm/makedcf.p(MobSub.MsSeq,
                  SubSer.SSDate,
                  TRUE).
   END.
    
   FIND Customer OF MobSub NO-LOCK.

   IF SubSer.ServCom BEGINS "SALDO" THEN DO:

      llReRate = TRUE.
     
      FOR EACH SaldoCounter EXCLUSIVE-LOCK WHERE 
               SaldoCounter.MsSeq  = MobSub.MsSeq AND 
               SaldoCounter.Period = YEAR(TODAY) * 100 + MONTH(TODAY):
         SaldoCounter.MobLimit = 0.      
      END.

      /* open subscription, if it has been closed due to saldo agreement and
         service has now been closed */
      IF SubSer.SSStat  = 0 AND
         SubSer.ServCom = "SALDOAGR" 
      THEN fOpenSaldoBarring().
   END.
      
   /* RUN Rate/rerate.p (needed especially with saldo-services) */
   IF llReRate AND NOT llDenyReRate THEN DO:
   
      fReRateTriggerEvent(INPUT MsRequest.MSRequest,RECID(MSRequest)).
      
      RUN pReRate(MobSub.CLI,
                  Customer.InvCust,
                  ldtActDate).

   END.
   
   RELEASE SubSer.

   /* text assigned to request itself has always priority 1 */
   IF MsRequest.SMSText > "" THEN lcSMSTxt = MsRequest.SMSText.

   /* BB Service SMS details */
   IF MsRequest.ReqCParam1 = "BB" THEN DO:

      ASSIGN lcPostpaidVoiceTariffs = fCParamC("POSTPAID_VOICE_TARIFFS")
             lcPrepaidVoiceTariffs  = fCParamC("PREPAID_VOICE_TARIFFS")
             lcDataBundleCLITypes   = fCParamC("DATA_BUNDLE_BASED_CLITYPES")
             lcOnlyVoiceContracts   = fCParamC("ONLY_VOICE_CONTRACTS").

      FOR FIRST bOrigRequest NO-LOCK WHERE
                bOrigRequest.Brand     = Syst.Var:gcBrand AND
                bOrigRequest.MsRequest = MsRequest.OrigRequest:
         IF MsRequest.ReqIParam1 = 2 AND /* STC/BTC Initiator */
            (bOrigRequest.ReqType = 0 OR bOrigRequest.ReqType = 81) THEN DO:
            IF bOrigRequest.ReqType = 81 THEN
               lcSMSTxt = MsRequest.ReqCParam1 + "DeActSTCPostV2".
            ELSE IF LOOKUP(bOrigRequest.ReqCParam2,lcPrepaidVoiceTariffs) > 0
            THEN lcSMSTxt = MsRequest.ReqCParam1 + "DeActSTCPreV_2".
            ELSE IF bOrigRequest.ReqCParam2 = "TARJRD1" THEN
               lcSMSTxt = MsRequest.ReqCParam1 + "DeActSTCPreD_2".
            ELSE IF LOOKUP(bOrigRequest.ReqCParam2,
                           lcPostpaidVoiceTariffs) > 0 THEN DO:
               IF (bOrigRequest.ReqCParam5 = "" OR
                   LOOKUP(bOrigRequest.ReqCParam5,lcOnlyVoiceContracts) > 0)
                  AND LOOKUP(bOrigRequest.ReqCParam2,lcDataBundleCLITypes) = 0 THEN
                  lcSMSTxt = MsRequest.ReqCParam1 + "DeActSTCPostV2".
            END. /* ELSE IF LOOKUP(bOrigRequest.ReqCParam2, */
         END. /* IF bOrigRequest.ReqType = 0 */
         ELSE IF bOrigRequest.ReqType = 13 THEN DO:  /* New Subs with BB */
            FOR FIRST bOrder WHERE
                      bOrder.Brand   = Syst.Var:gcBrand AND
                      bOrder.OrderId = INT(bOrigRequest.ReqCParam2) NO-LOCK,
                FIRST bOrderAccessory WHERE
                      bOrderAccessory.Brand   = Syst.Var:gcBrand AND
                      bOrderAccessory.OrderId = bOrder.OrderId AND
                      bOrderAccessory.TerminalType = {&TERMINAL_TYPE_PHONE} NO-LOCK:
                lcSMSTxt = MsRequest.ReqCParam1 + "ActNewTerm".
            END. /* FOR FIRST bOrder WHERE */
            IF lcSMSTxt = "" OR lcSMSTxt = ? THEN
               lcSMSTxt = MsRequest.ReqCParam1 + "ActNewSim".
         END. /* ELSE IF bOrigRequest.ReqType = 13 THEN DO: */
         ELSE IF bOrigRequest.ReqType = 9 THEN DO:   /* Bundle Term - BB */
            FIND FIRST bMobSub WHERE
                       bMobSub.MsSeq = bOrigRequest.MsSeq AND
                       LOOKUP(bMobSub.CliType,lcPostpaidVoiceTariffs) > 0
            NO-LOCK NO-ERROR.
            IF AVAIL bMobSub AND
               LOOKUP(bMobSub.CliType,lcDataBundleCLITypes) = 0 THEN DO:
               IF bMobSub.TariffBundle = "" OR
                  LOOKUP(bMobSub.TariffBundle,lcOnlyVoiceContracts) > 0 THEN
                  lcSMSTxt = MsRequest.ReqCParam1 + "DeActBundPost_2".
            END. /* IF AVAIL bMobSub THEN DO: */
            ELSE IF CAN-FIND(FIRST bMobSub WHERE
                                   bMobSub.MsSeq = bOrigRequest.MsSeq AND
                                   LOOKUP(bMobSub.CliType,lcPrepaidVoiceTariffs) > 0)
            THEN lcSMSTxt = MsRequest.ReqCParam1 + "DeActBundPre_2".
         END. /* ELSE IF bOrigRequest.ReqType = 9 THEN DO: */
      END. /* FOR FIRST bOrigRequest NO-LOCK WHERE */

      /* Manual BB service activation/deactivation SMS */
      IF lcSMSTxt = "" AND
         (MsRequest.ReqSource EQ {&REQUEST_SOURCE_NEWTON} OR
          MsRequest.ReqSource EQ {&REQUEST_SOURCE_EXTERNAL_API}) THEN DO:
         IF MsRequest.ReqIParam1 = 1 THEN
            lcSMSTxt = MsRequest.ReqCParam1 + "ActMan".
         ELSE IF MsRequest.ReqIParam1 = 2 THEN
            lcSMSTxt = MsRequest.ReqCParam1 + "DeActMan".
      END. /* IF MsRequest.ReqSource EQ {&REQUEST_SOURCE_NEWTON} */

      /* SMS Sender */
      lcSender = {&BB_SMS_SENDER}.
   END. /* IF MsRequest.OrigRequest > 0 THEN DO: */

   /* send SMS */
   IF lcSMSTxt > "" THEN DO:

      IF MsRequest.ReqCParam1 = "BB" THEN
         liCustLang = Customer.Language.
      ELSE
         liCustLang = 1.

      lcSMSText = fGetSMSTxt(lcSMSTxt,
                             TODAY,
                             liCustLang,
                             OUTPUT ldReqStamp).

      IF lcSMSText > "" THEN DO:                    
         ASSIGN 
            lcSMSText = REPLACE(lcSMSText,"#SERVICE",MsRequest.ReqCParam1)
            lcSMSText = REPLACE(lcSMSText,"#REQDATE",STRING(ldtActDate,
                                                            "99.99.9999")). 
         /* replace tags */
         Func.Common:mReplaceSMS
             ( Customer.CustName,
               Mobsub.CLI,
               lcSMSText,
               MsRequest.MsSeq,
               TODAY,
               OUTPUT lcSMSText).

         fMakeSchedSMS2(MobSub.CustNum,
                       MobSub.CLI,
                       11,
                       lcSMSText,
                       ldReqStamp,
                       lcSender,
                       "").
                      
         /* also to notify number */
         IF MsRequest.ReqCParam1 BEGINS "SALDO" THEN DO:
            lcReqChar = fNotifyNbrValue(MobSub.MsSeq).
            IF lcReqChar > "" AND lcReqChar NE MobSub.CLI THEN DO:
               fMakeSchedSMS(MobSub.CustNum,
                             lcReqChar,
                             11,
                             lcSMSText,
                             ldReqStamp).
            END.
         END. 
                       
      END. 
   END.
 
   RELEASE MobSub.

   IF MsRequest.ReqSource  = {&REQUEST_SOURCE_IFS} AND
      MsRequest.ReqCParam1 = "LP"                  AND 
      MsRequest.ReqIParam1 = 1                     THEN DO:
      CREATE Memo.
      ASSIGN Memo.Brand     = Syst.Var:gcBrand
             Memo.HostTable = "MobSub"
             Memo.KeyValue  = STRING(MsRequest.MsSeq)
             Memo.CustNum   = MsRequest.CustNum
             Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
             Memo.CreUser   = "IFS" 
             Memo.MemoType  = "service"
             Memo.MemoTitle = "Collection Action"
             Memo.MemoText  = MsRequest.ReqCParam1 + " applied"
             Memo.CreStamp  = Func.Common:mMakeTS().         
   END.       
            
   /* request handled succesfully */   
   fReqStatus(2,""). 

END PROCEDURE.


PROCEDURE pServAttrUpdate:

   DEF VAR lcDefValue AS CHAR NO-UNDO. 
   DEF VAR ldtFeeDate AS DATE NO-UNDO. 
   DEF VAR lcServCom  AS CHAR NO-UNDO.
   DEF VAR lcServAttr AS CHAR NO-UNDO.
   DEF VAR lcOldParam AS CHAR NO-UNDO. 
   
   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR". 

   FIND MobSub WHERE MobSub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.
      
   IF NOT AVAILABLE MobSub THEN DO:
      fReqError("MobSub not found").
      RETURN.
   END.

   FIND Customer OF MobSub NO-LOCK. 

   IF NUM-ENTRIES(MsRequest.ReqCParam1,".") NE 2 THEN DO:
      fReqError("Invalid attribute definition").
      RETURN.
   END.

   ASSIGN lcServCom  = ENTRY(1,MsRequest.ReqCParam1,".")
          lcServAttr = ENTRY(2,MsRequest.ReqCParam1,".").
   
   Func.Common:mSplitTS(MsRequest.ActStamp,
            OUTPUT ldtActDate,
            OUTPUT liActTime).
        
   FIND ServCom NO-LOCK WHERE
        ServCom.Brand   = Syst.Var:gcBrand AND
        ServCom.ServCom = lcServCom NO-ERROR.
   IF NOT AVAILABLE ServCom THEN DO:
      fReqError("Unknown service " + lcServCom).
      RETURN.
   END. 

   FIND ServAttr NO-LOCK WHERE
        ServAttr.Brand    = Syst.Var:gcBrand   AND
        ServAttr.ServCom  = lcServCom AND
        ServAttr.ServAttr = lcServAttr NO-ERROR.
   IF NOT AVAILABLE ServAttr THEN DO:
      fReqError("Unknown attribute " + lcServAttr).
      RETURN.
   END. 

   /* is component / change allowed for clitype */
   liReqCnt = fServComValue(MobSub.CLIType,
                            lcServCom,
                            OUTPUT llAllowed).

   IF liReqCnt = ? THEN DO:
      fReqError("Service is not allowed for " + MobSub.CLIType).
      RETURN.
   END. 

   lcDefValue = fServAttrValue(MobSub.CLIType,
                               lcServCom,
                               lcServAttr,
                               OUTPUT llAllowed).
   IF lcDefValue = ? THEN DO:
      fReqError("Attribute is not allowed for " + MobSub.CLIType).
      RETURN.
   END. 

   FIND FIRST SubSer NO-LOCK WHERE
              SubSer.MsSeq   = MobSub.MsSeq AND
              SubSer.ServCom = lcServCom NO-ERROR.
   IF NOT AVAILABLE SubSer THEN DO:
      fReqError("Subscription hasn't got service definition for " + 
                lcServCom).
      RETURN.
   END.
   
   FIND FIRST SubSerPara NO-LOCK WHERE
              SubSerPara.MsSeq    = MobSub.MsSeq AND
              SubSerPara.ServCom  = lcServCom AND
              SubSerPara.ParaName = lcServAttr NO-ERROR.
    
   IF AVAILABLE SubSerPara THEN DO:
      IF SubserPara.ParaValue = MsRequest.ReqCParam2 THEN DO:
         /* accept request as done, but mark that TMS was not updated */
         fReqStatus(2,"Nothing to do").
         RETURN.
      END.

      /* new request is further in the past than latest one */
      IF SubSerPara.SSDate > ldtActDate THEN DO:
         fReqError("A newer setting exists for attribute").
         RETURN.
      END. 
      
      lcOldParam = SubSerPara.ParaValue.
   END.
   
   IF AVAILABLE SubSerPara AND SubSerPara.SSDate = ldtActDate THEN DO:
      FIND CURRENT SubSerPara EXCLUSIVE-LOCK.
   END.

   ELSE DO:
      CREATE SubSerPara.
      ASSIGN SubSerPara.MsSeq    = MobSub.MsSeq
             SubSerPara.ServCom  = lcServCom
             SubSerPara.ParaName = lcServAttr
             SubSerPara.SSDate   = ldtActDate.
   END.
   
   ASSIGN SubSerPara.ParaValue = MsRequest.ReqCParam2.

   RELEASE SubSerPara.

   /* send SMS */
   IF MsRequest.SendSMS = 1 THEN DO:

      lcSMSText = fGetTxt("SMS",
                          "PALVOMMUU",
                          TODAY,
                          Customer.Language).
                         
      IF lcSMSText > "" THEN DO:                    
         ASSIGN lcSMSText = REPLACE(lcSMSText,"#SERVICE",
                                    MsRequest.ReqCParam1).
             
         /* don't send messages before 8 am. */
         IF TIME > 28800
         THEN ldReqStamp = Func.Common:mMakeTS().
         ELSE ldReqStamp = Func.Common:mMake2DT(TODAY,28800).                    
             
         fMakeSchedSMS(MobSub.CustNum,
                       MobSub.CLI,
                       13,
                       lcSMSText,
                       ldReqStamp).
      END. 
   END.
    
   /* request handled succesfully */   
   fReqStatus(2,""). 
 
END PROCEDURE.

PROCEDURE pReRate:

   DEF INPUT PARAMETER icCLI      AS CHAR NO-UNDO.
   DEF INPUT PARAMETER iiInvCust  AS INT  NO-UNDO.
   DEF INPUT PARAMETER idtActDate AS DATE NO-UNDO.

   DEF VAR ldtFrom  AS DATE NO-UNDO.
   DEF VAR ldtTo    AS DATE NO-UNDO.
   
   /* from the beginning of change month */
   ldtFrom = DATE(MONTH(idtActDate),1,YEAR(idtActDate)).
      
   /* to the end of current month */
   ldtTo = IF MONTH(TODAY) = 12
           THEN DATE(12,31,YEAR(TODAY))
           ELSE DATE(MONTH(TODAY) + 1,1,YEAR(TODAY)) - 1.

   /* check if there is for some reason an invseq with larger scope */
   FOR EACH InvSeq NO-LOCK WHERE
            InvSeq.CustNum   = iiInvCust  AND
            InvSeq.FromDate <= idtActDate AND
            InvSeq.ToDate   >= idtActDate AND
            InvSeq.Billed    = FALSE:
      ldtFrom = MIN(ldtFrom,InvSeq.FromDate).       
   END.   
      
   IF ldtTo > ldtFrom THEN DO:
         
      fReqLog("Rerate " + STRING(ldtFrom,"99.99.99") + "-" +
                          STRING(ldtTo,"99.99.99")).

      /* rerate routines may be persistent */
      &IF "{&PersistentRerate}" = "YES"
      &THEN RUN pRunRerate IN hRerate (icCLI,
                                       ldtFrom,
                                       ldtTo,
                                       TRUE).
      &ELSE RUN Rate/cli_rate.p (icCLI,
                          ldtFrom,
                          ldtTo,
                          TRUE).    
      &ENDIF
   END.
 
END PROCEDURE.

FINALLY:
   EMPTY TEMP-TABLE ttBarring.
END FINALLY.
