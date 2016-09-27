DEF VAR searchkey AS CHAR INIT "CONTDSL55".
DEF VAR tariffkey AS CHAR INIT "CONTDSL10".
DEF VAR execution AS LOGICAL INIT TruE.

IF execution THEN
   MESSAGE "Warning, going to remove table fields with values: " +
      searchkey + ", " + tariffkey VIEW-AS ALERT-BOX.

/*
FOR EACH BDest WHERE BDest.bdest BEGINS searchkey:
   IF execution THEN 
      DELETE BDest.
   ELSE
   DISP BDest.
END.
MESSAGE "BDest" VIEW-AS ALERT-BOX.
FOR EACH DCServiceComponent WHERE DCServiceComponent.defparam BEGINS searchkey:
   IF execution THEN
      DELETE DCServiceComponent.
   ELSE
   DISP DCServiceComponent.
END.
MESSAGE "DCSErvComp" VIEW-AS ALERT-BOX.
*/
/*
FOR EACH RatePlan WHERE Rateplan.rateplan BEGINS REPLACE(searchkey,"CONT","CONTRATO"):
   IF execution THEN
      DELETE RatePlan.
   ELSE
   DISP RatePlan.
END.
MESSAGE "RatePlan" VIEW-AS ALERT-BOX.

FOR EACH PListConf WHERE PListConf.rateplan BEGINS REPLACE(searchkey,"CONT","CONTRATO"):
   IF execution THEN
      DELETE PListConf.
   ELSE
   DISP PlistConf.
END.
MESSAGE "PlistConf" VIEW-AS ALERT-BOX.
*/
/*
FOR EACH DCSErvicePackage WHERE DCServicePackage.DCEvent BEGINS searchkey:
   IF execution THEN
      DELETE DCSErvicePackage.
   ELSE
   DISP DCSErvicePackage.
END.
MESSAGE "DCServicePackage" VIEW-AS ALERT-BOX.
*/
FOR EACH ServiceLimitTarget WHERE ServiceLimitTarget.outsiderate BEGINS searchkey:
   IF execution THEN
      DELETE ServiceLimitTarget.
   ELSE
   DISP ServiceLimitTarget.
END.
MESSAGE "ServiceLimitTarget" VIEW-AS ALERT-BOX.

FOR EACH ServiceLimit WHERE ServiceLimit.slcode BEGINS searchkey:
   IF execution THEN
      DELETE ServiceLimit.
   ELSE
   DISP ServiceLimit.
END.
MESSAGE "ServiceLimit" VIEW-AS ALERT-BOX.

FOR EACH ServiceLimitGroup WHERE ServiceLimitgroup.groupcode EQ searchkey:
   IF execution THEN
      DELETE ServiceLimitgroup.
   ELSE
   DISP ServiceLimitgroup.
END.
MESSAGE "ServiceLimitgroup" VIEW-AS ALERT-BOX.

/*
FOR EACH CLIType WHERE CLIType.Clitype EQ searchkey:
   IF execution THEN
      DELETE CLIType.
   ELSE
   DISP Clitype.clitype.
END.
*/
/*
FOR EACH CLIType WHERE CLIType.Clitype EQ Tariffkey:
   IF execution THEN
      DELETE CLIType.
   ELSE
   DISP Clitype.clitype.
END.

MESSAGE "CliType" VIEW-AS ALERT-BOX.
*/
/*
FOR EACH FeeModel WHERE FeeModel.feemodel BEGINS searchkey:
   IF execution THEN
      DELETE FeeModel.
   ELSE
   DISP feemodel.feemodel.
END.
MESSAGE "feemodel" VIEW-AS ALERT-BOX.
*/
/*
FOR EACH CTServPac WHERE CTServPac.clitype EQ tariffkey:
   IF execution THEN
      DELETE CTServPac.
   ELSE
   DISP CTSErvPac.clitype.
END.
MESSAGE "CTServPAc" VIEW-AS ALERT-BOX.
*/
/*
FOR EACH CTServPac WHERE CTServPac.clitype EQ searchkey:
   IF execution THEN
      DELETE CTServPac.
   ELSE
      DISP CTSErvPac.clitype.
END.
MESSAGE "CTServPAc" VIEW-AS ALERT-BOX.
*/
/*
FOR EACH FMItem WHERE FMItem.billcode BEGINS searchkey:
   IF execution THEN
      DELETE FMItem.
   ELSE
   DISP FMItem.billcode.
END.
MESSAGE "FMItem" VIEW-AS ALERT-BOX.
*/
/*
FOR EACH Reptext WHERE RepText.linkcode BEGINS tariffkey:
   IF execution THEN
      DELETE Reptext.
   ELSE
   DISP RepText.
END.
MESSAGE "Reptext" VIEW-AS ALERT-BOX.
*/
/*
FOR EACH Daycampaign WHERE Daycampaign.billcode BEGINS searchkey:
   IF execution THEN
      DELETE Daycampaign.
   ELSE
   DISP Daycampaign.billcode.
END.
MESSAGE "dayCampaign" VIEW-AS ALERT-BOX.
*/
/*
FOR EACH CTServEl WHERE CTServEl.clitype BEGINS searchkey:
   IF execution THEN
      DELETE CTServEl.
   ELSE
   DISP CTServEl.
END.
MESSAGE "ctservel" VIEW-AS ALERT-BOX.

FOR EACH CTServEl WHERE CTServEl.clitype BEGINS tariffkey:
   IF execution THEN
      DELETE CTServEl.
   ELSE
   DISP CTServEl.
END.
MESSAGE "ctservel" VIEW-AS ALERT-BOX.
*/
