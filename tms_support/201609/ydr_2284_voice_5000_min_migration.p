/*Migration of mServiceLimit*/


DEF VAR i               AS INT  NO-UNDO.
DEF VAR ldNOW           AS DEC  NO-UNDO.
DEF VAR lcCLITypeList   AS CHAR NO-UNDO.
DEF VAR lcSLGroupList   AS CHAR NO-UNDO.
DEF VAR lcSLCodeList    AS CHAR NO-UNDO.
DEF VAR lcNewSLCodeList AS CHAR NO-UNDO.
DEF VAR liMobSubCount   AS INT  NO-UNDO.
DEF VAR liMsSeq         AS INT FORMAT 9999999999 NO-UNDO.

DEFINE BUFFER bSL  FOR ServiceLimit.
DEFINE BUFFER bMSL FOR mServiceLimit.

DEFINE STREAM sSL.

UPDATE liMsSeq.

MESSAGE "Migrate the mServiceLimit With New Values ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
   TITLE "YDR-2284: MServiceLimit Migration" UPDATE llConfirm AS LOGICAL.

ASSIGN ldNOW         = Func.Common:mMakeTS()
       lcCLITypeList = "CONT23,CONT24,CONTS,CONTSF".

OUTPUT STREAM sSL TO "YDR-2284-Voice-5000-Min-Migration_20161003-action-10.txt".

PUT STREAM sSL UNFORMATTED
   "MsSeq,CLI,CustNum,CLIType,ActivationTS,SLGroupCode,SLCode,SlSeq,OldDialType,OldSLSeq,OldInclUnit,OldInclAmt,NewDialType,NewSLSeq,NewInclUnit,NewInclAmt"
   SKIP.
DEFINE VARIABLE liActionCount AS INTEGER NO-UNDO. 

IF liMsSeq = 0 THEN
DO :
   DO i = 1 TO NUM-ENTRIES(lcCLITypeList,","):
      FOR EACH MobSub NO-LOCK WHERE
               MobSub.Brand   = "1" AND
               MobSub.CLIType = ENTRY(i,lcCLITypeList,","):
         DO TRANS ON ERROR UNDO, LEAVE:
             RUN pUpd.
             liActionCount = liActionCount + 1.
         END.  
      END.
   END.
END.
ELSE
DO TRANSACTION ON ERROR UNDO, LEAVE:
   FOR EACH MobSub NO-LOCK WHERE
            MobSub.MsSeq = liMsSeq:
      RUN pUpd.
   END.
END.

OUTPUT STREAM sSL CLOSE.

PROCEDURE pUpd:

   DEF VAR lcTariffBundle AS CHAR NO-UNDO.
   DEF VAR liOldSLSeq     AS INT  NO-UNDO.
   DEF VAR ldSLCLimit      AS DEC  NO-UNDO.

   liMobSubCount = liMobSubCount + 1.

   IF MobSub.TariffBundle <> "" THEN
        lcTariffBundle = MobSub.TariffBundle.
   ELSE lcTariffBundle = MobSub.CliType.
   
   FOR FIRST ServiceLimit NO-LOCK WHERE
             ServiceLimit.GroupCode = lcTariffBundle AND
             ServiceLimit.DialType  = 0,
       FIRST MServiceLimit EXCLUSIVE-LOCK WHERE
             MServiceLimit.MsSeq    = MobSub.MsSeq AND
             MServiceLimit.DialType = ServiceLimit.DialType AND
             MServiceLimit.SlSeq    = ServiceLimit.SlSeq AND
             MServiceLimit.FromTS  <= ldNOW AND
             MServiceLimit.EndTS   >= ldNOW:

      PUT STREAM sSL UNFORMATTED
          MobSub.MsSeq           ","
          MobSub.CLI             ","
          MobSub.CustNum         ","
          MobSub.CLIType         ","
          MobSub.ActivationTS    ","
          ServiceLimit.GroupCode ","
          ServiceLimit.SLCode    ","
          ServiceLimit.SlSeq     ","
          mServiceLimit.DialType ","
          mServiceLimit.SLSeq    ","
          mServiceLimit.InclUnit ","
          mServiceLimit.InclAmt  ","
      .
      ASSIGN liOldSLSeq = ServiceLimit.SLSeq.
      IF llConfirm THEN
         ASSIGN mServiceLimit.EndTS = Func.Common:mMake2DT(09/30/2016,86399).
   END.

   IF liOldSLSeq EQ 0 THEN NEXT.

   /*Creating New MServiceLimit*/
   FIND FIRST bSL NO-LOCK WHERE
              bSL.GroupCode = lcTariffBundle AND
              bSL.DialType  = 4 NO-ERROR.
   IF llConfirm AND AVAILABLE bSL THEN DO:

      FIND FIRST MServiceLimit NO-LOCK where 
                 MServiceLimit.MsSeq = MobSub.MsSeq AND 
                 MServiceLimit.SlSeq = bSL.SlSeq    NO-ERROR. 
      
      IF NOT AVAIL MServiceLimit THEN DO:
         CREATE bMSL.
         ASSIGN bMSL.MSID     = NEXT-VALUE(mServiceLimit)
                bMSL.SLSeq    = bSL.SLSeq
                bMSL.MsSeq    = MobSub.MsSeq
                bMSL.DialType = bSL.DialType
                bMSL.InclUnit = bSL.InclUnit
                bMSL.InclAmt  = bSL.InclAmt
                bMSL.FromTS   = Func.Common:mMake2DT(10/01/2016,0)
                bMSL.EndTS    = 99999999.99999
                .
         FOR EACH ServiceLCounter EXCLUSIVE-LOCK WHERE
                  ServiceLCounter.MsSeq  = MobSub.MsSeq AND
                  ServiceLCounter.Period = 201610       AND
                  ServiceLCounter.SLSeq  = liOldSLSeq:
            ASSIGN ServiceLCounter.SLSeq = bSL.SLSeq
                   ldSLCLimit            = ServiceLCounter.Limit
                   ServiceLCounter.Limit = ServiceLCounter.Amt
                   ServiceLCounter.Amt   = ldSLCLimit.
         END.
         PUT STREAM sSL UNFORMATTED
             bMSL.DialType ","
             bMSL.SLSeq    ","
             bMSL.InclUnit ","
             bMSL.InclAmt
         SKIP.
         RELEASE bMSL.
      END.
   END.
   ELSE IF AVAILABLE bSL THEN
      PUT STREAM sSL UNFORMATTED
          bSL.DialType ","
          bSL.SLSeq    ","
          bSL.InclUnit ","
          bSL.InclAmt
      SKIP.
   ELSE
      PUT STREAM sSL UNFORMATTED SKIP.
   RELEASE bSL.

   IF liMobSubCount MOD 1 EQ 0 THEN DO:
      DISP liMobSubCount.
      PAUSE 0.
   END.

   RELEASE ServiceLimit.
   RELEASE mServiceLimit.
   RELEASE ServiceLCounter. 

END PROCEDURE.
