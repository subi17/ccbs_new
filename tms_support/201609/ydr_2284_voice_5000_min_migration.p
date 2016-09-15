/*Migration of mServiceLimit*/

{timestamp.i}

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

ASSIGN ldNOW         = fMakeTS()
       lcCLITypeList = "CONT23,CONT24,CONTS,CONTSF".

OUTPUT STREAM sSL TO "YDR-2284-Voice-5000-Min-Migration.txt".

PUT STREAM sSL UNFORMATTED
   "MsSeq,CLI,CustNum,CLIType,ActivationTS,SLGroupCode,SLCode,SlSeq,OldDialType,OldSLSeq,OldInclUnit,OldInclAmt,NewDialType,NewSLSeq,NewInclUnit,NewInclAmt"
   SKIP.

IF liMsSeq = 0 THEN
DO TRANSACTION i = 1 TO NUM-ENTRIES(lcCLITypeList,","):
   FOR EACH MobSub NO-LOCK WHERE
            MobSub.Brand   = "1" AND
            MobSub.CLIType = ENTRY(i,lcCLITypeList,","):
      RUN pUpd.
   END.
END.
ELSE
DO TRANSACTION:
   FOR EACH MobSub NO-LOCK WHERE
            MobSub.MsSeq = liMsSeq:
      RUN pUpd.
   END.
END.

OUTPUT STREAM sSL CLOSE.

PROCEDURE pUpd:

   DEF VAR lcTariffBundle AS CHAR NO-UNDO.
   
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
      IF llConfirm THEN
         ASSIGN mServiceLimit.EndTS = fMake2Dt(09/30/2016,86399).
   END.

   /*Creating New MServiceLimit*/
   FIND FIRST bSL NO-LOCK WHERE
              bSL.GroupCode = lcTariffBundle AND
              bSL.DialType  = 4 NO-ERROR.
   IF llConfirm AND AVAILABLE bSL THEN DO:
      CREATE bMSL.
      ASSIGN bMSL.MSID     = NEXT-VALUE(mServiceLimit)
             bMSL.SLSeq    = bSL.SLSeq
             bMSL.MsSeq    = MobSub.MsSeq
             bMSL.CustNum  = MobSub.CustNum
             bMSL.DialType = bSL.DialType
             bMSL.InclUnit = bSL.InclUnit
             bMSL.InclAmt  = bSL.InclAmt
             bMSL.FromTS   = fMake2Dt(10/01/2016,0)
             bMSL.EndTS    = 99999999.99999
                .
      PUT STREAM sSL UNFORMATTED
          bMSL.DialType ","
          bMSL.SLSeq    ","
          bMSL.InclUnit ","
          bMSL.InclAmt
      SKIP.
      RELEASE bMSL.
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

END PROCEDURE.
