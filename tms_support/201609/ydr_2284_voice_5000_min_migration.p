/*Migration of mServiceLimit*/

{timestamp.i}

DEF VAR i             AS INT  NO-UNDO.
DEF VAR ldNOW         AS DEC  NO-UNDO.
DEF VAR lcCLITypeList AS CHAR NO-UNDO.
DEF VAR lcSLGroupList AS CHAR NO-UNDO.
DEF VAR lcSLCodeList  AS CHAR NO-UNDO.
DEF VAR lcNewSLCodeList AS CHAR NO-UNDO.

DEFINE BUFFER bSL FOR ServiceLimit.
DEFINE STREAM sSL.

MESSAGE "Migrate the mServiceLimit With New Values ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
   TITLE "YDR-2284: MServiceLimit Migration" UPDATE llConfirm AS LOGICAL.

ASSIGN ldNOW = fMakeTS()
       lcCLITypeList = "CONT23,CONT24,CONTS,CONTS,CONTS,CONTS,CONTS,CONTS,CONTS,CONTS,CONTS,CONTS,CONTS,CONTSF,CONTSF"
       lcSLGroupList = "CONT23,CONT24,CONTS12,CONTS15,CONTS16,CONTS20,CONTS21,CONTS25,CONTS26,CONTS30,CONTS32,CONTS35,CONTS39,CONTSF10,CONTSF14"
       lcSLCodeList  = "CONT23_QTY,CONT24_QTY,CONTS12_QTY,S15_QTY,S16_QTY,S20_QTY,S21_QTY,S25_QTY,S26_QTY,S30_QTY,S32_QTY,S35_QTY,S39_QTY,SF10_QTY,SF14_QTY"
       lcNEWSLCodeList = "CONT23_MIN,CONT24_MIN,CONTS12_MIN,S15_MIN,S16_MIN,S20_MIN,S21_MIN,S25_MIN,S26_MIN,S30_MIN,S32_MIN,S35_MIN,S39_MIN,SF10_MIN,SF14_MIN".

OUTPUT STREAM sSL TO "YDR-2284-Voice-5000-Min-Migration.txt".
PUT STREAM sSL UNFORMATTED
   "MsSeq,CLI,CustNum,CLIType,ActivationTS,SLGroupCode,SLCode,SlSeq,OldDialType,OldSLSeq,OldInclUnit,OldInclAmt,NewDialType,NewSLSeq,NewInclUnit,NewInclAmt"
   SKIP.
DO i = 1 TO NUM-ENTRIES(lcCLITypeList,","):
   FOR EACH MobSub NO-LOCK WHERE
            MobSub.Brand = "1" AND
            MobSub.CLIType = ENTRY(i,lcCLITypeList,","):
      FOR FIRST ServiceLimit NO-LOCK WHERE
                ServiceLimit.GroupCode = ENTRY(i,lcSLGroupList,",") AND
                ServiceLimit.SLCode    = ENTRY(i,lcSLCodeList,","),
          FIRST MServiceLimit EXCLUSIVE-LOCK WHERE
                MServiceLimit.MsSeq    = MobSub.MsSeq AND
                MServiceLimit.DialType = ServiceLimit.DialType AND
                MServiceLimit.SlSeq    = ServiceLimit.SlSeq AND
                MServiceLimit.FromTS  <= ldNOW AND
                MServiceLimit.EndTS   >= ldNOW:

         PUT STREAM sSL UNFORMATTED
            MobSub.MsSeq        ","
            MobSub.CLI          ","
            MobSub.CustNum      ","
            MobSub.CLIType      ","
            MobSub.ActivationTS ","
            ServiceLimit.GroupCode ","
            ServiceLimit.SLCode    ","
            ServiceLimit.SlSeq     ","
            mServiceLimit.DialType ","
            mServiceLimit.SLSeq    ","
            mServiceLimit.InclUnit ","
            mServiceLimit.InclAmt  ","
         .
         FIND FIRST bSL NO-LOCK WHERE
                    bSL.GroupCode = ENTRY(i,lcSLGroupList,",") AND
                    bSL.SLCode    = ENTRY(i,lcNewSLCodeList,",") NO-ERROR.
         IF llConfirm AND AVAILABLE bSL THEN DO:
            ASSIGN mServiceLimit.DialType = bSL.DialType
                   mServiceLimit.SLSeq    = bSL.SLSeq
                   mServiceLimit.InclUnit = bSL.InclUnit
                   mServiceLimit.InclAmt  = bSL.InclAmt
                   .
            PUT STREAM sSL UNFORMATTED
                mServiceLimit.DialType ","
                mServiceLimit.SLSeq    ","
                mServiceLimit.InclUnit ","
                mServiceLimit.InclAmt
            SKIP.
         END.
         ELSE IF AVAILABLE bSL THEN
            PUT STREAM sSL UNFORMATTED
                bSL.DialType ","
                bSL.SLSeq    ","
                bSL.InclUnit ","
                bSL.InclAmt
            SKIP.
         ELSE PUT STREAM sSL UNFORMATTED SKIP.
      END.
   END.
END.
OUTPUT STREAM sSL CLOSE.
