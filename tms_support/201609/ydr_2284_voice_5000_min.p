
/*SINFIN*/
RUN CreateServiceLimit("CONT23").    /*CONT24/CONT24: La Sinfín 20 GB                    */
RUN CreateServiceLimit("CONT24").    /*CONT23/CONT23: La Sinfín 8 GB                     */

/*INFINITAS*/
RUN CreateServiceLimit("CONTS12").   /*CONTS/CONTS12: La Infinita 2.5 GB additional      */
RUN CreateServiceLimitS("CONTS15").  /*CONTS/CONTS15: La Infinita 15                     */
RUN CreateServiceLimitS("CONTS16").  /*CONTS/CONTS20: La Infinita 16                     */
RUN CreateServiceLimitS("CONTS20").  /*CONTS/CONTS25: La Infinita 20                     */
RUN CreateServiceLimitS("CONTS21").  /*CONTS/CONTS30: La Infinita 21                     */
RUN CreateServiceLimitS("CONTS25").  /*CONTS/CONTS30: La Infinita 25                     */
RUN CreateServiceLimitS("CONTS26").  /*CONTS/CONTS30: La Infinita 26                     */
RUN CreateServiceLimitS("CONTS30").  /*CONTS/CONTS30: La Infinita 30                     */
RUN CreateServiceLimitS("CONTS32").  /*CONTS/CONTS30: La Infinita 32                     */
RUN CreateServiceLimitS("CONTS35").  /*CONTS/CONTS30: La Infinita 35                     */
RUN CreateServiceLimitS("CONTS39").  /*CONTS/CONTS30: La Infinita 39                     */
RUN CreateServiceLimitS("CONTSF10"). /*CONTSF/CONTSF10: Fusión Fibra Infinita a lo Yoigo */
RUN CreateServiceLimitS("CONTSF14"). /*CONTSF/CONTSF14: Fusión Infinita a lo Yoigo       */

/*TMRULE*/
RUN CreateTM.

PROCEDURE CreateServiceLimit:
   
   DEF INPUT PARAM icCLIType AS CHAR NO-UNDO.

   DEF VAR i          AS INT  NO-UNDO.
   DEF VAR lcSLCode   AS CHAR NO-UNDO.
   DEF VAR lcOldSlSeq AS INT  NO-UNDO.

   DEFINE BUFFER bSL  FOR ServiceLimit.
   DEFINE BUFFER bSLT FOR ServiceLimitTarget.
   
   lcSLCode = icCLIType + "_QTY".
   
   /*Expiring the Old ServiceLimits*/
   FIND FIRST ServiceLimit EXCLUSIVE-LOCK WHERE
              ServiceLimit.GroupCode = icCliType AND
              ServiceLimit.SLCode    = lcSLCode NO-ERROR.
   ASSIGN ServiceLimit.ValidTo = 09/30/2016
          lcOldSlSeq           = ServiceLimit.SLSeq.
   RELEASE ServiceLimit.
   
   FIND LAST bSL USE-INDEX SLSeq NO-LOCK NO-ERROR.
   i = bSL.SLSeq + 1.

   /*Creating the New ServiceLimits*/
   CREATE ServiceLimit.
   ASSIGN ServiceLimit.GroupCode  = icCLIType
          ServiceLimit.SLCode     = icCLIType + "_MIN"
          ServiceLimit.SLName     = "National Calls"
          ServiceLimit.SLSeq      = i
          ServiceLimit.DialType   = 4
          ServiceLimit.InclAmt    = 5000.00
          ServiceLimit.InclUnit   = 1
          ServiceLimit.BDestLimit = 300
          ServiceLimit.ValidFrom  = 10/01/2016
          ServiceLimit.ValidTo    = 12/31/2049
          ServiceLimit.FirstMonthCalc = 0
          ServiceLimit.LastMonthCalc  = 0
          ServiceLimit.WebDisp        = 1
          .
   
   RELEASE bSL.
   RELEASE ServiceLimit.
   
   /*Creating the New ServiceLimitTargets*/
   FOR EACH bSLT NO-LOCK WHERE
            bSLT.SLSeq = lcOldSlSeq:
      CREATE ServiceLimitTarget.
      BUFFER-COPY bSLT EXCEPT SLSeq TO ServiceLimitTarget.
      ASSIGN ServiceLimitTarget.SLSeq = i.
   END.
   
   RELEASE bSLT.
   RELEASE ServiceLimitTarget.

END PROCEDURE.

PROCEDURE CreateServiceLimitS:

   DEF INPUT PARAM icCLIType AS CHAR NO-UNDO.

   DEF VAR i          AS INT  NO-UNDO.
   DEF VAR lcSLCode   AS CHAR NO-UNDO.
   DEF VAR lcOldSlSeq AS INT  NO-UNDO.

   DEFINE BUFFER bSL  FOR ServiceLimit.
   DEFINE BUFFER bSLT FOR ServiceLimitTarget.

   IF icCLIType BEGINS "CONTS" THEN
      lcSLCode = REPLACE(icCLIType,"CONT","") + "_QTY".
   ELSE
      lcSLCode = icCLIType + "_QTY".

   /*Expiring the Old ServiceLimits*/
   FIND FIRST ServiceLimit EXCLUSIVE-LOCK WHERE
              ServiceLimit.GroupCode = icCliType AND
              ServiceLimit.SLCode    = lcSLCode NO-ERROR.
   ASSIGN ServiceLimit.ValidTo = 09/30/2016
          lcOldSlSeq           = ServiceLimit.SLSeq.
   RELEASE ServiceLimit.

   FIND LAST bSL USE-INDEX SLSeq NO-LOCK NO-ERROR.
   i = bSL.SLSeq + 1.

   /*Creating the New ServiceLimits*/
   CREATE ServiceLimit.
   ASSIGN ServiceLimit.GroupCode  = icCLIType
          ServiceLimit.SLCode     = IF icCLIType BEGINS "CONTS" THEN REPLACE(icCLIType,"CONT","") + "_MIN"
                                    ELSE icCLIType + "_MIN"
          ServiceLimit.SLName     = "National Calls"
          ServiceLimit.SLSeq      = i
          ServiceLimit.DialType   = 4
          ServiceLimit.InclAmt    = 5000.00
          ServiceLimit.InclUnit   = 1
          ServiceLimit.BDestLimit = 300
          ServiceLimit.ValidFrom  = 10/01/2016
          ServiceLimit.ValidTo    = 12/31/2049
          ServiceLimit.FirstMonthCalc = 0
          ServiceLimit.LastMonthCalc  = 0
          ServiceLimit.WebDisp        = 1
          .

   RELEASE bSL.
   RELEASE ServiceLimit.

   /*Creating the New ServiceLimitTargets*/
   FOR EACH bSLT NO-LOCK WHERE
            bSLT.SLSeq = lcOldSlSeq:
      CREATE ServiceLimitTarget.
      BUFFER-COPY bSLT EXCEPT SLSeq TO ServiceLimitTarget.
      ASSIGN ServiceLimitTarget.SLSeq = i.
   END.

   RELEASE bSLT.
   RELEASE ServiceLimitTarget.

END PROCEDURE.


PROCEDURE CreateTM:
   
   DEF VAR i                 AS INT  NO-UNDO.
   DEF VAR liTMRuleSeq       AS INT  NO-UNDO.
   DEF VAR lcCounterItemList AS CHAR NO-UNDO.

   DEFINE BUFFER bTMRule FOR TMRule.

   FIND LAST bTMRule USE-INDEX TMRuleSeq NO-LOCK NO-ERROR.
   IF AVAILABLE bTMRule
   THEN liTMRuleSeq = bTMRule.TMRuleSeq + 1.
   
   CREATE TMRule.
   ASSIGN TMRule.Brand = "1"
          TMRule.TMRuleSeq = liTMRuleSeq
          TMRule.Name      = "VOICE 5000 Minutes"
          TMRule.FromDate  = 10/01/2016
          TMRule.ToDate    = 12/31/2049
          TMRule.NewCustomer = FALSE
          TMRule.TicketType = 1
          TMRule.PayType = 1
          TMRule.CounterType = 1
          TMRule.CounterItems = "BDest,CLIType"
          TMRule.CounterAmount = "1"
          TMRule.LimitSource = 3
          TMRule.LimitCompare = 1
          TMRule.CounterPeriod = 2
          .
   RELEASE TMRule.
   RELEASE bTMRule.

   lcCounterItemList = "CONT23_VOICE_IN,CONT23|" +
                       "CONT24_VOICE_IN,CONT24|" +
                       "CONTS12_VOICE_IN,CONTS|" +
                       "CONTSVOICE15_A,CONTS|" +
                       "CONTSVOICE16_A,CONTS|" +
                       "CONTSVOICE20_A,CONTS|" +
                       "CONTSVOICE21_A,CONTS|" +
                       "CONTSVOICE25_A,CONTS|" +
                       "CONTSVOICE26_A,CONTS|" +
                       "CONTSVOICE32_A,CONTS|" +
                       "CONTSVOICE35_A,CONTS|" +
                       "CONTSVOICE39_A,CONTS|" +
                       "CONTSFVOICE10_A,CONTSF|" +
                       "CONTSFVOICE14_A,CONTSF"
                       .

   DO i = 1 TO NUM-ENTRIES(lcCounterItemList, "|"):
      CREATE TMRItemValue.
      ASSIGN TMRItemValue.TMRuleSeq = liTMRuleSeq
             TMRItemValue.FromDate  = 10/01/2016
             TMRItemValue.ToDate    = 12/31/2049
             TMRItemValue.CounterItemValues = ENTRY(i,lcCounterItemList,"|")
             .
      RELEASE TMRItemValue.
   END.
   
   CREATE InvText.
   ASSIGN InvText.Brand = "1"
          InvText.ITNum = NEXT-VALUE(it-seq)
          InvText.Target = "SMS"
          InvText.KeyValue = "VOICE4800Limit"
          InvText.FromDate = 10/01/2016
          InvText.ToDate   = 12/31/2049
          InvText.Position = 1
          InvText.Report   = 5
          InvText.Language = 1
          InvText.SendRule = "R1"
          InvText.LetterClass = 0
          InvText.TxtTitle = "The VOICE4800 contract limit is reached"
          InvText.InvText  = "Recuerda, para hacer un uso razonable de tu tarifa hay un limite de 5000 min/mes. Te quedan 200 min. "
                           + "Despues: llam. nac. 14,52 cent/min y estb. llam. 18,15 cent."
          .
   RELEASE InvText.
   
   CREATE InvText.
   ASSIGN InvText.Brand = "1"
          InvText.ITNum = NEXT-VALUE(it-seq)
          InvText.Target = "SMS"
          InvText.KeyValue = "VOICE5000Limit"
          InvText.FromDate = 10/01/2016
          InvText.ToDate   = 12/31/2049
          InvText.Position = 1
          InvText.Report   = 5
          InvText.Language = 1
          InvText.SendRule = "R1"
          InvText.LetterClass = 0
          InvText.TxtTitle = "The VOICE5000 contract limit is reached"
          InvText.InvText  = "Para hacer un uso razonable de tu tarifa hay un limite de 5000 min/mes. Los has superado. "
                           + "A partir de ahora: llam. nac. 14,52 cent/min y estb. llam. 18,15 cent."
          .
   RELEASE InvText.

   CREATE TMRLimit.
   ASSIGN TMRLimit.TMRuleSeq = liTMRuleSeq
          TMRLimit.LimitID   = 1
          TMRLimit.FromDate  = 10/01/2016
          TMRLimit.TODate    = 12/31/2049
          TMRLimit.ValueType = 1
          TMRLimit.LimitAmt  = 4800.00
          TMRLimit.Action    = 30
          TMRLimit.SMSText   = "VOICE4800Limit"
          .
   RELEASE TMRLimit.
   
   CREATE TMRLimit.
   ASSIGN TMRLimit.TMRuleSeq = liTMRuleSeq
          TMRLimit.LimitID   = 2
          TMRLimit.FromDate  = 10/01/2016
          TMRLimit.TODate    = 12/31/2049
          TMRLimit.ValueType = 1
          TMRLimit.LimitAmt  = 5000.00
          TMRLimit.Action    = 30
          TMRLimit.SMSText   = "VOICE5000Limit"
          .
   RELEASE TMRLimit.


END PROCEDURE.
