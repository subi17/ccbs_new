
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
      lcSLCode = REPLACE(icCLIType,"CONT","") + "QTY".
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

