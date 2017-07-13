/* Q25 eligible orders */
DEF STREAM sOut.

/*DEF VAR lcOrderType    AS CHAR NO-UNDO VIEW-AS SELECTION-LIST 
                       LIST-ITEM-PAIRS "Item1","Item2" 
                       INNER-CHARS 10 INNER-LINES 2. */


DEF VAR ldaContrEndDate1   AS DATE NO-UNDO.
DEF VAR ldaContrEndDate2   AS DATE NO-UNDO.
DEF VAR lcOutfile          AS CHAR NO-UNDO.
DEF VAR lcOngoingStatus    AS CHAR NO-UNDO INIT "0,1,3,5,6,7,8,15,16,17,19,20".
DEF VAR liLoop             AS INTEGER NO-UNDO.

DEF BUTTON run-button  LABEL "RUN".
DEF BUTTON quit-button LABEL "QUIT".

DEFINE TEMP-TABLE ttData
    FIELD custnum   LIKE Mobsub.CustNum
    FIELD cli       LIKE MobSub.Cli
    FIELD msseq     LIKE MobSub.MsSeq
    FIELD dcevent   LIKE DCCli.dcevent
    FIELD validfrom LIKE DCCli.ValidFrom
    FIELD validto   LIKE DCCli.ValidTo
    INDEX idx1 AS PRIMARY msseq.

DEFINE BUFFER bfttdata FOR ttData.

ASSIGN
   lcOutfile = "/tmp/q25_eligible_data_" + STRING(TODAY,"99999999") + "_" + REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".csv".

OUTPUT STREAM sOut TO VALUE(lcOutfile).

PUT STREAM sout
   "Cust Num,MSISDN,SubID,DCEvent,Valid From,Valid To " SKIP.

FORM
   ldaContrEndDate1   LABEL "End Date From"     HELP "End Date From"  FORMAT "99-99-99"
   ldaContrEndDate2   LABEL "End Date To"       HELP "End Date To"    FORMAT "99-99-99"
   /*lcOrderType        LABEL "Order Type"      HELP "Order Type" */
   run-button     AT COLUMN 29 ROW 13
   quit-button    AT COLUMN 34 ROW 13
   WITH OVERLAY SIDE-LABELS 2 COLUMN ROW 4 
   CENTERED 
   TITLE  "Q25 Eligible Order Search "
   FRAME ffind.

ENABLE ALL WITH FRAME ffind.

/*ASSIGN lcOrderType:LIST-ITEM-PAIRS IN FRAME ffind = "Any,Any,New,New,Renewal,Renewal". */

FUNCTION fAvailTempData RETURNS LOG
   (INPUT iMsSeq AS INTEGER):

   IF CAN-FIND(FIRST ttData WHERE 
                     ttData.msseq     = iMsSeq) THEN
      RETURN YES.
   ELSE
      RETURN NO.
END FUNCTION.

ON 'choose':U OF run-button
DO:
   RUN p_out_data.
   FOR EACH ttData NO-LOCK:   
      PUT STREAM sOut UNFORMATTED ttData.custnum "," ttData.cli "," ttData.msseq "," ttData.dcevent "," ttData.validfrom "," ttData.validto  SKIP.
   END.

   OUTPUT STREAM sOut CLOSE.

   MESSAGE "Done...File generated under /tmp directory with name q25_eligible_data_"
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

   APPLY "CHOOSE" TO quit-button IN FRAME ffind.
END.

ON 'choose':U OF quit-button
   QUIT.

/*ON 'leave':U OF lcOrderType
DO:      
   
END.*/

PROCEDURE p_enable_all:
    ENABLE ALL WITH FRAME ffind.
END PROCEDURE.

PROCEDURE p_out_data:   
   DO WITH FRAME ffind:
      dccli-blk:
      FOR EACH DCCli WHERE
               DCCli.Brand      = "1" AND
               DCCli.DCEvent BEGINS "PAYTERM24" AND                             
              (IF DATE(ldaContrEndDate1:SCREEN-VALUE) <> ? THEN
                  (DCCli.ValidTo >= DATE(ldaContrEnddate1:SCREEN-VALUE) AND
                   DCCli.TermDate = ?)
               ELSE DCCli.ValidTo <= TODAY AND DCCli.TermDate = ?
               ) AND
               (IF DATE(ldaContrEndDate2:SCREEN-VALUE) <> ? THEN
                  (DCCli.ValidTo <= DATE(ldaContrEnddate2:SCREEN-VALUE) AND
                   DCCli.TermDate = ?)
               ELSE DCCli.ValidTo <= TODAY AND DCCli.TermDate = ?
               ) NO-LOCK:            

         FIND FIRST MobSub WHERE
                    MobSub.Brand = DCCli.Brand AND
                    Mobsub.MsSeq = DCCli.MsSeq NO-LOCK NO-ERROR.

         IF NOT AVAIL MobSub THEN
            NEXT dccli-blk.

         /* If there is no residual fee then no point picking the record because
            residual fees is actually prorated for next 1 year to ceate rvterm12 
            contract */

         FIND SingleFee USE-INDEX Custnum WHERE
              SingleFee.Brand       = "1" AND
              SingleFee.Custnum     = MobSub.CustNum AND
              SingleFee.HostTable   = "Mobsub" AND
              SingleFee.KeyValue    = STRING(Mobsub.MsSeq) AND
              SingleFee.SourceTable = "DCCLI" AND
              SingleFee.SourceKey   = STRING(DCCLI.PerContractId) AND
              SingleFee.CalcObj     = "RVTERM" NO-LOCK NO-ERROR.         

         IF NOT AVAIL SingleFee THEN 
            NEXT dccli-blk.
         ELSE IF SingleFee.orderid <= 0 THEN
            NEXT dccli-blk.

         DO liLoop = 1 TO NUM-ENTRIES(lcOngoingStatus):
            IF CAN-FIND(FIRST MsRequest WHERE
                              MsRequest.MsSeq      = MobSub.MsSeq  AND
                              MsRequest.ReqType    = 8 AND
                              MsRequest.ReqStatus  = INT(ENTRY(liLoop,lcOngoingStatus)) AND
                              MsRequest.ReqCParam3 = "RVTERM12") THEN
               NEXT dccli-blk.   
         END.

         IF NOT fAvailTempData(Mobsub.MsSeq) THEN       
         DO:   
            RUN pCreateData(Mobsub.CustNum,Mobsub.Cli,Mobsub.MsSeq,DCCli.DCEvent,DCCli.ValidFrom,DCCli.ValidTo).                              
         END.           
      END.      
   END.       
END PROCEDURE.

PROCEDURE pCreateData:
   DEFINE INPUT PARAMETER iCustNum    AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER cCli        AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iMsSeq      AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER cDCEvent    AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER dtValidFrom AS DATE      NO-UNDO.
   DEFINE INPUT PARAMETER dtValidTo   AS DATE      NO-UNDO.

   CREATE ttData.
   ASSIGN ttData.custnum   = iCustNum   
          ttData.cli       = cCli       
          ttData.msseq     = iMsSeq     
          ttData.dcevent   = cDCEvent    
          ttData.validfrom = dtValidFrom
          ttData.validto   = dtValidTo.

END PROCEDURE.

WAIT-FOR 'choose' OF quit-button.


