/* Instalment search utility */


DEF STREAM sOut.

DEF VAR lcNumInst      AS INT NO-UNDO INITIAL 1
                       VIEW-AS RADIO-SET RADIO-BUTTONS 
                       "Only one instalment", 1,
                       "Only two Instalment", 2,
                       "Any", 99.

DEF VAR lcContrList    AS CHAR NO-UNDO VIEW-AS SELECTION-LIST 
                       MULTIPLE LIST-ITEM-PAIRS "Item1","Item2" 
                       INNER-CHARS 16 INNER-LINES 4.
DEF VAR lcFinanceStat1 AS CHAR NO-UNDO.
DEF VAR lcFinanceStat2 AS CHAR NO-UNDO.
DEF VAR lcTempString   AS CHAR NO-UNDO.
DEF VAR ldafromdate1   AS DATE NO-UNDO.
DEF VAR ldafromdate2   AS DATE NO-UNDO.
DEF VAR ldatodate1     AS DATE NO-UNDO.
DEF VAR ldatodate2     AS DATE NO-UNDO.
DEF VAR ldaTerminate1  AS DATE NO-UNDO.
DEF VAR ldaTerminate2  AS DATE NO-UNDO.
DEF VAR llFinalPay1    AS LOG  NO-UNDO.
DEF VAR llFinalPay2    AS LOG  NO-UNDO.
DEF VAR lcOutfile      AS CHAR NO-UNDO.
DEF VAR llfinalpay     AS LOG  NO-UNDO.
DEF VAR iCnt           AS INT  NO-UNDO. 
DEF VAR llNext         AS LOG  NO-UNDO.
DEF button run-button  LABEL "RUN".
DEF button quit-button LABEL "QUIT".

DEF VAR lcValidFinStat AS CHAR NO-UNDO.

DEFINE TEMP-TABLE ttData
    FIELD custnum   LIKE Mobsub.CustNum
    FIELD cli       LIKE MobSub.Cli
    FIELD msseq     LIKE MobSub.MsSeq
    FIELD dcevent   LIKE DCCli.dcevent
    FIELD orderid   LIKE FixedFee.OrderId
    FIELD contract  LIKE FixedFee.Contract
    FIELD validfrom LIKE DCCli.ValidFrom
    FIELD validto   LIKE DCCli.ValidTo
    FIELD finalpay  AS LOG
    FIELD termdate  AS CHAR 
    INDEX idx1 AS PRIMARY msseq cli contract.

DEFINE BUFFER bfttdata FOR ttData.

lcValidFinStat = "00,B00,B01,B02,B99,Y00,Y01,Y02,Y03,Y04,Y05,Y06,Y07,Y08,Y09,Y10,Y11,Y12,Y13,Y14,Y15,0049,0081,0182,2100".

ASSIGN
   lcOutfile = "/tmp/instalment_search_" + 
               STRING(TODAY,"99999999") + 
               "_" + REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt".

OUTPUT STREAM sOut TO VALUE(lcOutfile).

PUT STREAM sout
   "Cust Num|MSISDN|SubID|DCEvent|OrderId|Contract|From|To|Final Payment|Termination Date " 
   SKIP.

FORM
   lcNumInst      LABEL "Num of Instalment"
   lcContrList    LABEL "Contract list"   
   ldafromdate1   LABEL "Period From"     HELP "From date"  FORMAT "99-99-99"
   ldafromdate2   LABEL "Period From"     HELP "From date"  FORMAT "99-99-99"
   ldatodate1     LABEL "Period To"       HELP "To date"    FORMAT "99-99-99"
   ldatodate2     LABEL "Period To"       HELP "To date"    FORMAT "99-99-99"
   ldaTerminate1  LABEL "Terminate" HELP "Termination date" FORMAT "99-99-99"
   ldaTerminate2  LABEL "Terminate" HELP "Termination date" FORMAT "99-99-99"
   lcFinanceStat1 LABEL "Financed Status" HELP "Financial status" FORMAT "x(8)"
   lcFinanceStat2 LABEL "Financed Status" HELP "Financial status" FORMAT "x(8)"
   llFinalPay1    LABEL "Final Pay"       HELP "Is final pay"   FORMAT "yes/no"
   llFinalPay2    LABEL "Final Pay"       HELP "Is final pay"   FORMAT "yes/no"
   run-button     AT COLUMN 32 ROW 13
   quit-button    AT COLUMN 37 ROW 13
   WITH OVERLAY SIDE-LABELS 2 COLUMN ROW 4 
   CENTERED 
   TITLE  " Instalment Search "
   FRAME ffind.

ENABLE ALL WITH FRAME ffind.
RUN p_hide_few.

lcTempString = "".

FOR EACH daycampaign NO-LOCK WHERE
         daycampaign.brand  = "1" AND
         daycampaign.dctype = "5":    
 IF lcTempString = "" THEN
    ASSIGN lcTempString = daycampaign.dcevent + "," + 
                          daycampaign.dcevent.
 ELSE
    ASSIGN lcTempString = lcTempString + "," + 
                          daycampaign.dcevent + "," + 
                          daycampaign.dcevent.
END.    

ASSIGN lcContrList:LIST-ITEM-PAIRS IN FRAME ffind = lcTempString.

ON 'value-changed':U OF lcNumInst
DO:
   IF lcNumInst:SCREEN-VALUE = "2" OR lcNumInst:SCREEN-VALUE = "99" THEN
      RUN p_enable_all.      
   ELSE
      RUN p_hide_few.      
END.

ON 'leave':U of lcFinanceStat1
DO:
   IF lcFinanceStat1:SCREEN-VALUE <> "" AND 
      LOOKUP(lcFinanceStat1:SCREEN-VALUE,lcValidFinStat) = 0 THEN
   DO:   
     MESSAGE "Please enter valid financed status"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN NO-APPLY.
   END.
END.

ON 'leave':U of lcFinanceStat2
DO:
   IF lcFinanceStat2:SCREEN-VALUE <> "" AND 
      LOOKUP(lcFinanceStat2:SCREEN-VALUE,lcValidFinStat) = 0 THEN
   DO:   
     MESSAGE "Please enter valid financed status"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN NO-APPLY.
   END.
END.

ON 'choose':U OF run-button
DO:
   IF lcNumInst:SCREEN-VALUE = "1" THEN
      RUN p_out_data_one_inst.
   ELSE IF (lcNumInst:SCREEN-VALUE = "2" OR lcNumInst:SCREEN-VALUE = "99") THEN           
      RUN p_out_data_two_inst.   
END.

ON 'choose':U OF quit-button
   QUIT.

ON 'leave':U OF lcContrList
DO:
   IF (lcNumInst:SCREEN-VALUE = "1") THEN
   DO:
      IF lcContrList:SCREEN-VALUE <> ? AND       
         NUM-ENTRIES(lcContrList:SCREEN-VALUE) > 1 THEN
      DO:         
         MESSAGE "More than one selection not allowed"             
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN NO-APPLY.      
      END.
   END.
   ELSE IF (lcNumInst:SCREEN-VALUE = "2" OR lcNumInst:SCREEN-VALUE = "99") THEN
   DO:
      IF lcContrList:SCREEN-VALUE <> ? AND 
         NUM-ENTRIES(lcContrList:SCREEN-VALUE) > 2 THEN
      DO:      
         MESSAGE "More than two selection not allowed"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN NO-APPLY.
      END.
   END.      
END.

PROCEDURE p_enable_all:
    ENABLE ALL WITH FRAME ffind.
END PROCEDURE.

PROCEDURE p_hide_few:
   DO WITH FRAM ffind.   
      HIDE ldafromdate2 
           ldatodate2
           ldaTerminate2  
           lcFinanceStat2
           llFinalPay2.
   END.

END PROCEDURE.

/* This procedure is to find if the current month payment will be the final payment or not */
PROCEDURE p_final_pay:
   DEFINE INPUT  PARAMETER ipFinalPay AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER ipFFNUm    AS INT  NO-UNDO.
   DEFINE OUTPUT PARAMETER opNext     AS LOG  NO-UNDO.
   DEFINE OUTPUT PARAMETER opFinalPay AS LOG  NO-UNDO.   
   
   IF ipFinalPay = "yes" THEN
   DO:            
      IF NOT CAN-FIND(FIRST FFItem WHERE 
                            FFItem.FFnum      = ipFFNUm AND
                            FFItem.billed     = FALSE AND
                            FFItem.billperiod = YEAR(TODAY) * 100 + 
                                                MONTH(TODAY)   
                            NO-LOCK) THEN
         ASSIGN opNext     = YES.
      ELSE IF NOT CAN-FIND(FIRST FFItem WHERE 
                                 FFItem.FFnum      = ipFFNUm AND
                                 FFItem.billed     = FALSE AND
                                 FFItem.billperiod > YEAR(TODAY) * 100 + 
                                                     MONTH(TODAY) NO-LOCK) THEN      
         ASSIGN opFinalPay = YES.
      ELSE
         ASSIGN opNext     = YES.
         
   END.
   ELSE IF ipFinalPay = "no" THEN
   DO:
      IF CAN-FIND(FIRST FFItem WHERE 
                        FFItem.FFnum  = ipFFNUm AND
                        FFItem.billed = FALSE AND
                        FFItem.billperiod > YEAR(TODAY) * 100 + 
                                            MONTH(TODAY)   
                           NO-LOCK) THEN
         ASSIGN opFinalPay = NO.         
      ELSE
         ASSIGN opNext = YES.  
         
   END.            
   ELSE 
   DO:
      IF CAN-FIND(FIRST FFItem WHERE 
                        FFItem.FFnum  = ipFFNUm AND
                        FFItem.billed = FALSE AND
                        FFItem.billperiod > YEAR(TODAY) * 100 + 
                                            MONTH(TODAY)   
                        NO-LOCK) THEN
         ASSIGN opFinalPay = NO.
      ELSE IF NOT CAN-FIND(FIRST FFItem WHERE 
                                 FFItem.FFnum  = ipFFNUm AND
                                 FFItem.billed = FALSE AND
                                 FFItem.billperiod > YEAR(TODAY) * 100 + 
                                                     MONTH(TODAY) 
                                 NO-LOCK) AND
                  CAN-FIND(FIRST FFItem WHERE 
                                 FFItem.FFnum  = ipFFNUm AND
                                 FFItem.billed = FALSE AND
                                 FFItem.billperiod = YEAR(TODAY) * 100 + 
                                                     MONTH(TODAY) 
                                 NO-LOCK) THEN
         ASSIGN opFinalPay = YES.
      
   END.            
END PROCEDURE.

/* This procedure is to fetch the subscriptions with 1 active or inactive instalment(payterm* or rvterm12) based on search criteria */

PROCEDURE p_out_data_one_inst:   
   DO WITH FRAME ffind:
      FOR EACH DCCli WHERE
               DCCli.Brand      = "1" AND
               (IF lcContrList:SCREEN-VALUE <> ? THEN
                DCCli.DCEvent    = lcContrList:SCREEN-VALUE
                ELSE (DCCli.DCEvent BEGINS "PAYTERM" OR
                      DCCli.DCEvent BEGINS "RVTERM" )) AND
              (IF DATE(ldafromdate1:SCREEN-VALUE) <> ? THEN
               DCCli.ValidFrom  >= DATE(ldafromdate1:SCREEN-VALUE)
               ELSE TRUE) AND
              (IF DATE(ldaTerminate1:SCREEN-VALUE) <> ? THEN
                  DCCli.ValidTo = DATE(ldaTerminate1:SCREEN-VALUE)                                  
               ELSE IF DATE(ldatodate1:SCREEN-VALUE) <> ? THEN
                  (DCCli.ValidTo <= DATE(ldatodate1:SCREEN-VALUE) AND
                   DCCli.ValidTo >= TODAY)
               ELSE IF DATE(ldatodate1:SCREEN-VALUE) = ? THEN
                  DCCli.ValidTo >= TODAY
               ELSE TRUE 
              ) AND 
              (IF DATE(ldaTerminate1:SCREEN-VALUE) <> ? THEN
               DCCli.TermDate   = DATE(ldaTerminate1:SCREEN-VALUE)
               ELSE DCCli.TermDate = ?) NO-LOCK:                 

         FIND FIRST MobSub WHERE
                    MobSub.Brand = DCCli.Brand AND
                    Mobsub.MsSeq = DCCli.MsSeq NO-LOCK NO-ERROR.

         IF NOT AVAIL MobSub THEN
            NEXT.

         FIND FIRST FixedFee WHERE 
                    FixedFee.Brand     = DCCli.Brand AND
                    FixedFee.Contract  = STRING(DCCli.PerContractID) AND
                    FixedFee.CalcObj   = DCCli.DCEvent AND
                    FixedFee.EndPeriod >= YEAR(TODAY) * 100 + MONTH(TODAY) AND
                   ((IF lcFinanceStat1:SCREEN-VALUE <> "" THEN
                    FixedFee.FinancedResult = lcFinanceStat1:SCREEN-VALUE
                    ELSE TRUE)) NO-LOCK NO-ERROR.
                    
         IF AVAIL FixedFee THEN
         DO:
            RUN p_final_pay(llFinalPay1:SCREEN-VALUE,FixedFee.FFnum,OUTPUT llNext, OUTPUT llFinalPay).            
            IF llNext THEN
               NEXT.
   
            IF NOT CAN-FIND(FIRST ttData WHERE 
                                  ttData.custnum   = Mobsub.CustNum    AND
                                  ttData.cli       = Mobsub.Cli        AND
                                  ttData.msseq     = Mobsub.MsSeq)  THEN
            DO:            
               CREATE ttData.
               ASSIGN ttData.custnum   = Mobsub.CustNum
                      ttData.cli       = Mobsub.Cli
                      ttData.msseq     = Mobsub.MsSeq
                      ttData.dcevent   = DCCli.DCEvent
                      ttData.orderid   = FixedFee.OrderId
                      ttData.contract  = FixedFee.Contract
                      ttData.validfrom = DCCli.ValidFrom
                      ttData.validto   = DCCLi.ValidTo
                      ttData.finalpay  = llfinalPay
                      ttData.TermDate  = (IF DCCli.TermDate <> ? THEN STRING(DCCli.TermDate) ELSE "").
            END.
            ELSE
            DO:
               /* This logic is to delete the record if there is 2 active contracts for same msseq
                  This logic doesn't applied to terminated contracts */
               IF DATE(ldaTerminate1:SCREEN-VALUE) = ? THEN
               DO:               
                  FIND FIRST ttData WHERE 
                             ttData.custnum   = Mobsub.CustNum    AND
                             ttData.cli       = Mobsub.Cli        AND
                             ttData.msseq     = Mobsub.MsSeq EXCLUSIVE-LOCK NO-ERROR.
                  IF AVAIL ttData THEN
                     DELETE ttData.
               END.
            END.            
         END.
      END.      
   END.  

   FOR EACH ttData NO-LOCK:   
      PUT STREAM sOut UNFORMATTED ttData.custnum "|" ttData.cli "|" ttData.msseq "|" ttData.dcevent "|" ttData.orderid "|" ttData.contract "|" ttData.validfrom "|" ttData.validto "|" ttData.finalpay "|" ttData.TermDate SKIP.
   END.

   OUTPUT STREAM sOut CLOSE.

   MESSAGE "Searched Finished...Check the file under /tmp directory"
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

   APPLY "CHOOSE" TO quit-button IN FRAME ffind.
   
END PROCEDURE.

/* This procedure is to fetch the subscriptions with 2 active or inactive instalment(payterm* or rvterm12) based on search criteria */
PROCEDURE p_out_data_two_inst:
   DEFINE VARIABLE liContrlist AS INTEGER     NO-UNDO.   
   DO WITH FRAME ffind:         
      ASSIGN liContrList = NUM-ENTRIES(lcContrList:SCREEN-VALUE).            
      FOR EACH DCCli WHERE
               DCCli.Brand      = "1" AND
              (IF liContrList > 0 THEN
               DCCli.DCEvent    = ENTRY(1,lcContrList:SCREEN-VALUE) 
               ELSE (DCCli.DCEvent BEGINS "PAYTERM" OR
                     DCCli.DCEvent BEGINS "RVTERM" )) AND
              (IF DATE(ldafromdate1:SCREEN-VALUE) <> ? THEN
               DCCli.ValidFrom >= DATE(ldafromdate1:SCREEN-VALUE)
               ELSE TRUE) AND
              (IF DATE(ldaTerminate1:SCREEN-VALUE) <> ? THEN
                  DCCli.ValidTo = DATE(ldaTerminate1:SCREEN-VALUE)                                  
               ELSE IF DATE(ldatodate1:SCREEN-VALUE) <> ? THEN
                  (DCCli.ValidTo <= DATE(ldatodate1:SCREEN-VALUE) AND
                   DCCli.ValidTo >= TODAY)
               ELSE IF DATE(ldatodate1:SCREEN-VALUE) = ? THEN
                  DCCli.ValidTo >= TODAY
               ELSE TRUE 
              ) AND 
              (IF DATE(ldaTerminate1:SCREEN-VALUE) <> ? THEN
               DCCli.TermDate   = DATE(ldaTerminate1:SCREEN-VALUE)
               ELSE DCCli.TermDate = ?) NO-LOCK:         
         
         FIND FIRST MobSub WHERE
                    MobSub.Brand = DCCli.Brand AND
                    Mobsub.MsSeq = DCCli.MsSeq NO-LOCK NO-ERROR.

         IF NOT AVAIL MobSub THEN
            NEXT.

         FIND FIRST FixedFee WHERE 
                    FixedFee.Brand = DCCli.Brand AND
                    FixedFee.Contract = STRING(DCCli.PerContractID) AND
                    FixedFee.CalcObj  = DCCli.DCEvent AND
                    FixedFee.EndPeriod >= YEAR(TODAY) * 100 + MONTH(TODAY) AND
                   ((IF lcFinanceStat1:SCREEN-VALUE <> "" THEN
                    FixedFee.FinancedResult = lcFinanceStat1:SCREEN-VALUE
                    ELSE TRUE)) NO-LOCK NO-ERROR.
                    
         IF AVAIL FixedFee THEN
         DO:
            /* If customer is paying the last payment in the current month */
            RUN p_final_pay(llFinalPay1:SCREEN-VALUE,FixedFee.FFnum,OUTPUT llNext, OUTPUT llFinalPay).
            IF llNext THEN
               NEXT.

            IF NOT CAN-FIND(FIRST ttData WHERE 
                                  ttData.custnum   = Mobsub.CustNum    AND
                                  ttData.cli       = Mobsub.Cli        AND
                                  ttData.msseq     = Mobsub.MsSeq      AND
                                  ttData.dcevent   = DCCli.DCEvent) THEN
            DO:            
               CREATE ttData.
               ASSIGN ttData.custnum   = Mobsub.CustNum
                      ttData.cli       = Mobsub.Cli
                      ttData.msseq     = Mobsub.MsSeq
                      ttData.dcevent   = DCCLi.DCEvent
                      ttData.orderid   = FixedFee.OrderId
                      ttData.contract  = FixedFee.Contract
                      ttData.validfrom = DCCli.ValidFrom
                      ttData.validto   = DCCLi.ValidTo
                      ttData.finalpay  = llfinalPay
                      ttData.TermDate  = (IF DCCli.TermDate <> ? THEN STRING(DCCli.TermDate) ELSE "").
            END.
         END.
      END.
      /* This logic run only when atleast one instalment(payterm* or rvterm12 is selected from the list) 
         If no instalment is selected from the list then the above loop is sufficient to fetch the required data */
      IF liContrList > 0 THEN
      DO:      
         FOR EACH bfttData NO-LOCK:      
            FOR EACH DCCli WHERE
                     DCCli.Brand      = "1" AND
                     (IF liContrList = 2 THEN
                     DCCli.DCEvent    = ENTRY(2,lcContrList:SCREEN-VALUE) 
                     ELSE (DCCli.DCEvent BEGINS "PAYTERM" OR
                           DCCli.DCEvent BEGINS "RVTERM" )) AND
                     DCCLi.MsSeq = bfttData.msseq AND
                    (IF DATE(ldafromdate2:SCREEN-VALUE) <> ? THEN
                     DCCli.ValidFrom  >= DATE(ldafromdate2:SCREEN-VALUE)
                     ELSE TRUE) AND
                    (IF DATE(ldaTerminate2:SCREEN-VALUE) <> ? THEN
                        DCCli.ValidTo = DATE(ldaTerminate2:SCREEN-VALUE)                                  
                     ELSE IF DATE(ldatodate2:SCREEN-VALUE) <> ? THEN
                        (DCCli.ValidTo <= DATE(ldatodate2:SCREEN-VALUE) AND
                         DCCli.ValidTo >= TODAY)
                     ELSE IF DATE(ldatodate2:SCREEN-VALUE) = ? THEN
                        DCCli.ValidTo >= TODAY
                     ELSE TRUE 
                    ) AND 
                    (IF DATE(ldaTerminate2:SCREEN-VALUE) <> ? THEN
                     DCCli.TermDate   = DATE(ldaTerminate2:SCREEN-VALUE)
                     ELSE DCCli.TermDate = ?) NO-LOCK:
        
               FIND FIRST MobSub WHERE
                          MobSub.Brand = DCCli.Brand AND
                          Mobsub.MsSeq = DCCli.MsSeq NO-LOCK NO-ERROR.
        
               IF NOT AVAIL MobSub THEN
                  NEXT.
        
               FIND FIRST FixedFee WHERE 
                          FixedFee.Brand = DCCli.Brand AND
                          FixedFee.Contract = STRING(DCCli.PerContractID) AND
                          FixedFee.CalcObj  = DCCli.DCEvent AND
                          FixedFee.EndPeriod >= YEAR(TODAY) * 100 + MONTH(TODAY) AND
                         ((IF lcFinanceStat2:SCREEN-VALUE <> "" THEN
                          FixedFee.FinancedResult = lcFinanceStat2:SCREEN-VALUE
                          ELSE TRUE)) NO-LOCK NO-ERROR.
                          
               IF AVAIL FixedFee THEN
               DO:
                  RUN p_final_pay(llFinalPay2:SCREEN-VALUE,FixedFee.FFnum,OUTPUT llNext, OUTPUT llFinalPay).
                  IF llNext THEN
                     NEXT.

                  IF NOT CAN-FIND(FIRST ttData WHERE 
                                        ttData.custnum   = Mobsub.CustNum    AND
                                        ttData.cli       = Mobsub.Cli        AND
                                        ttData.msseq     = Mobsub.MsSeq      AND
                                        ttData.dcevent   = DCCli.DCEvent) THEN
                  DO:            
                     CREATE ttData.
                     ASSIGN ttData.custnum   = Mobsub.CustNum
                            ttData.cli       = Mobsub.Cli
                            ttData.msseq     = Mobsub.MsSeq
                            ttData.dcevent   = DCCLi.DCEvent
                            ttData.orderid   = FixedFee.OrderId
                            ttData.contract  = FixedFee.Contract
                            ttData.validfrom = DCCli.ValidFrom
                            ttData.validto   = DCCLi.ValidTo
                            ttData.finalpay  = llfinalPay
                            ttData.TermDate  = (IF DCCli.TermDate <> ? THEN STRING(DCCli.TermDate) ELSE "").
                  END.
               END.
            END.  
         END.
      END.      
   END.

   /* This logic is just to remove the record if there is less than 2 records for one msseq 
      There must be 2 records for each msseq 
      This logic is not needed when "Any" is selected*/
   IF lcNumInst:SCREEN-VALUE = "2" THEN
   DO:   
      FOR EACH ttData NO-LOCK BREAK BY ttData.msseq:
         ASSIGN iCnt = iCnt + 1.
         IF LAST-OF(ttData.msseq) AND iCnt <> 2 THEN
         DO:      
            ASSIGN iCnt = 0.
            FOR EACH bfttData WHERE bfttData.msseq = ttData.msseq:
               DELETE bfttData.
            END.
         END.
      END.
   END.
   
   FOR EACH ttData NO-LOCK:   
      PUT STREAM sOut UNFORMATTED ttData.custnum "|" ttData.cli "|" ttData.msseq "|" ttData.dcevent "|" ttData.orderid "|" ttData.contract "|" ttData.validfrom "|" ttData.validto "|" ttData.finalpay "|" ttData.TermDate SKIP.
   END.

   OUTPUT STREAM sOut CLOSE.

   MESSAGE "Searched Finished...Check the file under /tmp directory"
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

   APPLY "CHOOSE" TO quit-button IN FRAME ffind.

END PROCEDURE.

WAIT-FOR 'choose' OF quit-button.


