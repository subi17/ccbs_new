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
DEF button run-button  LABEL "RUN".
DEF button quit-button LABEL "QUIT".

DEF VAR lcValidFinStat AS CHAR NO-UNDO.

lcValidFinStat = "00,B00,B01,B02,B99,Y00,Y01,Y02,Y03,Y04,Y05,Y06,Y07,Y08,Y09,Y10,Y11,Y12,Y13,Y14,Y15,0049,0081,0182,2100".

ASSIGN
   lcOutfile = "/tmp/instalment_search_" + 
               STRING(TODAY,"99999999") + 
               "_" + REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt".

OUTPUT STREAM sOut TO VALUE(lcOutfile).

PUT STREAM sout
   "Cust no|MSISDN|SubID|OrderId|Contract|From|To|Final Payment|Terminated " 
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
RUN hide_few.

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
   IF lcNumInst:SCREEN-VALUE = "2" THEN
      RUN enable_all.      
   ELSE
      RUN hide_few.      
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
   IF (lcNumInst:SCREEN-VALUE = "1" OR lcNumInst:SCREEN-VALUE = "99") THEN
      RUN out_data_one_inst.
   ELSE IF lcNumInst:SCREEN-VALUE = "2" AND 
           NUM-ENTRIES(lcContrList:SCREEN-VALUE) = 2 THEN
      RUN out_data_two_inst.
END.

ON 'choose':U OF quit-button
DO:
   QUIT.
END.

ON 'leave':U OF lcContrList
DO:
   IF (lcNumInst:SCREEN-VALUE = "1" OR lcNumInst:SCREEN-VALUE = "99") THEN
   DO:
      IF NUM-ENTRIES(lcContrList:SCREEN-VALUE) >=2 THEN
      DO:      
         MESSAGE "Please select only one contract"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN NO-APPLY.
      END.
   END.
   IF lcNumInst:SCREEN-VALUE = "2" THEN
   DO:
      IF lcContrList:SCREEN-VALUE <> ? AND 
         NUM-ENTRIES(lcContrList:SCREEN-VALUE) <> 2 THEN
      DO:      
         MESSAGE "Please select two contracts"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN NO-APPLY.
      END.
   END.   
END.

PROCEDURE enable_all:
    ENABLE ALL WITH FRAME ffind.
END PROCEDURE.

PROCEDURE hide_few:
   DO WITH FRAM ffind.   
      HIDE ldafromdate2 
           ldatodate2
           ldaTerminate2  
           lcFinanceStat2
           llFinalPay2.
   END.

END PROCEDURE.

PROCEDURE out_data_one_inst:   
   DO WITH FRAME ffind:
       MESSAGE "lcContrList:SCREEN-VALUE - " lcContrList:SCREEN-VALUE SKIP
               "ldafromdate1:SCREEN-VALUE - " ldafromdate1:SCREEN-VALUE SKIP
               "ldatodate1:SCREEN-VALUE - " ldatodate1:SCREEN-VALUE SKIP 
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
      FOR EACH DCCli WHERE
               DCCli.Brand      = "1" AND
               (IF lcContrList:SCREEN-VALUE <> ? THEN
                DCCli.DCEvent    = lcContrList:SCREEN-VALUE
                ELSE (DCCli.DCEvent BEGINS "PAYTERM" OR
                      DCCli.DCEvent BEGINS "RVTERM" )) AND
              (IF DATE(ldafromdate1:SCREEN-VALUE) <> ? THEN
               DCCli.ValidFrom  >= DATE(ldafromdate1:SCREEN-VALUE)
               ELSE TRUE) AND
              (IF DATE(ldatodate1:SCREEN-VALUE) <> ? THEN
               DCCli.ValidTo    <= DATE(ldatodate1:SCREEN-VALUE)
               ELSE TRUE ) AND
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
                   ((IF lcFinanceStat1:SCREEN-VALUE <> "" THEN
                    FixedFee.FinancedResult = lcFinanceStat1:SCREEN-VALUE
                    ELSE TRUE)) NO-LOCK NO-ERROR.
                    
         IF AVAIL FixedFee THEN
         DO:
            IF llFinalPay1:SCREEN-VALUE = "yes" THEN
            DO:            
               IF CAN-FIND(FIRST FFItem WHERE 
                                 FFItem.FFnum  = FixedFee.FFnum AND
                                 FFItem.billed = FALSE AND
                                 FFItem.billperiod > YEAR(TODAY) * 100 + 
                                                     MONTH(TODAY)   
                                 NO-LOCK) THEN
                  NEXT.   
               ELSE
                  ASSIGN llFinalPay = YES.
            END.
            ELSE
            DO:
               IF NOT CAN-FIND(FIRST FFItem WHERE 
                                     FFItem.FFnum  = FixedFee.FFnum AND
                                     FFItem.billed = FALSE AND
                                     FFItem.billperiod > YEAR(TODAY) * 100 + 
                                                         MONTH(TODAY)   
                                    NO-LOCK) THEN
                  NEXT.                                 
               ELSE
                  ASSIGN llFinalPay = NO.
            END.               
            PUT STREAM sOut UNFORMATTED STRING(Mobsub.CustNum) "|" 
                                        MobSub.Cli "|" 
                                        STRING(MobSub.MsSeq) "|"
                                        STRING(FixedFee.OrderId) "|" 
                                        FixedFee.Contract "|" 
                                        STRING(DCCli.ValidFrom) "|" 
                                        STRING(DCCli.ValidTo) "|" 
                                        STRING(llfinalPay) "|" 
                                        (IF DCCli.TermDate = ? THEN "" 
                                         ELSE STRING(DCCli.TermDate)) SKIP.
         END.
      END.      
   END.   
   OUTPUT STREAM sOut CLOSE.
   MESSAGE "Done...1"
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
   
END PROCEDURE.

PROCEDURE out_data_two_inst:
   DO WITH FRAME ffind:         
      FOR EACH DCCli WHERE
               DCCli.Brand      = "1" AND
              (IF lcContrList:SCREEN-VALUE <> ? THEN
               DCCli.DCEvent    = ENTRY(1,lcContrList:SCREEN-VALUE) 
               ELSE (DCCli.DCEvent BEGINS "PAYTERM" OR
                     DCCli.DCEvent BEGINS "RVTERM" )) AND
              (IF DATE(ldafromdate1:SCREEN-VALUE) <> ? THEN
               DCCli.ValidFrom  >= DATE(ldafromdate1:SCREEN-VALUE)
               ELSE TRUE) AND
              (IF DATE(ldatodate1:SCREEN-VALUE) <> ? THEN
               DCCli.ValidTo    <= DATE(ldatodate1:SCREEN-VALUE)
               ELSE TRUE ) AND
              (IF DATE(ldaTerminate1:SCREEN-VALUE) <> ? THEN
               DCCli.TermDate   = DATE(ldaTerminate1:SCREEN-VALUE)
               ELSE DCCli.TermDate = ?) NO-LOCK:
         FIND FIRST MobSub WHERE
                    MobSub.Brand = DCCli.Brand AND
                    Mobsub.MsSeq = DCCli.MsSeq NO-LOCK NO-ERROR.
         FIND FIRST FixedFee WHERE 
                    FixedFee.Brand = DCCli.Brand AND
                    FixedFee.Contract = STRING(DCCli.PerContractID) AND
                   ((IF lcFinanceStat1:SCREEN-VALUE <> "" THEN
                    FixedFee.FinancedResult = lcFinanceStat1:SCREEN-VALUE
                    ELSE TRUE)) NO-LOCK NO-ERROR.
                    
         IF AVAIL FixedFee THEN
         DO:
            IF llFinalPay1:SCREEN-VALUE = "yes" THEN
            DO:            
               IF CAN-FIND(FIRST FFItem WHERE 
                                 FFItem.FFnum  = FixedFee.FFnum AND
                                 FFItem.billed = FALSE AND
                                 FFItem.billperiod > YEAR(TODAY) * 100 + 
                                                     MONTH(TODAY)   
                                 NO-LOCK) THEN
                  NEXT.  
               ELSE
                  ASSIGN llFinalPay = YES.
            END.
            ELSE
            DO:
               IF NOT CAN-FIND(FIRST FFItem WHERE 
                                     FFItem.FFnum  = FixedFee.FFnum AND
                                     FFItem.billed = FALSE AND
                                     FFItem.billperiod > YEAR(TODAY) * 100 + 
                                                         MONTH(TODAY)   
                                    NO-LOCK) THEN
                  NEXT. 
               ELSE
                  ASSIGN llFinalPay = NO.
            END.            
            PUT STREAM sOut UNFORMATTED STRING(Mobsub.CustNum) "|" 
                                        MobSub.Cli "|" 
                                        STRING(MobSub.MsSeq) "|"
                                        STRING(FixedFee.OrderId) "|" 
                                        FixedFee.Contract "|" 
                                        STRING(DCCli.ValidFrom) "|" 
                                        STRING(DCCli.ValidTo) "|" 
                                        STRING(llfinalPay) "|" 
                                        (IF DCCli.TermDate = ? THEN "" 
                                         ELSE STRING(DCCli.TermDate)) SKIP.
         END.
      END.                  
      FOR EACH DCCli WHERE
               DCCli.Brand      = "1" AND
               (IF lcContrList:SCREEN-VALUE <> ? THEN
               DCCli.DCEvent    = ENTRY(2,lcContrList:SCREEN-VALUE) 
               ELSE (DCCli.DCEvent BEGINS "PAYTERM" OR
                     DCCli.DCEvent BEGINS "RVTERM" )) AND
              (IF DATE(ldafromdate2:SCREEN-VALUE) <> ? THEN
               DCCli.ValidFrom  >= DATE(ldafromdate2:SCREEN-VALUE)
               ELSE TRUE) AND
              (IF DATE(ldatodate2:SCREEN-VALUE) <> ? THEN
               DCCli.ValidTo    <= DATE(ldatodate2:SCREEN-VALUE)
               ELSE TRUE ) AND
              (IF DATE(ldaTerminate2:SCREEN-VALUE) <> ? THEN
               DCCli.TermDate   = DATE(ldaTerminate2:SCREEN-VALUE)
               ELSE DCCli.TermDate = ?) NO-LOCK:
         FIND FIRST MobSub WHERE
                    MobSub.Brand = DCCli.Brand AND
                    Mobsub.MsSeq = DCCli.MsSeq NO-LOCK NO-ERROR.
         FIND FIRST FixedFee WHERE 
                    FixedFee.Brand = DCCli.Brand AND
                    FixedFee.Contract = STRING(DCCli.PerContractID) AND
                   ((IF lcFinanceStat2:SCREEN-VALUE <> "" THEN
                    FixedFee.FinancedResult = lcFinanceStat2:SCREEN-VALUE
                    ELSE TRUE)) NO-LOCK NO-ERROR.
                    
         IF AVAIL FixedFee THEN
         DO:
            IF llFinalPay2:SCREEN-VALUE = "yes" THEN
            DO:            
               IF CAN-FIND(FIRST FFItem WHERE 
                                 FFItem.FFnum  = FixedFee.FFnum AND
                                 FFItem.billed = FALSE AND
                                 FFItem.billperiod > YEAR(TODAY) * 100 + 
                                                     MONTH(TODAY)   
                                 NO-LOCK) THEN
                  NEXT.  
               ELSE
                  ASSIGN llFinalPay = YES.
            END.
            ELSE
            DO:
               IF NOT CAN-FIND(FIRST FFItem WHERE 
                                     FFItem.FFnum  = FixedFee.FFnum AND
                                     FFItem.billed = FALSE AND
                                     FFItem.billperiod > YEAR(TODAY) * 100 + 
                                                         MONTH(TODAY)   
                                    NO-LOCK) THEN
                  NEXT. 
               ELSE
                  ASSIGN llFinalPay = NO.
            END.            
            PUT STREAM sOut UNFORMATTED STRING(Mobsub.CustNum) "|" 
                                        MobSub.Cli "|" 
                                        STRING(MobSub.MsSeq) "|"
                                        STRING(FixedFee.OrderId) "|" 
                                        FixedFee.Contract "|" 
                                        STRING(DCCli.ValidFrom) "|" 
                                        STRING(DCCli.ValidTo) "|" 
                                        STRING(llfinalPay) "|" 
                                        (IF DCCli.TermDate = ? THEN "" 
                                         ELSE STRING(DCCli.TermDate)) SKIP.
         END.
      END.            
   END.
   OUTPUT STREAM sOut CLOSE.
   MESSAGE "Done...2"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

WAIT-FOR 'choose' OF quit-button.


