/*------------------------------------------------------------------------
  MODULE .......: rateplan.p
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: Subhash Sanjeevi
  CREATED ......: Wed Feb 11 16:01:32 EET 2015
  CHANGED ......:
  Version ......: Yoigo
  ----------------------------------------------------------------------*/
  
/* ***************************  Definitions  ************************** */
{Syst/commpaa.i}
katun = "Cron".
gcBrand = "1".
{Func/cparam2.i}
{Syst/eventlog.i}
{Func/ftransdir.i}
{../tms_support/utilities/newtariff/tariffconfig.i}
{../tms_support/utilities/newtariff/tariffcons.i}
 
DEFINE INPUT  PARAMETER icIncDir   AS CHARACTER NO-UNDO. 
DEFINE INPUT  PARAMETER icSpoolDir AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oPayType   AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER oRateplan  AS CHARACTER NO-UNDO.
 
DEFINE VARIABLE lcLine      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLogFile   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcInputFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE liFirstLine AS INTEGER   NO-UNDO INITIAL 1.

DEFINE TEMP-TABLE ttRatePlan NO-UNDO 
   FIELD Action    AS CHARACTER 
   FIELD RPSubType AS CHARACTER 
   FIELD ESubType  AS CHARACTER 
   FIELD PriceList AS CHARACTER.

DEFINE TEMP-TABLE ttTariff NO-UNDO 
   FIELD CCN      AS CHARACTER 
   FIELD BDest    AS CHARACTER 
   FIELD BillItem AS CHARACTER 
   FIELD Price    AS CHARACTER 
   FIELD SetupFee AS CHARACTER.

DEFINE BUFFER bRatePlan   FOR RatePlan.
DEFINE BUFFER bPListConf  FOR PListConf.
DEFINE BUFFER bPriceList  FOR PriceList.       
DEFINE BUFFER bTariff     FOR Tariff. 
DEFINE BUFFER b_PListConf FOR PListConf.      

DEFINE STREAM RPIn.
DEFINE STREAM RPLog.
DEFINE STREAM RPTIn.
DEFINE STREAM RPTLog.
DEFINE STREAM RPNIn.
DEFINE STREAM RPNlog.

/* ********************  Preprocessor Definitions  ******************** */
FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   icMessage = "ERROR:" + icMessage.
   
   PUT STREAM RPLog UNFORMATTED
      TODAY                   " | " 
      STRING(TIME,"HH:MM:SS") " | "
      icMessage SKIP.
      
END FUNCTION.

/* ***************************  Main Block  *************************** */
    
ASSIGN lcInputFile = icIncDir + "rateplan.txt"
       lcLogFile   = icSpoolDir + "rateplan.log".
      
INPUT STREAM RPIn FROM VALUE(lcInputFile).
OUTPUT STREAM RPLog TO VALUE(lcLogFile) APPEND.
                          
REPEAT:

   IMPORT STREAM RPIn UNFORMATTED lcLine.

   /* Ignore the first line - (Header) */
   IF liFirstLine = 1 THEN DO:
      liFirstLine = liFirstLine + 1.
      NEXT.
   END.
       
   CREATE ttRatePlan.
   ASSIGN    
      ttRatePlan.Action    = TRIM(ENTRY(1,lcLine,";")) 
      ttRatePlan.RPSubType = TRIM(ENTRY(2,lcLine,";"))
      ttRatePlan.ESubType  = TRIM(ENTRY(3,lcLine,";"))
      ttRatePlan.PriceList = TRIM(ENTRY(4,lcLine,";")) NO-ERROR.
      
   IF ERROR-STATUS:ERROR THEN DO: 
      fError("Wrong rateplan input data").
      RETURN "ERROR".
   END.      
   
END.

FIND FIRST ttRatePlan WHERE 
           ttRatePlan.Action EQ "NEW" NO-LOCK NO-ERROR.
           
IF AVAILABLE ttRatePlan THEN DO:
   TTRATEPLAN: 
   DO TRANSACTION:
      ASSIGN 
         lcLogFile   = icSpoolDir + "rptariff_new.log"
         lcInputFile = icIncDir + "rptariff_new.txt"
         liFirstLine = 1.
      
      IF SEARCH(lcInputFile) EQ ? THEN 
         LEAVE TTRATEPLAN.

      INPUT STREAM RPNIn FROM VALUE(lcInputFile).
      OUTPUT STREAM RPNLog TO VALUE(lcLogFile) APPEND.
      
      REPEAT:
         IMPORT STREAM RPNIn UNFORMATTED lcLine.

         /* Ignore the first line - (Header) */
         IF liFirstLine = 1 THEN DO:
            liFirstLine = liFirstLine + 1.
            NEXT.
         END.
          
         CREATE ttTariff.
         ASSIGN    
            ttTariff.CCN      = TRIM(ENTRY(1,lcLine,";")) 
            ttTariff.BDest    = TRIM(ENTRY(2,lcLine,";"))
            ttTariff.BillItem = TRIM(ENTRY(3,lcLine,";"))
            ttTariff.Price    = TRIM(ENTRY(4,lcLine,";"))
            ttTariff.SetupFee = TRIM(ENTRY(5,lcLine,";")) NO-ERROR.
         
         IF ERROR-STATUS:ERROR THEN DO: 
            fError("Wrong rateplan tariff input data").
            RETURN "ERROR".
         END.      
      END.
   END.
END.

ASSIGN 
   lcLogFile   = icSpoolDir + "rptariff_trans.log"
   lcInputFile = icIncDir + "rptariff_trans.txt"
   liFirstLine = 1.

INPUT STREAM RPTIn FROM VALUE(lcInputFile).
OUTPUT STREAM RPTLog TO VALUE(lcLogFile) APPEND.
                           
REPEAT:

   IMPORT STREAM RPTIn UNFORMATTED lcLine.
   
   /* Ignore the first line - (Header) */
   IF liFirstLine = 1 THEN DO:
      liFirstLine = liFirstLine + 1.
      NEXT.
   END.
    
   CREATE ttTrans.
   ASSIGN 
      ttTrans.tLangType  = TRIM(ENTRY(1,lcLine,";"))
      ttTrans.tLangint   = TRIM(ENTRY(2,lcLine,";"))
      ttTrans.tLangtext  = TRIM(ENTRY(3,lcLine,";")) 
      ttTrans.tLangTrans = TRIM(ENTRY(4,lcLine,";")) NO-ERROR.
   
   IF ERROR-STATUS:ERROR THEN DO:
      fError("Incorrect input data").
      RETURN "ERROR". 
   END.
   
END.
                
RUN pValidateFileData.

IF RETURN-VALUE <> "OK" THEN 
   RETURN "ERROR".

RUN pCreRatePlan.

IF RETURN-VALUE <> "OK" THEN 
   RETURN "ERROR".

OUTPUT STREAM RPLog  CLOSE.
OUTPUT STREAM RPTLog CLOSE.
OUTPUT STREAM RPNLog CLOSE.
INPUT  STREAM RPNIn  CLOSE.
INPUT  STREAM RPTIn  CLOSE.
INPUT  STREAM RPIn   CLOSE.

/* ***************************  Main End  *************************** */

PROCEDURE pValidateFileData:
   
   FIND FIRST ttRatePlan NO-LOCK NO-ERROR.
   
   IF AVAILABLE ttRatePlan THEN DO:
      IF ttRatePlan.Action EQ "" THEN DO:
         fError("No Action data available").
         RETURN "ERROR".
      END.
      
      IF ttRatePlan.RPSubType EQ "" THEN DO:
         fError("No Subcription type data available").
         RETURN "ERROR".
      END.
      
      IF ttRatePlan.ESubType EQ "" THEN DO:
         fError("No existing subscription type data available").
         RETURN "ERROR".
      END.
      
      IF ttRatePlan.PriceList EQ "" THEN DO:
         fError("No price list available").
         RETURN "ERROR".
      END.
                              
   END.
   ELSE DO:
      fError("No ttRateplan records available").
      RETURN "ERROR".
   END.        

   FOR EACH ttTariff NO-LOCK:
       IF ttTariff.CCN EQ "" THEN DO:
          fError("No CCN data available").
          RETURN "ERROR".
       END.        
       
       IF ttTariff.BillItem EQ "" THEN DO:
           fError("No BillingItem data available").
           RETURN "ERROR".
       END.    
   END.
   
   RETURN "OK".
        
END PROCEDURE.    

PROCEDURE pCreRatePlan:
DEFINE VARIABLE lcERPlan AS CHARACTER NO-UNDO.   
   
   FIND FIRST ttRatePlan NO-LOCK NO-ERROR.
   
   IF AVAILABLE ttRatePlan THEN DO:
      FIND FIRST CLIType WHERE 
                 CLIType.Brand   = gcBrand             AND 
                 CLIType.CLIType = ttRatePlan.ESubType NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CLIType THEN DO:
         fError("Exsisting Subcription-type rateplan doesn't exists").
         RETURN "ERROR".
      END.
                        
      CASE ttRatePlan.Action:
         WHEN "EXISTING" THEN DO:
            
            RUN pCreRPData NO-ERROR.

            IF RETURN-VALUE EQ "ERROR" THEN 
               RETURN RETURN-VALUE.

            FOR EACH PListConf WHERE 
                     PListConf.Brand    = gcBrand AND
                     PListConf.RatePlan = RatePlan.RatePlan NO-LOCK:
               CREATE bPListConf.
               BUFFER-COPY PListConf EXCEPT PListConf.RatePlan 
                                            PListConf.dFrom 
                                            PListConf.dTo
                                         TO bPListConf.
               ASSIGN bPListConf.RatePlan = REPLACE(ttRatePlan.RPSubType,"CONT","CONTRATO") 
                      bPListConf.dFrom    = TODAY 
                      bPListConf.dTo      = 12/31/49 NO-ERROR.
               
               IF ERROR-STATUS:ERROR THEN DO:
                  fError("Error creating PListConf").
                  RETURN "ERROR".    
               END.                 
            END.             
             
            ASSIGN oRateplan = CLIType.PricePlan
                   oPayType  = CLIType.PayType.
                   
         END.          
         WHEN "NEW" THEN DO:
                    
            RUN pCreRPData.
                                    
            FOR EACH PListConf WHERE 
                     PListConf.Brand    = gcBrand AND
                     PListConf.RatePlan = RatePlan.RatePlan NO-LOCK:
                 
               IF PListConf.RatePlan EQ PListConf.PriceList THEN DO:

                  FOR EACH Tariff WHERE 
                           Tariff.Brand     = gcBrand AND 
                           Tariff.PriceList = PListConf.PriceList NO-LOCK:
                     CREATE bTariff.
                     BUFFER-COPY Tariff EXCEPT Tariff.PriceList
                                               Tariff.ValidFrom
                                               Tariff.TariffNum
                                            TO bTariff.
                     ASSIGN bTariff.PriceList = REPLACE(ttRatePlan.RPSubType,"CONT","CONTRATO")
                            bTariff.ValidFrom = TODAY 
                            bTariff.TariffNum = NEXT-VALUE(Tariff) NO-ERROR. 

                     IF ERROR-STATUS:ERROR THEN DO:    
                        fError("Error creating Tariff").
                        RETURN "ERROR".
                     END.
                  END. 

                  FOR EACH PriceList WHERE 
                           PriceList.Brand     = gcBrand             AND 
                           PriceList.PriceList = PListConf.PriceList NO-LOCK:
                     CREATE bPriceList.
                     BUFFER-COPY PriceList EXCEPT PriceList.PriceList 
                                                  PriceList.PLName
                                               TO bPriceList.
                     ASSIGN bPriceList.PriceList = REPLACE(ttRatePlan.RPSubType,"CONT","CONTRATO")
                            bPriceList.PLName    = REPLACE(ttRatePlan.RPSubType,"CONT","Contrato") NO-ERROR.
                         
                     IF ERROR-STATUS:ERROR THEN DO:    
                        fError("Error creating PriceList").
                        RETURN "ERROR".
                     END.
                  END.

                  CREATE bPListConf.
                  BUFFER-COPY PListConf EXCEPT PListConf.PriceList
                                               PListConf.RatePlan 
                                               PListConf.dFrom 
                                               PListConf.dTo 
                                            TO bPListConf.
                  ASSIGN 
                     bPListConf.PriceList = REPLACE(ttRatePlan.RPSubType,"CONT","CONTRATO")
                     bPListConf.RatePlan  = REPLACE(ttRatePlan.RPSubType,"CONT","CONTRATO")
                     bPListConf.dFrom     = TODAY 
                     bPListConf.dTo       = 12/31/49 NO-ERROR.
                  
                  IF ERROR-STATUS:ERROR THEN DO:
                     fError("Error creating PListConf").
                     RETURN "ERROR".    
                  END.
                  
                  IF CAN-FIND(FIRST ttTariff NO-LOCK) THEN DO:
                     FOR EACH ttTariff NO-LOCK:
                        FIND Tariff WHERE 
                             Tariff.Brand     = gcBrand              AND 
                             Tariff.PriceList = ttRatePlan.RPSubType AND 
                             Tariff.CCN       = INT(ttTariff.CCN)    AND
                             Tariff.BDest     = ttTariff.BDest       AND
                             Tariff.ValidFrom <= TODAY               AND
                             Tariff.ValidTo   >= TODAY              
                        EXCLUSIVE-LOCK NO-ERROR.
                                                    
                        IF AVAILABLE Tariff THEN DO:
                           ASSIGN 
                              Tariff.BillCode    = ttTariff.BillItem
                              Tariff.PriceList   = REPLACE(ttRatePlan.RPSubType,"CONT","CONTRATO")
                              Tariff.Price       = DECIMAL(ttTariff.Price)
                              Tariff.StartCharge = DECIMAL(ttTariff.SetupFee) NO-ERROR.
                              
                           IF ERROR-STATUS:ERROR THEN DO:
                              fError("Error updating Tariff").
                              RETURN "ERROR".
                           END.   
                        END.
                        ELSE DO:
                           CREATE Tariff.
                           ASSIGN 
                              Tariff.Brand       = gcBrand
                              Tariff.TariffNum   = NEXT-VALUE(Tariff)
                              Tariff.PriceList   = REPLACE(ttRatePlan.RPSubType,"CONT","CONTRATO")
                              Tariff.CCN         = INT(ttTariff.CCN)
                              Tariff.BDest       = ttTariff.BDest
                              Tariff.BillCode    = ttTariff.BillItem
                              Tariff.Price       = DECIMAL(ttTariff.Price)
                              Tariff.StartCharge = DECIMAL(ttTariff.SetupFee)
                              Tariff.ValidFrom   = TODAY 
                              Tariff.Validto     = 12/31/49 NO-ERROR.
                           
                           IF ERROR-STATUS:ERROR THEN DO:
                              fError("Error creating Tariff").
                              RETURN "ERROR".
                           END.
                        END.  /* ELSE DO: */  
                     END. /* FOR EACH ttTariff */           
                  END.  /* IF CAN-FIND(FIRST */
                  
               END. /* IF PListConf.RatePlan EQ PListConf.PriceList */
               ELSE DO:        
                  CREATE bPListConf.
                  BUFFER-COPY PListConf EXCEPT PListConf.RatePlan 
                                               PListConf.dFrom 
                                               PListConf.dTo 
                                            TO bPListConf.
                  ASSIGN 
                     bPListConf.RatePlan = REPLACE(ttRatePlan.RPSubType,"CONT","CONTRATO")
                     bPListConf.dFrom    = TODAY 
                     bPListConf.dTo      = 12/31/49 NO-ERROR.
               END.
               
               IF ERROR-STATUS:ERROR THEN DO:
                   fError("Error creating PListConf").
                   RETURN "ERROR".
               END.                                                    
            
            END.  /* FOR EACH PListConf */  
         END.  /* WHEN "NEW" */       
      END CASE.
   END.
   
   RETURN "OK".
           
END PROCEDURE.    

PROCEDURE pCreRPData:
   
   FIND FIRST RatePlan WHERE
              RatePlan.Brand    = gcBrand AND
              RatePlan.RatePlan = TRIM(CLIType.PricePlan) NO-LOCK NO-ERROR.
   IF NOT AVAILABLE RatePlan THEN DO:           
      fError("Rateplan doesn't exists").
      RETURN "ERROR".
   END. 
    
   CREATE bRatePlan.
   BUFFER-COPY RatePlan EXCEPT Rateplan.RatePlan 
                               Rateplan.RPName  
                            TO bRatePlan.
   ASSIGN bRatePlan.RatePlan = REPLACE(ttRatePlan.RPSubType,"CONT","CONTRATO") 
          bRateplan.RPName   = REPLACE(ttRatePlan.RPSubType,"CONT","Contrato ") + " (Post paid)"  
          oRatePlan          = ttRatePlan.RPSubType 
          oPayType           = CLIType.PayType     NO-ERROR.
    
   IF ERROR-STATUS:ERROR THEN DO:
      fError("Error creating Rateplan").
      RETURN "ERROR".
   END.
    
   RUN pCreTranslations NO-ERROR.
   
   IF ERROR-STATUS:ERROR THEN DO:
      fError("Error creating Rateplan translations").
      RETURN "ERROR".
   END.
                         
END PROCEDURE.    

PROCEDURE pCreTranslations:

   FOR EACH ttTrans NO-LOCK:                 
      IF CAN-FIND(FIRST RatePlan WHERE
                        RatePlan.Brand    = gcBrand            AND 
                        RatePlan.RatePlan = ttTrans.tLangType) THEN DO:

         IF CAN-FIND(FIRST RepText WHERE
                           RepText.Brand    = gcBrand                    AND
                           RepText.LinkCode = ttTrans.tLangType          AND
                           RepText.Language = INTEGER(ttTrans.tLangint)) THEN
            NEXT.
            
         CREATE RepText.
         ASSIGN 
            RepText.Brand    = gcBrand    
            RepText.TextType = 11               /* Default value */       
            RepText.LinkCode = ttTrans.tLangType        
            RepText.Language = INTEGER(ttTrans.tLangint)    
            RepText.FromDate = TODAY     
            RepText.ToDate   = 12/31/49       
            RepText.RepText  = ttTrans.tLangTrans  NO-ERROR.
            
         IF ERROR-STATUS:ERROR THEN DO:
            fError("Creating translations for BillItem").
            RETURN "ERROR".
         END.
      END.
      ELSE DO:
         fError("Rateplan doesn't exists").
         RETURN "ERROR".
      END.      
   END.
   
   RETURN "OK".
        
END PROCEDURE.


