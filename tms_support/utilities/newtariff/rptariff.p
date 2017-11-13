/*------------------------------------------------------------------------
  MODULE .......: rptariff.p
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: 
  CREATED ......: 13.11.2017
  CHANGED ......:
  Version ......: Yoigo
  ----------------------------------------------------------------------*/  

/* ***************************  Definitions  ************************** */

/* Parameters */
DEFINE INPUT PARAMETER icBaseFile AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER icFile     AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER icSpoolDir AS CHARACTER NO-UNDO.

/* Temp tables */
DEFINE TEMP-TABLE ttTariff NO-UNDO 
   FIELD PriceList AS CHARACTER 
   FIELD CCN       AS CHARACTER 
   FIELD BDest     AS CHARACTER 
   FIELD BillItem  AS CHARACTER 
   FIELD PriceUnit AS CHARACTER
   FIELD Price     AS CHARACTER 
   FIELD SetupFee  AS CHARACTER.
 
/* Variables */
DEFINE VARIABLE lcLine    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLogFile AS CHARACTER NO-UNDO.

/* Streams */
DEFINE STREAM strin.
DEFINE STREAM RPLog.

/* ********************  Functions And Procedures ******************** */

FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   icMessage = "ERROR:" + icMessage.
   
   PUT STREAM RTLog UNFORMATTED
      TODAY                   " | " 
      STRING(TIME,"HH:MM:SS") " | "
      icMessage SKIP.
      
END FUNCTION.


PROCEDURE pCreTariff:

   FOR EACH ttTariff NO-LOCK:
      FIND FIRST Tariff WHERE Tariff.Brand      EQ Syst.Var:gcBrand   AND 
                              Tariff.PriceList  EQ ttTariff.PriceList AND 
                              Tariff.CCN        EQ INT(ttTariff.CCN)  AND 
                              Tariff.BDest      EQ ttTariff.BDest     AND 
                              Tariff.ValidFrom <= TODAY               AND 
                              Tariff.ValidTo   >= TODAY
                              NO-LOCK NO-ERROR.                                    
      IF NOT AVAIL Tariff THEN DO:
         CREATE Tariff.
         ASSIGN 
            Tariff.Brand          = Syst.Var:gcBrand
            Tariff.TariffNum      = NEXT-VALUE(Tariff)
            Tariff.PriceList      = ttTariff.PriceList
            Tariff.CCN            = INT(ttTariff.CCN)
            Tariff.BDest          = ttTariff.BDest
            Tariff.BillCode       = ttTariff.BillItem
            Tariff.DataType       = INT(ttTariff.PriceUnit)
            Tariff.Discount[4]    = Yes
            Tariff.TZName[1]      = "Off Peak"
            Tariff.DayType[1]     = 1
            Tariff.TZFrom[1]      = "0000" 
            Tariff.TZTo[1]        = "2400"
            Tariff.Price[1]       = DECIMAL(ttTariff.Price)
            Tariff.StartCharge[1] = DECIMAL(ttTariff.SetupFee)
            Tariff.TZName[2]      = "Peak"
            Tariff.DayType[2]     = 0
            Tariff.TZFrom[2]      = "0000" 
            Tariff.TZTo[2]        = "0000"
            Tariff.Price[2]       = 0
            Tariff.StartCharge[2] = 0
            Tariff.TZName[3]      = "Off Peak"
            Tariff.DayType[3]     = 0
            Tariff.TZFrom[3]      = "0000" 
            Tariff.TZTo[3]        = "0000"
            Tariff.Price[3]       = 0
            Tariff.StartCharge[3] = 0
            Tariff.ValidFrom      = TODAY 
            Tariff.Validto        = DATE(12,31,2049) NO-ERROR.

         IF ERROR-STATUS:ERROR THEN DO:
            fError("Creating Tariff from rptariff").
            RETURN "ERROR".
         END.
      END.            
  END.
         
  RETURN "OK".

END PROCEDURE.

 
/* ***************************  Main Block  *************************** */

lcLogFile = icSpoolDir + icBaseFile + ".log".

OUTPUT STREAM RPlog TO   VALUE(lcLogFile) APPEND.     
INPUT  STREAM strin FROM VALUE(icFile).

IMPORT STREAM strin UNFORMATTED lcLine. /* Reads first line (Header). Ignore it */                          
    
REPEAT: 
   IMPORT STREAM strin UNFORMATTED lcLine.  

   IF TRIM(lcLine) EQ "" OR NUM-ENTRIES(lcLine, ";") <> 7 THEN
      NEXT.
   CREATE ttTariff.
   ASSIGN    
      ttTariff.PriceList = TRIM(ENTRY(1,lcLine,";")) 
      ttTariff.CCN       = TRIM(ENTRY(2,lcLine,";")) 
      ttTariff.BDest     = TRIM(ENTRY(3,lcLine,";"))
      ttTariff.BillItem  = TRIM(ENTRY(4,lcLine,";"))
      ttTariff.PriceUnit = TRIM(ENTRY(5,lcLine,";"))
      ttTariff.Price     = TRIM(ENTRY(6,lcLine,";"))
      ttTariff.SetupFee  = TRIM(ENTRY(7,lcLine,";")) NO-ERROR.          
   END.

   IF ERROR-STATUS:ERROR THEN DO:
      fError("Incorrect input rate plan tariff").
      RETURN "ERROR". 
   END.
END.
   
RUN pCreTariff.

IF RETURN-VALUE <> "OK" THEN 
   RETURN RETURN-VALUE.   

RETURN "OK".

FINALLY: 
   INPUT STREAM strin  CLOSE.
   OUTPUT STREAM RTLog CLOSE.
END FINALLY.

/* ************************* Main Block ends  ************************* */









