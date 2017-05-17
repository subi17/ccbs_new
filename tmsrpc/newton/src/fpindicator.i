

FUNCTION fGetPIndicatorHistory RETURNS LOGICAL
   (pcHostTable AS CHAR,
    pcKeyValue AS CHAR,
    piIndicatorType AS INT,
    pcResultArray AS CHAR) :

   DEFINE VARIABLE lcStruct AS CHARACTER NO-UNDO. 
   FOR EACH PIndicator NO-LOCK WHERE
            PIndicator.Brand = gcBrand AND
            PIndicator.HostTable = pcHostTable AND
            PIndicator.KeyValue = pcKeyValue AND
            PIndicator.IndicatorType = piIndicatorType USE-INDEX HostTable :
     
     lcStruct = add_struct(pcResultArray,"").
     add_string(lcStruct,"value",PIndicator.IndicatorValue).
     add_timestamp(lcStruct,"updated_at",PIndicator.TimeStamp).
     IF PIndicator.Memo NE "" THEN 
     add_string(lcStruct,"additional_info",PIndicator.Memo).

   END.

   RETURN TRUE.

END FUNCTION.
