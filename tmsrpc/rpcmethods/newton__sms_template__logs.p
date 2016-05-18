/** 
 * RPC to see system logs of certain SMS.
 *
 * @input empty;

 * @output      loglist;array;array of structs
   @loglist     usercode;string;
                keyvalue;string;
                language;int;
                timestamp;string;
*/
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
gcBrand = "1".
{Func/timestamp.i}

DEFINE VARIABLE resp_array AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcResultStruct AS CHAR NO-UNDO. 
DEFINE VARIABLE lcKeyValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcmodday AS CHAR NO-UNDO. 
DEFINE VARIABLE ldeTimeStamp AS DEC NO-UNDO.

resp_array = add_array(response_toplevel_id, "").

FOR EACH InvText NO-LOCK WHERE
         InvText.Brand = gcBrand AND
         InvText.Target = "SMS" AND
         InvText.ToDate >= TODAY AND
         InvText.FromDate <= TODAY:

   lcmodday = STRING(year(InvText.FromDate),"9999") + "/" + 
   STRING(month(InvText.FromDate),"99") + "/" + STRING(day(InvText.FromDate),"99").

   lcKeyValue = InvText.Brand + chr(255) + InvText.Target + chr(255) + 
   InvText.KeyValue + chr(255) + lcmodday.
   
   FOR EACH EventLog NO-LOCK WHERE
            EventLog.TableName = "InvText" AND
            EventLog.EventDate >= 11/01/12 AND
            EventLog.Key = lcKeyValue:
      lcResultStruct = add_struct(resp_array,"").
      add_string(lcResultStruct, "usercode", EventLog.UserCode).
      add_string(lcResultStruct, "keyvalue", InvText.KeyValue).
      add_int(lcResultStruct, "language", InvText.Language).
      ldeTimeStamp = fHMS2TS(EventLog.EventDate, EventLog.EventTime).
      add_timestamp(lcResultStruct, "timestamp", ldeTimeStamp).
   END.
   FOR EACH RepText NO-LOCK WHERE
            RepText.Brand     = gcBrand AND
            RepText.LinkCode  = STRING(InvText.ITNum) AND
            RepText.TextType  = 32 AND
            RepText.ToDate   >= TODAY AND
            RepText.FromDate <= TODAY:
      lcmodday = STRING(year(RepText.ToDate),"9999") + "/" + 
      STRING(month(RepText.ToDate),"99") + "/" + STRING(day(RepText.ToDate),"99").

      lcKeyValue = RepText.Brand + chr(255) + STRING(RepText.TextType) + 
      chr(255) + RepText.Linkcode + chr(255) + STRING(RepText.Language) + 
      chr(255) + lcmodday.
   
      FOR EACH EventLog NO-LOCK WHERE
               EventLog.TableName = "RepText" AND
               EventLog.EventDate >= 11/01/12 AND
               EventLog.Key = lcKeyValue:
      lcResultStruct = add_struct(resp_array,"").
      add_string(lcResultStruct, "usercode", EventLog.UserCode).
      add_string(lcResultStruct, "keyvalue", InvText.KeyValue).
      add_int(lcResultStruct, "language", RepText.Language).
      ldeTimeStamp = fHMS2TS(EventLog.EventDate, EventLog.EventTime).
      add_timestamp(lcResultStruct, "timestamp", ldeTimeStamp).
      END.
   END.
END.


FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.

