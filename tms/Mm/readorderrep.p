/* ----------------------------------------------------------------------
  MODULE .......: readorderrep.p
  TASK .........: Print a report from orders which were read from files
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 08.11.07
  CHANGED ......: 
  Version ......: yoigo
---------------------------------------------------------------------- */


{commali.i}
{timestamp.i}

DEF INPUT  PARAMETER idtDate      AS DATE NO-UNDO.
DEF INPUT  PARAMETER icChannel    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icSalesman   AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icFile       AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiOrderCount AS INT  NO-UNDO.

DEF VAR ldReportDay   AS DEC  NO-UNDO EXTENT 2.
DEF VAR lcSep         AS CHAR NO-UNDO INIT "|".

DEF STREAM sWrite.


OUTPUT STREAM sWrite TO VALUE(icFile).

ASSIGN 
   ldReportDay[1] = fMake2DT(idtDate,0)
   ldReportDay[2] = fMake2DT(idtDate,86399).


/* report all gift orders created on given day */
FOR EACH Order NO-LOCK USE-INDEX Stamp WHERE
         Order.Brand        = gcBrand        AND
         Order.CrStamp     >= ldReportDay[1] AND
         Order.CrStamp     <= ldReportDay[2] AND
         Order.OrderChannel = icChannel      AND
         Order.Salesman     = icSalesman,
   FIRST OrderCustomer OF Order NO-LOCK WHERE
         OrderCustomer.RowType = 1
BY Order.CrStamp
BY Order.OrderID:
         
   FIND FIRST MsRequest WHERE
              MsRequest.MsSeq   = Order.MsSeq  AND
              MsRequest.ReqType = 13 NO-LOCK NO-ERROR.
              
   FIND SIM WHERE SIM.ICC = Order.ICC NO-LOCK NO-ERROR.
   
   PUT STREAM sWrite UNFORMATTED
      STRING(Order.CrStamp,"99999999.99999") lcSep
      Order.CLI             lcSep
      OrderCustomer.CustID  lcSep
      Order.OrderID         lcSep
      Order.MsSeq           lcSep.
      
   IF AVAILABLE MsRequest THEN PUT STREAM sWrite UNFORMATTED
      MsRequest.MsRequest   lcSep
      MsRequest.ReqStat     lcSep.
      
   ELSE PUT STREAM sWrite UNFORMATTED
      lcSep
      lcSep.
      
   IF AVAILABLE SIM THEN PUT STREAM sWrite UNFORMATTED
      SIM.SIMStat           lcSep.
   ELSE PUT STREAM sWrite UNFORMATTED
      lcSep.
      
   PUT STREAM sWrite UNFORMATTED
      Order.StatusCode     SKIP.

   oiOrderCount = oiOrderCount + 1. 
END.         

OUTPUT STREAM sWrite CLOSE.


