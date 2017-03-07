/* DUMP FROM ALL orders: 
jl 

*/

{Syst/commpaa.i}
{Func/date.i}
{Func/cparam2.i}

ASSIGN
   katun   = "cron"
   gcBrand = "1".

DEF STREAM sLog.
DEF VAR lcNPStatName AS CHAR NO-UNDO. 
DEF VAR lcFileName   AS CHAR NO-UNDO.
DEF VAR lcOutDir     AS CHAR NO-UNDO.
DEF VAR lcSpoolDir   AS CHAR NO-UNDO.


ASSIGN
   lcSpoolDir = fCParam("DUMPSPOOL","orderchkrep.p")
   lcOutDir   = fCParam("DUMPOUTGOING","orderchkrep.p")
   lcFileName = "CHECKLIST" + fdatefmt(TODAY,"yyyymmdd") + ".dump".

OUTPUT STREAM sLog TO VALUE(lcSpoolDir + lcfilename).

FOR EACH order NO-LOCK:

   FIND FIRST msrequest NO-LOCK WHERE  
              msrequest.msseq = order.msseq and
              msrequest.reqtype = 13 
   No-Error.

   FIND FIRST mobsub NO-LOCK WHERE
              mobsub.msseq = order.msseq NO-ERROR.
              
   FIND FIRST TMSCodes WHERE 
              TMSCodes.TableName = "MNPProcess" AND
              TMSCodes.FieldName = "StatusCode" AND
              TMSCodes.CodeGroup = "MNP" AND
              TMSCodes.CodeValue = STRING(Order.MNPStatus - 1)
   NO-LOCK NO-ERROR.

   IF AVAIL TMSCodes THEN lcNPStatName = TMSCodes.CodeName.
   ELSE lcNPStatName = "".

   FIND FIRST MNPSub NO-LOCK WHERE
              MNPSub.msseq = order.msseq NO-ERROR.

   IF AVAIL MNPSub THEN 
   FIND FIRST MNPProcess NO-LOCK WHERE 
              MNPProcess.mnpseq = MNPSub.mnpseq NO-ERROR.

   FIND FIRST sim NO-LOCK WHERE  
              sim.icc = order.icc NO-ERROR.
              
   FIND FIRST ordercustomer NO-LOCK WHERE
              OrderCustomer.Brand = "1" AND
              ordercustomer.orderid = order.orderid NO-ERROR.

   PUT STREAM sLog UNFORMATTED 
      order.orderid CHR(9)
      order.statuscode CHR(9)
      order.crstamp CHR(9)
      order.cli CHR(9)
      order.ICC CHR(9)
      order.Clitype CHR(9)
      Order.msseq CHR(9)
      lcNPStatName CHR(9)
      Order.OrderChannel CHR(9).
                      
      IF AVAIL msrequest THEN
      PUT STREAM sLog UNFORMATTED
      msrequest.actstamp CHR(9)
      msrequest.reqstatus CHR(9).
      
      Else
      PUT STREAM sLog UNFORMATTED
      "no actstamp"  CHR(9)
      "no reqstatus" CHR(9).

      IF AVAIL mobsub THEN
         PUT STREAM sLog UNFORMATTED 
            mobsub.msstatus CHR(9).
      ELSE
         PUT STREAM sLog UNFORMATTED
                 "no mobsub" CHR(9).
      IF AVAIL MNPProcess THEN
         PUT STREAM sLog UNFORMATTED 
            MNPProcess.FormRequest CHR(9)
            MNPProcess.PortRequest CHR(9).
      ELSE 
         PUT STREAM sLog UNFORMATTED 
            " " CHR(9) " " CHR(9).
      
      IF AVAIL sim THEN 
         PUT STREAM sLog UNFORMATTED 
            sim.simstat CHR(9).
      ELSE
         PUT STREAM sLog UNFORMATTED   
            "no sim" CHR(9).
      
      IF AVAIL ordercustomer THEN
         PUT STREAM sLog UNFORMATTED
            ordercustomer.custnum CHR(9)
            ordercustomer.firstname CHR(9)
            ordercustomer.surname1 CHR(9)
            ordercustomer.surname2 CHR(9)
            ordercustomer.CustIdType CHR(9)
            ordercustomer.CustId CHR(9)
            ordercustomer.email CHR(9)
            ordercustomer.MobileNumber CHR(9)
            ordercustomer.FixedNumber CHR(9)
            ordercustomer.address CHR(9)
            ordercustomer.zipCode CHR(9)
            ordercustomer.PostOffice CHR(9)
            ordercustomer.region CHR(9)
            ordercustomer.country CHR(9).            
            
      PUT STREAM sLog UNFORMATTED SKIP.      
END. 

unix silent value("mv " + lcSpoolDir + lcFilename + " " + lcOutDir).

OUTPUT STREAM sLog CLOSE.
