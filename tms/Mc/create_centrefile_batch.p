/*--------------------------------------------------------------------
  MODULE .......: create_centrefile_batch.p
  TASK .........: Create centre file each hour
  APPLICATION ..: tms
  AUTHOR .......: kaaikas
  CREATED ......: 22.09.16
  Version ......: yoigo
---------------------------------------------------------------------- */

{commpaa.i}
katun = "Cron".
gcBrand = "1".
{tmsconst.i}
{ftransdir.i}
{timestamp.i}
{cparam2.i}

DEF STREAM sLogFile.
DEF STREAM sOutFile.

DEF VAR lcStatus        AS CHAR NO-UNDO.
DEF VAR ldCurrentTime   AS DEC  NO-UNDO.
DEF VAR lcSep         AS CHAR NO-UNDO.
DEF VAR lcSpoolDir        AS CHAR NO-UNDO.
DEF VAR lcOutDir          AS CHAR NO-UNDO.
DEF VAR lcDate             AS CHAR NO-UNDO.
DEF VAR lcRootDir          AS CHAR NO-UNDO.
DEF VAR lcProcDir          AS CHAR NO-UNDO.
DEF VAR lcFile             AS CHAR NO-UNDO.
DEF VAR lcLogFile          AS CHAR NO-UNDO.
DEF VAR lcTime             AS CHAR NO-UNDO.

ASSIGN
       lcDate      = STRING(YEAR(TODAY)) +
                      STRING(MONTH(TODAY),"99") +
                      STRING(DAY(TODAY),"99")
       lcTime      = REPLACE(STRING(TIME,"HH:MM:SS"),":","")
       lcRootDir   = fCParam("MasMovil","RootDir")
       lcSpoolDir  = lcRootDir + "/outgoing/spool/"
       lcOutDir    = lcRootDir + "/outgoing/outgoing/"
       lcProcDir   = lcRootDir + "/outgoing/processed/"
       lcFile      = lcSpoolDir + "CENTRE-" +
                     lcDate + "-" +
                     lcTime.
       lcLogFile   = lcSpoolDir + "Log-CENTRE-" +
                     lcDate + "-" +
                     lcTime + ".txt".
       lcSep = ";;".


/*Functions:*/

FUNCTION fLogLine RETURNS LOGICAL
   (icLine AS CHAR,
   icMessage AS CHAR):
   PUT STREAM sLogFile UNFORMATTED
      icLine ";;"
      icMessage ";;"
      "TMS" SKIP.
END FUNCTION.

FUNCTION fCreateCentreFileRow RETURNS CHAR
   ():

   DEF VAR lcCentreFileRow AS CHAR NO-UNDO.
   DEF VAR lcdeliverydate AS CHAR NO-UNDO.

   FIND FIRST Order NO-LOCK WHERE
              Order.Brand EQ gcBrand AND
              Order.OrderID EQ fusionMessage.OrderId NO-ERROR.
   IF NOT AVAIL Order THEN
      RETURN "Order not available" + STRING(fusionMessage.OrderId).

   FIND FIRST OrderFusion NO-LOCK WHERE
              OrderFusion.Brand EQ gcBrand AND
              OrderFusion.OrderId EQ fusionMessage.OrderId NO-ERROR.
   IF NOT AVAIL OrderFusion THEN
      RETURN "OrderFusion not available" + STRING(fusionMessage.OrderId).
   IF LOOKUP(Order.OrderChannel,"pos") > 0 THEN
      lcdeliverydate = STRING(OrderFusion.OrderDate).
   ELSE
       lcdeliverydate = lcDate + 
                        SUBSTRING(lcTime,1,2) + "24" + substring(lcTime,3).

   lcCentreFileRow =
   "CENTRE"                        + lcSep +
   /*SAL_Order_ID*/ 
   FusionMessage.AdditionalInfo    + lcSep +
   /*Line Number (static 1) */
   "1"                             + lcSep +
   /*Service code*/
   "ROUTER_ADSL"                   + lcSep +
   /*Product model*/
   "Router001"                     + lcSep +
   /*Qvantity*/
   "1"                             + lcSep +
   /*router Serial number*/
   OrderFusion.SerialNumber        + lcSep +
   /*Delivery date/time */
   lcdeliverydate                  + lcSep +
   /*Status*/
   "Entregado"                     + lcSep +
   /*Pendiente Definir*/
   ""                              + lcSep +
   /*campo auxiliar String1*/
   ""                              + lcSep +
   /*campo auxiliar String2*/
   ""                              + lcSep +
   /*campo auxiliar String3*/
   ""                              + lcSep +
   /*campo auxiliar Entero1*/
   ""                              + lcSep +
   /*campo auxiliar Entero2*/
   ""                              + lcSep +
   /*campo auxiliar Entero3*/
   ""                              + lcSep +
   /*campo auxiliar Flag1*/
   ""                              + lcSep +
   /*campo auxiliar Flag2*/
   ""                              + lcSep +
   /*campo auxiliar Flag3*/   
   "" .


   OUTPUT STREAM sOutFile to VALUE(lcFile) APPEND.
   PUT STREAM sOutFile UNFORMATTED lcCentreFileRow SKIP.
   OUTPUT STREAM sOutFile CLOSE.

   fLogLine(lcCentreFileRow, "").
   RETURN "".
END.

/*Main functionality*/
OUTPUT STREAM sLogFile TO VALUE(lcLogFile) APPEND.

ldCurrentTime = fMakeTS().

fLogLine("","Centre file creation start " + fTS2HMS(ldCurrentTime)).

FOR EACH FusionMessage WHERE 
         FusionMessage.source EQ "MasMovil" AND
         FusionMessage.messagestatus EQ {&FUSIONMESSAGE_STATUS_ONGOING} AND
         FusionMessage.messagetype EQ "Logistics":
   lcStatus = fCreateCentreFileRow().
   IF lcStatus EQ "" THEN
      FusionMessage.messagestatus = {&FUSIONMESSAGE_STATUS_HANDLED}.
END.

ldCurrentTime = fMakeTS().
fLogLine("","Centre file creation end " + fTS2HMS(ldCurrentTime)).
OUTPUT STREAM sLogFile CLOSE.

fMove2TransDir(lcFile, "", lcOutDir).
fMove2TransDir(lcLogFile, "", lcProcDir).
