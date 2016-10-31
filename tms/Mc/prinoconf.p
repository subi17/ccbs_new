/* ----------------------------------------------------------------------------
  MODULI .......: PRINOCONF.P
  TEHTAVA ......: print order confirmation
  SOVELLUS .....: TMS
  TEKIJA .......: aam
  LUONTIPVM ....: 01.12.06
  MUUTOSPVM ....: 14.12.06/aam use language 1 if text otherwise not available,
                               mark printed timestamp
                  14.03.07/aam check '"' and '|' on email address             
                  25.05.07/aam more checks on email
  VERSIO .......: yoigo
---------------------------------------------------------------------------- */

{commali.i}
{timestamp.i}
{utumaa.i new }
{feplstart.i}
{cparam2.i}
{edefine.i NEW}
{forderstamp.i}
{tmsconst.i}

DEF INPUT  PARAMETER iiOrderID AS INT  NO-UNDO.

DEF VAR liTextID     AS INT  NO-UNDO.
DEF VAR ldtDate      AS DATE NO-UNDO.
DEF VAR liTime       AS INT  NO-UNDO. 
DEF VAR liLanguage   AS INT  NO-UNDO. 
DEF VAR lcErrFile    AS CHAR NO-UNDO.
DEF VAR liCount      AS INT  NO-UNDO.
DEF VAR lcCheck      AS CHAR NO-UNDO.
DEF VAR lcEmailKey   AS CHAR NO-UNDO.
DEF VAR lcRootDir    AS CHAR NO-UNDO.
DEF VAR lcEmailFile  AS CHAR NO-UNDO.

FUNCTION fTxtSendLog RETURNS LOGIC
   (iiSendMethod AS INT):

   /* mark text as sent */
   CREATE ITSendLog.
   ASSIGN ITSendLog.Brand      = gcBrand
          ITSendLog.TxtType    = 1         /* 1=invtext */
          ITSendLog.ITNum      = liTextID  
          ITSendLog.CustNum    = iiOrderID
          ITSendLog.InvNum     = IF AVAILABLE Order
                                 THEN Order.OrderID
                                 ELSE 0
          ITSendLog.SendMethod = 4
          ITSendLog.EMail      = IF AVAILABLE OrderCustomer 
                                    THEN OrderCustomer.Email
                                 ELSE "foo@bar.fi"
          ITSendLog.RepType    = IF AVAILABLE Order THEN "ITOrd"
                                 ELSE "IT"
          ITSendLog.UserCode   = katun
          ITSendLog.SendStamp  = fMakeTS().
END.

FIND Order WHERE
     Order.Brand   = gcBrand  AND
     Order.OrderID = iiOrderID NO-LOCK NO-ERROR.
IF NOT AVAILABLE Order THEN RETURN "ERROR:Order not available".

IF Order.OrderType NE {&ORDER_TYPE_NEW} AND
   Order.OrderType NE {&ORDER_TYPE_MNP} AND
   Order.OrderType NE {&ORDER_TYPE_RENEWAL} THEN RETURN "".

IF LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES} + ",4") > 0 THEN 
   RETURN "INFO:Status of order doesn't allow printing".

IF LOOKUP(Order.OrderChannel,"pos,vip,gift,yoigo,renewal_pos,renewal_pos_stc,fusion_pos") > 0 THEN 
   RETURN "INFO:" + Order.OrderChannel + " order".

IF Order.CLI = "" THEN RETURN "ERROR:MSISDN not defined".

fSplitTS(Order.CrStamp,
         OUTPUT ldtDate,
         OUTPUT liTime).

FOR FIRST OrderCustomer OF Order NO-LOCK WHERE
          OrderCustomer.RowType = 1:

   liLanguage = INTEGER(OrderCustomer.Language) NO-ERROR.
   lcEmailKey = (IF LOOKUP(OrderCustomer.CustIdType, "CIF,CFraud,CInternal") > 0 THEN "EmailConfCIF"
              ELSE "EmailConf").

   IF Order.OrderType EQ 1 THEN 
      lcEmailKey = lcEmailKey + "MNP".

   IF Order.OrderType EQ 2 THEN
      lcEmailKey = "RenewalConf" + 
            (IF LOOKUP(OrderCustomer.CustIdType, "CIF,CFraud,CInternal") > 0 THEN "CIF" ELSE "").
END.

IF liLanguage = 0 OR 
   NOT CAN-FIND(Language WHERE Language.Language = liLanguage)
THEN RETURN "ERROR:Unknown language".

IF INDEX(OrderCustomer.EMail,"@") = 0 OR
   SUBSTRING(OrderCustomer.EMail,1,1) = "@" OR
   SUBSTRING(OrderCustomer.EMail,LENGTH(OrderCustomer.EMail),1) = "@" OR
   INDEX(OrderCustomer.EMail,"|") > 0 OR
   INDEX(OrderCustomer.EMail,",") > 0 OR
   NUM-ENTRIES(OrderCustomer.EMail,"@") NE 2
THEN RETURN "ERROR:Invalid email address".

DO liCount = 1 TO LENGTH(OrderCustomer.EMail):
   lcCheck = SUBSTRING(OrderCustomer.EMail,liCount,1).
   
   IF ASC(lcCheck) <= 42 THEN RETURN "ERROR:Invalid email address".
END.

DO WHILE TRUE:

   FOR FIRST InvText NO-LOCK WHERE 
             InvText.Brand     = gcBrand     AND
             InvText.Target    = "OrderConf" AND
             InvText.KeyValue  = lcEmailKey  AND
             InvText.FromDate <= ldtDate     AND
             InvText.ToDate   >= ldtDate     AND
             InvText.Language  = liLanguage:
      liTextID = InvText.ITNum.
   END.            

   /* use spanish if text not available for another language */ 
   IF liTextID = 0 AND liLanguage NE 1 THEN DO:
      liLanguage = 1.
      NEXT.
   END.
   
   LEAVE.
END.

IF liTextID = 0 THEN RETURN "ERROR:Text not available".

IF CAN-FIND(FIRST ITSendLog USE-INDEX InvNum WHERE 
                  ITSendLog.InvNum  = Order.OrderID AND
                  ITSendLog.RepType = "ITOrd"       AND
                  (ITSendLog.ITNum   = liTextID OR
                   ITSendLog.ITNum   = -10)) /*temp value for new html email */
THEN RETURN "INFO:Already printed".

IF Order.OrderType < 2 THEN /* New or MNP */
   RUN sendorderconf.p(Order.OrderId, OrderCustomer.email, OUTPUT lcErrFile).
ELSE /* Renewal, Rollback  and STC will be changed HTML in later YDR.
        These will need own HTML templates. */
   RUN printxt.p (iiOrderID,
                  0,
                  Order.CLI,
                  1,                      /* 1=invtext */
                  6,                      /* order */
                  "",
                  "",
                  liTextID,
                  6,                      /* email */
                  0,                      /* letterclass */
                  OUTPUT lcErrFile).



IF lcErrFile > "" THEN DO:
   DEF STREAM outfile.
   OUTPUT STREAM outfile to VALUE( lcRootDir + "errors.txt").
   PUT STREAM outfile UNFORMATTED
           liLanguage SKIP
           OrderCustomer.CustIDType SKIP
           Order.Ordertype SKIP
           lcErrFile SKIP
           iiOrderId SKIP.
   OUTPUT STREAM outfile CLOSE.
   RETURN "ERROR: Printout failed " + lcErrFile.
END.

ELSE DO:
   fTxtSendLog(4). 
   /* mark printing time */
   fMarkOrderStamp(Order.OrderID,
                   "Print",
                   0.0).
   
   RETURN "". 

END.
             
