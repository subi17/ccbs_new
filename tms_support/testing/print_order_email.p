{commpaa.i}
katun = "Qvantel".
gcBrand = "1".
{timestamp.i}
{utumaa.i new }
{feplstart.i}
{cparam2.i}
{edefine.i NEW}
{forderstamp.i}

DEF VAR liTextID     AS INT  NO-UNDO.
DEF VAR ldtDate      AS DATE NO-UNDO.
DEF VAR liTime       AS INT  NO-UNDO. 
DEF VAR liLanguage   AS INT  NO-UNDO. 
DEF VAR lcErrFile    AS CHAR NO-UNDO.
DEF VAR liCount      AS INT  NO-UNDO.
DEF VAR lcCheck      AS CHAR NO-UNDO.
DEF VAR lcEmailKey   AS CHAR NO-UNDO.
DEF VAR liOrderId LIKE Order.OrderID. 

/*
FIND FIRST Order WHERE
   Order.Brand = "1" and
   Order.OrderId = 10048697 NO-LOCK NO-ERROR.
*/

UPDATE liOrderId LABEL "ORDER EMAIL TEST - Order ID" WITH FRAME row1.

FIND FIRST Order WHERE
   Order.Brand = "1" and
   Order.OrderId = liOrderId NO-LOCK NO-ERROR.
IF NOT AVAILABLE Order THEN DO:
   MESSAGE  "ERROR:Order not available" VIEW-AS ALERT-BOX.
   QUIT.
END.

IF LOOKUP(Order.OrderChannel,"pos,vip,gift,yoigo,renewal_pos,renewal_pos_stc") > 0 THEN DO:
   MESSAGE  "INFO:" + Order.OrderChannel + " order, email not created" VIEW-AS ALERT-BOX.
   RETURN.
END.

IF Order.CLI = "" THEN DO:
   MESSAGE "ERROR:MSISDN not defined" VIEW-AS ALERT-BOX.
   RETURN.
END.
/*
FIND FIRST Order WHERE
   Order.Brand = "1" and
   Order.OrderId = 10048718 NO-LOCK NO-ERROR.
*/
fSplitTS(Order.CrStamp,
         OUTPUT ldtDate,
         OUTPUT liTime).

FOR FIRST OrderCustomer OF Order NO-LOCK WHERE
          OrderCustomer.RowType = 1:
   liLanguage = INTEGER(OrderCustomer.Language) NO-ERROR.
   lcEmailKey = (IF OrderCustomer.CustIdType EQ "CIF" THEN "EmailConfCIF"
              ELSE "EmailConf").
   
   IF Order.OrderType EQ 1 THEN 
      lcEmailKey = lcEmailKey + "MNP".

   IF Order.OrderType EQ 2 THEN DO:
      lcEmailKey = "RenewalConf" + 
            (IF OrderCustomer.CustIdType EQ "CIF" THEN "CIF" ELSE "").
   END.
END.

IF liLanguage = 0 OR 
   NOT CAN-FIND(Language WHERE Language.Language = liLanguage)
THEN DO:
   MESSAGE  "ERROR:Unknown language" VIEW-AS ALERT-BOX.
   RETURN.
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

IF liTextID = 0 THEN DO:
   MESSAGE "ERROR:Text not available" VIEW-AS ALERT-BOX.
   RETURN.
END.

RUN /apps/xfera/tms/Mc/printxt.p (Order.OrdeRId,
             0,
             Order.CLI,
             1,                      /* 1=invtext */
             6,                      /* order */
             "",
             "",
             liTextID, /* email ID */
             6,                      /* email */
             0,                      /* letterclass */
             OUTPUT lcErrFile).
IF lcErrFile EQ "" THEN MESSAGE "Mail printed" VIEW-AS ALERT-BOX.
ELSE MESSAGE lcErrFile VIEW-AS ALERT-BOX ERROR.
