/* ----------------------------------------------------------------------
MODULE .......: sendorderreq.p
TASK .........: Sends HTML5 order confirmation req messages
APPLICATION ..: TMS
AUTHOR .......: kariaika
CREATED ......: 8.5.2015
CHANGED ......:
Version ......: Yoigo
----------------------------------------------------------------------- */
{commali.i}
gcBrand = "1".
{cparam2.i}
/*{utumaa.i new }
{edefine.i new}*/
{tmsconst.i}

DEF INPUT PARAM iiOrderId AS INT NO-UNDO.
DEF INPUT PARAM icEmailAddress AS CHAR NO-UNDO.
DEF OUTPUT PARAM lcError AS CHAR NO-UNDO.

DEF VAR lcRootDir    AS CHAR NO-UNDO.
DEF VAR lcEmailFile  AS CHAR NO-UNDO.
DEF VAR liLanguage   AS INT  NO-UNDO.

/*
lcRootDir = SEARCH("do_not_remove_templatefolder.txt").
lcRootDir = REPLACE(lcrootDir, "do_not_remove_templatefolder.txt", "").
*/
lcrootDir = "/apps/yoigo/tms/templates/".
lcEmailFile = fCParam("Printing","MailPrintFile") +
              "_" + STRING(TODAY,"999999") + "_" + STRING(TIME) + 
              "_" + STRING(iiOrderId) + "_req.html".

FIND Order WHERE
  Order.Brand   = gcBrand  AND
  Order.OrderID = iiOrderID NO-LOCK NO-ERROR.

IF Order.OrderType NE {&ORDER_TYPE_NEW} AND
   Order.OrderType NE {&ORDER_TYPE_MNP} THEN RETURN "".

FIND FIRST OrderCustomer WHERE OrderCustomer.Brand = gcBrand AND
                               OrderCustomer.OrderId = iiOrderId AND
                               OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.
/* Only Spanish decided to be supported at the moment
liLanguage = INT(OrderCustomer.Language). 

IF liLanguage = 5 THEN DO: /*English*/
   IF OrderCustomer.CustIDType EQ "CIF" THEN
      RUN parse_tags.p (lcRootDir + "email_company_en.html",
             lcEmailFile, iiOrderId, 1, 
             icEmailAddress, OUTPUT lcError).
   ELSE IF Order.Ordertype = {&ORDER_TYPE_NEW} THEN
       RUN parse_tags.p (lcRootDir + "email_new_en.html",
              lcEmailFile, iiOrderId, 1, 
              icEmailAddress, OUTPUT lcError).
   ELSE IF Order.Ordertype = {&ORDER_TYPE_MNP} THEN
       RUN parse_tags.p (lcRootDir + "email_mnp_en.html",
              lcEmailFile, iiOrderId, 1, 
              icEmailAddress, OUTPUT lcError).
   /* do we need some other handling here */
END.
ELSE DO:*/ /*Spanish*/
   IF LOOKUP(OrderCustomer.CustIDType,"CIF,CFraud,CInternal") > 0 THEN
      RUN parse_tags.p (lcRootDir + "email_company_es.html",
             lcEmailFile, iiOrderId, 1, 
             icEmailAddress, OUTPUT lcError).
   ELSE IF Order.Ordertype = {&ORDER_TYPE_NEW} THEN
       RUN parse_tags.p (lcRootDir + "email_new_es.html",
              lcEmailFile, iiOrderId, 1, 
              icEmailAddress, OUTPUT lcError).
   ELSE IF Order.Ordertype = {&ORDER_TYPE_MNP} THEN
       RUN parse_tags.p (lcRootDir + "email_mnp_es.html",
              lcEmailFile, iiOrderId, 1, 
              icEmailAddress, OUTPUT lcError).
   /* do we need some other handling/checks  here */
/*END.*/

