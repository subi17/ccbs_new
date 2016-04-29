/* ----------------------------------------------------------------------
MODULE .......: sendorderconf.p
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
/*{utumaa.i new }*/
/*{edefine.i new}*/
{tmsconst.i}
{timestamp.i}

DEF INPUT PARAM iiOrderId AS INT NO-UNDO.
DEF INPUT PARAM icEmailAddress AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocErrFile AS CHAR NO-UNDO.

DEF VAR lcRootDir    AS CHAR NO-UNDO.
DEF VAR lcEmailFile  AS CHAR NO-UNDO.
DEF VAR liLanguage   AS INT  NO-UNDO.
DEF VAR lcError      AS CHAR NO-UNDO.


FUNCTION fTxtSendLog RETURNS LOGIC
   (iiSendMethod AS INT):

   /* mark text as sent */
   CREATE ITSendLog.
   ASSIGN ITSendLog.Brand      = gcBrand
          ITSendLog.TxtType    = 1       /* inf. text */
          ITSendLog.ITNum      = -10     /* temp value for avoiding email sending twice */ 
          ITSendLog.CustNum    = iiOrderID
          ITSendLog.InvNum     = iiOrderID
          ITSendLog.SendMethod = iiSendMethod 
          ITSendLog.EMail      = icEmailAddress
          ITSendLog.RepType    = "ITOrd"
          ITSendLog.UserCode   = katun.
          ITSendLog.SendStamp  = fMakeTS().
END.

/*
lcRootDir = SEARCH("do_not_remove_templatefolder.txt").
lcRootDir = REPLACE(lcrootDir, "do_not_remove_templatefolder.txt", "").
*/

lcrootDir = "/apps/yoigo/tms/templates/".
lcEmailFile = fCParam("Printing","MailPrintFile") + "_" + STRING(iiOrderId) +
              "_" + STRING(TODAY,"999999") + "_" + STRING(TIME) + 
              "_conf.html".

FIND Order WHERE
  Order.Brand   = gcBrand  AND
  Order.OrderID = iiOrderID NO-LOCK NO-ERROR.

FIND FIRST OrderCustomer WHERE OrderCustomer.Brand = gcBrand AND
                               OrderCustomer.OrderId = iiOrderId AND
                               OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.

   IF Ordercustomer.CustIdType EQ "CIF" THEN .
   ELSE IF Order.Ordertype = {&ORDER_TYPE_NEW} THEN
       RUN parse_tags.p (lcRootDir + "conf_email_new_es.html",
                 lcEmailFile, iiOrderId, 2, 
                 icEmailAddress, OUTPUT ocErrFile). /* 2 = conf mes */
   ELSE IF Order.Ordertype = {&ORDER_TYPE_MNP} THEN
       RUN parse_tags.p (lcRootDir + "conf_email_mnp_es.html",
                 lcEmailFile, iiOrderId, 2, 
                 icEmailAddress, OUTPUT ocErrFile). /* 2 = conf mes */
   /* fusion STC needs propably own template. TODO later
   ELSE IF (Order.Ordertype = {&ORDER_TYPE_STC} AND 
            Order.OrderChannel BEGINS "fusion") THEN  /* new fusion order */
          RUN parse_tags.p (lcRootDir + "conf_email_new_es.html",
                           lcEmailFile, iiOrderId, 2, OUTPUT lcErrFile). /* 2 = conf mes */
   */
   ELSE ocErrFile = "Not supported type in order " + " " + 
                    STRING(Order.Ordertype) + " " + STRING(iiOrderId).
/*END.*/

IF ocErrFile > "" OR PROGRAM-NAME(2) BEGINS "Test" THEN .
ELSE
   fTxtSendLog(6).

