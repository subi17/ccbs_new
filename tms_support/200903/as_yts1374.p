
/* ----------------------------------------------------------------------
  MODULE .......: **_FILENAME_**
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 20.11.08
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */
/*
FIND FIRST mnpprocess where
   formrequest = "00500484062" NO-LOCK NO-ERROR.

   disp mnpprocess.mnpseq.
*/

{commpaa.i}
katun = "anttis".
gcBrand = "1".
{timestamp.i}
{mnp.i}

FUNCTION fNewXML RETURNS CHAR 
(iiOrderid AS INTEGER, lcFormRequest as char):

   DEFINE VARIABLE lhSAX AS HANDLE NO-UNDO.
   DEFINE VARIABLE lmXML AS MEMPTR NO-UNDO.
   DEFINE VARIABLE lcXML AS CHAR   NO-UNDO.

   DEFINE VARIABLE lcOper        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcIdType      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcAlarmMess   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liLang        AS INTEGER   NO-UNDO.
   DEFINE VARIABLE ldeActStamp1  AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE ldeActStamp2  AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE ldChgDate     AS DATE      NO-UNDO. 
   DEFINE VARIABLE ldeChgStamp   AS DECIMAL   NO-UNDO. 


   FIND FIRST Order WHERE
              Order.Brand   = gcBrand AND
              Order.OrderId = iiOrderId
   NO-LOCK NO-ERROR.

   FIND FIRST OrderCustomer WHERE
              OrderCustomer.Brand   = Order.Brand   AND
              OrderCustomer.OrderId = Order.OrderId AND
              OrderCustomer.RowType = 1
   NO-LOCK NO-ERROR.

   FIND FIRST TMSCodes WHERE 
              TMSCodes.TableName = "Order"    AND
              TMSCodes.FieldName = "CurrOper" AND
              TMSCodes.CodeValue  = STRING(order.curroper)
   NO-LOCK NO-ERROR.
   IF AVAIL TMSCodes THEN lcOper = TMSCodes.CodeName.
   ELSE lcOper = order.curroper.

   ldChgDate = fMNPChangeWindowDate(
      fMakeTS(),
      Order.OrderChannel,
      OrderCustomer.Region).

   ldeChgStamp = fMake2Dt(ldChgDate,10800).

   CREATE SAX-WRITER lhSAX.

   lhSAX:FORMATTED = FALSE. 
   lhSAX:ENCODING = "UTF-8".
   lhSAX:STANDALONE = TRUE.
   lhSAX:SET-OUTPUT-DESTINATION("memptr",lmXML).
      
   lhSAX:START-DOCUMENT().
   lhSAX:START-ELEMENT("ns2:portabilityRequest").
   lhSAX:INSERT-ATTRIBUTE("xmlns:ns2","http://www.example.org/altaPortabilidad").
   lhSAX:WRITE-DATA-ELEMENT("formRequestCode",lcFormRequest).
   lhSAX:START-ELEMENT("subscriberData").
   IF OrderCustomer.CustIdType = "CIF" THEN DO:
      lhSAX:WRITE-DATA-ELEMENT("companyName",ordercustomer.company).
      lhSAX:WRITE-DATA-ELEMENT("CIF",ordercustomer.CustId).
   END.
   ELSE DO:
      lcIdType = OrderCustomer.CustIdType.

      /* passport in lowercase */
      IF lcIdType = "Passport" THEN lcIdType = LOWER(lcIdType).

      lhSAX:WRITE-DATA-ELEMENT("name",ordercustomer.firstname).
      lhSAX:WRITE-DATA-ELEMENT("firstSurname",ordercustomer.surname1).
      lhSAX:WRITE-DATA-ELEMENT("secondSurname",ordercustomer.surname2).
      lhSAX:WRITE-DATA-ELEMENT(lcIdType,ordercustomer.CustId).
      lhSAX:WRITE-DATA-ELEMENT("nationality",ordercustomer.nationality).
   END.
   lhSAX:END-ELEMENT("subscriberData").
   lhSAX:START-ELEMENT("portabilityData").
   lhSAX:WRITE-EMPTY-ELEMENT("msisdn").
   lhSAX:INSERT-ATTRIBUTE("msisdnStart",Order.CLI).
   lhSAX:INSERT-ATTRIBUTE("msisdnEnd",Order.CLI).
   lhSAX:INSERT-ATTRIBUTE("icc-idStart",substr(Order.OldICC,1,19)).
   lhSAX:INSERT-ATTRIBUTE("icc-idEnd",substr(Order.OldICC,1,19)).
   lhSAX:INSERT-ATTRIBUTE("nrn","741111").
   lhSAX:WRITE-DATA-ELEMENT("requestedDate",STRING(fUTCTime(Order.CrStamp))).
   lhSAX:WRITE-DATA-ELEMENT("changeWindowDate",STRING(fUTCTime(ldeChgStamp))).
   lhSAX:END-ELEMENT("portabilityData").
   lhSAX:WRITE-DATA-ELEMENT("opDonor",lcOper).
   lhSAX:WRITE-DATA-ELEMENT("opReceiver","YOI").
   lhSAX:END-ELEMENT("ns2:portabilityRequest").
   lhSAX:END-DOCUMENT().

   lcXML = GET-STRING(lmXML,1).

   RETURN lcXML.

END FUNCTION. 


def buffer bufMessage for mnpmessage.


DEFINE TEMP-TABLE ttDups
FIELD i AS INT
INDEX i IS PRIMARY UNIQUE i. 

DEFINE VARIABLE lcNewXML AS CHARACTER NO-UNDO. 
DEFINE VARIABLE i AS INTEGER NO-UNDO.    

def stream sin.
input stream sin from /apps/snet/200903/as_yts1374.input.

def stream slog.
output stream slog to /apps/snet/200903/as_yts1374.output append.

DEFINE VARIABLE lcFormRequest AS CHARACTER NO-UNDO. 

repeat:
 
   import stream sin unformatted lcFormRequest.


   i = i + 1.
   if i <= 2 then next.

   FIND FIRST mnpprocess where
      mnpprocess.formrequest = lcFormRequest NO-LOCK.
   
   if not mnpprocess.statuscode = 0 then do:
      put stream slog unformatted mnpprocess.formrequest " NOK" skip.
      next.
   end.



   FIND bufmessage where
      bufmessage.mnpseq = mnpprocess.mnpseq and
      bufmessage.messagetype begins "portabilityRequest" 
      NO-LOCK.
      /* and bufmessage.statuscode = 500 NO-LOCK NO-ERROR. */
/*
   FIND FIRST mnpmessage where
      mnpmessage.mnpseq = bufmessage.mnpseq and
      mnpmessage.messagetype = "confirmack" NO-LOCK NO-ERROR.

   IF AVAIL mnpmessage then do:
      MESSAGE mnpmessage.mnpseq VIEW-AS ALERT-BOX.
      next.
   end.

   FIND FIRST ttDups where
      ttDups.i = bufmessage.mnpseq NO-LOCK NO-eRROR.
   IF NOT AVAIL ttDups THEN DO:
      create ttDups.
      assign
         ttDups.i = bufmessage.mnpseq.
   END.
   ELSE do:
      message "foo" VIEW-AS ALERT-BOX.
      NEXT.
   end.
   
   FIND FIRST mnpprocess where
         mnpprocess.mnpseq = bufmessage.mnpseq NO-LOCK NO-ERROR.
*/

   lcNewXML = fNewXML(mnpprocess.orderid, lcFormRequest).
   /*  disp lcNewXML view-as editor size 70 by 50. */
   DO TRANS: 
   put stream slog unformatted mnpprocess.formrequest skip.
  CREATE MNPMessage.

  BUFFER-COPY bufMessage EXCEPT
     bufMessage.XMLMessage
     bufMessage.StatusCode
     bufMessage.CreatedTS
     bufMessage.SentTS
  TO MNPMessage.

  ASSIGN
     MNPMessage.CreatedTS = fMakeTS()
     MNPMessage.StatusCode = 1
     MNPMessage.XMLMessage = lcNewXML
     MNPMessage.MsgTurn = MNPMessage.MsgTurn + 1.
  
  RELEASE MNPMessage.
   END.

END.

input stream sin close.
output stream slog close.

disp i.
