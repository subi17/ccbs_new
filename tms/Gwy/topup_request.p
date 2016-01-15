/* -----------------------------------------------
  MODULE .......: TOPUP_REQUEST.P
  FUNCTION .....: Manual TopUp Request handler
  APPLICATION ..: TMS
  CREATED ......: 20.10.2006 KL
                  10.04.07 kl SMS including VAT
                  24.04.07 kl no SMS for CC negative
                  01.06.07 aam new sources MINCONS and MANFIX
                  04.09.07 kl llOK for Payment and PPStatus
                  11.09.07 kl fixed: llOK = (liRespCode = 0)
                  22.11.07 tk/handles atm-topups
                  24.01.08 aam  payments through TopupQueue
  VERSION ......: XFERA
------------------------------------------------------ */

{commpaa.i} 
{xmlfunction.i}
{timestamp.i}
{fgettxt.i}
{fmakesms.i}

ASSIGN
   katun   = "Cron"
   gcBrand = "1".

{heartbeat.i}

FUNCTION fCallAlarm RETURNS LOGICAL
  (INPUT pcAction AS CHARACTER,
   INPUT pcCLI    AS CHARACTER,
   INPUT pdeAmt   AS INTEGER,
   INPUT pcOrig   AS CHARACTER):
   
   DEFINE VARIABLE ldeActStamp  AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE lcAlarmMess  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liLang       AS INTEGER   NO-UNDO.

   FIND FIRST MobSub WHERE
              MobSub.CLI = pcCLI
   NO-LOCK NO-ERROR.

   IF NOT AVAIL MobSub THEN RETURN False.

   ASSIGN
      /* others don't exist yet ! */
      liLang      = 1
      lcAlarmMess = fGetSMSTxt(pcAction, TODAY, liLang, OUTPUT ldeActStamp)
      lcAlarmMess = REPLACE(lcAlarmMess,"#TOPUP", TRIM(STRING(pdeAmt / 100,">>>99.99"))).

   IF lcAlarmMess > "" THEN DO:
      fMakeSchedSMS2(MobSub.CustNum,
                     MobSub.CLI,
                     22,
                     lcAlarmMess,
                     ldeActStamp,
                     pcOrig,
                     "").
      RELEASE CallAlarm.
   END.

   RETURN TRUE.
   
END FUNCTION.

DEFINE VARIABLE lcNagios  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcStarted AS CHARACTER NO-UNDO FORMAT "X(19)".
DEFINE VARIABLE lcNow     AS CHARACTER NO-UNDO FORMAT "X(19)".
DEFINE VARIABLE liLoop    AS INTEGER   NO-UNDO.

FORM
   lcStarted COLUMN-LABEL "      Started"
   liLoop    COLUMN-LABEL "Loops"
   lcNow     COLUMN-LABEL "      Latest"
WITH ROW 8 CENTERED TITLE " TopUp request sender " FRAME frm.

lcNagios = "TopUp:CC TopUps".

fKeepAlive(lcNagios).

lcStarted = fTS2HMS(fMakeTS()).

DISP lcStarted WITH FRAME frm.

LOOP:
DO WHILE TRUE:
   
   PUT SCREEN ROW 22 "Sending TopUps ....".

   RUN pPPRequests(fMakeTS()).

   ASSIGN
      liLoop = liLoop + 1
      lcNow  = fTS2HMS(fMakeTS()).

   DISP
      liLoop
      lcNow
   WITH FRAME frm.

   PUT SCREEN ROW 22 "F8 to QUIT         ".
   
   READKEY PAUSE 10.

   fKeepAlive(lcNagios).
  
   IF LOOKUP(KEYLABEL(LASTKEY),"8,F8") > 0 THEN LEAVE LOOP.

END.

QUIT.

PROCEDURE pPPRequests:

   DEFINE INPUT PARAMETER pdeTS AS DECIMAL.

   DEFINE VARIABLE lcXML      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liRespCode AS INTEGER   NO-UNDO.
   DEFINE VARIABLE llNext     AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE lcSource   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE llOK       AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE liBatchLoop AS INTEGER NO-UNDO. 
   DEFINE VARIABLE ldeTopUpAmount AS DECIMAL NO-UNDO.
   
   DEFINE BUFFER bufPP FOR PrePaidRequest.

   lcSource = "CC,Web Order,MINCONS,MANFIX,COMM,CHARGE,COMP".
   liBatchLoop = 0.
   
   BATCH_LOOP:
   FOR EACH PrePaidRequest NO-LOCK WHERE
            PrePaidRequest.Brand      = gcBrand      AND
            PrePaidRequest.PPStatus   = 0            AND
            PrePaidRequest.TSRequest <= pdeTS        AND
            LOOKUP(PrePaidRequest.Source,lcSource) > 0:

      liBatchLoop = liBatchLoop + 1.
      IF liBatchLoop >= 500 THEN LEAVE BATCH_LOOP.

      DO FOR bufPP:

         FIND FIRST bufPP WHERE
              RECID(bufPP) = RECID(PrePaidRequest)
         EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      
         llNEXT = LOCKED(bufPP).

      END.
      
      IF llNext THEN NEXT.
     
      RUN pp_platform(gcBrand,PrePaidRequest.PPRequest).
      
      lcXML = RETURN-VALUE.
      
      liRespCode = 9.
      IF lcXML > "" AND NOT lcXML BEGINS "ERR:" THEN
      liRespCode = INT(fGetRPCNodeValue(lcXML,"responseCode")) NO-ERROR.
      
      /* YDR-390, 1 and 2 are no longer considered as errors */
      llOK = (liRespCode = 0 OR liRespCode = 1 OR liRespCode = 2).

      DO FOR bufPP:

         FIND FIRST bufPP WHERE RECID(bufPP) = RECID(PrePaidRequest) EXCLUSIVE-LOCK.

         ASSIGN
            bufPP.TSResponse = fMakeTS()
            bufPP.Response   = lcXML
            bufPP.RespCode   = liRespCode.

         IF llOK THEN bufPP.PPStatus = 2.
         ELSE         bufPP.PPStatus = 3.

         DEFINE VARIABLE liAccValue AS INTEGER NO-UNDO.   

         liAccValue = INT(bufPP.TopUpAmt + bufPP.VatAmt).   
         
         CASE PrePaidRequest.Source:

            /* Customer Care TopUp */
            WHEN "CC" OR 
            
            WHEN "MINCONS" OR

            WHEN "MANFIX" THEN DO:
               
               /* only positive compensations */
               IF bufPP.TopUpAmt > 0 THEN
                  fCallAlarm("CompInfo",
                             bufPP.CLI,
                             INT(bufPP.TopUpAmt + bufPP.VatAmt),
                             "800622800").
            
            END.

            /* PrePaid initial TopUp */
            WHEN "Web Order" THEN DO:
               IF llOK AND LOOKUP(bufPP.PPReqPrefix,"992,993") = 0 THEN DO:

                  ldeTopUpAmount = bufPP.TopUpAmt + bufPP.VatAmt.

                  FOR FIRST Order NO-LOCK WHERE
                            Order.Brand     = gcBrand AND
                            Order.MSSeq     = bufPP.MSSeq AND
                            Order.OrderType < 2 AND
                            LOOKUP(Order.StatusCode,{&ORDER_CLOSE_STATUSES}) = 0,
                     FIRST Offer NO-LOCK WHERE
                           Offer.Brand = gcBrand AND
                           Offer.Offer = Order.Offer,
                        FIRST OfferItem NO-LOCK WHERE
                              OfferItem.Brand       = gcBrand AND
                              OfferItem.Offer       = Offer.Offer AND
                              OfferItem.ItemType    = "TopUp" AND
                              OfferItem.EndStamp   >= Order.CrStamp AND
                              OfferItem.BeginStamp <= Order.CrStamp,
                           FIRST TopupScheme NO-LOCK WHERE
                                 TopupScheme.Brand       = gcBrand AND
                                 TopupScheme.TopupScheme = OfferItem.ItemKey,
                              FIRST TopupSchemeRow NO-LOCK WHERE
                                    TopupSchemeRow.Brand        = gcBrand AND
                                    TopupSchemeRow.TopupScheme  = TopupScheme.TopupScheme AND
                                    TopupSchemeRow.EndStamp    >= Order.CrStamp AND
                                    TopupSchemeRow.BeginStamp  <= Order.CrStamp:

                     IF TopupSchemeRow.DisplayAmount > 0 THEN
                        ldeTopUpAmount = TopupSchemeRow.DisplayAmount * 100.
                  END.

                  fCallAlarm("TopUpOrder",
                             bufPP.CLI,
                             INT(ldeTopUpAmount),
                             IF bufPP.PPReqPrefix = "994"
                             THEN "800622800"
                             ELSE "622").
               END.
            END.    
                            
            /* commission */
            WHEN "Comm" THEN DO:
               IF llOk THEN              
                  fCallAlarm("MGMTopupRecharge",
                             bufPP.CLI,
                             INT(bufPP.TopUpAmt + bufPP.VatAmt),
                             "800622800").
            END.
         END.

         /* payment is created from each succesful event */
         IF llOk THEN DO:
         
            CREATE TopUpQueue.
            ASSIGN
               TopUpQueue.PPRequest = bufPP.PPRequest
               TopUpQueue.CLI       = bufPP.CLI
               TopUpQueue.TopUpAmt  = bufPP.TopUpAmt / 100
               TopUpQueue.VatAmt    = bufPP.VatAmt / 100
               TopUpQueue.Date      = TODAY
               TopUpQueue.Source    = bufPP.Source.
            RELEASE TopUpQueue.   
         END. 
         
      END.

   END.

END PROCEDURE.

