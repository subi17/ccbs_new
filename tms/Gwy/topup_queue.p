/* -----------------------------------------------
  MODULE .......: TOPUPQUEUE.P
  FUNCTION .....: ATM / TopUp request payments to TMS
  APPLICATION ..: TMS
  CREATED ......: 16.10.2007 KL
  MODIFIED .....: 04.04.2008 kl 5000 loops

  VERSION ......: XFERA
------------------------------------------------------ */

{commpaa.i}

ASSIGN
   gcBrand = "1"
   katun   = "ATM".

{tmsconst.i}
{barrfunc.i}
{cparam2.i}

DEFINE VARIABLE ldaDate     AS DATE      NO-UNDO FORMAT "99.99.9999".
DEFINE VARIABLE lcTime      AS CHARACTER NO-UNDO.
DEFINE VARIABLE liLoops     AS INTEGER   NO-UNDO.
DEFINE VARIABLE liPayments  AS INTEGER   NO-UNDO.
DEFINE VARIABLE liVoucher   AS INTEGER   NO-UNDO.
DEFINE VARIABLE liTransact  AS INTEGER   NO-UNDO.
DEF VAR ldeTopUpUnbarrLimit AS DEC NO-UNDO. 

ldeTopUpUnbarrLimit = fCParamDe("TopUpUnbarrLimit").

FORM 
   liLoops    COLUMN-LABEL "Loops"
   liPayments COLUMN-LABEL "Payments"
   ldaDate    COLUMN-LABEL "Date"
   lcTime     COLUMN-LABEL "Time"
WITH
   WIDTH 48 TITLE " YOIGO TopUpPayment Queues " CENTERED ROW 4
FRAME frm.

DEFINE BUFFER bufQueue FOR TopUpQueue.

DISP
   liLoops
   liPayments
   TODAY @ ldaDate
   STRING(TIME,"HH:MM:SS") @ lcTime 
WITH FRAME frm.

LOOP:
DO WHILE TRUE:
   
   FOR EACH TopUpQueue NO-LOCK WHERE
            TopUpQueue.State = 0
   liTransact = 1 TO 5000:
      
      IF liTransact MOD 50 = 0 THEN
         PUT SCREEN ROW 1 COL 1 STRING(liTransact).
   
      PUT SCREEN ROW 23 "Processing ......".
      
      FIND FIRST bufQueue WHERE
           RECID(bufQueue) = RECID(TopUpQueue)
      EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

      IF NOT LOCKED(bufQueue) AND NOT ERROR-STATUS:ERROR THEN DO:

         RUN topuppaym(TopUpQueue.PPRequest,
                       TopUpQueue.CLI,
                       TopUpQueue.TopUpAmt,
                       TopUpQueue.VatAmt,
                       TopUpQueue.Date,
                       TopUpQueue.Source,
                       OUTPUT liVoucher).

         IF liVoucher NE 0 THEN ASSIGN
            bufQueue.State = 1  WHEN liVoucher > 0
            bufQueue.State = 99 WHEN liVoucher < 0
            liPayments     = liPayments + 1.
      
         /* YDR-157 */
         IF bufQueue.State NE 0 AND TopUpQueue.Source = "ATM" AND
            TopUpQueue.TopUpAmt + TopUpQueue.VatAmt >= ldeTopUpUnbarrLimit THEN
            RUN pUnbarrPrepaid(TopUpQueue.CLI).
      END.
   END.

   liLoops = liLoops + 1.

   PUT SCREEN ROW 1 COL 1 "     ".

   DISP
      liLoops
      liPayments
      TODAY @ ldaDate
      STRING(TIME,"HH:MM:SS") @ lcTime 
   WITH FRAME frm.

   PUT SCREEN ROW 23 "Hit F8 to QUIT ..".

   READKEY PAUSE 5.
   HIDE MESSAGE NO-PAUSE.

   IF LOOKUP(KEYLABEL(LASTKEY),"F8,8") > 0 THEN LEAVE LOOP.

END.

QUIT.

PROCEDURE pUnbarrPrepaid:

   DEF INPUT PARAM pcCLI AS CHAR NO-UNDO.  

   DEF VAR lcBarring AS CHAR NO-UNDO. 
   DEF VAR lcUnBarring AS CHAR NO-UNDO. 
   DEF VAR lrBarring AS ROWID NO-UNDO.
   DEF VAR lcResult AS CHAR NO-UNDO. 
         
   FIND FIRST MobSub WHERE
              MobSub.CLI = pcCLI AND
              MobSub.PayType = True NO-LOCK NO-ERROR.
   IF NOT AVAIL MobSub THEN RETURN.
   
   /* check current barring (or pending) */
   lcBarring  = fCheckBarrStatus(MobSub.MsSeq, OUTPUT lrBarring).
   IF lcBarring = "OK" THEN RETURN.
   
   FIND MsRequest WHERE ROWID(MsRequest) = lrBarring NO-LOCK NO-ERROR.
   IF NOT AVAIL MsRequest THEN RETURN.

   IF MsRequest.UserCode NE "CreSub / CreSub" THEN RETURN.

   IF lcBarring EQ "91" THEN DO:
      IF MsRequest.ReqCParam1 NE "Y_HURP" THEN RETURN.
   END.
   ELSE IF lcBarring = "Y_HURP" THEN DO:

      lcUnBarring = "UN" + lcBarring.
      /* create barring request */
      RUN barrengine.p (MobSub.MsSeq,
                      lcUnBarring,
                      {&REQUEST_SOURCE_SCRIPT}, /* source  */
                      "", /* creator */
                      fMakeTS(), /* activate */
                      "", /* SMS */
                      OUTPUT lcResult).

      INT(lcResult) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN RETURN.
   END.
   ELSE RETURN.

   /* write possible error to a memo */
   DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                "MobSub",
                STRING(MobSub.MsSeq),
                MobSub.Custnum,
                "Fallo la retirada automatica del barring Y_HURP",
                "Fallo la retirada automatica del barring Y_HURP").

END PROCEDURE. 

