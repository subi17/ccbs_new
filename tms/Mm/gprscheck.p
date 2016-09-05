{Syst/commali.i}
{Func/timestamp.i}
DEFINE VARIABLE lcCli AS CHARACTER NO-UNDO format "x(9)". 
DEFINE VARIABLE lcFrom AS CHARACTER NO-UNDO FORMAT "x(8)". 
DEFINE VARIABLE lcTo AS CHARACTER NO-UNDO FORMAT "x(8)".
DEFINE VARIABLE ldeFrom AS DECIMAL NO-UNDO. 
DEFINE VARIABLE ldeTo AS DECIMAL NO-UNDO. 
DEFINE VARIABLE lcInfo AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldaFrom AS DATE NO-UNDO format "99-99-9999".
DEFINE VARIABLE ldaTo AS DATE NO-UNDO format "99-99-9999". 
DEFINE VARIABLE liFrom AS INTEGER NO-UNDO.
DEFINE VARIABLE liTo AS INTEGER NO-UNDO. 
DEFINE VARIABLE liTotal AS INT64 no-undo.
DEFINE VARIABLE ldeTotal AS DECIMAL NO-UNDO. 
DEFINE VARIABLE liQty AS INTEGER NO-UNDO. 
DEFINE VARIABLE liCC AS INTEGER NO-UNDO INIT 93.
DEFINE VARIABLE liCCN AS INTEGER NO-UNDO. 
DEFINE VARIABLE ldeCharge AS DEC NO-UNDO. 

lcInfo = " GPRS CDR data counter ".

assign
   ldaFrom = today
   ldato = today
   lcFrom = "00:00:00"
   lcTo   = "23:59:59".

form
   lcCLi label   "MSISDN." skip
   liCC label      "CC....." help "90=Roam.OUT ALL, 91=Roam.OUT EU, 92=Roam.OUT ROW, 93=national" skip
   ldaFrom     label  "From..."
   lcFrom no-label skip
   ldaTo   label  "To....." 
   lcTo no-label skip
   liQty       label  "Qty...." skip
   ldeTotal    label  "Mb....." skip
   ldeCharge   label  "Charge."
WITH  OVERLAY ROW 8 centered
TITLE  lcInfo side-LABELS
FRAME a.

LOOP:
DO WHILE TRUE:

   ehto = 9. RUN Syst/ufkey.p.
   REPEAT ON ENDKEY UNDO, LEAVE:

      UPDATE 
         lcCLi
         liCC
         ldaFrom
         lcFrom
         ldaTo
         lcTo WITH FRAM a.

      FIND MobSub where
           MobSub.cli = lcCLi
      NO-LOCK no-error.

      IF liCC NE 90 AND  
         liCC NE 91 AND
         liCC NE 92 AND
         liCC NE 93 THEN DO:
         MESSAGE "Unsupported CC" 
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.

      IF NOT lcCLI > "" OR 
         NOT AVAIL MobSub THEN DO:
         MESSAGE "MSISDN" lcCLi "not found"
         VIEW-AS ALERT-BOX ERROR.
         next.
      END.
      
      ldeFrom = fHMS2TS(ldaFrom,lcFrom) no-error.
      if error-status:error then do:
         MESSAGE "Incorrect from stamp:" ldaFrom lcFrom 
         VIEW-AS ALERT-BOX ERROR.
         next.
      end.
      fSplitTS(ldeFrom, output ldaFrom, output liFrom).
      if error-status:error then do:
         MESSAGE "Incorrect to stamp:" ldaTo lcTo 
         VIEW-AS ALERT-BOX ERROR.
         next.
      end.
      
      ldeTo = fHMS2TS(ldato,lcto) no-error.
      if error-status:error then do:
         MESSAGE "Incorrect to stamp:" ldaTo lcTo 
         VIEW-AS ALERT-BOX ERROR.
         next.
      end.
      fSplitTS(ldeTo, output ldaTo, output lito).
      if error-status:error then do:
         MESSAGE "Incorrect to stamp:" ldaTo lcTo 
         VIEW-AS ALERT-BOX ERROR.
         next.
      end.
      
      leave. 
   end.

   IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN do:
      hide frame a.
      pause 0.
      LEAVE LOOP.
   END.

   ASSIGN
      liTotal = 0
      liQty = 0
      ldeCharge = 0.

   CASE liCC:
      WHEN 91 THEN ASSIGN
         liCC = 90
         liCCN = 91.
      WHEN 92 THEN ASSIGN
         liCC = 90
         liCCN = 92.
      OTHERWISE liCCN = 0.
   end.

   IF NOT mobsub.paytype THEN
   FOR EACH mobcdr NO-LOCK WHERE
      mobcdr.cli = mobsub.cli and
      mobcdr.datest >= ldaFrom and
      mobcdr.datest <= ldaTo and
      mobcdr.rateccn = liCC and
      mobcdr.errorcode = 0 use-index cli:
      
      if mobcdr.datest = ldaTo and mobcdr.timestart > liTo then next.
      if mobcdr.datest = ldafrom and mobcdr.timestart < lifrom then next.
      if liCCN > 0 and mobcdr.ccn ne liCCN then next.

      liQty = liQty + 1.
      ldeCharge = ldeCharge + mobcdr.amount.
      liTotal = liTotal + mobcdr.datain + mobcdr.dataout. 
   END.
   ELSE
   FOR EACH prepcdr NO-LOCK WHERE
      prepcdr.cli = mobsub.cli and
      prepcdr.datest >= ldaFrom and
      prepcdr.datest <= ldaTo and
      prepcdr.rateccn = liCC and
      prepcdr.errorcode = 0 use-index cli:
      
      if prepcdr.datest = ldaTo and prepcdr.timestart > liTo then next.
      if prepcdr.datest = ldafrom and prepcdr.timestart < lifrom then next.
      if liCCN > 0 and prepcdr.ccn ne liCCN then next.
      ldeCharge = ldeCharge + prepcdr.charge.

      liQty = liQty + 1.
      liTotal = liTotal + prepcdr.datain + prepcdr.dataout. 
   END.

   IF NOT mobsub.paytype THEN
   ldeTotal = trunc((liTotal / 1024 / 1024),2).
   ELSE
   ldeTotal = trunc((liTotal / 1024),2).
   
   disp lcCLi
        ldafrom
        ldaTo
        lcFrom
        lcTo
        liQty
        ldeTotal
        ldeCharge with frame a.

   pause.

END. 
