/* ------------------------------------------------------
  MODULE .......: mswdog.p
  FUNCTION .....: List ALL subscribers WITH no contract OR 
                  exceeded CreditInvNum limit
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 17.08.99
  MODIFIED .....: 05.04.00 jpo Bbatch session
                  14.10.02 jr  Removed BillLevel
                  04.01.05 aam Balance from SubSer
                  26.01.06 jt  lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN                                             ghFunc1, BUFFER Customer).
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}                             
{Func/excel.i}
{Func/fsubser.i}

DEF VAR xdays         AS I  NO-UNDO INIT 7.
DEF VAR ok            AS LO NO-UNDO FORMAT "Yes/No".
DEF VAR outfile       AS C  NO-UNDO.
DEF VAR amt1          AS I  NO-UNDO.
DEF VAR amt2          AS I  NO-UNDO.
DEF VAR hdr           AS C  NO-UNDO.
DEF VAR climit        AS LO NO-UNDO.
DEF VAR nocont        AS LO NO-UNDO.
DEF VAR i             AS I  NO-UNDO.
DEF VAR mi-no-2       AS C  NO-UNDO.
DEF VAR dlimit        AS c  NO-UNDO.
DEF VAR dbalance      AS c  NO-UNDO.
DEF VAR bbatch        AS lo NO-UNDO.
DEF VAR ldPrice       AS DE No-UNDO.
DEF VAR lcRatePlan    AS C  NO-UNDO.
DEF VAR outfile2      AS C  NO-UNDO.
DEF VAR liBalance     AS I  NO-UNDO. 
DEF VAR lcCustName    AS C  NO-UNDO.

DEF STREAM fraud.
{Func/tmsparam.i WatchDogFile RETURN}.  outfile = TMSParam.CharVal.
{Func/cparam.i Fraudfile     RETURN}.  outfile2  = tmsparam.CharVal.

hdr = "SubId,MSISDN,GSM No,Activated,Contract,CreditLimit,CustNo,"
    + "Customer's Name,Add'l Name,Address,PostCode,City,RatePlan,"
    + "InvT,Name of InvT,UsersLastName,FirstName,AddlName,"
    + "Address,Add'l Address,PostCode,City,Email,CCode,"
    + "Balance,Limit".
bBatch   = session:batch.

IF bbatch THEN 
ASSIGN
xdays = 99.

FORM
SKIP(1)
"  Note:  This program searches all MOBILE SUBSCRIPTIONS where either"  skip(1)
"         - CreditInvNum Limit has been exceeded OR"                          skip
"         - the contract is unsigned still" xdays  FORMAT "z9"
help "No. of days"
"days after activation"
                                                                        skip(1)
"         Such subscriptions are listed into a TAB separated PaymFile"      skip
"         for further actions."                                         skip(1)
"         Output PaymFile" outfile  format "x(40)"
help "Directory and Name of output PaymFile" 
skip(7)
WITH
   WIDTH 80 OVERLAY NO-LABELS TITLE " SUBSCRIBER WATCHDOG " FRAME main.



PAUSE 0.
MAIN:
REPEAT WITH FRAME main:
IF NOT bbatch THEN DO:
   ehto = 9. RUN ufkey.

      UPDATE xdays outfile.

ACTION:
   REPEAT WITH FRAME MAIN:
      ASSIGN ufk = 0 ufk[1] = 7 ufk[5] = 15 ufk[8] = 8 ehto = 0.
      RUN ufkey.

      IF toimi = 8 THEN LEAVE main.
      IF toimi = 1 THEN NEXT  main.
      IF toimi = 5 THEN DO:
         MESSAGE "Do You REALLY want to start (Y/N) ?" UPDATE ok.
         IF NOT ok THEN NEXT Action.
         ELSE LEAVE Action.
      END.   
   END. /* Action */

   MESSAGE "Searching subscriptions, wait ...".
END.  /* bbatch */
   OUTPUT STREAM excel TO VALUE(outfile).
   OUTPUT STREAM fraud to VALUE(outfile2).
   
   /* Headers TO the PaymFile */
   PUT STREAM excel UNFORMATTED
   ynimi " " 
   "Watchdog Report of Mobile Subscribers, Printed out "
   YEAR (TODAY) FORMAT "9999" "-"
   MONTH(TODAY) FORMAT "99"   "-"
   DAY  (TODAY) FORMAT "99".
   RUN uexskip(2).                           

   PUT STREAM fraud UNFORMATTED
   ynimi " Credit balance report of mobile subscribers, Printed out " 
   YEAR (TODAY) FORMAT "9999" "-"
   MONTH(TODAY) FORMAT "99"   "-"
   DAY  (TODAY) FORMAT "99" my-nl my-nl
   "CustNum " TAB "Cli" TAB "Balance" tab "CreditLimit" my-nl
   .

   /* clomn labels */
   DO i = 1 TO NUM-ENTRIES(hdr).
      PUT 
      STREAM EXCEL 
      UNFORMATTED 
         ENTRY(i,hdr) 
         tab.
   END.
   RUN uexskip(1).   

   FOR
   EACH MobSub NO-LOCK WHERE 
        MobSub.Brand = gcBrand .

      ASSIGN
      nocont   = FALSE
      climit   = FALSE
      dlimit   = "Not Available"
      dbalance = "Not Available".

      IF MobSub.Contract = FALSE AND (TODAY - MobSub.ActivationDate) > xdays
      THEN ASSIGN nocont = TRUE amt1 = amt1 + 1.
 
      ASSIGN ldPrice = 0
             i       = fCreditTypeValue(MobSub.MsSeq,
                                        OUTPUT liBalance).
      
      FOR EACH InvSeq NO-LOCK WHERE 
               InvSeq.CustNum = Mobsub.CustNum AND 
               InvSeq.Billed  = FALSE.
          
          FOR EACH saldocounter WHERE
               saldocounter.invSeq = InvSeq.Invseq AND
               saldocounter.cli    = mobsub.cli NO-LOCK .
             ASSIGN  
                ldPrice = ldPrice + SaldoCounter.Amt.
          END.
          
          If ldPrice           >= liBalance AND 
             liBalance > 0 THEN     
          ASSIGN 
             climit    = TRUE
             amt2      = amt2 + 1
             dlimit    = string(liBalance)
             dbalance  = string(ldPrice) .

      END. 
         
      IF climit THEN DO:
         PUT STREAM Fraud Unformatted
         mobsub.CustNum TAB
         Mobsub.cli     TAB
         dbalance       TAB
         dlimit  my-nl.
      
      END.
      
      
      ELSE IF nocont  THEN DO:

         FIND Customer WHERE 
              Customer.CustNum  = MobSub.CustNum NO-LOCK.

         FIND BillTarg WHERE 
              BillTarg.BillTarget = Mobsub.BillTarget AND 
              BillTarg.CustNum    = MobSub.CustNum NO-LOCK NO-ERROR.


         if avail billtarget then lcrateplan = billtarget.rateplan.
         else lcrateplan = "".

         /* prepare alternative presentation of MSISDN No. */
         mi-no-2 = "0" + substr(MobSub.CLI,3,3) + "-" +
                         substr(MobSub.CLI,6,3) + " " +
                         substr(MobSub.CLI,9).

         PUT STREAM EXCEL UNFORMATTED
            MobSub.MsSeq                       tab
            MobSub.CLI                        tab
            mi-no-2                             tab
            MobSub.ActivationDate format "99.99.9999" tab

            nocont FORMAT "NoContract/" tab
            climit FORMAT "CreditExc/"  tab

            Customer.CustNum        tab
            Customer.CustName       tab
            Customer.COName         tab
            Customer.Address        tab
            Customer.ZipCode        tab
            Customer.PostOffice     tab
            lcrateplan              TAB
            lcCustName              tab     /* User's LAST Name       */
            Customer.Address          tab     /* Address                */
            Customer.ZipCode          tab     /* Post Code              */
            Customer.PostOffice       tab     /* Post Office            */
            Customer.Email            tab     /* User's EMail           */
            Customer.Country          tab     /* Country code           */
            dbalance                tab     /* Unbilled Balance       */
            dlimit  .                      /* CreditInvNum Limit           */

         RUN uexskip(1).

      END.
   END.

   OUTPUT STREAM excel CLOSE.
   OUTPUT STREAM fraud CLOSE.
   IF NOT bbatch THEN DO:
      MESSAGE 
      "All Mobile Subscriber records were checked."  skip(1)
      amt1 "unsigned contracts   and"                SKIP
      amt2 "exceeded CreditInvNum limits were found."      skip(1)
      "Subscribers are listed in PaymFile"               SKIP
      outfile
      VIEW-AS ALERT-BOX INFORMATION TITLE
      " WATCHDOG RUN IS READY ".
   END. /* bbatch */
   LEAVE main.
END.
HIDE FRAME main NO-PAUSE.
HIDE MESSAGE.
