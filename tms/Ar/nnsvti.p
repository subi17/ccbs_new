/* ===========================================================================
 MODULE ........: nnsvti.p
 APPLICATION ...: nn
 TASK ..........: Creates direct debit invoice file FOR bank
 CREATED .......: 01.11.1999
 CHANGED .......: 27.11.2001 ht
                  03.12.2002/aam check direct debiting from Invoice.DDBankAcc
                  11.02.2003/aam for JG; check Invoice.cType etc. 
                  26.02.2003/aam bank account from invgroup.DDBankAcc,
                                 check and mark Invoice.DDState,
                                 handle credit invoices (as cancellations),
                                 payment subject from parameter DDPaymSubj
                  05.05.2003/aam check due date vrs. bank days (fBankDays)
                  30.05.2003/aam date to file name,
                                 move file to transfer dir 
                  03.06.2003/aam all invoicegroups with one run
                  15.09.2003/aam brand
                  20.10.2003/aam use original reference for cancellations
                  09.02.2004/aam fFormRefNum()
                  28.05.2004/aam SMS
                  10.08.2004/aam bank account from param DDebitBankAccount
                  29.11.2005/aam output file to Invoice.DDFile
                  24.01.2006/jt  DYNAMIC-FUNCTION("fDispCustName"
 VERSION .......: M15
 ============================================================================*/

{Syst/commali.i}
{Func/excel.i}
{Func/refcode.i}
{Func/cparam2.i}
{Func/ftransdir.i}
{Func/timestamp.i}
{Func/fbankday.i}
{Func/frefnum.i}
{Func/fmakesms.i}
{Func/fgettxt.i}

FUNCTION fYear2digit RETURNS INTEGER
   (INPUT yy AS INTEGER).

   DEF VAR yyy AS I NO-UNDO.

   yyy = yy.
   IF YYY >= 2000 THEN yyy = yyy - 2000.
                  ELSE yyy = yyy - 1900.
   RETURN yyy.               
END.

FUNCTION fViivaPois RETURNS CHARACTER
   (INPUT ptili  AS CHARACTER).

   DEF VAR paikka AS I  NO-UNDO.
   DEF VAR ptili2 AS C  NO-UNDO.

   paikka = 0.
   paikka = INDEX(ptili, "-").
   IF paikka NE 0 THEN DO:
      ptili2  = SUBSTR(ptili,1,paikka - 1) + SUBSTR(ptili,paikka + 1).
      RETURN ptili2.
   END.
   ELSE IF paikka = 0 THEN DO:
      RETURN ptili.
   END.
END.

DEF STREAM suoravel.

DEF VAR svtied      AS C  NO-UNDO.
DEF VAR rlkm        AS I  NO-UNDO.
DEF VAR lkm         AS I  NO-UNDO.
DEF VAR lkm0        AS I  NO-UNDO.
DEF VAR lkm1        AS I  NO-UNDO.
DEF VAR aloaika     AS C  NO-UNDO.
DEF VAR palvtunn    AS C  NO-UNDO.
DEF VAR parytunn    AS C  NO-UNDO.
DEF VAR cur         AS C  NO-UNDO.
DEF VAR kastunn     AS I  NO-UNDO.
DEF VAR raha0       AS I  NO-UNDO.
DEF VAR raha1       AS I  NO-UNDO.
DEF VAR la-ptili    AS C  NO-UNDO.
DEF VAR la-ptiliNum AS DE NO-UNDO.

DEF VAR typekoodi   AS I  NO-UNDO.
DEF VAR pviite      AS C  NO-UNDO.
DEF VAR viiteNum    AS C  NO-UNDO.
DEF VAR Customernro  AS I  FORMAT "zzzzzz9" NO-UNDO.
DEF VAR ptili       AS C  FORMAT "x(14)"   NO-UNDO INIT ?.
DEF VAR ptiliNum    AS DE NO-UNDO.
DEF VAR xPit        AS I  NO-UNDO.

DEF VAR ots-pvm     AS C    FORMAT "x(10)"            NO-UNDO.
DEF VAR i-date1     AS DA   FORMAT "99-99-99"         NO-UNDO.
DEF VAR i-date2     AS DA   FORMAT "99-99-99"         NO-UNDO.
DEF VAR lano1       AS I    FORMAT ">>>>>>>9"         NO-UNDO.
DEF VAR lano2       AS I    FORMAT ">>>>>>>9"         NO-UNDO.
DEF VAR asno1       AS I    FORMAT ">>>>>>>9"         NO-UNDO.
DEF VAR asno2       AS I    FORMAT ">>>>>>>9"         NO-UNDO.
DEF VAR lcInvGroup     LIKE    invgroup.InvGroup          NO-UNDO.
DEF VAR CustGroup     LIKE    custgroup.custgroup       NO-UNDO.
DEF VAR status1     AS I    FORMAT "9"                NO-UNDO.
DEF VAR status2     AS I    FORMAT "9"                NO-UNDO.
DEF VAR InvDate       AS DATE FORMAT "99-99-99"         NO-UNDO.
DEF VAR ok          AS LO   FORMAT "Kyllä/Ei"         NO-UNDO.
DEF VAR llok        AS LO                             NO-UNDO.

DEF VAR lcCode       AS CHAR  NO-UNDO. 
DEF VAR lcFrameField AS CHAR  NO-UNDO. 
DEF VAR llRerun      AS LOGIC NO-UNDO.
DEF VAR lcPaymSubj   AS CHAR  NO-UNDO. 
DEF VAR liCount      AS INT   NO-UNDO. 
DEF VAR ldtDueDate   AS DATE  NO-UNDO.
DEF VAR xBuAdd       AS CHAR  NO-UNDO.
DEF VAR xTransDir    AS CHAR  NO-UNDO.
DEF VAR xFileExt     AS CHAR  NO-UNDO.
DEF VAR liCnt        AS INT   NO-UNDO. 
DEF VAR lcFile       AS CHAR  NO-UNDO. 
DEF VAR liFileCnt    AS INT   NO-UNDO. 
DEF VAR liInvCnt     AS INT   NO-UNDO. 
DEF VAR lcPrevStart  AS CHAR  NO-UNDO. 
DEF VAR liTime       AS INT   NO-UNDO.
DEF VAR liInvNum     AS INT   NO-UNDO. 
DEF VAR liInvType    AS INT   NO-UNDO. 
DEF VAR lcSMSMessage AS CHAR  NO-UNDO. 
DEF VAR lcTxt        AS CHAR  NO-UNDO. 
DEF VAR lcBankAcc    AS CHAR  NO-UNDO. 
DEF VAR lcPlainFile  AS CHAR  NO-UNDO.
DEF VAR lcCustName   AS CHAR  NO-UNDO.

DEF STREAM invoice.

DEF BUFFER bInvoice  FOR Invoice.
DEF BUFFER bOriginal FOR Invoice.

DEF TEMP-TABLE ttBankAcc NO-UNDO
   FIELD BankAcc AS CHAR.
   
DEF TEMP-TABLE ttInv NO-UNDO
   FIELD InvNum  AS INT
   FIELD BankAcc AS CHAR
   INDEX BankAcc BankAcc InvNum.
   
              
ASSIGN svtied     = fCParamC("DDebitFileName")
       palvtunn   = fCParamC("DDebitServiceCode")
       lcPaymSubj = STRING(fCParamI("DDPaymSubj"))
       cur        = fCParamC("DefCurrency")
       xTransDir  = fCParamC("DDTransDir")
       xFileExt   = ".txt".
 
/* sms text */
lcSMSMessage = fGetTxt("SMS",
                       "suorav",
                       TODAY,
                       1).
                       
IF cur = "EUR" THEN cur = "1".             /*        1 = euro             */
ELSE                cur = " ".             /* " " ja 0 = markka           */

ots-pvm = string(pvm,"99.99.99").                   

lcBankAcc = fCParamC("DDebitBankAccount").

form
   SKIP(2)
"  INSTRUCTION: This program creates a direct debit file" skip
"               from invoices defined below:"
   SKIP(13)
   WITH TITLE COLOR value(ctc)
   " " + ynimi + " DIRECT DEBIT FILE CREATION " + ots-pvm + " "
COLOR value(cfc) width 80 OVERLAY FRAME taka.

form
   " Invoice group  ..........:" lcInvGroup  NO-LABEL FORMAT "X(10)"
      help "Invoice group to print, EMPTY = ALL"
      InvGroup.IgName no-label format "x(30)"SKIP

   " External Customer Group .:" CustGroup NO-LABEL
      help "Code of an External Customer Group; (Empty = NONE)"
      custgroup.cgname no-label  format "x(30)"  SKIP

   " Customer numbers ........:" asno1    NO-LABEL  
      help "Customers from number ..."
   "-" asno2 NO-LABEL 
      help "Customers to number ..." 
      VALIDATE(INPUT asno2 >= INPUT asno1,
               "Invalid definition") SKIP

   lano1 label " Invoice numbers ........."
      help "Invoices from number ..."
   "-" lano2 NO-LABEL 
      help "Invoices to number ..."  
      VALIDATE(INPUT lano2 >= INPUT lano1,
               "Invalid definition") SKIP

   i-date1 label " Invoice dates ..........."
      help "From date (INVOICE DATE) ..." 
      VALIDATE(INPUT i-date1 NE ?,
               "Date is mandatory")
   "-" i-date2 NO-LABEL
      help "To date (INVOICE DATE) ..." 
      VALIDATE(INPUT i-date2 NE ? AND INPUT i-date2 >= INPUT i-date1,
               "Invalid definition") 
      SKIP
   " Printing status .........:" status1  NO-LABEL AT 36
      help "Invoices from status code ..."
   "-" status2 NO-LABEL
      help "Invoices to status code ..."                            
      VALIDATE(INPUT status2 >= INPUT status1, 
               "Invalid definition")
      SKIP

   " Rerun of previous file ..:" llRerun NO-LABEL AT 36
      FORMAT "Yes/No"
      HELP "Pick invoices that have already been sent to DD"
      SKIP
      
with title color value(ctc) " DIRECT DEBIT INVOICE CRITERIA " side-labels
COLOR value(cfc) ROW 7 centered OVERLAY FRAME rajat.

cfc = "sel". RUN Syst/ufcolor. ccc = cfc.
view FRAME taka. PAUSE 0 no-message.

view FRAME rajat. view FRAME statu. PAUSE 0 no-message.

/* Haetaan pvm-ehdotus */
ASSIGN 
   i-date1  = pvm
   i-date2  = pvm

cfc = "lis". RUN Syst/ufcolor.
ehto = 9. RUN Syst/ufkey.

ASSIGN lano1 = 000000 lano2 = 99999999
       asno1 = 0      asno2 = 99999999.

VIEW FRAME taka. PAUSE 0 NO-MESSAGE.

disp "NONE" @ custgroup.cgname 
     "ALL"  @ invgroup.IgName 
     WITH FRAME rajat.

PAUSE 0 no-message.

LOOP:
repeat ON ENDKEY UNDO, NEXT:

   /* KysellAAn rajaukset */
   ehto = 9. RUN Syst/ufkey.
   PAUSE 0 no-message.
   
   REPEAT ON ENDKEY UNDO, LEAVE:
   
   UPDATE
      lcInvGroup
      CustGroup
      asno1    asno2
      lano1    lano2
      i-date1  i-date2
      status1  status2 
      llRerun
   WITH FRAME rajat EDITING:

      READKEY. nap = keylabel(LASTKEY).

      IF nap = "F9" AND 
         INDEX(FRAME-FIELD,"status") > 0 
      THEN DO:

         lcFrameField = FRAME-FIELD.
                  
         IF INDEX(FRAME-FIELD,"status") > 0
         THEN DO:
              
                     
            RUN Help/h-tmscodes.p(INPUT "Invoice",    /* TableName*/
                                 "PrintState", /* FieldName */
                                 "Report",     /* GroupCode */
                           OUTPUT lcCode).
              
            IF lcCode ne "" AND lcCode NE ?
            THEN DO WITH FRAME rajat:
                IF lcFrameField  = "Status1" 
                THEN DISPLAY INTEGER(lcCode) ;& Status1.
                ELSE DISPLAY INTEGER(lcCode) ;& Status2.
            END.
         END.
                
         ehto = 9.
         RUN Syst/ufkey.
         NEXT. 
      END.
       
      IF lookup(nap,poisnap) > 0 THEN DO:
      
         PAUSE 0.
         
         if frame-field = "lano2" THEN DO:
            ASSIGN INPUT lano1 INPUT lano2.
            IF INPUT lano2 = 0 THEN DO:
               lano2 = INPUT lano1.
               DISP lano1 @ lano2 WITH FRAME rajat.
               ASSIGN lano2.
            END.

            IF INPUT lano1 = INPUT lano2 THEN DO:

               FIND FIRST Invoice NO-LOCK where
                          Invoice.Brand   = gcBrand AND
                          Invoice.InvNum  = INPUT lano1.
               FIND FIRST Customer NO-LOCK where
                          Customer.CustNum = Invoice.CustNum.
               ASSIGN
                  lcInvGroup = Customer.InvGroup
                  i-date1 = Invoice.InvDate
                  i-date2 = Invoice.InvDate
                  asno1   = Customer.CustNum
                  asno2   = Customer.CustNum
                  status1 = Invoice.PrintState
                  status2 = Invoice.PrintState. 
               DISP 
                  lcInvGroup 
                  i-date1 i-date2
                  asno1 asno2 
                  status1 status2 
               WITH FRAME rajat.

            END.
         END.

         else if frame-field = "lcInvGroup" THEN DO:
             ASSIGN INPUT lcInvGroup.
             
             IF lcInvGroup = "" 
             THEN DISPLAY "ALL" @ invgroup.IgName WITH FRAME rajat.

             ELSE DO:
                Find invgroup where 
                     InvGroup.Brand   = gcBrand AND
                     invgroup.InvGroup = lcInvGroup 
                   NO-LOCK NO-ERROR.
                IF NOT AVAILABLE InvGroup THEN DO:
                   BELL.
                   message "UNKNOWN INVOICEGROUP !".
                   NEXT-PROMPT lcInvGroup.
                   NEXT.
                END.
                ELSE DISPLAY InvGroup.IgName WITH FRAME rajat.

            END. 
         END.

         else if frame-field = "CustGroup" THEN DO WITH FRAME rajat:
             ASSIGN INPUT FRAME rajat CustGroup.
             if CustGroup = "" then disp "NONE" @ custgroup.cgname.
             ELSE DO:
                FIND custgroup where 
                     CustGroup.Brand     = gcBrand AND
                     custgroup.custgroup = CustGroup 
                   NO-LOCK NO-ERROR.
                IF NOT AVAIL custgroup THEN DO:
                   BELL.
                   message "UNKNOWN EXTERNAL CUSTOMER GROUP !".
                   NEXT.
                END.
                DISP custgroup.cgname.
             END.   
         END.

         if frame-field = "asno2" THEN DO:
            ASSIGN INPUT asno1 INPUT asno2.
            IF INPUT asno2 = 0 THEN DO:
               asno2 = INPUT asno1.
               DISP asno1 @ asno2 WITH FRAME rajat.
               ASSIGN asno2.
            END.

         END.

      END.
      APPLY LASTKEY.
   END.
   
   LEAVE.
   
   END. 

   TOIMI:
   repeat WITH FRAME valinta ON ENDKEY UNDO toimi, NEXT toimi:
   
      ASSIGN
      ufk = 0 ufk[1] = 132 ufk[4] = 0  ufk[5] = 795 ufk[8] = 8 ehto = 0.
      IF lcBankAcc = "" THEN ufk[5] = 0.
      
      RUN Syst/ufkey.

      IF TOIMI = 1 THEN NEXT loop.

      ELSE IF TOIMI = 8 THEN DO:
         HIDE FRAME rajat NO-PAUSE.
         HIDE FRAME taka  NO-PAUSE.
         RETURN.
      END.
      
      ELSE IF TOIMI = 5 THEN DO:
      
         IF llRerun THEN DO:
            ok = FALSE.
            MESSAGE "You have chosen to pick invoices that have already"
                    "previously been sent to direct debit. Are you sure"
                    "that you want to continue ?"
            VIEW-AS ALERT-BOX
            QUESTION
            BUTTONS YES-NO
            SET ok.
            IF NOT ok THEN NEXT.
         END. 
            
         ok = FALSE.
         MESSAGE "Start creating a direct debit file ?"
         VIEW-AS ALERT-BOX 
         QUESTION
         BUTTONS YES-NO
         TITLE " CREATE A FILE "
         SET ok.
         IF OK THEN LEAVE LOOP.
         
      END.
      
   END. /* toimi */
END.  /* LOOP */

MESSAGE "Creating direct debit file ...".

FOR EACH Invoice NO-LOCK USE-INDEX InvDate WHERE
         Invoice.Brand   = gcBrand     AND
         Invoice.InvDate >= i-date1     AND
         Invoice.InvDate <= i-date2     AND
         Invoice.InvNum >= lano1       AND
         Invoice.InvNum <= lano2       AND
         /* charge type is direct debit */
         Invoice.ChargeType  = 2           AND 
         /* direct debiting was valid when invoice was formed */
         Invoice.DDBankAcc > ""       AND
         Invoice.CustNum    >= asno1   AND
         Invoice.CustNum    <= asno2   AND
         (IF Invoice.CrInvNum = 0 
          THEN Invoice.PrintState    >= status1 AND
               Invoice.PrintState    <= status2 
          ELSE TRUE)                 AND 
         Invoice.InvCfg[1] = FALSE   AND
         Invoice.InvAmt  NE 0       AND 
         (IF Invoice.CrInvNum = 0 OR Invoice.InvAmt > 0
          THEN Invoice.PaymState = 0 
          ELSE TRUE)                AND 
         /* check DD-status */
         (IF NOT llRerun
          THEN Invoice.DDState = 0
          ELSE TRUE),

   FIRST Customer of Invoice NO-LOCK:
   
      IF lcInvGroup > "" AND Customer.InvGroup NE lcInvGroup THEN NEXT.

      /* is an ext cust group selected ? */
      if CustGroup ne "" AND
         NOT can-find(first CGMember where 
                            CGMember.Brand     = gcBrand AND
                            CGMember.custgroup = CustGroup and
                            CGMember.custnum   = Customer.CustNum) 
      THEN NEXT. 

      CREATE ttInv.
      ASSIGN ttInv.InvNum  = Invoice.InvNum
             ttInv.BankAcc = lcBankAcc.
             
      IF NOT CAN-FIND(FIRST ttBankAcc WHERE 
                            ttBankAcc.BankAcc = lcBankAcc)
      THEN DO:
         CREATE ttBankAcc.
         ASSIGN ttBankAcc.BankAcc = lcBankAcc.
      END.
      
END.


FOR EACH ttBankAcc:
   
          /* laskuttajan tilistä viiva pois */
   ASSIGN la-ptili    = fViivaPois(ttBankAcc.BankAcc)   
          parytunn    = SUBSTRING(la-ptili,1,1)
          la-ptiliNum = DECIMAL(la-ptili)
          rlkm        = 0
          lkm         = 0
          lkm0        = 0
          lkm1        = 0
          raha0       = 0
          raha1       = 0
          typekoodi   = 50     /* tyyppikoodi: 50 = veloituspyyntö  */
          liTime      = TIME. 
          
   /* if more than one file per day -> time must be different */
   REPEAT:
      aloaika = SUBSTR(STRING(liTime,"HH:MM:SS"),1,2) +
                SUBSTR(STRING(liTime,"HH:MM:SS"),4,2).
      IF lcPrevStart = aloaika 
      THEN liTime = liTime + 60.
      ELSE LEAVE.
   END.
   
   /* only one bank account per file */
   ASSIGN lcFile      = svtied + 
                        STRING(DAY(TODAY),"99") +
                        STRING(MONTH(TODAY),"99") + 
                        STRING(YEAR(TODAY) MOD 100,"99")
          liCnt       = 0
          xBuAdd      = xFileExt
          lcPrevStart = aloaika. 

   /* check that the file doesn't exist */        
   REPEAT:                                                  
      IF SEARCH(lcFile + xBuAdd) = ? THEN LEAVE.
      ASSIGN liCnt  = liCnt + 1
             xBuAdd = string(liCnt) + xFileExt.
   END.

   lcFile = lcFile + xBuAdd.

   /* file without the path -> invoice */ 
   lcPlainFile = lcFile.
   liCnt = R-INDEX(lcPlainFile,"/").
   IF liCnt > 0 
   THEN lcPlainFile = SUBSTRING(lcPlainFile,liCnt + 1).
   
   OUTPUT STREAM suoravel TO VALUE(lcFile).

   FOR EACH ttInv WHERE
            ttInv.BankAcc = ttBankAcc.BankAcc,
      FIRST Invoice NO-LOCK WHERE
            Invoice.InvNum = ttInv.InvNum,
      FIRST Customer OF Invoice NO-LOCK:
      
      lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                     BUFFER Customer).
                                                       
      /* check how many bank days are left before due date,
         minimum is 4 bank days */
      IF Invoice.InvAmt > 0 AND fBankDays(Invoice.DueDate) < 4 
      THEN NEXT. 

      /* credit invoices are handled as cancellations */
      IF Invoice.InvAmt < 0 THEN DO:
         FIND bOriginal NO-LOCK WHERE
              bOriginal.InvNum = Invoice.CrInvNum NO-ERROR.
         IF NOT AVAILABLE bOriginal THEN NEXT.
         
            /* entire invoice must be credited */
         IF bOriginal.InvAmt NE -1 * Invoice.InvAmt OR
            /* original invoice has not been sent to dd  */
            bOriginal.DDState = 0 OR
            /* due date will expire tomorrow or earlier */
            bOriginal.DueDate <= TODAY + 1 
         THEN NEXT. 

         /* cancellation must be done at least two bank days before 
            original due date */
         IF fBankDays(bOriginal.DueDate) < 2 THEN NEXT. 
         
         ASSIGN kastunn    = 1
                ldtDueDate = bOriginal.DueDate
                liInvNum   = bOriginal.InvNum
                liInvType  = bOriginal.InvType.

      END. 
      
      /* normal debits */
      ELSE ASSIGN kastunn    = 0
                  ldtDueDate = Invoice.DueDate
                  liInvNum   = Invoice.InvNum
                  liInvType  = Invoice.InvType.

      ASSIGN rlkm    = rlkm + 1.

      /***************************************************
      * Jos löytyi ensimmäinen suoraveloituslasku,       *
      * tulostetaan erätietue ja sitten veloitustietueet *
      ***************************************************/
      IF rlkm = 1 THEN DO:

         /* Header */
         PUT STREAM suoravel UNFORMATTED
         "0"                                           /* tietuetunnus       */
         lkm                        FORMAT "999999"    /* tietueen järjnro   */
         fYear2digit(YEAR (today))  FORMAT "99"        /* kirjoituspvm:n vv  */
         MONTH(today)               FORMAT "99"        /*               kk   */
         DAY  (today)               FORMAT "99"        /*               pp   */
         aloaika                    FORMAT "x(4)"      /* aloitushetki       */
         palvtunn                   FORMAT "x(9)"      /* laskuttajan
                                                           palvelutunnus      */
         parytunn                   FORMAT "x(3)"      /* pankkiryhmän tunnus*/
         cur                        FORMAT "x"         /* rahayksikkö        */
         SPACE(90)
         my-nl2.

         lkm = lkm + 1.
      END.

      IF rlkm MOD 10 = 0 THEN DO:
         PAUSE 0.
         DISPLAY rlkm LABEL "Invoice qty" FORMAT ">>>>>>>9"
         WITH  OVERLAY ROW 3 centered 11 DOWN 
            TITLE " CREATING DIRECT DEBIT RECORDS "
            FRAME LOG.
      END.
       
      /* reference nbr */
      pviite = fFormRefNum(Customer.CustNum,
                           liInvNum,
                           liInvType).

      ASSIGN xPit = LENGTH(pviite)
             viiteNum = FILL("0",20 - xPit) + pviite.

      ptili    = fViivaPois(Invoice.DDBankAcc). 
      ptiliNum = DECIMAL(ptili).

      /* Detail */
      PUT STREAM suoravel UNFORMATTED
      "3"                                /* tietuetunnus:   3 = suoraveloitus */
      lkm                                FORMAT "999999" /* tietueen järj.nro */
      typekoodi                          FORMAT "99"     /* tyyppikoodi       */
      fyear2digit(YEAR (ldtDueDate))     FORMAT "99"     /* laskun eräpv:n vv */
      MONTH(ldtDueDate)                  FORMAT "99"     /*                kk */
      DAY  (ldtDueDate)                  FORMAT "99"     /*                pp */
      kastunn                            FORMAT "9"      /* käsittelytunnus   */
      ptiliNum                           FORMAT "99999999999999"
                                                         /* veloitustili      */
      ABS(100 * Invoice.InvAmt)        FORMAT "99999999999"  
                                                         /* veloituksen määrä */
      lcPaymSubj                         FORMAT "X(3)"   /* maksun aihe */
      la-ptiliNum                        FORMAT "99999999999999"
                                                         /* laskuttajan tili  */
      lcCustName                         FORMAT "x(19)"  /* maksajan nimi     */
      viiteNum                           FORMAT "x(20)"  /* viitenumero       */
      SPACE(23)
      my-nl2.

      lkm = lkm + 1.

      IF kastunn = 0 THEN DO:
         lkm0 = lkm0 + 1.
         raha0 = raha0 + 100 * Invoice.InvAmt.
      END.
      ELSE IF kastunn = 1 THEN DO:
         lkm1 = lkm1 + 1.
         raha1 = raha1 + ABS(100 * Invoice.InvAmt).
      END.
      
      /* mark invoice as sent */
      FIND bInvoice WHERE RECID(bInvoice) = RECID(Invoice) EXCLUSIVE-LOCK.
      ASSIGN bInvoice.DDState = 1
             bInvoice.DDFile  = lcPlainFile.

      /* sms */
      IF Customer.Language = 1 
      THEN lcTxt = lcSMSMessage.
      ELSE lcTxt = fGetTxt("SMS",
                           "suorav",
                           TODAY,
                           Customer.Language).
      IF lcTxt > "" AND Customer.SMSNumber > "" AND bInvoice.InvAmt > 0 
      THEN DO:
         ASSIGN lcTxt = REPLACE(lcTxt,"#BANKACC",Invoice.DDBankAcc)
                lcTxt = REPLACE(lcTxt,"#INVAMT",
                                     TRIM(STRING(Invoice.InvAmt,"->>>>>9.99")))
                lcTxt = REPLACE(lcTxt,"#DUEDATE",
                                     STRING(Invoice.DueDate,"99.99.99")).
                                     
         fMakeSchedSMS(Customer.CustNum,
                        Customer.SMSNumber,
                        8,
                        lcTxt,
                        fMakeTS()).
      END.

   END.  /* foreach ttInv */

   /**********************************************
   * Jos  löytyi yksikin lasku, joka kuului      *
   * rajaukseen, tulostetaan summatietue.        *
   **********************************************/
   IF lkm > 0 THEN DO:
      /* Trailer */
      PUT STREAM suoravel UNFORMATTED
      "9"                                   /* tietuetunnus              */
      lkm              FORMAT "999999"      /* veloitustietueet yhteensä */
      typekoodi        FORMAT "99"          /* tyyppikoodi               */
      lkm0             FORMAT "999999"      /* veloitukset kpl           */
      raha0            FORMAT "999999999999"/*             rahaa         */
      lkm1             FORMAT "999999"      /* hyvitykset  kpl           */
      raha1            FORMAT "999999999999"/*             rahaa         */
      SPACE(75)
      my-nl2.
   END.

   OUTPUT STREAM suoravel CLOSE.

   /* move the file to transfer dir */
   IF xTransDir NE ? AND xTransDir NE "" AND rlkm > 0 THEN DO:
      fTransDir(lcFile,
                xFileExt,
                xTransDir).
   END.

   ASSIGN liFileCnt = liFileCnt + 1
          liInvCnt  = liInvCnt  + rlkm.
          
END. /* foreach ttBankAcc */

MESSAGE 
   liInvCnt "invoices were written"           SKIP
   "into" liFileCnt "direct debit file(s)."   SKIP
   ""
   (IF xTransDir NE "" AND rlkm > 0
    THEN "Files were moved to directory " + xTransDir + "."
    ELSE "")
VIEW-AS ALERT-BOX TITLE " Done ".


HIDE FRAME LOG   NO-PAUSE. 
HIDE FRAME rajat NO-PAUSE.
HIDE FRAME taka  NO-PAUSE.
HIDE MESSAGE.

