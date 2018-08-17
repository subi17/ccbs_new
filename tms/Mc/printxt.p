/* ----------------------------------------------------------------------------
  MODULI .......: PRINTXT.P
  TEHTAVA ......: EPL-file or local print for Information Texts and Memos
  SOVELLUS .....: TMS
  TEKIJA .......: aam
  LUONTIPVM ....: 10.02.04
  MUUTOSPVM ....: 07.05.04/aam MainTitle
                  17.05.04/aam new form and title from #eplsheet
                  24.05.04/aam iiAddress=6 -> Order
                  16.06.04/aam sender from hdrtext (178)
                  09.11.04/aam margin, font, etc.
                  20.01.05/aam saldo limit from SubSer
                  22.02.05/aam icCLI can contain a list
                  02.08.05/aam sender address under logo
                  14.12.05/aam username from customer, not msowner
                  27.01.06/aam lastname/firstname tags
                  30.01.06/aam new print target 4; attachment letter
                  06.02.06/aam tag PCPIN
                  14.02.06/aam use automatically class 1 for foreign addresses
                  21.02.06/aam tag SERVICELIST etc.
                  17.03.06/aam payment plan and bank account tags
                  28.03.06/aam attachment in local printing
                  02.05.06/aam check if address data is empty
                  06.09.06/aam sender info with 12 rows/inch
                  21.11.06/aam OrderCustomer
                  01.12.06/aam new tags
                  09.03.07 kl  NOT lcCustEMail BEGINS "@"
                  31.05.07/aam new order related tags,
                               send confirmation as mail text (no attachments)
                  26.09.07/aam tags Campaign and Fatime (Order)
  VERSIO .......: yoigo
---------------------------------------------------------------------------- */

&GLOBAL-DEFINE LocalContactInfo YES
&GLOBAL-DEFINE SetLastFirstName YES
&GLOBAL-DEFINE ServiceListTag YES
&GLOBAL-DEFINE MailTitleSpaces Allow

{Syst/commali.i}
{Func/cparam2.i}
{Func/ftransdir.i}
{Func/email.i}
{Func/fdivtxt.i}
{Inv/edefine.i}
{Func/refcode.i}
{Func/fcustref.i}
{Syst/utumaa.i}
{Func/fsubser.i}
{Func/ftxttag.i}
{Func/feffect.i}
{Func/fconinfo.i}
{Func/fwidlst.i}
{Func/fcustdata.i}
{Func/forderstamp.i}
{Func/ftaxdata.i}
{Func/transname.i}
{Func/mdub.i}
{Mm/fbundle.i}
{Mc/offer.i}
{Func/fbankdata.i}
{Mnp/mnp.i}
{Syst/tmsconst.i}
{Func/fixedlinefunc.i}

DEF TEMP-TABLE wError NO-UNDO
    FIELD Cust   AS INT
    FIELD CLI    AS CHAR
    FIELD ErrMsg AS CHAR
    FIELD TxtID  AS CHAR.

DEF INPUT  PARAMETER iiCustNum     AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiMSSeq       AS INT  NO-UNDO.
DEF INPUT  PARAMETER icCLI         AS CHAR NO-UNDO.
DEF INPUT  PARAMETER iiTxtType     AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiAddress     AS INT  NO-UNDO.
DEF INPUT  PARAMETER icTable       AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icValue       AS CHAR NO-UNDO.
DEF INPUT  PARAMETER iiKey         AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiPrintTarget AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiLetterClass AS INT  NO-UNDO.
DEF OUTPUT PARAMETER ocErrFile     AS CHAR NO-UNDO.

 /* iiTxtType:
       1=inf.text, 2=memo
    iiAddress:
       1=use customer address
       2=inv.delivery addr.
       3=user customer
       4=owner
       5=invoice customer
       6=order
       7=already set in caller
    iiKey:
       Memo.MemoSeq / InvText.ITNum
    iiPrintTarget:
       1=epl (separate file),
       2=local,
       3=epl (cover sheet i.e. part of a larger file)
       4=epl (attachment i.e. printed in the end of another letter)
       5=local attachment
       6=email
*/

DEF VAR lcErr         AS CHAR NO-UNDO.
DEF VAR lcTermName    AS CHAR NO-UNDO.
DEF VAR lcEPLFile     AS CHAR NO-UNDO.
DEF VAR liCount       AS INT  NO-UNDO.
DEF VAR lcErrTxt      AS CHAR NO-UNDO.
DEF VAR llErrors      AS LOG  NO-UNDO.
DEF VAR lcText        AS CHAR NO-UNDO.

DEF VAR liLine        AS INT  NO-UNDO.
DEF VAR liPage        AS INT  NO-UNDO.
DEF VAR liEPLMaxLine  AS INT  NO-UNDO.
DEF VAR lcTitle       AS CHAR NO-UNDO.
DEF VAR lcDateHead    AS CHAR NO-UNDO.
DEF VAR lcCustHead    AS CHAR NO-UNDO.
DEF VAR liCustNum     AS INT  NO-UNDO.
DEF VAR lcMacros      AS CHAR NO-UNDO.
DEF VAR lcMacroDir    AS CHAR NO-UNDO.
DEF VAR lcMacroEff    AS CHAR NO-UNDO.
DEF VAR llNewSheet    AS LOG  NO-UNDO.
DEF VAR lcList        AS CHAR NO-UNDO.
DEF VAR liPos         AS INT  NO-UNDO.
DEF VAR lcMainTitle   AS CHAR NO-UNDO.
DEF VAR lcEntry       AS CHAR NO-UNDO.
DEF VAR liTxtChan     AS INT  NO-UNDO.
DEF VAR llConInfo     AS LOG  NO-UNDO.
DEF VAR lcFont        AS CHAR NO-UNDO.
DEF VAR liLanguage    AS INT  NO-UNDO.
DEF VAR llAttach      AS LOG  NO-UNDO.
DEF VAR liMargin      AS INT  NO-UNDO.
DEF VAR lcBasicFont   AS CHAR NO-UNDO.
DEF VAR lcGenForm     AS CHAR NO-UNDO.
DEF VAR llFirstRow    AS LOG  NO-UNDO.
DEF VAR liSaldoLimit  AS INT  NO-UNDO.
DEF VAR liType        AS INT  NO-UNDO.
DEF VAR ldtReqDate    AS DATE NO-UNDO.
DEF VAR liTime        AS INT  NO-UNDO.
DEF VAR llEPLPrint    AS LOG  NO-UNDO.
DEF VAR lcControl     AS CHAR NO-UNDO.
DEF VAR lcServiceLst  AS CHAR NO-UNDO.
DEF VAR lcDivTxt      AS CHAR NO-UNDO.
DEF VAR lcFinTxt      AS CHAR NO-UNDO.
DEF VAR liSmallLength AS INT  NO-UNDO.
DEF VAR llSmall       AS LOG  NO-UNDO.
DEF VAR lcDCEvent     AS CHAR NO-UNDO.
DEF VAR liPaymPlan    AS INT  NO-UNDO.
DEF VAR ldInvTot      AS DEC  NO-UNDO.
DEF VAR lcCurrentFont AS CHAR NO-UNDO.
DEF VAR liRequest     AS INT  NO-UNDO.
DEF VAR ldStamp       AS DEC  NO-UNDO.
DEF VAR ldtOrder      AS DATE NO-UNDO.
DEF VAR ldtEventDate  AS DATE NO-UNDO.
DEF VAR ldAmt         AS DEC  NO-UNDO.
DEF VAR lcCustEMail   AS CHAR NO-UNDO.
DEF VAR lcTransDir    AS CHAR NO-UNDO.
DEF VAR lcMsgFile     AS CHAR NO-UNDO.
DEF VAR ldVatPerc     AS DEC  NO-UNDO.
DEF VAR lcError       AS CHAR  NO-UNDO.
DEF VAR lcValue       AS CHAR NO-UNDO.
DEF VAR lcTopUpItems  AS CHAR NO-UNDO.
DEF VAR ldeFAT        AS DECIMAL NO-UNDO.
DEF VAR lcNotMinConList AS CHAR NO-UNDO.
DEF VAR liNeedSpaces  AS INT  NO-UNDO.
DEF VAR liTermMonths  AS INT  NO-UNDO.
DEF VAR lcBundle      AS CHAR NO-UNDO.
DEF VAR lcBundlesInfo AS CHAR NO-UNDO.
DEF VAR lcBundleInfo  AS CHAR NO-UNDO.
DEF VAR liNumEntries  AS INT  NO-UNDO.
DEF VAR ldeBundleFee  AS DEC  NO-UNDO.
DEF VAR lcBundleBillItem AS CHAR NO-UNDO.
DEF VAR ldeDeferredPayment AS DEC NO-UNDO.
DEF VAR lcTemp AS CHAR NO-UNDO.
DEF VAR lcTemp2 AS CHAR NO-UNDO.
DEF VAR ldeMonthlyFee AS DEC NO-UNDO.
DEF VAR liMonths AS INT NO-UNDO.
DEF VAR lcFATGroup     AS CHAR NO-UNDO.
DEF VAR llDSSPromotion AS LOG  NO-UNDO.
DEF VAR ldaDSSPromotionFromDate AS DATE NO-UNDO.
DEF VAR ldaDSSPromotionEndDate  AS DATE NO-UNDO.
DEF VAR liBonoTextID AS INT NO-UNDO.
DEF VAR liBonoPromoTextID AS INT NO-UNDO.
DEF VAR ldaPMDUBPromoStartDate AS DATE NO-UNDO.
DEF VAR lcBundleCLITypes AS CHAR NO-UNDO.
DEF VAR ldeFinalFee AS DEC NO-UNDO.
DEF VAR lcTariffType AS CHAR NO-UNDO.
DEF VAR liDelType AS INT NO-UNDO. 
DEF VAR ldeCommFee AS DEC NO-UNDO.
DEF VAR ldeRVPerc  AS DEC NO-UNDO.
DEF VAR lcProfession AS CHAR NO-UNDO.

{Func/faddress.i}

DEF STREAM slog.

DEF BUFFER bCLICust FOR Customer.
def buffer contactcustomer for ordercustomer.

FUNCTION fEPLPage RETURNS LOGICAL
    (iAddLine AS INT).

    IF liLine + iAddLine < liEPLMaxLine
    THEN RETURN FALSE.

    IF lcKutsu BEGINS "EPL5" AND llConInfo AND NOT llAttach THEN DO:
       /* contact info to channel 4 (only to front page) */
       fContactInfo().
    END.

    /* llNewSheet = new sheet forced */
    IF lcKutsu BEGINS "EPL5" AND NOT llNewSheet THEN DO:
       /* backside of the sheet */
       ASSIGN lcKutsu      = REPLACE(lcKutsu,"EPL5","EPL6")
              liEPLMaxLine = liCoverPage2.
    END.
    /* front side */
    ELSE ASSIGN lcKutsu      = REPLACE(lcKutsu,"EPL6","EPL5")
                liEPLMaxLine = liCoverPage1
                llNewSheet   = FALSE.

    ASSIGN liPage     = liPage + 1
           llFirstRow = TRUE.

    IF lcKutsu = "EPL" + lcSpecForm THEN llConInfo = FALSE.
    ELSE IF lcKutsu BEGINS "EPL5"   THEN llConInfo = TRUE.

    IF llAttach
    THEN lcRepFLine = "".
    ELSE lcRepFLine = (IF liTxtChan = 1 AND lcKutsu BEGINS "EPL5"
                       THEN FILL(" ",40) + STRING(lcMainTitle,"X(34)")
                       ELSE FILL(" ",74)) +
                      STRING(liPage).

    PUT STREAM ekirje UNFORMATTED
        "1H"
        lcRepFLine
        MY-NL
        lcKutsu
        MY-NL.

    IF NOT llAttach THEN DO:

       /* sender's address */
       IF lcKutsu BEGINS "EPL53504" THEN PUT STREAM ekirje UNFORMATTED
          "0J" FILL(" ",10)  lcBTName       MY-NL
          " J" FILL(" ",10)  lcHSender      MY-NL
          " J" FILL(" ",10)  lcBTCCAddr[1]  MY-NL
          " J" FILL(" ",10)  lcBTCCAddr[2]  MY-NL.

       ELSE PUT STREAM ekirje UNFORMATTED
          "-I" MY-NL
          "-I" MY-NL.
    END.

    /* main title and date */
    IF lcKutsu BEGINS "EPL5" AND NOT llAttach THEN DO:

       IF liTxtChan = 3 THEN
       PUT STREAM eKirje UNFORMATTED
          "5H"
          STRING(lcMainTitle,"X(35)")
          MY-NL
          "0I"
          lcDateHead
          " "
          STRING(TODAY,"99.99.9999")
          MY-NL.

    END.

    IF liTxtChan = 3 AND NOT llAttach THEN DO:
       /* use channel 3 for frontside, channel 1 for backside */
       IF lcKutsu BEGINS "EPL5" THEN PUT STREAM ekirje UNFORMATTED
           "3H"
           MY-NL.

       liLine = 1.
    END.
    ELSE liLine = 6.

    RETURN TRUE.

END FUNCTION.

FUNCTION fLocalPage RETURNS LOGICAL
    (iAddLine AS INT).

    IF iiPrintTarget = 6 THEN RETURN FALSE.

    IF liLine + iAddLine < skayt1
    THEN RETURN FALSE.

    IF liPage > 0 THEN DO:

       IF liLine < skayt1 THEN
          PUT STREAM tul SKIP(skayt1 - liLine).

       /* contact info to footer */
       fLocalContactInfo(lcCurrentFont,
                         (oso > "")).

       IF iiPrintTarget = 6 THEN DO:
          PUT STREAM tul skip(spit1 - liLine).
       END.
       ELSE DO:
          {Syst/uprfeed.i liLine}
       END.

    END.

    liPage = liPage + 1.

    IF oso > "" THEN DO:
       IF lcMacroEff > "" THEN
       PUT STREAM tul CONTROL lcMacroEff.

       IF lcMFontOn > "" THEN
       PUT STREAM tul CONTROL lcMFontOn.
    END.

    PUT STREAM tul
       SKIP(1)
       lcMainTitle AT 45 FORMAT "X(30)"
       liPage format ">>>" AT 76
       SKIP(2).

    /* don't print dateheader for attachment */
    IF iiPrintTarget = 5
    THEN PUT STREAM tul SKIP(2).
    ELSE PUT STREAM tul
       (IF lcDateHead > ""
        THEN lcDateHead + "  "
        ELSE "") +
       STRING(TODAY,"99.99.9999") AT 45 FORMAT "X(25)"
       SKIP(1).

    liLine = 6.

    IF liPage = 1 THEN DO:

       /* don't print sender, mainheader and receiver for attachment */
       IF iiPrintTarget NE 5 THEN DO:

          IF oso > "" THEN DO:
             PUT STREAM tul CONTROL lcSFontOn.

             /* 12 rows per inch */
             PUT STREAM tul CONTROL CHR(027) + "&l12D".

             PUT STREAM tul
                lcBTName       AT 15 FORMAT "X(30)" SKIP
                lcHSender      AT 15 FORMAT "X(30)" SKIP
                lcBTCCAddr[1]  AT 15 FORMAT "X(30)" SKIP
                lcBTCCAddr[2]  AT 15 FORMAT "X(30)" SKIP.

             /* 12 rows per inch -> doubles the qty of lines compared
                to normal */
             liLine = liLine + 2.

             /* 6 rows per inch */
             PUT STREAM tul CONTROL CHR(027) + "&l6D".

             IF lcSFontOff > "" THEN
             PUT STREAM tul CONTROL lcSFontOff.
          END.
          ELSE DO:

             PUT STREAM tul
                lcBTName       AT 6 FORMAT "X(30)" SKIP
                lcHSender      AT 6 FORMAT "X(30)" SKIP
                lcBTCCAddr[1]  AT 6 FORMAT "X(30)" SKIP
                lcBTCCAddr[2]  AT 6 FORMAT "X(30)" SKIP.

             liLine = liLine + 4.
          END.

          PUT STREAM tul
             SKIP(1)
             lcEPLRName               AT 6 FORMAT "X(30)" SKIP
             lcEPLRCOName             AT 6 FORMAT "X(30)" SKIP
             lcEPLRAddr               AT 6 FORMAT "X(30)" SKIP ""
            (IF LOOKUP(lcEPLACountry,",FI") = 0 AND
             lcEPLRCountry > ""
             THEN lcEPLACountry + "-"
             ELSE "") + lcEPLRPost    AT 6 FORMAT "X(30)" SKIP ""
            (IF LOOKUP(lcEPLRCountry,",FI,FIN,FINLAND") = 0
             THEN lcEPLRCountry
             ELSE "")                 AT 6 FORMAT "X(30)"
             SKIP(2).

          liLine = liLine + 8.
       END.
       ELSE DO:
          PUT STREAM tul SKIP(2).
          liLine = liLine + 2.
       END.

       IF oso > "" THEN DO:
          IF lcMFontOn > "" THEN PUT STREAM tul CONTROL lcMFontOn.
       END.
    END.
    ELSE DO:
       PUT STREAM tul SKIP(3).
       liLine = liLine + 3.
    END.

    /* return the last used font */
    IF oso > "" AND lcCurrentFont > "" AND lcCurrentFont NE lcMFontOn THEN
       PUT STREAM tul CONTROL lcCurrentFont.

    ELSE lcCurrentFont = lcMFontOn.

    RETURN TRUE.

END FUNCTION.

FUNCTION fErrLine RETURNS LOGICAL
    (iMessage AS CHAR).

    CREATE wError.
    ASSIGN wError.Cust   = iiCustNum
           wError.CLI    = lcTagCLI
           wError.ErrMsg = iMessage
           wError.TxtID  = IF lcMainTitle > ""
                           THEN lcMainTitle
                           ELSE IF lcTitle > ""
                                THEN lcTitle
                                ELSE STRING(iiTxtType) + "/" +
                                     STRING(iiKey)
           llErrors      = TRUE.

END FUNCTION.

FUNCTION fTxtSendLog RETURNS LOGIC
   (iiSendMethod AS INT):

   /* mark text as sent */
   CREATE ITSendLog.
   ASSIGN ITSendLog.Brand      = Syst.Var:gcBrand
          ITSendLog.TxtType    = iiTxtType
          ITSendLog.ITNum      = iiKey
          ITSendLog.CustNum    = iiCustNum
          ITSendLog.InvNum     = IF iiAddress = 6 AND AVAILABLE Order
                                 THEN Order.OrderID
                                 ELSE 0
          ITSendLog.SendMethod = iiSendMethod
          ITSendLog.EMail      = lcCustEMail
          ITSendLog.RepType    = IF iiTxtType = 1
                                 THEN (IF iiAddress = 6 AND AVAILABLE Order
                                       THEN "ITOrd"
                                       ELSE "IT")
                                 ELSE "Memo"
          ITSendLog.UserCode   = Syst.Var:katun.
          ITSendLog.SendStamp  = Func.Common:mMakeTS().
END.

FUNCTION fBIName RETURNS CHARACTER
   (idtDate AS DATE):

   DEF VAR lcBiName AS CHAR NO-UNDO.

   lcBiName = fTranslationName(Syst.Var:gcBrand,
                               1,
                               BillItem.BillCode,
                               liLanguage,
                               idtDate).

   IF lcBiName = "" OR lcBiName = ? THEN lcBiName = BillItem.BIName.

   RETURN lcBiName.

END FUNCTION.


ASSIGN llErrors     = FALSE
       liEPLMaxLine = liCoverPage1
       llNewSheet   = FALSE
       liTxtChan    = 3
       llConInfo    = TRUE
       lcBasicFont  = "I"
       llAttach     = FALSE
       llFirstRow   = FALSE
       lcControl    = " "
       llEPLPrint   = (iiPrintTarget = 1 OR
                       iiPrintTarget = 3 OR
                       iiPrintTarget = 4)
       ldtEventDate = TODAY
       ldaDSSPromotionFromDate = fCParamDa("DSSPromoFromDate")
       ldaDSSPromotionEndDate  = fCParamDa("DSSPromoEndDate")
       lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").

/* iimsseq may contain payment plan id */
IF icCLI = "PP" THEN ASSIGN
   liPaymPlan = iiMsSeq
   iiMsSeq    = 0
   icCLI      = "".
/* or a request id */
ELSE IF icCLI = "RQ" THEN ASSIGN
   liRequest  = iiMsSeq
   iiMsSeq    = 0
   icCLi      = "".

IF NOT AVAIL Company THEN DO:
   fErrLine("Company data is missing.").
END.
ELSE lcTagPhone = Company.Phone.

lcHSender = fTeksti(178,1).

liCount = 0.
FOR EACH BankAcc NO-LOCK WHERE
         BankAcc.Brand = Syst.Var:gcBrand:
   ASSIGN liCount         = liCount + 1
          lcBank[liCount] = STRING(BankAccount.BankOffice,"X(8)") + " " +
                            STRING(BankAcc.BankAccount,"X(15)").
   IF liCount >= 3 THEN LEAVE.
END.

/* initialize EPL */
IF llEPLPrint THEN DO:

   liSmallLength = 92.

   IF iiPrintTarget NE 3 AND iiPrintTarget NE 4 THEN DO:
      fEPLInit().

      lcEPLFile = fCParamC("EPLGenFile").

      IF lcEPLFile = "" OR lcEPLFile = ?
      THEN lcEPLFile = "/tmp/gen".
   END.

   /* make sure that values are valid */
   IF iiLetterClass = ? OR
      iiLetterClass > 4
   THEN iiLetterClass = 2.

   lcGenForm = fCParamC("EPLGenForm").
   IF lcGenForm = ? THEN lcGenForm = "".

   xEplForm = lcGenForm.

END.

/* email */
ELSE IF iiPrintTarget = 6 THEN DO:

   /* spool file and archive directory */
   ASSIGN lcEPLFile  = fCParam("Printing","MailPrintFile") + "_" +
                               STRING(TODAY,"999999") + "_" +
                               STRING(TIME) + ".rtf"
          lcTransDir = fCParam("Printing","MailArcDir")
          skayt1     = 298
          spit1      = 300.

   lcEPLFile = fEPLFileName(lcEPLFile).

   OUTPUT STREAM tul TO VALUE(lcEPLFile).
END.

/* macro for local printing */
ELSE DO:

   fInitEffects().

   ASSIGN lcMacroEff    = ""
          lcMacroDir    = fCParamC("MacroDir")
          lcMacros      = fCParamC("Macro" +
                                   IF iiTxtType = 2 THEN "Memo" ELSE "Info")
          iiLetterClass = 1
          liSmallLength = 105.

   IF lcMacros NE ? AND lcMacros > "" THEN DO:

      /* copy macros TO printer */
      RUN Syst/umakro.p(lcMacroDir + lcMacros).

      /* macro nbr is in it's name */
      liCount = INTEGER(SUBSTRING(lcMacros,LENGTH(lcMacros),1)) NO-ERROR.

      /* effect code is the same as macro nbr */
      IF liCount > 0 THEN
      FOR FIRST PrintCodes NO-LOCK WHERE
                PrintCodes.PrinterId = Syst.Var:TMSPrinter AND
                PrintCodes.Effect    = STRING(liCount):

          lcMacroEff = PrintCodes.EffOn[2].
      END.

   END.

END.

EMPTY TEMP-TABLE wError.

/* order */
IF iiAddress = 6 THEN DO:
   FIND Order WHERE
        Order.Brand   = Syst.Var:gcBrand AND
        Order.OrderID = iiCustNum NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Order THEN DO:
      fErrLine("Order " + STRING(iiCustNum) + " was not found.").
   END.

   iiCustNum = Order.CustNum.

   IF Order.MsSeq > 0 THEN DO:
      FIND MobSub WHERE MobSub.MsSeq = Order.MsSeq NO-LOCK NO-ERROR.
      IF AVAILABLE MobSub
      THEN ASSIGN iiMsSeq   = MobSub.MsSeq
                  iiCustNum = MobSub.CustNum.
   END.
END.

FIND FIRST Customer WHERE
           Customer.Brand   = Syst.Var:gcBrand AND
           Customer.CustNum = iiCustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN DO:
   IF iiAddress NE 6 THEN DO:
      fErrLine("Customer " + STRING(iiCustNum) + " was not found.").
   END.
   ELSE liLanguage = 1.
END.
ELSE DO:
   liLanguage = Customer.Language.
   IF Customer.CustNum = Customer.AgrCust THEN
      lcTagOwner = Func.Common:mDispCustName(BUFFER Customer).
   ELSE DO:
      FIND bOwner WHERE bOwner.CustNum = Customer.AgrCust NO-LOCK NO-ERROR.
      IF AVAILABLE bOwner THEN
         lcTagOwner = Func.Common:mDispCustName(BUFFER bOwner).
   END.
END.

IF iiMSSeq > 0 THEN DO:

   FIND FIRST MsOwner NO-LOCK USE-INDEX MSSeq WHERE
              MsOwner.MSSeq   = iiMSSeq AND
              MsOwner.CustNum = iiCustNum NO-ERROR.

   IF NOT AVAILABLE MsOwner AND icCLI > "" THEN
      FIND FIRST MsOwner NO-LOCK USE-INDEX MsSeq WHERE
                 MsOwner.MSSeq   = iiMSSeq AND
                 MsOwner.CLI     = icCLI NO-ERROR.

   IF NOT AVAILABLE MSOwner THEN DO:
      fErrLine("CLI was not found").
   END.
   ELSE DO:
      ASSIGN lcTagCLI     = MsOwner.CLI
             lcTagCLIType = MsOwner.CLIType.

      FIND bCLICust WHERE bCLICust.CustNum = MsOwner.CustNum NO-LOCK.
      lcTagUser = Func.Common:mDispCustName(BUFFER bCLICust).

      FIND MobSub WHERE MobSub.MsSeq = MsOwner.MsSeq NO-LOCK NO-ERROR.
   END.

END.
ELSE IF icCLI > "" THEN DO:

   lcTagCLI = icCLI.

   /* if a list is given then take username from first */
   IF NUM-ENTRIES(icCLI) > 1 THEN icCLI = ENTRY(1,icCLI).

   FOR FIRST MsOwner NO-LOCK WHERE
             MsOwner.CustNum = iiCustNum AND
             MsOwner.CLI     = icCLI,
      FIRST bCLICust NO-LOCK WHERE
            bCLICust.CustNum = MsOwner.CustNum:

      lcTagUser = Func.Common:mDispCustName(BUFFER bCLICust).
   END.

END.

IF NOT llErrors THEN DO:

   /* message text */
   IF iiTxtType = 2 THEN DO:

      FIND FIRST Memo NO-LOCK WHERE
                 Memo.Brand     = Syst.Var:gcBrand AND
                 Memo.HostTable = icTable AND
                 Memo.KeyValue  = icValue AND
                 Memo.MemoSeq   = iiKey NO-ERROR.

      IF NOT AVAILABLE Memo THEN DO:
         fErrLine("Memo for eLetter was not found").
      END.

      ELSE ASSIGN lcText  = Memo.MemoText
                  lcTitle = Memo.MemoTitle.
   END.

   ELSE IF iiTxtType <= 1 THEN DO:

      IF iiKey > 0
      THEN FIND InvText WHERE InvText.ITNum = iiKey NO-LOCK NO-ERROR.

      ELSE
      FIND FIRST InvText NO-LOCK WHERE
                 InvText.Brand     = Syst.Var:gcBrand  AND
                 InvText.Target    = icTable  AND
                 InvText.KeyValue  = icValue  AND
                 InvText.FromDate <= TODAY    AND
                 InvText.ToDate   >= TODAY    AND
                 InvText.Language  = liLanguage
                 NO-ERROR.

      IF NOT AVAILABLE InvText THEN DO:
         fErrLine("Information text for eLetter was not found").
      END.

      ELSE DO:
         IF InvText.EPLForm > "" THEN xEplForm = InvText.EPLForm.

         ASSIGN lcText      = InvText.InvText
                lcTitle     = InvText.TxtTitle
                lcMainTitle = InvText.MainTitle
                iiKey       = InvText.ITNum.

         IF iiLetterClass = 0 THEN iiLetterClass = InvText.LetterClass.
      END.
   END.

END.

IF iiLetterClass = 0 THEN iiLetterClass = 2.

IF llEPLPrint AND xEplForm = "" THEN DO:
   fErrLine("EPL form is not defined").
END.

IF NOT llErrors THEN DO:

   liCustNum = iiCustNum.


   /* order */
   IF iiAddress = 6 THEN DO:

      liCustNum = 0.
      
      Func.Common:mSplitTS(Order.CrStamp,
               OUTPUT ldtOrder,
               OUTPUT liTime).

      FIND FIRST OrderCustomer NO-LOCK WHERE
                 OrderCustomer.Brand   = Syst.Var:gcBrand       AND
                 OrderCustomer.OrderID = Order.OrderID AND
                 OrderCustomer.RowType = 1 NO-ERROR.

      DEF VAR ldeTaxPerc AS DEC NO-UNDO.
      IF AVAILABLE OrderCustomer THEN ASSIGN
         lcEPLRName      = Func.Common:mDispOrderName(BUFFER OrderCustomer)
         lcEPLRCoName    = ""
         lcEPLRAddr      = OrderCustomer.Address
         lcEPLRZipCode   = OrderCustomer.ZipCode
         lcEPLRPost      = OrderCustomer.ZipCode + " " +
                           OrderCustomer.PostOffice
         lcEPLRCountry   = ""
         lcTagLastName   = OrderCustomer.SurName1
         lcTagFirstName  = OrderCustomer.FirstName
         lcTagCLI        = Order.CLI
         lcTagUser       = lcEPLRName
         lcTagOwner      = lcEPLRName
         lcTagCustIDType = OrderCustomer.CustIDType
         lcTagCustID     = OrderCustomer.CustID
         lcTagBankAcc    = OrderCustomer.BankCode
         lcCustEMail     = OrderCustomer.EMail
         liDeltype       = OrderCustomer.Deltype
         ldeTaxPerc = fRegionTaxPerc(OrderCustomer.Region,
                                     "1",
                                     ldtOrder)
         lcTagContact1   = IF OrderCustomer.MobileNumber > ""
                           THEN OrderCustomer.MobileNumber
                           ELSE OrderCustomer.FixedNumber
         lcTagContact2   = IF OrderCustomer.MobileNumber > ""
                           THEN OrderCustomer.FixedNumber
                           ELSE "".

         ldeFAT = Order.FATAmount.
      
      IF OrderCustomer.Language > "" THEN
         liLanguage      = INTEGER(OrderCustomer.Language) NO-ERROR.
      /* Corporate customer */
      IF OrderCustomer.CustIDType EQ "CIF" THEN DO:
         ASSIGN
            lcTagCustIDType  = OrderCustomer.AuthCustIdType
            lcTagCustID      = OrderCustomer.AuthCustId
            lcTagCompanyName = OrderCustomer.Company
            lcTagCompanyCIF  = OrderCustomer.CustId
            lcTagLastName2   = OrderCustomer.SurName2
            lcTagCustEmail   = OrderCustomer.Email
            lcTagCompanyFoundDate =
               STRING(OrderCustomer.FoundationDate,"99-99-9999").

         FIND FIRST Region WHERE
            Region.Region = OrderCustomer.Region
         NO-LOCK NO-ERROR.

         IF AVAIL Region THEN
            lcTagCustRegion = Region.RgName.
         ELSE lcTagCustRegion = "".

         FIND FIRST ContactCustomer NO-LOCK WHERE
            ContactCustomer.Brand   = Order.Brand AND
            ContactCustomer.OrderId = Order.OrderID AND
            ContactCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_CIF_CONTACT}
         NO-ERROR.

         IF AVAIL ContactCustomer THEN DO:
            lcTagContactData =
            ContactCustomer.FirstName + " " +
            ContactCustomer.Surname1  + " " +
            ContactCustomer.Surname2  + CHR(10) +
            ContactCustomer.MobileNumber + CHR(10) +
            ContactCustomer.Email + CHR(10) +
            ContactCustomer.Address + " " + ContactCustomer.ZipCode + " " +
            ContactCustomer.PostOffice +  " " + lcTagCustRegion.

         END.
         ELSE lcTagContactData = fTeksti(302,liLanguage).
      END.

      /* format bank account */
      IF LENGTH(lcTagBankAcc) = 20 THEN
         lcTagBankAcc = SUBSTRING(lcTagBankAcc,1,4) +
                        " **** ** ******" +
                        SUBSTRING(lcTagBankAcc,17,4).
      ELSE IF LENGTH(lcTagBankAcc) = 24 THEN
         lcTagBankAcc = SUBSTRING(lcTagBankAcc,1,4) +
                        " **** **** ** ******" +
                        SUBSTRING(lcTagBankAcc,21,4).

      /* is there a separate user */
      IF Order.UserRole NE 1 THEN
      FOR FIRST OrderCustomer NO-LOCK WHERE
                OrderCustomer.Brand   = Syst.Var:gcBrand AND
                OrderCustomer.OrderID = Order.OrderID AND
                OrderCustomer.RowType = Order.UserRole:

         lcTagUser = Func.Common:mDispOrderName(BUFFER OrderCustomer).
      END.

      IF Order.DeliverySecure EQ 1 OR
         Order.DeliveryType EQ {&ORDER_DELTYPE_POST} THEN ASSIGN
         lcTagDelAddress = fTeksti(560,liLanguage)
         lcTagDelPost = "".
      ELSE DO:
         /* separate delivery address */
         FIND FIRST OrderCustomer NO-LOCK WHERE
                    OrderCustomer.Brand   = Syst.Var:gcBrand       AND
                    OrderCustomer.OrderID = Order.OrderID AND
                    OrderCustomer.RowType = 4 NO-ERROR.

         IF AVAILABLE OrderCustomer THEN DO:
            ASSIGN
               lcTagDelAddress = (IF Order.DeliveryType EQ {&ORDER_DELTYPE_KIALA}
                                  THEN OrderCustomer.Company + CHR(10)
                                  ELSE "") +
                                 OrderCustomer.Address
               lcTagDelPost    = OrderCustomer.ZipCode + " " +
                                 OrderCustomer.PostOffice.

            FIND FIRST OrderAction WHERE
                       OrderAction.Brand = Syst.Var:gcBrand AND
                       OrderAction.OrderId  = OrderCustomer.OrderId AND
                       OrderAction.ItemType = "UPSHours"
                       NO-LOCK NO-ERROR.
            IF AVAILABLE OrderAction THEN
               DO liCount = 1 TO NUM-ENTRIES(OrderAction.ItemKey,";"):
               lcTagUPSHours = lcTagUPSHours +
                               ENTRY(liCount,OrderAction.ItemKey,";") +
                               CHR(10).

               IF liLanguage = 5 THEN 
                  ASSIGN lcTagUPSHours = REPLACE(lcTagUPSHours, "Dom", "Sun")
                         lcTagUPSHours = REPLACE(lcTagUPSHours, "Lun", "Mon")
                         lcTagUPSHours = REPLACE(lcTagUPSHours, "Mar", "Tue")
                         lcTagUPSHours = REPLACE(lcTagUPSHours, "Mi" + CHR(233), "Wed")
                         lcTagUPSHours = REPLACE(lcTagUPSHours, "Jue", "Thu")
                         lcTagUPSHours = REPLACE(lcTagUPSHours, "Vie", "Fri")
                         lcTagUPSHours = REPLACE(lcTagUPSHours, "S" + CHR(225) + "b", "Sat").

            END.

         END.
         ELSE ASSIGN
            lcTagDelAddress = lcEPLRAddr
            lcTagDelPost    = lcEPLRPost
            lcTagUPSHours   = "".
      END.

      /* MGM */
      IF Order.Referee NE "" THEN DO:
         lcTagMGMReferee = fTeksti(306,liLanguage) + " " + Order.Referee.

      END.

      lcTagMGMTienda = fTeksti(307,liLanguage).

   END.

   /* if address has been set in calling procedure then first/lastname need
      to be set separately */
   ELSE IF iiAddress = 7 THEN DO:
      IF lcEPLRLast > "" THEN ASSIGN
        lcTagLastName  = lcEPLRLast
        lcTagFirstName = lcEPLRFirst.
      ELSE ASSIGN
         lcTagLastName  = Customer.CustName +
                          IF Customer.Category = "2" AND
                             Customer.COName > ""
                          THEN " " + Customer.COName
                          ELSE ""
         lcTagFirstName = Customer.FirstName.

      /* take only one firstname */
      IF INDEX(lcTagFirstName," ") > 1
      THEN lcTagFirstName = SUBSTRING(lcTagFirstName,1,
                                      INDEX(lcTagFirstName," ") - 1).
   END.

   ELSE DO:
      /* address for letter */
      fTargetAddress(iiAddress).

      IF Customer.AgrCust NE Customer.CustNum THEN DO:
         IF iiAddress = 4 THEN liCustNum = Customer.AgrCust.
      END.

   END.

   liTagCustNum = liCustNum.

   /* if foreign address then use automatically 1. class
      (possible only if this is printed separately, into own file) */
   IF iiPrintTarget = 1 AND
      LOOKUP(lcEPLRCountry,"FI,FIN,FINLAND,") = 0
   THEN iiLetterClass = 1.

   /* check that address is valid */
   lcErrTxt = fEPLCheckAddr(iiLetterClass,
                            lcEPLRName,
                            lcEPLRZipCode,
                            lcEPLRCountry,
                            OUTPUT lcEPLACountry).

   IF lcErrTxt = "" THEN DO:
      IF lcEPLRAddr = "" AND lcEPLRZipCode = "" AND lcEPLRPost = "" THEN
         lcErrTxt = "Address data is missing".
   END.

   /* tags for order */
   IF iiAddress = 6 AND AVAILABLE Order THEN DO:

      IF ldtOrder NE ? THEN ASSIGN
         lcText       = REPLACE(lcText,"#ODATE",STRING(ldtOrder,"99.99.9999"))
         ldtEventDate = ldtOrder.

      ASSIGN lcText   = REPLACE(lcText,"#OCONTRID",Order.ContractID)
             ldInvTot = -1.

      IF Order.CLIType > "" THEN lcTagCLIType = Order.CLIType.

      IF INDEX(lcText,"#ORDER_DATE") > 0 THEN DO:

         DEF VAR lcDate AS CHAR NO-UNDO.
         DEF VAR lcMonth AS CHAR NO-UNDO.

         lcMonth = fTeksti(542 + MONTH(ldtOrder),(IF liLanguage EQ 5 THEN 5 ELSE 1)).
         lcDate = SUBST("&1 &2 &3", DAY(ldtOrder), lcMonth, YEAR(ldtOrder)).

         lcText = REPLACE(lcText, "#ORDER_DATE", lcDate).
      END.

      IF INDEX(lcText,"#DELIVERY_DATE") > 0 THEN DO:

         DEF VAR liDays AS INT NO-UNDO.
         DEF VAR ldaDate AS DATE NO-UNDO.

         FIND FIRST OrderCustomer OF Order NO-LOCK WHERE
                    OrderCustomer.RowType = 1 NO-ERROR.
         IF AVAIL OrderCustomer THEN CASE OrderCustomer.Region:
            WHEN "07" THEN liDays = 5.
            WHEN "38" OR WHEN "35" OR WHEN "51" OR WHEN "52" THEN liDays = 7.
            OTHERWISE liDays = 3.
         END.
         ELSE liDays = 3.

         ldaDate = TODAY.
         DO liCount = 1 TO liDays:
            ldaDate = ldaDate + 1.
            ldaDate = fMNPHoliday(ldaDate,TRUE).
         END.

         lcText = REPLACE(lcText,"#DELIVERY_DATE",Func.Common:mDateFmt(ldaDate,"dd/mm/yy")).
      END.


      IF INDEX(lcText,"#MNP_DATE") > 0 THEN DO:

         DEF VAR ldaMNP AS DATE NO-UNDO.
         DEF VAR ldePortingTime AS DEC NO-UNDO. 
         DEF VAR lcProduct AS CHAR NO-UNDO. 

         FIND FIRST OrderCustomer OF Order NO-LOCK WHERE
                    OrderCustomer.RowType = 1.
         
         IF Order.PortingDate <> ? THEN
            ldePortingTime = Func.Common:mMake2DT(Order.PortingDate,0).

         FIND FIRST OrderAccessory NO-LOCK WHERE
                    OrderAccessory.Brand = Syst.Var:gcBrand AND
                    OrderAccessory.OrderId = Order.OrderID AND
                    OrderAccessory.TerminalType = {&TERMINAL_TYPE_PHONE}
                    NO-ERROR.
         IF AVAIL OrderAccessory THEN
            lcProduct = "T".
         ELSE
            lcProduct = "S".
         
         lcTariffType = fGetDataBundleInOrderAction(Order.OrderId,
                                                    Order.CLIType).
         IF lcTariffType = "" THEN
            lcTariffType = Order.CLIType.

         
         IF ldePortingTime <= Func.Common:mMakeTS() THEN
            ldamnp = fmnpchangewindowdate(
                                Func.Common:mMakeTS(),
                                order.orderchannel,
                                ordercustomer.region,
                                lcProduct,
                                lcTariffType,
                                Order.DeliveryType).
         ELSE ldaMNP = Order.PortingDate.

         lcMonth = fTeksti(542 + MONTH(ldaMNP),(IF liLanguage EQ 5 THEN 5 ELSE 1)).
         lcList = SUBST("&1 &2 &3", DAY(ldaMNP), lcMonth, YEAR(ldaMNP)).

         lcText = REPLACE(lcText, "#MNP_DATE", lcList + " 02:00").

      END.

      IF INDEX(lcText,"#SERVICE_AVAILABLE") > 0 THEN DO:
         IF Order.OrderType EQ {&ORDER_TYPE_NEW} THEN
            lcList = fTeksti((IF Order.ResignationPeriod
                              THEN 563 ELSE 562), liLanguage).
         ELSE lcList = "".

         lcText = REPLACE(lcText, "#SERVICE_AVAILABLE", lcList).
      END.

      IF INDEX(lcText,"#CUSTJOB") > 0 THEN DO:
         lcList = "".
         IF AVAIL OrderCustomer AND OrderCustomer.Profession > "" THEN DO:
            lcProfession = fGetItemName(Syst.Var:gcBrand,"Profession",OrderCustomer.Profession,
                                        liLanguage,ldtOrder).
            IF liLanguage = 5 THEN
               lcList = "Job: " + lcProfession.
            ELSE
               lcList = "Empleo: " + lcProfession.
         END.
         lcText = REPLACE(lcText, "#CUSTJOB", lcList).
      END.

      IF INDEX(lcText,"#CUSTCOMPANY") > 0 THEN DO:
         lcList = "".
         IF AVAIL OrderCustomer AND OrderCustomer.Company > "" THEN DO:
            IF liLanguage = 5 THEN
               lcList = "Company: " + OrderCustomer.Company.
            ELSE
               lcList = "Empresa: " + OrderCustomer.Company.
         END.
         lcText = REPLACE(lcText, "#CUSTCOMPANY", lcList).
      END.
      
      IF INDEX(lcText,"#OFEES") > 0 THEN DO:

         ASSIGN
            lcList       = ""
            ldInvTot     = 0
            lcTopupItems = fCParamC("OrderTopUp") + "," +
                           fCParamC("OrderTopUpDisc").

         ldeDeferredPayment = fGetOfferDeferredPayment(Order.Offer,
                                                       Order.CrStamp,
                                                       OUTPUT ldeMonthlyFee,
                                                       OUTPUT liMonths,
                                                       OUTPUT ldeFinalFee).
         IF ldeFinalFee = ? THEN ldeFinalFee = 0.

         /* cash invoice has already been created */
         IF Order.InvNum > 0 THEN
         FOR FIRST Invoice NO-LOCK WHERE
                   Invoice.InvNum = Order.InvNum,
              EACH InvRow OF Invoice NO-LOCK,
             FIRST BillItem NO-LOCK WHERE
                   BillItem.Brand    = Syst.Var:gcBrand AND
                   BillItem.BillCode = InvRow.BillCode
         BY InvRow.InvRowNum:

            /* campaign topups are not displayed */
            IF LOOKUP(InvRow.BillCode,lcTopUpItems) > 0 THEN NEXT.

            /* amount with vat */
            ldAmt = InvRow.Amt.
            IF Invoice.VatIncl = FALSE THEN
               ldAmt = ROUND(ldAmt * (1 + InvRow.VatPerc / 100),2).

            /* billing item name */
            lcValue = fBIName(IF InvRow.ToDate NE ?
                              THEN InvRow.ToDate
                              ELSE Invoice.ToDate).

            /* Check if TopUpScheme has DisplayAmount to show */
            IF Order.CliType BEGINS "TARJ7" OR
               Order.CliType BEGINS "TARJ9" OR
               Order.CliType BEGINS "TARJ10" OR
               Order.CliType BEGINS "TARJ11" OR 
               Order.CliType BEGINS "TARJ12" OR
               Order.CliType BEGINS "TARJ13" THEN
               FOR EACH TopUpSchemeRow NO-LOCK WHERE
                        TopUpSchemeRow.BillCode = InvRow.BillCode AND
                        TopUpSchemeRow.Amount = InvRow.Amt:
                  IF TopUpSchemeRow.DisplayAmount > 0 THEN
                     ldAmt = TopUpSchemeRow.DisplayAmount.
               END.

            ASSIGN
               ldInvTot = ldInvTot + ldAmt
               ldAmt = ldAmt + ldeDeferredPayment + ldeFinalFee
                  WHEN BillItem.BiGroup EQ {&BITEM_GRP_TERMINAL} AND
                       ldeDeferredPayment > 0
               lcList   = lcList + STRING(lcValue,"X(40)") +
                                   STRING(ldAmt,"->>>>>9.99") +
                                   " EUR" + CHR(10).
         END.

         /* fees have been created but amounts have been zero */
         ELSE IF
            CAN-FIND(FIRST SingleFee USE-INDEX HostTable WHERE
                           SingleFee.Brand     = Syst.Var:gcBrand AND
                           SingleFee.HostTable = "Order" AND
                           SingleFee.KeyValue  = STRING(Order.OrderID) AND
                           SingleFee.CalcObj   = "CASHFEE")
         THEN DO:
            FOR EACH SingleFee USE-INDEX HostTable WHERE
                     SingleFee.Brand     = Syst.Var:gcBrand AND
                     SingleFee.HostTable = "Order" AND
                     SingleFee.KeyValue  = STRING(Order.OrderID) AND
                     SingleFee.CalcObj   = "CASHFEE",
               FIRST BillItem NO-LOCK WHERE
                     BillItem.Brand    = Syst.Var:gcBrand AND
                     BillItem.BillCode = SingleFee.BillCode
               BY SingleFee.BillCode:

               /* campaign topups are not displayed */
               IF LOOKUP(SingleFee.BillCode,lcTopUpItems) > 0 THEN NEXT.

               /* amount with vat */
               ldAmt = SingleFee.Amt.
               IF SingleFee.VatIncl = FALSE THEN DO:
                  FIND FIRST OrderCustomer OF Order NO-LOCK WHERE
                             OrderCustomer.RowType = 1 NO-ERROR.
                  ldVatPerc = fRegionTaxPerc(IF AVAILABLE OrderCustomer
                                             THEN OrderCustomer.Region
                                             ELSE "",
                                             BillItem.TaxClass,
                                             ldtOrder).
                  ldAmt = ROUND(ldAmt * (1 + ldVatPerc / 100),2).
               END.

               /* billing item name */
               lcValue = fBIName(ldtEventDate).

               ASSIGN
                  ldInvTot = ldInvTot + ldAmt
                  ldAmt = ldAmt + ldeDeferredPayment + ldeFinalFee
                     WHEN BillItem.BiGroup EQ {&BITEM_GRP_TERMINAL} AND
                          ldeDeferredPayment > 0
                  lcList   = lcList + STRING(lcValue,"X(40)") +
                                      STRING(ldAmt,"->>>>>9.99") +
                                     " EUR" + CHR(10).
            END.
         END.

         /* fees have not been created yet */
         ELSE DO:
            /* make virtual creation */
            RUN Mc/cashfee.p (Order.OrderID,
                         3,     /* leave out campaign topups */
                         OUTPUT lcList,
                         OUTPUT ldInvTot,
                         OUTPUT lcError).
            IF lcError <> "" THEN
               Func.Common:mWriteMemo("Order",
                                STRING(Order.OrderID),
                                0,
                                "Confirmation Email Cash Invoice Data Failed",
                                lcError).
         END.

         FOR FIRST OfferItem WHERE
                   OfferItem.Brand       = Syst.Var:gcBrand          AND
                   OfferItem.Offer       = Order.Offer      AND
                   OfferItem.ItemType    = "ServicePackage" AND
                   OfferItem.ItemKey     = "BB"             AND
                   OfferItem.EndStamp   >= Order.CrStamp    AND
                   OfferItem.BeginStamp <= Order.CrStamp NO-LOCK:

             lcList  = lcList + STRING(fTeksti(513,liLanguage),"X(40)") +
                       STRING(OfferItem.Amount,"->>>>>9.99") +
                       " EUR" + CHR(10).
         END. /* FOR FIRST OfferItem WHERE */

         /* Bundle Information in product list */
         RUN pGetBundleInfo(INPUT Order.OrderID, OUTPUT lcBundlesInfo).

         liNumEntries = NUM-ENTRIES(lcBundlesInfo).
         DO liCount = 1 TO liNumEntries:
            lcBundleInfo = ENTRY(liCount,lcBundlesInfo).
            IF lcBundleInfo > "" AND NUM-ENTRIES(lcBundleInfo,"=") = 2 THEN DO:
               ASSIGN lcBundle     = ENTRY(1,lcBundleInfo,"=")
                      ldeBundleFee = DECIMAL(ENTRY(2,lcBundleInfo,"=")) NO-ERROR.
               IF ERROR-STATUS:ERROR OR ldeBundleFee = ? THEN ldeBundleFee = 0.

               CASE lcBundle:
                  WHEN {&DSS} THEN
                     lcList = lcList + STRING(fTeksti(526,liLanguage),"X(40)") +
                              STRING(ldeBundleFee,"->>>>>9.99") + " EUR" + CHR(10).
                  WHEN "MDUB2" OR WHEN "MDUB3" OR WHEN "MDUB4" OR
                  WHEN "CONTDATA" OR WHEN "CONTD2" OR WHEN "CONTD4" THEN
                     llDSSPromotion = TRUE.
                  OTHERWISE NEXT.
               END CASE. /* CASE lcBundle: */
            END. /* IF lcBundleInfo > "" THEN DO: */
         END. /* DO liCount = 1 TO liNumEntries: */

         /* YDR-470 - DSS Free Promotion */
         IF INDEX(lcBundlesInfo,{&DSS}) > 0 AND llDSSPromotion AND
            ldtOrder >= ldaDSSPromotionFromDate AND
            ldtOrder <= ldaDSSPromotionEndDate THEN
            lcList = lcList + fTeksti(531,liLanguage) + CHR(10).

         /* total row */
         IF lcList > "" THEN
            lcList = lcList + (IF ldeDeferredPayment > 0 THEN CHR(10) ELSE "") +
                              STRING(fTeksti((IF ldeDeferredPayment > 0 THEN
                                              515 ELSE 80),liLanguage),"X(40)") +
                              STRING(ldInvTot,"->>>>>9.99") +
                              " EUR".

         IF ldeDeferredPayment > 0 THEN DO:
            IF ldeFinalFee > 0 THEN
               lcTemp = fTeksti(556,liLanguage).
            ELSE
               lcTemp = fTeksti(516,liLanguage).

            ASSIGN
               lcTemp = REPLACE(lcTemp,"#AMOUNT",
                             TRIM(STRING(ldeMonthlyFee,"->>>>>9.99")))
               lcTemp = REPLACE(lcTemp,"#MONTHS", STRING(liMonths))
               lcTemp = REPLACE(lcTemp,"#XX", STRING(liMonths + 1))
               lcTemp2 = "".

            /* split to fixed with lines */
            DO liPos = 1 to num-entries(lcTemp,CHR(10)):
               lcTemp2 = lcTemp2 + CHR(10) +
                         STRING(ENTRY(liPos,lcTemp,CHR(10)),"X(40)").
               IF liPos = 2 THEN
                  lcTemp2 = lcTemp2 +
                            STRING(ldeDeferredPayment,"->>>>>9.99") + " EUR".
               ELSE IF liPos = 3 AND ldeFinalFee > 0 THEN
                  lcTemp2 = lcTemp2 +
                            STRING(ldeFinalFee,"->>>>>9.99") + " EUR".
            END.
            lcList = lcList + lcTemp2.
         END.

         /* YDR-737 & YDR-1065 */
         IF INDEX(lcList,"iPhone") > 0 AND Order.CrStamp < 20130701 THEN
            FOR FIRST OfferItem NO-LOCK WHERE
                      OfferItem.Brand = Syst.Var:gcBrand AND
                      OfferItem.Offer = Order.Offer AND
                      OfferItem.ItemType = "Percontract" AND
                      OfferItem.ItemKey = "PAYTERM24_25" AND
                      OfferItem.EndStamp >= Order.CrStamp AND
                      OfferItem.BeginStamp <= Order.CrStamp:
               lcTemp = fTeksti(534,liLanguage).
               IF lcTemp > "" THEN
                  lcList = lcList + CHR(10) + CHR(10) + lcTemp.
            END.

         lcText = REPLACE(lcText,"#OFEES",lcList).

      END.

      /* not minimum consuption for some prepaid in campaign */
      IF INDEX(lcText,"#CAMPMINCON") > 0 THEN DO:
         lcList = "".
         lcNotMinConList = fCParamC("CLITypeCampMinCon").
         IF LOOKUP(Order.CLIType,lcNotMinConList) > 0 THEN DO:
            lcList =  CHR(10) + fTeksti(371,liLanguage).
         END.
         lcText = REPLACE(lcText,"#CAMPMINCON",lcList).
      END.

      /* terminal purchased */
      IF INDEX(lcText,"#PENALTYFEE") > 0 THEN DO:
         lcList = "".
           
         IF fIsConvergenceTariff(Order.CLIType) AND
           (Order.OrderType EQ {&ORDER_TYPE_NEW} OR
            Order.OrderType EQ {&ORDER_TYPE_MNP} OR
           (Order.OrderType EQ {&ORDER_TYPE_STC} AND
            AVAIL Mobsub AND
            NOT (fIsConvergenceTariff(Mobsub.CLIType) OR
             LOOKUP(MobSub.CLIType,{&MOBSUB_CLITYPE_FUSION}) > 0)))
         THEN lcList = CHR(10) + fTeksti(532,liLanguage).

         /*YCO-279 + refactoring text 532 writing*/
         /*before this 532 was hardcoded with 100E + 12 months*/
         lcErr =  fSelectFTERMFee(Order.OrderId,
                                  OUTPUT ldAmt,
                                  OUTPUT lcTermName).
         IF lcErr EQ "" THEN
            lcList = REPLACE(lcList,"#AMOUNT",STRING(ldAmt)).
         ELSE
            lcList = REPLACE(lcList,"#AMOUNT",STRING(100)).


         RUN Mc/offer_penaltyfee.p(Order.OrderID,
                              Output liTermMonths,
                              OUTPUT ldAmt).

         IF ldAmt NE 0 AND Order.PayType = FALSE THEN DO:

            lcList = lcList + CHR(10) + fTeksti(510,liLanguage).

            assign lcList = REPLACE(lcList,"#xxx",TRIM(STRING(ldAmt,"->>>>>9")))
                   lcList = REPLACE(lcList,"#yy",TRIM(STRING(liTermMonths))).
         END. /* IF ldAmt NE 0 AND Order.PayType = FALSE THEN DO: */

         IF CAN-FIND(FIRST CLIType WHERE
                           CLIType.Brand = Syst.Var:gcBrand AND
                           CLIType.CLItype = Order.CliType AND
                           ClIType.LineType > 0) THEN DO:

            IF LOOKUP(Order.CLIType,lcBundleCLITypes) > 0 THEN
               lcTariffType = fGetDataBundleInOrderAction(Order.OrderId,
                                                          Order.CLIType).
            ELSE lcTariffType = Order.CLIType.

            IF CAN-FIND(FIRST CLIType NO-LOCK WHERE
                              CLIType.Brand = Syst.Var:gcBrand AND
                              CLIType.CLItype = lcTariffType AND
                              CLItype.LineType = {&CLITYPE_LINETYPE_ADDITIONAL}) THEN
               lcList = lcList + CHR(10) + fTeksti(539,liLanguage).
         END.

         lcText = REPLACE(lcText,"#PENALTYFEE",lcList).
      END.

      /* YDR-328 */
      IF INDEX(lcText,"#LEGALFINANCING") > 0 THEN DO:
         lcList = "".
         IF ldeDeferredPayment > 0 THEN DO:
            FIND FIRST OrderCustomer OF Order NO-LOCK WHERE
                       OrderCustomer.RowType = 1 NO-ERROR.
            IF AVAIL OrderCustomer AND OrderCustomer.CustIdType = "CIF"
            THEN DO:
               IF ldeFinalFee > 0 THEN
                  ASSIGN lcList = CHR(10) + fTeksti(555,liLanguage)
                         lcList = REPLACE(lcList,"#XX",STRING(liMonths + 1)).
               ELSE
                  lcList = CHR(10) + fTeksti(517,liLanguage).
            END.
            ELSE DO:
               ldeRVPerc = TRUNC(ldeFinalFee / 
                                 (ldeDeferredPayment + ldeFinalFee) * 100 + 0.05,1).
               FIND FIRST TFConf NO-LOCK WHERE
                          TFConf.RVPercentage = ldeRVPerc AND
                          TFConf.ValidTo >= ldtOrder AND
                          TFConf.ValidFrom <= ldtOrder NO-ERROR.
               IF NOT AVAIL TFConf THEN
                  lcList = "".
               ELSE DO:
                  ldeCommFee = ROUND((TFConf.CommFeePerc / 100) * 
                                     (ldeDeferredPayment + ldeFinalFee),2).
                  IF ldeFinalFee > 0 THEN
                     lcList = CHR(10) + fTeksti(566,liLanguage).
                  ELSE
                     lcList = CHR(10) + fTeksti(567,liLanguage).

                  ASSIGN lcList = REPLACE(lcList,"#LOAN_FEE",TRIM(STRING(ldeDeferredPayment + ldeFinalFee,"->>9.99")))
                         lcList = REPLACE(lcList,"#TAE",TRIM(STRING(TFConf.TAE,"9.99")))
                         lcList = REPLACE(lcList,"#CF_PER",TRIM(STRING(TFConf.CommFeePerc,"9.99")))
                         lcList = REPLACE(lcList,"#CF_FEE",TRIM(STRING(ldeCommFee,"->>9.99")))
                         lcList = REPLACE(lcList,"#TOTAL_FEE",TRIM(STRING(ldeDeferredPayment + ldeFinalFee + ldeCommFee,"->>9.99"))).
               END.
            END.
         END.
         lcText = REPLACE(lcText,"#LEGALFINANCING",lcList).
      END.

      IF Order.MNPStatus EQ 0 THEN
         lcText = REPLACE(lcText,"#SERVICE_AVAILABLE",
           fTeksti((IF Order.ResignationPeriod THEN 563 ELSE 562), liLanguage)).
      ELSE lcText = REPLACE(lcText,"#SERVICE_AVAILABLE", "").

      /* MDUB */
      IF INDEX(lcText,"#MDUB") > 0 THEN DO:
         ASSIGN lcBundle = fMDUBInOrder(Order.OrderId, OUTPUT ldAmt)
                liBonoTextID = 538.

         CASE lcBundle:
            WHEN "MDUB" THEN DO:
               /* YOT-1513 */
               FIND FIRST OfferItem WHERE
                          OfferItem.Brand = Syst.Var:gcBrand AND
                          OfferItem.Offer = Order.Offer AND
                          OfferItem.ItemType = "Fatime" AND
                          OfferItem.ItemKey = "BONO8CPFREE" AND
                          OfferItem.BeginStamp <= Order.CrStamp AND
                          OfferItem.EndStamp >= Order.CrStamp NO-LOCK NO-ERROR.

               liBonoPromoTextID = (IF AVAIL OfferItem THEN 520 ELSE 518).
            END.
            WHEN "MDUB2" THEN
               liBonoPromoTextID = 519.
            WHEN "PMDUB" THEN DO:
               ASSIGN
                  liBonoTextID = 512
                  ldaPMDUBPromoStartDate = fCParamDa("PMDUB_PROMO_START_DATE").

               IF ldtOrder < ldaPMDUBPromoStartDate THEN
                  liBonoPromoTextID = 518.
               ELSE
                  liBonoPromoTextID = 535.
            END. /* WHEN "PMDUB" THEN DO: */
            WHEN "MDUB3" THEN
               liBonoPromoTextID = 524.
            WHEN "MDUB4" THEN
               liBonoPromoTextID = 525.
            WHEN "MDUB5" THEN
               liBonoPromoTextID = 530.
            WHEN "DATA6" THEN ASSIGN
               liBonoTextID = 561
               ldamt = (1 + ldeTaxPerc / 100) * ldAmt.
            OTHERWISE liBonoTextID = 0.
         END CASE.

         IF liBonoTextID > 0 THEN DO:
            lcList = CHR(10) + fTeksti(liBonoTextID,liLanguage).
            IF liBonoPromoTextID > 0 AND
               (lcBundle EQ "PMDUB" OR
                Order.CrStamp < 20121210.32400) THEN
             lcList = lcList + CHR(10) + fTeksti(liBonoPromoTextID,liLanguage).
         END.
         ELSE lcList = "".

         IF lcList > "" THEN
            lcList = REPLACE(lcList,"#MF",TRIM(STRING(ldAmt,"->>>>>9.99"))).
         lcText = REPLACE(lcText,"#MDUB",lcList).
      END. /* IF INDEX(lcText,"#MDUB") > 0 THEN DO: */

      /* renewal order discount info */
      IF INDEX(lcText,"#POSTDISC") > 0 THEN DO:
         lcList = "".
         IF Order.Paytype = FALSE THEN DO:
            RUN Mc/offer_penaltyfee.p(Order.OrderID,
                                   OUTPUT liTermMonths,
                                   OUTPUT ldAmt).
            IF ldAmt > 0 THEN DO:
               lcList = CHR(10) + fTeksti(304,liLanguage).
               lcList = REPLACE(lcList, "#AMOUNT", TRIM(STRING(ldAmt,"->>>>>9"))).
               lcList = REPLACE(lcList, "#MONTHS", STRING(liTermMonths)).
            END.
         END.
         lcText = REPLACE(lcText,"#POSTDISC",lcList).
      END.

      /* mnp */
      IF INDEX(lcText,"#OMNPDATA") > 0 THEN DO:
         lcList = "".

         IF Order.MNPStatus > 0 THEN DO:

            ASSIGN
               lcValue = fTeksti(287,liLanguage)
               lcList  = CHR(10) +
                         lcValue + CHR(10) +
                         FILL("-",LENGTH(lcValue) + 2) + CHR(10).

            lcValue = Order.CurrOper.

            lcList = lcList + fTeksti(288,liLanguage) + " " +
                     lcValue + CHR(10).

            lcValue = fTeksti(263,liLanguage).
            IF lcValue = "" OR lcValue = ? THEN lcValue = "Prepaid/Postpaid".
            lcList = lcList + fTeksti(289,liLanguage) + " " +
                     STRING(Order.OldPayType,lcValue).

            IF Order.OldPayType THEN lcList = lcList + CHR(10) +
               fTeksti(290,liLanguage) + " " + Order.OldICC.
         END.

         lcText = REPLACE(lcText,"#OMNPDATA",lcList).
      END.

      /* payment method */
      IF INDEX(lcText,"#OPMETHOD") > 0 THEN DO:

         lcList = "".
         /* not when total amount is 0 */
         IF ldInvTot NE 0 THEN DO:
            ASSIGN
               lcValue = fTeksti(292,liLanguage)
               lcList  = CHR(10) +
                         lcValue + CHR(10) +
                         FILL("-",LENGTH(lcValue) + 2) + CHR(10).

            lcValue = fTeksti(263,liLanguage).
            IF lcValue = "" OR lcValue = ? THEN lcValue = "Prepaid/Postpaid".

            lcList = lcList + STRING(Order.PayType,lcValue).
         END.

         lcText = REPLACE(lcText,"#OPMETHOD",lcList).
      END.

      /* bank account */
      IF INDEX(lcText,"#OBANKDATA") > 0 THEN DO:
         lcList = "".

         /* not for prepaid */
         IF Order.PayType = FALSE THEN
         DO:

            DEFINE VARIABLE lcBankAccHeader AS CHARACTER NO-UNDO.
            DEFINE VARIABLE lcBkAccHolderFieldLabel AS CHARACTER NO-UNDO.
            DEFINE VARIABLE lcBkAccFieldLabel AS CHARACTER NO-UNDO.
            DEFINE VARIABLE lcMandateIdLabel AS CHARACTER NO-UNDO.
            DEFINE VARIABLE lcMandateId AS CHAR NO-UNDO.
            DEFINE VARIABLE ldaMandateDate AS DATE NO-UNDO.

            ASSIGN
               lcBankAccHeader = fTeksti(293, liLanguage)
               lcBkAccHolderFieldLabel = fTeksti(294, liLanguage)
               lcBkAccFieldLabel = fTeksti(542, liLanguage)
               lcMandateIdLabel = fTeksti(541,liLanguage).

            lcList  = CHR(10) +
                      lcBankAccHeader + CHR(10) +
                      FILL("-",LENGTH(lcBankAccHeader) + 2) + CHR(10) +
                      lcBkAccHolderFieldLabel + " " + lcEPLRName + CHR(10) +
                      lcBkAccFieldLabel + " " + lcTagBankAcc.
           
            IF fGetOrderMandateId(BUFFER Order,
                                  OUTPUT lcMandateId,
                                  OUTPUT ldaMandateDate)
            THEN ASSIGN
               lcList = lcList + CHR(10) + lcMandateIdLabel + " " +
                  lcMandateId.
            
            CASE liDeltype:
               WHEN {&INV_DEL_TYPE_PAPER} THEN
                  lcList = lcList + CHR(10) + fTeksti(565,liLanguage).
               WHEN {&INV_DEL_TYPE_EMAIL} THEN
                  lcList = lcList + CHR(10) + fTeksti(564,liLanguage).
            END.
         END.

         lcText = REPLACE(lcText,"#OBANKDATA",lcList).
      END.


      /* campaign data */
      IF INDEX(lcText,"#CAMPAIGN") > 0 THEN DO:
         lcList = "".

         IF Order.Campaign > "" THEN DO:
            ASSIGN
               lcValue = fTeksti(298,liLanguage)
               lcList  = lcValue + CHR(10) +
                         FILL("-",LENGTH(lcValue) + 2) + CHR(10).

            IF NUM-ENTRIES(Order.Campaign,";") > 1 THEN
               lcList = lcList + ENTRY(2,Order.Campaign,";") + CHR(10).

            lcList = lcList + fTeksti(299,liLanguage) + " " +
                     ENTRY(1,Order.Campaign,";").
         END.

         lcText = REPLACE(lcText,"#CAMPAIGN",lcList).
      END.

      /* campaign fatime or topup */
      IF INDEX(lcText,"#FATIME") > 0 THEN DO:
         lcList = "".

         IF Order.Campaign > "" THEN DO:
            ldAmt = Order.FatAmount.

            IF ldAmt = 0 THEN
            FOR EACH OrderTopup OF Order NO-LOCK:
               ldAmt = ldAmt + OrderTopup.Amount + OrderTopup.VatAmount.
            END.

            IF ldAmt > 0 THEN
               lcList  = REPLACE(fTeksti(300,liLanguage),
                                 "#FATAMT",
                                 TRIM(STRING(ldAmt,"->>>>>>9.99"))) +
                         CHR(10).
         END.

         lcText = REPLACE(lcText,"#FATIME",lcList).
      END.

   END.

   /* saldo agreement */
   IF INDEX(lcText,"#SALDO") > 0 AND AVAILABLE MobSub THEN DO:

      liType = fCreditTypeValue(MobSub.MsSeq,
                                OUTPUT liSaldoLimit).

      IF liType = 3 THEN DO:

         ASSIGN lcTagSaldoAmt  = TRIM(STRING(liSaldoLimit,">>>>>9.99"))
                lcTagSaldoDate = STRING(TODAY,"99.99.9999").
         FOR FIRST FixedFee NO-LOCK WHERE
                   FixedFee.Brand     = MobSub.Brand   AND
                   FixedFee.BillCode  = "SALDOSOPKK"   AND
                   FixedFee.CustNum   = MobSub.InvCust AND
                   FixedFee.HostTable = "MobSub"       AND
                   FixedFee.KeyValue  = STRING(MobSub.MsSeq),
             FIRST FFItem OF FixedFee NO-LOCK WHERE
                   FFItem.Billed = FALSE:
            lcTagSaldoDate = STRING(FixedFee.BegDate,"99.99.9999").
         END.
      END.
   END.

   ASSIGN lcDateHead    = fTeksti(136,liLanguage)
          lcCustHead    = fTeksti(140,liLanguage)
          lcBTHeader[1] = fTeksti(176,liLanguage)
          lcBTHeader[2] = fTeksti(365,liLanguage)
          lcBTHeader[3] = fTeksti(366,liLanguage).

   IF iiAddress = 6 AND AVAILABLE Order THEN DO:
      FIND FIRST OrderCustomer OF Order NO-LOCK WHERE
                 OrderCustomer.RowType = 1 NO-ERROR.
      IF AVAIL OrderCustomer AND OrderCustomer.CustIdType = "CIF" THEN ASSIGN
         lcBTHeader[2] = fTeksti(367,liLanguage)
         lcBTHeader[3] = fTeksti(368,liLanguage).
   END.

   /* special tag; user ids for web */
   IF INDEX(lcText,"#WEBUSER") > 0 THEN DO:

      IF NOT fWebUserIDs(liCustNum,
                         INPUT-OUTPUT lcText)
      THEN DO:
         fErrLine("User account is not available").
      END.
   END.

   IF INDEX(lcText,"#NEWOWNER") > 0 AND AVAILABLE MobSub THEN DO:

      FIND bCLICust WHERE bCLICust.CustNum = MobSub.AgrCust NO-LOCK.
      lcEntry = Func.Common:mDispCustName(BUFFER bCLICust).

      lcText = REPLACE(lcText,"#NEWOWNER",lcEntry).
   END.

   IF INDEX(lcText,"#REQDATE") > 0 OR INDEX(lcText,"#PCTYPE") > 0
   THEN DO:
      IF liRequest > 0 THEN
         FIND MsRequest WHERE MsRequest.MsRequest = liRequest NO-LOCK NO-ERROR.
      ELSE
         FIND FIRST MsRequest NO-LOCK WHERE
                    MsRequest.Brand   = "1"       AND
                    MsRequest.CustNum = iiCustNum AND
                    MsRequest.ReqType = 5         AND
                    MsRequest.ReqStat = 1 NO-ERROR.

      IF AVAILABLE MsRequest THEN DO:
         Func.Common:mSplitTS(MsRequest.CreStamp,
                  OUTPUT ldtReqDate,
                  OUTPUT liTime).

         CASE MsRequest.ReqType:
         WHEN 7 THEN lcDCEvent = MsRequest.ReqCParam1.
         WHEN 8 OR
         WHEN 9 THEN lcDCEvent = MsRequest.ReqCParam3.
         OTHERWISE lcDCEvent = "".
         END CASE.

         IF lcDCEvent > "" THEN lcText = REPLACE(lcText,"#PCTYPE",lcDCEvent).

      END.
      ELSE ldtReqDate = TODAY.

      lcText = REPLACE(lcText,"#REQDATE",STRING(ldtReqDate,"99.99.9999")).

   END.

   /* PIN for periodical contract */
   IF INDEX(lcText,"#PCPIN") > 0 THEN DO:
      IF AVAILABLE Customer AND
         Customer.CustNum = Customer.AgrCust AND
         Customer.PContractPIN > ""
      THEN lcText = REPLACE(lcText,"#PCPIN",Customer.PContractPIN).
   END.

   /* periodical contract */
   IF INDEX(lcText,"#PERCONTR") > 0 THEN DO:

      lcDCEvent = fCParamC("PerContractID").
      FOR FIRST DCCLI NO-LOCK WHERE
                DCCLI.Brand      = Syst.Var:gcBrand   AND
                DCCLI.DCEvent    = lcDCEvent AND
                DCCLI.MsSeq      = iiMsSeq   AND
                DCCLI.ValidTo   >= TODAY     AND
                DCCLI.ValidFrom <= TODAY,
          FIRST DayCampaign NO-LOCK WHERE
                DayCampaign.Brand   = Syst.Var:gcBrand AND
                DayCampaign.DCEvent = DCCLI.DCEvent:

         lcText = REPLACE(lcText,"#PERCONTR",
                                 DayCampaign.DCName + ", " +
                                 fTeksti(227,1) + " " +
                                 STRING(DCCLI.ValidFrom,"99.99.9999") + " - " +
                                 STRING(DCCLI.ValidTo,"99.99.9999") +
                                 CHR(10) +
                                 fTeksti(228,1)).
      END.

      /* no contracts */
      IF INDEX(lcText,"#PERCONTR") > 0 THEN
         lcText = REPLACE(lcText,"#PERCONTR",fTeksti(226,1)).
   END.

   /* payment plan */
   IF liPaymPlan > 0 AND INDEX(lcText,"#PPLAN") > 0 THEN DO:
      FIND PaymPlan WHERE PaymPlan.PPlanID = liPaymPlan NO-LOCK NO-ERROR.
      IF AVAILABLE PaymPlan THEN DO:

         lcText = REPLACE(lcText,"#PPLANTOT",STRING(PaymPlan.Amount,
                                                    "->>>>>>>9.99")).

         ASSIGN lcList   = ""
                ldInvTot = 0.
         FOR EACH PPInv OF PaymPlan NO-LOCK,
            FIRST Invoice OF PPInv NO-LOCK:
            lcList = lcList + (IF lcList > "" THEN CHR(10) ELSE "") +
                     STRING(STRING(PPInv.InvNum),"X(22)") +
                     STRING(Invoice.DueDate,"99.99.9999") +
                     FILL(" ",10) +
                     STRING(Invoice.InvAmt - Invoice.PaidAmt,"->>>>>>>9.99").
            ldInvTot = ldInvTot + Invoice.InvAmt - Invoice.PaidAmt.
         END.

         ASSIGN lcText = REPLACE(lcText,"#PPLANINV",lcList)
                lcText = REPLACE(lcText,"#PPLANITOT",STRING(ldInvTot,
                                                    "->>>>>>>9.99")).

         lcList = "".
         FOR EACH PPBatch OF PaymPlan NO-LOCK:
            lcList = lcList + (IF lcList > "" THEN CHR(10) ELSE "") +
                     FILL(" ",22) +
                     STRING(PPBatch.DueDate,"99.99.9999") +
                     FILL(" ",10) +
                     STRING(PPBatch.Amount,"->>>>>>>9.99").
            lcTagRefNum = PPBatch.RefNum.
         END.

         /* divide into groups */
         IF LENGTH(lcTagRefNum) > 6 THEN lcTagRefNum = fViite(lcTagRefNum).

         lcText = REPLACE(lcText,"#PPLANBATCH",lcList).
      END.

   END.

   /* banks */
   ASSIGN lcText = REPLACE(lcText,"#BANK1",lcBank[1])
          lcText = REPLACE(lcText,"#BANK2",lcBank[2])
          lcText = REPLACE(lcText,"#BANK3",lcBank[3]).

   /* services */
   lcText = fMsServiceList(lcText,
                           iiMsSeq,
                           1).

   /* clitype and language have now their final values */
   IF lcTagCLIType > "" THEN DO:
      lcBundle = "".
      IF LOOKUP(lcTagCLIType,lcBundleCLITypes) > 0 THEN DO:
         lcBundle = fGetDataBundleInOrderAction(Order.OrderID,lcTagCLIType).
         lcBundleBillItem = fConvBundleToBillItem(lcBundle).
         lcTagCTName = fTranslationName(Syst.Var:gcBrand,
                                        1,
                                        lcBundleBillItem,
                                        liLanguage,
                                        ldtEventDate).
      END. /* IF LOOKUP(lcTagCLIType,lcBundleCLITypes) > 0 THEN DO: */
      ELSE DO:
         lcTagCTName = fTranslationName(Syst.Var:gcBrand,
                                        9,
                                        lcTagCLIType,
                                        liLanguage,
                                        ldtEventDate).
         /* CONT8 (La del Cero + [Internet]) */
         IF lcTagCLIType EQ "CONT8" AND
            Order.UsageType EQ "DATA" THEN
            lcTagCTName = lcTagCTName + " Internet".
      END.

      IF lcTagCTName = ? OR lcTagCTName = "" THEN DO:
         FIND CLIType WHERE
              CLIType.Brand   = Syst.Var:gcBrand AND
              CLIType.CLIType = lcTagCLIType NO-LOCK NO-ERROR.
         IF AVAILABLE CLIType THEN lcTagCTName = CLIType.CLIName.
      END.
   END.

   ASSIGN /* replace common tags with data */
         lcText  = fReplaceTag(lcText,iiMsSeq)
         lcTitle = fReplaceTag(lcTitle,iiMsSeq).

   /* divide into lines */
   IF INDEX(lcText,"#FONTON,J") = 0 THEN DO:
      lcText = fSeparateInvoTxt(lcText,IF iiPrintTarget = 6
                                       THEN 90
                                       ELSE 75).
   END.

   /* smaller font used -> length of lines varies */
   ELSE DO:

      ASSIGN llSmall = FALSE
             lcFinTxt = "".

      SmallFont:
      REPEAT:

         IF NOT llSmall THEN DO:
            liCount = INDEX(lcText,"#FONTON,J").

            If liCount > 0 THEN ASSIGN
               llSmall = TRUE
               lcDivTxt = SUBSTRING(lcText,1,liCount + 9).
            ELSE ASSIGN lcDivTxt = lcText
                        llSmall  = ?.

            ASSIGN lcDivTxt = fSeparateInvoTxt(lcDivTxt,75)
                   lcFinTxt  = lcFinTxt + lcDivTxt + CHR(9).

            IF llSmall = ? THEN LEAVE SmallFont.

            lcText = SUBSTRING(lcText,liCount + 10).

         END.

         ELSE DO:
            liCount = INDEX(lcText,"#FONTOFF,J").

            IF liCount > 0 THEN ASSIGN
               llSmall = FALSE
               lcDivTxt = SUBSTRING(lcText,1,liCount + 10).
            ELSE ASSIGN lcDivTxt = lcText
                        llSmall  = ?.

            ASSIGN lcDivTxt = fSeparateInvoTxt(lcDivTxt,liSmallLength)
                   lcFinTxt  = lcFinTxt + lcDivTxt + CHR(9).

            IF llSmall = ? THEN LEAVE SmallFont.

            lcText = SUBSTRING(lcText,liCount + 11).

         END.
      END.

      lcText = SUBSTRING(lcFinTxt,1,LENGTH(lcFinTxt) - 1).
   END.

END.

IF lcErrTxt NE "" THEN DO:
   fErrLine(lcErrTxt).
END.

FOR FIRST CLIType NO-LOCK WHERE
          CLIType.Brand = Syst.Var:gcBrand AND
          CLIType.CLIType = (IF lcBundle > "" THEN lcBundle ELSE lcTagCLIType):

   DEF VAR ldeCallPrice AS DEC NO-UNDO.
   DEF VAR ldeMFWithTax AS DEC NO-UNDO.
   DEF VAR ldeMFNoDisc  AS DEC NO-UNDO.

   DEFINE VARIABLE ldtOrderDate AS DATE NO-UNDO.
   DEFINE VARIABLE ldiOrderDate AS INT  NO-UNDO.
   DEFINE VARIABLE llgOrderDate AS LOG  NO-UNDO.
   DEFINE VARIABLE lcMFText     AS CHAR No-UNDO.
   DEFINE VARIABLE llAddLineDiscount AS LOG NO-UNDO.
   DEFINE VARIABLE ldDiscValue  AS DEC  NO-UNDO.
   
   llgOrderDate = Func.Common:mSplitTS(Order.CrStamp,
                           OUTPUT ldtOrderDate,
                           OUTPUT ldiOrderDate).

   ldeMFWithTax = (1 + ldeTaxPerc / 100) * CLIType.CompareFee.

    CASE CLIType.CLIType:
      WHEN "CONT9" OR WHEN "CONT10" OR WHEN "CONT15" THEN lcList = "0 cent/min".
      WHEN "TARJ7" THEN lcList = "1 cent/min".
      WHEN "TARJ8" THEN lcList = "6,05 cent/min".
      WHEN "TARJ9" THEN lcList = "1 cent/min".
      WHEN "TARJ10" THEN lcList = "20 min/mes gratis,".
      WHEN "TARJ11" THEN lcList = "50 min/mes gratis,".
      WHEN "TARJ12" THEN lcList = "100 min/mes gratis,".
      WHEN "TARJ13" THEN lcList = "5000 min/mes gratis,".
      OTHERWISE lcList = "".
    END.

    IF LOOKUP(Order.CLIType, "CONT9,CONT10,CONT15,CONT24,CONT23,CONT25,CONT26,CONT27") > 0 THEN DO:
       /* ADDLINE-144 Additional Line Renewal Email Changes */
       llAddLineDiscount = FALSE.

       /* Additional line discount information in emails */
       FOR FIRST OrderAction NO-LOCK WHERE
                 OrderAction.Brand    = Syst.Var:gcBrand       AND
                 OrderAction.OrderID  = Order.OrderID AND
                 OrderAction.ItemType = "AddLineDiscount":

          FOR FIRST DiscountPlan NO-LOCK WHERE
                    DiscountPlan.DPRuleID = OrderAction.ItemKey,
              FIRST DPRate WHERE
                    DPRate.DPId       = DiscountPlan.DPId AND
                    DPRate.ValidFrom <= ldtOrderDate      AND
                    DPRate.ValidTo   >= ldtOrderDate      NO-LOCK:
          
              llAddLineDiscount = TRUE.
              ldeMFNoDisc       = ldeMFWithTax.
              ldDiscValue       = DPRate.DiscValue.
              ldeMFWithTax      = ldeMFWithTax - ((DPRate.DiscValue / 100) * ldeMFWithTax).

          END.

       END. /* ADDITIONAL-LINE */

       IF NOT llAddLineDiscount THEN
          FOR FIRST OfferItem WHERE
                    OfferItem.Brand       = Syst.Var:gcBrand             AND
                    OfferItem.Offer       = Order.Offer         AND
                    OfferItem.ItemType    = "discountplan"      AND
                    LOOKUP(OfferItem.ItemKey,
                    "TariffMarchDISC,CONT9DISC,CONT10DISC,CONT15DISC,CONT24DISC,CONT23DISC,CONT25DISC,CONT26DISC") > 0 AND
                    OfferItem.BeginStamp <= Order.CrStamp       AND
                    OfferItem.EndStamp   >= Order.CrStamp     NO-LOCK,
             FIRST DiscountPlan WHERE 
                   DiscountPlan.Brand    = Syst.Var:gcBrand AND
                   DiscountPlan.DPRuleId = OfferItem.ItemKey NO-LOCK,
             FIRST DPRate WHERE 
                   DPRate.DPId = DiscountPlan.DPId AND
                   DPRate.ValidFrom <= ldtOrderDate AND
                   DPRate.ValidTo   >= ldtOrderDate NO-LOCK:

             lcMFText = STRING(DiscountPlan.ValidPeriods)                    + 
                       (IF liLanguage EQ 5 THEN " months. " ELSE " meses. ") + 
                       (IF liLanguage EQ 5 THEN "After " ELSE "Después ")    + 
                       TRIM(STRING(ldeMFWithTax,"->>>>>>>9.99"))             + " E/" +
                       (IF liLanguage EQ 5 THEN "month" ELSE "mes")          + 
                       (IF liLanguage EQ 5 THEN " VAT. incl" ELSE " imp. incl.").
              
             IF DiscountPlan.DPUnit EQ "Percentage" THEN 
                ldeMFWithTax = ldeMFWithTax - ((DPRate.DiscValue / 100) * ldeMFWithTax).
             ELSE IF DiscountPlan.DPUnit EQ "Fixed" THEN    
                 ldeMFWithTax = ldeMFWithTax - DPRate.DiscValue.
          END.
    END.

    IF ldeMFWithTax > 0 THEN DO:
       IF llAddLineDiscount THEN
          lcList = lcList + (IF lcList > "" THEN "," ELSE "") + " " + TRIM(STRING(ldeMFWithTax,"->>>>>>>9.99")) + " E/" +
                   (IF liLanguage EQ 5 THEN "month" ELSE "mes") + " IVA incl.~n" +
                   TRIM(STRING(ldDiscValue,"99"))+ "%" + (IF liLanguage EQ 5 THEN " DTO. Forever" ELSE " DTO. para siempre").
       ELSE
          lcList = lcList + (IF lcList > "" THEN ", " ELSE "") +
                   TRIM(STRING(ldeMFWithTax,"->>>>>>>9.99")) + " E/" +
                   (IF liLanguage EQ 5 THEN "month" ELSE "mes").
    END.

    IF lcList > "" THEN DO:
      IF llAddLineDiscount THEN
         lcTagCTName = lcTagCTName + ", " + lcList.
      ELSE
         lcTagCTName = lcTagCTName + ", " + lcList + 
                       (IF liLanguage EQ 5 THEN " VAT. incl"
                        ELSE " imp. incl.").
    END.

     IF lcMFText NE ""  THEN 
        lcTagCTName = lcTagCTName + " " + lcMFText.

END.

/* YOT-1512/YOT-1732/YDR-468 */
IF lcBundle EQ "CONTD3"      THEN lcFATGroup = "IPL8CPACT2".
ELSE IF lcBundle EQ "CONTD4" THEN lcFATGroup = "IPL15CPACT".

IF lcFATGroup > "" THEN DO:
   FIND FIRST OfferItem WHERE
              OfferItem.Brand = Syst.Var:gcBrand AND
              OfferItem.Offer = Order.Offer AND
              OfferItem.ItemType = "Fatime" AND
              OfferItem.ItemKey  = lcFATGroup AND
              OfferItem.BeginStamp <= Order.CrStamp AND
              OfferItem.EndStamp >= Order.CrStamp NO-LOCK NO-ERROR.
   IF AVAIL OfferItem THEN ASSIGN
      lcTagCTName = lcTagCTName + CHR(9) +
      FILL(" ",36) + fTeksti(521,liLanguage).
END. /* IF lcFATGroup > "" THEN DO: */

IF Order.CLIType = "TARJ7" AND 
   Order.OrderType < 2 THEN DO:

    FIND FIRST OrderAction NO-LOCK WHERE 
           OrderAction.Brand    = Syst.Var:gcBrand        AND 
           OrderAction.OrderId  = Order.OrderID  AND
           OrderAction.ItemType = "Promotion"    AND 
           OrderAction.ItemKey  = Order.CLIType  NO-ERROR.
    
    lcTagCTName = lcTagCTName + CHR(9) + CHR(9) + fTeksti(540,liLanguage) + 
                  IF AVAILABLE OrderAction THEN 
                     CHR(9) + CHR(9) + fTeksti(568,liLanguage) + CHR(9)
                  ELSE "".
END.

/* ADDLINE-144 Additional Line Changes */
IF LOOKUP(Order.OrderChannel,"renewal_pos_stc,retention_stc") > 0 THEN
   lcText = replace(lcText, "#CTNAME", lcTagCTName).
ELSE
   lcText = replace(lcText, "#CTNAME", "").

IF INDEX(lcText, "#COMPANY_CONTACT") > 0 THEN DO:
   lcList = fGetLocalContactInfo().
   lcText = REPLACE(lcText, "#COMPANY_CONTACT", lcList).
END.

/* EPL */
IF NOT llErrors AND llEPLPrint THEN DO:

   ASSIGN xEPLForm = "EPL" + xEplForm
          lcKutsu  = xEPLForm.

   IF lcKutsu = "EPL" + lcSpecForm THEN llConInfo = FALSE.

   IF iiPrintTarget = 1 THEN DO:
      /* check that file doesn't exist and form the complete name */
      lcEPLFile = fEPLSeqFileName(lcEPLFile).

      OUTPUT STREAM ekirje TO VALUE(lcEPLFile).

      /* main header for EPL file (sender data) */
      fEPLMainHeader(iiLetterClass).
   END.

   liPage = 1.
   lcRepFLine = FILL(" ",74) + STRING(liPage) +
                "¤¤¤" + lcBTName +
                "¤" + lcHSender +
                "¤" + lcBTCCAddr[1] +
                "¤" + lcBTCCAddr[2].

   /* receiver data; for attachment print only receiver address, for others
      print also an EPLK-line as the beginning of a new letter */
   IF iiPrintTarget = 4 THEN DO:
      fEPLReceiver(lcRepFLine).
   END.
   ELSE DO:
      fEPLCustHeader(lcRepFLine).
   END.

   /* main title and date */
   PUT STREAM eKirje UNFORMATTED
      "5H"
      STRING(lcMainTitle,"X(35)")
      MY-NL
      "0I"
      lcDateHead
      " "
      STRING(TODAY,"99.99.9999")
      MY-NL.

   PUT STREAM eKirje UNFORMATTED
      "37"
      SPACE(4)
      lcTitle
      MY-NL
      " I"
      MY-NL.

   ASSIGN liLine   = 2
          liMargin = 0
          lcFont   = lcBasicFont.

   DO liCount = 1 TO NUM-ENTRIES(lcText,CHR(9)):

      lcEntry = ENTRY(liCount,lcText,CHR(9)).

      /* force new page */
      IF lcEntry = "#NEWPAGE" THEN DO:
         fEPLPage(999).
         NEXT.
      END.

      /* new sheet or attachment */
      ELSE IF lcEntry BEGINS "#EPLSHEET" THEN DO:

         IF lcKutsu BEGINS "EPL5" AND llConInfo AND NOT llAttach THEN DO:
            /* contact info to channel 4 (only to front page) */
            fContactInfo().
            llConInfo = FALSE.
         END.

         llAttach = FALSE.

         /* new form */
         IF NUM-ENTRIES(lcEntry) > 1 AND
            ENTRY(2,lcEntry) > ""
         THEN DO:

            IF lcKutsu BEGINS "EPL6" THEN llConInfo = FALSE.

            ASSIGN xEplForm = "EPL" + ENTRY(2,lcEntry)
                   lcKutsu  = xEplForm.
            IF ENTRY(2,lcEntry) = lcSpecForm
            THEN ASSIGN liCoverPage1 = liRepPage
                        liCoverPage2 = liRepPage
                        liTxtChan    = 1.
         END.

         /* title changed */
         IF NUM-ENTRIES(lcEntry) > 2 AND
            ENTRY(3,lcEntry) > ""
         THEN lcMainTitle = ENTRY(3,lcEntry).

         /* only sheet id, no data printed */
         IF NUM-ENTRIES(lcEntry) > 3 AND
            ENTRY(4,lcEntry) = "Attach"
         THEN llAttach = TRUE.

         llNewSheet = TRUE.

         NEXT.

      END.

      /* font changed */
      ELSE IF lcEntry BEGINS "#FONT" THEN DO:
         IF lcEntry BEGINS "#FONTOFF" OR
            NUM-ENTRIES(lcEntry) < 2
         THEN lcFont = lcBasicFont.
         ELSE lcFont = ENTRY(2,lcEntry).
         NEXT.
      END.

      /* margin set */
      ELSE IF lcEntry BEGINS "#MARGIN" THEN DO:
         IF lcEntry BEGINS "#MARGINOFF" OR
            NUM-ENTRIES(lcEntry) < 2
         THEN liMargin = 0.
         ELSE liMargin = INTEGER(ENTRY(2,lcEntry)) NO-ERROR.
         NEXT.
      END.

      /* print next line to same row as previous */
      ELSE IF lcEntry BEGINS "#SAMEROW" THEN DO:
         lcControl = "+".
         NEXT.
      END.

      fEPLPage(0).

      /* skip empty rows after page change */
      IF llFirstRow AND lcEntry = "" THEN NEXT.

      PUT STREAM eKirje UNFORMATTED
         (IF lcControl > ""
          THEN lcControl
          ELSE " ")
         lcFont
         FILL(" ",11 + liMargin)
         lcEntry
         MY-NL.

      IF lcControl NE "+" THEN liLine     = liLine + 1.

      ASSIGN llFirstRow = FALSE.
             lcControl  = " ".

   END.

   /* contact info to channel 4 (only to front page) */
   IF lcKutsu BEGINS "EPL5" AND llConInfo AND NOT llAttach THEN DO:
      fContactInfo().
   END.

   IF iiPrintTarget = 1 THEN DO:

      OUTPUT STREAM ekirje CLOSE.

      /* move the new file to the actual transfer directory */
      fTransDir(lcEPLFile,
                xFileExt,
                IF lcPrintHouse = "ps"
                THEN xPSTransDir
                ELSE xTransDir).
   END.

   /* mark text as sent */
   fTxtSendLog(2).

   ocErrFile = "".

END.

/* local printing */
ELSE IF NOT llErrors AND NOT llEPLPrint THEN DO:

   liPage = 0.

   IF iiPrintTarget = 6 THEN DO:
       PUT STREAM tul UNFORMATTED
          lcMainTitle AT 6 SKIP(4).
   END.

   ELSE DO:
       fLocalPage(999).

      IF lcBFontOn > "" THEN PUT STREAM tul CONTROL lcBFontOn.
   END.

   IF iiPrintTarget = 6 THEN PUT STREAM tul UNFORMATTED
      lcTitle AT 6 SKIP(1).
   ELSE PUT STREAM tul UNFORMATTED
      lcTitle AT 3 SKIP(1).
   liLine = liLine + 2.

   IF iiPrintTarget NE 6 AND lcBFontOff > ""
   THEN PUT STREAM tul CONTROL lcBFontOff.

   ASSIGN lcFont   = ""
          liMargin = 0.

   DO liCount = 1 TO NUM-ENTRIES(lcText,CHR(9)):

      lcEntry = ENTRY(liCount,lcText,CHR(9)).

      /* force new page */
      IF lcEntry  = "#NEWPAGE" THEN DO:
         fLocalPage(999).
         NEXT.
      END.

      /* disregard some tag */
      ELSE IF LOOKUP(lcEntry,"#EPLSHEET,#SAMEROW") > 0 THEN NEXT.

      /* font changed */
      ELSE IF lcEntry BEGINS "#FONT" THEN DO:
         IF lcEntry BEGINS "#FONTOFF" OR
            NUM-ENTRIES(lcEntry) < 2
         THEN DO:
            PUT STREAM tul CONTROL lcBFontOff.
            PUT STREAM tul CONTROL lcMFontOn.
            lcCurrentFont = lcMFontOn.
         END.

         ELSE DO:

            /* map epl-fonts to printer fonts */
            CASE ENTRY(2,lcEntry):
            WHEN "8" THEN lcFont = lcBFontOn.
            WHEN "I" THEN lcFont = lcBFontOff.
            WHEN "J" THEN lcFont = lcSFontOn.
            OTHERWISE lcFont = "".
            END CASE.

            IF lcFont > "" THEN DO:
               PUT STREAM tul CONTROL lcFont.
               lcCurrentFont = lcFont.
            END.
         END.

         NEXT.
      END.

      /* margin set */
      ELSE IF lcEntry BEGINS "#MARGIN" THEN DO:
         IF lcEntry BEGINS "#MARGINOFF" OR
            NUM-ENTRIES(lcEntry) < 2
         THEN liMargin = 0.
         ELSE liMargin = INTEGER(ENTRY(2,lcEntry)) NO-ERROR.
         NEXT.
      END.

      fLocalPage(0).

      lcEntry = REPLACE(lcEntry,"¤",CHR(9)).

      PUT STREAM tul UNFORMATTED
         lcEntry AT 6 + liMargin.

      /* print next line to same row as previous */
      IF NUM-ENTRIES(lcText,CHR(9)) > liCount AND
         ENTRY(liCount + 1,lcText,CHR(9)) = "#SAMEROW"
      THEN PUT STREAM tul CONTROL CHR(13).

      ELSE DO:
         PUT STREAM tul SKIP.

         liLine = liLine + 1.
      END.
   END.

   /* contact info to footer */
   IF liLine < skayt1 AND iiPrintTarget NE 6 THEN DO:
      PUT STREAM tul SKIP(skayt1 - liLine).
      liLine = skayt1.
   END.
   ELSE IF iiPrintTarget = 6 THEN DO:
      PUT STREAM tul SKIP(4).
   END.

   IF iiPrintTarget NE 6 THEN DO:
      {Syst/uprfeed.i liLine}
   END.

   /* mark text as sent */
   fTxtSendLog(4).

END.

/* email */
IF iiPrintTarget = 6 THEN DO:

   OUTPUT STREAM tul CLOSE.

   IF NOT llErrors AND lcCustEMail > "" AND
                   NOT lcCustEMail BEGINS "@" THEN DO:

      ASSIGN
         xMailFrom = fCParamC("DefEmailSender")
         xMailAddr = lcCustEmail
         xMailSubj = fTeksti(201,liLanguage).

      IF xMailSubj = "" THEN xMailSubj = fTeksti(201,5).

      /* Send the email */
      IF LOOKUP(lcMailHost,{&HOSTNAME_STAGING}) > 0 THEN
         SendMaileInvoice("Order Confirmation Email",lcEPLFile,"").
      ELSE DO:
         xMailSubj = "'" + xMailSubj + "'".
         SendMail(lcEPLFile,"").
      END. /* ELSE DO: */

   END.

   /* move the file to archive directory */
   IF lcTransDir > "" THEN
      fTransDir(lcEPLFile,
                ".rtf",
                lcTransDir).

END.

/* possible errors */
IF llErrors THEN DO:

   IF xErrFile = "" THEN xErrFile = "/tmp/prtxt".
   IF xConfDir = "" THEN xConfDir = fCParamC("RepConfDir").

   ASSIGN xErrFile  = xErrFile + "_" +
                               STRING(YEAR(TODAY),"9999") +
                               STRING(MONTH(TODAY),"99")  +
                               STRING(DAY(TODAY),"99")    +
                               "_" + STRING(TIME) + ".txt".
          ocErrFile = xErrFile.

   OUTPUT STREAM slog TO VALUE(xErrFile).
   PUT STREAM slog UNFORMATTED
       "Customer"  TAB
       "CLI"       TAB
       "Letter"    TAB
       "Error"     MY-NL.

   FOR EACH wError:
       PUT STREAM slog UNFORMATTED
           wError.Cust   TAB
           wError.CLI    TAB
           wError.TxtID  TAB
           wError.ErrMsg MY-NL.
   END.

   OUTPUT STREAM slog CLOSE.

   /* mail recipients */
   GetRecipients(xConfDir + "prtxt_error.email").

   /* send the report AS email */
   IF xMailAddr > "" THEN DO:

      ASSIGN xMailAttach = xErrFile
             xErrFile    = "/tmp/prtxt_errmsg_" +
                           STRING(iiPrintTarget) + ".txt".

      OUTPUT STREAM slog TO VALUE(xErrFile).
      PUT STREAM slog UNFORMATTED
         "Errors from " +
         (IF iiPrintTarget = 1
          THEN "creating an EPL-file from "
          ELSE "local printing of ") +
         (IF iiTxtType <= 1 THEN "information text " ELSE "memo ") +
         STRING(TODAY,"99.99.9999") + "." + my-nl + my-nl +
         "Open the attachment file in Excel." + my-nl + my-nl + "  ".
      OUTPUT STREAM slog CLOSE.

      SendMail(xErrFile,xMailAttach).
   END.

END.



