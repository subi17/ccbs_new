/* ----------------------------------------------------------------------------
  MODULI .......: CLIMEPL.P
  TEHTAVA ......: EPL-file for credit limit letter
  SOVELLUS .....: TMS
  TEKIJA .......: aam
  LUONTIPVM ....: 22.05.03
  MUUTOSPVM ....: 19.09.03/aam brand
                  16.02.04/aam fdivtxt.i
  VERSIO .......: M15
---------------------------------------------------------------------------- */

{commali.i}
{cparam2.i}
{ftransdir.i}
{email.i}
{edefine.i NEW}
{refcode.i}
{invotxtp.i}
{fcustref.i}
{fdivtxt.i}

DEF TEMP-TABLE wError NO-UNDO
    FIELD Cust   AS INT
    FIELD ErrMsg AS CHAR.

/* customer */
DEFINE INPUT  PARAMETER iiCustNum  AS INT  NO-UNDO.  
DEFINE OUTPUT PARAMETER ocErrFile  AS CHAR NO-UNDO. 

DEF VAR liLetterClass AS INT  NO-UNDO.
DEF VAR lcEPLFile     AS CHAR NO-UNDO.
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR lcRefNum      AS CHAR NO-UNDO.
DEF VAR lcErrTxt      AS CHAR NO-UNDO. 
DEF VAR llErrors      AS LOG  NO-UNDO.
DEF VAR lcText        AS CHAR NO-UNDO.
DEF VAR lcPhone       AS CHAR NO-UNDO.

DEF STREAM slog.


FUNCTION fErrLine RETURNS LOGICAL
    (iMessage AS CHAR).

    CREATE wError.
    ASSIGN wError.Cust   = iiCustNum
           wError.ErrMsg = iMessage
           llErrors      = TRUE. 

END FUNCTION.


fEPLInit().

ASSIGN liLetterClass = fCParamI("EPLCLimitLClass")
       lcEPLFile     = fCParamC("EPLCLimitFile")
       xEPLForm      = "EPL5"
       ocErrFile     = xErrFile
       llErrors      = FALSE.

/* make sure that values are valid */
IF liLetterClass = ? OR
   liLetterClass < 1 OR 
   liLetterClass > 2 
THEN liLetterClass = 1.

IF lcEPLFile = "" OR lcEPLFile = ? 
THEN lcEPLFile = "/tmp/cl". 

FIND FIRST Company WHERE
           Company.Brand = gcBrand 
   NO-LOCK NO-ERROR. 
IF NOT AVAIL Company THEN DO:
   fErrLine("Company data is missing.").
END.

EMPTY TEMP-TABLE wError. 

FIND Customer WHERE Customer.CustNum = iiCustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN DO:
   fErrLine("Customer " + STRING(iiCustNum) + " was not found.").
END.

IF NOT llErrors THEN DO:
   /* message text */
   FIND FIRST InvText NO-LOCK WHERE
              InvText.Brand     = gcBrand  AND
              InvText.Target    = "EKIRJE" AND
              InvText.KeyValue  = "CLimit" AND
              InvText.FromDate <= TODAY    AND
              InvText.ToDate   >= TODAY    AND
              InvText.Language  = Customer.Language 
              NO-ERROR.

   IF NOT AVAILABLE InvText THEN DO:
      fErrLine("Text for eLetter was not found").
   END.

   ELSE IF InvText.EPLForm = "" THEN DO:
      fErrLine("EPL form is not defined"). 
   END.

   ELSE DO:

      ASSIGN xEPLForm = InvText.EPLForm
             lcText   = InvText.InvText
             lcRefNum = fCustRefNum(Customer.CustNum)

             /* replace tags with data */
             lcText   = REPLACE(lcText,"#SALDO",STRING(Customer.CreditLimit))
             lcText   = REPLACE(lcText,"#VIITE",lcRefNum)
             lcText   = REPLACE(lcText,"#PUHNUM",lcPhone)

             /* divide into lines */
             lcText   = fSeparateInvoTxt(lcText,87).
   END.   

END.

IF NOT llErrors THEN      
   /* check that customer is valid */
   lcErrTxt = fEPLCheckAddr(liLetterClass,
                            Customer.CustName,
                            Customer.ZipCode,
                            Customer.Country,
                            OUTPUT lcEPLACountry).

IF lcErrTxt NE "" THEN DO:
   fErrLine(lcErrTxt).
END.

IF NOT llErrors THEN DO:

   /* check that file doesn't exist and form the complete name */
   lcEPLFile = fEPLFileName(lcEPLFile).

   OUTPUT STREAM ekirje TO VALUE(lcEPLFile).

   /* main header for EPL file (sender data) */
   fEPLMainHeader(liLetterClass).

   ASSIGN lcEPLRName    = Customer.CustName
          lcEPLRCoName  = Customer.CoName
          lcEPLRAddr    = Customer.Address
          lcEPLRZipCode = Customer.ZipCode
          lcEPLRPost    = Customer.ZipCode + " " + Customer.PostOffice
          lcEPLRCountry = Customer.Country.
 
   /* receiver data */
   fEPLCustHeader("").


   /* message text */
   PUT STREAM eKirje UNFORMATTED
      "30" MY-NL.

   DO liCount = 1 TO NUM-ENTRIES(lcText,CHR(9)):
      PUT STREAM eKirje UNFORMATTED
         " 1" ENTRY(liCount,lcText,CHR(9))
         MY-NL.
   END.

   OUTPUT STREAM ekirje CLOSE.

   /* move the new file to the actual transfer directory */
   fTransDir(lcEPLFile,
             xFileExt,
             xTransDir).

   ocErrFile = "".          
END.

/* possible errors */
IF llErrors THEN DO:

    ASSIGN xErrFile  = xErrFile + "_" + 
                                STRING(YEAR(TODAY),"9999") +
                                STRING(MONTH(TODAY),"99")  +
                                STRING(DAY(TODAY),"99")    + 
                                "_" + STRING(TIME) + ".txt".                   
           ocErrFile = xErrFile.

    OUTPUT STREAM slog TO VALUE(xErrFile).
    PUT STREAM slog UNFORMATTED
        "Customer"  TAB
        "Error"     MY-NL.

    FOR EACH wError:
        PUT STREAM slog UNFORMATTED
            wError.Cust   TAB
            wError.ErrMsg MY-NL.
    END.

    OUTPUT STREAM slog CLOSE. 

    /* send the report AS email */
    ASSIGN xMailAttach = xErrFile
           xErrFile    = "/tmp/clepl_errmsg.txt".
    OUTPUT STREAM slog TO VALUE(xErrFile).
    PUT STREAM slog UNFORMATTED
        "Errors from creating a credit limit letter EPL-file " + 
        STRING(TODAY,"99.99.9999") + "." + my-nl + my-nl +
        "Open the attachment file in Excel." + my-nl + my-nl + "  ".
    OUTPUT STREAM slog CLOSE.

    /* mail recipients AND actual sending */
    GetRecipients(xConfDir + "clepl_error.email").
    SendMail(xErrFile,xMailAttach).

END.



