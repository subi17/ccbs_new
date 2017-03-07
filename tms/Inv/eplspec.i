/* eplspec.i       23.01.04/aam

   epl routines for printing a call specification report
   
   callers: mclispec.p
            nnprky4.p

   changes:        10.02.06/aam don't find invcust before fTargetAddress       
  
*/

{Func/cparam2.i}
{Func/ftransdir.i}
{Func/email.i}
{Inv/edefine.i NEW}
{Func/faddress.i}

DEF TEMP-TABLE wError NO-UNDO
    FIELD CLI    AS CHAR
    FIELD ErrMsg AS CHAR.

DEF VAR liLetterClass AS INT  NO-UNDO.
DEF VAR lcEPLFile     AS CHAR NO-UNDO.
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR lcErrTxt      AS CHAR NO-UNDO. 
DEF VAR llErrors      AS LOG  NO-UNDO.
DEF VAR lcTestFlag    AS CHAR NO-UNDO. 

DEF STREAM slog.

ASSIGN lcTestFlag   = fCParamC("EPLTest").

FUNCTION fErrLine RETURNS LOGICAL
    (iMessage AS CHAR,
     icCLI    AS CHAR).

    CREATE wError.
    ASSIGN wError.CLI    = icCLI
           wError.ErrMsg = iMessage
           llErrors      = TRUE. 

END FUNCTION.

FUNCTION fSetAddress RETURNS LOGICAL
   (iiCustNum AS INT, /* calling customer, i.e. cli user (if target address
                         is other than 3 then this can be inv.customer) */
    iiMSSeq   AS INT,
    iiAddress AS INT).

   IF iiAddress = 3 THEN DO:
      FIND FIRST MsOwner WHERE
           MsOwner.Brand   = gcBrand   AND
           MsOwner.CustNum = iiCustNum AND
           MsOwner.MSSeq   = iiMSSeq NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MsOwner THEN DO:
         fErrLine("CLI was not found.",
                  STRING(iiMSSeq)).
      END.
   END.
   
   FIND Customer WHERE Customer.CustNum = iiCustNum NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Customer THEN DO:
      fErrLine("Customer was not found",
               STRING(iiCustNum)).
   END.
   
   IF NOT llErrors THEN DO:

      /* get address */
      fTargetAddress(iiAddress).
   END.
    
END FUNCTION.
   
FUNCTION fStartEPL RETURNS LOGICAL.

   fEPLInit().

   ASSIGN lcEPLFile     = fCParamC("EPLSpecFile")
          llErrors      = FALSE.

   IF liLetterClass = 0 
   THEN liLetterClass = fCParamI("EPLSpecClass"). 
   
   /* make sure that values are valid */
   IF liLetterClass = ? OR
      liLetterClass < 1 OR 
      liLetterClass > 2 
   THEN liLetterClass = 2.

   IF lcEPLFile = "" OR lcEPLFile = ? 
   THEN lcEPLFile = "/tmp/cl". 

   EMPTY TEMP-TABLE wError. 

   FIND FIRST Company WHERE
              Company.Brand = gcBrand 
   NO-LOCK NO-ERROR. 
   IF NOT AVAIL Company THEN DO:
      fErrLine("Company data is missing.",
               "").
   END.

   IF NOT llErrors THEN DO:

      /* check that address etc. are valid */
      lcErrTxt = fEPLCheckAddr(liLetterClass,
                               lcEPLRName,
                               lcEPLRZipCode,
                               lcEPLRCountry,
                               OUTPUT lcEPLACountry).

      IF lcErrTxt NE "" THEN DO:
         fErrLine(lcErrTxt,
                  "").
      END.
   END.
   
   IF NOT llErrors THEN DO:

      /* check that file doesn't exist and form the complete name */
      lcEPLFile = fEPLFileName(lcEPLFile).

      OUTPUT STREAM ekirje TO VALUE(lcEPLFile).

      /* main header for EPL file (sender data) */
      fEPLMainHeader(liLetterClass).
      
      /* start a new letter */
      ASSIGN etusivuerit   = FALSE
             llCaKanto     = FALSE
             llCaSivu      = -1.
   END.

   RETURN NOT llErrors. 
   
END FUNCTION.

FUNCTION fEndEPL RETURNS CHARACTER.

   DEF VAR lcErrFile AS CHAR NO-UNDO. 
    
   OUTPUT STREAM ekirje CLOSE.

   /* move the new file to the actual transfer directory */
   IF NOT llErrors THEN DO:
      fTransDir(lcEPLFile,
                xFileExt,
                IF lcPrintHouse = "ps"
                THEN xPSTransDir
                ELSE xTransDir).
   END.
   /* or delete the failed one */
   ELSE DO:
      OS-DELETE VALUE(lcEPLFile).
   END.
   
   lcErrFile = "".          

   /* possible errors */
   IF CAN-FIND(FIRST wError) THEN DO:

      ASSIGN xErrFile  = xErrFile + "_" + 
                         STRING(YEAR(TODAY),"9999") +
                         STRING(MONTH(TODAY),"99")  +
                         STRING(DAY(TODAY),"99")    + 
                         "_" + STRING(TIME) + ".txt".                   
             lcErrFile = xErrFile.

      OUTPUT STREAM slog TO VALUE(xErrFile).
      PUT STREAM slog UNFORMATTED
          "CLI"   TAB
          "Error" MY-NL.

      FOR EACH wError:
          PUT STREAM slog UNFORMATTED
              wError.CLI    TAB
              wError.ErrMsg MY-NL.
      END.

      OUTPUT STREAM slog CLOSE. 

      /* send the report AS email */
      ASSIGN xMailAttach = xErrFile
             xErrFile    = "/tmp/specepl_errmsg.txt".
      OUTPUT STREAM slog TO VALUE(xErrFile).
      PUT STREAM slog UNFORMATTED
         "Errors from creating a credit limit letter EPL-file " + 
         STRING(TODAY,"99.99.9999") + "." + my-nl + my-nl +
         "Open the attachment file in Excel." + my-nl + my-nl + "  ".
      OUTPUT STREAM slog CLOSE.

      /* mail recipients AND actual sending */
      GetRecipients(xConfDir + "specepl_error.email").
      SendMail(xErrFile,xMailAttach).
   END.
   
   RETURN lcErrFile.
    
END.



