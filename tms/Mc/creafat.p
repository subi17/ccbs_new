/* ----------------------------------------------------------------------------
  MODULI .......: CREAFAT.P
  TEHTAVA ......: Add customer to a fat group
  SOVELLUS .....: TMS
  TEKIJA .......: aam
  LUONTIPVM ....: 22.09.04
  MUUTOSPVM ....: 29.11.04/aam new parameter to fcpfat-functions
                  21.09.06/aam new input parameter idPerc
  VERSIO .......: M15
---------------------------------------------------------------------------- */

{commali.i}
{cparam2.i}
{timestamp.i}
{fcpfat.i}

DEF INPUT  PARAMETER iiCustNum     AS INT  NO-UNDO.  
DEF INPUT  PARAMETER iiMSSeq       AS INT  NO-UNDO. 
DEF INPUT  PARAMETER icFatGroup    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idAmt         AS DEC  NO-UNDO.
DEF INPUT  PARAMETER idPerc        AS DEC  NO-UNDO.
DEF INPUT  PARAMETER ilVatIncl     AS LOG  NO-UNDO.
DEF INPUT  PARAMETER iiFromPeriod  AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiToPeriod    AS INT  NO-UNDO.
DEF OUTPUT PARAMETER ocError       AS CHAR NO-UNDO.

DEF VAR lcCLI  AS CHAR NO-UNDO. 


FIND Customer WHERE 
     Customer.Brand   = gcBrand AND
     Customer.CustNum = iiCustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN DO:
  ocError = "Customer was not found.".
  RETURN.
END.
 
IF iiMSSeq > 0 THEN DO:

   FIND FIRST MsOwner NO-LOCK USE-INDEX MSSeq WHERE
              MsOwner.MSSeq   = iiMSSeq AND
              MsOwner.CustNum = iiCustNum     
              NO-ERROR.

   IF NOT AVAILABLE MSOwner THEN DO:
      ocError = "CLI was not found".
      RETURN.
   END.

   lcCLI = MsOwner.CLI.

END.

FIND FatGroup WHERE
     FatGroup.Brand = gcBrand AND
     FatGroup.FtGrp = icFatGroup NO-LOCK NO-ERROR.
IF NOT AVAILABLE FatGroup THEN DO:
   ocError = "FAT group was not found".
   RETURN.
END.

ocError = fCreateFatRow(FatGroup.FtGrp,
                        Customer.CustNum,
                        iiMsSeq,
                        lcCli,
                        "",
                        "",
                        idAmt,
                        idPerc,
                        ilVatIncl,
                        iiFromPeriod,
                        iiToPeriod,
                        "").

 

