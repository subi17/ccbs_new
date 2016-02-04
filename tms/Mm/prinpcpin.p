/* ----------------------------------------------------------------------------
  MODULI .......: PRINPCPIN.P
  TEHTAVA ......: EPL file for periodical contract's PIN letter
  SOVELLUS .....: TMS
  TEKIJA .......: aam
  LUONTIPVM ....: 06.02.06
  MUUTOSPVM ....: 03.04.06/aam icContrType
  VERSIO .......: M15
---------------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/utumaa.i new}
{Func/cparam2.i}
{Inv/edefine.i new}
{Func/finvtxt.i}
{Func/fcustdata.i}


DEF INPUT  PARAMETER iiCustNum   AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiRequest   AS INT  NO-UNDO.
DEF OUTPUT PARAMETER ocError     AS CHAR NO-UNDO. 

DEF VAR lcErrFile     AS CHAR NO-UNDO. 
DEF VAR liITNum       AS INT  NO-UNDO. 
DEF VAR lcCustRole    AS CHAR NO-UNDO.
DEF VAR lcTxtType     AS CHAR NO-UNDO.

FIND Customer WHERE Customer.CustNum = iiCustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN DO:
   ocError = "Unknown customer".
   RETURN.
END.

IF Customer.CustNum NE Customer.AgrCust THEN DO:
   ocError = "Not an agreement customer".
   RETURN.
END.

IF Customer.PContractPIN = "" THEN DO:
   /* generate or error */
END. 

/* text for letter */
liITNum = fGetInvTextID("General",
                        "PerContrPIN",
                        1,   /* only one language used */
                        TODAY).

IF liITNum = 0 THEN DO:
   ocError = "Text for PIN letter has not been defined".
   RETURN.
END.

RUN Mc/printxt (Customer.CustNum,
             iiRequest, 
             "RQ",
             1,  /* 1=invtext */
             1,  /* address */
             "",
             "",
             liITNum,
             1,  /* epl */
             0,  /* letterclass from invtext */
             OUTPUT ocError).
