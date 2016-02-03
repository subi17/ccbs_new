/* ----------------------------------------------------------------------------
  MODULI .......: PRINTERC.P
  TEHTAVA ......: EPL-file for subscription's termination confirmation 
  SOVELLUS .....: TMS
  TEKIJA .......: aam
  LUONTIPVM ....: 09.08.05
  MUUTOSPVM ....: 
  VERSIO .......: M15
---------------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/utumaa.i new}
{Func/feplstart.i}
{Func/cparam2.i}
{Inv/edefine.i new}
{Func/finvtxt.i}

DEF INPUT  PARAMETER icCLI     AS CHAR NO-UNDO.
DEF INPUT  PARAMETER iiCustNum AS INT  NO-UNDO. 

DEF VAR lcErrFile     AS CHAR NO-UNDO. 
DEF VAR liITNum       AS INT  NO-UNDO. 
DEF VAR lcError       AS CHAR NO-UNDO. 


FOR EACH MsOwner NO-LOCK WHERE
         MsOwner.CLI     = icCLI AND
         MsOwner.CustNum = iiCustNum 
BY MsOwner.TsEnd DESC:
   LEAVE. 
END. 
         
IF NOT AVAILABLE MsOwner THEN DO:
   lcError = "Unknown subscription".
   RETURN lcError.
END.

/* text for confirmation of termination */
liITNum = fGetInvTextID("General",
                        "IRTISANOMISVAHV",
                        1,   /* only one language used */
                        TODAY).

IF liITNum = 0 THEN DO:
   lcError = "Text for confirmation of termination has not been defined".
   RETURN lcError.
END.

RUN printxt (MsOwner.CustNum,
             MsOwner.MsSeq, 
             "",
             1,  /* 1=invtext */
             4,  /* owner's address */
             "",
             "",
             liITNum,
             1,  /* epl */
             0,  /* letterclass from invtext */
             OUTPUT lcError).

RETURN lcError.

