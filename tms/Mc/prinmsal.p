/* ----------------------------------------------------------------------------
  MODULI .......: PRINMSAL.P
  TEHTAVA ......: EPL-file for saldo agreement confirmation from mobsub
  SOVELLUS .....: TMS
  TEKIJA .......: aam
  LUONTIPVM ....: 11.10.04
  MUUTOSPVM ....: 
  VERSIO .......: M15
---------------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/utumaa.i new}
{Func/feplstart.i}
{Func/cparam2.i}
{Inv/edefine.i new}
{Func/finvtxt.i}

DEF INPUT  PARAMETER iiMsSeq   AS INT  NO-UNDO.
DEF OUTPUT PARAMETER ocError   AS CHAR NO-UNDO. 

DEF VAR lcErrFile     AS CHAR NO-UNDO. 
DEF VAR liITNum       AS INT  NO-UNDO. 

FIND MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
IF NOT AVAILABLE MobSub THEN DO:
   ocError = "Unknown subscription".
   RETURN.
END.

/* text for order confirmation of saldo agreement */
liITNum = fGetInvTextID("General",
                        "SaldoSopimusVah",
                        1,   /* only one language used */
                        TODAY).

IF liITNum = 0 THEN DO:
   ocError = "Text for confirmation has not been defined".
   RETURN.
END.

RUN Mc/printxt (MobSub.CustNum,
             MobSub.MsSeq, 
             "",
             1,  /* 1=invtext */
             1,  /* address */
             "",
             "",
             liITNum,
             1,  /* epl */
             0,  /* letterclass from invtext */
             OUTPUT ocError).

      

 
