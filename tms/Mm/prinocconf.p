/* ----------------------------------------------------------------------------
  MODULI .......: PRINOCCONF.P
  TEHTAVA ......: EPL-file for owner change confirmation
  SOVELLUS .....: TMS
  TEKIJA .......: aam
  LUONTIPVM ....: 24.02.06
  MUUTOSPVM ....: 
  VERSIO .......: M15
---------------------------------------------------------------------------- */

{commali.i}
{utumaa.i new}
{cparam2.i}
{edefine.i new}
{finvtxt.i}
{fcustdata.i}


DEF INPUT  PARAMETER iiOldOwner AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiNewOwner AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiMsSeq    AS INT  NO-UNDO.
DEF INPUT  PARAMETER icCLI      AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER ocError    AS CHAR NO-UNDO. 

DEF VAR lcErrFile     AS CHAR NO-UNDO. 
DEF VAR liITNum       AS INT  NO-UNDO. 
DEF VAR lcCustRole    AS CHAR NO-UNDO.
DEF VAR lcTxtType     AS CHAR NO-UNDO.
DEF VAR liConfCnt     AS INT  NO-UNDO.
DEF VAR liCustNum     AS INT  NO-UNDO. 
DEF VAR lcError       AS CHAR NO-UNDO.

/* letter both to old owner and new owner */
DO liConfCnt = 1 TO 2:

   CASE liConfCnt:
   WHEN 1 THEN ASSIGN liCustNum = iiOldOwner
                      lcTxtType = "OwnerChgConfOld".
   WHEN 2 THEN ASSIGN liCustNum = iiNewOwner
                      lcTxtType = "OwnerChgConfNew".
   END CASE.
   
   FIND Customer WHERE Customer.CustNum = liCustNum NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Customer THEN DO:
      ocError = ocError + " Unknown customer " + STRING(liCustNum).
      NEXT.
   END.

   /* text for letter */
   liITNum = fGetInvTextID("General",
                           lcTxtType,
                           1,   /* only one language used */
                           TODAY).

   IF liITNum = 0 THEN DO:
      ocError = ocError + " Text for owner change letter (" + 
                          STRING(liConfCnt) + ") has not been defined".
      NEXT.
   END.

   RUN printxt (Customer.CustNum,
                iiMsSeq, 
                icCLI,
                1,  /* 1=invtext */
                1,  /* address */
                "",
                "",
                liITNum,
                1,  /* epl */
                0,  /* letterclass from invtext */
                OUTPUT lcError).
                
   ocError = ocError + lcError.              
END.

      

 
