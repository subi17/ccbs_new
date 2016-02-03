/* ----------------------------------------------------------------------------
  MODULI .......: PRINUSER.P
  TEHTAVA ......: EPL-file for new user account data
  SOVELLUS .....: TMS
  TEKIJA .......: aam
  LUONTIPVM ....: 18.01.06
  MUUTOSPVM ....: 21.04.06/aam use same letter for all roles in "new"
  VERSIO .......: M15
---------------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/utumaa.i new}
{Func/cparam2.i}
{Inv/edefine.i new}
{Func/finvtxt.i}
{Func/fcustdata.i}


DEF INPUT  PARAMETER iiCustNum AS INT  NO-UNDO.
DEF INPUT  PARAMETER icTxtType AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER ocError   AS CHAR NO-UNDO. 

DEF VAR lcErrFile     AS CHAR NO-UNDO. 
DEF VAR liITNum       AS INT  NO-UNDO. 
DEF VAR lcCustRole    AS CHAR NO-UNDO.
DEF VAR lcTxtType     AS CHAR NO-UNDO.

FIND Customer WHERE Customer.CustNum = iiCustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN DO:
   ocError = "Unknown customer".
   RETURN.
END.

CASE icTxtType:
WHEN "passwd" OR 
WHEN "loginpwd"   THEN lcTxtType = "UserAccPwd".
WHEN "ownerchg"   THEN lcTxtType = "OwnerChgPwd".
OTHERWISE              lcTxtType = "UserAccount".   /* new */
END CASE. 

/* text for letter */
liITNum = fGetInvTextID("General",
                        lcTxtType,
                        1,   /* only one language used */
                        TODAY).

IF liITNum = 0 THEN DO:
   ocError = "Text for user account has not been defined".
   RETURN.
END.

RUN printxt (Customer.CustNum,
             0, 
             "",
             1,  /* 1=invtext */
             1,  /* address */
             "",
             "",
             liITNum,
             1,  /* epl */
             0,  /* letterclass from invtext */
             OUTPUT ocError).

      

 
