/* ----------------------------------------------------------------------------
  MODULI .......: PRINPPLAN.P
  TEHTAVA ......: EPL-file for payment plan confirmation
  SOVELLUS .....: TMS
  TEKIJA .......: aam
  LUONTIPVM ....: 16.03.06
  MUUTOSPVM ....: 
  VERSIO .......: M15
---------------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/utumaa.i new}
{Func/cparam2.i}
{Inv/edefine.i new}
{Func/finvtxt.i}
{Func/fcustdata.i}


DEF INPUT  PARAMETER iiPaymPlan AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiCustNum  AS INT  NO-UNDO.
DEF OUTPUT PARAMETER ocError    AS CHAR NO-UNDO. 

DEF VAR liITNum       AS INT  NO-UNDO. 
DEF VAR lcTxtType     AS CHAR NO-UNDO.

FIND PaymPlan WHERE PaymPlan.PPlanID = iiPaymPlan NO-LOCK NO-ERROR.
IF NOT AVAILABLE PaymPlan THEN DO:
   ocError = "Unknown payment plan".
   RETURN.
END.

/* text according to plan type */
CASE PaymPlan.PPType:
WHEN 1 THEN lcTxtType = "DueDateTrans".
WHEN 2 THEN lcTxtType = "PaymPlanSingle".
WHEN 3 THEN lcTxtType = "PaymPlanMulti".
END CASE.
   
/* text for letter */
liITNum = fGetInvTextID("General",
                        lcTxtType,
                        1,   /* only one language used */
                        TODAY).

IF liITNum = 0 THEN DO:
   ocError = ocError + " Text for payment plan letter (" + 
                       STRING(iiPaymPlan) + ") has not been defined".
   RETURN.
END.

RUN printxt (iiCustNum,    /* letter to the one who ordered the change */
             PaymPlan.PPlanID, 
             "PP",
             1,  /* 1=invtext */
             1,  /* address */
             "",
             "",
             liITNum,
             1,  /* epl */
             0,  /* letterclass from invtext */
             OUTPUT ocError).
                
/* mark printed */
IF ocError = "" AND PaymPlan.PPStatus < 2 THEN DO:
   FIND CURRENT PaymPlan EXCLUSIVE-LOCK.
   PaymPlan.PPStatus = 2.
END.


      

 
