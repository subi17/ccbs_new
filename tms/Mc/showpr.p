/* --------------------------------------------------------------------------
  MODULE .......: showpr.p
  FUNCTION .....: Show Payment Refence on screen
  APPLICATION ..: TMS
  AUTHOR .......: PT
  CREATED ......: 21-01-02
  MODIFIED .....: 09.02.2004/aam brand,
                                 fFormRefNum()
  Version ......: M15
------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/refcode.i}
{Func/cparam2.i}
{Func/frefnum.i}

DEF INPUT PARAMETER iiCustNum AS INT NO-UNDO. 
DEF INPUT PARAMETER iiInvNum  AS INT NO-UNDO.

DEF VAR RefNum    AS C   NO-UNDO.
DEF VAR liInvType AS INT NO-UNDO. 

IF iiInvNum > 0 THEN DO: 
   FIND Invoice WHERE Invoice.InvNum = iiInvNum NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Invoice OR Invoice.Brand NE gcBrand THEN DO:
      MESSAGE "Unknown invoice" iiInvNum
      VIEW-AS ALERT-BOX
      ERROR.
      RETURN.
   END.
   
   liInvType = Invoice.InvType.
   
END.

ELSE liInvType = 0.

/* reference nbr */
RefNum = fFormRefNum(iiCustNum,
                     iiInvNum,
                     liInvType).
 
MESSAGE
"Payment Reference number (FINNISH STANDARD)"  SKIP
"for Customer No." iiCustNum "and Invoice No." iiInvNum "is" SKIP(1)
"< " + RefNum + " >"                           SKIP
VIEW-AS ALERT-BOX INFORMATION.

