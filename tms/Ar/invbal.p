/* invbal.p 

   changed:    24.07.02/aam use account type 7 for type 3 invoices 
               11.04.03/aam ePayment 
               20.05.03/aam finvbal
*/

{Syst/commali.i}
{Func/cparam2.i}
{Func/finvbal.i}

def input  parameter iiInvNum  like Invoice.InvNum no-undo.
def output parameter BalDue    as dec              no-undo.

FIND Invoice WHERE Invoice.InvNum = iiInvNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Invoice THEN RETURN. 

BalDue = fInvBal(BUFFER Invoice,
                 TODAY).
