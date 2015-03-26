/* feplform.i       09.01.02/aam 
   determine the correct invoice form 
   iType: 0 = Invoice
          1 = Reminder
          3 = Deposit payment
          4 = Advance payment 

                08.03.2002/aam types FOR deposit AND adv.p changed TO 3 & 4
                               (match the invoice type; Invoice.ltyyppi) 
*/

DEF VAR xFType AS INT NO-UNDO.

FUNCTION fEplForm RETURNS CHARACTER
    (iType AS INT).

    ASSIGN xFType = 0.

    CASE iType:
    /* reminders */
    WHEN 1 THEN RETURN "53500".

    /* deposit payment invoices */
    WHEN 3 THEN RETURN "53500". 
    
    /* advance payment invoices */
    WHEN 4 THEN RETURN "53500".

    /* normal invoices */
    OTHERWISE RETURN "53500".

    END CASE.

END FUNCTION.
