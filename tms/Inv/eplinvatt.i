/* eplinvatt.i     30.01.06/aam
   
   temporary version; print an attachment to the end of epl invoice 
*/

{Syst/commali.i}
{Func/finvtxt.i}

DEF VAR liAttITNum AS INT NO-UNDO.

/* text for letter */
liAttITNum = fGetInvTextID("General",
                           "UserAccConv",
                           1,   /* only one language used */
                           TODAY).

FUNCTION fPrintAttachment RETURNS LOGIC
   (iiCustNum     AS INT,
    iiLetterClass AS INT).

   DEF VAR lcAttError AS CHAR NO-UNDO.
   
   IF liAttITNum = 0 THEN RETURN FALSE.
   
   /* letter is being sent only once per customer */
   IF CAN-FIND(FIRST ITSendLog WHERE
                     ITSendLog.TxtType = 1          AND
                     ITSendLog.ITNum   = liAttITNum AND
                     ITSendLog.CustNum = iiCustNum)
   THEN RETURN FALSE. 
   
   /* account already in use */
   IF CAN-FIND(FIRST UserAccount WHERE
                     UserAccount.CustNum = iiCustNum AND
                     UserAccount.Active > 1)
   THEN RETURN FALSE. 
   
   /* print letter */
   RUN Mc/printxt.p (iiCustNum,
                0, 
                "",
                1,  /* 1=invtext */
                7,  /* address set already */
                "",
                "",
                liAttITNum,
                4,  /* print as an attachment  */
                iiLetterClass,  /* same letterclass as this invoice */
                OUTPUT lcAttError).
   
   RETURN (lcAttError = "").
   
END FUNCTION.



