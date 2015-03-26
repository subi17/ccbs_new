DEFINE STREAM sConfEmail.

FUNCTION fText RETURN LOGICAL (INPUT pcText AS CHARACTER):
   PUT STREAM sConfEmail UNFORMATTED pcText SKIP.
   RETURN TRUE.
END.


OUTPUT STREAM sConfEmail TO VALUE("ycm_1606_confirm_email.txt"). 
DEFINE VARIABLE iITNum AS INTEGER NO-UNDO. 

fText("Email text").
fText("-----------------------").


FOR EACH InvText NO-LOCK WHERE
         InvText.Brand = "1" AND
         InvText.Target = "OrderConf" AND
         LOOKUP(InvText.KeyValue,"EmailConf,EmailConfCIF,RenewalConf,RenewalConfCIF") > 0 AND
         InvText.FromDate <= TODAY AND
         InvText.ToDate >= TODAY:

   fText( "Target = " + InvText.Target + CHR(10) + 
          "KeyValue = " + InvText.KeyValue + CHR(10) + 
          "Language = " + STRING(InvText.Language) + CHR(10) + 
          "Title = " + InvText.TxtTitle + CHR(10) + 
          "Text = " + InvText.InvText + CHR(10) + CHR(10)).

END.

OUTPUT STREAM sConfEmail CLOSE. 
