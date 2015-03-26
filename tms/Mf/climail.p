/*------------------------------------------------------------
MODULE .......: climail.p
FUNCTION .....: Sends mail TO an address WITH subject AND content.
USAGE: Parameters with address, subject and content. Relay on the existens of a    "CLI" -user in network      WITH an EMail AccNum.  
APPLICATION ..: 
AUTHOR .......: PG Orbil    
MODIFIED .....: 
VERSION ......: M15
--------------------------------------------------------------*/
DEFINE INPUT PARAMETER pAddress AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pSubject AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pContent AS CHARACTER NO-UNDO.

IF OPSYS = "UNIX" THEN DO:

    UNIX SILENT VALUE ("mailx -s '"  + 
                        pSubject + "' " 
                        + pAddress + " < " 
                        + pContent).
END.
ELSE DO:
    MESSAGE "Emailaddress: " + pAddress ". " SKIP
            "Subject: " + pSubject SKIP
            "Content: " + pContent
             VIEW-AS ALERT-BOX.
END.                                        





