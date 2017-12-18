
/*------------------------------------------------------------------------
    File        : YDR-2660-config.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Oct 30 15:09:37 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE iloop AS INTEGER NO-UNDO.
DEFINE BUFFER lbf-invtext FOR invtext.

blk:
DO TRANSACTION ON ERROR UNDO BLK, LEAVE BLK
               ON STOP UNDO BLK, LEAVE BLK:

MESSAGE "This program will create records for:" SKIP 
        "  - SMS/MNPIdentDirect " SKIP  
        "  - SMS/MNPIccidPOS    " SKIP 
        "in table InvText so they re-direct the business logic to the new Message Manager." SKIP(1)
        "(Note: this program works only with brand 1)" SKIP(1)    
        "Do you want to continue?" 
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lCont AS LOGICAL.
IF lCont THEN 
DO:
    DO iLoop = 1 TO 5:
        FIND LAST InvText EXCLUSIVE-LOCK USE-INDEX target 
            WHERE InvText.Brand    = "1"              AND
                  InvText.Target   = "SMS"            AND
                  InvText.KeyValue = "MNPIdentDirect" AND
                  InvText.FromDate <= TODAY     AND
                  InvText.ToDate   >= TODAY     AND
                  InvText.Language = iLoop
            NO-ERROR.
        IF AVAILABLE InvText THEN
        DO:
            CREATE lbf-invtext.
            BUFFER-COPY invtext EXCEPT ITNum fromdate TO lbf-invtext.
            
            ASSIGN InvText.ToDate                 = TODAY - 1
                   lbf-InvText.ITNum              = NEXT-VALUE(it-seq)
                   lbf-InvText.FromDate           = TODAY
                   lbf-InvText.TemplateID         = "mnp/RECH_IDENT" 
                /* lbf-InvText.SchedulingPolicy   = Data unnecessary, template ID is enough 
                   lbf-InvText.SchedulingPriority = */ 
                   lbf-InvText.Paramkey           = "cli=#CLI"
                   lbf-InvText.UseMman            = TRUE.  /* Use message manager */
            
            MESSAGE "YDR-2660 - Invtext record created for" lbf-InvText.Target "/" lbf-InvText.KeyValue " and language" lbf-InvText.Language SKIP 
                    "Template Id:" lbf-InvText.TemplateID SKIP
                    "InvText Id:" lbf-InvText.ITNum
                VIEW-AS ALERT-BOX.
        END.
    END.
    DO iLoop = 1 TO 5:
        FIND LAST InvText EXCLUSIVE-LOCK USE-INDEX target 
            WHERE InvText.Brand    = "1"            AND
                  InvText.Target   = "SMS"          AND
                  InvText.KeyValue = "MNPIccidPOS"  AND
                  InvText.FromDate <= TODAY         AND
                  InvText.ToDate   >= TODAY         AND
                  InvText.Language = iLoop
            NO-ERROR.
        IF AVAILABLE InvText THEN
        DO:
            CREATE lbf-invtext.
            BUFFER-COPY invtext EXCEPT ITNum fromdate TO lbf-invtext.
            
            ASSIGN InvText.ToDate                 = TODAY - 1
                   lbf-InvText.ITNum              = NEXT-VALUE(it-seq)
                   lbf-InvText.FromDate           = TODAY
                   lbf-InvText.TemplateID         = "mnp/RECH_ICCID"                     
                /* lbf-InvText.SchedulingPolicy   = Data unnecessary, template ID is enough 
                   lbf-InvText.SchedulingPriority = */ 
                   lbf-InvText.Paramkey           = "cli=#CLI"
                   lbf-InvText.UseMman            = TRUE.  /* Use message manager */
    
            MESSAGE "YDR-2660 - Invtext record created for" lbf-InvText.Target "/" lbf-InvText.KeyValue " and language" lbf-InvText.Language SKIP 
                    "Template Id:" lbf-InvText.TemplateID SKIP
                    "InvText Id:" lbf-InvText.ITNum 
                VIEW-AS ALERT-BOX.
        END.
    END.
END.

MESSAGE "Done" VIEW-AS ALERT-BOX.

END.  /* BLK */
