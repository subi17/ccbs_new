
/*------------------------------------------------------------------------
    File        : cont28_name_update.p
    Purpose     : This program will change the name of CONT28

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Dec 15 12:26:54 IST 2017
    Notes       : This Program will change the Name of CONT28 subscription Plan.

                  Input Parameters : 1) Whether to Update the records or not (Report mode or Update Mode)
                                     2) Output Directory for the generated  files
                                     
This program will run in two modes. Report mode and Update mode.
First a user needs to run this program in Report mode and check the log file.
If everything is as expected , then user needs to run in the Update mode where data will be permanently updated.                                     
                                     
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE ipLgUpdate  AS LOGICAL   LABEL "Update?"                     NO-UNDO.
DEFINE VARIABLE ipChOutDir  AS CHARACTER LABEL "Output Dir" FORMAT "X(50)"   NO-UNDO.


DEFINE VARIABLE chCLIType   AS CHARACTER INITIAL "CONT28" NO-UNDO.
DEFINE VARIABLE chNewValue  AS CHARACTER INITIAL "La Dúo" NO-UNDO.
DEFINE VARIABLE chBkpFile   AS CHARACTER                  NO-UNDO.
DEFINE VARIABLE chRptFile   AS CHARACTER                  NO-UNDO.
DEFINE VARIABLE chErrFile   AS CHARACTER                  NO-UNDO.
DEFINE VARIABLE chOldValue  AS CHARACTER                  NO-UNDO.
DEFINE VARIABLE chBrandName AS CHARACTER                  NO-UNDO.
DEFINE VARIABLE chLogMsg    AS CHARACTER                  NO-UNDO.

DEFINE STREAM strRpt.
DEFINE STREAM setErr.
DEFINE STREAM strBkp.

DEFINE BUFFER bfRepText FOR RepText.

UPDATE ipLgUpdate ipChOutDir WITH FRAME upd-frame.
ASSIGN ipLgUpdate ipChOutDir.
HIDE FRAME upd-frame.

IF TRIM(ipChOutDir) = ""
THEN DO:
            
    MESSAGE "Empty Output Directory Provided. Aborting.."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.

END.

FILE-INFO:FILE-NAME = ipChOutDir.

IF FILE-INFO:FULL-PATHNAME = ?  
THEN DO:
        
    MESSAGE "Directory does not exist. Aborting.. "
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.

END.

IF NOT FILE-INFO:FILE-TYPE  BEGINS "D" 
THEN DO:

    MESSAGE "Entered Path is not a directory. Aborting .."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.

END.


IF INDEX(FILE-INFO:FILE-TYPE , "W" ) = 0 
THEN DO:
            
    MESSAGE "Input Directory does not have write permissions." SKIP
            "Please change directory attributes. Aborting.." 
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
    
END.

MESSAGE "Script will run in" (IF ipLgUpdate THEN " UPDATE " ELSE " REPORT ") "mode."
 SKIP "Do you want to Continue?"
VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lgChoice AS LOGICAL.

IF NOT lgChoice THEN RETURN. 

ASSIGN 
    chErrFile   = RIGHT-TRIM(ipChOutdir , "/" ) + "/cont28err.txt"
    chBkpFile   = RIGHT-TRIM(ipChOutdir , "/" ) + "/cont28bkp.txt"
    chRptFile   = RIGHT-TRIM(ipChOutdir , "/" ) + "/cont28rpt.txt" 
    chBrandName = Syst.Var:gcBrand
    chLogMsg    = (IF ipLgUpdate THEN " is " ELSE " will be ")
    .

OUTPUT STREAM strRpt  TO VALUE(chRptFile).
OUTPUT STREAM setErr  TO VALUE(chErrFile).
OUTPUT STREAM strBkp  TO VALUE(chBkpFile).

/* Mobile Subscriptions*/

FIND FIRST clitype NO-LOCK
    WHERE  clitype.brand    =   chBrandName
      AND  clitype.clitype  =   chCLIType
    NO-ERROR.

IF NOT AVAIL clitype
THEN EXPORT STREAM setErr "Mobile Subscription with the CLITYPE: " chCLIType " is not available".          
ELSE DO:

    ASSIGN chOldValue = clitype.cliname.

    IF ipLgUpdate
    THEN DO:

        FIND CURRENT clitype EXCLUSIVE-LOCK NO-WAIT NO-ERROR. 
                   
        IF LOCKED(clitype) 
        THEN EXPORT STREAM setErr "Mobile Subscription with the CLITYPE: " chCLIType " is locked. Update skipped..".
        ELSE IF NOT AVAIL(clitype)
        THEN EXPORT STREAM setErr "Mobile Subscription with the CLITYPE: " chCLIType " is not available. Update skipped..".
        ELSE DO:
            PUT STREAM strBkp SKIP "clitype " .
            EXPORT STREAM strBkp  clitype . /* Backing up for the further use.*/
            ASSIGN clitype.cliname = chNewValue .
        END. /*  ELSE DO: */
    END. /* IF ipLgUpdate */
    PUT STREAM strRpt UNFORMATTED SKIP  "Mobile Subscription with the CLITYPE: " + chCLIType  + 
                                        chLogMsg +   "updated with the New name : "  +  chNewValue +  
                                        " . Old value is: " +  chOldValue.

    FIND CURRENT clitype NO-LOCK NO-ERROR.

END.

/*   Translation Names for the Mobile Subscriptions and monthly Billing Items */

FOR EACH  reptext NO-LOCK
    WHERE reptext.brand        =   chBrandName
      AND (  reptext.linkcode  =   chCLIType OR reptext.linkcode = "CONT28MF" )  :

    ASSIGN chOldValue = reptext.reptext.
    
    IF ipLgUpdate
    THEN DO:

        FIND FIRST bfRepText  WHERE ROWID(bfRepText) = ROWID(reptext) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.       
        IF LOCKED(bfRepText)
        THEN DO:  
            EXPORT STREAM setErr "Translation Name record with the linkcode: " CLIType.CLIType " is locked. Update Skipped.." .
            NEXT.
        END.
        ELSE IF NOT AVAIL(bfRepText)
        THEN DO:
            EXPORT STREAM setErr "Translation Name record with the linkcode: " CLIType.CLIType " is not available. Update Skipped.." .
            NEXT.
        END.
        ELSE DO:
            PUT STREAM strBkp "Reptext " .
            EXPORT STREAM strBkp bfRepText.
            ASSIGN bfRepText.reptext = chNewValue .
        END.
        
    END. /* IF ipLgUpdate */

    PUT STREAM strRpt UNFORMATTED SKIP "Translation Name record with the linkcode : " +  reptext.linkcode + 
                                       chLogMsg + " updated with the New name : " + chNewValue + 
                                        " . Old Value is: " + chOldValue.

    RELEASE bfRepText NO-ERROR.

END. /* FOR EACH reptext NO-LOCK */

/*  Fee Model Billing Items */

FIND FIRST feemodel NO-LOCK
    WHERE  feemodel.brand   = chBrandName 
    AND  feemodel.feemodel  = "CONT28MF"
    NO-ERROR.

IF AVAIL feemodel
THEN DO:

    ASSIGN chOldValue = feemodel.feemodel .

    IF ipLgUpdate 
    THEN DO:
                    
        FIND CURRENT feemodel EXCLUSIVE-LOCK NO-WAIT NO-ERROR.            
        IF LOCKED(feemodel)
        THEN EXPORT STREAM setErr "FeeModel record with the feemodel: CONT28MF  is locked. Update Skipped.." .
        ELSE IF NOT AVAIL(feemodel)
        THEN EXPORT STREAM setErr "FeeModel  record with the feemodel: CONT28MF  is not available. Update Skipped.." .
        ELSE DO:
                                 
            PUT STREAM strBkp "feemodel " .
            EXPORT STREAM strBkp feemodel.
            ASSIGN feemodel.feemodel = chNewValue .
        END.
        
    END. 
    PUT STREAM strRpt UNFORMATTED SKIP "Fee Model record with the feemodel: CONT28MF " +  chLogMsg + " updated with the New name : " + chNewValue 
                           + " . Old value is: " + chOldValue.
    FIND CURRENT feemodel NO-LOCK NO-ERROR.

END.

/* Periodical Contracts*/

FIND FIRST daycampaign NO-LOCK
    WHERE  daycampaign.brand    =  chBrandName
      AND  daycampaign.dcevent  =  chCLIType
    NO-ERROR.
IF AVAIL daycampaign
THEN DO:

    ASSIGN chOldValue = DayCampaign.DCName .

    IF iplgUpdate
    THEN DO:

        FIND CURRENT daycampaign EXCLUSIVE-LOCK NO-WAIT NO-ERROR.            
        IF LOCKED(daycampaign) 
        THEN EXPORT STREAM setErr "Periodical Contract record with the Event: "  chCLIType " is locked. Update Skipped.. " .
        ELSE IF NOT AVAIL(daycampaign)
        THEN EXPORT STREAM setErr "Periodical Contract record with the Event: "  chCLIType "  is not available. Update Skipped.. " .
        ELSE DO:
                                
            PUT STREAM strBkp "daycampaign " .
            EXPORT STREAM strBkp daycampaign.
            ASSIGN DayCampaign.DCName = chNewValue .
        END.
    END.
    PUT STREAM strRpt UNFORMATTED SKIP  "Periodical Contract record with the Event: " + chCLIType + chLogMsg 
                           + " updated with the New name : "  + chNewValue + " . Old value is: " + chOldValue.
    FIND CURRENT daycampaign NO-LOCK NO-ERROR.

END.

/* ServiceLimitGroup  */

FIND FIRST servicelimitgroup NO-LOCK
    WHERE  servicelimitgroup.brand     =  chBrandName
      AND  servicelimitgroup.GroupCode =  chCLIType
    NO-ERROR.

IF AVAIL servicelimitgroup
THEN DO:

    ASSIGN chOldValue = ServiceLimitGroup.GroupName .

    IF ipLgUpdate 
    THEN DO:

        FIND CURRENT ServiceLimitGroup EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

        IF LOCKED(ServiceLimitGroup)
        THEN EXPORT STREAM setErr "ServiceLimitGroup  record with the GroupCode: " chCLIType " is locked. Update Skipped.. " .
        ELSE IF NOT AVAIL(ServiceLimitGroup)
        THEN EXPORT STREAM setErr "ServiceLimitGroup  record with the GroupCode: " chCLIType " is not available. Update Skipped.. " .
        ELSE DO:
                                
            PUT STREAM strBkp "servicelimitgroup ".
            EXPORT STREAM strBkp ServiceLimitGroup .
            ASSIGN ServiceLimitGroup.GroupName = chNewValue.
        END.
    END.
    PUT STREAM strRpt UNFORMATTED SKIP  "ServiceLimitGroup record with the GroupCode: " + chCLIType + chLogMsg + 
                           " assigned with the new GroupName: " + chNewValue + " . Old Value is : " + chOldValue .

    FIND CURRENT ServiceLimitGroup NO-LOCK NO-ERROR.
END.


MESSAGE "Script Execution completed.." SKIP
    chRptFile SKIP chBkpFile SKIP chErrFile
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

