/* email.i  03.07.2001/aam
   get the EMail addresses from a configuration File
   send the mail WITH possible attachments

            16.05.2002/jp  sendattmail* FUNCTION
            28.03.2003/tk  fStr2Unix - adds \ to unix special characters
            08.05.2003/aam return value for sendmail,
                           direct errors to sendmail.log
            30.05.2003/aam don't use "text/plain" for mimetype
            08.06.2006/aam get hostname and use it for sender
            09.11.2006/aam &MailTitleSpaces
            31.05.2007/aam SendMailX,
                           attachments are no longer mandatory with sendatt
*/
{Syst/commali.i}
{Func/ftransdir.i}

DEF VAR xMailRecip    AS CHAR NO-UNDO.
DEF VAR xMailAddr     AS CHAR NO-UNDO.
DEF VAR xMailSubj     AS CHAR NO-UNDO.
DEF VAR xMailAttach   AS CHAR NO-UNDO.
DEF VAR xMailFrom     AS CHAR NO-UNDO.
DEF VAR xMailTxt      AS CHAR NO-UNDO.
DEF VAR xMailFileType AS CHAR NO-UNDO.
DEF VAR xMailComm     AS CHAR NO-UNDO.
DEF VAR xMailError    AS CHAR NO-UNDO.
/* administrator, errors send TO this address */
DEF VAR xMailAdmin    AS CHAR NO-UNDO INIT "ari@starnet.fi".
DEF VAR lcMailHost    AS CHAR NO-UNDO. 

DEF STREAM sMailNotify.

/* get hostname, it must be correct in xMailFrom */
INPUT THROUGH hostname.
IMPORT lcMailHost.
INPUT CLOSE.

/* get the recipients */
FUNCTION GetRecipients RETURNS LOGIC
    (iConfigFile AS CHAR):

    ASSIGN xMailAddr = ""
           xMailFrom = ""
           xMailSubj = "".
    
    IF SEARCH(iConfigFile) = ?
        THEN ASSIGN xMailError = "Config File " + iConfigFile +
                                 " missing. ".
    ELSE DO:                   
        INPUT FROM VALUE(SEARCH(iConfigFile)) NO-ECHO.
        REPEAT:
            IMPORT UNFORMATTED xMailRecip.

            IF xMailRecip BEGINS "#" THEN DO:
                CASE SUBSTRING(xMailRecip,2,4):
                /* sender */
                WHEN "From" THEN ASSIGN xMailFrom = SUBSTR(xMailRecip,7).
                /* subject */
                WHEN "Subj" THEN ASSIGN xMailSubj = SUBSTR(xMailRecip,7).
                /* FileType */
                WHEN "Type" THEN ASSIGN xMailFileType = SUBSTR(xMailRecip,7).
                END CASE.
            END.

            /* recipients */
            ELSE IF INDEX(xMailRecip,"@") GT 0
            THEN ASSIGN xMailAddr = xMailAddr +
                                    (IF xMailAddr ne ""
                                     THEN ","
                                     ELSE "") +
                                    xMailRecip.
        END.
        INPUT CLOSE.
    END.

END FUNCTION.

/* send the mail WITH possible attachments */
FUNCTION SendMail RETURNS LOGIC
    (iMailTxt  AS CHAR,
     iAttachTxt AS CHAR).

    IF xMailFileType = "" THEN xMailFileType = "text/plain".

    /* don't send mail IF recipients are missing 
       no error raised, user propably wants TO prevent mailing */
    IF xMailAddr = "" THEN RETURN FALSE.

    IF xMailFrom = "" THEN 
        ASSIGN xMailFrom = "starnet@starnet.fi".
    IF xMailSubj = "" THEN 
        ASSIGN xMailError = xMailError + "Subject missing. ". 

    &IF "{&MailTitleSpaces}" NE "Allow"
    &THEN
    /* convert spaces to "_" because spaces cause errors */
    xMailSubj = REPLACE(xMailSubj," ","_").    
    &ENDIF
    
    IF iMailTxt NE "" AND SEARCH(iMailTxt) = ? THEN 
        ASSIGN xMailError = xMailError + "Mail contents missing (" +
                            iMailTxt + ") ".
    
    /* correct host for sender */
    IF lcMailHost > "" THEN 
       xMailFrom = REPLACE(xMailFrom,"#host",lcMailHost).
        
    IF xMailError = "" THEN DO:               
        ASSIGN 
        /* parameters FOR sending the EMail */
        xMailComm = "/opt/local/bin/sendatt " +
                    " -s " + xMailSubj     +    /* subject */
                    " -c " + iMailTxt      +    /* content */
                    " -f " + xMailFrom     +    /* sender */
                    " -m " + xMailFiletype +    /* mail type */
                    " "    + xMailAddr     +    /* recipient */
                    " "    + iAttachTxt.        /* attached files */
    END.

    /* possible errors */
    ELSE DO:
        OUTPUT TO /tmp/tmsmail.log.
        PUT UNFORMATTED 
            THIS-PROCEDURE:FILE-NAME SKIP
            iMailTxt " + " iAttachTxt SKIP
            xMailError SKIP.
        OUTPUT CLOSE.

        ASSIGN 
           /* parameters FOR sending the EMail */
           xMailComm = "/opt/local/bin/sendatt" +
                       " -s TMS_eMail_error" +             /* subject */
                       " -c /tmp/tmsmail.log" +   /* content */
                       " -f starnet@starnet.fi" + /* sender */
                       " -m " + xMailFiletype +  
                       " "    + xMailAdmin +
                       " /tmp/tmsmail.log".            /* recipient */
    END.

    UNIX SILENT VALUE(xMailComm + " >>/tmp/sendmail.log").

    RETURN (xMailError = "").

END FUNCTION.

FUNCTION SendMailX RETURNS LOGIC
    (icSubject     AS CHAR,
     icSender      AS CHAR,
     icMailAddress AS CHAR,
     icMailText    AS CHAR):
     
   DEF VAR lcMailComm AS CHAR NO-UNDO.

   IF xMailFileType = "" THEN xMailFileType = "text/plain".

   lcMailComm = "mailx" + 
                " -s " + icSubject  +       /* subject */
                (IF icSender > ""           /* from    */
                 THEN " -r " + icSender
                 ELSE "")     + " " + 
                icMailAddress       +       /* to */ 
                " < " + icMailText.         /* content */

   UNIX SILENT VALUE(lcMailComm + " >>/tmp/sendmailx.log"). 

END FUNCTION.

FUNCTION SendMailXOS RETURNS LOGIC
    (icSubject     AS CHAR,
     icSender      AS CHAR,
     icMailAddress AS CHAR,
     icMailText    AS CHAR,
     icOS          AS CHAR):
     
   DEF VAR lcMailComm AS CHAR NO-UNDO.

   IF xMailFileType = "" THEN xMailFileType = "text/plain".

   IF icSender > "" THEN DO:
      IF icOS EQ "Linux" THEN icSender = " -a 'From: " + icSender + "'".
      ELSE icSender = " -r " + icSender.
   END.
   ELSE icSender = "".

   lcMailComm = "mailx" + 
                " -s " + icSubject  +       /* subject */
                icSender + " "     +       /* from    */
                icMailAddress       +       /* to */ 
                " < " + icMailText.         /* content */

   UNIX SILENT VALUE(lcMailComm + " >>/tmp/sendmailx.log"). 

END FUNCTION.

/* send the mail WITH possible attachments */
FUNCTION SendAttMail RETURNS LOGIC
    (xsubj      AS CHAR,
     XEMail     AS CHAR,
     iAttachTxt AS CHAR).

    DEF VAR i         AS INT  NO-UNDO.
    DEF VAR eadd      AS CHAR NO-UNDO.
    DEF VAR xmailcomm AS CHAR NO-UNDO.

    IF xMailFileType = "" THEN xMailFileType = "text/plain".

    DO i = 1 TO num-entries(XEMail):
        eadd = entry(i,XEMail).
        
        SendMailX(xSubj,
                  "",
                  eadd,
                  iAttachTxt).
    END.
       
END FUNCTION.


/* send the mail WITH possible attachments */
FUNCTION SendAttMail5 RETURNS LOGIC
    (xsubj       AS CHAR,
     content     AS CHAR,
     Xemail      AS CHAR,
     sender      AS CHAR,
     iAttachfile AS CHAR).

   DEF VAR i         AS INT  NO-UNDO.
   DEF VAR eadd      AS CHAR NO-UNDO.
   DEF VAR xmailcomm AS CHAR NO-UNDO.

   IF xMailFileType = "" THEN xMailFileType = "text/plain".

    DO i = 1 TO num-entries(XEMail):
        eadd = entry(i,XEMail).

        xMailComm = "/opt/local/bin/sendatt -s " +
                    xsubj +                /* subject */
                   " -c " + content     +  /* content */
                   " -f " + sender      +  /* sender */
                   " -m text/plain "    +
                   eadd +                                                                       " "    + iAttachfile +
                   " ".

       UNIX SILENT VALUE(xMailComm). 

    END.   
END FUNCTION.


FUNCTION fStr2Unix RETURNS CHAR
   (istring AS CHAR):

   DEF VAR ostring as c no-undo.
   def var convert as c no-undo.
   def var i       as i no-undo.

   convert = " ,!,*,^,$,%,&,*,?,(,),~{,~},[,],~",',`,\,~,|,;,&,<,>". 

   DO i = 1 TO LENGTH(istring):
      IF LOOKUP(SUBSTR(istring,i,1),convert) > 0 THEN
         ostring = ostring + "\\".  

      ostring = ostring + SUBSTR(istring,i,1).
   END.

   RETURN ostring.

END FUNCTION.


/* send the mail WITH possible related to the eInvoice Project */
FUNCTION SendMaileInvoice RETURNS LOGIC (iMailTxt AS CHAR,
                                         iAttachFileName AS CHAR,
                                         iContentFile AS CHAR).

    DEF VAR lcFileName   AS CHAR NO-UNDO.
    DEF VAR lcErrorLog   AS CHAR NO-UNDO.

    IF iAttachFileName > "" THEN
       lcFileName = ENTRY(NUM-ENTRIES(iAttachFileName,"/"),
                          iAttachFileName,"/").

    /* don't send mail IF recipients are missing 
       no error raised, user propably wants TO prevent mailing */
    IF xMailAddr = "" THEN RETURN FALSE.

    IF xMailFrom = "" THEN 
       xMailFrom = "starnet@starnet.fi".
    IF xMailSubj = "" THEN 
       xMailError = xMailError + "Subject missing.". 

    &IF "{&MailTitleSpaces}" NE "Allow"
    &THEN
    /* convert spaces to "_" because spaces cause errors */
    xMailSubj = REPLACE(xMailSubj," ","_").    
    &ENDIF
    
    IF iMailTxt = "" AND iContentFile = "" THEN 
       xMailError = xMailError + "Mail contents missing (" + iMailTxt + ") ".
    
    /* correct host for sender */
    IF lcMailHost > "" THEN 
       xMailFrom = REPLACE(xMailFrom,"#host",lcMailHost).
        
    IF xMailError = "" THEN        
        ASSIGN 
        /* parameters FOR sending the EMail */
        xMailComm = "/opt/local/bin/sendeinvoice" +
                    " -s " + "'" + xMailSubj + "'" +  /* subject */
                    " -c " + "'" + iMailTxt  + "'" +  /* content */
                    " -f " + "'" + xMailFrom + "'" +  /* sender */
                    " -r " + "'" + xMailAddr + "'" +  /* recipient */
                    " -a " + "'" + iAttachFileName + "'" + /* attachment */
                    " -n " + "'" + lcFileName + "'" + /* attachment file name */
                    " -i " + "'" + iContentFile + "'". /* content as file */

    /* possible errors */
    ELSE DO:
        /* YTS-7530, this can be removed when TMS is running under user account
        with same group as tmsrpc. Currently using root, planned to be changed
        at begining of 2016. */
        IF katun EQ "NewtonRPC" THEN DO:
           lcErrorLog = "/tmp/sendmail_einvoice_error_" + katun + ".log".
           OUTPUT TO lcErrorLog.
        END.   
        ELSE
           OUTPUT TO /tmp/sendmail_einvoice_error.log.
        PUT UNFORMATTED 
            THIS-PROCEDURE:FILE-NAME SKIP
            iMailTxt                 SKIP
            xMailError               SKIP.
        OUTPUT CLOSE.

        ASSIGN 
           /* parameters FOR sending the EMail */
           xMailComm = "/opt/local/bin/sendeinvoice" +
                       " -s TMS_eMail_error" +              /* subject */
                       " -c " + "'" + xMailError + "'" +    /* content */
                       " -f starnet@starnet.fi" +           /* sender */
                       " -r " + xMailAdmin.                 /* recipient */
    END.
    /* YTS-7530, this can be removed when TMS is running under user account  
       with same group as tmsrpc. Currently using root, planned to be changed
       at begining of 2016. */
    IF katun EQ "NewtonRPC" THEN
       UNIX SILENT VALUE(xMailComm + " >>/tmp/sendmail_" + katun + ".log 2>&1").
    ELSE
       UNIX SILENT VALUE(xMailComm + " >> /tmp/sendmail_einvoice.log 2>&1").

    RETURN (xMailError = "").

END FUNCTION.

FUNCTION fMailNotify RETURN CHARACTER
   (iiCustNum           AS INT,
    icType              AS CHAR,
    icEmailReplacedText AS CHAR,
    icMailSubj          AS CHAR,
    icEmailFile         AS CHAR,
    icTransDir          AS CHAR,
    icAddrConfDir       AS CHAR):

   DEF VAR lcAddrConfDirNotify     AS CHAR NO-UNDO.
   DEF VAR lcLatestEmailFileNotify AS CHAR NO-UNDO.
   DEF VAR lcMailAddr              AS CHAR NO-UNDO.
   DEF VAR lcMailFrom              AS CHAR NO-UNDO.
   DEF VAR i                       AS INT  NO-UNDO.

   ASSIGN lcAddrConfDirNotify = icAddrConfDir + "emailinvoicenotify.email"
          lcMailFrom          = xMailFrom.

   GetRecipients(lcAddrConfDirNotify).
   
   ASSIGN xMailSubj  = icType + " Invoice " + icMailSubj.

   ASSIGN lcLatestEmailFileNotify = icEmailFile + "_" + STRING(iiCustNum) +
                                    "_Notify_" + STRING(TODAY,"999999") + "_" +
                                    STRING(TIME) + ".html".

   OUTPUT STREAM sMailNotify TO VALUE(lcLatestEmailFileNotify).
   PUT STREAM sMailNotify UNFORMATTED xMailSubj SKIP(1).
   PUT STREAM sMailNotify UNFORMATTED icEmailReplacedText SKIP.
   OUTPUT STREAM sMailNotify CLOSE.

   lcMailAddr = xMailAddr.
   DO i = 1 TO NUM-ENTRIES(lcMailAddr,","):
      xMailAddr = ENTRY(i,lcMailAddr,",").
      SendMaileInvoice(icEmailReplacedText,"","").
   END.

   IF icTransDir > "" THEN
      fTransDir(lcLatestEmailFileNotify,
                ".html",
                icTransDir).
   ASSIGN xMailSubj = icMailSubj
          xMailFrom = lcMailFrom.
END FUNCTION.
