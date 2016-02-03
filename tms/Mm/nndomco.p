/* -----------------------------------------------------------------
  MODULE .......: NNDOMCO.P
  TASK .........: Erases double MOBILE calls fom database
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 10.11.1999 jpo
  CHANGED ......: 13.12.1999 jpo Added TO search rule: ErrorCode NE ErrCode 
                  02.02.2000 jpo DEF.CountryCode
                  29.06.2001/aam batch RUN possibility,
                                 send the report AS email
                  11.09.2003/  Brand 
                  30.10.2003 /jpo  Dont erase billed calls
                  13.11.2003 /jp fDirExists
                  18.07.2005 tk CLI
                  
  Version ......: M15
  ------------------------------------------------------------------ */

{Syst/commali.i}
{Func/excel.i}
{Func/email.i}
{Func/direct_dbconnect.i}

def var cadate1   as da no-undo.
def var cadate2   as da no-undo. 
DEF VAR i         AS i  NO-UNDO.
def var ok        as lo no-undo format "Yes/No".
def var erase     as lo no-undo format "Yes/No".
DEF VAR logfile   AS c  NO-UNDO.
def var bDisp     as lo no-undo format "Yes/No".
DEF VAR lcCLI    LIKE MobCDR.CLI NO-UNDO.

DEF STREAM msg.

DEF VAR cfile     AS CHAR NO-UNDO.
DEF VAR xConfDir  AS CHAR NO-UNDO.
DEF VAR bbatch    AS LOG  NO-UNDO.
DEF VAR ldaOldDb  AS DATE NO-UNDO.
DEF VAR liCurrent AS INT  NO-UNDO.

bbatch = session:batch.

{Func/tmsparam.i DefDoubMDir   return}.   logfile      = TMSParam.CharVal.
{Func/tmsparam.i RepConfDir     return}.  xConfDir     = TMSParam.CharVal. 

IF bbatch THEN DO: 
   /* get the recipients FOR EMail */
    GetRecipients(xConfDir + "doublecalls.email").
END.

function fDirExists returns logical
   (input fPath as char):

   DEF VAR rivi as c no-undo.
   DEF VAR Path AS c NO-UNDO init "/".
   DEF VAR loop AS C NO-UNDO.

   /* remove filename from path */        
   path = Substring(fPath,1,
          Length(fpath) - LENGTH(entry(num-entries(fpath,"/"),fpath,"/"))).    
   
    input through value(" sh -c " + "\"if test ! -d " + path +
                         "; then echo error; fi\"").
    REPEAT :
       import unformatted rivi.
    END. 

    return rivi = "".
end.



form
   skip(1)
   "  Instruction:  This program searches all DOUBLE MOBILE calls within"
   "                the time Period determined below.                 " skip(1)
   "                UNINVOICED double calls can be erased.    " skip(1)
   "                It is possible to create a TAB-separated ASCII-file "
   "                with the information of the double ISValue." skip(1)
   "                calls between ...............:" cadate1
   format "99-99-9999"
   help "Earliest call Date"
   "-" cadate2 format "99-99-9999" help "Latest call Date"  SKIP
   "                MSISDN ......................:" lcCLI SKIP
   "                Shall double calls be marked :" erase
   help "Do you want to mark  all found Unbilled double calls ?"
   "                Name of log -file ...........:"  logfile format "x(24)"
   help "Logfile's name,  empty: no log"                   SKIP
   "                Display double calls ........:" bDisp  skip(3)
WITH
   width 80 ROW 1 OVERLAY COLOR value(cfc) TITLE COLOR value(ctc)
   " " + ynimi + " MARK DOUBLE MOBILE calls " + string(pvm,"99-99-99") + " "
   NO-LABELS FRAME start.

IF NOT bbatch THEN DO:
    cfc = "sel". run ufcolor.
    ASSIGN
    cadate2 = date(month(TODAY),1,year(TODAY)) - 1
    cadate1 = date(month(cadate2),1,year(cadate2)).
END.
ELSE 
ASSIGN erase = TRUE bDisp = FALSE   .


CRIT:
repeat WITH FRAME start:

   IF NOT bbatch THEN DO:
      ehto = 9. RUN ufkey.

      UPDATE
      cadate1  validate(cadate1 ne ?,"Give first Date !")
      cadate2  validate(input cadate2 >= input cadate1,"Wrong order !")
      lcCLI
      erase
      logfile
      bDisp
      WITH FRAME start
      EDITING.
         READKEY.
         IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:
            PAUSE 0.

            if frame-field = "logfile" THEN DO:
               ASSIGN FRAME start logfile.
               FILE-INFO:FILE-NAME = logfile.

               IF FILE-INFO:FULL-PATHNAME NE ? THEN 
               MESSAGE
               "NOTE: Log file already exists"
               VIEW-AS ALERT-BOX.
            END.
         END.
         APPLY LASTKEY.
      END. /* EDITING */

      task:
      repeat WITH FRAME start:
         ASSIGN ufk = 0 ufk[1] = 7 ufk[5] = 795 ufk[8] = 8 ehto = 0.
         RUN ufkey.
         IF toimi = 1 THEN NEXT  CRIT.
         IF toimi = 8 THEN LEAVE CRIT.

         IF toimi = 5 THEN DO:
            ok = FALSE.
            BELL. 
            IF erase THEN DO: 
               MESSAGE
               "Are You sure You want to start MARKING (Y/N)?" UPDATE ok.
            END.   
            ELSE DO:
               MESSAGE
               "Are You sure You want to start SEARCHING (Y/N)?" UPDATE ok.
            END.   
            IF ok THEN LEAVE TASK.
         END.
      END.

   END.  /* NOT bbatch */
   
   IF fDirExists(logfile) = FALSE THEN DO:
      MESSAGE
      "Directory " logfile "does not exist"
      VIEW-AS ALERT-BOX.
   END.

   /* connect db(s) */
   fInitializeConnectTables("MobCDR","").
   RUN pDirectConnect2Dbs(gcBrand,
                          "",
                          cadate2,
                          cadate2).
   IF RETURN-VALUE BEGINS "ERROR" THEN DO:
      MESSAGE RETURN-VALUE
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.

   ldaOldDb = ?.
   FOR FIRST ttDB WHERE ttDB.ConnName = "",
       FIRST DBConfig NO-LOCK WHERE
             DBConfig.DBConfigID = ttDB.DBConfigID:
      IF DBConfig.FromDate > cadate1 THEN ldaOldDb = DBConfig.FromDate - 1.    
      liCurrent = ttDB.DBConfigID.
   END.
 
   IF ldaOldDb NE ? THEN DO:
      fInitializeConnectTables("MobCDR","old").
      RUN pDirectConnect2Dbs(gcBrand,
                             "old",
                             ldaOldDb,
                             ldaOldDb).
      IF RETURN-VALUE BEGINS "ERROR" THEN DO:
         MESSAGE RETURN-VALUE
         VIEW-AS ALERT-BOX ERROR.
         RETURN.
      END.

      FOR FIRST ttDB WHERE ttDB.ConnName = "old":
         IF ttDB.DBConfigID = liCurrent THEN DO:
            DISCONNECT oldmcdr NO-ERROR.
            DELETE ttDB.
         END.
      END.
   END.

   RUN mobcdr_double_check.p ("",
                              cadate1,
                              cadate2,
                              lccli,
                              erase,
                              bdisp,
                              logfile,
                              0,
                              0,
                              "",
                              OUTPUT i).

   IF CONNECTED("oldmcdr") THEN 
      DISCONNECT oldmcdr NO-ERROR.
      
   if logfile ne "" THEN DO:

        /* send AS EMail */
        IF bBatch THEN DO:
            ASSIGN cfile = "/data/mobile/snet/dcalls-msg.txt".

            OUTPUT STREAM msg TO value(cfile).
            PUT STREAM msg UNFORMATTED
                " Double CDRs   " + 
                string(cadate1,"99.99.9999") + "-" +
                string(cadate2,"99.99.9999")  +
                "." +  my-nl + my-nl +
                STRING(erase,"Marked/Listed")
                "  " my-nl.
            OUTPUT STREAM msg CLOSE.

            SendMail(cFile,LogFile).
        END.
   END.

   IF NOT bBatch THEN 
       MESSAGE "DONE" VIEW-AS ALERT-BOX INFORMATION.

   LEAVE crit.

END.  /* crit */

IF NOT bbatch THEN DO:
    HIDE FRAME LOG no-pause.
    HIDE FRAME start no-pause.
END.

