/* ===========================================================================
 MODULE ........: DDAUTHIN
 APPLICATION ...: TMS
 TASK ..........: Reads Direct Debit Authorization file
 CREATED .......: 05.11.1999 
 CHANGED .......: 02.11.01 ht
                  21.11.01 ht  DELETE off from termination
                  27.11.01 ht  previous back
                               REMARK: Identification = CustNum
                  28.02.03 aam mark Customer.ChargeType,
                               wait for F5 before starting 
                  05.05.03 aam transfer to processed, 
                               use choosefile.p,
                               allow many days in one file
                  17.06.03/aam old bankaccount not mandatory on changes 
                  17.09.03/aam brand 
                  09.02.04/aam if customer not found try to find cli
                  16.07.04/aam directory and file name separately
                  03.08.04/aam remove leading letters from identification
                  10.08.04/aam AuthID
                  02.09.04/aam save failed lines to db with custnum 0
                  29.03.05/aam default date from header
                  12.04.05/aam skip empty rows and rows beginning with "$"
                  30.05.05/aam logic separated from nnsvlu -> ddauthin.p
                  07.11.05/aam eventlog
                  24.05.06/aam check if other authorizations exist when deleted
                  19.09.06/aam reject if id is msisdn and user customer is
                               not the invoice customer
 VERSION .......: M15
============================================================================*/

{Syst/commali.i}
{Syst/utumaa.i}
{Func/cparam2.i}
{Func/timestamp.i}
{Func/email.i}
{Ar/ddtrans.i}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhDDAuth AS HANDLE NO-UNDO.
   lhDDAuth = BUFFER DDAuth:HANDLE.
   RUN StarEventInitialize(lhDDAuth).

END.


DEF INPUT  PARAMETER icAuthFile AS CHAR NO-UNDO. 
DEF INPUT  PARAMETER ilDispMsg  AS LOG  NO-UNDO. 
DEF INPUT  PARAMETER ilSendMail AS LOG  NO-UNDO. 
DEF OUTPUT PARAMETER oiRead     AS INT  NO-UNDO.


DEF VAR svtied          AS C  NO-UNDO FORMAT "x(50)".
DEF VAR svrivi          AS C  NO-UNDO.
DEF VAR palvtunn        AS C  NO-UNDO FORMAT "x(9)".
DEF VAR rlkm            AS I  NO-UNDO.
DEF VAR lkm             AS I  NO-UNDO.
DEF VAR tietuetunnus    AS C  NO-UNDO INIT " ".
DEF VAR Yksilointi      AS C  NO-UNDO.
DEF VAR Nimi            AS C  NO-UNDO.
DEF VAR KasTun          AS I  NO-UNDO.
DEF VAR ArTun           AS C  NO-UNDO.
DEF VAR PankkiTili      AS C  NO-UNDO.
DEF VAR TaPVM           AS DA NO-UNDO.
DEF VAR VanhaPankkiTili AS C  NO-UNDO.
DEF VAR HandMsg         AS C  NO-UNDO.
DEF VAR Pituus          AS I  NO-UNDO.
DEF VAR i               AS I  NO-UNDO.
DEF VAR llFailed        AS LO NO-UNDO.
DEF VAR rl              AS I  NO-UNDO.
DEF VAR sl              AS I  NO-UNDO.

DEF VAR liCount    AS INT  NO-UNDO.
DEF VAR liRead     AS INT  NO-UNDO. 
DEF VAR liCustNum  AS INT  NO-UNDO. 
DEF VAR liInvCust  AS INT  NO-UNDO. 
DEF VAR lcCLI      AS CHAR NO-UNDO. 
DEF VAR lcFromDir  AS CHAR NO-UNDO. 
DEF VAR lcChoose   AS CHAR NO-UNDO. 
DEF VAR ldtDefDate AS DATE NO-UNDO.
DEF VAR lcMailFile AS CHAR NO-UNDO. 
DEF VAR lcConfDir  AS CHAR NO-UNDO. 
DEF VAR lcRepDir   AS CHAR NO-UNDO. 
DEF VAR llRejected AS LOG  NO-UNDO. 
DEF VAR lcCKatun   AS CHAR NO-UNDO.
DEF VAR lcBankAcc  AS CHAR NO-UNDO.

DEF TEMP-TABLE ttError NO-UNDO
   FIELD ErrorMsG AS CHAR.
   
DEF TEMP-TABLE SvRapo NO-UNDO
   FIELD tprocess     AS I
   FIELD tidentif     AS C 
   FIELD tname        AS C
   FIELD tbankacc     AS C
   FIELD toldbankacc  AS C
   FIELD tHandMsg     AS C
   INDEX tprocess tprocess ASCENDING.

DEF STREAM suoravalt.


FORM HEADER
   FILL("=",112) FORMAT "x(112)" SKIP
   ynimi AT 1 FORMAT "X(30)" 
      "DIRECT DEBIT AUTHORIZATIONS" AT 45 "Page" AT 103
      sl FORMAT "ZZZZ9" SKIP
   icAuthFile AT 1 FORMAT "X(100)" 
      STRING(today,"99-99-99") TO 112 SKIP
   FILL ("=",112) FORMAT "x(112)" SKIP(1)

   "Ident."         AT 1
   "Name"           AT 20 
   "Bank Account"   AT 42
   "Process"        AT 57
   SKIP
   FILL("-",112) FORMAT "x(112)" SKIP
WITH WIDTH 112 NO-LABEL NO-BOX FRAME SivuOts.


FUNCTION fCopy2Db RETURNS LOGICAL.

   ASSIGN DDAuth.Identification = Yksilointi 
          DDAuth.CustName       = Nimi
          DDAuth.ddProcess      = KasTun
          DDAuth.Archive        = ArTun
          DDAuth.OldBankAcc     = IF VanhaPankkiTili NE ""
                                  THEN VanhaPankkiTili
                                  ELSE IF Kastun = 2 
                                       THEN DDAuth.BankAcc
                                       ELSE ""
          DDAuth.BankAcc        = PankkiTili
          DDAuth.AuthDate       = TaPVM.

END FUNCTION.

FUNCTION fCopy2Failed RETURNS LOGICAL.

   CREATE DDAuth.
   ASSIGN DDAuth.Brand   = gcBrand
          DDAuth.CustNum = 0
          DDAuth.AuthID  = NEXT-VALUE(dpseq).
          
   fCopy2Db().   

   IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhDDAuth).

   CREATE Memo.
   ASSIGN Memo.Brand     = gcBrand
          Memo.HostTable = "DDAuth"
          Memo.KeyValue  = STRING(DDAuth.AuthId)
          Memo.CustNum   = 0
          Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
          Memo.CreUser   = katun 
          Memo.MemoTitle = "Failed Authorization"
          Memo.MemoText  = HandMsg.
          Memo.CreStamp  = fMakeTS().
        
END FUNCTION.


ASSIGN palvtunn   = fCParamC("DDebitServiceCode")
       lcConfDir  = fCParamC("RepConfDir")
       /* directory for booking list */
       lcRepDir   = fCParamC("RefPrintDir")
       llRejected = FALSE
       lcCKatun   = katun
       katun      = "Bank". 


TIEDOSTO:
REPEAT TRANSACTION:

   INPUT STREAM suoravalt FROM VALUE(icAuthFile) NO-ECHO. 

   ASSIGN rlkm       = 0
          lkm        = 0
          ldtDefDate = TODAY.

   REPEAT:

      IMPORT STREAM suoravalt UNFORMATTED svrivi.
      IF trim(svrivi) = "EOF" OR
         svrivi = ""          OR
         svrivi BEGINS "$" 
      THEN NEXT.

      tietuetunnus = SUBSTR(svrivi,1,5). /* on muotoa "SVVA9", sis‰lt‰‰                                                      samalla aineistotunnuksen */

      IF tietuetunnus = "SVVA0" THEN DO:
         
         IF SUBSTR(svrivi,21,9) NE palvtunn THEN DO: /* jos v‰‰r‰ aineisto */
            HandMsg = "Header record erroneous, file is rejected".
            
            IF ilDispMsg THEN 
            MESSAGE HandMsg
            VIEW-AS ALERT-BOX TITLE " REMARK ! ".
           
            RUN pFileError.
            
            LEAVE.
         END.

         /* default date for events */
         ldtDefDate = DATE(INT(SUBSTR(svrivi,10,2)),
                           INT(SUBSTR(svrivi,12,2)), 
                           INT(SUBSTR(svrivi,6,4))) NO-ERROR.
      END.

      ELSE  DO:

         IF tietuetunnus = "SVVA9" THEN DO:
            /* tuli summatietue */
            IF INTEGER(SUBSTR(svrivi,6,6)) NE rlkm THEN DO:

               HandMsg = "Summary record erroneous, file is rejected".
               IF ilDispMsg THEN
               MESSAGE HandMsg
               VIEW-AS ALERT-BOX TITLE " REMARK ! ".
             
               RUN pFileError.
               
               UNDO tiedosto, LEAVE tiedosto.
            END.   

            rlkm = 0. 

         END.
         ELSE DO:  /* valtuutusaineisto */

           IF tietuetunnus NE "SVVA1" THEN DO: 

              HandMsg = "Authorization record erroneous, file is rejected".
              IF ilDispMsg THEN 
              MESSAGE HandMsg
              VIEW-AS ALERT-BOX TITLE " REMARK ! ".

              RUN pFileError.
              
              UNDO tiedosto, LEAVE tiedosto.   /* ollutkaan valtuutustietue */
           END.                                /* koko aineisto hyl‰t‰‰n    */

           ASSIGN 
           rlkm            = rlkm + 1
           liRead          = liRead + 1
           Yksilointi      = SUBSTR(svrivi,27,30) /* sopimus: = asnro */
           Nimi            = SUBSTR(svrivi,57,35)
           KasTun          = INTEGER(SUBSTR(svrivi,6,1))
           ArTun           = SUBSTR(svrivi,92,20)
           PankkiTili      = SUBSTR(svrivi,118,14)
           TaPVM           = IF SUBSTR(svrivi,132,8) = "" OR 
                                SUBSTR(svrivi,132,4) = "0000"
                             THEN ldtDefDate
                             ELSE DATE(INT(SUBSTR(svrivi,136,2)) , /* mm */
                                      INT(SUBSTR(svrivi,138,2)) , /* dd */
                                      INT(SUBSTR(svrivi,132,4)))  /* yy */
           VanhaPankkiTili = SUBSTR(svrivi,145,14)
           HandMsg          = "".

           /* Viestit k‰sittelytunnuksittain alkaa */
           IF      KasTun = 1 THEN HandMsg = "New/ ". 
           ELSE IF KasTun = 2 THEN HandMsg = "Change/ ". 
           ELSE IF KasTun = 4 THEN HandMsg = "Maintenance/ ". 
           ELSE IF KasTun = 3 THEN HandMsg = "Termination/ ". 
           /* tmscodes:sta... */

           ASSIGN yksilointi = RIGHT-TRIM(yksilointi)
                  yksilointi = TRIM(yksilointi).
                  
           /* Yksilˆintitiedon oikeellisuustarkistus */

           Pituus = LENGTH(Yksilointi).
           
           llFailed = FALSE.

           DO i = 1 TO Pituus:
              IF LOOKUP(SUBSTR(Yksilointi,i,1),"1,2,3,4,5,6,7,8,9,0") = 0 
              THEN NEXT.
              LEAVE.
           END.
           IF i < LENGTH(yksilointi) 
           THEN yksilointi = TRIM(SUBSTRING(yksilointi,i)). 

           Pituus = LENGTH(Yksilointi).
           
           DO i = 1 TO Pituus:
              IF LOOKUP(SUBSTR(Yksilointi,i,1),
                        "1,2,3,4,5,6,7,8,9,0,-, ") = 0 
                 THEN DO:
                 HandMsg = HandMsg + 
                 "Identification consists of wrong characters". 
                 RUN pErrorMsg.
                 llFailed = TRUE.
                 LEAVE.
              END.
           END.

           IF llFailed THEN DO:
              fCopy2Failed().
              NEXT. 
           END.
           
           /* first try to find customer, if not available then check if 
              cli exists */
           liCustNum = 0.   
           IF INDEX(yksilointi,"-") = 0 AND LENGTH(yksilointi) <= 8
           THEN DO:
              FIND Customer NO-LOCK WHERE
                   Customer.Brand = gcBrand AND
                   Customer.CustNum = INTEGER(yksilointi) NO-ERROR.
              IF AVAILABLE Customer THEN liCustNum = Customer.CustNum.
           END.
           
           IF liCustNum = 0 AND 
              yksilointi BEGINS "0" AND 
              LENGTH(yksilointi) > 8
           THEN DO:
           
              ASSIGN lcCLI     = REPLACE(yksilointi,"-","")
                     lcCLI     = REPLACE(lcCLI," ","")
                     liInvCust = 0.
              
              FOR FIRST MobSub NO-LOCK WHERE
                        MobSub.Brand = gcBrand AND
                        MobSub.CLI   = lcCLI:
                 ASSIGN liCustNum = MobSub.CustNum
                        liInvCust = MobSub.InvCust.
              END.
              
              /* already killed ? */
              IF liCustNum = 0 THEN 
              FOR FIRST MsOwner NO-LOCK WHERE
                        MsOwner.Brand = gcBrand AND
                        MsOwner.CLI   = lcCLI:
                 ASSIGN liCustNum = MsOwner.CustNum
                        liInvCust = MsOwner.InvCust.
              END.
              
              IF liCustNum > 0 THEN DO:

                 /* user customer cannot make the authorization */
                 IF liInvCust NE liCustNum THEN DO:
                    HandMsg = HandMsg + 
                             "MSISDN belongs to user customer". 
                    RUN pErrorMsg.
              
                    fCopy2Failed().
                    NEXT. 
                 END. 
                 
                 FIND Customer WHERE Customer.CustNum = liCustNum 
                 NO-LOCK NO-ERROR.  
              END.
                 
           END.

           IF liCustNum = 0 OR 
              NOT AVAILABLE Customer OR
              Customer.Brand NE gcBrand
           THEN DO:
              HandMsg = HandMsg + 
                        "Customer could not be identified". 
              RUN pErrorMsg.

              fCopy2Failed().
              NEXT. 
           END.
           
           /* Pankkitilin oikeellisuustarkistus */
           Pituus = LENGTH(TRIM(Pankkitili)).
           IF Pituus NE 14 THEN DO:
              HandMsg = HandMsg + "Length of bank account is wrong". 
              RUN pErrorMsg.
              fCopy2Failed().
              NEXT. 
           END.

           DO i = 1 TO Pituus:
              IF LOOKUP(SUBSTR(Pankkitili,i,1),"1,2,3,4,5,6,7,8,9,0") = 0 
                 THEN DO:
                 HandMsg = HandMsg +
                 "Bank account consists of wrong characters". 
                 RUN pErrorMsg.
                 llFailed = TRUE.
                 LEAVE.
              END.
           END.
           
           IF llFailed THEN DO:
              fCopy2Failed().
              NEXT. 
           END.
           
           /* K‰sittelytunnusten mukaiset k‰sittelyt */

           /* Uusi */
           IF KasTun = 1 THEN DO: 
              /*********************************************** 
              * Criteria based on agreement WITH TMS user !  *
              * Yksilointi means customer number here.       *
              ***********************************************/
              FIND CURRENT Customer EXCLUSIVE-LOCK.

              /* tarkistetaan, onko sv-valtuutusta 
                 samalla pankkiyhteydell‰                         */
              Customer.ChargeType = 2.

              FIND FIRST DDAuth WHERE 
                 DDAuth.CustNum = liCustNum AND
                 DDAuth.BankAcc = PankkiTili NO-LOCK NO-ERROR.

              IF NOT AVAILABLE DDAuth THEN DO:
                 CREATE DDAuth.
                 ASSIGN DDAuth.Brand   = Customer.Brand 
                        DDAuth.CustNum = Customer.CustNum
                        DDAuth.AuthID  = NEXT-VALUE(dpseq).
              END.
              ELSE IF AVAILABLE DDAuth THEN DO:
                 HandMsg = HandMsg + "Authorization already exists". 
                 RUN pErrorMsg.
                 fCopy2Failed().
                 NEXT.
              END.

           END.

           /* Muutos ja Huolto */
           ELSE IF KasTun = 2 OR KasTun = 4 THEN DO: 
             
              FIND FIRST DDAuth WHERE 
              DDAuth.Brand   = gcBrand AND
              DDAuth.CustNum = Customer.CustNum
              EXCLUSIVE-LOCK NO-ERROR.

              /* Jos ei ole sv-valtuutusta 
                 samalla tai vanhalla pankkiyhteydell‰ */
              IF NOT AVAILABLE DDAuth THEN DO:
                 HandMsg = HandMsg + "Authorization was not found".
                 RUN pErrorMsg.
                 fCopy2Failed().
                 NEXT.
              END.
       
              IF llDoEvent THEN RUN StarEventSetOldBuffer(lhDDAuth).
           END.

           /* Irtisanominen */
           ELSE IF KasTun = 3 THEN DO: 
              FIND FIRST DDAuth WHERE 
                 DDAuth.CustNum = Customer.CustNum AND
                 DDAuth.BankAcc = PankkiTili
                 EXCLUSIVE-LOCK NO-ERROR.

              IF NOT AVAILABLE DDAuth THEN DO:
                 HandMsg = HandMsg + "Authorization is not found".
                 RUN pErrorMsg.
                 fCopy2Failed().
                 NEXT.
              END.
 
              IF DDAuth.AuthDate > Tapvm THEN DO:
                 HandMsg = HandMsg + "Current authorization is newer".
                 RUN pErrorMsg.
                 fCopy2Failed().
                 NEXT.
              END.
               
              IF TaPVM <= TODAY THEN DO:
                 HandMsg = HandMsg + "Authorization is removed".

                 IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhDDAuth).

                 DELETE DDAuth. 

                 FIND CURRENT Customer EXCLUSIVE-LOCK.
                 
                 
                 /* are there still other authorizations */
                 RUN Ar/nnsvte.p (Customer.CustNum,
                             TODAY,
                             OUTPUT lcBankAcc).
                 /* if not then update charge type */            
                 IF lcBankAcc = "" THEN Customer.ChargeType = 1. 

                 RUN pErrorMsg.
                 lkm = lkm + 1. /* kuuluu hyv‰ksytt, vaikka ei talletusta */
                 NEXT.
              END.
              
           END.

           /********************************************* 
            *  yhteiset toimenpiteet 1,2,4 -tyypeille * 
            *  jos valtuutus hyv‰ksyttiin               *
            *********************************************/
           fCopy2Db(). 
           
           IF llDoEvent THEN DO:
              IF Kastun = 1 
              THEN RUN StarEventMakeCreateEvent(lhDDAuth).
              ELSE RUN StarEventMakeModifyEvent(lhDDAuth).
           END. 
           
           lkm = lkm + 1.       

           HandMsg = HandMsg + "Ok". 
           RUN pErrorMsg.

         END.
      END.

      NEXT.    
   END.

   PAUSE 0.
   INPUT STREAM suoravalt CLOSE.

 
   IF NOT llRejected THEN DO:
      IF tietuetunnus NE "SVVA9" THEN DO:  /* jos jostakin syyst‰ viimeinen */
      
         HandMsg = "Summary record erroneous, file is rejected".

         IF ilDispMsg THEN
         MESSAGE HandMsg
         VIEW-AS ALERT-BOX ERROR.

         RUN pFileError.
 
         UNDO tiedosto, LEAVE tiedosto.    /* ei ollutkaan summatietue,     */
      END.                                 /* aineisto hyl‰t‰‰n             */
   END.

   LEAVE.

END. /* tiedosto TRANSACTION */

oiRead = lkm.

IF lkm >= 0 AND ilDispMsg THEN DO:
   MESSAGE 
   liRead "authorization lines were read from file" icAuthFile SKIP
   lkm "of them were accepted." 
   VIEW-AS ALERT-BOX 
   TITLE "".
END.

IF CAN-FIND(FIRST SvRapo) OR CAN-FIND(FIRST ttError) THEN DO:

   ASSIGN
   rl = 7
   sl = sl + 1.
   VIEW STREAM tul FRAME SivuOts.

   FOR EACH SvRapo:
      IF rl + 1 >= skayt1 THEN DO:
         /* Turn the page */
         {Syst/uprfeed.i rl}
         ASSIGN sl = sl + 1 rl = 7.
         VIEW STREAM tul FRAME SivuOts.
      END.   

      PUT STREAM tul
      TRIM(SvRapo.tidentif)   FORMAT "x(18)" AT 1
      SvRapo.tname            FORMAT "x(21)" AT 20
      SvRapo.tbankacc         FORMAT "x(14)" AT 42
      SvRapo.tHandMsg         FORMAT "x(55)" AT 57.   
      rl = rl + 1.
   END.
 
   FOR EACH ttError:
      IF rl + 1 >= skayt1 THEN DO:
         {Syst/uprfeed.i rl}
         ASSIGN sl = sl + 1 rl = 7.
         VIEW STREAM tul FRAME SivuOts.
      END.   

      PUT STREAM tul
      ttError.ErrorMsg AT 1 FORMAT "X(112)" SKIP.
      rl = rl + 1.
   END.
    
END.      

{Syst/uprfeed.i rl}

/* CLOSE STREAM */
tila = FALSE.
{Syst/utuloste.i}

/* reset normal user */
katun = lcCKatun.

/* clean eventlog */
fCleanEventObjects().

HIDE FRAME LOG NO-PAUSE.
HIDE MESSAGE NO-PAUSE.

/* send the booking list via mail */
IF ilSendMail AND SEARCH(oso) NE ? THEN DO:
    
   /* mail recipients */
   GetRecipients(lcConfDir + 
                 "ddauthorization.email").
                  
   IF xMailAddr > "" THEN DO:
    
      ASSIGN lcMailFile = "/tmp/ddauth_" +
                          STRING(YEAR(TODAY),"9999") +
                          STRING(MONTH(TODAY),"99")  +
                          STRING(DAY(TODAY),"99")    + 
                          "_" + STRING(TIME) + ".txt".
                                    
      OUTPUT STREAM tul TO VALUE(lcMailFile).
      PUT STREAM tul UNFORMATTED
         "Direct debit authorizations read on " 
         STRING(TODAY,"99.99.9999") 
         " from file " icAuthFile
         SKIP.
      OUTPUT STREAM tul CLOSE. 

      SendMail(lcMailFile,
               oso).
   END.
                
END.

/* move the booking list to archive */
IF lcRepDir > "" AND SEARCH(oso) NE ? THEN DO:
   fTransDir(oso,
             "",
             lcRepDir).
END.

PROCEDURE pFileError:

   CREATE ttError.
   ASSIGN ttError.ErrorMsg = HandMsg
          lkm              = -1
          llRejected       = TRUE.

END PROCEDURE.

PROCEDURE pErrorMsg:

   /* Add into TEMP-TABLE FOR reporting */
   CREATE SvRapo.
   ASSIGN
   SvRapo.tprocess    = KasTun
   SvRapo.tidentif    = Yksilointi
   SvRapo.tname       = Nimi
   SvRapo.tbankacc    = PankkiTili
   SvRapo.toldbankacc = VanhaPankkiTili
   SvRapo.tHandMsg    = HandMsg.
END.



