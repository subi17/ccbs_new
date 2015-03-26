{commpaa.i} katun = "SOG". gcbrand = "1".
{fsubser.i}
{timestamp.i}
{msreqfunc.i}

DEF BUFFER provMSREquest FOR MSRequest.
DEF BUFFER provCliType   FOR CLIType.
DEF BUFFER provIMSI      FOR IMSI.
DEF BUFFER provSIM       FOR SIM.
DEF BUFFER provOrder     FOR Order.

function fMakeCommLine returns CHAR
(INPUT icPayType AS CHAR, 
INPUT icProfile AS CHAR,
 INPUT icValue     AS CHAR).
                      
   DEF VAR lcAdkey    AS CHAR NO-UNDO.
   DEF VAR lcReturn   AS CHAR NO-UNDO.
   DEF VAR lcPayTypes AS CHAR NO-UNDO INIT "UNKNOWN,POSTPAID,PREPAID".


   FIND FIRST customer where
      customer.custnum = mobsub.custnum NO-LOCK NO-ERROR.
  
   /* CREATE extra parameters */ 
   IF icValue  = "CREATE" THEN DO:
      
      FIND FIRST Sim WHERE
                 Sim.ICC = MobSub.ICC NO-LOCK NO-ERROR.
      
      FIND FIRST CliType WHERE 
                 CliType.Brand    = "1"  AND 
                 CliType.CliType  = msrequest.reqcparam2 NO-LOCK NO-ERROR.
      
      IF AVAIL SIM THEN
      FIND FIRST SimBatch WHERE
                 SimBatch.Brand    = gcBrand AND
                 SimBatch.SimBatch = Sim.SimBatch NO-LOCK NO-ERROR. 

      FIND FIRST IMSI WHERE IMSI.ICC = SIM.ICC NO-LOCK NO-ERROR.
      
      IF Avail SimBatch THEN 
      ASSIGN lcAdkey = ",ADKEY=" + SimBatch.TPkey.
      
      ASSIGN lcADkey = lcAdKey + 
                       ",PAYTYPE=" + 

         (  IF CLIType.PayType + 1 <= NUM-ENTRIES(lcPayTypes) 
            THEN ENTRY(CLIType.PayType + 1,lcPayTypes) else icPaytype)  +
                       ",PROFILE=" + CLIType.ServicePack +
                       ",KI=" + IMSI.KI +
                       ",COS=2,ORGID=1,".
      
      IF Order.MnpStatus > 0 THEN lcAdkey = lcAdKey + "MNP=1,".
       
      IF Avail CliType THEN DO:
         IF CliType.ServiceClass ne "" THEN 
            lcAdkey = lcAdkey + "SERVICECLASS=" + CliType.ServiceClass + ",".
      END.

      /* Set lang for create command */
      lcAdkey = lcAdkey + "LANG=" + STRING(Customer.Language) + ",". 
         
   END.

   if avail solog then lcReturn = string(solog.solog) + " ".
   lcReturn = lcReturn + icValue                      + ","  +     /* Action  */
             "MSISDN=34" + mobsub.Cli        + ","  +     /* MSISDN       */
             "ICC="    + IMSI.icc        + ","  +     /* ICC          */
             "IMSI="   + Imsi.Imsi        + ","  +     /* IMSI         */
             "OPERATOR=YOIGO,NW=ERICSSON" +
             lcAdkey .



   RETURN lcReturn.

END.   

def stream slog.
def stream sreq.
/*
output stream slog to /apps/snet/200901/as_yts1250.modify.log append.
output stream sreq to /apps/snet/200901/as_yts1250.modify_req.log append.
*/
output stream slog to /home/anttis/ongoing3.log append.
output stream sreq to /home/anttis/ongoing3.mod.log append.

DEF VAR lcPaytype AS CHAR NO-UNDO.
DEF VAR lcProfile as CHAR NO-UNDO.
DEF VAR lcCommline AS CHAR NO-undo.
def var lcservices as char no-undo.
DEF VAR lcActStamp as dec no-undo.

def stream sin.
input stream sin from /home/anttis/ongoing.txt.

DEFINE VARIABLE lcCli AS CHARACTER NO-UNDO. 
DEFINE VARIABLE llPaytype AS LOGICAL NO-UNDO. 
DEFINE VARIABLE i AS INTEGER NO-UNDO. 

i = 0.
REPEAT:

   import stream sin unformatted lcCli.
/*
   i = i + 1.
   if i > 1 then next.
*/
   lcservices = "".
   lcCommline = "".

   find mobsub where mobsub.cli = entry(1,lcCli," ") no-lock no-error.

   FIND Imsi WHERE Imsi.Imsi = MobSub.Imsi NO-LOCK NO-ERROR.
/*
   IF imsi.imsi ne entry(2, lcCLi, " ") then do: 
      put stream slog unformatted "Error IMSI " mobsub.cli " "
      imsi.imsi " ne " entry(2, lcCLi, " ") skip. 
   END.

   IF imsi.icc ne entry(3, lcCLi, " ") then do: 
      put stream slog unformatted "Error ICC " mobsub.cli " "
      imsi.icc " ne " entry(3, lcCLi, " ") skip. 
   END.*/
/*  
   if lookup(mobsub.cli, "622495933 622501074 622554620 622721305 663539001 663788631 665265922 667346569 675351800 676572692 677225120 677350743 679409809 679982602 686593914 690213839 691725556 692231994 696035608 696189887 699646188", " ") > 0 then next.
 */
   FIND FIRST msrequest where
      msrequest.msseq   = mobsub.msseq and
      msrequest.reqtype = 0 and
      msrequest.reqstatus = 3 and
      msrequest.actstamp >= 20090101 use-index msseq NO-LOCK NO-ERROR.
   
   IF NOT avail msrequest then
   FIND FIRST msrequest where
      msrequest.msseq   = mobsub.msseq and
      msrequest.reqtype = 0 and
      msrequest.reqstatus = 7 and
      msrequest.actstamp >= 20090101 use-index msseq NO-LOCK NO-ERROR.
   
   IF AVAIL msrequest then do:
      IF msrequest.reqcparam2 ne mobsub.clitype then do:
        
        find current msrequest EXCLUSIVE-LOCK NO-ERROR.
        put stream sreq unformatted msrequest.msrequest " " msrequest.reqsource skip.
       assign msrequest.reqsource = "5".  
   end.
   else put stream sreq unformatted "OK " mobsub.cli skip.
   end.
   else do:
      put stream sreq unformatted "NOT FOUND " mobsub.cli skip.
      next.
   end.
   
   /*IF INT(MobSub.PayType) = 1 THEN */

   IF msrequest.reqcparam2 begins "cont" THEN 
   ASSIGN lcPayType = "POSTPAID"
          lcProfile = "1"
          llPayType = FALSE.
      ELSE 
   ASSIGN lcPayType = "PREPAID"
         lcProfile = "2"
         llPayType = TRUE.

   lcActStamp = fMakeTS().
   
   FIND FIRST Order WHERE 
              Order.MSSeq = MobSub.MSSeq AND
              Order.OrderType NE 2 NO-LOCK NO-ERROR.
  /* 
   CREATE Solog.
   ASSIGN
      Solog.Solog = NEXT-VALUE(Solog).

   ASSIGN
      Solog.CreatedTS    = lcActStamp 
      Solog.MsSeq        = Mobsub.MsSeq    /* Mobile Subscription No.    */
      Solog.CLI          = Mobsub.CLI      /* MSISDN                     */
      Solog.Stat         = 0               /* just created               */
      Solog.Brand        = gcBrand
      Solog.Users        = katun.
  
   ASSIGN
      Solog.TimeSlotTMS  = lcActStamp
      Solog.ActivationTS = lcActStamp
      Solog.MSrequest    = 0.
   
   lcCommline = "".
   lcCommLine = fMakeCommLine(lcProfile,lcPaytype,"CREATE").
   ASSIGN
      lcCommLine = lcCommLine + ",REQUESTID=0"
      lcCommLine = REPLACE(lcCommLine,",,",",").
   ASSIGN 
   Solog.CommLine = lcCommLine.

   put stream slog unformatted mobsub.cli "|" Solog.Solog "|" lcCommLine skip.

/*
   disp lcCommLine view-as editor size 60 by 60. 
pause. */
   /*
                 disp substr(lcCommline,150,78) format "X(78)".
*/
   */
   /* MODIFY SOLOG BEGINS */
 
   FOR EACH SubSer where SubSer.MsSeq = mobsub.msseq and subser.servpac = "" no-lock break by subser.servcom by subser.servcom.
   if first-of(subser.servcom) then do:
   lcServices = lcServices + "," + subser.servcom + "=" + STRING(fCurrSubSer(MobSub.MsSeq,subser.servcom)) .
   end.
   end.
  
   lcCommline = " MODIFY,MSISDN=34" + MobSub.Cli + ",ICC=" + 
                Imsi.ICC + ",IMSI=" + Imsi.IMSI + 
                ",OPERATOR=YOIGO,NW=ERICSSON,ADKEY=1,PAYTYPE="+ lcPaytype +
                 ",PROFILE=" + lcProfile 
                 + ",KI=" + Imsi.ki + ",COS=2,ORGID=1,".
    IF Order.MNPStatus > 0 THEN lcCommline = lcCommline + "MNP=1".
                       
   lcCommline =  lcCommLine +  lcservices  + ",REQUESTID=0".

/*   disp lcCommLine view-as editor size 60 by 60. */

    
   CREATE Solog.
    ASSIGN
        Solog.Solog = NEXT-VALUE(Solog).
              
    ASSIGN
        Solog.CreatedTS    = lcActStamp
        Solog.MsSeq        = MobSub.MsSeq    /* Mobile Subscription No.    */
        Solog.CLI          = MobSub.Cli      /* MSISDN                     */
        Solog.Stat         = 0               /* just created               */
        Solog.Brand        = gcBrand
        Solog.Users        = katun.
   ASSIGN     
        Solog.TimeSlotTMS  = lcActStamp
        Solog.ActivationTS = lcActStamp
        Solog.MSrequest    = 0.

    ASSIGN 
       Solog.Commline = STRING(Solog.Solog) + lcCommLine.  
       
   put stream slog unformatted mobsub.cli "|" Solog.Solog "|" lcCommLine skip.

END.

input stream sin close.
output stream slog close.
output stream sreq close.
