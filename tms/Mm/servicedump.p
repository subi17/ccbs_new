/* -----------------------------------------------------------------
   MODULE .......: servicedump.p
   TASK .........: dump client netservices to netservice.dump
   APPLICATION ..: TMS
   AUTHOR .......: JL
   CREATED ......: 09.11.04
   CHANGED ......: 13.12.04/aam SSDate for SubSer
                   25.02.05/mvi+jl one delim changed to ":"
                            taken into production use 25.2.2005
                   13.01.06/aam OrgId from customer         
   Version ......: SHARK
----------------------------------------------------------------- */

{Syst/commpaa.i}
{Func/excel.i}
{Func/date.i}
{Func/timestamp.i}
{Func/cparam2.i}

gcbrand = "1".
katun = "batch".

DEF TEMP-TABLE ttService
   FIELD tmsseq    as integer
   FIELD tcustnum  as integer
   FIELD tcli      as ch
   FIELD tpersonid as ch
   FIELD torigid   as ch
   FIELD tclist    as ch.

DEFINE var afilename AS CHARACTER  NO-UNDO.

def var numform     as c  no-undo.
def var lcDelim     as c no-undo.
def var lcspooldir  as ch no-undo.
def var lcoutdir    as ch no-undo.
def var lcdir       as ch no-undo.
def var lcfilename  as ch no-undo.
def var liToday     as integer no-undo.
def var timeto      as integer no-undo.
def var icounter    as integer no-undo init 0.

ASSIGN
  lcOutDir   = fCParam("dumpoutgoing","servicedump.p")
  lcspooldir = fCParam("dumpspool","servicedump.p")
  lcDelim = "|"
  liToday = integer(fdatefmt(TODAY,"yyyymmdd"))
  lcFileName = "services" + fdatefmt(TODAY,"yyyymmdd") + ".dump"
  timeto = fmakets().

FOR EACH mobsub NO-LOCK WHERE
         mobsub.brand = "1":

   FIND Customer WHERE Customer.CustNum = MobSub.AgrCust NO-LOCK NO-ERROR.

   CREATE ttService.
   ASSIGN
     ttService.tmsseq    = mobsub.msseq
     ttService.tcustnum  = mobsub.custnum
     ttService.tcli      = mobsub.cli
     ttService.tpersonid = Customer.OrgId WHEN AVAILABLE Customer
     ttService.torigid   = ttService.tpersonid.

   FOR EACH subser NO-LOCK WHERE
            subser.msseq = mobsub.msseq 
            BREAK BY SubSer.ServCom 
                  BY SubSer.SSDate DESC:
            
      /* use newest */
      IF FIRST-OF(SubSer.ServCom) THEN DO:          
         IF subser.ssstat <= 0 THEN NEXT.
         ttService.tclist  = ttService.tclist + subser.servcom + "=" 
         + STRING(subser.ssstat) + ":".
      END.   
   END.
END.

OUTPUT STREAM excel TO VALUE(lcSpooldir + lcfilename).



FOR EACH ttService NO-LOCK:

/* dump the information */
PUT STREAM excel UNFORMATTED
   ttService.tmsseq    lcDelim
   ttService.tcustnum  lcDelim
   ttService.tcli      lcDelim
   ttService.tpersonid lcDelim
   ttService.tclist    SKIP.
END.

OUTPUT STREAM excel CLOSE.
    
UNIX SILENT VALUE("mv " + lcSpooldir + lcfilename + " " + lcOutDir).
