/* ----------------------------------------------------------------------
  MODULE .......: dumpall2.i
  TASK .........: dump all fields for tables
  APPLICATION ..: tms
  AUTHOR .......: tk
  CREATED ......: 26.05.04
  CHANGED ......: 07.02.06 mvi/ dump files compressed with zip
                  05.09.06/aam  spool directed to /tmp
                  11.09.06 kl /tmp/ instead of /tmp
                  14.09.06 kl commpaa + gcBrand

  Version ......: TeleFinland
  ---------------------------------------------------------------------- */

{commpaa.i}
{excel.i}
{date.i}
{cparam2.i}

def var i as i no-undo.
def var lcspooldir as ch no-undo.
def var lcoutdir as ch no-undo.
def var lcdir as ch no-undo.
def var lcfilename as ch no-undo.

gcBrand = "1".

assign 
   lcoutdir   = fCParam("dumpoutgoing","dumpall2.i")
   lcspooldir = fCParam("dumpspool","dumpall2.i")
   lcFileName = "{1}" + fdatefmt(TODAY,"yyyymmdd") + 
                STRING(TIME,"HH:MM:SS") + ".dump".
   lcFileName = REPLACE(lcFileName,":","").


output stream excel to value(lcspooldir + lcfilename).

for each {1} no-lock :
   put stream excel unformatted
      {2} skip.
end.


output stream excel close.

/* move .dump file */
unix silent value("mv " + lcspooldir + lcfilename + " " + lcoutdir).      
