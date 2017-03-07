 /* ------------------------------------------------------
  MODULE .......: ageanalb.p
  FUNCTION .....: age analysis report as batch
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 20.08.03 
  MODIFIED .....: 
  VERSION ......: M15
  ------------------------------------------------------ */


{Syst/commpaa.i}
{Syst/utumaa.i "new"}
{Ar/ageanal.i}
{Func/cparam2.i}
{Func/email.i}

DEF VAR exfile      AS CHAR  FORMAT "X(40)"    NO-UNDO.
DEF VAR lcConfDir   AS CHAR                    NO-UNDO.

gcBrand = "1".

ASSIGN lcConfDir = fCParamC("RepConfDir")
       exfile    = "/tmp/ageanal_" + 
                   STRING(YEAR(TODAY),"9999") +
                   STRING(MONTH(TODAY),"99")  +
                   STRING(DAY(TODAY),"99") +
                   "_" + STRING(TIME) + ".txt".

FIND FIRST Company NO-LOCK.
ynimi = Company.CompName.

CREATE ttCriter.
ASSIGN ttCriter.InvGroup = ""
       ttCriter.RepDate  = TODAY
       ttCriter.Day11    = 1
       ttCriter.Day12    = 7
       ttCriter.Day21    = 8
       ttCriter.Day22    = 30
       ttCriter.Day31    = 31
       ttCriter.Day32    = 60
       ttCriter.Day41    = 61
       ttCriter.Day42    = 90
       ttCriter.Day51    = 91
       ttCriter.Day52    = 180
       ttCriter.DayOver  = 180
       ttCriter.ToFile   = ExFile
       ttCriter.OnlySum  = TRUE
       ttCriter.SortBy   = 1.

RUN Ar/ageanal.p (INPUT TABLE TCustGroup,
             INPUT TABLE ttCriter).

/* send the report AS email */
ASSIGN xMailAttach = exfile
       exfile      = "/tmp/ageanal_msg.txt".

/* header message */      
OUTPUT STREAM tul TO VALUE(exfile).
PUT STREAM tul UNFORMATTED
   "Age analysis report " + 
   STRING(TODAY,"99.99.9999") + "."
   SKIP.
OUTPUT STREAM tul CLOSE.

/* mail recipients */
GetRecipients(lcConfDir + "ageanal.email").

IF xMailAddr > "" THEN DO:
   /* actual sending */  
   SendMail(exfile,xMailAttach).
END.





