DEFINE VARIABLE katun AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcBrand AS CHARACTER NO-UNDO.
katun = "cron".
gcbrand = "1".

{date.i}
{xmlfunction.i}
{tsformat.i}

DEF VAR lcFilename   AS C    NO-UNDO.

def stream excel.

ASSIGN
   session:numeric-format = "AMERICAN"
   lcfilename = "ycm309.dat".

OUTPUT STREAM excel TO VALUE("/apps/snet/200802/" + lcfilename).
DEFINE VARIABLE i AS INTEGER NO-UNDO.

FOR EACH PrepaidRequest NO-LOCK WHERE
         PrepaidRequest.Brand = "1" AND
         PrepaidRequest.SOurce = "ATM" AND
         PrepaidRequest.TSRequest >= 20080125 and
         PrepaidRequest.TSREquest < 20080126:
   PUT STREAM excel UNFORMATTED
      
      fGetNodeValue(PrepaidRequest.CommLine,"Entidad") ";"
      fGetNodeValue(PrepaidRequest.CommLine,"entidadOrigen") ";"
      fGetNodeValue(PrepaidRequest.CommLine,"Tipo") ";"
      fGetNodeValue(PrepaidRequest.CommLine,"numOper") ";"
      fGetNodeValue(PrepaidRequest.CommLine,"referencia") ";"
      fGetNodeValue(PrepaidRequest.CommLine,"claveLocal") ";"
      fGetNodeValue(PrepaidRequest.CommLine,"fecha") ";"
      fTSFormat("yyyymmdd HH:MM:ss", PrepaidRequest.TSRequest) ";" 
      fGetNodeValue(PrepaidRequest.CommLine,"Tlf") ";"
      fGetNodeValue(PrepaidRequest.CommLine,"Importe") ";"
      fGetNodeValue(PrepaidRequest.CommLine,"Impuesto") ";"
      fGetNodeValue(PrepaidRequest.CommLine,"codPos") ";"
      PrepaidRequest.RespCode
   SKIP. 
END.

OUTPUT STREAM excel CLOSE.
