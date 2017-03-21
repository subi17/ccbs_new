/* ----------------------------------------------------------------------
  MODULE .......: nodo_response_mock.p
  TASK .........: Program simulates NodoCentral functionality in
                  MB_migration handling.
                  It reads input file and updates statuses as required.
                  Current version uses hard coded rules.
  APPLICATION ..: tms
  AUTHOR .......: ilsavola
  VERSION.......:
  CREATED ......: 7.2.17
  CHANGED ......:
  ------------------------------------------------------------------------*/
{tmsconst.i}
{commpaa.i}
{cparam2.i}
{timestamp.i}
{ftransdir.i}
gcBrand = "1".
DEF INPUT PARAMETER icInput AS INT.
/* 0 OK, 1 or 2 errors*/
DEF STREAM sIn.
DEF STREAM sOut.
DEF STREAM sFile.
DEF VAR lcDir AS CHAR NO-UNDO.
DEF VAR lcInputFile AS CHAR No-UNDO.
DEF VAR lcFileName AS CHAR NO-UNDO.
DEF VAR lcMSISDN AS CHAR NO-UNDO.
DEF VAR lcLine AS CHAR NO-UNDO.
DEF VAR lcOutLine AS CHAR NO-UNDO.
DEF VAR lcResponseDir AS CHAR NO-UNDO.
DEF VAR lcOutFile AS CHAR NO-UNDO.

ASSIGN
   lcResponseDir = fCParam("MB_Migration", "MigrationInDir")
   lcDir = fCParam("MB_Migration", "MigrationOutDir").


IF lcDir EQ "" OR lcDir EQ ? THEN lcDir = "/tmp/".
ELSE DO:
   lcDir = lcDir + "/outgoing/".
   lcResponseDir = lcREsponsedir + "/incoming/".
END.

INPUT STREAM sFile THROUGH VALUE("ls -ltr " + lcDir + "/MM_MIGRATION_LIST_*.txt").
REPEAT:
message lcFilename VIEW-AS ALERT-BOX.
message lcdir VIEW-AS ALERT-BOX.

   IMPORT STREAM sFile UNFORMATTED lcFileName.
   lcInputFile = lcDir + lcFileName.

   lcMSISDN = STRING(ENTRY(1,lcline,";")).
   IF icInput EQ 1 THEN DO:
      lcOutLine = lcMSISDN + ";MNMO NUMPO;No es posible migrar un msisdn que está portado a un operador distinto AS LOG NO-UNDO. operador origen de la migración que lo solicita. Este msisdn está portado AS LOG NO-UNDO. operador con código ~"501~"".
   END.
   ELSE IF icInput EQ 2 THEN DO:
      lcOutLine = lcMSISDN + ";MNMO NPROP;No es posible migrar un msisdn que no pertenece a un rango subasignado del PS".
   END.
   ELSE DO:
      lcOutLine = lcMSISDN + ";La operación se ha realizado con éxito. La solicitud de numeración de migración de numeración móvil ha sido creada con el código ~"00551131170118152200001~".".
   END.
   lcOutFile = lcResponseDir + "MM_MIGRATION_RESPONSE_BY_MOCK" + REPLACE(STRING(fmakets()),".","").
   OUTPUT STREAM sOut TO VALUE(lcOutFile) APPEND.

END.

