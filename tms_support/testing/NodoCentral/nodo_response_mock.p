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
DEF STREAM sIn.
DEF STREAM sFile.
DEF VAR lcDir AS CHAR NO-UNDO.
DEF VAR lcInputFile AS CHAR No-UNDO.
DEF VAR lcFileName AS CHAR NO-UNDO.
DEF VAR lcMSISDN AS CHAR NO-UNDO.
DEF VAR lcLine AS CHAR NO-UNDO.
DEF VAR lcOutLine AS CHAR NO-UNDO.

ASSIGN
   lcDir = fCParam("MB_Migration", "MigrationInDir").

IF lcDir EQ "" OR lcDir EQ ? THEN lcDir = "/tmp/".

INPUT STREAM sFile THROUGH VALUE("ls -ltr " + lcDir).
REPEAT:
   IMPORT STREAM sFile UNFORMATTED lcFileName.
   lcInputFile = lcDir + lcFileName.

   lcMSISDN = STRING(ENTRY(1,lcline,";")).
   lcOutLine = lcMSISDN + ";La operación se ha realizado con éxito. La solicitud de numeración de migración de numeración móvil ha sido creada con el código ~"00551131170118152200001~".".
  /* lcOutLine = lcMSISDN + ";MNMO NUMPO;No es posible migrar un msisdn que está portado a un operador distinto AS LOG NO-UNDO. operador origen de la migración que lo solicita. Este msisdn está portado AS LOG NO-UNDO. operador con código "501"".
*/
/*
   lcOutLine = lcMSISDN + ";MNMO NPROP;No es posible migrar un msisdn que no pertenece a un rango subasignado del PS".*/



END.

