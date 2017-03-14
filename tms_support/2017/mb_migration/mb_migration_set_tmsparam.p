DEF VAR lcgParamGroup AS CHAR NO-UNDO.
DEF VAR lcgHandledBrand AS CHAR NO-UNDO.

lcgHandledBrand = "2". /*Brand that will be set in this execution time*/
lcgParamGroup = "MB_Migration". /*Global variable that defines parameter group*/

FUNCTION fSetTMSParamC RETURNS LOGICAL
   (icBrand AS CHAR,
    icParamCode AS CHAR, /*This is used when seeking the data*/
    icParamName AS CHAR, /*Parameter description / name*/
    icParamValue AS CHAR): /*Value that is set to parameter.*/
   FIND FIRST TMSParam NO-LOCK WHERE
              TMSParam.Brand EQ icBrand AND
              TMSParam.ParamGroup EQ lcgParamGroup AND
              TMSParam.ParamCode EQ icParamCode NO-ERROR.
   IF AVAIL TMSParam THEN DO:
      MESSAGE TMSParam.ParamCode + " already found. Updating value. " 
         VIEW-AS ALERT-BOX.
      TMSParam.CharVal = icParamValue.
   END.
   ELSE DO:
      MESSAGE TMSParam.ParamCode + " will be created. "
         VIEW-AS ALERT-BOX.

      CREATE TMSParam.

      ASSIGN TMSParam.Brand      = icBrand
             TMSParam.ParamGroup = lcgParamGroup
             TMSParam.ParamCode  = icParamCode
             TMSParam.ParamName  = icParamName
             TMSParam.CharVal    = icParamValue
             TMSParam.ParamType  = "C".

   END.
END.
FUNCTION fSetTMSParamI RETURNS LOGICAL
   (icBrand AS CHAR,
    icParamCode AS CHAR, /*This is used when seeking the data*/
    icParamName AS CHAR, /*Parameter description / name*/
    iiParamValue AS INT): /*Value that is set to parameter.*/
   FIND FIRST TMSParam NO-LOCK WHERE
              TMSParam.Brand EQ icBrand AND
              TMSParam.ParamGroup EQ lcgParamGroup AND
              TMSParam.ParamCode EQ icParamCode NO-ERROR.
   IF AVAIL TMSParam THEN DO:
      MESSAGE TMSParam.ParamCode + " already found. Updating value. " 
         VIEW-AS ALERT-BOX.
      TMSParam.IntVal = iiParamValue.
   END.
   ELSE DO:
      MESSAGE TMSParam.ParamCode + " will be created. "
         VIEW-AS ALERT-BOX.

      CREATE TMSParam.

      ASSIGN TMSParam.Brand      = icBrand
             TMSParam.ParamGroup = lcgParamGroup
             TMSParam.ParamCode  = icParamCode
             TMSParam.ParamName  = icParamName
             TMSParam.IntVal    = iiParamValue
             TMSParam.ParamType  = "I".

   END.
END.
fSetTMSParamI(lcgHandledBrand, /*Brand*/
              "MigrationOn", /*Code*/ 
              "Multibrand Migration on/off", /*Name*/
              1). /* Value 0 == OFF*/

fSetTMSParamC(lcgHandledBrand, /*Brand*/
              "MigrationLogDir", /*Code*/ 
              "Multibrand Migration log dir", /*Name*/
              "/mnt/store/riftp/migration/log/"). /* Value 0 == OFF*/

fSetTMSParamC(lcgHandledBrand, /*Brand*/
              "MigrationInDir", /*Code*/ 
              "Multibrand Migration incoming dir", /*Name*/
              "/mnt/store/riftp/migration/incoming/"). /* Value 0 == OFF*/

fSetTMSParamC(lcgHandledBrand, /*Brand*/
              "MigrationSpoolDir", /*Code*/ 
              "Multibrand Migration spool dir", /*Name*/
              "/mnt/store/riftp/migration/spool/"). /* Value 0 == OFF*/

fSetTMSParamC(lcgHandledBrand, /*Brand*/
              "MigrationOutDir", /*Code*/ 
              "Multibrand Migration outgoing dir", /*Name*/
              "/mnt/store/riftp/migration/outgoing/"). /* Value 0 == OFF*/








