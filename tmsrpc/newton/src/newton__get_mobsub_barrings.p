/**
 * List of barrings for a subscription
 *
 * @Input msseq;int;mandatory;the subscription
 * @output ongoing_request;boolean;ongoing barring request
           barrings;struct;[barring_group] structs
           final_mask;array_of_struct;provisioned services
 * @[barring_group] array_of_struct;barrings under group
 * @barring barring_code;string;barring code
            barring_en;string;English name
            barring_es;string;Spanish name
            barring_status;string;Status (ACTIVE/INACTIVE/ACTIVATION_ONGOING/INACTIVATION_ONGOING)
            services;array_of_struct;provisioned barring specific services
 * @service service_en;string;English service name
            service_es;string;Spanish service name
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
katun    = "NewtonAd".
gcBrand  = "1".
{Syst/tmsconst.i}
{Func/barrfunc.i}
{Func/transname.i}
/* Input parameters */
DEF VAR piMsSeq AS INT NO-UNDO.

/* Output parameters */
DEF VAR top_array       AS CHAR NO-UNDO.
DEF VAR top_struct      AS CHAR NO-UNDO.
DEF VAR barr_struct     AS CHAR NO-UNDO.
DEF VAR barr_array     AS CHAR NO-UNDO.
DEF VAR barr_groups     AS CHAR NO-UNDO.
DEF VAR serv_array      AS CHAR NO-UNDO.
DEF VAR serv_struct     AS CHAR NO-UNDO.
DEF VAR mask_struct     AS CHAR NO-UNDO.
DEF VAR mask_array      AS CHAR NO-UNDO.

DEF VAR lcEnName AS CHAR NO-UNDO.
DEF VAR lcSpName AS CHAR NO-UNDO.
DEF VAR lcEnSer AS CHAR NO-UNDO.
DEF VAR lcSpSer AS CHAR NO-UNDO.
DEF VAR liCount AS INT NO-UNDO.
DEF VAR lcServices AS CHAR NO-UNDO.
DEF VAR lcService AS CHAR NO-UNDO.
DEF VAR lcFinalMask AS CHAR NO-UNDO.
DEF VAR lcBarrStatus AS CHAR NO-UNDO.
DEF VAR lcSpGroupName AS CHAR NO-UNDO.

/*Defines order how the data is sent to newton.*/
DEF TEMP-TABLE ttBGroup NO-UNDO
   FIELD BarringGroup AS CHAR
   FIELD Priority AS INT
   INDEX Priority Priority ASC.

/*Customer specific barring data is stored here*/
DEF TEMP-TABLE ttBarringList NO-UNDO
   FIELD BarringGroup AS CHAR
   FIELD BarringCode AS CHAR
   FIELD UIPriority AS INT
   FIELD Mask AS CHAR
   FIELD BarringStatus AS CHAR
   INDEX UIPriority UIPriority ASC.

DEF TEMP-TABLE ttServices
   FIELD digit AS INT
   FIELD service_es AS INT
   FIELD service_en AS INT
INDEX digit IS PRIMARY UNIQUE digit.

/*returns list of barrings that are restricted by given mask*/
/*the mask is also masked with provisioned mask and only provisionde are shown
Example:
icMask                  00011
Mask in configuration   00110
Provisioned             00010
                        00010
*/
FUNCTION fGetRelatedServices RETURNS CHAR
   (icMask AS CHAR,
    icFinalMask aS CHAR).

   DEF VAR lcServices AS CHAR NO-UNDO.
   DEF VAR liDigit AS INT NO-UNDO.

   DO liDigit = 1 TO LENGTH(icMask):
      IF SUBSTRING(icMask,liDigit,1) NE "1" THEN NEXT.
      IF SUBSTRING(icFinalMask,liDigit,1) NE "1" THEN NEXT.
      lcServices = lcServices + STRING(liDigit) + ",".
   END.
   RETURN RIGHT-TRIM(lcServices, ",").

END.


/*Store first information that will be sent to Newton.*/
/*Index will help to send the data in correct order.*/
FUNCTION fFillBarringList RETURNS CHAR
   (iiMsSeq AS INT):

   EMPTY TEMP-TABLE ttBarringList.

   FOR EACH BarringConf NO-LOCK :
      CREATE ttBarringList.
      ASSIGN
      ttBarringList.BarringGroup = BarringConf.BarringGroup
      ttBarringList.BarringCode = BarringConf.BarringCode
      ttBarringList.UIPriority = BarringConf.UIPriority
      ttBarringList.Mask = BarringConf.Mask
      ttBarringList.BarringStatus = fGetBarringStatus(BarringConf.BarringCode,
                                                      iiMsSeq).

   END.

END.


FUNCTION fGetGroupOrder RETURNS LOGICAL.

   DEF VAR liPriority AS INT NO-UNDO.

   EMPTY TEMP-TABLE ttBGroup.
   FOR EACH Tmscodes WHERE
            Tmscodes.TableName EQ "Barring" AND
            Tmscodes.FieldName EQ "BarringGroup":

      liPriority = INT(Tmscodes.ConfigValue) NO-ERROR.
      IF liPriority = ? THEN liPriority = 0.

      CREATE ttBGroup.
      ASSIGN
         ttBGroup.BarringGroup = Tmscodes.CodeValue
         ttBGroup.Priority = liPriority.
   END.

   RETURN TRUE.
END.


FUNCTION fGetServiceName RETURN CHARACTER
  (INPUT pcDigit AS CHARACTER, INPUT piLang AS INTEGER):

   DEF BUFFER TMSCodes FOR TMSCodes.
   DEF VAR lcTransKey AS CHAR NO-UNDO.

   FIND FIRST Tmscodes WHERE
              Tmscodes.TableName EQ "BarringConf" AND
              Tmscodes.FieldName EQ "ServiceMask" AND
              Tmscodes.CodeValue EQ pcDigit NO-ERROR.

   IF NOT AVAIL Tmscodes THEN RETURN pcDigit.

   RETURN fGetItemName(gcBrand,
                "TMSCodes",
                tmscodes.tablename + "|" +
                tmscodes.fieldname + "|" +
                tmscodes.codevalue,
                piLang,
                TODAY).
END.

FUNCTION fGetBarringName RETURN CHARACTER
  (INPUT pcCode AS CHARACTER,
   INPUT piLang AS INTEGER):

   RETURN fGetItemName(gcBrand,
               "BarringCode",
                pcCode,
                piLang,
                TODAY).
END.

FUNCTION fGetBarringGroupName RETURN CHARACTER
  (INPUT pcBarringGroup AS CHARACTER,
   INPUT piLang AS INTEGER):

   DEF BUFFER TMSCodes FOR TMSCodes.

   FIND FIRST Tmscodes WHERE
              Tmscodes.TableName EQ "Barring" AND
              Tmscodes.FieldName EQ "BarringGroup" AND
              Tmscodes.CodeValue EQ pcBarringGroup NO-ERROR.

   IF NOT AVAIL Tmscodes THEN RETURN pcBarringGroup.

   RETURN fGetItemName(gcBrand,
                "TMSCodes",
                Tmscodes.TableName + "|" +
                Tmscodes.FieldName + "|" +
                Tmscodes.CodeValue,
                piLang,
                TODAY).
END.

/*Main functionality begins*/

IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.
piMsSeq = get_pos_int(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/findtenant.i NO Ordercanal MobSub MsSeq piMsSeq}

top_struct = add_struct(response_toplevel_id, "").
barr_groups = add_struct(top_struct, "barrings").

lcFinalMask = fGetFinalMask(MobSub.MsSeq).
/*Fetch data for sending it to Newton*/
fGetGroupOrder().
fFillBarringList(piMsSeq).

FIND FIRST MsRequest NO-LOCK WHERE
           MsRequest.MsSeq      = piMsSeq AND
           MsRequest.ReqType    = 35       AND
           LOOKUP(STRING(MsRequest.ReqStat),"2,4,9") = 0 NO-ERROR.

IF AVAIL MsRequest THEN add_boolean(top_struct,"ongoing_request",TRUE).
ELSE add_boolean(top_struct,"ongoing_request",FALSE).

/*Provide barrings grouped by priority of group and priority of barrings*/
FOR EACH ttBGroup USE-INDEX Priority:

   lcSpGroupName = fGetBarringGroupName(ttBGroup.BarringGroup,1).

   barr_array = add_array(barr_groups, lcSpGroupName).

   FOR EACH ttBarringList WHERE
            ttBarringList.BarringGroup = ttBGroup.BarringGroup
      USE-INDEX UIPriority:

      lcEnName = fGetBarringName(ttBarringList.BarringCode, 5).
      lcSpName = fGetBarringName(ttBarringList.BarringCode, 1).

      barr_struct = add_struct(barr_array,"").
      add_string(barr_struct, "barring_code",ttBarringList.BarringCode).
      add_string(barr_struct, "barring_en", lcEnName).
      add_string(barr_struct, "barring_es", lcSpName).

      lcBarrStatus = ttBarringList.BarringStatus.
      IF AVAIL MsRequest THEN DO:
         IF LOOKUP(ttBarringList.BarringCode + "=1", MsRequest.ReqCparam1) > 0
            THEN lcBarrStatus = "ACTIVATION_ONGOING".
         ELSE IF LOOKUP(ttBarringList.BarringCode + "=0",
                        MsRequest.ReqCparam1) > 0
            THEN lcBarrStatus = "INACTIVATION_ONGOING".
      END.

      add_string(barr_struct, "barring_status", lcBarrStatus).

      lcServices = fGetRelatedServices(ttBarringList.Mask, lcFinalMask).
      serv_array = add_array(barr_struct, "services").

      DO liCount = 1 TO NUM-ENTRIES(lcServices):
         lcService = ENTRY(liCount, lcServices).
         lcEnSer =  fGetServiceName(ENTRY(liCount, lcServices), 5).
         lcSpSer =  fGetServiceName(ENTRY(liCount, lcServices), 1).
         serv_struct = add_struct(serv_array, "").
         add_string(serv_struct, "service_en", lcEnSer).
         add_string(serv_struct, "service_es", lcSpSer).
      END.
   END.
END.

mask_array = add_array(top_struct,"final_mask").

lcServices = fGetRelatedServices(lcFinalMask, lcFinalMask).
DO liCount = 1 TO NUM-ENTRIES(lcServices):
   lcService = ENTRY(liCount, lcServices).
   lcEnSer =  fGetServiceName(lcService, 5).
   lcSpSer =  fGetServiceName(lcService, 1).

   mask_struct = add_struct(mask_array,"").
   add_string(mask_struct, "service_en", lcEnSer).
    add_string(mask_struct, "service_es", lcSpSer ).
END.

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
   EMPTY TEMP-TABLE ttBarringList.
   EMPTY TEMP-TABLE ttBGroup.
END.
