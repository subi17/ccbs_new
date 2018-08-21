FUNCTION fCreateTMSParam RETURNS LOGICAL
   ( icParamGroup AS CHARACTER,
     icParamCode  AS CHARACTER,
     icParamType  AS CHARACTER,
     icParamValue AS CHARACTER,
     icParamName  AS CHARACTER):

   FIND TMSParam EXCLUSIVE-LOCK WHERE
      TMSParam.Brand       = "1" AND
      TMSParam.ParamGroup  = icParamGroup AND
      TMSParam.ParamCode   = icParamCode
   NO-ERROR.
   
   IF NOT AVAILABLE TMSParam
   THEN DO:
      CREATE TMSParam.
   END.
   
   ASSIGN
      TMSParam.Brand       = "1"
      TMSParam.ParamGroup  = icParamGroup
      TMSParam.ParamCode   = icParamCode
      TMSParam.ParamType   = icParamType
      TMSParam.ParamName   = icParamName
      .
  
   CASE IcParamType:
      WHEN "I"
      THEN TMSParam.IntVal = INTEGER(icParamValue).
      WHEN "C"
      THEN TMSParam.CharVal = icParamValue.
   END CASE.

   RETURN FALSE.

END FUNCTION.


fCreateTMSParam('StcMassiveBob', 'IncDir',         'C', '/store/riftp/massive_stc/incoming/',   'STC Massive BOB Incoming DIR'   ).
fCreateTMSParam('StcMassiveBob', 'IncProcessDir',  'C', '/store/riftp/massive_stc/processed/',  'STC Massive BOB Processed DIR'  ).
fCreateTMSParam('StcMassiveBob', 'OutDir',         'C', '/store/riftp/massive_stc/outgoing/',   'STC Massive BOB Outgoing DIR'   ).
fCreateTMSParam('StcMassiveBob', 'OutSpoolDir',    'C', '/store/riftp/massive_stc/spool/',      'STC Massive BOB Spooling DIR'   ).
fCreateTMSParam('StcMassiveBob', 'RootDir',        'C', '/store/riftp/massive_stc/',            'STC Massive BOB Root DIR'       ).
