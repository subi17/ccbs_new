/*Configure DMS paths*/


CREATE TMSParam.
ASSIGN
   TMSParam.Brand = "1"
   TMSParam.ParamCode = "TMS_IncDir"
   TMSParam.ParamGroup = "DMS"
   TMSParam.ParamType = "C"
   TMSParam.ParamName = "DMS to TMS incoming directory"
   TMSParam.Online = FALSE
   TMSParam.CharVal = "/mnt/store/riftp/dms/case_file_update_dms_to_tms/incoming/".


CREATE TMSParam.
ASSIGN
   TMSParam.Brand = "1"
   TMSParam.ParamCode = "TMS_LogDir"
   TMSParam.ParamGroup = "DMS"
   TMSParam.ParamType = "C"
   TMSParam.ParamName = "DMS to TMS log directory"
   TMSParam.Online = FALSE
   TMSParam.CharVal = "/mnt/store/riftp/dms/case_file_update_dms_to_tms/logs/".


CREATE TMSParam.
ASSIGN
   TMSParam.Brand = "1"
   TMSParam.ParamCode = "TMS_ProcDir"
   TMSParam.ParamGroup = "DMS"
   TMSParam.ParamType = "C"
   TMSParam.ParamName = "DMS to TMS processed directory"
   TMSParam.Online = FALSE
   TMSParam.CharVal = "/mnt/store/riftp/dms/case_file_update_dms_to_tms/processed/".

CREATE TMSParam.
ASSIGN
   TMSParam.Brand = "1"
   TMSParam.ParamCode = "TMS_SpoolDir"
   TMSParam.ParamGroup = "DMS"
   TMSParam.ParamType = "C"
   TMSParam.ParamName = "DMS to TMS spool directory"
   TMSParam.Online = FALSE
   TMSParam.CharVal = "/mnt/store/riftp/dms/case_file_update_dms_to_tms/spool/".

CREATE TMSParam.
ASSIGN
   TMSParam.Brand = "1"
   TMSParam.ParamCode = "TMS_to_DMS_LogDir"
   TMSParam.ParamGroup = "DMS"
   TMSParam.ParamType = "C"
   TMSParam.ParamName = "TMS to DMS log directory"
   TMSParam.Online = FALSE
   TMSParam.CharVal = "/mnt/store/riftp/dms/case_file_creation_removal_tms_to_dms/logs/".


CREATE TMSParam.
ASSIGN
   TMSParam.Brand = "1"
   TMSParam.ParamCode = "TMS_to_DMS_OutDir"
   TMSParam.ParamGroup = "DMS"
   TMSParam.ParamType = "C"
   TMSParam.ParamName = "TMS to DMS outgoing directory"
   TMSParam.Online = FALSE
   TMSParam.CharVal = "/mnt/store/riftp/dms/case_file_creation_removal_tms_to_dms/outgoing/".

CREATE TMSParam.
ASSIGN
   TMSParam.Brand = "1"
   TMSParam.ParamCode = "TMS_to_DMS_ProcDir"
   TMSParam.ParamGroup = "DMS"
   TMSParam.ParamType = "C"
   TMSParam.ParamName = "TMS to DMS processed directory"
   TMSParam.Online = FALSE
   TMSParam.CharVal = "/mnt/store/riftp/dms/case_file_creation_removal_tms_to_dms/processed/".

CREATE TMSParam.
ASSIGN
   TMSParam.Brand = "1"
   TMSParam.ParamCode = "TMS_to_DMS_SpoolDir"
   TMSParam.ParamGroup = "DMS"
   TMSParam.ParamType = "C"
   TMSParam.ParamName = "TMS to DMS Spool directory"
   TMSParam.Online = FALSE
   TMSParam.CharVal = "/mnt/store/riftp/dms/case_file_creation_removal_tms_to_dms/spool/".




