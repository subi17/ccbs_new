&IF "{&SMPP_DEFS_I}" NE "YES" &THEN
&GLOBAL-DEFINE SMPP_DEFS_I YES

&GLOBAL-DEFINE LIBSMPPCLIENT "/apps/xfera/tms/lib/smpp/libsmpp4gl/libsmpp4gl.so.1.0.1"
&GLOBAL-DEFINE SMPP_PDU_MAX_SIZE 2048
&GLOBAL-DEFINE SMPP_SYSTEM_ID "002_TMSCRM_"
&GLOBAL-DEFINE SMPP_PASSWORD "TMSCRM"
&GLOBAL-DEFINE SMPP_SYSTEM_TYPE "Fsub_Fdel"
&GLOBAL-DEFINE SMPP_DEFAULT_SOURCE_ADDRESS "800622800"

&ENDIF /* SMPP_DEFS_I */
