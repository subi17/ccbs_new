/* ----------------------------------------------------------------------
  MODULE .......: smpp34_wrapper.c
  TASK .........: Wrapper functions for smpp34 library.
     
    Compilation requires following libraries:

    Libxml2 : http://www.xmlsoft.org/
    C Open SMPP library: http://c-open-smpp-34.sourceforge.net/

  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 27.03.13
  Version ......: Yoigo
----------------------------------------------------------------------- */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <sys/types.h>
#include <netinet/in.h>

#ifdef __linux__
#include <stdint.h>
#endif

#include <smpp34.h>
#include <smpp34_structs.h>
#include <smpp34_params.h>

int is_numeric(const char *p);

int smpp_bind_transceiver (
   char *i_system_id,
   char *i_password,
   char *i_system_type,
   int i_sequence_number,
   
   int i_resp_text_max_size,
   void *o_resp_text,
   
   int i_pdu_max_size,
   void *o_pdu_binary,
   int  *o_pdu_size);

int smpp_bind_transceiver_resp (
   int i_pdu_size,
   void *i_pdu_binary,
   int i_resp_text_max_size,
   void *o_resp_text,
   int *o_command_status,
   int *o_sequence_number);

int smpp_submit_sm (
   int i_sequence_number,
   char *i_destination_addr,
   char *i_source_addr,
   char *i_short_message,
   
   int i_resp_text_max_size,
   void *o_resp_text,
   
   int i_pdu_max_size,
   void *o_pdu_binary,
   int  *o_pdu_size);

int smpp_submit_sm_resp (
   int i_pdu_size,
   void *i_pdu_binary,
   int i_resp_text_max_size,
   void *o_resp_text,
   int *o_command_status,
   int *o_sequence_number);

int smpp_unbind (
   int i_sequence_number,
   
   int i_resp_text_max_size,
   void *o_resp_text,
   
   int i_pdu_max_size,
   void *o_pdu_binary,
   int  *o_pdu_size);

int smpp_unbind_resp (
   int i_pdu_size,
   void *i_pdu_binary,
   int i_resp_text_max_size,
   void *o_resp_text,
   int *o_command_status,
   int *o_sequence_number);

int is_numeric(const char *p) {
   if (*p) {
      char c;
      while ((c=*p++)) {
         if (!isdigit(c)) return 0;
      }
      return 1;
   }
   return 0;
}

int smpp_bind_transceiver (
   char *i_system_id,
   char *i_password,
   char *i_system_type,
   int i_sequence_number,
   
   int i_resp_text_max_size,
   void *o_resp_text,
   
   int i_pdu_max_size,
   void *o_pdu_binary,
   int  *o_pdu_size) {

   int ret;
   bind_transceiver_t pdu_src;
    
   memset(&pdu_src, 0, sizeof(bind_transceiver_t));

   pdu_src.command_length  = 0;
   pdu_src.command_id      = BIND_TRANSCEIVER;
   pdu_src.command_status  = ESME_ROK;
   pdu_src.sequence_number = i_sequence_number;
   strncpy(pdu_src.system_id, i_system_id, sizeof(pdu_src.system_id));
   strncpy(pdu_src.password, i_password, sizeof(pdu_src.password));
/*   strncpy(pdu_src.system_type, "Fsub_Fdel", sizeof(pdu_src.system_type)); */
   strncpy(pdu_src.system_type, i_system_type, sizeof(pdu_src.system_type));
   pdu_src.interface_version = 0x34;

   ret = smpp34_pack(BIND_TRANSCEIVER,
                     (uint8_t*)o_pdu_binary,
                     i_pdu_max_size,
                     o_pdu_size,
                     &pdu_src); 
   if ( ret != 0) {
      snprintf((char*)o_resp_text,
               i_resp_text_max_size,
               "Error in smpp34_pack():%d:\n%s\n", 
               smpp34_errno, smpp34_strerror);
   };
   
  return ret;
};

int smpp_bind_transceiver_resp (
   int i_pdu_size,
   void *i_pdu_binary,
   int i_resp_text_max_size,
   void *o_resp_text,
   int *o_command_status,
   int *o_sequence_number) {

   int ret;
   bind_transceiver_resp_t pdu_dest;

   memset(&pdu_dest, 0, sizeof(bind_transceiver_resp_t));
   
   ret = smpp34_unpack(BIND_TRANSCEIVER_RESP,
                       &pdu_dest,
                       (uint8_t*)i_pdu_binary,
                       i_pdu_size); 

   *o_command_status  = pdu_dest.command_status; 
   *o_sequence_number = pdu_dest.sequence_number;
      snprintf((char*)o_resp_text,
               i_resp_text_max_size,
               "Error in smpp34unpack():%d:\n%s\n", 
               smpp34_errno, smpp34_strerror);

   if( ret != 0 ) {
      snprintf((char*)o_resp_text,
               i_resp_text_max_size,
               "Error in smpp34unpack():%d:\n%s\n", 
               smpp34_errno, smpp34_strerror);
   };

   return ret;
};

int smpp_submit_sm (
   int i_sequence_number,
   char *i_destination_addr,
   char *i_source_addr,
   char *i_short_message,
   
   int i_resp_text_max_size,
   void *o_resp_text,
   
   int i_pdu_max_size,
   void *o_pdu_binary,
   int  *o_pdu_size) {
   
   int ret;
   submit_sm_t pdu_src;

   memset(&pdu_src, 0, sizeof(submit_sm_t));

   /* Init PDU ***********************************************************/
   pdu_src.command_length   = 0;
   pdu_src.command_id       = SUBMIT_SM;
   pdu_src.command_status   = ESME_ROK;
   pdu_src.sequence_number  = i_sequence_number;
/* snprintf((char*)pdu_src.service_type, sizeof(pdu_src.service_type), "%s", "SMS"); */
   memset(pdu_src.service_type,0,sizeof(pdu_src.service_type));

   if ( i_source_addr[0] ==  '\0' ) {
      pdu_src.source_addr_ton  = 0;
      pdu_src.source_addr_npi  = 0;
   } else if ( is_numeric(i_source_addr) ) { 
      pdu_src.source_addr_ton  = 0;
      pdu_src.source_addr_npi  = 0;
   } else {
      pdu_src.source_addr_ton  = 5; /* 5 = Alphanumeric */
      pdu_src.source_addr_npi  = 0;
   }
         
   snprintf((char*)pdu_src.source_addr, sizeof(pdu_src.source_addr), "%s", i_source_addr);
/* memset(pdu_src.source_addr,0,sizeof(pdu_src.source_addr)); */
   pdu_src.dest_addr_ton    = 0;
   pdu_src.dest_addr_npi    = 0;
   snprintf((char*)pdu_src.destination_addr, sizeof(pdu_src.destination_addr), "%s", i_destination_addr);
   pdu_src.esm_class        = 0;
   pdu_src.protocol_id      = 0;
   pdu_src.priority_flag    = 0;
   memset(pdu_src.schedule_delivery_time,0,sizeof(pdu_src.schedule_delivery_time));
   memset(pdu_src.validity_period,0,sizeof(pdu_src.validity_period));
   pdu_src.registered_delivery = 0;
   pdu_src.replace_if_present_flag = 0;
   pdu_src.data_coding       = 0;
   pdu_src.sm_default_msg_id = 0;
   pdu_src.sm_length         = strlen(i_short_message);
   memcpy(pdu_src.short_message, i_short_message, pdu_src.sm_length);
   
   ret = smpp34_pack(SUBMIT_SM,
                     (uint8_t*)o_pdu_binary,
                     i_pdu_max_size,
                     o_pdu_size,
                     &pdu_src); 

   if( ret != 0 ) {
      snprintf((char*)o_resp_text,
               i_resp_text_max_size,
               "Error in smpp34_pack():%d:\n%s\n", 
               smpp34_errno, smpp34_strerror);
   };
   
   return ret;
};

int smpp_submit_sm_resp (
   int i_pdu_size,
   void *i_pdu_binary,
   int i_resp_text_max_size,
   void *o_resp_text,
   int *o_command_status,
   int *o_sequence_number) {
    
   int ret;
   submit_sm_resp_t pdu_dest;

   memset(&pdu_dest, 0, sizeof(submit_sm_resp_t));
   
   ret = smpp34_unpack(SUBMIT_SM_RESP,
                       &pdu_dest,
                       (uint8_t*)i_pdu_binary,
                       i_pdu_size); 

   *o_command_status  = pdu_dest.command_status;
   *o_sequence_number = pdu_dest.sequence_number;

   if ( ret != 0 ) {
      snprintf((char*)o_resp_text,
               i_resp_text_max_size,
               "Error in smpp34_unpack():%d:\n%s\n", 
               smpp34_errno, smpp34_strerror);
   };

   return ret;
};

int smpp_unbind (
   int i_sequence_number,
   
   int i_resp_text_max_size,
   void *o_resp_text,
   
   int i_pdu_max_size,
   void *o_pdu_binary,
   int  *o_pdu_size) {
   
   int ret;
   unbind_t pdu_src;
    
   memset(&pdu_src, 0, sizeof(unbind_t));

   pdu_src.command_length  = 0;
   pdu_src.command_id      = UNBIND;
   pdu_src.command_status  = ESME_ROK;
   pdu_src.sequence_number = i_sequence_number;

   ret = smpp34_pack(pdu_src.command_id,
                     (uint8_t*)o_pdu_binary,
                     i_pdu_max_size,
                     o_pdu_size,
                     &pdu_src); 

   if( ret != 0 ) {
      snprintf((char*)o_resp_text,
               i_resp_text_max_size,
               "Error in smpp34_pack():%d:\n%s\n", 
               smpp34_errno, smpp34_strerror);
   };
   
   return ret;
};

int smpp_unbind_resp (
   int i_pdu_size,
   void *i_pdu_binary,
   int i_resp_text_max_size,
   void *o_resp_text,
   int *o_command_status,
   int *o_sequence_number) {

   int ret;
   unbind_resp_t pdu_dest;

   memset(&pdu_dest, 0, sizeof(unbind_resp_t));
   
   ret = smpp34_unpack(UNBIND_RESP,
                       &pdu_dest,
                       (uint8_t*)i_pdu_binary,
                       i_pdu_size); 
   
   *o_command_status  = pdu_dest.command_status;
   *o_sequence_number = pdu_dest.sequence_number;

   if ( ret != 0 ) {
      snprintf((char*)o_resp_text,
               i_resp_text_max_size,
               "Error in smpp34_unpack():%d:\n%s\n", 
               smpp34_errno, smpp34_strerror);
   };

   return ret;
}
