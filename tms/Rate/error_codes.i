&IF "{&RATE_ERROR_CODES_I}" NE "YES"
&THEN
&GLOBAL-DEFINE RATE_ERROR_CODES_I YES

&GLOBAL-DEFINE CDR_ERROR_NO_ERROR                          0000 /* No error */
&GLOBAL-DEFINE CDR_ERROR_UNKNOWN_MSISDN                    1001 /* Unknown MSISDN for the Billing System */
&GLOBAL-DEFINE CDR_ERROR_MSISDN_NOT_ACTIVE                 1002 /* MSISDN not active in the BS when connection was opened */
&GLOBAL-DEFINE CDR_ERROR_UNKNOWN_BILLING_TARGET            1004 /* Unknown Billing target */
&GLOBAL-DEFINE CDR_ERROR_CUSTOMER_NOT_FOUND                1005 /* Customer not found */
&GLOBAL-DEFINE CDR_ERROR_UNKNOWN_MOBILE_SUBSCRIPTION_TYPE  1006 /* Unknown mobile subscription type */
&GLOBAL-DEFINE CDR_ERROR_MYSTERIOUS                        1008 /* */
&GLOBAL-DEFINE CDR_ERROR_WRONG_PAY_TYPE                    1020 /* Wrong Paytype */
&GLOBAL-DEFINE CDR_ERROR_UNKNOWN_B_DESTINATION             2001 /* Unknown B-destination */
&GLOBAL-DEFINE CDR_ERROR_UNKNOWN_VOIP_B_DESTINATION        2002 
&GLOBAL-DEFINE CDR_ERROR_NO_BILLING_TARGET_FOUND           3001 /* No rate found for a destination (BillingTarget) */
&GLOBAL-DEFINE CDR_ERROR_NO_RATE_PLAN_FOUND                3002 /* No rate found for a destination (RatePlan) */
&GLOBAL-DEFINE CDR_ERROR_NO_RATE_PREF_FOUND                3003 /* No rate found for a destination (RatePref) */
&GLOBAL-DEFINE CDR_ERROR_NO_TARIFF_FOUND                   3004 /* No rate found for a destination (Tariff) */
&GLOBAL-DEFINE CDR_TRANSLATED_ADDRESS_NOT_FOUND           4007  /* Translated address not found */
&GLOBAL-DEFINE CDR_ERROR_UNKNOWN_CALL_MODULE_TYPE          7001 /* Unknown Call Module Type */
&GLOBAL-DEFINE CDR_ERROR_INVALID_DATE_TIME                 7002 /* Invalid Date or Time field(s) */
&GLOBAL-DEFINE CDR_ERROR_INVALID_GSM_A_SUB                 7003 /* Invalid GsmAsub */
&GLOBAL-DEFINE CDR_ERROR_INVALID_GSM_B_SUM                 7004 /* Invalid GsmBsub */
&GLOBAL-DEFINE CDR_ERROR_NON_BILLABLE_CALL_FORWARDING      7006 /* NON-Billable Call Forwarding */
&GLOBAL-DEFINE CDR_ERROR_BILLING_DURATION_TOO_SHORT        7009 /* Billing duration less than minimum billing length */
&GLOBAL-DEFINE CDR_ERROR_DOUBLE_CALL                       8001 /* Double Call */
&GLOBAL-DEFINE CDR_ERROR_NON_BILLABLE_CALL                 8002 /* Non-billable call */
&GLOBAL-DEFINE CDR_ERROR_DOUBLE_CHECK_FAILED               8003
&GLOBAL-DEFINE CDR_ERROR_DOUBLE_CCGW_CDR                   8004 /* Double CCGW CDR */
&GLOBAL-DEFINE CDR_ERROR_DOUBLE_DATA_CDR                   8005 /* Double data CDR */
&GLOBAL-DEFINE CDR_ERROR_NON_BILLABLE_AMLI                 8010 /* Non-billable AMLI */
&GLOBAL-DEFINE CDR_ERROR_NON_BILLABLE_VAS_MT_SMS           8011 /* NON-BILLABLE VAS MT-SMS */
&GLOBAL-DEFINE CDR_ERROR_NON_BILLABLE_VAS_MO_SMS           8012 /* NON-BILLABLE VAS MO-SMS */
&GLOBAL-DEFINE CDR_ERROR_NON_BILLABLE_CONTM2_CALL          8014 /* Non-billable CONTM2 call */
&GLOBAL-DEFINE CDR_ERROR_OUT_OF_INVOICE_RUN                8040 /* Out of the Invoicing Run (temporarily) */
&GLOBAL-DEFINE CDR_ERROR_ROAMING_OUT_DOUBLE_CALL           8041 /* Roaming Out double calls */
&GLOBAL-DEFINE CDR_ERROR_ROAMING_GPRS_ZERO_DATA            8042 /* Roaming GPRS (international) with zero data amount */
&GLOBAL-DEFINE CDR_ERROR_NATIONAL_GPRS_ZERO_DATA           8043 /* National GPRS with zero data amount */
&GLOBAL-DEFINE CDR_ERROR_INCORRECT_BRAND                   8048 /* Wrong customer (tenant)*/
&GLOBAL-DEFINE CDR_ERROR_NON_INVOICEABLE_CALL              8049 /* Not Invoiceable Calls */
&GLOBAL-DEFINE CDR_ERROR_NON_BILLABLE_UNKNOWN_TICKET       9000 /* NON-BILLABLE, UNKNOWN TICKET */
&GLOBAL-DEFINE CDR_ERROR_SSP_CALL                          9001 /* SSP Call; not billable */
&GLOBAL-DEFINE CDR_ERROR_NON_BILLABLE_CFO_CALL             9002 /* */
&GLOBAL-DEFINE CDR_ERROR_GSM_B_NUMBER_MISSING_ON_MST       9003 /* GsmBnr missing on NON-billable MST Call */
&GLOBAL-DEFINE CDR_ERROR_DURATION_TOO_SHORT                9004 /* Duration less than minimum billing length */
&GLOBAL-DEFINE CDR_ERROR_FREE_CALL                         9005 /* 0800 calls / Free Calls */
&GLOBAL-DEFINE CDR_ERROR_EMERGENCY_CALL                    9006 /* Emergency calls: not billable */
&GLOBAL-DEFINE CDR_ERROR_ROAMING_CALL                      9007 /* Roaming calls */
&GLOBAL-DEFINE CDR_ERROR_UNKNOWN_GSM_B_NUMBER              9008 /* Gsmbnr unknown */
&GLOBAL-DEFINE CDR_ERROR_FREE_VOICE_MAIL_CALL_FORWARDING   9009 /* Free call forwarding to voice mail: not billable */
&GLOBAL-DEFINE CDR_ERROR_ROAMING_CALL_FORWARDING_VMS_MT    9010 /* Roaming call forwarding to VMS (MT) */
&GLOBAL-DEFINE CDR_ERROR_ROAMING_CALL_FORWARDING_VMS_MO    9011 /* Roaming call forwarding to VMS (MO) */
&GLOBAL-DEFINE CDR_ERROR_FOR_ANALYSIS                      9100 /* For analysis */
&GLOBAL-DEFINE CDR_ERROR_ANALYSED_NON_BILLABLE_TERMINATING 9997 /* CDR analysed: not billable / Terminating call */
&GLOBAL-DEFINE CDR_ERROR_NOT_RATED_YET                     9998 /* CDR analysed but not RATED yet */
&GLOBAL-DEFINE CDR_ERROR_NOT_ANALYSED_YET                  9999 /* CDR not analysed yet */

/* unused in rating code: */
&GLOBAL-DEFINE CDR_ERROR_NO_SUBSCRIPTION_FOUND             1003 /* No mobile subscription found */
&GLOBAL-DEFINE CDR_ERROR_UNKNOWN_RATE_PLAN                 1007 /* Unknown rateplan */
&GLOBAL-DEFINE CDR_ERROR_IMSI_MISSING                      1009 /* IMSI Missing */
&GLOBAL-DEFINE CDR_ERROR_SMS_ERROR                         1100 /* SMS Error; Network Determined Errors */
&GLOBAL-DEFINE CDR_ERROR_ERROR_IN_VAS_TICKET               4001 /* Errorneous VAS ticket */
&GLOBAL-DEFINE CDR_ERROR_UNKNOWN_VAS_B_DESTINATION         4002 /* Unknown VAS B-destination */
&GLOBAL-DEFINE CDR_ERROR_UNKNOWN_VAS_NAME                  4003 /* Unknown Value Added Service name */
&GLOBAL-DEFINE CDR_ERROR_UNKNOWN_TARIFF_CLASS              4004 /* Unknown tariff class */
&GLOBAL-DEFINE CDR_ERROR_NON_BILLABLE_VAS                  4005 /* Non-billable VAS ticket */
&GLOBAL-DEFINE CDR_ERROR_NON_BILLABLE_WAP_VAS              4006 /* Non-billable WAP VAS ticket (unknown operator) */
&GLOBAL-DEFINE CDR_ERROR_NON_BILLABLE_WAP                  4101 /* Non-billable WAP ticket (additional services) */
&GLOBAL-DEFINE CDR_ERROR_NON_BILLABLE_GPRS                 4102 /* Non-billable GPRS (mms) ticket */
&GLOBAL-DEFINE CDR_ERROR_UNKNOWN_CALL_TYPE                 7005 /* Unknown Call type */

&GLOBAL-DEFINE CDR_ERROR_UNKNOWN_MSISDN_FIXED              6001 /* Unknown fixed line CDR for the Billing System */
&GLOBAL-DEFINE CDR_ERROR_MSISDN_NOT_ACTIVE_FIXED           6002 /* Fixed line number not active in the BS when connection was opened */

&GLOBAL-DEFINE CDR_ERROR_NON_BILLABLE_ROAM_MT_CALL         9062 /* */
&GLOBAL-DEFINE CDR_ERROR_NON_BILLABLE_ROAM_MT_MMS          9066 /* */
&ENDIF
