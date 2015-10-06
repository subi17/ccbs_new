/* TMS Global Constants */
/* Hint: Use ({&SOME_VARIABLE}) with messages so that compiler notices if constant does not exists  */
&IF "{&TMSCONST_I}" NE "YES"
&THEN
&GLOBAL-DEFINE TMSCONST_I YES

&GLOBAL-DEFINE REQ_INACTIVE_STATUSES "2,4,9,99"
&GLOBAL-DEFINE REQ_ONGOING_STATUSES "0,1,3,5,6,7,8,15,16,17,19"
&GLOBAL-DEFINE ORDER_INACTIVE_STATUSES "6,7,8,9"
&GLOBAL-DEFINE ORDER_CLOSE_STATUSES "7,8,9"
&GLOBAL-DEFINE ORDER_ROI_STATUSES "41,42,43,44"
&GLOBAL-DEFINE SKIP_MsRequest "999"


/*Payment Methods for OrderPayment.Method*/
&GLOBAL-DEFINE ORDERPAYMENT_M_POD 1
&GLOBAL-DEFINE ORDERPAYMENT_M_CREDIT_CARD 2
&GLOBAL-DEFINE ORDERPAYMENT_M_PAYPAL 6

/* Order.StatusCode */
&GLOBAL-DEFINE ORDER_STATUS_NEW "1"
&GLOBAL-DEFINE ORDER_STATUS_MNP "3"
&GLOBAL-DEFINE ORDER_STATUS_IN_CONTROL "4"
&GLOBAL-DEFINE ORDER_STATUS_DELIVERED "6"
&GLOBAL-DEFINE ORDER_STATUS_CLOSED "7"
&GLOBAL-DEFINE ORDER_STATUS_CLOSED_BY_FRAUD "8"
&GLOBAL-DEFINE ORDER_STATUS_AUTO_CLOSED "9"
&GLOBAL-DEFINE ORDER_STATUS_ONGOING "12"
&GLOBAL-DEFINE ORDER_STATUS_COMPANY_NEW "20"
&GLOBAL-DEFINE ORDER_STATUS_COMPANY_MNP "21"
&GLOBAL-DEFINE ORDER_STATUS_MNP_ON_HOLD "22"
&GLOBAL-DEFINE ORDER_STATUS_RENEWAL "30"
&GLOBAL-DEFINE ORDER_STATUS_RENEWAL_HOLD "31"
&GLOBAL-DEFINE ORDER_STATUS_RENEWAL_STC "32"
&GLOBAL-DEFINE ORDER_STATUS_RENEWAL_STC_COMPANY "33"
&GLOBAL-DEFINE ORDER_STATUS_ROI_LEVEL_1 "41" 
&GLOBAL-DEFINE ORDER_STATUS_ROI_LEVEL_2 "42" 
&GLOBAL-DEFINE ORDER_STATUS_ROI_LEVEL_3 "43" 
&GLOBAL-DEFINE ORDER_STATUS_OFFER_SENT "50" 
&GLOBAL-DEFINE ORDER_STATUS_RESIGNATION "51" 
&GLOBAL-DEFINE ORDER_STATUS_MORE_DOC_NEEDED "44" 
&GLOBAL-DEFINE ORDER_STATUS_MNP_REJECTED "73" 
&GLOBAL-DEFINE ORDER_STATUS_MNP_PENDING "74" 
&GLOBAL-DEFINE ORDER_STATUS_MNP_RETENTION "75"
&GLOBAL-DEFINE ORDER_STATUS_PENDING_MAIN_LINE "76"
&GLOBAL-DEFINE ORDER_STATUS_PENDING_FIXED_LINE "77"
&GLOBAL-DEFINE ORDER_STATUS_SIM_ONLY_MNP_IN "99"

/* OrderFusion.FusionStatusCode  */
&GLOBAL-DEFINE FUSION_ORDER_STATUS_NEW "NEW"
&GLOBAL-DEFINE FUSION_ORDER_STATUS_ONGOING "ONG"
&GLOBAL-DEFINE FUSION_ORDER_STATUS_CANCELLED "CAN"
&GLOBAL-DEFINE FUSION_ORDER_STATUS_FINALIZED "FIN"
&GLOBAL-DEFINE FUSION_ORDER_STATUS_PENDING_FINALIZED "PFIN"
&GLOBAL-DEFINE FUSION_ORDER_STATUS_PENDING_CANCELLED "PCAN"

/* OrderCustomer.RowType  */
&GLOBAL-DEFINE ORDERCUSTOMER_ROWTYPE_AGREEMENT 1
&GLOBAL-DEFINE ORDERCUSTOMER_ROWTYPE_DELIVERY 4
&GLOBAL-DEFINE ORDERCUSTOMER_ROWTYPE_CIF_CONTACT 5
&GLOBAL-DEFINE ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL 6
&GLOBAL-DEFINE ORDERCUSTOMER_ROWTYPE_FIXED_BILLING 7
&GLOBAL-DEFINE ORDERCUSTOMER_ROWTYPE_LOGISTICS 8

/* Dextra */
&GLOBAL-DEFINE DEXTRA_CANCELLED_STATUSES "3,66,875,22"

/* OrderType */
&GLOBAL-DEFINE ORDER_TYPE_NEW 0
&GLOBAL-DEFINE ORDER_TYPE_MNP 1
&GLOBAL-DEFINE ORDER_TYPE_RENEWAL 2
&GLOBAL-DEFINE ORDER_TYPE_ROLLBACK 3
&GLOBAL-DEFINE ORDER_TYPE_STC 4

/* Order.DeliveryType */
&GLOBAL-DEFINE ORDER_DELTYPE_COURIER 1
&GLOBAL-DEFINE ORDER_DELTYPE_KIALA 2
&GLOBAL-DEFINE ORDER_DELTYPE_SECURE 3
&GLOBAL-DEFINE ORDER_DELTYPE_POST 4

/* OrderTimeStamp */
&GLOBAL-DEFINE ORDERTIMESTAMP_CHANGE 1
&GLOBAL-DEFINE ORDERTIMESTAMP_DELIVERY 2
&GLOBAL-DEFINE ORDERTIMESTAMP_CLOSE 3
&GLOBAL-DEFINE ORDERTIMESTAMP_PRINT 4
&GLOBAL-DEFINE ORDERTIMESTAMP_SIMONLY 5
&GLOBAL-DEFINE ORDERTIMESTAMP_SEND 6

/* TMRule */
&GLOBAL-DEFINE TMRULE_PERIOD_BALANCE 3

/* Common TMS CUI Messages */
&GLOBAL-DEFINE MSG_ONG_REQUEST "Function not allowed due to existing requests"
&GLOBAL-DEFINE MSG_NOT_ALLOWED "Function not allowed due to business rules"
&GLOBAL-DEFINE MSG_NOT_RIGHTS  "Function not allowed due to user rights"
&GLOBAL-DEFINE MSG_INCORRECT_VALUE "Incorrect or missing value"
&GLOBAL-DEFINE MSG_TRY_AGAIN "Try again"
&GLOBAL-DEFINE MSG_DATA_NOT_FOUND "Data not found"
&GLOBAL-DEFINE MSG_RECORD_DELETED "Record has been deleted by another user!"
&GLOBAL-DEFINE MSG_RECORD_CHANGED "Record has changed while you were working"
&GLOBAL-DEFINE MSG_RECORD_LOCKED "Record has been locked by another user"

&GLOBAL-DEFINE MSG_LAST_PAGE "YOU ARE ON THE LAST PAGE !"

/* MSISDN STOCKS */
&GLOBAL-DEFINE MSISDN_STOCK_ONLINE "ONLINE"
&GLOBAL-DEFINE MSISDN_STOCK_OFFLINE "OFFLINE"
&GLOBAL-DEFINE MSISDN_STOCK_VIP "VIP"
&GLOBAL-DEFINE MSISDN_STOCK_NOTPUBLIC "NOTPUBLIC"
&GLOBAL-DEFINE MSISDN_STOCK_PREACTIVATED "PREACTIVATED"

/* msisdn statuses */
&GLOBAL-DEFINE MSISDN_ST_UNKNOWN 0
&GLOBAL-DEFINE MSISDN_ST_AVAILABLE 1
&GLOBAL-DEFINE MSISDN_ST_QUARANTINE 4
&GLOBAL-DEFINE MSISDN_ST_MNP_OUT_YOIGO 6 
&GLOBAL-DEFINE MSISDN_ST_WAITING_RETURN 11
&GLOBAL-DEFINE MSISDN_ST_RETURNED 12
&GLOBAL-DEFINE MSISDN_ST_MNP_OUT_OTHER 13
&GLOBAL-DEFINE MSISDN_ST_RETURN_NOTICE_SENT 14
&GLOBAL-DEFINE MSISDN_ST_RETURNED_TO_YOIGO 15

/* PATHS & FILES */
&GLOBAL-DEFINE PATH_MSISDN_RELEASE "/scratch/log/msisdn_release/"

/* ICC Stock types */
&GLOBAL-DEFINE ICC_STOCK_TYPES "NEW,MNP,CC"

/* SIM.SimStat */
&GLOBAL-DEFINE SIM_SIMSTAT_AVAILABLE 1
&GLOBAL-DEFINE SIM_SIMSTAT_TEMP 5
&GLOBAL-DEFINE SIM_SIMSTAT_LOST 7
&GLOBAL-DEFINE SIM_SIMSTAT_SENT_TO_LOGISTICS 21

/* Limit Types */
&GLOBAL-DEFINE LIMIT_TYPE_TMRLIMIT 1
&GLOBAL-DEFINE LIMIT_TYPE_SUBQTY 2
&GLOBAL-DEFINE LIMIT_TYPE_BILLPERM 3
&GLOBAL-DEFINE LIMIT_TYPE_SUBACTQTY 4

&GLOBAL-DEFINE LIMIT_BILLPERM_PROHIBITED 2
&GLOBAL-DEFINE LIMIT_BILLPERM_ALLOWED 0
&GLOBAL-DEFINE LIMIT_BILLPERM_SUSPENDED 1

&GLOBAL-DEFINE SLANALYSE_FULL_PACKET  1
&GLOBAL-DEFINE SLANALYSE_BROKEN_PACKET 2
&GLOBAL-DEFINE SLANALYSE_NO_PACKET 0

/* CustContactTypes */
&GLOBAL-DEFINE CUSTCONTACT_REPRESENTATIVE 1
&GLOBAL-DEFINE CUSTCONTACT_CONTACT 5

/* DayCampaign.DCType */
&GLOBAL-DEFINE DCTYPE_SERVICE_PACKAGE "1"
&GLOBAL-DEFINE DCTYPE_RATING_LIMIT "2"
&GLOBAL-DEFINE DCTYPE_DISCOUNT "3"
&GLOBAL-DEFINE DCTYPE_BUNDLE "4"
&GLOBAL-DEFINE DCTYPE_INSTALLMENT "5"
&GLOBAL-DEFINE DCTYPE_POOL_RATING "6"
&GLOBAL-DEFINE DCTYPE_CUMULATIVE_RATING "8"

&GLOBAL-DEFINE DAYCAMPAIGN_STATUSCODE_ACTIVE 1 

&GLOBAL-DEFINE LANGUAGES "es_ES,es_CA,es_EU,es_GA,en"

/* Request Types */
&GLOBAL-DEFINE REQTYPE_SUBSCRIPTION_TYPE_CHANGE 0        
&GLOBAL-DEFINE REQTYPE_SERVICE_CHANGE 1
&GLOBAL-DEFINE REQTYPE_SALDO_PAYMENT 2
&GLOBAL-DEFINE REQTYPE_USER_CHANGE 3
&GLOBAL-DEFINE REQTYPE_INVOICE_CUSTOMER_CHANGE 4
&GLOBAL-DEFINE REQTYPE_USER_ACCOUNT_CHANGE 5
&GLOBAL-DEFINE REQTYPE_ADDRESS_CHANGE 6
&GLOBAL-DEFINE REQTYPE_CONTRACT_PIN 7
&GLOBAL-DEFINE REQTYPE_CONTRACT_ACTIVATION 8
&GLOBAL-DEFINE REQTYPE_CONTRACT_TERMINATION 9
&GLOBAL-DEFINE REQTYPE_AGREEMENT_CUSTOMER_CHANGE 10
&GLOBAL-DEFINE REQTYPE_PAYMENT_PLAN 11
&GLOBAL-DEFINE REQTYPE_MARKETING_VALUES 12
&GLOBAL-DEFINE REQTYPE_SUBSCRIPTION_CREATE 13
&GLOBAL-DEFINE REQTYPE_ICC_CHANGE 15
&GLOBAL-DEFINE REQTYPE_SUBSCRIPTION_TERMINATION 18
&GLOBAL-DEFINE REQTYPE_MSISDN_CHANGE 19
&GLOBAL-DEFINE REQTYPE_ONDEMAND_INVOICE 20
&GLOBAL-DEFINE REQTYPE_CREDIT_NOTE 22       
&GLOBAL-DEFINE REQTYPE_REFUND 23       
&GLOBAL-DEFINE REQTYPE_BANK_ACCOUNT_NUMBER_CHANGE 24       
&GLOBAL-DEFINE REQTYPE_FEEMODEL_PRICE_CHANGE 27 
&GLOBAL-DEFINE REQTYPE_PRICE_GUARANTEE  28
&GLOBAL-DEFINE REQTYPE_SMS_SEND 30
&GLOBAL-DEFINE REQTYPE_PAYMENT_CREATE 31
&GLOBAL-DEFINE REQTYPE_BANK_REF_UPDATE 32       
&GLOBAL-DEFINE REQTYPE_CREDIT_CHECK 33
&GLOBAL-DEFINE REQTYPE_MANUAL_PAYMENT 34       
&GLOBAL-DEFINE REQTYPE_BARRING_REQUEST 35
&GLOBAL-DEFINE REQTYPE_HEAT_BALANCE 37
&GLOBAL-DEFINE REQTYPE_AFTER_SALES_ORDER 46
&GLOBAL-DEFINE REQTYPE_REVERT_RENEWAL_ORDER 49
&GLOBAL-DEFINE REQTYPE_RERATE 65
&GLOBAL-DEFINE REQTYPE_CHARGE_AND_COMPENSATION 76
&GLOBAL-DEFINE REQTYPE_DUPLICATE_INVOICE 77
&GLOBAL-DEFINE REQTYPE_BUNDLE 78
&GLOBAL-DEFINE REQTYPE_SMS_INVOICE 79
&GLOBAL-DEFINE REQTYPE_IMEI_CHANGE 80
&GLOBAL-DEFINE REQTYPE_BUNDLE_CHANGE 81
&GLOBAL-DEFINE REQTYPE_SUBSCRIPTION_REACTIVATION 82
&GLOBAL-DEFINE REQTYPE_DSS 83
&GLOBAL-DEFINE REQTYPE_ACTIVATE_EMAIL_INVOICE 84
&GLOBAL-DEFINE REQTYPE_SEND_EMAIL_INVOICE 85
&GLOBAL-DEFINE REQTYPE_EMAIL_SENDING 86
&GLOBAL-DEFINE REQTYPE_LOGISTICS 87
&GLOBAL-DEFINE REQTYPE_INSTALLMENT_CONTRACT_CHANGE 88
&GLOBAL-DEFINE REQTYPE_TERMINAL_FINANCE_BANK_FILE 89
&GLOBAL-DEFINE REQTYPE_FUSION_EMAIL 90

&GLOBAL-DEFINE REQTYPES_HPD "0,1,6,8,9,10,13,15,18,19,35,46,49,80,81,82,88"
&GLOBAL-DEFINE HPD_SERVICES "VMS,LANG,CF,IRDCUTOFF,BB,NAM,CALLSPEC,LTE,BPSUB,LP"

&GLOBAL-DEFINE ORDER_CHANNEL_DIRECT "cc,telesales,self,emission,fusion_telesales,fusion_emission,fusion_cc,inversa"
&GLOBAL-DEFINE ORDER_CHANNEL_INDIRECT "pos,fusion_pos"

/* Request Statuses */
&GLOBAL-DEFINE REQUEST_STATUS_NEW 0
&GLOBAL-DEFINE REQUEST_STATUS_UNDER_WORK 1
&GLOBAL-DEFINE REQUEST_STATUS_DONE 2
&GLOBAL-DEFINE REQUEST_STATUS_REJECTED 3               /* Rejected (error occurred) */
&GLOBAL-DEFINE REQUEST_STATUS_CANCELLED 4              /* Cancelled */
&GLOBAL-DEFINE REQUEST_STATUS_HLR_PENDING 5            /* Pending HLR */
&GLOBAL-DEFINE REQUEST_STATUS_HLR_DONE 6               /* Pending - HLR DONE */
&GLOBAL-DEFINE REQUEST_STATUS_SUB_REQUEST_PENDING 7    /* Pending Sub-request */
&GLOBAL-DEFINE REQUEST_STATUS_SUB_REQUEST_DONE 8       /* Sub-request DONE */
&GLOBAL-DEFINE REQUEST_STATUS_HANDLED 9                /* Handled */
&GLOBAL-DEFINE REQUEST_STATUS_APPROVED 15              /* Approved for handling */
&GLOBAL-DEFINE REQUEST_STATUS_FILE_PENDING 16          /* Pending file creation */
&GLOBAL-DEFINE REQUEST_STATUS_FILE_DONE 17             /* File created */
&GLOBAL-DEFINE REQUEST_STATUS_CONFIRMATION_PENDING 19  /* Waiting for confirmation */
&GLOBAL-DEFINE REQUEST_STATUS_PROCESS_ERROR 99         /* Process error (Qvantel maintenance required) */

/* Request Sources */
&GLOBAL-DEFINE REQUEST_SOURCE_SUBSCRIPTION_CREATION "1"
&GLOBAL-DEFINE REQUEST_SOURCE_STC "2"
&GLOBAL-DEFINE REQUEST_SOURCE_SUBSCRIPTION_TERMINATION "3"
&GLOBAL-DEFINE REQUEST_SOURCE_MANUAL_TMS "4"
&GLOBAL-DEFINE REQUEST_SOURCE_SCRIPT "5"
&GLOBAL-DEFINE REQUEST_SOURCE_NEWTON "6"
&GLOBAL-DEFINE REQUEST_SOURCE_RENEWAL "7"
&GLOBAL-DEFINE REQUEST_SOURCE_IFS "9"
&GLOBAL-DEFINE REQUEST_SOURCE_YOIGO_TOOL "11"
&GLOBAL-DEFINE REQUEST_SOURCE_ACC "12"
&GLOBAL-DEFINE REQUEST_SOURCE_ORDER_CANCELLATION "14"
&GLOBAL-DEFINE REQUEST_SOURCE_EXTERNAL_API "15"
&GLOBAL-DEFINE REQUEST_SOURCE_BUNDLE_STC "16"
&GLOBAL-DEFINE REQUEST_SOURCE_BILLING "17"
&GLOBAL-DEFINE REQUEST_SOURCE_BTC "18"
&GLOBAL-DEFINE REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION "19"
&GLOBAL-DEFINE REQUEST_SOURCE_DSS "20"
&GLOBAL-DEFINE REQUEST_SOURCE_CONTRACT_TERMINATION "21"
&GLOBAL-DEFINE REQUEST_SOURCE_REVERT_RENEWAL_ORDER "22"
&GLOBAL-DEFINE REQUEST_SOURCE_MULTISIM "23"
&GLOBAL-DEFINE REQUEST_SOURCE_INSTALLMENT_CONTRACT_CHANGE "24"
&GLOBAL-DEFINE REQUEST_SOURCE_ICC_CHANGE_AUTO "25"
&GLOBAL-DEFINE REQUEST_SOURCE_CONTS30_PROMOTION "26"
&GLOBAL-DEFINE REQUEST_SOURCE_SERVICE_CHANGE "27"
&GLOBAL-DEFINE REQUEST_SOURCE_FUSION_ORDER_FALLBACK "28"
&GLOBAL-DEFINE REQUEST_SOURCE_FUSION_ORDER "29"
&GLOBAL-DEFINE REQUEST_SOURCE_FUSION_EMAIL "30"
&GLOBAL-DEFINE REQUEST_SOURCE_CONTRACT_ACTIVATION "31"
&GLOBAL-DEFINE REQUEST_SOURCE_MAIN_LINE_DEACTIVATION "32"

&GLOBAL-DEFINE REQUEST_SOURCES_MANUAL "4,6,11,15"

/* Request Action Rule List */
&GLOBAL-DEFINE REQUEST_ACTIONLIST_ALL "1,2,3,5,6,11"
&GLOBAL-DEFINE REQUEST_ACTIONLIST_PENALTYFEE "5,11"
&GLOBAL-DEFINE REQUEST_ACTIONLIST_ALL_EXCL_PENALTYFEE "1,2,3,6"

/* Mobsub Termimation Reason */
&GLOBAL-DEFINE SUBSCRIPTION_TERM_REASON_MNP 2 
&GLOBAL-DEFINE SUBSCRIPTION_TERM_REASON_ORDER_CANCELLATION 3 
&GLOBAL-DEFINE SUBSCRIPTION_TERM_REASON_POS_ORDER_CANCELATION 11 
&GLOBAL-DEFINE SUBSCRIPTION_TERM_REASON_DIRECT_ORDER_CANCELATION 12 
&GLOBAL-DEFINE SUBSCRIPTION_TERM_REASON_MULTISIM 13
&GLOBAL-DEFINE SUBSCRIPTION_TERM_REASON_ADDITIONALSIM 14

/* user limits for charges and compensations types */
&GLOBAL-DEFINE POST_CHARGE_LIMIT_TYPE 1
&GLOBAL-DEFINE POST_COMP_LIMIT_TYPE 2
&GLOBAL-DEFINE PREP_CHARGE_LIMIT_TYPE 3
&GLOBAL-DEFINE PREP_COMP_LIMIT_TYPE 4
&GLOBAL-DEFINE POST_CHARGE_MONTHLY_LIMIT_TYPE 5
&GLOBAL-DEFINE POST_COMP_MONTHLY_LIMIT_TYPE 6
&GLOBAL-DEFINE PREP_CHARGE_MONTHLY_LIMIT_TYPE 7
&GLOBAL-DEFINE PREP_COMP_MONTHLY_LIMIT_TYPE 8
&GLOBAL-DEFINE CREDIT_NOTE_LIMIT_TYPE 9

/* MNPProcess.MNPType */
&GLOBAL-DEFINE MNP_TYPE_OLD 0
&GLOBAL-DEFINE MNP_TYPE_IN 1
&GLOBAL-DEFINE MNP_TYPE_OUT 2
&GLOBAL-DEFINE MNP_TYPE_TERMINATION 3
&GLOBAL-DEFINE MNP_TYPE_MIGRATION 5

/* MNPProcess.StatusCode (common) */
&GLOBAL-DEFINE MNP_ST_NEW 0
&GLOBAL-DEFINE MNP_ST_ERROR 999
/* MNPProcess.StatusCode (in & out) */
&GLOBAL-DEFINE MNP_ST_AENV 1
&GLOBAL-DEFINE MNP_ST_ASOL 2
&GLOBAL-DEFINE MNP_ST_ACON 5
&GLOBAL-DEFINE MNP_ST_APOR 6
&GLOBAL-DEFINE MNP_ST_AREC 4
&GLOBAL-DEFINE MNP_ST_ACAN 7
&GLOBAL-DEFINE MNP_ST_AREC_CLOSED 8
/* MNPProcess.StatusCode (migration) */
&GLOBAL-DEFINE MNP_ST_MENV 10 
&GLOBAL-DEFINE MNP_ST_MPRC 11
&GLOBAL-DEFINE MNP_ST_MFIN 12
&GLOBAL-DEFINE MNP_ST_NENV 13
&GLOBAL-DEFINE MNP_ST_NCON 14
&GLOBAL-DEFINE MNP_ST_NMIG 15
/* MNPProcess.StatusCode (termination) */
&GLOBAL-DEFINE MNP_ST_BNOT 20 
&GLOBAL-DEFINE MNP_ST_BCAN 21
&GLOBAL-DEFINE MNP_ST_BDEF 22
&GLOBAL-DEFINE MNP_ST_BDET 23

/* MNPProcess.StateFlag */
&GLOBAL-DEFINE MNP_STATEFLAG_NOT_ANALYSED 0 
&GLOBAL-DEFINE MNP_STATEFLAG_REJECT_PROPOSAL 1 
&GLOBAL-DEFINE MNP_STATEFLAG_REJECT 2 
&GLOBAL-DEFINE MNP_STATEFLAG_CONFIRM_PROPOSAL 3
&GLOBAL-DEFINE MNP_STATEFLAG_CONFIRM 4

/* MNP cancel proposal statuses */
&GLOBAL-DEFINE MNP_CANCEL_PROPOSAL_NEW 0
&GLOBAL-DEFINE MNP_CANCEL_PROPOSAL_CONFIRMED 1
&GLOBAL-DEFINE MNP_CANCEL_PROPOSAL_REJECTED 2
&GLOBAL-DEFINE MNP_CANCEL_PROPOSAL_CANCELLED 3
&GLOBAL-DEFINE MNP_CANCEL_PROPOSAL_RECIPIENT 4

/* MNPOperation.Sender */
&GLOBAL-DEFINE MNP_SENDER_TMS 1
&GLOBAL-DEFINE MNP_SENDER_MNP 2

/* MNPProcess.MNPSeq */
&GLOBAL-DEFINE MNP_PROCESS_QUERY 50000000
&GLOBAL-DEFINE MNP_PROCESS_DUMMY_IN 100000000
&GLOBAL-DEFINE MNP_PROCESS_DUMMY_OUT 100000001

/* MNPOperation.StatusCode */
&GLOBAL-DEFINE MNP_MSG_WAITING_SEND 1
&GLOBAL-DEFINE MNP_MSG_SENT_TO_FILE 2
&GLOBAL-DEFINE MNP_MSG_WAITING_RESPONSE_HANDLE 5
&GLOBAL-DEFINE MNP_MSG_HANDLED 10
&GLOBAL-DEFINE MNP_MSG_ERROR 999 /* ccbs handling error */
&GLOBAL-DEFINE MNP_MSG_DELAYED 998 /* message is delayed */
&GLOBAL-DEFINE MNP_MSG_PARSING 900 /* request/response parsing error */
&GLOBAL-DEFINE MNP_MSG_NC 901 /* NC error */
&GLOBAL-DEFINE MNP_MSG_HANDLING 902 /* handling error */
&GLOBAL-DEFINE MNP_MSG_ADAPTER 903 /* mnp adapter error */

/* MNPOperation.ErrorCode */
&GLOBAL-DEFINE MNP_ERRORCODE_PARSE "MNP_PARSE"
&GLOBAL-DEFINE MNP_ERRORCODE_HANDLE "MNP_PROCE"
&GLOBAL-DEFINE MNP_ERRORCODE_ADAPTER "MNP_ADAPT"

/* Common MNP handling errors */
&GLOBAL-DEFINE MNP_ERROR_UNKNOWN_PROCESS "Unknown MNP process"
&GLOBAL-DEFINE MNP_ERROR_WRONG_TYPE "Wrong MNP process type"
&GLOBAL-DEFINE MNP_ERROR_WRONG_STATUS "Wrong statuscode"

/* MNPOperation.ErrorHandled */
&GLOBAL-DEFINE MNP_ERRORHANDLED_NO 1
&GLOBAL-DEFINE MNP_ERRORHANDLED_YES 2

/* BillItem Definitions */
&GLOBAL-DEFINE BITEM_GRP_INTERNET "3"
&GLOBAL-DEFINE BITEM_GRP_TERMINAL "7"
&GLOBAL-DEFINE BITEM_GRP_CHARGE "31"
&GLOBAL-DEFINE BITEM_GRP_COMPENSATION "32"
&GLOBAL-DEFINE BITEM_GRP_LAPTOP "34"
&GLOBAL-DEFINE BITEM_GRP_DATABUNDLE "35"
&GLOBAL-DEFINE BITEM_GRP_DATABUNDLE2 "37"
&GLOBAL-DEFINE BITEM_GRP_TERMINAL_ACCOUNT 70020105
&GLOBAL-DEFINE BITEM_GRP_TERMINAL_TAXCLASS "2"
&GLOBAL-DEFINE BITEM_GRP_TERMINAL_SAPRID "032"
&GLOBAL-DEFINE BITEM_GRP_INTERNET_TARJ7DATA 70514100

/* ROI Send History Status */
&GLOBAL-DEFINE ROI_HISTORY_TO_SEND 1 
&GLOBAL-DEFINE ROI_HISTORY_SENT 2 
&GLOBAL-DEFINE ROI_HISTORY_ERROR 3 

/* Payment method */
&GLOBAL-DEFINE PAYM_METHOD_PAYPAL "6"

/* Invoice.ChargeType*/
&GLOBAL-DEFINE INV_CHARGE_T_PAYPAL 6

/* Invoice.InvType */
&GLOBAL-DEFINE INV_TYPE_NORMAL 1
&GLOBAL-DEFINE INV_TYPE_CASH 6
&GLOBAL-DEFINE INV_TYPE_RECEIPT 7
&GLOBAL-DEFINE INV_TYPE_TEST 99

/* Invoice.PrintState */
&GLOBAL-DEFINE INV_PRINTSTATE_PRINTHOUSE 1

/* Invoice delivery types */
&GLOBAL-DEFINE INV_DEL_TYPE_PAPER 1
&GLOBAL-DEFINE INV_DEL_TYPE_EMAIL 2
&GLOBAL-DEFINE INV_DEL_TYPE_FUSION_EMAIL 3
&GLOBAL-DEFINE INV_DEL_TYPE_SMS 4
&GLOBAL-DEFINE INV_DEL_TYPE_NO_DELIVERY 10
&GLOBAL-DEFINE INV_DEL_TYPE_EMAIL_PENDING 11
&GLOBAL-DEFINE INV_DEL_TYPE_NO_TRAFFIC 12
&GLOBAL-DEFINE INV_DEL_TYPE_FUSION_EMAIL_PENDING 13

/* call cases */
&GLOBAL-DEFINE ROAMING_CALLCASE "3,4,7,32,33,53,54"

/* SelfService API */
&GLOBAL-DEFINE DPL_REASON_MIYOIGO 0
&GLOBAL-DEFINE DPL_REASON_TRBLE_SHOOT 1
&GLOBAL-DEFINE DPL_REASON_AUTH_REQ 2
&GLOBAL-DEFINE DPL_REASON_INVOICE 3
&GLOBAL-DEFINE DPL_REASON_OTHER 4
&GLOBAL-DEFINE DPL_REASON_VISTA 5
&GLOBAL-DEFINE DPL_REASON_EAPI 6

/* Performance Indicator types */
&GLOBAL-DEFINE P_INDICATOR_TYPE_SATISFACTION_VALUE 1

/* Substerminal.TerminalType, OrderAccessory.TerminalType */
&GLOBAL-DEFINE TERMINAL_TYPE_PHONE 1
&GLOBAL-DEFINE TERMINAL_TYPE_LAPTOP 2

/* Customer.InvoiceTargetRule */
&GLOBAL-DEFINE INVOICE_TARGET_RULE_UNDEFINED 0
&GLOBAL-DEFINE INVOICE_TARGET_RULE_DEFAULT_GROUP 1
&GLOBAL-DEFINE INVOICE_TARGET_RULE_SEPARATE_GROUP 2

/* Invoice target grouping states */
/* used on Web side, do not change descriptions */
&GLOBAL-DEFINE INVOICE_TARGET_ALL_GROUPED "all_grouped"
&GLOBAL-DEFINE INVOICE_TARGET_ALL_SPLIT "all_split"
&GLOBAL-DEFINE INVOICE_TARGET_CUSTOMIZED "customized"

/* OrderAccessory.IMEIStatus */
&GLOBAL-DEFINE IMEI_STATUS_UNKNOWN 0
&GLOBAL-DEFINE IMEI_STATUS_USED 1
&GLOBAL-DEFINE IMEI_STATUS_TO_BE_RELEASED 2
&GLOBAL-DEFINE IMEI_STATUS_RELEASED 3

/* CLIType.PayType */
&GLOBAL-DEFINE CLITYPE_PAYTYPE_NOT_DEFINED 0
&GLOBAL-DEFINE CLITYPE_PAYTYPE_POSTPAID 1
&GLOBAL-DEFINE CLITYPE_PAYTYPE_PREPAID 2
&GLOBAL-DEFINE CLITYPE_WEB_ACTIVE_STATUSES "1,2"
&GLOBAL-DEFINE CLITYPE_STC_ACTIVE_STATUSES "1,2"

&GLOBAL-DEFINE CLITYPE_WEBSTATUSCODE_ACTIVE 1

&GLOBAL-DEFINE CLITYPE_LINETYPE_NONMAIN 0
&GLOBAL-DEFINE CLITYPE_LINETYPE_MAIN 1
&GLOBAL-DEFINE CLITYPE_LINETYPE_ADDITIONAL 2

/* MobSub.PayType */
&GLOBAL-DEFINE MOBSUB_PAYTYPE_POSTPAID False
&GLOBAL-DEFINE MOBSUB_PAYTYPE_PREPAID True

/* ActionLog.ActionStatus */
&GLOBAL-DEFINE ACTIONLOG_STATUS_ACTIVE 0
&GLOBAL-DEFINE ACTIONLOG_STATUS_ERROR 1
&GLOBAL-DEFINE ACTIONLOG_STATUS_SUCCESS 2
&GLOBAL-DEFINE ACTIONLOG_STATUS_LOGGED 3
&GLOBAL-DEFINE ACTIONLOG_STATUS_CANCELLED 5
&GLOBAL-DEFINE ACTIONLOG_STATUS_PROCESSING 6

/* MobSub.MSStatus */
&GLOBAL-DEFINE MSSTATUS_ACTIVE 4
&GLOBAL-DEFINE MSSTATUS_BARRED 8

/* TMRule.TicketType */
&GLOBAL-DEFINE TICKET_TYPE_MOBILE 1
&GLOBAL-DEFINE TICKET_TYPE_FRAUD 2

/* periodical contract type */
&GLOBAL-DEFINE PERCONTRACT_RATING_PACKAGE "1,4,8"
&GLOBAL-DEFINE PERCONTRACT_DCCLI_DCTYPE "3,5,7"

/* ServiceLimit.InclUnit */
&GLOBAL-DEFINE INCLUNIT_MINUTE 1
&GLOBAL-DEFINE INCLUNIT_SECOND 2
&GLOBAL-DEFINE INCLUNIT_GIGABYTE 3
&GLOBAL-DEFINE INCLUNIT_MEGABYTE 4
&GLOBAL-DEFINE INCLUNIT_QUANTITY 5
&GLOBAL-DEFINE INCLUNIT_AMOUNT 6
&GLOBAL-DEFINE INCLUNIT_BDEST_QTY 7

&GLOBAL-DEFINE DIAL_TYPE_VOICE 4
&GLOBAL-DEFINE DIAL_TYPE_GPRS 7

&GLOBAL-DEFINE PMDUB "PMDUB"
&GLOBAL-DEFINE HSPA_ROAM_EU "HSPA_ROAM_EU"
&GLOBAL-DEFINE BONO8_SMS_SENDER "22642"
&GLOBAL-DEFINE UPSELL_SMS_SENDER "622"
&GLOBAL-DEFINE BB_SMS_SENDER "622"
&GLOBAL-DEFINE STC_SMS_SENDER "622"
&GLOBAL-DEFINE FRAUD_BARR_CODES "Debt_Hotl,Debt_Restricted,Limits_Restricted,Risk_Restricted,ATC_Service_Suspension,Risk_Suspension,Debt_HOTLP,Debt_LP"
&GLOBAL-DEFINE DSS "DSS"
&GLOBAL-DEFINE DSS_LOWSPEED_BDEST "GPRSDSS_B"
&GLOBAL-DEFINE DSS_BUNDLES "DSS,DSS2"
&GLOBAL-DEFINE DSS2_LOWSPEED_BDEST "GPRSDSS2_B"
&GLOBAL-DEFINE TARJ_UPSELL "TARJ_UPSELL"

/* CallAlarm.CrediType */
&GLOBAL-DEFINE SMSTYPE_INFO 9 
&GLOBAL-DEFINE SMSTYPE_STC 6 
&GLOBAL-DEFINE SMSTYPE_CONTRACT_ACTIVATION 10
&GLOBAL-DEFINE SMSTYPE_BARRING 24
&GLOBAL-DEFINE SMSTYPE_PREMIUM 26
&GLOBAL-DEFINE SMSTYPE_AFTER_SALES_ORDER 41 
&GLOBAL-DEFINE SMSTYPE_MNP_RETENTION 45
&GLOBAL-DEFINE SMSTYPE_MARKETING 46
&GLOBAL-DEFINE SMSTYPE_HPD 47

&GLOBAL-DEFINE CA_DELISTAT_NEW 1
&GLOBAL-DEFINE CA_DELISTAT_SENT 3

/* orderdelivery.CourierId */
&GLOBAL-DEFINE COURIER_ID_DEXTRA 0
&GLOBAL-DEFINE COURIER_ID_CORREOS 1
&GLOBAL-DEFINE COURIER_ID_SEUR 2
&GLOBAL-DEFINE COURIER_ID_OTHER 3
&GLOBAL-DEFINE COURIER_ID_ASM 4

/* Prepaid service class id values */
&GLOBAL-DEFINE SC_TARJ5_NORMAL 6
&GLOBAL-DEFINE SC_TARJ5_NORMAL_BONO 86
&GLOBAL-DEFINE SC_TARJ5_PROMOTIONAL 106
&GLOBAL-DEFINE SC_TARJ5_PROMOTIONAL_BONO 186
&GLOBAL-DEFINE SC_TARJ7_OUTSIDE_DATABUNDLE 103
&GLOBAL-DEFINE SC_TARJ7_INSIDE_DATABUNDLE1 3
&GLOBAL-DEFINE SC_TARJ7_INSIDE_DATABUNDLE2 303

&GLOBAL-DEFINE MSISDN_YOIGO_PREFIXES "622,633,7220,7221,7222,7223,7224,7225,7226,7227,7228"
&GLOBAL-DEFINE YOIGO_FREE_NUMBERS "622,622622622,622100100,622FF,633,633633633,633800800,633633556,556"

/* InvText.SendRule */
&GLOBAL-DEFINE SMS_SENDRULE_NOT_DEFINED ""
&GLOBAL-DEFINE SMS_SENDRULE_OFFICEH "R2"
&GLOBAL-DEFINE SMS_SENDRULE_OFFICEH_EXCEPT_LAST_DAY "R2+R3"
&GLOBAL-DEFINE SMS_SENDRULE_24H "R1"
&GLOBAL-DEFINE SMS_SENDRULE_24H_EXCEPT_LAST_DAY "R1+R3"
&GLOBAL-DEFINE SMS_SENDRULES "R1,R2,R1+R3,R2+R3"

/* counter.countertype */
&GLOBAL-DEFINE COUNTERTYPE_FAT_SMSBUNDLE 1
&GLOBAL-DEFINE COUNTERTYPE_FAY_SMS 2
&GLOBAL-DEFINE COUNTERTYPE_FAT_AMOUNT 3
&GLOBAL-DEFINE COUNTERTYPE_DISCOUNT_AMOUNT 4

/* Order.MultiSimType */
&GLOBAL-DEFINE MULTISIMTYPE_PRIMARY 1
&GLOBAL-DEFINE MULTISIMTYPE_SECONDARY 2

/* RepText TextType */
&GLOBAL-DEFINE REPTEXT_SMS 32

/* FixedFee.FinancedResult */
&GLOBAL-DEFINE TF_STATUS_BANK "00"
&GLOBAL-DEFINE TF_STATUS_HOLD_SENDING "B00"
&GLOBAL-DEFINE TF_STATUS_WAITING_SENDING "B01"
&GLOBAL-DEFINE TF_STATUS_SENT_TO_BANK "B02"
&GLOBAL-DEFINE TF_STATUS_BANK_MANUAL "B99"

&GLOBAL-DEFINE TF_STATUS_YOIGO "Y00"
&GLOBAL-DEFINE TF_STATUS_YOIGO_ACC "Y01"
&GLOBAL-DEFINE TF_STATUS_YOIGO_SUB_TERMINATED "Y02"
&GLOBAL-DEFINE TF_STATUS_YOIGO_FF_TERMINATED "Y03"
&GLOBAL-DEFINE TF_STATUS_YOIGO_FF_CHANGED "Y04"
&GLOBAL-DEFINE TF_STATUS_YOIGO_ANALYZE_FAILED "Y05"
&GLOBAL-DEFINE TF_STATUS_YOIGO_NO_BANK_RESPONSE "Y06"
&GLOBAL-DEFINE TF_STATUS_YOIGO_REACTIVATION "Y07"
&GLOBAL-DEFINE TF_STATUS_YOIGO_INSTALLMENT_CHANGE "Y08"
&GLOBAL-DEFINE TF_STATUS_YOIGO_REVERT_RENEWAL "Y09"
&GLOBAL-DEFINE TF_STATUS_YOIGO_RENEWAL "Y10"
&GLOBAL-DEFINE TF_STATUS_YOIGO_STC "Y11"
&GLOBAL-DEFINE TF_STATUS_YOIGO_RVTERM_FEE_DELETION "Y12"
&GLOBAL-DEFINE TF_STATUS_YOIGO_LOGISTICS "Y13"
&GLOBAL-DEFINE TF_STATUS_YOIGO_OTHER "Y14"
&GLOBAL-DEFINE TF_STATUS_YOIGO_REACTIVATION_FBB "Y15"
&GLOBAL-DEFINE TF_STATUS_YOIGO_MANUAL "Y99"

&GLOBAL-DEFINE TF_STATUSES_MANUAL "B99,Y99"
&GLOBAL-DEFINE TF_STATUSES_BANK "00,B99"

&GLOBAL-DEFINE TF_CANCEL_ORDER_CANCEL "ORDER_CANCEL"
&GLOBAL-DEFINE TF_CANCEL_RENEWAL "RENEWAL"
&GLOBAL-DEFINE TF_CANCEL_INSTALLMENT "INSTALLMENT"
&GLOBAL-DEFINE TF_CANCEL_TERMINATION "TERMINATION"
&GLOBAL-DEFINE TF_CANCEL_STC "STC"
&GLOBAL-DEFINE TF_CANCEL_RENEWAL_CANCEL "RENEWAL_CANCEL"
&GLOBAL-DEFINE TF_CANCEL_MNP "MNP"
&GLOBAL-DEFINE TF_CANCEL_ACC "ACC"
&GLOBAL-DEFINE TF_CANCEL_OTHER "OTHER"

&GLOBAL-DEFINE TF_BANK_CODES "0049,0081"
&GLOBAL-DEFINE TF_BANK_UNOE "0049"
&GLOBAL-DEFINE TF_BANK_SABADELL "0081"

&GLOBAL-DEFINE TF_BANK_RVTERM_BILLCODES "RVTERM1EF,RVTERMBSF"
&GLOBAL-DEFINE TF_BANK_UNOE_PAYTERM_BILLCODES "PAYTERM1E,PAYTERMEND1E"
&GLOBAL-DEFINE TF_BANK_SABADELL_PAYTERM_BILLCODES "PAYTERMBS,PAYTERMENDBS"
&GLOBAL-DEFINE TF_BANK_ALL_ACTIVE_PAYTERM_BILLCODES "PAYTERM1E,PAYTERMBS"
&GLOBAL-DEFINE TF_BANK_ALL_CLOSED_PAYTERM_BILLCODES "PAYTERMEND1E,PAYTERMENDBS"
&GLOBAL-DEFINE TF_BANK_COMMISSION_BILLCODES "PAYTERMCG1E,PAYTERMCGBS"

&GLOBAL-DEFINE IFS_STATUS_WAITING_SENDING 1
&GLOBAL-DEFINE IFS_STATUS_SENT 2
&GLOBAL-DEFINE IFS_STATUS_SENDING_CANCELLED 3

&GLOBAL-DEFINE HOSTNAME_STAGING "merak,botein,merga,alpheratz"
&GLOBAL-DEFINE HOSTNAME_DEVEL "sadira"

&GLOBAL-DEFINE SERVCOM_TARGET_STATELESS 3

/*Barring */
/*Barring status*/
&GLOBAL-DEFINE BARR_STATUS_ACTIVE "ACTIVE"
&GLOBAL-DEFINE BARR_STATUS_INACTIVE "INACTIVE"
/*Barring Rule status values*/
&GLOBAL-DEFINE BARR_RULE_STATUS_ACTIVE "active"
&GLOBAL-DEFINE BARR_RULE_STATUS_INACTIVE "inactive"
&GLOBAL-DEFINE BARR_RULE_STATUS_RETIRED "retired"
/*Barring rule action values*/
&GLOBAL-DEFINE BARR_RULE_ACT_ON_HOLD "onhold"
&GLOBAL-DEFINE BARR_RULE_ACT_REMOVE_NO_RESTORE "removenorestore"
&GLOBAL-DEFINE BARR_RULE_ACT_REM_WITH_RESTORE "removewithrestore"
&GLOBAL-DEFINE BARR_RULE_ACT_NOT_ALLOWED "notallowed"


&ENDIF
