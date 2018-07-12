&IF "{&HPDConst}" NE "YES"
&THEN

&GLOBAL-DEFINE HPDConst YES

&GLOBAL-DEFINE HPDKeyDelimiter CHR(255)
&GLOBAL-DEFINE HPDDumpDelimiter '|'
&GLOBAL-DEFINE HPDTimeSeparator '_'
&GLOBAL-DEFINE HPDHeaderFieldCount 6

&GLOBAL-DEFINE FileIDForMQ __MQ__

&GLOBAL-DEFINE SKIP_NEW_CDR_DATA NO

/* DSS,DSS_UPSELL,DSS_UPSELL_UPGRADE,DSS2,DSS2_UPSELL,DSS200_UPSELL,DSS_FLEX_500MB_UPSELL,DSS_FLEX_5GB_UPSELL,DSS_FLEX_UPSELL */
&GLOBAL-DEFINE DSS_SLSEQS 28,29,48,73,74,111,163,164,171

&GLOBAL-DEFINE BARRING_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE BARRINGCONF_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE BILLITEM_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE BITEMGROUP_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE CLITYPE_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE CUSTOMER_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE CUSTCONTACT_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE DAYCAMPAIGN_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE DCCLI_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE DISCOUNTPLAN_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE DMS_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE DMSDOC_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE DPMEMBER_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE DPTARGET_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE FATGROUP_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE FATIME_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE FIXEDFEE_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE FMITEM_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE IMSI_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE INVOICE_WRITE_TRIGGER_ACTIVE NO
&GLOBAL-DEFINE INVROW_WRITE_TRIGGER_ACTIVE NO
&GLOBAL-DEFINE LIMIT_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE MNPPROCESS_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE MOBCDR_WRITE_TRIGGER_ACTIVE NO
&GLOBAL-DEFINE MOBSUB_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE MSERVICELIMIT_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE MSERVICELPOOL_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE MSREQUEST_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE NATHOLIDAY_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE ORDER_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE ORDERACCESSORY_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE ORDERACTION_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE ORDERCUSTOMER_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE ORDERDELIVERY_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE ORDERFUSION_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE ORDERPRODUCT_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE ORDERPRODUCTPARAM_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE ORDERSUBSCRIPTION_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE ORDERPAYMENT_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE PAYMENT_WRITE_TRIGGER_ACTIVE NO
&GLOBAL-DEFINE PREPAIDREQUEST_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE PREPCDR_WRITE_TRIGGER_ACTIVE NO
&GLOBAL-DEFINE PREPEDR_WRITE_TRIGGER_ACTIVE NO
&GLOBAL-DEFINE REPTEXT_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE REQUESTTYPE_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE SERVICELCOUNTER_WRITE_TRIGGER_ACTIVE NO
&GLOBAL-DEFINE SERVICELIMIT_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE SERVPAC_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE SINGLEFEE_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE SUBINVOICE_WRITE_TRIGGER_ACTIVE NO
&GLOBAL-DEFINE SUBSER_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE SUBSTERMINAL_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE TERMRETURN_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE TOPUPSCHEMEROW_WRITE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE TPSERVICE_WRITE_TRIGGER_ACTIVE YES


&GLOBAL-DEFINE BARRING_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE BARRINGCONF_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE BILLITEM_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE BITEMGROUP_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE CLITYPE_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE CUSTOMER_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE CUSTCONTACT_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE DAYCAMPAIGN_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE DCCLI_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE DISCOUNTPLAN_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE DMS_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE DMSDOC_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE DPMEMBER_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE DPTARGET_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE FATGROUP_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE FATIME_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE FIXEDFEE_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE FMITEM_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE IMSI_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE INVOICE_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE INVROW_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE LIMIT_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE MNPPROCESS_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE MOBCDR_DELETE_TRIGGER_ACTIVE NO
&GLOBAL-DEFINE MOBSUB_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE MSERVICELIMIT_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE MSERVICELPOOL_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE MSREQUEST_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE NATHOLIDAY_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE PREPAIDREQUEST_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE ORDER_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE ORDERACCESSORY_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE ORDERACTION_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE ORDERCUSTOMER_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE ORDERDELIVERY_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE ORDERFUSION_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE ORDERPRODUCT_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE ORDERPRODUCTPARAM_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE ORDERSUBSCRIPTION_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE ORDERPAYMENT_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE PAYMENT_DELETE_TRIGGER_ACTIVE NO
&GLOBAL-DEFINE PREPAIDREQUEST_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE PREPCDR_DELETE_TRIGGER_ACTIVE NO
&GLOBAL-DEFINE PREPEDR_DELETE_TRIGGER_ACTIVE NO
&GLOBAL-DEFINE REQUESTTYPE_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE REPTEXT_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE SERVICELCOUNTER_DELETE_TRIGGER_ACTIVE NO
&GLOBAL-DEFINE SERVICELIMIT_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE SERVPAC_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE SINGLEFEE_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE SUBINVOICE_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE SUBSER_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE SUBSTERMINAL_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE TERMRETURN_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE TOPUPSCHEMEROW_DELETE_TRIGGER_ACTIVE YES
&GLOBAL-DEFINE TPSERVICE_DELETE_TRIGGER_ACTIVE YES

&ENDIF
