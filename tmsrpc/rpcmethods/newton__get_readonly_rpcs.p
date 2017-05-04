/**
 * Returns a list of READ-ONLY TMS-RPCs that can be called against TMS replication server.
 *
 * @input       none;no input
 * @output      array of string;read-only method names
*/
{xmlrpc/xmlrpc_access.i}
DEF VAR resp_array AS CHAR NO-UNDO.
resp_array = add_array(response_toplevel_id, "").

DEF VAR lcReadOnlyRpcs AS CHAR NO-UNDO.
lcReadOnlyRpcs =
"newton__check_mnp_fixed_number.p " +
"newton__customers__get_deliverables.p " +
"newton__get_terminal_information.p " +
"newton__get_router_logistics.p " +
"newton__get_history.p " +
"newton__mnp_retention_rules__get.p " +
"newton__mnp_xmlmessages__get.p " +
"newton__order_sms__get.p " +
"newton__sms__get.p " +
"newton__test_numeric_format.p " +
"newton__subscriptions__single_fees.p " +
"newton__subscriptions__get_segmentation_history.p " +
"newton__usage_counter.p " +
"newton__tms_codes__list.p " +
"newton__tms_codes__get.p " +
"tmsdump__get_subscription_types.p " +
"tmsdump__get_services_subscription_types.p " +
"tmsdump__get_services.p " +
"tmsdump__get_order_item_types.p " +
"tmsdump__get_countries.p " +
"newton__get_credit_limit.p " +
"newton__acc_check_customer.p " +
"newton__customers__get_counters.p " +
"newton__get_dss_details.p " +
"newton__get_commissions.p " +
"newton__fusion_invoices_list.p " +
"newton__edrs__list.p " +
"newton__customers__get_satisfaction_history.p " +
"newton__get_order_customer_details.p " +
"newton__get_mobsub_services.p " +
"newton__get_mobsub_barrings.p " +
"newton__get_free_iccs.p " +
"newton__get_freeairtime.p " +
"newton__get_top_up_history.p " +
"newton__get_order_logistics.p " +
"newton__invrow__list.p " +
"newton__invoices__get.p " +
"newton__invoice_groups__list.p " +
"newton__order_sms__list.p " +
"newton__mnp_processes__get.p " +
"newton__search_orders.p " +
"newton__search_mobsub_with_sales_id.p " +
"newton__queue_orders__list.p " +
"newton__subscriptions__get_satisfaction_history.p " +
"newton__stc_termination_precheck.p " +
"newton__sms__list.p " +
"newton__show_active_test_subscriptions__list.p " +
"test__order_exists.p " +
"newton__check_referee.p " +
"newton__validate_customer.p " +
"newton__get_cdrfiles.p " +
"newton__periodical_contracts__get.p " +
"newton__optional_bundle_items__get.p " +
"newton__fat_schemes__get.p " +
"newton__bundle_items__get.p " +
"newton__topup_schemes__get.p " +
"newton__topup_scheme_rows__get.p " +
"newton__service_packages__get.p " +
"newton__resellers__get.p " +
"newton__configurations__get.p " +
"newton__check_mnp_number.p " +
"newton__get_memos.p " +
"newton__get_free_numbers.p " +
"newton__get_contract_details.p " +
"newton__fusion_invoices_get.p " +
"newton__email_template__list.p " +
"newton__invoices__list.p " +
"newton__invoices__einvoice_query.p " +
"newton__invoice_groups__get.p " +
"newton__imeis__to_be_released.p " +
"newton__icc_detail.p " +
"newton__get_subscription_types.p " +
"newton__get_mincons_file_statuses.p " +
"newton__mnp_retention_rules_sms__get.p " +
"newton__mnp_retention_rules_log__get.p " +
"newton__mnp_retention_dispatch_log__get.p " +
"newton__mnp_retention_dispatch__get.p " +
"newton__mnppdfs__get.p " +
"newton__search_fusion_orders.p " +
"newton__scheduled_dumps__get.p " +
"newton__requests__search.p " +
"newton__requests__list.p " +
"newton__requests__get.p " +
"newton__requests__counts.p " +
"newton__order_final_check.p " +
"newton__sms_template__logs.p " +
"newton__sms_template__list.p " +
"tmsdump__highspender.p " +
"tmsdump__get_device_models_subscription_types.p " +
"tmsdump__get_device_models.p " +
"newton__topups__list.p " +
"newton__topups__get.p " +
"newton__subscriptions__search_terminated.p " +
"tmsdump__sms_report.p " +
"newton__mnp_processes__list.p " +
"newton__charge_events__get.p " +
"newton__billing_items__get.p " +
"newton__service_packages__list.p " +
"newton__get_order_details.p " +
"newton__customer_categories__get.p " +
"newton__check_mnp_porting_date.p " +
"newton__offer_criteria__list.p " +
"newton__offer_criteria__get.p " +
"newton__fat_schemes__list.p " +
"newton__events__list.p " +
"newton__events__get.p " +
"newton__discount_plans__list.p " +
"newton__customer_categories__list.p " +
"newton__charge_events__list.p " +
"newton__bundles__list.p " +
"newton__bundle_items__list.p " +
"newton__billing_items__list.p " +
"newton__topup_schemes__list.p " +
"newton__topup_scheme_rows__list.p " +
"newton__subscription_types__list.p " +
"newton__resellers__list.p " +
"newton__periodical_contracts__list.p " +
"newton__optional_bundle_items__list.p " +
"newton__offers__list.p " +
"newton__offers__get.p " +
"newton__offer_items__list.p " +
"newton__offer_items__get.p " +
"newton__search_mobsub.p " +
"newton__subscription_search_tool.p " +
"newton__get_customer_details.p " +
"newton__subscription_types__get.p " +
"newton__dump_status__get.p " +
"newton__discount_plans__get.p " +
"newton__check_customer.p " +
"newton__subscriptions__search_by_criteria.p " +
"newton__get_mobsub_balance.p " +
"newton__check_renove.p " +
"newton__bundles__get.p " +
"newton__get_mobsub_usage.p " +
"newton__get_mobsub_details.p " +
"newton__mobsub_bundles__list.p " +
"newton__get_subscription_type_rule.p " +
"newton__subscriptions__get_terminated_details.p " +
"newton__tfconfs__list.p " +
"newton__tfconfs__get.p " +
"newton__mobsub_bundles__get.p " +
"newton__get_order_memos.p".

DEF VAR i AS INT NO-UNDO.
DEF VAR lcRPC AS CHAR NO-UNDO.

DO i = 1 TO NUM-ENTRIES(lcReadOnlyRpcs," "):
   ASSIGN
      lcRPC = entry(i,lcReadOnlyRpcs, " ")
      lcRPC = REPLACE(lcRPC,"__",".")
      lcRPC = REPLACE(lcRPC,".p","").
   add_string(resp_array,"",lcRPC).
end.

