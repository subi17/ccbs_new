"BillRunSplit" 1 1 "Split customers for the billing run" "sergey.strizhov@qvantel.com,toni.valve@qvantel.com" "00358406487542,00358407670038,00358400591125" "/opt/local/bin/xfear -bg_batch funcrun_invrun_split.p tms.pf #PARAM" 1000 "1" yes
"BillRun" 2 15 "Billing run" "toni.valve@qvantel.com,sergey.strizhov@qvantel.com" "00358406487542,00358407670038,00358400591125,0034633000140" "/opt/local/bin/xfear -bg_batch funcrun_invrun_start.p tms.pf #PARAM
" 1000 "1" yes
"InvoiceNumbering" 3 1 "Set external invoice ids" "sergey.strizhov@qvantel.com,toni.valve@qvantel.com" "00358406487542,00358407670038,00358400591125" "/opt/local/bin/xfear -bg_batch funcrun_invoice_extinvid.p tms.pf #PARAM" 10000 "1" yes
"PrintDOC1" 4 25 "Print invoices to a DOC1 file" "toni.valve@qvantel.com" "00358407670038,00358400591125" "/opt/local/bin/xfear -bg_batch funcrun_print_invoice.p tms.pf #PARAM" 1000 "1" yes
"InvPrintSplit" 5 1 "Split invoices into groups for printing" "sergey.strizhov@qvantel.com,toni.valve@qvantel.com" "00358406487542,00358407670038,00358400591125" "/opt/local/bin/xfear -bg_batch funcrun_invprint_split.p tms.pf #PARAM" 1000 "1" yes
"PrintXML" 6 25 "Print invoices to xml files" "sergey.strizhov@qvantel.com,toni.valve@qvantel.com" "00358406487542,00358407670038,00358400591125,0034633000140" "/opt/local/bin/xfear -bg_batch funcrun_print_invoice.p tms#HOST.pf #PARAM" 100 "1" yes
"CreateCSB19" 7 1 "Create direct debit file CSB19 from invoices " "sergey.strizhov@qvantel.com,toni.valve@qvantel.com" "00358406487542,00358407670038,00358400591125,0034633000140" "/opt/local/bin/xfear -bg_batch funcrun_create_ddfile.p tms.pf #PARAM" 1000 "1" yes
"BillingQualityRepI" 8 1 "Print a billing quality report (no details)" "toni.valve@qvantel.com,sergey.strizhov@qvantel.com" "00358406487542,00358407670038,00358400591125" "/opt/local/bin/xfear -bg_batch funcrun_billing_quality.p tms.pf #PARAM" 10000 "1" yes
"BillingQualityRepII" 9 1 "Print a billing quality report (with details)" "toni.valve@qvantel.com,sergey.strizhov@qvantel.com" "00358406487542,00358407670038,00358400591125" "/opt/local/bin/xfear -bg_batch funcrun_billing_quality.p tms.pf #PARAM" 10000 "1" yes
"CCReport" 10 1 "Print a CC report" "sergey.strizhov@qvantel.com,toni.valve@qvantel.com" "00358406487542,00358407670038,00358400591125" "/opt/local/bin/xfear -bg_batch funcrun_ccreport.p tms.pf #PARAM
" 10000 "1" yes
"DeleteTestInvoices" 11 1 "Delete test invoices before starting production run" "sergey.strizhov@qvantel.com,toni.valve@qvantel.com" "00358406487542,00358407670038,00358400591125" "/opt/local/bin/xfear -bg_batch funcrun_delete_testinv.p tms.pf #PARAM" 1000 "1" yes
"CombineDOC1Files" 12 1 "Combine DOC1 files created by subprocess into one transferrable file" "jaana.silvennoinen@qvantel.com,toni.valve@qvantel.com" "00358407670038,00358400591125" "/opt/local/bin/xfear -bg_batch funcrun_combine_doc1.p tms.pf #PARAM" 0 "1" yes
"BillRunStatistic" 13 1 "Statistics of billing run performance" "sergey.strizhov@qvantel.com,toni.valve@qvantel.com" "00358406487542,00358407670038,00358400591125" "/opt/local/bin/xfear -bg_batch funcrun_billrun_statistics.p tms.pf #PARAM" 1 "1" yes
"Bundle 1. month fee" 14 1 "Calculate 1. month fee for bundles" "sergey.strizhov@qvantel.com,toni.valve@qvantel.com" "00358406487542,00358407670038,00358400591125" "/opt/local/bin/xfear -bg_batch funcrun_bundle_first_month.p tms.pf #PARAM" 100 "1" yes
"BillRun_Counter" 15 25 "Counter version" "" "" "/opt/local/bin/xfear -bg_batch funcrun_invrun_startc.p tms_counter.pf #PARAM" 1000 "1" no
"LateBilledCalls" 16 1 "Generate Late Billed Calls Report" "sergey.strizhov@qvantel.com,toni.valve@qvantel.com" "00358406487542,00358407670038,00358400591125" "/opt/local/bin/xfear -bg_batch funcrun_late_billed_calls_report.p tms.pf #PARAM" 100 "1" yes
"UnbilledCalls" 17 1 "Generate Unbilled Calls Report" "sergey.strizhov@qvantel.com,toni.valve@qvantel.com" "00358406487542,00358407670038,00358400591125" "/opt/local/bin/xfear -bg_batch funcrun_unbilled_calls_report.p tms.pf #PARAM" 5000 "1" yes
"DoubleCall" 18 1 "Double mobcdr check for one day (billing day)" "sergey.strizhov@qvantel.com,toni.valve@qvantel.com" "00358406487542,00358407670038" "/opt/local/bin/xfear -bg_batch funcrun_mobcdr_double.p tms.pf #PARAM" 1 "1" yes
"TriggerRerate" 19 6 "Rerate handler for triggered events" "sergey.strizhov@qvantel.com,toni.valve@qvantel.com" "00358406487542,00358407670038" "/opt/local/bin/xfear -bg_batch funcrun_triggerrate.p tms.pf #PARAM" 100 "1" yes
"InvrowCounterCheck" 20 15 "Check unbilled invoice row counters against CDRs" "sergey.strizhov@qvantel.com,toni.valve@qvantel.com" "" "/opt/local/bin/xfear -bg_batch funcrun_chk_invrowcounter.p tms#HOST.pf #PARAM" 100 "1" yes
"IRCBilledCheck" 21 15 "Check billed invoice row counters against CDRs" "" "" "/opt/local/bin/xfear -bg_batch funcrun_chk_irc_billed.p tms#HOST.pf #PARAM" 100 "1" yes
"PrintXMLTar" 22 10 "Test invoice xmls" "" "" "/opt/local/bin/xfear -bg_batch /home/ari/test/funcrun_print_invoice.p tms#HOST.pf #PARAM" 100 "1" no
"TestCaseCollect" 23 1 "Collect billing run test cases" "sergey.strizhov@qvantel.com,toni.valve@qvantel.com" "00358406487542,00358407670038,00358400591125" "/opt/local/bin/xfear -bg_batch funcrun_brtestcase_collect.p tms.pf #PARAM" 100 "1" yes
"TestResultAnalysis" 24 1 "Analyse test results" "sergey.strizhov@qvantel.com,toni.valve@qvantel.com" "00358406487542,00358407670038,00358400591125" "/opt/local/bin/xfear -bg_batch funcrun_brtestresult_analysis.p tms.pf #PARAM" 100 "1" yes
"InvoiceSentToIFS" 25 1 "Send Invoices to IFS" "" "" "/opt/local/bin/xfear -bg_batch funcrun_ifs_invoice.p tms.pf #PARAM" 1000 "1" yes
"InvoicesReadyForIFS" 26 1 "Mark invoices to be sent to IFS
" "" "" "/opt/local/bin/xfear -bg_batch funcrun_invoice_deliverystate.p tms.pf #PARAM" 1000 "1" yes
"InvPrintSplitTest" 27 1 "Test invoice print split" "" "" "/opt/local/bin/xfear -bg_batch /home/ari/test/funcrun_invprint_split.p tms.pf #PARAM" 100 "1" no
"InvPrintSplitTestNP" 28 1 "Test invoice print split (no paper)" "" "" "/opt/local/bin/xfear -bg_batch /home/ari/test/funcrun_invprint_split.p tms.pf #PARAM" 100 "1" no
"IRCUnbilledSplit" 29 1 "Split subscriptions for unbilled invrowcounter check" "" "" "/opt/local/bin/xfear -bg_batch funcrun_invrowcounter_unbilled_split.p tms.pf #PARAM" 100 "1" yes
"InvPrintSplitNP" 30 1 "Split 'no paper' invoices into groups for printing" "sergey.strizhov@qvantel.com,toni.valve@qvantel.com" "00358406487542,00358407670038,00358400591125" "/opt/local/bin/xfear -bg_batch funcrun_invprint_split.p tms.pf #PARAM" 1000 "1" yes
"IRCBilledSplit" 31 1 "Split invoices for billed invrowcounter check" "" "" "/opt/local/bin/xfear -bg_batch funcrun_invrowcounter_billed_split.p tms.pf #PARAM" 100 "1" yes
"TerminalFinanceMove" 32 1 "Move terminal financing first month fees without answer from the bank to the future (out of the billing)" "sergey.strizhov@qvantel.com,toni.valve@qvantel.com" "00358406487542,00358407670038,00358400591125" "/opt/local/bin/xfear -bg_batch funcrun_terminal_financing_move.p tms.pf #PARAM" 100 "1" yes
"PrintXMLNoPaper" 33 25 "Print non-paper invoices to xml files" "sergey.strizhov@qvantel.com,toni.valve@qvantel.com" "00358406487542,00358407670038,00358400591125" "/opt/local/bin/xfear -bg_batch funcrun_print_invoice.p tms#HOST.pf #PARAM" 100 "1" yes
"FRInvXMLTarReport" 34 1 "Generates the log file regarding invoice xml tar file report" "" "" "/opt/local/bin/xfear -bg_batch funcrun_invoice_xml_tar_report.p tms.pf #PARAM" 1000 "1" yes
