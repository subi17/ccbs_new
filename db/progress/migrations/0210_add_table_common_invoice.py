from gearbox.migrations import Migration

class AddTableInvoice(Migration):

    database = "common"

    def up(self):
        t = self.table('Invoice', area="Dyn_Data_32", multitenant="yes", label="Invoice", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-invoice.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-invoice.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="invoice", desc="Invoice")
        t.column('InvNum', 'integer', format="zzzzzzzz9", initial="0", max_width=4, label="InvNo", column_label="InvNo", position=2, order=10, help="Consecutive Invoice Number, 1 ... 99999999")
        t.column('InvDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="InvDate", column_label="InvDate", position=3, order=20, help="Invoice's date")
        t.column('CustNum', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=4, order=30, help="Customer being billed")
        t.column('DueDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="DueDate", column_label="DueDate", position=5, order=40, help="Invoice's dueday")
        t.column('AmtExclVAT', 'decimal', format="-z,zzz,zz9.99", decimals=3, initial="0", max_width=18, label="Total Excl.VAT", column_label="TotExVAT", position=6, order=50, help="Billed total value ex VAT")
        t.column('VATAmt', 'decimal', format="-zzz,zzz.99", decimals=3, initial="0", help="Amount of VAT", max_width=18, label="VAT", column_label="VAT", position=7, order=60, description="Total, details in VatAmount")
        t.column('InvAmt', 'decimal', format="-zzz,zz9.99", decimals=3, initial="0", max_width=18, label="To pay", column_label="To pay", position=8, order=70, help="Total payable")
        t.column('PrintState', 'integer', format="9", initial="0", max_width=4, label="S", column_label="S", position=9, order=80, help="Invoice status 0: not printed yet 1:printed")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Calls from", column_label="Calls from", position=10, order=90, help="Date of Invoice's earliest call ")
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="LastCall", column_label="LastCall", position=11, order=100, help="Discount's previous call date")
        t.column('FirstCall', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="FirstCall", column_label="FirstCall", position=12, order=110, help="Invoice's first call date")
        t.column('Rounding', 'decimal', format="9.99-", decimals=2, initial="0", max_width=17, label="Round", column_label="Round", position=13, order=120, help="Rounding")
        t.column('xxVATPerc', 'decimal', format="z9.99", decimals=2, initial="0", max_width=17, label="VAT%", column_label="VAT%", position=14, order=130, help="VAT's - percent in the invoice")
        t.column('CustName', 'character', format="x(30)", initial="", max_width=60, label="CustName", column_label="CustName", position=15, order=132, help="Customer's name")
        t.column('Address', 'character', format="x(30)", initial="", max_width=60, label="address", column_label="address", position=16, order=140, help="Customer's address")
        t.column('PostOffice', 'character', format="x(30)", initial="", max_width=60, label="Postadress", column_label="Postadress", position=17, order=150, help="Customer's post number + post address")
        t.column('InvType', 'integer', format="9", initial="1", max_width=4, label="InvoiceType", column_label="InvoiceType", position=18, order=160, help="1 = Normal, 2 = Interest invoice")
        t.column('PaymState', 'integer', format="9", initial="0", max_width=4, label="Pstatus", column_label="Pstatus", position=19, order=170, help="Status of Payment 0:Unpaid 1:PartPaid 2:Paid")
        t.column('ClaimPerm', 'logical', format="Yes/No", initial="yes", max_width=1, label="ClaimAllow", column_label="ClAllow", position=20, order=180, help="Send a payment reminder of the invoice (Y/N)")
        t.column('InterestPerm', 'logical', format="Yes/No", initial="yes", max_width=1, label="IntrAllow", column_label="IntrAll", position=21, order=191, help="Charge an overtime interest for the invoice (Y/N)")
        t.column('ITGroupID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Invoice Target Group", column_label="ITGroup", position=22, order=201, help="Invoice target group ID")
        t.column('OPAccNum', 'integer', format=">>>>>9", initial="0", max_width=4, label="OverPayment Acc", column_label="OP Acc", position=23, order=211, help="Overpayment account")
        t.column('CashDisc', 'decimal', format="ZZZ9.99-", decimals=2, initial="0", max_width=17, label="CashDisc", column_label="CashDisc", position=24, order=231, help="Allowed (conditional) cash discount")
        t.column('PaidAmt', 'decimal', format="ZZZZZZ9.99-", decimals=2, initial="0", max_width=17, label="PaidAmt", column_label="Paid", position=25, order=240, help="Total payment this far")
        t.column('OverPaym', 'decimal', format="ZZZZZZ9.99-", decimals=2, initial="0", max_width=17, label="OverPayment", column_label="OverPaym", position=26, order=250, help="Overpayment")
        t.column('PaymDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="PrevPayInDay", column_label="PrevPay", position=27, order=190, help="Invoice's previous payment day")
        t.column('ClaimDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="LastRem", column_label="LastRem", position=28, order=200, help="Date when latest reminder was sent to customer")
        t.column('EarliestRem', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="EarlRem", column_label="EarlRem", position=29, order=210, help="Date when first reminder could send ")
        t.column('CashDiscDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="CashDisc", column_label="CashDisc", position=31, order=230, help="Due day for payments with decrement of cash discount")
        t.column('ARAccNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Fkonto", column_label="Fkonto", position=32, order=260, help="Acct No. for receivables")
        t.column('EndInvoice', 'integer', format="9", initial="0", max_width=4, label="End Invoice", column_label="End Inv", position=33, order=270, help="End invoice")
        t.column('RoundAccNum', 'integer', format=">>>>>9", initial="0", max_width=4, label="Uaccount", column_label="Uaccount", position=34, order=280, help="Acct No for rounding")
        t.column('InterestAmt', 'decimal', format="ZZZZZ9.99-", decimals=2, initial="0", max_width=17, label="Interest", column_label="Interest", position=35, order=290, help="Billed interest, from earlier payments")
        t.column('IntAccNum', 'integer', format=">>>>>9", initial="0", max_width=4, label="Interest", column_label="Interest", position=36, order=300, help="Acct no of invoiced overtime interest")
        t.column('TransFile', 'integer', format=">>>>>>9", initial="0", max_width=4, label="Xfer file", column_label="Xfer file", position=37, order=310, help="Consecutive number of transfer file (for bookkeeping)")
        t.column('InvCfg', 'logical', format="Yes/No", initial="no", max_width=40, label="Special codes", column_label="Special codes", extent=5, position=38, order=320, help="Special logical codes for invoices")
        t.column('xxMemo', 'character', format="x(60)", initial="", max_width=610, label="Memo", column_label="Memo", extent=5, position=39, order=330, help="Explanation / memory field for invoice")
        t.column('CrInvNum', 'integer', format="zzzzzzzz9", initial="0", max_width=4, label="Credit InvNo", column_label="Credit InvNo", position=40, order=340, help="Credit invoice number")
        t.column('WInvDisp', 'logical', format="Yes/No", initial="Yes", max_width=1, label="Web", column_label="Web", position=41, order=350, help="Allow displaying in WEB Invoice Browser")
        t.column('Currency', 'character', format="x(5)", initial="", max_width=10, label="Code", column_label="Code", position=42, order=490, help="Currency code")
        t.column('VATUsage', 'integer', format="9", initial="1", max_width=4, label="VAT Usage", column_label="VAT", position=43, order=480, help="How VAT is calculated for this invoice")
        t.column('ExchRate', 'decimal', format=">>9.999999", decimals=5, initial="0", max_width=20, label="Rate", column_label="Rate", position=44, order=500, help="Currency rate")
        t.column('ChgStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Date and Time When Invoice Record was last changed", max_width=20, label="Created", column_label="Created", position=45, order=900, description="Time Stamp yyyymmdd.time (sec)")
        t.column('ExpStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Date and Time when this record was last exported", max_width=20, label="Xferred", column_label="Xferred", position=46, order=910, description="Time Stamp yyyymmdd.time (sec)")
        t.column('CurrAmt', 'decimal', format="->,>>>,>>9.99", decimals=2, initial="0", max_width=17, label="Net value in currency", column_label="Currency net", position=47, order=510, help="Invoices NET value in used currency")
        t.column('AdvPaym', 'decimal', format=">,>>>,>>9.99", decimals=2, initial="0", max_width=17, label="Advance payment", column_label="Advanve payment", position=48, order=520, help="Value of advance payment used in this invoice")
        t.column('APAccNum', 'integer', format=">>>>>9", initial="0", max_width=4, label="OverPAccount", column_label="OverPAccount", position=49, order=920, help="Account number for over payments")
        t.column('VATPercent', 'decimal', format="z9.99", decimals=2, initial="0", max_width=120, label="VAT%", column_label="VAT%", extent=10, position=51, order=940, help="VAT Percentage (%)")
        t.column('VATAmount', 'decimal', format="->>>,>>9.99", decimals=3, initial="0", max_width=240, label="Vat Amount", column_label="VatAmt", extent=10, position=52, order=950, help="Amount of VAT")
        t.column('VATAccount', 'integer', format=">>>>>9", initial="0", max_width=140, label="VatAcct", column_label="VatAcct", extent=10, position=53, order=960, help="Account number of VAT")
        t.column('ClaimBatch', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Batch nbr for claiming", column_label="ClaimBatch", position=54, order=970, help="Batch nbr for transferring invoice to claiming")
        t.column('InvSeq', 'integer', format=">>>>>>9", initial="0", max_width=4, label="InvSeq", column_label="InvSeq", position=55, order=610, help="Calls invoice sequence")
        t.column('DiscPerc', 'decimal', format=">9.99", decimals=2, initial="0", max_width=17, label="Dir.Disc.", column_label="Dir.Disc.", position=56, order=1030, help="Customers direct discount percentage, reduced from invoice tota")
        t.column('DirDisc', 'decimal', format="-zzz,zzz.99", decimals=2, initial="0", max_width=17, label="Dir.Disc.", column_label="Dir.Disc.", position=57, order=1040, help="Value of direct discount")
        t.column('ClaimStamp', 'decimal', format="99999999.99999", decimals=5, initial=self.unknown, max_width=20, label="Claimed", column_label="Claimed", position=60, order=1050, help="Date and time when invoice is claimed")
        t.column('CancelStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Cancelled", column_label="Cancelled", position=61, order=1060, help="Date and time when invoice's claiming is cancelled")
        t.column('VATIncl', 'logical', format="Incl/Excl", initial="yes", max_width=1, label="VAT", column_label="VAT", position=62, order=1070, help="Is VAT Included/Excluded in line amounts")
        t.column('VATBasis', 'decimal', format="->>,>>9.99", decimals=3, initial="0", max_width=220, label="VATBasis", column_label="VATBasis", extent=10, position=63, order=1080, help="Base sum for VAT")
        t.column('ClaimCancel', 'integer', format=">9", initial="0", help="Reason for cancelling claiming process", max_width=4, label="Claim Cancel", column_label="Cl.Cancel", position=64, order=1090, description="Codes in TMSCodes")
        t.column('DDBankAcc', 'character', format="x(30)", initial="", max_width=60, label="Bank Account", column_label="Bank", position=65, order=1100, help="Payer's current bank account when using direct debiting")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=66, order=1110, help="Code Of Brand")
        t.column('EPaymAmt', 'decimal', format="->>>>>>>9.99", decimals=2, initial="0", max_width=17, label="EPayment Amount", column_label="EPaymAmt", position=67, order=1160, help="Amount of ePayment")
        t.column('ChargeType', 'integer', format="9", initial="0", max_width=4, label="Charge Type", column_label="Charge", position=68, order=1120, help="Charge type")
        t.column('DelType', 'integer', format="9", initial="0", max_width=4, label="Delivery Type", column_label="Deliv.", position=69, order=1130, help="Invoice's delivery type")
        t.column('DDState', 'integer', format="9", initial="0", max_width=4, label="DD Status", column_label="DDStat", position=70, order=1140, help="Direct Debit status")
        t.column('EPaymDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="EPayment Date", column_label="EPaymDate", position=71, order=1150, help="Date when ePayment was made")
        t.column('SpecDel', 'integer', format="9", initial="0", max_width=4, label="Specification Delivery", column_label="Spec Del.", position=72, order=1170, help="Specification report delivery type")
        t.column('BillRun', 'character', format="x(15)", initial="", max_width=30, label="Billing Run", column_label="BillRun", position=76, order=1290, help="Billing run ID")
        t.column('IDelAddr', 'character', format="x(30)", initial="", max_width=60, label="Delivery Address", column_label="Del.Address", position=77, order=1230, help="Invoice delivery address")
        t.column('IDelCountry', 'character', format="x(30)", initial="", max_width=60, label="Delivery Country", column_label="Del.Country", position=78, order=1260, help="Invoice delivery country")
        t.column('IDelName', 'character', format="x(30)", initial="", max_width=60, label="Delivery Name", column_label="Del.Name", position=79, order=1220, help="Invoice delivery name")
        t.column('IDelPost', 'character', format="x(30)", initial="", max_width=60, label="Delivery Post Office", column_label="Del.Post", position=80, order=1250, help="Invoice delivery post office")
        t.column('IDelZipCode', 'character', format="x(8)", initial="", max_width=16, label="Delivery Zip Code", column_label="Del.ZipCode", position=81, order=1240, help="Invoice delivery zip code")
        t.column('IDelCOName', 'character', format="x(30)", initial="", max_width=60, label="Delivery Add.Name", column_label="Del.AName", position=82, order=1270, help="Additional invoice delivery name")
        t.column('CoName', 'character', format="x(30)", initial="", max_width=60, label="Addt'l name", column_label="Addt'l name", position=83, order=1280, help="Additional name of a customer")
        t.column('DDFile', 'character', format="x(40)", initial="", max_width=80, label="Direct Debit File", column_label="DD File", position=84, order=1300, help="Name of the direct debit file (sent to bank)")
        t.column('ClaimState', 'decimal', format=">9.99", decimals=2, initial="0", max_width=17, label="Claiming Status", column_label="Claimed", position=85, order=1310, help="Claiming status")
        t.column('ExtInvID', 'character', format="x(12)", initial="", help="External invoice ID", max_width=24, label="External Invoice ID", column_label="Ext.ID", position=86, order=1320, description='''

''')
        t.column('RefNum', 'character', format="x(20)", initial="", max_width=40, label="Reference Nbr", column_label="Reference", position=87, order=1330, help="Reference number")
        t.column('DeliveryState', 'integer', format=">9", initial="0", max_width=4, label="Delivery State", column_label="Del.State", position=88, order=1340, help="Delivery state")
        t.column('TaxZone', 'character', format="x(8)", initial="", max_width=16, label="Tax Zone", column_label="Zone", position=89, order=1350, help="Tax Zone")
        t.column('FirstName', 'character', format="x(20)", initial="", help="Customer's fore/given name", max_width=40, label="ForeName", column_label="ForeName", position=90, order=1360, description="Customer's forename")
        t.column('SurName2', 'character', format="x(20)", initial="", help="Customer's 2nd last name", max_width=40, label="SurName2", column_label="SurName2", position=91, order=1370, description="Customer's 2nd last name")
        t.column('Region', 'character', format="x(8)", initial="", max_width=16, label="Region", position=92, order=1380, help="Region code")
        t.column('CLI', 'character', format="x(15)", initial="", max_width=30, label="MSISDN", position=93, order=1390, help="MSISDN")
        t.column('MsSeq', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Subscription ID", column_label="MobSub", position=94, order=1400, help="Mobile subscription ID")
        t.column('SurName1', 'character', format="x(20)", initial="", help="Customer's first last name", max_width=40, label="Surname 1", column_label="Surname1", position=95, order=1410, description='''
''')
        t.column('CreditReason', 'character', format="x(8)", initial="", help="Crediting reason", max_width=16, label="Crediting Reason", column_label="Cr.Reason", position=96, order=1420, description='''
''')
        t.column('InvGroup', 'character', format="x(12)", initial="", help="Invoice group", max_width=24, label="InvGroup", position=97, order=1430, description='''

''')
        t.column('AgrCust', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Agreement Customer", column_label="Agr.Cust", position=98, order=1440, help="Agreement customer")
        t.column('ClaimStatus', 'character', format="X(8)", initial="", max_width=16, label="Claim Status", column_label="ClaimStatus", position=99, order=1450, help="Claiming Status (new format)")
        t.column('MandateId', 'character', format="X(35)", initial="", max_width=70, label="Mandate Id", column_label="MandateId", position=100, order=1460, help="Mandate ID for CSB19.14 file")
        t.index('InvNum_s', [['InvNum']], area="Dyn_Index_1", primary=True, unique=True)
        t.index('CLI', [['Brand'], ['CLI']], area="Dyn_Index_1")
        t.index('CustNum', [['Brand'], ['CustNum'], ['InvDate', 'DESC']], area="Dyn_Index_1")
        t.index('ExtInvID', [['Brand'], ['ExtInvID']], area="Dyn_Index_1")
        t.index('InvAmt', [['Brand'], ['PaymState'], ['InvAmt'], ['InvDate', 'DESC']], area="Dyn_Index_1")
        t.index('InvDate', [['Brand'], ['InvDate', 'DESC'], ['DelType']], area="Dyn_Index_1")
        t.index('InvNum', [['Brand'], ['InvNum']], area="Dyn_Index_1")
        t.index('InvType', [['Brand'], ['InvType'], ['InvDate', 'DESC']], area="Dyn_Index_1")

    def down(self):
        self.drop_table('Invoice')
