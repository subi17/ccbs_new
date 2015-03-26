from gearbox.migrations import Migration

class AddInvoice(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Invoice', area='Dyn_Data_32',
                       label='Invoice',
                       dump_name='invoice',
                       desc='Invoice')
        t.column('InvNum', 'integer', format='zzzzzzz9', initial='0',
                 label='InvNo',
                 column_label='InvNo',
                 help='Consecutive Invoice Number, 1 ... 99999999')
        t.column('InvDate', 'date', format='99-99-99',
                 column_label='InvDate',
                 help='Invoice\'s date')
        t.column('CustNum', 'integer', format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer being billed')
        t.column('DueDate', 'date', format='99-99-99',
                 column_label='DueDate',
                 help='Invoice\'s dueday')
        t.column('AmtExclVAT', 'decimal', decimals=3, format='-z,zzz,zz9.99', initial='0',
                 label='Total Excl.VAT',
                 column_label='TotExVAT',
                 help='Billed total value ex VAT')
        t.column('VATAmt', 'decimal', decimals=3, format='-zzz,zzz.99', initial='0',
                 label='VAT',
                 column_label='VAT',
                 help='Amount of VAT',
                 description='Total, details in VatAmount')
        t.column('InvAmt', 'decimal', decimals=3, format='-zzz,zz9.99', initial='0',
                 label='To pay',
                 column_label='To pay',
                 help='Total payable')
        t.column('PrintState', 'integer', format='9', initial='0',
                 label='S',
                 column_label='S',
                 help='Invoice status 0: not printed yet 1:printed')
        t.column('FromDate', 'date', format='99-99-99',
                 label='Calls from',
                 column_label='Calls from',
                 help='Date of Invoice\'s earliest call ')
        t.column('ToDate', 'date', format='99-99-99',
                 label='LastCall',
                 column_label='LastCall',
                 help='Discount\'s previous call date')
        t.column('FirstCall', 'date', format='99-99-99',
                 column_label='FirstCall',
                 help='Invoice\'s first call date')
        t.column('Rounding', 'decimal', decimals=2, format='9.99-', initial='0',
                 label='Round',
                 column_label='Round',
                 help='Rounding')
        t.column('xxVATPerc', 'decimal', decimals=2, format='z9.99', initial='0',
                 label='VAT%',
                 column_label='VAT%',
                 help='VAT\'s - percent in the invoice')
        t.column('CustName', 'character', format='x(30)', initial='',
                 column_label='CustName',
                 help='Customer\'s name')
        t.column('Address', 'character', format='x(30)', initial='',
                 label='address',
                 column_label='address',
                 help='Customer\'s address')
        t.column('PostOffice', 'character', format='x(30)', initial='',
                 label='Postadress',
                 column_label='Postadress',
                 help='Customer\'s post number + post address')
        t.column('InvType', 'integer', format='9', initial='1',
                 label='InvoiceType',
                 column_label='InvoiceType',
                 help='1 = Normal, 2 = Interest invoice')
        t.column('PaymState', 'integer', format='9', initial='0',
                 label='Pstatus',
                 column_label='Pstatus',
                 help='Status of Payment 0:Unpaid 1:PartPaid 2:Paid')
        t.column('ClaimPerm', 'logical', initial='yes',
                 label='ClaimAllow',
                 column_label='ClAllow',
                 help='Send a payment reminder of the invoice (Y/N)')
        t.column('PaymDate', 'date', format='99-99-99',
                 label='PrevPayInDay',
                 column_label='PrevPay',
                 help='Invoice\'s previous payment day')
        t.column('InterestPerm', 'logical', initial='yes',
                 label='IntrAllow',
                 column_label='IntrAll',
                 help='Charge an overtime interest for the invoice (Y/N)')
        t.column('ClaimDate', 'date', format='99-99-99',
                 label='LastRem',
                 column_label='LastRem',
                 help='Date when latest reminder was sent to customer')
        t.column('ClaimQty', 'integer', format='Z9', initial='0',
                 label='AmtRem',
                 column_label='matRem',
                 help='How many reminders have sent to customer of this invoice')
        t.column('EarliestRem', 'date', format='99-99-99',
                 label='EarlRem',
                 column_label='EarlRem',
                 help='Date when first reminder could send ')
        t.column('OPAccNum', 'integer', format='>>>>>9', initial='0',
                 label='OverPayment Acc',
                 column_label='OP Acc',
                 help='Overpayment account')
        t.column('CashDiscDate', 'date', format='99-99-99',
                 label='CashDisc',
                 column_label='CashDisc',
                 help='Due day for payments with decrement of cash discount')
        t.column('CashDisc', 'decimal', decimals=2, format='ZZZ9.99-', initial='0',
                 column_label='CashDisc',
                 help='Allowed (conditional) cash discount')
        t.column('PaidAmt', 'decimal', decimals=2, format='ZZZZZZ9.99-', initial='0',
                 column_label='Paid',
                 help='Total payment this far')
        t.column('OverPaym', 'decimal', decimals=2, format='ZZZZZZ9.99-', initial='0',
                 label='OverPayment',
                 column_label='OverPaym',
                 help='Overpayment')
        t.column('ARAccNum', 'integer', format='>>>>>9', initial='0',
                 label='Fkonto',
                 column_label='Fkonto',
                 help='Acct No. for receivables')
        t.column('EndInvoice', 'integer', format='9', initial='0',
                 label='End Invoice',
                 column_label='End Inv',
                 help='End invoice')
        t.column('RoundAccNum', 'integer', format='>>>>>9', initial='0',
                 label='Uaccount',
                 column_label='Uaccount',
                 help='Acct No for rounding')
        t.column('InterestAmt', 'decimal', decimals=2, format='ZZZZZ9.99-', initial='0',
                 label='Interest',
                 column_label='Interest',
                 help='Billed interest, from earlier payments')
        t.column('IntAccNum', 'integer', format='>>>>>9', initial='0',
                 label='Interest',
                 column_label='Interest',
                 help='Acct no of invoiced overtime interest')
        t.column('TransFile', 'integer', format='>>>>>>9', initial='0',
                 label='Xfer file',
                 column_label='Xfer file',
                 help='Consecutive number of transfer file (for bookkeeping)')
        t.column('InvCfg', 'logical', extent=5, initial='no',
                 label='Special codes',
                 column_label='Special codes',
                 help='Special logical codes for invoices')
        t.column('xxMemo', 'character', extent=5, format='x(60)', initial='',
                 label='Memo',
                 column_label='Memo',
                 help='Explanation / memory field for invoice')
        t.column('CrInvNum', 'integer', format='zzzzzzz9', initial='0',
                 label='Credit InvNo',
                 column_label='Credit InvNo',
                 help='Credit invoice number')
        t.column('WInvDisp', 'logical', initial='Yes',
                 label='Web',
                 column_label='Web',
                 help='Allow displaying in WEB Invoice Browser')
        t.column('VATUsage', 'integer', format='9', initial='1',
                 label='VAT Usage',
                 column_label='VAT',
                 help='How VAT is calculated for this invoice')
        t.column('Currency', 'character', format='x(5)', initial='',
                 label='Code',
                 column_label='Code',
                 help='Currency code')
        t.column('ExchRate', 'decimal', decimals=5, format='>>9.999999', initial='0',
                 label='Rate',
                 column_label='Rate',
                 help='Currency rate')
        t.column('CurrAmt', 'decimal', decimals=2, format='->,>>>,>>9.99', initial='0',
                 label='Net value in currency',
                 column_label='Currency net',
                 help='Invoices NET value in used currency')
        t.column('AdvPaym', 'decimal', decimals=2, format='>,>>>,>>9.99', initial='0',
                 label='Advance payment',
                 column_label='Advanve payment',
                 help='Value of advance payment used in this invoice')
        t.column('InvSeq', 'integer', format='>>>>>>9', initial='0',
                 column_label='InvSeq',
                 help='Calls invoice sequence')
        t.column('ChgStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Created',
                 column_label='Created',
                 help='Date and Time When Invoice Record was last changed',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.column('ExpStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Xferred',
                 column_label='Xferred',
                 help='Date and Time when this record was last exported',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.column('APAccNum', 'integer', format='>>>>>9', initial='0',
                 label='OverPAccount',
                 column_label='OverPAccount',
                 help='Account number for over payments')
        t.column('VATPercent', 'decimal', extent=10, decimals=2, format='z9.99', initial='0',
                 label='VAT%',
                 column_label='VAT%',
                 help='VAT Percentage (%)')
        t.column('VATAmount', 'decimal', extent=10, decimals=3, format='->>>,>>9.99', initial='0',
                 label='Vat Amount',
                 column_label='VatAmt',
                 help='Amount of VAT')
        t.column('VATAccount', 'integer', extent=10, format='>>>>>9', initial='0',
                 label='VatAcct',
                 column_label='VatAcct',
                 help='Account number of VAT')
        t.column('ClaimBatch', 'integer', format='>>>>>>>9', initial='0',
                 label='Batch nbr for claiming',
                 column_label='ClaimBatch',
                 help='Batch nbr for transferring invoice to claiming')
        t.column('DiscPerc', 'decimal', decimals=2, format='>9.99', initial='0',
                 label='Dir.Disc.',
                 column_label='Dir.Disc.',
                 help='Customers direct discount percentage, reduced from invoice tota')
        t.column('DirDisc', 'decimal', decimals=2, format='-zzz,zzz.99', initial='0',
                 label='Dir.Disc.',
                 column_label='Dir.Disc.',
                 help='Value of direct discount')
        t.column('ClaimStamp', 'decimal', decimals=5, format='99999999.99999',
                 label='Claimed',
                 column_label='Claimed',
                 help='Date and time when invoice is claimed')
        t.column('CancelStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Cancelled',
                 column_label='Cancelled',
                 help='Date and time when invoice\'s claiming is cancelled')
        t.column('VATIncl', 'logical', format='Incl/Excl', initial='yes',
                 label='VAT',
                 column_label='VAT',
                 help='Is VAT Included/Excluded in line amounts')
        t.column('VATBasis', 'decimal', extent=10, decimals=3, initial='0',
                 column_label='VATBasis',
                 help='Base sum for VAT')
        t.column('ClaimCancel', 'integer', format='>9', initial='0',
                 label='Claim Cancel',
                 column_label='Cl.Cancel',
                 help='Reason for cancelling claiming process',
                 description='Codes in TMSCodes')
        t.column('DDBankAcc', 'character', format='x(30)', initial='',
                 label='Bank Account',
                 column_label='Bank',
                 help='Payer\'s current bank account when using direct debiting')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('ChargeType', 'integer', format='9', initial='0',
                 label='Charge Type',
                 column_label='Charge',
                 help='Charge type')
        t.column('DelType', 'integer', format='9', initial='0',
                 label='Delivery Type',
                 column_label='Deliv.',
                 help='Invoice\'s delivery type')
        t.column('DDState', 'integer', format='9', initial='0',
                 label='DD Status',
                 column_label='DDStat',
                 help='Direct Debit status')
        t.column('EPaymDate', 'date', format='99-99-99',
                 label='EPayment Date',
                 column_label='EPaymDate',
                 help='Date when ePayment was made')
        t.column('EPaymAmt', 'decimal', decimals=2, format='->>>>>>>9.99', initial='0',
                 label='EPayment Amount',
                 column_label='EPaymAmt',
                 help='Amount of ePayment')
        t.column('SpecDel', 'integer', format='9', initial='0',
                 label='Specification Delivery',
                 column_label='Spec Del.',
                 help='Specification report delivery type')
        t.column('IDelName', 'character', format='x(30)', initial='',
                 label='Delivery Name',
                 column_label='Del.Name',
                 help='Invoice delivery name')
        t.column('IDelAddr', 'character', format='x(30)', initial='',
                 label='Delivery Address',
                 column_label='Del.Address',
                 help='Invoice delivery address')
        t.column('IDelZipCode', 'character', initial='',
                 label='Delivery Zip Code',
                 column_label='Del.ZipCode',
                 help='Invoice delivery zip code')
        t.column('IDelPost', 'character', format='x(30)', initial='',
                 label='Delivery Post Office',
                 column_label='Del.Post',
                 help='Invoice delivery post office')
        t.column('IDelCountry', 'character', format='x(30)', initial='',
                 label='Delivery Country',
                 column_label='Del.Country',
                 help='Invoice delivery country')
        t.column('IDelCOName', 'character', format='x(30)', initial='',
                 label='Delivery Add.Name',
                 column_label='Del.AName',
                 help='Additional invoice delivery name')
        t.column('CoName', 'character', format='x(30)', initial='',
                 label='Addt\'l name',
                 column_label='Addt\'l name',
                 help='Additional name of a customer')
        t.column('BillRun', 'character', format='x(15)', initial='',
                 label='Billing Run',
                 column_label='BillRun',
                 help='Billing run ID')
        t.column('DDFile', 'character', format='x(40)', initial='',
                 label='Direct Debit File',
                 column_label='DD File',
                 help='Name of the direct debit file (sent to bank)')
        t.column('ClaimState', 'decimal', decimals=2, format='>9.99', initial='0',
                 label='Claiming Status',
                 column_label='Claimed',
                 help='Claiming status')
        t.column('ExtInvID', 'character', format='x(12)', initial='',
                 label='External Invoice ID',
                 column_label='Ext.ID',
                 help='External invoice ID')
        t.column('RefNum', 'character', format='x(20)', initial='',
                 label='Reference Nbr',
                 column_label='Reference',
                 help='Reference number')
        t.column('DeliveryState', 'integer', format='>9', initial='0',
                 label='Delivery State',
                 column_label='Del.State',
                 help='Delivery state')
        t.column('TaxZone', 'character', initial='',
                 label='Tax Zone',
                 column_label='Zone')
        t.column('FirstName', 'character', format='x(20)', initial='',
                 label='ForeName',
                 column_label='ForeName',
                 help='Customer\'s fore/given name',
                 description='Customer\'s forename')
        t.column('SurName2', 'character', format='x(20)', initial='',
                 column_label='SurName2',
                 help='Customer\'s 2nd last name',
                 description='Customer\'s 2nd last name')
        t.column('Region', 'character', initial='',
                 help='Region code')
        t.column('CLI', 'character', format='x(15)', initial='',
                 label='MSISDN')
        t.column('MsSeq', 'integer', format='>>>>>>>>9', initial='0',
                 label='Subscription ID',
                 column_label='MobSub',
                 help='Mobile subscription ID')
        t.column('SurName1', 'character', format='x(20)', initial='',
                 label='Surname 1',
                 column_label='Surname1',
                 help='Customer\'s first last name')
        t.column('CreditReason', 'character', initial='',
                 label='Crediting Reason',
                 column_label='Cr.Reason',
                 help='Crediting reason')
        t.column('InvGroup', 'character', format='x(12)', initial='',
                 help='Invoice group')
        t.index('CLI', ['Brand', 'CLI'], area='Dyn_Index_1')
        t.index('CustNum', ['Brand', 'CustNum', ('InvDate', 'DESCENDING')], area='Dyn_Index_1')
        t.index('ExtInvID', ['Brand', 'ExtInvID'], area='Dyn_Index_1')
        t.index('InvAmt', ['Brand', 'PaymState', 'InvAmt', ('InvDate', 'DESCENDING')], area='Dyn_Index_1')
        t.index('InvDate', ['Brand', ('InvDate', 'DESCENDING'), 'DelType'], area='Dyn_Index_1')
        t.index('InvNum', ['Brand', 'InvNum'], area='Dyn_Index_1')
        t.index('InvNum_s', ['InvNum'], area='Dyn_Index_1',
                primary=True, unique=True)
        t.index('InvType', ['Brand', 'InvType', ('InvDate', 'DESCENDING')], area='Dyn_Index_1')

    def down(self):
        self.drop_table('Invoice')

