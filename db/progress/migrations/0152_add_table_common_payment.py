from gearbox.migrations import Migration

class AddPayment(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Payment', area='Dyn_Data_32',
                       label='Payments',
                       dump_name='payment',
                       desc='Payments')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer number, 1 - 999999')
        t.column('CustName', 'character', format='x(30)', initial='',
                 column_label='Customer name',
                 help='Customer\'s name')
        t.column('InvNum', 'integer', format='zzzzzzz9', initial='0',
                 label='InvoiceNo',
                 column_label='InvoiceNo',
                 help='Invoice\'s number')
        t.column('InvAmt', 'decimal', decimals=2, format='>>>>>>9.99-', initial='0',
                 label='Invoice Amount',
                 column_label='Invoice Amt',
                 help='Invoice\'s amount (payable)')
        t.column('PaymAmt', 'decimal', decimals=2, format='ZZZZZZ9.99-', initial='0',
                 label='Payment',
                 column_label='Payment')
        t.column('TotAmt', 'decimal', decimals=2, format='ZZZZZZ9.99-', initial='0',
                 label='Total',
                 column_label='Total',
                 help='Total payment ')
        t.column('Discount', 'decimal', decimals=2, format='ZZZ9.99-', initial='0',
                 column_label='Discount',
                 help='Total discount with in this payment ')
        t.column('InvDate', 'date', format='99-99-99',
                 label='Invoice date',
                 column_label='Invoice date')
        t.column('DueDate', 'date', format='99-99-99',
                 label='DueDay',
                 column_label='DueDay',
                 help='Invoice\'s due day')
        t.column('PaymDate', 'date', format='99-99-99',
                 label='Payment Date',
                 column_label='Payment Date',
                 help='Date of Invoice Payment')
        t.column('AccNum', 'integer', extent=10, format='>>>>>9', initial='0',
                 label='AcctNo',
                 column_label='AcctNo',
                 help='Booking account\'s number')
        t.column('Posting', 'decimal', extent=10, decimals=2, format='ZZZZZZ9.99-', initial='0',
                 label='Amt',
                 column_label='Amt',
                 help='Booked amount')
        t.column('Voucher', 'integer', format='ZZZZZZ9', initial='0',
                 label='VoucherNo',
                 column_label='VoucherNo',
                 help='Voucher number of payment')
        t.column('AccDate', 'date', format='99-99-99',
                 label='BookDate',
                 column_label='BookDate',
                 help='Date, when payment has been registered in bank')
        t.column('xxMemo', 'character', extent=5, format='x(60)', initial='',
                 label='Memo',
                 column_label='Memo',
                 help='Explanation / memory field for payment')
        t.column('AccType', 'integer', extent=10, format='>9', initial='0',
                 label='Type',
                 column_label='Type',
                 help='Account usage type number')
        t.column('PaymFile', 'character', initial='',
                 label='File',
                 column_label='File',
                 help='Name of input file')
        t.column('PaymArc', 'character', format='x(30)', initial='',
                 label='ArchiveId',
                 column_label='ArcId',
                 help='Archive id')
        t.column('PaymSrc', 'character', initial='',
                 label='Payment source',
                 column_label='Source',
                 help='Source of payment',
                 description='empty=manual, dd=direct debit, rf=reference file, ca=collection agency')
        t.column('ImportStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Imported',
                 column_label='Imported',
                 help='Date and time when payment is imported into TMS')
        t.column('ClaimStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Claimed',
                 column_label='Claimed',
                 help='Date and time when payment is claimed or claiming cancelled')
        t.column('PaymType', 'integer', format='>9', initial='0',
                 label='Payment Type',
                 column_label='PaymType',
                 help='Payment type')
        t.column('ExchRate', 'decimal', decimals=6, format='>>9.999999', initial='0',
                 label='Exchange Rate',
                 column_label='Rate',
                 help='Currency exchange rate')
        t.column('BankAcc', 'character', format='x(20)', initial='',
                 label='Bank Acc.',
                 column_label='Bank Acc.',
                 help='Bank account number')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('PPlanID', 'integer', format='>>>>>>>9', initial='0',
                 label='Payment Plan ID',
                 column_label='PP ID',
                 help='Payment plan ID')
        t.column('PPBatch', 'integer', format='>9', initial='0',
                 label='Payment Plan Batch',
                 column_label='PP Batch',
                 help='Payment plan batch number')
        t.column('ExpStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Imported',
                 column_label='Imported',
                 help='Date and time when payment is imported into TMS')
        t.column('ExpUser', 'character', initial='',
                 label='Export User',
                 column_label='User',
                 help='User who exported payment')
        t.column('RefNum', 'character', format='x(20)', initial='',
                 label='Reference Nbr',
                 column_label='Reference',
                 help='Reference number')
        t.column('ExtInvID', 'character', format='x(12)', initial='',
                 label='External Invoice ID',
                 column_label='Ext.ID',
                 help='External invoice ID')
        t.column('ExtVoucher', 'character', format='x(12)', initial='',
                 label='External Voucher',
                 column_label='Ext.Voucher',
                 help='External voucher')
        t.index('AccDate', ['Brand', ('AccDate', 'DESCENDING'), 'Voucher'], area='Dyn_Index_1')
        t.index('CustNum', ['Brand', 'CustNum', 'InvNum', 'Voucher'], area='Dyn_Index_1',
                primary=True)
        t.index('CustNum_s', ['CustNum', 'InvNum', 'Voucher'], area='Dyn_Index_1')
        t.index('ExtInvID', ['Brand', 'ExtInvID'], area='Dyn_Index_1')
        t.index('ExtVoucher', ['Brand', 'ExtVoucher'], area='Dyn_Index_1')
        t.index('InvNum', ['Brand', 'InvNum'], area='Dyn_Index_1')
        t.index('InvNum_s', ['InvNum'], area='Dyn_Index_1')
        t.index('PaymArc', ['Brand', 'PaymArc'], area='Dyn_Index_1')
        t.index('PaymSrc', ['Brand', 'PaymSrc', 'PaymAmt'], area='Dyn_Index_1')
        t.index('PaymType', ['Brand', 'PaymType', ('AccDate', 'DESCENDING')], area='Dyn_Index_1')
        t.index('PPlanID', ['PPlanID', 'PPBatch'], area='Dyn_Index_1')
        t.index('RefNum', ['Brand', 'RefNum'], area='Dyn_Index_1')
        t.index('Voucher', ['Brand', 'Voucher'], area='Dyn_Index_1',
                unique=True)
        t.index('Voucher_s', ['Voucher'], area='Dyn_Index_1',
                unique=True)

    def down(self):
        self.drop_table('Payment')

