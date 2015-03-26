from gearbox.migrations import Migration

class AddUnregPaym(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('UnregPaym', area='Sta_Data_32',
                       label='Unregistered Payments',
                       dump_name='unregpay',
                       desc='Unregistered payments')
        t.column('AccDate', 'date', format='99.99.9999',
                 label='Book day',
                 column_label='Book day',
                 help='Bookkeeping day')
        t.column('PaymDate', 'date', format='99.99.9999',
                 label='Payment day',
                 column_label='Payment day',
                 help='Date of invoice payment')
        t.column('RefNum', 'character', format='x(20)', initial='',
                 label='Ref.Num.',
                 column_label='Ref.Num.',
                 help='Reference Number')
        t.column('InvNum', 'character', initial='0',
                 label='Inv.Num.',
                 column_label='Inv.Num.',
                 help='Invoice\'s number')
        t.column('ArchiveId', 'character', format='x(16)', initial='',
                 label='Arch.ID',
                 column_label='Arch.ID',
                 help='Archive ID')
        t.column('PaidAmt', 'decimal', decimals=2, initial='0',
                 label='Payment',
                 column_label='Payment')
        t.column('AccNum', 'integer', format='>>>>>9', initial='0',
                 label='Acc.Num.',
                 column_label='Acc.Num.',
                 help='Account number')
        t.column('Interest', 'decimal', decimals=2, initial='0',
                 column_label='Interest')
        t.column('CustName', 'character', format='x(12)', initial='',
                 label='Cust.name',
                 column_label='Cust.name',
                 help='Customer name')
        t.column('BankAcc', 'character', format='x(20)', initial='',
                 label='Bank Acc.',
                 column_label='Bank Acc.',
                 help='Bank account number')
        t.column('Memo', 'character', format='x(30)', initial='',
                 help='Info')
        t.column('PaymSrc', 'character', initial='',
                 label='Payment source',
                 column_label='Source',
                 help='Source of payment',
                 description='empty=manual, dd=direct debit, rf=reference file, ca=collection agency')
        t.column('Booked', 'decimal', decimals=2, initial='0',
                 label='Booked sum',
                 column_label='Booked sum')
        t.column('State', 'integer', format='9', initial='0',
                 column_label='State',
                 help='0 - not processed, 1 - deleted')
        t.column('UrSeq', 'integer', initial='0',
                 help='Sequence')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('archiveID', ['Brand', 'ArchiveId'], area='Sta_Index_2',
                primary=True)
        t.index('bankacc', ['Brand', 'State', 'BankAcc', ('PaymDate', 'DESCENDING')], area='Sta_Index_2')
        t.index('custname', ['Brand', 'State', 'CustName'], area='Sta_Index_2')
        t.index('PaidAmt', ['Brand', 'State', 'PaidAmt'], area='Sta_Index_2')
        t.index('PaymDate', ['Brand', 'State', 'PaymDate'], area='Sta_Index_2')
        t.index('RefNum', ['Brand', 'State', 'RefNum'], area='Sta_Index_2')
        t.index('Urseq', ['UrSeq'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('UnregPaym')

