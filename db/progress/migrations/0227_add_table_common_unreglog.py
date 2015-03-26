from gearbox.migrations import Migration

class AddUnregLog(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('UnregLog', area='Sta_Data_128',
                       label='Unreg. payment log',
                       dump_name='unreglog',
                       desc='Log for unregistered payment handling')
        t.column('UrSeq', 'integer', initial='0',
                 help='Sequence')
        t.column('AccDate', 'date', format='99.99.9999',
                 label='Book day',
                 column_label='Book day',
                 help='Bookkeeping day')
        t.column('Voucher', 'integer', format='ZZZZZZ9', initial='0',
                 label='VoucherNo',
                 column_label='VoucherNo',
                 help='Consecutive recipt number of payment')
        t.column('CustNum', 'integer', format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer\'s number')
        t.column('CustBal', 'character', initial='',
                 column_label='CustBal',
                 help='Type of registeration')
        t.column('Amount', 'decimal', decimals=2, format='z,zzz,zz9.99-', initial='0',
                 help='Amount of payment')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('AccDate', ['AccDate'], area='Sta_Index_2')
        t.index('CustNum', ['CustNum'], area='Sta_Index_2')
        t.index('Urseq', ['UrSeq'], area='Sta_Index_2',
                primary=True)
        t.index('Voucher', ['Voucher'], area='Sta_Index_2')

    def down(self):
        self.drop_table('UnregLog')

