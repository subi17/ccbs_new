from gearbox.migrations import Migration

class AddOPLog(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('OPLog', area='Sta_Data_128',
                       label='Overpayment Transaction',
                       dump_name='oplog',
                       desc='Overpayment Transaction Log')
        t.column('CustNum', 'integer', mandatory=True, format='ZZZZZZZZ9', initial='0',
                 label='CustNo',
                 column_label='CustNo',
                 help='Customer\'s number')
        t.column('EventDate', 'date', format='99-99-99',
                 label='Date',
                 column_label='Date',
                 help='Date of transaction/event')
        t.column('UserCode', 'character', initial='',
                 label='User',
                 column_label='User',
                 help='Id of the TMS User')
        t.column('EventType', 'integer', format='9', initial='0',
                 label='Type',
                 column_label='Type',
                 help='Transaction Type Code')
        t.column('InvNum', 'integer', format='zzzzzzz9', initial='0',
                 label='InvNo',
                 column_label='InvNo',
                 help='Number of Associated Invoice')
        t.column('Voucher', 'integer', format='zzzzzz9', initial='0',
                 label='VoucherNo',
                 column_label='VoucherNo',
                 help='Voucher Number of Associated Payment (if any)')
        t.column('Amt', 'decimal', decimals=2, format='-z,zzz,zz9.99', initial='0',
                 label='Sum',
                 column_label='Sum',
                 help='Sum (amount of money)')
        t.column('CreStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='TStamp',
                 column_label='TStamp',
                 help='TimeStamp; when created')
        t.column('Info', 'character', format='x(30)', initial='')
        t.index('CustDate', ['CustNum', ('EventDate', 'DESCENDING'), 'Voucher'], area='Sta_Index_2')
        t.index('CustInv', ['CustNum', 'InvNum', ('EventDate', 'DESCENDING')], area='Sta_Index_2')
        t.index('CustNum', ['CustNum', ('CreStamp', 'DESCENDING')], area='Sta_Index_2',
                primary=True)
        t.index('InvNum', ['InvNum', 'EventDate'], area='Sta_Index_2')

    def down(self):
        self.drop_table('OPLog')

