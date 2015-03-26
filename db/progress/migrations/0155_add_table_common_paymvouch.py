from gearbox.migrations import Migration

class AddPaymVouch(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('PaymVouch', area='Sta_Data_256',
                       label='Payment Voucher',
                       dump_name='paymvouc',
                       desc='Number series for payment vouchers')
        t.column('Brand', 'character', initial='',
                 help='Code Of Brand')
        t.column('Voucher', 'integer', format='>>>>>>>9', initial='0',
                 label='Voucher Nbr',
                 column_label='Voucher',
                 help='Last used voucher number')
        t.column('VoucherType', 'integer', format='>9', initial='0',
                 label='Voucher Type',
                 column_label='Type',
                 help='Voucher type')
        t.column('FromDate', 'date', format='99-99-99',
                 label='Valid From',
                 column_label='From',
                 help='Date when sequence becomes effective')
        t.index('Voucher', ['Brand', 'VoucherType', ('FromDate', 'DESCENDING'), 'Voucher'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('PaymVouch')

