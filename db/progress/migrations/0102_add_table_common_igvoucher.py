from gearbox.migrations import Migration

class AddIGVoucher(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('IGVoucher', area='Sta_Data_256',
                       label='InvGroup Invoice Nbr',
                       dump_name='IGVouche',
                       desc='Invoice group invoice number sequences\
')
        t.column('Brand', 'character', initial='',
                 help='Code Of Brand')
        t.column('InvGroup', 'character', initial='',
                 help='Invoice group')
        t.column('PaymType', 'integer', format='>9', initial='0',
                 label='Payment Type',
                 column_label='Type',
                 help='Payment type')
        t.column('Voucher', 'integer', format='>>>>>>>9', initial='0',
                 label='Voucher Number',
                 column_label='Voucher',
                 help='Voucher number sequence (last used number)')
        t.column('SeqPrefix', 'character', initial='',
                 label='Sequence Prefix',
                 column_label='Prefix',
                 help='Sequence prefix')
        t.column('FromDate', 'date', format='99-99-99',
                 label='Valid From',
                 column_label='From',
                 help='Date when sequence becomes effective')
        t.index('InvGroup', ['Brand', 'InvGroup', 'PaymType', ('FromDate', 'DESCENDING')], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('IGVoucher')

