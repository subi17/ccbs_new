from gearbox.migrations import Migration

class AddPPInv(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('PPInv', area='Sta_Data_256',
                       label='PP Invoices',
                       dump_name='ppinv',
                       desc='Payment plan\'s invoices')
        t.column('PPlanID', 'integer', format='>>>>>>>9', initial='0',
                 label='Payment Plan ID',
                 column_label='PP ID',
                 help='Payment plan ID')
        t.column('InvNum', 'integer', format='>>>>>>>9', initial='0',
                 label='Invoice',
                 column_label='InvNum',
                 help='Invoice number')
        t.column('Amount', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 help='Invoice\'s debt when it was added to payment plan')
        t.index('InvNum', ['InvNum'], area='Sta_Index_2')
        t.index('PPlanID', ['PPlanID', 'InvNum'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('PPInv')

