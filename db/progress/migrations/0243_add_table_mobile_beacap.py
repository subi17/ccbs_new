from gearbox.migrations import Migration

class AddBeaCap(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('BeaCap', area='Sta_Data_256',
                       dump_name='beacap',
                       desc='Bearer Capability ')
        t.column('BeaCap', 'integer', mandatory=True, format='zzzz9', initial='0',
                 label='BC No.',
                 column_label='BC No.',
                 help='Bearer Capability No.',
                 valexp='input bc_number < 65535 and \
(input bc_number =  1 or input bc_number > 7)',
                 valmsg='Shall be 1 or 8 ... 65534 !')
        t.column('BcName', 'character', format='x(40)', initial='',
                 label='Name',
                 column_label='Name',
                 help='Name of Bearer Capability')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.index('BcName', ['Brand', 'BcName', 'BeaCap'], area='Sta_Index_3')
        t.index('BeaCap', ['Brand', 'BeaCap'], area='Sta_Index_3',
                primary=True, unique=True)

    def down(self):
        self.drop_table('BeaCap')

