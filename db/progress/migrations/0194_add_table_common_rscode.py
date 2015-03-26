from gearbox.migrations import Migration

class Addrscode(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('rscode', area='Sta_Data_256',
                       label='Reseller Codes',
                       desc='Reseller codes')
        t.column('Reseller', 'integer', format='99', initial='0',
                 label='Code',
                 column_label='Code',
                 help='Reseller code')
        t.column('RsName', 'character', initial='',
                 label='Name',
                 column_label='Name',
                 help='Name of reseller type')

    def down(self):
        self.drop_table('rscode')

