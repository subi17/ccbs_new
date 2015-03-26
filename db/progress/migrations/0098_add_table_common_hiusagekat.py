from gearbox.migrations import Migration

class AddHiUsageKat(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('HiUsageKat', area='Sta_Data_256',
                       dump_name='hiusagek')
        t.column('Category', 'character', format='x(4)', initial='',
                 label='Cat',
                 column_label='Cat',
                 help='Category code')
        t.column('AgeFrom', 'integer', format='>>9', initial='0',
                 column_label='Age From',
                 help='From Age')
        t.column('AgeTo', 'integer', format='>>9', initial='999',
                 column_label='AgeTo',
                 help='To Age')
        t.column('CLIType', 'character', initial='',
                 column_label='CLIType',
                 help='CLI Type',
                 description='CLI Type')
        t.column('ActInDays', 'integer', format='>>>9', initial='0',
                 label='ActiveInDays',
                 column_label='ActiveInDays',
                 help='Active in days')
        t.column('CustClass', 'integer', format='9', initial='0',
                 label='Class',
                 column_label='Class',
                 help='Customer Class (depend on avg. amount of invoices)')
        t.index('Category', ['CustClass'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('HiUsageKat')

