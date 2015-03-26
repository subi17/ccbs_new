from gearbox.migrations import Migration

class AddTaxClass(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('TaxClass', area='Sta_Data_128',
                       label='Tax Class',
                       dump_name='taxclass',
                       desc='Tax classes')
        t.column('TaxClass', 'character', initial='',
                 label='Tax Class',
                 column_label='Class',
                 help='Tax class')
        t.column('TCName', 'character', format='x(30)', initial='',
                 label='Class Name',
                 column_label='Name',
                 help='Tax class name')
        t.index('TaxClass', ['TaxClass'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('TaxClass')

