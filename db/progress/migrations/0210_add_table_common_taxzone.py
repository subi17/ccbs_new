from gearbox.migrations import Migration

class AddTaxZone(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('TaxZone', area='Sta_Data_128',
                       label='Tax Zone',
                       dump_name='taxzone',
                       desc='Tax zones')
        t.column('TaxZone', 'character', initial='',
                 label='Tax Zone',
                 column_label='Zone')
        t.column('TZName', 'character', format='x(30)', initial='',
                 label='Zone Name',
                 column_label='Name',
                 help='Tax zone name')
        t.index('TaxZone', ['TaxZone'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('TaxZone')

