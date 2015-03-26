from gearbox.migrations import Migration

class AddRegion(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Region', area='Sta_Data_128',
                       label='Region',
                       dump_name='region',
                       desc='Customer region')
        t.column('Region', 'character', initial='',
                 help='Region code')
        t.column('RgName', 'character', format='x(30)', initial='',
                 label='Name',
                 help='Region name')
        t.column('TaxZone', 'character', initial='',
                 label='Tax Zone',
                 column_label='Zone')
        t.index('Region', ['Region'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('RgName', ['RgName'], area='Sta_Index_2')

    def down(self):
        self.drop_table('Region')

