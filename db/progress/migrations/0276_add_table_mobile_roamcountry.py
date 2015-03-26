from gearbox.migrations import Migration

class AddRoamCountry(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('RoamCountry', area='Sta_Data_256',
                       dump_name='roamcoun')
        t.column('Prefix', 'character', format='x(5)', initial='',
                 column_label='Prefix')
        t.column('RateZone', 'integer', format='>9', initial='0',
                 label='Zone',
                 column_label='Zone')
        t.column('CountryName', 'character', format='x(16)', initial='',
                 column_label='CountryName')
        t.index('RateZone', ['RateZone', 'Prefix'], area='Sta_Index_1')
        t.index('RoamCountry', ['Prefix'], area='Sta_Index_1',
                primary=True)

    def down(self):
        self.drop_table('RoamCountry')

