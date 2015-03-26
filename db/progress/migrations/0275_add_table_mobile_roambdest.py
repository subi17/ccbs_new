from gearbox.migrations import Migration

class AddRoamBDest(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('RoamBDest', area='Sta_Data_256',
                       dump_name='roambdes')
        t.column('TariffNum', 'integer', format='>>>>>>9', initial='0')
        t.column('BDest', 'character', format='x(12)', initial='',
                 column_label='BDest')
        t.column('TZFrom', 'character', format='99:99', initial='',
                 label='From',
                 column_label='From')
        t.column('TZTo', 'character', format='99:99', initial='',
                 label='To',
                 column_label='To')
        t.column('Tariff', 'decimal', decimals=5, format='>>9.999', initial='0',
                 column_label='Tariff')
        t.index('RoamBDest', ['TariffNum', 'TZFrom', 'TZTo'], area='Sta_Index_1',
                primary=True)

    def down(self):
        self.drop_table('RoamBDest')

