from gearbox.migrations import Migration

class AddRoamTariff(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('RoamTariff', area='Sta_Data_256',
                       dump_name='roamtari')
        t.column('TariffNum', 'integer', format='>>>>>>9', initial='0')
        t.column('PriceList', 'character', initial='',
                 column_label='PriceList')
        t.column('RoamGroup', 'character', initial='',
                 column_label='RoamGroup')
        t.column('Service', 'character', initial='',
                 column_label='Service')
        t.column('ValidFrom', 'date', format='99-99-9999',
                 label='Valid From',
                 column_label='From',
                 help='Valid from')
        t.column('ValidTo', 'date', format='99-99-9999',
                 label='Valid To',
                 column_label='To',
                 help='Valid to')
        t.column('RateType', 'integer', format='>9', initial='0',
                 label='Type',
                 column_label='Type')
        t.column('Tariff', 'decimal', decimals=5, format='>>9.999', initial='0',
                 column_label='Tariff')
        t.column('RoamingType', 'integer', format='9', initial='0',
                 column_label='RoamingType')
        t.index('PriceList', ['PriceList', ('ValidFrom', 'DESCENDING'), ('ValidTo', 'DESCENDING'), 'RateType', 'RoamingType', 'Service'], area='Sta_Index_1')
        t.index('RateType', ['RateType', 'PriceList', ('ValidFrom', 'DESCENDING'), ('ValidTo', 'DESCENDING'), 'RoamingType', 'Service'], area='Sta_Index_1')
        t.index('RoamTariff', ['TariffNum'], area='Sta_Index_1',
                primary=True)

    def down(self):
        self.drop_table('RoamTariff')

