from gearbox.migrations import Migration

class AddRTItem(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('RTItem', area='Sta_Data_256',
                       dump_name='roamgprs')
        t.column('TariffNum', 'integer', format='>>>>>>9', initial='0')
        t.column('StepNum', 'integer', format='>9', initial='0',
                 label='StepNumber',
                 column_label='StepNumber')
        t.column('Unit', 'character', format='x(3)', initial='',
                 column_label='Unit')
        t.column('StepSize', 'integer', format='>>>>>9', initial='0',
                 column_label='StepSize')
        t.column('Tariff', 'decimal', decimals=7, format='>>9.999', initial='0',
                 column_label='Tariff')
        t.column('SizeLimit', 'integer', format='>>>>>9', initial='0',
                 column_label='SizeLimit')
        t.index('RTItem', ['TariffNum', 'StepNum'], area='Sta_Index_1',
                primary=True)

    def down(self):
        self.drop_table('RTItem')

