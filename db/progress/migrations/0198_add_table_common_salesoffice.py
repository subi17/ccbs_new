from gearbox.migrations import Migration

class AddSalesoffice(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Salesoffice', area='Sta_Data_256',
                       label='Salesoffices',
                       dump_name='salesoff',
                       desc='Salesoffices for salesmen')
        t.column('SalesOffice', 'character', initial='',
                 column_label='SalesOffice',
                 help='Sales office code')
        t.column('SOName', 'character', format='x(30)', initial='',
                 label='Office Name',
                 column_label='SalesOffice Name',
                 help='Sales office name')
        t.column('CostCentre', 'integer', format='999', initial='0',
                 label='Cost Center',
                 column_label='Cost Center')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('SalesOffice', ['Brand', 'SalesOffice'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('SOName', ['Brand', 'SOName', 'SalesOffice'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('Salesoffice')

