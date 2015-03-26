from gearbox.migrations import Migration

class AddCampRow(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CampRow', area='Sta_Data_128',
                       label='CampRow',
                       dump_name='camprow',
                       desc='Campaign rows\
')
        t.column('Campaign', 'character', initial='',
                 label='Campaign ID',
                 column_label='ID')
        t.column('Brand', 'character', initial='',
                 help='Code Of Brand')
        t.column('CLIType', 'character', initial='',
                 label='CLI Type',
                 column_label='CLIType')
        t.column('BillCode', 'character', mandatory=True, format='x(16)', initial='',
                 label='Billing Item',
                 column_label='BillItem',
                 help='Billing item code')
        t.column('CRowType', 'integer', format='9', initial='0',
                 label='Row Type',
                 column_label='Type',
                 help='Row type',
                 description='1=pricelist,2=discplan etc.')
        t.column('CRowItem', 'character', format='x(16)', initial='',
                 label='Row Item',
                 column_label='Item',
                 help='Campaign row item',
                 description='pricelist code, discplan code etc.')
        t.index('BillCode', ['Brand', 'BillCode'], area='Sta_Index_2')
        t.index('Campaign', ['Brand', 'Campaign', 'CRowType', 'CRowItem'], area='Sta_Index_2',
                primary=True)
        t.index('CLIType', ['Brand', 'CLIType'], area='Sta_Index_2')
        t.index('CRowType', ['Brand', 'CRowType', 'CLIType'], area='Sta_Index_2')

    def down(self):
        self.drop_table('CampRow')

