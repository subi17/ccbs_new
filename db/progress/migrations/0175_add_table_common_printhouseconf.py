from gearbox.migrations import Migration

class AddPrintHouseConf(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('PrintHouseConf', area='Sta_Data_256',
                       label='Printhouse Configuration',
                       dump_name='printhouseconf',
                       desc='Printhouse configuration')
        t.column('Brand', 'character', initial='',
                 help='Code of brand')
        t.column('TableName', 'character', format='x(16)', initial='',
                 label='Table Name',
                 column_label='Table',
                 help='Table name')
        t.column('KeyValue', 'character', format='x(20)', initial='',
                 label='Key Value',
                 column_label='Key',
                 help='Key value')
        t.column('FromDate', 'date', format='99-99-99',
                 label='Effective Date',
                 column_label='Eff.Date',
                 help='Date when rule becomes effective')
        t.column('ToDate', 'date', format='99-99-99',
                 label='End Date',
                 column_label='To',
                 help='Date when rule expires')
        t.column('PrintHouse', 'character', format='x(12)', initial='',
                 label='Printing House',
                 column_label='PrintHouse',
                 help='Printing house')
        t.column('Report', 'character', initial='')
        t.column('FieldName', 'character', format='x(16)', initial='',
                 label='Field Name',
                 column_label='Field',
                 help='Field name')
        t.index('PrintHouse', ['Brand', 'PrintHouse', 'Report', ('ToDate', 'DESCENDING')], area='Sta_Index_1')
        t.index('Report', ['Brand', 'Report', 'TableName', 'KeyValue', ('ToDate', 'DESCENDING')], area='Sta_Index_1',
                primary=True)

    def down(self):
        self.drop_table('PrintHouseConf')

