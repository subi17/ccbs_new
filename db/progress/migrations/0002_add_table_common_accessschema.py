from gearbox.migrations import Migration

class AddAccessSchema(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('AccessSchema', area='Sta_Data_128',
                       label='Access schemas',
                       dump_name='schema',
                       desc='Access schema definitions for limiting access to certain data')
        t.column('AccessSchemaSeq', 'integer', mandatory=True, initial='0',
                 column_label='AccessSchemaSeq')
        t.column('AScName', 'character', format='x(20)', initial='',
                 label='Access Schema Name',
                 column_label='Access Schema Name')
        t.column('AScType', 'integer', mandatory=True, initial='0',
                 label='Access Schema Type',
                 column_label='Access Schema Type',
                 help='Type for the Access Schema rule rules')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('AccessSchemaSeq', ['AccessSchemaSeq'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('AScName', ['Brand', 'AScName'], area='Sta_Index_2')
        t.index('ASCType', ['Brand', 'AScType'], area='Sta_Index_2')

    def down(self):
        self.drop_table('AccessSchema')

