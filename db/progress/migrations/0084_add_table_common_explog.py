from gearbox.migrations import Migration

class AddExpLog(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('ExpLog', area='Sta_Data_256',
                       label='Transfer Log',
                       dump_name='explog',
                       desc='Transfer log for exported data ')
        t.column('ExpNum', 'integer', format='>>>>>>>9', initial='0',
                 label='ID number',
                 column_label='ID nr.',
                 help='Identification number for exported data')
        t.column('DataType', 'character', initial='',
                 label='Type',
                 column_label='Type',
                 help='Type of exported data')
        t.column('ExpStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='XStamp',
                 column_label='XStamp',
                 help='Time stamp for exported data')
        t.column('ExpType', 'character', initial='',
                 column_label='ExpType',
                 help='Export type of data, ie. receiver type')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('DataType', ['Brand', 'DataType', 'ExpType', 'ExpNum'], area='Sta_Index_2',
                unique=True)
        t.index('ExpType', ['Brand', 'ExpType', 'DataType', 'ExpNum'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('ExpLog')

