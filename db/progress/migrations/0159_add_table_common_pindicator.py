from gearbox.migrations import Migration

class AddPIndicator(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('PIndicator', area='Sta_Data_32',
                       label='PIndicator',
                       desc=' Performance Indicator')
        t.column('Brand', 'character', format='x(10)', initial='',
                 column_label='Brand',
                 help='Code Of Brands')
        t.column('HostTable', 'character', format='x(16)', initial='',
                 column_label='HostTable')
        t.column('KeyValue', 'character', format='x(20)', initial='',
                 column_label='KeyValue')
        t.column('IndicatorType', 'integer', format='99', initial='?',
                 label='Indicator Type',
                 column_label='Type',
                 help='Indicator type')
        t.column('IndicatorValue', 'character', format='x(20)', initial='',
                 label='Indicator Value',
                 column_label='Value')
        t.column('TimeStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Time Stamp')
        t.column('Memo', 'character', format='x(20)', initial='',
                 label='Indicator Memo',
                 column_label='Indicator Memo')
        t.index('HostTable', ['Brand', 'HostTable', 'KeyValue', 'IndicatorType', ('TimeStamp', 'DESCENDING')], area='Dyn_Index_1',
                primary=True)

    def down(self):
        self.drop_table('PIndicator')

