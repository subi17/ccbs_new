from gearbox.migrations import Migration

class AddCounter(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Counter', area='Sta_Data_256',
                       label='Counter',
                       dump_name='counter',
                       desc='Counter')
        t.column('Brand', 'character', format='x(40)', initial='',
                 column_label='Brand',
                 help='Code Of Brands')
        t.column('HostTable', 'character', format='x(16)', initial='',
                 column_label='HostTable')
        t.column('KeyValue', 'character', format='x(20)', initial='',
                 column_label='KeyValue')
        t.column('CounterType', 'integer', format='99', initial='0',
                 label='Counter Type',
                 column_label='Type',
                 help='Counter type')
        t.column('CounterAmt', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 label='Amount')
        t.column('BeginStamp', 'decimal', decimals=2, format='99999999.99999', initial='0',
                 label='Begin Stamp')
        t.column('EndStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='End Stamp',
                 column_label='End Stamp')
        t.column('CounterSeq', 'integer', format='>>>>>>>9', initial='0')
        t.index('CounterSeq', ['CounterSeq'], area='Dyn_Index_1',
                primary=True, unique=True)
        t.index('HostTable', ['Brand', 'HostTable', 'KeyValue', 'CounterType', ('EndStamp', 'DESCENDING')], area='Dyn_Index_1')

    def down(self):
        self.drop_table('Counter')

