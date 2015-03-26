from gearbox.migrations import Migration

class AddCDRStreamCounter(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CDRStreamCounter', area='Dyn_Data_128',
                       label='CDR Stream Counter',
                       dump_name='cdrstreamcounter',
                       desc='CDR Stream Counter')
        t.column('Brand', 'character', initial='',
                 help='Code of brand')
        t.column('ImportDate', 'date', format='99-99-99',
                 label='Import Date',
                 column_label='Import',
                 help='Import date')
        t.column('MSCID', 'character', initial='',
                 help='Mobile switching center')
        t.column('OnlineStream', 'integer', format='>>9', initial='0',
                 label='Online Stream',
                 column_label='Stream',
                 help='Stream from which CDR was read')
        t.column('CounterType', 'character', initial='',
                 label='Counter Type',
                 column_label='Type',
                 help='Counter type')
        t.column('CDRQty', 'integer', format='>>>>>>>>>9', initial='0',
                 label='Quantity',
                 column_label='Qty',
                 help='CDR quantity')
        t.index('ImportDate', ['Brand', 'ImportDate', 'CounterType', 'MSCID', 'OnlineStream'], area='Dyn_Index_1',
                primary=True, unique=True)

    def down(self):
        self.drop_table('CDRStreamCounter')

