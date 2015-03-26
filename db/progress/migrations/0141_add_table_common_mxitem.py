from gearbox.migrations import Migration

class AddMXItem(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('MXItem', area='Sta_Data_128')
        t.column('MXSeq', 'integer', format='>>>>>>>>9', initial='0',
                 column_label='MXSeq',
                 help='Matrix Sequence',
                 description='Matrix Key')
        t.column('MXName', 'character', format='x(16)', initial='',
                 label='Name MXItem',
                 column_label='Matrix item')
        t.column('MXValue', 'character', format='X(18)', initial='',
                 column_label='MXValue',
                 help='Matrix Value',
                 description='Matrix Value')
        t.index('MXSeq', ['MXSeq', 'MXName'], area='Sta_Index_1',
                primary=True)

    def down(self):
        self.drop_table('MXItem')

