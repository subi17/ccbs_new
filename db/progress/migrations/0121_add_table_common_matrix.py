from gearbox.migrations import Migration

class AddMatrix(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Matrix', area='Sta_Data_128')
        t.column('Brand', 'character', initial='1',
                 column_label='Brand',
                 description='Brand')
        t.column('MXName', 'character', format='x(30)', initial='',
                 label='Name of the Matrix',
                 column_label='Matrix Name')
        t.column('MXKey', 'character', initial='',
                 column_label='MXKey',
                 help='Matrix key',
                 description='Matrix Key')
        t.column('MXSeq', 'integer', format='>>>>>>>>9', initial='0',
                 column_label='MXSeq',
                 help='Matrix Sequence',
                 description='Matrix Key')
        t.column('MXRes', 'integer', format='>9',
                 column_label='MXRes',
                 help='Matrix Response',
                 description='Matrix Response')
        t.column('Prior', 'integer', format='>>9', initial='0',
                 column_label='Prior',
                 help='Matrix Prior',
                 description='Matrix Prior')
        t.index('Brand', ['Brand', 'MXKey', 'Prior'], area='Sta_Index_1',
                primary=True)

    def down(self):
        self.drop_table('Matrix')

