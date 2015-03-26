from gearbox.migrations import Migration

class AddOrderFunction(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('OrderFunction', area='Sta_Data_128',
                       dump_name='orderfun')
        t.column('OFID', 'integer', format='>>>>>9', initial='0',
                 column_label='OFID')
        t.column('OFName', 'character', format='x(30)', initial='',
                 label='Name of the Order Function',
                 column_label='Name of the Order Function')
        t.column('OFModule', 'character', format='x(16)', initial='',
                 label='Module',
                 column_label='Module',
                 help='Progress module name')
        t.index('OFID', ['OFID'], area='Sta_Index_1',
                primary=True)
        t.index('OFName', ['OFName'], area='Sta_Index_2')

    def down(self):
        self.drop_table('OrderFunction')

