from gearbox.migrations import Migration

class AddSLItem(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('SLItem', area='Sta_Data_64',
                       dump_name='slitem')
        t.column('SoLog', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='Order Seq',
                 column_label='Order Seq',
                 help='Sequence for a Service Order')
        t.column('RequestId', 'integer', format='>>9', initial='0',
                 label='Id.',
                 column_label='Id.')
        t.column('ParamName', 'character', format='x(32)', initial='',
                 column_label='ParamName')
        t.column('ItemStatus', 'integer', format='>>9', initial='0',
                 column_label='St.')
        t.column('ErrorMessage', 'character', format='x(31)', initial='',
                 column_label='ErrorMessage')
        t.column('ErrorCode', 'integer', format='>>9', initial='0',
                 column_label='E.C')
        t.column('TMSParam', 'character', initial='',
                 column_label='TMSParam')
        t.index('SoLog', ['SoLog'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('SLItem')

