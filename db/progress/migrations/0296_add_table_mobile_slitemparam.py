from gearbox.migrations import Migration

class AddSLItemParam(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('SLItemParam', area='Sta_Data_64',
                       dump_name='slitempa')
        t.column('SoLog', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='Order Seq',
                 column_label='Order Seq',
                 help='Sequence for a Service Order')
        t.column('RequestId', 'integer', format='>>9', initial='0',
                 column_label='RId')
        t.column('ParamName', 'character', format='x(35)', initial='',
                 column_label='ParamName')
        t.column('ParamValue', 'character', format='x(35)', initial='',
                 column_label='ParamValue')
        t.index('SoLog', ['SoLog', 'RequestId'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('SLItemParam')

