from gearbox.migrations import Migration

class AddM2MDetail(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('M2MDetail', area='Sta_Data_128',
                       dump_name='m2mdetai')
        t.column('MsSeq', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='SubSeq',
                 column_label='SubSeq',
                 help='Sequence for a Subscription')
        t.column('CLI', 'character', format='X(11)', initial='',
                 column_label='CLI',
                 help='CLI, subscriber no.')
        t.column('ReqStatus', 'integer', format='>>9', initial='999',
                 label='Status',
                 column_label='Status')
        t.column('RejCode', 'character', format='x(2)', initial='',
                 column_label='RejCode',
                 help='Rejection code')
        t.column('RejText', 'character', format='x(20)', initial='',
                 column_label='RejText',
                 help='Rejection text')
        t.column('M2MRequest', 'integer', format='>>>>>>9', initial='0')
        t.index('M2MRequest', ['M2MRequest'], area='Sta_Index_2',
                primary=True)
        t.index('MsSeq', ['MsSeq'], area='Sta_Index_2')

    def down(self):
        self.drop_table('M2MDetail')

