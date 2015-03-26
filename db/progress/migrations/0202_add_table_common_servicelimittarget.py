from gearbox.migrations import Migration

class AddServiceLimitTarget(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('ServiceLimitTarget', area='Sta_Data_128',
                       dump_name='servlt')
        t.column('SLSeq', 'integer', format='>>>>>>9', initial='0',
                 column_label='SLseq',
                 help='Sequence for Servicelimit')
        t.column('ServiceLMember', 'character', format='x(16)', initial='',
                 label='ServiceLimit Member',
                 column_label='ServiceLimit Member',
                 help='Momber of Service Limit')
        t.column('ServiceLimitMT', 'integer', format='9', initial='0',
                 label='Type',
                 column_label='Type',
                 help='Member Type of Service Limit')
        t.column('OutsideRate', 'character', initial='',
                 column_label='OutsideRate',
                 help='Rate-key when limit is full')
        t.column('InsideRate', 'character', initial='',
                 label='LimitRate',
                 column_label='LimitRate',
                 help='Rate-key when belongs to group')
        t.index('SLSeq', ['SLSeq', 'ServiceLMember'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('ServiceLimitTarget')

