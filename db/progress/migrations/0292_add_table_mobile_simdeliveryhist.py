from gearbox.migrations import Migration

class AddSimDeliveryHist(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('SimDeliveryHist', area='Sta_Index_1',
                       dump_name='simdeliv')
        t.column('OrderID', 'integer', format='>>>>>>>>9', initial='0',
                 column_label='OrderID',
                 help='Order ID')
        t.column('MsSeq', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='MobSub Sequence',
                 column_label='SubSeq',
                 help='Sequence for a subscription')
        t.column('StatusCode', 'integer', format='z9', initial='0',
                 label='Status',
                 column_label='St',
                 help='Status Code')
        t.column('TimeStamp', 'decimal', decimals=2, format='99999999.99999', initial='0',
                 column_label='TimeStamp',
                 help='Activation Timestamp')
        t.column('CancelCode', 'integer', format='z9', initial='0',
                 label='Status',
                 column_label='St',
                 help='Status Code')
        t.column('Memo', 'character', format='x(30)', initial='',
                 column_label='Memo')
        t.index('MSSeq', ['MsSeq', ('TimeStamp', 'DESCENDING')], area='Sta_Index_2',
                primary=True)
        t.index('OrderID', ['OrderID', ('TimeStamp', 'DESCENDING')], area='Sta_Index_2')

    def down(self):
        self.drop_table('SimDeliveryHist')

