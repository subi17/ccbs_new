from gearbox.migrations import Migration

class AddMsReqStatisticQ(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('MsReqStatisticQ', area='Sta_Data_32')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('ReqType', 'integer', format='zz9', initial='0',
                 label='MsRequest type',
                 help='Type Of MsRequest',
                 description='MsRequest type')
        t.column('ReqStatus', 'integer', format='zz9', initial='0',
                 label='MsRequest Status',
                 column_label='ReqStat',
                 help='Status Code',
                 description='MsRequest status')
        t.column('ReqStatUpdate', 'integer', format='-9', initial='0',
                 label='Update Count',
                 column_label='Update',
                 help='Update status count',
                 description='MsRequest status count update')
        t.index('Brand', ['Brand'], area='Sta_Index_1',
                primary=True)

    def down(self):
        self.drop_table('MsReqStatisticQ')

