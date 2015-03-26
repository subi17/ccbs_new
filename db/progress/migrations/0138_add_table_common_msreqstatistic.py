from gearbox.migrations import Migration

class AddMsReqStatistic(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('MsReqStatistic', area='Sta_Data_256')
        t.column('ReqType', 'integer', format='zz9', initial='0',
                 label='Request type',
                 column_label='Type',
                 description='Request type')
        t.column('ReqStatus', 'integer', format='zz9', initial='0',
                 label='MsRequest Status',
                 column_label='RequestStatus',
                 help='Request status',
                 description='Available status of MsRequest')
        t.column('ReqStatusCount', 'integer', format='>>>>>>>>>9', initial='0',
                 label='MsRequest Status Count',
                 column_label='ReqStatusCount',
                 help='Count of statuses',
                 description='The number of msrequests per reqtype and reqstatus')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('ReqType', ['Brand', 'ReqType', 'ReqStatus'], area='Sta_Index_1',
                primary=True, unique=True)

    def down(self):
        self.drop_table('MsReqStatistic')

