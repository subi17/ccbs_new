from gearbox.migrations import Migration

class AddTablexMsReqStatistic(Migration):

    database = "common"

    def up(self):
        t = self.table('xMsReqStatistic', area="Sta_Data_256", dump_name="MsReqStatistic")
        t.column('ReqType', 'integer', format="zz9", initial="0", help="Request type", max_width=4, label="Request type", column_label="Type", position=2, order=10, description="Request type")
        t.column('ReqStatus', 'integer', format="zz9", initial="0", help="Request status", max_width=4, label="MsRequest Status", column_label="RequestStatus", position=3, order=20, description="Available status of MsRequest")
        t.column('ReqStatusCount', 'integer', format=">>>>>>>>>9", initial="0", help="Count of statuses", max_width=4, label="MsRequest Status Count", column_label="ReqStatusCount", position=4, order=30, description="The number of msrequests per reqtype and reqstatus")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=5, order=40, help="Code Of Brand")
        t.index('ReqType', [['Brand'], ['ReqType'], ['ReqStatus']], area="Sta_Index_1", primary=True, unique=True)

    def down(self):
        self.drop_table('xMsReqStatistic')
