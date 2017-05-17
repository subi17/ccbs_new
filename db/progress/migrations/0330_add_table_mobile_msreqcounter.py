from gearbox.migrations import Migration

class AddTableMsReqCounter(Migration):

    database = "mobile"

    def up(self):
        t = self.table('MsReqCounter', area="Sta_Data_64", dump_name="MsReqCounter")
        t.column('ReqType', 'integer', format="zz9", initial="0", help="Request type", max_width=4, label="Request type", column_label="Type", position=2, order=10, description="Request type")
        t.column('ReqStatus', 'integer', format="zz9", initial="0", help="Request status", max_width=4, label="MsRequest Status", column_label="RequestStatus", position=3, order=20, description="Available status of MsRequest")
        t.column('ReqStatusCount', 'integer', format=">>>>>>>>>9", initial="0", help="Count of statuses", max_width=4, label="MsRequest Status Count", column_label="ReqStatusCount", position=4, order=30, description="The number of msrequests per reqtype and reqstatus")
        t.column('TransNbr', 'integer', format=">>9", initial="0", help="Transaction number", max_width=4, label="Transaction number", column_label="TransNbr", position=5, order=40, description="The transaction number")
        t.index('ReqType', [['ReqType'], ['ReqStatus'], ['TransNbr']], area="Sta_Index_3", primary=True, unique=True)

    def down(self):
        self.drop_table('MsReqCounter')
