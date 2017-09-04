from gearbox.migrations import Migration

class AddTableMsReqStatisticQ(Migration):

    database = "star"

    def up(self):
        t = self.table('MsReqStatisticQ', area="Dyn_Data_128", dump_name="MsReqStatisticQ")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=2, order=10, help="Code Of Brand")
        t.column('ReqType', 'integer', format="zz9", initial="0", help="Type Of MsRequest", max_width=4, label="MsRequest type", position=3, order=20, description="MsRequest type")
        t.column('ReqStatus', 'integer', format="zz9", initial="0", help="Status Code", max_width=4, label="MsRequest Status", column_label="ReqStat", position=4, order=30, description="MsRequest status")
        t.column('ReqStatUpdate', 'integer', format="-9", initial="0", help="Update status count", max_width=4, label="Update Count", column_label="Update", position=5, order=40, description="MsRequest status count update")
        t.index('Brand', [['Brand']], area="Dyn_Index_1", primary=True)

    def down(self):
        self.drop_table('MsReqStatisticQ')
