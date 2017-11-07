from gearbox.migrations import Migration

class AddTableMsReqStatFunc(Migration):

    database = "common"

    def up(self):
        t = self.table('MsReqStatFunc', area="Sta_Data_2_256", dump_name="MsReqStatFunc")
        t.column('ReqType', 'integer', format="zz9", initial="0", max_width=4, label="Request type", column_label="Type", position=2, order=10, description="Request type")
        t.column('ReqStatus', 'integer', format="zz9", initial="0", max_width=4, label="MsRquest Status", column_label="RequestStatus", position=3, order=20, description="Avaialble status of MsRequest")
        t.column('FuncGroup', 'character', format="x(40)", initial="", max_width=80, label="FuncGroup", column_label="FuncGrp", position=4, order=30, description="Available functions for status")
        t.index('ReqType', [['ReqType'], ['ReqStatus']], area="Sta_Index_3", primary=True)

    def down(self):
        self.drop_table('MsReqStatFunc')
