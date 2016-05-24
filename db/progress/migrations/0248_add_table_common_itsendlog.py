from gearbox.migrations import Migration

class AddTableITSendLog(Migration):

    database = "common"

    def up(self):
        t = self.table('ITSendLog', area="Dyn_Data_128", dump_name="itsendlo")
        t.column('ITNum', 'integer', format=">>>>>>9", initial="0", max_width=4, label="Seq", column_label="Seq", position=2, order=10, help="Internal sequenco no. of an Invoice Text element")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Cust", position=3, order=20, help="Customer's number")
        t.column('SendStamp', 'decimal', format=">>>>>>>9.99999", decimals=5, initial="0", max_width=20, label="Sending time", column_label="Sent", position=5, order=30, help="Time when text was sent to customer")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=6, order=40, help="Code Of Brand")
        t.column('InvNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Invoice Nbr", column_label="Invoice", position=7, order=50, help="Invoice number")
        t.column('EMail', 'character', format="x(50)", initial="", max_width=100, label="EMail Address", column_label="EMail", position=8, order=60, help="eMail address")
        t.column('RepType', 'character', format="x(8)", initial="", max_width=16, label="Report Type", column_label="Report", position=9, order=70, help="Report Type")
        t.column('SendInfo', 'character', format="x(50)", initial="", max_width=100, label="Info", position=10, order=80, help="Info")
        t.column('UserCode', 'character', format="x(8)", initial="", max_width=16, label="User", column_label="User", position=11, order=90, help="Id of the TMS User")
        t.column('SendMethod', 'integer', format="9", initial="0", help="Send method", max_width=4, label="Send Method", column_label="Method", position=12, order=110, description="1=email, 2=epl")
        t.column('TxtType', 'integer', format="9", initial="0", help="Text type", max_width=4, label="Text Type", column_label="Text", position=13, order=100, description="1=IT, 2=memo")
        t.index('CustNum_s', [['CustNum'], ['SendStamp', 'DESC']], area="Dyn_Index_1", primary=True)
        t.index('CustNum', [['Brand'], ['CustNum'], ['SendStamp', 'DESC']], area="Dyn_Index_1")
        t.index('InvNum', [['InvNum'], ['RepType']], area="Dyn_Index_1")
        t.index('RepType', [['Brand'], ['RepType'], ['SendStamp', 'DESC']], area="Dyn_Index_1")
        t.index('TxtType', [['TxtType'], ['ITNum'], ['CustNum']], area="Dyn_Index_1")

    def down(self):
        self.drop_table('ITSendLog')
