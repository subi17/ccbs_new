from gearbox.migrations import Migration

class AddTableM2MQueue(Migration):

    database = "common"

    def up(self):
        t = self.table('M2MQueue', area="Sta_Data_32", dump_name="m2mqueue")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=2, order=10, help="Code Of Brand")
        t.column('CLIType', 'character', format="x(3)", initial="", max_width=6, label="CLIType", column_label="CLIType", position=4, order=30, help="CLI type info")
        t.column('TSCreated', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Created", column_label="Created", position=6, order=50)
        t.column('TSNumpac', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="TS Numpac", column_label="TS Numpac", position=7, order=60)
        t.column('TSRequest', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="TS Request", column_label="TS Request", position=8, order=70)
        t.column('TSResponse', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="TS Response", column_label="TS Response", position=9, order=80)
        t.column('XMLMessage', 'character', format="x(8)", initial="", max_width=16, label="XML Message", column_label="XML Message", position=11, order=100)
        t.column('Response', 'character', format="x(8)", initial="", max_width=16, label="Response", column_label="Response", position=14, order=130, help="Response/error message")
        t.column('ReqStatus', 'integer', format=">>9", initial="999", max_width=4, label="Status", column_label="Status", position=15, order=140)
        t.column('Command', 'character', format="x(4)", initial="", max_width=8, label="Command", column_label="Command", position=18, order=160)
        t.column('UserDate', 'date', format="99-99-99", max_width=4, label="UserDate", column_label="UsedDate", position=21, order=220, help="User / handling date")
        t.column('ReqType', 'integer', format=">9", initial="0", max_width=4, label="Req.Type", column_label="Req.Type", position=22, order=230, help="Type of request (f.ex in or out)")
        t.column('RequestId', 'character', format="x(12)", initial="", max_width=24, label="RequestId", column_label="RequestId", position=23, order=240)
        t.column('NumDetails', 'integer', format=">>>9", initial="0", max_width=4, label="NumDetails", column_label="NumDetails", position=24, order=270, help="Nr. of details (amount)")
        t.column('CommNum', 'integer', format=">>9", initial="0", max_width=4, label="CommNum", column_label="CommNum", position=25, order=280, help="Command as integer")
        t.column('AddtlInfo', 'character', format="x(40)", initial="", max_width=80, label="Info", column_label="Info", position=26, order=290, help="Additional information text")
        t.column('AgrCust', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Agreement Customer", column_label="Agr.Cust", position=88, order=180, help="Agreement customer")
        t.column('CustNum', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=89, order=170, help="Customer's number")
        t.column('UserCode', 'character', mandatory=True, format="x(8)", initial="", max_width=16, label="User ID", column_label="User ID", position=90, order=190, help="User ID, 1 - 8 characters")
        t.column('CustType', 'integer', format=">9", initial="0", max_width=4, label="Customer Type", column_label="CustType", position=91, order=250, help="Customer type: person/company")
        t.column('M2MRequest', 'integer', format=">>>>>>9", initial="0", max_width=4, position=92, order=260)
        t.index('M2MRequest', [['M2MRequest'], ['TSCreated']], area="Sta_Index_2", primary=True)
        t.index('CommNum', [['CommNum']], area="Sta_Index_2")
        t.index('ReqStatus', [['ReqType'], ['ReqStatus']], area="Sta_Index_2")
        t.index('RequestId', [['ReqType'], ['RequestId']], area="Sta_Index_2")

    def down(self):
        self.drop_table('M2MQueue')
