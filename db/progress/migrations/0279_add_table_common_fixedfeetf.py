from gearbox.migrations import Migration

class AddTableFixedFeeTF(Migration):

    database = "common"

    def up(self):
        t = self.table('FixedFeeTF', area="Sta_Data_128", label="Financed Contract Fees", dump_name="fixedfeetf", desc="Extension of FixedFee table. Contains financing info")
        t.column('FFNum', 'integer', format="zzzzzzz9", initial="0", max_width=4, label="Contract", column_label="Contract", position=2, order=10, help="Consecutive number (sequence) of contract")
        t.column('TFBank', 'character', format="X(8)", initial="", max_width=16, label="TFBank", column_label="TFBank", position=3, order=20, help="Terminal Financing Bank Code")
        t.column('BankDate', 'date', format="99-99-9999", max_width=4, label="Bank Date", column_label="BankDate", position=4, order=30, help="Date of Contract")
        t.column('BankRespDate', 'date', format="99-99-9999", max_width=4, label="Bank Resp. Date", column_label="BankRespDate", position=5, order=40, help="Date of Contract")
        t.column('CancelDate', 'date', format="99-99-9999", max_width=4, label="Cancel Date", column_label="Cancel Date", position=6, order=50, help="Date of Contract")
        t.column('CancelStatus', 'character', format="x(8)", initial="", max_width=16, label="Cancellation Status", column_label="CancelStatus", position=7, order=60)
        t.column('CancelReason', 'character', format="x(15)", initial="", max_width=30, label="Cancellation Reason", column_label="CancelReason", position=8, order=70)
        t.column('BankResult', 'character', format="x(8)", initial="", max_width=16, label="BankResult", column_label="BankResult", position=9, order=80, help="Bank result code")
        t.column('Amount', 'decimal', format="->>>>>9.99", decimals=2, initial="0", max_width=17, label="Amount", column_label="Amount", position=10, order=100)
        t.column('ResidualAmount', 'decimal', format="->>>>>9.99", decimals=2, initial="0", max_width=17, label="ResidualAmount", column_label="ResidualAmount", position=11, order=110)
        t.column('OrgId', 'character', format="x(11)", initial="", max_width=22, label="Pers/Comp.ID", position=12, order=120, help="CustContact's organisation ID or personal ID")
        t.column('CancelMemo', 'character', format="x(30)", initial="", max_width=60, label="CancelMemo", column_label="CancelMemo", position=13, order=130, help="Cancel Memo")
        t.column('CancelResp', 'character', format="x(12)", initial="", max_width=24, label="Cancel Response", column_label="CancelResp", position=14, order=140)
        t.column('CancelFile', 'character', format="x(30)", initial="", max_width=60, label="Cancel File", column_label="CancelFile", position=15, order=150)
        t.column('OrderId', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="OrderId", column_label="OrderId", position=16, order=160)
        t.index('FFNum', [['FFNum']], area="Sta_Index_4", primary=True, unique=True)
        t.index('CancelStatus', [['CancelStatus']], area="Sta_Index_4")
        t.index('OrderId', [['OrderId']], area="Sta_Index_4")
        t.index('OrgId', [['OrgId']], area="Sta_Index_4")

    def down(self):
        self.drop_table('FixedFeeTF')
