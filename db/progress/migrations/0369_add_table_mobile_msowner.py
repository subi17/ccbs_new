from gearbox.migrations import Migration

class AddTableMSOwner(Migration):

    database = "mobile"

    def up(self):
        t = self.table('MSOwner', area="Sta_Data_64", label="Owner of a MSISDN Number", dump_name="msowner1", desc='''


''')
        t.column('CLI', 'character', format="X(11)", initial="", max_width=22, label="MSISDN", column_label="MSISDN No", position=2, order=10, help="MSISDN Subscriber No")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=3, order=20, help="Customer's number")
        t.column('BillTarget', 'integer', format="z9", initial="0", max_width=4, label="InvTarg", column_label="IT", position=4, order=30, help="Invoicing Target No")
        t.column('TsBegin', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Usage begun", column_label="Usage begun", position=7, order=60, help="Time Stamp: when usage begun")
        t.column('TsEnd', 'decimal', format="99999999.99999", decimals=5, initial="99999999,99999", max_width=20, label="Usage ended", column_label="Usage ended", position=8, order=70, help="Time Stamp, when usage ended")
        t.column('MsSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", help="Sequence for a Subscription", max_width=4, label="SubSeq", column_label="SubSeq", position=9, order=80, description="Id of a mobsub record. Note that mobsub can have been deleted")
        t.column('IMSI', 'character', format="x(18)", initial="", max_width=36, label="IMSI Number", column_label="IMSI Number", position=14, order=130, help="IMSI Number")
        t.column('CliEvent', 'character', format="x(1)", initial="", max_width=2, label="Type", column_label="Type", position=16, order=150, help="Type of MSISDN number")
        t.column('Clitype', 'character', format="x(8)", initial="", max_width=16, label="MType", column_label="Mtype", position=17, order=160, help="Type Of Mobsub (connection type)")
        t.column('PayType', 'logical', format="PrePaid/PostPaid", initial="FALSE", help="PayType", max_width=1, label="PayType", column_label="PayType", position=18, order=320, description='''
''')
        t.column('MandateId', 'character', format="X(35)", initial="", max_width=70, label="Mandate Id", column_label="MandateID", position=19, order=330, help="Mandate ID for CSB19.14 file")
        t.column('MandateDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Mandate Date", column_label="MandateDate", position=20, order=340, help="Mandate ID creation date")
        t.column('TariffBundle', 'character', format="x(10)", initial="", max_width=20, label="Tariff Bundle", column_label="TariffBundle", position=21, order=350)
        t.column('FixedNumber', 'character', format="x(11)", initial=self.unknown, max_width=22, label="Fixed Number", column_label="FixedNumber", position=22, order=360, help="Fixed line number")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=24, order=230, help="Code Of Brand")
        t.column('Contract', 'character', format="x(8)", initial="", max_width=16, label="ContractID", column_label="ContrID", position=25, order=240, help="Contract ID")
        t.column('RepCodes', 'character', format="x(30)", initial="", max_width=60, label="ReportCode", column_label="ReportCode", position=26, order=250, help="Report Codes")
        t.column('OutPortOper', 'character', format="x(8)", initial="", max_width=16, label="OutPortOper", column_label="OutPortOper", position=29, order=290, help="Out ported operator code")
        t.column('InPortOper', 'character', format="x(8)", initial="", max_width=16, label="InPortOper", column_label="InPortOper", position=30, order=280, help="In porter operator code")
        t.column('InvCust', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Inv.Customer", column_label="InvCust", position=31, order=310, help="Invoicing customer's number")
        t.column('AgrCust', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Agr.Customer", column_label="AgrCust", position=32, order=300, help="Agreement customer's number")
        t.index('CLI', [['Brand'], ['CLI'], ['TsEnd', 'DESC']], area="Sta_Index_1", primary=True, unique=True)
        t.index('AgrCust', [['AgrCust'], ['CLI'], ['TsEnd', 'DESC']], area="Sta_Index_1")
        t.index('BillTarget', [['Brand'], ['CustNum'], ['BillTarget']], area="Sta_Index_1")
        t.index('CLIEvent', [['CLI'], ['TsBegin'], ['TsEnd'], ['CliEvent']], area="Sta_Index_1")
        t.index('CLI_s', [['CLI'], ['TsEnd', 'DESC']], area="Sta_Index_1", unique=True)
        t.index('Contract', [['Brand'], ['Contract']], area="Sta_Index_1")
        t.index('CustNum', [['Brand'], ['CustNum'], ['CLI'], ['TsEnd', 'DESC']], area="Sta_Index_1", unique=True)
        t.index('CustNum_s', [['CustNum'], ['CLI'], ['TsEnd', 'DESC']], area="Sta_Index_1")
        t.index('FixedNumber_s', [['Brand'], ['FixedNumber'], ['TsEnd', 'DESC']], area="Sta_Index_1")
        t.index('imsi', [['Brand'], ['IMSI'], ['TsBegin', 'DESC']], area="Sta_Index_1")
        t.index('imsi_s', [['IMSI'], ['TsBegin', 'DESC']], area="Sta_Index_1")
        t.index('InvCust', [['InvCust'], ['CLI'], ['TsEnd', 'DESC']], area="Sta_Index_1")
        t.index('MsSeq', [['MsSeq'], ['TsBegin', 'DESC']], area="Sta_Index_1")

    def down(self):
        self.drop_table('MSOwner')
