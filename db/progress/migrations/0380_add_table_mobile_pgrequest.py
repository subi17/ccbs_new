from gearbox.migrations import Migration

class AddTablePGRequest(Migration):

    database = "mobile"

    def up(self):
        t = self.table('PGRequest', area="Sta_Data_256", dump_name="pgrequest")
        t.column('ReqID', 'character', format="x(10)", initial="", max_width=20, label="Request ID", column_label="Request ID", position=2, order=10, description="Unique ID for request")
        t.column('ReqDate', 'date', format="99-99-9999", max_width=4, label="Request Date", column_label="RequestDate", position=3, order=20, description="Date when this request has been placed")
        t.column('CLI', 'character', format="x(11)", initial="", max_width=22, label="MSISDN", column_label="MSISDN", position=4, order=30, description="MSISDN number (633xxxxxx)")
        t.column('DNI', 'character', format="x(10)", initial="", max_width=20, label="Personal ID", position=5, order=40, description="NIF, NIE, Passport (nif=12345678X nie=X1234567X Passport: 1234567890)")
        t.column('CDRReqMonth', 'character', format="x(6)", initial="000000", max_width=12, label="CDR Request Month", column_label="CDR Request Month", position=6, order=50, description="Requested CDR month e.g. 200707")
        t.column('PlanName', 'character', format="x(30)", initial="", max_width=186, label="Plan Name", extent=3, position=7, order=60, description="Plan for the option requested by the user for comparison")
        t.column('PlanData', 'character', format="x(130)", initial="", max_width=786, label="Plan Data", extent=3, position=8, order=70, description="Plan for the option requested by user for comparison.")
        t.column('PlanAmount', 'decimal', format="zz9.99", decimals=2, initial="0", max_width=42, label="Plan Amount", extent=3, position=9, order=80)
        t.column('PGStatus', 'integer', format="9", initial="0", max_width=4, label="Status", position=10, order=90)
        t.index('ReqID', [['ReqID']], area="Sta_Index_1", primary=True)

    def down(self):
        self.drop_table('PGRequest')
