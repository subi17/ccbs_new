from gearbox.migrations import Migration

class AddTableMNPDetails(Migration):

    database = "common"

    def up(self):
        t = self.table('MNPDetails', area="Sta_Data_64", dump_name="mnpdetails")
        t.column('MNPSeq', 'integer', format=">>>>>>>9", initial="0", max_width=4, position=2, order=10)
        t.column('CustId', 'character', format="x(12)", initial="", max_width=24, label="CustId", column_label="CustId", position=3, order=20)
        t.column('CustIdType', 'character', format="x(8)", initial="", max_width=16, label="CustIdType", column_label="CustIdType", position=4, order=30)
        t.column('FirstName', 'character', format="x(20)", initial="", help="Customer's fore/given name", max_width=40, label="ForeName", column_label="ForeName", position=5, order=40, description="Customer's forename")
        t.column('SurName1', 'character', format="x(20)", initial="", help="Customer's 1st last name", max_width=40, label="SurName1", column_label="SurName1", position=6, order=50, description="Customer's 1st last name")
        t.column('SurName2', 'character', format="x(20)", initial="", help="Customer's 1st last name", max_width=40, label="SurName2", column_label="SurName2", position=7, order=60, description="Customer's 1st last name")
        t.column('Nationality', 'character', format="x(2)", initial="", max_width=4, label="Nationality", column_label="Nationality", position=8, order=70)
        t.column('ReceptorCode', 'character', format="x(3)", initial="", max_width=6, label="ReceptorCode", column_label="ReceptorCode", position=9, order=80)
        t.column('CompanyName', 'character', format="x(30)", initial="", max_width=60, label="CompanyName", column_label="CompanyName", position=10, order=90, help="Company name")
        t.column('ReceptorNRN', 'character', format="x(8)", initial="", max_width=16, label="ReceptorNRN", column_label="ReceptorNRN", position=11, order=100, help="Receptor operator NRN")
        t.column('PortingTimeFromCustomer', 'logical', format="Yes/No", initial="No", max_width=1, label="PortingTimeFromCustomer", column_label="PortingTimeFromCustomer", position=12, order=110, help="Is the porting time defined by customer")
        t.column('DonorCode', 'character', format="x(3)", initial="", max_width=6, label="DonorCode", column_label="DonorCode", position=13, order=120, help="Donor operator Code")
        t.column('RequestedTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=15, label="RequestedTS", column_label="RequestedTS", position=14, order=130, help="Process creation time on operator side")
        t.column('StatusLimitTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=15, label="StatusLimitTS", column_label="StatusLimitTS", position=15, order=140, help="Time limit for status change")
        t.column('DonorExtraOrdinary', 'logical', format="Yes/No", initial="No", max_width=1, label="DonorExtraOrdinary", column_label="DonorExtraOrdinary", position=16, order=150, help="Donor operator is in extraordinary state")
        t.index('MNPSeq', [['MNPSeq']], area="Sta_Index_2", primary=True)
        t.index('CustId', [['CustId']], area="Sta_Index_2")

    def down(self):
        self.drop_table('MNPDetails')
