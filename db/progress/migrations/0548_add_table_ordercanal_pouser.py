from gearbox.migrations import Migration

class AddTablePoUser(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('PoUser', area="Sta_Data_32", label="Portability user", dump_name="pouser", desc="Portability user")
        t.column('MsSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="SubSeq", column_label="SubSeq", position=2, order=10, help="Sequence for a Subscription")
        t.column('LineType', 'integer', mandatory=True, format=">9", initial="0", max_width=4, label="Line type", column_label="LineType", position=3, order=20, help="Line type (1=Mobile, 2=Fixed)")
        t.column('Company', 'character', format="x(20)", initial="", help="Company name", max_width=40, label="Company", column_label="Company", position=4, order=30, description="Company name")
        t.column('CompanyId', 'character', format="x(11)", initial="", max_width=22, label="Comp.ID", position=5, order=40, help="Customer's organisation ID (CIF)")
        t.column('PersonIdType', 'character', format="x(8)", initial="", max_width=16, label="PersonIdType", column_label="PersonIdType", position=6, order=50, help="Person ID Type (NIF/NIE/PASSPORT)")
        t.column('PersonId', 'character', format="x(11)", initial="", max_width=22, label="PersonId", column_label="PersonId", position=7, order=60, help="Person ID")
        t.column('CustName', 'character', format="x(30)", initial="", max_width=60, label="Customer's name", column_label="Customer's name", position=8, order=70, help="Customer's name")
        t.column('SurName2', 'character', format="x(30)", initial="", help="Second surname", max_width=60, label="Second Surname", column_label="2.Surname", position=9, order=80)
        t.column('FirstName', 'character', format="x(20)", initial="", max_width=40, label="Forename", column_label="Forename", position=10, order=90, help="Customer's forename")
        t.index('MsSeq', [['MsSeq'], ['LineType']], area="Sta_Index_1", unique=True)

    def down(self):
        self.drop_table('PoUser')
