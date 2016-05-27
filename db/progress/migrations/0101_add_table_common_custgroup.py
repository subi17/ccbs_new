from gearbox.migrations import Migration

class AddTableCustGroup(Migration):

    database = "common"

    def up(self):
        t = self.table('CustGroup', area="Sta_Data_256", label="Customer Groups", dump_name="custgrou", desc="Customer Groups")
        t.column('CustGroup', 'character', format="x(10)", initial="", max_width=20, label="GroupCode", column_label="GroupCode", position=2, order=10, help="Individual Code for a Customer Group")
        t.column('CGName', 'character', format="x(40)", initial="", max_width=80, label="Name", column_label="Name", position=3, order=20, help="Group name")
        t.column('CreDate', 'date', format="99-99-99", initial="today", max_width=4, label="Created", column_label="Created", position=4, order=30, help="Date when group was created")
        t.column('ChgDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Changed", column_label="Changed", position=5, order=40, help="Date when Group and/or its members were changed")
        t.column('CreUser', 'character', format="x(8)", initial="", max_width=16, label="Created by", column_label="Created by", position=6, order=50, help="User who created this group")
        t.column('ChgUser', 'character', format="x(8)", initial="", max_width=16, label="Changed by", column_label="Changed by", position=7, order=400, help="User who changed/updated this group latest")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=1830, label="Memo", column_label="Memo", extent=15, position=8, order=410, help="Memo text")
        t.column('PrevCrit', 'character', format="x(70)", initial="", max_width=140, label="Criteria", column_label="Criteria", position=9, order=420, help="Previous Criteria")
        t.column('EnterTask', 'character', format="x(30)", initial="", max_width=60, label="Entering Task", column_label="Enter", position=10, order=430, help="Task that is performed when customer is entered into group")
        t.column('LeaveTask', 'character', format="x(30)", initial="", max_width=60, label="Leaving Task", column_label="Leave", position=11, order=440, help="Task that is performed when customer is removed from group")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=12, order=450, help="Code Of Brand")
        t.index('CustGroup', [['Brand'], ['CustGroup']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CGName', [['Brand'], ['CGName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CustGroup')
