from gearbox.migrations import Migration

class AddTableSalesman(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('Salesman', area="Sta_Data_256", label="Salesman", dump_name="salesman", desc="Salesman")
        t.column('Salesman', 'character', format="x(8)", initial="", max_width=16, label="Salesman", column_label="Salesman", position=2, order=10, help="Salesman's code")
        t.column('SmName', 'character', format="x(30)", initial="", max_width=60, label="Salesman Name", column_label="Salesman Name", position=3, order=20, help="Salesman's name")
        t.column('SalesOffice', 'character', mandatory=True, format="x(8)", initial="", max_width=16, label="SalesOffice", column_label="SalesOffice", position=4, order=30, help="Sales office code")
        t.column('EMail', 'character', format="x(40)", initial="", max_width=80, label="E-mail", column_label="E-mail", position=5, order=40, help="Salesman's e-mail address")
        t.column('Parent', 'character', format="x(8)", initial="", max_width=16, label="Parent Salesman", column_label="Parent Salesman", position=6, order=50, help="Salesman's parent code")
        t.column('CommPerc', 'decimal', format="z9.99", decimals=2, initial="0", max_width=17, label="Prov%", column_label="Prov%", position=7, order=60, help="Commission %")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=8, order=70, help="Code Of Brand")
        t.column('Reseller', 'character', format="x(8)", initial="", max_width=16, label="Reseller", column_label="Resell", position=9, order=80, help="Reseller code")
        t.column('RsLevel', 'integer', format=">9", initial="0", max_width=4, label="Reseller Level", column_label="RSLevel", position=10, order=90, help="Salesman's level in reseller's organization")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Cust", position=11, order=100, help="Customer that is related to this salesman")
        t.column('Active', 'logical', format="Yes/No", initial="yes", max_width=1, label="Active", position=12, order=110, help="Is salesman active")
        t.index('Salesman', [['Brand'], ['Salesman'], ['SmName']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CustNum', [['Brand'], ['CustNum'], ['Salesman']], area="Sta_Index_2")
        t.index('Reseller', [['Brand'], ['Reseller'], ['RsLevel'], ['Salesman']], area="Sta_Index_2")
        t.index('SalesOffice', [['Brand'], ['SalesOffice'], ['Salesman']], area="Sta_Index_2", unique=True)
        t.index('SmName', [['Brand'], ['SmName'], ['SalesOffice']], area="Sta_Index_2")

    def down(self):
        self.drop_table('Salesman')
