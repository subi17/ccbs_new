from gearbox.migrations import Migration

class AddTableCustPP(Migration):

    database = "common"

    def up(self):
        t = self.table('CustPP', area="Sta_Data_256", label="Customer's Prod Packages", dump_name="custpp", desc="Customer's Prod Packages")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=2, order=10, help="Customer's number")
        t.column('ProdPack', 'character', mandatory=True, format="x(8)", initial="", max_width=16, label="PpId", column_label="PpId", position=3, order=20, help="Product Package ID")
        t.column('CustPP', 'integer', format="->>>>>>9", initial="0", max_width=4, label="Seq", column_label="Seq", position=4, order=30, help="Internal sequence no of Contract")
        t.column('ValidFrom', 'date', mandatory=True, format="99-99-99", initial=self.unknown, max_width=4, label="CDate", column_label="ContDate", position=5, order=40, help="Date from which the contract is valid")
        t.column('Salesman', 'character', format="x(8)", initial="", max_width=16, label="Salesman", column_label="Salesman", position=6, order=60, help="Salesman's code")
        t.column('Reseller', 'character', format="x(8)", initial="", max_width=16, label="Reseller", column_label="Reseller", position=7, order=70, help="Code of reseller (if applicable)")
        t.column('xxMemo', 'character', format="x(60)", initial="", max_width=610, label="Memo", column_label="Memo", extent=5, position=8, order=80, help="Explanation / memory field for Customer's ProdPack")
        t.column('ValidTo', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="ExpDate", column_label="ExpDate", position=9, order=50, help="Date when contract was ended/expired")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=10, order=90, help="Code Of Brand")
        t.index('ProdPack', [['Brand'], ['ProdPack'], ['CustNum']], area="Sta_Index_2", primary=True)
        t.index('CustNum', [['Brand'], ['CustNum'], ['ProdPack'], ['CustPP']], area="Sta_Index_2", unique=True)
        t.index('CustNum_s', [['CustNum'], ['ProdPack'], ['CustPP']], area="Sta_Index_2", unique=True)
        t.index('CustPP', [['Brand'], ['CustPP']], area="Sta_Index_2", unique=True)
        t.index('Reseller', [['Brand'], ['Reseller']], area="Sta_Index_2")
        t.index('Salesman', [['Brand'], ['Salesman']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CustPP')
