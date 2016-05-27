from gearbox.migrations import Migration

class AddTableDCServicePackage(Migration):

    database = "mobile"

    def up(self):
        t = self.table('DCServicePackage', area="Sta_Data_256", label="DC Service Package", dump_name="dcservicepackage", desc="Service packages related to periodical contract")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of brand")
        t.column('DCEvent', 'character', format="x(12)", initial="", max_width=24, label="Periodical Contract", column_label="Per.Contract", position=3, order=20, help="ID of periodical contract")
        t.column('DCServicePackageID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="DC Service Package ID", column_label="ID", position=4, order=30, help="Unique id for record")
        t.column('ServPac', 'character', format="x(12)", initial="", max_width=24, label="Service Package", column_label="Serv.Pack", position=5, order=40, help="Service package code")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="From", position=6, order=50, help="Date when becomes effective")
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="To", position=7, order=60, help="Date when expires")
        t.index('DCServicePackageID', [['DCServicePackageID']], area="Sta_Index_3", primary=True, unique=True)
        t.index('DCEvent', [['Brand'], ['DCEvent'], ['ServPac'], ['ToDate', 'DESC']], area="Sta_Index_3")
        t.index('ServPac', [['Brand'], ['ServPac'], ['ToDate', 'DESC']], area="Sta_Index_3")

    def down(self):
        self.drop_table('DCServicePackage')
