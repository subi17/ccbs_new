from gearbox.migrations import Migration

class AddTableDCServiceComponent(Migration):

    database = "mobile"

    def up(self):
        t = self.table('DCServiceComponent', area="Sta_Data_256", label="DC Service Component", dump_name="dcservicecomponent", desc="Service component settings for packages related to periodical contract")
        t.column('DCServicePackageID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="ID", position=2, order=10, help="Unique id for package definition")
        t.column('ServCom', 'character', format="x(12)", initial="", max_width=24, label="Service Component", column_label="Component", position=3, order=20, help="Service component ID")
        t.column('DefValue', 'integer', format=">>>9", initial="0", max_width=4, label="Default Value", column_label="Default", position=4, order=30, help="Default value")
        t.column('DefParam', 'character', format="x(20)", initial="", max_width=40, label="Default Parameter", column_label="Parameter", position=5, order=40, help="Default value for parameter")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="From", position=6, order=50, help="Date when becomes effective")
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="To", position=7, order=60, help="Date when expires")
        t.column('DCServiceComponentID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="DC Service Component ID", column_label="ID", position=8, order=70, help="Unique id for record")
        t.index('DCServiceComponentID', [['DCServiceComponentID']], area="Sta_Index_3", primary=True, unique=True)
        t.index('DCServicePackageID', [['DCServicePackageID'], ['ServCom'], ['ToDate', 'DESC']], area="Sta_Index_3")

    def down(self):
        self.drop_table('DCServiceComponent')
