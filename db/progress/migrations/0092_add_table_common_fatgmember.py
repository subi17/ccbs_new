from gearbox.migrations import Migration

class AddTableFATGMember(Migration):

    database = "common"

    def up(self):
        t = self.table('FATGMember', area="Sta_Data_128", label="FAT Group Members", dump_name="fatgmemb", desc="Members for the FAT Group (FtGrp)")
        t.column('FtGrp', 'character', format="x(8)", initial="", max_width=16, label="FatGroup", column_label="FtGrp", position=2, order=10, help="Fat Group (for products)")
        t.column('MemberType', 'integer', format="9", initial="0", max_width=4, label="Member Type", column_label="Type", position=3, order=20, help="Type of the member (product, FAT Group)")
        t.column('FtgMember', 'character', format="x(16)", initial="", max_width=32, label="Group Member", column_label="Member", position=4, order=30, help="Group member")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=610, label="Memo", column_label="Memo", extent=5, position=5, order=40, help="Explanation / memory field for FTGmember")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=6, order=50, help="Code Of Brand")
        t.index('FtGrp', [['Brand'], ['FtGrp'], ['MemberType'], ['FtgMember']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('FATGMember')
