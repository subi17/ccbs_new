from gearbox.migrations import Migration

class AddTableFATConfig(Migration):

    database = "common"

    def up(self):
        t = self.table('FATConfig', area="Sta_Data_128", label="FAT Configuration", dump_name="fatconfi", desc='''FAT configuration
''')
        t.column('FtGrp', 'character', format="x(8)", initial="", max_width=16, label="FatGroup", column_label="FtGrp", position=2, order=10, help="Fat group")
        t.column('ConfType', 'integer', format=">9", initial="0", max_width=4, label="Config Type", column_label="Type", position=3, order=20, help="Configuration type")
        t.column('ConfTarget', 'character', format="x(12)", initial="", max_width=24, label="Configuration Target", column_label="Target", position=4, order=30, help="Configuration target")
        t.column('ConfRule1', 'character', format="x(30)", initial="", max_width=60, label="Configuration Rule 1", column_label="Rule1", position=5, order=40, help="Configuration rule 1")
        t.column('ConfRule2', 'character', format="x(30)", initial="", max_width=60, label="Configuration Rule 2", column_label="Rule2", position=6, order=50, help="Configuration rule 2")
        t.column('ConfRule3', 'character', format="x(30)", initial="", max_width=60, label="Configuration Rule 3", column_label="Rule3", position=7, order=60, help="Configuration rule 3")
        t.column('ConfRule4', 'character', format="x(30)", initial="", max_width=60, label="Configuration Rule 4", column_label="Rule4", position=8, order=70, help="Configuration rule 4")
        t.column('ValidFrom', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid From", column_label="From", position=9, order=80, help="Valid from")
        t.column('ValidTo', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid To", column_label="To", position=10, order=90, help="Valid to")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=11, order=100, help="Code Of Brand")
        t.index('FTGrp', [['Brand'], ['FtGrp'], ['ConfType'], ['ConfTarget'], ['ValidTo', 'DESC']], area="Sta_Index_2", primary=True)

    def down(self):
        self.drop_table('FATConfig')
