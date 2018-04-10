from gearbox.migrations import Migration

class AddTableSLTemplate(Migration):

    database = "rating"

    def up(self):
        t = self.table('SLTemplate', area="Sta_Data_64", label="Service Limit Template", dump_name="sltemplate", desc="")
        t.column('SLCode', 'character', format="x(12)", max_width=12, label="ServiceLimit", column_label="ServiceLimit", order=10, help="Code of Servicelimit")
        t.column('elementtype', 'integer', format=">9", initial="0", order=30 )
        t.column('BCC', 'integer', format=">>>>>>>9", initial="0", label="BCC", column_label="BCC", help="Billing call case", order=40)
        t.column('unlimitedUsage', 'logical', format="yes/no", initial="false", order=50 )
        t.column('trafficType', 'character', format="x(12)", max_width=12, label="TrafficType", order=60)
        t.column('InclAmt', 'integer', format=">>>9", initial="0", order=70, help="parameter" )
        t.column('bcc_list', 'character', format="x(50)", initial="", order=80, help="bcc-list" )   
        t.column('SLType', 'integer', format=">>9", initial="0", label="SLType", help="ServiceLimit Type")
        t.column('Prior', 'integer', format=">>9", initial="0", label="Priority", column_label="Pri", position=14,  help="Relative priority to other rating buckets")
        t.index('SLCode', ['SLCode'], area="Sta_Index_1")

    def down(self):
        self.drop_table('SLTemplate')
