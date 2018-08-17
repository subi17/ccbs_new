from gearbox.migrations import Migration
class AddTableAccessLog(Migration):

   database = "star"

   def up(self):
      t = self.table('AccessLog', area="Sta_Data_128", label="AccessLog", dump_name="AccessLog", desc="AccessLog")
      t.column('EventTS', 'datetime-tz', format="99-99-9999 hh:mm:ss.sss+hh:mm", initial="", label="EventTS", column_label="EventTS", help="EventTS", position=2, order=10)
      t.column('Action', 'character', format="x(10)", initial="", label="Action", column_label="Action", help="Action", position=3, order=20)
      t.column('UserCode', 'character', format="x(10)", initial="", label="UserCode", column_label="UserCode", help="User id of event", position=4, order=30)
      t.column('Key', 'character', format="x(20)", initial="", label="Key", column_label="Key", help="Key", position=5, order=40)
      t.column('TableName', 'character', format="x(20)", initial="", label="TableName", column_label="Table", help="Table", position=6, order=50)
      t.column('Info', 'character', format="x(20)", initial="", label="Info", column_label="Info", help="Info", position=7, order=60)
      t.column('KeyColumn', 'character', format="x(20)", initial="", label="KeyColumn", column_label="KeyColumn", help="KeyColumn", position=8, order=70)
      t.index('EventTS', [['EventTS' , 'DESC']], area="Sta_Index_64", primary=True)
      t.index('TableName', [['TableName'], ['EventTS' , 'DESC']], area="Sta_Index_64")
      t.index('Key', [['Key'], ['TableName']], area="Sta_Index_64")

   def down(self):
      self.drop_table('AccessLog')

