from gearbox.migrations import Migration

class AddTableAMQMsg(Migration):

    database = "common"

    def up(self):
        t = self.table('AMQMsg', area="Sta_Data_128", label="Active MQ Messages", dump_name="amqmsg", desc="Active MQ Messages")
        t.column('MQName', 'character', format="X(15)", initial="", max_width=30, label="MQ Name", column_label="MQ Name", position=2, order=10, help="Message Queue Name")
        t.column('MsgContent', 'character', format="X(30)", initial="", max_width=60, label="Message", column_label="Message", position=3, order=20, help="Message")
        t.column('StatusCode', 'character', format="X(8)", initial="", max_width=16, label="Status", column_label="Status", position=4, order=30, help="Status")
        t.column('Usage', 'character', format="X(8)", initial="", max_width=16, label="Usage", column_label="Usage", position=5, order=40, help="Additional usage information")
        t.column('ConfFile', 'character', format="X(8)", initial="", max_width=16, label="Conf File", column_label="Conf File", position=6, order=50, help="Configuration File")
        t.column('InsertTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Creation Time", column_label="InsertTS", position=7, order=60, help="Creation Time")
        t.column('ResendCount', 'integer', format="zzzzzzz9", initial="0", max_width=4, label="Counter", column_label="Counter", position=8, order=70, help="Resending counter")
        t.index('MQName', [['MQName'], ['InsertTS'], ['StatusCode']], area="Dyn_Index_1", primary=True)
        t.index('InsertTS', [['InsertTS']], area="Dyn_Index_1")
        t.index('StatusCode', [['StatusCode']], area="Dyn_Index_1")

    def down(self):
        self.drop_table('AMQMsg')
