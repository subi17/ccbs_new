from gearbox.migrations import Migration

class AddTableMMLog(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('MMLog', area="CDF_Data", dump_name="mmlog")
        t.column('RequestId', 'character', format="x(39)", initial="", max_width=78, label="RequestId", column_label="RequestId", position=2, order=10, help="First three digits to identify sender + Unique identifier of the message in UUID format")
        t.column('BrandName', 'character', format="x(15)", initial="", max_width=30, label="Brand name", column_label="BrandName", position=3, order=20, help="Brand name")
        t.column('Originator', 'character', format="x(30)", initial="", max_width=60, label="Originator", column_label="Originator", position=4, order=30, help="Name of a calling process / function (optional)")
        t.column('SMSRecipient', 'character', format="x(11)", initial="", max_width=22, label="SMSRecipient", column_label="SMSRecipient", position=5, order=40, help="The subscriber msisdn where the message will be sent (if present)")
        t.column('EmailRecipient', 'character', format="x(60)", initial="", max_width=120, label="EmailRecipient", column_label="EmailRecipient", position=6, order=50, help="The subscriber email where the message will be sent (if present)")
        t.column('PushRecipient', 'character', format="x(60)", initial="", max_width=120, label="PushRecipient", column_label="PushRecipient", position=7, order=60, help="The subscriber msisdn where the message will be sent (if present)")
        t.column('TemplateId', 'character', format="x(20)", initial="", max_width=40, label="TemplateId", column_label="TemplateId", position=8, order=70, help="Identifier of the message template to be used. (if present)")
        t.column('Language', 'character', format="x(2)", initial="", max_width=4, label="Language", column_label="Language", position=9, order=80, help="ISO 639-1 code for language of the message to be sent. (if present)")
        t.column('MessageBody', 'character', format="x(60)", initial="", max_width=120, label="MessageBody", column_label="MessageBody", position=10, order=90, help="Message body to be sent. (if present)")
        t.column('Category', 'character', format="x(30)", initial="", max_width=60, label="Category", column_label="Category", position=11, order=100, help="From template. Identifies application/category of the sender like stock_handling or invoice. (optional)")
        t.column('SchedulingPolicy', 'character', format="x(30)", initial="", max_width=60, label="SchedulingPolicy", column_label="SPolicy", position=110, order=30, help="From template. Name of scheduling policy. (optional)")
        t.column('SchedulingPriority', 'integer', format=">>>9", initial="", max_width=4, label="SchedulingPriority", column_label="SPriority", position=120, order=30, help="From template. Priority of message for scheduling in RabbitMQ. (optional)")
        t.column('Callbacks', 'character', format="x(60)", initial="", max_width=120, label="Callbacks", column_label="Callbacks", position=14, order=130, help="From template. List of callbacks to call (optional)")
        t.column('JsonParam', 'character', format="x(60)", initial="", max_width=120, label="JsonParam", column_label="JsonParam", position=15, order=140, help="Parameters to be used in template substitution. (if present)")
        t.column('RequestTime', 'datetime-tz', format="99-99-9999 HH:MM:SS.SSS+HH:MM", initial="?", max_width=12, label="RequestTime", column_label="RequestTime", position=16, order=150, help="Time stamp when the message was created")
        t.index('RequestTime', [['RequestTime']], area="CDF_Index", primary=True)

    def down(self):
        self.drop_table('MMLog')
