from gearbox.migrations import Migration

class AddFieldDeliveryType(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('InvText')
        t.column('TemplateId', 'character', format="x(20)", initial="", max_width=40, label="TemplateId", column_label="TemplateId", position=27, order=260, help="Identifier of the message template to be used. (if present)")
        t.column('SchedulingPolicy', 'character', format="x(30)", initial="", max_width=60, label="SchedulingPolicy", column_label="SPolicy", position=28, order=270, help="From template. Name of scheduling policy. (optional)")
        t.column('SchedulingPriority', 'integer', format=">>>9", initial="", max_width=4, label="SchedulingPriority", column_label="SPriority", position=29, order=280, help="From template. Priority of message for scheduling in RabbitMQ. (optional)")
        t.column('Callbacks', 'character', format="x(60)", initial="", max_width=120, label="Callbacks", column_label="Callbacks", position=30, order=290, help="From template. List of callbacks to call (optional)")
        t.column('ParamKeyValue', 'character', format="x(60)", initial="", max_width=120, label="ParamKeyValue", column_label="ParamKV", position=31, order=300, help="¤ separated parameter key=value list. (if present)")
        t.column('UseMMan', 'logical', format="Yes/No", initial="yes", max_width=1, label="Use message manager", column_label="UseMMan", position=32, order=310, help="Use message manager")

    def down(self):
        t = self.alter_table('InvText')
        t.drop_column('TemplateId')
        t.drop_column('SchedulingPolicy')
        t.drop_column('SchedulingPriority')
        t.drop_column('Callbacks')
        t.drop_column('ParamKeys')
        t.drop_column('UseMMan')
