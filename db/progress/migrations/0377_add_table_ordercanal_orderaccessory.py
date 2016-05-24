from gearbox.migrations import Migration

class AddTableOrderAccessory(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('OrderAccessory', area="Sta_Data_32", table_trigger=[{'crc': '?', 'procedure': 'rd-orderaccessory.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'rw-orderaccessory.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="orderacc")
        t.column('OrderId', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="OrderId", column_label="OrderId", position=2, order=10, description="Order sequence number")
        t.column('Amount', 'decimal', format=">>>>9.99", decimals=2, initial="0", max_width=17, label="Amount", column_label="Amount", position=3, order=20)
        t.column('VatAmount', 'decimal', format=">>>>9.99", decimals=2, initial="0", max_width=17, label="VatAmount", column_label="VatAmount", position=4, order=40)
        t.column('ProductCode', 'character', format="x(8)", initial="", max_width=16, label="ProductCode", column_label="ProductCode", position=6, order=50)
        t.column('IMEI', 'character', format="x(12)", initial="", max_width=24, label="IMEI", column_label="IMEI", position=7, order=60)
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=8, order=70, help="Code Of Brand")
        t.column('Discount', 'decimal', format="->>>>9.99", decimals=2, initial="0", max_width=17, label="Discount", position=9, order=80, help="Discount amount")
        t.column('Model', 'character', format="x(30)", initial="", max_width=60, label="Model", column_label="Model", position=10, order=90, help="Terminal model")
        t.column('ModelColor', 'character', format="x(30)", initial="", max_width=60, label="ModelColor", column_label="ModelColor", position=11, order=100, help="Terminal color")
        t.column('Manufacturer', 'character', format="x(30)", initial="", max_width=60, label="Manufacturer", column_label="Manufacturer", position=12, order=110, help="Terminal manufacturer")
        t.column('SIMLockCode', 'character', format="x(20)", initial="", max_width=40, label="SIM Lock Code", position=13, order=120)
        t.column('TerminalType', 'integer', format=">9", initial="0", max_width=4, label="Terminal Type", column_label="Term.Type", position=14, order=130, help="Type of terminal")
        t.column('IMEIReleased', 'date', format="99-99-99", max_width=4, label="IMEIReleased", column_label="IMEIReleased", position=15, order=150, help="Date when IMEI was released")
        t.column('IMEIStatus', 'integer', format=">9", initial="0", max_width=4, label="IMEI Status", column_label="IMEIStat", position=16, order=140, help="Status of IMEI")
        t.column('HardBookState', 'character', format="x(10)", initial="", max_width=20, label="Hard Book State", column_label="HardBookState", position=17, order=180, help="State of hard booking")
        t.column('HardBook', 'integer', format="9", initial="0", max_width=4, label="Hard Book", column_label="HardBook", position=18, order=170, help="Is the terminal hard booked")
        t.index('OrderId', [['Brand'], ['OrderId']], area="Sta_Index_1", primary=True)
        t.index('IMEIStatus', [['Brand'], ['IMEIStatus'], ['IMEIReleased', 'DESC']], area="Sta_Index_1")

    def down(self):
        self.drop_table('OrderAccessory')
